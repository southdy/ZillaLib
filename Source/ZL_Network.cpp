/*
  ZillaLib
  Copyright (C) 2010-2016 Bernhard Schelling

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
*/

#include "ZL_Network.h"
#include "ZL_Application.h"
#include "ZL_Impl.h"
#include "ZL_Platform.h"
#include <assert.h>

#ifdef ZL_USE_ENET

#include "enet/enet.h"
#include <vector>

struct ZL_Connection_Impl;
static std::vector<ZL_Connection_Impl*> *OpenConnections = NULL;
static ZL_MutexHandle ZL_OpenConnectionsMutex;

#define SWAPBE32(v) (((v & 0xFF) << 24) | ((v & 0xFF00) << 8) | ((v >> 8) & 0xFF00) | ((v >> 24) & 0xFF))

struct ZL_Connection_Impl : ZL_Impl
{
	virtual bool KeepAlive() = 0;
	virtual void ShutDown() = 0;
};

static void OpenConnectionsAdd(ZL_Connection_Impl* c)
{
	assert(OpenConnections || (ZL_Application::Log("NETWORK", "*** ERROR: ZL_Network::Init has not been run!"), 0));
	ZL_MutexLock(ZL_OpenConnectionsMutex);
	for (std::vector<ZL_Connection_Impl*>::iterator it = OpenConnections->begin(); it != OpenConnections->end(); ++it)
		if (*it == c) goto skipadd;
	//c->AddRef();
	OpenConnections->push_back(c);
	skipadd:
	ZL_MutexUnlock(ZL_OpenConnectionsMutex);
}

static void OpenConnectionsDel(ZL_Connection_Impl* c)
{
	ZL_MutexLock(ZL_OpenConnectionsMutex);
	for (std::vector<ZL_Connection_Impl*>::iterator it = OpenConnections->begin(); it != OpenConnections->end(); ++it)
		if (*it == c)
		{
			//c->DelRef();
			OpenConnections->erase(it);
			break;
		}
	ZL_MutexUnlock(ZL_OpenConnectionsMutex);
}

static void _ZL_Network_KeepAlive()
{
	ZL_MutexLock(ZL_OpenConnectionsMutex);
	for (std::vector<ZL_Connection_Impl*>::iterator it = OpenConnections->begin(); it != OpenConnections->end();)
	{
		if (!(*it)->KeepAlive())
			it = OpenConnections->erase(it);
		else ++it;
	}
	ZL_MutexUnlock(ZL_OpenConnectionsMutex);
}

bool ZL_Network::Init()
{
	if (OpenConnections) return true;
	if (enet_initialize() != 0) return false;
	OpenConnections = new std::vector<ZL_Connection_Impl*>();
	ZL_MutexInit(ZL_OpenConnectionsMutex);
	ZL_Application::sigKeepAlive.connect(&_ZL_Network_KeepAlive);
	return true;
}

void ZL_Network::DeInit()
{
	if (!OpenConnections) return;
	ZL_MutexLock(ZL_OpenConnectionsMutex);
	for (std::vector<ZL_Connection_Impl*>::iterator it = OpenConnections->begin(); it != OpenConnections->end(); ++it)
	{
		(*it)->ShutDown();
	}
	OpenConnections->clear();
	delete OpenConnections;
	OpenConnections = NULL;
	ZL_MutexUnlock(ZL_OpenConnectionsMutex);
	ZL_MutexDestroy(ZL_OpenConnectionsMutex);
}

//----------------------------------------------------------------------------------------------------------------------------------------------

struct ZL_Server_Impl : ZL_Connection_Impl
{
	ENetHost *host;
	std::vector<ZL_PeerHandle> clients;
	ZL_Signal_v1<ZL_Peer&> sigConnected;
	ZL_Signal_v1<ZL_Peer&> sigDisconnected;
	ZL_Signal_v2<ZL_Peer&, ZL_Packet&> sigReceived;

	ZL_Server_Impl() : host(NULL) { }

	ZL_Server_Impl(int server_port, int num_connections, const char *bind_host) : host(NULL)
	{
		assert(OpenConnections || (ZL_Application::Log("NETWORK", "Tried to open a connection without initializing with ZL_Network::Init first"), 0));
		Open(server_port, num_connections, bind_host);
	}

	~ZL_Server_Impl() { Close(0, true); }

	void ShutDown() { Close(0, false); }

	void Open(int server_port, int num_connections, const char *bind_host)
	{
		if (host) Close(0, true);

		ENetAddress address;
		if (bind_host) enet_address_set_host(&address, bind_host);
		else address.host = ENET_HOST_ANY;
		address.port = server_port;

		host = enet_host_create(&address, num_connections, 0, 0, 0);
		if (!host) return;
		OpenConnectionsAdd(this);
	}

	void Close(unsigned int closemsg, bool remove_openconnection)
	{
		for (std::vector<ZL_PeerHandle>::iterator it = clients.begin(); it != clients.end(); ++it)
		    enet_peer_disconnect((ENetPeer*)(*it), closemsg);
		if (host && clients.size()) enet_host_flush(host);
		clients.clear();
		if (!host) return;
		enet_host_destroy(host);
		host = NULL;
		if (remove_openconnection) OpenConnectionsDel(this);
	}

	bool KeepAlive()
	{
		assert(host);
		if (!host) return false;
		ENetEvent event;
		while (enet_host_service(host, &event, 0) > 0)
		{
			ZL_Peer peer = { SWAPBE32(event.peer->address.host), event.peer->address.port, &event.peer->data, event.peer };
			switch (event.type)
			{
				case ENET_EVENT_TYPE_RECEIVE:
					{
						ZL_Packet packet = { event.packet->data, event.packet->dataLength, event.channelID, (event.packet->flags & ENET_PACKET_FLAG_RELIABLE ? ZL_PACKET_RELIABLE : (event.packet->flags & ENET_PACKET_FLAG_UNSEQUENCED ? ZL_PACKET_UNSEQUENCED : ZL_PACKET_UNRELIABLE)) };
						sigReceived.call(peer, packet);
					}
					enet_packet_destroy(event.packet);
					break;
				case ENET_EVENT_TYPE_CONNECT:
					clients.push_back(event.peer);
					sigConnected.call(peer); //ToDo: include event.data
					break;
				case ENET_EVENT_TYPE_DISCONNECT:
					clients.erase(std::remove(clients.begin(), clients.end(), event.peer), clients.end());
					sigDisconnected.call(peer);
					break;
				case ENET_EVENT_TYPE_NONE:
					break;
			}
		}
		return true;
	}
};

ZL_IMPL_OWNER_DEFAULT_IMPLEMENTATIONS(ZL_Server)

ZL_Server::ZL_Server(int server_port, int num_connections, const char *bind_host) : impl(new ZL_Server_Impl(server_port, num_connections, bind_host)) { }

void ZL_Server::Open(int server_port, int num_connections, const char *bind_host)
{
	if (!impl) impl = new ZL_Server_Impl(server_port, num_connections, bind_host);
	else impl->Open(server_port, num_connections, bind_host);
}

void ZL_Server::Close(unsigned int closemsg)
{
	if (impl && impl->host) impl->Close(closemsg, true);
}

void ZL_Server::Send(std::vector<ZL_PeerHandle> peerhandles, ZL_Packet &packet)
{
	if (!impl || !impl->host) return;
	ENetPacket* enetpacket = enet_packet_create(packet.data, packet.length, (packet.type == ZL_PACKET_RELIABLE ? ENET_PACKET_FLAG_RELIABLE : (packet.type == ZL_PACKET_UNRELIABLE ? 0 : ENET_PACKET_FLAG_UNSEQUENCED)));
	for (std::vector<ZL_PeerHandle>::iterator it = peerhandles.begin(); it != peerhandles.end(); ++it)
		enet_peer_send((ENetPeer*)(*it), packet.channel, enetpacket);
}

void ZL_Server::Send(ZL_PeerHandle peerhandle, ZL_Packet &packet)
{
	if (!impl || !impl->host) return;
	enet_peer_send((ENetPeer*)peerhandle, packet.channel, enet_packet_create(packet.data, packet.length, (packet.type == ZL_PACKET_RELIABLE ? ENET_PACKET_FLAG_RELIABLE : (packet.type == ZL_PACKET_UNRELIABLE ? 0 : ENET_PACKET_FLAG_UNSEQUENCED))));
}

void ZL_Server::Broadcast(ZL_Packet &packet)
{
	if (!impl || !impl->host) return;
	enet_host_broadcast(impl->host, packet.channel, enet_packet_create(packet.data, packet.length, (packet.type == ZL_PACKET_RELIABLE ? ENET_PACKET_FLAG_RELIABLE : (packet.type == ZL_PACKET_UNRELIABLE ? 0 : ENET_PACKET_FLAG_UNSEQUENCED))));
}

void ZL_Server::Broadcast(ZL_Packet &packet, ZL_PeerHandle peerhandle_except)
{
	if (!impl || !impl->host) return;
	ENetPacket* enetpacket = enet_packet_create(packet.data, packet.length, (packet.type == ZL_PACKET_RELIABLE ? ENET_PACKET_FLAG_RELIABLE : (packet.type == ZL_PACKET_UNRELIABLE ? 0 : ENET_PACKET_FLAG_UNSEQUENCED)));
	for (std::vector<ZL_PeerHandle>::iterator it = impl->clients.begin(); it != impl->clients.end(); ++it)
		if ((*it) != peerhandle_except)
			enet_peer_send((ENetPeer*)(*it), packet.channel, enetpacket);
}

void ZL_Server::Send(std::vector<ZL_PeerHandle> peerhandles, const void* data, size_t length, unsigned char channel, ZL_Packet_Reliability type)
{
	if (!impl || !impl->host) return;
	ENetPacket* enetpacket = enet_packet_create(data, length, (type == ZL_PACKET_RELIABLE ? ENET_PACKET_FLAG_RELIABLE : (type == ZL_PACKET_UNRELIABLE ? 0 : ENET_PACKET_FLAG_UNSEQUENCED)));
	for (std::vector<ZL_PeerHandle>::iterator it = peerhandles.begin(); it != peerhandles.end(); ++it)
		enet_peer_send((ENetPeer*)(*it), channel, enetpacket);
}

void ZL_Server::Send(ZL_PeerHandle peerhandle, const void* data, size_t length, unsigned char channel, ZL_Packet_Reliability type)
{
	if (!impl || !impl->host) return;
	enet_peer_send((ENetPeer*)peerhandle, channel, enet_packet_create(data, length, (type == ZL_PACKET_RELIABLE ? ENET_PACKET_FLAG_RELIABLE : (type == ZL_PACKET_UNRELIABLE ? 0 : ENET_PACKET_FLAG_UNSEQUENCED))));
}

void ZL_Server::Broadcast(const void* data, size_t length, unsigned char channel, ZL_Packet_Reliability type)
{
	if (!impl || !impl->host) return;
	enet_host_broadcast(impl->host, channel, enet_packet_create(data, length, (type == ZL_PACKET_RELIABLE ? ENET_PACKET_FLAG_RELIABLE : (type == ZL_PACKET_UNRELIABLE ? 0 : ENET_PACKET_FLAG_UNSEQUENCED))));
}

void ZL_Server::Broadcast(const void* data, size_t length, ZL_PeerHandle peerhandle_except, unsigned char channel, ZL_Packet_Reliability type)
{
	if (!impl || !impl->host) return;
	ENetPacket* enetpacket = enet_packet_create(data, length, (type == ZL_PACKET_RELIABLE ? ENET_PACKET_FLAG_RELIABLE : (type == ZL_PACKET_UNRELIABLE ? 0 : ENET_PACKET_FLAG_UNSEQUENCED)));
	for (std::vector<ZL_PeerHandle>::iterator it = impl->clients.begin(); it != impl->clients.end(); ++it)
		if ((*it) != peerhandle_except)
			enet_peer_send((ENetPeer*)(*it), channel, enetpacket);
}

const std::vector<ZL_PeerHandle> &ZL_Server::GetPeerHandles()
{
	if (!impl) impl = new ZL_Server_Impl();
	return impl->clients;
}

std::vector<ZL_Peer> ZL_Server::GetPeerDetails()
{
	if (!impl) impl = new ZL_Server_Impl();
	std::vector<ZL_Peer> list;
	for (std::vector<ZL_PeerHandle>::iterator it = impl->clients.begin(); it != impl->clients.end(); ++it)
	{
		ZL_Peer peer = { SWAPBE32(((ENetPeer*)(*it))->address.host), ((ENetPeer*)(*it))->address.port, &((ENetPeer*)(*it))->data, (*it) };
		list.push_back(peer);
	}
	return list;
}

ZL_Signal_v1<ZL_Peer&>& ZL_Server::sigConnected()
{
	if (!impl) impl = new ZL_Server_Impl();
	return impl->sigConnected;
}

ZL_Signal_v1<ZL_Peer&>& ZL_Server::sigDisconnected()
{
	if (!impl) impl = new ZL_Server_Impl();
	return impl->sigDisconnected;
}

ZL_Signal_v2<ZL_Peer&, ZL_Packet&>& ZL_Server::sigReceived()
{
	if (!impl) impl = new ZL_Server_Impl();
	return impl->sigReceived;
}

bool ZL_Server::IsOpened()
{
	return (impl && impl->host);
}

//----------------------------------------------------------------------------------------------------------------------------------------------

struct ZL_Client_Impl : ZL_Connection_Impl
{
	ENetHost *host;
	ENetPeer *server;
	bool (*doNatPunchKeepAlive)(ZL_Client_Impl*);
	ZL_Signal_v0 sigConnected;
	ZL_Signal_v0 sigDisconnected;
	ZL_Signal_v1<ZL_Packet&> sigReceived;

	ZL_Client_Impl() : host(NULL), server(NULL), doNatPunchKeepAlive(NULL) { }

	ZL_Client_Impl(const char *connect_host, int port, int num_channels) : host(NULL), server(NULL), doNatPunchKeepAlive(NULL)
	{
		assert(OpenConnections || (ZL_Application::Log("NETWORK", "Tried to open a connection without initializing with ZL_Network::Init first"),0));
		Connect(connect_host, port, num_channels);
	}

	~ZL_Client_Impl() { Disconnect(0, true); }

	void ShutDown() { Disconnect(0, false); }

	void Connect(const char *connect_host, int port, int num_channels)
	{
		if (host) Disconnect(0, true);

		host = enet_host_create(NULL, 1, num_channels, 0, 0);
		if (!host) return;

		ENetAddress address;
		enet_address_set_host(&address, connect_host);
		address.port = port;

		server = enet_host_connect(host, &address, num_channels, 0);

		OpenConnectionsAdd(this);
	}

	void NatPunch(const char *relay_host, int relay_port, const unsigned char punch_key[8], int num_channels)
	{
		if (host) Disconnect(0, true);

		host = enet_host_create(NULL, 1, num_channels, 0, 0);
		if (!host) return;

		ENetAddress address;
		enet_address_set_host(&address, relay_host);
		address.port = relay_port;

		ENetBuffer bs;
		bs.data = (void*)punch_key;
		bs.dataLength = 8;
		enet_socket_send(host->socket, &address, &bs, 1);
		memcpy(host->packetData, punch_key, 8); //store temporary in unused place for validation before real connection

		doNatPunchKeepAlive = &NatPunchKeepAlive;

		OpenConnectionsAdd(this);
	}

	void Disconnect(unsigned int closemsg, bool remove_openconnection)
	{
		if (server) enet_peer_disconnect(server, closemsg);
		if (!host) return;
		if (server) enet_host_flush(host);
		enet_host_destroy(host);
		host = NULL; server = NULL;
		if (remove_openconnection) OpenConnectionsDel(this);
	}

	static bool NatPunchKeepAlive(ZL_Client_Impl* self)
	{
		unsigned int waitCondition = ENET_SOCKET_WAIT_RECEIVE | ENET_SOCKET_WAIT_SEND;
		if (enet_socket_wait(self->host->socket, &waitCondition, 0) != 0) return true;
		if (!(waitCondition & ENET_SOCKET_WAIT_RECEIVE)) return true;

		unsigned char buffer[8+4+2]; //key + ip + port
		ENetBuffer br;
		br.data = buffer;
		br.dataLength = sizeof(buffer);
		int rec = enet_socket_receive(self->host->socket, NULL, &br, 1);
		if ((rec != 8 && rec != 8+4+2) || memcmp(buffer, self->host->packetData, 8)) return true; //unknown response
		if (rec == 8+4+2) //master
		{
			//ZL_LOG3("NET", "    Received nat punch data: [%.*s] (%d bytes)", rec, (rec > 0 ? buffer : NULL), rec);
			ENetAddress address;
			address.host = buffer[ 8] | (buffer[ 9] << 8) | (buffer[10] << 16) | (buffer[11] << 24);
			address.port = buffer[12] | (buffer[13] << 8);
			self->server = enet_host_connect(self->host, &address, 1, 1);
		}
		else //slave
		{
			//ZL_LOG3("NET", "    Received wait for connection data: [%.*s] (%d bytes)", rec, (rec > 0 ? buffer : NULL), rec);
			self->server = self->host->peers; //will be first connecting client
			self->server->outgoingSessionID = 0; //change from 0xFF to 0 to make outgoingSessionID to 1 on first connection (incomingSessionID will be 1 on master)
		}
		self->doNatPunchKeepAlive = NULL;
		return false;
	}

	bool KeepAlive()
	{
		assert(host);
		if (!host) return false;
		if (doNatPunchKeepAlive && doNatPunchKeepAlive(this)) return true;
		ENetEvent event;
		while (enet_host_service(host, &event, 0) > 0)
		{
			switch (event.type)
			{
				case ENET_EVENT_TYPE_RECEIVE:
					{
						ZL_Packet packet = { event.packet->data, event.packet->dataLength, event.channelID, (event.packet->flags & ENET_PACKET_FLAG_RELIABLE ? ZL_PACKET_RELIABLE : (event.packet->flags & ENET_PACKET_FLAG_UNSEQUENCED ? ZL_PACKET_UNSEQUENCED : ZL_PACKET_UNRELIABLE)) };
						sigReceived.call(packet);
					}
					enet_packet_destroy(event.packet);
					break;
				case ENET_EVENT_TYPE_CONNECT:
					sigConnected.call();
					break;
				case ENET_EVENT_TYPE_DISCONNECT:
					enet_host_destroy(host);
					host = NULL; server = NULL;
					sigDisconnected.call();
					return false;
				case ENET_EVENT_TYPE_NONE:
					break;
			}
		}
		return true;
	}
};

ZL_IMPL_OWNER_DEFAULT_IMPLEMENTATIONS(ZL_Client)

ZL_Client::ZL_Client(const char *host, int port, int num_channels) : impl(new ZL_Client_Impl(host, port, num_channels)) { }

void ZL_Client::Connect(const char *host, int port, int num_channels)
{
	if (!impl) impl = new ZL_Client_Impl(host, port, num_channels);
	else impl->Connect(host, port, num_channels);
}

void ZL_Client::NatPunch(const char *relay_host, int relay_port, const unsigned char punch_key[8], int num_channels)
{
	if (!impl) impl = new ZL_Client_Impl();
	impl->NatPunch(relay_host, relay_port, punch_key, num_channels);
}

void ZL_Client::Disconnect(unsigned int closemsg)
{
	if (impl) impl->Disconnect(closemsg, true);
}

void ZL_Client::Send(ZL_Packet &packet)
{
	if (!impl || !impl->host ||!impl->server) return;
	enet_peer_send(impl->server, packet.channel, enet_packet_create(packet.data, packet.length, (packet.type == ZL_PACKET_RELIABLE ? ENET_PACKET_FLAG_RELIABLE : (packet.type == ZL_PACKET_UNRELIABLE ? 0 : ENET_PACKET_FLAG_UNSEQUENCED))));
}

void ZL_Client::Send(const void* data, size_t length, unsigned char channel, ZL_Packet_Reliability type)
{
	if (!impl || !impl->host ||!impl->server) return;
	enet_peer_send(impl->server, channel, enet_packet_create(data, length, (type == ZL_PACKET_RELIABLE ? ENET_PACKET_FLAG_RELIABLE : (type == ZL_PACKET_UNRELIABLE ? 0 : ENET_PACKET_FLAG_UNSEQUENCED))));
}

ZL_Signal_v0& ZL_Client::sigConnected()
{
	if (!impl) impl = new ZL_Client_Impl();
	return impl->sigConnected;
}

ZL_Signal_v0& ZL_Client::sigDisconnected()
{
	if (!impl) impl = new ZL_Client_Impl();
	return impl->sigDisconnected;
}

ZL_Signal_v1<ZL_Packet&>& ZL_Client::sigReceived()
{
	if (!impl) impl = new ZL_Client_Impl();
	return impl->sigReceived;
}

bool ZL_Client::IsConnected()
{
	return (impl && impl->host && impl->server);
}

bool ZL_Client::IsNatPunchSlave()
{
	return (impl && impl->server && impl->server->incomingSessionID == 0);
}

//----------------------------------------------------------------------------------------------------------------------------------------------

struct ZL_RawSocket_Impl : ZL_Connection_Impl
{
	ENetSocket socket;
	ZL_Signal_v1<ZL_Packet&> sigReceived;
	ZL_RawSocket::Type type;

	ZL_RawSocket_Impl() : socket(0) { }

	~ZL_RawSocket_Impl()
	{
		OpenConnectionsDel(this);
		ShutDown();
	}

	void ShutDown()
	{
		if (!socket) return;
		enet_socket_destroy(socket);
		socket = 0;
	}

	bool ConnectSync(const char *host, int port, ZL_RawSocket::Type type)
	{
		if (socket) OpenConnectionsDel(this);
		ShutDown();
		//ZL_LOG2("NET", "Connect to host: %s - Port: %d", host, port);
		this->type = type;
		ENetAddress address;
		if (enet_address_set_host(&address, host) != 0) return false;
		address.port = port;
		//ZL_LOG1("NET", "    Host resolved address: %x", address.host);
		socket = enet_socket_create(type == ZL_RawSocket::TYPE_TCP ? ENET_SOCKET_TYPE_STREAM : ENET_SOCKET_TYPE_DATAGRAM);
		if (!socket) return false;
		//ZL_LOG0("NET", "    Socket created");
		if (enet_socket_connect(socket, &address) != 0) { enet_socket_destroy(socket); socket = 0; return false; }
		//ZL_LOG0("NET", "    Connected");
		OpenConnectionsAdd(this);
		return true;
	}

	bool KeepAlive()
	{
		assert(socket);
		if (!socket) return false;
		while (1)
		{
			unsigned int waitCondition = ENET_SOCKET_WAIT_RECEIVE | ENET_SOCKET_WAIT_SEND;
			if (enet_socket_wait(socket, &waitCondition, 0) != 0) return true;
			if (!(waitCondition & ENET_SOCKET_WAIT_RECEIVE)) return true;

			char buffer[1024];
			ENetBuffer br;
			br.data = buffer;
			br.dataLength = 1024;
			int rec = enet_socket_receive(socket, NULL, &br, 1);
			//ZL_LOG1("NET", "    Received packet of size: %d", rec);
			ZL_Packet p;
			p.data = (rec>0 ? buffer : NULL);
			p.length = (rec>0 ? rec : 0);
			p.channel = 0;
			p.type = (rec < 0 ? ZL_PACKET_ERROR : ZL_PACKET_RELIABLE);
			sigReceived.call(p);
			if (type == ZL_RawSocket::TYPE_TCP && rec <= 0) return false;
		}
		return true;
	}
};

ZL_IMPL_OWNER_DEFAULT_IMPLEMENTATIONS(ZL_RawSocket)

bool ZL_RawSocket::ConnectSync(const char *host, int port, Type type)
{
	if (!impl) impl = new ZL_RawSocket_Impl();
	return impl->ConnectSync(host, port, type);
}

void ZL_RawSocket::Disconnect()
{
	if (!impl) return;
	OpenConnectionsDel(impl);
	impl->ShutDown();
}

int ZL_RawSocket::Send(const void* data, size_t length)
{
	if (!impl) return 0;
	ENetBuffer bs;
	bs.data = (void*)data;
	bs.dataLength = length;
	return enet_socket_send(impl->socket, NULL, &bs, 1);
}

bool ZL_RawSocket::IsConnected() { return (impl && impl->socket != 0); }

ZL_Signal_v1<ZL_Packet&>& ZL_RawSocket::sigReceived() const
{
	return impl->sigReceived;
}

//----------------------------------------------------------------------------------------------------------------------------------------------

struct ZL_BasicTCPConnection_Impl : ZL_Connection_Impl
{
	ENetSocket socket;
	bool started, first_keep_alive, cancel;

	ZL_BasicTCPConnection_Impl() : socket(0), started(false), first_keep_alive(true), cancel(false) { }

	~ZL_BasicTCPConnection_Impl()
	{
		ShutDown();
		if (started && !first_keep_alive) OpenConnectionsDel(this);
	}

	void ShutDown()
	{
		cancel = true;
		if (!socket) return;
		enet_socket_destroy(socket);
		socket = 0;
	}

	void Connect()
	{
		if (started) return;
		started = true;
		AddRef();
		ZL_CreateThread(&ConnectRunThread, (void*)this);
	}

	virtual bool GetHost(ZL_String& host, int& port) = 0;
	
	static void* ConnectRunThread(void* vimpl)
	{
		ZL_BasicTCPConnection_Impl* impl = (ZL_BasicTCPConnection_Impl*)vimpl;
		ENetSocket tmpsocket = 0;
		ENetAddress address;
		ZL_String host; int port; if (!impl->GetHost(host, port)) goto done;

		//ZL_LOG2("NET", "Connect to host: %s - Port: %d", host.c_str(), port);
		if (impl->cancel || impl->GetRefCount() == 1 || enet_address_set_host(&address, host.c_str()) != 0) goto done;

		address.port = port;
		//ZL_LOG1("NET", "    Host resolved address: %x", address.host);
		if (impl->cancel || impl->GetRefCount() == 1 || (!(tmpsocket = enet_socket_create(ENET_SOCKET_TYPE_STREAM)))) goto done;

		//ZL_LOG0("NET", "    Socket created");
		if (impl->cancel || impl->GetRefCount() == 1 || enet_socket_connect(tmpsocket, &address) != 0) goto done;

		impl->socket = tmpsocket;
		tmpsocket = 0;

		done:
		if (tmpsocket) enet_socket_destroy(tmpsocket);
		if (impl->cancel) { impl->DelRef(); return NULL; }
		else { OpenConnectionsAdd(impl); return NULL; }
	}

	static bool SplitUrl(const ZL_String& url, ZL_String::size_type protocol_name_length, ZL_String* out_path = NULL, ZL_String* out_host = NULL, int* out_port = NULL)
	{
		ZL_String::size_type host_begin = url.find("://");
		if (host_begin == ZL_String::npos || host_begin != protocol_name_length) return false;
		host_begin += 3;

		ZL_String::size_type host_end = url.find("/", host_begin);
		if (host_end == ZL_String::npos) host_end = url.length();

		ZL_String::size_type auth_split = url.find("@", host_begin);
		if (auth_split != ZL_String::npos && auth_split < host_end) { assert(false); return false; } //http authentication not supported

		int port;
		ZL_String::size_type port_split = url.find(":", host_begin);
		if (port_split != ZL_String::npos && port_split > host_end) port_split = ZL_String::npos;
		port = (port_split != ZL_String::npos ? atoi(url.c_str()+port_split+1) : 80);
		if (port <= 0) return false; //port supplied was error

		if (out_port) *out_port = port;
		if (out_host) *out_host = url.substr(host_begin, (port_split == ZL_String::npos ? host_end : port_split) - host_begin);
		if (out_path) *out_path = (host_end == url.length() ? "/" : url.substr(host_end));
		return true;
	}

	char* GetHTTPContentStart(char* buffer, size_t length)
	{
		for (char *content = buffer, *end = buffer + length; content < end-2; content++)
			if ((content[0]=='\n'&&content[1]=='\n') || (content < end-4 && content[0]=='\r'&&content[1]=='\n'&&content[2]=='\r'&&content[3]=='\n'))
				return content + (content[0]=='\n' ? 2 : 4);
		return NULL;
	}
};

struct ZL_HttpConnection_Impl : ZL_BasicTCPConnection_Impl
{
	ZL_String url;
	std::vector<char> post_data;
	ZL_Signal_v2<int, const ZL_String&> sigReceivedString;
	ZL_Signal_v3<int, const char*, size_t> sigReceivedData;
	std::vector<char> data;
	unsigned int timeout_tick, timeout_msec;
	bool dostream;
	int http_status;

	ZL_HttpConnection_Impl() : timeout_msec(10000), dostream(false), http_status(0) { }

	virtual bool GetHost(ZL_String& host, int& port)
	{
		return SplitUrl(url, 4, NULL, &host, &port); //only support "http" protocol
	}

	bool KeepAlive()
	{
		if (first_keep_alive)
		{
			bool done = (GetRefCount() == 1);
			DelRef();
			if (socket)
			{
				ZL_String header, path, host;
				if (done || !SplitUrl(url, 4, &path, &host)) return false; //only support "http" protocol
				first_keep_alive = false;

				ENetBuffer bs;
				header << (post_data.size() ? "POST " : "GET ") << path << " HTTP/1.0\nHost: " << host << "\nUser-Agent: ZillaLib/1.0\n";
				if (post_data.size())
				{
					header << "Content-Length: " << ((unsigned int)post_data.size()) << "\n\n";
					size_t header_postdata_offset = header.size();
					header.resize(header_postdata_offset + post_data.size());
					memcpy(((char*)header.c_str())+header_postdata_offset, &post_data[0], post_data.size());
					bs.data = (void*)header.c_str();
					bs.dataLength = header.size();
				}
				else { header << "\n"; bs.data = (void*)header.c_str(); bs.dataLength = header.size(); }

				if (enet_socket_send(socket, NULL, &bs, 1) <= 0) return false;

				timeout_tick = (timeout_msec ? ZL_Application::Ticks + timeout_msec : 0);
				data.clear();
			}
		}
		if (!socket)
		{
			sigReceivedString.call(-1, ZL_String::EmptyString);
			sigReceivedData.call(-1, NULL, 0);
			ShutDown();
			return false;
		}
		while (1)
		{
			unsigned int waitCondition = ENET_SOCKET_WAIT_RECEIVE | ENET_SOCKET_WAIT_SEND;
			if (enet_socket_wait(socket, &waitCondition, 0) != 0) return true;
			if ((!(waitCondition & ENET_SOCKET_WAIT_RECEIVE)) && (!timeout_msec || ZL_Application::Ticks < timeout_tick)) return true;

			unsigned char buffer[1024];
			ENetBuffer br;
			br.data = buffer;
			br.dataLength = 1024;
			int rec = ((timeout_msec && ZL_Application::Ticks >= timeout_tick) ? 0 : enet_socket_receive(socket, NULL, &br, 1));
			//ZL_LOG1("NET", "    Received packet of size: %d", rec);
			if (dostream && http_status == 0)
			{
				char *status = NULL;
				for (status = (char*)buffer; status < (char*)buffer+rec-2; status++) if (*status == ' ') break;
				http_status = (status < (char*)buffer+rec-2 ? atoi(status+1) : 0);
			}
			if (rec > 0)
			{
				if (timeout_msec) timeout_tick = ZL_Application::Ticks + timeout_msec; //got data, update timeout tick
				if (dostream)
				{
					if (sigReceivedString.HasConnections())
						sigReceivedString.call(http_status, ZL_String((char*)buffer, rec));
					if (sigReceivedData.HasConnections())
						sigReceivedData.call(http_status, (char*)buffer, rec);
				}
				else
				{
					//store data until finished
					data.resize(data.size()+rec);
					memcpy(&data[data.size()-rec], buffer, rec);
				}
			}
			else
			{
				char* content = NULL, *end = (data.empty() ? NULL : &data[0]+data.size()), *status = NULL;
				if (!dostream && data.size())
				{
					content = GetHTTPContentStart(&data[0], data.size());
					for (status = &data[0]; status < end-2; status++) if (*status == ' ') break;
					http_status = atoi(status+1);
				}
				size_t data_size = (content ? end - content : 0);

				if (sigReceivedString.HasConnections())
					sigReceivedString.call(http_status, (content ? ZL_String(content, data_size) : ZL_String::EmptyString));
				if (sigReceivedData.HasConnections())
					sigReceivedData.call(http_status, content, data_size);

				data.clear();
				ShutDown();
				return false;
			}
		}
	}
};

struct ZL_WebSocketConnection_Impl : ZL_BasicTCPConnection_Impl
{
	ZL_String url;
	bool websocket_active;
	ZL_Signal_v1<const ZL_String&> sigReceivedText;
	ZL_Signal_v2<const char*, size_t> sigReceivedBinary;
	ZL_Signal_v0 sigConnected;
	ZL_Signal_v0 sigDisconnected;

	ZL_WebSocketConnection_Impl() : websocket_active(false) { }
	
	virtual bool GetHost(ZL_String& host, int& port)
	{
		return SplitUrl(url, 2, NULL, &host, &port); //only support "ws" protocol
	}

	void Disconnect(unsigned short code, const char* buf, size_t len)
	{
		code = ENET_HOST_TO_NET_16(code);
		if (socket && len)
		{
			unsigned short* sendbuf = (unsigned short*)malloc(2 + len); sendbuf[0] = code; memcpy(sendbuf+1, buf, len);
			Send(OPCODE_CONNECTIONCLOSE, sendbuf, 2 + len);
			free(sendbuf);
		}
		else if (socket) Send(OPCODE_CONNECTIONCLOSE, &code, 2);
		ShutDown();
	}

	bool KeepAlive()
	{
		if (first_keep_alive)
		{
			bool done = (GetRefCount() == 1);
			DelRef();
			if (socket)
			{
				ZL_String header, path, host;
				if (done || !SplitUrl(url, 2, &path, &host)) return false; //only support "ws" protocol
				first_keep_alive = false;

				ENetBuffer bs;
				header << "GET " << path << " HTTP/1.1\r\nHost: " << host << "\r\nUser-Agent: ZillaLib/1.0\r\nSec-WebSocket-Version: 13\r\n"
					"Origin: null\r\nSec-WebSocket-Key: YPersQl07qSADJGSpuu5jw==\r\nConnection: keep-alive, Upgrade\r\nPragma: no-cache\r\nCache-Control: no-cache\r\nUpgrade: websocket\r\n\r\n";
				bs.data = (void*)header.c_str(); bs.dataLength = header.size();
				if (enet_socket_send(socket, NULL, &bs, 1) <= 0) return false;
			}
		}
		if (!socket)
		{
			sigDisconnected.call();
			ShutDown();
			return false;
		}
		while (1)
		{
			unsigned int waitCondition = ENET_SOCKET_WAIT_RECEIVE | ENET_SOCKET_WAIT_SEND;
			if (enet_socket_wait(socket, &waitCondition, 0) != 0) return true;
			if (!(waitCondition & ENET_SOCKET_WAIT_RECEIVE)) return true;

			char bufferstack[1024], *bufferheap = NULL, *buffer = bufferstack;
			ENetBuffer br;
			br.data = buffer;
			br.dataLength = 1024;
			size_t rec = 0;
			for (int r; (r = enet_socket_receive(socket, NULL, &br, 1)) > 0;)
			{
				rec += r;
				if (r != 1024) break;
				buffer = bufferheap = (char*)realloc(bufferheap, rec + 1024);
				if (rec == 1024) memcpy(bufferheap, bufferstack, 1024);
				br.data = bufferheap + rec;
			}
			if (rec < 2) websocket_active = false;
			else
			{
				if (!websocket_active)
				{
					char *status = NULL;
					for (status = (char*)buffer; status < (char*)buffer+rec-2; status++) if (*status == ' ') break;
					int http_status = (status < (char*)buffer+rec-2 ? atoi(status+1) : 0);
					if (http_status == 101)
					{
						websocket_active = true;
						sigConnected.call();
						if ((status = GetHTTPContentStart(buffer, rec)) != NULL) { rec -= status - buffer; buffer = status; }
						else rec = 2; //ignore rest
					}
				}
				while (websocket_active && rec >= 2)
				{
					unsigned char opcode = (buffer[0] & _OPCODE_BITMASK);
					size_t headerlen = 2 + ((buffer[1]&127)==127 ? 8 : ((buffer[1]&127)==126 ? 2 : 0));
					if (rec < headerlen) break;
					size_t len = (headerlen == 2 ? buffer[1]&127 : (headerlen == 4 ? ENET_NET_TO_HOST_16(*(const short*)(buffer+2)) : ENET_NET_TO_HOST_32(*(unsigned int*)(buffer+6))));
					if (rec < headerlen+len) break;
					switch (opcode)
					{
						case OPCODE_TEXT:   if (sigReceivedText.HasConnections()) sigReceivedText.call(ZL_String((char*)buffer+headerlen, len)); break;
						case OPCODE_BINARY: if (sigReceivedBinary.HasConnections()) sigReceivedBinary.call((char*)buffer+headerlen, len); break;
						case OPCODE_PING:   Send(OPCODE_PONG, buffer+headerlen, len); break;
						case OPCODE_CONNECTIONCLOSE: websocket_active = false; break;
					}
					rec -= headerlen+len;
					buffer += headerlen+len;
				}
			}
			if (bufferheap) free(bufferheap);
			if (!websocket_active) { ShutDown(); return KeepAlive(); } //abort
		}
	}

	enum { OPCODE_CONTINUATION = 0, OPCODE_TEXT = 1, OPCODE_BINARY = 2, OPCODE_CONNECTIONCLOSE = 8, OPCODE_PING = 9, OPCODE_PONG = 10, _OPCODE_BITMASK = 0xF };

	void Send(unsigned char opcode, const void* buf, size_t len)
	{
		if (!socket) return;
		unsigned char header[2 + 8 + 4] = { (unsigned char)(0x80 | opcode), 0x80 }, headerlen = 2;
		if      (len > 0xFFFF) { headerlen += 8; header[1] |= 127; ((unsigned int*)(header+2))[0] = 0; ((unsigned int*)(header+2))[1] = ENET_HOST_TO_NET_32((unsigned int)len); }
		else if (len >    125) { headerlen += 2; header[1] |= 126; ((unsigned short*)header)[1] = ENET_HOST_TO_NET_16((unsigned short)len); }
		else header[1] |= len;
		*(unsigned int*)(header+headerlen) = 0; headerlen += 4; //mask
		ENetBuffer bs;
		bs.data = malloc(bs.dataLength = headerlen + len);
		memcpy(bs.data, header, headerlen);
		memcpy((unsigned char*)bs.data+headerlen, buf, len);
		enet_socket_send(socket, NULL, &bs, 1);
		free(bs.data);
	}

	void SendText(const char* buf, size_t len) { Send(ZL_WebSocketConnection_Impl::OPCODE_TEXT, buf, len); }
	void SendBinary(const void* buf, size_t len) { Send(ZL_WebSocketConnection_Impl::OPCODE_BINARY, buf, len); }
};

#else //ZL_USE_ENET

bool ZL_Network::Init() { return true; }
void ZL_Network::DeInit() { }

ZL_HTTPCONNECTION_IMPL_INTERFACE
ZL_WEBSOCKETCONNECTION_IMPL_INTERFACE

#endif //ZL_USE_ENET

ZL_IMPL_OWNER_NONULLCON_IMPLEMENTATIONS(ZL_HttpConnection)
ZL_HttpConnection::ZL_HttpConnection() : impl(new ZL_HttpConnection_Impl()) { }
ZL_HttpConnection::ZL_HttpConnection(const char *url) : impl(new ZL_HttpConnection_Impl()) { SetURL(url); }
ZL_HttpConnection& ZL_HttpConnection::SetURL(const char *url) { impl->url = url; return *this; }
ZL_HttpConnection& ZL_HttpConnection::SetPostData(const char *data) { return SetPostData(data, strlen(data)); }
ZL_HttpConnection& ZL_HttpConnection::SetPostData(const void* data, size_t length) { impl->post_data.resize(length); memcpy(&impl->post_data[0], data, length); return *this; }
#ifndef ZL_NO_SOCKETS
ZL_HttpConnection& ZL_HttpConnection::SetTimeout(unsigned int timeout_msec) { impl->timeout_msec = timeout_msec; return *this; }
#endif
ZL_HttpConnection& ZL_HttpConnection::SetDoStreamData(bool DoStreamData) { impl->dostream = DoStreamData; return *this; }
void ZL_HttpConnection::Connect() const { if (impl->url.length()) impl->Connect(); }
ZL_Signal_v2<int, const ZL_String&>& ZL_HttpConnection::sigReceivedString() { return impl->sigReceivedString; }
ZL_Signal_v3<int, const char*, size_t>& ZL_HttpConnection::sigReceivedData() { return impl->sigReceivedData; }

ZL_IMPL_OWNER_NONULLCON_IMPLEMENTATIONS(ZL_WebSocketConnection)
ZL_WebSocketConnection::ZL_WebSocketConnection() : impl(new ZL_WebSocketConnection_Impl) { }
ZL_WebSocketConnection::ZL_WebSocketConnection(const char *url) : impl(new ZL_WebSocketConnection_Impl()) { SetURL(url); }
ZL_WebSocketConnection& ZL_WebSocketConnection::SetURL(const char *url) { impl->url = url; return *this; }
ZL_WebSocketConnection& ZL_WebSocketConnection::SendText(const char *data) { impl->SendText(data, strlen(data)); return *this; }
ZL_WebSocketConnection& ZL_WebSocketConnection::SendText(const char *data, size_t length) { impl->SendText(data, length); return *this; }
ZL_WebSocketConnection& ZL_WebSocketConnection::SendText(const ZL_String& str) { impl->SendText(str.c_str(), str.length()); return *this; }
ZL_WebSocketConnection& ZL_WebSocketConnection::SendBinary(const void* data, size_t length) { impl->SendBinary(data, length); return *this; }
void ZL_WebSocketConnection::Connect() const { if (impl->url.length()) impl->Connect(); }
void ZL_WebSocketConnection::Disconnect(unsigned short code, const char *reason, size_t reason_length) const { impl->Disconnect(code, reason, reason_length); }
bool ZL_WebSocketConnection::IsConnected() const { return impl->websocket_active; }
ZL_Signal_v1<const ZL_String&>& ZL_WebSocketConnection::sigReceivedText() { return impl->sigReceivedText; }
ZL_Signal_v2<const char*, size_t>& ZL_WebSocketConnection::sigReceivedBinary() { return impl->sigReceivedBinary; }
ZL_Signal_v0& ZL_WebSocketConnection::sigConnected() { return impl->sigConnected; }
ZL_Signal_v0& ZL_WebSocketConnection::sigDisconnected() { return impl->sigDisconnected; }
