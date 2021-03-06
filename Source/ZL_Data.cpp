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

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "ZL_Data.h"
#include "ZL_Application.h"
#include "ZL_Impl.h"

struct ZL_JSON_Impl : public ZL_Impl
{
	enum { PARSE_ERROR = -1, TYPE_PROXY = -2 };
	union { struct { char Type, KeyNeedFree, StringNeedFree, IsRoot; }; void* TypeAndFlags; };
	char* ObjectKey;
	union { char* DataString; std::vector<ZL_JSON_Impl*>* DataChildren; scalar DataNumber; ZL_JSON_Impl* DataProxy; };

	ZL_JSON_Impl() : TypeAndFlags(NULL), ObjectKey(NULL), DataChildren(NULL) { ZL_STATIC_ASSERT(ZL_Json::TYPE_NULL==0, null_type_must_be_zero_for_TypeAndFlags_init); }
	ZL_JSON_Impl(const char* src) { Construct(src, strlen(src)); }
	ZL_JSON_Impl(const ZL_String& src) { Construct(src.c_str(), src.length()); }
	~ZL_JSON_Impl() { KeepStringsIfReferenced(); ResetValue(); if (KeyNeedFree) { free(ObjectKey); } }
	static ZL_JSON_Impl* GetProxyTarget(ZL_JSON_Impl* impl) { while (impl && impl->Type == TYPE_PROXY) { impl = impl->DataProxy; } return impl; }

	ZL_JSON_Impl* AddChild()
	{
		ZL_JSON_Impl* Object = new ZL_JSON_Impl;
		assert(Type == ZL_Json::TYPE_ARRAY || Type == ZL_Json::TYPE_OBJECT);
		if (!DataChildren) DataChildren = new std::vector<ZL_JSON_Impl*>();
		DataChildren->push_back(Object);
		return DataChildren->back();
	}

	void ToString(ZL_String& out, bool pretty_print, int indent = 0)
	{
		if (Type == TYPE_PROXY) DataProxy->ToString(out, pretty_print, indent);
		else if (Type == ZL_Json::TYPE_OBJECT)
		{
			if (!DataChildren || DataChildren->empty()) { out << '{' << '}';  return; }
			bool pp = (pretty_print && DataChildren && DataChildren->size() > 0);
			ZL_String nltabs; if (pp) { nltabs << '\n'; for (int i = 0; i < indent; i++) nltabs << '\t'; }
			out << '{';
			for (std::vector<ZL_JSON_Impl*>::iterator it = DataChildren->begin(); it != DataChildren->end(); ++it)
				{ if (it != DataChildren->begin()) out << ','; if (pp) out << nltabs << '\t'; else out << ' '; AppendJsonString(out, (*it)->ObjectKey); out << " : "; (*it)->ToString(out, pretty_print, indent+pp); }
			if (pp) out << nltabs;
			out << '}';
		}
		else if (Type == ZL_Json::TYPE_ARRAY)
		{
			out << "[";
			if (DataChildren) for (std::vector<ZL_JSON_Impl*>::iterator it = DataChildren->begin(); it != DataChildren->end(); ++it)
				{ if (it != DataChildren->begin()) out << ','; out << ' '; (*it)->ToString(out, pretty_print, indent); }
			out << " ]";
		}
		else if (Type == ZL_Json::TYPE_STRING) AppendJsonString(out, DataString);
		else if (Type == ZL_Json::TYPE_NUMBER) out << DataNumber;
		else out << (Type == ZL_Json::TYPE_TRUE ? "true" : (Type == ZL_Json::TYPE_FALSE ? "false" : "null"));
	}

	bool CheckIfHasChild(ZL_JSON_Impl* other)
	{
		if (this == other) return true;
		if (Type == TYPE_PROXY) return DataProxy->CheckIfHasChild(other);
		else if ((Type == ZL_Json::TYPE_OBJECT || Type == ZL_Json::TYPE_ARRAY) && DataChildren)
			for (std::vector<ZL_JSON_Impl*>::iterator it = DataChildren->begin(); it != DataChildren->end(); ++it) if ((*it)->CheckIfHasChild(other)) return true;
		return false;
	}

	void ResetValue(char NewType = ZL_Json::TYPE_NULL)
	{
		if ((Type == ZL_Json::TYPE_OBJECT || Type == ZL_Json::TYPE_ARRAY) && DataChildren) DeleteChildren();
		else if (Type == TYPE_PROXY) DataProxy->DelRef();
		else if (Type == ZL_Json::TYPE_STRING && StringNeedFree) free(DataString);
		Type = NewType; DataChildren = NULL;
	}

private:
	void Construct(const char* src, size_t srclen)
	{
		if (!src || !*src) { Type = ZL_Json::TYPE_NULL; TypeAndFlags = NULL; ObjectKey = NULL; DataChildren = NULL; return; }
		Type = TYPE_PROXY, KeyNeedFree = true, IsRoot = true, ObjectKey = (char*)malloc(srclen + 1), DataProxy = new ZL_JSON_Impl;
		memcpy(ObjectKey, src, srclen + 1);
		char *end = ObjectKey + srclen, *parseend = DataProxy->Parse(EndOfWhitespace(ObjectKey), end);
		if (DataProxy->Type != PARSE_ERROR && EndOfWhitespace(parseend) == end) return; //success

		ZL_LOG2("JSON", "Parsing Error - Offset: %d - Error At: %.100s\n", parseend - ObjectKey, parseend);
		free(ObjectKey); ObjectKey = NULL; KeyNeedFree = false; ResetValue(); //cleanup
	}

	void KeepStringsIfReferenced(bool parent_is_referenced = false)
	{
		if (IsRoot) return; //is a referenced separately allocated tree
		if ((Type == ZL_Json::TYPE_OBJECT || Type == ZL_Json::TYPE_ARRAY) && DataChildren)
			for (std::vector<ZL_JSON_Impl*>::iterator it = DataChildren->begin(); it != DataChildren->end(); ++it)
				(*it)->KeepStringsIfReferenced(GetRefCount() > 1);
		if (!parent_is_referenced && GetRefCount() <= 1) return;
		if (ObjectKey && !KeyNeedFree)                       { KeyNeedFree = true;    char* src = ObjectKey;  size_t ln = strlen(src); memcpy((ObjectKey  = (char*)malloc(ln + 1)), src, ln + 1); }
		if (Type == ZL_Json::TYPE_STRING && !StringNeedFree) { StringNeedFree = true; char* src = DataString; size_t ln = strlen(src); memcpy((DataString = (char*)malloc(ln + 1)), src, ln + 1); }
	}

	void AppendJsonString(ZL_String& out, char* jstr)
	{
		for (out << '"'; *jstr; jstr++)
			if (*jstr == '\\' || *jstr == '"') out << '\\' << *jstr;
			else if (*jstr == '\b') out << '\\' <<  'b'; else if (*jstr == '\f') out << '\\' <<  'f';
			else if (*jstr == '\n') out << '\\' <<  'n'; else if (*jstr == '\r') out << '\\' <<  'r';
			else if (*jstr == '\t') out << '\\' <<  't'; else out << *jstr;
		out << '"';
	}
	char* Parse(char* p, char *end)
	{
		#define CLC(c) (c|0x60) //char to lower case
		if ((*p == 'N' || *p == 'n') && (end-p) >= 4 && CLC(p[1]) == 'u' && CLC(p[2]) == 'l' && CLC(p[3]) == 'l') { Type = ZL_Json::TYPE_NULL; return p + 4; }
		if ((*p == 'T' || *p == 't') && (end-p) >= 4 && CLC(p[1]) == 'r' && CLC(p[2]) == 'u' && CLC(p[3]) == 'e') { Type = ZL_Json::TYPE_TRUE; return p + 4; }
		if ((*p == 'F' || *p == 'f') && (end-p) >= 5 && CLC(p[1]) == 'a' && CLC(p[2]) == 'l' && CLC(p[3]) == 's' && CLC(p[4]) == 'e') { Type = ZL_Json::TYPE_FALSE; return p + 5; }
		#undef CLC
		if ((*p >= '0' && *p <= '9') || *p == '-')
		{
			Type = ZL_Json::TYPE_NUMBER;
			char *AfterNumber = NULL;
			DataNumber = (scalar)strtod(p, &AfterNumber);
			if (!AfterNumber || AfterNumber == p) { Type = PARSE_ERROR; return p; }
			return AfterNumber;
		}
		if (*p == '"')
		{
			DataString = ++p;
			char* trg = NULL;
			for (; p < end; p++)
			{
				if (*p == '\\')
				{
					if (!trg) trg = p;
					if (p[1] == '\\' || p[1] == '"' || p[1] == '/') *trg = p[1];
					else if (p[1] == 'b') *trg = '\b';
					else if (p[1] == 'f') *trg = '\f';
					else if (p[1] == 'n') *trg = '\n';
					else if (p[1] == 'r') *trg = '\r';
					else if (p[1] == 't') *trg = '\t';
					else break;
					p++;
					trg++;
				}
				else if (*p == '"') { *(trg ? trg : p) = '\0'; Type = ZL_Json::TYPE_STRING; return p+1; }
				else if (trg) *(trg++) = *p;
			}
			Type = PARSE_ERROR;
			return p;
		}
		ZL_JSON_Impl* Child;
		if (*p == '[')
		{
			Type = ZL_Json::TYPE_ARRAY;
			p = EndOfWhitespace(p+1);
			if (*p == ']') { Type = ZL_Json::TYPE_ARRAY; DataChildren = NULL; return p+1; }
			for (DataChildren = new std::vector<ZL_JSON_Impl*>(), Type = PARSE_ERROR; (Child = new ZL_JSON_Impl); p = EndOfWhitespace(p+1))
			{
				p = Child->Parse(p, end);
				if (Child->Type == PARSE_ERROR) break;
				DataChildren->push_back(Child);
				Child = NULL;

				p = EndOfWhitespace(p);
				if (*p == ']') { Type = ZL_Json::TYPE_ARRAY; break; }
				else if (*p != ',') break; //error
			}
			if (Type == PARSE_ERROR) { if (Child) delete Child; DeleteChildren(); return p; }
			return p+1;
		}
		if (*p == '{')
		{
			p = EndOfWhitespace(p+1);
			if (*p == '}') { Type = ZL_Json::TYPE_OBJECT; DataChildren = NULL; return p+1; }
			for (DataChildren = new std::vector<ZL_JSON_Impl*>(), Type = PARSE_ERROR; (Child = new ZL_JSON_Impl); p = EndOfWhitespace(p+1))
			{
				p = Child->Parse(p, end);
				if (Child->Type != ZL_Json::TYPE_STRING) break;
				Child->ObjectKey = Child->DataString;

				p = EndOfWhitespace(p);
				if (*p != ':') break;
				p = EndOfWhitespace(p+1);

				p = Child->Parse(p, end);
				if (Child->Type == PARSE_ERROR) break;
				DataChildren->push_back(Child);
				Child = NULL;

				p = EndOfWhitespace(p);
				if (*p == '}') { Type = ZL_Json::TYPE_OBJECT; break; }
				else if (*p != ',') break; //error
			}
			if (Type == PARSE_ERROR) { if (Child) delete Child; DeleteChildren(); return p; }
			return p+1;
		}
		Type = PARSE_ERROR;
		return p;
	}
	void DeleteChildren() { for (std::vector<ZL_JSON_Impl*>::iterator it = DataChildren->begin(); it != DataChildren->end(); ++it) (*it)->DelRef(); delete DataChildren; }
	char* EndOfWhitespace(char* p) { while (*p == ' ' || *p == '\n' || *p == '\r' || *p == '\t') p++; return p; }
	char* EndOfString(char* start, char* end) { for (char*p = start; p < end-1; p++) if (p[1] == '"' && (p[0] != '\\' || p == start)) return p+1; return NULL; }
};

ZL_IMPL_OWNER_NOASSIGNMENT_IMPLEMENTATIONS(ZL_Json)
ZL_Json::ZL_Json(const char *json) : impl(new ZL_JSON_Impl(json)) {}
ZL_Json::ZL_Json(const ZL_String &json) : impl(new ZL_JSON_Impl(json)) {}
ZL_Json::ZL_Json(const ZL_File &file) : impl(new ZL_JSON_Impl(file.GetContents())) {}
ZL_Json::ZL_Json(ZL_JSON_Impl *fromimpl) : impl(fromimpl) { impl->AddRef(); }

ZL_Json::eType ZL_Json::GetType() const
{
	ZL_JSON_Impl *imp = ZL_JSON_Impl::GetProxyTarget(impl);
	return (imp ? (ZL_Json::eType)imp->Type : TYPE_NULL);
}

size_t ZL_Json::Size() const
{
	ZL_JSON_Impl *imp = ZL_JSON_Impl::GetProxyTarget(impl);
	return (imp && imp->DataChildren && (imp->Type == TYPE_OBJECT || imp->Type == TYPE_ARRAY) ? imp->DataChildren->size() : 0);
}

ZL_Json::Iterator ZL_Json::GetIterator() const
{
	ZL_JSON_Impl *imp = ZL_JSON_Impl::GetProxyTarget(impl);
	ZL_Json::Iterator ret;
	if (!imp || !imp->DataChildren || imp->DataChildren->empty() || (imp->Type != TYPE_OBJECT && imp->Type != TYPE_ARRAY)) memset(&ret, 0, sizeof(ret));
	else { ret.cursor = ret.begin = &*imp->DataChildren->begin(); ret.end = ret.cursor + imp->DataChildren->size(); }
	return ret;
}

std::vector<ZL_Json> ZL_Json::GetChildren() const
{
	std::vector<ZL_Json> ret;
	ZL_JSON_Impl *imp = ZL_JSON_Impl::GetProxyTarget(impl);
	if (imp && imp->DataChildren && (imp->Type == TYPE_OBJECT || imp->Type == TYPE_ARRAY))
		for (std::vector<ZL_JSON_Impl*>::iterator it = imp->DataChildren->begin(); it != imp->DataChildren->end(); ++it)
			ret.push_back(ZL_Json(*it));
	return ret;
}

ZL_Json ZL_Json::GetChild(size_t index) const
{
	ZL_JSON_Impl *imp = ZL_JSON_Impl::GetProxyTarget(impl);
	if (!imp || (imp->Type != TYPE_OBJECT && imp->Type != TYPE_ARRAY) || !imp->DataChildren || index >= imp->DataChildren->size()) return ZL_Json();
	return ZL_Json(imp->DataChildren->operator[](index));
}

ZL_Json ZL_Json::GetByKey(const char* key) const
{
	ZL_JSON_Impl *imp = ZL_JSON_Impl::GetProxyTarget(impl);
	if (imp && imp->DataChildren && imp->Type == TYPE_OBJECT && key)
		for (std::vector<ZL_JSON_Impl*>::iterator it = imp->DataChildren->begin(); it != imp->DataChildren->end(); ++it)
			if (!strcmp((*it)->ObjectKey, key)) return ZL_Json(*it);
	return ZL_Json();
}

const char* ZL_Json::GetString() const
{
	ZL_JSON_Impl *imp = ZL_JSON_Impl::GetProxyTarget(impl);
	return (imp && imp->Type == TYPE_STRING ? imp->DataString : NULL);
}

scalar ZL_Json::GetFloat() const
{
	ZL_JSON_Impl *imp = ZL_JSON_Impl::GetProxyTarget(impl);
	return (imp && imp->Type == TYPE_NUMBER ? (scalar)imp->DataNumber : 0.0f);
}

int ZL_Json::GetInt() const
{
	ZL_JSON_Impl *imp = ZL_JSON_Impl::GetProxyTarget(impl);
	return (imp && imp->Type == TYPE_NUMBER ? (int)imp->DataNumber : 0);
}

bool ZL_Json::GetBool() const
{
	ZL_JSON_Impl *imp = ZL_JSON_Impl::GetProxyTarget(impl);
	return (imp && imp->Type == TYPE_TRUE);
}

bool ZL_Json::IsNull() const
{
	ZL_JSON_Impl *imp = ZL_JSON_Impl::GetProxyTarget(impl);
	return (!imp || imp->Type == TYPE_NULL);
}

bool ZL_Json::HasKey(const char* key) const
{
	ZL_JSON_Impl *imp = ZL_JSON_Impl::GetProxyTarget(impl);
	if (imp && imp->DataChildren && imp->Type == TYPE_OBJECT && key)
		for (std::vector<ZL_JSON_Impl*>::iterator it = imp->DataChildren->begin(); it != imp->DataChildren->end(); ++it)
			if (!strcmp((*it)->ObjectKey, key)) return true;
	return false;
}

const char* ZL_Json::GetKey() const
{
	return (impl ? impl->ObjectKey : NULL);
}

ZL_Json ZL_Json::operator[](const char* key)
{
	ZL_JSON_Impl *imp = (impl ? ZL_JSON_Impl::GetProxyTarget(impl) : (impl = new ZL_JSON_Impl));
	if (imp->Type == TYPE_NULL) imp->ResetValue(TYPE_OBJECT);
	if (imp->Type != TYPE_OBJECT || !key) return ZL_Json();
	if (imp->DataChildren)
		for (std::vector<ZL_JSON_Impl*>::iterator it = imp->DataChildren->begin(); it != imp->DataChildren->end(); ++it)
			if (!strcmp((*it)->ObjectKey, key)) return ZL_Json(*it);
	ZL_JSON_Impl* child = imp->AddChild();
	child->KeyNeedFree = true;
	size_t len = strlen(key);
	memcpy((child->ObjectKey = (char*)malloc(len + 1)), key, len + 1);
	return ZL_Json(child);
}

ZL_Json ZL_Json::Add()
{
	ZL_JSON_Impl *imp = (impl ? ZL_JSON_Impl::GetProxyTarget(impl) : (impl = new ZL_JSON_Impl));
	if (imp->Type == TYPE_NULL) imp->ResetValue(TYPE_ARRAY);
	if (imp->Type != TYPE_ARRAY) return ZL_Json();
	return ZL_Json(imp->AddChild());
}

void ZL_Json::SetString(const char* NewString)
{
	if (!NewString) { SetNull(); return; }
	ZL_JSON_Impl *imp = (impl ? impl : (impl = new ZL_JSON_Impl));
	imp->ResetValue(TYPE_STRING);
	imp->StringNeedFree = true;
	size_t len = strlen(NewString);
	memcpy((imp->DataString = (char*)malloc(len + 1)), NewString, len + 1);
}

void ZL_Json::SetFloat(scalar NewFloat)
{
	ZL_JSON_Impl *imp = (impl ? impl : (impl = new ZL_JSON_Impl));
	imp->ResetValue(TYPE_NUMBER);
	imp->DataNumber = NewFloat;
}

void ZL_Json::SetInt(int NewInt)
{
	ZL_JSON_Impl *imp = (impl ? impl : (impl = new ZL_JSON_Impl));
	imp->ResetValue(TYPE_NUMBER);
	imp->DataNumber = (scalar)NewInt;
}

void ZL_Json::SetBool(bool NewBool)
{
	(impl ? impl : (impl = new ZL_JSON_Impl))->ResetValue(NewBool ? TYPE_TRUE : TYPE_FALSE);
}

void ZL_Json::SetNull()
{
	(impl ? impl : (impl = new ZL_JSON_Impl))->ResetValue();
}

void ZL_Json::SetReference(const ZL_Json &source)
{
	if (!impl || impl->IsRoot) { ZL_Impl::CopyRef(source.impl, (ZL_Impl*&)impl); return; }
	if (source.impl->CheckIfHasChild(impl)) { SetNull(); return; }
	impl->ResetValue(ZL_JSON_Impl::TYPE_PROXY);
	impl->DataProxy = source.impl;
	impl->DataProxy->AddRef();
}

void ZL_Json::SetKey(const char* NewKey)
{
	if (!impl || impl->IsRoot) return;
	if (impl->KeyNeedFree) free(impl->ObjectKey);
	impl->KeyNeedFree = true;
	size_t len = strlen(NewKey);
	memcpy((impl->ObjectKey = (char*)malloc(len + 1)), NewKey, len + 1);
}

ZL_Json::Iterator ZL_Json::Erase(Iterator it)
{
	ZL_JSON_Impl *imp = ZL_JSON_Impl::GetProxyTarget(impl);
	if (!imp || !imp->DataChildren || imp->DataChildren->empty() || (imp->Type != TYPE_OBJECT && imp->Type != TYPE_ARRAY) ||
		it.cursor < &*imp->DataChildren->begin() || it.cursor >= &*imp->DataChildren->begin()+imp->DataChildren->size()) { memset(&it, 0, sizeof(it)); return it; }
	(*(it.cursor))->DelRef();
	imp->DataChildren->erase(imp->DataChildren->begin() + (it.cursor - it.begin));
	it.end--;
	return it;
}

bool ZL_Json::Erase(const ZL_Json &child)
{
	ZL_JSON_Impl *imp = ZL_JSON_Impl::GetProxyTarget(impl), *childimpl = child.impl, *childimp = ZL_JSON_Impl::GetProxyTarget(childimpl);
	if (imp && imp->DataChildren && (imp->Type == TYPE_OBJECT || imp->Type == TYPE_ARRAY))
		for (std::vector<ZL_JSON_Impl*>::iterator it = imp->DataChildren->begin(); it != imp->DataChildren->end(); ++it)
			if (*it == childimpl || *it == childimp) { (*it)->DelRef(); imp->DataChildren->erase(it); return true; }
	return false;
}

bool ZL_Json::Erase(const char* key)
{
	ZL_JSON_Impl *imp = ZL_JSON_Impl::GetProxyTarget(impl);
	if (imp && imp->DataChildren && (imp->Type == TYPE_OBJECT || imp->Type == TYPE_ARRAY) && key)
		for (std::vector<ZL_JSON_Impl*>::iterator it = imp->DataChildren->begin(); it != imp->DataChildren->end(); ++it)
			if ((*it)->ObjectKey && !strcmp((*it)->ObjectKey, key)) { (*it)->DelRef(); imp->DataChildren->erase(it); return true; }
	return false;
}

bool ZL_Json::EraseAt(size_t index)
{
	ZL_JSON_Impl *imp = ZL_JSON_Impl::GetProxyTarget(impl);
	if (imp && imp->DataChildren && (imp->Type == TYPE_OBJECT || imp->Type == TYPE_ARRAY) && index < imp->DataChildren->size())
		{ (*imp->DataChildren->operator[](index)).DelRef(); imp->DataChildren->erase(imp->DataChildren->begin() + index); return true; }
	return false;
}

void ZL_Json::Clear()
{
	ZL_JSON_Impl *imp = ZL_JSON_Impl::GetProxyTarget(impl);
	if (imp && (imp->Type == TYPE_OBJECT || imp->Type == TYPE_ARRAY)) imp->ResetValue(imp->Type);
}

ZL_String ZL_Json::ToString(bool pretty_print) const
{
	ZL_String ret;
	if (impl) impl->ToString(ret, pretty_print);
	return ret;
}

struct ZL_Xml_Impl : public ZL_Impl
{
	const ZL_Xml_Impl *parent;
	std::vector<ZL_Xml_Impl*> children;
	std::map<ZL_String, ZL_String> parameters;
	ZL_String name, text;

	ZL_Xml_Impl(const ZL_Xml_Impl *parent) : parent(parent) { }

	ZL_Xml_Impl(const ZL_Xml_Impl *parent, const ZL_String &name) : parent(parent), name(name) { }

	~ZL_Xml_Impl()
	{
		for (std::vector<ZL_Xml_Impl*>::iterator itc = children.begin(); itc != children.end(); ++itc)
			(*itc)->DelRef();
	}

	bool hasparent(const ZL_Xml_Impl *impl)
	{
		if (!impl || !parent) return false;
		for (const ZL_Xml_Impl *p = parent; p; p = p->parent) if (p == impl) return true;
		return false;
	}

	ZL_Xml_Impl(const ZL_Xml_Impl *parent, ZL_Xml_Impl* clone) : parent(parent)
	{
		if (!clone) return;
		name = clone->name;
		text = clone->text;
		parameters = clone->parameters;
		for (std::vector<ZL_Xml_Impl*>::iterator itc = clone->children.begin(); itc != clone->children.end(); ++itc)
			children.push_back(new ZL_Xml_Impl(this, *itc));
	}

	ZL_Xml_Impl(const ZL_Xml_Impl *parent, const ZL_String &xml, const ZL_String::size_type from, const ZL_String::size_type to) : parent(parent)
	{
		// cursor, open tag <, close tag >, name start, name end, parameter start, parameter end, equal sign, valua start, value end, child from
		ZL_String::size_type cur = from, op, cl, nen = 0, pst = 0, pen = 0, veq = 0, vst = 0, cfrom = 0;
		const ZL_String::chr *p, *pxml = xml.c_str();
		bool terminated = false, closing;

		xml_next_tag:
		op = xml.find("<", cur);
		if (op == ZL_String::npos || op > to) return;
		cur = op + 1;
		p = &pxml[cur];

		if (from == 0)
		{
			if      (to-cur >= 4 &&*p=='!' &&*(p+1)=='-'&&*(p+2)=='-' ) { for (; cur < to; cur++, p++) if (*p=='>'&&*(p-2)=='-'&&*(p-1)=='-') goto xml_next_tag; }
			else if (to-cur >= 2 &&*p=='?')                             { for (; cur < to; cur++, p++) if (*p=='>'&&*(p-1)=='?')              goto xml_next_tag; }
		}

		cl = xml.find(">", cur);
		if (cl == ZL_String::npos || cl > to || cl < op + 2) return;

		#define ADD_PARAM_VAL  { parameters[xml.substr(pst, pen - pst)] = xml.substr(vst, cur - vst); pst = cur+1; vst = veq = pen = 0; }
		#define ADD_PARAM_FLAG { parameters[xml.substr(pst, pen - pst)] = ZL_String(); pst = cur; pen = 0; }

		for (; cur <= cl; cur++, p++)
		{
			if (*p==' '||*p=='='||*p=='"'||*p=='\''||*p=='/'||*p=='\t'||*p=='\r'||*p=='\n'||*p=='<'||*p=='>')
			{
				if (nen == 0) { nen = cur; name = xml.substr(op+1, cur - op - 1); }
				if (vst == 0 && !terminated && *p == '/') terminated = true;
				if (pst == 0 || pst == cur) { pst = cur+1; continue; }
				if (terminated) terminated = false;
				if (pen == 0) { pen = cur; }
				if (veq == 0) { if (*p == '=') { veq = cur; vst = cur+1; } continue; }
				if (vst == 0 || (vst == cur && pxml[vst-1]!='"' && pxml[vst-1]!='\'')) { vst = cur+1; continue; }
				if (pxml[vst-1]=='"'||pxml[vst-1]=='\'')
				{
					if (*p=='>') { if ((cl = xml.find(">", cur+1)) == ZL_String::npos || cl > to || cl < cur + 2) break; }
					else if (pxml[vst-1]==*p) ADD_PARAM_VAL
				}
				else ADD_PARAM_VAL
			}
			else if (pen && veq == 0) ADD_PARAM_FLAG
			else if (terminated) terminated = false;
		}
		if (pen && veq == 0) ADD_PARAM_FLAG

		for (std::map<ZL_String, ZL_String>::iterator itp = parameters.begin(); itp != parameters.end(); ++itp)
			ConvertFromEntities(itp->second);

		if (terminated) return;

		op = cl = 0; terminated = closing = false;

		int level = 0;
		for (; cur < to; cur++, p++)
		{
			if (op == 0)
			{
				if      (*p == '<' && to-cur >= 4 &&*(p+1)=='!' &&*(p+2)=='-'&&*(p+3)=='-' )            { for (            ; cur < to; cur++, p++) if (*p=='>'&&*(p-2)=='-'&&*(p-1)=='-') break; }
				else if (*p == '<' && to-cur >= 2 &&*(p+1)=='?')                                        { for (            ; cur < to; cur++, p++) if (*p=='>'&&*(p-1)=='?')              break; }
				else if (*p == '<' && to-cur >= 9 &&*(p+1)=='!' &&*(p+2)=='['&&!memcmp(p+3,"CDATA[",6)) { for (op = cur + 9; cur < to; cur++, p++) if (*p=='>'&&*(p-2)==']'&&*(p-1)==']') { if (level == 0) text += ZL_String::str_replace(xml.substr(op, cur-2-op), "&", "&amp;"); op = 0; break; } }
				else if (*p == '<') { if (level++ == 0) { cfrom = cur; } op = cur; cl = 0; }
				else if (level == 0 && (text.length() || *p > ' ')) { text += *p; }
			}
			else
			{
				if (*p=='/') { if (cl == 0) { level--; closing = true; } terminated = true; }
				else if (*p > ' ') { cl = cur; if (terminated && *p!='>') terminated = false; }

				if      (vst == 0 && (*p=='"'||*p=='\'')) { vst = cur+1; }
				else if (vst != 0 && pxml[vst-1]==*p) { vst = 0; }
				else if (vst == 0 && *p=='>')
				{
					if ((closing || terminated) && (level == 0)) break;
					if ((closing || terminated) && (--level == 0)) children.push_back(new ZL_Xml_Impl(this, xml, cfrom, cur+1));
					op = cl = 0; terminated = closing = false;
				}
			}
		}

		while (text.length()) { if (*text.rbegin()<=' ') text.erase(text.end()-1); else break; }
		ConvertFromEntities(text);
	}

	void ToString(ZL_String &xml, int iteration_level = 0)
	{
		ZL_String vtmp;
		if (iteration_level) xml += ZL_String('\t') * iteration_level;
		//xml << '<' << (int)this << '_' << iRefCount << '_' << name;
		xml << '<' << name;

		for (std::map<ZL_String, ZL_String>::iterator itp = parameters.begin(); itp != parameters.end(); ++itp)
			xml << ' ' << itp->first << "=\"" << ConvertToEntities(itp->second, vtmp) << '"';

		xml += (children.size() || text.size() ? ">\n" : (parameters.size() ? " />\n" : "/>\n"));

		if (text.size()) xml << ZL_String('\t') * (iteration_level+1) << ConvertToEntities(text, vtmp) << '\n';

		for (std::vector<ZL_Xml_Impl*>::iterator itc = children.begin(); itc != children.end(); ++itc)
			(*itc)->ToString(xml, iteration_level + 1);

		if (children.size() || text.size()) xml << ZL_String('\t') * iteration_level << "</" << name << ">\n";
	}

	static bool CompareLowerCase(const char* pCharsAnyCase, const char* pCompareValLower, int pCompareValLen)
	{
		while (pCompareValLen--) if (tolower(*pCharsAnyCase++) != *pCompareValLower++) return false;
		return true;
	}

	static void ConvertFromEntities(ZL_String &v)
	{
		for (ZL_String::size_type p = 0; p < v.size(); p++)
			if (v[p] != '&') { }
			else if (v[p+3] == ';' && CompareLowerCase(&v[p+1], "lt",   2)) v.replace(p, 4, "<");
			else if (v[p+3] == ';' && CompareLowerCase(&v[p+1], "gt",   2)) v.replace(p, 4, ">");
			else if (v[p+5] == ';' && CompareLowerCase(&v[p+1], "quot", 4)) v.replace(p, 6, "\"");
			else if (v[p+5] == ';' && CompareLowerCase(&v[p+1], "apos", 4)) v.replace(p, 6, "\'");
			else if (v[p+4] == ';' && CompareLowerCase(&v[p+1], "amp",  3)) v.replace(p, 5, "&");
	}

	static ZL_String & ConvertToEntities(const ZL_String &vsource, ZL_String &v)
	{
		v = vsource; for (ZL_String::size_type p = 0; p < v.size(); p++)
			if      (v[p] == '<')  { v.replace(p, 1, "&lt;");   p += 3; }
			else if (v[p] == '>')  { v.replace(p, 1, "&gt;");   p += 3; }
			else if (v[p] == '"')  { v.replace(p, 1, "&quot;"); p += 5; }
			else if (v[p] == '\'') { v.replace(p, 1, "&apos;"); p += 5; }
			else if (v[p] == '&')  { v.replace(p, 1, "&amp;");  p += 4; }
		return v;
	}
};

ZL_IMPL_OWNER_DEFAULT_IMPLEMENTATIONS(ZL_Xml)
ZL_Xml::ZL_Xml(const ZL_File &file) : impl(new ZL_Xml_Impl(NULL, file.GetContents(), 0, file.Size())) { }
ZL_Xml::ZL_Xml(const ZL_String &xml) : impl(new ZL_Xml_Impl(NULL, xml, 0, xml.size())) { }
ZL_Xml::ZL_Xml(ZL_Xml_Impl *fromimpl) : impl(fromimpl) { impl->AddRef(); }

ZL_Xml ZL_Xml::AddChild(ZL_Xml &Child)
{
	if (!impl) impl = new ZL_Xml_Impl(NULL);
	//else if (impl == Child.impl || impl->hasparent(Child.impl)) return ZL_Xml();
	ZL_Xml_Impl *newchild = new ZL_Xml_Impl(impl, Child.impl);
	impl->children.push_back(newchild);
	return ZL_Xml(newchild);
}

void ZL_Xml::AddChild(ZL_Xml *pChild)
{
	if (!impl) impl = new ZL_Xml_Impl(NULL);
	if (!pChild || impl->hasparent(pChild->impl)) return;
	ZL_Xml_Impl *newchild;
	if (!pChild->impl) { newchild = pChild->impl = new ZL_Xml_Impl(impl); newchild->AddRef(); }
	else if (pChild->impl->parent == NULL) { newchild = pChild->impl; newchild->parent = impl; newchild->AddRef(); }
	else newchild = new ZL_Xml_Impl(impl, pChild->impl);
	impl->children.push_back(newchild);
}

ZL_Xml ZL_Xml::AddChild(const ZL_String& name)
{
	if (!impl) impl = new ZL_Xml_Impl(NULL);
	ZL_Xml_Impl *newchild = new ZL_Xml_Impl(impl, name);
	impl->children.push_back(newchild);
	return ZL_Xml(newchild);
}

void ZL_Xml::RemoveChild(const ZL_Xml *pChild)
{
	if (!impl || !pChild->impl) return;
	for (std::vector<ZL_Xml_Impl*>::iterator itc = impl->children.begin(); itc != impl->children.end(); ++itc)
		if ((*itc) == pChild->impl) { (*itc)->DelRef(); impl->children.erase(itc); return; }
}

void ZL_Xml::RemoveChildren(const ZL_String& name)
{
	if (!impl) return;
	for (std::vector<ZL_Xml_Impl*>::iterator itc = impl->children.begin(); itc != impl->children.end();)
		if ((*itc)->name == name) { (*itc)->DelRef(); itc = impl->children.erase(itc); return; } else ++itc;
}

ZL_String ZL_Xml::ToString() const
{
	ZL_String s;
	if (impl) impl->ToString(s, 0);
	return s;
}

const ZL_String& ZL_Xml::GetName() const
{
	if (impl) return impl->name;
	return ZL_String::EmptyString;
}

void ZL_Xml::SetName(const ZL_String& newname)
{
	if (!impl) impl = new ZL_Xml_Impl(NULL);
	impl->name = newname;
}

const ZL_String& ZL_Xml::GetText() const
{
	if (impl) return impl->text;
	return ZL_String::EmptyString;
}

void ZL_Xml::SetText(const ZL_String& newtext)
{
	if (!impl) impl = new ZL_Xml_Impl(NULL);
	impl->text = newtext;
}

void ZL_Xml::SetParameter(const ZL_String &name, const ZL_String &paramval)
{
	if (!impl) impl = new ZL_Xml_Impl(NULL);
	impl->parameters[name] = paramval;
}

void ZL_Xml::RemoveParameter(const ZL_String &name)
{
	if (!impl) return;
	impl->parameters.erase(name);
}

bool ZL_Xml::HasParameter(const ZL_String &name) const
{
	if (!impl) return false;
	return (impl->parameters.count(name) ? true : false);
}

std::map<ZL_String, ZL_String> &ZL_Xml::GetParameters()
{
	if (!impl) impl = new ZL_Xml_Impl(NULL);
	return impl->parameters;
}

std::vector<ZL_Xml> ZL_Xml::GetChildren() const
{
	std::vector<ZL_Xml> children;
	if (!impl) return children;
	for (std::vector<ZL_Xml_Impl*>::iterator itc = impl->children.begin(); itc != impl->children.end(); ++itc)
		children.push_back(ZL_Xml(*itc));
	return children;
}

std::vector<ZL_Xml> ZL_Xml::GetChildrenByName(const ZL_String &name) const
{
	std::vector<ZL_Xml> children;
	if (!impl) return children;
	for (std::vector<ZL_Xml_Impl*>::iterator itc = impl->children.begin(); itc != impl->children.end(); ++itc)
		if ((*itc)->name == name) children.push_back(ZL_Xml(*itc));
	return children;
}

ZL_String& ZL_Xml::operator[](const ZL_String &paramname) const
{
	if (!impl) return ZL_String::EmptyString;
	std::map<ZL_String, ZL_String>::iterator it = impl->parameters.find(paramname);
	if (it == impl->parameters.end()) return ZL_String::EmptyString;
	return it->second;
}

std::allocator<ZL_String>::reference ZL_Xml::operator[](const ZL_String &paramname)
{
	if (!impl) impl = new ZL_Xml_Impl(NULL);
	return impl->parameters[paramname];
}

ZL_Xml ZL_Xml::operator()(const ZL_String &childname)
{
	if (!impl) impl = new ZL_Xml_Impl(NULL);
	for (std::vector<ZL_Xml_Impl*>::iterator itc = impl->children.begin(); itc != impl->children.end(); ++itc)
		if ((*itc)->name == childname) return ZL_Xml(*itc);
	return ZL_Xml();
}

static const char base64chars[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
static const char base64pad = '=';

ZL_String ZL_Base64::Encode(const void* InputBuffer, size_t BufSize)
{
	if (BufSize == 0) return "";
	unsigned int n = 0;
	unsigned char n0, n1, n2, n3;
	std::stringstream res;
	const unsigned char *p = (const unsigned char*)InputBuffer;
	for (size_t x = 0; x < BufSize; x += 3)
	{
		n = p[x] << 16;
		if ((x + 1) < BufSize) n += p[x + 1] << 8;
		if ((x + 2) < BufSize) n += p[x + 2];
		n0 = (unsigned char)(n >> 18) & 63;
		n1 = (unsigned char)(n >> 12) & 63;
		n2 = (unsigned char)(n >> 6) & 63;
		n3 = (unsigned char)n & 63;
		res << base64chars[n0];
		res << base64chars[n1];
		if ((x + 1) < BufSize) res << base64chars[n2];
		if ((x + 2) < BufSize) res << base64chars[n3];
	}
	for (size_t pad_count = 0; pad_count < res.str().size() % 4; pad_count++) res << base64pad;
	return res.str();
}

size_t ZL_Base64::Decode(const ZL_String& Base64Data, std::vector<unsigned char> &output)
{
	unsigned char DecodeMatrix[256];
	memset(DecodeMatrix, 0, 256);
	for (unsigned char im = 0; im <= 63; im++) DecodeMatrix[(int)base64chars[(int)im]] = im;
	size_t iBuf = 0, total = Base64Data.size();
	output = std::vector<unsigned char>(DecodedMaxSize(total));
	unsigned char* pOutput = &output[0];
	unsigned char* pText = (unsigned char*)Base64Data.c_str();
	for (size_t i = 0, left = total; i < total; left -= 4, i += 4)
	{
		pOutput[iBuf] = DecodeMatrix[pText[i]] << 2;
		if (left == 1 || pText[i + 1] == base64pad) break;
		pOutput[iBuf++] += (DecodeMatrix[pText[i + 1]] >> 4) & 0x3;
		pOutput[iBuf] = DecodeMatrix[pText[i + 1]] << 4;
		if (left == 2 || pText[i + 2] == base64pad) break;
		pOutput[iBuf++] += (DecodeMatrix[pText[i + 2]] >> 2) & 0xf;
		pOutput[iBuf] = DecodeMatrix[pText[i + 2]] << 6;
		if (left == 3 || pText[i + 3] == base64pad) break;
		pOutput[iBuf++] += DecodeMatrix[pText[i + 3]];
	}
	if (iBuf != output.size()) output.erase(output.begin()+iBuf, output.end());
	return iBuf;
}

size_t ZL_Base64::Decode(const ZL_String& Base64Data, void* OutputBuffer, size_t BufSize)
{
	unsigned char DecodeMatrix[256];
	memset(DecodeMatrix, 0, 256);
	for (unsigned char im = 0; im <= 63; im++) DecodeMatrix[(int)base64chars[(int)im]] = im;
	size_t iBuf = 0, total = Base64Data.size();
	unsigned char* pOutput = (unsigned char*)OutputBuffer;
	unsigned char* pText = (unsigned char*)Base64Data.c_str();
	for (size_t i = 0, left = total; i < total && iBuf < BufSize; left -= 4, i += 4)
	{
		pOutput[iBuf] = DecodeMatrix[pText[i]] << 2;
		if (left == 1 || pText[i + 1] == base64pad) break;
		pOutput[iBuf++] += (DecodeMatrix[pText[i + 1]] >> 4) & 0x3;
		if (iBuf == BufSize) break;
		pOutput[iBuf] = DecodeMatrix[pText[i + 1]] << 4;
		if (left == 2 || pText[i + 2] == base64pad) break;
		pOutput[iBuf++] += (DecodeMatrix[pText[i + 2]] >> 2) & 0xf;
		if (iBuf == BufSize) break;
		pOutput[iBuf] = DecodeMatrix[pText[i + 2]] << 6;
		if (left == 3 || pText[i + 3] == base64pad) break;
		pOutput[iBuf++] += DecodeMatrix[pText[i + 3]];
	}
	return iBuf;
}

size_t ZL_Base64::DecodedMaxSize(size_t EncodedSize)
{
	return (EncodedSize + 3) / 4 * 3;
}

bool ZL_Base64::IsBase64(const ZL_String& Base64Data)
{
	unsigned char TestMatrix[256], *pText = (unsigned char*)Base64Data.c_str();
	memset(TestMatrix, 65, 256);
	for (unsigned char im = 0; im <= 63; im++) TestMatrix[(int)base64chars[(int)im]] = im;
	TestMatrix[(int)base64pad] = 64; //Padding character for test
	for (size_t i = 0; i < Base64Data.size(); i++)
	{
		if (TestMatrix[pText[i]] == 65) return false; //bad char
		if ((TestMatrix[pText[i]] == 64) && (i < Base64Data.size() - 3)) return false; //bad padding
	}
	return true;
}

#include "zlib/zlib.h"
unsigned int ZL_Checksum::CRC32(const void* Data, size_t DataSize)
{
	return crc32(0, (unsigned char*)Data, DataSize);
	//if (!Data) return 0;
	//unsigned int crcu32 = ~(unsigned int)0;
	//unsigned char b, *p = (unsigned char*)Data;
	//// Karl Malbrain's compact CRC-32. See "A compact CCITT crc16 and crc32 C implementation that balances processor cache usage against speed": http://www.geocities.com/malbrain/
	//static const unsigned int s_crc32[16] = { 0, 0x1db71064, 0x3b6e20c8, 0x26d930ac, 0x76dc4190, 0x6b6b51f4, 0x4db26158, 0x5005713c, 0xedb88320, 0xf00f9344, 0xd6d6a3e8, 0xcb61b38c, 0x9b64c2b0, 0x86d3d2d4, 0xa00ae278, 0xbdbdf21c };
	//while (DataSize--) { b = *p++; crcu32 = (crcu32 >> 4) ^ s_crc32[(crcu32 & 0xF) ^ (b & 0xF)]; crcu32 = (crcu32 >> 4) ^ s_crc32[(crcu32 & 0xF) ^ (b >> 4)]; }
	//return ~crcu32;
}

unsigned int ZL_Checksum::Fast(const void* Data, size_t DataSize)
{
	unsigned int res = 0, *p = (unsigned int*)Data, *pMax = (unsigned int*)((char*)p + DataSize - 3);
	while (p < pMax) res = res*65599 + *(p++);
	switch (DataSize & 4) { case 0: return res; case 1: return res*65599 + *(char*)p; case 2: return res*65599 + *(short*)p; default: return res*65599 + ((((char*)p)[0]<<16)|(((char*)p)[1]<<8)|((char*)p)[2]); }
}

unsigned int ZL_Checksum::Fast4(const void* Data, size_t DataSize)
{
	unsigned int res = 0, *p = (unsigned int*)Data, *pMax = (unsigned int*)((char*)p + DataSize);
	while (p != pMax) res = res*65599 + *(p++);
	return res;
}
