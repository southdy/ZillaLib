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

#ifndef __ZL_APPLICATION__
#define __ZL_APPLICATION__

#include "ZL_Math.h"
#include "ZL_String.h"
#include "ZL_Signal.h"

#if (defined(_MSC_VER) && !defined(WINAPI_FAMILY))
#pragma comment (lib, "kernel32.lib")
#pragma comment (lib, "user32.lib")
#pragma comment (lib, "gdi32.lib")
#pragma comment (lib, "winmm.lib")
#pragma comment (lib, "shell32.lib")
#if(_MSC_VER < 1300)
#pragma comment (linker, "/ignore:4089")
#endif
#endif

//Find out if we're running on a mobile platform (define __SMARTPHONE__) or on the web (define __WEBAPP__)
#ifndef __SMARTPHONE__
 #if defined(ANDROID)
  #ifndef __ANDROID__
   #define __ANDROID__
  #endif
  #define __SMARTPHONE__
 #elif defined(WINAPI_FAMILY) && WINAPI_FAMILY==WINAPI_FAMILY_PHONE_APP
  #define __WINDOWSPHONE__
  #define __SMARTPHONE__
 #elif defined(__APPLE__)
  #include "AvailabilityMacros.h"
  #ifdef MAC_OS_X_VERSION_10_3
   #include "TargetConditionals.h"
   #if TARGET_OS_IPHONE
    #define __IPHONEOS__
    #define __SMARTPHONE__
   #endif
  #endif
 #endif
#endif
#ifndef __WEBAPP__
 #if defined(__native_client__)
  #define __NACL__
  #define __WEBAPP__
 #elif defined(__EMSCRIPTEN__)
  #define __WEBAPP__
 #endif
#endif

//Logging functions that only do something on debug builds and are empty on release builds
#if defined(ZILLALOG)
#define ZL_LOG0(tag,fmt) do { ZL_Application::Log(tag, fmt); } while (false)
#define ZL_LOG1(tag,fmt,a) do { ZL_Application::Log(tag, fmt, a); } while (false)
#define ZL_LOG2(tag,fmt,a,b) do { ZL_Application::Log(tag, fmt, a, b); } while (false)
#define ZL_LOG3(tag,fmt,a,b,c) do { ZL_Application::Log(tag, fmt, a, b, c); } while (false)
#define ZL_LOG4(tag,fmt,a,b,c,d) do { ZL_Application::Log(tag, fmt, a, b, c,d); } while (false)
#if (!defined(_MSC_VER) || (_MSC_VER >= 1400))
#define ZL_LOG(tag,fmt, ...) do { ZL_Application::Log(tag, fmt, ##__VA_ARGS__); } while (false)
#endif
#else
#define ZL_LOG0(tag,f) ((void)0)
#define ZL_LOG1(tag,f,a) ((void)0)
#define ZL_LOG2(tag,f,a,b) ((void)0)
#define ZL_LOG3(tag,f,a,b,c) ((void)0)
#define ZL_LOG4(tag,f,a,b,c,d) ((void)0)
#if (!defined(_MSC_VER) || (_MSC_VER > 1200))
#define ZL_LOG(tag,fmt, ...) ((void)0)
#endif
#endif

//Used type for timing ticks (one tick is one millisecond)
typedef unsigned int ticks_t;

//Time related helper macros (elapsed is the delta time of the current frame, ticks is the total number of ticks (milliseconds) passed)
#define ZLELAPSED          ZL_Application::Elapsed
#define ZLELAPSEDF(factor) (ZL_Application::Elapsed*(s(factor)))
#define ZLELAPSEDTICKS     ZL_Application::ElapsedTicks
#define ZLTICKS            ZL_Application::Ticks
#define ZLSECONDS          (s(ZL_Application::Ticks)/s(1000))
#define ZLSINCE(t)         ((int)(ZL_Application::Ticks-(ticks_t)(t)))
#define ZLSINCEF(t,factor) (s((int)(ZL_Application::Ticks-(ticks_t)(t)))/s(factor))
#define ZLSINCESECONDS(t)  (s((int)(ZL_Application::Ticks-(ticks_t)(t)))/s(1000))
#define ZLUNTIL(t)         ((int)((ticks_t)(t)-ZL_Application::Ticks))
#define ZLUNTILF(t,factor) (s((int)((ticks_t)(t)-ZL_Application::Ticks))/s(factor))
#define ZLUNTILSECONDS(t)  (s((int)((ticks_t)(t)-ZL_Application::Ticks))/s(1000))

//The core of initialization and the game loop. Needs to be subclassed in any application using ZillaLib.
struct ZL_Application
{
	ZL_Application(unsigned short fpslimit = 60);

	//Will be called upon initialization of the program, should call Init() on required submodules, load initial assets and switch to the first scene if scene manager is used.
	virtual void Load(int argc, char *argv[]) = 0;

	//Virtual functions to be overridden
	virtual void BeforeFrame() { }
	virtual void AfterFrame() { }
	virtual void OnQuit() { }

	//Static functions
	static ZL_Application& GetApplication();
	static void SetFpsLimit(unsigned short fps);
	static void Quit(int Return = 0);
	static void Log(const char *logtag, const char *format, ...);

	//Only smartphone targets have a system to generate a unique ID, other platforms should store a random ID in the settings
	static ZL_String DeviceUniqueID();

	//This will initialize a default file archive for release builds on desktop targets (debug builds will load assets directly without packaging)
	#if defined(NDEBUG) && !defined(__SMARTPHONE__) && !defined(__WEBAPP__)
	static bool LoadReleaseDesktopDataBundle(const char* DataBundleFileName = NULL);
	#else
	static inline bool LoadReleaseDesktopDataBundle(const char* = NULL) { return true; }
	#endif

	//Key/value setting storage
	static void SettingsInit(const char* FallbackConfigFilePrefix);
	static ZL_String SettingsGet(const char* Key);
	static void SettingsSet(const char* Key, const ZL_String& Value);
	static void SettingsDel(const char* Key);
	static bool SettingsHas(const char* Key);
	static void SettingsSynchronize();
	static void OpenExternalUrl(const char* url);

	//Timing variables
	static unsigned short FPS;
	static ticks_t Ticks, ElapsedTicks;
	static unsigned int FrameCount;
	static scalar Elapsed;
	static ZL_Signal_v0 sigKeepAlive;

	//Called by the engine every frame, updates the timing and submodules and calls the scene manager and BeforeFrame/AfterFrame.
	virtual void Frame();

private:
	ZL_Application(const ZL_Application&);
	ZL_Application& operator=(const ZL_Application&);
};

//this class can be used to have ticks per seconds (tps, number of calculation loop) separate from rendered frames per second (fps)
class ZL_ApplicationConstantTicks : public ZL_Application
{
protected:
	ZL_ApplicationConstantTicks(unsigned short fps = 60, unsigned short tps = 60);
	virtual void Frame();
	virtual void AfterCalculate() { }
	static void SetFpsTps(unsigned short fps = 60, unsigned short tps = 60);
};

#endif //__ZL_APPLICATION__
