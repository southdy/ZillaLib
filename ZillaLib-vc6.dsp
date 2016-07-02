# Microsoft Developer Studio Project File - Name="ZillaLib" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **
# TARGTYPE "Win32 (x86) Static Library" 0x0104
# TARGTYPE "Win32 (x86) External Target" 0x0106
CFG=ZillaLib - Win32 Debug
!MESSAGE NMAKE /f "ZillaLib-vc6.mak".
!MESSAGE NMAKE /f "ZillaLib-vc6.mak" CFG="ZillaLib - Win32 Debug"
!MESSAGE "ZillaLib - Win32 Release" (basierend auf  "Win32 (x86) Static Library")
!MESSAGE "ZillaLib - Win32 Debug" (basierend auf  "Win32 (x86) Static Library")
!MESSAGE "ZillaLib - NACL Release" (basierend auf  "Win32 (x86) External Target")
!MESSAGE "ZillaLib - NACL Debug" (basierend auf  "Win32 (x86) External Target")
!MESSAGE "ZillaLib - Emscripten Release" (basierend auf  "Win32 (x86) External Target")
!MESSAGE "ZillaLib - Emscripten Debug" (basierend auf  "Win32 (x86) External Target")
!MESSAGE "ZillaLib - Android Release" (basierend auf  "Win32 (x86) External Target")
!MESSAGE "ZillaLib - Android Debug" (basierend auf  "Win32 (x86) External Target")
# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""

!IF  "$(CFG)" == "ZillaLib - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release-vc6"
# PROP Intermediate_Dir "Release-vc6"
# PROP Target_Dir ""
CPP=cl.exe
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_LIB" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /O2 /I "./Include" /I "./Source/zlib" /I "./Source/sdl/include" /D "WIN32" /D "NDEBUG" /D "_LIB" /D "_MBCS" /FD /c
# SUBTRACT CPP /YX
RSC=rc.exe
# ADD BASE RSC /l 0x807 /d "NDEBUG"
# ADD RSC /l 0x807 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"Release-vc6/ZillaLib.lib"

!ELSEIF  "$(CFG)" == "ZillaLib - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug-vc6"
# PROP Intermediate_Dir "Debug-vc6"
# PROP Target_Dir ""
CPP=cl.exe
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_LIB" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /ZI /Od /I "./Include" /I "./Source/zlib" /I "./Source/sdl/include" /D "WIN32" /D "_DEBUG" /D "_LIB" /D "_MBCS" /D "ZILLALOG" /FD /GZ /c
# SUBTRACT CPP /YX
RSC=rc.exe
# ADD BASE RSC /l 0x807 /d "_DEBUG"
# ADD RSC /l 0x807 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"Debug-vc6/ZillaLib.lib"

!ELSEIF  "$(CFG)" == "ZillaLib - NACL Release"

# PROP Output_Dir "NACL/build"
# PROP Intermediate_Dir "NACL/build"
# PROP Cmd_Line "python NACL/ZillaLibNACL.py build -rel -vc"
# PROP Rebuild_Opt "-clean"
# PROP Target_File "NACL/build/ZillaLib_x86_64.a"
RSC=rc.exe
CPP=cl.exe

!ELSEIF  "$(CFG)" == "ZillaLib - NACL Debug"

# PROP Output_Dir "NACL/build-debug"
# PROP Intermediate_Dir "NACL/build-debug"
# PROP Cmd_Line "python NACL/ZillaLibNACL.py build -vc"
# PROP Rebuild_Opt "-clean"
# PROP Target_File "NACL/build-debug/ZillaLib_x86_64.a"
RSC=rc.exe
CPP=cl.exe

!ELSEIF  "$(CFG)" == "ZillaLib - Emscripten Release"

# PROP Output_Dir "Emscripten/build"
# PROP Intermediate_Dir "Emscripten/build"
# PROP Cmd_Line "python Emscripten/ZillaLibEmscripten.py build -rel -vc"
# PROP Rebuild_Opt "-clean"
# PROP Target_File "Emscripten/build/ZillaLib_x86_64.a"
RSC=rc.exe
CPP=cl.exe

!ELSEIF  "$(CFG)" == "ZillaLib - Emscripten Debug"

# PROP Output_Dir "Emscripten/build-debug"
# PROP Intermediate_Dir "Emscripten/build-debug"
# PROP Cmd_Line "python Emscripten/ZillaLibEmscripten.py build -vc"
# PROP Rebuild_Opt "-clean"
# PROP Target_File "Emscripten/build-debug/ZillaLib_x86_64.a"
RSC=rc.exe
CPP=cl.exe

!ELSEIF  "$(CFG)" == "ZillaLib - Android Release"

# PROP Output_Dir "Android/build"
# PROP Intermediate_Dir "Android/build"
# PROP Cmd_Line "Tools\make.exe -f Android\ZillaLibAndroid.mk ZLDEBUG=0"
# PROP Rebuild_Opt "-B"
# PROP Target_File "Android/build/armaeabi/libZillaLib.a"
RSC=rc.exe
CPP=cl.exe

!ELSEIF  "$(CFG)" == "ZillaLib - Android Debug"

# PROP Output_Dir "Android/build-debug"
# PROP Intermediate_Dir "Android/build-debug"
# PROP Cmd_Line "Tools\make.exe -f Android\ZillaLibAndroid.mk ZLDEBUG=1"
# PROP Rebuild_Opt "-B"
# PROP Target_File "Android/build-debug/armaeabi/libZillaLib.a"
RSC=rc.exe
CPP=cl.exe

!ENDIF 

# Begin Target

# Name "ZillaLib - Win32 Release"
# Name "ZillaLib - Win32 Debug"
# Name "ZillaLib - NACL Release"
# Name "ZillaLib - NACL Debug"
# Name "ZillaLib - Emscripten Release"
# Name "ZillaLib - Emscripten Debug"
# Name "ZillaLib - Android Release"
# Name "ZillaLib - Android Debug"
# Begin Group "Dependencies"
# PROP Default_Filter ""
# Begin Group "ZLIB"
# PROP Default_Filter ""
# Begin Source File
SOURCE=.\Source\zlib\miniz.c
# End Source File
# End Group
# Begin Group "ENET"
# PROP Default_Filter ""
# Begin Source File
SOURCE=.\Source\enet\callbacks.c
# End Source File
# Begin Source File
SOURCE=.\Source\enet\host.c
# End Source File
# Begin Source File
SOURCE=.\Source\enet\list.c
# End Source File
# Begin Source File
SOURCE=.\Source\enet\packet.c
# End Source File
# Begin Source File
SOURCE=.\Source\enet\peer.c
# End Source File
# Begin Source File
SOURCE=.\Source\enet\protocol.c
# End Source File
# Begin Source File
SOURCE=.\Source\enet\win32.c
# End Source File
# End Group
# Begin Group "SDL"
# PROP Default_Filter ""
# Begin Source File
SOURCE=.\Source\sdl\audio\SDL_audio.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\events\SDL_clipboardevents.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\include\SDL_config.h
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\include\SDL_config_zillalib.h
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\audio\directsound\SDL_directsound.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\events\SDL_dropevents.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\joystick\windows\SDL_dxjoystick.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\events\SDL_events.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\joystick\SDL_joystick.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\events\SDL_keyboard.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\events\SDL_mouse.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\events\SDL_quit.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\video\SDL_rect.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\include\SDL_stdinc.h
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\thread\generic\SDL_syscond.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\loadso\windows\SDL_sysloadso.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\thread\windows\SDL_sysmutex.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\thread\windows\SDL_syssem.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\thread\windows\SDL_systhread.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\timer\windows\SDL_systimer.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\thread\SDL_thread.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\events\SDL_touch.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\video\SDL_video.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\events\SDL_windowevents.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\video\windows\SDL_windowsclipboard.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\video\windows\SDL_windowsevents.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\video\windows\SDL_windowsframebuffer.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\video\windows\SDL_windowskeyboard.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\video\windows\SDL_windowsmessagebox.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\video\windows\SDL_windowsmodes.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\video\windows\SDL_windowsmouse.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\video\windows\SDL_windowsopengl.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\video\windows\SDL_windowsvideo.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\sdl\video\windows\SDL_windowswindow.c
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1 /Ob1
!ENDIF 
# End Source File
# End Group
# Begin Group "LIBTESS2"
# PROP Default_Filter ""
# Begin Source File
SOURCE=.\Source\libtess2\tesselator.c
# End Source File
# End Group
# Begin Group "STB"
# PROP Default_Filter ""
# Begin Source File
SOURCE=.\Source\stb\stb_image.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\stb\stb_image.h
# End Source File
# Begin Source File
SOURCE=.\Source\stb\stb_truetype.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\stb\stb_truetype.h
# End Source File
# Begin Source File
SOURCE=.\Source\stb\stb_vorbis.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\stb\stb_vorbis.h
# End Source File
# End Group
# End Group
# Begin Group "Include"
# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File
SOURCE=.\Include\ZL_Application.h
# End Source File
# Begin Source File
SOURCE=.\Include\ZL_Audio.h
# End Source File
# Begin Source File
SOURCE=.\Include\ZL_Display.h
# End Source File
# Begin Source File
SOURCE=.\Include\ZL_Display3D.h
# End Source File
# Begin Source File
SOURCE=.\Include\ZL_Events.h
# End Source File
# Begin Source File
SOURCE=.\Include\ZL_File.h
# End Source File
# Begin Source File
SOURCE=.\Include\ZL_Font.h
# End Source File
# Begin Source File
SOURCE=.\Include\ZL_Input.h
# End Source File
# Begin Source File
SOURCE=.\Include\ZL_Math.h
# End Source File
# Begin Source File
SOURCE=.\Include\ZL_Math3D.h
# End Source File
# Begin Source File
SOURCE=.\Include\ZL_Network.h
# End Source File
# Begin Source File
SOURCE=.\Include\ZL_Particles.h
# End Source File
# Begin Source File
SOURCE=.\Include\ZL_Scene.h
# End Source File
# Begin Source File
SOURCE=.\Include\ZL_Signal.h
# End Source File
# Begin Source File
SOURCE=.\Include\ZL_String.h
# End Source File
# Begin Source File
SOURCE=.\Include\ZL_Surface.h
# End Source File
# Begin Source File
SOURCE=.\Include\ZL_SynthImc.h
# End Source File
# Begin Source File
SOURCE=.\Include\ZL_Thread.h
# End Source File
# Begin Source File
SOURCE=.\Include\ZL_Timer.h
# End Source File
# Begin Source File
SOURCE=.\Include\ZL_Data.h
# End Source File
# End Group
# Begin Group "Source"
# PROP Default_Filter "cpp"
# Begin Source File
SOURCE=.\Source\ZL_Application.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_Audio.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_Display.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_Display3D.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_Display_Impl.h
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_File.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_File_Impl.h
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_File_ZIP.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_Font.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_Impl.h
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_Input.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_Math.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_Network.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_Particles.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_Platform.h
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_PlatformAndroid.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_PlatformAndroid.h
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_PlatformConfig.h
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_PlatformEmscripten.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_PlatformEmscripten.h
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_PlatformGLSL.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_PlatformGLSL.h
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_PlatformIOS.h
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_PlatformIOS.mm
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_PlatformNACL.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_PlatformNACL.h
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_PlatformPosix.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_PlatformPosix.h
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_PlatformSDL.cpp
!IF  "$(CFG)" == "ZillaLib - Win32 Release"
# ADD CPP /O1
!ENDIF 
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_PlatformSDL.h
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_PlatformWP.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_PlatformWP.h
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_Scene.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_Signal.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_String.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_Surface.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_SynthImc.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_Texture_Impl.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_Texture_Impl.h
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_Thread.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_Timer.cpp
# End Source File
# Begin Source File
SOURCE=.\Source\ZL_Data.cpp
# End Source File
# End Group
# End Target
# End Project
