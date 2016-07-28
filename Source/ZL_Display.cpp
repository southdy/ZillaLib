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

#include "ZL_Display.h"
#include "ZL_Display_Impl.h"
#include <assert.h>

#undef KMOD_META

static bool use_aa = false, use_inputscale;
static scalar thickness = s(1.0), thicknessmaxhalf = 63.0f;
static scalar inputscale_x, inputscale_y;
scalar ZL_Display::Width = 0, ZL_Display::Height = 0;
int native_width = 0, native_height = 0, window_viewport[4], window_framebuffer = 0, *active_viewport = window_viewport, active_framebuffer = 0;
bool native_aspectcorrection = false; //GLscalar native_aspectborders[2*2*6];
bool (*funcProcessEventsJoystick)(ZL_Event&) = NULL;
void (*funcInitGL3D)(bool RecreateContext) = NULL;

const ZL_Color ZL_Color::Transparent(0.00f, 0.00f, 0.00f, 0.f), ZL_Color::Red    (1.00f, 0.00f, 0.00f, 1.f), ZL_Color::DarkRed    (0.50f, 0.00f, 0.00f, 1.f);
const ZL_Color ZL_Color::White      (1.00f, 1.00f, 1.00f, 1.f), ZL_Color::Green  (0.00f, 1.00f, 0.00f, 1.f), ZL_Color::DarkGreen  (0.00f, 0.50f, 0.00f, 1.f);
const ZL_Color ZL_Color::Black      (0.00f, 0.00f, 0.00f, 1.f), ZL_Color::Blue   (0.00f, 0.00f, 1.00f, 1.f), ZL_Color::DarkBlue   (0.00f, 0.00f, 0.50f, 1.f);
const ZL_Color ZL_Color::Silver     (0.75f, 0.75f, 0.75f, 1.f), ZL_Color::Yellow (1.00f, 1.00f, 0.00f, 1.f), ZL_Color::DarkYellow (0.50f, 0.50f, 0.00f, 1.f);
const ZL_Color ZL_Color::Gray       (0.50f, 0.50f, 0.50f, 1.f), ZL_Color::Magenta(1.00f, 0.00f, 1.00f, 1.f), ZL_Color::DarkMagenta(0.50f, 0.00f, 0.50f, 1.f);
const ZL_Color ZL_Color::DarkGray   (0.25f, 0.25f, 0.25f, 1.f), ZL_Color::Cyan   (0.00f, 1.00f, 1.00f, 1.f), ZL_Color::DarkCyan   (0.00f, 0.50f, 0.50f, 1.f);
const ZL_Color ZL_Color::Orange     (1.00f, 0.70f, 0.00f, 1.f), ZL_Color::Brown  (0.50f, 0.25f, 0.05f, 1.f), ZL_Color::Pink       (1.00f, 0.41f, 0.70f, 1.f);

ZL_Signal_v1<ZL_KeyboardEvent&> ZL_Display::sigKeyDown, ZL_Display::sigKeyUp;
ZL_Signal_v1<const ZL_String&> ZL_Display::sigTextInput;
ZL_Signal_v1<ZL_WindowResizeEvent&> ZL_Display::sigResized;
ZL_Signal_v1<ZL_PointerMoveEvent&> ZL_Display::sigPointerMove;
ZL_Signal_v1<ZL_PointerPressEvent&> ZL_Display::sigPointerDown, ZL_Display::sigPointerUp;
ZL_Signal_v1<ZL_MouseWheelEvent&> ZL_Display::sigMouseWheel;
ZL_Signal_v1<ZL_WindowActivateEvent&> ZL_Display::sigActivated; //focus, key input, mouse input

bool ZL_Display::KeyDown[ZLK_LAST], ZL_Display::MouseDown[8];
scalar ZL_Display::PointerX, ZL_Display::PointerY;

void ZL_Display_Process_Event(ZL_Event& event)
{
	switch (event.type)
	{
		case ZL_EVENT_MOUSEMOTION:
			if (use_inputscale) { event.motion.x -= window_viewport[0]; event.motion.x *= inputscale_x; event.motion.y -= window_viewport[1]; event.motion.y *= inputscale_y; event.motion.xrel *= inputscale_x; event.motion.yrel *= inputscale_y; }
			event.motion.y = ZL_Display::Height - event.motion.y;
			ZL_Display::PointerX = event.motion.x;
			ZL_Display::PointerY = event.motion.y;
			event.motion.yrel = -event.motion.yrel;
			ZL_Display::sigPointerMove.call(event.motion);
			break;
		case ZL_EVENT_MOUSEBUTTONDOWN:
		case ZL_EVENT_MOUSEBUTTONUP:
			assert(event.button.is_down == (event.type == ZL_EVENT_MOUSEBUTTONDOWN));
			if (use_inputscale) { event.button.x -= window_viewport[0]; event.button.x *= inputscale_x; event.button.y -= window_viewport[1]; event.button.y *= inputscale_y; }
			event.button.y = ZL_Display::Height - event.button.y;
			ZL_Display::PointerX = event.button.x;
			ZL_Display::PointerY = event.button.y;
			ZL_Display::MouseDown[event.button.button] = event.button.is_down;
			if (ZL_WINDOWFLAGS_HAS(ZL_WINDOW_INPUT_FOCUS)) (event.button.is_down ? ZL_Display::sigPointerDown : ZL_Display::sigPointerUp).call(event.button);
			break;
		case ZL_EVENT_MOUSEWHEEL:
			if (ZL_WINDOWFLAGS_HAS(ZL_WINDOW_INPUT_FOCUS)) ZL_Display::sigMouseWheel.call(event.wheel);
			break;
		case ZL_EVENT_KEYDOWN:
			event.key.is_down = true;
			event.key.is_repeat = ZL_Display::KeyDown[event.key.key];
			ZL_Display::KeyDown[event.key.key] = true;
			#ifdef ZL_HAS_FULLSCREEN
			if (event.key.key == ZLK_RETURN && !ZL_WINDOWFLAGS_HAS(ZL_WINDOW_PREVENTALTENTER) && (event.key.mod & ZLKMOD_ALT)) { ZL_Display::ToggleFullscreen(); return; }
			if (event.key.key == ZLK_F4 && !ZL_WINDOWFLAGS_HAS(ZL_WINDOW_PREVENTALTF4) && (event.key.mod & ZLKMOD_ALT)) { ZL_Application::Quit(); return; }
			#endif
			if (ZL_WINDOWFLAGS_HAS(ZL_WINDOW_INPUT_FOCUS)) ZL_Display::sigKeyDown.call(event.key);
			break;
		case ZL_EVENT_KEYUP:
			event.key.is_down = false;
			event.key.is_repeat = false;
			ZL_Display::KeyDown[event.key.key] = false;
			if (ZL_WINDOWFLAGS_HAS(ZL_WINDOW_INPUT_FOCUS)) ZL_Display::sigKeyUp.call(event.key);
			break;
		case ZL_EVENT_TEXTINPUT:
			if (ZL_WINDOWFLAGS_HAS(ZL_WINDOW_INPUT_FOCUS) && ZL_Display::sigTextInput.HasConnections()) ZL_Display::sigTextInput.call(event.text.text);
			break;
		case ZL_EVENT_QUIT:
			ZL_Application::Quit();
			break;
		case ZL_EVENT_WINDOW:
			if (event.window.event == ZL_WINDOWEVENT_MOVED) { }
			else if (event.window.event == ZL_WINDOWEVENT_CLOSE) ZL_Application::Quit();
			else if (event.window.event == ZL_WINDOWEVENT_RESIZED)
			{
				if (native_width == event.window.data1 && native_height == event.window.data2) break;
				native_width = event.window.data1;
				native_height = event.window.data2;
				if (ZL_WINDOWFLAGS_HAS(ZL_WINDOW_ALLOWRESIZEHORIZONTAL))
				{
					event.window.data1 = (int)(ZL_Display::Height * native_width / native_height);
					event.window.data2 = (int)(ZL_Display::Height);
				}
				else if (ZL_WINDOWFLAGS_HAS(ZL_WINDOW_ALLOWRESIZEVERTICAL))
				{
					event.window.data1 = (int)(ZL_Display::Width);
					event.window.data2 = (int)(ZL_Display::Width * native_height / native_width);
				}
				else if (!ZL_WINDOWFLAGS_HAS(ZL_WINDOW_RESIZABLE))
				{
					event.window.data1 = (int)(ZL_Display::Width);
					event.window.data2 = (int)(ZL_Display::Height);
				}
				ZL_LOG4("DISPLAY", "Resized to: %d x %d [%d x %d]", event.window.data1, event.window.data2, native_width, native_height);
				ZL_WindowResizeEvent resizeEvent = { ZL_Display::Width, ZL_Display::Height };
				InitGL(event.window.data1, event.window.data2);
				ZL_Display::sigResized.call(resizeEvent);
			}
			else
			{
				ZL_WindowActivateEvent WindowActivateEvent;
				WindowActivateEvent.key_focus = (ZL_WINDOWFLAGS_HAS(ZL_WINDOW_INPUT_FOCUS)>0);
				WindowActivateEvent.mouse_focus = (ZL_WINDOWFLAGS_HAS(ZL_WINDOW_MOUSE_FOCUS)>0);
				WindowActivateEvent.minimized = (ZL_WINDOWFLAGS_HAS(ZL_WINDOW_MINIMIZED)>0);
				ZL_Display::sigActivated.call(WindowActivateEvent);
			}
			break;
		default:
			if (funcProcessEventsJoystick && funcProcessEventsJoystick(event)) break;
	}
}

void ZL_Display::AllSigDisconnect(void *callback_class_inst)
{
	sigKeyDown.disconnect_class(callback_class_inst);
	sigKeyUp.disconnect_class(callback_class_inst);
	sigTextInput.disconnect_class(callback_class_inst);
	sigPointerDown.disconnect_class(callback_class_inst);
	sigPointerUp.disconnect_class(callback_class_inst);
	sigMouseWheel.disconnect_class(callback_class_inst);
	sigPointerMove.disconnect_class(callback_class_inst);
	sigActivated.disconnect_class(callback_class_inst);
	sigResized.disconnect_class(callback_class_inst);
}

bool ZL_Display::Init(const char* title, int width, int height, int displayflags)
{
	if (width == height) displayflags |= ZL_DISPLAY_ALLOWANYORIENTATION;
	if (!ZL_CreateWindow(title, width, height, displayflags)) return false;

	#if defined(ZL_VIDEO_USE_GLSL) && !defined(ZL_VIDEO_DIRECT3D)
	ZLGLSL::CreateShaders();
	#endif

	//for (char* pcGlVersion = ((char*)glGetString(GL_VERSION)); *pcGlVersion; pcGlVersion++)
	//	if (*pcGlVersion >= '1' && *pcGlVersion <= '9') { RunningOpenGL1 = (*pcGlVersion == '1'); break; }
	//ZL_LOG2("DISPLAY", "GL_Version: %s (IS1:%d)", glGetString(GL_VERSION), RunningOpenGL1);

	ZL_LOG2("DISPLAY", "Requested width: %d - height: %d", width, height);
	ZL_GetWindowSize(&native_width, &native_height); //read back what we actually got at window creation
	if ((displayflags & ZL_DISPLAY_RESIZABLE) == ZL_DISPLAY_RESIZABLE)
	{
		width = native_width;
		height = native_height;
		*pZL_WindowFlags |= ZL_WINDOW_RESIZABLE;
	}
	else if (displayflags & ZL_DISPLAY_ALLOWRESIZEHORIZONTAL)
	{
		width = height * native_width / native_height;
		*pZL_WindowFlags |= ZL_WINDOW_ALLOWRESIZEHORIZONTAL;
	}
	else if (displayflags & ZL_DISPLAY_ALLOWRESIZEVERTICAL)
	{
		height = width * native_height / native_width;
		*pZL_WindowFlags |= ZL_WINDOW_ALLOWRESIZEVERTICAL;
	}
	if (displayflags & ZL_DISPLAY_ALLOWANYORIENTATION) *pZL_WindowFlags |= ZL_WINDOW_ALLOWANYORIENTATION;
	if (displayflags & ZL_DISPLAY_PREVENTALTENTER) *pZL_WindowFlags |= ZL_WINDOW_PREVENTALTENTER;
	if (displayflags & ZL_DISPLAY_PREVENTALTF4) *pZL_WindowFlags |= ZL_WINDOW_PREVENTALTF4;

	ZL_LOG2("DISPLAY", "Actually got width: %d - height: %d", native_width, native_height);

	InitGL(width, height);

	memset(KeyDown, 0, sizeof(KeyDown));
	memset(MouseDown, 0, sizeof(MouseDown));
	PointerX = PointerY = 0;

	return true;
}

#ifdef ZL_HAS_FULLSCREEN
void ZL_Display::ToggleFullscreen() { ZL_SetFullscreen(!ZL_WINDOWFLAGS_HAS(ZL_WINDOW_FULLSCREEN)); }
void ZL_Display::SetFullscreen(bool toFullscreen) { ZL_SetFullscreen(toFullscreen); }
#else
void ZL_Display::ToggleFullscreen() { }
void ZL_Display::SetFullscreen(bool /*toFullscreen*/) { }
#endif

#ifdef ZL_HAS_POINTERLOCK
void ZL_Display::TogglePointerLock() { ZL_SetPointerLock(!ZL_WINDOWFLAGS_HAS(ZL_WINDOW_POINTERLOCK)); }
void ZL_Display::SetPointerLock(bool doLockPointer) { ZL_SetPointerLock(doLockPointer); }
#else
void ZL_Display::TogglePointerLock() { }
void ZL_Display::SetPointerLock(bool /*doLockPointer*/) { }
#endif

void ZL_Display::SetClip(const ZL_Rectf &clip)
{
	glScissor((int)(active_viewport[0]+(clip.left*active_viewport[2]/Width)),
	          (int)(active_viewport[1]+(clip.low*active_viewport[3]/Height)),
	          (int)((clip.right-clip.left)*active_viewport[2]/Width),
	          (int)((clip.high-clip.low)*active_viewport[3]/Height));
	glEnable(GL_SCISSOR_TEST);
}

void ZL_Display::SetClip(int x, int y, int clip_width, int clip_height)
{
	glScissor((int)(active_viewport[0]+(x*active_viewport[2]/Width)),
	          (int)(active_viewport[1]+(y*active_viewport[3]/Height)),
	          (int)(clip_width*active_viewport[2]/Width),
	          (int)(clip_height*active_viewport[3]/Height));
	glEnable(GL_SCISSOR_TEST);
}

void ZL_Display::ResetClip()
{
	glDisable(GL_SCISSOR_TEST);
}

void InitGL(int width, int height)
{
	window_viewport[2] = (native_width * height / width > native_height ? native_height * width / height : native_width);
	window_viewport[3] = (native_width * height / width > native_height ? native_height : (native_width * height + (width - 1)) / width);
	window_viewport[0] = (native_width-window_viewport[2])>>1;
	window_viewport[1] = (native_height-window_viewport[3])>>1;
	native_aspectcorrection = ((native_width-window_viewport[2]) || (native_height-window_viewport[3]));

	glViewport(window_viewport[0], window_viewport[1], window_viewport[2], window_viewport[3]);
	glGetIntegerv(GL_FRAMEBUFFER_BINDING, (GLint*)&window_framebuffer);
	active_framebuffer = window_framebuffer;
	ZL_LOG4("DISPLAY", "Setting viewport to pos: %d , %d - size: %d , %d", window_viewport[0], window_viewport[1], window_viewport[2], window_viewport[3]);

	ZL_Display::Width = s(width);
	ZL_Display::Height = s(height);
	use_inputscale = (native_width != width || native_height != height);
	if (use_inputscale)
	{
		inputscale_x = ZL_Display::Width/s(window_viewport[2]);
		inputscale_y = ZL_Display::Height/s(window_viewport[3]);
	}

	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glDisable(GL_DEPTH_TEST);
	glDepthFunc(GL_LEQUAL);
	glDisable(GL_CULL_FACE);
	glDepthMask(GL_FALSE);

	#if defined(GL_LINE_SMOOTH) && !defined(ZL_VIDEO_OPENGL_ES2)
	GLfloat thickness_range[2];
	glGetFloatv(GL_SMOOTH_LINE_WIDTH_RANGE, &thickness_range[0]);
	thicknessmaxhalf = MIN(s(thickness_range[0]), s(1))*s(0.5);
	#else
	thicknessmaxhalf = s(0.5);
	#endif

	#ifdef ZL_VIDEO_USE_GLSL
	GLORTHO(0.0f, (GLfloat)width, 0.0f, (GLfloat)height);
	#else
	glMatrixMode(GL_PROJECTION);
	GLLOADIDENTITY();
	GLORTHO(0.0f, (GLfloat)width, 0.0f, (GLfloat)height);
	glMatrixMode(GL_MODELVIEW);
	#endif

	#if defined(ZL_VIDEO_OPENGL1) || defined(ZL_VIDEO_OPENGL_ES1)
	glEnable(GL_ALPHA_TEST);
	glShadeModel(GL_SMOOTH);
	glDisable(GL_LIGHTING);
	glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_FASTEST);
	glHint(GL_POINT_SMOOTH_HINT, GL_FASTEST); //GL_NICEST);
	glHint(GL_LINE_SMOOTH_HINT, GL_FASTEST); //GL_NICEST);
	glDisable(GL_POINT_SMOOTH);
	glEnable(GL_LINE_SMOOTH);
	//glEnable(GL_POLYGON_SMOOTH);
	//glPolygonMode( GL_FRONT_AND_BACK, GL_LINE ); //wireframe debuggingbla
	#if defined(GL_MULTISAMPLE) && !defined(__WEBAPP__)
	if (use_aa) glEnable(GL_MULTISAMPLE); else glDisable(GL_MULTISAMPLE);
	#endif
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_TEXTURE_COORD_ARRAY);
	GLLOADIDENTITY();
	#endif

	#ifdef ZL_VIDEO_WEAKCONTEXT
	#ifdef ZL_VIDEO_USE_GLSL
	bool RecreateContext = CheckProgramsIfContextLost();
	#else
	bool RecreateContext = (CheckTexturesIfContextLost() || CheckFontTexturesIfContextLost());
	#endif
	if (RecreateContext)
	{
		#ifdef ZL_VIDEO_USE_GLSL
		RecreateAllProgramsOnContextLost();
		#endif
		RecreateAllTexturesOnContextLost();
		RecreateAllFontTexturesOnContextLost();
	}
	if (funcInitGL3D) funcInitGL3D(RecreateContext);
	#else
	if (funcInitGL3D) funcInitGL3D(false);
	#endif
}

void ZL_Display::SetAA(bool aa)
{
	use_aa = aa;
	#if defined(GL_MULTISAMPLE) && !defined(__WEBAPP__)
	if (use_aa) glEnable(GL_MULTISAMPLE); else glDisable(GL_MULTISAMPLE);
	#endif
}

void ZL_Display::SetThickness(scalar newthickness)
{
	thickness = newthickness;
#if defined(GL_LINE_SMOOTH) && !defined(ZL_VIDEO_OPENGL_ES2)
	if (newthickness == 1.0) glEnable(GL_LINE_SMOOTH);
	else                    glDisable(GL_LINE_SMOOTH);
	glPointSize((float)newthickness*0.9f);
#endif
	glLineWidth((float)newthickness);
}

void ZL_Display::ClearFill(ZL_Color col)
{
	glClearColor(col.r, col.g, col.b, col.a);
	glClear(GL_COLOR_BUFFER_BIT);
}

void ZL_Display::SetBlendFunc(BlendFunc mode_src, BlendFunc mode_dest)
{
	glBlendFunc((GLenum)mode_src, (GLenum)mode_dest);
}

void ZL_Display::SetBlendModeSeparate(BlendFunc mode_rgb_src, BlendFunc mode_rgb_dest, BlendFunc mode_alpha_src, BlendFunc mode_alpha_dest)
{
	glBlendFuncSeparate((GLenum)mode_rgb_src, (GLenum)mode_rgb_dest, (GLenum)mode_alpha_src, (GLenum)mode_alpha_dest);
}

void ZL_Display::SetBlendEquation(BlendEquation func)
{
	glBlendEquation((GLenum)func);
}

void ZL_Display::SetBlendEquationSeparate(BlendEquation func_rgb, BlendEquation func_alpha)
{
	glBlendEquationSeparate((GLenum)func_rgb, (GLenum)func_alpha);
}

void ZL_Display::SetBlendConstantColor(const ZL_Color& constant_color)
{
	glBlendColor((GLfloat)constant_color.r, (GLfloat)constant_color.g, (GLfloat)constant_color.b, (GLfloat)constant_color.a);
}

void ZL_Display::PushMatrix() { GLPUSHMATRIX(); }
void ZL_Display::PopMatrix() { GLPOPMATRIX(); }
void ZL_Display::Translate(const ZL_Vector& v) { GLTRANSLATE(v.x, v.y); }
void ZL_Display::Translate(scalar x, scalar y) { GLTRANSLATE(x, y); }
void ZL_Display::Rotate(scalar angle_rad)  { GLROTATERAD(angle_rad); }
void ZL_Display::RotateDeg(scalar angle_deg)  { GLROTATEDEG(angle_deg); }
void ZL_Display::Rotate(scalar rotx, scalar roty) { GLROTATEXY(rotx, roty); }
void ZL_Display::Transform(scalar x, scalar y, scalar rotx, scalar roty) { GLTRANSFORMXYROTXY(x, y, rotx, roty); }
void ZL_Display::TransformReverse(scalar x, scalar y, scalar rotx, scalar roty) { GLTRANSFORMREVXYROTXY(x, y, rotx, roty); }
void ZL_Display::Scale(scalar scale) { GLSCALE(scale, scale); }
void ZL_Display::Scale(scalar scalex, scalar scaley) { GLSCALE(scalex, scaley); }

void ZL_Display::PushOrtho(scalar left, scalar right, scalar bottom, scalar top)
{
	#ifdef ZL_VIDEO_USE_GLSL
	GLPUSHMATRIX();
	GLORTHO(left, right, bottom, top);
	#else
	glMatrixMode(GL_PROJECTION);
	GLPUSHMATRIX();
	GLLOADIDENTITY();
	GLORTHO(left, right, bottom, top);
	glMatrixMode(GL_MODELVIEW);
	GLPUSHMATRIX();
	#endif
}

void ZL_Display::PopOrtho()
{
	#ifdef ZL_VIDEO_USE_GLSL
	GLPOPMATRIX();
	#else
	GLPOPMATRIX();
	glMatrixMode(GL_PROJECTION);
	GLPOPMATRIX();
	glMatrixMode(GL_MODELVIEW);
	#endif
}

ZL_Vector ZL_Display::WorldToScreen(scalar x, scalar y)
{
	#if defined(ZL_VIDEO_USE_GLSL) && !defined(ZL_DOUBLE_PRECISCION)
	ZLGLSL::Project(x, y);
	return ZLV((x+1)*ZLHALFW, (y+1)*ZLHALFH);
	#elif defined(ZL_VIDEO_USE_GLSL)
	GLscalar glx = (GLscalar)x, gly = (GLscalar)y;
	ZLGLSL::Project(glx, gly);
	return ZLV(s(glx+1)*ZLHALFW, s(gly+1)*ZLHALFH);
	#else
	GLfloat mproj[16], mview[16];
	glGetFloatv(GL_PROJECTION_MATRIX, mproj);
	glGetFloatv(GL_MODELVIEW_MATRIX, mview);
	GLfloat m11 = mview[0]*mproj[0] + mview[1]*mproj[4] + mview[2]*mproj[8] + mview[3]*mproj[12];
	GLfloat m12 = mview[0]*mproj[1] + mview[1]*mproj[5] + mview[2]*mproj[9] + mview[3]*mproj[13];
	GLfloat m21 = mview[4]*mproj[0] + mview[5]*mproj[4] + mview[6]*mproj[8] + mview[7]*mproj[12];
	GLfloat m22 = mview[4]*mproj[1] + mview[5]*mproj[5] + mview[6]*mproj[9] + mview[7]*mproj[13];
	GLfloat m41 = mview[12]*mproj[0] + mview[13]*mproj[4] + mview[14]*mproj[8] + mview[15]*mproj[12];
	GLfloat m42 = mview[12]*mproj[1] + mview[13]*mproj[5] + mview[14]*mproj[9] + mview[15]*mproj[13];
	GLfloat oldx = x;
	return ZLV((((x * m11) + (y * m21) + m41)+1)*ZLHALFW, (((y * m12) + (y * m22) + m42)+1)*ZLHALFH);
	#endif
}

ZL_Vector ZL_Display::ScreenToWorld(scalar x, scalar y)
{
	x = x/ZLHALFW-s(1); y = y/ZLHALFH-s(1);
	#if defined(ZL_VIDEO_USE_GLSL) && !defined(ZL_DOUBLE_PRECISCION)
	ZLGLSL::Unproject(x, y);
	return ZLV(x,y);
	#elif defined(ZL_VIDEO_USE_GLSL)
	GLscalar glx = (GLscalar)x, gly = (GLscalar)y;
	ZLGLSL::Unproject(glx, gly);
	return ZLV(glx,gly);
	#else
	GLfloat mproj[16], mview[16];
	glGetFloatv(GL_PROJECTION_MATRIX, mproj);
	glGetFloatv(GL_MODELVIEW_MATRIX, mview);
	GLfloat m11 = mview[0]*mproj[0] + mview[1]*mproj[4] + mview[2]*mproj[8] + mview[3]*mproj[12];
	GLfloat m12 = mview[0]*mproj[1] + mview[1]*mproj[5] + mview[2]*mproj[9] + mview[3]*mproj[13];
	GLfloat m21 = mview[4]*mproj[0] + mview[5]*mproj[4] + mview[6]*mproj[8] + mview[7]*mproj[12];
	GLfloat m22 = mview[4]*mproj[1] + mview[5]*mproj[5] + mview[6]*mproj[9] + mview[7]*mproj[13];
	GLfloat m33 = mview[8]*mproj[2] + mview[9]*mproj[6] + mview[10]*mproj[10] + mview[11]*mproj[14];
	GLfloat m41 = mview[12]*mproj[0] + mview[13]*mproj[4] + mview[14]*mproj[8] + mview[15]*mproj[12];
	GLfloat m42 = mview[12]*mproj[1] + mview[13]*mproj[5] + mview[14]*mproj[9] + mview[15]*mproj[13];
	GLfloat det8 = -m33 * m41;
	GLfloat det10 = -m33 * m42;
	GLfloat invdetmatrix = 1.0f / ((m11 * m22 - m12 * m21)*m33);
	GLfloat invm11 = ( m22*m33) * invdetmatrix;
	GLfloat invm12 = (-m12*m33) * invdetmatrix;
	GLfloat invm21 = (-m21*m33) * invdetmatrix;
	GLfloat invm22 = ( m11*m33) * invdetmatrix;
	GLfloat invm41 = (-m21*det10 + m22*det8) * invdetmatrix;
	GLfloat invm42 = ( m11*det10 - m12*det8) * invdetmatrix;
	return ZLV((x * invm11) + (y * invm21) + invm41, (x * invm12) + (y * invm22) + invm42);
	#endif
}

void ZL_Display::DrawLine(scalar x1, scalar y1, scalar x2, scalar y2, const ZL_Color &color)
{
	GLscalar vertices[4] = { x1 , y1 , x2 , y2 };
	ZLGL_DISABLE_TEXTURE();
	ZLGL_COLOR(color);
	ZLGL_VERTEXTPOINTER(2, GL_SCALAR, 0, vertices);
	glDrawArraysUnbuffered(GL_LINES, 0, 2);
	/*
	ZL_Vector direction(x1, y1, x2, y2);
	ZL_Vector perpDirection = direction.Perp();
	perpDirection = perpDirection.NormVec() * thickness * 0.5;
	glDisable(GL_TEXTURE_2D);
	ZLCOLORGL(color);
	glBegin(GL_TRIANGLE_STRIP);
	glVertex2d(x1-perpDirection.x, y1-perpDirection.y);
	glVertex2d(x1+perpDirection.x, y1+perpDirection.y);
	glVertex2d(x2+perpDirection.x, y2+perpDirection.y);
	glVertex2d(x2-perpDirection.x, y2-perpDirection.y);
	glEnd();
	*/
}

void ZL_Display::DrawBezier(scalar x1, scalar y1, scalar x2, scalar y2, scalar x3, scalar y3, scalar x4, scalar y4, const ZL_Color &color)
{
	scalar x = x1, y = y1;
	float tstep = 18.0f/ssqrt((float)((x1-x2)*(x1-x2)+(x2-x3)*(x2-x3)+(x4-x3)*(x4-x3)+(y1-y2)*(y1-y2)+(y2-y3)*(y2-y3)+(y4-y3)*(y4-y3)));
	if (tstep > 0.25f) tstep = 0.25f;
	for (scalar t=tstep; t <= 1; t += tstep)
	{
		scalar xx = (x1 * (1-t)*(1-t)*(1-t)*(1-t) + 4 * x2 * t*(1-t)*(1-t)*(1-t) + 6 * x3 * t*t*(1-t)*(1-t) + 4 * x4 * t*t*t*(1-t) + x4 * t*t*t*t);
		scalar yy = (y1 * (1-t)*(1-t)*(1-t)*(1-t) + 4 * y2 * t*(1-t)*(1-t)*(1-t) + 6 * y3 * t*t*(1-t)*(1-t) + 4 * y4 * t*t*t*(1-t) + y4 * t*t*t*t);
		ZL_Display::DrawLine(x, y, xx, yy, color);
		x= xx; y = yy;
	}
}

void ZL_Display::DrawEllipse(scalar cx, scalar cy, scalar rx, scalar ry, const ZL_Color &color_border, const ZL_Color &color_fill)
{
	int iSize = (rx+ry > 300 ? 2 : (rx+ry > 80 ? 1 : 0));
	static GLscalar *pCircleVertices[3] = { NULL, NULL, NULL };
	int iNumVert = (iSize == 0 ? 23 : (iSize == 1 ? 34 : 65));
	#ifdef ZL_VIDEO_DIRECT3D //avoid triangle fans
	static GLscalar *pCircleVerticesFill[3] = { NULL, NULL, NULL };
	int iNumVertFill = (iSize == 0 ? 21*3 : (iSize == 1 ? 32*3 : 63*3));
	#endif
	if (!pCircleVertices[iSize])
	{
		scalar delta = (iSize == 0 ? s(0.3) : (iSize == 1 ? s(0.2) : s(0.1)));
		GLscalar *pVert = pCircleVertices[iSize] = new GLscalar[iNumVert*2];
		pVert[0] = pVert[1] = pVert[2] = pVert[iNumVert*2-2] = 0; pVert[3] = pVert[iNumVert*2-1] = -1; pVert += 4;
		#ifdef ZL_VIDEO_DIRECT3D //avoid triangle fans
		GLscalar *pVertFill = pCircleVerticesFill[iSize] = new GLscalar[iNumVertFill*2];
		pVertFill[0] = pVertFill[1] = pVertFill[2] = pVertFill[iNumVertFill*2-2] = 0; pVertFill[3] = pVertFill[iNumVertFill*2-1] = -1; pVertFill += 4;
		#endif
		for (scalar a = -PI+delta; a < PI; a+= delta)
		{
			*(pVert++) = ssin(a); *(pVert++) = scos(a);
			#ifdef ZL_VIDEO_DIRECT3D //avoid triangle fans
			pVertFill[0] = pVertFill[4] = pVert[-2]; pVertFill[1] = pVertFill[5] = pVert[-1]; pVertFill[2] = pVertFill[3] = 0; pVertFill += 6;
			#endif
		}
	}

	GLPUSHMATRIX();
	ZLGL_DISABLE_PROGRAM();
	GLTRANSLATE(cx, cy);
	GLSCALE(rx, ry);
	ZLGL_DISABLE_TEXTURE();
	#ifndef ZL_VIDEO_DIRECT3D //avoid triangle fans
	ZLGL_VERTEXTPOINTER(2, GL_SCALAR, 0, pCircleVertices[iSize]);
	#endif
	if (color_fill.a)
	{
		ZLGL_COLOR(color_fill);
		#ifndef ZL_VIDEO_DIRECT3D //avoid triangle fans
		glDrawArraysUnbuffered(GL_TRIANGLE_FAN, 0, iNumVert);
		#else
		ZLGL_VERTEXTPOINTER(2, GL_SCALAR, 0, pCircleVerticesFill[iSize]);
		glDrawArraysUnbuffered(GL_TRIANGLES, 0, iNumVertFill);
		#endif
	}
	if (color_border.a)
	{
		#ifdef ZL_VIDEO_DIRECT3D //avoid triangle fans
		ZLGL_VERTEXTPOINTER(2, GL_SCALAR, 0, pCircleVertices[iSize]);
		#endif
		ZLGL_COLOR(color_border);
		glDrawArraysUnbuffered(GL_LINE_STRIP, 1, iNumVert-1);
		glDrawArraysUnbuffered(GL_POINTS, 1, iNumVert-2);
	}
	GLPOPMATRIX();
}

void ZL_Display::FillGradient(const scalar& x1, const scalar& y1, const scalar& x2, const scalar& y2, const ZL_Color &col1, const ZL_Color &col2, const ZL_Color &col3, const ZL_Color &col4)
{
	GLscalar colorsbox[4*4] = { col1.r, col1.g, col1.b, col1.a, col2.r, col2.g, col2.b, col2.a, col3.r, col3.g, col3.b, col3.a, col4.r, col4.g, col4.b, col4.a };
	GLscalar verticesbox[8] = { x1 , y2 , x2 , y2 , x1 , y1 ,  x2 , y1 };
	ZLGL_DISABLE_TEXTURE();
	ZLGL_COLORARRAY_ENABLE();
	ZLGL_COLORARRAY_POINTER(4, GL_SCALAR, 0, colorsbox);
	ZLGL_VERTEXTPOINTER(2, GL_SCALAR, 0, verticesbox);
	glDrawArraysUnbuffered(GL_TRIANGLE_STRIP, 0, 4);
	ZLGL_COLORARRAY_DISABLE();
}

void ZL_Display::DrawRect(const scalar& x1, const scalar& y1, const scalar& x2, const scalar& y2, const ZL_Color &color_border, const ZL_Color &color_fill)
{
	ZLGL_DISABLE_TEXTURE();

	if (!color_border.a)
	{
		GLscalar verticesbox[8] = { x1 , y2 , x2 , y2 , x1 , y1 ,  x2 , y1 };
		ZLGL_COLOR(color_fill);
		ZLGL_VERTEXTPOINTER(2, GL_SCALAR, 0, verticesbox);
		glDrawArraysUnbuffered(GL_TRIANGLE_STRIP, 0, 4);
		return;
	}

	scalar t = thickness * s(0.5);

	ZLGL_COLOR(color_border);
	if (color_fill.a >= 1)
	{
		GLscalar verticesouter[8] = { x1-t , y2+t , x2+t , y2+t , x1-t , y1-t ,  x2+t , y1-t };
		ZLGL_VERTEXTPOINTER(2, GL_SCALAR, 0, verticesouter);
		glDrawArraysUnbuffered(GL_TRIANGLE_STRIP, 0, 4);
	}
	else
	{
		if (t > thicknessmaxhalf)  t = thicknessmaxhalf;
		GLscalar b = t - s(0.5), verticesborder[16] = { x1-t, y1, x2+b, y1, x2, y1-t, x2, y2+b, x2+b, y2, x1-b, y2,x1, y2+b, x1, y1-b };
		ZLGL_VERTEXTPOINTER(2, GL_SCALAR, 0, verticesborder);
		glDrawArraysUnbuffered(GL_LINES, 0, 8);
	}

	GLscalar verticesinner[8] = { x1+t , y2-t , x2-t , y2-t , x1+t , y1+t ,  x2-t , y1+t };
	ZLGL_COLOR(color_fill);
	ZLGL_VERTEXTPOINTER(2, GL_SCALAR, 0, verticesinner);
	glDrawArraysUnbuffered(GL_TRIANGLE_STRIP, 0, 4);
}

void ZL_Display::DrawTriangle(scalar x1, scalar y1, scalar x2, scalar y2, scalar x3, scalar y3, const ZL_Color &color_border, const ZL_Color &color_fill)
{
	ZLGL_DISABLE_TEXTURE();
	GLscalar vertices[8] = { x1, y1 , x2, y2 , x3, y3 , x1, y1 };
	ZLGL_VERTEXTPOINTER(2, GL_SCALAR, 0, vertices);
	if (color_fill.a)
	{
		ZLGL_COLOR(color_fill);
		glDrawArraysUnbuffered(GL_TRIANGLES, 0, 3);
	}
	if (color_border.a)
	{
		ZLGL_COLOR(color_border);
		glDrawArraysUnbuffered(GL_LINE_STRIP, 0, 4);
		glDrawArraysUnbuffered(GL_POINTS, 0, 3);
	}
}

void ZL_Display::DrawQuad(scalar x1, scalar y1, scalar x2, scalar y2, scalar x3, scalar y3, scalar x4, scalar y4, const ZL_Color &color_border, const ZL_Color &color_fill)
{
	ZLGL_DISABLE_TEXTURE();
	GLscalar vertices[10] = { x1, y1 , x2, y2 , x4, y4 , x3, y3 , x1, y1 };
	ZLGL_VERTEXTPOINTER(2, GL_SCALAR, 0, vertices);
	if (color_fill.a)
	{
		ZLGL_COLOR(color_fill);
		glDrawArraysUnbuffered(GL_TRIANGLE_STRIP, 0, 4);
	}
	if (color_border.a)
	{
		vertices[4] = x3; vertices[5] = y3; vertices[6] = x4; vertices[7] = y4;
		ZLGL_COLOR(color_border);
		glDrawArraysUnbuffered(GL_LINE_STRIP, 0, 5);
		glDrawArraysUnbuffered(GL_POINTS, 0, 4);
	}
}

ZL_String ZL_Display::KeyScancodeName(ZL_Key key)
{
	static const char* ZL_Scancode_Names[] = {
		/*   0 */ 0, 0, 0, 0, "A", "B", "C", "D", "E", "F",
		/*  10 */ "G", "H", "I", "J", "K", "L", "M", "N", "O", "P",
		/*  20 */ "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
		/*  30 */ "1", "2", "3", "4", "5", "6", "7", "8", "9", "0",
		/*  40 */ "Return", "Escape", "Backspace", "Tab", "Space", "-", "=", "[", "]", "\\",
		/*  50 */ "#", ";", "'", "`", ",", ".", "/", "CapsLock", "F1", "F2",
		/*  60 */ "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12",
		/*  70 */ "PrintScreen", "ScrollLock", "Pause", "Insert", "Home", "PageUp", "Delete", "End", "PageDown", "Right",
		/*  80 */ "Left", "Down", "Up", "Numlock", "Keypad /", "Keypad *", "Keypad -", "Keypad +", "Keypad Enter", "Keypad 1",
		/*  90 */ "Keypad 2", "Keypad 3", "Keypad 4", "Keypad 5", "Keypad 6", "Keypad 7", "Keypad 8", "Keypad 9", "Keypad 0", "Keypad .",
		/* 100 */ "\\", "Application", "Power", "Keypad =", "F13", "F14", "F15", "F16", "F17", "F18",
		/* 110 */ "F19", "F20", "F21", "F22", "F23", "F24", "Execute", "Help", "Menu", "Select",
		/* 120 */ "Stop", "Again", "Undo", "Cut", "Copy", "Paste", "Find", "Mute", "VolumeUp", "VolumeDown",
		/* 130 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		/* 150 */ 0, 0, 0, 0, 0, "Cancel", "Clear", "Prior", "Return", "Separator",
		/* 160 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		/* 190 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		/* 220 */ 0, 0, 0, 0, "Left Ctrl", "Left Shift", "Left Alt", "Left GUI", "Right Ctrl", "Right Shift",
		/* 230 */ "Right Alt", "Right GUI", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		/* 250 */ 0, 0, 0, 0, 0, 0, 0, "ModeSwitch", "AudioNext", "AudioPrev",
		/* 260 */ "AudioStop", "AudioPlay", "AudioMute", "MediaSelect", "WWW", "Mail", "Calculator", "Computer", "AC Search", "AC Home",
		/* 270 */ "AC Back", "AC Forward", "AC Stop", "AC Refresh", "AC Bookmarks", "BrightnessDown", "BrightnessUp", "DisplaySwitch", "KBDIllumToggle", "KBDIllumDown",
		/* 280 */ "KBDIllumUp", "Eject", "Sleep"
	};
	const char *name = ((int)key < 0 || (int)key > (int)(sizeof(ZL_Scancode_Names)/sizeof(ZL_Scancode_Names[0])) ? NULL : ZL_Scancode_Names[(int)key]);
	return (name ? ZL_String(name) : ZL_String::EmptyString);
}

bool ZL_Display::IsFullscreen() { return ZL_WINDOWFLAGS_HAS(ZL_WINDOW_FULLSCREEN)!=0; }
bool ZL_Display::IsMinimized() { return ZL_WINDOWFLAGS_HAS(ZL_WINDOW_MINIMIZED)!=0; }
bool ZL_Display::HasInputFocus() { return ZL_WINDOWFLAGS_HAS(ZL_WINDOW_INPUT_FOCUS)!=0; }
bool ZL_Display::HasMouseFocus() { return ZL_WINDOWFLAGS_HAS(ZL_WINDOW_MOUSE_FOCUS)!=0; }

void ZL_Display::SoftKeyboardToggle()
{
	#ifdef ZL_HAS_SOFTKEYBOARD
	ZL_SoftKeyboardToggle();
	#endif
}

void ZL_Display::SoftKeyboardShow()
{
	#ifdef ZL_HAS_SOFTKEYBOARD
	ZL_SoftKeyboardShow();
	#endif
}

void ZL_Display::SoftKeyboardHide()
{
	#ifdef ZL_HAS_SOFTKEYBOARD
	ZL_SoftKeyboardHide();
	#endif
}

bool ZL_Display::SoftKeyboardIsShown()
{
	#ifdef ZL_HAS_SOFTKEYBOARD
	return ZL_SoftKeyboardIsShown();
	#else
	return true;
	#endif
}

void ZL_Display::DeviceVibrate(int duration)
{
	#ifdef ZL_HAS_DEVICEVIBRATE
	ZL_DeviceVibrate(duration);
	#endif
}

bool ZL_Rectf::Overlaps( const ZL_Vector& c, scalar r ) const
{
	scalar s, d = 0;
	if      (c.x < left) { s = c.x - left; d += s*s; }
	else if (c.x > right) { s = c.x - right; d += s*s; }
	if      (c.y < low) { s = c.y - low; d += s*s; }
	else if (c.y > high) { s = c.y - high; d += s*s; }
	return !d || d <= r*r;
}

ZL_Vector ZL_Rectf::GetCorner(ZL_Origin::Type orCorner) const
{
	scalar x,y;
	switch (orCorner)
	{
		case ZL_Origin::TopLeft:      x = left;   y = high;   break;
		case ZL_Origin::TopCenter:    x = MidX(); y = high;   break;
		case ZL_Origin::TopRight:     x = right;  y = high;   break;
		case ZL_Origin::CenterLeft:   x = left;   y = MidY(); break;
		case ZL_Origin::Center:       x = MidX(); y = MidY(); break;
		case ZL_Origin::CenterRight:  x = right;  y = MidY(); break;
		case ZL_Origin::BottomLeft:   x = left;   y = low;    break;
		case ZL_Origin::BottomCenter: x = MidX(); y = low;    break;
		case ZL_Origin::BottomRight:  x = right;  y = low;    break;
		default:
			x = left + (right-left)*ZL_Origin::FromCustomGetX(orCorner);
			y = high - (high -low )*ZL_Origin::FromCustomGetY(orCorner);
	}
	return ZL_Vector(x, y);
}

// ------------------------------------------------------------------------------------------

#ifndef ZL_VIDEO_USE_GLSL
#include "ZL_Impl.h"
struct ZL_Shader_Impl : ZL_Impl { };
ZL_Shader::ZL_Shader(const char*, const char*, const char*, const char*) : impl(NULL) { }
ZL_IMPL_OWNER_DEFAULT_IMPLEMENTATIONS(ZL_Shader)
void ZL_Shader::Activate() { }
void ZL_Shader::SetUniform(scalar uni1) { }
void ZL_Shader::SetUniform(scalar uni1, scalar uni2) { }
void ZL_Shader::Deactivate() { }
struct ZL_PostProcess_Impl : ZL_Impl { };
ZL_PostProcess::ZL_PostProcess(const char*, bool, const char*, const char*) : impl(NULL) { }
ZL_IMPL_OWNER_DEFAULT_IMPLEMENTATIONS(ZL_PostProcess)
void ZL_PostProcess::Start(bool) { }
void ZL_PostProcess::Apply() { }
void ZL_PostProcess::Apply(scalar uni1) { }
void ZL_PostProcess::Apply(scalar uni1, scalar uni2) { }
#endif

// ------------------------------------------------------------------------------------------


#include "libtess2/tesselator.h"
#include "ZL_Impl.h"
#include "ZL_Surface.h"
#include "ZL_Texture_Impl.h"
struct ZL_Polygon_Impl : ZL_Impl
{
	struct TessElementPair { GLenum Mode; GLushort IdxEnd; TessElementPair(GLenum Mode, GLushort IdxEnd) : Mode(Mode), IdxEnd(IdxEnd) {} };
	struct TessElementGroup { std::vector<TessElementPair> TessElements; std::vector<GLushort> TessVerticeIdx; };
	TessElementGroup *fill, *border;
	std::vector<GLscalar> TessVertices;
	ZL_Surface_Impl *pSurfaceImpl;
	GLscalar *TessVerticesTexCoords;
	ZL_Rectf bbox;

	struct TessMemPool
	{
		TessMemPool(size_t init) : begin((char*)malloc(init)), end(begin), last(begin+init), OutsideAlloc(0) {}
		~TessMemPool() { /*printf("[TessMemPool] Free with unused amount: %d of %d - Outside: %d\n", last - end, last - begin, OutsideAlloc);*/ free(begin); }
		void Clear()   { /*printf("[TessMemPool] Clear with unused amount: %d of %d - Outside: %d\n", last - end, last - begin, OutsideAlloc);*/ end = begin; OutsideAlloc = 0; }
		char *begin, *end, *last; unsigned int OutsideAlloc;
		static void* Alloc(void* userdata, unsigned int size)
		{
			TessMemPool* self = (TessMemPool*)userdata;
			if (!size) return NULL;
			if (self->end + size >= self->last) { self->OutsideAlloc += size; /*printf("[TessMemPool] OUTSIDE ALLOC %d (HAVE: %d - TOTAL: %d)\n", size, self->last - self->end, self->last - self->begin);*/ return malloc(size); }
			char* ret = self->end;
			self->end += (size+0x7) & ~0x7; //align to 8 byte boundry
			//printf("[TessMemPool] USE %d (%d ~ %d)\n", size, ret - self->begin, self->end - self->begin);
			return ret;
		}
		static void Free(void* userdata, void* ptr)
		{
			TessMemPool* self = (TessMemPool*)userdata;
			if (ptr < self->begin || ptr >= self->last) { free(ptr); /*printf("[TessMemPool] OUTSIDE FREE\n");*/ }
		}
	};

	typedef void (*funcCreateContour)(TESStesselator* t, const void* data1, int data2);

	static void CreateContourSingle(TESStesselator* t, const ZL_Vector* pstart, int pnum)
	{
		tessAddContour(t, 2, pstart, sizeof(scalar)*2, pnum);
	}

	static void CreateContourVector(TESStesselator* t, const std::vector<ZL_Polygon::PointList> *contours, int)
	{
		for (std::vector<ZL_Polygon::PointList>::const_iterator it = contours->begin(); it != contours->end(); ++it)
			if (it->size() >= 3)
				tessAddContour(t, 2, &*it->begin(), sizeof(scalar)*2, (int)it->size());
	}

	static void CreateContourMulti(TESStesselator* t, ZL_Polygon::PointList** contours, int vnum)
	{
		for (ZL_Polygon::PointList **it = contours, **itEnd = it+vnum; it != itEnd; ++it)
			if ((*it)->size() >= 3)
				tessAddContour(t, 2, &*(*it)->begin(), sizeof(scalar)*2, (int)(*it)->size());
	}

	void CreateTesselation(ZL_Polygon::IntersectMode intersect, funcCreateContour CreateContour, const void* data1, int data2, size_t TotalPoints)
	{
		if (!border && !fill) return;
		//printf("--------------------------------------------------------------\nCreating tesselation with %d points\n", TotalPoints);
		int BaseBuckedSize = (8+((int)TotalPoints/8));
		TessMemPool MemPool(3072 + 256 * TotalPoints); //tesselation uses at least 3588 bytes of memory (for 1 contour with 3 points)
		TESSalloc ma;
		ma.memalloc = TessMemPool::Alloc;
		ma.memfree = TessMemPool::Free;
		ma.userData = &MemPool;
		ma.meshEdgeBucketSize   = BaseBuckedSize*2;
		ma.meshVertexBucketSize = BaseBuckedSize*2;
		ma.meshFaceBucketSize   = BaseBuckedSize;
		ma.dictNodeBucketSize   = BaseBuckedSize*2;
		ma.regionBucketSize     = BaseBuckedSize;
		ma.extraVertices        = BaseBuckedSize;

		const int nvp = 6;
		TessWindingRule WindingRule = (TessWindingRule)(TESS_WINDING_ODD + intersect);

		TESStesselator* t = tessNewTess(&ma);
		CreateContour(t, data1, data2);
		TESSreal norm[3] = { 0, 0, 1 };
		tessTesselate(t, WindingRule, (border ? TESS_BOUNDARY_CONTOURS : TESS_POLYGONS), nvp, 2, norm);
		GLushort IndexStart = (GLushort)TessVertices.size();
		TessVertices.insert(TessVertices.end(), tessGetVertices(t), tessGetVertices(t) + tessGetVertexCount(t) * 2);
		const int* elems = tessGetElements(t);

		if (border)
		{
			size_t BorderElementStart = border->TessElements.size();
			for (int nelems = tessGetElementCount(t), i = 0; i < nelems; ++i)
			{
				const int b = elems[i * 2], n = elems[i * 2 + 1], nsum = (i == 0 ? 0 : border->TessElements.back().IdxEnd);
				for (int j = 0; j < n; j++) border->TessVerticeIdx.push_back(IndexStart+b+j);
				border->TessElements.push_back(TessElementPair(GL_LINE_LOOP, nsum + n));
			}
			if (border->TessElements.empty()) { delete border; border = NULL; }
			if (MemPool.OutsideAlloc) tessDeleteTess(t);

			if (fill && border)
			{
				MemPool.Clear();
				t = tessNewTess(&ma);
				for (TessElementPair *itBegin = &border->TessElements[0], *it = itBegin+BorderElementStart, *itEnd = it+border->TessElements.size(); it != itEnd; ++it)
				{
					int b = (it == itBegin ? 0 : it[-1].IdxEnd), n = it[0].IdxEnd - b;
					tessAddContour(t, 2, &TessVertices[b*2], sizeof(float)*2, n);
				}
				tessTesselate(t, TESS_WINDING_POSITIVE, TESS_POLYGONS, nvp, 2, norm);
				const int *fill_elems = tessGetElements(t), *fill_vinds = tessGetVertexIndices(t);
				for (int i = 0, iMax = tessGetElementCount(t), n; i != iMax; ++i)
				{
					const int* p = &fill_elems[i * nvp], nsum = (i == 0 ? 0 : fill->TessElements.back().IdxEnd);
					for (n = 0; n < nvp && p[n] != TESS_UNDEF; ++n) fill->TessVerticeIdx.push_back(IndexStart + fill_vinds[p[n]]);
					fill->TessElements.push_back(TessElementPair(GL_TRIANGLE_FAN, nsum + n));
				}
				if (fill->TessElements.empty()) { delete fill; fill = NULL; }
			}
		}
		else if (fill)
		{
			for (int i = 0, iMax = tessGetElementCount(t), n; i != iMax; ++i)
			{
				const int* p = &elems[i * nvp], nsum = (i == 0 ? 0 : fill->TessElements.back().IdxEnd);
				for (n = 0; n < nvp && p[n] != TESS_UNDEF; ++n) fill->TessVerticeIdx.push_back(IndexStart + p[n]);
				fill->TessElements.push_back(TessElementPair(GL_TRIANGLE_FAN, nsum + n));
			}
			if (fill->TessElements.empty()) { delete fill; fill = NULL; }
			if (MemPool.OutsideAlloc) tessDeleteTess(t);
		}

		CalculateBBox(IndexStart);
		CreateSurfaceTextureCoords(IndexStart);
	}

	void CalculateBBox(size_t IndexStart = 0)
	{
		if (!fill && !border) return;
		size_t i = IndexStart;
		if (!i) { bbox = ZL_Rectf(TessVertices[0], TessVertices[1], ZL_Vector()); i += 2; }
		for (size_t iEnd = TessVertices.size(); i < iEnd; i+= 2)
		{
			if      (TessVertices[i+0] < bbox.left ) bbox.left  = TessVertices[i+0];
			else if (TessVertices[i+0] > bbox.right) bbox.right = TessVertices[i+0];
			if      (TessVertices[i+1] < bbox.low  ) bbox.low   = TessVertices[i+1];
			else if (TessVertices[i+1] > bbox.high ) bbox.high  = TessVertices[i+1];
		}
	}

	void CreateSurfaceTextureCoords(int IndexStart = 0)
	{
		if (!fill || !pSurfaceImpl) return;
		//create texture coordinates
		scalar boxw = bbox.Width(), boxh = bbox.Height();
		scalar texw = ((pSurfaceImpl->tex->wraps == GL_REPEAT) ? (boxw/s(pSurfaceImpl->tex->wRep*pSurfaceImpl->fScaleW)) : s(1)/pSurfaceImpl->fScaleW);
		scalar texh = ((pSurfaceImpl->tex->wraps == GL_REPEAT) ? (boxh/s(pSurfaceImpl->tex->hRep*pSurfaceImpl->fScaleH)) : s(1)/pSurfaceImpl->fScaleH);
		TessVerticesTexCoords = (GLscalar*)realloc(TessVerticesTexCoords, sizeof(GLscalar)*TessVertices.size());
		for (size_t iEnd = TessVertices.size(), i = IndexStart; i < iEnd; i += 2)
		{
			TessVerticesTexCoords[i+0] = (TessVertices[i+0] - bbox.left) * texw / boxw;
			TessVerticesTexCoords[i+1] = (TessVertices[i+1] - bbox.low ) * texh / boxh;
		}
	}

	ZL_Polygon_Impl(bool withFill, bool withBorder, const ZL_Surface* pSurface = NULL) : fill(withFill ? new TessElementGroup() : NULL), border(withBorder ? new TessElementGroup() : NULL), pSurfaceImpl(pSurface ? ZL_ImplFromOwner<ZL_Surface_Impl>(*pSurface) : NULL), TessVerticesTexCoords(NULL)
	{
		if (pSurfaceImpl) pSurfaceImpl->AddRef();
	}

	~ZL_Polygon_Impl()
	{
		if (fill) delete fill;
		if (pSurfaceImpl) pSurfaceImpl->DelRef();
		if (border) delete border;
		if (TessVerticesTexCoords) free(TessVerticesTexCoords);
	}

	void AddSingleContour(const ZL_Vector *p, int pnum, ZL_Polygon::IntersectMode selfintersect)
	{
		CreateTesselation(selfintersect, (funcCreateContour)&CreateContourSingle, p, pnum, pnum);
	}

	void AddVectorContour(const std::vector<ZL_Polygon::PointList> &contours, ZL_Polygon::IntersectMode intersect)
	{
		size_t TotalCount = 0;
		for (std::vector<ZL_Polygon::PointList>::const_iterator it = contours.begin(); it != contours.end(); ++it)
			if (it->size() >= 3) TotalCount += it->size();
		CreateTesselation(intersect, (funcCreateContour)&CreateContourVector, &contours, 0, TotalCount);
	}

	void AddMultiContour(ZL_Polygon::PointList** contours, int vnum, ZL_Polygon::IntersectMode intersect)
	{
		size_t TotalCount = 0;
		for (ZL_Polygon::PointList **it = contours, **itEnd = it+vnum; it != itEnd; ++it)
			if ((*it)->size() >= 3) TotalCount += (*it)->size();
		CreateTesselation(intersect, (funcCreateContour)&CreateContourMulti, contours, vnum, TotalCount);
	}

	void AddExtrudedOutline(const ZL_Vector *p, int pnum, scalar offsetout, scalar offsetin, bool offsetjoints, bool loop, scalar capscale)
	{
		if (pnum < (loop ? 3 : 2)) return;

		if (!fill) fill = new TessElementGroup();
		GLushort VtxIndexStart = (GLushort)(TessVertices.size());

		//create vertices and indexes for offset fill
		for (GLushort i = 0; i < pnum; i++)
		{
			ZL_Vector x;
			const ZL_Vector &p2 = p[i];
			scalar in = offsetin, out = offsetout;
			if (!loop && (i == 0 || i == pnum-1))
			{
				x = (i ? (p[i-1] - p2) : (p2 - p[1])).Perp().Norm();
				in *= capscale, out *= capscale;
			}
			else
			{
				const ZL_Vector &p1 = p[(pnum+i-1)%pnum], &p3 = p[(i+1)%pnum];
				ZL_Vector a = (p2 - p1), b = (p3 - p2);
				ZL_Vector az = a.VecNorm(), bz = b.VecNorm();
				if (sabs(az.CrossP(bz)) < FLT_EPSILON*20) continue; //skip parallel edge
				az.Perp().Add(p1); bz.Perp().Add(p2);
				a *= b.CrossP(az - bz) / a.CrossP(b);
				x = ZL_Vector(p2.x - az.x - a.x, p2.y - az.y - a.y);
			}
			GLushort idx = (GLushort)(TessVertices.size()/2);
			ZL_Vector p2i(in  ? p2 + (offsetjoints ? x.VecWithLength(in)  : x*in)  : p2);
			ZL_Vector p2o(out ? p2 + (offsetjoints ? x.VecWithLength(out) : x*out) : p2);
			TessVertices.push_back(p2i.x); TessVertices.push_back(p2i.y); TessVertices.push_back(p2o.x); TessVertices.push_back(p2o.y);
			if (loop || i != pnum-1)
			{
				fill->TessVerticeIdx.push_back(idx+2);
				fill->TessVerticeIdx.push_back(idx+0);
				fill->TessVerticeIdx.push_back(idx+3);
				fill->TessVerticeIdx.push_back(idx+1);
				fill->TessElements.push_back(TessElementPair(GL_TRIANGLE_STRIP, (GLushort)fill->TessVerticeIdx.size()));
			}
		}
		CalculateBBox();
		if (loop)
		{
			if (TessVertices.size() - VtxIndexStart == 0) { if (!TessVertices.size()) { delete fill; fill = NULL; } return; } //only a line with parallel edges...
			//add the first pair again to close the loop
			TessVertices.push_back(TessVertices[VtxIndexStart+0]); TessVertices.push_back(TessVertices[VtxIndexStart+1]);
			TessVertices.push_back(TessVertices[VtxIndexStart+2]); TessVertices.push_back(TessVertices[VtxIndexStart+3]);
		}

		if (pSurfaceImpl)
		{
			//create texture coordinates
			bool rep = (pSurfaceImpl->tex->wraps == GL_REPEAT);
			scalar lentotal = 0, w2 = (rep ? s(pSurfaceImpl->tex->wRep*pSurfaceImpl->fScaleW*2) : 1);
			TessVerticesTexCoords = (GLscalar*)realloc(TessVerticesTexCoords, sizeof(GLscalar)*TessVertices.size());

			const int divisor = (pSurfaceImpl->tex->filtermag == GL_NEAREST ? 12 : 2);
			GLscalar TexCordOff = (GLscalar)(divisor+1) / (pSurfaceImpl->tex->hTex*divisor);
			for (GLushort i = VtxIndexStart, iEnd = (GLushort)(TessVertices.size()-4); i < iEnd; i += 4)
			{
				scalar from, to;
				if (rep)
				{
					from = lentotal;
					ZL_Vector l1(TessVertices[i+0]+TessVertices[i+2], TessVertices[i+1]+TessVertices[i+3]);
					ZL_Vector l2(TessVertices[i+4]+TessVertices[i+6], TessVertices[i+5]+TessVertices[i+7]);
					to = (lentotal += (l1.GetDistance(l2)/w2));
					if (loop && i == iEnd-4) { to += s(.6); to -= smod(to, 1); }
				}
				else { from = 0; to = 1; }
				TessVerticesTexCoords[i+0] = TessVerticesTexCoords[i+2] = from;
				TessVerticesTexCoords[i+1] = TessVerticesTexCoords[i+5] = 0+TexCordOff;
				TessVerticesTexCoords[i+3] = TessVerticesTexCoords[i+7] = 1-TexCordOff;
				TessVerticesTexCoords[i+4] = TessVerticesTexCoords[i+6] = to;
			}
		}
	}

	void AddExtrudedOutlineFromOtherBorder(const ZL_Polygon_Impl* other, scalar offsetout, scalar offsetin, bool offsetjoints, bool loop, scalar capscale)
	{
		if (!other->border) return;
		GLushort i=0; for (TessElementPair *it = &other->border->TessElements[0], *itEnd = it+other->border->TessElements.size(); it != itEnd; i = it->IdxEnd, ++it)
		{
			ZL_Polygon::PointList PointList;
			other->GetBorderOfPoints(i, it->IdxEnd, PointList);
			AddExtrudedOutline(&PointList[0], (int)PointList.size(), offsetout, offsetin, offsetjoints, loop, capscale);
		}
	}

	void Draw(const ZL_Color &color_border, const ZL_Color &color_fill)
	{
		if (!fill && !border) return;
		GLushort i;
		ZLGL_DISABLE_TEXTURE();
		ZLGL_VERTEXTPOINTER(2, GL_SCALAR, 0, &TessVertices[0]);
		if (color_fill.a && fill)
		{
			ZLGL_COLOR(color_fill);
			i=0; for (ZL_Polygon_Impl::TessElementPair* it = &fill->TessElements[0], *itEnd = it+fill->TessElements.size(); it != itEnd; ++it)
			{ glDrawElementsUnbuffered(it->Mode, it->IdxEnd - i, GL_UNSIGNED_SHORT, &fill->TessVerticeIdx[i]); i = it->IdxEnd; }
		}

		if (color_border.a && !pSurfaceImpl && border)
		{
			ZLGL_COLOR(color_border);
			i=0; for (ZL_Polygon_Impl::TessElementPair *it = &border->TessElements[0], *itEnd = it+border->TessElements.size(); it != itEnd; ++it)
			{ glDrawElementsUnbuffered(it->Mode, it->IdxEnd - i, GL_UNSIGNED_SHORT, &border->TessVerticeIdx[i]); i = it->IdxEnd; }
			glDrawArraysUnbuffered(GL_POINTS, 0, (GLsizei)(TessVertices.size()/2));
		}
	}

	void Draw(ZL_Surface_Impl* pSurfaceImpl)
	{
		if (!pSurfaceImpl || !fill || !TessVerticesTexCoords) return;
		GLushort i;
		ZLGL_ENABLE_TEXTURE();
		glBindTexture(GL_TEXTURE_2D, pSurfaceImpl->tex->gltexid);
		ZLGL_COLORA(pSurfaceImpl->color, pSurfaceImpl->fOpacity);
		ZLGL_TEXCOORDPOINTER(2, GL_SCALAR, 0, TessVerticesTexCoords);
		ZLGL_VERTEXTPOINTER(2, GL_SCALAR, 0, &TessVertices[0]);
		i=0; for (ZL_Polygon_Impl::TessElementPair *it = &fill->TessElements[0], *itEnd = it+fill->TessElements.size(); it != itEnd; ++it)
		{ glDrawElementsUnbuffered(it->Mode, it->IdxEnd - i, GL_UNSIGNED_SHORT, &fill->TessVerticeIdx[i]); i=it->IdxEnd; }
	}

	size_t GetBorders(std::vector<ZL_Polygon::PointList>& out) const
	{
		if (!border) return 0;
		GLushort i=0; for (TessElementPair *it = &border->TessElements[0], *itEnd = it+border->TessElements.size(); it != itEnd; i = it->IdxEnd, ++it)
		{
			out.push_back(ZL_Polygon::PointList());
			GetBorderOfPoints(i, it->IdxEnd, out.back());
		}
		return out.size();
	}

	bool GetBorder(ZL_Polygon::PointList& out) const
	{
		if (!border) return false;
		GetBorderOfPoints(0, border->TessElements[0].IdxEnd, out);
		return true;
	}

	void GetBorderOfPoints(GLushort begin, GLushort end, ZL_Polygon::PointList& out) const
	{
		out.clear();
		out.reserve(end-begin);
		const GLscalar *vtx = &TessVertices[0];
		for (GLushort *idx = &border->TessVerticeIdx[0], *idxREnd = idx+begin-1, *idxIt = idx+end-1; idxIt != idxREnd; idxIt--)
		{
			const GLscalar *v = vtx+(*idxIt*2);
			out.push_back(ZL_Vector(v[0], v[1]));
		}
	}
};

ZL_IMPL_OWNER_DEFAULT_IMPLEMENTATIONS(ZL_Polygon)
ZL_Polygon::ZL_Polygon(ColoredMode mode) : impl(new ZL_Polygon_Impl(!!(mode & FILL), !!(mode & BORDER))) { }
ZL_Polygon::ZL_Polygon(const ZL_Surface& surface, bool withBorder) : impl(new ZL_Polygon_Impl(true, withBorder, &surface)) { }
ZL_Polygon& ZL_Polygon::Add(const ZL_Vector *p, int pnum, IntersectMode selfintersect)  { if (impl) impl->AddSingleContour(p, pnum, selfintersect); return *this; }
ZL_Polygon& ZL_Polygon::Add(const PointList &contour, IntersectMode selfintersect) { if (impl) impl->AddSingleContour((contour.empty() ? NULL : &contour[0]), (int)contour.size(), selfintersect); return *this; }
ZL_Polygon& ZL_Polygon::Add(const std::vector<PointList> &contours, IntersectMode intersect) { if (impl) impl->AddVectorContour(contours, intersect); return *this; }
ZL_Polygon& ZL_Polygon::Add(PointList** contours, int cnum, IntersectMode intersect) { if (impl) impl->AddMultiContour(contours, cnum, intersect); return *this; }
ZL_Polygon& ZL_Polygon::Extrude(const ZL_Vector *p, int pnum, scalar offsetout, scalar offsetin, bool offsetjoints, bool loop, bool ccw, scalar capscale) { if (impl && p) impl->AddExtrudedOutline(p, pnum, (ccw ? offsetout : -offsetout), (ccw ? offsetin : -offsetin), offsetjoints, loop, capscale); return *this; }
ZL_Polygon& ZL_Polygon::Extrude(const PointList &contour, scalar offsetout, scalar offsetin, bool offsetjoints, bool loop, bool ccw, scalar capscale) { if (impl && !contour.empty()) impl->AddExtrudedOutline(&contour[0], (int)contour.size(), (ccw ? offsetout : -offsetout), (ccw ? offsetin : -offsetin), offsetjoints, loop, capscale); return *this; }
ZL_Polygon& ZL_Polygon::ExtrudeFromBorder(const ZL_Polygon& source, scalar offsetout, scalar offsetin, bool offsetjoints, bool loop, bool ccw, scalar capscale) { if (impl && source.impl) impl->AddExtrudedOutlineFromOtherBorder(source.impl, (ccw ? offsetout : -offsetout), (ccw ? offsetin : -offsetin), offsetjoints, loop, capscale); return *this; }

void ZL_Polygon::Draw(const ZL_Color &color_border, const ZL_Color &color_fill) const
{
	if (impl) impl->Draw(color_border, color_fill);
}

void ZL_Polygon::Fill(const ZL_Color &color_fill) const
{
	if (impl) impl->Draw(ZLTRANSPARENT, color_fill);
}

ZL_Polygon& ZL_Polygon::SetSurfaceColor(const ZL_Color &color)
{
	if (!impl || !impl->pSurfaceImpl) return *this;
	impl->pSurfaceImpl->color = color;
	return *this;
}

void ZL_Polygon::Draw() const
{
	if (impl && impl->pSurfaceImpl) impl->Draw(impl->pSurfaceImpl);
}

void ZL_Polygon::Draw(const ZL_Surface& surface) const
{
	if (impl && impl->pSurfaceImpl) impl->Draw(ZL_ImplFromOwner<ZL_Surface_Impl>(surface));
}

ZL_Surface ZL_Polygon::GetSurface() const
{
	return ZL_ImplMakeOwner<ZL_Surface>(impl->pSurfaceImpl, true);
}

const ZL_Rectf& ZL_Polygon::GetBoundingBox() const
{
	assert(impl); //can only be called on valid polygon instances
	return impl->bbox;
}

void ZL_Polygon::Clear()
{
	if (!impl) return;
	if (impl->fill) { impl->fill->TessElements.clear(); impl->fill->TessVerticeIdx.clear(); }
	if (impl->border) { impl->border->TessElements.clear(); impl->border->TessVerticeIdx.clear(); }
	impl->TessVertices.clear();
}

void ZL_Polygon::RemoveBorder()
{
	if (impl && impl->border) { delete impl->border; impl->border = NULL; }
}

size_t ZL_Polygon::GetBorders(std::vector<PointList>& out) const { return (impl ? impl->GetBorders(out) : 0); }
bool ZL_Polygon::GetBorder(std::vector<ZL_Vector>& out) const { return (impl ? impl->GetBorder(out) : false); }

size_t ZL_Polygon::GetBorders(const std::vector<PointList>& contours, std::vector<PointList>& out, IntersectMode intersect)      { ZL_Polygon_Impl impl(false, true); impl.AddVectorContour(contours, intersect);      return impl.GetBorders(out); }
bool ZL_Polygon::GetBorder(const std::vector<PointList>& contours, std::vector<ZL_Vector>& out, IntersectMode intersect)         { ZL_Polygon_Impl impl(false, true); impl.AddVectorContour(contours, intersect);      return impl.GetBorder(out);  }
size_t ZL_Polygon::GetBorders(std::vector<ZL_Vector>** contours, int cnum, std::vector<PointList>& out, IntersectMode intersect) { ZL_Polygon_Impl impl(false, true); impl.AddMultiContour(contours, cnum, intersect); return impl.GetBorders(out); }
bool ZL_Polygon::GetBorder(std::vector<ZL_Vector>** contours, int cnum, std::vector<ZL_Vector>& out, IntersectMode intersect)    { ZL_Polygon_Impl impl(false, true); impl.AddMultiContour(contours, cnum, intersect); return impl.GetBorder(out); }
