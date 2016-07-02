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

#ifndef __ZL_DISPLAY3D__
#define __ZL_DISPLAY3D__

#include "ZL_Display.h"
#include "ZL_Surface.h"
#include "ZL_Math3D.h"
#include "ZL_Data.h"

namespace ZL_MaterialModes { enum
{
	//Material modes
	MM_STATICCOLOR = 0x1, MM_VERTEXCOLOR = 0x2, MM_DIFFUSEMAP = 0x4, MM_DIFFUSEFUNC = 0x8,
	MM_LIT = 0x10, MM_SPECULARSTATIC = 0x20, MM_SPECULARMAP = 0x40, MM_SPECULARFUNC = 0x80, MM_NORMALMAP = 0x100, MM_NORMALFUNC = 0x200,
	MM_SHADOWMAP = 0x400, MM_PARALLAXMAP = 0x800,
	MM_VERTEXCOLORFUNC = 0x1000, MM_POSITIONFUNC = 0x2000, MM_DISPLACEMENTFUNC = 0x4000,
	//Material requests for custom shader code
	MR_POSITION = 0x100000, MR_TEXCOORD = 0x200000, MR_NORMALS = 0x400000, MR_CAMERATANGENT = 0x800000, MR_PRECISIONTANGENT = 0x1000000, MR_TIME = 0x2000000
};};

//Shader code variable names (attributes, for vertex shader)
#define Z3A_POSITION         ZL_SHADERVARNAME("ap", "a_position")       //input mesh vertex position (vec3)
#define Z3A_NORMAL           ZL_SHADERVARNAME("ab", "a_normal")         //input mesh normal vector (vec3)
#define Z3A_TEXCOORD         ZL_SHADERVARNAME("at", "a_texcoord")       //input mesh texture coordinate (vec2)
#define Z3A_TANGENT          ZL_SHADERVARNAME("ag", "a_tangent")        //input mesh tangent vector (vec3)
#define Z3A_COLOR            ZL_SHADERVARNAME("ac", "a_color")          //input mesh vertex color (vec4)

//Shader code variable names (uniforms) (default values are 1 if not specified)
#define Z3U_VIEW             ZL_SHADERVARNAME("umv", "u_view")          //view matrix (camera+projection) (mat4)
#define Z3U_MODEL            ZL_SHADERVARNAME("umm", "u_model")         //model matrix (mat4)
#define Z3U_NORMAL           ZL_SHADERVARNAME("umn", "u_normal")        //normal matrix (inversed transposed model) (mat4)
#define Z3U_LIGHT            ZL_SHADERVARNAME("uml", "u_light")         //light transformation matrix (mat4)
#define Z3U_VIEWPOS          ZL_SHADERVARNAME("uvv", "u_viewpos")       //camera position (vec3)
#define Z3U_LIGHTPOS         ZL_SHADERVARNAME("uvlp", "u_lightpos")     //light position (vec3)
#define Z3U_LIGHTCOLOR       ZL_SHADERVARNAME("uvlc", "u_lightcolor")   //light color (vec3)
#define Z3U_DIFFUSEMAP       ZL_SHADERVARNAME("usd", "u_diffuse")       //diffuse texture (sampler2D)
#define Z3U_NORMALMAP        ZL_SHADERVARNAME("usn", "u_normalmap")     //normal texture (sampler2D)
#define Z3U_SPECULARMAP      ZL_SHADERVARNAME("uss", "u_specularmap")   //specular texture (sampler2D)
#define Z3U_PARALLAXMAP      ZL_SHADERVARNAME("usp", "u_parallaxmap")   //parallax texture (sampler2D)
#define Z3U_SHADOWMAP        ZL_SHADERVARNAME("ush", "u_shadowmap")     //calculated shadow map (sampler2D)
#define Z3U_COLOR            ZL_SHADERVARNAME("uvc", "u_color")         //static color (vec4)
#define Z3U_PARALLAXSCALE    ZL_SHADERVARNAME("ufp", "u_parallaxscale") //parallax mapping amount (float, default 0.05)
#define Z3U_AMBIENT          ZL_SHADERVARNAME("ufa", "u_ambient")       //ambient light amount (float, default 0.2)
#define Z3U_SPECULAR         ZL_SHADERVARNAME("ufs", "u_specular")      //specular amount (float, default 5)
#define Z3U_SHININESS        ZL_SHADERVARNAME("ufh", "u_shininess")     //specular shininess factor (float, default 16)
#define Z3U_TIME             ZL_SHADERVARNAME("ut", "u_time")           //time passed in seconds (float)

//Shader code variable names (varyings, set up by vertex shader, used in fragment shader)
#define Z3V_COLOR            ZL_SHADERVARNAME("co", "v_color")          //color as calculated by the vertex shader (vec4)
#define Z3V_TEXCOORD         ZL_SHADERVARNAME("t",  "v_texcoord")       //fragment texture coordinate (vec2)
#define Z3V_NORMAL           ZL_SHADERVARNAME("n",  "v_normal")         //fragment normal (vec3)
#define Z3V_POSITION         ZL_SHADERVARNAME("p",  "v_position")       //fragment position (vec3)
#define Z3V_CAMERATANGENT    ZL_SHADERVARNAME("ct", "v_cameratangent")  //camera tangent relative to fragment (vec3)
#define Z3V_LIGHTSPACE       ZL_SHADERVARNAME("ls", "v_lightspace")     //position in light space (vec3)
#define Z3V_TANGENT          ZL_SHADERVARNAME("nt",  "v_tangent")       //fragment tangent (vec3)
#define Z3V_BITANGENT        ZL_SHADERVARNAME("nb",  "v_bitangent")     //fragment binormal (vec3)

//Shader code variable names (statics, available for custom functions)
#define Z3S_NORMAL           ZL_SHADERVARNAME("N", "s_normal")          //final normal vector inside fragment shader (vec3)
#define Z3S_CAMERATANGENT    ZL_SHADERVARNAME("CT", "s_cameratangent")  //calculated camera tangent inside fragment shader (vec3)

//Shader code variable names (output)
#define Z3O_POSITION  "gl_Position"  //result of vertex shader (usually VIEW * MODEL * POSITION if not calculated or displaced) (vec4)
#define Z3O_FRAGCOLOR "gl_FragColor" //result color of fragment shader (vec4)

struct ZL_Material
{
	ZL_Material();
	ZL_Material(unsigned int MaterialModes);
	ZL_Material(unsigned int MaterialModes, const char* CustomFragmentCode, const char* CustomVertexCode = NULL);
	~ZL_Material();
	ZL_Material(const ZL_Material &source);
	ZL_Material &operator=(const ZL_Material &source);
	operator bool () const { return (impl!=NULL); }
	bool operator==(const ZL_Material &b) const { return (impl==b.impl); }
	bool operator!=(const ZL_Material &b) const { return (impl!=b.impl); }

	ZL_Material MakeNewMaterialInstance();
	ZL_Material& SetUniformFloat(ZL_NameID Name, scalar val);
	ZL_Material& SetUniformVec2(ZL_NameID Name, const ZL_Vector& val);
	ZL_Material& SetUniformVec3(ZL_NameID Name, const ZL_Vector3& val);
	ZL_Material& SetUniformVec4(ZL_NameID Name, const ZL_Color& val);
	ZL_Material& SetDiffuseTexture(ZL_Surface& srf);
	ZL_Material& SetNormalTexture(ZL_Surface& srf);
	ZL_Material& SetSpecularTexture(ZL_Surface& srf);
	ZL_Material& SetParallaxTexture(ZL_Surface& srf);

	private: struct ZL_Material_Impl* impl;
};

struct ZL_Mesh
{
	ZL_Mesh();
	ZL_Mesh(const ZL_FileLink& ModelFile);
	~ZL_Mesh();
	ZL_Mesh(const ZL_Mesh &source);
	ZL_Mesh &operator=(const ZL_Mesh &source);
	operator bool () const { return (impl!=NULL); }
	bool operator==(const ZL_Mesh &b) const { return (impl==b.impl); }
	bool operator!=(const ZL_Mesh &b) const { return (impl!=b.impl); }
	static ZL_Mesh FromPLY(const ZL_FileLink& PLYFile);
	static ZL_Mesh FromOBJ(const ZL_FileLink& OBJFile);
	
	ZL_Material GetMaterial(unsigned int PartNumber = 0) const;
	ZL_Material GetMaterial(ZL_NameID PartName) const;
	ZL_Mesh& SetMaterial(unsigned int PartNumber, const ZL_Material& Material);
	ZL_Mesh& SetMaterial(ZL_NameID PartName, const ZL_Material& Material);
	ZL_Mesh& SetMaterial(const ZL_Material& Material);

	void Draw(const ZL_Matrix& Matrix, const struct ZL_Camera* Camera, const struct ZL_Light* Light);

	static ZL_Mesh BuildPlane(ZL_Vector Extents);
	static ZL_Mesh BuildLandscape();
	static ZL_Mesh BuildSphere(scalar Radius, int Segments, bool Inside = false);

	#if defined(ZILLALOG) && !defined(ZL_VIDEO_OPENGL_ES2)
	//Debug drawing is only available on desktop debug builds
	void DrawDebug(const ZL_Matrix& Matrix, const struct ZL_Camera& Camera);
	#endif

	protected: struct ZL_Mesh_Impl* impl;
};

struct ZL_MeshAnimated : public ZL_Mesh
{
	ZL_MeshAnimated();
	ZL_MeshAnimated(const ZL_FileLink& AnimZipFile);
	~ZL_MeshAnimated();
	ZL_MeshAnimated(const ZL_MeshAnimated &source);
	ZL_MeshAnimated &operator=(const ZL_MeshAnimated &source);
	operator bool () const { return (impl!=NULL); }
	bool operator==(const ZL_MeshAnimated &b) const { return (impl==b.impl); }
	bool operator!=(const ZL_MeshAnimated &b) const { return (impl!=b.impl); }

	ZL_MeshAnimated& SetFrame(unsigned int FrameIndex);
};

struct ZL_Camera
{
	ZL_Camera();
	~ZL_Camera();
	ZL_Camera(const ZL_Camera &source);
	ZL_Camera &operator=(const ZL_Camera &source);
	operator bool () const { return (impl!=NULL); }
	bool operator==(const ZL_Camera &b) const { return (impl==b.impl); }
	bool operator!=(const ZL_Camera &b) const { return (impl!=b.impl); }

	ZL_Camera& SetPosition(const ZL_Vector3& pos);
	ZL_Camera& SetDirection(const ZL_Vector3& dir);
	ZL_Camera& SetFOV(scalar fov);
	ZL_Camera& SetClipPlane(scalar znear, scalar zfar);
	ZL_Matrix& GetVPMatrix();
	const ZL_Matrix& GetVPMatrix() const;
	ZL_Vector3 GetPosition() const;
	ZL_Vector3 GetDirection() const;
	scalar GetFOV() const;
	scalar GetNearClipPlane() const;
	scalar GetFarClipPlane() const;

	private: struct ZL_Camera_Impl* impl;
};

struct ZL_Light
{
	ZL_Light();
	~ZL_Light();
	ZL_Light(const ZL_Light &source);
	ZL_Light &operator=(const ZL_Light &source);
	operator bool () const { return (impl!=NULL); }
	bool operator==(const ZL_Light &b) const { return (impl==b.impl); }
	bool operator!=(const ZL_Light &b) const { return (impl!=b.impl); }

	ZL_Light& SetPosition(const ZL_Vector3& pos);
	ZL_Light& SetDirection(const ZL_Vector3& dir);
	ZL_Light& SetColor(const ZL_Color& color);
	ZL_Vector3 GetPosition() const;
	ZL_Vector3 GetDirection() const;
	ZL_Color GetColor() const;

	private: struct ZL_Light_Impl* impl;
};

struct ZL_RenderList
{
	ZL_RenderList();
	~ZL_RenderList();
	ZL_RenderList(const ZL_RenderList &source);
	ZL_RenderList &operator=(const ZL_RenderList &source);
	operator bool () const { return (impl!=NULL); }
	bool operator==(const ZL_RenderList &b) const { return (impl==b.impl); }
	bool operator!=(const ZL_RenderList &b) const { return (impl!=b.impl); }

	void Reset();
	void Add(const ZL_Mesh& Mesh, const ZL_Matrix& Matrix);
	void AddReferenced(const ZL_Mesh& Mesh, const ZL_Matrix& Matrix); //keep mesh reference in list

	private: struct ZL_RenderList_Impl* impl;
};

struct ZL_Display3D
{
	//Initialize 3D rendering features
	static bool Init();
	static bool InitShadowMapping();

	//Draw prepared rendering lists
	static inline void DrawList(const ZL_RenderList& RenderList, const ZL_Camera& Camera) { const ZL_RenderList* p = &RenderList; DrawLists(&p, 1, Camera); }
	static inline void DrawListWithShadowMapping(const ZL_RenderList& RenderList, const ZL_Camera& Camera, const ZL_Light& Light) { const ZL_RenderList* p = &RenderList; DrawListsWithShadowMapping(&p, 1, Camera, Light); }
	static void DrawLists(const ZL_RenderList** RenderLists, size_t NumLists, const ZL_Camera& Camera);
	static void DrawListsWithShadowMapping(const ZL_RenderList** RenderLists, size_t NumLists, const ZL_Camera& Camera, const ZL_Light& Light);

	//Manual render state handling when issueing multiple draw list calls or when using the simple shape rendering functions below
	static void BeginRendering();
	static void FinishRendering();

	//Simple shape rendering
	static void DrawLine(const ZL_Camera& cam, const ZL_Vector3& a, const ZL_Vector3& b, const ZL_Color& color = ZL_Color::White, scalar width = 0.01f);
	static void DrawPlane(const ZL_Camera& cam, const ZL_Vector3& pos, const ZL_Vector3& normal, const ZL_Vector& extents, const ZL_Color& color = ZL_Color::White);
};

//returns glsl function 'float snoise(vec2)'
#define ZL_GLSL_IMPORTSNOISE() "vec3 snoiseprm(vec3 x){x*=((x*34.0)+1.0);return x-floor(x*(1.0/289.0))*289.0;}float snoise(vec2 v){const vec4 C=vec4(0.211324865405187,0.366025403784439,-0.577350269189626,0.024390243902439);vec2 i=floor(v+dot(v,C.yy)),y=v-i+dot(i,C.xx),j=((y.x>y.y)?vec2(1.0,0.0):vec2(0.0,1.0));i-=floor(i*(1.0/289.0))*289.0;vec3 p=snoiseprm(snoiseprm(i.y+vec3(0.0,j.y,1.0))+i.x+vec3(0.0,j.x,1.0));vec3 x=2.0*fract(p*C.www)-1.0,h=abs(x)-0.5,a=x-floor(x+0.5);vec4 z=vec4(y.x+C.x-j.x,y.y+C.x-j.y,y.x+C.z,y.y+C.z);vec3 m=max(0.5-vec3(dot(y,y),dot(z.xy,z.xy),dot(z.zw,z.zw)),0.0);m*=m;m*=m;m*=1.79284291400159-0.85373472095314*(a*a+h*h);return 0.5+65.0*dot(m,vec3(a.x*y.x+h.x*y.y,a.y*z.x+h.y*z.y,a.z*z.z+h.z*z.w));}"

#ifdef ZILLALOG
#define ZL_SHADERVARNAME(release_name, debug_name) debug_name
#else
#define ZL_SHADERVARNAME(release_name, debug_name) release_name
#endif

#endif //__ZL_DISPLAY3D__
