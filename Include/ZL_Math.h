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

#ifndef __ZL_MATH__
#define __ZL_MATH__

#include <math.h>
#include <float.h>
#include <stdlib.h>

//#define ZL_DOUBLE_PRECISCION

#ifndef ZL_DOUBLE_PRECISCION
	#undef PI
	#define PI          3.1415926535897932384626433832795f
	#define PI2         6.2831853071795864769252867665590f
	#define PIHALF      1.5707963267948966192313216916398f
	#define PIQUARTER   0.7853981633974483096156608458198f
	#define PIOVER180   0.0174532925199432957692369076848f
	#define PIUNDER180 57.2957795130823208767981548141052f
	#define HALF        0.5f
	#define S_MAX FLT_MAX
	#define S_MIN FLT_MIN
	#define S_EPSILON FLT_EPSILON

	typedef float scalar;
	#define ssqrt sqrtf
	#define ssin sinf
	#define scos cosf
	#define stan tanf
	#define sacos acosf
	#define satan2 atan2f
	#define sabs fabsf
	#define spow powf
	#define sexp expf
	#define smod fmodf
	#define sfloor floorf
	#define sceil ceilf
#else
	#undef PI
	#define PI          3.1415926535897932384626433832795
	#define PI2         6.2831853071795864769252867665590
	#define PIHALF      1.5707963267948966192313216916398
	#define PIQUARTER   0.7853981633974483096156608458198
	#define PIOVER180   0.0174532925199432957692369076848
	#define PIUNDER180 57.2957795130823208767981548141052
	#define HALF        0.5
	#define S_MAX DBL_MAX
	#define S_MIN DBL_MIN
	#define S_EPSILON DBL_EPSILON

	typedef double scalar;
	#define ssqrt sqrt
	#define ssin sin
	#define scos cos
	#define stan tan
	#define sacos acos
	#define satan2 atan2
	#define sabs abs
	#define spow pow
	#define sexp exp
	#define smod fmod
	#define sfloor floor
	#define sceil ceil
#endif

#if defined(__cplusplus) && (!defined(_MSC_VER) || _MSC_VER > 1200) //c-array element count macro
template <typename C_ARRAY, size_t N> char (&__COUNT_OF_HELPER(C_ARRAY(&)[N]))[N];
#define COUNT_OF(arr) (sizeof(__COUNT_OF_HELPER(arr)))
#else //same without c++, a bit less safe (runs without error given certain pointers)
#define COUNT_OF(arr) ((sizeof(arr)/sizeof(0[arr]))/((size_t)(!(sizeof(arr) % sizeof(0[arr])))))
#endif

// This header can be included by c code solely for the scalar definition
#ifdef __cplusplus

#if defined(_MSC_VER) && (_MSC_VER <= 1200)
#pragma warning(disable:4786) //'Some STL template class' : identifier was truncated to '255' characters in the debug information
#pragma warning(disable:4530) //C++ exception handler used, but unwind semantics are not enabled
#endif

#include <algorithm>
#include <vector>

#define s(a) ((scalar)(a))
#define ssign(a) ((a) < 0 ? -1 : 1)
#define ssign0(a) ((a) <= 0 ? ((a) ? -1 : 0) : 1)

#define ZLV(x,y) ZL_Vector(s(x), s(y))
#define ZLR(left,low,right,high) ZL_Rectf(s(left), s(low), s(right), s(high))

#define RAND_RANGE(min, max)     (ZL_Rand::Range(s(min), s(max)))                // >= min  && <= max
#define RAND_RANGE_MAX(max)      (ZL_Rand::Range(s(max)))                        // >= 0.0  && <= max
#define RAND_VARIATION(var)      (ZL_Rand::Variation(s(var)))                    // >= -var && <= +var
#define RAND_INT_MAX(max)        (ZL_Rand::Int((int)(max)))                      // >= 0    && <= max
#define RAND_INT_RANGE(min, max) (ZL_Rand::Int((int)(min), (int)(max)))          // >= min  && <= max
#define RAND_FACTOR              (ZL_Rand::Factor())                             // >= 0.0  && <= 1.0
#define RAND_BOOL                (ZL_Rand::Bool())                               // true or false
#define RAND_SIGN                (ZL_Rand::Sign())                               // +1 or -1
#define RAND_ANGLE               (ZL_Rand::Angle())                              // >= 0 && < PI2
#define RAND_ANGLEVEC            (ZL_Rand::AngleVec())                           // random rotation vector
#define RAND_POINTIN(rec)        (ZL_Rand::PointIn(rec))                         // random point inside rec
#define RAND_COLOR               (ZL_Color(RAND_FACTOR,RAND_FACTOR,RAND_FACTOR)) // random color (any brightness)
#define RAND_BRIGHTCOLOR         (ZL_Color::HSVA(RAND_FACTOR,1,1))               // random hued color with max brightness
#define RAND_CHANCE(val)         (ZL_Rand::Int((int)(val)-1)==0)                 // returns true one out of [val] times
#define RAND_ARRAYELEMENT(arr)   arr[ZL_Rand::Int(COUNT_OF(arr)-1)]              // random element of c-array
#define RAND_VECTORELEMENT(vec)  vec[ZL_Rand::Int(vec.size()-1)]                 // random element of vector

#undef MAX
#define MAX(a,b) (((a) > (b)) ? (a) : (b))

#undef MIN
#define MIN(a,b) (((a) < (b)) ? (a) : (b))

#define SMALL_NUMBER       ((scalar)(1.e-8f))
#define KINDA_SMALL_NUMBER ((scalar)(1.e-4f))

enum ZL_NoInitType { ZL_NoInit };

//Integer based 2d point
struct ZL_Point
{
	ZL_Point() : x(0), y(0) { }
	ZL_Point(int x, int y) : x(x), y(y) { }
	ZL_Point(const ZL_Point &p) : x(p.x), y(p.y) { }
	ZL_Point(const struct ZL_Vector &p);
	ZL_Point &operator+=(const ZL_Point &p) { x += p.x; y += p.y; return *this; }
	ZL_Point &operator-=(const ZL_Point &p) { x -= p.x; y -= p.y; return *this; }
	ZL_Point operator+(const ZL_Point &p) const { return ZL_Point(x + p.x, y + p.y); }
	ZL_Point operator-(const ZL_Point &p) const { return ZL_Point(x - p.x, y - p.y); }
	inline bool operator==(const ZL_Point &p) const { return (x == p.x) && (y == p.y); }
	inline bool operator!=(const ZL_Point &p) const { return (x != p.x) || (y != p.y); }
	inline bool operator!() const { return (x == 0 && y == 0); }
	int x,y;
	int GetDistance(const ZL_Point &p) const { return int(ssqrt(float((x-p.x)*(x-p.x) + (y-p.y)*(y-p.y))) + s(.5)); }
};

//Float based 2d vector (also used as points, positions and any kind of 2d coordinates)
struct ZL_Vector
{
	scalar x, y;

	//Construction
	inline ZL_Vector() : x(0), y(0) { }
	inline ZL_Vector(ZL_NoInitType) { }
	inline ZL_Vector(scalar x, scalar y) : x(x), y(y) { }
	inline ZL_Vector(const ZL_Point &p) : x((scalar)p.x), y((scalar)p.y) { }
	inline ZL_Vector(scalar x1, scalar y1, scalar x2, scalar y2) : x(x2-x1), y(y2-y1) { }
	inline ZL_Vector(const ZL_Vector &p1, const ZL_Vector &p2) : x(p2.x-p1.x), y(p2.y-p1.y) { }
	inline ZL_Vector(const ZL_Point &p1, const ZL_Point &p2) : x(s(p2.x-p1.x)), y(s(p2.y-p1.y)) { }

	//Comparison operators
	inline bool operator!() const { return (x == 0 && y == 0); }
	inline bool operator==(const ZL_Vector &p) const { return (x == p.x) && (y == p.y); }
	inline bool operator!=(const ZL_Vector &p) const { return (x != p.x) || (y != p.y); }

	//Math operators
	inline ZL_Vector operator-() const { return ZL_Vector(-x, -y); }
	inline ZL_Vector &operator+=(const ZL_Vector &p) { x += p.x; y += p.y; return *this; }
	inline ZL_Vector &operator-=(const ZL_Vector &p) { x -= p.x; y -= p.y; return *this; }
	inline ZL_Vector &operator*=(const ZL_Vector &p) { x *= p.x; y *= p.y; return *this; }
	inline ZL_Vector &operator/=(const ZL_Vector &p) { x /= p.x; y /= p.y; return *this; }
	inline ZL_Vector &operator+=(const scalar f) { x += f; y += f; return *this; }
	inline ZL_Vector &operator-=(const scalar f) { x -= f; y -= f; return *this; }
	inline ZL_Vector &operator*=(const scalar f) { x *= f; y *= f; return *this; }
	inline ZL_Vector &operator/=(const scalar f) { x /= f; y /= f; return *this; }
	inline ZL_Vector operator+(const ZL_Vector &p) const { return ZL_Vector(x + p.x, y + p.y); }
	inline ZL_Vector operator-(const ZL_Vector &p) const { return ZL_Vector(x - p.x, y - p.y); }
	inline ZL_Vector operator*(const ZL_Vector &p) const { return ZL_Vector(x * p.x, y * p.y); }
	inline ZL_Vector operator/(const ZL_Vector &p) const { return ZL_Vector(x / p.x, y / p.y); }
	inline ZL_Vector operator+(const scalar f) const { return ZL_Vector(x + f, y + f); }
	inline ZL_Vector operator-(const scalar f) const { return ZL_Vector(x - f, y - f); }
	inline ZL_Vector operator*(const scalar f) const { return ZL_Vector(x * f, y * f); }
	inline ZL_Vector operator/(const scalar f) const { return ZL_Vector(x / f, y / f); }
	inline scalar operator|(const ZL_Vector &p) const { return x * p.x + y * p.y; } //dot product
	inline scalar operator^(const ZL_Vector &p) const { return x * p.y - y * p.x; } //cross product

	//Length comparison operators  (faster than calling and comparing GetLength())
	bool operator<(const ZL_Vector &b) const { return ((x*x + y*y) < (b.x*b.x + b.y*b.y)); }
	bool operator<=(const ZL_Vector &b) const { return ((x*x + y*y) <= (b.x*b.x + b.y*b.y)); }
	bool operator>(const ZL_Vector &b) const { return ((x*x + y*y) > (b.x*b.x + b.y*b.y)); }
	bool operator>=(const ZL_Vector &b) const { return ((x*x + y*y) >= (b.x*b.x + b.y*b.y)); }
	bool operator==(const scalar l) const { return ((x*x + y*y) == (l*l)); }
	bool operator!=(const scalar l) const { return ((x*x + y*y) != (l*l));}
	bool operator<(const scalar l) const { return ((x*x + y*y) < (l*l)); }
	bool operator<=(const scalar l) const { return ((x*x + y*y) <= (l*l)); }
	bool operator>(const scalar l) const { return ((x*x + y*y) > (l*l)); }
	bool operator>=(const scalar l) const { return ((x*x + y*y) >= (l*l)); }

	//Non modifying operators as functions
	inline scalar GetDistance(const ZL_Vector &p) const { return ssqrt((x-p.x)*(x-p.x) + (y-p.y)*(y-p.y)); }
	inline scalar GetDistanceSq(const ZL_Vector &p) const { return ((x-p.x)*(x-p.x) + (y-p.y)*(y-p.y)); }
	inline scalar GetLength() const { return ssqrt(x*x + y*y); }
	inline scalar GetLengthSq() const { return (x*x + y*y); }
	inline scalar CompareDistance(const ZL_Vector &p, scalar l) const { return (x-p.x)*(x-p.x) + (y-p.y)*(y-p.y) - (l*l); }
	inline scalar DotP(const ZL_Vector &v) const { return x * v.x + y * v.y; } //dot product
	inline scalar CrossP(const ZL_Vector &v) const { return x * v.y - y * v.x; } //cross product
	inline bool Near(const ZL_Vector &p, scalar l) const { return (x-p.x)*(x-p.x) + (y-p.y)*(y-p.y) < (l*l); }
	inline bool Far(const ZL_Vector &p, scalar l) const { return (x-p.x)*(x-p.x) + (y-p.y)*(y-p.y) > (l*l); }
	inline bool AlmostZero(scalar ErrorTolerance = SMALL_NUMBER) const { return sabs(x)<ErrorTolerance && sabs(y)<ErrorTolerance; }
	inline bool AlmostEqual(const ZL_Vector &v, scalar ErrorTolerance = SMALL_NUMBER) const { return sabs(x-v.x)<ErrorTolerance && sabs(y-v.y)<ErrorTolerance; }

	//Self modifying operators as functions (non const call)
	inline ZL_Vector &Set(scalar x, scalar y) { this->x = x; this->y = y; return *this; }
	ZL_Vector &Rotate(scalar angle_rad);
	ZL_Vector &Rotate(const ZL_Vector &hotspot, scalar angle_rad);
	ZL_Vector &RotateDeg(scalar angle_deg);
	ZL_Vector &RotateDeg(const ZL_Vector &hotspot, scalar angle_deg);
	inline ZL_Vector &Norm() { return (x || y ? Div(GetLength()) : Set(1,0)); ; }
	inline ZL_Vector &NormUnsafe() { return Div(GetLength()); }
	inline ZL_Vector &Perp() { scalar oldx = x; x = -y; y = oldx; return *this; }
	inline ZL_Vector &RPerp() { scalar oldx = x; x = y; y = -oldx; return *this; }
	inline ZL_Vector &Add(const ZL_Vector &p) { x += p.x; y += p.y; return *this; }
	inline ZL_Vector &Sub(const ZL_Vector &p) { x -= p.x; y -= p.y; return *this; }
	inline ZL_Vector &Mul(const ZL_Vector &p) { x *= p.x; y *= p.y; return *this; }
	inline ZL_Vector &Div(const ZL_Vector &p) { x /= p.x; y /= p.y; return *this; }
	inline ZL_Vector &Add(const scalar f)     { x += f;   y += f;   return *this; }
	inline ZL_Vector &Sub(const scalar f)     { x -= f;   y -= f;   return *this; }
	inline ZL_Vector &Mul(const scalar f)     { x *= f;   y *= f;   return *this; }
	inline ZL_Vector &Div(const scalar f)     { x /= f;   y /= f;   return *this; }
	inline ZL_Vector &Mod(const ZL_Vector &v) { x=smod(x,v.x); y=smod(y,v.y); return *this; }
	inline ZL_Vector &Abs() { x = sabs(x); y = sabs(y); return *this; }
	inline ZL_Vector &Lerp(const ZL_Vector &v, const scalar t) { x = x+(v.x-x)*t, y = y+(v.y-y)*t; return *this; }
	inline ZL_Vector &SLerp(const ZL_Vector &v, const scalar t) { return Rotate(GetRelAngle(v)*t); }
	inline ZL_Vector &SLerpConst(const ZL_Vector &v, const scalar a) { scalar r = GetRelAngle(v); return Rotate(MIN(MAX(r, -a),a)); }
	ZL_Vector &SetLength(scalar length);
	ZL_Vector &AddLength(scalar length);
	ZL_Vector &SetMaxLength(scalar maximum_length);
	ZL_Vector &SetMinLength(scalar minimum_length);

	//Modifying operators as functions returning new instances (const call)
	ZL_Vector VecRotate(scalar angle_rad) const;
	ZL_Vector VecRotate(const ZL_Vector &hotspot, scalar angle_rad) const;
	ZL_Vector VecRotateDeg(scalar angle_deg) const;
	ZL_Vector VecRotateDeg(const ZL_Vector &hotspot, scalar angle_deg) const;
	inline ZL_Vector VecNorm() const { return (x || y ? *this / GetLength() : ZL_Vector(1,0)); }
	inline ZL_Vector VecNormUnsafe() const { return *this / GetLength(); }
	inline ZL_Vector VecPerp() const { return ZL_Vector(-y, x); }
	inline ZL_Vector VecRPerp() const { return ZL_Vector(y, -x); }
	inline ZL_Vector VecMod(const ZL_Vector &v) const { return ZL_Vector(smod(x,v.x), smod(y,v.y)); }
	inline ZL_Vector VecAbs() const { return ZL_Vector(sabs(x), sabs(y)); }
	inline ZL_Vector VecLerp(const ZL_Vector &v, const scalar f) const { return ZL_Vector(x+(v.x-x)*f, y+(v.y-y)*f); }
	inline ZL_Vector VecSLerp(const ZL_Vector &v, const scalar t) { return VecRotate(GetRelAngle(v)*t); }
	inline ZL_Vector VecSLerpConst(const ZL_Vector &v, const scalar a) { scalar r = GetRelAngle(v); return VecRotate(MIN(MAX(r, -a),a)); }
	ZL_Vector VecWithLength(scalar length) const;
	ZL_Vector VecWithAddLength(scalar length) const;
	ZL_Vector VecWithMaxLength(scalar maximum_length) const;
	ZL_Vector VecWithMinLength(scalar minimum_length) const;

	//Static functions returning new instances
	static inline ZL_Vector FromAngle(const scalar rad) { return ZL_Vector(scos(rad), ssin(rad)); }
	static inline ZL_Vector FromAngleDeg(const scalar deg) { return ZL_Vector(scos(deg*PIOVER180), ssin(deg*PIOVER180)); }
	static inline ZL_Vector Perp(const ZL_Vector &v) { return ZL_Vector(-v.y, v.x); }
	static inline ZL_Vector RPerp(const ZL_Vector &v) { return ZL_Vector(v.y, -v.x); }
	static inline ZL_Vector Abs(const ZL_Vector &v) { return ZL_Vector(sabs(v.x), sabs(v.y)); }
	static inline ZL_Vector Lerp(const ZL_Vector &from, const ZL_Vector &to, const scalar f) { return ZL_Vector(from.x+(to.x-from.x)*f, from.y+(to.y-from.y)*f); }
	static inline ZL_Vector SLerp(const ZL_Vector &from, const ZL_Vector &to, const scalar t) { return from.VecRotate(from.GetRelAngle(to)*t); }
	static inline ZL_Vector SLerpConst(const ZL_Vector &from, const ZL_Vector &to, const scalar a) { scalar r = from.GetRelAngle(to); return from.VecRotate(MIN(MAX(r, -a),a)); }

	//Returns angle in rad from 0 to PI*2 related to world coordinates (1,0)
	inline scalar GetAngle() const { scalar a=satan2(y,x);return(a<0?PI2+a:a); }

	//Returns angle in rad from 0 to PI*2 related to another vector
	inline scalar GetAngle(const ZL_Vector &v) const { scalar a=satan2(v.y,v.x)-satan2(y,x);return(a<0?PI2+a:a); }

	//Returns relative (nearest) angle in rad from -PI to +PI from this to another vector
	inline scalar GetRelAngle(const ZL_Vector &v) const { scalar a=satan2(v.y,v.x)-satan2(y,x);return(a<-PI?PI2+a:(a>PI?a-PI2:a)); }

	//Returns relative (nearest) angle in rad from -PI to +PI from this to another angle in rad
	inline scalar GetRelAngle(scalar rad) const { rad-=satan2(y,x);while(rad<-PI)rad+=PI2;while(rad>PI)rad-=PI2;return rad; }

	//Returns acute angle in rad from 0 to PI/2 between this and another vector
	inline scalar GetAcuteAngle(const ZL_Vector &v) const { scalar a=GetAngle(v);if(a>PI)a=PI2-a;return(a>PIHALF?PI-a:a); }

	//Returns angle in degree from 0 to 360 related to world coordinates (1,0)
	inline scalar GetAngleDeg() const { return GetAngle()*PIUNDER180; }

	//Returns angle in degree from 0 to 360 related to another vector
	inline scalar GetAngleDeg(const ZL_Vector &v) const { return GetAngle(v)*PIUNDER180; }

	//Returns relative (nearest) angle in degree from -180 to +180 from this to another vector
	inline scalar GetRelAngleDeg(const ZL_Vector &v) const { return GetRelAngle(v)*PIUNDER180; }

	//Returns relative (nearest) angle in degree from -180 to +180 from this to another angle in degree
	inline scalar GetRelAngleDeg(scalar deg) const { return GetRelAngle(deg*PIOVER180)*PIUNDER180; }

	//Returns acute angle in degree from 0 to 90 between this and another vector
	inline scalar GetAcuteAngleDeg(const ZL_Vector &v) const { return GetAcuteAngle(v)*PIUNDER180; }

	static const ZL_Vector Zero, One, Right, Up;
};

struct ZL_Plane
{
	ZL_Vector N; //unit normal
	scalar D; //distance from the plane to the origin from a normal and a point
	ZL_Plane() : N(1,0), D(0) {}
	ZL_Plane(const ZL_Vector& N, scalar D) : N(N), D(D) {}
	ZL_Plane(const ZL_Vector& p1, const ZL_Vector& p2) : N((p2-p1).Perp().Norm()), D(-N.DotP(p1)) {}
	inline ZL_Plane& Invert() { N.x *= -1; N.y *= -1; D *= -1; return *this; }
	inline ZL_Plane PlaneInvert() const { ZL_Plane r; r.N.x = N.x*-1; r.N.y = N.y*-1; r.D = D*-1; return r; }
	scalar GetDistanceToPoint(const ZL_Vector& p) const { return (N|p) + D; }
	ZL_Vector GetRayPoint(const ZL_Vector& a, const ZL_Vector& b) { ZL_Vector ba = b-a; scalar x = N|ba; return (x ? a + (ba * ((D - (N|a))/(N|ba))) : a); }
	scalar GetRayT(const ZL_Vector& a, const ZL_Vector& b) { scalar x = N|(b-a); return (x ? (D - (N|a))/x : S_MAX); } // < 0 then ray intersection is before a, > 1 after b
};

struct ZL_AABB
{
	ZL_Vector P; //position
	ZL_Vector E; //extents
	ZL_AABB() { }
	ZL_AABB(const struct ZL_Rectf& rect);
	ZL_AABB(const ZL_Vector& p, const ZL_Vector& e) : P(p), E(e) {}
	ZL_AABB(const ZL_Vector& p, scalar r) : P(p), E(r, r) {}
	ZL_AABB(const ZL_Vector& p, scalar w, scalar h) : P(p), E(w/s(2), h/s(2)) {}
	bool Overlaps(const ZL_AABB& b) const { return sabs(b.P.x-P.x) <= (E.x + b.E.x) && sabs(b.P.y-P.y) <= (E.y + b.E.y); }
	bool Overlaps(const ZL_Vector& c, scalar r) const;
};

struct ZL_RotBB
{
	ZL_Vector P; //position
	ZL_Vector E; //extents
	scalar A; //angle (rad)
	ZL_RotBB() {}
	ZL_RotBB(const struct ZL_Rectf& rect, scalar a);
	ZL_RotBB(const ZL_Vector& p, const ZL_Vector& e, scalar a) : P(p), E(e), A(a) {}
	ZL_RotBB(const ZL_Vector& p, scalar w, scalar h, scalar a) : P(p), E(w/s(2), h/s(2)), A(a) {}
	bool Overlaps(const ZL_Vector& c, scalar r) const;
};

struct ZL_Math
{
	//Circle to Circle collision (returns true on collision and fills pIntersectLength if given)
	static bool CircleCollision(const ZL_Vector& Center1, const scalar Radius1, const ZL_Vector& Center2, const scalar Radius2, scalar* pIntersectLength = NULL);

	//Line to Line collision (returns true on collision and fills pCollision with collision point if given)
	static bool LineCollision(const ZL_Vector& LineA1, const ZL_Vector& LineA2, const ZL_Vector& LineB1, const ZL_Vector& LineB2, ZL_Vector* pCollision = NULL);

	//Line to Circle collision (returns number of colliding points 0, 1 or 2 and fills pCollision1/pCollision2 with collision points if given)
	static int LineCircleCollision(const ZL_Vector& Line1, const ZL_Vector& Line2, const ZL_Vector& CircleCenter, const scalar Radius, ZL_Vector* pCollision1 = NULL, ZL_Vector* pCollision2 = NULL);

	//Line to Rect collision (returns number of colliding points and fills pCollision with collision points if given)
	static int LineRectCollision(const ZL_Vector& Line1, const ZL_Vector& Line2, const struct ZL_Rectf& Rectangle, ZL_Vector* pCollision1 = NULL, ZL_Vector* pCollision2 = NULL, ZL_Vector* pCollision3 = NULL, ZL_Vector* pCollision4 = NULL);

	//Sweep test for two moving circles (returns true on collision and fills normalized time of first and second collision if given)
	static bool CircleSweep(const ZL_Vector& CenterA1, const ZL_Vector& CenterA2, const scalar RadiusA, const ZL_Vector& CenterB1, const ZL_Vector& CenterB2, const scalar RadiusB, scalar* pCollisionTime1 = NULL, scalar* pCollisionTime2 = NULL);

	//Sweep test for a moving circle and a static point (returns true on collision and fills position of circle when it first touched the point and normalized time of collision)
	static bool CirclePointSweep(const ZL_Vector& Center1, const ZL_Vector& Center2, const scalar Radius, const ZL_Vector& Point, ZL_Vector* pCollision = NULL, scalar* pCollisionTime = NULL);

	//Sweep test for a moving circle and a static plane (returns true on collision and fills position of circle when it first touched the plane and normalized time of collision)
	static bool CirclePlaneSweep(const ZL_Vector& Center1, const ZL_Vector& Center2, const scalar Radius, const ZL_Plane& plane, ZL_Vector* pCollision = NULL, scalar* pCollisionTime = NULL);

	//Sweep test for a moving circle and a static line (returns true on collision and fills position of circle when it first touched the line and normalized time of collision)
	static bool CircleLineSweep(const ZL_Vector& Center1, const ZL_Vector& Center2, const scalar Radius, const ZL_Vector& p1, const ZL_Vector& p2, ZL_Vector* pCollision = NULL, scalar* pCollisionTime = NULL);

	//Sweep test for two moving AABB (returns true on collision and fills normalized time of first and second collision if given)
	static bool AABBSweep(const ZL_AABB& A, const ZL_Vector& OldPosA, const ZL_AABB& B, const ZL_Vector& OldPosB, scalar* pCollisionTime1 = NULL, scalar* pCollisionTime2 = NULL);

	//Sweep test for one moving AABB and a fixed AABB (returns true on collision and fills position of AABB when it first touched the other box and normalized time of collision)
	static bool AABBSweep(const ZL_AABB& A, const ZL_Vector& OldPosA, const ZL_AABB& B, ZL_Vector* pCollision = NULL, scalar* pCollisionTime = NULL);

	//Sweep test for a moving circle and a static RotBB (returns true on collision and fills position of circle when it first touched the line and normalized time of collision)
	static bool CircleRotBBSweep(const ZL_Vector& Center1, const ZL_Vector& Center2, const scalar Radius, const ZL_RotBB& RotBB, ZL_Vector* pCollision = NULL, scalar* pCollisionTime = NULL);

	//Sweep test for a moving circle and a static AABB (returns true on collision and fills position of circle when it first touched the line and normalized time of collision)
	static bool CircleAABBSweep(const ZL_Vector& Center1, const ZL_Vector& Center2, const scalar Radius, const ZL_AABB& AABB, ZL_Vector* pCollision = NULL, scalar* pCollisionTime = NULL);

	//Find closest point on a line
	static ZL_Vector ClosestPointOnLine(const ZL_Vector& p, const ZL_Vector& a, const ZL_Vector& b) { ZL_Vector d = (a - b); return b + d * (Clamp01((d.DotP(p - b)) / d.GetLengthSq())); }

	//Returns angle value in rad clamped from 0 to PI2
	static inline scalar Angle(scalar a) { while(a<0)a+=PI2;while(a>PI2)a-=PI2;return a; }

	//Returns angle value in rad clamped from -PI to +PI
	static inline scalar AngleSpread(scalar a) { while (a<-PI)a+=PI2;while(a>PI)a-=PI2;return a; }

	//Returns angle value in degree clamped from 0 to 360
	static inline scalar AngleDeg(scalar a) { while(a<0)a+=360;while(a>360)a-=360;return a; }

	//Returns relative (nearest) angle in rad from -PI to +PI from one angle in rad to another
	static inline scalar RelAngle(scalar from, scalar to) { to-=from;while(to<0)to+=PI2;while(to>PI2)to-=PI2;return(to>PI?-PI2+to:to); }

	//Returns relative (nearest) angle in degree from -180 to +180 from one angle in degree to another
	static inline scalar RelAngleDeg(scalar from, scalar to) { to-=from;while(to<0)to+=360;while(to>360)to-=360;return(to>180?-360+to:to); }

	//Returns acute angle in rad from 0 to PI/2
	static inline scalar AcuteAngle(scalar a) { while(a<0)a+=PI2;while(a>PI2)a-=PI2;if(a>PI)a=PI2-a;if(a>PI/2)a=PI-a;return a; }

	//Returns acute angle in degree from 0 to 90
	static inline scalar AcuteAngleDeg(scalar a) { while(a<0)a+=360;while(a>360)a-=360;if(a>180)a=360-a;if(a>90)a=180-a;return a; }

	//Returns the higher of two scalar values
	static inline scalar Max(const scalar a, const scalar b) { return (a > b ? a : b); }

	//Returns the lower of two scalar values
	static inline scalar Min(const scalar a, const scalar b) { return (a < b ? a : b); }

	//Returns the absolute value of a scalar value
	static inline scalar Abs(const scalar val) { return (val < 0 ? -val : val); }

	//Clamp value between min and max
	static inline scalar Clamp(const scalar val, const scalar min, const scalar max) { return Min(Max(val, min), max); }

	//Clamp value between 0 and 1
	static inline scalar Clamp01(const scalar val) { return Max(0, Min(val, 1)); }

	//Linear inpolate value
	static inline scalar Lerp(const scalar from, const scalar to, const scalar f) { return from + (to-from) * f; }
};

//Easing functions (linear input 0.0 -> 1.0 returns curved 0.0 -> 1.0)
struct ZL_Easing
{
	static inline scalar InSine(scalar t)       { return ssin(PIHALF * t); }
	static inline scalar OutSine(scalar t)      { return s(1) + ssin(PIHALF * (t-1)); }
	static inline scalar InOutSine(scalar t)    { return s(.5) * (s(1) + ssin(PI * (t - s(.5)))); }
	static inline scalar InQuad(scalar t)       { return t * t; }
	static inline scalar OutQuad(scalar t)      { return t * (s(2) - t); }
	static inline scalar InOutQuad(scalar t)    { return (t < s(.5) ? s(2)*t*t : s(-2)*t*t + s(4)*t - s(1)); }
	static inline scalar InCubic(scalar t)      { return t * t * t; }
	static inline scalar OutCubic(scalar t)     { t--; return s(1) + t*t*t; }
	static inline scalar InOutCubic(scalar t)   { t *= 2; if (t < s(1)) return s(.5)*t*t*t; t -= s(2); return s(.5)*(t*t*t + s(2)); }
	static inline scalar InQuart(scalar t)      { t *= t; return t * t; }
	static inline scalar OutQuart(scalar t)     { t--; t = t * t; return s(1) - t * t; }
	static inline scalar InOutQuart(scalar t)   { if(t < s(.5)) { t *= t; return s(8) * t * t; } else { t = (t-1) * (t-1); return s(1) - s(8) * t * t; } }
	static inline scalar InQuint(scalar t)      { scalar t2 = t * t; return t * t2 * t2; }
	static inline scalar OutQuint(scalar t)     { t--; scalar t2 = t*t; return s(1) + t*t2*t2; }
	static inline scalar InOutQuint(scalar t)   { scalar t2; if(t < s(.5)) { t2 = t * t; return s(16) * t * t2 * t2; } else { t--; t2 = t*t; return s(1) + s(16) * t * t2 * t2; } }
	static inline scalar InExpo(scalar t)       { return (spow(s(2), s(8) * t) - s(1)) / s(255); }
	static inline scalar OutExpo(scalar t)      { return s(1) - spow(s(2), s(-8) * t); }
	static inline scalar InOutExpo(scalar t)    { if(t < s(.5)) { return (spow(s(2), s(16) * t) - 1) / s(510); } else { return s(1) - s(.5) * spow(s(2), s(-16) * (t - s(.5))); } }
	static inline scalar InCirc(scalar t)       { return s(1) - ssqrt(s(1) - t); }
	static inline scalar OutCirc(scalar t)      { return ssqrt(t); }
	static inline scalar InOutCirc(scalar t)    { if(t < s(.5)) { return (s(1) - ssqrt(s(1) - s(2) * t)) * s(.5); } else { return (s(1) + ssqrt(s(2) * t - s(1))) * s(.5); } }
	static inline scalar InBack(scalar t)       { return t * t * (s(2.70158) * t - s(1.70158)); }
	static inline scalar OutBack(scalar t)      { --t; return s(1) + (t) * t * (s(2.70158) * t + s(1.70158)); }
	static inline scalar InOutBack(scalar t)    { if(t < s(.5)) { return t * t * (s(7) * t - s(2.5)) * 2; } else { t--; return s(1) + (t) * t * s(2) * (s(7) * t + s(2.5)); } }
	static inline scalar InElastic(scalar t)    { scalar t2 = t * t; return t2 * t2 * ssin(t * PI * s(4.5)); }
	static inline scalar OutElastic(scalar t)   { scalar t2 = (t - s(1)) * (t - s(1)); return s(1) - t2 * t2 * scos(t * PI * s(4.5)); }
	static inline scalar InOutElastic(scalar t) { scalar t2; if(t < s(.45)) { t2 = t * t; return s(8) * t2 * t2 * ssin(t * PI * s(9)); } else if(t < s(.55)) { return s(.5) + s(.75) * ssin(t * PI * s(4)); } else { t2 = (t - s(1)) * (t - s(1)); return s(1) - s(8) * t2 * t2 * ssin(t * PI * s(9)); } }
	static inline scalar InBounce(scalar t)     { return spow(s(2), s(6) * (t - s(1))) * sabs(ssin(t * PI * s(3.5))); }
	static inline scalar OutBounce(scalar t)    { return 1 - spow(s(2), s(-6) * t) * sabs(scos(t * PI * s(3.5))); }
	static inline scalar InOutBounce(scalar t)  { if(t < s(.5)) { return s(8) * spow(s(2), s(8) * (t - s(1))) * sabs(ssin(t * PI * s(7))); } else { return s(1) - s(8) * spow(s(2), s(-8) * t) * sabs(ssin(t * PI * s(7))); } }
};

//static random number generator using the system rand() function (implementation and randomness may vary depending on platform)
struct ZL_Rand
{
	static inline unsigned int UInt() { return ((unsigned int)rand()<<30)^(rand()<<15)^rand(); }
	static inline scalar Factor() { return s(rand())/s(RAND_MAX); }          // >= 0.0 && <= 1.0
	static inline scalar FactorEx() { return s(rand())/(s(RAND_MAX)+s(1)); } // >= 0.0 && <  1.0

	static inline int Int(const int max) { return (int)(FactorEx()*(max + 1)); }                                   // >= 0    && <= max
	static inline int Int(const int min, const int max) { return min + (int)(FactorEx()*(max + 1 - min)); }        // >= min  && <= max
	static inline scalar Range(const scalar max) { return (max*Factor()); }                                        // >= 0.0  && <= max
	static inline scalar RangeEx(const scalar max) { return (max*FactorEx()); }                                    // >= 0.0  && <  max
	static inline scalar Range(const scalar min, const scalar max) { return min+((max-min)*Factor()); }            // >= min  && <= max
	static inline scalar RangeEx(const scalar min, const scalar max) { return min+((max-min)*FactorEx()); }        // >= min  && <  max
	static inline scalar Variation(const scalar var) { return -var + var*s(2)*Factor(); }                          // >= -var && <= +var
	static inline scalar VariationEx(const scalar var) { return -var + var*s(2)*FactorEx(); }                      // >= -var && <  +var
	static inline scalar Variation(const scalar base, const scalar var) { return base-var+var*s(2)*Factor(); }     // >=base-var && <= base+var
	static inline scalar VariationEx(const scalar base, const scalar var) { return base-var+var*s(2)*FactorEx(); } // >=base-var && <  base+var
	static inline int Sign() { return  (rand() & 1 ? 1 : -1); }                                                    // +1 or -1
	static inline bool Bool() { return  ((rand() & 1) != 0); }                                                     // true or false
	static inline scalar Angle() { return PI2*FactorEx(); }                                                        // >=0 && < PI2
	static inline ZL_Vector AngleVec() { return ZL_Vector::FromAngle(PI2*FactorEx()); }                            // random direction vector
	static inline ZL_Vector PointIn(const struct ZL_Rectf& rec);                                                   // random point inside rec
	template <typename O> static inline O& Element(std::vector<O>& vec) { return vec[Int((int)vec.size()-1)]; }    // random element of vector

	//randomize order of vectors, c-arrays, or array-like classes with a given size
	template <typename O> static void Shuffle(O& vec) { for (int i = (int)vec.size() - 1; i > 0; i--) std::swap(vec[i], vec[Int(i)]); }
	template <typename O> static void ShuffleArray(O& arr) { for (int i = (int)COUNT_OF(arr) - 1; i > 0; i--) std::swap(arr[i], arr[Int(i)]); }
	template <typename O> static void Shuffle(O& arr, size_t count) { for (int i = (int)count - 1; i > 0; i--) std::swap(arr[i], arr[Int(i)]); }
};

//instantiatable seedable random number generator that returns the same sequence of pseudo random values on all platforms
struct ZL_SeededRand
{
	ZL_SeededRand(unsigned int rand_seed = ZL_Rand::UInt());
	ZL_SeededRand(unsigned int rand_seed, int seed_xor);
	void Seed(unsigned int rand_seed = ZL_Rand::UInt());
	void Seed(unsigned int rand_seed, int seed_xor);
	void Reset();
	unsigned int UInt();
	scalar Factor();    // >= 0.0 && <= 1.0
	scalar FactorEx();  // >= 0.0 && <  1.0

	inline int Int(const int max) { return (int)(FactorEx()*(max + 1)); }                                   // >= 0    && <= max
	inline int Int(const int min, const int max) { return min + (int)(FactorEx()*(max + 1 - min)); }        // >= min  && <= max
	inline scalar Range(const scalar max) { return max*Factor(); }                                          // >= 0.0  && <= max
	inline scalar RangeEx(const scalar max) { return max*FactorEx(); }                                      // >= 0.0  && <  max
	inline scalar Range(const scalar min, const scalar max) { return min + (max - min)*Factor(); }          // >= min  && <= max
	inline scalar RangeEx(const scalar min, const scalar max) { return min + (max - min)*FactorEx(); }      // >= min  && <  max
	inline scalar Variation(const scalar var) { return -var + var*s(2)*Factor(); }                          // >= -var && <= +var
	inline scalar VariationEx(const scalar var) { return -var + var*s(2)*FactorEx(); }                      // >= -var && <  +var
	inline scalar Variation(const scalar base, const scalar var) { return base-var+var*s(2)*Factor(); }     // >= base-var && <= base+var
	inline scalar VariationEx(const scalar base, const scalar var) { return base-var+var*s(2)*FactorEx(); } // >= base-var && <  base+var
	inline int Sign() { return (UInt() & 1 ? 1 : -1); }                                                     // +1 or -1
	inline bool Bool() { return ((UInt() & 1) != 0); }                                                      // true or false
	inline scalar Angle() { return FactorEx()*PI2; }                                                        // >=0 && < PI2
	inline ZL_Vector AngleVec() { return ZL_Vector::FromAngle(FactorEx()*PI2); }                            // random direction vector
	inline ZL_Vector PointIn(const struct ZL_Rectf& rec);                                                   // random point inside rec
	template <typename O> inline O& Element(std::vector<O>& vec) { return vec[Int(vec.size()-1)]; }         // random element of vector

	//randomize order of vectors, c-arrays, or array-like classes with a given size
	template <typename O> void Shuffle(O& vec) { for (int i = (int)vec.size() - 1; i > 0; i--) std::swap(vec[i], vec[Int(i)]); }
	template <typename O> void ShuffleArray(O& arr) { for (int i = (int)COUNT_OF(arr) - 1; i > 0; i--) std::swap(arr[i], arr[Int(i)]); }
	template <typename O> void Shuffle(O& arr, size_t count) { for (int i = (int)count - 1; i > 0; i--) std::swap(arr[i], arr[Int(i)]); }

	unsigned int w, z, base_w, base_z;
};

#endif //__cplusplus
#endif //__ZL_MATH__
