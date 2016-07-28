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

#ifndef __ZL_MATH3D__
#define __ZL_MATH3D__

#include "ZL_Math.h"

//Float based 3d vector (also used as points, positions and any kind of 2d coordinates)
struct ZL_Vector3
{
	scalar x, y, z;

	//Construction
	inline ZL_Vector3() : x(0), y(0), z(0) { }
	inline ZL_Vector3(ZL_NoInitType) { }
	inline ZL_Vector3(scalar x, scalar y, scalar z = 0) : x(x), y(y), z(z) { }
	inline ZL_Vector3(const ZL_Point &p, scalar z = 0) : x((scalar)p.x), y((scalar)p.y), z(z) { }
	inline ZL_Vector3(const ZL_Vector &p, scalar z = 0) : x(p.x), y(p.y), z(z) { }
	inline ZL_Vector3(const ZL_Vector3 &p1, const ZL_Vector3 &p2) : x(p2.x-p1.x), y(p2.y-p1.y), z(p2.z-p1.z) { }

	//Comparison operators
	inline bool operator!() const { return (x == 0 && y == 0 && z == 0); }
	inline bool operator==(const ZL_Vector3 &p) const { return (x == p.x) && (y == p.y) && (z == p.z); }
	inline bool operator!=(const ZL_Vector3 &p) const { return (x != p.x) || (y != p.y) || (z != p.z); }

	//Math operators
	inline ZL_Vector3 operator-() const { return ZL_Vector3(-x, -y, -z); }
	inline ZL_Vector3 &operator+=(const ZL_Vector3 &p) { x += p.x; y += p.y; z += p.z; return *this; }
	inline ZL_Vector3 &operator-=(const ZL_Vector3 &p) { x -= p.x; y -= p.y; z -= p.z; return *this; }
	inline ZL_Vector3 &operator*=(const ZL_Vector3 &p) { x *= p.x; y *= p.y; z *= p.z; return *this; }
	inline ZL_Vector3 &operator/=(const ZL_Vector3 &p) { x /= p.x; y /= p.y; z /= p.z; return *this; }
	inline ZL_Vector3 &operator+=(const scalar f) { x += f; y += f; z += f; return *this; }
	inline ZL_Vector3 &operator-=(const scalar f) { x -= f; y -= f; z -= f; return *this; }
	inline ZL_Vector3 &operator*=(const scalar f) { x *= f; y *= f; z *= f; return *this; }
	inline ZL_Vector3 &operator/=(const scalar f) { x /= f; y /= f; z /= f; return *this; }
	inline ZL_Vector3 operator+(const ZL_Vector3 &p) const { return ZL_Vector3(x + p.x, y + p.y, z + p.z); }
	inline ZL_Vector3 operator-(const ZL_Vector3 &p) const { return ZL_Vector3(x - p.x, y - p.y, z - p.z); }
	inline ZL_Vector3 operator*(const ZL_Vector3 &p) const { return ZL_Vector3(x * p.x, y * p.y, z * p.z); }
	inline ZL_Vector3 operator/(const ZL_Vector3 &p) const { return ZL_Vector3(x / p.x, y / p.y, z / p.z); }
	inline ZL_Vector3 operator+(const scalar f) const { return ZL_Vector3(x + f, y + f, z + f); }
	inline ZL_Vector3 operator-(const scalar f) const { return ZL_Vector3(x - f, y - f, z - f); }
	inline ZL_Vector3 operator*(const scalar f) const { return ZL_Vector3(x * f, y * f, z * f); }
	inline ZL_Vector3 operator/(const scalar f) const { return ZL_Vector3(x / f, y / f, z / f); }
	inline scalar operator|(const ZL_Vector3 &v) const { return x * v.x + y * v.y + z * v.z; } //dot product
	inline ZL_Vector3 operator^(const ZL_Vector3 &v) const { return ZL_Vector3(y*v.z-z*v.y, z*v.x-x*v.z, x*v.y-y*v.x); } //cross product

	//Length comparison operators  (faster than calling and comparing GetLength())
	inline bool operator<(const ZL_Vector3 &b) const  { return ((x*x + y*y + z*z) <  (b.x*b.x + b.y*b.y + b.z*b.z)); }
	inline bool operator<=(const ZL_Vector3 &b) const { return ((x*x + y*y + z*z) <= (b.x*b.x + b.y*b.y + b.z*b.z)); }
	inline bool operator>(const ZL_Vector3 &b) const  { return ((x*x + y*y + z*z) >  (b.x*b.x + b.y*b.y + b.z*b.z)); }
	inline bool operator>=(const ZL_Vector3 &b) const { return ((x*x + y*y + z*z) >= (b.x*b.x + b.y*b.y + b.z*b.z)); }
	inline bool operator==(const scalar l) const { return ((x*x + y*y + z*z) == (l*l)); }
	inline bool operator!=(const scalar l) const { return ((x*x + y*y + z*z) != (l*l)); }
	inline bool operator<(const scalar l) const  { return ((x*x + y*y + z*z) <  (l*l)); }
	inline bool operator<=(const scalar l) const { return ((x*x + y*y + z*z) <= (l*l)); }
	inline bool operator>(const scalar l) const  { return ((x*x + y*y + z*z) >  (l*l)); }
	inline bool operator>=(const scalar l) const { return ((x*x + y*y + z*z) >= (l*l)); }

	//Non modifying operators as functions
	inline scalar GetDistance(const ZL_Vector3 &p) const { return ssqrt((x-p.x)*(x-p.x) + (y-p.y)*(y-p.y) + (z-p.z)*(z-p.z)); }
	inline scalar GetDistanceSq(const ZL_Vector3 &p) const { return ((x-p.x)*(x-p.x) + (y-p.y)*(y-p.y) + (z-p.z)*(z-p.z)); }
	inline scalar GetLength() const { return ssqrt(x*x + y*y + z*z); }
	inline scalar GetLengthSq() const { return (x*x + y*y + z*z); }
	inline scalar CompareDistance(const ZL_Vector3 &p, scalar l) const { return (x-p.x)*(x-p.x) + (y-p.y)*(y-p.y) + (z-p.z)*(z-p.z) - (l*l); }
	inline scalar DotP(const ZL_Vector3 &v) const { return x * v.x + y * v.y + z * v.z; } //dot product
	inline ZL_Vector3 CrossP(const ZL_Vector3 &v) const { return ZL_Vector3(y*v.z-z*v.y, z*v.x-x*v.z, x*v.y-y*v.x); } //cross product
	inline bool Near(const ZL_Vector3 &v, scalar l) const { return (x-v.x)*(x-v.x) + (y-v.y)*(y-v.y) + (z-v.z)*(z-v.z) < (l*l); }
	inline bool Far(const ZL_Vector3 &v, scalar l) const { return (x-v.x)*(x-v.x) + (y-v.y)*(y-v.y) + (z-v.z)*(z-v.z) > (l*l); }
	inline bool AlmostZero(scalar ErrorTolerance = SMALL_NUMBER) const { return sabs(x)<ErrorTolerance && sabs(y)<ErrorTolerance && sabs(z)<ErrorTolerance; }
	inline bool AlmostEqual(const ZL_Vector3 &v, scalar ErrorTolerance = SMALL_NUMBER) const { return sabs(x-v.x)<ErrorTolerance && sabs(y-v.y)<ErrorTolerance && sabs(z-v.z)<ErrorTolerance; }

	//Self modifying operators as functions (non const call)
	inline ZL_Vector3 &Set(scalar x, scalar y, scalar z = 0) { this->x = x; this->y = y; this->z = z; return *this; }
	inline ZL_Vector3 &Norm() { if (x==0 && y==0 && z==0) return this->Set(1,0,0); return *this /= GetLength(); }
	inline ZL_Vector3 &NormUnsafe() { return *this /= GetLength(); }
	inline ZL_Vector3 &Add(const ZL_Vector3 &p) { x += p.x; y += p.y; z += p.z; return *this; }
	inline ZL_Vector3 &Sub(const ZL_Vector3 &p) { x -= p.x; y -= p.y; z -= p.z; return *this; }
	inline ZL_Vector3 &Mul(const ZL_Vector3 &p) { x *= p.x; y *= p.y; z *= p.z; return *this; }
	inline ZL_Vector3 &Div(const ZL_Vector3 &p) { x /= p.x; y /= p.y; z /= p.z; return *this; }
	inline ZL_Vector3 &Add(const scalar f)      { x += f;   y += f;   z += f;   return *this; }
	inline ZL_Vector3 &Sub(const scalar f)      { x -= f;   y -= f;   z -= f;   return *this; }
	inline ZL_Vector3 &Mul(const scalar f)      { x *= f;   y *= f;   z *= f;   return *this; }
	inline ZL_Vector3 &Div(const scalar f)      { x /= f;   y /= f;   z /= f;   return *this; }
	inline ZL_Vector3 &Mod(const ZL_Vector3 &v) { x=smod(x,v.x); y=smod(y,v.y); z=smod(z,v.z); return *this; }
	inline ZL_Vector3 &Abs() { x = sabs(x); y = sabs(y); z = sabs(z); return *this; }
	inline ZL_Vector3 &Lerp(const ZL_Vector3 &v, const scalar t) { x = x+(v.x-x)*t, y = y+(v.y-y)*t, z = z+(v.z-z)*t; return *this; }
	inline ZL_Vector3 &SetLength(scalar length) { if (!x && !y && !z) { x = length; return *this; } return Mul(length / GetLength()); }
	inline ZL_Vector3 &AddLength(scalar length) { if (!x && !y && !z) { x = length; return *this; } return Mul(length / GetLength() + 1); }
	inline ZL_Vector3 &SetMaxLength(scalar maximum_length) { if (!x && !y && !z) return *this; scalar lensq = x*x+y*y; return (maximum_length*maximum_length >= lensq ? *this : Mul(maximum_length / ssqrt(lensq))); }
	inline ZL_Vector3 &SetMinLength(scalar minimum_length) { if (!x && !y && !z) { x = minimum_length; return *this; } scalar lensq = x*x+y*y; return (minimum_length*minimum_length <= lensq ? *this : Mul(minimum_length / ssqrt(lensq))); }

	//Modifying operators as functions returning new instances (const call)
	inline ZL_Vector3 VecNorm() const { if (x==0 && y==0 && z==0) return ZL_Vector3(1,0,0); return *this / GetLength(); }
	inline ZL_Vector3 VecNormUnsafe() const { return *this / GetLength(); }
	inline ZL_Vector3 VecMod(const ZL_Vector3 &v) const { return ZL_Vector3(smod(x,v.x), smod(y,v.y), smod(z,v.z)); }
	inline ZL_Vector3 VecAbs() const { return ZL_Vector3(sabs(x), sabs(y), sabs(z)); }
	inline ZL_Vector3 VecLerp(const ZL_Vector3 &v, const scalar f) const { return ZL_Vector3(x+(v.x-x)*f, y+(v.y-y)*f, z+(v.z-z)*f); }
	inline ZL_Vector3 VecWithLength(scalar length) const { if (!x && !y && !z) return ZL_Vector3(length,0,0); return *this * (length / GetLength()); }
	inline ZL_Vector3 VecWithAddLength(scalar length) const { if (!x && !y && !z) return ZL_Vector3(length,0,0); return *this * (length / GetLength() + 1); }
	inline ZL_Vector3 VecWithMaxLength(scalar maximum_length) const { if (!x && !y && !z) return *this; scalar lensq = x*x+y*y; return (maximum_length*maximum_length >= lensq ? *this : *this * (maximum_length / ssqrt(lensq))); }
	inline ZL_Vector3 VecWithMinLength(scalar minimum_length) const { if (!x && !y && !z) return ZL_Vector3(minimum_length,0,0); scalar lensq = x*x+y*y; return (minimum_length*minimum_length <= lensq ? *this : *this * (minimum_length / ssqrt(lensq))); }

	//Static functions returning new instances
	static inline ZL_Vector3 Norm(const ZL_Vector3 &v) { if (v.x==0 && v.y==0 && v.z==0) return ZL_Vector3(1,0,0); return v / v.GetLength(); }
	static inline ZL_Vector3 NormUnsafe(const ZL_Vector3 &v) { return v / v.GetLength(); }
	static inline ZL_Vector3 Mod(const ZL_Vector3 &a, const ZL_Vector3 &b) { return ZL_Vector3(smod(a.x,b.x), smod(a.y,b.y), smod(a.z,b.z)); }
	static inline ZL_Vector3 Abs(const ZL_Vector3 &v) { return ZL_Vector3(sabs(v.x), sabs(v.y), sabs(v.z)); }
	static inline ZL_Vector3 Lerp(const ZL_Vector3 &from, const ZL_Vector3 &to, const scalar f) { return ZL_Vector3(from.x+(to.x-from.x)*f, from.y+(to.y-from.y)*f, from.z+(to.z-from.z)*f); }
	static inline ZL_Vector3 FromXY(const ZL_Vector &p, scalar z = 0) { return ZL_Vector3(p.x, p.y, z); }
	static inline ZL_Vector3 FromXZ(const ZL_Vector &p, scalar y = 0) { return ZL_Vector3(p.x, y, p.y); }
	static inline ZL_Vector3 FromYZ(const ZL_Vector &p, scalar x = 0) { return ZL_Vector3(x, p.x, p.y); }

	//Returns angle in rad from 0 to PI related to world coordinates (1,0)
	inline scalar GetAbsAngle() const { scalar d=VecNorm().x;return(d<0?PI-sacos(-d):sacos(d)); }

	//Returns relative (nearest) angle in rad from 0 to PI from this to another vector
	inline scalar GetRelAbsAngle(const ZL_Vector3 &v) const { scalar d=DotP(v);return(d<0?PI-sacos(-d):sacos(d)); }

	//Returns angle in degree from 0 to 180 related to world coordinates (1,0)
	inline scalar GetAbsAngleDeg() const { scalar d=VecNorm().x;return(d<0?PI-sacos(-d):sacos(d))*PIUNDER180; }

	//Returns relative (nearest) angle in degree from 0 to 180 from this to another vector
	inline scalar GetRelAbsAngleDeg(const ZL_Vector3 &v) const { scalar d=DotP(v);return(d<0?PI-sacos(-d):sacos(d))*PIUNDER180; }

	//Returns a normal vector created by pitch (horizontal angle) and yaw (vertical angle)
	static inline ZL_Vector3 FromRotation(scalar HoirzontalAngleRad, scalar VerticalAngleRad) { return ZL_Vector3(scos(HoirzontalAngleRad)*scos(VerticalAngleRad), ssin(VerticalAngleRad), ssin(HoirzontalAngleRad)*scos(VerticalAngleRad));  }

	static const ZL_Vector3 Zero, One, Forward, Right, Up;
};

struct ZL_Plane3
{
	ZL_Vector3 N; //unit normal
	scalar D; //distance from the plane to the origin from a normal and a point
	ZL_Plane3() : N(ZL_Vector3::Forward), D(0) {}
	ZL_Plane3(const ZL_Vector3& N, scalar D = 0) : N(N), D(D) {}
	inline ZL_Plane3& Invert() { N *= -1; D *= -1; return *this; }
	inline ZL_Plane3 PlaneInvert() const { return ZL_Plane3(-N, -D); }
	scalar GetDistanceToPoint(const ZL_Vector3& p) const { return (N|p) + D; }
	ZL_Vector3 GetRayPoint(const ZL_Vector3& a, const ZL_Vector3& b) { ZL_Vector3 ba = b-a; scalar x = N|ba; return (x ? a + (ba * ((D - (N|a))/(N|ba))) : a); }
	scalar GetRayT(const ZL_Vector3& a, const ZL_Vector3& b) { scalar x = N|(b-a); return (x ? (D - (N|a))/x : S_MAX); } // < 0 then ray intersection is before a, > 1 after b
};

struct ZL_Quat
{
	scalar x, y, z, w;

	//Construction
	inline ZL_Quat() : x(0), y(0), z(0), w(1) {}
	inline ZL_Quat(ZL_NoInitType) { }
	inline ZL_Quat(scalar x, scalar y, scalar z, scalar w) : x(x), y(y), z(z), w(w) {}
	inline ZL_Quat(scalar w) : x(0), y(0), z(0), w(w) {}
	inline ZL_Quat(const ZL_Vector3& NormalizedAxis, scalar AngleRad) { scalar ah = AngleRad*s(.5); *this = FromVector(NormalizedAxis*ssin(ah), scos(ah)); }
	inline ZL_Quat(struct ZL_Matrix& m);

	//Comparison operators
	inline bool operator!() const { return (!x && !y && !z && !w); }
	inline bool operator==(const ZL_Quat &q) const { return (x == q.x && y == q.y && z == q.z && w == q.w); }
	inline bool operator!=(const ZL_Quat &q) const { return (x != q.x || y != q.y || z != q.z || w != q.w); }

	//Math operators
	inline ZL_Quat operator-() const { return ZL_Quat(-x, -y, -z, -w); }
	inline ZL_Quat &operator+=(const ZL_Quat &q) { x += q.x; y += q.y; z += q.z; w += q.w; return *this; }
	inline ZL_Quat &operator-=(const ZL_Quat &q) { x -= q.x; y -= q.y; z -= q.z; w -= q.w; return *this; }
	inline ZL_Quat &operator*=(const ZL_Quat &q) { x = (q.x*w) + (x*q.w) + (y*q.z-z*q.y), y = (q.y*w) + (y*q.w) + (z*q.x-x*q.z), z = (q.z*w) + (z*q.w) + (x*q.y-y*q.x), w = w*q.w - (x*q.x+y*q.y+z*q.z); return *this; }
	inline ZL_Quat &operator/=(const ZL_Quat &q) { return *this *= q.QuatInverse(); }
	inline ZL_Quat &operator*=(scalar f) { x *= f; y *= f; z *= f; w *= f; return *this; }
	inline ZL_Quat &operator/=(scalar f) { f = s(1)/f; x *= f; y *= f; z *= f; w *= f; return *this; }
	inline ZL_Quat operator+(const ZL_Quat &q) const { return ZL_Quat(x + q.x, y + q.y, z + q.z, w + q.w); }
	inline ZL_Quat operator-(const ZL_Quat &q) const { return ZL_Quat(x - q.x, y - q.y, z - q.z, w - q.w); }
	inline ZL_Quat operator*(const ZL_Quat &q) const { return ZL_Quat((q.x*w) + (x*q.w) + (y*q.z-z*q.y), (q.y*w) + (y*q.w) + (z*q.x-x*q.z), (q.z*w) + (z*q.w) + (x*q.y-y*q.x), w*q.w - (x*q.x+y*q.y+z*q.z)); }
	inline ZL_Quat operator/(const ZL_Quat &q) const { return *this * q.QuatInverse(); }
	inline ZL_Quat operator*(scalar f) const { return ZL_Quat(x * f, y * f, z * f, w * f); }
	inline ZL_Quat operator/(scalar f) const { return ZL_Quat(x / f, y / f, z / f, w / f); }
	inline scalar operator|(const ZL_Quat &q) const { return x*q.x + y*q.y + z*q.z + w*q.w; } //dot product
	inline ZL_Vector3 operator*(const ZL_Vector3 &v) const { const ZL_Vector3 q(x,y,z), t = (q^v)*2; return v + (t*w) + (q^t); } //rotate vector

	//Non modifying operators as functions
	inline scalar Dot(const ZL_Quat& q) const { return x*q.x + y*q.y + z*q.z + w*q.w; }
	inline scalar GetLength() const { return ssqrt(x*x + y*y + z*z + w*w); }
	inline scalar GetLengthSq() const { return x*x + y*y + z*z + w*w; }
	inline ZL_Vector3 RotateVector(const ZL_Vector3& v) { const ZL_Vector3 q(x,y,z), t = (q^v)*2; return v + (t*w) + (q^t); }
	inline ZL_Vector3 UnrotateVector(const ZL_Vector3& v) { const ZL_Vector3 q(-x,-y,-z), t = (q^v)*2; return v + (t*w) + (q^t); }
	inline bool AlmostEqual(const ZL_Quat &q, scalar ErrorTolerance = SMALL_NUMBER) { return sabs(x-q.x)<ErrorTolerance && sabs(y-q.y)<ErrorTolerance && sabs(z-q.z)<ErrorTolerance && sabs(w-q.w)<ErrorTolerance; }
	inline bool IsIdentity() { return w==1||w==-1; }
	inline bool AlmostIdentity(scalar ErrorTolerance = SMALL_NUMBER) { return sabs(s(1)-w)<ErrorTolerance; }

	//Self modifying operators as functions (non const call)
	inline ZL_Quat& Conjugate() { x=-x; y=-y; z=-z; return *this; }
	inline ZL_Quat& Negate() { x=-x; y=-y; z=-z; w=-w; return *this; }
	inline ZL_Quat& Inverse() { scalar sq = x*x+y*y+z*z+w*w; if (!sq) return *this = ZL_Quat(); return *this/=sq; }
	inline ZL_Quat& Norm() { scalar sq = x*x+y*y+z*z+w*w; if (!sq) return *this = ZL_Quat(); return *this/=ssqrt(sq); }
	inline ZL_Quat& Lerp(const ZL_Quat& v, scalar t) { x = x+(v.x-x)*t, y = y+(v.y-y)*t, z = z+(v.z-z)*t, w = w+(v.w-w)*t; return *this; }
	inline ZL_Quat& SLerp(const ZL_Quat& v, scalar t) { scalar qc = Dot(v); if (qc > s(0.9999)) return Lerp(v, t).Norm(); if (qc < 0) { Negate(); qc = -qc; } scalar qa = sacos(qc), qs = ssin(qa); return Mul(ssin((1-t)*qa)/qs).Add(v * ssin(t*qa)/qs); }
	inline ZL_Quat& Add(const ZL_Quat &q) { x += q.x; y += q.y; z += q.z; w += q.w; return *this; }
	inline ZL_Quat& Sub(const ZL_Quat &q) { x -= q.x; y -= q.y; z -= q.z; w -= q.w; return *this; }
	inline ZL_Quat& Mul(const ZL_Quat &q) { x = (q.x*w) + (x*q.w) + (y*q.z-z*q.y), y = (q.y*w) + (y*q.w) + (z*q.x-x*q.z), z = (q.z*w) + (z*q.w) + (x*q.y-y*q.x), w = w*q.w - (x*q.x+y*q.y+z*q.z); return *this; }
	inline ZL_Quat& Div(const ZL_Quat &q) { return *this *= q.QuatInverse(); }
	inline ZL_Quat& Mul(scalar f) { x *= f; y *= f; z *= f; w *= f; return *this; }
	inline ZL_Quat& Div(scalar f) { f = s(1)/f; x *= f; y *= f; z *= f; w *= f; return *this; }

	//Modifying operators as functions returning new instances (const call)
	inline ZL_Quat QuatConjugate() const { return ZL_Quat(-x, -y, -z, w); }
	inline ZL_Quat QuatNegate() const { return ZL_Quat(-x, -y, -z, -w); }
	inline ZL_Quat QuatInverse() const { scalar sq = x*x+y*y+z*z+w*w; if (!sq) return ZL_Quat(); return *this/sq; }
	inline ZL_Quat QuatNorm() const { scalar sq = x*x+y*y+z*z+w*w; if (!sq) return ZL_Quat(); return *this/ssqrt(sq); }
	inline ZL_Quat QuatLerp(const ZL_Quat& v, scalar t) const { return ZL_Quat(x+(v.x-x)*t, y+(v.y-y)*t, z+(v.z-z)*t, w+(v.w-w)*t); }
	inline ZL_Quat QuatSLerp(const ZL_Quat& v, scalar t) const { scalar qc = Dot(v); if (qc > s(0.9999)) return QuatLerp(v, t).Norm();  scalar qa = sacos(sabs(qc)), qs = ssin(qa); return (qc < 0 ? QuatNegate() : *this) * (ssin((1-t)*qa)/qs) + (v * ssin(t*qa)/qs); }

	//Static functions returning new instances
	static inline ZL_Quat FromVector(const ZL_Vector3& xyz, scalar w = 0) { return ZL_Quat(xyz.x, xyz.y, xyz.z, w); }
	static inline ZL_Quat FromNormalizedAxis(const ZL_Vector3& NormalizedAxis, scalar AngleRad) { scalar ah = AngleRad*s(.5); return FromVector(NormalizedAxis*ssin(ah), scos(ah)); }
	static inline ZL_Quat FromRotateX(scalar AngleRad) { scalar ah = AngleRad*s(.5); return ZL_Quat(ssin(ah), 0, 0, scos(ah)); }
	static inline ZL_Quat FromRotateY(scalar AngleRad) { scalar ah = AngleRad*s(.5); return ZL_Quat(0, ssin(ah), 0, scos(ah)); }
	static inline ZL_Quat FromRotateZ(scalar AngleRad) { scalar ah = AngleRad*s(.5); return ZL_Quat(0, 0, ssin(ah), scos(ah)); }
};

struct ZL_Matrix
{
	scalar m[16]; //[4 Rows][4 Columns]

	//Construction
	inline ZL_Matrix() { m[0]=1,m[1]=0,m[2]=0,m[3]=0,m[4]=0,m[5]=1,m[6]=0,m[7]=0,m[8]=0,m[9]=0,m[10]=1,m[11]=0,m[12]=0,m[13]=0,m[14]=0,m[15]=1; }
	inline ZL_Matrix(ZL_NoInitType) { }
	inline ZL_Matrix(ZL_Quat& q) { scalar x2=q.x*q.x, y2=q.y*q.y, z2=q.z*q.z, w2=q.w*q.w; m[0]=w2+x2-y2-z2,m[1]=(q.x*q.y+q.w*q.z)*2,m[2]=(q.x*q.z-q.w*q.y)*2,m[3]=0,m[4]=(q.x*q.y-q.w*q.z)*2,m[5]=w2-x2+y2-z2,m[6]=(q.y*q.z+q.w*q.x)*2,m[7]=0,m[8]=(q.x*q.z+q.w*q.y)*2,m[9]=(q.y*q.z-q.w*q.x)*2,m[10]=w2-x2-y2+z2,m[11]=0,m[12]=0,m[13]=0,m[14]=0,m[15]=1; }
	inline ZL_Matrix(scalar m00, scalar m01, scalar m02, scalar m03, scalar m10, scalar m11, scalar m12, scalar m13, scalar m20, scalar m21, scalar m22, scalar m23, scalar m30, scalar m31, scalar m32, scalar m33) { m[0]=m00,m[1]=m01,m[2]=m02,m[3]=m03,m[4]=m10,m[5]=m11,m[6]=m12,m[7]=m13,m[8]=m20,m[9]=m21,m[10]=m22,m[11]=m23,m[12]=m30,m[13]=m31,m[14]=m32,m[15]=m33; }

	//Math operators
	inline ZL_Matrix operator+(const ZL_Matrix& b) const { ZL_Matrix r(ZL_NoInit); for (int i = 0; i < 16; i++) r.m[i] = m[i]+b.m[i]; return r; }
	inline ZL_Matrix operator+(const ZL_Vector3& v) const { ZL_Matrix r = *this; r.m[12]+=v.x, r.m[13]+=v.y, r.m[14]+=v.z; return r; }
	inline ZL_Matrix& operator+=(const ZL_Matrix& b) { for (int i = 0; i < 16; i++) m[i] += b.m[i]; return *this; }
	inline ZL_Matrix& operator+=(const ZL_Vector3& v) { m[12]+=v.x, m[13]+=v.y, m[14]+=v.z; return *this; }
	inline ZL_Matrix operator*(scalar scale) const { ZL_Matrix r(ZL_NoInit); for (int i = 0; i < 16; i++) r.m[i] = m[i]*scale; return r; }
	inline ZL_Matrix& operator*=(scalar scale) { for (int i = 0; i < 16; i++) m[i] *= scale; return *this; }
	inline bool operator==(const ZL_Matrix& b) const { for (int i = 0; i < 16; i++) if (m[i] != b.m[i]) return false; return true; }
	inline bool operator!=(const ZL_Matrix& b) const { for (int i = 0; i < 16; i++) if (m[i] != b.m[i]) return true; return false; }

	//Concatenation operator
	inline ZL_Matrix operator*=(const ZL_Matrix& Other) { return *this = *this*Other; }
	inline ZL_Matrix operator*(const ZL_Matrix& Other) const
	{
		const scalar *b = Other.m;
		return ZL_Matrix(m[ 0]*b[0]+m[ 1]*b[4]+m[ 2]*b[8]+m[ 3]*b[12], m[ 0]*b[1]+m[ 1]*b[5]+m[ 2]*b[9]+m[ 3]*b[13], m[ 0]*b[2]+m[ 1]*b[6]+m[ 2]*b[10]+m[ 3]*b[14], m[ 0]*b[3]+m[ 1]*b[7]+m[ 2]*b[11]+m[ 3]*b[15],
		                 m[ 4]*b[0]+m[ 5]*b[4]+m[ 6]*b[8]+m[ 7]*b[12], m[ 4]*b[1]+m[ 5]*b[5]+m[ 6]*b[9]+m[ 7]*b[13], m[ 4]*b[2]+m[ 5]*b[6]+m[ 6]*b[10]+m[ 7]*b[14], m[ 4]*b[3]+m[ 5]*b[7]+m[ 6]*b[11]+m[ 7]*b[15],
		                 m[ 8]*b[0]+m[ 9]*b[4]+m[10]*b[8]+m[11]*b[12], m[ 8]*b[1]+m[ 9]*b[5]+m[10]*b[9]+m[11]*b[13], m[ 8]*b[2]+m[ 9]*b[6]+m[10]*b[10]+m[11]*b[14], m[ 8]*b[3]+m[ 9]*b[7]+m[10]*b[11]+m[11]*b[15],
		                 m[12]*b[0]+m[13]*b[4]+m[14]*b[8]+m[15]*b[12], m[12]*b[1]+m[13]*b[5]+m[14]*b[9]+m[15]*b[13], m[12]*b[2]+m[13]*b[6]+m[14]*b[10]+m[15]*b[14], m[12]*b[3]+m[13]*b[7]+m[14]*b[11]+m[15]*b[15]);
	}

	//Transform position operator
	inline ZL_Vector3 operator*(const ZL_Vector3& v) const { return ZL_Vector3(v.x*m[0]+v.y*m[4]+v.z*m[8]+m[12], v.x*m[1]+v.y*m[5]+v.z*m[9]+m[13], v.x*m[2]+v.y*m[6]+v.z*m[10]+m[14]); }

	//Non modifying operators as functions
	inline bool AlmostEqual(const ZL_Matrix &b, scalar ErrorTolerance = SMALL_NUMBER) const { for (int i = 0; i < 16; i++) if (sabs(m[i]-b.m[i]) > ErrorTolerance) return false; return true; }
	inline ZL_Vector3 GetAxisX() const  { return ZL_Vector3(m[0], m[1], m[2]); }
	inline ZL_Vector3 GetAxisY() const  { return ZL_Vector3(m[4], m[5], m[6]); }
	inline ZL_Vector3 GetAxisZ() const  { return ZL_Vector3(m[8], m[9], m[10]); }
	inline ZL_Vector3 GetOrigin() const { return ZL_Vector3(m[12], m[13], m[14]); }
	inline ZL_Vector3 TransformDirection(const ZL_Vector3& v) const { return ZL_Vector3(v.x*m[0]+v.y*m[4]+v.z*m[8], v.x*m[1]+v.y*m[5]+v.z*m[9], v.x*m[2]+v.y*m[6]+v.z*m[10]); }
	inline ZL_Vector3 TransformPosition(const ZL_Vector3& v) const { return ZL_Vector3(v.x*m[0]+v.y*m[4]+v.z*m[8]+m[12], v.x*m[1]+v.y*m[5]+v.z*m[9]+m[13], v.x*m[2]+v.y*m[6]+v.z*m[10]+m[14]); }
	inline ZL_Vector3 PerspectiveTransformPosition(const ZL_Vector3& v) const { scalar w = (v.x*m[3]+v.y*m[7]+v.z*m[11]+m[15]); w=w?s(1)/w:s(1); return ZL_Vector3((v.x*m[0]+v.y*m[4]+v.z*m[8]+m[12])*w, (v.x*m[1]+v.y*m[5]+v.z*m[9]+m[13])*w,( v.x*m[2]+v.y*m[6]+v.z*m[10]+m[14])*w); }

	inline ZL_Matrix GetTransposed() const { return ZL_Matrix(m[0],m[4],m[8],m[12],m[1],m[5],m[9],m[13],m[2],m[6],m[10],m[14],m[3],m[7],m[11],m[15]); }
	inline ZL_Matrix GetInverted() const
	{
		const scalar a0 = m[0]*m[ 5]-m[1]*m[ 4], a1 = m[0]*m[ 6]-m[ 2]*m[ 4], a2 = m[0]*m[ 7]-m[ 3]*m[ 4], a3 = m[1]*m[ 6]-m[ 2]*m[ 5], a4 = m[1]*m[ 7]-m[ 3]*m[ 5], a5 = m[ 2]*m[ 7]-m[ 3]*m[ 6];
		const scalar b0 = m[8]*m[13]-m[9]*m[12], b1 = m[8]*m[14]-m[10]*m[12], b2 = m[8]*m[15]-m[11]*m[12], b3 = m[9]*m[14]-m[10]*m[13], b4 = m[9]*m[15]-m[11]*m[13], b5 = m[10]*m[15]-m[11]*m[14];
		const scalar det = a0*b5-a1*b4+a2*b3+a3*b2-a4*b1+a5*b0; if (!det) return ZL_Matrix(); const scalar rdet = s(1) / det;
		return ZL_Matrix((m[5]*b5-m[6]*b4+m[7]*b3)*rdet, (m[2]*b4-m[1]*b5-m[3]*b3)*rdet, (m[13]*a5-m[14]*a4+m[15]*a3)*rdet, (m[10]*a4-m[ 9]*a5-m[11]*a3)*rdet,
		                 (m[6]*b2-m[4]*b5-m[7]*b1)*rdet, (m[0]*b5-m[2]*b2+m[3]*b1)*rdet, (m[14]*a2-m[12]*a5-m[15]*a1)*rdet, (m[ 8]*a5-m[10]*a2+m[11]*a1)*rdet,
		                 (m[4]*b4-m[5]*b2+m[7]*b0)*rdet, (m[1]*b2-m[0]*b4-m[3]*b0)*rdet, (m[12]*a4-m[13]*a2+m[15]*a0)*rdet, (m[ 9]*a2-m[ 8]*a4-m[11]*a0)*rdet,
		                 (m[5]*b1-m[4]*b3-m[6]*b0)*rdet, (m[0]*b3-m[1]*b1+m[2]*b0)*rdet, (m[13]*a1-m[12]*a3-m[14]*a0)*rdet, (m[ 8]*a3-m[ 9]*a1+m[10]*a0)*rdet);
	}
	inline ZL_Matrix GetInverseTransposed() const
	{
		const scalar a0 = m[0]*m[ 5]-m[1]*m[ 4], a1 = m[0]*m[ 6]-m[ 2]*m[ 4], a2 = m[0]*m[ 7]-m[ 3]*m[ 4], a3 = m[1]*m[ 6]-m[ 2]*m[ 5], a4 = m[1]*m[ 7]-m[ 3]*m[ 5], a5 = m[ 2]*m[ 7]-m[ 3]*m[ 6];
		const scalar b0 = m[8]*m[13]-m[9]*m[12], b1 = m[8]*m[14]-m[10]*m[12], b2 = m[8]*m[15]-m[11]*m[12], b3 = m[9]*m[14]-m[10]*m[13], b4 = m[9]*m[15]-m[11]*m[13], b5 = m[10]*m[15]-m[11]*m[14];
		const scalar det = a0*b5-a1*b4+a2*b3+a3*b2-a4*b1+a5*b0; if (!det) return ZL_Matrix(); const scalar rdet = s(1) / det;
		return ZL_Matrix((m[ 5]*b5-m[ 6]*b4+m[ 7]*b3)*rdet, (m[ 6]*b2-m[ 4]*b5-m[ 7]*b1)*rdet, (m[ 4]*b4-m[ 5]*b2+m[ 7]*b0)*rdet, (m[ 5]*b1-m[ 4]*b3-m[ 6]*b0)*rdet,
		                 (m[ 2]*b4-m[ 1]*b5-m[ 3]*b3)*rdet, (m[ 0]*b5-m[ 2]*b2+m[ 3]*b1)*rdet, (m[ 1]*b2-m[ 0]*b4-m[ 3]*b0)*rdet, (m[ 0]*b3-m[ 1]*b1+m[ 2]*b0)*rdet,
		                 (m[13]*a5-m[14]*a4+m[15]*a3)*rdet, (m[14]*a2-m[12]*a5-m[15]*a1)*rdet, (m[12]*a4-m[13]*a2+m[15]*a0)*rdet, (m[13]*a1-m[12]*a3-m[14]*a0)*rdet,
		                 (m[10]*a4-m[ 9]*a5-m[11]*a3)*rdet, (m[ 8]*a5-m[10]*a2+m[11]*a1)*rdet, (m[ 9]*a2-m[ 8]*a4-m[11]*a0)*rdet, (m[ 8]*a3-m[ 9]*a1+m[10]*a0)*rdet);
	}

	//Self modifying operators as functions (non const call)
	inline ZL_Matrix& SetOrigin(const ZL_Vector3& v) { m[12]=v.x, m[13]=v.y, m[14]=v.z; return *this; }
	inline ZL_Matrix& SetOrigin(scalar x, scalar y = 0, scalar z = 0) { m[12]=x, m[13]=y, m[14]=z; return *this; }
	inline ZL_Matrix& AddOrigin(const ZL_Vector3& v) { m[12]+=v.x, m[13]+=v.y, m[14]+=v.z; return *this; }
	inline ZL_Matrix& AddOrigin(scalar x, scalar y = 0, scalar z = 0) { m[12]+=x, m[13]+=y, m[14]+=z; return *this; }
	inline ZL_Matrix& Scale(scalar f) { m[0] *= f; m[1] *= f; m[2] *= f; m[3] *= f; m[4] *= f; m[5] *= f; m[6] *= f; m[7] *= f; m[8] *= f; m[9] *= f; m[10] *= f; m[11] *= f; return *this; }
	inline ZL_Matrix& Scale(ZL_Vector3 v) { m[0] *= v.x; m[1] *= v.x; m[2] *= v.x; m[3] *= v.x; m[4] *= v.y; m[5] *= v.y; m[6] *= v.y; m[7] *= v.y; m[8] *= v.z; m[9] *= v.z; m[10] *= v.z; m[11] *= v.z; return *this; }
	inline ZL_Matrix& Scale(scalar x, scalar y, scalar z) { m[0] *= x; m[1] *= x; m[2] *= x; m[3] *= x; m[4] *= y; m[5] *= y; m[6] *= y; m[7] *= y; m[8] *= z; m[9] *= z; m[10] *= z; m[11] *= z; return *this; }
	inline ZL_Matrix& Transpose() { return *this = GetTransposed(); }
	inline ZL_Matrix& Inverse() { return *this = GetInverted(); }
	inline ZL_Matrix& InverseTranspose() { return *this = GetInverseTransposed(); }

	//Building various transformation matrix types
	static inline ZL_Matrix MakeTranslate(const ZL_Vector3& v) { return ZL_Matrix(1,0,0,0,0,1,0,0,0,0,1,0,v.x,v.y,v.z,1); }
	static inline ZL_Matrix MakeTranslate(scalar x, scalar y = 0, scalar z = 0) { return ZL_Matrix(1,0,0,0,0,1,0,0,0,0,1,0,x,y,z,1); }
	static inline ZL_Matrix MakeScale(scalar scale) { return ZL_Matrix(scale,0,0,0,0,scale,0,0,0,0,scale,0,0,0,0,1); }
	static inline ZL_Matrix MakeScale(scalar x, scalar y, scalar z = 0) { return ZL_Matrix(x,0,0,0,0,y,0,0,0,0,z,0,0,0,0,1); }
	static inline ZL_Matrix MakeScale(const ZL_Vector3& v) { return ZL_Matrix(v.x,0,0,0,0,v.y,0,0,0,0,v.z,0,0,0,0,1); }
	static inline ZL_Matrix MakeRotateX(scalar AngleRad) { scalar sn = ssin(AngleRad), cs = scos(AngleRad); return ZL_Matrix(1,0,0,0,0,cs,sn,0,0,-sn,cs,0,0,0,0,1); }
	static inline ZL_Matrix MakeRotateY(scalar AngleRad) { scalar sn = ssin(AngleRad), cs = scos(AngleRad); return ZL_Matrix(cs,0,-sn,0,0,1,0,0,sn,0,cs,0,0,0,0,1); }
	static inline ZL_Matrix MakeRotateZ(scalar AngleRad) { scalar sn = ssin(AngleRad), cs = scos(AngleRad); return ZL_Matrix(cs,sn,0,0,-sn,cs,0,0,0,0,1,0,0,0,0,1); }
	static inline ZL_Matrix MakeRotate(const ZL_Quat& Quat)
	{
		scalar x = Quat.x, y = Quat.y, z = Quat.z, w = Quat.w, x2 = x + x, y2 = y + y, z2 = z + z, xx = x*x2, xy = x*y2, xz = x*z2, yy = y*y2, yz = y*z2, zz = z*z2, wx = w*x2, wy = w*y2, wz = w*z2;
		return ZL_Matrix(1 - (yy + zz), xy + wz, xz - wy, 0, xy - wz, 1 - (xx + zz), yz + wx, 0, xz + wy, yz - wx, 1 - (xx + yy), 0, 0, 0, 0, 1);
	}
	static inline ZL_Matrix MakeRotateTranslate(const ZL_Quat& q, const ZL_Vector3& v)
	{
		scalar x = q.x, y = q.y, z = q.z, w = q.w, x2 = x + x, y2 = y + y, z2 = z + z, xx = x*x2, xy = x*y2, xz = x*z2, yy = y*y2, yz = y*z2, zz = z*z2, wx = w*x2, wy = w*y2, wz = w*z2;
		return ZL_Matrix(1 - (yy + zz), xy + wz, xz - wy, 0, xy - wz, 1 - (xx + zz), yz + wx, 0, xz + wy, yz - wx, 1 - (xx + yy), 0, v.x, v.y, v.z, 1);
	}
	static inline ZL_Matrix MakeRotate(const ZL_Vector3& normalized_forward, const ZL_Vector3& normalized_up = ZL_Vector3(0,1,0))
	{
		const ZL_Vector3 N = normalized_forward, V = (normalized_up ^ N).VecNorm(), U = (N ^ V);
		return ZL_Matrix(V.x,V.y,V.z,0,U.x,U.y,U.z,0,N.x,N.y,N.z,0,0,0,0,1);
	}
	static inline ZL_Matrix MakeRotateTranslate(const ZL_Vector3& normalized_forward, const ZL_Vector3& location, const ZL_Vector3& normalized_up = ZL_Vector3(0,1,0))
	{
		const ZL_Vector3 N = normalized_forward, V = (normalized_up ^ N).VecNorm(), U = (N ^ V);
		return ZL_Matrix(V.x,V.y,V.z,0,U.x,U.y,U.z,0,N.x,N.y,N.z,0,location.x,location.y,location.z,1);
	}
	static inline ZL_Matrix MakeRotateTranslateScale(const ZL_Vector3& normalized_forward, const ZL_Vector3& location, const ZL_Vector3& scale, const ZL_Vector3& normalized_up = ZL_Vector3(0,1,0))
	{
		const ZL_Vector3 N = normalized_forward * scale.z, V = (normalized_up ^ N).SetLength(scale.x), U = (N ^ V).SetLength(scale.y);
		return ZL_Matrix(V.x,V.y,V.z,0,U.x,U.y,U.z,0,N.x,N.y,N.z,0,location.x,location.y,location.z,1);
	}
	static inline ZL_Matrix MakeCamera(const ZL_Vector3& location, const ZL_Vector3& normalized_forward, const ZL_Vector3& normalized_up = ZL_Vector3(0,1,0))
	{
		const ZL_Vector3 N = -normalized_forward, V = (normalized_up ^ N).VecNorm(), U = (N ^ V);
		const ZL_Vector3 offset(location.x*V.x+location.y*V.y+location.z*V.z, location.x*U.x+location.y*U.y+location.z*U.z, location.x*N.x+location.y*N.y+location.z*N.z);
		return ZL_Matrix(V.x,U.x,N.x,0,V.y,U.y,N.y,0,V.z,U.z,N.z,0,-offset.x,-offset.y,-offset.z,1);
	}
	static inline ZL_Matrix MakeLookAt(const ZL_Vector3& eye, const ZL_Vector3& center, const ZL_Vector3& normalized_up = ZL_Vector3(0,1,0))
	{
		const ZL_Vector3 N = (eye - center).VecNorm(), V = (normalized_up ^ N).VecNorm(), U = (N ^ V);
		const ZL_Vector3 offset(eye.x*V.x+eye.y*V.y+eye.z*V.z, eye.x*U.x+eye.y*U.y+eye.z*U.z, eye.x*N.x+eye.y*N.y+eye.z*N.z);
		return ZL_Matrix(V.x,U.x,N.x,0,V.y,U.y,N.y,0,V.z,U.z,N.z,0,-offset.x,-offset.y,-offset.z,1);
	}
	static inline ZL_Matrix MakePerspectiveHorizontal(scalar fov_x_degree, scalar aspect, scalar znear, scalar zfar)
	{
		scalar fx = s(1) / stan(fov_x_degree*PIOVER180*s(.5)), fy = fx*aspect, zf = s(1) / (znear - zfar);
		return ZL_Matrix(fx,0,0,0,0,fy,0,0,0,0,(zfar+znear)*zf,-1,0,0,zfar*znear*2*zf,0);
	}
	static inline ZL_Matrix MakeOrtho(scalar xmin, scalar xmax, scalar ymin, scalar ymax, scalar zmin, scalar zmax)
	{
		scalar xx = s(1) / (xmin - xmax), yy = s(1) / (ymin - ymax), zz = s(1) / (zmin - zmax);
		return ZL_Matrix(s(-2) * xx, 0, 0, 0, 0, s(-2) * yy, 0, 0, 0, 0, 2 * zz, 0, (xmin + xmax) * xx, (ymin + ymax) * yy, (zmin + zmax) * zz, 1);
	}
};

inline ZL_Quat::ZL_Quat(struct ZL_Matrix& m)
{
	if (m.m[0] > m.m[5] && m.m[0] > m.m[10])
	{
		scalar r = sqrtf(s(1)+m.m[0]-m.m[5]-m.m[10]), rr = s(.5)/r;
		if (r) { x = r/2, y = (m.m[4]+m.m[1])*rr, z = (m.m[2]+m.m[8])*rr, w = (m.m[6]-m.m[9])*rr; return; }
	}
	else if(m.m[5] > m.m[0] && m.m[5] > m.m[10])
	{
		scalar r = sqrtf(s(1)+m.m[5]-m.m[10]-m.m[0]), rr = s(.5)/r;
		if (r) { x = (m.m[4]+m.m[1])*rr, y = r/2, z = (m.m[9]+m.m[6])*rr, w = (m.m[8]-m.m[2])*rr; return; }
	}
	else
	{
		float r = sqrtf(s(1)+m.m[10]-m.m[0]-m.m[5]), rr = s(.5)/r;
		if (r) { x = (m.m[2]+m.m[8])*rr, y = (m.m[9]+m.m[6])*rr, z = r/2, w = (m.m[1]-m.m[4])*rr; return; }
	}
	x = 0, y = 0, z = 0, w = 1;
}

#endif //__ZL_MATH3D__
