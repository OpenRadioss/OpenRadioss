/*Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2025 Altair Engineering Inc.
//Copyright>
//Copyright>    This program is free software: you can redistribute it and/or modify
//Copyright>    it under the terms of the GNU Affero General Public License as published by
//Copyright>    the Free Software Foundation, either version 3 of the License, or
//Copyright>    (at your option) any later version.
//Copyright>
//Copyright>    This program is distributed in the hope that it will be useful,
//Copyright>    but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>    GNU Affero General Public License for more details.
//Copyright>
//Copyright>    You should have received a copy of the GNU Affero General Public License
//Copyright>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>    Commercial Alternative: Altair Radioss Software
//Copyright>
//Copyright>    As an alternative to this open-source version, Altair also offers Altair Radioss
//Copyright>    software under a commercial license.  Contact Altair to discuss further if the
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.*/




////////////////////////////////////////////////////////////////////

#ifndef __SDITRIPLE_H__
#define __SDITRIPLE_H__

#include <sdiUtilsDefs.h>
#include <math.h>
#include <string.h>

#define SDITRIPLE_ZERO  1e-8
//! Tolerance used for finding equality between double values
#define SDITRIPLE_ZERO2 1e-20

// -------------------------------------------------------------------
//! \class sdiTTriple sdiTTriple.h
//! \brief A class to represent a triple coordinate/vector.
//!
//! NOTE: Always use the typedef for double (float and int might get added)
//! \code
//! typedef sdiTTriple<double> sdiTriple;
//! typedef sdiTTriple<float>  hwTripleF;
//! typedef sdiTTriple<int>    hwTripleI;
//! \endcode
//!
// -------------------------------------------------------------------
template<typename T>
class sdiTTriple
{
public:
    //! Create a triple representing origin.
    sdiTTriple() { xyz[0] = xyz[1] = xyz[2] = T(0); }
    //! Create a triple with specified x, y, z components.
    sdiTTriple(T x, T y, T z) { xyz[0] = x; xyz[1] = y; xyz[2] = z; }
    //HwConvert }}
            //! Create a triple using a type T array.
    sdiTTriple(const T v[3]) { memcpy(xyz, v, sizeof(T) * 3); }
    //HwConvert {{
            //! Copy constructor.
    sdiTTriple(const sdiTTriple<T> &s) { memcpy(xyz, s.xyz, sizeof(xyz)); }
    //! Typical vector a->b constructor - essentially b-a
    sdiTTriple(const sdiTTriple<T> &a, const sdiTTriple<T> &b)
    {
        xyz[0] = b.X() - a.X();
        xyz[1] = b.Y() - a.Y();
        xyz[2] = b.Z() - a.Z();
    }
    //! Destructor
    ~sdiTTriple() { ; }

    //! Equality comparison
    /*! \return 'true' if equal, 'false' otherwise */
    bool operator == (const sdiTTriple<T> &t) const
    {
        return IsEqualPrivate(t);
    }
    //! Less than comparison
    /*! \return 'true' if less, 'false' otherwise */
    bool operator < (const sdiTTriple<T> &t) const
    {
        return IsLessThanPrivate(t);
    }
    //HwConvert }}
            //! Not equals comparison.
            /*! \return 'true' if not equal, 'false' otherwise */
    bool operator !=(const sdiTTriple<T>& t) const { return !(*this == t); }
    //HwConvert {{
            //! \brief Indexing operator. 
            //!
            //! no bounds checking performed
            //! \param index - maybe 0, 1 or 2
            //! \return value at specified index
    T operator[](size_t index) const { return xyz[index]; }

    //! \brief Indexing operator. Can be used as lvalue.
    //!
    //! no bounds checking performed
    //! \param index - maybe 0, 1 or 2
    //! \return value at specified index
    T& operator[](size_t index) { return xyz[index]; }


    //HwConvert }}
            //! Assign a constant value.
            /*! \return reference to self */
    sdiTTriple<T>& operator=(T s) { xyz[0] = xyz[1] = xyz[2] = s; return *this; }
    //! Subtract a constant value.
    /*! \return reference to self */
    sdiTTriple<T>& operator-=(T s) { xyz[0] -= s; xyz[1] -= s; xyz[2] -= s; return *this; }
    //! Add a constant to value.
    /*! \return reference to self */
    sdiTTriple<T>& operator+=(T s) { xyz[0] += s; xyz[1] += s; xyz[2] += s; return *this; }
    //! Multiply by a constant value.
    /*! \return reference to self */
    sdiTTriple<T>& operator*=(T s) { xyz[0] *= s; xyz[1] *= s; xyz[2] *= s; return *this; }
    //! Divide by a constant value.
    /*! \return reference to self */
    sdiTTriple<T>& operator/=(T s) { xyz[0] /= s; xyz[1] /= s; xyz[2] /= s; return *this; }
    //! Assign an array of type T
    /*! \return reference to self */
    sdiTTriple<T>& operator= (const T s[3]) { memcpy(xyz, s, sizeof(T) * 3); return *this; }
    //! Subtract an array of type T
    /*! \return reference to self */
    sdiTTriple<T>& operator-=(const T s[3]) { xyz[0] -= s[0]; xyz[1] -= s[1]; xyz[2] -= s[2]; return *this; }
    //! Add an array of type T
    /*! \return reference to self */
    sdiTTriple<T>& operator+=(const T s[3]) { xyz[0] += s[0]; xyz[1] += s[1]; xyz[2] += s[2]; return *this; }
    //! Assign another triple
    /*! \return reference to self */
    sdiTTriple<T>& operator= (const sdiTTriple<T>&s)  { memcpy(xyz, s.xyz, sizeof(T) * 3); return *this; }
    //! Subtract another triple
    /*! \return reference to self */
    sdiTTriple<T>& operator-=(const sdiTTriple<T>&s) { X(X() - s.X()); Y(Y() - s.Y()); Z(Z() - s.Z()); return *this; }
    //! Add another triple
    /*! \return reference to self */
    sdiTTriple<T>& operator+=(const sdiTTriple<T>&s) { X(X() + s.X()); Y(Y() + s.Y()); Z(Z() + s.Z()); return *this; }
    //HwConvert {{
            //! Negate x, y, and z components
            /*! \return result as another triple by value */
    sdiTTriple<T> operator-(void) const { return sdiTTriple<T>(-xyz[0], -xyz[1], -xyz[2]); }
    //! Subtract another triple
    /*! \return result as another triple by value */
    sdiTTriple<T> operator-(const sdiTTriple<T> &t) const { return sdiTTriple<T>(xyz[0]-t[0], xyz[1]-t[1], xyz[2]-t[2]); }
    //! Add another triple
    /*! \return result as another triple by value */
    sdiTTriple<T> operator+(const sdiTTriple<T> &t) const { return sdiTTriple<T>(xyz[0]+t[0], xyz[1]+t[1], xyz[2]+t[2]); }
    //! Multiply by a constant value.
    /*! \return result as another triple by value */
    sdiTTriple<T> operator*(T s) const { return sdiTTriple(xyz[0]*s, xyz[1]*s, xyz[2]*s); }
    //HwConvert }}  
            //! Divide by a constant value.
            /*! \return result as another triple by value */
    sdiTTriple<T> operator/(const T &s) const { return sdiTTriple<T>(xyz[0]/s, xyz[1]/s, xyz[2]/s); }
    //! Cross Product with an array of type T
    /*! \return reference to self */
    sdiTTriple<T>& operator*=(const T s[3])
    {
        T x = xyz[0];
        T y = xyz[1];
        xyz[0] = (y*s[2] - Z()*s[1]);
        xyz[1] = (Z()*s[0] - x*s[2]);
        xyz[2] = (x*s[1] - y*s[0]);
        return *this;
    }
    //! Cross Product with another triple
    /*! \return reference to self */
    sdiTTriple<T>& operator*=(const sdiTTriple<T> &s)
    {
        T x = xyz[0];
        T y = xyz[1];
        xyz[0] = y*s.Z() - Z()*s.Y();
        xyz[1] = Z()*s.X() - x*s.Z();
        xyz[2] = x*s.Y() - y*s.X();
        return *this;
    }
    //! Cross Product with another triple
    /*! \return result as another triple by value */
    sdiTTriple<T> operator*(const sdiTTriple<T> &s) const
    {
        return sdiTTriple<T>(Y()*s.Z() - Z()*s.Y(), Z()*s.X() - X()*s.Z(), X()*s.Y() - Y()*s.X());
    }

    //! Access directly the xyz data member.
    T* operator*() { return xyz; }
    //! Access directly the xyz data member.
    const T *operator*() const { return xyz; }

    //! Dot Product with an array of type T
    /*! \return resultant scalar */
    double operator%(const T s[3]) const { return (X()*s[0] + Y()*s[1] + Z()*s[2]); }
    //! Dot Product with another triple
    /*! \return resultant scalar */
    double operator%(const sdiTTriple<T>& s) const { return (X()*s.X() + Y()*s.Y() + Z()*s.Z()); }
    //HwConvert {{
            //! Get value of X component of triple
    T X() const { return xyz[0]; }
    //! Get value of Y component of triple
    T Y() const { return xyz[1]; }
    //! Get value of Z component of triple
    T Z() const { return xyz[2]; }

    //! Get value of X component of triple
    T GetX() const { return xyz[0]; }
    //! Get value of Y component of triple
    T GetY() const { return xyz[1]; }
    //! Get value of Z component of triple
    T GetZ() const { return xyz[2]; }

    //! Set value of X component of triple
    void X(T v) { xyz[0] = v; }
    //! Set value of Y component of triple
    void Y(T v) { xyz[1] = v; }
    //! Set value of Z component of triple
    void Z(T v) { xyz[2] = v; }

    //! Set value of X component of triple
    void SetX(T v) { xyz[0] = v; }
    //! Set value of Y component of triple
    void SetY(T v) { xyz[1] = v; }
    //! Set value of Z component of triple
    void SetZ(T v) { xyz[2] = v; }

    //! Set self to 0,0,0 (null vector / origin)
    void SetZero() { xyz[0] = xyz[1] = xyz[2] = T(0); }

    //! Calculate length of this triple
    /*! \return length */
    double Len() const     { return Length(); }
    //! Calculate length of this triple
    /*! \return length */
    double Length() const  { return sqrt(double(xyz[0]*xyz[0] + xyz[1]*xyz[1] + xyz[2]*xyz[2])); }
    //! \brief Calculate square of length of this triple
    //!
    //! Faster where actual length is not required
    //! \return square of length
    double Len2() const    { return Length2(); }
    //! \brief Calculate square of length of this triple
    //!
    //! Faster where actual length is not required
    //! \return square of length
    double Length2() const { return (xyz[0]*xyz[0] + xyz[1]*xyz[1] + xyz[2]*xyz[2]); }

    //! \brief Normalizes a triple to unit Length.
    //!
    //! \param len - gives the length before normalization.
    //! \return result as another triple by value
    sdiTTriple<T> Normalize(double& len)
    {
        len = Len();

        if(len==0.0 || len==1.0)
            return *this;

        if(len > SDITRIPLE_ZERO) {
            *this /= T(len);
        }
        else {
            // Input vector too small for correct normalization!
            // If you get this assert, you need to specifically
            // handle case of small vectors in your code.
            //assert(0);
        }
        return *this;
    }
    //! Normalizes a triple to unit Length 
    /*! \return result as another triple by value */
    sdiTTriple<T> Normalize()
    {
        double l = Len2();

        if(l==0.0 || l==1.0)
            return *this;

        if(l > SDITRIPLE_ZERO2) {
            *this /= T(sqrt(double(l)));
        }
        else {
            // Input vector too small for correct normalization!
            // If you get this assert, you need to specifically
            // handle case of small vectors in your code.
            //assert(0);
        }
        return *this;
    }

    //! Add another triple
    void Add(const sdiTTriple<T> &s) { *this += s; }
    //! Add x, y, and z components
    void Add(T x, T y, T z) { *this += sdiTTriple<T>(x, y, z); }
    //! Subtract another triple
    void Sub(const sdiTTriple<T> &s) { *this -= s; }
    //! Subtract specified x, y, and z components
    void Sub(T x, T y, T z) { *this -= sdiTTriple<T>(x, y, z); }
    //! Subtract another triple
    void Subtract(const sdiTTriple<T> &s) { *this -= s; }
    //! Subtract specified x, y, and z components
    void Subtract(T x, T y, T z) { *this -= sdiTTriple<T>(x, y, z); }

    //! Assign another triple
    void Assign(const sdiTTriple<T> &s) { *this = s; }
    //! Assign specified x, y, z components.
    void Assign(T x, T y, T z) { *this = sdiTTriple<T>(x, y, z); }
    //! Assign a constant value.
    void Assign(T v) { *this = v; }
    //! Assign another triple
    void Set(const sdiTTriple<T> &s) { *this = s; }
    //! Assign specified x, y, z components.
    void Set(T x, T y, T z) { *this = sdiTTriple<T>(x, y, z); }
    //! Assign a constant value.
    void Set(T v) { *this = v; }

    //! Multiply by a constant value.
    void Mul(T v) { *this *= v; }
    //! Multiply by a constant value.
    void Multiply(T v) { *this *= v; }
    //! Divide by a constant value.
    void Div(T v) { *this /= v; }
    //! Divide by a constant value.
    void Divide(T v) { *this /= v; }

private:
    //! Set positive infinity to double precision Triple.
    void SetPosInfinityPrivate	(sdiTTriple<double>& dTriple);

    //! Set negative infinity to double precision Triple.
    void SetNegInfinityPrivate	(sdiTTriple<double>& dTriple);

    //! Check if at least one of x,y,z is infinity
    bool HasInfinityPrivate		(const sdiTTriple<double>& dTriple) const;

    //! Equality comparison of double precision Triples.
    /*! \return 'true' if equal, 'false' otherwise */
    bool IsEqualPrivate			(const sdiTTriple<double>& dTriple) const;

    //! Less than comparison of double precision Triples.
    /*! \return 'true' if less, 'false' otherwise */
    bool IsLessThanPrivate		(const sdiTTriple<double>& dTriple) const;


private:
    //! Data representing triple
    T xyz[3];
};

//! Check for equality
template <typename T>
bool sdiTTriple<T>::IsEqualPrivate(const sdiTTriple<double>& dTriple) const 
{ 
    if((fabs(X() - dTriple.X()) <= SDITRIPLE_ZERO) &&(fabs(Y() - dTriple.Y()) <= SDITRIPLE_ZERO) && (fabs(Z() - dTriple.Z()) <= SDITRIPLE_ZERO)) 
    {
        return true;
    }
    return false;
}

//! Less than comparison.
template <typename T>
bool sdiTTriple<T>::IsLessThanPrivate( const sdiTTriple<double>& dTriple ) const
{
    double local_x = (double)X();
    double local_y = (double)Y();
    double local_z = (double)Z();
    if (fabs( local_x - dTriple.X() ) > SDITRIPLE_ZERO)
        return local_x < dTriple.X();
    if (fabs( local_y - dTriple.Y() ) > SDITRIPLE_ZERO)
        return local_y < dTriple.Y();
    if (fabs( local_z - dTriple.Z() ) > SDITRIPLE_ZERO)
        return local_z < dTriple.Z();

    return false;
}


typedef sdiTTriple<double> sdiTriple;

typedef sdiVector<sdiTriple> sdiTripleList;
typedef sdiVector<sdiTripleList> sdiTripleList2;

#endif // __SDITRIPLE_H__
