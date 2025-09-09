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




////////////////////////////////////////////////////////////////////////////////////

#if !defined(SDIUTILS__INCLUDED_)
#define SDIUTILS__INCLUDED_

#include <sdiDefs.h>

#include <algorithm>

// *********************************************************************************
// sdiVector utils
// *********************************************************************************

//! sort a vector
template<typename T>
inline void sdiVectorSort(sdiVector<T>& vec)
{
    std::sort(vec.begin(), vec.end());
}

//! make consecutive duplicate elements in the vector between first and last unique
template<typename T>
inline void sdiVectorUnique(sdiVector<T>& vec)
{
    vec.resize(std::unique(vec.begin(), vec.end()) - vec.begin());
}

//! sort and unique a vector
template<typename T>
inline void sdiVectorSortAndUnique(sdiVector<T>& vec)
{
    std::sort(vec.begin(), vec.end());
    vec.resize(std::unique(vec.begin(), vec.end()) - vec.begin());
}

//! return true if the vector contains elem, else return false
template<typename T>
inline bool sdiVectorContains(const sdiVector<T>& vec, const T& elem)
{
    return std::find(vec.begin(), vec.end(), elem) != vec.end();
}

//! remove elem from vector if it exists and return true, else return false
template<typename T>
inline bool sdiVectorRemove(sdiVector<T>& vec, const T& elem)
{
    typename sdiVector<T>::iterator it = 
        std::remove(vec.begin(), vec.end(), elem);
    if(it != vec.end())
    {
        vec.resize(it - vec.begin());
        return true;
    }
    return false;
}


#endif //! !defined(SDIUTILS__INCLUDED_)
