//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2024 Altair Engineering Inc.
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
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.
#include <algorithm>
#include <vector>
#include <utility>

#define _FCALL

#ifdef MYREAL8
#define my_real double
#else
#define my_real float
#endif


template<typename K, typename V>
void stlsort_generic_generic(int *len,  K *keys, V *values){
    int n = *len;
    std::vector<std::pair<K, V>> pairs(n);

    for (int i = 0; i < n; ++i) {
        pairs[i] = std::make_pair(keys[i], values[i]);
    }

    std::sort(pairs.begin(), pairs.end(), [](const std::pair<K, V> &a, const std::pair<K, V> &b) {
        return a.first < b.first;
    });

    for (int i = 0; i < n; ++i) {
        keys[i] = pairs[i].first;
        values[i] = pairs[i].second;
    }
}

extern "C" {
// sort array
    void stlsort(int * len, my_real * array)
    {
            std::sort(array,array+ *len);
    }
    void stlsort__(int * len, my_real * array)
    {
            std::sort(array,array+ *len);
    }
    void _FCALL stlsort_(int * len, my_real * array)
    {
            std::sort(array,array+ *len);
    }
    void _FCALL STLSORT(int * len, my_real * array)
    {
            std::sort(array,array+ *len);
    }
    void STLSORT_(int * len, my_real * array)
    {
            std::sort(array,array+ *len);
    }
// sort array with int and key
    void stlsort_int_int(int *len, int* keys,  int *values) {
         stlsort_generic_generic<int,int>(len, keys, values); 
    }
    void stlsort_int_int__(int *len, int* keys,  int *values) {
         stlsort_generic_generic<int,int>(len, keys, values); 
    }
    void _FCALL stlsort_int_int_(int *len, int* keys,  int *values) {
         stlsort_generic_generic<int,int>(len, keys, values); 
    }
    void _FCALL STLSORT_INT_INT(int *len, int* keys,  int *values) {
         stlsort_generic_generic<int,int>(len, keys, values); 
    }
    void STLSORT_INT_INT_(int *len, int* keys,  int *values) {
         stlsort_generic_generic<int,int>(len, keys, values); 
    }
}

