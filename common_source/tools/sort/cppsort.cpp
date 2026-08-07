//Copyright>        OpenRadioss
//Copyright>        Copyright (C) 2026 Siemens
//Copyright>
//Copyright>        This program is free software: you can redistribute it and/or modify
//Copyright>        it under the terms of the GNU Affero General Public License as published by
//Copyright>        the Free Software Foundation, either version 3 of the License, or
//Copyright>        (at your option) any later version.
//Copyright>
//Copyright>        This program is distributed in the hope that it will be useful,
//Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>        GNU Affero General Public License for more details.
//Copyright>
//Copyright>        You should have received a copy of the GNU Affero General Public License
//Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>        Commercial Alternative: Simcenter Radioss Software
//Copyright>
//Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
//Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
//Copyright>        commercial version may interest you: 
//Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
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

    std::stable_sort(pairs.begin(), pairs.end(), [](const std::pair<K, V> &a, const std::pair<K, V> &b) {
        return a.first < b.first;
    });

    for (int i = 0; i < n; ++i) {
        keys[i] = pairs[i].first;
        values[i] = pairs[i].second;
    }
}

// Sort by two real keys (key1 primary, key2 secondary), tracking a 1-based integer permutation.
// Used for reproducible /DT/NODA sorting independent of MPI partition order.
template<typename K>
void stlsort_real2_int_generic(int *len, K *key1, K *key2, int *perm) {
    int n = *len;
    // Build index array (0-based internally, returned as 1-based)
    std::vector<int> idx(n);
    for (int i = 0; i < n; ++i) idx[i] = i;

    std::stable_sort(idx.begin(), idx.end(), [&](int a, int b) {
        if (key1[a] != key1[b]) return key1[a] < key1[b];
        return key2[a] < key2[b];
    });

    // Reorder key1 and key2 in place and write 1-based perm
    std::vector<K> k1(n), k2(n);
    for (int i = 0; i < n; ++i) { k1[i] = key1[idx[i]]; k2[i] = key2[idx[i]]; }
    for (int i = 0; i < n; ++i) { key1[i] = k1[i]; key2[i] = k2[i]; perm[i] = idx[i] + 1; }
}

// Sort by real key (primary) then integer key (secondary), tracking a 1-based integer permutation.
// Used for reproducible /DT/NODA sorting: primary=DT ratio, secondary=node user ID.
template<typename K>
void stlsort_real_int2_int_generic(int *len, K *key1, int *key2, int *perm) {
    int n = *len;
    std::vector<int> idx(n);
    for (int i = 0; i < n; ++i) idx[i] = i;

    std::stable_sort(idx.begin(), idx.end(), [&](int a, int b) {
        if (key1[a] != key1[b]) return key1[a] < key1[b];
        return key2[a] < key2[b];
    });

    // Reorder key1 and key2 in place and write 1-based perm
    std::vector<K> k1(n);
    std::vector<int> k2(n);
    for (int i = 0; i < n; ++i) { k1[i] = key1[idx[i]]; k2[i] = key2[idx[i]]; }
    for (int i = 0; i < n; ++i) { key1[i] = k1[i]; key2[i] = k2[i]; perm[i] = idx[i] + 1; }
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

 // sort array with real and key
    void stlsort_real_int(int *len, my_real* keys,  int *values) {
         stlsort_generic_generic<my_real,int>(len, keys, values); 
    }
    void stlsort_real_int__(int *len, my_real* keys,  int *values) {
         stlsort_generic_generic<my_real,int>(len, keys, values); 
    }
    void _FCALL stlsort_real_int_(int *len, my_real* keys,  int *values) {
         stlsort_generic_generic<my_real,int>(len, keys, values); 
    }
    void _FCALL STLSORT_REAL_INT(int *len, my_real* keys,  int *values) {
         stlsort_generic_generic<my_real,int>(len, keys, values); 
    }
    void STLSORT_REAL_INT_(int *len, my_real* keys,  int *values) {
         stlsort_generic_generic<my_real,int>(len, keys, values); 
    } 

// sort by two real keys (primary, secondary) with 1-based integer permutation output
    void stlsort_real2_int(int *len, my_real* key1, my_real* key2, int *perm) {
         stlsort_real2_int_generic<my_real>(len, key1, key2, perm);
    }
    void stlsort_real2_int__(int *len, my_real* key1, my_real* key2, int *perm) {
         stlsort_real2_int_generic<my_real>(len, key1, key2, perm);
    }
    void _FCALL stlsort_real2_int_(int *len, my_real* key1, my_real* key2, int *perm) {
         stlsort_real2_int_generic<my_real>(len, key1, key2, perm);
    }
    void _FCALL STLSORT_REAL2_INT(int *len, my_real* key1, my_real* key2, int *perm) {
         stlsort_real2_int_generic<my_real>(len, key1, key2, perm);
    }
    void STLSORT_REAL2_INT_(int *len, my_real* key1, my_real* key2, int *perm) {
         stlsort_real2_int_generic<my_real>(len, key1, key2, perm);
    }

// sort by real primary key + integer secondary key (e.g. node user ID) with 1-based permutation output
    void stlsort_real_int2_int(int *len, my_real* key1, int* key2, int *perm) {
         stlsort_real_int2_int_generic<my_real>(len, key1, key2, perm);
    }
    void stlsort_real_int2_int__(int *len, my_real* key1, int* key2, int *perm) {
         stlsort_real_int2_int_generic<my_real>(len, key1, key2, perm);
    }
    void _FCALL stlsort_real_int2_int_(int *len, my_real* key1, int* key2, int *perm) {
         stlsort_real_int2_int_generic<my_real>(len, key1, key2, perm);
    }
    void _FCALL STLSORT_REAL_INT2_INT(int *len, my_real* key1, int* key2, int *perm) {
         stlsort_real_int2_int_generic<my_real>(len, key1, key2, perm);
    }
    void STLSORT_REAL_INT2_INT_(int *len, my_real* key1, int* key2, int *perm) {
         stlsort_real_int2_int_generic<my_real>(len, key1, key2, perm);
    }
}
