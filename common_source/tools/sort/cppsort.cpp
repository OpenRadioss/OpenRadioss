//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2026 Altair Engineering Inc.
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

// C++ wrappers around STL sort, callable from Fortran via iso_c_binding.
// Both float and double variants are provided explicitly so callers are
// not limited to a single precision configured at compile time.

#include <algorithm>
#include <utility>
#include <vector>

// ---------------------------------------------------------------------------
// Internal generic key-value helpers
// ---------------------------------------------------------------------------

template<typename K, typename V>
static void stlsort_kv_impl(int n, K *keys, V *values)
{
    std::vector<std::pair<K, V>> pairs(n);
    for (int i = 0; i < n; ++i)
        pairs[i] = {keys[i], values[i]};

    std::sort(pairs.begin(), pairs.end(),
              [](const std::pair<K,V> &a, const std::pair<K,V> &b){
                  return a.first < b.first;
              });

    for (int i = 0; i < n; ++i) {
        keys[i]   = pairs[i].first;
        values[i] = pairs[i].second;
    }
}

template<typename K, typename V>
static void stlstable_sort_kv_impl(int n, K *keys, V *values)
{
    std::vector<std::pair<K, V>> pairs(n);
    for (int i = 0; i < n; ++i)
        pairs[i] = {keys[i], values[i]};

    std::stable_sort(pairs.begin(), pairs.end(),
                     [](const std::pair<K,V> &a, const std::pair<K,V> &b){
                         return a.first < b.first;
                     });

    for (int i = 0; i < n; ++i) {
        keys[i]   = pairs[i].first;
        values[i] = pairs[i].second;
    }
}

// ---------------------------------------------------------------------------
// Public C interface  (consumed by cppsort_mod via iso_c_binding)
// ---------------------------------------------------------------------------

extern "C" {

// --- sort a plain array ------------------------------------------------------

void stlsort_float (int *len, float  *array) { std::sort(array, array + *len); }
void stlsort_double(int *len, double *array) { std::sort(array, array + *len); }
void stlsort_int   (int *len, int    *array) { std::sort(array, array + *len); }

// --- sort integer keys, carrying integer values ------------------------------

void stlsort_int_int(int *len, int *keys, int *values)
{
    stlsort_kv_impl<int, int>(*len, keys, values);
}

// --- sort float/double keys, carrying integer values ------------------------

void stlsort_float_int(int *len, float *keys, int *values)
{
    stlsort_kv_impl<float, int>(*len, keys, values);
}

void stlsort_double_int(int *len, double *keys, int *values)
{
    stlsort_kv_impl<double, int>(*len, keys, values);
}

// --- sort float/double keys, carrying float/double values -------------------

void stlsort_float_float(int *len, float *keys, float *values)
{
    stlsort_kv_impl<float, float>(*len, keys, values);
}

void stlsort_double_double(int *len, double *keys, double *values)
{
    stlsort_kv_impl<double, double>(*len, keys, values);
}

// ---------------------------------------------------------------------------
// stable_sort – same signatures, preserves relative order of equal keys
// ---------------------------------------------------------------------------

// --- sort a plain array ------------------------------------------------------
void stlstable_sort_int   (int *len, int    *array) { std::stable_sort(array, array + *len); }
void stlstable_sort_float (int *len, float  *array) { std::stable_sort(array, array + *len); }
void stlstable_sort_double(int *len, double *array) { std::stable_sort(array, array + *len); }

// --- sort integer keys, carrying integer values ------------------------------

void stlstable_sort_int_int(int *len, int *keys, int *values)
{
    stlstable_sort_kv_impl<int, int>(*len, keys, values);
}

// --- sort float/double keys, carrying integer values ------------------------

void stlstable_sort_float_int(int *len, float *keys, int *values)
{
    stlstable_sort_kv_impl<float, int>(*len, keys, values);
}

void stlstable_sort_double_int(int *len, double *keys, int *values)
{
    stlstable_sort_kv_impl<double, int>(*len, keys, values);
}

// --- sort float/double keys, carrying float/double values -------------------

void stlstable_sort_float_float(int *len, float *keys, float *values)
{
    stlstable_sort_kv_impl<float, float>(*len, keys, values);
}

void stlstable_sort_double_double(int *len, double *keys, double *values)
{
    stlstable_sort_kv_impl<double, double>(*len, keys, values);
}

} // extern "C"
