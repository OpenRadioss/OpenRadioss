//Copyright>        OpenRadioss
//Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
//Copyright>        Commercial Alternative: Altair Radioss Software
//Copyright>
//Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
//Copyright>        software under a commercial license.  Contact Altair to discuss further if the
//Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
// ----------------------------------------------------------------------------------------------------------------------
 
#include <cstdint>
#include <cstring>  // For memcpy
#include <cmath>    // For std::isnan
#include <limits>   // For std::numeric_limits
 
extern "C" {

 
void simple_checksum(const double* vector, const int* length, int32_t* hash) {
    uint32_t hash_value = 0;
    const int prime     = 31;

        for (size_t i = 0; i < *length; i++) {
        hash_value = hash_value * prime + (uint32_t)(vector[i] * (i+1000000));
    }
    *hash = hash_value & 0xFFFFFFFF;
  }
} 
