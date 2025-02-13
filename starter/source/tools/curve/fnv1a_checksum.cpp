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

    void fnv1a_checksum(const double* vector, const int* length, int32_t* hash);
}
 
uint64_t fnv1a_hash_bytes(const void* data, size_t len) {

    const uint8_t* bytes = static_cast<const uint8_t*>(data);
    uint64_t hash = 14695981039346656037ULL;  // 64-bit FNV-1a offset basis
    const uint64_t fnv_prime = 1099511628211ULL;  // 64-bit FNV-1a prime
 
    for (size_t i = 0; i < len; ++i) {
        hash ^= bytes[i];
        hash *= fnv_prime;
    }
    return hash;
}
 
void fnv1a_checksum(const double* vector, const int* length, int32_t* hash) {
    uint64_t hash_value = 14695981039346656037ULL;
 
    for (int i = 0; i < *length; i++) {

        uint64_t bits;

        std::memcpy(&bits, &vector[i], sizeof(bits));
 
        // Improve entropy by mixing upper and lower bits

        bits = (bits ^ (bits >> 32)) * 0x9E3779B97F4A7C15ULL;  // A good mixing constant
        hash_value ^= fnv1a_hash_bytes(&bits, sizeof(bits));
    }
 
    // Reduce 64-bit hash to 32-bit while preserving entropy

    *hash = static_cast<int32_t>((hash_value >> 32) ^ (hash_value & 0xFFFFFFFF));
}
