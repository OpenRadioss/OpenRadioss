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

#define POLYNOMIAL 0xEDB88320  // standard polynomial for CRC32

void init_crc32_table(uint32_t *crc32_table) {
    for (uint32_t i = 0; i < 256; i++) {
        uint32_t crc = i;
        for (uint32_t j = 0; j < 8; j++) {
            crc = (crc >> 1) ^ (POLYNOMIAL * (crc & 1));
        }
        crc32_table[i] = crc;
    }
} 
void simple_checksum(const double* vector, const int* length, double* hash) {
    uint32_t crc32_table[256];
    uint32_t crc = 0xFFFFFFFF; // initial value

        init_crc32_table(crc32_table);

        for (size_t i = 0; i < *length; i++) {
        uint64_t value = *(uint64_t*)&vector[i]; // Conversion of double to uint64_t
        
        for (size_t j = 0; j < sizeof(value); j++) {
            uint8_t byte = value & 0xFF; // byte extraction
            crc = (crc >> 8) ^ crc32_table[(crc ^ byte) & 0xFF];
            value >>= 8; // shift for the next byte
        }
     }
     *hash = (double) (crc ^ 0xFFFFFFFF);
  }
} 
