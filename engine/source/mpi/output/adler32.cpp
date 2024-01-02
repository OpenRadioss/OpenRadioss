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
#include <cstddef>
#include <cstdint>
#include <limits>
#include <cmath>
#include <iostream>
constexpr uint32_t ROOT = 65521;

extern "C" void cpp_adler32(const unsigned char* data, int len, int * result) {
    uint32_t a = 1, b = 0;
    for (size_t i = 0; i < len; ++i) {
        a = (a + data[i]) % ROOT;
        b = (b + a) % ROOT;
    }
    // cast the result into an int (32  bits)
    uint32_t r = (b << 16) | a;
    *result =  *reinterpret_cast<int32_t*>(&r);
}


// test cpp_adler32

void test_cpp_adler32()
{
    double * data = new double[100];
    double * data2 = new double[100];

    for (int i = 0; i < 100; ++i) {
        data[i] = i;
        data2[i] = i;
    }

    // find the next representable value of data2[50]
    data2[50] = std::nextafter(data2[50], std::numeric_limits<double>::max());

    int result1, result2;
    cpp_adler32(reinterpret_cast<unsigned char*>(data), 100 * sizeof(double), &result1);
    cpp_adler32(reinterpret_cast<unsigned char*>(data2), 100 * sizeof(double), &result2);
    //print the two values
    std::cout << "result1: " << result1 << std::endl;
    std::cout << "result2: " << result2 << std::endl;

}