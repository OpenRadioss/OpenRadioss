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


#include <iostream>
#include <fstream>
#include <list>
#include <string>
#include <tuple>
#include <algorithm>    // std::remove
using std::cout;
using std::endl;

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <zlib.h>

#ifdef _WIN64

#include <winsock2.h>
#define htobe64(x) htonll(x)

#else

#include <arpa/inet.h>

#endif


#define FASTMAGI10 0x542c   // output format : only FASTMAGI10 exists
#define SHORT2FLOAT 3000.   // used to convert a uint16_t to a float


class CheckSum_Output_Files{
private:
  // Debug flag
    // Set to 1 to enable debug mode, 0 to disable it
#ifdef DEBUG
    int debug=1;
#else
    int debug=0;
#endif
  FILE * fstream;
  gzFile gzstream;
  std::string file_mode;

  void remove_cr(std::string &line);
  void remove_trailing_blanks(std::string& str);
  inline void SWAP_MANY2BYTES(uint16_t *intPtr, size_t number);
  inline void SWAP_MANY4BYTES(int *intPtr, size_t number);
  inline void SWAP_MANY8BYTES(double *intPtr, size_t number);
  inline void SWAP_BYTESINDATA(void *itemList, size_t itemCount, size_t sizeOfItem);
  int Ufread(void *pchar, size_t sizeOfItem,
           size_t numItems,
           bool text = false);

public:
  int open_binary_file(std::string filename);
  void close_binary_file();
  std::list<std::string> Animation();
  std::list<std::string> Time_History();
  std::list<std::string> Out_File(std::fstream *new_file);
  std::list<std::tuple<std::string,std::string>> Checksum_File(std::fstream *new_file);
};

