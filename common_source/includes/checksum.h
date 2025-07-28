//Copyright>    OpenRadioss
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
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.
#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <list>
#include <stdio.h>
#include <stdlib.h>
#include <sstream>
#include <string.h>
#include <tuple>
#include <filesystem>
#include <algorithm>

#include <md5.h>

#define _FCALL 
#define BUFFERSIZE 4096

class checksum {
private:
    std::list <std::tuple< std::string, std::string>> checksum_list;   // List of checksums : Filename, checksum
public:
    std::string compute_checksum(std::string file);
    std::list <std::tuple< std::string, std::string>> dump_list();
};


#ifdef _WIN64
#define write_out_file WRITE_OUT_FILE
#else
#define write_out_file write_out_file_
#endif

extern "C" {
    checksum* new_file_checksum_list();
    void compute_binary_checksum(checksum* cs_output_files,char *file, int len , int izip);
    void print_checksum_list( checksum* cs_output_files,int fd);
    void write_out_file(int * fd,const char * line,int * len_line);
}
