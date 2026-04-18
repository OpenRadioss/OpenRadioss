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
#include <iomanip>
#include <fstream>
#include <string>
#include <list>
#include <stdio.h>
#include <stdlib.h>
#include <sstream>
#include <string.h>
#include <tuple>
#include <md5.h>
#include <filesystem>
#include <algorithm>
#define _FCALL 


class MD5Checksum {
    std::list<std::tuple<int,std::string, md5_state_t, std::string>>  md5_states;     // List of options : active flag,id, title, checksum digest
    
    private:

    #ifdef DEBUG
      int debug=1;
    #else
      int debug=0;
    #endif
      void remove_carriage_return(std::string& line) ;
      std::string separator();
      std::string get_path(const std::string& filepath);
      void new_checksum( std::string title, std::list<std::tuple<int,std::string, md5_state_t, std::string>> *md5_states_tmp);
      void process_checksum(std::string line, std::list<std::tuple<int,std::string, md5_state_t, std::string>> *md5_states_tmp);
      void end_checksum(std::list<std::tuple<int,std::string, md5_state_t, std::string>> *md5_states_tmp);
      void finalize_checksum(std::list<std::tuple<int,std::string, md5_state_t, std::string>> *md5_states_tmp);
      int  file_read(std::string filename,std::string deck_directory,int level,std::list<std::tuple<int,std::string, md5_state_t, std::string>> *md5_states_tmp);
    
    public:
       // --------------------------------------------------------------------------------------------------------   
       // constructor
       // --------------------------------------------------------------------------------------------------------
       MD5Checksum();
       void parse(std::string filenam);
       int  count();
       void member(int N,char* checksum_title,int *len_title,char* checksum,int *len_checksum);
       std::list<std::string> get_checksums();
       void print();
    };
    