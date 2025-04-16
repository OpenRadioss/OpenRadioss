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
#include <filesystem>
#include <algorithm>
#include <stdexcept>
#include <map>
#include <checksum.h>
#include <vector>

// C calls fortran 
#define _FCALL

#ifdef _WIN64
#define write_out_file WRITE_OUT_FILE
#define grab_checksums _FCALL GRAB_CHECKSUMS
#else
#include <dirent.h>
#define write_out_file write_out_file_
#define grab_checksums grab_checksums_
#endif

extern "C" {
     void write_out_file(int * fd,const char * line,int * len_line);
     void grab_checksums(int *fd,char *input,int *leni,char *path,int *lenp);
}


class List_checksum {

    private:

    // Debug flag
    // Set to 1 to enable debug mode, 0 to disable it
#ifdef DEBUG
    int debug=1;
#else
    int debug=0;
  #endif

    // List of files to be processed sorted o, file_list
    std::list<std::string> deck_file_list;                                          // deck files
    std::list<std::string> out_file_list;                                           // .out files
    std::list<std::string> th_file_list;                                            // .thy files
    std::list<std::string> anim_file_list;                                          // rootnameAxxx
    std::list<std::string> checksum_file_list;                                      // .checksum files
    std::map<std::string,std::string> file_checksum_list;                           // File checksums : Filename, checksum
    std::list<std::tuple<std::string,std::list<std::string>>> checksum_list ;       // extracted checksum list from the output files : Filename, checksum list
    
    // -----------------------------------------------------------------------------------
    // Tool : get directory path from a file path
    // -----------------------------------------------------------------------------------
      bool is_integer(const std::string s);
      void sort_in_lists(std::string file,std::string rootname);
      std::string format_as_4_digits(int number);
      std::string format_as_3_digits(int number);
      void remove_cr(std::string &line);
      std::string separator();
      int compare_lists(std::list<std::string> list1, std::list<std::string> list2);
      bool is_file_valid(std::string file);
      void file_list(std::string directory,std::string rootname);
      void parse_output_files(std::string directory, std::string rootname);
      void parse_animation_files(std::string directory, std::string rootname);
      void parse_th_files(std::string directory, std::string rootname);
      void parse_checksum_files(std::string directory, std::string rootname);

    public:
      std::list<std::tuple<std::string,std::list<std::string>>> chk_list(std::string input,std::string directory);
      std::string get_path(const std::string& filepath) ;
      List_checksum();
  }; 

  