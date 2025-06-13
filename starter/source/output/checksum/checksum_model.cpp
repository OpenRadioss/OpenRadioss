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
#include <checksum_model.h>

using namespace std;

  // -----------------------------------------------------------------------------------
  // Function to remove carriage return characters from a string
  // -----------------------------------------------------------------------------------
  void MD5Checksum::remove_carriage_return(std::string& line) {
     line.erase(std::remove(line.begin(), line.end(), '\r'), line.end());
  }

  // -----------------------------------------------------------------------------------
  // Tool : Return the separator for the file path according to the OS
  // output:
  // separator : "/" for Unix, "\" for Windows
  // -----------------------------------------------------------------------------------
  string MD5Checksum::separator(){
    // -----------------------------------------------------------------------------------
      #ifdef _WIN64
        return "\\"; // Windows separator
      #else
        return "/";  // Unix separator
      #endif
    }
  // -----------------------------------------------------------------------------------
  // Tool : get directory path from a file path
  // -----------------------------------------------------------------------------------
    std::string MD5Checksum::get_path(const std::string& filepath) {
      // Find the last occurrence of the path separator
#ifdef _WIN64
      size_t pos = filepath.find_last_of("/\\");
      if (pos == std::string::npos) {
         pos = filepath.find_last_of("/");
      }
#else
      size_t pos = filepath.find_last_of("/");
#endif
      if (pos != std::string::npos) {
          // Extract the substring up to the last separator
          return filepath.substr(0, pos);
      }
      // If no separator is found, return an empty string
      return "";
  }

  // ------------------------------------------------------------------------------------------------------------------------
  void MD5Checksum::new_checksum( string title, list<tuple<int,string, md5_state_t, string>> *md5_states_tmp){
  // ------------------------------------------------------------------------------------------------------------------------  
  // CHECKSUM/START was met, create a new checksum state
  // input:
  // title : title of the checksum
  // input / output:
  // md5_states_tmp : list of checksums computed in the file
  // ------------------------------------------------------------------------------------------------------------------------
    md5_state_t new_md5;
    md5_init(&new_md5);
    string md5_digest(16,'0');
    md5_states_tmp->push_front(make_tuple(1,title,new_md5,md5_digest));
  };

  // ------------------------------------------------------------------------------------------------------------------------
  // Add line to all active MD5 digests
  // input:
  // line : string to be added to the checksum
  // input / output:
  // md5_states_tmp : list of checksums computed in the file
  // ------------------------------------------------------------------------------------------------------------------------
  void MD5Checksum::process_checksum(string line, list<tuple<int,string, md5_state_t, string>> *md5_states_tmp){
  // ------------------------------------------------------------------------------------------------------------------------
      for (auto& item : *md5_states_tmp ) {
         if (get<0>(item) == 1){
            md5_state_t state = get<2>(item);
            md5_append( &state, (const md5_byte_t *) line.c_str(), line.length() );
            get<2>(item) = state;
         }
        }
  };

  // --------------------------------------------------------------------------------------------------------------------
  // /CHECKSUM/STOP : Finish the MD5 checksum for the first active state
  // --------------------------------------------------------------------------------------------------------------------
  // input / output 
  // md5_states_tmp : list of checksums computed in the file
  // --------------------------------------------------------------------------------------------------------------------
  void MD5Checksum::end_checksum(list<tuple<int,string, md5_state_t, string>> *md5_states_tmp){
  // --------------------------------------------------------------------------------------------------------------------
      int state=-1;
      for (auto& item : *md5_states_tmp ) {
            state=get<0>(item);
             if (state == 1) {
                get<0>(item) = 0;   // Set Active flag to 0

                // Finish the MD5 checksum
                md5_state_t state = get<2>(item);
                unsigned char md5[16];
                md5_finish (&state, md5);  

                // Add MD5 in hexadecimal format in tuplet
                ostringstream formatted_line;
                for (int i = 0; i < 16; ++i) {
                    formatted_line << hex << setw(2) << setfill('0') << static_cast<int>(md5[i]);
                }
                get<3>(item)=formatted_line.str();
                break;
             }
      }
  };

  // --------------------------------------------------------------------------------------------------------------------
  // Finalize checksum : After deck file processing : finalize all active checksums (/CHECKSUM/STOP missing) 
  // --------------------------------------------------------------------------------------------------------------------
  // input / output 
  // md5_states_tmp : list of checksums computed in the file
  // --------------------------------------------------------------------------------------------------------------------
  void MD5Checksum::finalize_checksum(list<tuple<int,string, md5_state_t, string>> *md5_states_tmp){
    // --------------------------------------------------------------------------------------------------------------------
        int state=-1;
        for (auto& item : *md5_states_tmp ) {
              state=get<0>(item);
               if (state == 1) {
                  get<0>(item) = 0;   // Set Active flag to 0
  
                  // Finish the MD5 checksum
                  md5_state_t state = get<2>(item);
                  unsigned char md5[16];
                  md5_finish (&state, md5);  
  
                  // Add MD5 in hexadecimal format in tuplet
                  ostringstream formatted_line;
                  for (int i = 0; i < 16; ++i) {
                      formatted_line << hex << setw(2) << setfill('0') << static_cast<int>(md5[i]);
                  }
                  get<3>(item)=formatted_line.str();
               }
        }
    };
  // -----------------------------------------------------------------------------------------------------------------------------
  // main checksum computation function
  // read the input deck line by line , commpute checksums between CHECKSUM/START and CHECKSUM/END
  // ------------------------------------------------------------------------------------------------------------------------------
  // input:
  // filename,string : name of the file to be read
  // level,int    : recursion level (used to limit the number of include files)
  // output:
  // md5_states_tmp : list of checksums computed in the file
  // ------------------------------------------------------------------------------------------------------------------------------
  int  MD5Checksum::file_read(string filename,string deck_directory,int level,list<tuple<int,string, md5_state_t, string>> *md5_states_tmp){
  // -----------------------------------------------------------------------------------------------------------------------------
       string chksum_start=( "/CHECKSUM/START");
       string chksum_end=(   "/CHECKSUM/END");
       string chksum_include=( "#include ");
       fstream new_file;
       new_file.open(filename, ios::in);
       
       // Stop  after 15 levels of recursion
       if (level > 15) return 0;

       if ( !new_file.is_open() ) {
          return -1;
       }

       string line;
       while (getline(new_file, line)) {

          remove_carriage_return(line); // Remove carriage return characters
          // Search for /CHECKSUM/START keyword
         if (line == chksum_start) {
           string title;
           getline(new_file, title);
           remove_carriage_return(title); // Remove carriage return characters
           new_checksum(title,md5_states_tmp);
           continue;
         }
         // Search for /CHECKSUM/END keyword
         if (line == chksum_end) {
           end_checksum(md5_states_tmp);
           continue;
         }

         string comp=line.substr(0,chksum_include.length());
         if (comp == chksum_include) {
           // Process include files
           string include_file = line.substr(chksum_include.length());
           if (deck_directory.length() > 0){
              include_file = deck_directory+separator()+include_file; // Get the path of the file
           }
           if (debug){
              cout << "Include file: " << include_file << endl;
           }
        //   include_file.erase( remove(include_file.rbegin(), include_file.end(), ' '), include_file.end());
           // debug cout << "Include file: " << include_file << endl;
           file_read(include_file,deck_directory, level + 1,md5_states_tmp);
           continue;
         }

         if (line[0] == '#') {
           // Skip comment lines
           continue;
         }
         process_checksum(line,md5_states_tmp);
        
       }
       new_file.close();
       return 0;
      }

   // --------------------------------------------------------------------------------------------------------   
   // constructor
   // --------------------------------------------------------------------------------------------------------
   MD5Checksum::MD5Checksum()
   // --------------------------------------------------------------------------------------------------------
   {};

   void MD5Checksum::parse(string filenam)  {
      list<tuple<int,string, md5_state_t, string>>  md5_states_tmp;
      string deck_directory = get_path(filenam); // Get the directory of the file
      file_read(filenam,deck_directory,0,&md5_states_tmp);
      finalize_checksum(&md5_states_tmp); // Finalize all active checksums
      // intvert the list to have it in deck order
      for (const auto& item : md5_states_tmp){
          md5_states.push_front(item); // Add the checksums to the main list
      }

      md5_states_tmp.clear(); // Clear the temporary list
   };

   // --------------------------------------------------------------------------------------------------------
   // CPP / Fortran interface : Get the number of checksums
   // -------------------------------------------------------------------------------------------------------- 
   int MD5Checksum::count()  {
   // --------------------------------------------------------------------------------------------------------
     return md5_states.size();
   }

   // --------------------------------------------------------------------------------------------------------
   // CPP / Fortran interface : Get the N-th checksum
   // --------------------------------------------------------------------------------------------------------
   // input:
   // N : checksum number to be retrieved
   // output:
   // checksum_title : title of the checksum
   // len_title : length of the checksum title
   // checksum : checksum value
   // len_checksum : length of the checksum value
   // --------------------------------------------------------------------------------------------------------
   void MD5Checksum::member(int N,char* checksum_title,int *len_title,char* checksum,int  *len_checksum)  {
   // --------------------------------------------------------------------------------------------------------
    int count= md5_states.size();
    if (N > count) {
      cout << "Error: N=" << N << " is greater than the number of checksums " << count << endl;
      checksum[0]='\0';
      checksum_title[0]='\0';
      *len_checksum=0;
      *len_title=0;
    }else{
      auto it = md5_states.begin();
      advance(it, N-1); // Move iterator to the N-th element (0-based index)

      // Copy checksum_title
      int size_title = get<1>(*it).size();
      get<1>(*it).copy(checksum_title ,size_title);
      checksum_title[size_title]='\0';
      *len_title=size_title;

      // Copy checksum
      int size_checksum = get<3>(*it).size();
      get<3>(*it).copy(checksum ,size_checksum);
      checksum[size_checksum]='\0';
      *len_checksum=size_checksum;

      // cout << "Member " << *N << " Checksum : " << get<1>(*it) << " " <<  get<3>(*it) << endl;
    }
  }

   // --------------------------------------------------------------------------------------------------------
   // Get the list of all computed checksums 
   // --------------------------------------------------------------------------------------------------------
   // output:
   // computed checksums in hexadecimal format
   // --------------------------------------------------------------------------------------------------------
   list<string> MD5Checksum::get_checksums(){
   // --------------------------------------------------------------------------------------------------------
      list<string> checksums;
      for (const auto& item : md5_states){
        if (get<0>(item) == 0){
            string chksum_item=get<1>(item)+"_"+get<3>(item);
            checksums.push_back(chksum_item);
        }
      }
      return checksums;
   }

   // ------------------------
   // print the checksum list
   // ------------------------
   void MD5Checksum::print(){
   // ------------------------
      cout << "Checksum list " << endl;
      cout << "==============" << endl;
      for (const auto& item : md5_states){
        if (get<0>(item) == 0){
            cout << "Checksum : " << get<1>(item) << " " <<  get<3>(item) << endl;
        }
      }
   }

// End of class MD5Checksum

// ------------------------------------------------------------------------------------------------------------------------
// C/Fortran interface to the C++ class MD5Checksum
// The interface is used to create the checksum from the input deck and to read the checksums
// To be called from Starter.

extern "C" {
  MD5Checksum * deck_checksum_creation(int len_filename,char filename[]) {
    int i;

    MD5Checksum * md_compute = new MD5Checksum();
    char * c_filename = (char*) malloc(len_filename+1);
    for (i=0; i<len_filename; i++){
      c_filename[i]=filename[i];
    }
    c_filename[len_filename]='\0';
    string cpp_filename(c_filename);
    
    md_compute->parse(cpp_filename);
    return md_compute;
  }


  int  deck_checksum_count(MD5Checksum * md_compute) {
    return md_compute->count();
  }

  void  deck_checksum_read(MD5Checksum * md_compute, int  count,char* checksum_title,int *len_title,char* checksum,int *len_checksum) {
    md_compute->member(count,checksum_title,len_title,checksum,len_checksum);
  }

}
