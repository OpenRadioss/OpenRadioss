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
#include <checksum_output_files.h>
#include <checksum_list.h>
using namespace std;


bool List_checksum::is_integer(const std::string s) {
   char * p;
   char * s_cchar = (char*)s.c_str();
    // strtol will convert the string to a long integer
    // If the conversion is successful, p will point to the first character after the number
    // If the conversion fails, p will point to the original string
   long converted = strtol(s_cchar, &p, 10);
   if (*p) {
       return false; // Not a valid integer
   }
   return true;
    

}
  // -----------------------------------------------------------------------------------
  // Tool : Format the number as a 4-digit string with leading zeros for .out files
  // input:
  // number : integer to be formatted
  // output:
  // formatted string
  // -----------------------------------------------------------------------------------
  string List_checksum::format_as_4_digits(int number) {
  // -----------------------------------------------------------------------------------
      ostringstream oss;
      oss << setw(4) << setfill('0') << number;
      return oss.str();
  }

  // -----------------------------------------------------------------------------------
  // Tool : Format the number as a 3-digit string with leading zeros for Animation & Time History files
  // input:
  // number : integer to be formatted
  // output:
  // formatted string
  // -----------------------------------------------------------------------------------
  string List_checksum::format_as_3_digits(int number) {
    // -----------------------------------------------------------------------------------
        ostringstream oss;
        oss << setw(3) << setfill('0') << number;
        return oss.str();
    }
  
  // -----------------------------------------------------------------------------------
  // Dos2Unix : function to remove (cr) characters from a string
  // -----------------------------------------------------------------------------------
  void List_checksum::remove_cr(std::string& line) {
    line.erase(std::remove(line.begin(), line.end(), '\r'), line.end());
 }


  
  // -----------------------------------------------------------------------------------
  // Tool : Return the separator for the file path according to the OS
  // output:
  // separator : "/" for Unix, "\" for Windows
  // -----------------------------------------------------------------------------------
  string List_checksum::separator(){
  // -----------------------------------------------------------------------------------
    #ifdef _WIN64
      return "\\"; // Windows separator
    #else
      return "/";  // Unix separator
    #endif
  }

  // -----------------------------------------------------------------------------------
  // Compare two lists of checksums
  // input:
  // list1 : first list of checksums
  // list2 : second list of checksums
  // output:
  // 1 if the lists are equal, 0 if they are not equal
  // -----------------------------------------------------------------------------------
  int List_checksum::compare_lists(list<string> list1, list<string> list2){
  // -----------------------------------------------------------------------------------  
      if (list1.size() != list2.size()) {
          return 0; // Lists are not equal in size
      }
      auto it1 = list1.begin();
      auto it2 = list2.begin();
      while (it1 != list1.end() && it2 != list2.end()) {
          if (*it1 != *it2) {
              return 0; // Lists are not equal
          }
          ++it1;
          ++it2;
      }
      return 1; // Lists are equal
  }

  // -----------------------------------------------------------------------------------
  // sorst in lists : get a filename & sort it in the corresponding list if pattern found
  // input:
  // fname : filename
  // rootname : deck rootname
  // output:
  // void, out_file_list, th_file_list, anim_file_list are updated
  // -----------------------------------------------------------------------------------
  void List_checksum::sort_in_lists(std::string fname,std::string rootname){

      // Grab the file extension
      size_t pos = string(fname).find_last_of('.');
      std::string extension = fname.substr(pos + 1);

      // Input deck
      string input_deck=rootname+"_0000.rad";
      if (fname == input_deck){
          deck_file_list.push_back(fname);
      }

      if (extension == "out") {
             out_file_list.push_back(fname);
      }
      if (extension == "thy") {
             th_file_list.push_back(fname);
      }

      if (extension == "checksum") {
          int filename_length = rootname.length()+5+extension.length()+1;
          if (fname.length() == filename_length){
             std::string str_runnumber=fname.substr(rootname.length()+1,4); 
             if (is_integer(str_runnumber)){
                   checksum_file_list.push_back(fname);
             }
          }
      }

      // Old styled files Axxx, Txxx
      string rd_run;
      string file_A;
      if (extension == "gz"){
            rd_run = fname.substr(fname.length()-6,3);
            file_A=fname.substr(0,fname.length()-6);
      }else{
            rd_run = fname.substr(fname.length()-3);
            file_A=fname.substr(0,fname.length()-3);        
      }
      string anim_pattern=rootname+ "A";
      if ( is_integer(rd_run)  && anim_pattern == file_A){
          anim_file_list.push_back(fname);
      }
      string th_pattern=rootname+ "T";
      string file_T=fname.substr(0,fname.length()-2);
      rd_run = fname.substr(fname.length()-2);
      if ( is_integer(rd_run)  && th_pattern == file_T){
          th_file_list.push_back(fname);
      }

  }

  // -----------------------------------------------------------------------------------
  // file_list : read all files in the directory and sort them in the corresponding list
  // input:
  // directory : directory where the files are located
  // rootname  : rootname of the files (without run number and extension)
  // output:
  // out_file_list, th_file_list, anim_file_list are updated
  // -----------------------------------------------------------------------------------
  void List_checksum::file_list(std::string directory,std::string rootname){

    std::vector<std::string> files;

#ifdef _WIN64
    std::string search_path = directory + rootname + "*";
    WIN32_FIND_DATAA fd;
    HANDLE hFind = ::FindFirstFileA(search_path.c_str(), &fd);
    if (hFind != INVALID_HANDLE_VALUE) {
        do {
            if (!(fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) {
                string fname(fd.cFileName);
                sort_in_lists(fname,rootname);
            }
        } while (::FindNextFileA(hFind, &fd));
        ::FindClose(hFind);
    }
#else
    string directory_cp = directory;
    if ( directory_cp.length()==0){
	     directory_cp="."; 
    }
    DIR* dir = opendir(directory_cp.c_str());
    if (dir) {
        struct dirent* entry;
        while ((entry = readdir(dir)) != nullptr) {
            std::string fname(entry->d_name);
            if (fname.find(rootname) == 0) { // starts with rootname
                sort_in_lists(fname,rootname);
            }
        }
        closedir(dir);
    }
#endif

};


  // -----------------------------------------------------------------------------------
  // is_file_valid : check if the file has valid fingerprint
  // -----------------------------------------------------------------------------------

  bool List_checksum::is_file_valid(std::string file){
      return true;
  }


  // -------------------------------------------------------------------------------------------------------------------------------------------------
  // Parse all .out files in the directory, grab the deck checsksums fingerprints
  // Do an MD5 on the file
  // input:
  // directory : directory where the .out files are located
  // rootname  : rootname of the .out files (without run number and extension)
  // output:
  // list of tuples (filename, checksum match)
  // The checksum match is 1 if the checksums are equal, 0 if they are not equal
  // -------------------------------------------------------------------------------------------------------------------------------------------------
  void List_checksum::parse_output_files(string directory, string rootname){
  // -------------------------------------------------------------------------------------------------------------------------------------------------
  
    for (const auto& item : out_file_list){ 

      string outfile;
      if ( directory.length() > 0 ){
          outfile = directory + item;
      }else{
          outfile = item;
      }

      fstream new_file;
      new_file.open(outfile, ios::in);

      if ( !new_file.is_open() ) {
          // cout << "Error: Unable to open file " << outfile << endl;
      }else{
             if (debug){
                cout << "Parsing file: " << outfile << endl;
             }
             // Grab file checksums
             CheckSum_Output_Files out;
             list<string> checksum_list_out=out.Out_File( &new_file );
             new_file.close();

             // Compute file checksum
             checksum file_cs;
             string file_checksum = file_cs.compute_checksum(outfile);
             string formated_out = outfile + " : " + file_checksum;

             checksum_list.push_back(make_tuple(formated_out,checksum_list_out)); // Add the checksum list to the collection
             file_checksum_list.insert(pair<string,string>(outfile, file_checksum)); // Add the file,checksum tuple in specific list
      }
    }
  }

  // -------------------------------------------------------------------------------------------------------------------------------------------------
  // Parse all Animation files from the directory, grab the deck checsksums fingerprints
  // Do an MD5 on the file
  // input:
  // directory : directory where the .out files are located
  // rootname  : rootname of the .out files (without run number and extension)
  // output:
  // list of tuples (filename, checksum match)
  // The checksum match is 1 if the checksums are equal, 0 if they are not equal
  // -------------------------------------------------------------------------------------------------------------------------------------------------
  void List_checksum::parse_animation_files(string directory, string rootname){ 
  // -------------------------------------------------------------------------------------------------------------------------------------------------
    int run_number=1;
    int found_out_file=1;

    for (const auto& item : anim_file_list){ 

      string anim_file;
      if ( directory.length() > 0 ){
          anim_file = directory + item;
      }else{
          anim_file = item;
      }

      CheckSum_Output_Files out;
      FILE *new_file;
      int success=0;
      success=out.open_binary_file(anim_file);

      if ( success == 0 ) {
          // cout << "Error: Unable to open file " << anim_file << endl;
          found_out_file=0; // No more .out files to process
      }else{
             if (debug){
                cout << "Parsing file: " << anim_file << endl;
             }


             list<string> checksum_list_out=out.Animation( );
             out.close_binary_file();
             // Compute file checksum
             checksum file_cs;
             string file_checksum = file_cs.compute_checksum(anim_file);
             string formated_out = anim_file + " : " + file_checksum;
             
             checksum_list.push_back(make_tuple(formated_out,checksum_list_out)); // Add the checksum list to the collection
             file_checksum_list.insert(pair<string,string>(anim_file, file_checksum)); // Add the file,checksum tuple in specific list

      
            }
            run_number++;
    }
  }

  // -------------------------------------------------------------------------------------------------------------------------------------------------
  // Parse all time history files from the directory, grab the deck checsksums fingerprints
  // Do an MD5 on the file
  // input:
  // directory : directory where the .out files are located
  // rootname  : rootname of the .out files (without run number and extension)
  // output:
  // list of tuples (filename, checksum match)
  // The checksum match is 1 if the checksums are equal, 0 if they are not equal
  // -------------------------------------------------------------------------------------------------------------------------------------------------
  void List_checksum::parse_th_files(string directory, string rootname){ 
  // -------------------------------------------------------------------------------------------------------------------------------------------------
      int found_out_file=1;
  
    for (const auto& item : th_file_list){ 

      string th_file;
      if ( directory.length() > 0 ){
          th_file = directory + item;
      }else{
          th_file = item;
      }

      CheckSum_Output_Files out;
      int success = out.open_binary_file(th_file);
  
        if ( success == 0 ) {
            // cout << "Error: Unable to open file " << anim_file << endl;
            found_out_file=0; // No more .out files to process
        }else{
                 if (debug){
                    cout << "Parsing file: " << th_file << endl;
                 }
                 list<string> checksum_list_th=out.Time_History();
                 out.close_binary_file();

                 checksum file_cs;
                 string file_checksum = file_cs.compute_checksum(th_file);
                 string formated_out = th_file + " : " + file_checksum;

                 checksum_list.push_back(make_tuple(formated_out,checksum_list_th)); // Add the checksum list to the collection
                 file_checksum_list.insert(pair<string,string>(th_file, file_checksum)); // Add the file,checksum tuple in specific list
              }
      }
    }

  // -------------------------------------------------------------------------------------------------------------------------------------------------
  // Parse all .checksum files from the directory
  // Verifies the .checksum filefingerprint
  // 
  // input:
  // directory : directory where the .out files are located
  // rootname  : rootname of the .out files (without run number and extension)
  // output:
  // formated list of tupes
  // -------------------------------------------------------------------------------------------------------------------------------------------------
  void List_checksum::parse_checksum_files(string directory, string rootname){

    for (const auto& item : checksum_file_list){ 

      string chkfile;
      if ( directory.length() > 0 ){
          chkfile = directory + item;
      }else{
          chkfile = item;
      }
             
      bool valid_file = is_file_valid(chkfile);
      string valid_message;
      if (valid_file) {
          valid_message = "Valid Fingerprint";
      }else{
          valid_message = "Invalid Fingerprint";
      }

      fstream new_file;
      new_file.open(chkfile, ios::in);

      if ( !new_file.is_open() ) {
          // cout << "Error: Unable to open file " << outfile << endl;
      }else{
             if (debug){
                cout << "Parsing file: " << chkfile << endl;
             }
             // Grab file checksums
             CheckSum_Output_Files out;
             std::list<std::tuple<std::string,std::string>> checksum_list_chk;
             checksum_list_chk=out.Checksum_File( &new_file );
             new_file.close();

             string formated_out = chkfile + " : " ;
             list<string> verify_checksum_list;

             // print the checksum list
             for (const auto& item2 :checksum_list_chk){
                string filename = get<0>(item2);
                string checksum = get<1>(item2);

                string computed_checksum= file_checksum_list[filename];

                if (computed_checksum.length() > 0){
                   if (checksum == computed_checksum){
                      verify_checksum_list.push_back(filename + "_" + "Valid Checksum" );
                   }else{
                      verify_checksum_list.push_back(filename + "_" + "Failed checksum check : File: "+ checksum + "   Computed: " + computed_checksum);
                   }
                }
             }
             checksum_list.push_back(make_tuple(formated_out,verify_checksum_list)); // Add the checksum list to the collection

      }
    }
  };


  List_checksum::List_checksum()    // Constructor
  {};

  
// Class to list the compute starter input file and parse output files for checksums
// -----------------------------------------------------------------------------------

std::string List_checksum::get_path(const std::string& filepath) {
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

  // -------------------------------------------------------------------------------
  // Compare the checksum from deck to output file
  // Computes input deck checksum & parse all output files to compare the results.
  // -------------------------------------------------------------------------------
  // input:
  // filename : string starter input filename
  // directory : string directory where the input file is located
  // output:
  // list of checksums found in the .out file : format (filename, checksum list)
  // -------------------------------------------------------------------------------
  list<tuple<string,list<string>>> List_checksum::chk_list(string rootname,string directory){
  // -------------------------------------------------------------------------------

    // Debug prints
    if (debug){
      cout <<  endl;
      cout << "Directory: " << directory << endl;  
      cout << "Rootname: " << rootname << endl;
      cout << endl;
    }

    file_list(directory,rootname); // List all files in the directory
  

    for (const auto& deck_file : deck_file_list){ 

         // If deck is present:
        // Compute checksum from input deck
        MD5Checksum my_checksums;
        my_checksums.parse(deck_file);
        list<string> deck_checksum_list=my_checksums.get_checksums();    // Compute checksum from input deck

        // Add Starter computed checksum to the list
        checksum_list.push_back(make_tuple(deck_file,deck_checksum_list)); // Add the checksum list to the collection

        if (debug){    
          cout << "Commputed Checksum list from deck: " << endl;
          cout << "===================================" << endl; 
          for (const auto& item : deck_checksum_list){
            cout << item << endl;
          }
          cout << "==============================" << endl;
          cout << endl;
        }
    }

    
    // Parse all .out files in the directory
    parse_output_files(directory, rootname);

    // parse all animation files in the directory
    parse_animation_files(directory, rootname);

    // parse all animation files in the directory
    parse_th_files(directory, rootname);

    // Parse all checksum files in the directory
    parse_checksum_files(directory, rootname); 

    // print the checksum list from all output files
    if (debug){
      cout << "Checksum list from output files: " << endl;
      cout << "==============================" << endl; 
      for (const auto& item : checksum_list){
        cout << "File: " << get<0>(item) << endl;
        for (const auto& checksum : get<1>(item)){
          cout << "       "<< checksum << endl;
        }
        cout << "==============================" << endl; 
      }
    }

    return checksum_list ;
  }

// End of class Verify_checksum
// ------------------------------------------------------------------------------------------------------------------------



// C/Fortran interface for Starter
extern "C" {
  void grab_checksums(int *fd,char *rootname,int *lenr,char *path,int *lenp){
    int i;

    // Convert input fortran to C Character

    char rootname_c[257];             // maximum length of the input string is 256 in Starter
    for (i=0;i<*lenr;i++){
      rootname_c[i]=rootname[i];
    }
    rootname_c[*lenr]='\0';           // Add null character to the end of the string
    string str_rootname(rootname_c);  // Convert to string


    char path_c[2029];                // maximum length of the path string is 2048 in Starter
    for (i=0;i<*lenp;i++){
      path_c[i]=path[i];
    }
    path_c[*lenp]='\0';               // Add null character to the end of the string
    string str_path(path_c);          // Convert to string
    

    // Create checksum_list object
    // grab the input deck name
    list<tuple<string,list<string>>> checksum_list;            // checksum list collection from all decks : filename, checksum list
    List_checksum chksum_tool;
    checksum_list=chksum_tool.chk_list(str_rootname,str_path); // Compute the checksums from the input deck and parse the output files


    // Print the checksum list to the output file
    const char* blank=" ";
    int len_blank= strlen(blank);

    for (const auto& item : checksum_list){
      string filename="    File. . . .   "+get<0>(item);
      const char* line=filename.c_str();
      int len_line= strlen(line);
      write_out_file(fd,line,&len_line);
      int len_blanck=1;
      write_out_file(fd," ",&len_blank);
   
      for (const auto& checksum : get<1>(item)){
        size_t pos = checksum.find_last_of("_");
        string title=checksum.substr(0,pos); // Remove the checksum value
        string digest=checksum.substr(pos+1);  // Keep only the checksum value
        string checksum_line="                  "+title+" : "+digest;
        const char* line=checksum_line.c_str();
        len_line= strlen(line);
        write_out_file(fd,line,&len_line);   // Fortran routine to write the checksum line
      }
      write_out_file(fd,blank,&len_blank);
      write_out_file(fd,blank,&len_blank);
    }
  
  }
  
}


// ------------------------------------------------------------------------------------------------------------------------
// Main Standalone function to test the checksum tool
// The function will read the input file and compute the checksums
// The function will also parse the .out files and compare the checksums with the ones computed in the input deck
// The function will print the results to the console
// To build in Standalone mode, use the following command:
// On Windows:
// icx -DMAIN         -o ..\exec\checksum.exe -Ishare/includes -ID:\WS\GitHub\OpenRadioss_VS\OpenRadioss\extlib\md5\include source\output\checksum\checksum_list.cpp source\output\checksum\checksum_model.cpp source\output\checksum\checksum_output_files.cpp D:\WS\GitHub\OpenRadioss_VS\OpenRadioss\extlib\md5\win64\md5.lib ws2_32.lib
// g++ -DMAIN -no-pie -o ../exec/checksum -Ishare/includes -I/mnt/d/WS/GitHub/OpenRadioss_VS/OpenRadioss/extlib/md5/include source/output/checksum/checksum_list.cpp source/output/checksum/checksum_model.cpp source/output/checksum/checksum_output_files.cpp /mnt/d/WS/GitHub/OpenRadioss_VS/OpenRadioss/extlib/md5/linux64/libmd5.a   -std=c++14
// Add -DDEBUG for additional debug information
// ------------------------------------------------------------------------------------------------------------------------
#ifdef MAIN

void write_out_file(int * fd,const char * line,int * len_line){
  // Dummy routine to permit link out of Starter
}

int main(int argc, char *argv[])
{
  List_checksum verify_chksum_tool;
  cout << endl;
  cout << "Filename to process: "<< argv[1] << endl;
  string file=string(argv[1]);

  string path=verify_chksum_tool.get_path(file); // Get the directory of the file
  list<tuple<string,list<string>>>  list = verify_chksum_tool.chk_list(file,path);
  
  cout << endl;
  cout << "Checksum list from output files: " << endl;
  cout << "==============================" << endl << endl; 

  for (const auto& item : list){
    cout << "File: " << get<0>(item) << endl;
    for (const auto& checksum : get<1>(item)){
      string title=checksum.substr(0,checksum.length()-33); // Remove the checksum value
      string digest=checksum.substr(checksum.length()-32); // Keep only the checksum value
      cout << "    "<< title << " : " <<  digest << endl;
    }
    cout <<  endl; 
  }
}
#endif
