/*Copyright>    OpenRadioss
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
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.*/
#ifndef FILE_UTILS_H
#define FILE_UTILS_H

#include <UTILS/mv_cstdio.h>


#define CUR_DIR_VAR     "MV_CUR_DIR"
#define TMP_DIR_VAR     "MV_TMP_DIR"
#define USR_DIR_VAR     "MV_USR_DIR"
#define DEFAULT_TMP_DIR "/tmp"



/* following lines wee moved from file_utils.cp */

#ifdef WIN32 
  #include <io.h>
  #define R_OK    4
  #define W_OK    2
  #define F_OK    0
#else
  #include <unistd.h>
#endif



#ifndef _HC_LONG_
#define _HC_LONG_
#if defined _WIN32 || defined WIN32 || defined _WIN64 || defined WIN64
    typedef __int64 _HC_LONG;
#elif LINUX
    typedef __off64_t _HC_LONG;
#endif
#endif

#include <HCDI/hcdi.h>
#ifdef __cplusplus
extern "C" {
#endif /*__cplusplus*/

HC_DATA_DLL_API FILE *myfopen (const char *name,const char *type);
FILE *myfopendeck (const char *name,const char *type);
HC_DATA_DLL_API FILE *myfopenwrite (const char *name,const char *type);
HC_DATA_DLL_API void  myfclose (FILE *fich);


int _HC_FSEEK(FILE *stream, _HC_LONG offset, int whence);
_HC_LONG _HC_FTELL(FILE *stream);



HC_DATA_DLL_API void utility_move_files(const char *old_name,const char *new_name);
char *utility_get_next_bak_name(const char *name);
char *utility_get_relative_path_from_absolute_path(const char *main_path,const char *other_path);


#ifdef __cplusplus
}
#endif /*__cplusplus*/


#ifdef __cplusplus


#include <UTILS/mv_string.h>
#include <HCDI/hcdi.h>



void   my_split_file_full_name(const string &file_fullname,
			       string *dir_fullname_p,string *file_shortname_p,string *extension_p);
string my_get_file_abs_path(const string &file_fullname, bool is_file); 
HC_DATA_DLL_API string my_get_file_basename(const string &file_fullname); 
bool   my_file_copy(const string &src_fullname,const string &dest_fullname);
HC_DATA_DLL_API void   my_file_rename(const string &file_fullname,const string &new_file_fullname);
void   my_file_remove(const string &file_fullname);
HC_DATA_DLL_API bool   my_file_exists(const string &file_fullname);
void   my_chmod_file(const string &file_fullname,const string &mode); 

//bool my_is_file_path_relative(const string& file_path_name, const string& current_path, string *full_name, string* relative_name); 
HC_DATA_DLL_API bool   my_is_file_path_relative(const string& file_path_name);
string my_file_path_modify_with_relative(const string& file_path,  const string& relative_path, bool is_file);    
string my_file_path_modify_with_absolute(const string& file_path, const string& absolute_path, bool is_file, bool is_unix); 
HC_DATA_DLL_API string my_file_full_name_modify_with_relative(const string& file_path,  const string& relative_path);   
string my_file_full_name_modify_with_absolute(const string& file_path,  const string& absolute_path, bool is_unix);   
string my_file_get_relative_path_from_main(const string& file, const string& main);
bool   my_is_file_absolute_path_compare(const string& file_path_1, const string& file_path2, string* relative_path);  
HC_DATA_DLL_API string my_file_full_name_append(const string& file_path_name, const string& base_name);      
string my_file_path_add_suffixe(const string& file_path,  const string& suffixe); 


HC_DATA_DLL_API string my_get_dir_modify_end_path(const string &dir_fullname ,bool is_added_sep);    
HC_DATA_DLL_API string my_get_dir_path(const string &file_fullname);
bool   my_is_dir_writable(const string &dir_fullname);
bool   my_dir_exists(string &dir_fullname);

HC_DATA_DLL_API bool my_dir_set_current_path(const string &dir_fullname);

HC_DATA_DLL_API int my_get_file_access(const string& pathname, const string &fullname); 
bool my_create_file_path(const string &path_name, const string& file_name, bool is_file);  



string my_get_current_dir();
string my_get_home_dir();
string my_get_root_dir();
HC_DATA_DLL_API string my_get_tmp_dir();

HC_DATA_DLL_API string my_dir_get_current_path();

HC_DATA_DLL_API bool my_dir_restore_current_path(const string &path);


bool my_create_dir(const string &directory_fullname);

/*RAR_HC_0001_05_31_2007  (BEG)*/
bool my_is_path_separator(char separ);  

#endif /*__cplusplus*/

#endif /* FILE_UTILS_H */




