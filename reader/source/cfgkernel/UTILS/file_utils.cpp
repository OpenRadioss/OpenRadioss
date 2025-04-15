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
#include <iostream>
#include <UTILS/mv_cmath.h>
#include <string.h>
#include <array>
#include <UTILS/mv_string.h>
#include <stdio.h>
#ifndef WIN32
#include <unistd.h>
#endif
#include <sys/types.h>  // For stat().
#include <sys/stat.h>   // For stat().

#ifdef WIN32
#include <direct.h>
#define GetCurrentDir _getcwd
#else
#define GetCurrentDir getcwd
#endif


#include <UTILS/mv_cmath.h>
#include <UTILS/mv_stl_various.h> 
#include <MESSAGE/msg_manager.h> 
#include <MESSAGE/mv_messages.h> 

#include <filesystem>
#if defined(_WIN32)
#include <windows.h>
#endif



/* Definitions of W_OK, R_OK moved to file_utils.h */


#include "error.h"
#include "system_utils.h"        
#include "str_utils.h"           
#include "file_utils.h"
#include "General/general_memory.h"


static std::string getDefaultTempDirectoryPath();



int _HC_FSEEK(FILE *stream, _HC_LONG offset, int whence)
{
#if defined _WIN32 || defined WIN32 || defined _WIN64 || defined WIN64
    return _fseeki64(stream, offset, whence);
#elif LINUX
    return fseeko64(stream, offset, whence);
#endif
    return 0;
}

_HC_LONG _HC_FTELL(FILE *stream)
{
   _HC_LONG f_pos = 0 ;
#if defined _WIN32 || defined WIN32 || defined _WIN64 || defined WIN64
    return _ftelli64(stream);
#elif LINUX
    return ftello64(stream);
#endif
    return f_pos;
}
#ifdef LINUX  
static int do_mkdir(const char *path, mode_t mode);
#endif



class LocFilePath_t {
public:
    LocFilePath_t(const string &name, const string& current_string=".", const string& parent_string="..",bool is_file=true):
      my_name(name),
          my_current_string(current_string),
          my_previous_string(parent_string),
          my_nb_non_relatif_at_start(0)
      {
          my_string_vect.reserve(200);
          string loop_string=name; //RAR_HC_0013_11_29_2007
          bool do_stop=false;
          //
          string a_string_to_insert;
          string a_remaining_string=name; // 

          while(!do_stop)
          {
              string a_sep="";
              int a_size= (int)loop_string.size();
              int pos1=(int)loop_string.find("\\");
              int pos2=(int)loop_string.find("/");
              //
              if(pos1>=0 && pos1<=a_size)
              {
                  if(pos1<pos2 || pos2<0)
                  {
                      a_remaining_string=loop_string.substr(pos1+1, a_size-pos1);
                      a_string_to_insert=loop_string.substr(0, pos1);
                      a_sep="\\";
                  }
              }
              if( pos2>=0 && pos2<=a_size) 
              {
                  if(pos2<pos1 || pos1<0)
                  {
                      a_remaining_string=loop_string.substr(pos2+1, a_size-pos2);
                      a_string_to_insert=loop_string.substr(0, pos2);
                      a_sep="/";
                  }
              }
              if(a_sep=="")
                  do_stop=true;
              else
              {
                  AddStringAndSep(a_string_to_insert, a_sep);
                  loop_string=a_remaining_string;
              }
          }
          if(is_file)
              my_base_name=a_remaining_string;
                
          else if(a_remaining_string!="")
              AddStringAndSep(a_remaining_string);

      }
       /* this = C:\TMP\1\11  file_path= ../12 ---> result =C:\TMP\1\12        */ 
      void Concatenate(const string& file_path_string, string* result_string, bool do_change_base_name=true)
      {
          LocFilePath_t file_path(file_path_string);
		  string this_string = CreateString(); 
          LocFilePath_t a_new_file_path(this_string);

          int nb_strings_to_add = file_path.GetNbStrings();
      
          for(int istring=0; istring<nb_strings_to_add; ++istring)
          {
              string a_string = file_path.GetString(istring);
              istring++;
              string a_sep = file_path.GetString(istring);

              a_new_file_path.AddStringAndSep(a_string, a_sep);
          }
          string a_new_base_name=file_path.GetBaseName();
          if(do_change_base_name && a_new_base_name!="")
              a_new_file_path.my_base_name=a_new_base_name;
          *result_string =  a_new_file_path.CreateString();
      }

      /* this = C:\TMP\1\11\111   file_path= C:\TMP\2\22\222 ---> result = ..\..\..\2\22\222     */
      /* return true if OK false if a file_path is relative */
      bool Substact(const string& file_path_string, string* result_string)
      {
          LocFilePath_t file_path(file_path_string);
          if(IsRelative() ||file_path.IsRelative())
          {
              *result_string=file_path.CreateString();
              return false ;          
          }
          // Test if first strings are different
          string this_string=GetString(0);
          string file_string=file_path.GetString(0);
          if(this_string!=file_string)
          {
              LocFilePath_t a_file_path(file_path_string);
              *result_string = a_file_path.CreateString();
              return false;
          }

          vector<string>::iterator this_ite=my_string_vect.begin();
          vector<string>::iterator this_ite_end=my_string_vect.end();

          vector<string>::iterator file_ite=file_path.my_string_vect.begin();
          vector<string>::iterator file_ite_end=file_path.my_string_vect.end();

          // last_ite is the last valid iterator
          vector<string>::iterator last_this_ite=my_string_vect.begin();
          vector<string>::iterator last_file_ite=file_path.my_string_vect.begin();

          bool do_exit_loop=false;
    
          while(this_string!="" && file_string!="" && !do_exit_loop)
          {
              this_string=GetNextString(this_ite, this_ite_end, true);
              file_string=file_path.GetNextString(file_ite, file_ite_end, true);
              if( this_string == file_string)
              {
                  last_this_ite=this_ite;
                  last_file_ite=file_ite;
              }
              else
                  do_exit_loop = true;
          }

          LocFilePath_t a_new_file_path("");
          string a_result_string="";

          // from  last_this_ite to this_ite_end path is ..
          string string_to_add = this_string;
          while(string_to_add!="")
          {
              string_to_add = file_path.GetNextString(last_this_ite, this_ite_end, true);
              if(string_to_add!="")
                  a_new_file_path.AddStringAndSep("..", my_last_sep);      
          }

          // from last_file_ite to file_ite_end path is complete
          string_to_add = file_string;
          while(string_to_add!="")
          {
              string_to_add = file_path.GetNextString(last_file_ite, file_ite_end, true);
              if(string_to_add!="")
                  a_new_file_path.AddStringAndSep(string_to_add, my_last_sep);      
          }
          a_result_string+=a_new_file_path.CreateString();
          *result_string=a_result_string;
          
          if(a_new_file_path.GetNbStrings()==0)
              return true;
         
          return false;
      }
    
      string CreateString(const string& sep_string="",
          int start_index=0, int end_index=0, bool from_end=false ,const string& short_string="",
          bool do_add_base_name=true)
      {
          string res_string="";
          int nb_strings=(int)my_string_vect.size();
          // compute the start_index and end_index from parameters
          // default : from_end=false
          int a_start_index = start_index;
          int a_end_index = nb_strings;
          if(end_index>0 && end_index<a_end_index)
              a_end_index = end_index;

          if(from_end )
          {
              if(start_index>0)
              {
                  a_start_index = a_end_index-start_index;  // count from end
                  if( a_start_index<0 || a_start_index>nb_strings)
                      return res_string;
              }
              if(end_index>0)
              {
                  a_end_index = a_end_index-end_index; // count from end
                  if( a_end_index<0 || a_end_index>nb_strings)
                      return res_string;
              }
              if(short_string!="")
                  res_string+=short_string;
          }
          // generate the string
		  bool last_add_is_sep = false;
          for(int istring=a_start_index; istring<a_end_index; istring++)
          {
			  last_add_is_sep = false;
              if(my_string_vect[istring]=="\\" || my_string_vect[istring]=="/")
              {
                  if(sep_string!="")
                      res_string+=sep_string;
                  else
                      res_string+=my_string_vect[istring];
				  last_add_is_sep = true;
              }
              else
                  res_string+=my_string_vect[istring];
          }
          if(!from_end && short_string!="")
              res_string+=short_string;
          if(do_add_base_name)
          {
			  if(!last_add_is_sep)
				  res_string+=sep_string;
              res_string+=my_base_name;
          }
         
          return res_string;
      }

      bool IsRelative() const
      {  
          if(my_string_vect.size()==0)
              return true;
          string a_string= GetString(0);
           // true if string 0 is . or ..
          if(a_string==my_current_string || a_string==my_previous_string)
              return true;
          // false if string 0 is a directory
          if(my_dir_exists(a_string))
              return false;
          return true;
      }

      bool Mkdir()
      {
          if(IsRelative())
              return false;
          int nb_strings=(int)my_string_vect.size();

          // get the last existing directory

          int index_dir=-1;
          string a_last_string_dir="";
          int istring=0;
          for(istring=0; (istring<nb_strings && index_dir==-1); ++istring)
          {
              string a_string_dir = CreateString("", 0, istring+1, false, "", false);
              if(!my_file_exists(a_string_dir))
                  index_dir=istring;
              else
                  a_last_string_dir=a_string_dir;
          }
          if(index_dir==-1 || a_last_string_dir=="")
              return false;
          // go to directory 
          for(istring=index_dir;istring<nb_strings; ++istring) 
          {
              string a_new_dir_string= GetString(istring);
              if(a_new_dir_string!="\\" && a_new_dir_string!="/")
              {
#ifdef WIN32 
                  if (!mkdir (a_new_dir_string.c_str()) )
                      return false;
#else
                  if (!mkdir (a_new_dir_string.c_str(), S_IRWXU) )
                      return false;
#endif
/*
                  if(a_directory.Open(a_new_dir_qstring)!=VOS_Success)
                  {
                      return false;
                  }
*/
              }
          }

          return true;
      }

      
      void AddBaseNameSuffixe(const string& suffixe)
      {
          int index_dot = (int)my_base_name.rfind(".");
          int a_size = (int)my_base_name.size();
          if(index_dot>=0 && index_dot<=a_size)
          {
              string a_remaining_string = my_base_name.substr(index_dot+1,a_size);
              string a_new_base_name=my_base_name.substr(0,index_dot);
              a_new_base_name+=suffixe;
              a_new_base_name+=".";
              a_new_base_name+=a_remaining_string;
              my_base_name=a_new_base_name;
          }
          else
              my_base_name+=suffixe;
      }
      

      void ComputeRelative(LocFilePath_t& main, LocFilePath_t* relative_p)
      {
          /* find the last common path item*/
          int last_common_index = -2;
          int nb_main_items = main.GetNbStrings();
          int nb_file_items = GetNbStrings();
          int nb_items = (nb_main_items<nb_file_items) ? nb_main_items: nb_file_items;
          bool is_found = true;
          for(int i=0; i<nb_items && is_found ; ++i)
          {
              string main_item=main.GetString(i);
              string file_item=GetString(i);
              if(main_item==file_item)
                  last_common_index = i;
              else
                  is_found=false;
          }
          /*  if item_index <0 no common path so the path stays */
          if(last_common_index<0)
              return;
          /* start by eventual ".."*/
          for(int i=last_common_index; i<nb_main_items; ++i)
          {
              string file_item=main.GetString(i);
              if(file_item!=my_last_sep)
                   relative_p->AddStringAndSep("..",my_last_sep);
          }
          if(relative_p->GetNbStrings()==0)
              relative_p->AddStringAndSep(my_current_string, my_last_sep);

          /* then add the remaining main items*/
         
          for(int i=last_common_index; i<nb_file_items; ++i)
          {
              string main_item=GetString(i);
              if(main_item!=my_last_sep)
                  relative_p->AddStringAndSep(main_item, my_last_sep);
          }
      }


      inline int GetNbStrings() const {return  (int)my_string_vect.size();}

      inline string GetString(int i) const {return my_string_vect[i];}

      inline string GetBaseName() const {return  my_base_name;}
      inline void SetBaseName(const string &string_data) {my_base_name= string_data;}

      inline void RemoveLastString()  {my_string_vect.pop_back(); }

      inline void AddString (const string& data_string) {my_string_vect.push_back(data_string);}

      inline const string& GetLastSeparator()const  {return my_last_sep;} 
	  inline void  SetLastSeparator(const string &str_sep) {my_last_sep =str_sep;} 

private:
    inline string GetNextString(vector<string>::iterator& ite, vector<string>::const_iterator ite_end, bool no_separator=true) const
    {
        ite++;
        if(ite==ite_end)
            return("");

         string a_string = *ite;
         if(!no_separator)
           return a_string;
       
        if(a_string == "\\" || a_string=="/")
            return GetNextString(ite, ite_end, no_separator);
        return a_string;
    }

    inline void AddStringAndSep(const string& a_string, const string& a_sep="")
    {
        bool add_string_and_sep = true;
        if(a_string==my_current_string)
        {
            if(my_string_vect.size()>0)
                add_string_and_sep=false;
        }
        else if(a_string==my_previous_string)
        {
            if(my_nb_non_relatif_at_start>0)
            {
                my_string_vect.pop_back();
                my_string_vect.pop_back();
                add_string_and_sep=false;
                my_nb_non_relatif_at_start--;
            }
        }
        if(add_string_and_sep)
        {

	/* MODIF_RA_NOT_SUBMITTED (BEG)*/
	if(my_string_vect.size()>0 || a_string!="")
		my_string_vect.push_back(a_string);
	/* MODIF_RA_NOT_SUBMITTED (END)*/            
            if(a_sep!="")
            {
                my_last_sep = a_sep;
                my_string_vect.push_back(a_sep);
            }
            if(a_string!=my_current_string && a_string!=my_previous_string)
                my_nb_non_relatif_at_start++;
        }         
    }



  

private:
    vector<string>  my_string_vect;
    string         my_name;
    string         my_current_string;
    string         my_previous_string;
    string         my_base_name;
    int            my_nb_non_relatif_at_start;
    string         my_last_sep;
};



extern "C" FILE *myfopen (const char *name,const char *type) {
  FILE    *fich ;
  fich = fopen ( name, type ) ;
  return fich;
}

extern "C" FILE *myfopendeck (const char *name,const char *type) {
    FILE    *fich = NULL;

    if (!name)
        return NULL;

    //static data member stored the file path and file pointer map
    static map<string, FILE *>    fp_file;

    //close all the stored files pointers and clear space
    if(name == NULL && type == NULL)
    {
        map<string, FILE *>::iterator iter;
        for(iter = fp_file.begin(); iter != fp_file.end(); ++iter)
            fclose((*iter).second);

        fp_file.clear();
        return NULL;
    }
    //search the
    map<string, FILE *>::iterator iter = fp_file.find(name);


    if(iter != fp_file.end())
    {
        fich = (*iter).second;
        rewind(fich); 
    }
    else
    {
        fich = myfopen ( name, type ) ;
        if(fich)
        {
            fp_file[name] = fich;
            setvbuf ( fich , NULL , _IOFBF , 1048576 );
        }
    }
/*  if(fich && IsFromRemoteServer(name))
    {
        setvbuf ( fich , NULL , _IOFBF , 1048576 );
    }
*/
    return fich;
}

extern "C" FILE *myfopenwrite (const char *name,const char *type) {

  FILE *fp = myfopen(name, type);

  if(fp)
      setvbuf (fp, NULL , _IOFBF , 1048576 * 2 /*1024*1024 */);

  return fp;
}


extern "C" void myfclose (FILE *fich_p) {
  if(fich_p!=NULL) fclose(fich_p) ;
}



extern "C" void utility_move_files(const char *old_name,const char *new_name) {
  my_file_rename(old_name,new_name);
}



extern "C" char *utility_get_next_bak_name(const char *name) {
   int length1 = 0, length2 = 0 ;
   char *bak_name = NULL ;
   const char *format[4] = {"%1d","%2d", "%3d","%4d"} ;
   int index = 0, found = 0 ;

   if (name == NULL) return NULL ;
   if (strcmp(name,"") == 0) return NULL ;
   length1 = (int)(strlen(name)) ;
   bak_name = (char *)calloc(length1+8+1, sizeof(char)) ;

   if (bak_name)
   {
       strcpy(bak_name, name);
       strcpy(bak_name + length1, ".bak000");
       length2 = length1 + 7;

       while ((!found) && (index <= 9999))
       {
           int size = 0;
           if (index > 0)
           {
               size = (int)log10((float)index);
           }
           sprintf(bak_name + length2 - size, format[size], index);
           found = access(bak_name, R_OK);
           index++;
       }
   }

  if (index > 9999) 
  {
     own_free(bak_name);
     return NULL ;
  }
  return bak_name ;
}

extern "C" char *utility_get_relative_path_from_absolute_path(const char *main_path,const char *other_path)
{
   int i = 0 ,i_save = 0 ;
   int main_length = 0, other_length = 0, short_length = 0 ;
   char *relative_path = NULL;
   const char *short_main = NULL, *short_other = NULL ;
   int  nbr_level = 0 ;
#ifdef WIN32
   char separator = '\\' ;
   char *go_up_string = "..\\" ;
#else
   char separator = '/' ;
   char *go_up_string = const_cast<char *>("../") ;
#endif

   if (main_path == NULL) return NULL ;
   if (other_path == NULL) return NULL ;
   main_length = (int)(strlen(main_path)) ;
   other_length = (int)(strlen(other_path)) ;

   for (i=0 ; (i<main_length) && (i<other_length) ; i++)
   {
       if (main_path[i] == other_path[i]) 
       {
          continue ;
       }
       else
       {
          break ;
       }
   }

   i_save = i;
   for (i=i_save-1 ; i > -1 ; i--)
   {
       if (main_path[i] == separator) break;
   }
   if(i_save!=i)
   {
       i++;
   }

   short_main = main_path + i ;
   short_other  = other_path  + i ;
   short_length = (int)strlen(short_main) ;
   for (i=0 ; i<short_length ; i++)
   {
       if (short_main[i] == separator) nbr_level++ ;
   }
   relative_path = (char *)calloc((nbr_level)*3+strlen(short_other)+1, sizeof(char)) ;
   if (relative_path)
   {
       for (i = 0; i < nbr_level; i++)
       {
           strcpy(relative_path + 3 * i, go_up_string);
       }
       strcpy(relative_path + 3 * (nbr_level), short_other);
   }

   return relative_path ;
}

/*
Split full path name into path root extension
*/
void my_split_file_full_name(const string &file_fullname,
			     string *dir_fullname_p,string *file_shortname_p,string *extension_p)
{
  int a_length=(int)(file_fullname.size());
  int a_end=a_length-1;
   
  /*	while(a_end>=0 && file_fullname[a_end]!='/') --a_end; */
  /* if(file_fullname[a_end]!='/') ++a_end; */
  while(a_end>=0 &&!(my_is_path_separator(file_fullname[a_end]))) --a_end;
  if(	  !(my_is_path_separator(file_fullname[a_end]))) ++a_end;
  // Dir
  *dir_fullname_p=file_fullname.substr(0,a_end);
  // Name
  /*if(file_fullname[a_end]=='/') ++a_end; */
  if(my_is_path_separator(file_fullname[a_end])) ++a_end;
   
  *file_shortname_p=file_fullname.substr(a_end,a_length-a_end);
  // Extension
  
  //unsigned int a_pos=file_shortname_p->rfind(".");
  size_t a_pos=file_shortname_p->rfind(".");
  if(a_pos!=file_shortname_p->npos) {
    ++a_pos;
    *extension_p=file_shortname_p->substr(a_pos,file_shortname_p->size()-a_pos);
    *file_shortname_p=file_shortname_p->substr(0,--a_pos);
  } else {
    *extension_p="";
  }
}


string my_get_file_abs_path(const string &file_fullname, bool is_file) {
     LocFilePath_t a_file_path(file_fullname,".","..", is_file);
     string a_path = a_file_path.CreateString("",0,0,false,"",false);
     return a_path;
}



string my_get_file_basename(const string &file_fullname) {
     LocFilePath_t a_file_path(file_fullname);
     string a_base_name = a_file_path.GetBaseName();
     return a_base_name;
}



/*
return true if file_name is relative
*/
bool my_is_file_path_relative(const string& file_path_name)
{
    LocFilePath_t a_file_path(file_path_name);

    bool is_relative = a_file_path.IsRelative();
    return is_relative;
}



/* return true if file_name is relative to file_name2 
   if yes full_file_name is the full (path+base name) name of file_name , relative_name does not change
   if no  relative_name is fil_name, full_file_name does not change

   ex file_name = ../toto.incl file_tame = E:\TMP/TOTO\tutu.txt
   --> TRUE full_file_name=E:\TMP/TOTO\../toto.incl = E:\TMP/toto.incl */
/*bool my_is_file_path_relative(const string& file_name, const string& file_name2,  string* full_file_name, string* relative_name) 
{
      LocFilePath_t a_file_path(file_name);
      if(!a_file_path.IsRelative())
      {
          *full_file_name = a_file_path.CreateString();            
          return false;
      }
      if(file_name2=="")
      {
          *relative_name = file_name;
          return true;
      }
      LocFilePath_t a_file_path2(file_name2);
      a_file_path2.Concatenate(file_name, full_file_name);
      *relative_name = file_name;
      return true;
}*/



/* return the resulting file_path from the concatenation of relative_path and file_path
   EX: relative = ../../TMP/toto.txt file= ../TUTU -> return = ../../../TUTU/TMP/toto.txt
*/
string my_file_path_modify_with_relative(const string& file_path,  const string& relative_path, bool is_file)
{
    LocFilePath_t a_file_path_relative(relative_path);
    string res_string="";
    a_file_path_relative.Concatenate(file_path, &res_string, is_file);
    return res_string;
}



/* return the resuting file path_name from the concatenation of a file path name(relative) and an absolute path
EX file = ..\TUTU absolute = C:\TMP\TOTO return C:\TMP\TUTU */
string my_file_path_modify_with_absolute(const string& file_path, const string& absolute_path, bool is_file, bool is_unix)
{
	LocFilePath_t a_file_path_absolute(absolute_path);
	string a_file_path_res_string="";
	a_file_path_absolute.Concatenate(file_path, &a_file_path_res_string, is_file);
	LocFilePath_t a_file_path_res(a_file_path_res_string);
	if(is_unix)
		a_file_path_res_string=a_file_path_res.CreateString("/",0,0,false,"",is_file);
	else
		a_file_path_res_string=a_file_path_res.CreateString("\\",0,0,false,"",is_file);

	return a_file_path_res_string;
}





/* return the resulting file_full_name  from the concatenation of a file full name and a relative_path 
EX: file= C:\TMP\TUTU\toto.txt relative = ../TATA/tutu.txt  return = C:\TMP\TATA\toto.txt
*/
string my_file_full_name_modify_with_relative(const string& file_path,  const string& relative_path)
{
    LocFilePath_t a_full_file_path(file_path);
    string res_string="";
    a_full_file_path.Concatenate(relative_path, &res_string, true);
    return res_string;
}


  
/* return the resuting file full_nam from the concatenation of a file full name(relative) and an absolute path
EX file = ..\tutu.txt absolute = C:\TMP\TOTO return C:\TMP\tutu.txt*/
string my_file_full_name_modify_with_absolute(const string& file_path,  const string& absolute_path, bool is_unix)
{
	return my_file_path_modify_with_absolute(file_path, absolute_path, true, is_unix);
}
  

/* return the path ....*/
string my_file_get_relative_path_from_main(const string& file, const string& main)
{
    LocFilePath_t a_main_file_path(main);
    LocFilePath_t a_file_path(file);
    LocFilePath_t a_relative_path("");
    a_file_path.ComputeRelative(a_main_file_path, &a_relative_path);
    return a_relative_path.CreateString();
}

/* file_path1 and file_path2 must not be relative!!!
 * return true if files are the "the  same" , false else
   if yes relative_path is ""
   else relative_path is the path to go from file_path1 to file_path2
*/
bool my_is_file_absolute_path_compare(const string& file_path1, const string& file_path2, string* relative_path)
{
     LocFilePath_t a_file_path1(file_path1);
     bool res_bool = a_file_path1.Substact(file_path2,relative_path);
     return res_bool;
}



string my_file_full_name_append(const string& file_path_name, const string& base_name)
{
     LocFilePath_t a_file_path(file_path_name);
     a_file_path.SetBaseName(base_name);
     string res_string = a_file_path.CreateString();
     return res_string;
}



string my_file_path_add_suffixe(const string& file_path,  const string& suffixe)
{
     LocFilePath_t a_file_path(file_path);
     a_file_path.AddBaseNameSuffixe(suffixe);
      string res_string = a_file_path.CreateString();
     return res_string;
    
}




bool my_file_copy(const string &src_fullname,const string &dest_fullname) {
  FILE *a_src_pf=myfopen(src_fullname.c_str(),"rb");
  if(a_src_pf==NULL) return true;
  FILE *a_dest_pf=myfopen(dest_fullname.c_str(),"wb");
  if(a_dest_pf==NULL) { myfclose(a_src_pf); return true; }
  //
  char buffer[1000];
  while(!feof(a_src_pf)) {
    size_t a_nb_read=fread(buffer,1,1000,a_src_pf);
    fwrite(buffer,1,a_nb_read,a_dest_pf);
  }
  //
  myfclose(a_src_pf);
  myfclose(a_dest_pf);
  return false;
}
void my_file_rename(const string &file_fullname,const string &new_file_fullname) {
  rename( file_fullname.c_str(), new_file_fullname.c_str() );

}

void my_file_remove(const string &file_fullname) {
    remove(file_fullname.c_str());
}

bool my_file_exists(const string &file_fullname) {

    FILE *file = fopen(file_fullname.c_str(), "r");
    if (NULL != file)
    {
        fclose(file);
        return true;
    }
    return false;
}


void my_chmod_file(const string &file_fullname,const string &mode) {
#ifndef WIN32
  string a_command=str_printf("chmod %s %s",mode.c_str(),file_fullname.c_str());
  system(a_command.c_str());
#endif //WIN32 
}

   
string my_get_dir_modify_end_path(const string &dir_fullname ,bool is_added_sep)
{
    LocFilePath_t a_file_path(dir_fullname,".","..",false);
    if(is_added_sep)
    {
         
          int nb_strings=a_file_path.GetNbStrings();
          if(nb_strings>0)
          {
              string last_string = a_file_path.GetString(nb_strings-1);
              if(last_string!="/" && last_string!="\\")
              {
                  string last_sep = a_file_path.GetLastSeparator();
				   
				  if (last_sep=="")
					  last_sep="/";
				   
				  a_file_path.AddString(last_sep);
              }
          }
           
    }
    else
    {
        int nb_strings=a_file_path.GetNbStrings();
        if(nb_strings>0)
        {
            string last_string = a_file_path.GetString(nb_strings-1);
            if(last_string=="/" || last_string=="\\")
                a_file_path.RemoveLastString();
        }
    }

    string a_string = a_file_path.CreateString();
    return a_string;
}
   




string my_get_dir_path(const string &file_fullname) {
  int a_length=(int)(file_fullname.size());
  int a_end=a_length-1;
    
 // while(a_end>=0 && file_fullname[a_end]!='/') --a_end;
  while(a_end>=0 && !my_is_path_separator(file_fullname[a_end]) ) --a_end;
 //if(a_end<0 || file_fullname[a_end]!='/') ++a_end;
  if(a_end<0 || !my_is_path_separator(file_fullname[a_end])) ++a_end;
    
  // Dir
  return file_fullname.substr(0,a_end);
}

bool my_is_dir_writable(const string &dir_fullname) {
  string a_file_name=dir_fullname;
  if(!a_file_name.empty()) a_file_name+="/";
  a_file_name+="helioss.tmp";
  FILE *a_file_pf     = myfopen(a_file_name.c_str(),"wt");
  bool  a_is_writable = (a_file_pf!=NULL);
  if(a_is_writable) {
    myfclose(a_file_pf);
    my_file_remove(a_file_name);
  }
  return a_is_writable;
}

bool my_dir_exists(string &dir_fullname) {
    struct stat info;
    size_t a_sz = dir_fullname.size();
    if (a_sz > 0 && (dir_fullname[a_sz - 1] != '\\' || dir_fullname[a_sz - 1] != '/'))
        dir_fullname += "/";


    if (stat(dir_fullname.c_str(), &info) != 0)
        return false;
    else if (info.st_mode & S_IFDIR)  // S_ISDIR() doesn't exist on my windows 
        return true;

    return false;
}

bool my_dir_set_current_path(const string &dir_fullname)
{
    return false;
}


string my_dir_get_current_path()
{
    return "";
}


bool my_dir_restore_current_path(const string &path)
{
    return false;
}


int my_get_file_access(const string& pathname, const string &filename) {
	string fullname = (pathname=="") ? filename: my_file_full_name_append(pathname, filename);
	int acces_res=-1;
#ifdef WIN32
	if(_access(fullname.c_str(), F_OK) == 0) 
		acces_res=F_OK;
	if(_access(fullname.c_str(), R_OK) == 0) 
		acces_res=R_OK;
	if(_access(fullname.c_str(), W_OK) == 0) 
		acces_res=W_OK;
#else
	if(access(fullname.c_str(), F_OK) == 0) 
		acces_res=F_OK;
	if(access(fullname.c_str(), R_OK) == 0) 
		acces_res=R_OK;
	if(access(fullname.c_str(), W_OK) == 0) 
		acces_res=W_OK;
#endif /*WIN32*/
	return acces_res;
}



bool my_create_file_path(const string &path_name, const string& file_name, bool is_file)
{
    LocFilePath_t a_path(path_name, ".", "..",is_file);
    bool bool_res = a_path.Mkdir();
   // QDir::mkdir(QString(path_name.c_str()));
    return bool_res;
}



string my_get_current_dir() {
  static string a_cur_dir = mygetenv(CUR_DIR_VAR);
  static bool   a_first   = true;
  if(a_first) {
    a_first=false;
    char cCurrentPath[1024];

    if (!GetCurrentDir(cCurrentPath, sizeof(cCurrentPath)))
    {
        return "";
    }
    a_cur_dir = string(cCurrentPath);
    if(!my_is_dir_writable(a_cur_dir)) {
      string a_tmp_dir=my_get_tmp_dir();
      MvMsgManager_t::DisplayMess(CFG_WARNING,MV_get_msg_array(MSGT_UTILS)[1],a_cur_dir.c_str(),a_tmp_dir.c_str());
      a_cur_dir=a_tmp_dir;
    }
  }
  return a_cur_dir;
}



string my_get_home_dir() {
    char cCurrentPath[1024];

    if (!GetCurrentDir(cCurrentPath, sizeof(cCurrentPath)))
    {
        return "";
    }
    string a_cur_dir(cCurrentPath);
    return a_cur_dir;
}






string my_get_tmp_dir() {
  static string a_tmp_dir=mygetenv(TMP_DIR_VAR);
  if(a_tmp_dir=="") {
    a_tmp_dir = getDefaultTempDirectoryPath();
    //a_tmp_dir=DEFAULT_TMP_DIR;
    //MvMsgManager_t::DisplayMess(CFG_WARNING,MV_get_msg_array(MSGT_UTILS)[0],TMP_DIR_VAR,DEFAULT_TMP_DIR);
  }
  if(!my_is_dir_writable(a_tmp_dir)) {
    throw MvError_t(MV_get_msg_array(MSGT_UTILS)[2],a_tmp_dir.c_str(),TMP_DIR_VAR);
  }
  return a_tmp_dir;
}



std::string getDefaultTempDirectoryPath() {
#if defined(_WIN32)
    char temppath[MAX_PATH];
    if (GetTempPath(MAX_PATH, temppath)) {
        return temppath;
    }
    //if GetTempPath fails
    const char* tempenv = std::getenv("TEMP");
    return tempenv ? tempenv : "C:\\Windows\\Temp";
#else
    static const std::array<const char*, 4> envvars = { "TMPDIR", "TMP", "TEMP", "TEMPDIR" };
    for (const char* var : envvars) {
        const char* tempdir = std::getenv(var);
        if (tempdir) return tempdir;
    }
    return "/tmp"; // default for platforms without env vars
#endif
}










/*RAR_HC_0001_05_31_2007  (BEG)*/
/*RAR_HC_0001_05_31_2007 (END)*/

 
bool my_is_path_separator(char separ)
{
	bool is_sep =false;
#ifndef WIN32
	 if(separ=='/') is_sep=true;
#else
	if(separ=='/' || separ=='\\')	is_sep=true;
#endif
	return is_sep;
}
#ifdef LINUX  
static int do_mkdir(const char *path, mode_t mode)
{
    typedef struct stat Stat;
    Stat            st;
    int             status = 0;

    if (stat(path, &st) != 0)
    {
        /* Directory does not exist. EEXIST for race condition */
        if (mkdir(path, mode) != 0 && errno != EEXIST)
            status = -1;
    }
    else if (!S_ISDIR(st.st_mode))
    {
        errno = ENOTDIR;
        status = -1;
    }

    return(status);
}
#endif
 



