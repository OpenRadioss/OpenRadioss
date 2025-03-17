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
#include "win32_utils.h"  

#include "mv_cstring.h"
#include "set_utils.h"       
#include "error.h"
#include "direction_utils.h"


typedef set<string> DirSet_t;


dir_type_e MV_get_direction(const string &dir,bool is_extended) { 
  const char *a_char_p=strpbrk(dir.c_str(),"XYZ");
  if(a_char_p==NULL) return DIR_UNKNOWN;
  //
  const char *a_dummy_p=dir.c_str();
  while(a_dummy_p!=a_char_p) if(*(a_dummy_p++)!=' ') return DIR_UNKNOWN;
  //
  string a_dir=a_char_p;
  if(a_dir=="X")  return DIR_X;
  if(a_dir=="Y")  return DIR_Y;
  if(a_dir=="Z")  return DIR_Z;
  if(is_extended) {
    if(a_dir=="XX") return DIR_XX;
    if(a_dir=="YY") return DIR_YY;
    if(a_dir=="ZZ") return DIR_ZZ;
  }
  return DIR_UNKNOWN;
}

string MV_get_direction(dir_type_e dir,bool is_extended) { 
  switch(dir) {
  case DIR_X:  return "X";                     //break;
  case DIR_Y:  return "Y";                     //break;
  case DIR_Z:  return "Z";                     //break;
  case DIR_XX: return is_extended ? "XX" : ""; //break;
  case DIR_YY: return is_extended ? "YY" : ""; //break;
  case DIR_ZZ: return is_extended ? "ZZ" : ""; //break;
  default:     return "";                      //break;
  }
}

bool MV_get_direction(const string &dir,bool *xdir_p,bool *ydir_p,bool *zdir_p) { 
  *xdir_p=false;
  *ydir_p=false;
  *zdir_p=false;
  //
  const char *a_char_p=strpbrk(dir.c_str(),"XYZ");
  if(a_char_p==NULL) a_char_p=dir.c_str()+dir.size();
  //
  const char *a_dummy_p=dir.c_str();
  while(a_dummy_p!=a_char_p) if(*(a_dummy_p++)!=' ') return false;
  //
  while(*a_char_p) switch(*(a_char_p++)) {
  case 'X': if(*xdir_p) return false; else *xdir_p=true; break;
  case 'Y': if(*ydir_p) return false; else *ydir_p=true; break;
  case 'Z': if(*zdir_p) return false; else *zdir_p=true; break;
  default:  return false; //break;
  }
  //
  return true;
}

string MV_get_direction(bool xdir,bool ydir,bool zdir) { 
  string a_dir="";
  if(xdir) a_dir+="X";
  if(ydir) a_dir+="Y";
  if(zdir) a_dir+="Z";
  if(a_dir=="XZ") a_dir="ZX"; 
  return a_dir;
}



dir_type_e MV_get_direction(const char *dir,int is_extended) {
  string a_dir         = dir;
  bool   a_is_extended = (is_extended!=0);
  return MV_get_direction(a_dir,a_is_extended);
}



const char *MV_get_direction_str(dir_type_e dir,int is_extended) {
  static DirSet_t a_dir_set;
  //
  string a_dir=MV_get_direction(dir,is_extended!=0);
  if(!(a_dir<a_dir_set)) a_dir_set+=a_dir;
  //
  return (*(a_dir_set.find(a_dir))).c_str();
}



int MV_get_directions(const char *dir,int *xdir_p,int *ydir_p,int*zdir_p) {
  string a_dir=dir;
  bool   a_xdir,a_ydir,a_zdir;
  bool   a_result=MV_get_direction(a_dir,&a_xdir,&a_ydir,&a_zdir);
  *xdir_p=(int)a_xdir;
  *ydir_p=(int)a_ydir;
  *zdir_p=(int)a_zdir;
  return (int)a_result;
}



const char *MV_get_directions_str(int xdir,int ydir,int zdir) {
  static string   a_dir;
  static DirSet_t a_dir_set;
  //
  a_dir=MV_get_direction(xdir!=0,ydir!=0,zdir!=0);
  if(!(a_dir<a_dir_set)) a_dir_set+=a_dir;
  //
  return (*(a_dir_set.find(a_dir))).c_str();
}





