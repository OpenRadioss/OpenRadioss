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
#include <iterator> 
#include <tuple>
#include <vector> 
#include <algorithm>

#include <dll_settings.h>



#define _FCALL

#define IS_RD_MODEL 1
#define IS_DYNA_MODEL 2



// dummy type 
// char *  : keyid
// char *  : Feature1 / Feature to verify
// char *  : Feature2 / Feature to checkout
// int     : Ntoken
// int     : Crypting key
// int     : interface ID


// starter_lic.cpp

CDECL void requested_rf_version_(int * rf_ver );
CDECL void _FCALL REQUESTED_RF_VERSION (int * rf_ver );
CDECL int create_radflex_process();
CDECL int starter_lic_checkout( char * filename,int model);

CDECL void read_dummies(int * dummy_index,int *num_stacked_dummies);
CDECL void read_dummy_encrypt();
CDECL void read_dummy_key();
CDECL void read_altdoctag(char * filename);

void strptime_impl(const char* date, struct tm *tm);
int atag_timediff(const char* time_string);

CDECL void stack_dummy(const char * key_id,const char * FT1,const char * FT2, int ntoken,int key_num, int interface_id);
void compute_md5( char* file,unsigned char* md5);
char * dataset_checksum ( char* input);


extern "C" {

CDECL int give_atag(char **atag);
CDECL int give_checksum(char **chk);
CDECL void starter_lic_release();
CDECL void starter_lic_release_();
CDECL void sort_dummy_and_stack_in_rf(int * dummy_index,int *num_stacked_dummies);
CDECL char * get_feature_verify(int num);
}

// read_dummies.cpp



