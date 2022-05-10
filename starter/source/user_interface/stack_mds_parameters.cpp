//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2022 Altair Engineering Inc.
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
#include <iterator> 
#include <tuple> 
#include <vector> 

#include <stdio.h>
#include <string.h>

#define _FCALL

#ifdef _WIN64
#define add_mds_material_      ADD_MDS_MATERIAL
#define print_mds_             PRINT_MDS
#define unstack_mds_material_  UNSTACK_MDS_MATERIAL
#define get_mds_materials_     GET_MDS_MATERIALS
#endif

using namespace std; 


/* Tuple surface_memb;
   int    : MDSid
   char * : file1
   char * : file2
   char * : file3
   char * : label
   int    : ndepvar 

*/

#define MDSTUPLE tuple < int, char * ,char * ,char * ,char **, int >
vector < MDSTUPLE > mds_stack;

/* ----------------------------------------------------------------------------------------- */
extern "C"
{
/* ----------------------------------------------------------------------------------------- */

/* ---------------------------------------------------------------
   Headers
   --------------------------------------------------------------- */
void _FCALL add_mds_material_( int * mds_id,
                               char * file1, int * len_file1,
                               char * file2, int * len_file2,
                               char * file3, int * len_file3,
                               char * label, int * len_label,
                               int  * ndepvar);
void _FCALL print_mds_();

void _FCALL  unstack_mds_material_(int *mds_num,
                           int * mds_id,
                           char * file1, int * len_file1,
                           char * file2, int * len_file2,
                           char * file3, int * len_file3,
                           char * label, int * len_label,
                           int  * ndepvar);

void _FCALL get_mds_materials_( int * mds_nmat);



/* -----------------------------------------------------------------------------------------
    add_mds_material_ : stack MDS informations 
   -----------------------------------------------------------------------------------------
    input  : int * mds_id    - MDS Material number
    input  : char * file1    - File1
    input  : int * len_file1 - length of file 1
    input  : char * file2    - File2
    input  : int * len_file2 - length of file 2
    input  : char * file3    - File2
    input  : int * len_file3 - length of file 3
    input  : char * label    - Label
    input  : int * len_label - length of label
    input  : int  * ndepvar  - number of stzte variables
   ----------------------------------------------------------------------------------------- */
void _FCALL add_mds_material_( int * mds_id,
                       char * file1, int * len_file1,
                       char * file2, int * len_file2,
                       char * file3, int * len_file3,
                       char * label, int * len_label,
                       int  * ndepvar){

  char * cfile1;
  char * cfile2;
  char * cfile3;
  char ** clabel;
  
  int mdsid = *mds_id;
  int ndepv = *ndepvar;

  cfile1 = new char [ *len_file1 + 1 ];
  cfile2 = new char [ *len_file2 + 1 ];
  cfile3 = new char [ *len_file3 + 1 ];

  clabel = new char * [*ndepvar];

  strncpy(cfile1,file1, *len_file1);
  cfile1[*len_file1]='\0';

  strncpy(cfile2,file2, *len_file2);
  cfile2[*len_file2]='\0';

  strncpy(cfile3,file3, *len_file3);
  cfile3[*len_file3]='\0';

  for (int j=0;j< *ndepvar; j++){
    int siz = len_label[j];
    char * my_label= &label[64*j];    // Fortran style array
    
    clabel[j] = new char [ siz + 1 ];
    strncpy(clabel[j],my_label, siz);
    clabel[j][siz]='\0';
  }

  MDSTUPLE mds_entry = make_tuple(mdsid, &cfile1[0], &cfile2[0],&cfile3[0],&clabel[0], ndepv);
  mds_stack.push_back( mds_entry) ;


}

/* -----------------------------------------------------------------------------------------
    get_mds_materials_ : give the numbers of MDS materials
   -----------------------------------------------------------------------------------------
    output  : int *mds_nmat    - number of MDS Materials
   ----------------------------------------------------------------------------------------- */
void _FCALL get_mds_materials_( int * mds_nmat){
  * mds_nmat = mds_stack.size();
}

/* -----------------------------------------------------------------------------------------
    unstack_mds_material_ : unstack MDS informations 
   -----------------------------------------------------------------------------------------
    intput  : int *mds_num    - number of MDS variable
    output  : int * mds_id    - MDS Material number
    output  : char * file1    - File1
    output  : int * len_file1 - length of file 1
    output  : char * file2    - File2
    output  : int * len_file2 - length of file 2
    output  : char * file3    - File2
    output  : int * len_file3 - length of file 3
    output  : char * label    - Label
    output  : int * len_label - length of label
    output  : int  * ndepvar  - number of state variables
   ----------------------------------------------------------------------------------------- */
void _FCALL unstack_mds_material_(int *mds_num,
                           int * mds_id,
                           char * file1, int * len_file1,
                           char * file2, int * len_file2,
                           char * file3, int * len_file3,
                           char * label, int * len_label,
                           int  * ndepvar){
 
  int i = *mds_num -1;
  auto mem = mds_stack.begin();
  mem = mem+i ;
  MDSTUPLE member = *mem;

  *mds_id = get<0>(member);

  char * cfile1 = get<1>(member);
  file1 = strcpy(file1,cfile1);
  *len_file1 = strlen(cfile1);

  char * cfile2 = get<2>(member);
  file2 = strcpy(file2,cfile2);
  *len_file2 = strlen(cfile2);

  char * cfile3 = get<3>(member);
  file3 = strcpy(file3,cfile3);
  *len_file3 = strlen(cfile3);

  char ** clabel = get<4>(member);

  * ndepvar = get<5>(member); 

  for (int i=0;i<*ndepvar;i++){
    int siz=strlen(clabel[i]);
    len_label[i]=siz;
    char * my_label = &label[64*i];
    for (int j=0;j< siz ; j++) my_label[j] = clabel[i][j];
  }

}



/* -----------------------------------------------------------------------------------------
    print_mds_ : Dump MDS Stack
   -----------------------------------------------------------------------------------------
   ----------------------------------------------------------------------------------------- */
void print_mds_(){
  cout << "\n\n";
  cout << " MDS Vector size: "  << mds_stack.size() << "\n";
  cout << "---------------------\n";

  int i=0;
  for (auto mem = mds_stack.begin(); mem != mds_stack.end(); mem++){
    MDSTUPLE member = *mem;
    i++;
    cout << i << " -- id : " << get<0>(member) << " -- files : " << get<1>(member) << " , " << get<2>(member) << " , " << get<3>(member) <<  
                 " -- label :" << get<4>(member) << "  -- ndepvar :" << get<5>(member) << endl;
  }

  cout << "\n";
}


/* ----- Extern C end ---------------------------------------------------------------------- */
}
/* ----------------------------------------------------------------------------------------- */

#ifdef MAIN




int main()
{

// MDS Mat 1 
 int mds_id=99;
 char *file1_1="file1.dat";
 char *file2_1="my_file2.dat";
 char *file3_1="my_my_file3.dat";
 char *label_1="labels.txt";
 int lfile1 = strlen(file1_1);
 int lfile2 = strlen(file2_1);
 int lfile3 = strlen(file3_1);
 int llabel = strlen(label_1);
 int ndepvar=4 ;

 add_mds_material_( &mds_id,
                   file1_1, &lfile1,
                   file2_1, &lfile2,
                   file3_1, &lfile3,
                   label_1, &llabel,
                   &ndepvar);

// MDS Mat 2
 int mds_id_2=101;
 char *file1_2="file1_2.dat";
 char *file2_2="my_file2_2.dat";
 char *file3_2="my_my_file3_2.dat";
 char *label_2="labels_2.txt";

 int lfile1_2 = strlen(file1_2);
 int lfile2_2 = strlen(file2_2);
 int lfile3_2 = strlen(file3_2);
 int llabel_2 = strlen(label_2);
 int ndepvar_2=666;

 add_mds_material_( &mds_id_2,
                    file1_2, &lfile1_2,
                    file2_2, &lfile2_2,
                    file3_2, &lfile3_2,
                    label_2, &llabel_2,
                    &ndepvar_2);

// MDS Mat 1 
  mds_id=333;
  file1_1="file1_3.dat";
  file2_1="my_file2_3.dat";
  file3_1="my_my_file3_3.dat";
  label_1="labels_3.txt";
  lfile1 = strlen(file1_1);
  lfile2 = strlen(file2_1);
  lfile3 = strlen(file3_1);
  llabel = strlen(label_1);
  ndepvar=3 ;

 add_mds_material_( &mds_id,
                   file1_1, &lfile1,
                   file2_1, &lfile2,
                   file3_1, &lfile3,
                   label_1, &llabel,
                   &ndepvar);

 print_mds_();

cout << "\n\n";

int mdid;
char f1[256];
char f2[256];
char f3[256];
char la[256];
int lf1,lf2,lf3,lla;
int nv;
int pl;



pl=2;
 unstack_mds_material_(&pl,
                       &mdid,
                       f1, &lf1,
                       f2, &lf2,
                       f3, &lf3,
                       la, &lla,
                       &nv);

cout << pl << " : " << mdid << " " << f1 << " " << f2 << " " << f3 << " " << la << " " << nv << "\n\n";

pl=3;
 unstack_mds_material_(&pl,
                       &mdid,
                       f1, &lf1,
                       f2, &lf2,
                       f3, &lf3,
                       la, &lla,
                       &nv);

cout << pl << " : " << mdid << " " << f1 << " " << f2 << " " << f3 << " " << la << " " << nv << "\n\n";

pl=1;
 unstack_mds_material_(&pl,
                       &mdid,
                       f1, &lf1,
                       f2, &lf2,
                       f3, &lf3,
                       la, &lla,
                       &nv);

cout << pl << " : " << mdid << " " << f1 << " " << f2 << " " << f3 << " " << la << " " << nv << "\n\n";

 return 0;
}

#endif



