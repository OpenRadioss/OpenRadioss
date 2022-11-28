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
#include<tuple> // for tuple 
#include <vector> 
#include <algorithm>

#define _FCALL

#ifdef _WIN64
#define get_merged_surface_     GET_MERGED_SURFACE
#define get_merged_lines_       GET_MERGED_LINES

#define union_surface_          UNION_SURFACE
#define delete_surface_         DELETE_SURFACE
#define intersect_surface_      INTERSECT_SURFACE

#define union_line_             UNION_LINE  
#define delete_line_            DELETE_LINE
#define intersect_line_         INTERSECT_LINE

#define surf_remove_duplicates_   SURF_REMOVE_DUPLICATES
#define line_remove_duplicates_   LINE_REMOVE_DUPLICATES

#endif

using namespace std; 


/* Tuple surface_memb;
   1st int : node 1
   2nd int : node 2
   3rd int : node 3
   4th int : node 4 
   5th int : eltype
   6xt int : elid
*/

#define STUPL tuple < int,int,int,int,int,int >
vector < STUPL > surface;


/* Tuple line_memb;
   1st int : node 1
   2nd int : node 2
   3rd int : eltype
   4th int : elid
*/
#define LTUPL tuple < int,int,int,int >
vector < LTUPL > lines;




/* ----------------------------------------------------------------------------
    STUPL create_surface_member  : create a new STUPL with all its members : 4 nodes + eltype + elid
   ---------------------------------------------------------------------------- */
STUPL create_surface_member (int n1,int n2,int n3,int n4 , int eltype, int elid){
  STUPL new_member =  make_tuple ( n1,n2,n3,n4,eltype,elid);
  return new_member;
}

/* ----------------------------------------------------------------------------
    STUPL create_line_member  : create a new STUPL with all its members : 4 nodes + eltype + elid
   ---------------------------------------------------------------------------- */
LTUPL create_line_member (int n1,int n2, int eltype, int elid){
  LTUPL new_member =  make_tuple ( n1,n2,eltype,elid);
  return new_member;
}

/* ----------------------------------------------------------------------------
    comp : compare 2 integers a & b
   ----------------------------------------------------------------------------
INPUT
  a,b : integer
RESULT 
  0 if a == b
  1 if a >  b
 -1 if a <  b
   --------------------------------------------------------- */
int comp (int a,int b)
{
  int res;
  if (a==b) res=0;
  if (a > b) res=1;
  if (a < b) res=-1;

  return res;
}

/* ----------------------------------------------------------------------------
    tupl_compare : compare 2 surfaces with their Element ID
                   each surface is described with a TUPL containing the values 
                   comparison take cares on the surface 4 nodes only
   ----------------------------------------------------------------------------
    STUPL lhs : lef had side surface Tuple
    STUPL rhs : right hnd side surface tuple

    return value : 1 if lhs > rhs
                   0 if lhs = rhs
                  -1 if lhs < rhs
   --------------------------------------------------------- */
int tupl_compare( STUPL lhs, STUPL rhs  ){

    int res;
    int a,b; 

    a=get<5>(lhs);
    b=get<5>(rhs);

    res = comp ( a, b);
    return res;
}


/* ----------------------------------------------------------------------------
    tupl_compare_surf : compare 2 surfaces with their nodeID + EltID
                   each surface is described with a TUPL containing the values 
                   comparison take cares on the surface 4 nodes only
   ----------------------------------------------------------------------------
    STUPL lhs : lef had side surface Tuple
    STUPL rhs : right hnd side surface tuple

    return value : 1 if lhs > rhs
                   0 if lhs = rhs
                  -1 if lhs < rhs
   --------------------------------------------------------- */
int tupl_compare_surf( STUPL lhs, STUPL rhs  ){

    int res;
    int a,b; 

    a=get<0>(lhs);                // first node
    b=get<0>(rhs);

    res = comp ( a, b);
    if (res != 0) return res;

    a=get<1>(lhs);                // 2nd node
    b=get<1>(rhs);

    res = comp ( a, b);
    if (res != 0) return res;

    a=get<2>(lhs);                // 3rd node
    b=get<2>(rhs);

    res = comp ( a, b);
    if (res != 0) return res;

    a=get<3>(lhs);                // 4th node
    b=get<3>(rhs);

    res = comp ( a, b);
    if (res != 0) return res;

    a=get<5>(lhs);                // eltID
    b=get<5>(rhs);

    res = comp ( a, b);

    return res;
}

/* ----------------------------------------------------------------------------
    tupl_compare : compare 2 lines with their Node + Element ID
                   each surface is described with a TUPL containing the values 
                   comparison take cares on the surface 4 nodes only
   ----------------------------------------------------------------------------
    STUPL lhs : lef hand side line Tuple
    STUPL rhs : right hand side line tuple

    return value : 1 if lhs > rhs
                   0 if lhs = rhs
                  -1 if lhs < rhs
   --------------------------------------------------------- */
int ltupl_compare( LTUPL lhs, LTUPL rhs  ){

    int res;
    int a,b; 

// Compare first node
    a=get<0>(lhs);
    b=get<0>(rhs);

    res = comp ( a, b);
    if (res != 0) return res;

// Compare second node
    a=get<1>(lhs);
    b=get<1>(rhs);
    res = comp ( a, b);
    if (res != 0) return res;

// Compare element ID
    a=get<3>(lhs);
    b=get<3>(rhs);
    res = comp ( a, b);
    return res;
}

/* ----------------------------------------------------------------------------
    void print_surface() : debug print the merged surface
   ---------------------------------------------------------------------------- */
void print_surface()
{
  cout << "Number of members= " <<  surface.size() << endl;
  int i=0;
  for (auto mem = surface.begin(); mem != surface.end(); mem++){
    STUPL member = *mem;
    i++;
    cout << i << " -- nodes : " << get<0>(member) << " , " << get<1>(member) << " , " << get<2>(member) << " , " << get<3>(member) <<  
                 " -- eltyp :" << get<4>(member) << "  -- elid :" << get<5>(member) << endl;
  }
 
}

/* ----------------------------------------------------------------------------
    void print_surface() : debug print the merged surface
   ---------------------------------------------------------------------------- */
void print_line()
{
  cout << "Number of members= " <<  lines.size() << endl;
  int i=0;
  for (auto mem = lines.begin(); mem != lines.end(); mem++){
    LTUPL member = *mem;
    i++;
    cout << i << " -- nodes : " << get<0>(member) << " , " << get<1>(member) << " -- eltyp :" << get<2>(member) << "  -- elid :" << get<3>(member) << endl;
  }
 
}


/* ---------------------------------------------------------------------------- 
   C/Fortran usable
  ---------------------------------------------------------------------------- */
extern "C"
{
/* ----------------------------------------------------------------------------
    union_surface : union of 2 surfaces according to their 4 node IDs
                    this is dedicated for Fortran call
                    -> fills a global vector in C++,
                    other routine pass the values back to Fortran
   ----------------------------------------------------------------------------
   INPUT
     s1_nd1 - s1_nd4 : 1st surface - 4 nodes
     s1_eltyp        : 1st surface - element type
     s1_elid         : 1st surface - element ID
     s1_nmemb        : 1st surface - number of surfzce segments.

     s2_nd1 - s2_nd4 : 2nd surface - 4 nodes
     s2_eltyp        : 2nd surface - element type
     s2_elid         : 2nd surface - element ID
     s2_nmemb        : 2nd surface - number of surface segments.

   OUTPUT
     int * nmember : number of surface segments 
   ----------------------------------------------------------------------------
*/

void union_surface_(int * s1_nd1, int * s1_nd2, int * s1_nd3, int* s1_nd4,int * s1_eltyp,int * s1_elid,int * s1_nmemb,
                   int * s2_nd1, int * s2_nd2, int * s2_nd3, int* s2_nd4,int * s2_eltyp,int * s2_elid,int * s2_nmemb,
                   int * nmember )
{
  bool iterator;
  int i1, i2;

  iterator = true;
  i1 = 0;
  i2 = 0;

  while (iterator == true ){
    if (i1 == (*s1_nmemb) ) {
      for (int j=i2; j< *s2_nmemb; j++){
         STUPL member = create_surface_member (s2_nd1[j],s2_nd2[j],s2_nd3[j],s2_nd4[j],s2_eltyp[j],s2_elid[j] );
         surface.push_back(member);
      }
     * nmember = surface.size();
     return;
    }

    if (i2 == (*s2_nmemb) ) {
      for (int j=i1; j< * s1_nmemb; j++){
         STUPL member = create_surface_member (s1_nd1[j],s1_nd2[j],s1_nd3[j],s1_nd4[j],s1_eltyp[j],s1_elid[j] );
         surface.push_back(member);
      }
     * nmember = surface.size();
     return;
    }

    STUPL member1 = create_surface_member (s1_nd1[i1],s1_nd2[i1],s1_nd3[i1],s1_nd4[i1],s1_eltyp[i1],s1_elid[i1] );
    STUPL member2 = create_surface_member (s2_nd1[i2],s2_nd2[i2],s2_nd3[i2],s2_nd4[i2],s2_eltyp[i2],s2_elid[i2] );

    int res = tupl_compare_surf( member1, member2  );
    if      (res == 1)  { surface.push_back(member2); i2++ ;      }  
    else if (res == -1) { surface.push_back(member1); i1++;       }
    else                { surface.push_back(member1); i1++; i2++; }
    
  }
  
}



/* ----------------------------------------------------------------------------
    delete_surface : remove all elements from surface 2 in surface 1
                    this is dedicated for Fortran call
                    -> fills a global vector in C++,
                    other routine pass the values back to Fortran
   ----------------------------------------------------------------------------
   INPUT
     s1_nd1 - s1_nd4 : 1st surface - 4 nodes
     s1_eltyp        : 1st surface - element type
     s1_elid         : 1st surface - element ID
     s1_nmemb        : 1st surface - number of surface segments.

     s2_nd1 - s2_nd4 : 2nd surface - 4 nodes
     s2_eltyp        : 2nd surface - element type
     s2_elid         : 2nd surface - element ID
     s2_nmemb        : 2nd surface - number of surface segments.

   OUTPUT
     int * nmember : number of surface segments 
   ----------------------------------------------------------------------------
*/

void delete_surface_(int * s1_nd1, int * s1_nd2, int * s1_nd3, int* s1_nd4,int * s1_eltyp,int * s1_elid,int * s1_nmemb,
                     int * s2_nd1, int * s2_nd2, int * s2_nd3, int* s2_nd4,int * s2_eltyp,int * s2_elid,int * s2_nmemb,
                     int * nmember )
{
  bool iterator;
  int i1, i2;

  iterator = true;
  i1 = 0;
  i2 = 0;

  while (iterator == true ){

    if (i1 == (*s1_nmemb) ) {                  // surface 1 is terminated - terminated 
      * nmember = surface.size();
      return;
    }

    if (i2 == (*s2_nmemb) ) {                  // surface 2 is terminated - terminate with all surface 1 elements
      for (int j=i1; j< * s1_nmemb; j++){
         STUPL member = create_surface_member (s1_nd1[j],s1_nd2[j],s1_nd3[j],s1_nd4[j],s1_eltyp[j],s1_elid[j] );
         surface.push_back(member);
      }
     * nmember = surface.size();
     return;
    }

    STUPL member1 = create_surface_member (s1_nd1[i1],s1_nd2[i1],s1_nd3[i1],s1_nd4[i1],s1_eltyp[i1],s1_elid[i1] );
    STUPL member2 = create_surface_member (s2_nd1[i2],s2_nd2[i2],s2_nd3[i2],s2_nd4[i2],s2_eltyp[i2],s2_elid[i2] );

    int res = tupl_compare_surf( member1, member2  );
    if      (res == 1)  { i2++ ;      }  
    else if (res == -1) { surface.push_back(member1); i1++;       }
    else                { i1++; i2++; }                             // Surfaces are identic - don't keep the surface
    
  }
  
}



/* ----------------------------------------------------------------------------
    intersect_surface : intersection between surface 1 & surface 2
                    this is dedicated for Fortran call
                    -> fills a global vector in C++,
                    other routine pass the values back to Fortran
   ----------------------------------------------------------------------------
   INPUT
     s1_nd1 - s1_nd4 : 1st surface - 4 nodes
     s1_eltyp        : 1st surface - element type
     s1_elid         : 1st surface - element ID
     s1_nmemb        : 1st surface - number of surface segments.

     s2_nd1 - s2_nd4 : 2nd surface - 4 nodes
     s2_eltyp        : 2nd surface - element type
     s2_elid         : 2nd surface - element ID
     s2_nmemb        : 2nd surface - number of surface segments.

   OUTPUT
     int * nmember : number of surface segments 
   ----------------------------------------------------------------------------
*/

void intersect_surface_(int * s1_nd1, int * s1_nd2, int * s1_nd3, int* s1_nd4,int * s1_eltyp,int * s1_elid,int * s1_nmemb,
                        int * s2_nd1, int * s2_nd2, int * s2_nd3, int* s2_nd4,int * s2_eltyp,int * s2_elid,int * s2_nmemb,
                        int * nmember )
{
  bool iterator;
  int i1, i2;

  iterator = true;
  i1 = 0;
  i2 = 0;

  while (iterator == true ){

    if (i1 == (*s1_nmemb) ) {                  // surface 1 is terminated - terminated 
      * nmember = surface.size();
      return;
    }

    if (i2 == (*s2_nmemb) ) {                  // surface 2 is terminated - terminated 
     * nmember = surface.size();
     return;
    }

    STUPL member1 = create_surface_member (s1_nd1[i1],s1_nd2[i1],s1_nd3[i1],s1_nd4[i1],s1_eltyp[i1],s1_elid[i1] );
    STUPL member2 = create_surface_member (s2_nd1[i2],s2_nd2[i2],s2_nd3[i2],s2_nd4[i2],s2_eltyp[i2],s2_elid[i2] );

    int res = tupl_compare_surf( member1, member2  );
    if      (res == 1)  { i2++ ; }  
    else if (res == -1) { i1++ ; }
    else                { surface.push_back(member1); i1++; i2++; }        // Surfaces are same - keep the surface
    
  }
  
}


/* ----------------------------------------------------------------------------
    surf_remove_duplicates : Remove duplicates in sorted lines
   ----------------------------------------------------------------------------
   INPUT/OUTPUT
     s1_nd1 - s1_nd4 : surface - 4 nodes
     s1_eltyp        : surface - element type
     s1_elid         : surface - element ID
     s1_nmemb        : surface - number of surface segments.
   ----------------------------------------------------------------------------
*/
   void surf_remove_duplicates_(int * s1_nd1, int * s1_nd2, int * s1_nd3, int* s1_nd4,int * s1_eltyp,int * s1_elid,int *size,int * new_size)
{
  int sz=1 ;
  int i=1;

  while (i< *size) {
     
     STUPL member1 = create_surface_member (s1_nd1[i],s1_nd2[i],s1_nd3[i],s1_nd4[i],s1_eltyp[i],s1_elid[i] );
     STUPL member2 = create_surface_member (s1_nd1[i-1],s1_nd2[i-1],s1_nd3[i-1],s1_nd4[i-1],s1_eltyp[i-1],s1_elid[i-1] );
     int res = tupl_compare_surf( member1, member2  );
     if ( res != 0) {
          s1_nd1[sz] = s1_nd1[i];
          s1_nd2[sz] = s1_nd2[i];
          s1_nd3[sz] = s1_nd3[i];
          s1_nd4[sz] = s1_nd4[i];
          s1_eltyp[sz] = s1_eltyp[i];
          s1_elid[sz] = s1_elid[i];
          sz++;
     }
     i++;
}

     *new_size = sz;
     return;

}



/* ----------------------------------------------------------------------------
    union_line : union of 2 lines according to their 4 node IDs
                    this is dedicated for Fortran call
                    -> fills a global vector in C++,
                    other routine pass the values back to Fortran
   ----------------------------------------------------------------------------
   INPUT
     s1_nd1 - s1_nd2 : 1st surface - 4 nodes
     s1_eltyp        : 1st surface - element type
     s1_elid         : 1st surface - element ID
     s1_nmemb        : 1st surface - number of surfzce segments.

     s2_nd1 - s2_nd2 : 2nd surface - 4 nodes
     s2_eltyp        : 2nd surface - element type
     s2_elid         : 2nd surface - element ID
     s2_nmemb        : 2nd surface - number of surface segments.

   OUTPUT
     int * nmember : number of surface segments 
   ----------------------------------------------------------------------------
*/
void _FCALL union_line_(int * s1_nd1, int * s1_nd2, int * s1_eltyp,int * s1_elid,int * s1_nmemb,
                 int * s2_nd1, int * s2_nd2, int * s2_eltyp,int * s2_elid,int * s2_nmemb,
                 int * nmember )
{
  bool iterator;
  int i1, i2;

  iterator = true;
  i1 = 0;
  i2 = 0;

  while (iterator == true ){
    if (i1 == (*s1_nmemb) ) {
      for (int j=i2; j< *s2_nmemb; j++){
         LTUPL member = create_line_member (s2_nd1[j],s2_nd2[j],s2_eltyp[j],s2_elid[j] );
         lines.push_back(member);
      }
     * nmember = lines.size();
     return;
    }

    if (i2 == (*s2_nmemb) ) {
      for (int j=i1; j< * s1_nmemb; j++){
         LTUPL member = create_line_member (s1_nd1[j],s1_nd2[j],s1_eltyp[j],s1_elid[j] );
         lines.push_back(member);
      }
     * nmember = lines.size();
     return;
    }

    LTUPL member1 = create_line_member (s1_nd1[i1],s1_nd2[i1],s1_eltyp[i1],s1_elid[i1] );
    LTUPL member2 = create_line_member (s2_nd1[i2],s2_nd2[i2],s2_eltyp[i2],s2_elid[i2] );

    int res = ltupl_compare( member1, member2  );
    if      (res == 1)  { lines.push_back(member2); i2++ ;      }  
    else if (res == -1) { lines.push_back(member1); i1++;       }
    else                { lines.push_back(member1); i1++; i2++; }
    
  }
  
}


/* ----------------------------------------------------------------------------
    delete_line :   delete all elements from Surface 2 in Surface according to their 2 node IDs + ElementID
                    this is dedicated for Fortran call
                    -> fills a global vector in C++,
                    other routine pass the values back to Fortran
   ----------------------------------------------------------------------------
   INPUT
     s1_nd1 - s1_nd2 : 1st line - 2 nodes
     s1_eltyp        : 1st line - element type
     s1_elid         : 1st line - element ID
     s1_nmemb        : 1st line - number of surfzce segments.

     s2_nd1 - s2_nd2 : 2nd line - 2 nodes
     s2_eltyp        : 2nd line - element type
     s2_elid         : 2nd line - element ID
     s2_nmemb        : 2nd line - number of surface segments.

   OUTPUT
     int * nmember : number of surface segments 
   ----------------------------------------------------------------------------
*/
void _FCALL delete_line_(int * s1_nd1, int * s1_nd2, int * s1_eltyp,int * s1_elid,int * s1_nmemb,
                         int * s2_nd1, int * s2_nd2, int * s2_eltyp,int * s2_elid,int * s2_nmemb,
                         int * nmember )
{
  bool iterator;
  int i1, i2;

  iterator = true;
  i1 = 0;
  i2 = 0;

  while (iterator == true ){
    if (i1 == (*s1_nmemb) ) {                  // surface 1 is terminated - terminated 
     * nmember = lines.size();
     return;
    }

    if (i2 == (*s2_nmemb) ) {
      for (int j=i1; j< * s1_nmemb; j++){
         LTUPL member = create_line_member (s1_nd1[j],s1_nd2[j],s1_eltyp[j],s1_elid[j] );
         lines.push_back(member);
      }
     * nmember = lines.size();
     return;
    }

    LTUPL member1 = create_line_member (s1_nd1[i1],s1_nd2[i1],s1_eltyp[i1],s1_elid[i1] );
    LTUPL member2 = create_line_member (s2_nd1[i2],s2_nd2[i2],s2_eltyp[i2],s2_elid[i2] );

    int res = ltupl_compare( member1, member2  );
    if      (res == 1)  {  i2++ ;      }  
    else if (res == -1) { lines.push_back(member1); i1++;       }
    else                {  i1++; i2++; }                               // lines are same - don't keep the line
    
  }
  
}


/* ----------------------------------------------------------------------------
    intersect_line : line intersection between line1 and line2 according to their 2 node IDs + ElementID
                    this is dedicated for Fortran call
                    -> fills a global vector in C++,
                    other routine pass the values back to Fortran
   ----------------------------------------------------------------------------
   INPUT
     s1_nd1 - s1_nd2 : 1st line - 2 nodes
     s1_eltyp        : 1st line - element type
     s1_elid         : 1st line - element ID
     s1_nmemb        : 1st line - number of surfzce segments.

     s2_nd1 - s2_nd2 : 2nd line - 2 nodes
     s2_eltyp        : 2nd line - element type
     s2_elid         : 2nd line - element ID
     s2_nmemb        : 2nd line - number of surface segments.

   OUTPUT
     int * nmember : number of surface segments 
   ----------------------------------------------------------------------------
*/
void _FCALL intersect_line_(int * s1_nd1, int * s1_nd2, int * s1_eltyp,int * s1_elid,int * s1_nmemb,
                            int * s2_nd1, int * s2_nd2, int * s2_eltyp,int * s2_elid,int * s2_nmemb,
                            int * nmember )
{
  bool iterator;
  int i1, i2;

  iterator = true;
  i1 = 0;
  i2 = 0;

  while (iterator == true ){
    if (i1 == (*s1_nmemb) ) {
     * nmember = lines.size();
     return;
    }

    if (i2 == (*s2_nmemb) ) {
     * nmember = lines.size();
     return;
    }

    LTUPL member1 = create_line_member (s1_nd1[i1],s1_nd2[i1],s1_eltyp[i1],s1_elid[i1] );
    LTUPL member2 = create_line_member (s2_nd1[i2],s2_nd2[i2],s2_eltyp[i2],s2_elid[i2] );

    int res = ltupl_compare( member1, member2  );
    if      (res == 1)  { i2++ ;      }  
    else if (res == -1) { i1++;       }
    else                { lines.push_back(member1); i1++; i2++; }         // lines are same - keep member1
    
  }
  
}

/* ----------------------------------------------------------------------------
    surf_remove_duplicates : Remove duplicates in sorted lines
   ----------------------------------------------------------------------------
   INPUT/OUTPUT
     s1_nd1 - s1_nd4 : surface - 4 nodes
     s1_eltyp        : surface - element type
     s1_elid         : surface - element ID
     s1_nmemb        : surface - number of surface segments.
   ----------------------------------------------------------------------------
*/
   void line_remove_duplicates_(int * l1_nd1, int * l1_nd2,int * l1_eltyp,int * l1_elid,int *size,int * new_size)
{
  int sz=1 ;
  int i=1;

  while (i< *size) {
     
     LTUPL member1 = create_line_member (l1_nd1[i],l1_nd2[i],l1_eltyp[i],l1_elid[i] );
     LTUPL member2 = create_line_member (l1_nd1[i-1],l1_nd2[i-1],l1_eltyp[i-1],l1_elid[i-1] );
     int res = ltupl_compare( member1, member2  );
     if ( res != 0) {
          l1_nd1[sz] = l1_nd1[i];
          l1_nd2[sz] = l1_nd2[i];
          l1_eltyp[sz] = l1_eltyp[i];
          l1_elid[sz] = l1_elid[i];
          sz++;
     }
     i++;
}
     *new_size = sz;
     return;
}


/* ----------------------------------------------------------------------------
    get_merged_surface_ : Pass the surface vector in Fortran & deletes the surface.
   ----------------------------------------------------------------------------
   OUTPUT
     s_nd1 - s_nd4 : 1st surface - 2 nodes
     s_eltyp        : 1st surface - element type
     s_elid         : 1st surface - element ID
   ----------------------------------------------------------------------------  */
void _FCALL get_merged_surface_ ( int * s_nd1, int * s_nd2, int * s_nd3, int* s_nd4,int * s_eltyp,int * s_elid )
{
 int i=0;
 for (auto mem = surface.begin(); mem != surface.end(); mem++){
    STUPL member = *mem;
    s_nd1[i]   = get<0>(member);
    s_nd2[i]   = get<1>(member);
    s_nd3[i]   = get<2>(member);
    s_nd4[i]   = get<3>(member);
    s_eltyp[i] = get<4>(member);
    s_elid[i]  = get<5>(member);
    i++;
  }
  // erase the Surface after get back the results
  std::vector<STUPL>().swap(surface);
}

/* ----------------------------------------------------------------------------
    get_merged_lines_ : Pass the line vector in Fortran & deletes the surface.
   ----------------------------------------------------------------------------
   OUTPUT
     s_nd1 - s_nd2 : 1st surface - 2 nodes
     s_eltyp        : 1st surface - element type
     s_elid         : 1st surface - element ID
   ----------------------------------------------------------------------------  */
void _FCALL get_merged_lines_ ( int * s_nd1, int * s_nd2,int * s_eltyp,int * s_elid )
{
 int i=0;
 for (auto mem = lines.begin(); mem != lines.end(); mem++){
    LTUPL member = *mem;
    s_nd1[i]   = get<0>(member);
    s_nd2[i]   = get<1>(member);
    s_eltyp[i] = get<2>(member);
    s_elid[i]  = get<3>(member);
    i++;
  }
  // erase the Surface after get back the results
  std::vector<LTUPL>().swap(lines);
}


/* ----------------------------------------------------------------------------------------------------------- 
   EXTERN C END
   ----------------------------------------------------------------------------------------------------------- */
}

#ifdef MAIN
int main() 
{ 
    int nmember;
    int   s1_nd1[6]={3,3,4,4,7,7};
    int   s1_nd2[6]={5,5,1,1,8,8};
    int   s1_nd3[6]={4,6,2,4,1,4};
    int   s1_nd4[6]={1,2,1,5,2,5};
    int s1_eltyp[6]={3,3,3,3,3,3};
    int  s1_elid[6]={1,2,3,4,5,6};
    int  s1_nmemb = 6;

    int   s2_nd1[4]={1,3,4,9};
    int   s2_nd2[4]={5,5,8,8};
    int   s2_nd3[4]={4,6,2,7};
    int   s2_nd4[4]={1,2,1,6};
    int s2_eltyp[4]={3,3,3,3};
    int  s2_elid[4]={9,2,7,8};
    int  s2_nmemb = 4;

    cout << "Union Surface \n";
    cout << "------------- \n" << "\n";

    union_surface_(s1_nd1, s1_nd2, s1_nd3, s1_nd4, s1_eltyp, s1_elid,&s1_nmemb,
                   s2_nd1, s2_nd2, s2_nd3, s2_nd4, s2_eltyp, s2_elid,&s2_nmemb,
                   &nmember );

    print_surface();

    int s_nd1[nmember];
    int s_nd2[nmember];
    int s_nd3[nmember];
    int s_nd4[nmember];
    int s_eltyp[nmember];
    int s_elid[nmember];
    get_merged_surface_ ( s_nd1, s_nd2, s_nd3, s_nd4,s_eltyp, s_elid );


    cout << "\n\n";
    cout << "Delete Surface \n";
    cout << "-------------- \n" << "\n";

    delete_surface_(s1_nd1, s1_nd2, s1_nd3, s1_nd4, s1_eltyp, s1_elid,&s1_nmemb,
                  s2_nd1, s2_nd2, s2_nd3, s2_nd4, s2_eltyp, s2_elid,&s2_nmemb,
                  &nmember );

    print_surface();

    int sd_nd1[nmember];
    int sd_nd2[nmember];
    int sd_nd3[nmember];
    int sd_nd4[nmember];
    int sd_eltyp[nmember];
    int sd_elid[nmember];

    get_merged_surface_ ( sd_nd1, sd_nd2, sd_nd3, sd_nd4,sd_eltyp, sd_elid );


    cout << "\n\n";
    cout << "Intersect Surface \n";
    cout << "----------------- \n" << "\n";

    intersect_surface_(s1_nd1, s1_nd2, s1_nd3, s1_nd4, s1_eltyp, s1_elid,&s1_nmemb,
                       s2_nd1, s2_nd2, s2_nd3, s2_nd4, s2_eltyp, s2_elid,&s2_nmemb,
                       &nmember );

    print_surface();

    int si_nd1[nmember];
    int si_nd2[nmember];
    int si_nd3[nmember];
    int si_nd4[nmember];
    int si_eltyp[nmember];
    int si_elid[nmember];

    get_merged_surface_ ( si_nd1, si_nd2, si_nd3, si_nd4,si_eltyp, si_elid );
    
   
    cout << "\n\nLines"<< endl;;
    cout << "----- \n\n";


    int   l1_nd1[6]={3,3,4,4,7,7};
    int   l1_nd2[6]={5,6,1,5,1,8};
    int l1_eltyp[6]={3,3,3,3,3,3};
    int  l1_elid[6]={1,2,3,4,5,6};
    int  l1_nmemb = 6;

    int   l2_nd1[4]={1,3,4,9};
    int   l2_nd2[4]={5,6,8,8};
    int l2_eltyp[4]={3,3,3,3};
    int  l2_elid[4]={9,2,7,8};
    int  l2_nmemb = 4;

    cout << "Union Line \n";
    cout << "---------- \n" << "\n";

    union_line_(l1_nd1, l1_nd2, l1_eltyp, l1_elid, &l1_nmemb,
                l2_nd1, l2_nd2, l2_eltyp, l2_elid, &l2_nmemb,
                &nmember );

    print_line();

    int lu_nd1[nmember];
    int lu_nd2[nmember];
    int lu_eltyp[nmember];
    int lu_elid[nmember];

    get_merged_lines_ ( lu_nd1, lu_nd2,lu_eltyp, lu_elid );

    cout << "\nDelete Line \n";
    cout << "------------- \n" << "\n";

    delete_line_(l1_nd1, l1_nd2, l1_eltyp, l1_elid, &l1_nmemb,
                 l2_nd1, l2_nd2, l2_eltyp, l2_elid, &l2_nmemb,
                 &nmember );

    print_line();

    int ld_nd1[nmember];
    int ld_nd2[nmember];
    int ld_eltyp[nmember];
    int ld_elid[nmember];

    get_merged_lines_ ( ld_nd1, ld_nd2,ld_eltyp, ld_elid );
    get_merged_lines_ ( lu_nd1, lu_nd2,lu_eltyp, lu_elid );


    cout << "\nIntsersect Line \n";
    cout << "---------------- \n" << "\n";

    intersect_line_(l1_nd1, l1_nd2, l1_eltyp, l1_elid, &l1_nmemb,
                    l2_nd1, l2_nd2, l2_eltyp, l2_elid, &l2_nmemb,
                    &nmember );

    print_line();

    int li_nd1[nmember];
    int li_nd2[nmember];
    int li_eltyp[nmember];
    int li_elid[nmember];

    get_merged_lines_ ( li_nd1, li_nd2,li_eltyp, li_elid );

    cout << "\n\n";

    int ns;
    int   l5_nd1[7]={3,3,4,4,4,7,7};
    int   l5_nd2[7]={5,6,1,1,5,1,8};
    int l5_eltyp[7]={3,3,3,3,3,3,3};
    int  l5_elid[7]={1,2,3,3,4,5,6};
    int  l5_nmemb = 7;
    line_remove_duplicates_(l5_nd1, l5_nd2,l5_eltyp,l5_elid,&l5_nmemb,&ns);

    cout << "New Size :" << ns << "\n";
    for (int i=0;i<ns;i++){
      cout << i << " - " << l5_nd1[i] << " " <<  l5_nd2[i]<< " "  << l5_eltyp[i]<< " "  << l5_elid[i] << "\n";
    }
    return 0; 
}

#endif

