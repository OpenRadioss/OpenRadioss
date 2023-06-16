//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2023 Altair Engineering Inc.
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
#include <algorithm>
 
using namespace std;

#define _FCALL

#ifdef _WIN64

#define remove_duplicates_ REMOVE_DUPLICATES
#define union_2_sorted_sets_ UNION_2_SORTED_SETS
#define intersect_2_sorted_sets_ INTERSECT_2_SORTED_SETS
#define difference_2_sorted_sets_ DIFFERENCE_2_SORTED_SETS
#define count_member_list_ COUNT_MEMBER_LIST


#endif

extern "C" {


/* -----------------------------------------------------------------------------------------------------
    remove_duplicates : removes duplicates in a sorted array
   -----------------------------------------------------------------------------------------------------
    int * array1      : input/output - array
    int * size        : input  - size of array
    int * new_size    : output - new_size after duplicate removal
   ------------------------------------------------------------------------------------------------ */
    void remove_duplicates_ ( int * array, int * size, int *new_size)
    {
      if ( * size == 0){
        *new_size=0;
        return;
      }
      int sz=1 ;
      int i=1;

      while (i< *size) {
         if ( array[i-1] !=  array[i]) {
            array[sz] = array[i];
            sz++;
         }
      i++;
     }

     *new_size = sz;
     return;
    }

/* -----------------------------------------------------------------------------------------------------
    union_2_sorted_sets : union 2 sets of sorted arrays.
    The Arrays shall not contain 2 time same entity
    The result arrays must be already allocated & big enough, result_size is the size of the nex set.
   -----------------------------------------------------------------------------------------------------
    int * array1      : input  - array1
    int * array1_size : input  - size of array1
    int * array2      : input  - array2
    int * array2_size : input  - size of array2
    int * result      : output - result
    int * result_size : output - number of members in result
   ------------------------------------------------------------------------------------------------ */
  void _FCALL union_2_sorted_sets_(int * array1, int * array1_size,
                     int * array2, int * array2_size,
                     int * result, int * result_size )
  {

    int *fin = set_union(array1, array1+*array1_size, 
                         array2, array2+*array2_size, 
                         result);
    
    *result_size = fin-result;
  }


/* -----------------------------------------------------------------------------------------------------
    intersect_2_sorted_sets : intersections of 2 sets of sorted arrays.
    The Arrays shall not contain 2 time same entity
    The result arrays must be already allocated & big enough, result_size is the size of the nex set.
   -----------------------------------------------------------------------------------------------------
    int * array1      : input  - array1
    int * array1_size : input  - size of array1
    int * array2      : input  - array2
    int * array2_size : input  - size of array2
    int * result      : output - result
    int * result_size : output - number of members in result
   ------------------------------------------------------------------------------------------------ */
  void _FCALL intersect_2_sorted_sets_(int * array1, int * array1_size,
                     int * array2, int * array2_size,
                     int * result, int * result_size )
  {

    int *fin = set_intersection(array1, array1+*array1_size, 
                                array2, array2+*array2_size, 
                                result);
    
    *result_size = fin-result;
  }

/* -----------------------------------------------------------------------------------------------------
    difference_2_sorted_sets : difference of 2 sets of sorted arrays.
    Does array1-array2 : Removes all members of array2 in array1
    The Arrays shall not contain 2 time same entity
    The result arrays must be already allocated & big enough, result_size is the size of the nex set.
   -----------------------------------------------------------------------------------------------------
    int * array1      : input  - array1
    int * array1_size : input  - size of array1
    int * array2      : input  - array2
    int * array2_size : input  - size of array2
    int * result      : output - result
    int * result_size : output - number of members in result
   ------------------------------------------------------------------------------------------------ */
  void _FCALL difference_2_sorted_sets_(int * array1, int * array1_size,
                     int * array2, int * array2_size,
                     int * result, int * result_size )
  {

    int *fin = set_difference(array1, array1+*array1_size, 
                              array2, array2+*array2_size, 
                              result);
    
    *result_size = fin-result;
  }
/* -----------------------------------------------------------------------------------------------------
    count_member_list : count the number of appeareance of union_list's members in the merged_list
   -----------------------------------------------------------------------------------------------------
    int * union_list : input - size=size_union_list
    int * size_union_list : input - size of union_list
    int * merged_list : input - size=size_merged_list
    int * size_merged_list : input - size of merged_list
    int * number_appearance : output - number of appearance of the union_list's members in the merged_list
    int * proc_id : output - processor id of the segment
   ------------------------------------------------------------------------------------------------ */
  void _FCALL count_member_list_(int * union_list, int * size_union_list,
                                 int * merged_list, int * size_merged_list,
                                 int * number_appearance, int * proc_id )
  {
    int max_number_appearance = 0 ; 
    for(int i=0; i<*size_union_list ; i++)
    {
        int my_value = union_list[i] ;
        int mycount = std::count ( merged_list,  merged_list+ *size_merged_list, my_value);
        number_appearance[i] = mycount ;
        if(max_number_appearance<mycount)
        {
            *proc_id = i+1 ;
            max_number_appearance = mycount;
        }
    }
  }



}



