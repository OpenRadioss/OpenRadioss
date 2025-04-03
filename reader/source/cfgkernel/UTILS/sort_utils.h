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
#ifndef SORT_UTILS_H
#define SORT_UTILS_H

#ifdef __cplusplus
extern "C" {
#endif

  int mysort_int(int *array,int size_array);
  int myunique_int(int *array,int size_array);
  int mysort_and_unique_int(int *array,int size_array);
  int myunique_and_resize_int(int **array,int *size_array);
  int mydicho_int(int *nod, int low, int high, int number);
  int mydicho_int_multi(int *array,int size_array,int index_to_found,int *number_index_to_found);
  int mycmp_int(int *n1,int *n2);
  int mycpy_int(int size_index1,int *index1,int *size_index2,int **index2,int if_resize);
  int mykill_int_on(int **index,int *nb_in,int *index_ref,int nb_ref);
  
  int mycmp_ptr(void **n1,void **n2);
  int mysort_ptr(void **array,int size_array);
  int mydicho_ptr(void **nod, int low, int high, void *number);
  
  
  int myunique_ptr(void **ptr_tab,int nb_ptrs);
  
  
  int mysort_and_unique_ptr(void **array,int size_array);
  
  /*SB/5/04/07/2005*/
  int mycmp_double(double *d1, double *d2);
  /*SB/5/04/07/2005/END*/


#if defined _WIN32 || defined WIN32
  void myqsort (void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *));
#else
#define myqsort qsort
#endif /*_WIN32  */


#ifdef __cplusplus
}
#endif

#define utility_qsort_integer                  mysort_int
#define utility_kill_double_integer            myunique_int
#define utility_qsort_integer_and_kill_double  mysort_and_unique_int
#define utility_kill_double_integer_and_resize myunique_and_resize_int
#define utility_dicho_int                      mydicho_int
#define utility_found_int_in_array_with_double mydicho_int_multi
#define utility_cmp_integer                    mycmp_int
#define utility_copy_array                     mycpy_int
#define utility_kill_object_on                 mykill_int_on

#define utility_cmp_void_p                     mycmp_ptr
#define utility_qsort_void_p                   mysort_ptr
#define utility_dicho_void                     mydicho_ptr


#define utility_kill_double_void_p             myunique_ptr


#define utility_qsort_void_p_and_kill_double   mysort_and_unique_ptr
#endif /* SORT_UTILS_H */




