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
#ifndef UTILS_H
#define UTILS_H

#include <stdlib.h> 
#if defined _WIN32 || defined WIN32
#include <malloc.h>
#else
#include <unistd.h>
#endif
#include <HCDI/hcdi.h>
#define MCDS_ZERO 1.E-30

#ifdef __cplusplus
extern "C" {
#endif

#define my_free(ptr) if(ptr!=NULL) { free(ptr); ptr=NULL; }

HC_DATA_DLL_API void *my_malloc(int nelem, size_t size);
HC_DATA_DLL_API void *my_realloc(void *ptr, size_t oldnelem, size_t newnelem, size_t size);

void *my_memcpy(void *dest, const void *src, size_t size);

char *my_strcpy(char *dest,const char *src);

int my_strcmp(const char *str1,const char *str2);

int   my_dicho(const void *elem_tab,size_t nb_elems,size_t width,const void *elem_p,
	       int (*cmp_func)(const void *, const void *));

int   my_dicho4insert(const void *elem_tab,size_t nb_elems,size_t width,const void *elem_p,
		      int (*cmp_func)(const void *, const void *),int *is_found_p);



/** sort of an array of objects with 2 states.<br>
    The algorithm needs n calls of a function returning the state of an object,<br>
    compared to n * log(n) calls of the comparison function of a qsort!<br>
    Example (with successive calls, see doc of returned value):<br>
    nlower = my_sort_2states (array, nmemb, sizeof (array_t), get_state1);<br>
    if (nlower > 1) my_sort_2states (array, nlower, sizeof (array_t), get_state2_of_lower);<br>
    if (nlower < nmemb - 1) my_sort_2states (array + nlower, nmemb - nlower, sizeof (array_t), get_state2_of_upper);

    @param base points to the element at the base of the array, like for qsort (input)
    @param nmemb number of elements in the array, like for qsort (input)
    @param size the size of each element in bytes, like for qsort (input)
    @param getstate name of the function, which is called with an argument that points to an element of the array (input)<br>
           This function has to return:<br>
	   0 for elements to put in the lower part of the array,<br>
	   non 0 for elements to put in the upper part of the array.
    @return the number of elements in the lower part of the array.<br>
    This means that successive sorts (my_sort_2states, qsort, ...) of the two parts of the array can be called as shown in the example above.
 */
size_t my_sort_2states (void *base, size_t nmemb, size_t size, int (*getstate)(const void *));

#ifdef __cplusplus
}
#endif

#endif /* UTILS_H */




