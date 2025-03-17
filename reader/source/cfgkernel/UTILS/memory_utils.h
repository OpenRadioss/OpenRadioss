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
#ifndef MEMORY_UTILS_H
#define MEMORY_UTILS_H

#include <stdlib.h>
#include <HCDI/hcdi.h>

#ifdef __cplusplus
extern "C" {
#endif

/*#define MV_MALLOC_WITH_INFO */
  
  void *myfree_with_info(void *ptr/*, int aline, char *filename, char* varname*/);
  HC_DATA_DLL_API void *myfree_without_info(void *ptr/*, int aline, char *filename, char* varname*/);

  void *myrealloc_with_info(void *ptr,size_t size /*,int line, char *filename, char* varname*/);
  HC_DATA_DLL_API void *myrealloc_without_info(void *ptr,size_t size /*,int line, char *filename, char* varname*/);

  void *mymalloc_with_info(size_t  size/*, int line, char *filename*/);
  HC_DATA_DLL_API void *mymalloc_without_info(size_t  size/*, int line, char *filename*/);

#ifdef MV_MALLOC_WITH_INFO 
#define myfree(ptr) myfree_with_info(ptr)
/*#define myfree(ptr) myfree_with_info(ptr, __LINE__, __FILE__, #ptr)*/
#define mymalloc(size) mymalloc_with_info (size)
/*#define mymalloc(size) mymalloc_with_info (size, __LINE__, __FILE__)*/
#define myrealloc(ptr, size) myrealloc_with_info (ptr, size)
/*#define myrealloc(ptr, size) myrealloc_with_info (ptr, size, __LINE__, __FILE__, #ptr)*/
#else
#define myfree(ptr) myfree_without_info(ptr)
/*#define myfree(ptr) myfree_without_info(ptr, __LINE__, __FILE__, #ptr)*/
#define mymalloc(size) mymalloc_without_info (size)
/*#define mymalloc(size) mymalloc_without_info (size,  __LINE__, __FILE__)*/
#define myrealloc(ptr, size) myrealloc_without_info (ptr, size)
/*#define myrealloc(ptr, size) myrealloc_without_info (ptr, size , __LINE__, __FILE__, #ptr)*/
#endif

   

 HC_DATA_DLL_API void *mycalloc(size_t nelem,size_t elsize);
  

#ifdef __cplusplus
}
#endif

#include <General/general_memory.h>


#endif /* MEMORY_UTILS_H */




