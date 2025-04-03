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
#ifndef __GENERAL_MEMORY_H__
#define __GENERAL_MEMORY_H__
#include <HCDI/hcdi.h>
#ifdef __cplusplus
extern "C" {
#endif




#ifdef MALLOC_INFO


#define own_malloc(size) own_malloc_with_info (size, __LINE__, __FILE__)
#define own_realloc(block,size) own_realloc_with_info (block, size, __LINE__, __FILE__, #block)
#define own_calloc(nitems,size) own_calloc_with_info (nitems, size, __LINE__, __FILE__)
#define own_free(block) own_free_with_info (block, __LINE__, __FILE__, #block)
#else 

#define own_malloc(size) own_malloc_ (size, __LINE__, __FILE__)
#define own_realloc(block,size) own_realloc_ (block, size, __LINE__, __FILE__)
#define own_calloc(nitems,size) own_calloc_ (nitems, size, __LINE__, __FILE__)

#define own_free own_free_

#endif 



void general_memory_allocation_failed_function_set (void (*function) (size_t size, int line, const char *filename, void *user_data),
                                                    void *user_data);

void *own_malloc_with_info(size_t size, int line, char *filename);
void *own_malloc_( size_t size,
                   int line, const char *filename); 

void *own_realloc_with_info (void *block, size_t size, int line, const char *filename, const char *varname);
void *own_realloc_(void *block, size_t size,
                   int line, const char *filename); 


void *own_calloc_with_info (size_t nitems, size_t size, int line, const char *filename);
void *own_calloc_(size_t nitems, size_t size,
                  int line, const char *filename); 

void own_free_with_info(void *block, int line, const char *filename, const char *varname);
HC_DATA_DLL_API void own_free_(void *block);

void *own_recalloc(void *block, size_t nitems, size_t size) ;

void
Print_Size (void);
extern void
own_freenull(void **block) ;

#ifdef __cplusplus
}
#endif

#endif 
