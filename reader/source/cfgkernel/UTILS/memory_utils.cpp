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
#if defined _WIN32 || defined WIN32
#include <malloc.h>
#else
#include <unistd.h>
#endif
#include "mv_cstring.h"  /* Call of strlen */
#include "mv_cstdio.h"
#include "memory_utils.h"
#include "error.h"
#include "mv_iostream.h" 

#define L_PATH    512


#define MEMORY_MIN_SIZE_INFO 0

static double memoryAllocSize = 0;

/* Returns last part of the filepath, i.e. the part starting at the next-to-last separator */
static char *get_real_filename (char *path)
{
#if defined WIN32 || defined _WIN32
    static char separator = '\\';
#else
    static char separator = '/';
#endif
    int index;

    /* we do not use strrchr, because we would have to allocate memory! */
    if (NULL == path) return NULL;
    index = (int)strlen (path) - 1;
    if (0 > index) return path;

    while ((0 <= index) && (path[index] != separator)) index--; /* this finds the last separator */
    index--;
    while ((0 <= index) && (path[index] != separator)) index--; /* this finds the next-to-last separator */

    if (0 <= index) {
        return path + index + 1;
    } else {
        return path;
    }
}

extern "C" void	*myfree_with_info (void	*ptr/*, int line, char *filename, char* varname*/)
{
	
#if (!defined SUN) 
  if ( ptr ) ::free ( ptr ) ;
#else 
  if ( ptr ) std::free ( ptr ) ;
#endif

  return ( NULL ) ;
 /* printf ("MEM_INFO ** myfree\tcalled\tby\t%s:L %d :\tFREE of 0x%8x  \n"  ,   
		 get_real_filename (filename), line,(unsigned) ptr);*/
/*	void* newPtr;
	newPtr = myfree_without_info(ptr, line, filename, varname);
	return newPtr;*/
}



extern "C" void	*myfree_without_info (void	*ptr/*, int line, char *filename, char* varname*/) 
{

#if (!defined SUN) 
  if ( ptr ) ::free ( ptr ) ;
#else 
  if ( ptr ) std::free ( ptr ) ;
#endif

  return ( NULL ) ;
}


extern "C" void	*mymalloc_with_info (size_t size/*, int line, char* filename*/)
{
	void* ptr= NULL;
	 ptr = mymalloc_without_info(size/*,
                       line, filename*/);
	 memoryAllocSize+=size;
/*
	 if(size>MEMORY_MIN_SIZE_INFO)
		 printf ("MEM_INFO ** mymalloc\tcalled\tby\t%s:L %d :\tALLOC of 0x%8x:%9d bytes  TOTAL = %f Meg \n",   
		 get_real_filename (filename), line,(unsigned) ptr, size, (memoryAllocSize/(1024*1024)));
	 
*/
	 return (ptr);
}



extern "C" void	*mymalloc_without_info (size_t size) 
{
  void	*ptr = NULL ;
  if ( size <= 0 ) return ( NULL ) ;

#if (!defined SUN) 
  ptr = ::calloc ( size, sizeof (char) ) ;
#else 
  ptr = std::calloc ( size, sizeof (char) ) ;
#endif

  return ( ptr ) ;
}


extern "C" void	*myrealloc_with_info(void *ptr,size_t size/*, int line, char* filename, char* varname*/)
{
    void *newptr=NULL;
    newptr = myrealloc_without_info(ptr, size/*, line, filename, varname*/);
	/*if(size>MEMORY_MIN_SIZE_INFO)
		printf ("MEM_INFO ** myrealloc\tcalled\tby\t%s:L %d :\tREALLOC of 0x%8x:%9d bytes  \n",  
		 get_real_filename (filename), line,(unsigned) ptr, size);*/
    return ( newptr ) ;
}

  


extern "C" void	*myrealloc_without_info(void *ptr,size_t size) 
{
  void	*ptmp ;
  if(!ptr)    return mymalloc(size);
  if(size<=0) return myfree(ptr);

#if (!defined SUN) 
  ptmp = ::realloc ( ptr, size ) ;
#else 
  ptmp = std::realloc ( ptr, size ) ;
#endif

  return ( ptmp ) ;
}



extern "C" void *mycalloc(size_t nelem,size_t elsize) {
	return mymalloc(nelem*elsize);
}
