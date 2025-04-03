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
#include <string.h> 
#include <stdio.h>
#include <stdlib.h>
#if defined _WIN32 || defined WIN32
#include <malloc.h>
#else
#include <unistd.h>
#endif

#if defined _WIN32 || defined WIN32
#include <process.h>

#include <windows.h>
#pragma message("use psapi.h : must add psapi.lib in the project settings!\n")

#else
#include <unistd.h>
#endif



#include "general_memory.h"


static void *general_memory_user_data = NULL; 

/* Memory info is exported in csv format. To view it in a spreadsheet, do sth like this:
* grep own_ log.txt > hc_meminfo.csv
* and import this file in Excel or a similar software.
*/
static int nb_mem_message = 0;

static void general_memory_stop (void)
{
}


static void
general_memory_allocation_failed (size_t size, int line, const char *filename, void *user_data)
{
    int back = 2;
    if ( back == 2 )
        general_memory_stop();
}

static void (*general_memory_allocation_failed_function) (size_t size, int line,const char *filename, void *user_data)
    = general_memory_allocation_failed;

/* Returns the "MCrash" part of the path, i.e. the part starting at the next-to-last separator (CS#660#20_01_06 beg) */
static const char *get_mcrash_filename (const char *path)
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


void general_memory_allocation_failed_function_set (void (*function) (size_t size, int line, const char *filename, void *user_data),
                                                    void *user_data)
{
    general_memory_allocation_failed_function = function;
    general_memory_user_data = user_data;
}



static void write_meminfo_header(void)
{
    if(0 < nb_mem_message) return; /* only write first time */
    printf("No, own_fct, ptr, size (bytes), variable name, line /filename, old ptr, remark");
#ifdef PRINT_SIZE
#if defined _WIN32 || defined WIN32
    printf (", Pagefile [KB], Pagefile peak, WorkSet [KB], WorkSet Peak\n");
#else
    printf (", proc-size\n");
#endif
#else
    printf ("\n");
#endif
}

void *own_malloc_with_info (size_t size, int line, char *filename)
{
    void *ptr;

    ptr = own_malloc_ (size,
                       line, filename);

#ifdef  MALLOC_INFO
    write_meminfo_header(); /* writes column headers if first time */
    printf ("%d, own_malloc, 0x%8x, %d,, line%5d / %s,,",
        nb_mem_message, (unsigned) ptr, (int)size, line, get_mcrash_filename (filename));
#ifdef PRINT_SIZE
#else
    printf (".\n");
#endif

    nb_mem_message++;
#endif //  MALLOC_INFO
    return ptr;
}

void *own_malloc_ (size_t size,
                   int line, const char *filename) 
{
    void *tmp;
    
/*    char tampon[160] = "";*/
    
/*    sprintf(tampon,"No more memory available! Yes: Try again, No: Stop %s ?",PROGRAM_NAME);*/

    if (size == 0)
	return NULL;


    tmp = malloc(size);
  
    while(tmp == NULL)
    {
        
        
        general_memory_allocation_failed_function (size, line, get_mcrash_filename (filename), general_memory_user_data);

	tmp = malloc(size);
    }

    return tmp;
}

void *own_realloc_with_info (void *block, size_t size, int line, const char *filename, const char *varname)
{
    void *ptr;

    ptr = own_realloc_ (block,size,
                        line, filename);

#ifdef  MALLOC_INFO
    write_meminfo_header(); /* writes column headers if first time */
    if (block == NULL)
    {
        printf ("%d, own_realloc, 0x%8x, %d, %s, line%5d / %s, 0x0, realloc of NULL",
            nb_mem_message, (unsigned) ptr, (int)size, varname, line, get_mcrash_filename (filename));
    }
    else if (ptr == NULL)
    {
        printf ("%d, own_realloc, 0x0, %d, %s, line%5d / %s, 0x%8x, freed (realloc 0)",
            nb_mem_message, (int)size, varname, line, get_mcrash_filename (filename), block);
    }
    else if (block == ptr)
    {
        printf ("%d, own_realloc, 0x%8x, %d, %s, line%5d / %s, 0x%8x, realloc not moved",
            nb_mem_message, (unsigned) ptr, (int)size, varname, line, get_mcrash_filename (filename), block);
    }
    else /* block != ptr */
    {
        printf ("%d, own_realloc, 0x%8x, %d, %s, line%5d / %s, 0x%8x, realloc moved",
            nb_mem_message, (unsigned) ptr, (int)size, varname, line, get_mcrash_filename (filename), block);
    }

#ifdef PRINT_SIZE
#else
    printf (".\n");
#endif

    nb_mem_message++;
#endif
    return ptr;
}

void *own_realloc_ (void *block, size_t size,
                    int line,const char *filename) 
{
    void *tmp = NULL;
/*    char tampon[160] = "";*/
/*    sprintf(tampon,"No more memory available! Yes: Try again, No: Stop %s ?",PROGRAM_NAME);*/

    if (size == 0)
    {
	own_free (block);
	return NULL;
    }


    if(block == NULL)
	return own_malloc(size);
    else
    {
	tmp = realloc(block, size);
  
	while(tmp == NULL)
	{
            
            
            general_memory_allocation_failed_function (size, line, get_mcrash_filename (filename), general_memory_user_data);

	    tmp = realloc(block, size);
	}

	return tmp;
    }
}

void *own_calloc_with_info (size_t nitems, size_t size, int line, const char *filename)
{
    void *ptr;

    ptr = own_calloc_ (nitems,size,
                       line, filename); 

#ifdef  MALLOC_INFO
    write_meminfo_header(); /* writes column headers if first time */
    printf ("%d, own_calloc, 0x%8x, %d,, line%5d / %s,,",
        nb_mem_message, (unsigned) ptr, (int)(nitems*size), line, get_mcrash_filename (filename));

#ifdef PRINT_SIZE
#else
    printf (".\n");
#endif

    nb_mem_message++;
#endif
    return ptr;
}

void *own_calloc_( size_t nitems, size_t size,
                   int line, const char *filename) 
{
    void *tmp = NULL;
/*    char tampon[160] = "";*/
/*    sprintf(tampon,"No more memory available! Yes: Try again, No: Stop %s ?",PROGRAM_NAME);*/

    if ((size == 0) || (nitems == 0))
	return NULL;

  
    tmp = calloc(nitems, size);
  
    while(tmp == NULL)
    {
        
        
        general_memory_allocation_failed_function (nitems*size, line, get_mcrash_filename (filename), general_memory_user_data);

	tmp = calloc(nitems, size);
    }

    return tmp;
}


void own_free_(void *block)
{
    if( block == NULL)
	return;
  
    free(block);
}
void own_free_with_info(void *block, int line, const char *filename, const char *varname)
{
    /* size_t size; */

    if( block == NULL)
    {
	return;
    }
#ifdef  MALLOC_INFO
    /* size = sizeof(block); this does not work */
    printf ("%d, own_free,,, %s, line%5d / %s, 0x%8x,\n",
        nb_mem_message, varname, line, get_mcrash_filename (filename), block);
#endif
    free(block);

    nb_mem_message++;

    return;
}
void *own_recalloc_(void *block, size_t nitems, size_t size)
{
    void *array ;
 

    if ((size == 0) || (nitems == 0))
    {
	own_free (block);
	return NULL;
    }

    array = own_calloc(nitems, size) ;

    /*
      if (array == NULL)
      {
      printf("Not enough memory left...\n") ;
      return (array) ;
      }
    */

    if (sizeof(block) <= sizeof(array)) /* does this really work? */
    {
	array = memcpy(array, block, sizeof(block)) ;
    }
    else
    {
	array = memcpy(array, block, sizeof(array)) ;
    }

    own_free(block) ;

    return (array) ;
}


static unsigned long general_memory_getpid(void) {
#if defined _WIN32 || defined WIN32
  return (unsigned long)_getpid();
#else
  return (unsigned long)getpid();
#endif
}

void
Print_Size (void)
{
    printf ("proc: ");

#if defined _WIN32 || defined WIN32
    printf ("Pagefile, Pagefile peak, WorkSet, WorkSet peak [KB]: ");
#endif
}

extern void
own_freenull(void **block)
{
  if (block[0] != NULL)
  {
     own_free(block[0]) ;
     block[0] = NULL ;
  }
  return ;
}
