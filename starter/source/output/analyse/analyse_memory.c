//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2024 Altair Engineering Inc.
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
/* gw45a1 
#include <malloc.h>
#include <stdio.h>
#include <string.h>
*//* ls41k17+1 *//*
#include <stdlib.h> */ /* for tequila's malloc *//*
*/
#include <stdlib.h> 
#if CPP_mach != CPP_macosx64
#include <malloc.h>
#endif
#include <stdio.h>
#include <string.h>

#include "analyse_memory.h"

void *analyse_malloc(size_t size)
{
  if (size == 0)
    return NULL;
  
  return malloc(size);
}


void *analyse_realloc(void *block, size_t size)
{
  if(block == NULL)
    return analyse_malloc(size);
  else
    return realloc(block, size);
}


void *analyse_calloc( size_t nitems, size_t size)
{
  return calloc(nitems, size);
}


void analyse_free(void *block)
{
  if( block == NULL)
    return;
  
  free(block);

}
void *analyse_recalloc(void *block, size_t nitems, size_t size)
{
   void *array ;

   array = (void *)calloc(nitems, size) ;

   if (array == NULL)
   {
      printf("Not enough memory left...\n") ;
      return (array) ;
   }

   if (sizeof(block) <= sizeof(array))
   {
      array = memcpy(array, block, sizeof(block)) ;
   }
   else
   {
      array = memcpy(array, block, sizeof(array)) ;
      
   }

   free(block) ;

   return (array) ;
}
