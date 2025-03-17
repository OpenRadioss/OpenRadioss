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
#include <stdio.h>
#include <string.h>



#include "utils.h"



/* my_malloc  */
void *my_malloc(int nelem, size_t size)
{
   void *array =NULL;


   
   




   if (nelem <= 0)
   {
#ifdef MEMORY_DEBUG
     fprintf(stderr,"Warning: allocation of an array of size less or equal to 0\n") ;
#endif 
     return(NULL) ;
   }

   array = (void *)calloc(nelem, size ) ;


   if (array != NULL)
   {
      return(array) ;
   }
   else
   {
#ifdef MEMORY_DEBUG
      fprintf(stderr,"Impossible to allocate memory\n");
#endif 
      return(NULL) ;
   }
}

void *my_realloc(void *ptr, size_t oldnelem, size_t newnelem, size_t size) {
  void *newptr=NULL;
  int   minnelem=0;
  if(oldnelem == newnelem) { /* Same size => nothing to do (CS#30_06_11) */
      if (0 < oldnelem) return ptr;
      else              return NULL; /* Just in case it is not initialized. */
  }
  newptr=my_malloc((int)(newnelem),size);
  minnelem=(int)((oldnelem<newnelem ? oldnelem : newnelem));
  if(newptr==NULL) {
#ifdef MEMORY_DEBUG
    fprintf(stderr,"Impossible to allocate memory\n");
#endif 
  } else if(minnelem>0) { 
    if (ptr!=NULL)
    {
        memcpy(newptr,ptr,minnelem*size);
    }
  }
  my_free(ptr); /* avant, quand on reallouait a 0, le free n'etait pas fait */
  return newptr;
}

void *my_memcpy(void *dest, const void *src, size_t size)
{
    if(NULL == dest) return dest;

    
    if((0 < size) && (NULL == src))
    {
        memset(dest, 0, size);
        return dest;
    }

    return memcpy(dest, src, size);
}

char *my_strcpy(char *dest,const char *src) {
  int nb = (src==NULL) ? 0 : (int)(strlen(src));
  char *old=dest;
  dest=(char *)my_malloc(nb+1,sizeof(char));
  if(nb) strcpy(dest,src); else *dest='\0';
  my_free(old);
  return dest;
}

int my_strcmp(const char *str1,const char *str2) {
   if (str1 == NULL) str1 = "";
   if (str2 == NULL) str2 = "";
   return strcmp(str1, str2);
}

int my_dicho(const void *elem_tab,size_t nb_elems,size_t width,const void *elem_p,
	     int (*cmp_func)(const void *, const void *))
{
  
  int a_is_found = 0;
  int a_ind      = my_dicho4insert(elem_tab,nb_elems,width,elem_p,cmp_func,&a_is_found);
  return a_is_found ? a_ind : -1;
  
}


int my_dicho4insert(const void *elem_tab,size_t nb_elems,size_t width,const void *elem_p,
		    int (*cmp_func)(const void *, const void *),int *is_found_p)
{
  int         a_result  = -1;
  const char *a_first_p = (const char *)elem_tab;
  const char *a_last_p  = a_first_p+(nb_elems-1)*width;
  int         a_cmp     = 0;
  
  a_cmp=cmp_func(elem_p,a_first_p);
  *is_found_p=(a_cmp==0);
  if(a_cmp<=0) return 0;
  if(nb_elems==1) return 1;

  a_cmp=cmp_func(elem_p,a_last_p);
  *is_found_p=(a_cmp==0);
  if(a_cmp>0)  return (int) nb_elems;
  if(a_cmp==0) return (int)nb_elems-1;

  if(nb_elems==2) {
    a_result=1;
  } else {    
    int         a_ind      = (int)nb_elems/2;
    const char *a_middle_p = a_first_p+a_ind*width;
    if(cmp_func(elem_p,a_middle_p)<0) {
      a_result=my_dicho4insert(a_first_p,a_ind,width,elem_p,cmp_func,is_found_p);
    } else {
      a_result=a_ind+my_dicho4insert(a_middle_p,nb_elems-a_ind,width,elem_p,cmp_func,is_found_p);
    }
  }
  
  return a_result;
}



size_t my_sort_2states (void *base, size_t nmemb, size_t size, int (*getstate) (const void *))
{
    char *tab = (char *) base;
    size_t char_per_size = size / sizeof(char);
    size_t i_lower = 0, i_upper = nmemb - 1;
    char *obj_tmp = NULL; 

    if ((char_per_size * sizeof(char)) != size) return 0; /* copying will not work */

    if ((base == NULL) || (nmemb <= 0) || (size <= 0) || (getstate == NULL)) return 0;
    
    obj_tmp = (char *) my_malloc (1, size); 

    /* !getstate (object_p) ==> object has to be in lower part of array
     * getstate (object_p)  ==> object has to be in upper part of array */

    /* find the lowest object to be put in the upper part */
    while ((i_lower < i_upper) && 
	   !getstate ((void *) (tab + i_lower * char_per_size)))
	i_lower ++;
    /* find the highest object to be put in the lower part */
    while ((i_lower < i_upper) && 
	   getstate ((void *) (tab + i_upper * char_per_size)))
	i_upper --;

    /* as long as there is a couple left to be switched */
    while (i_lower < i_upper)
    {
	/* switch couple */
	memcpy (obj_tmp, tab + i_upper * char_per_size, size);
	memcpy (tab + i_upper * char_per_size, tab + i_lower * char_per_size, size);
	memcpy (tab + i_lower * char_per_size, obj_tmp, size);
	i_lower ++;
	i_upper --;

	/* search next objects to be switched */
	/* find the lowest object to be put in the upper part */
	while ((i_lower < i_upper) && 
	       !getstate ((void *) (tab + i_lower * char_per_size)))
	    i_lower ++;
	/* find the highest object to be put in the lower part */
	while ((i_lower < i_upper) && 
	       getstate ((void *) (tab + i_upper * char_per_size)))
	    i_upper --;
    }

    /* find the right value to return */
    if ((i_lower == i_upper) &&
	!getstate ((void *) (tab + i_lower * char_per_size)))
	i_lower ++;

    my_free(obj_tmp); 

    return i_lower;
}




