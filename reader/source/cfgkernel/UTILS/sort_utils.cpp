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
#include "mv_cstdio.h"
#include "mv_cstring.h"
#include "mv_cstdlib.h"      
#include "mv_cmath.h" /*SB/40/22/09/2005*/
#include "mv_stl_various.h"  
#include "memory_utils.h"
#include "sort_utils.h"



#define EPSILON 1.E-09  /*SB/5/04/07/2005*/


extern "C" int mysort_int(int *array,int size_array) {
  if (array == NULL || size_array <= 0)
    return -1 ;

  sort(array,array+size_array);
  return 0;
}

extern "C" int myunique_int(int *array,int size_array) {
	
	return (int)(unique(array,array+size_array) - array);
}

extern "C" int mysort_and_unique_int(int *array,int size_array) {
  mysort_int(array,size_array);
  return myunique_int(array,size_array);
}

extern "C" int myunique_and_resize_int(int **array,int *size_array) {
   int new_size_tempo= 0;
   if(array[0]==NULL) return 0;
   if(*size_array==0) return 0;
   if(mysort_int(array[0],*size_array)==-1) return 1;
   new_size_tempo=myunique_int(array[0],*size_array);
   if(new_size_tempo!=*size_array)
   {
       if(new_size_tempo == 0)
       {
	   myfree(array[0]);
	   array[0] = NULL;
	   *size_array = 0;
       }
       else
       {
	   array[0] = (int *) myrealloc(array[0], (size_t) (new_size_tempo)*sizeof(int));
	   if(array[0]==NULL)
	   {
#ifdef DEBUG
	       printf("Memory allocation problem in utility_kill_double_integer_and_resize");
#endif 
	       return 0;
	   }
	   *size_array = new_size_tempo ;			   
       }
   }
   return 1;
}

extern "C" int mydicho_int(int *nod, int low, int high, int number) {
   int middle ;

   if (low == high)
   {
      if (nod[low] == number)
      {
         return(low) ;
      }
      else
      {
         return(-1) ;
      }
   }
   if (high - low <= 1)
   {
      if (nod[low] == number)
      {
         return(low) ;
      }
      else if (nod[high] == number)
      {
         return(high) ;
      }
      else
      {
         return(-1) ;
      }
   }
/*   if(nod[high] < number)
   {
      return(-1) ;
   }
*/
   if(nod[low] > number)
   {
      return(-1) ;
   }

   middle = (low + high)/2 ;
   if (nod[middle] < number)
   {
      return mydicho_int(nod, middle, high, number) ;
   }
   else if (nod[middle] > number)
   {
      return mydicho_int(nod,low,middle, number) ;
   }
   else if (nod[middle] == number)
   {
      return(middle) ;
   }
   else
   {
      return(-1) ;
   }  
}

extern "C" int mydicho_int_multi(int *array,int size_array, 
				 int index_to_found,int *number_index_to_found)
{
    int low = 0;
    int high = 0;
    int indice =0;
    int I_found_deb = 0;
    int index_deb_temp = -1;
    int nb_deb_temp = 0;
    int I_found_end = 0;
    int nb_end_temp = 0;
    int index_out = -1;

    if(array == NULL) return -1;
    if(size_array < 1) return -1;
    high = size_array - 1;
    indice = mydicho_int(array, low, high,index_to_found);
    if(indice < 0) return -1;
    index_deb_temp = indice;
    nb_deb_temp = 1;
    index_out = indice;
    indice--;
    while( I_found_deb == 0)
    {
	if(indice < 0 )
	{ 
	    I_found_deb = 1;
	    break;
	}
	if(array[indice] == index_to_found)
	{
	    nb_deb_temp++;
	    index_out = indice;
	    indice--;
	}
	else
	{
	    break;
	}
    }
    if(number_index_to_found != NULL)
    {
	index_deb_temp++;
	while( I_found_end == 0)
	{
	    if(index_deb_temp > high )
	    { 
		I_found_end = 1;
		break;
	    }
	    if(array[index_deb_temp] == index_to_found)
	    {
		nb_end_temp++;
		index_deb_temp++;
	    }
	    else
	    {
		break;
	    }
	}
	*number_index_to_found = nb_end_temp + nb_deb_temp;
    }
    return index_out;
}

extern "C" int mycmp_int(int *n1,int *n2) {
  return (n1[0]-n2[0]);
}

extern "C" int mycpy_int(int size_index1,int *index1,int *size_index2,int **index2,int if_resize) {
  int i;
  if(size_index1 == 0) return 0;
  if(*size_index2 ==0)
    {
      index2[0] = (int *) mymalloc((size_t )(size_index1*sizeof(int))) ;
    }
  else
    {
      index2[0] = (int *) myrealloc(index2[0], (size_t) (*size_index2 + size_index1)*sizeof(int));
      
    }
  if(index2[0] ==NULL)
  {
    //print_msg(RIP,util_msg(2,0));
    return -1;
  }
  for (i=0; i < size_index1 ; i++)
    {
      *(index2[0] + *size_index2 + i) = *(index1 + i);
    }
  
  *size_index2 =  *size_index2  + size_index1;
  if(if_resize == 1)
    {
      if(myunique_and_resize_int(index2,size_index2)==0) return(1);
    }
  else if(if_resize == 0)
    {
      if(mysort_int(index2[0],*size_index2)==-1) return 0;
      *size_index2 = myunique_int(index2[0],*size_index2);
    }
  
  return(0);
}

extern "C" int mykill_int_on(int **index,int *nb_in,int *index_ref,int nb_ref) {
    int i;
    int k_deb_ref =0;

    if(nb_ref <1) return 0;
    if(nb_in==NULL)return 0;
    if(*nb_in<1)return 0;
    mysort_int(index_ref,nb_ref);
    mysort_int(index[0],*nb_in);
    k_deb_ref =*nb_in;
    for(i=0;i<nb_ref;i++)
    {
	int indice =0;
	int nb_indice = 0;

	if(*nb_in<1) break;
        if(*(index_ref+i) < index[0][0]) continue;
        if(*(index_ref+i) > index[0][*nb_in - 1]) break;
	
	indice = mydicho_int_multi(index[0],*nb_in,*(index_ref+i),&nb_indice);
	
        if(indice == -1) continue;
	if(indice + nb_indice != *nb_in )
	{
	    memmove(index[0]+indice,index[0]+indice+nb_indice,(*nb_in-indice-nb_indice)*sizeof(int));
	}
        *nb_in = *nb_in - nb_indice;		  		 
	if(*nb_in<1) break;
    } 
    if(*nb_in > 0)
    {
	if(k_deb_ref != *nb_in)
	{
	    index[0] = (int *)myrealloc(index[0], (*nb_in) * sizeof(int));
	    if(index[0] ==NULL) return -1;
	}
    }
    else
    {
	myfree(index[0]);
	index[0] = NULL;
	*nb_in = 0;
    }
    return 0;
}


extern "C" int mycmp_ptr(void **n1,void **n2) {
  return (*n1)<(*n2) ? -1 : (*n1)==(*n2) ? 0 : 1;
}



extern "C" int mysort_ptr(void **array,int size_array) {
  if(array==NULL) return -1;
  if(size_array<1) return -1;

#if (!defined SUN) 
  qsort(array,size_array,sizeof(void *),(int (*)(const void *,const void *))mycmp_ptr);
#else 
  std::qsort(array,size_array,sizeof(void *),(int (*)(const void *,const void *))mycmp_ptr);
#endif

  return 0;
}



extern "C" int mydicho_ptr(void **nod, int low, int high, void *number) {
   int middle ;

   if(high<low) return -1;
   
   if (low == high)
   {
      if (nod[low] == number)
      {
         return(low) ;
      }
      else
      {
         return(-1) ;
      }
   }
   if (high - low <= 1)
   {
      if (nod[low] == number)
      {
         return(low) ;
      }
      else if (nod[high] == number)
      {
         return(high) ;
      }
      else
      {
         return(-1) ;
      }
   }
/*   if(nod[high] < number)
   {
      return(-1) ;
   }
*/
   if(nod[low] > number)
   {
      return(-1) ;
   }

   middle = (low + high)/2 ;
   if (nod[middle] < number)
   {
      return mydicho_ptr(nod, middle, high, number) ;
   }
   else if (nod[middle] > number)
   {
      return mydicho_ptr(nod,low,middle, number) ;
   }
   else if (nod[middle] == number)
   {
      return(middle) ;
   }
   else
   {
      return(-1) ;
   }
}



extern "C" int myunique_ptr(void **ptr_tab,int nb_ptrs) {
  void **a_it_end=unique(ptr_tab,ptr_tab+nb_ptrs);
  
  return (int)(a_it_end-ptr_tab);
}



extern "C" int mysort_and_unique_ptr(void **array,int size_array) {
  mysort_ptr(array,size_array);
  return myunique_ptr(array,size_array);
}


/*SB/5/04/07/2005*/
extern "C" int mycmp_double(double *d1, double *d2)
{
   double diff = 0. ;

   if ((fabs(d1[0])>EPSILON) || (fabs(d2[0])>EPSILON))
   {
      diff = (d1[0] - d2[0])/(d1[0]+d2[0]) ;
   }
   else
   {
      diff = (d1[0] - d2[0]) ;
   }
   if (fabs(diff) > EPSILON)
   {
      if (d1[0]>d2[0]) return 1 ;
      if (d1[0]<d2[0]) return -1 ;
   }
   return 0 ;
}
/*SB/5/04/07/2005/END*/



