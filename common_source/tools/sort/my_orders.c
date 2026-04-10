//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2026 Altair Engineering Inc.
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define _FCALL
#define SMALL_ARRAY_THRESHOLD 1000

/* External C++ stable sort from cppsort.cpp */
extern void stlstable_sort_int_int(int *len, int *keys, int *values);

void tri_direct(unsigned *data,unsigned *iwork, unsigned *index,int n,int irecl,unsigned *inds)
{
    /* tri a adressage direct                               */
    /*  0 <= x <= 2^16 - 1                                  */

    unsigned range=65535,mask=0xffff;
    int i,j,k;

    for(k=irecl-1;k >= 0;k--){

        /* poid faible (16bits)*/
        for(i = 0 ;i < range+2 ; i++) iwork[i] = 0 ;
        for(i = 0 ;i < n ; i++){
            iwork[(data[irecl*index[i]+k] & mask) + 1] += 1 ;
        }
        for(i = 0 ;i < range ; i++) iwork[i+1] += iwork[i];
        for(i = 0 ;i < n ; i++){
            j = data[irecl*index[i]+k] & mask;
            inds[iwork[j]] = index[i] ;
            iwork[j] += 1;
        }

        /* poid fort (16bits)*/
        for(i = 0 ;i < range+2 ; i++) iwork[i] = 0 ;
        for(i = 0 ;i < n ; i++) {
            iwork[((data[irecl*inds[i]+k] >> 16) & mask) + 1] += 1 ;
        }
        for(i = 0 ;i < range ; i++) iwork[i+1] += iwork[i];
        for(i = 0 ;i < n ; i++){
            j = (data[irecl*inds[i]+k] >> 16) & mask;
            index[iwork[j]] = inds[i] ;
            iwork[j] += 1;
        }
    }

}
/*  my_orders_ -- Sort unsigned integer keys and return a Fortran permutation.
 *
 *  Arguments
 *  ---------
 *  mode   [in/out] Sorting mode:
 *           0  : fresh sort – initialise index to identity, sort, return
 *                1-based Fortran indices.
 *          10  : re-sort   – index already contains a valid 1-based Fortran
 *                permutation (e.g. from a previous call); convert to 0-based,
 *                stable-sort on the new keys, convert back to 1-based.
 *                Useful for multi-key sorting (sort by secondary key first
 *                with mode=0, then by primary key with mode=10).
 *          other: sets *mode = -1 (error).
 *
 *  iwork  [scratch] Work array of at least 65538 unsigned ints
 *                    (used as a counting / histogram buffer by the radix sort).
 *
 *  data   [in]  Array of unsigned keys to sort.
 *               Layout: data[irecl * i + k]  is key-field k of element i.
 *               Total size: n * irecl  unsigned ints.
 *
 *  index  [in/out] Permutation array.  Must be at least 2*n unsigned ints:
 *               - index[0..n-1]   : the permutation (input for mode=10,
 *                                   output for both modes), returned as
 *                                   1-based Fortran indices.
 *               - index[n..2n-1]  : scratch space used internally by
 *                                   tri_direct.
 *
 *  n      [in]  Number of elements to sort.
 *
 *  irecl  [in]  Number of 32-bit key fields per element ("record length").
 *               The sort is lexicographic: field irecl-1 is most significant,
 *               field 0 is least significant.  Typically irecl=1 when sorting
 *               a simple integer array.
 */
void my_orders_(int *mode,unsigned *iwork,unsigned *data,unsigned *index,int *n,int *irecl)
{
    int i;
    if(*mode == 0){
        if(*irecl == 1 && *n < SMALL_ARRAY_THRESHOLD){
            /* For small single-key arrays, std::stable_sort is faster
               (better cache behaviour, lower constant overhead). */
            int *keys   = (int*)iwork;       /* reuse iwork as temp keys  */
            int *values = (int*)(index + *n); /* reuse scratch half of index */
            memcpy(keys, data, (*n) * sizeof(int));
            for(i = 0 ;i < *n ; i++) values[i] = i + 1 ; /* 1-based index */
            stlstable_sort_int_int(n, keys, values);
            memcpy(index, values, (*n) * sizeof(int)); /* copy result back */
        } else {
            for(i = 0 ;i < *n ; i++) index[i] = i ;
            tri_direct(data,iwork,index,*n,*irecl,index+*n);
            for(i = 0 ;i < *n ; i++) index[i] += 1 ;/* c => fortran */
        }
    } else if(*mode == 10){
        if(*irecl == 1 && *n < SMALL_ARRAY_THRESHOLD){
            /* Small re-sort: build (key, existing_index) pairs and stable-sort */
            int *keys   = (int*)iwork;
            int *values = (int*)(index + *n);
            for(i = 0 ;i < *n ; i++){
                values[i] = (int)index[i];                  /* keep 1-based */
                keys[i]   = (int)data[index[i] - 1];       /* 0-based lookup */
            }
            stlstable_sort_int_int(n, keys, values);
            memcpy(index, values, (*n) * sizeof(int));
        } else {
            for(i = 0 ;i < *n ; i++) index[i] -= 1 ;
            tri_direct(data,iwork,index,*n,*irecl,index+*n);
            for(i = 0 ;i < *n ; i++) index[i] += 1 ;/* c => fortran */
        }
    } else {
        *mode = -1;
    }
}
void _FCALL MY_ORDERS(int *mode,int *iwork,int *data,int *index,int *n,int *irecl)
{my_orders_(mode,(unsigned*)iwork,(unsigned*)data,(unsigned*)index,n,irecl);}

void my_orders(int *mode,int *iwork,int *data,int *index,int *n,int *irecl)
{my_orders_(mode,(unsigned*)iwork,(unsigned*)data,(unsigned*)index,n,irecl);}

void my_orders__(int *mode,int *iwork,int *data,int *index,int *n,int *irecl)
{my_orders_(mode,(unsigned*)iwork,(unsigned*)data,(unsigned*)index,n,irecl);}

