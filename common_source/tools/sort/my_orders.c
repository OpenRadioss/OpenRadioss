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
#include <stdio.h>
#include <stdlib.h>

#define _FCALL

void tri_direct(data,iwork,index,n,irecl,inds)
int n,irecl;
unsigned data[],index[],inds[],iwork[];
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
void my_orders_(mode,iwork,data,index,n,irecl)
int *mode,*n,*irecl;
unsigned *iwork,*data,*index;
{
    int i;
    if(*mode == 0){
        for(i = 0 ;i < *n ; i++) index[i] = i ;
        tri_direct(data,iwork,index,*n,*irecl,index+*n);
        for(i = 0 ;i < *n ; i++) index[i] += 1 ;/* c => fortran */
    } else if(*mode == 10){
        for(i = 0 ;i < *n ; i++) index[i] -= 1 ;
        tri_direct(data,iwork,index,*n,*irecl,index+*n);
        for(i = 0 ;i < *n ; i++) index[i] += 1 ;/* c => fortran */
    } else {
        *mode = -1;
    }
}
void _FCALL MY_ORDERS(mode,iwork,data,index,n,irecl)
int *mode,*iwork,*data,*index,*n,*irecl;
{my_orders_(mode,iwork,data,index,n,irecl);}

void my_orders(mode,iwork,data,index,n,irecl)
int *mode,*iwork,*data,*index,*n,*irecl;
{my_orders_(mode,iwork,data,index,n,irecl);}

void my_orders__(mode,iwork,data,index,n,irecl)
int *mode,*iwork,*data,*index,*n,*irecl;
{my_orders_(mode,iwork,data,index,n,irecl);}

