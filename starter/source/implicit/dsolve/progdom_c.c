//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2022 Altair Engineering Inc.
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
#include "hardware.inc"
#define _FCALL
/*Cvf51e6 Nouvelle routine*/
/*Routine pour afficher la progression de la domdec sur la sortie standard */
#include <stdio.h>

void progdom_c(int *icur, int *imax, int *iproc, int *ilvl, int *imach)
{
    char outline[80] ;
    double percent ;

    percent=(double)(*icur) / *imax * 100 ;
    if (*imach == 3) {
       sprintf(outline,"%s%4d%s%4d%s%5.1f%s","    --> DOMAIN DECOMPOSITION - PROC: ",*iproc," LEVEL: ",*ilvl," - ",percent,"%") ;
       fprintf(stdout,"\r%s",outline) ;
       if (*icur == *imax) {fprintf(stdout,"\r%s%4d%s\n","    --> DOMAIN DECOMPOSITION - PROC: ",*iproc," - COMPLETE          ") ;}
                      }
    else {
       sprintf(outline,"%s%4d%s%4d%s%5.1f%s","    --> DOMAIN DECOMPOSITION - SUBGRAPH: ",*iproc," LEVEL: ",*ilvl," - ",percent,"%") ;
       fprintf(stdout,"\r%s",outline) ;
       if (*icur == *imax) {fprintf(stdout,"\r%s%4d%s\n","    --> DOMAIN DECOMPOSITION - SUBGRAPH: ",*iproc," - COMPLETE          ") ;}
          }
    fflush(stdout) ;
}

void _FCALL PROGDOM_C(int *icur, int *imax, int *iproc, int *ilvl, int *imach)
{
    progdom_c(icur, imax, iproc, ilvl, imach) ;
}
void progdom_c_(int *icur, int *imax, int *iproc, int *ilvl, int *imach)
{
    progdom_c(icur, imax, iproc, ilvl, imach) ;
}
void progdom_c__(int *icur, int *imax, int *iproc, int *ilvl, int *imach)
{
    progdom_c(icur, imax, iproc, ilvl, imach) ;
}
    
