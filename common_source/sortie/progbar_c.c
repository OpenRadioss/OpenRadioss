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
#include "hardware.inc"
#include "c_header.inc"

/*Cvf50c1 Nouvelle routine*/
/*Routine pour ecrire une barre de progression sur la sortie standard */
#include <stdio.h>

void progbar_c(int *icur, int *imax)
{
    int i, j ;
    char line[62], format[40], outline[80] ;
    double percent ;
    
    percent=(double)(*icur) / *imax * 100 ;
    j=*icur * 60 / *imax ;
    line[0]=' ' ;
    line[1]='*' ;
    for (i=0;i<j;i++) {line[i+2]='=';}
    line[i+2]='\0' ;
    
    sprintf(format, "%%-%ds %%5.1f%%%%", 62 );
    sprintf(outline,format,line,percent) ;
    fprintf(stdout,"\r%s",outline) ;
    if (*icur == *imax) {fprintf(stdout,"\n") ;}
    fflush(stdout) ;
}

void _FCALL PROGBAR_C(int *icur, int *imax)
{
    progbar_c(icur, imax) ;
}
void progbar_c_(int *icur, int *imax)
{
    progbar_c(icur, imax) ;
}
void progbar_c__(int *icur, int *imax)
{
    progbar_c(icur, imax) ;
}
    
