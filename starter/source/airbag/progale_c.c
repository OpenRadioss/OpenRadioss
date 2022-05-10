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

void progale_c(int *icur, int *imax, int *ityp)
{
    char outline[80] ;
    double percent ;

    percent=(double)(*icur) / *imax * 100 ;
    if (*ityp == 1) {
       sprintf(outline,"%s%5.1f%s","        BUILDING POLYGONS: ",percent,"%") ;
       fprintf(stdout,"\r%s",outline) ;
       if (*icur == *imax) {fprintf(stdout,"\r%s\n","        BUILDING POLYGONS - COMPLETE          ") ;}
                    }
    else if (*ityp == 2) {
       sprintf(outline,"%s%5.1f%s","        BUILDING POLYHEDRA: ",percent,"%") ;
       fprintf(stdout,"\r%s",outline) ;
       if (*icur == *imax) {fprintf(stdout,"\r%s\n","        BUILDING POLYHEDRA - COMPLETE          ") ;}
                        }
    else if (*ityp == 3) {
       sprintf(outline,"%s%5.1f%s","        MERGING COINCIDENT NODES FOR ANIM: ",percent,"%") ;
       fprintf(stdout,"\r%s",outline) ;
       if (*icur == *imax) {fprintf(stdout,"\r%s\n","        MERGING COINCIDENT NODES FOR ANIM - COMPLETE          ") ;}
                        }
    else if (*ityp == 4) {
       sprintf(outline,"%s%5.1f%s","        BUILDING ELEMENT CONNECTIVITY: ",percent,"%") ;
       fprintf(stdout,"\r%s",outline) ;
       if (*icur == *imax) {fprintf(stdout,"\r%s\n","        BUILDING ELEMENT CONNECTIVITY - COMPLETE          ") ;}
                        }
    fflush(stdout) ;
}

void _FCALL PROGALE_C(int *icur, int *imax, int *ityp)
{
    progale_c(icur, imax, ityp) ;
}
void progale_c_(int *icur, int *imax, int *ityp)
{
    progale_c(icur, imax, ityp) ;
}
void progale_c__(int *icur, int *imax, int *ityp)
{
    progale_c(icur, imax, ityp) ;
}
    
