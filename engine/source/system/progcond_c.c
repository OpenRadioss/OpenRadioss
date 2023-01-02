//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2023 Altair Engineering Inc.
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

/*Cvf51e6 Nouvelle routine*/
/*Routine pour afficher la progression de la condensation sur la sortie standard */
#include <stdio.h>

void progcond_c(int *icur, int *imax, int *iproc, int *ilvl, int *id, int *istep)
{
    char outline[80];
    double percent ;
    
    percent=(double)(*icur) / *imax * 100 ;
    if (*istep == 0) {sprintf(outline,"%s%4d%s%4d%s%4d%s%5.1f%s","   SUBGRAPH: ",*iproc," LEVEL: ",*ilvl," SUP-ELEM: ",*id," - ",percent,"%               ") ;}
    else if (*istep == 1) {sprintf(outline,"%s%4d%s%4d%s%4d%s%5.1f%s","   SUBGRAPH: ",*iproc," LEVEL: ",*ilvl," SUP-ELEM: ",*id," - ",percent,"% - EXTRACTION  ") ;}
    else if (*istep == 2) {sprintf(outline,"%s%4d%s%4d%s%4d%s%5.1f%s","   SUBGRAPH: ",*iproc," LEVEL: ",*ilvl," SUP-ELEM: ",*id," - ",percent,"% - RESOLUTION  ") ;}
    else if (*istep == 3) {sprintf(outline,"%s%4d%s%4d%s%4d%s%5.1f%s","   SUBGRAPH: ",*iproc," LEVEL: ",*ilvl," SUP-ELEM: ",*id," - ",percent,"% - CONDENSATION") ;}
    fprintf(stdout,"\r%s",outline) ;
    if (*icur == *imax && (*istep == 0 || *istep == 3)) {fprintf(stdout,"\r%s%4d%s\n","   SUBGRAPH: ",*iproc," - COMPLETE                                        ") ;}
    fflush(stdout) ;
}

void _FCALL PROGCOND_C(int *icur, int *imax, int *iproc, int *ilvl, int *id, int *istep)
{
    progcond_c(icur, imax, iproc, ilvl, id, istep) ;
}
void progcond_c_(int *icur, int *imax, int *iproc, int *ilvl, int *id, int *istep)
{
    progcond_c(icur, imax, iproc, ilvl, id, istep) ;
}
void progcond_c__(int *icur, int *imax, int *iproc, int *ilvl, int *id, int *istep)
{
    progcond_c(icur, imax, iproc, ilvl, id, istep) ;
}

/*Cvf51e6 Nouvelle routine*/
/*Routine pour afficher la progression de la condensation sur la sortie standard en SPMD*/

void progcondp_c(int *icur, int *imax, int *iproc, int *id)
{
     char outline[10];
     double percent ;
     char ctab[80] ;
     int i,j ;
     
     percent=(double)(*icur) / *imax * 100 ;
     ctab[0]='\r' ;
     for (i=1;i<(*iproc);i++) {j=2*(i-1)+1 ;
                             ctab[j]='\t' ;
                             j=2*i ;
                             ctab[j]='\t';}
     j=2 * (*iproc - 1) + 1 ;
     ctab[j]='%' ;
     j+=1 ;
     ctab[j]='s' ;
     j+=1 ;
     ctab[j]='\0' ;
     sprintf(outline,"%s%2d%s%3.0f%s","P ",*iproc," ",percent,"% ") ;
     fprintf(stdout,ctab,outline) ;
     fflush(stdout) ;
}

void _FCALL PROGCONDP_C(int *icur, int *imax, int *iproc, int *id)
{
    progcondp_c(icur, imax, iproc, id) ;
}
void progcondp_c_(int *icur, int *imax, int *iproc, int *id)
{
    progcondp_c(icur, imax, iproc, id) ;
}
void progcondp_c__(int *icur, int *imax, int *iproc, int *id)
{
    progcondp_c(icur, imax, iproc, id) ;
}
