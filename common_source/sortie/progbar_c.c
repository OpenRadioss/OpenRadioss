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
    
