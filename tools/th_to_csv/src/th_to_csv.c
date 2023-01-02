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

// For linux compilation :
// gcc -o th_to_csv.linux64.exe th_to_csv.c

// To launch conversion :
// th_to_csv.linux64.exe  T01File 


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define BUFLEN_CONV 256

/*---------------------------------------------------------------------*/
void t01PreRead(char* t01Filename,int *nbglobVar,int *nbPartVar, int *nbSubsVar,int *nbTimeStep,int *cptData,int *cptThGroupNames);
void t01Read(char* t01Filename,float *allData,char **ThPartNames,char **ThSubsNames,char **ThGroupNames);
void csvFileWrite(char* csvFilename,char* titleFilename,int *nbglobVar,int *nbPartVar, int *nbSubsVar,int *nbTimeStep,int *cptData,float *allData,char **ThPartNames,char **ThSubsNames,char **ThGroupNames);
void read_r_c(float *w,int len);
void read_i_c(int *w,int len);
void read_c_c(int *w,int len);
void read_db_c(double *w,int len);
void print_r_c(float *w,int len);
void print_i_c(int *w,int len);
void print_c_c(int *w,int len);
void print_c_c_to_char(int *w,int len,char *title);
void print_i_c_to_char(int *w,int len,char *title);
void print_db_c(double *w,int len);
void eor_c_read(int *len);
void IEEE_ASCII_to_real(float *real,unsigned char octet[4]);
void IEEE_ASCII_to_integer(int *entier,unsigned char octet[4]);
void IEEE_ASCII_to_double(double *real,unsigned char octet[1000][8],int len);
/*---------------------------------------------------------------------*/

int EndOfFile;
FILE *curfile;

int main( int argc, char *argv[] )
{
    int i;
    char csvFilename[100];
    char t01Filename[100];
    char titleFilename[100];
    char **ThPartNames;
    char **ThSubsNames;
    char **ThGroupNames;
    float *allData;
 /* Initialization */
    int nbTimeStep=0;
    int cptData=0;
    int cptThGroupNames=0;
    int nbglobVar=0;
    int nbPartVar=0;
    int nbSubsVar=0;

    if (argc == 2)
    {
        printf("\n T01 TO CSV CONVERTOR\n\n");
        printf("FILE    = %s\n", argv[1]);
        printf("OUTPUT FILE    = %s.csv",argv[1]);
        printf("\n");
    }

    sprintf(csvFilename,"%s.csv",argv[1]);
    sprintf(t01Filename,"%s",argv[1]);
    sprintf(titleFilename,"%s_TITLES",argv[1]);
/*-----------------------------
     PRE READ & DIMENSION
-----------------------------*/
    t01PreRead(t01Filename,&nbglobVar,&nbPartVar,&nbSubsVar,&nbTimeStep,&cptData,&cptThGroupNames);
    ThPartNames =(char**)malloc(sizeof(char**)*nbPartVar);
    ThSubsNames =(char**)malloc(sizeof(char**)*nbSubsVar);
    ThGroupNames =(char**)malloc(sizeof(char**)*cptThGroupNames);
    allData = malloc(sizeof(float*)*cptData);
/*-----------------------------
     READ DATA
-----------------------------*/
    t01Read(t01Filename,allData,ThPartNames,ThSubsNames,ThGroupNames);
/*-----------------------------
     WRITE CSV FILE
-----------------------------*/
    csvFileWrite(csvFilename,titleFilename,&nbglobVar,&nbPartVar,&nbSubsVar,&nbTimeStep,&cptData,allData,ThPartNames,ThSubsNames,ThGroupNames);
/*-----------------------------
     FREE
-----------------------------*/
     free(ThPartNames);
     free(ThSubsNames);
     free(ThGroupNames);
     free(allData);
/*-----------------------------
     END
-----------------------------*/
    printf(" ** CONVERSION COMPLETED\n");
    return 1;         
}

/***************************************************************************/
void t01PreRead(char* t01Filename,int *nbglobVar,int *nbPartVar, int *nbSubsVar,int *nbTimeStep,int *cptData,int *cptThGroupNames)
/*      write abf file structure  */
{
    int ICODE[100],Ivar[10];
    int CCODE[100];
    float RCODE[100];
    int length[100];
    int i,j,k;
    int THICODE,NUMMAT,NUMGEO,NVAR,NBELEM,NBSUBSF,NBPARTF,NSUBS;
    int NPART_NTHPART,NTHGRP2,NGLOB;
    int NVAR_PART_TOT=0;
    int NVAR_SUBS=0;
    int titleLength=0;
    int *NVAR_PART = NULL;
    int *NBELEM_THGRP = NULL;
    int *NVAR_THGRP = NULL;

/*-----------------------------
     PRE READ  
-----------------------------*/
/*      read temporary file  */
    curfile=fopen(t01Filename,"rb");
    if (!curfile)
    {
        printf(" ** ERROR: FILE %s NOT FOUND\n",t01Filename);
        return;
    }
/*-------TITRE------------*/
/*        printf("*********************************\n");
        printf("TITRE\n");
        printf("*********************************\n");*/
    eor_c_read(length);
    read_i_c(ICODE,1);
    THICODE = *ICODE;
    if(THICODE >= 4021)
    {
        titleLength = 100;
    }
    else if(THICODE >= 3041)
    {
        titleLength = 80;
    }
    else 
    {
        titleLength = 40;
    }
    read_c_c(CCODE,80);
    eor_c_read(length);
/*-------ivers date------------*/
/*        printf("*********************************\n");
        printf("ivers\n");
        printf("*********************************\n");*/
    eor_c_read(length);
    read_c_c(CCODE,80);
    eor_c_read(length);
/*-------ADDITIONAL RECORDS------------*/
    if(THICODE > 3050)
    {
/*            printf("*********************************\n");
            printf("ADDITIONAL RECORDS\n");
            printf("*********************************\n");*/
        eor_c_read(length);
        read_i_c(ICODE,1);
        eor_c_read(length);
/*
           1ST RECORD : title length*/
        eor_c_read(length);
        read_i_c(ICODE,1);
        eor_c_read(length);
/*
C           2ND RECORD : FAC_MASS,FAC_LENGTH,FAC_TIME */
        eor_c_read(length);
        read_r_c(RCODE,1);
        read_r_c(RCODE,1);
        read_r_c(RCODE,1);
        eor_c_read(length);
    }
/*-------HIERARCHY INFO------------*/
/*        printf("*********************************\n");
        printf("HIERARCHY INFO_\n");
        printf("*********************************\n");*/
    eor_c_read(length);
    read_i_c(ICODE,1);
    NPART_NTHPART = *ICODE;

    read_i_c(ICODE,1);
    NUMMAT = *ICODE;

    read_i_c(ICODE,1);
    NUMGEO = *ICODE;

    read_i_c(ICODE,1);
    NSUBS = *ICODE;

    read_i_c(ICODE,1);
    NTHGRP2  = *ICODE; 

    read_i_c(ICODE,1);
    NGLOB = *ICODE; 
    *nbglobVar = NGLOB;

    eor_c_read(length);

    fclose(curfile); 
/*-----------------------------
     READ DATA  
-----------------------------*/
    NVAR_PART =(int*) malloc(sizeof(int)*NPART_NTHPART);
    NBELEM_THGRP=(int*) malloc(sizeof(int)*NTHGRP2);
    NVAR_THGRP=(int*) malloc(sizeof(int)*NTHGRP2);
/*      read temporary file  */
    curfile=fopen(t01Filename,"rb");
    if (!curfile)
    {
        printf(" ** ERROR: FILE %s NOT FOUND\n",t01Filename);
        if (NVAR_PART)    free(NVAR_PART);
        if (NBELEM_THGRP) free(NBELEM_THGRP);
        if (NVAR_THGRP)   free(NVAR_THGRP);
        return;
    }
/*-------TITRE------------*/
/*        printf("*********************************\n");
        printf("TITRE\n");
        printf("*********************************\n");*/
    eor_c_read(length);
    read_i_c(ICODE,1);
    THICODE = *ICODE;
    read_c_c(CCODE,80);
    eor_c_read(length);
/*-------ivers date------------*/
/*        printf("*********************************\n");
        printf("ivers\n");
        printf("*********************************\n");*/
    eor_c_read(length);
    read_c_c(CCODE,80);
    eor_c_read(length);
/*-------ADDITIONAL RECORDS------------*/
    if(THICODE > 3050)
    {
/*            printf("*********************************\n");
            printf("ADDITIONAL RECORDS\n");
            printf("*********************************\n");*/
        eor_c_read(length);
        read_i_c(ICODE,1);
        eor_c_read(length);
/*
           1ST RECORD : title length*/
        eor_c_read(length);
        read_i_c(ICODE,1);
        eor_c_read(length);
/*
C           2ND RECORD : FAC_MASS,FAC_LENGTH,FAC_TIME */
        eor_c_read(length);
        read_r_c(RCODE,1);
        read_r_c(RCODE,1);
        read_r_c(RCODE,1);
        eor_c_read(length);
    }
/*
/*-------HIERARCHY INFO------------*/
/*        printf("*********************************\n");
        printf("HIERARCHY INFO_\n");
        printf("*********************************\n");*/
    eor_c_read(length);
    read_i_c(ICODE,1);
    NPART_NTHPART = *ICODE;

    read_i_c(ICODE,1);
    NUMMAT = *ICODE;

    read_i_c(ICODE,1);
    NUMGEO = *ICODE;

    read_i_c(ICODE,1);
    NSUBS = *ICODE;

    read_i_c(ICODE,1);
    NTHGRP2  = *ICODE; 

    read_i_c(ICODE,1);
    NGLOB = *ICODE;  

    eor_c_read(length);

/*-----------------------------------*/
    if(NGLOB > 0)
    {
        eor_c_read(length);
        for(i=0;i<NGLOB;i++){
            read_i_c(ICODE,1);
        }
        eor_c_read(length);
    }
/*-------PART DESCRIPTION------------*/
/*        printf("*********************************\n");
        printf("PART DESCRIPTION _%d_\n",NPART_NTHPART);
        printf("*********************************\n");*/
    if(NPART_NTHPART > 0)
    {
        for(i=0;i<NPART_NTHPART;i++){
            eor_c_read(length);
            read_i_c(ICODE,1);
            read_c_c(CCODE,titleLength);
            read_i_c(ICODE,1);
            read_i_c(ICODE,1);
            read_i_c(ICODE,1);
            read_i_c(ICODE,1);
            eor_c_read(length);

            NVAR = *ICODE;

            NVAR_PART[i] = *ICODE;
            NVAR_PART_TOT = NVAR_PART_TOT + NVAR;
            for(j=0;j<NVAR;j++){
                if (j==0){eor_c_read(length);}
                read_i_c(ICODE,1);
                if (j==NVAR-1){eor_c_read(length);}
            }
        }
    }
    *nbPartVar = NVAR_PART_TOT;
/*-------MATER DESCRIPTION------------*/
/*        printf("*********************************\n");
        printf("MATER DESCRIPTION _%d_\n",NUMMAT);
        printf("*********************************\n");*/
    if(NUMMAT > 0)
    {
        for(i=0;i<NUMMAT;i++){
            eor_c_read(length);
            read_i_c(ICODE,1);
            read_c_c(CCODE,titleLength); 
            eor_c_read(length);
        }
    }
/*-------GEO DESCRIPTION------------*/
/*        printf("*********************************\n");
        printf("GEO DESCRIPTION _%d_\n",NUMGEO);
        printf("*********************************\n");*/
    if(NUMMAT > 0)
    {
        for(i=0;i<NUMGEO;i++){
            eor_c_read(length);
            read_i_c(ICODE,1);
            read_c_c(CCODE,titleLength); 
            eor_c_read(length);
        }
    }
/*-------HIERARCHY DESCRIPTION------------*/
/*        printf("*********************************\n");
        printf("HIERARCHY DESCRIPTION _%d_\n",NSUBS);
        printf("*********************************\n");*/
    if(NSUBS > 0)
    {
        for(i=0;i<NSUBS;i++){
            eor_c_read(length);
            read_i_c(ICODE,1);
            read_i_c(ICODE,1);
            read_i_c(ICODE,1);
            NBSUBSF = *ICODE;
            read_i_c(ICODE,1);
            NBPARTF = *ICODE;
            read_i_c(ICODE,1);
            NVAR = *ICODE;
            read_c_c(CCODE,titleLength);
            eor_c_read(length);
            if (NBSUBSF > 0)
            {
                for(j=0;j<NBSUBSF;j++){
                    if (j==0){eor_c_read(length);}
                    read_i_c(ICODE,1);
                    if (j==NBSUBSF-1){eor_c_read(length);}
                }
            }
            if (NBPARTF > 0)
            {
                for(j=0;j<NBPARTF;j++){
                    if (j==0){eor_c_read(length);}
                    read_i_c(ICODE,1);
                    if (j==NBPARTF-1){eor_c_read(length);}
                }
            }
            if (NVAR > 0)
            {
                for(j=0;j<NVAR;j++){
                    if (j==0){eor_c_read(length);}
                    read_i_c(ICODE,1);
                    if (j==NVAR-1){eor_c_read(length);}
                    NVAR_SUBS = NVAR_SUBS + 1;
                }
            }
        }
    }
    *nbSubsVar = NVAR_SUBS;
/*-------TH GROUP------------*/
/*        printf("*********************************\n");
        printf("TH GROUP_%d_\n",NTHGRP2);
        printf("*********************************\n");*/
    if(NTHGRP2 > 0)
    {
        for(i=0;i<NTHGRP2;i++){
            eor_c_read(length);
            read_i_c(ICODE,1);
            read_i_c(ICODE,1);
            read_i_c(ICODE,1);
            read_i_c(ICODE,1);
            NBELEM = *ICODE;
            NBELEM_THGRP[i]=NBELEM;

            read_i_c(ICODE,1);
            NVAR = *ICODE;
            NVAR_THGRP[i]=NVAR;

            read_c_c(CCODE,titleLength);
            eor_c_read(length);

            for(j=0;j<NBELEM;j++){
                eor_c_read(length);
                read_i_c(ICODE,1);
                read_c_c(CCODE,titleLength);
                eor_c_read(length);
                *cptThGroupNames = *cptThGroupNames + NVAR;
            }

            for(j=0;j<NVAR;j++){
                if (j==0){eor_c_read(length);}
                read_i_c(ICODE,1);
                if (j==NVAR-1){eor_c_read(length);}
            }
        }
    }

    EndOfFile = 2;
    while (EndOfFile == 2){
/*-----------------------------
     TIME   
-----------------------------*/
        eor_c_read(length);
        read_r_c(RCODE,1);
        *cptData = *cptData + 1;
        eor_c_read(length);
        if (EndOfFile == 1){return;}

              //TIME=*RCODE;
        *nbTimeStep = *nbTimeStep + 1;
/*-----------------------------
     GLOBAL VARIABLES
-----------------------------*/
        int nbval=NGLOB;
        for(i=0;i<nbval;i++){
            if (i==0){eor_c_read(length);}
            read_r_c(RCODE,1);
            *cptData = *cptData + 1;
            if (EndOfFile == 1){return;}
            if (i==nbval-1){eor_c_read(length);}
        }
/*-----------------------------
     PART VARIABLES
-----------------------------*/
        if (NPART_NTHPART>0)
        {
            if (NVAR_PART_TOT > 0){eor_c_read(length);}
            for(i=0;i<NPART_NTHPART;i++){
                for(j=0;j<NVAR_PART[i];j++){
                    read_r_c(RCODE,1);
                    *cptData = *cptData + 1;
                    if (EndOfFile == 1){return;}
                }
            }
            if (NVAR_PART_TOT > 0){eor_c_read(length);}
        }
/*-----------------------------
     SUBSET VARIABLES
-----------------------------*/
        if(NVAR_SUBS > 0)
        {
            eor_c_read(length);
            for(i=0;i<NVAR_SUBS;i++) {
                read_r_c(RCODE,1);
                *cptData = *cptData + 1;
            }
            eor_c_read(length);
        }
/*-------------------------------------------------------
    TH GROUP
-------------------------------------------------------*/
        for(i=0;i<NTHGRP2;i++){
            eor_c_read(length);
            for(j=0;j<NBELEM_THGRP[i];j++){
                for(k=0;k<NVAR_THGRP[i];k++){
                    read_r_c(RCODE,1);
                    *cptData = *cptData + 1;
                }
            }
            eor_c_read(length);
        }
    }

    fclose(curfile);
    if (NVAR_PART)    free(NVAR_PART);
    if (NBELEM_THGRP) free(NBELEM_THGRP);
    if (NVAR_THGRP)   free(NVAR_THGRP);
    return;
}

/***************************************************************************/
void t01Read(char* t01Filename,float *allData,char **ThPartNames,char **ThSubsNames,char **ThGroupNames)
/*      write abf file structure  */
{
    int ICODE[100],Ivar[10];
    int CCODE[100];
    float RCODE[100];
    int length[100];
    char name[100],title[110],NameRequest[100],IdRequest[100],NameVar[10],Id_Part[10],NamePart[100],NameOutput[100];
    int i,j,k;
    int THICODE,NUMMAT,NUMGEO,NVAR,NBELEM,NBSUBSF,NBPARTF,NSUBS;
    int NPART_NTHPART,NTHGRP2,NGLOB;
    int NVAR_PART_TOT=0;
    int NVAR_SUBS=0;
    int titleLength=0;

    int cptData=0;
    int cptDataTitles=0;
    int cptThPartNames=0;
    int cptThSubsNames=0;

    int *NVAR_PART;
    int *NBELEM_THGRP;
    int *NVAR_THGRP;

/*-----------------------------
     PRE READ  
-----------------------------*/
/*      read temporary file  */
    curfile=fopen(t01Filename,"rb");
    if (!curfile)
    {
        printf(" ** ERROR: FILE %s NOT FOUND\n",t01Filename);
        return;
    }
/*-------TITRE------------*/
/*        printf("*********************************\n");
        printf("TITRE\n");
        printf("*********************************\n");*/
    eor_c_read(length);
    read_i_c(ICODE,1);
    THICODE = *ICODE;
    if(THICODE >= 4021)
    {
        titleLength = 100;
    }
    else if(THICODE >= 3041)
    {
        titleLength = 80;
    }
    else 
    {
        titleLength = 40;
    }
    read_c_c(CCODE,80);
    eor_c_read(length);
/*-------ivers date------------*/
/*        printf("*********************************\n");
        printf("ivers\n");
        printf("*********************************\n");*/
    eor_c_read(length);
    read_c_c(CCODE,80);
    eor_c_read(length);
/*-------ADDITIONAL RECORDS------------*/
    if(THICODE > 3050)
    {
/*            printf("*********************************\n");
            printf("ADDITIONAL RECORDS\n");
            printf("*********************************\n");*/
        eor_c_read(length);
        read_i_c(ICODE,1);
        eor_c_read(length);
/*
           1ST RECORD : title length*/
        eor_c_read(length);
        read_i_c(ICODE,1);
        eor_c_read(length);
/*
C           2ND RECORD : FAC_MASS,FAC_LENGTH,FAC_TIME */
        eor_c_read(length);
        read_r_c(RCODE,1);
        read_r_c(RCODE,1);
        read_r_c(RCODE,1);
        eor_c_read(length);
    }
/*
/*-------HIERARCHY INFO------------*/
/*        printf("*********************************\n");
        printf("HIERARCHY INFO_\n");
        printf("*********************************\n");*/
    eor_c_read(length);
    read_i_c(ICODE,1);
    NPART_NTHPART = *ICODE;

    read_i_c(ICODE,1);
    NUMMAT = *ICODE;

    read_i_c(ICODE,1);
    NUMGEO = *ICODE;

    read_i_c(ICODE,1);
    NSUBS = *ICODE;

    read_i_c(ICODE,1);
    NTHGRP2  = *ICODE; 

    read_i_c(ICODE,1);
    NGLOB = *ICODE;  

    eor_c_read(length);

    fclose(curfile); 
/*-----------------------------
     READ DATA  
-----------------------------*/
    NVAR_PART    = (int*) malloc(sizeof(int)*NPART_NTHPART);
    NBELEM_THGRP = (int*) malloc(sizeof(int)*NTHGRP2);
    NVAR_THGRP   = (int*) malloc(sizeof(int)*NTHGRP2);
/*      read temporary file  */
    curfile=fopen(t01Filename,"rb");
    if (!curfile)
    {
        printf(" ** ERROR: FILE %s NOT FOUND\n",t01Filename);
        if (NVAR_PART)    free(NVAR_PART);
        if (NBELEM_THGRP) free(NBELEM_THGRP);
        if (NVAR_THGRP)   free(NVAR_THGRP);
        return;
    }
/*-------TITRE------------*/
/*        printf("*********************************\n");
        printf("TITRE\n");
        printf("*********************************\n");*/
    eor_c_read(length);
    read_i_c(ICODE,1);
    read_c_c(CCODE,80);
    eor_c_read(length);
/*-------ivers date------------*/
/*        printf("*********************************\n");
        printf("ivers\n");
        printf("*********************************\n");*/
    eor_c_read(length);
    read_c_c(CCODE,80);
    eor_c_read(length);
/*-------ADDITIONAL RECORDS------------*/
    if(THICODE > 3050)
    {
/*            printf("*********************************\n");
            printf("ADDITIONAL RECORDS\n");
            printf("*********************************\n");*/
        eor_c_read(length);
        read_i_c(ICODE,1);
        eor_c_read(length);
/*
           1ST RECORD : title length*/
        eor_c_read(length);
        read_i_c(ICODE,1);
        eor_c_read(length);
/*
C           2ND RECORD : FAC_MASS,FAC_LENGTH,FAC_TIME */
        eor_c_read(length);
        read_r_c(RCODE,1);
        read_r_c(RCODE,1);
        read_r_c(RCODE,1);
        eor_c_read(length);
    }
/*
/*-------HIERARCHY INFO------------*/
/*        printf("*********************************\n");
        printf("HIERARCHY INFO_\n");
        printf("*********************************\n");*/
    eor_c_read(length);
    read_i_c(ICODE,1);
    NPART_NTHPART = *ICODE;

    read_i_c(ICODE,1);
    NUMMAT = *ICODE;

    read_i_c(ICODE,1);
    NUMGEO = *ICODE;

    read_i_c(ICODE,1);
    NSUBS = *ICODE;

    read_i_c(ICODE,1);
    NTHGRP2  = *ICODE; 

    read_i_c(ICODE,1);
    NGLOB = *ICODE;

    eor_c_read(length);

/*-----------------------------------*/
    if(NGLOB > 0)
    {
        eor_c_read(length);
        for(i=0;i<NGLOB;i++){
            read_i_c(ICODE,1);
        }
        eor_c_read(length);
    }
/*-------PART DESCRIPTION------------*/
/*        printf("*********************************\n");
        printf("PART DESCRIPTION reading part _%d_\n",NPART_NTHPART);
        printf("*********************************\n");*/
    if(NPART_NTHPART > 0)
    {
        for(i=0;i<NPART_NTHPART;i++){
            eor_c_read(length);
            read_i_c(ICODE,1);
            print_i_c_to_char(ICODE,100,IdRequest);
            read_c_c(CCODE,titleLength);
            print_c_c_to_char(CCODE,titleLength,NameRequest);
            strcpy(title,NameRequest);
            strcat(title," ");
            read_i_c(ICODE,1);
            read_i_c(ICODE,1);
            read_i_c(ICODE,1);
            read_i_c(ICODE,1);
            eor_c_read(length);

            NVAR = *ICODE;

            NVAR_PART[i] = *ICODE;
            NVAR_PART_TOT = NVAR_PART_TOT + NVAR;

            for(j=0;j<NVAR;j++){
                if (j==0){eor_c_read(length);}
                read_i_c(ICODE,1);
                if (j==NVAR-1){eor_c_read(length);}
                ThPartNames[cptThPartNames] =(char*)malloc(sizeof(char*)*110);

                strcpy(ThPartNames[cptThPartNames], title);
                strcat(ThPartNames[cptThPartNames]," ");
                switch(*ICODE)
                {
                    case 1:
                    strcat(ThPartNames[cptThPartNames],"IE");
                    break;
                    case 2:
                    strcat(ThPartNames[cptThPartNames],"KE");                                         
                    break;                                                                            
                    case 3:                                                                               
                    strcat(ThPartNames[cptThPartNames],"XMOM");                                       
                    break;                                                                            
                    case 4:                                                                               
                    strcat(ThPartNames[cptThPartNames],"YMOM");                                       
                    break;                                                                            
                    case 5:                                                                               
                    strcat(ThPartNames[cptThPartNames],"ZMOM");                                       
                    break;                                                                            
                    case 6:                                                                               
                    strcat(ThPartNames[cptThPartNames],"MASS");                                       
                    break;                                                                            
                    case 7:                                                                               
                    strcat(ThPartNames[cptThPartNames],"HE");                                         
                    break;                                                                            
                    case 8:                                                                               
                    strcat(ThPartNames[cptThPartNames],"TURBKE");                                     
                    break;                                                                            
                    case 9:                                                                               
                    strcat(ThPartNames[cptThPartNames],"XCG");                                        
                    break;                                                                            
                    case 10:                                                                              
                    strcat(ThPartNames[cptThPartNames],"YCG");                                        
                    break;                                                                            
                    case 11:                                                                              
                    strcat(ThPartNames[cptThPartNames],"ZCG");                                        
                    break;
                    case 12:
                    strcat(ThPartNames[cptThPartNames],"XXMOM");
                    break;
                    case 13:
                    strcat(ThPartNames[cptThPartNames],"YYMOM");
                    break;
                    case 14:
                    strcat(ThPartNames[cptThPartNames],"ZZMOM");
                    break;
                    case 15:
                    strcat(ThPartNames[cptThPartNames],"IXX");
                    break;
                    case 16:
                    strcat(ThPartNames[cptThPartNames],"IYY");
                    break;
                    case 17:
                    strcat(ThPartNames[cptThPartNames],"IZZ");
                    break;
                    case 18:
                    strcat(ThPartNames[cptThPartNames],"IXY");
                    break;
                    case 19:
                    strcat(ThPartNames[cptThPartNames],"IYZ");
                    break;
                    case 20:
                    strcat(ThPartNames[cptThPartNames],"IZX");
                    break;
                    case 21:
                    strcat(ThPartNames[cptThPartNames],"RIE");
                    break;
                    case 22:
                    strcat(ThPartNames[cptThPartNames],"KERB");
                    break;
                    case 23:
                    strcat(ThPartNames[cptThPartNames],"RKERB");
                    break;
                    case 24:
                    strcat(ThPartNames[cptThPartNames],"RKE");
                    break;
                    case 25:
                    strcat(ThPartNames[cptThPartNames],"HEAT");
                    break;
                    case 26:
                    strcat(ThPartNames[cptThPartNames],"ZZMOM");
                    break;
                    case 27:
                    strcat(ThPartNames[cptThPartNames],"ZZMOM");
                    break;
                    case 28:
                    strcat(ThPartNames[cptThPartNames],"HEAT");
                    break;
                    default :
                    strcat(ThPartNames[cptThPartNames],"empty");
                    break;
                }

                cptThPartNames++;
            }
        }
    }
/*-------MATER DESCRIPTION------------*/
/*        printf("*********************************\n");
        printf("MATER DESCRIPTION _%d_\n",NUMMAT);
        printf("*********************************\n");*/
    if(NUMMAT > 0)
    {
        for(i=0;i<NUMMAT;i++){
            eor_c_read(length);
            read_i_c(ICODE,1);
            read_c_c(CCODE,titleLength); 
            eor_c_read(length);
        }
    }
/*-------GEO DESCRIPTION------------*/
/*        printf("*********************************\n");
        printf("GEO DESCRIPTION _%d_\n",NUMGEO);
        printf("*********************************\n");*/
    if(NUMMAT > 0)
    {
        for(i=0;i<NUMGEO;i++){
            eor_c_read(length);
            read_i_c(ICODE,1);
            read_c_c(CCODE,titleLength); 
            eor_c_read(length);
        }
    }
/*-------HIERARCHY DESCRIPTION------------*/
/*        printf("*********************************\n");
        printf("HIERARCHY DESCRIPTION _%d_\n",NSUBS);
        printf("*********************************\n");*/
    if(NSUBS > 0)
    {
        for(i=0;i<NSUBS;i++){
            eor_c_read(length);
            read_i_c(ICODE,1);
            print_i_c_to_char(ICODE,100,IdRequest);
            read_i_c(ICODE,1);
            read_i_c(ICODE,1);
            NBSUBSF = *ICODE;
            read_i_c(ICODE,1);
            NBPARTF = *ICODE;
            read_i_c(ICODE,1);
            NVAR = *ICODE;
            read_c_c(CCODE,titleLength);
            print_c_c_to_char(CCODE,titleLength,NameRequest);
            strcpy(title,NameRequest);
            strcat(title," ");
            eor_c_read(length);
            if (NBSUBSF > 0)
            {
                for(j=0;j<NBSUBSF;j++){
                    if (j==0){eor_c_read(length);}
                    read_i_c(ICODE,1);
                    if (j==NBSUBSF-1){eor_c_read(length);}
                }
            }
            if (NBPARTF > 0)
            {
                for(j=0;j<NBPARTF;j++){
                    if (j==0){eor_c_read(length);}
                    read_i_c(ICODE,1);
                    if (j==NBPARTF-1){eor_c_read(length);}
                }
            }
            if (NVAR > 0)
            {
                for(j=0;j<NVAR;j++){
                    if (j==0){eor_c_read(length);}
                    read_i_c(ICODE,1);
                    if (j==NVAR-1){eor_c_read(length);}
                    NVAR_SUBS = NVAR_SUBS + 1;

                    ThSubsNames[cptThSubsNames] =(char*)malloc(sizeof(char*)*110);
                    strcpy(ThSubsNames[cptThSubsNames], title);
                    strcat(ThSubsNames[cptThSubsNames]," ");
                    switch(*ICODE)
                    {
                        case 1:
                        strcat(ThSubsNames[cptThSubsNames],"IE");
                        break;
                        case 2:
                        strcat(ThSubsNames[cptThSubsNames],"KE");                                         
                        break;                                                                            
                        case 3:                                                                               
                        strcat(ThSubsNames[cptThSubsNames],"XMOM");                                       
                        break;                                                                            
                        case 4:                                                                               
                        strcat(ThSubsNames[cptThSubsNames],"YMOM");                                       
                        break;                                                                            
                        case 5:                                                                               
                        strcat(ThSubsNames[cptThSubsNames],"ZMOM");                                       
                        break;                                                                            
                        case 6:                                                                               
                        strcat(ThSubsNames[cptThSubsNames],"MASS");                                       
                        break;                                                                            
                        case 7:                                                                               
                        strcat(ThSubsNames[cptThSubsNames],"HE");                                         
                        break;                                                                            
                        case 8:                                                                               
                        strcat(ThSubsNames[cptThSubsNames],"TURBKE");                                     
                        break;                                                                            
                        case 9:                                                                               
                        strcat(ThSubsNames[cptThSubsNames],"XCG");                                        
                        break;                                                                            
                        case 10:                                                                              
                        strcat(ThSubsNames[cptThSubsNames],"YCG");                                        
                        break;                                                                            
                        case 11:                                                                              
                        strcat(ThSubsNames[cptThSubsNames],"ZCG");                                        
                        break;
                        case 12:
                        strcat(ThSubsNames[cptThSubsNames],"XXMOM");
                        break;
                        case 13:
                        strcat(ThSubsNames[cptThSubsNames],"YYMOM");
                        break;
                        case 14:
                        strcat(ThSubsNames[cptThSubsNames],"ZZMOM");
                        break;
                        case 15:
                        strcat(ThSubsNames[cptThSubsNames],"IXX");
                        break;
                        case 16:
                        strcat(ThSubsNames[cptThSubsNames],"IYY");
                        break;
                        case 17:
                        strcat(ThSubsNames[cptThSubsNames],"IZZ");
                        break;
                        case 18:
                        strcat(ThSubsNames[cptThSubsNames],"IXY");
                        break;
                        case 19:
                        strcat(ThSubsNames[cptThSubsNames],"IYZ");
                        break;
                        case 20:
                        strcat(ThSubsNames[cptThSubsNames],"IZX");
                        break;
                        case 21:
                        strcat(ThSubsNames[cptThSubsNames],"RIE");
                        break;
                        case 22:
                        strcat(ThSubsNames[cptThSubsNames],"KERB");
                        break;
                        case 23:
                        strcat(ThSubsNames[cptThSubsNames],"RKERB");
                        break;
                        case 24:
                        strcat(ThSubsNames[cptThSubsNames],"RKE");
                        break;
                        case 25:
                        strcat(ThSubsNames[cptThSubsNames],"HEAT");
                        break;
                        case 26:
                        strcat(ThSubsNames[cptThSubsNames],"ZZMOM");
                        break;
                        case 27:
                        strcat(ThSubsNames[cptThSubsNames],"ZZMOM");
                        break;
                        case 28:
                        strcat(ThSubsNames[cptThSubsNames],"HEAT");
                        break;
                        default :
                        strcat(ThSubsNames[cptThSubsNames],"empty");
                        break;
                    }

                    cptThSubsNames++;
                }
            }
        }
    }
/*-------TH GROUP------------*/
/*        printf("*********************************\n");
        printf("TH GROUP_%d_\n",NTHGRP2);
        printf("*********************************\n");*/

    int cptThGroupNames=0;
    if(NTHGRP2 > 0)
    {
        for(i=0;i<NTHGRP2;i++){
            eor_c_read(length);
            read_i_c(ICODE,1);
            read_i_c(ICODE,1);
            read_i_c(ICODE,1);
            read_i_c(ICODE,1);
            NBELEM = *ICODE;
            NBELEM_THGRP[i]=NBELEM;

            read_i_c(ICODE,1);
            NVAR = *ICODE;
            NVAR_THGRP[i]=NVAR;

            read_c_c(CCODE,titleLength);
            print_c_c_to_char(CCODE,titleLength,NameOutput);
            eor_c_read(length);


            for(j=0;j<NBELEM;j++){

                eor_c_read(length);
                read_i_c(ICODE,1);
                print_i_c_to_char(ICODE,100,IdRequest);
                read_c_c(CCODE,titleLength);
                print_c_c_to_char(CCODE,titleLength,NameRequest);
                strcpy(title,NameOutput);
                strcat(title," ");
                strcat(title,IdRequest);
                strcat(title," ");
                strcat(title,NameRequest);
                eor_c_read(length);

                for(k=0;k<NVAR;k++){
                    ThGroupNames[cptThGroupNames] =(char*)malloc(sizeof(char*)*110);
                    strcpy(ThGroupNames[cptThGroupNames], title);
                    cptThGroupNames++;
                }
            }


            for(j=0;j<NVAR;j++){
                if (j==0){eor_c_read(length);}
                read_i_c(ICODE,1);
                if (j==NVAR-1){eor_c_read(length);}
            }
        }

    }

    EndOfFile = 2;
    while (EndOfFile == 2){
/*-----------------------------
     TIME   
-----------------------------*/
        eor_c_read(length);
        read_r_c(RCODE,1);
        allData[cptData]=*RCODE;
        cptData++;
        eor_c_read(length);
        if (EndOfFile == 1){return;}
              //TIME=*RCODE;
/*-----------------------------
     GLOBAL VARIABLES 
-----------------------------*/
        int nbval=NGLOB;
        for(i=0;i<nbval;i++){
            if (i==0){eor_c_read(length);}
            read_r_c(RCODE,1);
            allData[cptData]=*RCODE;
            cptData++;
            if (EndOfFile == 1){return;}
            if (i==nbval-1){eor_c_read(length);}
        }
/*-----------------------------
     PART VARIABLES
-----------------------------*/
        if (NPART_NTHPART>0)
        {
            if (NVAR_PART_TOT > 0){eor_c_read(length);}
            for(i=0;i<NPART_NTHPART;i++){
                for(j=0;j<NVAR_PART[i];j++){
                    read_r_c(RCODE,1);
                    allData[cptData]=*RCODE;
                    cptData++;
                    if (EndOfFile == 1){return;}
                }
            }
            if (NVAR_PART_TOT > 0){eor_c_read(length);}
        }
/*-----------------------------
     SUBSET VARIABLES 
-----------------------------*/
        if(NVAR_SUBS > 0)
        {
            eor_c_read(length);
            for(i=0;i<NVAR_SUBS;i++) {
                read_r_c(RCODE,1);
                allData[cptData]=*RCODE;
                cptData++;
            }
            eor_c_read(length);
        }

/*-------------------------------------------------------
    TH GROUP
-------------------------------------------------------*/
        for(i=0;i<NTHGRP2;i++){
            eor_c_read(length);
            for(j=0;j<NBELEM_THGRP[i];j++){
                for(k=0;k<NVAR_THGRP[i];k++){
                    read_r_c(RCODE,1);
                    allData[cptData]=*RCODE;
                    cptData++;
                    
                }
            }
            eor_c_read(length);
        }
    }

    fclose(curfile);
    if (NVAR_PART)    free(NVAR_PART);
    if (NBELEM_THGRP) free(NBELEM_THGRP);
    if (NVAR_THGRP)   free(NVAR_THGRP);

    return;
}

/***************************************************************************/
void csvFileWrite(char* csvFilename,char* titleFilename,int *nbglobVar,int *nbPartVar,int *nbSubsVar,int *nbTimeStep,int *cptData,float *allData,char **ThPartNames,char **ThSubsNames,char **ThGroupNames)
/*      write csv file  */
{
    int i,j;
    int cpt;
    char buffer[100];
    float *tmpImpulse;
    int outpuType = 0;
    int nbData = *cptData / *nbTimeStep ;
    int *isImpulse;

    FILE *titlesFile=fopen(titleFilename,"r");
    FILE *csvFile=fopen(csvFilename,"w");
    
    tmpImpulse = (float *) malloc(sizeof(float)* *nbTimeStep);
    isImpulse = (int *)    malloc(sizeof(int)*nbData);


    fprintf(csvFile,"\"time\",");
    fprintf(csvFile,"\"INTERNAL ENERGY\",");
    fprintf(csvFile,"\"KINETIC ENERGY\",");
    fprintf(csvFile,"\"X-MOMENTUM\",");
    fprintf(csvFile,"\"Y-MOMENTUM\",");
    fprintf(csvFile,"\"Z-MOMENTUM\",");
    fprintf(csvFile,"\"MASS\",");
    fprintf(csvFile,"\"TIME STEP\",");
    fprintf(csvFile,"\"ROTATION ENERGY\",");
    fprintf(csvFile,"\"EXTERNAL WORK\",");
    fprintf(csvFile,"\"SPRING ENERGY\",");
    fprintf(csvFile,"\"CONTACT ENERGY\",");
    fprintf(csvFile,"\"HOURGLASS ENERGY\",");

    if (*nbglobVar == 15)
    {
        fprintf(csvFile,"\"ELASTIC CONTACT ENERGY\",");
        fprintf(csvFile,"\"FRICTIONAL CONTACT ENERGY\",");
        fprintf(csvFile,"\"DAMPING CONTACT ENERGY \",");
    }

    if (!titlesFile)
    { 


        cpt = 0;
        for(i=1+*nbglobVar;i<1+*nbglobVar+*nbPartVar ;i++)
        {
            fprintf(csvFile,"\"%s \",",ThPartNames[cpt]);
            cpt++;
        } 

        cpt = 0;
        for(i=1+*nbglobVar+*nbPartVar;i<1+*nbglobVar+*nbPartVar+*nbSubsVar ;i++)
        {
            fprintf(csvFile,"\"%s \",",ThSubsNames[cpt]);
            cpt++;
        } 

        cpt = 0;
        for(i=1+*nbglobVar+*nbPartVar+*nbSubsVar;i< nbData ;i++)
        {
            fprintf(csvFile,"\"%s var %d\"",ThGroupNames[cpt],i);
            if(i < nbData-1) fprintf(csvFile,",");
            cpt++;
        } 
    }
    else
    { 

        cpt = 0;
        for(i=1+*nbglobVar;i<1+*nbglobVar+*nbPartVar ;i++)
        {
            fprintf(csvFile,"\"%s \",",ThPartNames[cpt]);
            cpt++;
        } 

        cpt = 0;
        for(i=1+*nbglobVar+*nbPartVar;i<1+*nbglobVar+*nbPartVar+*nbSubsVar ;i++)
        {
            fprintf(csvFile,"\"%s \",",ThSubsNames[cpt]);
            cpt++;
        } 

        cpt = 0;
        for(i=1+*nbglobVar+*nbPartVar+*nbSubsVar;i< nbData ;i++)
        {
            fprintf(csvFile,"\"%s ",ThGroupNames[cpt]);
            fread(buffer, 10, 1, titlesFile);
            outpuType = atoi(buffer);
            fread(buffer, 1, 1, titlesFile);
            fread(buffer, 10, 1, titlesFile);
            fprintf(csvFile," %s \"",buffer);
            if(i < nbData-1) fprintf(csvFile,",");
            cpt++;

            if((strcmp(buffer,"FNX       ")==0 || strcmp(buffer,"FNY       ")==0 || strcmp(buffer,"FNZ       ")==0  ||
                strcmp(buffer,"FTX       ")==0 || strcmp(buffer,"FTY       ")==0 || strcmp(buffer,"FTZ       ")==0  ||
                strcmp(buffer,"MX        ")==0 || strcmp(buffer,"MY        ")==0 || strcmp(buffer,"MZ        ")==0  ||
                strcmp(buffer,"FX        ")==0 || strcmp(buffer,"FY        ")==0 || strcmp(buffer,"FZ        ")==0  ||
                strcmp(buffer,"REACX     ")==0 || strcmp(buffer,"REACY     ")==0 || strcmp(buffer,"REACZ     ")==0  ||
                strcmp(buffer,"REACXX    ")==0 || strcmp(buffer,"REACYY    ")==0 || strcmp(buffer,"REACZZ    ")==0  ||
                strcmp(buffer,"|FNX|     ")==0 || strcmp(buffer,"|FNY|     ")==0 || strcmp(buffer,"|FNZ|     ")==0  ||
                strcmp(buffer,"|FX|      ")==0 || strcmp(buffer,"|FY|      ")==0 || strcmp(buffer,"|FZ|      ")==0  ||
                strcmp(buffer,"||FN||    ")==0 || strcmp(buffer,"||F||     ")==0 ||
                strcmp(buffer,"FXI       ")==0 || strcmp(buffer,"FYI       ")==0 || strcmp(buffer,"FZI       ")==0  ||
                strcmp(buffer,"MXI       ")==0 || strcmp(buffer,"MYI       ")==0 || strcmp(buffer,"MZI       ")==0)  ||
                ((strcmp(buffer,"F1        ")==0 || strcmp(buffer,"F2        ")==0 || strcmp(buffer,"F3        ")==0 ||
                    strcmp(buffer,"M1        ")==0 || strcmp(buffer,"M2        ")==0 || strcmp(buffer,"M3        ")==0 ) && 
                outpuType == 102) )
            {
                isImpulse[i]=1;
            }
            else
            {
                isImpulse[i]=0;
            }
            fread(buffer, 1, 1, titlesFile);
        }
    }

    for(i=1+*nbglobVar+*nbPartVar+*nbSubsVar;i< nbData;i++)
    {
        if(isImpulse[i] == 1)
        {
            tmpImpulse[0] = (allData[nbData+i] - allData[i]) /
            (allData[nbData] - allData[0]);
            for(j=1;j<*nbTimeStep-1;j++)
            { 
                tmpImpulse[j] = (allData[nbData*(j+1)+i] - allData[nbData*(j-1)+i]) /
                (allData[nbData*(j+1)] - allData[nbData*(j-1)]);
            }
            tmpImpulse[*nbTimeStep-1]= (allData[nbData*(*nbTimeStep-1)+i] - allData[nbData*(*nbTimeStep-2)+i]) /
            (allData[nbData*(*nbTimeStep-1)] - allData[nbData*(*nbTimeStep-2)]);

            for(j=0;j<*nbTimeStep;j++)
            { 
                allData[nbData*j + i] = tmpImpulse[j];
            }
        }
    }


    fprintf(csvFile,"\n");

    for(i=0;i<*cptData-1;i++){
        fprintf(csvFile,"%f",allData[i]);
        if((i+1)%( (*cptData) / *nbTimeStep) == 0 )
        { 
            fprintf(csvFile,"\n");
        } 
        else
        { 
            fprintf(csvFile,",");
        } 
    }

    fclose(curfile);
    if (titlesFile)fclose(titlesFile);

    free(tmpImpulse);
    free(isImpulse);
    return;
}

/***************************************************************************/
void print_c_c(w,len)
int *w;
int len;
{
    int i;
    printf("ICODE_C_%c",w[0]);
    for(i=1;i<len-1;i++){
        printf("%c",w[i]);
    }
    printf("%c_\n",w[len-1]);
} 
/***************************************************************************/
void print_c_c_to_char(w,len,title)
int *w;
int len;
char *title;
{
    int i;
    sprintf(title++,"%c",w[0]);
    for(i=1;i<len-1;i++){
        sprintf(title++,"%c",w[i]);
    }
    sprintf(title++,"%c",w[len-1]);
} 
/***************************************************************************/
void print_i_c_to_char(w,len,title)
int *w;
int len;
char *title;
{
    sprintf(title,"%d",w[0]);
} 
/***************************************************************************/
void print_i_c(w,len)
int *w;
int len;
{
    printf("ICODE_I_%d_\n",*w);
} 
/***************************************************************************/
void print_r_c(w,len)
float *w;
int len;
{
    printf("ICODE_R_%f_\n",*w);
} 
/***************************************************************************/
void print_db_c(w,len)
double *w;
int len;
{
    printf("ICODE_DB_%g_\n",*w);
} 
/***************************************************************************/
void read_r_c(w,len)
float *w;
int len;
{
    int i, j, k, block, n;
    unsigned char buf[4*BUFLEN_CONV];
    for(k=0;k<len;k+=BUFLEN_CONV)
    {
        block = ((len-k) < BUFLEN_CONV)?(len-k):BUFLEN_CONV;
        n = fread(buf,sizeof(unsigned char),block*4,curfile);
        if (n!=block*4)
        {
//    if(EndOfFile == 0){printf(" ** ERROR: END OF FILE DURING READING\n");}
            EndOfFile = 1;
            return;
            for(i=0;i<n/4;i++){
                IEEE_ASCII_to_real(&w[i+k],&buf[4*i]);
            }
            w[k+n/4]=-1.;
        }
        for(i=0;i<block;i++){
            IEEE_ASCII_to_real(&w[i+k],&buf[4*i]);
        }
    }
} /* end read_r_c */
/***************************************************************************/
    void read_i_c(w,len)
    int *w, len;
    {
        int i, j, k, block;
        unsigned char buf[4*BUFLEN_CONV];

        
        for(k=0;k<len;k+=BUFLEN_CONV)
        {
            block = ((len-k) < BUFLEN_CONV)?(len-k):BUFLEN_CONV;
            if (fread(buf,sizeof(unsigned char),block*4,curfile)!=block*4)
            {
                if(EndOfFile == 0){printf(" ** ERROR: END OF FILE DURING READING\n");}
                EndOfFile = 1;
                return;
            }
            for(i=0;i<block;i++){
                IEEE_ASCII_to_integer(&w[i+k],&buf[4*i]);
            }
        }
} /* end read_i_c */
/***************************************************************************/
        void read_c_c(w,len)
        int *w;
        int len;
        {
            int j;
            
            for(j=0;j<len;j++)
            {
                if((w[j] = (int) getc(curfile)) == EOF)
                {
                    if(EndOfFile == 0){printf(" ** ERROR: END OF FILE DURING READING\n");}
                    EndOfFile = 1;
                    exit(-1);
                    return;
                }
            }
} /* end read_c_c */
/***************************************************************************/
            void read_db_c(w,len)
            double *w;
            int len;
            {
                int i, j, c;
                unsigned char octet[1000][8];

                if (len > 1000)
                {
                    printf(" ** ERROR: BAD SIZE FOR READING\n");
                    EndOfFile = 1;
                }
                if(fread(octet,sizeof(char),(len)*8,curfile)!=(len)*8)
                {
                    if(EndOfFile == 0){printf(" ** ERROR: END OF FILE DURING READING\n");}
                    EndOfFile = 1;
                    return;
                }
                IEEE_ASCII_to_double(w,octet,len);

} /* end read_db_c */
/***************************************************************************/
                void eor_c_read(len)
                int *len;
                {
                    int i;
                    unsigned char octet[4];

                    for(i=0;i<4;i++)
                    {
                        octet[i]=getc(curfile);
                    }
                    IEEE_ASCII_to_integer(len,octet);
                }
/***************************************************************************/
                void integer_to_IEEE_ASCII(entier,octet)
                int entier;
                unsigned char octet[4];
                {
                    octet[3] = entier & 0xff;
                    octet[2] = (entier >>  8)  & 0xff;
                    octet[1] = (entier >> 16)  & 0xff;
                    octet[0] = (entier >> 24)  & 0xff;
                }
/***************************************************************************/
                void IEEE_ASCII_to_real(real,octet)
                float *real;
                unsigned char octet[4];
                {
                    int exponent;
                    int sign;
                    double mantissa, shift;

  /* sign */
                    sign = octet[0] & 0x80;
                    if (sign==0)
                    {
                        sign = 1;
                    }
                    else
                    {
                        sign = -1;
  }  /* exponent */
                        exponent  = (octet[0] & 0x7f) << 1;
                        exponent += (octet[1] & 0x80) >> 7;

  if (exponent==0)  /* +0. ou -0. */
                        {
                            *real = 0.;
                            return;
                        }

                        exponent -= 126;

  /* mantissa */
                        shift = ldexp(1.,8);
                        mantissa = (octet[1] & 0x7f);
                        mantissa = mantissa * shift + octet[2];
                        mantissa = mantissa * shift + octet[3];
                        mantissa /= ldexp(1.,24);
                        mantissa += 0.5;
  /* number */
                        *real = (float) sign * mantissa * ldexp(1.,exponent);

} /* end IEEE_ASCII_to_real */
/***************************************************************************/
                        void IEEE_ASCII_to_integer(entier,octet)
                        int *entier;
                        unsigned char octet[4];
                        {
                            int result, a, b;
                            
                            result = octet[0];
                            result = (result << 8) + octet[1];
                            result = (result << 8) + octet[2];
                            result = (result << 8) + octet[3];
/* special 64 bits */
                            if((result & 0x80000000) ==  0x80000000) 
                            {
                                a = (-1);
                                b = 0xFFFFFFFF;
                                result = result + a - b;
                            }
                            *entier = result;
                            
} /* end IEEE_ASCII_to_integer */
/***************************************************************************/
                            void IEEE_ASCII_to_double(real,octet,len)
                            double *real;
                            unsigned char octet[1000][8];
                            int len;
                            {
                                int exponent,i;
                                int sign;
                                double mantissa, shift;
                                for(i=0;i<len;i++)
                                {
    /* sign */
                                    sign = octet[i][0] & 0x80;
                                    if (sign==0)
                                    {
                                        sign = 1;
                                    }
                                    else
                                    {
                                        sign = -1;
                                    }
    /* exponent */
                                    exponent  = (octet[i][0] & 0x7f) << 4;
                                    exponent += (octet[i][1] & 0xf0) >> 4;

    if (exponent==0)  /* +0. ou -0. */
                                    {
                                        real[i] = 0.;
                                        continue;
                                    }

                                    exponent -= 1022;

    /* mantissa */
    /*  shift = ldexp(1.,8); */
                                    shift = 256.;
                                    mantissa = (octet[i][1] & 0x0f);
                                    mantissa = mantissa * shift + octet[i][2];
                                    mantissa = mantissa * shift + octet[i][3];
                                    mantissa = mantissa * shift + octet[i][4];
                                    mantissa = mantissa * shift + octet[i][5];
                                    mantissa = mantissa * shift + octet[i][6];
                                    mantissa = mantissa * shift + octet[i][7];
    /*  mantissa /= ldexp(1.,53); */
                                    mantissa /= 9.0071992547409920E15;
                                    mantissa += 0.5;

    /* number */
                                    real[i] = sign * mantissa * ldexp(1.,exponent);
  } /* end for */

} /* end IEEE_ASCII_to_double */
/***************************************************************************/
