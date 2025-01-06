//Copyright>    OpenRadioss
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
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.
//
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <fcntl.h>



#ifdef _WIN32
/* Windows includes */
#include <windows.h>
#include <process.h>
#include <io.h>
#include <sys\types.h>
#include <sys/stat.h>


#elif 1

/* Linux includes */
#include <sys/resource.h>
#include <sys/types.h>
#include <time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dlfcn.h>
#define _FCALL
#include <math.h>
#include <stdbool.h>

#endif

#include "h3dpublic_defs.h"
#include "h3dpublic_export.h"

#define _FCALL 

#include "h3d_values.h"

extern "C" 
/*=================================================================*/
{

/*=================================================================*/
/*        C_H3D_CREATE_quad_TENSOR_DATATYPE                       */
/*=================================================================*/

void c_h3d_create_quad_tensor_datatype_(int *cpt_data, char *name1, int *size1, int *info, char *name2, int *size2,
                                        int *layer, int *nuvar, int *ir, int *is, int *it,
                                        char *comment, int *s_comment)
{
    char *cname,*cname2,*ccomment;
    int cname_len,cname_len1,ccomment_len;
    int i,cpt_size;
    float node[3]; 
    H3D_ID node_id;
    unsigned int elem_count = 1;

    cname_len = *size1 + 1;
    cname=(char*) malloc(sizeof(char)*cname_len);
    for(i=0;i<*size1;i++)  cname[i] = name1[i];

    cpt_size = 0;
    for(i=0;i<*size1;i++)
    { 
         if(name1[i] != ' ') cpt_size = i; 
    }
    cname[cpt_size+1]='\0';

    ccomment_len = *s_comment + 1;
    ccomment=(char*) malloc(sizeof(char)*ccomment_len);
    for(i=0;i<*s_comment;i++)  ccomment[i] = comment[i];
    ccomment[*s_comment]='\0';  

    if( strncmp(cname,"Strain",6)  == 0 ) tensor_type = H3D_DS_STRAIN; 
    if( strncmp(cname,"Strn rate",9)  == 0 ) tensor_type = H3D_DS_STRAIN;

    char * LAYERPOOL = new char [100];
    LAYERPOOL[0] ='\0'; 
    char * LAYER_STRING = new char [100];
    LAYER_STRING[0] ='\0'; 
    char * NUVAR_STRING = new char [100];
    NUVAR_STRING[0] ='\0';
    char * IR_STRING = new char [100];
    IR_STRING[0] ='\0'; 
    char * IS_STRING = new char [100];
    IS_STRING[0] ='\0'; 
    char * IT_STRING = new char [100];
    IT_STRING[0] ='\0';   
    char * MID_STRING = new char [100];
    MID_STRING[0] ='\0'; 

    H3D_ID layer_pool_id = H3D_NULL_ID;
    if(*layer > 0 ||  *nuvar > 0 || *ir > 0 || *is > 0 || *it > 0 )
    {
        if(*ir > 0 && *is > 0)
        {
             sprintf(LAYER_STRING, "IR IS %d %d" ,*ir,*is);
#ifdef _WIN64
             strcat_s(LAYERPOOL,100,LAYER_STRING);
#else
             LAYERPOOL = strcat(LAYERPOOL,LAYER_STRING);
#endif

        }
    }
    else
    {
        sprintf(LAYER_STRING, "Mid" );
#ifdef _WIN64
        strcat_s(LAYERPOOL,100,LAYER_STRING);
#else
        LAYERPOOL = strcat(LAYERPOOL,LAYER_STRING);
#endif
        
    }

    char edata_type[50];

    try {
        // create result data types
        dt_count++;
	
	rc = Hyper3DAddString(h3d_file, LAYERPOOL, &layer_pool_id);
        if( !rc ) throw rc;

        rc = Hyper3DDatatypeBegin(h3d_file, 1);
        if( !rc ) throw rc;

        pool_count = 2;

        dt_id++; 
        sprintf(edata_type,  cname, H3D_DT_DELIMITER); 

        rc = Hyper3DDatatypeWrite(h3d_file, edata_type, *cpt_data , H3D_DS_TENSOR3D, 
                                    H3D_DS_ELEM, pool_count);
        if( !rc ) throw rc;

        if (strlen(ccomment) != 0) 
        {
             rc = Hyper3DDatatypeDescriptionWrite(h3d_file, *cpt_data, ccomment);
             if( !rc ) throw rc;
        }

        rc = Hyper3DDatatypePools(h3d_file, *cpt_data , quad_poolname_id, 1, 
        			 &layer_pool_id, has_corners, tensor_type, poisson);
        if( !rc ) throw rc;

        rc = Hyper3DDatatypeEnd(h3d_file);
        if( !rc ) throw rc;

    } // end of try

    catch(...) {
        Hyper3DExportClearError(h3d_file);
    }
    delete []    LAYERPOOL;
    delete [] LAYER_STRING; 
    delete [] NUVAR_STRING;
    delete []    IR_STRING; 
    delete []    IS_STRING; 
    delete []    IT_STRING;   
    delete []   MID_STRING; 
    free(cname);
    free(ccomment);

}



void _FCALL C_H3D_CREATE_QUAD_TENSOR_DATATYPE(int *cpt_data, char *name1, int *size1, int *info, char *name2, int *size2,
                                              int *layer, int *nuvar, int *ir, int *is, int *it,
                                              char *comment, int *s_comment)
{c_h3d_create_quad_tensor_datatype_ (cpt_data, name1, size1, info, name2, size2, layer, nuvar, ir, is, it, comment, s_comment);}

void c_h3d_create_quad_tensor_datatype__ (int *cpt_data, char *name1, int *size1, int *info, char *name2, int *size2, 
                                          int *layer, int *nuvar, int *ir, int *is, int *it,
                                          char *comment, int *s_comment)
{c_h3d_create_quad_tensor_datatype_ (cpt_data, name1, size1, info, name2, size2, layer, nuvar, ir, is, it, comment, s_comment);}

void c_create_quad_tensor_datatype (int *cpt_data, char *name1, int *size1, int *info, char *name2, int *size2, 
                                    int *layer, int *nuvar, int *ir, int *is, int *it,
                                    char *comment, int *s_comment)
{c_h3d_create_quad_tensor_datatype_ (cpt_data, name1, size1, info, name2, size2, layer, nuvar, ir, is, it, comment, s_comment);}
}
