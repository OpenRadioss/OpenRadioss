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
/*        C_H3D_CREATE_1D_VECTOR_DATATYPE                       */
/*=================================================================*/

void c_h3d_create_1d_vector_datatype_(int *cpt_data, char *name, int *size, int *info1, char *name1, int *size1,
                                      char *comment, int *s_comment)
{
    char *cname,*cname1,*ccomment;
    int cname_len,cname_len1,ccomment_len;
    int i;
    float node[3]; 
    H3D_ID node_id;
    unsigned int elem_count = 1;

    cname_len = *size + 1;
    cname=(char*) malloc(sizeof(char)*cname_len);
    for(i=0;i<*size;i++)  cname[i] = name[i];
    cname[*size]='\0'; 

    cname_len1 = *size1 + 1;
    cname1=(char*) malloc(sizeof(char)*cname_len1);
    for(i=0;i<*size1;i++)  cname1[i] = name1[i];
    cname1[*size1]='\0'; 

    ccomment_len = *s_comment + 1;
    ccomment=(char*) malloc(sizeof(char)*ccomment_len);
    for(i=0;i<*s_comment;i++)  ccomment[i] = comment[i];
    ccomment[*s_comment]='\0';  

    char * LAYERPOOL = new char [*size1+11];

    H3D_ID layer_pool_id = H3D_NULL_ID;

    sprintf(LAYERPOOL, "%s %d" ,cname1,*info1);
    rc = Hyper3DAddString(h3d_file, LAYERPOOL, &layer_pool_id);

    char edata_type[50];
//

        //printf( "scalar  %d  info = %d  %s\n", *cpt_data , *info1, name);
        //fflush(stdout);

    try {

        char TRUSSPOOL[] = "TRUSS";
        rc = Hyper3DAddString(h3d_file, TRUSSPOOL, &truss_poolname_id);
        if( !rc ) throw rc;

        char BEAMPOOL[] = "BEAM";
        rc = Hyper3DAddString(h3d_file, BEAMPOOL, &beam_poolname_id);
        if( !rc ) throw rc;

        char SPRINGPOOL[] = "SPRING";
        rc = Hyper3DAddString(h3d_file, SPRINGPOOL, &spring_poolname_id);
        if( !rc ) throw rc;

        // create result data types
        dt_count++;

        rc = Hyper3DDatatypeBegin(h3d_file, 1);
        if( !rc ) throw rc;

        pool_count = 3;

        dt_id++; 
        sprintf(edata_type,  cname, H3D_DT_DELIMITER); 

        rc = Hyper3DDatatypeWrite(h3d_file, edata_type, *cpt_data , H3D_DS_VECTOR, 
                                    H3D_DS_ELEM, pool_count);
        if( !rc ) throw rc;

        if (strlen(ccomment) != 0) 
        {
             rc = Hyper3DDatatypeDescriptionWrite(h3d_file, *cpt_data, ccomment);
             if( !rc ) throw rc;
        }
 
 
        if(*info1 != 0 && *cpt_data != 0)
        {
            rc = Hyper3DDatatypePools(h3d_file, *cpt_data , truss_poolname_id, 1, 
                                    &layer_pool_id, has_corners, tensor_type, poisson);
            if( !rc ) throw rc;
        
            rc = Hyper3DDatatypePools(h3d_file, *cpt_data , beam_poolname_id, 1, 
                                    &layer_pool_id, has_corners, tensor_type, poisson);
            if( !rc ) throw rc;
        
            rc = Hyper3DDatatypePools(h3d_file, *cpt_data , spring_poolname_id, 1, 
                                    &layer_pool_id, has_corners, tensor_type, poisson);
            if( !rc ) throw rc;
        }
        else if (*cpt_data != 0)
        {
            rc = Hyper3DDatatypePools(h3d_file, *cpt_data , truss_poolname_id, 0, 
                                    layername_ids, has_corners, tensor_type, poisson);
            if( !rc ) throw rc;
        
            rc = Hyper3DDatatypePools(h3d_file, *cpt_data , beam_poolname_id, 0, 
                                    layername_ids, has_corners, tensor_type, poisson);
            if( !rc ) throw rc;
        
            rc = Hyper3DDatatypePools(h3d_file, *cpt_data , spring_poolname_id, 0, 
                                    layername_ids, has_corners, tensor_type, poisson);
        }

        rc = Hyper3DDatatypeEnd(h3d_file);
        if( !rc ) throw rc;

    } // end of try

    catch(...) {
        Hyper3DExportClearError(h3d_file);
    }
    delete [] LAYERPOOL;
    free(cname);
    free(cname1);
    free(ccomment);

}




void _FCALL C_H3D_CREATE_1D_VECTOR_DATATYPE(int *cpt_data, char *name, int *size, int *info1, char *name1, int *size1,
                                            char *comment, int *s_comment)
{c_h3d_create_1d_vector_datatype_ (cpt_data, name, size, info1, name1, size1, comment, s_comment);}

void c_h3d_create_1d_vector_datatype__ (int *cpt_data, char *name, int *size, int *info1, char *name1, int *size1,
                                        char *comment, int *s_comment)
{c_h3d_create_1d_vector_datatype_ (cpt_data, name, size, info1, name1, size1, comment, s_comment);}

void c_create_1d_vector_datatype (int *cpt_data, char *name, int *size, int *info1, char *name1, int *size1,
                                  char *comment, int *s_comment)
{c_h3d_create_1d_vector_datatype_ (cpt_data, name, size, info1, name1, size1, comment, s_comment);}

}
