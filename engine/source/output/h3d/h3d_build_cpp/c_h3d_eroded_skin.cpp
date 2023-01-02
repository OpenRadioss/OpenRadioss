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

#ifdef MYREAL4
#define my_real float
#endif

#ifdef MYREAL8
#define my_real double
#endif

extern "C" 
/*=================================================================*/
{
/*=================================================================*/
/*        C_H3D_UPDATE_SKIN_SCALAR                                */
/*=================================================================*/

void c_h3d_eroded_skin_(my_real *TT,int *IH3D, int *ITAB, int *NUMNOD, my_real *FUNC ,int *ID_ELEM,
                         int *CPT_DATATYPE, int *NUMELQ )
{
    int i;
    H3D_ID elem_id;
    H3D_ID comp_id;
//
    // initialize 
    try {
        // create Subcase (Loadcase)
        unsigned int       max_sims = 10;
        unsigned int      sub_count = 1;
        float elem_result[] = { 0.0f, 0.0f, 0.0f };

        // create Result Data sets
        unsigned int num_corners = 0;
        unsigned int   num_modes = 0;
        bool             complex = false;
        unsigned int cpt1 = 0;
        float value[1] ;

        sim_idx = *IH3D;

        if(*NUMELQ != 0)
        {
          rc = Hyper3DDatasetBegin(h3d_file, *NUMELQ, sim_idx, subcase_id, H3D_DS_ELEM, 
                                        H3D_DS_EROSION, num_corners, num_modes, *CPT_DATATYPE, 
                                        NULL, skin_poolname_id, complex); 
          if( !rc ) fflush(stdout);
          if( !rc ) throw rc;


          for( i = 0; i < *NUMELQ; i++ ) 
          {
              elem_id = ID_ELEM[i];
              elem_result[0] = 0.0f;
              if(  FUNC[i] == 0.0f ) 
              {
                elem_result[0] = 1.0f;
                rc = Hyper3DDatasetWrite(h3d_file, elem_id, &elem_result[0]);

                if( !rc ) fflush(stdout);
                if( !rc ) throw rc;
              }
          }

          rc = Hyper3DDatasetEnd(h3d_file);
          if( !rc ) throw rc;
        }


    } // end of try

    catch(...)    {
        Hyper3DExportClearError(h3d_file);
    }

}

void _FCALL C_H3D_ERODED_SKIN(my_real *TT,int *IH3D, int *ITAB, int *NUMNOD, my_real *FUNC ,int *ID_ELEM,
                              int *CPT_DATATYPE, int *NUMELQ)
{c_h3d_eroded_skin_ (TT,IH3D,ITAB,NUMNOD,FUNC,ID_ELEM,CPT_DATATYPE,NUMELQ);}

void c_h3d_eroded_skin__ (my_real *TT,int *IH3D, int *ITAB, int *NUMNOD, my_real *FUNC ,int *ID_ELEM,int *CPT_DATATYPE,
                            int *NUMELQ)
{c_h3d_eroded_skin_ (TT,IH3D,ITAB,NUMNOD,FUNC,ID_ELEM,CPT_DATATYPE,NUMELQ);}

void c_h3d_eroded_skin (my_real *TT,int *IH3D, int *ITAB, int *NUMNOD, my_real *FUNC ,int *ID_ELEM,int *CPT_DATATYPE,
                           int *NUMELQ)
{c_h3d_eroded_skin_ (TT,IH3D,ITAB,NUMNOD,FUNC,ID_ELEM,CPT_DATATYPE,NUMELQ);}


void c_h3d_create_skin_eroded_(int *cpt_data, char *name, int *size, int *info, char *name1, int *size1)
{
    char *cname,*cname1;
    int cname_len,cname_len1;
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

    char * LAYERPOOL = new char [*size1+11];
    sprintf(LAYERPOOL, "%s %d" ,cname1,*info);
    H3D_ID layer_pool_id = H3D_NULL_ID;
    rc = Hyper3DAddString(h3d_file, LAYERPOOL, &layer_pool_id);

    char edata_type[30];
//

        //printf( "scalar  %d  info = %d  %s\n", *cpt_data , *info, name);
        //fflush(stdout);

    try {
        // create result data types
        dt_count++;

        rc = Hyper3DDatatypeBegin(h3d_file, 1);
        if( !rc ) throw rc;

        pool_count = 2;

        dt_id++; 
        sprintf(edata_type,  cname, H3D_DT_DELIMITER); 
       // snprintf(edata_type, sizeof(edata_type), cname, H3D_DT_DELIMITER); 
        rc = Hyper3DDatatypeWrite(h3d_file, edata_type, *cpt_data , H3D_DS_EROSION, 
                                    H3D_DS_ELEM, pool_count);
        if( !rc ) throw rc;

        rc = Hyper3DDatatypeDescriptionWrite(h3d_file,dt_id, "N/A : element not deleted");
 
        rc = Hyper3DDatatypePools(h3d_file, *cpt_data , skin_poolname_id, 0, 
        			layername_ids, has_corners, tensor_type, poisson);
        if( !rc ) throw rc;

        rc = Hyper3DDatatypeEnd(h3d_file);
        if( !rc ) throw rc;

    } // end of try

    catch(...) {
        Hyper3DExportClearError(h3d_file);
    }
    delete [] LAYERPOOL;
    free(cname);
    free(cname1);

}




void _FCALL C_H3D_CREATE_SKIN_ERODED(int *cpt_data, char *name, int *size, int *info, char *name1, int *size1)
{c_h3d_create_skin_eroded_ (cpt_data, name, size, info, name1, size1);}

void c_h3d_create_skin_eroded__ (int *cpt_data, char *name, int *size, int *info, char *name1, int *size1)
{c_h3d_create_skin_eroded_ (cpt_data, name, size, info, name1, size1);}

void c_h3d_create_skin_eroded (int *cpt_data, char *name, int *size, int *info, char *name1, int *size1)
{c_h3d_create_skin_eroded_ (cpt_data, name, size, info, name1, size1);}


}
