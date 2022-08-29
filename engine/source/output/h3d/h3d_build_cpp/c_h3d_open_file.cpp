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

#include "h3d_values.h"

#ifdef MYREAL4
#define my_real float
#endif

#ifdef MYREAL8
#define my_real double
#endif

   H3DFileInfo* h3d_file;
   bool rc;
   H3D_ID subcase_id;

   unsigned int   model_count;
   bool model_tabular;
   H3D_TRIBOOL model_adaptive ;
   H3D_ID model_id;

   H3D_ID assm_poolname_id;


   unsigned int assm_count;
   H3D_ID assm_id;
   H3D_ID model_as_parent;

   H3D_ID assm_parent;   // assm_parent = 1

   H3D_ID comp_poolname_id;
   H3D_ID node_poolname_id;
   H3D_ID sphnode_poolname_id;

   unsigned int comp_count;
   H3D_ID comp_id;
   H3D_ID comp_parent_id;      // comp_parent_id = assm 1

   H3D_SIM_IDX sim_idx;
   H3D_ID rbody_poolname_id;
   H3D_ID rbe2_poolname_id;
   H3D_ID rbe3_poolname_id;
   H3D_ID rwall_poolname_id;
   H3D_ID spring_poolname_id;
   H3D_ID beam_poolname_id;
   H3D_ID truss_poolname_id;
   H3D_ID elem1D_poolname_id;
   H3D_ID elem2D_poolname_id;
   H3D_ID sh4n_poolname_id;
   H3D_ID sh3n_poolname_id;
   H3D_ID shell_poolname_id;
   H3D_ID quad_poolname_id;
   H3D_ID skin_poolname_id;
   H3D_ID solid4n_poolname_id;
   H3D_ID solid10n_poolname_id;
   H3D_ID solid5n_poolname_id;
   H3D_ID solid6n_poolname_id;
   H3D_ID solid8n_poolname_id;
   H3D_ID solid16n_poolname_id;
   H3D_ID solid20n_poolname_id;
   H3D_ID rigid_poolname_id;
   H3D_ID solid_poolname_id;
   H3D_ID onedelem_poolname_id;
   H3D_ID sph_poolname_id;
   H3D_ID sphcell_poolname_id;

   char edata_type[50];
   H3D_ID dt_id;
   unsigned int pool_count;
   unsigned int layer_count;
   bool has_corners;
   H3D_TENSOR_TYPE tensor_type; // unused
   float poisson;		// default & unused
   unsigned int dt_count;
   H3D_ID* layername_ids;

#define _FCALL 
extern "C" 
/*=================================================================*/
{


/*=================================================================*/
/*        REPORTERRORMESSAGE                                       */
/*=================================================================*/

void ReportErrorMsg(H3DFileInfo* h3d_file, const char* error)
{
    FILE* errorFile = (FILE*)h3d_file->client_data1;
    if( !errorFile ) {
        errorFile = fopen("export_error_messages", "a");
        h3d_file->client_data1 = (void*)errorFile;
    }

    fprintf(errorFile, "%s\n", error);

}
/*=================================================================*/
/*        OPEN_H3D_FILE                                            */
/*=================================================================*/
void c_h3d_open_file_(char *name, int *size, my_real *percentage_error, int *comp_level, char *RADVERS, int *LEN_RADVERS,
                      my_real *FAC_M, my_real *FAC_L, my_real *FAC_T)
{

    rc = true;
    subcase_id = 1;

    model_count = 1;
    model_tabular = false;
    model_adaptive = H3D_BOOL_FALSE;
    model_id = 1;

    assm_poolname_id = H3D_NULL_ID;


    assm_count = 2;
    assm_id = 1;
    model_as_parent = H3D_NULL_ID;

    assm_parent = 0;   // assm_parent = 1

    comp_poolname_id = H3D_NULL_ID;
    node_poolname_id = H3D_NULL_ID;
    sphnode_poolname_id = H3D_NULL_ID;

    comp_count = 1;
    comp_id = 1;
    comp_parent_id = 1;      // comp_parent_id = assm 1

    rbody_poolname_id = H3D_NULL_ID;
    rbe2_poolname_id = H3D_NULL_ID;
    rbe3_poolname_id = H3D_NULL_ID;
    rwall_poolname_id = H3D_NULL_ID;
    spring_poolname_id = H3D_NULL_ID;
    beam_poolname_id = H3D_NULL_ID;
    truss_poolname_id = H3D_NULL_ID;
    elem1D_poolname_id = H3D_NULL_ID;
    elem2D_poolname_id = H3D_NULL_ID;
    sh4n_poolname_id = H3D_NULL_ID;
    sh3n_poolname_id = H3D_NULL_ID;
    shell_poolname_id = H3D_NULL_ID;
    quad_poolname_id = H3D_NULL_ID;
    skin_poolname_id = H3D_NULL_ID;
    solid4n_poolname_id = H3D_NULL_ID;
    solid10n_poolname_id = H3D_NULL_ID;
    solid5n_poolname_id = H3D_NULL_ID;
    solid6n_poolname_id = H3D_NULL_ID;
    solid8n_poolname_id = H3D_NULL_ID;
    solid16n_poolname_id = H3D_NULL_ID;
    solid20n_poolname_id = H3D_NULL_ID;
    rigid_poolname_id = H3D_NULL_ID;
    solid_poolname_id = H3D_NULL_ID;
    onedelem_poolname_id = H3D_NULL_ID;
    sph_poolname_id = H3D_NULL_ID;
    sphcell_poolname_id = H3D_NULL_ID;

    dt_id = 1;
    pool_count = 0;
    layer_count = 0;
    has_corners = false;
    poisson = 0.3f;		// default & unused
    dt_count = 2;

    char *cname;
    int cname_len;
    int i;
    
    cname_len = *size + 1;
    cname=(char*) malloc(sizeof(char)*cname_len);
    for(i=0;i<*size;i++)  cname[i] = name[i];
    cname[*size]='\0'; 
//
//  open h3d file
//
    h3d_file = Hyper3DExportOpen(cname, H3D_SINGLEFILE, NULL, ReportErrorMsg);

    h3d_file -> quantize_error = *percentage_error / 100.f ;
    h3d_file -> compression_level = *comp_level ;

    // define pool names that will be used
    char* creating_application;
    creating_application=(char*) malloc(sizeof(char)*(*LEN_RADVERS+1));
    strncpy(creating_application, RADVERS, *LEN_RADVERS);
    creating_application[*LEN_RADVERS] = '\0';
//
    char* solver_name;
    solver_name=(char*) malloc(sizeof(char)*(*LEN_RADVERS+1));
    strncpy(solver_name, RADVERS, *LEN_RADVERS);
    solver_name[*LEN_RADVERS] = '\0';
//
    char   file_creation_date[] = __DATE__;
    char   original_data_file[] = " ";
    char   original_result_file[] = " ";

    char file_comment[100];
    sprintf(file_comment, "UNIT  MASS=%g LENGTH=%g TIME=%g", *FAC_M,*FAC_L,*FAC_T);

    try {


        // create Model block
        rc = Hyper3DModelBegin(h3d_file, model_count);
        if( !rc ) throw rc;
		
		


        rc = Hyper3DModelWrite(h3d_file, cname, model_id, 
                                model_tabular, model_adaptive);
        if( !rc ) throw rc;

		

        rc = Hyper3DModelEnd(h3d_file);
        if( !rc ) throw rc;

		

        // SetModel must be called!
        rc = Hyper3DSetModelToWrite(h3d_file, model_id, model_tabular);
        if( !rc ) throw rc;

        // create File Info block
        rc = Hyper3DFileInfoBegin(h3d_file, creating_application, file_creation_date, solver_name);
        if( !rc ) throw rc;
//        rc = Hyper3DFileInfoAddModelFile(h3d_file, original_data_file);
//        if( !rc ) throw rc;
//        rc = Hyper3DFileInfoAddResultFile(h3d_file, original_result_file);
//        if( !rc ) throw rc;
        rc = Hyper3DFileInfoAddComment(h3d_file, file_comment);
        if( !rc ) throw rc;
        rc = Hyper3DFileInfoEnd(h3d_file);
        if( !rc ) throw rc;



    } // end of try

    catch(...) {
        Hyper3DExportClearError(h3d_file);
    }

}

void _FCALL C_H3D_OPEN_FILE(char *name, int *size, my_real *percentage_error, int *comp_level, char *RADVERS, int *LEN_RADVERS,
                      my_real *FAC_M, my_real *FAC_L, my_real *FAC_T)
{c_h3d_open_file_ (name,size,percentage_error,comp_level,RADVERS,LEN_RADVERS,FAC_M,FAC_L,FAC_T);}

void c_h3d_open_file__ (char *name, int *size, my_real *percentage_error, int *comp_level, char *RADVERS, int *LEN_RADVERS,
                      my_real *FAC_M, my_real *FAC_L, my_real *FAC_T)
{c_h3d_open_file_ (name,size,percentage_error,comp_level,RADVERS,LEN_RADVERS,FAC_M,FAC_L,FAC_T);}

void c_h3d_open_file (char *name, int *size, my_real *percentage_error, int *comp_level, char *RADVERS, int *LEN_RADVERS,
                      my_real *FAC_M, my_real *FAC_L, my_real *FAC_T)
{c_h3d_open_file_ (name,size,percentage_error,comp_level,RADVERS,LEN_RADVERS,FAC_M,FAC_L,FAC_T);}


/*=================================================================*/
/*        REOPEN_H3D_FILE                                            */
/*=================================================================*/
void c_h3d_reopen_file_(char *name, int *size, my_real *percentage_error, int *comp_level)
{
    
    rc = true;
    subcase_id = 1;

    model_count = 1;
    model_tabular = false;
    model_adaptive = H3D_BOOL_FALSE;
    model_id = 1;

    assm_poolname_id = H3D_NULL_ID;


    assm_count = 2;
    assm_id = 1;
    model_as_parent = H3D_NULL_ID;

    assm_parent = 0;   // assm_parent = 1

    comp_poolname_id = H3D_NULL_ID;
    node_poolname_id = H3D_NULL_ID;

    comp_count = 1;
    comp_id = 1;
    comp_parent_id = 1;      // comp_parent_id = assm 1

    rbody_poolname_id = H3D_NULL_ID;
    rbe2_poolname_id = H3D_NULL_ID;
    rbe3_poolname_id = H3D_NULL_ID;
    rwall_poolname_id = H3D_NULL_ID;
    spring_poolname_id = H3D_NULL_ID;
    beam_poolname_id = H3D_NULL_ID;
    truss_poolname_id= H3D_NULL_ID;
    elem1D_poolname_id = H3D_NULL_ID;
    elem2D_poolname_id = H3D_NULL_ID;
    sh4n_poolname_id = H3D_NULL_ID;
    sh3n_poolname_id = H3D_NULL_ID;
    shell_poolname_id = H3D_NULL_ID;
    quad_poolname_id = H3D_NULL_ID;
    skin_poolname_id = H3D_NULL_ID;
    solid4n_poolname_id = H3D_NULL_ID;
    solid10n_poolname_id = H3D_NULL_ID;
    solid5n_poolname_id = H3D_NULL_ID;
    solid6n_poolname_id = H3D_NULL_ID;
    solid8n_poolname_id = H3D_NULL_ID;
    solid16n_poolname_id = H3D_NULL_ID;
    solid20n_poolname_id = H3D_NULL_ID;
    rigid_poolname_id = H3D_NULL_ID;
    solid_poolname_id = H3D_NULL_ID;
    onedelem_poolname_id = H3D_NULL_ID;
    sph_poolname_id = H3D_NULL_ID;
    sphcell_poolname_id = H3D_NULL_ID;
    sphnode_poolname_id = H3D_NULL_ID;

    dt_id = 2;
    pool_count = 0;
    layer_count = 0;
    has_corners = false;
    poisson = 0.3f;		// default & unused
    dt_count = 2;

    char *cname;
    int cname_len;
    int i;
    cname_len = *size + 1;
    cname=(char*) malloc(sizeof(char)*cname_len);
    for(i=0;i<*size;i++)  cname[i] = name[i];
    cname[*size]='\0'; 
//
//
//  open h3d file
//
    h3d_file = Hyper3DExportOpen(cname,H3D_SINGLEFILE|H3D_APPEND, NULL, ReportErrorMsg);

    try {
        // SetModel must be called!
        rc = Hyper3DSetModelToWrite(h3d_file, model_id, model_tabular);
        if( !rc ) throw rc;


        rc = Hyper3DAddString(h3d_file, H3D_DEFAULT_COMPPOOL, &comp_poolname_id);
        if( !rc ) throw rc;

        rc = Hyper3DAddString(h3d_file, H3D_DEFAULT_NODEPOOL, &node_poolname_id);
        if( !rc ) throw rc;

        rc = Hyper3DAddString(h3d_file, H3D_DEFAULT_ELEMPOOL1D, &elem1D_poolname_id);
        if( !rc ) throw rc;

        char SHELLPOOL[] = "Shell";
        rc = Hyper3DAddString(h3d_file, SHELLPOOL, &shell_poolname_id);
        if( !rc ) throw rc;

        char SOLIDPOOL[] = "Solid";
        rc = Hyper3DAddString(h3d_file, SOLIDPOOL, &solid_poolname_id);
        if( !rc ) throw rc;

        char QUADPOOL[] = "QUAD";
        rc = Hyper3DAddString(h3d_file, QUADPOOL, &quad_poolname_id);
        if( !rc ) throw rc;

        char SKINPOOL[] = "SKIN";
        rc = Hyper3DAddString(h3d_file, SKINPOOL, &skin_poolname_id);
        if( !rc ) throw rc;

        char ONEDELEMPOOL[] = "1D";
        rc = Hyper3DAddString(h3d_file, ONEDELEMPOOL, &onedelem_poolname_id);
        if( !rc ) throw rc;

        char RIGIDPOOL[] = "Rigid";
        rc = Hyper3DAddString(h3d_file, RIGIDPOOL, &rigid_poolname_id);
        if( !rc ) throw rc;

        char SPHCELLPOOL[] = "SPHCELL";
        rc = Hyper3DAddString(h3d_file, SPHCELLPOOL, &sphcell_poolname_id);
        if( !rc ) throw rc;

        char SPHNODEPOOL[] = "SPHNodes";
        rc = Hyper3DAddString(h3d_file, SPHNODEPOOL, &sphnode_poolname_id);
        if( !rc ) throw rc;

        char SH3NPOOL[] = "SH3N";
        rc = Hyper3DAddString(h3d_file, SH3NPOOL, &sh3n_poolname_id);
        if( !rc ) throw rc;

        char SH4NPOOL[] = "SH4N";
        rc = Hyper3DAddString(h3d_file, SH4NPOOL, &sh4n_poolname_id);
        if( !rc ) throw rc;

        char SPRINGPOOL[] = "SPRING";
        rc = Hyper3DAddString(h3d_file, SPRINGPOOL, &spring_poolname_id);
        if( !rc ) throw rc;

        char TRUSSPOOL[] = "TRUSS";
        rc = Hyper3DAddString(h3d_file, TRUSSPOOL, &truss_poolname_id);
        if( !rc ) throw rc;

        char BEAMPOOL[] = "BEAM";
        rc = Hyper3DAddString(h3d_file, BEAMPOOL, &beam_poolname_id);
        if( !rc ) throw rc;

        char SOLID4NPOOL[] = "TETRA4";
        rc = Hyper3DAddString(h3d_file, SOLID4NPOOL, &solid4n_poolname_id);
        if( !rc ) throw rc;

        char SOLID10NPOOL[] = "TETRA10";
        rc = Hyper3DAddString(h3d_file, SOLID10NPOOL, &solid10n_poolname_id);
        if( !rc ) throw rc;

        char SOLID5NPOOL[] = "PENTA5";
        rc = Hyper3DAddString(h3d_file, SOLID5NPOOL, &solid5n_poolname_id);
        if( !rc ) throw rc;

        char SOLID6NPOOL[] = "PENTA6";
        rc = Hyper3DAddString(h3d_file, SOLID6NPOOL, &solid6n_poolname_id);
        if( !rc ) throw rc;

        char SOLID8NPOOL[] = "BRICK";
        rc = Hyper3DAddString(h3d_file, SOLID8NPOOL, &solid8n_poolname_id);
        if( !rc ) throw rc;

        char SOLID16NPOOL[] = "BRICK16";
        rc = Hyper3DAddString(h3d_file, SOLID16NPOOL, &solid16n_poolname_id);
        if( !rc ) throw rc;

        char SOLID20NPOOL[] = "BRICK20";
        rc = Hyper3DAddString(h3d_file, SOLID20NPOOL, &solid20n_poolname_id);
        if( !rc ) throw rc;

        char SPHPOOL[] = "Sph";
        rc = Hyper3DAddString(h3d_file, SPHPOOL, &sph_poolname_id);
        if( !rc ) throw rc;

        char RBODYPOOL[] = "Rbody";
        rc = Hyper3DAddString(h3d_file, RBODYPOOL, &rbody_poolname_id);
        if( !rc ) throw rc;

        char RBE2POOL[] = "Rbe2";
        rc = Hyper3DAddString(h3d_file, RBE2POOL, &rbe2_poolname_id);
        if( !rc ) throw rc;

        char RBE3POOL[] = "Rbe3";
        rc = Hyper3DAddString(h3d_file, RBE3POOL, &rbe3_poolname_id);
        if( !rc ) throw rc;

        char RWALLPOOL[] = "Rwall";
        rc = Hyper3DAddString(h3d_file, RWALLPOOL, &rwall_poolname_id);
        if( !rc ) throw rc;



    } // end of try

    catch(...) {
        Hyper3DExportClearError(h3d_file);
    }

}

void _FCALL C_H3D_REOPEN_FILE(char *name, int *size, my_real *percentage_error, int *comp_level)
{c_h3d_reopen_file_ (name,size,percentage_error,comp_level);}

void c_h3d_reopen_file__ (char *name, int *size, my_real *percentage_error, int *comp_level)
{c_h3d_reopen_file_ (name,size,percentage_error,comp_level);}

void c_h3d_reopen_file (char *name, int *size, my_real *percentage_error, int *comp_level)
{c_h3d_reopen_file_ (name,size,percentage_error,comp_level);}



/*=================================================================*/
/*        H3D_WRITE_TOC                                         */
/*=================================================================*/

void c_h3d_write_toc_()
{
    try {
        bool rc2 = Hyper3DWriteTOC(h3d_file);
        if( !rc ) throw rc;

    } // end of try

    catch(...)    {
        Hyper3DExportClearError(h3d_file);
    }
}

void _FCALL C_H3D_WRITE_TOC()
{c_h3d_write_toc_ ();}

void h3d_write_toc__ ()
{c_h3d_write_toc_ ();}

void h3d_write_toc ()
{c_h3d_write_toc_ ();}



/*==================================
/*=================================================================*/
}
/*=================================================================*/
/*        TRACE BACK   end                                         */
/*=================================================================*/

