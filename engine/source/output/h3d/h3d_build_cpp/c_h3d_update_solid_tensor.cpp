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
/*        C_H3D_UPDATE_SOLID_TENSOR                                */
/*=================================================================*/

void c_h3d_update_solid_tensor_(my_real *TT,int *IH3D, int *ITAB, int *NUMNOD, int *IXS, int *NIXS, int *NUMELS, int *IPARTS,
                           my_real *FUNC ,int *ID_ELEM,int *CPT_DATATYPE,
                           int *ITY_ELEM, int *NUMELQ , int *NUMELT , int *NUMELP , int *NUMELR, int *IS_WRITTEN,
                           my_real *FUNC_CORNER, int *IS_CORNER_DATA,int *ISOLNOD,int *MAXNOD)
{
    int i,j,element_nb_nodes;
    int offset;
    unsigned int NNOD = *MAXNOD;
    H3D_ID elem_id;
    H3D_ID comp_id;
//
    // initialize 

    try {
        // create Subcase (Loadcase)
        unsigned int       max_sims = 10;
        unsigned int      sub_count = 1;
        float elem_result[6] = { 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
        float elem_result1[6*NNOD]; 
        for( i = 0; i < 6*NNOD; i++ ) 
        {
             elem_result1[i] = 0.0f;
        }

        // create Result Data sets
        unsigned int num_corners = 0;
        unsigned int   num_modes = 0;
        bool             complex = false;
        float value[1] ;

        sim_idx = *IH3D;

        if(*IS_CORNER_DATA == 1)
        {
             if(*NUMELS != 0)
             {
                  rc = Hyper3DDatasetBegin(h3d_file, *NUMELS, sim_idx, subcase_id, H3D_DS_ELEM, 
                                              H3D_DS_TENSOR3D, NNOD  , num_modes, *CPT_DATATYPE, 
                                              NULL, solid_poolname_id, complex); 
                  if( !rc ) throw rc;

                  for( i = 0; i < *NUMELS; i++ ) 
                  {
                       if( ITY_ELEM[i] == 1  && IS_WRITTEN[i] == 1) 
                       { 
                            elem_id = ID_ELEM[i];
                            element_nb_nodes = ISOLNOD[i];
// 4 nodes & 10 nodes
                            if( ISOLNOD[i] == 4) 
                            { 
                                for( j = 0; j < 4; j++ ) 
                                {
                                     elem_result1[j*6]   = FUNC_CORNER[6*(NNOD*(i)+j)];
                                     elem_result1[j*6+1] = FUNC_CORNER[6*(NNOD*(i)+j)+1];
                                     elem_result1[j*6+2] = FUNC_CORNER[6*(NNOD*(i)+j)+2];
                                     elem_result1[j*6+3] = FUNC_CORNER[6*(NNOD*(i)+j)+3];
                                     elem_result1[j*6+4] = FUNC_CORNER[6*(NNOD*(i)+j)+4];
                                     elem_result1[j*6+5] = FUNC_CORNER[6*(NNOD*(i)+j)+5];
                                }
                            } 
// 6 nodes
                            if( ISOLNOD[i] == 6) 
                            { 
                                for( j = 0; j < 6; j++ ) 
                                {
                                     elem_result1[j*6]   = FUNC_CORNER[6*(NNOD*(i)+j)];
                                     elem_result1[j*6+1] = FUNC_CORNER[6*(NNOD*(i)+j)+1];
                                     elem_result1[j*6+2] = FUNC_CORNER[6*(NNOD*(i)+j)+2];
                                     elem_result1[j*6+3] = FUNC_CORNER[6*(NNOD*(i)+j)+3];
                                     elem_result1[j*6+4] = FUNC_CORNER[6*(NNOD*(i)+j)+4];
                                     elem_result1[j*6+5] = FUNC_CORNER[6*(NNOD*(i)+j)+5];
                                }
                            } 
// 8 nodes 
                            if( ISOLNOD[i] == 8) 
                            { 
                                for( j = 0; j < 8; j++ ) 
                                {
                                     elem_result1[j*6]   = FUNC_CORNER[6*(NNOD*(i)+j)];
                                     elem_result1[j*6+1] = FUNC_CORNER[6*(NNOD*(i)+j)+1];
                                     elem_result1[j*6+2] = FUNC_CORNER[6*(NNOD*(i)+j)+2];
                                     elem_result1[j*6+3] = FUNC_CORNER[6*(NNOD*(i)+j)+3];
                                     elem_result1[j*6+4] = FUNC_CORNER[6*(NNOD*(i)+j)+4];
                                     elem_result1[j*6+5] = FUNC_CORNER[6*(NNOD*(i)+j)+5];
                                }
                            } 

// 10 nodes
                            if( ISOLNOD[i] == 10) 
                            { 
                                for( j = 0; j < 10; j++ ) 
                                {
                                     elem_result1[j*6]   = FUNC_CORNER[6*(NNOD*(i)+j)];
                                     elem_result1[j*6+1] = FUNC_CORNER[6*(NNOD*(i)+j)+1];
                                     elem_result1[j*6+2] = FUNC_CORNER[6*(NNOD*(i)+j)+2];
                                     elem_result1[j*6+3] = FUNC_CORNER[6*(NNOD*(i)+j)+3];
                                     elem_result1[j*6+4] = FUNC_CORNER[6*(NNOD*(i)+j)+4];
                                     elem_result1[j*6+5] = FUNC_CORNER[6*(NNOD*(i)+j)+5];
                                }
                            } 
// 16 nodes : not available for the moment
/*
                            if( ISOLNOD[i] == 16) 
                            { 
                                for( j = 0; j < 16; j++ ) 
                                {
                                     elem_result1[j*6]   = FUNC_CORNER[6*(20*(i)+j)];
                                     elem_result1[j*6+1] = FUNC_CORNER[6*(20*(i)+j)+1];
                                     elem_result1[j*6+2] = FUNC_CORNER[6*(20*(i)+j)+2];
                                     elem_result1[j*6+3] = FUNC_CORNER[6*(20*(i)+j)+3];
                                     elem_result1[j*6+4] = FUNC_CORNER[6*(20*(i)+j)+4];
                                     elem_result1[j*6+5] = FUNC_CORNER[6*(20*(i)+j)+5];
                                }
                            
                            }
*/                           
// 20 nodes
/*

                            if( ISOLNOD[i] == 20) 
                            { 
                                for( j = 0; j < 20; j++ ) 
                                {
                                     elem_result1[j*6]   = FUNC_CORNER[6*(20*(i)+j)];
                                     elem_result1[j*6+1] = FUNC_CORNER[6*(20*(i)+j)+1];
                                     elem_result1[j*6+2] = FUNC_CORNER[6*(20*(i)+j)+2];
                                     elem_result1[j*6+3] = FUNC_CORNER[6*(20*(i)+j)+3];
                                     elem_result1[j*6+4] = FUNC_CORNER[6*(20*(i)+j)+4];
                                     elem_result1[j*6+5] = FUNC_CORNER[6*(20*(i)+j)+5];
                                }
                            }
*/                           
                        rc = Hyper3DDatasetWrite(h3d_file, elem_id, &elem_result1[0]);
                       }
                  }
                  rc = Hyper3DDatasetEnd(h3d_file);
                  if( !rc ) throw rc;
             }
        }

        if(*NUMELS != 0)
        {
            rc = Hyper3DDatasetBegin(h3d_file, *NUMELS, sim_idx, subcase_id, H3D_DS_ELEM, 
                                        H3D_DS_TENSOR3D, num_corners, num_modes, *CPT_DATATYPE, 
                                        NULL, solid_poolname_id, complex); 
            if( !rc ) throw rc;

     	    for( i = 0; i < *NUMELS; i++ ) 
     	    {
     	      if( ITY_ELEM[i] == 1  && IS_WRITTEN[i] == 1) 
     	      { 
     	 	elem_id = ID_ELEM[i];
     	 	elem_result[0] = FUNC[6*i];
     	 	elem_result[1] = FUNC[6*i+1];
     	 	elem_result[2] = FUNC[6*i+2];
     	 	elem_result[3] = FUNC[6*i+3];
     	 	elem_result[4] = FUNC[6*i+4];
     	 	elem_result[5] = FUNC[6*i+5];
     	 	rc = Hyper3DDatasetWrite(h3d_file, elem_id, &elem_result[0]);
                IS_WRITTEN[i] = 0;
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

void _FCALL C_H3D_UPDATE_SOLID_TENSOR(my_real *TT,int *IH3D, int *ITAB, int *NUMNOD, int *IXS, int *NIXS, int *NUMELS, int *IPARTS,
                           my_real *FUNC ,int *ID_ELEM,int *CPT_DATATYPE,
                           int *ITY_ELEM, int *NUMELQ , int *NUMELT , int *NUMELP , int *NUMELR, int *IS_WRITTEN,
                           my_real *FUNC_CORNER, int *IS_CORNER_DATA,int *ISOLNOD,int *MAXNOD)
{c_h3d_update_solid_tensor_ (TT,IH3D,ITAB,NUMNOD,IXS,NIXS,NUMELS,IPARTS,FUNC,ID_ELEM,CPT_DATATYPE,ITY_ELEM,
                         NUMELQ,NUMELT,NUMELP,NUMELR,IS_WRITTEN,FUNC_CORNER,IS_CORNER_DATA,ISOLNOD, MAXNOD);}

void c_h3d_update_solid_tensor__ (my_real *TT,int *IH3D, int *ITAB, int *NUMNOD, int *IXS, int *NIXS, int *NUMELS, int *IPARTS,
                           my_real *FUNC ,int *ID_ELEM,int *CPT_DATATYPE,
                           int *ITY_ELEM, int *NUMELQ , int *NUMELT , int *NUMELP , int *NUMELR, int *IS_WRITTEN,
                           my_real *FUNC_CORNER, int *IS_CORNER_DATA,int *ISOLNOD,int *MAXNOD)
{c_h3d_update_solid_tensor_ (TT,IH3D,ITAB,NUMNOD,IXS,NIXS,NUMELS,IPARTS,FUNC,ID_ELEM,CPT_DATATYPE,ITY_ELEM,
                         NUMELQ,NUMELT,NUMELP,NUMELR,IS_WRITTEN,FUNC_CORNER,IS_CORNER_DATA,ISOLNOD,MAXNOD);}

void c_h3d_update_solid_tensor (my_real *TT,int *IH3D, int *ITAB, int *NUMNOD, int *IXS, int *NIXS, int *NUMELS, int *IPARTS,
                           my_real *FUNC ,int *ID_ELEM,int *CPT_DATATYPE,
                           int *ITY_ELEM, int *NUMELQ , int *NUMELT , int *NUMELP , int *NUMELR, int *IS_WRITTEN,
                           my_real *FUNC_CORNER, int *IS_CORNER_DATA,int *ISOLNOD,int *MAXNOD)
{c_h3d_update_solid_tensor_ (TT,IH3D,ITAB,NUMNOD,IXS,NIXS,NUMELS,IPARTS,FUNC,ID_ELEM,CPT_DATATYPE,ITY_ELEM,
                           NUMELQ,NUMELT,NUMELP,NUMELR,IS_WRITTEN,FUNC_CORNER,IS_CORNER_DATA,ISOLNOD,MAXNOD);}


}
