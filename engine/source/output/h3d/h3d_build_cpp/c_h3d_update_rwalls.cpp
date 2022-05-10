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
/*        C_H3D_update_RWALLS                                     */
/*=================================================================*/
void c_h3d_update_rwalls_(int *NOM_OPT, int *LNOPT1, int *I16D, int *NPRW, int *NRWALL, int *MAX_NOD_ID,
                          my_real *XWL, my_real *YWL , my_real *ZWL, my_real *V1, my_real *V2, my_real *V3, 
                          my_real *VV1, my_real *VV2 , my_real *VV3 )
{

    try {


        float node[3]; 
        H3D_ID node_id;

        // update Elements
        H3D_ID elem_id ;
        H3D_ID RigidElem ;
        int i,j,nsn,nbelemwrite;

        unsigned int elem_count = 1;

        for(i=0;i<*NRWALL;i++)  
        {
             elem_id = NOM_OPT[*I16D + i * *LNOPT1 - 1];
             RigidElem =  NOM_OPT[*I16D + i * *LNOPT1 - 1];

                 rc = Hyper3DPositionBegin(h3d_file, 4, node_poolname_id);
                 if( !rc ) throw rc;

                 node[0] =  XWL[i] + V1[i];
                 node[1] =  YWL[i] + V2[i];
                 node[2] =  ZWL[i] + V3[i];
                 node_id = *MAX_NOD_ID+ (4 * i) + 1 ;

                 rc = Hyper3DPositionWrite(h3d_file, node_id, node, H3D_NULL_ID, H3D_NULL_ID); 
                 if( !rc ) throw rc;

                 node[0] =  XWL[i] + VV1[i];
                 node[1] =  YWL[i] + VV2[i];
                 node[2] =  ZWL[i] + VV3[i];
                 node_id = *MAX_NOD_ID+ (4 * i) + 2 ;

                 rc = Hyper3DPositionWrite(h3d_file, node_id, node, H3D_NULL_ID, H3D_NULL_ID); 
                 if( !rc ) throw rc;

                 node[0] =  XWL[i] - V1[i];
                 node[1] =  YWL[i] - V2[i];
                 node[2] =  ZWL[i] - V3[i];
                 node_id = *MAX_NOD_ID+ (4 * i) + 3 ;

                 rc = Hyper3DPositionWrite(h3d_file, node_id, node, H3D_NULL_ID, H3D_NULL_ID); 
                 if( !rc ) throw rc;

                 node[0] =  XWL[i] - VV1[i];
                 node[1] =  YWL[i] - VV2[i];
                 node[2] =  ZWL[i] - VV3[i];
                 node_id = *MAX_NOD_ID+ (4 * i) + 4 ;

                 rc = Hyper3DPositionWrite(h3d_file, node_id, node, H3D_NULL_ID, H3D_NULL_ID); 
                 if( !rc ) throw rc;

                 rc = Hyper3DPositionEnd(h3d_file);
                 if( !rc ) throw rc;

        }


    } // end of try

    catch(...) {
        Hyper3DExportClearError(h3d_file);
    }

}

void _FCALL C_H3D_UPDATE_RWALLS(int *NOM_OPT, int *LNOPT1, int *I16D, int *NPRW, int *NRWALL, int *MAX_NOD_ID,
                          my_real *XWL, my_real *YWL , my_real *ZWL, my_real *V1, my_real *V2, my_real *V3, 
                          my_real *VV1, my_real *VV2 , my_real *VV3 )
{c_h3d_update_rwalls_ (NOM_OPT, LNOPT1, I16D, NPRW, NRWALL, MAX_NOD_ID,
                          XWL, YWL, ZWL, V1, V2, V3, VV1, VV2 , VV3 );}

void c_h3d_update_rwalls__ (int *NOM_OPT, int *LNOPT1, int *I16D, int *NPRW, int *NRWALL, int *MAX_NOD_ID,
                          my_real *XWL, my_real *YWL , my_real *ZWL, my_real *V1, my_real *V2, my_real *V3, 
                          my_real *VV1, my_real *VV2 , my_real *VV3 )
{c_h3d_update_rwalls_ (NOM_OPT, LNOPT1, I16D, NPRW, NRWALL, MAX_NOD_ID,
                          XWL, YWL, ZWL, V1, V2, V3, VV1, VV2 , VV3 );}

void c_h3d_update_rwalls (int *NOM_OPT, int *LNOPT1, int *I16D, int *NPRW, int *NRWALL, int *MAX_NOD_ID,
                          my_real *XWL, my_real *YWL , my_real *ZWL, my_real *V1, my_real *V2, my_real *V3, 
                          my_real *VV1, my_real *VV2 , my_real *VV3 )
{c_h3d_update_rwalls_ (NOM_OPT, LNOPT1, I16D, NPRW, NRWALL, MAX_NOD_ID,
                          XWL, YWL, ZWL, V1, V2, V3, VV1, VV2 , VV3 );}

}
