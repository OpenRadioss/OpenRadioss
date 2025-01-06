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
/*        C_H3D_UPDATE_H3DFILE_NODAL_VECTOR                       */
/*=================================================================*/

void c_h3d_update_nodal_vector_(my_real *TT,int *IH3D, int *ITAB, int *NUMNOD, my_real *FUNC ,int *ID_NODE,int *CPT_DATATYPE, int *IS_WRITTEN)
{
    int i;
    int offset;
    H3D_ID node_id;
    H3D_ID comp_id;
//
    // initialize 

    try {
        // create Subcase (Loadcase)
        unsigned int       max_sims = 10;
        unsigned int      sub_count = 1;
        float node_result[] = { 0.0f, 0.0f, 0.0f };

        // create Result Data sets
        unsigned int num_corners = 0;
        unsigned int   num_modes = 0;
        bool             complex = false;
        float value[1] ;

        sim_idx = *IH3D;


        rc = Hyper3DDatasetBegin(h3d_file, *NUMNOD, sim_idx, subcase_id, H3D_DS_NODE, 
                                        H3D_DS_VECTOR, num_corners, num_modes, *CPT_DATATYPE, 
                                        0, node_poolname_id, complex); 
        if( !rc ) throw rc;

        offset = 0;

        for( i = 0; i < *NUMNOD; i++ ) 
        {
          if(IS_WRITTEN[i] == 1) 
          { 
            node_id = ID_NODE[i];
            node_result[0] = FUNC[3*i];
            node_result[1] = FUNC[3*i+1];
            node_result[2] = FUNC[3*i+2];
            rc = Hyper3DDatasetWrite(h3d_file, node_id, &node_result[0]);
            if( !rc ) throw rc;
          }
        }

        rc = Hyper3DDatasetEnd(h3d_file);
        if( !rc ) throw rc;



    } // end of try

    catch(...)    {
        Hyper3DExportClearError(h3d_file);
    }
}

void _FCALL C_H3D_UPDATE_NODAL_VECTOR(my_real *TT,int *IH3D, int *ITAB, int *NUMNOD, my_real *FUNC ,int *ID_NODE,int *CPT_DATATYPE, int *IS_WRITTEN)
{c_h3d_update_nodal_vector_ (TT,IH3D,ITAB,NUMNOD,FUNC,ID_NODE,CPT_DATATYPE,IS_WRITTEN);}

void c_h3d_update_nodal_vector__ (my_real *TT,int *IH3D, int *ITAB, int *NUMNOD, my_real *FUNC ,int *ID_NODE,int *CPT_DATATYPE, int *IS_WRITTEN)
{c_h3d_update_nodal_vector_ (TT,IH3D,ITAB,NUMNOD,FUNC,ID_NODE,CPT_DATATYPE,IS_WRITTEN);}

void c_h3d_update_nodal_vector (my_real *TT,int *IH3D, int *ITAB, int *NUMNOD, my_real *FUNC ,int *ID_NODE,int *CPT_DATATYPE, int *IS_WRITTEN)
{c_h3d_update_nodal_vector_ (TT,IH3D,ITAB,NUMNOD,FUNC,ID_NODE,CPT_DATATYPE,IS_WRITTEN);}

}
