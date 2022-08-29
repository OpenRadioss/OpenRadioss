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

#define _FCALL 

#include "h3d_values.h"

extern "C" 
/*=================================================================*/
{

/*=================================================================*/
/*        C_H3D_CREATE_DISPLACEMENT_DATATYPE                       */
/*=================================================================*/

void c_h3d_create_displacement_datatype_()
{
    try {
        // create result data types

       
        rc = Hyper3DDatatypeBegin(h3d_file, dt_count);
        if( !rc ) throw rc;

        pool_count = 1;
        dt_id = 1;     // the displacement data type

        rc = Hyper3DDatatypeWrite(h3d_file, "Animation", dt_id, H3D_DS_NONE, 
                                    H3D_DS_UNKNOWN, pool_count);
        if( !rc ) throw rc;

        dt_id ++;  
        pool_count = 1; 
        rc = Hyper3DDatatypeWrite(h3d_file, "Displacement", dt_id, H3D_DS_VECTOR, 
                                    H3D_DS_NODE, pool_count);
        if( !rc ) throw rc;
        rc = Hyper3DDatatypePools(h3d_file, dt_id, node_poolname_id, layer_count, 
                                    layername_ids, has_corners, tensor_type, poisson);
        if( !rc ) throw rc;

        rc = Hyper3DDatatypeEnd(h3d_file);
        if( !rc ) throw rc;
/*
        // create Simulations
        H3D_SIM_IDX sim_idx;
        rc = Hyper3DSimulationBegin(h3d_file, max_sims, subcase_id);
        if( !rc ) throw rc;

        for( sim_idx=0; sim_idx < max_sims; sim_idx++ ) {
            char name[32];
            snprintf(name, sizeof(name), "Increment = %ld", sim_idx);
            rc = Hyper3DSimulationWrite(h3d_file, sim_idx, name, (float)sim_idx);
            if( !rc ) throw rc;
        }
        rc = Hyper3DSimulationEnd(h3d_file); 
        if( !rc ) throw rc;
*/

    } // end of try

    catch(...) {
        Hyper3DExportClearError(h3d_file);
    }

}




void _FCALL C_H3D_CREATE_DISPLACEMENT_DATATYPE()
{c_h3d_create_displacement_datatype_ ();}

void c_h3d_create_displacement_datatype__ ()
{c_h3d_create_displacement_datatype_ ();}

void c_h3d_create_displacement_datatype ()
{c_h3d_create_displacement_datatype_ ();}

}
