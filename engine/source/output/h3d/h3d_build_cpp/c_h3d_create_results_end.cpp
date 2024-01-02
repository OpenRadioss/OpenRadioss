//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2024 Altair Engineering Inc.
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
/*        C_H3D_CREATE_RESULTS_END                                 */
/*=================================================================*/

void c_h3d_create_results_end_()
{

    unsigned int * datatype_ids = new unsigned int [dt_count];
    try {

        // create Result defaults
        unsigned int  res_count = 1;
        unsigned int asys_count = 1;

        rc = Hyper3DResultBegin(h3d_file, res_count);
        if( !rc ) throw rc;
        rc = Hyper3DResultWrite(h3d_file, "Result", H3D_SM_MIDPOINT, asys_count);

        if( !rc ) throw rc;
        rc = Hyper3DResultAddSystem(h3d_file, node_poolname_id, 
                                        H3D_DS_ANALYSIS, H3D_POOL_ELEMENT); 

        if( !rc ) throw rc;
        rc = Hyper3DResultAddSystem(h3d_file, beam_poolname_id, 
                                        H3D_DS_GLOBAL, H3D_POOL_ELEMENT);

        if( !rc ) throw rc;
        rc = Hyper3DResultAddSystem(h3d_file, spring_poolname_id, 
                                        H3D_DS_GLOBAL, H3D_POOL_ELEMENT);

        if( !rc ) throw rc;
        rc = Hyper3DResultAddSystem(h3d_file, truss_poolname_id, 
                                        H3D_DS_GLOBAL, H3D_POOL_ELEMENT);

        if( !rc ) throw rc;
        rc = Hyper3DResultAddSystem(h3d_file, shell_poolname_id, 
                                        H3D_DS_ANALYSIS, H3D_POOL_ELEMENT); 

        if( !rc ) throw rc;
        rc = Hyper3DResultAddSystem(h3d_file, sh4n_poolname_id, 
                                        H3D_DS_ANALYSIS, H3D_POOL_ELEMENT); 
        if( !rc ) throw rc;

        rc = Hyper3DResultAddSystem(h3d_file, sh3n_poolname_id, 
                                        H3D_DS_ANALYSIS, H3D_POOL_ELEMENT); 
        if( !rc ) throw rc;

        rc = Hyper3DResultAddSystem(h3d_file, solid_poolname_id, 
                                        H3D_DS_GLOBAL, H3D_POOL_ELEMENT);
        if( !rc ) throw rc;  

        rc = Hyper3DResultAddSystem(h3d_file, sphcell_poolname_id, 
                                        H3D_DS_GLOBAL, H3D_POOL_ELEMENT);
        if( !rc ) throw rc; 

        rc = Hyper3DResultAddSystem(h3d_file, quad_poolname_id, 
                                        H3D_DS_ANALYSIS, H3D_POOL_ELEMENT);
        if( !rc ) throw rc; 

        rc = Hyper3DResultAddSystem(h3d_file, skin_poolname_id, 
                                        H3D_DS_ANALYSIS, H3D_POOL_ELEMENT);
        if( !rc ) throw rc; 


        if( !rc ) throw rc;
        rc = Hyper3DResultEnd(h3d_file);
        if( !rc ) throw rc;


        // create Subcase (Loadcase)
        for( int i=0; i < dt_count; i++ ) 
        {
            datatype_ids[i] = i + 1;
        }

        unsigned int      sub_count = 1;
        rc = Hyper3DSimSubcaseBegin(h3d_file, sub_count);
        if( !rc ) throw rc;
        rc = Hyper3DSimSubcaseWrite(h3d_file, "Loadcase 1", subcase_id, H3D_NONLINEAR, 
                                    dt_count, datatype_ids, H3D_NODAL_DISPLACEMENT); 
        if( !rc ) throw rc;

        unsigned int   anim_grp_count = 1;
        unsigned int grp_datatype_ids = 1;
        unsigned int num_dts_per_grp  = 3;

        datatype_ids[0] = 2;
        datatype_ids[1] = dt_count-1;
        datatype_ids[2] = dt_count;

        rc = Hyper3DSimSubcaseAnimationGroups(h3d_file, subcase_id, anim_grp_count, 
                            &grp_datatype_ids, &num_dts_per_grp, &datatype_ids[0]);  
        if( !rc ) throw rc;

        rc = Hyper3DSimSubcaseEnd(h3d_file); 
        if( !rc ) throw rc;

    } // end of try

    catch(...) {
        Hyper3DExportClearError(h3d_file);
    }
    delete [] datatype_ids;

    bool rc2 = Hyper3DWriteTOC(h3d_file);
}




void _FCALL C_H3D_CREATE_RESULTS_END()
{c_h3d_create_results_end_ ();}

void c_h3d_create_results_end__ ()
{c_h3d_create_results_end_ ();}

void c_h3d_create_results_end ()
{c_h3d_create_results_end_ ();}
}
