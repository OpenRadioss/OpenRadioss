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
/*        C_H3D_CREATE_beams                                     */
/*=================================================================*/
void c_h3d_create_beams_(int *ITAB, int *NUMNOD, int *IXP, int *NIXP, int *NUMELP, int *IPARTP, int *IPART, int *LIPART1, int *H3D_PART)
{
    try {

        // create Elements
        unsigned int conn[2] ;
        H3D_ID elem_id ;
        int i;

        unsigned int elem_count = 1;


        char BEAMPOOL[] = "BEAM";
        rc = Hyper3DAddString(h3d_file, BEAMPOOL, &beam_poolname_id);
        if( !rc ) throw rc;



        for(i=0;i<*NUMELP;i++)  
        {
            if(H3D_PART[IPARTP[i] - 1] == 1) 
            {
                 elem_id = IXP[*NIXP * i + *NIXP-1];
                 conn[0] = IXP[*NIXP * i + 1];
                 conn[1] = IXP[*NIXP * i + 2];
                 comp_id = IPART[*LIPART1 * (IPARTP[i] - 1) + 3];

                 rc = Hyper3DElementBegin(h3d_file, elem_count, beam_poolname_id, 
                                    H3D_ELEM_CONFIG_ROD, comp_id, 
                                    onedelem_poolname_id, node_poolname_id);
                 if( !rc ) throw rc;
                 rc = Hyper3DElementWrite(h3d_file, elem_id, conn);
                 if( !rc ) throw rc;
                 rc = Hyper3DElementEnd(h3d_file);
                 if( !rc ) throw rc;
            }
        }


    } // end of try

    catch(...) {
        Hyper3DExportClearError(h3d_file);
    }

}

void _FCALL C_H3D_CREATE_BEAMS(int *ITAB, int *NUMNOD, int *IXP, int *NIXP, int *NUMELP, int *IPARTP, int *IPART, int *LIPART1, int *H3D_PART)
{c_h3d_create_beams_ (ITAB,NUMNOD,IXP,NIXP,NUMELP,IPARTP,IPART,LIPART1,H3D_PART);}

void c_h3d_create_beams__ (int *ITAB, int *NUMNOD, int *IXP, int *NIXP, int *NUMELP, int *IPARTP, int *IPART, int *LIPART1, int *H3D_PART)
{c_h3d_create_beams_ (ITAB,NUMNOD,IXP,NIXP,NUMELP,IPARTP,IPART,LIPART1,H3D_PART);}

void c_h3d_create_beams (int *ITAB, int *NUMNOD, int *IXP, int *NIXP, int *NUMELP, int *IPARTP, int *IPART, int *LIPART1, int *H3D_PART)
{c_h3d_create_beams_ (ITAB,NUMNOD,IXP,NIXP,NUMELP,IPARTP,IPART,LIPART1,H3D_PART);}
}
