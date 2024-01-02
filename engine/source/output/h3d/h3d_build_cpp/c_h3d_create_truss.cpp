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
/*        C_H3D_CREATE_TRUSS                                     */
/*=================================================================*/
void c_h3d_create_truss_(int *ITAB, int *NUMNOD, int *IXT, int *NIXT, int *NUMELT, int *IPARTT, int *IPART, int *LIPART1, int *H3D_PART)
{
    try {

        // create Elements
        unsigned int conn[2] ;
        H3D_ID elem_id ;
        int i;


        char TRUSSPOOL[] = "TRUSS";
        rc = Hyper3DAddString(h3d_file, TRUSSPOOL, &truss_poolname_id);
        if( !rc ) throw rc;

        unsigned int elem_count = 1;




        for(i=0;i<*NUMELT;i++)  
        {
            if(H3D_PART[IPARTT[i] - 1] == 1) 
            {
                 elem_id = IXT[*NIXT * i + *NIXT-1];
                 conn[0] = IXT[*NIXT * i + 1];
                 conn[1] = IXT[*NIXT * i + 2];
                 comp_id = IPART[*LIPART1 * (IPARTT[i] - 1) + 3];

                 rc = Hyper3DElementBegin(h3d_file, elem_count, truss_poolname_id, 
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

void _FCALL C_H3D_CREATE_TRUSS(int *ITAB, int *NUMNOD, int *IXT, int *NIXT, int *NUMELT, int *IPARTT, int *IPART, int *LIPART1, int *H3D_PART)
{c_h3d_create_truss_ (ITAB,NUMNOD,IXT,NIXT,NUMELT,IPARTT,IPART,LIPART1,H3D_PART);}

void c_h3d_create_truss__ (int *ITAB, int *NUMNOD, int *IXT, int *NIXT, int *NUMELT, int *IPARTT, int *IPART, int *LIPART1, int *H3D_PART)
{c_h3d_create_truss_ (ITAB,NUMNOD,IXT,NIXT,NUMELT,IPARTT,IPART,LIPART1,H3D_PART);}

void c_h3d_create_truss (int *ITAB, int *NUMNOD, int *IXT, int *NIXT, int *NUMELT, int *IPARTT, int *IPART, int *LIPART1, int *H3D_PART)
{c_h3d_create_truss_ (ITAB,NUMNOD,IXT,NIXT,NUMELT,IPARTT,IPART,LIPART1,H3D_PART);}
}
