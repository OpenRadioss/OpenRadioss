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
/*     C_H3D_CREATE_NODES                                          */
/*=================================================================*/
void c_h3d_create_nodes_(int *ITAB, int *NUMNOD, my_real *X, int *TAGNOD, my_real *D)
{
    try {
       
        float node[3]; 
        int i;
        H3D_ID node_id;
//


 
        if( *NUMNOD  != 0 ) 
        {
            rc = Hyper3DPositionBegin(h3d_file, *NUMNOD , node_poolname_id);
            if( !rc ) throw rc;

            for(i=0;i<*NUMNOD;i++)  
            {
                if( TAGNOD[i] == 1 ) 
                {
             	     node[0] = X[3*i ]    - D[3*i ];
             	     node[1] = X[3*i + 1] - D[3*i + 1];
             	     node[2] = X[3*i + 2] - D[3*i + 2];
             	     node_id = ITAB[i];

             	     if (node_id != 0) rc = Hyper3DPositionWrite(h3d_file, node_id, node, H3D_NULL_ID, H3D_NULL_ID); 
             	     if( !rc ) throw rc;
             	}
            }

            rc = Hyper3DPositionEnd(h3d_file);
            if( !rc ) throw rc;
        }


    } // end of try

    catch(...) {
        Hyper3DExportClearError(h3d_file);
    }
}

void _FCALL C_H3D_CREATE_NODES(int *ITAB, int *NUMNOD, my_real *X, int *TAGNOD, my_real *D)
{c_h3d_create_nodes_ (ITAB,NUMNOD,X,TAGNOD,D);}

void c_h3d_create_nodes__ (int *ITAB, int *NUMNOD, my_real *X, int *TAGNOD, my_real *D)
{c_h3d_create_nodes_ (ITAB,NUMNOD,X,TAGNOD,D);}

void c_h3d_create_nodes (int *ITAB, int *NUMNOD, my_real *X, int *TAGNOD, my_real *D)
{c_h3d_create_nodes_ (ITAB,NUMNOD,X,TAGNOD,D);}

}
