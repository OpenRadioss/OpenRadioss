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
/*        C_H3D_CREATE_quads                                     */
/*=================================================================*/
void c_h3d_create_quads_(int *ITAB, int *NUMNOD, int *IPART, int *LIPART1,int *H3D_PART,
                           int *IXQ, int *NIXQ,int *NUMELQ,int *IPARTQ)
{
    unsigned int * nbelems = new unsigned int [*NUMELQ];
    try {


        // create Elements
        unsigned int conn4[4] ;
        H3D_ID elem_id ;
        int i,n1,n2,n3,n4,n5,n6,n7,n8,n9,n10;
        int cpt_elem,nbelemwrite;
        int elem_nodes = 0;
        int elem_nodes_m1 = 0;
        int j = 0;

        char QUADPOOL[] = "QUAD";
        rc = Hyper3DAddString(h3d_file, QUADPOOL, &quad_poolname_id);
        if( !rc ) throw rc;


        unsigned int elem_count = 1;

// QUAD

        for(i=0;i<*NUMELQ;i++)  nbelems[i] = 0;

        comp_id = 0;

        for(i=0;i<*NUMELQ;i++)
          {
          if(H3D_PART[IPARTQ[i] - 1] == 1)
            {
             elem_nodes = 4;

             if(IPART[*LIPART1 * (IPARTQ[i] - 1) + 3] != comp_id)
             {
             	comp_id = IPART[*LIPART1 * (IPARTQ[i] - 1) + 3];
                j = i;
             }
             nbelems[j] = nbelems[j] + 1;
             }
          }

        comp_id = 0;
        cpt_elem = 0;
        nbelemwrite = 0;

        for(i=0;i<*NUMELQ;i++)  
        {
            if(H3D_PART[IPARTQ[i] - 1] == 1)
            {  
             elem_id = IXQ[*NIXQ * i + *NIXQ - 1];
             comp_id = IPART[*LIPART1 * (IPARTQ[i] - 1) + 3];

             conn4[0] = IXQ[*NIXQ * i + 1];  
             conn4[1] = IXQ[*NIXQ * i + 2];
             conn4[2] = IXQ[*NIXQ * i + 3];
             conn4[3] = IXQ[*NIXQ * i + 4];



             if(nbelems[i] != 0)
             {
             	  cpt_elem = 0;
             	  nbelemwrite = nbelems[i];
             	  comp_id = IPART[*LIPART1 * (IPARTQ[i] - 1) + 3];

             	  rc = Hyper3DElementBegin(h3d_file, nbelemwrite, quad_poolname_id, 
             			H3D_ELEM_CONFIG_QUAD4, comp_id, 
             			quad_poolname_id, node_poolname_id);
             	  if( !rc ) throw rc;
             }


             cpt_elem++;
             rc = Hyper3DElementWrite(h3d_file, elem_id, conn4);
             if( !rc ) throw rc;




             if (cpt_elem == nbelemwrite)
             {
             	  rc = Hyper3DElementEnd(h3d_file);
             	  if( !rc ) throw rc;
             }

        }
      }



    } // end of try

    catch(...) {
        Hyper3DExportClearError(h3d_file);
    }
    delete []  nbelems;

//
}

void _FCALL C_H3D_CREATE_QUADS(int *ITAB, int *NUMNOD, int *IPART, int *LIPART1,int *H3D_PART,
                           int *IXQ, int *NIXQ,int *NUMELQ,int *IPARTQ)
{c_h3d_create_quads_ (ITAB, NUMNOD, IPART, LIPART1, H3D_PART, IXQ, NIXQ, NUMELQ, IPARTQ);}

void c_h3d_create_quads__ (int *ITAB, int *NUMNOD, int *IPART, int *LIPART1,int *H3D_PART,
                           int *IXQ, int *NIXQ,int *NUMELQ,int *IPARTQ)
{c_h3d_create_quads_ (ITAB, NUMNOD, IPART, LIPART1, H3D_PART, IXQ, NIXQ, NUMELQ, IPARTQ);}

void c_h3d_create_quads (int *ITAB, int *NUMNOD, int *IPART, int *LIPART1,int *H3D_PART,
                           int *IXQ, int *NIXQ,int *NUMELQ,int *IPARTQ)
{c_h3d_create_quads_ (ITAB, NUMNOD, IPART, LIPART1, H3D_PART, IXQ, NIXQ, NUMELQ, IPARTQ);}
}
