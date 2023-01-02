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
/*        C_H3D_CREATE_SPH                                    */
/*=================================================================*/
void c_h3d_create_sph_(int *ITAB, int *NUMNOD, int *KXSP, int *NISP, int *NUMSPH, int *IPARTSP, int *IPART, int *LIPART1,
                       my_real *X, int *H3D_PART)
{
    unsigned int * nbelems = new unsigned int [*NUMSPH];
    try {


        // create Elements
        unsigned int conn[1] ;
        H3D_ID elem_id ;
        int i,n1,n2,n3,n4,n5,n6,n7,n8;
        int cpt_elem,nbelemwrite;
        int elem_nodes = 0;
        int elem_nodes_m1 = 0;
        int j = 0;

        char NODEPOOL[] = "Nodes";
        rc = Hyper3DAddString(h3d_file, NODEPOOL, &node_poolname_id);
        if( !rc ) throw rc;

        char SPHCELLPOOL[] = "SPHCELL";
        rc = Hyper3DAddString(h3d_file, SPHCELLPOOL, &sphcell_poolname_id);
        if( !rc ) throw rc;

        char SPHPOOL[] = "Sph";
        rc = Hyper3DAddString(h3d_file, SPHPOOL, &sph_poolname_id);
        if( !rc ) throw rc;

        unsigned int elem_count = 1;



        comp_id = 0;
        j = 0;
       
        float node[3];
        H3D_ID node_id;
//
 
        if(*NUMSPH > 0) 
        {




        comp_id = 0;
        j = 0;
        for(i=0;i<*NUMSPH;i++)  nbelems[i] = 0;

        for(i=0;i<*NUMSPH;i++)
        {  
        if(H3D_PART[IPARTSP[i] - 1] == 1)
          {
             if(IPART[*LIPART1 * (IPARTSP[i] - 1) + 3] != comp_id)
             {
                  comp_id = IPART[*LIPART1 * (IPARTSP[i] - 1) + 3];
                  j = i;
             }
             nbelems[j] = nbelems[j] + 1;
          }
        }

        comp_id = 0;
        cpt_elem = 0;
        nbelemwrite = 0;

        for(i=0;i<*NUMSPH;i++)  
        {  
        if(H3D_PART[IPARTSP[i] - 1] == 1)
          {

             conn[0] = KXSP[*NISP * i + 2] ;
             elem_id = KXSP[*NISP * i + *NISP - 1] ;

             if(nbelems[i] != 0)
             {
                  cpt_elem = 0;
                  nbelemwrite = nbelems[i];
                  comp_id = IPART[*LIPART1 * (IPARTSP[i] - 1) + 3] ;

                  rc = Hyper3DElementBegin(h3d_file, nbelemwrite, sphcell_poolname_id, 
             		   H3D_ELEM_CONFIG_MASS, comp_id, 
             		   sphcell_poolname_id, node_poolname_id);
                  if( !rc ) throw rc;
             }
             cpt_elem++;

             rc = Hyper3DElementWrite(h3d_file, elem_id, conn);
             if( !rc ) throw rc;


             if (cpt_elem == nbelemwrite)
             {
                  rc = Hyper3DElementEnd(h3d_file);
                  if( !rc ) throw rc;
             }
          }
       }


        }

         


    } // end of try

    catch(...) {
        Hyper3DExportClearError(h3d_file);
    }
    delete [] nbelems;

//
}

void _FCALL C_H3D_CREATE_SPH(int *ITAB, int *NUMNOD, int *KXSP, int *NISP, int *NUMSPH, int *IPARTSP, int *IPART, int *LIPART1,
                       my_real *X, int *H3D_PART)
{c_h3d_create_sph_ (ITAB,NUMNOD,KXSP,NISP,NUMSPH,IPARTSP,IPART,LIPART1,X,H3D_PART);}

void c_h3d_create_sph__ (int *ITAB, int *NUMNOD, int *KXSP, int *NISP, int *NUMSPH, int *IPARTSP, int *IPART, int *LIPART1,
                       my_real *X, int *H3D_PART)
{c_h3d_create_sph_ (ITAB,NUMNOD,KXSP,NISP,NUMSPH,IPARTSP,IPART,LIPART1,X,H3D_PART);}

void c_h3d_create_sph (int *ITAB, int *NUMNOD, int *KXSP, int *NISP, int *NUMSPH, int *IPARTSP, int *IPART, int *LIPART1,
                       my_real *X, int *H3D_PART)
{c_h3d_create_sph_ (ITAB,NUMNOD,KXSP,NISP,NUMSPH,IPARTSP,IPART,LIPART1,X,H3D_PART);}
}
