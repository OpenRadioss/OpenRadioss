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

extern "C" 
/*=================================================================*/
{

/*=================================================================*/
/*        C_H3D_CREATE_SOLID8N                                     */
/*=================================================================*/
void c_h3d_create_solid8n_(int *ITAB, int *NUMNOD, int *IXS, int *NIXS, int *NUMELS, int *IPARTS, int *IPART, int *LIPART1,int *H3D_PART,
                           int *NUMELS10, int *IXS10, int *IPARTS10, int *NUMELS16, int *IXS16, int *IPARTS16, int *NUMELS20, int *IXS20,
                           int *IPARTS20)
{
    unsigned int * nbelems = new unsigned int [*NUMELS];
    try {


        // create Elements
        unsigned int conn4[4] ;
        unsigned int conn5[5] ;
        unsigned int conn6[6] ;
        unsigned int conn8[8] ;
        unsigned int conn10[10] ;
        unsigned int conn16[16] ;
        unsigned int conn20[20] ;
        int node[8] ;
        int node_ok[8] ;
        H3D_ID elem_id ;
        int i,n1,n2,n3,n4,n5,n6,n7,n8,n9,n10;
        int cpt_elem,nbelemwrite;
        int elem_nodes = 0;
        int elem_nodes_m1 = 0;
        int j = 0;
        int k = 0;
        int NUMELS8 = *NUMELS - *NUMELS10 - *NUMELS20 - *NUMELS16 ;
        int NIXS10 = 11;
        int NIXS16 = 17;
        int NIXS20 = 21;

        char SOLID4NPOOL[] = "TETRA4";
        rc = Hyper3DAddString(h3d_file, SOLID4NPOOL, &solid4n_poolname_id);
        if( !rc ) throw rc;

        char SOLID10NPOOL[] = "TETRA10";
        rc = Hyper3DAddString(h3d_file, SOLID10NPOOL, &solid10n_poolname_id);
        if( !rc ) throw rc;

        char SOLID5NPOOL[] = "PENTA5";
        rc = Hyper3DAddString(h3d_file, SOLID5NPOOL, &solid5n_poolname_id);
        if( !rc ) throw rc;

        char SOLID6NPOOL[] = "PENTA6";
        rc = Hyper3DAddString(h3d_file, SOLID6NPOOL, &solid6n_poolname_id);
        if( !rc ) throw rc;

        char SOLID8NPOOL[] = "BRICK";
        rc = Hyper3DAddString(h3d_file, SOLID8NPOOL, &solid8n_poolname_id);
        if( !rc ) throw rc;

        char SOLID16NPOOL[] = "BRICK16";
        rc = Hyper3DAddString(h3d_file, SOLID16NPOOL, &solid16n_poolname_id);
        if( !rc ) throw rc;

        char SOLID20NPOOL[] = "BRICK20";
        rc = Hyper3DAddString(h3d_file, SOLID20NPOOL, &solid20n_poolname_id);
        if( !rc ) throw rc;

        char SOLIDPOOL[] = "Solid";
        rc = Hyper3DAddString(h3d_file, SOLIDPOOL, &solid_poolname_id);
        if( !rc ) throw rc;

        unsigned int elem_count = 1;



// SOLID8

        comp_id = 0;
        j = 0;

        for(i=0;i<NUMELS8;i++)  nbelems[i] = 0;

        for(i=0;i<NUMELS8;i++)
          {
          if(H3D_PART[IPARTS[i] - 1] == 1)
            {
             n1 = IXS[*NIXS * i + 1];
             n2 = IXS[*NIXS * i + 2];
             n3 = IXS[*NIXS * i + 3];
             n4 = IXS[*NIXS * i + 4];
             n5 = IXS[*NIXS * i + 5];
             n6 = IXS[*NIXS * i + 6];
             n7 = IXS[*NIXS * i + 7];
             n8 = IXS[*NIXS * i + 8];

             node[0] = n1;
             node[1] = n2;
             node[2] = n3;
             node[3] = n4;
             node[4] = n5;
             node[5] = n6;
             node[6] = n7;
             node[7] = n8;

             for(k=0;k<8;k++)
             {
                 for(int l=k+1;l<8;l++)
                 {
                     if(node[k] == node[l]) node[l] = 0;
                 }
             }

             elem_nodes = 0;
             for(k=0;k<8;k++)
             {
                  if(node[k] != 0) elem_nodes = elem_nodes + 1;
             }

             if(elem_nodes_m1 != 4 && elem_nodes == 4)
             {
               if(IPART[*LIPART1 * (IPARTS[i] - 1) + 3] != comp_id)
               {
                  comp_id = IPART[*LIPART1 * (IPARTS[i] - 1) + 3];
               }
               j = i;
               elem_nodes_m1 = 4;
             }
             else if(elem_nodes_m1 != 6 &&  elem_nodes == 6) 
             {
               if(IPART[*LIPART1 * (IPARTS[i] - 1) + 3] != comp_id)
               {
                  comp_id = IPART[*LIPART1 * (IPARTS[i] - 1) + 3];
               }
               j = i;
               elem_nodes_m1 = 6;
             }
             else if(elem_nodes_m1 != 5 &&  elem_nodes == 5 )
             {
               if(IPART[*LIPART1 * (IPARTS[i] - 1) + 3] != comp_id)
               {
                  comp_id = IPART[*LIPART1 * (IPARTS[i] - 1) + 3];
               }
               j = i;
               elem_nodes_m1 = 5;
             }
             else if(elem_nodes_m1 != 8 &&  elem_nodes == 8 )
             {
               if(IPART[*LIPART1 * (IPARTS[i] - 1) + 3] != comp_id)
               {
                  comp_id = IPART[*LIPART1 * (IPARTS[i] - 1) + 3];
               }
               j = i;
               elem_nodes_m1 = 8;
             }
             else if(i == 0)
             {
               comp_id = IPART[*LIPART1 * (IPARTS[i] - 1) + 3];
               elem_nodes_m1 = elem_nodes;
               j = i;
             }
             else
             {
               if(IPART[*LIPART1 * (IPARTS[i] - 1) + 3] != comp_id)
               {
                  comp_id = IPART[*LIPART1 * (IPARTS[i] - 1) + 3];
                  j = i;
               }
             }


             nbelems[j] = nbelems[j] + 1;
             }
          }

        comp_id = 0;
        cpt_elem = 0;
        nbelemwrite = 0;




        for(i=0;i<NUMELS8;i++)  
        {
            if(H3D_PART[IPARTS[i] - 1] == 1)
            {  
             elem_id = IXS[*NIXS * i + *NIXS - 1];
             comp_id = IPART[*LIPART1 * (IPARTS[i] - 1) + 3];

             n1 = IXS[*NIXS * i + 1];
             n2 = IXS[*NIXS * i + 2];
             n3 = IXS[*NIXS * i + 3];
             n4 = IXS[*NIXS * i + 4];
             n5 = IXS[*NIXS * i + 5];
             n6 = IXS[*NIXS * i + 6];
             n7 = IXS[*NIXS * i + 7];
             n8 = IXS[*NIXS * i + 8];

             node[0] = n1;
             node[1] = n2;
             node[2] = n3;
             node[3] = n4;
             node[4] = n5;
             node[5] = n6;
             node[6] = n7;
             node[7] = n8;

             for(k=0;k<8;k++)
             {
                 for(int l=k+1;l<8;l++)
                 {
                     if(node[k] == node[l]) node[l] = 0;
                 }
             }

             for(k=0;k<8;k++) node_ok[k] = 0;
             elem_nodes = 0;
             for(k=0;k<8;k++)
             {
                  if(node[k] != 0) 
                  {
                      node_ok[elem_nodes] = node[k];
                      elem_nodes = elem_nodes + 1;
                  }
             }

             if( elem_nodes == 4)
             {
                 conn4[0] = node_ok[0];
                 conn4[1] = node_ok[1];
                 conn4[2] = node_ok[2];
                 conn4[3] = node_ok[3];
                 if(nbelems[i] != 0)
                 {
                      cpt_elem = 0;
                      nbelemwrite = nbelems[i];
                      comp_id = IPART[*LIPART1 * (IPARTS[i] - 1) + 3];

                      rc = Hyper3DElementBegin(h3d_file, nbelemwrite, solid_poolname_id, 
                                    H3D_ELEM_CONFIG_TETRA4, comp_id, 
                                    solid_poolname_id, node_poolname_id);
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
             else if(elem_nodes == 5)
             {
                 conn5[0] = node_ok[0];
                 conn5[1] = node_ok[1];
                 conn5[2] = node_ok[2];
                 conn5[3] = node_ok[3];
                 conn5[4] = node_ok[4];
                 if(nbelems[i] != 0)
                 {
                      cpt_elem = 0;
                      nbelemwrite = nbelems[i];
                      comp_id = IPART[*LIPART1 * (IPARTS[i] - 1) + 3];

                      rc = Hyper3DElementBegin(h3d_file, nbelemwrite, solid_poolname_id, 
                                    H3D_ELEM_CONFIG_PENTA5, comp_id, 
                                    solid_poolname_id, node_poolname_id);
                      if( !rc ) throw rc;
                 }


                 cpt_elem++;
                 rc = Hyper3DElementWrite(h3d_file, elem_id, conn5);
                 if( !rc ) throw rc;



                 if (cpt_elem == nbelemwrite)
                 {
                      rc = Hyper3DElementEnd(h3d_file);
                      if( !rc ) throw rc;
                 }
             }

             else if(elem_nodes == 6)
             {
                 if(n1 == n2 && n3 == n4 && n1 != n3)
                 {
                     conn6[0] = n1;
                     conn6[1] = n5;
                     conn6[2] = n6;
                     conn6[3] = n4;
                     conn6[4] = n8;
                     conn6[5] = n7;
                 }
                 else if(n5 == n6 && n7 == n8 && n5 != n7)
                 {
                     conn6[0] = n1;
                     conn6[1] = n5;
                     conn6[2] = n2;
                     conn6[3] = n4;
                     conn6[4] = n7;
                     conn6[5] = n3;
                 }
                 else if(n4 == n3 && n7 == n8 && n3 != n8)
                 {
                     conn6[0] = n1;
                     conn6[1] = n2;
                     conn6[2] = n3;
                     conn6[3] = n5;
                     conn6[4] = n6;
                     conn6[5] = n7;
                 }
                 else if(n1 == n2 && n5 == n6 && n1 != n5)
                 {
                     conn6[0] = n1;
                     conn6[1] = n3;
                     conn6[2] = n4;
                     conn6[3] = n5;
                     conn6[4] = n7;
                     conn6[5] = n8;
                 }
                 else if(n7 == n6 && n8 == n5 && n7 != n8)
                 {
                     conn6[0] = n1;
                     conn6[1] = n4;
                     conn6[2] = n5;
                     conn6[3] = n2;
                     conn6[4] = n3;
                     conn6[5] = n6;
                 }
                 else if(n4 == n1 && n8 == n5 && n4 != n8)
                 {
                     conn6[0] = n1;
                     conn6[1] = n2;
                     conn6[2] = n3;
                     conn6[3] = n5;
                     conn6[4] = n6;
                     conn6[5] = n7;
                 }
                 else if(n1 == n4 && n2 == n3 && n1 != n2)
                 {
                     conn6[0] = n1;
                     conn6[1] = n8;
                     conn6[2] = n5;
                     conn6[3] = n2;
                     conn6[4] = n7;
                     conn6[5] = n6;
                 }
                 else if(n2 == n3 && n6 == n7 && n2 != n6)
                 {
                     conn6[0] = n1;
                     conn6[1] = n3;
                     conn6[2] = n4;
                     conn6[3] = n5;
                     conn6[4] = n7;
                     conn6[5] = n8;
                 }
                 else
                 {
                     conn6[0] = node_ok[0];
                     conn6[1] = node_ok[1];
                     conn6[2] = node_ok[2];
                     conn6[3] = node_ok[3];
                     conn6[4] = node_ok[4];
                     conn6[5] = node_ok[5];
                 }
                 if(nbelems[i] != 0)
                 {
                      cpt_elem = 0;
                      nbelemwrite = nbelems[i];
                      comp_id = IPART[*LIPART1 * (IPARTS[i] - 1) + 3];

                      rc = Hyper3DElementBegin(h3d_file, nbelemwrite, solid_poolname_id, 
                                    H3D_ELEM_CONFIG_PENTA6, comp_id, 
                                    solid_poolname_id, node_poolname_id);
                      if( !rc ) throw rc;
                 }


                 cpt_elem++;
                 rc = Hyper3DElementWrite(h3d_file, elem_id, conn6);
                 if( !rc ) throw rc;



                 if (cpt_elem == nbelemwrite)
                 {
                      rc = Hyper3DElementEnd(h3d_file);
                      if( !rc ) throw rc;
                 }
             }
             else
             {
                 conn8[0] = n1;
                 conn8[1] = n2;
                 conn8[2] = n3;
                 conn8[3] = n4;
                 conn8[4] = n5;
                 conn8[5] = n6;
                 conn8[6] = n7;
                 conn8[7] = n8;

                 if(nbelems[i] != 0)
                 {
                      cpt_elem = 0;
                      nbelemwrite = nbelems[i];
                      comp_id = IPART[*LIPART1 * (IPARTS[i] - 1) + 3];

                      rc = Hyper3DElementBegin(h3d_file, nbelemwrite, solid_poolname_id, 
                                    H3D_ELEM_CONFIG_HEX8, comp_id, 
                                    solid_poolname_id, node_poolname_id);
                      if( !rc ) throw rc;
                 }

                 cpt_elem++;
                 rc = Hyper3DElementWrite(h3d_file, elem_id, conn8);
                 if( !rc ) throw rc;



                 if (cpt_elem == nbelemwrite)
                 {
                      rc = Hyper3DElementEnd(h3d_file);
                      if( !rc ) throw rc;
                 }
             }
         }
      }


// TETRA10

        for(i=0;i<*NUMELS10;i++)  nbelems[i] = 0;

        comp_id = 0;

        for(i=0;i<*NUMELS10;i++)
          {
          if(H3D_PART[IPARTS10[i] - 1] == 1)
            {
             elem_nodes = 10;

             if(IPART[*LIPART1 * (IPARTS10[i] - 1) + 3] != comp_id)
             {
             	comp_id = IPART[*LIPART1 * (IPARTS10[i] - 1) + 3];
                j = i;
             }
             nbelems[j] = nbelems[j] + 1;
             }
          }

        comp_id = 0;
        cpt_elem = 0;
        nbelemwrite = 0;

        for(i=0;i<*NUMELS10;i++)  
        {
            if(H3D_PART[IPARTS10[i] - 1] == 1)
            {  
             elem_id = IXS10[NIXS10 * i + NIXS10 - 1];
             comp_id = IPART[*LIPART1 * (IPARTS10[i] - 1) + 3];

             conn10[0] = IXS10[NIXS10 * i ];  
             conn10[1] = IXS10[NIXS10 * i + 1];
             conn10[2] = IXS10[NIXS10 * i + 2];
             conn10[3] = IXS10[NIXS10 * i + 3];
             conn10[4] = IXS10[NIXS10 * i + 4];
             conn10[5] = IXS10[NIXS10 * i + 5];
             conn10[6] = IXS10[NIXS10 * i + 6];
             conn10[7] = IXS10[NIXS10 * i + 7];
             conn10[8] = IXS10[NIXS10 * i + 8];
             conn10[9] = IXS10[NIXS10 * i + 9];



             if(nbelems[i] != 0)
             {
             	  cpt_elem = 0;
             	  nbelemwrite = nbelems[i];
             	  comp_id = IPART[*LIPART1 * (IPARTS10[i] - 1) + 3];

             	  rc = Hyper3DElementBegin(h3d_file, nbelemwrite, solid_poolname_id, 
             			H3D_ELEM_CONFIG_TETRA10, comp_id, 
             			solid_poolname_id, node_poolname_id);
             	  if( !rc ) throw rc;
             }


             cpt_elem++;
             rc = Hyper3DElementWrite(h3d_file, elem_id, conn10);
             if( !rc ) throw rc;


             if (cpt_elem == nbelemwrite)
             {
             	  rc = Hyper3DElementEnd(h3d_file);
             	  if( !rc ) throw rc;
             }

        }
      }

// SHELL16


        for(i=0;i<*NUMELS16;i++)  nbelems[i] = 0;

        comp_id = 0;

        for(i=0;i<*NUMELS16;i++)
          {
          if(H3D_PART[IPARTS16[i] - 1] == 1)
            {
             elem_nodes = 10;

             if(IPART[*LIPART1 * (IPARTS16[i] - 1) + 3] != comp_id)
             {
             	comp_id = IPART[*LIPART1 * (IPARTS16[i] - 1) + 3];
                j = i;
             }
             nbelems[j] = nbelems[j] + 1;
             }
          }

        comp_id = 0;
        cpt_elem = 0;
        nbelemwrite = 0;

        for(i=0;i<*NUMELS16;i++)  
        {
            if(H3D_PART[IPARTS16[i] - 1] == 1)
            {  
             elem_id = IXS16[NIXS16 * i + NIXS16 - 1];
             comp_id = IPART[*LIPART1 * (IPARTS16[i] - 1) + 3];

             conn8[0] = IXS16[NIXS16 * i ];   
             conn8[1] = IXS16[NIXS16 * i + 1];
             conn8[2] = IXS16[NIXS16 * i + 2];
             conn8[3] = IXS16[NIXS16 * i + 3];
             conn8[4] = IXS16[NIXS16 * i + 4];
             conn8[5] = IXS16[NIXS16 * i + 5];
             conn8[6] = IXS16[NIXS16 * i + 6];
             conn8[7] = IXS16[NIXS16 * i + 7];






             if(nbelems[i] != 0)
             {
             	  cpt_elem = 0;
             	  nbelemwrite = nbelems[i];
             	  comp_id = IPART[*LIPART1 * (IPARTS16[i] - 1) + 3];

             	  rc = Hyper3DElementBegin(h3d_file, nbelemwrite, solid_poolname_id, 
             			H3D_ELEM_CONFIG_HEX8, comp_id, 
             			solid_poolname_id, node_poolname_id);
             	  if( !rc ) throw rc;
             }


             cpt_elem++;
             rc = Hyper3DElementWrite(h3d_file, elem_id, conn8);
             if( !rc ) throw rc;



             if (cpt_elem == nbelemwrite)
             {
             	  rc = Hyper3DElementEnd(h3d_file);
             	  if( !rc ) throw rc;
             }

        }
      }


// BRICK20

        for(i=0;i<*NUMELS20;i++)  nbelems[i] = 0;

        comp_id = 0;

        for(i=0;i<*NUMELS20;i++)
          {
          if(H3D_PART[IPARTS20[i] - 1] == 1)
            {
             elem_nodes = 10;

             if(IPART[*LIPART1 * (IPARTS20[i] - 1) + 3] != comp_id)
             {
             	comp_id = IPART[*LIPART1 * (IPARTS20[i] - 1) + 3];
                j = i;
             }
             nbelems[j] = nbelems[j] + 1;
             }
          }

        comp_id = 0;
        cpt_elem = 0;
        nbelemwrite = 0;

        for(i=0;i<*NUMELS20;i++)  
        {
            if(H3D_PART[IPARTS20[i] - 1] == 1)
            {  
             elem_id = IXS20[NIXS20 * i + NIXS20 - 1];
             comp_id = IPART[*LIPART1 * (IPARTS20[i] - 1) + 3];

             conn20[0] = IXS20[NIXS20 * i ];
             conn20[1] = IXS20[NIXS20 * i + 1];
             conn20[2] = IXS20[NIXS20 * i + 2];
             conn20[3] = IXS20[NIXS20 * i + 3];
             conn20[4] = IXS20[NIXS20 * i + 4];
             conn20[5] = IXS20[NIXS20 * i + 5];
             conn20[6] = IXS20[NIXS20 * i + 6];
             conn20[7] = IXS20[NIXS20 * i + 7];
             conn20[8]  = IXS20[NIXS20 * i + 8];
             conn20[9]  = IXS20[NIXS20 * i + 9];
             conn20[10] = IXS20[NIXS20 * i + 10];
             conn20[11] = IXS20[NIXS20 * i + 11];
             conn20[12] = IXS20[NIXS20 * i + 12];
             conn20[13] = IXS20[NIXS20 * i + 13];
             conn20[14] = IXS20[NIXS20 * i + 14];
             conn20[15] = IXS20[NIXS20 * i + 15];
             conn20[16] = IXS20[NIXS20 * i + 16];
             conn20[17] = IXS20[NIXS20 * i + 17];
             conn20[18] = IXS20[NIXS20 * i + 18];
             conn20[19] = IXS20[NIXS20 * i + 19];


             if(nbelems[i] != 0)
             {
             	  cpt_elem = 0;
             	  nbelemwrite = nbelems[i];
             	  comp_id = IPART[*LIPART1 * (IPARTS20[i] - 1) + 3];

             	  rc = Hyper3DElementBegin(h3d_file, nbelemwrite, solid_poolname_id, 
             			H3D_ELEM_CONFIG_HEX20, comp_id, 
             			solid_poolname_id, node_poolname_id);
             	  if( !rc ) throw rc;
             }


             cpt_elem++;
             rc = Hyper3DElementWrite(h3d_file, elem_id, conn20);
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
    delete [] nbelems;

//
}

void _FCALL C_H3D_CREATE_SOLID8N(int *ITAB, int *NUMNOD, int *IXS, int *NIXS, int *NUMELS, int *IPARTS, int *IPART, int *LIPART1,int *H3D_PART,
                           int *NUMELS10, int *IXS10, int *IPARTS10, int *NUMELS16, int *IXS16, int *IPARTS16, int *NUMELS20, int *IXS20,
                           int *IPARTS20)
{c_h3d_create_solid8n_ (ITAB,NUMNOD,IXS,NIXS,NUMELS,IPARTS,IPART,LIPART1,H3D_PART,
                           NUMELS10,IXS10,IPARTS10,NUMELS16,IXS16,IPARTS16,NUMELS20,IXS20,
                           IPARTS20);}

void c_h3d_create_solid8n__ (int *ITAB, int *NUMNOD, int *IXS, int *NIXS, int *NUMELS, int *IPARTS, int *IPART, int *LIPART1,int *H3D_PART,
                           int *NUMELS10, int *IXS10, int *IPARTS10, int *NUMELS16, int *IXS16, int *IPARTS16, int *NUMELS20, int *IXS20,
                           int *IPARTS20)
{c_h3d_create_solid8n_ (ITAB,NUMNOD,IXS,NIXS,NUMELS,IPARTS,IPART,LIPART1,H3D_PART,
                           NUMELS10,IXS10,IPARTS10,NUMELS16,IXS16,IPARTS16,NUMELS20,IXS20,
                           IPARTS20);}

void c_h3d_create_solid8n (int *ITAB, int *NUMNOD, int *IXS, int *NIXS, int *NUMELS, int *IPARTS, int *IPART, int *LIPART1,int *H3D_PART,
                           int *NUMELS10, int *IXS10, int *IPARTS10, int *NUMELS16, int *IXS16, int *IPARTS16, int *NUMELS20, int *IXS20,
                           int *IPARTS20)
{c_h3d_create_solid8n_ (ITAB,NUMNOD,IXS,NIXS,NUMELS,IPARTS,IPART,LIPART1,H3D_PART,
                           NUMELS10,IXS10,IPARTS10,NUMELS16,IXS16,IPARTS16,NUMELS20,IXS20,
                           IPARTS20);}
}
