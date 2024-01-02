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
/*        C_H3D_CREATE_COMPONENTS                                  */
/*=================================================================*/
void c_h3d_create_components_(int *IPART, int *LIPART1, int *NPART, int *LTITR, int *IGEO, int *NPROPGI, int *H3D_PART,
                              int *NRBODY, int *NRWALL, int *NOM_OPT, int *LNOPT1, int *I16D, int *NPBY, int *NNPBY, 
                              int *SUB_NCHILD, int *NSUBS  , int *NRBE2, int *NRBE3, int *I16E,
                              int *I16F , int *N2D   , int *IRBE2   , int *NRBE2L , int *SUB_ID, int *SUB_CHILD,
                              int *SUB_LEVEL, int *SUB_IAD, int *SUB_TITLE, int *IRBE3   , int *NRBE3L ,
                              int *COMPID_RBODIES, int *COMPID_RBE2S, int *COMPID_RBE3S)
{
    char * name = new char [*LTITR * 3];
    int * ifiltmp = new int [*LTITR];
    char * name_part=new char [*LTITR * 3 ];
    char * name_sub=new char [*LTITR * 3 ]; 
    int * assembly_father = new int [*NSUBS];

    try {

        int i,j,k,cpt,ipid,igtyp,isid;
        unsigned char   red[] = {255, 0, 0};
        unsigned char green[] = {0, 255, 0};
        unsigned char  blue[] = {0, 0, 255};
        H3D_COMPONENT_ATTRIBS   wireframe = H3D_COMP_ATTR_WIREFRAME;
        H3D_COMPONENT_ATTRIBS      opaque = H3D_COMP_ATTR_OPAQUE;
        H3D_COMPONENT_ATTRIBS transparent = H3D_COMP_ATTR_TRANSPARENT;
        H3D_COMPONENT_ATTRIBS   meshlines = H3D_COMP_ATTR_MESHLINES;

        H3D_ID RigidElem = H3D_NULL_ID;

        // create Components
        H3D_ID comp_poolname_id = H3D_NULL_ID;
        rc = Hyper3DAddString(h3d_file, H3D_DEFAULT_COMPPOOL, &comp_poolname_id);
        if( !rc ) throw rc;

        rc = Hyper3DAddString(h3d_file, H3D_DEFAULT_NODEPOOL, &node_poolname_id);
        if( !rc ) throw rc;

        rc = Hyper3DAddString(h3d_file, H3D_DEFAULT_ELEMPOOL1D, &elem1D_poolname_id);
        if( !rc ) throw rc;

        char SHELLPOOL[] = "Shell";
        rc = Hyper3DAddString(h3d_file, SHELLPOOL, &shell_poolname_id);
        if( !rc ) throw rc;

        char SOLIDPOOL[] = "Solid";
        rc = Hyper3DAddString(h3d_file, SOLIDPOOL, &solid_poolname_id);
        if( !rc ) throw rc;

        char QUADPOOL[] = "QUAD";
        rc = Hyper3DAddString(h3d_file, QUADPOOL, &quad_poolname_id);
        if( !rc ) throw rc;

        char SKINPOOL[] = "SKIN";
        rc = Hyper3DAddString(h3d_file, SKINPOOL, &skin_poolname_id);
        if( !rc ) throw rc;

        char ONEDELEMPOOL[] = "1D";
        rc = Hyper3DAddString(h3d_file, ONEDELEMPOOL, &onedelem_poolname_id);
        if( !rc ) throw rc;

        char RIGIDPOOL[] = "Rigid";
        rc = Hyper3DAddString(h3d_file, RIGIDPOOL, &rigid_poolname_id);
        if( !rc ) throw rc;

        char SPHCELLPOOL[] = "SPHCELL";
        rc = Hyper3DAddString(h3d_file, SPHCELLPOOL, &sphcell_poolname_id);
        if( !rc ) throw rc;

        char SPHNODEPOOL[] = "SPHNodes";
        rc = Hyper3DAddString(h3d_file, SPHNODEPOOL, &sphnode_poolname_id);
        if( !rc ) throw rc;

        char RBODYPOOL[] = "Rbody";
        rc = Hyper3DAddString(h3d_file, RBODYPOOL, &rbody_poolname_id);
        if( !rc ) throw rc;

        char RBE2POOL[] = "Rbe2";
        rc = Hyper3DAddString(h3d_file, RBE2POOL, &rbe2_poolname_id);
        if( !rc ) throw rc;

        char RBE3POOL[] = "Rbe3";
        rc = Hyper3DAddString(h3d_file, RBE3POOL, &rbe3_poolname_id);
        if( !rc ) throw rc;

        char RWALLPOOL[] = "Rwall";
        rc = Hyper3DAddString(h3d_file, RWALLPOOL, &rwall_poolname_id);
        if( !rc ) throw rc;

        H3D_ID assm_poolname_id = H3D_NULL_ID;
        rc = Hyper3DAddString(h3d_file, H3D_DEFAULT_ASSMPOOL, &assm_poolname_id);
        if( !rc ) throw rc;

        for(i=0;i<*NSUBS;i++) 
            {  
              assembly_father[i] = -1;
            }

//        for(i=0;i<*NSUBS;i++) 
//            {  
//            int iad_sub = ISUBS[*LISUB1 * i + 2];
//            int nbson_sub = ISUBS[*LISUB1 * i + 1];
//            for(j=0;j<nbson_sub;j++) 
//                {  
//                  assembly_father[IBUFSSG[iad_sub  + j - 1] - 1] = i ;
//                }
//            }

        int current_son = 0;
        for(i=0;i<*NSUBS;i++)
            {  
            int nbson_sub = SUB_NCHILD[i];
            for(j=0;j<nbson_sub;j++) 
                {  
                  assembly_father[SUB_CHILD[current_son]-1] = i ;
                  current_son++ ;
                }
            }

        int level_max = 0;
        int id_max = 0;
        for(int isu=0;isu<*NSUBS;isu++) 
            {  
//            if(level_max < ISUBS[*LISUB1 * isu + 10]) level_max = ISUBS[*LISUB1 * isu + 10];
//            if(id_max < ISUBS[*LISUB1 * isu]) id_max = ISUBS[*LISUB1 * isu];
            if(level_max < SUB_LEVEL[isu]) level_max = SUB_LEVEL[isu];
            if(id_max < SUB_ID[isu]) id_max = SUB_ID[isu];
            }


        for(int ilevel=1;ilevel <= level_max;ilevel++) 
            {  
            for(int isu=0;isu<*NSUBS;isu++) 
                {  
//                if( ISUBS[*LISUB1 * isu + 10] == ilevel)
                if( SUB_LEVEL[isu] == ilevel)
                    {  
                    k = 0;
                    cpt = 0;
//	            for(j=0;j < *LTITR ; j=j+1) 
//                    {  
//                       name[k]=(char) (ISUBS[ *LISUB1 * (isu+1)  - *LTITR  + j]/65536);
//                       if(name[k] == ' ' || name[k] == '\0'){cpt++;} else {cpt = 0;}
//                       k++;
//                       name[k]=(char) ((ISUBS[ *LISUB1 * (isu+1)  - *LTITR  + j]%65536)/256);
//                       if(name[k] == ' ' || name[k] == '\0'){cpt++;} else {cpt = 0;}
//                       k++;
//                       name[k]=(char) (ISUBS[ *LISUB1 * (isu+1)  - *LTITR  + j]%256);
//                       if(name[k] == ' ' || name[k] == '\0'){cpt++;} else {cpt = 0;}
//                       k++;
//                    }

//	            for(j=0;j < *LTITR * 3 - cpt; j++) name_sub[j] = name[j];
//	            name_sub[*LTITR * 3 - cpt] = '\0' ;


	            for(j=0;j < *LTITR ; j=j+1) 
                    {
                       name[k]=(char) (SUB_TITLE[*LTITR * (isu) + j]/65536);
                       if(name[k] == ' ' || name[k] == '\0'){cpt++;} else {cpt = 0;}
                       k++;
                       name[k]=(char) ((SUB_TITLE[*LTITR * (isu) + j]%65536)/256);
                       if(name[k] == ' ' || name[k] == '\0'){cpt++;} else {cpt = 0;}
                       k++;
                       name[k]=(char) (SUB_TITLE[*LTITR * (isu) + j]%256);
                       if(name[k] == ' ' || name[k] == '\0'){cpt++;} else {cpt = 0;}
                       k++;
                    }

	            for(j=0;j < *LTITR * 3 - cpt; j++) name_sub[j] = name[j];
	            name_sub[*LTITR * 3 - cpt] = '\0' ;



//                    H3D_ID assm_id = ISUBS[*LISUB1 * isu ];
//                    H3D_ID model_as_parent =  ISUBS[*LISUB1 * (assembly_father[isu] )];
                    H3D_ID assm_id = SUB_ID[ isu ];
                    H3D_ID model_as_parent =  SUB_ID[assembly_father[isu]];

                    rc = Hyper3DAssemblyBegin(h3d_file, 1, assm_poolname_id, assm_poolname_id);
                    if( !rc ) throw rc;

                    rc = Hyper3DAssemblyWrite(h3d_file, name_sub, assm_id, model_as_parent);
                    if( !rc ) throw rc;

                    rc = Hyper3DAssemblyEnd(h3d_file);
                    if( !rc ) throw rc;

                    }
                }
            }



        for(i=0;i<*NPART;i++)  
            {
            ipid = IPART[ *LIPART1 * i  + 1 ] ;
            isid = IPART[ *LIPART1 * i  + 2 ] ;
            igtyp = IGEO[*NPROPGI * (ipid - 1) + 10 ] ;

            if(H3D_PART[i] == 1)
            {  


            // create Components
            if(igtyp == 1 || igtyp == 9|| igtyp == 10 || igtyp == 11 || igtyp == 16|| igtyp == 17|| igtyp == 19 || igtyp == 51) 
                 { 
                      rc = Hyper3DComponentBegin(h3d_file, 1, shell_poolname_id, assm_poolname_id);
                 }
            else if(*N2D != 0 &&(igtyp == 6 || igtyp == 14 || igtyp == 15 || igtyp == 20 || igtyp == 21 || igtyp == 22 || igtyp == 43))
                 { 
                      rc = Hyper3DComponentBegin(h3d_file, 1, quad_poolname_id, assm_poolname_id);
                 }
            else if(igtyp == 6 || igtyp == 14 || igtyp == 15 || igtyp == 20 || igtyp == 21 || igtyp == 22 || igtyp == 43)
                 { 
                      rc = Hyper3DComponentBegin(h3d_file, 1, solid_poolname_id, assm_poolname_id);
                 }
            else if(igtyp == 2 || igtyp == 3 || igtyp == 4 || igtyp == 8 || igtyp == 12 || igtyp == 13 || igtyp == 18 ||
                    igtyp == 25 || igtyp == 26 || igtyp == 28 || igtyp == 32|| igtyp == 33 || igtyp == 35 || igtyp == 36 ||
                    igtyp == 44 || igtyp == 45 || igtyp == 46 )
                 { 
                      rc = Hyper3DComponentBegin(h3d_file, 1, onedelem_poolname_id, assm_poolname_id);
                 }
            else if(igtyp == 34) 
                 { 
                      rc = Hyper3DComponentBegin(h3d_file, 1, sphcell_poolname_id, assm_poolname_id);
                 }
            else
                 { 
                      rc = Hyper3DComponentBegin(h3d_file, 1, comp_poolname_id, assm_poolname_id);
                 }


            if( !rc ) throw rc;

            k = 0;
            cpt = 0;
	    for(j=0;j < *LTITR ; j=j+1) 
            {  
               name[k]=(char) (IPART[ *LIPART1 * (i+1)  - *LTITR  + j]/65536);
               if(name[k] == ' ' || name[k] == '\0'){cpt++;} else {cpt = 0;}
               k++;
               name[k]=(char) ((IPART[ *LIPART1 * (i+1)  - *LTITR  + j]%65536)/256);
               if(name[k] == ' ' || name[k] == '\0'){cpt++;} else {cpt = 0;}
               k++;
               name[k]=(char) (IPART[ *LIPART1 * (i+1)  - *LTITR  + j]%256);
               if(name[k] == ' ' || name[k] == '\0'){cpt++;} else {cpt = 0;}
               k++;
            }

	        for(j=0;j < *LTITR * 3 - cpt; j++) name_part[j] = name[j];
	        name_part[*LTITR * 3 - cpt] = '\0' ;

                if(strlen(name_part) == 0)   sprintf(name_part,"no part name");

            if(igtyp == 1 || igtyp == 9|| igtyp == 10 || igtyp == 11 || igtyp == 16|| igtyp == 17|| igtyp == 19 || igtyp == 51) 
                 { 
                      rc = Hyper3DComponentWrite(h3d_file, name_part, IPART[*LIPART1 * i + 3],
//                                    node_poolname_id, ISUBS[*LISUB1 * (isid - 1) ]);
                                    node_poolname_id, SUB_ID[isid-1]);
 
                 }
            else if(igtyp == 6 || igtyp == 14 || igtyp == 15 || igtyp == 20 || igtyp == 21 || igtyp == 22 || igtyp == 43)
                 { 
                      rc = Hyper3DComponentWrite(h3d_file, name_part, IPART[*LIPART1 * i + 3],
//                                    node_poolname_id, ISUBS[*LISUB1 * (isid - 1) ]);
                                    node_poolname_id, SUB_ID[isid-1]);
                 }
            else if(igtyp == 2 || igtyp == 3 || igtyp == 4 || igtyp == 8 || igtyp == 12 || igtyp == 13 || igtyp == 18 ||
                    igtyp == 25 || igtyp == 26 || igtyp == 28 || igtyp == 32|| igtyp == 33 || igtyp == 35 || igtyp == 36 ||
                    igtyp == 44 || igtyp == 45 || igtyp == 46 )
                 { 
                      rc = Hyper3DComponentWrite(h3d_file, name_part, IPART[*LIPART1 * i + 3],
//                                    node_poolname_id, ISUBS[*LISUB1 * (isid - 1) ]);
                                    node_poolname_id, SUB_ID[isid-1]);
                 }
            else if(igtyp == 34)
                 { 
                      rc = Hyper3DComponentWrite(h3d_file, name_part, IPART[*LIPART1 * i + 3],
//                                    node_poolname_id, ISUBS[*LISUB1 * (isid - 1) ]);
                                    node_poolname_id, SUB_ID[isid-1]);
                 }
            else
                 {   
                      rc = Hyper3DComponentWrite(h3d_file, name_part, IPART[*LIPART1 * i + 3],
//                                    node_poolname_id, ISUBS[*LISUB1 * (isid - 1) ]);
                                    node_poolname_id, SUB_ID[isid-1]);
                 }

            if( !rc ) throw rc;

            rc = Hyper3DComponentEnd(h3d_file);
            if( !rc ) throw rc;

            }

        }
        for(i=0;i<*NPART;i++)  
            {
            ipid = IPART[ *LIPART1 * i  + 1 ] ;
            isid = IPART[ *LIPART1 * i  + 2 ] ;
            igtyp = IGEO[*NPROPGI * (ipid - 1) + 10 ] ;

            if(H3D_PART[i] == 1 || igtyp == 6 || igtyp == 14 || igtyp == 20 || igtyp == 21 || igtyp == 22)
            {  


            // create Components skins
               rc = Hyper3DComponentBegin(h3d_file, 1, skin_poolname_id, assm_poolname_id);

               if( !rc ) throw rc;

              k = 0;
              cpt = 0;
              for(j=0;j < *LTITR ; j=j+1) 
              {  
                 name[k]=(char) (IPART[ *LIPART1 * (i+1)  - *LTITR  + j]/65536);
                 if(name[k] == ' ' || name[k] == '\0'){cpt++;} else {cpt = 0;}
                 k++;
                 name[k]=(char) ((IPART[ *LIPART1 * (i+1)  - *LTITR  + j]%65536)/256);
                 if(name[k] == ' ' || name[k] == '\0'){cpt++;} else {cpt = 0;}
                 k++;
                 name[k]=(char) (IPART[ *LIPART1 * (i+1)  - *LTITR  + j]%256);
                 if(name[k] == ' ' || name[k] == '\0'){cpt++;} else {cpt = 0;}
                 k++;
              }

	        for(j=0;j < *LTITR * 3 - cpt; j++) name_part[j] = name[j];
	        name_part[*LTITR * 3 - cpt] = '\0' ;

                if(strlen(name_part) == 0)   sprintf(name_part,"no part name");

                       rc = Hyper3DComponentWrite(h3d_file, name_part, IPART[*LIPART1 * i + 3],
                                    node_poolname_id, SUB_ID[isid-1]);
 
                      if( !rc ) throw rc;

                     rc = Hyper3DComponentEnd(h3d_file);
                     if( !rc ) throw rc;
             }

        }


        int id_rbody;
        if(*NRBODY > 0)
        {
	    const char *name_rbody = "RBODIES MODEL" ;

            id_max ++;
            id_rbody = id_max;

            rc = Hyper3DAssemblyBegin(h3d_file, 1, assm_poolname_id, assm_poolname_id);
            if( !rc ) throw rc;

            rc = Hyper3DAssemblyWrite(h3d_file, name_rbody, id_rbody, 0);
            if( !rc ) throw rc;


            rc = Hyper3DAssemblyEnd(h3d_file);
            if( !rc ) throw rc;
        }


        if(*COMPID_RBODIES != 0) 
        {
            if(*NRBODY != 0) 
            {
            // create Components
            rc = Hyper3DComponentBegin(h3d_file, 1, rbody_poolname_id, assm_poolname_id);
            if( !rc ) throw rc;

            rc = Hyper3DComponentWrite(h3d_file, "Rbodies", *COMPID_RBODIES,
                            node_poolname_id, id_rbody);
            if( !rc ) throw rc;

            rc = Hyper3DComponentEnd(h3d_file);
            if( !rc ) throw rc;
            }
        }
        else
        {
            for(i=0;i<*NRBODY;i++)  
            {
                // create Components
                rc = Hyper3DComponentBegin(h3d_file, 1, rbody_poolname_id, assm_poolname_id);
                if( !rc ) throw rc;

                k = 0;
                cpt = 0;
                for(j=0;j < *LTITR ; j=j+1) 
                {  
                   name[k]=(char) (NOM_OPT[ *LNOPT1 * (i+1)  - *LTITR  + j]/65536);
                   if(name[k] == ' ' || name[k] == '\0'){cpt++;} else {cpt = 0;}
                   k++;
                   name[k]=(char) ((NOM_OPT[ *LIPART1 * (i+1)  - *LTITR  + j]%65536)/256);
                   if(name[k] == ' ' || name[k] == '\0'){cpt++;} else {cpt = 0;}
                   k++;
                   name[k]=(char) (NOM_OPT[ *LIPART1 * (i+1)  - *LTITR  + j]%256);
                   if(name[k] == ' ' || name[k] == '\0'){cpt++;} else {cpt = 0;}
                   k++;
                }

                for(j=0;j < *LTITR * 3 - cpt; j++) name_part[j] = name[j];
                name_part[*LTITR * 3 - cpt] = '\0' ;

                if(strlen(name_part) == 0)   sprintf(name_part,"RBODY");
            
                rc = Hyper3DComponentWrite(h3d_file, name_part, NPBY[*NNPBY * i + 5],
                                node_poolname_id, id_rbody);
                if( !rc ) throw rc;

                rc = Hyper3DComponentEnd(h3d_file);
                if( !rc ) throw rc;
            }
        }


     	int id_rbe2;
        if(*NRBE2 > 0)
        {
     	    const char *name_rbe2 = "RBE2 MODEL" ;

     	    id_max ++;
     	    id_rbe2 = id_max;

     	    rc = Hyper3DAssemblyBegin(h3d_file, 1, assm_poolname_id, assm_poolname_id);
     	    if( !rc ) throw rc;

     	    rc = Hyper3DAssemblyWrite(h3d_file, name_rbe2, id_rbe2, 0);
     	    if( !rc ) throw rc;


     	    rc = Hyper3DAssemblyEnd(h3d_file);
     	    if( !rc ) throw rc;
        }

        if(*COMPID_RBE2S != 0) 
        {
             if(*NRBE2 != 0) 
             {
                 // create Components
                 rc = Hyper3DComponentBegin(h3d_file, 1, rbe2_poolname_id, assm_poolname_id);
                 if( !rc ) throw rc;

                 rc = Hyper3DComponentWrite(h3d_file, "Rbe2s", *COMPID_RBE2S,
                                node_poolname_id, id_rbe2);
                 if( !rc ) throw rc;

                 rc = Hyper3DComponentEnd(h3d_file);
                 if( !rc ) throw rc;
             }
        }
        else
        {
            for(i=0;i<*NRBE2;i++)  
            {
                // create Components
                rc = Hyper3DComponentBegin(h3d_file, 1, rbe2_poolname_id, assm_poolname_id);
                if( !rc ) throw rc;

                k = 0;
                cpt = 0;
                for(j=0;j < *LTITR ; j=j+1) 
                {  
                   name[k]=(char) (NOM_OPT[*I16E-1 + *LNOPT1 * (i+1)  - *LTITR  + j]/65536);
                   if(name[k] == ' ' || name[k] == '\0'){cpt++;} else {cpt = 0;}
                   k++;
                   name[k]=(char) ((NOM_OPT[*I16E-1 +  *LIPART1 * (i+1)  - *LTITR  + j]%65536)/256);
                   if(name[k] == ' ' || name[k] == '\0'){cpt++;} else {cpt = 0;}
                   k++;
                   name[k]=(char) (NOM_OPT[*I16E-1 +  *LIPART1 * (i+1)  - *LTITR  + j]%256);
                   if(name[k] == ' ' || name[k] == '\0'){cpt++;} else {cpt = 0;}
                   k++;
                }

                for(j=0;j < *LTITR * 3 - cpt; j++) name_part[j] = name[j];
                name_part[*LTITR * 3 - cpt] = '\0' ;

                if(strlen(name_part) == 0)   sprintf(name_part,"RBE2");
                
                rc = Hyper3DComponentWrite(h3d_file, name_part, IRBE2[*NRBE2L * i + 1],
                                node_poolname_id, id_rbe2);
                if( !rc ) throw rc;


                rc = Hyper3DComponentEnd(h3d_file);
                if( !rc ) throw rc;
            }
        }

    	int id_rbe3;
        if(*NRBE3 > 0)
        {
    	    const char *name_rbe3 = "RBE3 MODEL" ;

    	    id_max ++;
    	    id_rbe3 = id_max;

    	    rc = Hyper3DAssemblyBegin(h3d_file, 1, assm_poolname_id, assm_poolname_id);
    	    if( !rc ) throw rc;

    	    rc = Hyper3DAssemblyWrite(h3d_file, name_rbe3, id_rbe3, 0);
    	    if( !rc ) throw rc;

    	    rc = Hyper3DAssemblyEnd(h3d_file);
    	    if( !rc ) throw rc;
        }


        if(*COMPID_RBE3S != 0) 
        {
            if(*NRBE3 != 0) 
            {
                // create Components
                rc = Hyper3DComponentBegin(h3d_file, 1, rbe3_poolname_id, assm_poolname_id);
                if( !rc ) throw rc;

                rc = Hyper3DComponentWrite(h3d_file, "Rbe3s", *COMPID_RBE3S,
                             node_poolname_id, id_rbe3);
                if( !rc ) throw rc;

                rc = Hyper3DComponentEnd(h3d_file);
                if( !rc ) throw rc;
            }
        }
        else
        {
        for(i=0;i<*NRBE3;i++)  
        {
            // create Components
            rc = Hyper3DComponentBegin(h3d_file, 1, rbe3_poolname_id, assm_poolname_id);
            if( !rc ) throw rc;

            k = 0;
            cpt = 0;
            for(j=0;j < *LTITR ; j=j+1) 
            {  
               name[k]=(char) (NOM_OPT[*I16F-1 + *LNOPT1 * (i+1)  - *LTITR  + j]/65536);
               if(name[k] == ' ' || name[k] == '\0'){cpt++;} else {cpt = 0;}
               k++;
               name[k]=(char) ((NOM_OPT[*I16F-1 +  *LIPART1 * (i+1)  - *LTITR  + j]%65536)/256);
               if(name[k] == ' ' || name[k] == '\0'){cpt++;} else {cpt = 0;}
               k++;
               name[k]=(char) (NOM_OPT[*I16F-1 +  *LIPART1 * (i+1)  - *LTITR  + j]%256);
               if(name[k] == ' ' || name[k] == '\0'){cpt++;} else {cpt = 0;}
               k++;
            }

            for(j=0;j < *LTITR * 3 - cpt; j++) name_part[j] = name[j];
            name_part[*LTITR * 3 - cpt] = '\0' ;

            if(strlen(name_part) == 0)   sprintf(name_part,"RBE3");
            
            rc = Hyper3DComponentWrite(h3d_file, name_part, IRBE3[*NRBE3L * i + 1],
                            node_poolname_id, id_rbe3);
            if( !rc ) throw rc;

            rc = Hyper3DComponentEnd(h3d_file);
            if( !rc ) throw rc;
            }
        }


    	int id_rwall;
        if(*NRWALL > 0)
            {
    	    const char *name_rwall = "RWALLS MODEL" ;

    	    id_max ++;
    	    id_rwall = id_max;

    	    rc = Hyper3DAssemblyBegin(h3d_file, 1, assm_poolname_id, assm_poolname_id);
    	    if( !rc ) throw rc;

    	    rc = Hyper3DAssemblyWrite(h3d_file, name_rwall, id_rwall, 0);
    	    if( !rc ) throw rc;


    	    rc = Hyper3DAssemblyEnd(h3d_file);
    	    if( !rc ) throw rc;
            }

    	for(i=0;i<*NRWALL;i++)  
            {

                // create Components
                rc = Hyper3DComponentBegin(h3d_file, 1, rwall_poolname_id, assm_poolname_id);
                if( !rc ) throw rc;

                k = 0;
                cpt = 0;
	        for(j=0;j < *LTITR ; j=j+1) 
                {  
                   name[k]=(char) (NOM_OPT[*I16D-1 + *LNOPT1 * (i+1)  - *LTITR  + j]/65536);
                   if(name[k] == ' ' || name[k] == '\0'){cpt++;} else {cpt = 0;}
                   k++;
                   name[k]=(char) ((NOM_OPT[*I16D-1 +  *LIPART1 * (i+1)  - *LTITR  + j]%65536)/256);
                   if(name[k] == ' ' || name[k] == '\0'){cpt++;} else {cpt = 0;}
                   k++;
                   name[k]=(char) (NOM_OPT[*I16D-1 +  *LIPART1 * (i+1)  - *LTITR  + j]%256);
                   if(name[k] == ' ' || name[k] == '\0'){cpt++;} else {cpt = 0;}
                   k++;
                }

	        for(j=0;j < *LTITR * 3 - cpt; j++) name_part[j] = name[j];
	        name_part[*LTITR * 3 - cpt] = '\0' ;
            
                rc = Hyper3DComponentWrite(h3d_file, name_part, NOM_OPT[*I16D + i * *LNOPT1 - 1],
            			node_poolname_id, id_rwall);
                if( !rc ) throw rc;

                rc = Hyper3DComponentEnd(h3d_file);
                if( !rc ) throw rc;
            }



        // assign Component display attributes
  /*
            rc = Hyper3DComponentAttribBegin(h3d_file, comp_count, comp_poolname_id);
            if( !rc ) throw rc;

            rc = Hyper3DComponentAttribWrite(h3d_file, IPART[*LIPART1 * i + 3], meshlines);
            if( !rc ) throw rc;

            rc = Hyper3DComponentAttribColor(h3d_file, IPART[*LIPART1 * i + 3], red);
            if( !rc ) throw rc;

            rc = Hyper3DComponentAttribEnd(h3d_file);
            if( !rc ) throw rc;

  */


    } // end of try

    catch(...) {
        Hyper3DExportClearError(h3d_file);
    }
    delete [] name;
    delete [] ifiltmp;
    delete [] name_part;
    delete [] name_sub;
    delete [] assembly_father;

}
void _FCALL C_H3D_CREATE_COMPONENTS(int *IPART, int *LIPART1, int *NPART, int *LTITR, int *IGEO, int *NPROPGI, int *H3D_PART,
                              int *NRBODY, int *NRWALL, int *NOM_OPT, int *LNOPT1, int *I16D, int *NPBY, int *NNPBY, 
                              int *SUB_NCHILD, int *NSUBS, int *NRBE2, int *NRBE3, int *I16E,
                              int *I16F, int *N2D, int *IRBE2   , int *NRBE2L, int *SUB_ID, int *SUB_CHILD, 
                              int *SUB_LEVEL, int *SUB_IAD, int *SUB_TITLE, int *IRBE3   , int *NRBE3L,
                              int *COMPID_RBODIES, int *COMPID_RBE2S, int *COMPID_RBE3S)
{c_h3d_create_components_ (IPART,LIPART1,NPART,LTITR,IGEO,NPROPGI,H3D_PART,NRBODY,NRWALL,NOM_OPT,LNOPT1,I16D, NPBY, NNPBY,SUB_NCHILD,NSUBS,NRBE2,NRBE3,
                           I16E,I16F,N2D,IRBE2,NRBE2L,SUB_ID,SUB_CHILD,SUB_LEVEL,SUB_IAD,SUB_TITLE,IRBE3,NRBE3L,
                           COMPID_RBODIES,COMPID_RBE2S,COMPID_RBE3S);}

void c_h3d_create_components__ (int *IPART, int *LIPART1, int *NPART, int *LTITR, int *IGEO, int *NPROPGI, int *H3D_PART,
                              int *NRBODY, int *NRWALL, int *NOM_OPT, int *LNOPT1, int *I16D, int *NPBY, int *NNPBY, 
                              int *SUB_NCHILD, int *NSUBS, int *NRBE2, int *NRBE3, int *I16E,
                              int *I16F, int *N2D, int *IRBE2   , int *NRBE2L, int *SUB_ID, int *SUB_CHILD,
                              int *SUB_LEVEL, int *SUB_IAD, int *SUB_TITLE, int *IRBE3   , int *NRBE3L,
                              int *COMPID_RBODIES, int *COMPID_RBE2S, int *COMPID_RBE3S)
{c_h3d_create_components_ (IPART,LIPART1,NPART,LTITR,IGEO,NPROPGI,H3D_PART,NRBODY,NRWALL,NOM_OPT,LNOPT1,I16D, NPBY, NNPBY,SUB_NCHILD,NSUBS,NRBE2,NRBE3,
                           I16E,I16F,N2D,IRBE2,NRBE2L,SUB_ID,SUB_CHILD,SUB_LEVEL,SUB_IAD,SUB_TITLE,IRBE3,NRBE3L,
                           COMPID_RBODIES,COMPID_RBE2S,COMPID_RBE3S);}

void c_h3d_create_components (int *IPART, int *LIPART1, int *NPART, int *LTITR, int *IGEO, int *NPROPGI, int *H3D_PART,
                              int *NRBODY, int *NRWALL, int *NOM_OPT, int *LNOPT1, int *I16D, int *NPBY, int *NNPBY, 
                              int *SUB_NCHILD, int *NSUBS, int *NRBE2, int *NRBE3, int *I16E,
                              int *I16F, int *N2D, int *IRBE2   , int *NRBE2L, int *SUB_ID, int *SUB_CHILD, 
                              int *SUB_LEVEL, int *SUB_IAD, int *SUB_TITLE, int *IRBE3   , int *NRBE3L,
                              int *COMPID_RBODIES, int *COMPID_RBE2S, int *COMPID_RBE3S)
{c_h3d_create_components_ (IPART,LIPART1,NPART,LTITR,IGEO,NPROPGI,H3D_PART,NRBODY,NRWALL,NOM_OPT,LNOPT1,I16D, NPBY, NNPBY,SUB_NCHILD,NSUBS,NRBE2,NRBE3,
                           I16E,I16F,N2D,IRBE2,NRBE2L,SUB_ID,SUB_CHILD,SUB_LEVEL,SUB_IAD,SUB_TITLE,IRBE3,NRBE3L,
                           COMPID_RBODIES,COMPID_RBE2S,COMPID_RBE3S);}


}
