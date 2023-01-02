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
/*        C_H3D_CREATE_RBE2                                     */
/*=================================================================*/
void c_h3d_create_rbe2_(int *ITAB, int *NUMNOD, int *IRBE2, int *NRBE2L, int *LRBE2, int *NRBE2, int *COMPID_RBE2S)
{
    try {


        // create Elements
        H3D_ID elem_id ;
        H3D_ID RigidElem ;
        int i,j,nsn;

        char RBE2POOL[] = "Rbe2";
        rc = Hyper3DAddString(h3d_file, RBE2POOL, &rbe2_poolname_id);
        if( !rc ) throw rc;

        char RIGIDPOOL[] = "Rigid";
        rc = Hyper3DAddString(h3d_file, RIGIDPOOL, &rigid_poolname_id);
        if( !rc ) throw rc;

        unsigned int elem_count = 1;
        unsigned int *conn;
        double *coef;
        int *dof;

        if(*COMPID_RBE2S != 0 && *NRBE2 != 0)
        {
             rc = Hyper3DElement2Begin(h3d_file, *NRBE2, rbe2_poolname_id, 
                                    H3D_ELEM_CONFIG_RIGIDLINK, *COMPID_RBE2S, 
                                    rbe2_poolname_id, node_poolname_id);
            for(i=0;i<*NRBE2;i++)  
            {
                 elem_id = IRBE2[*NRBE2L * i + 1];
                 nsn = IRBE2[*NRBE2L * i + 4];
                 comp_id = 1;

                 conn =(unsigned int *)malloc(nsn*sizeof(unsigned int));
                 coef =(double *)malloc(nsn*sizeof(double));
                 dof =(int *)malloc(nsn*sizeof(int));

                 for(j=0;j<nsn;j++)  conn[j] = ITAB[LRBE2[IRBE2[*NRBE2L * i ] + j ] - 1 ];

                 RigidElem = IRBE2[*NRBE2L * i + 1] ;

                 unsigned int conn1[1];
                 double coef1[1] = {1.0};
                 int dof1[1] = {6};
                 conn1[0] = ITAB[IRBE2[*NRBE2L * i + 2]-1];

                 if( !rc ) throw rc;
                 rc = Hyper3DElement2Write(h3d_file, elem_id, conn1, dof1 , coef1 , 1 ,conn, dof, coef, nsn);
                 if( !rc ) throw rc;

                 free(conn);
                 free(coef);
                 free(dof);
            }
            rc = Hyper3DElement2End(h3d_file);
            if( !rc ) throw rc;
        }
        else
        {
            for(i=0;i<*NRBE2;i++)  
            {
                 elem_id = IRBE2[*NRBE2L * i + 1];
                 nsn = IRBE2[*NRBE2L * i + 4];
                 comp_id = 1;

                 conn =(unsigned int *)malloc(nsn*sizeof(unsigned int));
                 coef =(double *)malloc(nsn*sizeof(double));
                 dof =(int *)malloc(nsn*sizeof(int));

                 for(j=0;j<nsn;j++)  conn[j] = ITAB[LRBE2[IRBE2[*NRBE2L * i ] + j ] - 1 ];

                 RigidElem = IRBE2[*NRBE2L * i + 1] ;

                 unsigned int conn1[1];
                 double coef1[1] = {1.0};
                 int dof1[1] = {6};
                 conn1[0] = ITAB[IRBE2[*NRBE2L * i + 2]-1];


                 rc = Hyper3DElement2Begin(h3d_file, elem_count, rbe2_poolname_id, 
                                        H3D_ELEM_CONFIG_RIGIDLINK, RigidElem, 
                                        rbe2_poolname_id, node_poolname_id);
                 if( !rc ) throw rc;
                 rc = Hyper3DElement2Write(h3d_file, elem_id, conn1, dof1 , coef1 , 1 ,conn, dof, coef, nsn);
                 if( !rc ) throw rc;
                 rc = Hyper3DElement2End(h3d_file);
                 if( !rc ) throw rc;

                 free(conn);
                 free(coef);
                 free(dof);

            }
        }


    } // end of try

    catch(...) {
        Hyper3DExportClearError(h3d_file);
    }

}

void _FCALL C_H3D_CREATE_RBE2(int *ITAB, int *NUMNOD, int *IRBE2, int *NRBE2L, int *LRBE2, int *NRBE2, int *COMPID_RBE2S)
{c_h3d_create_rbe2_ (ITAB, NUMNOD, IRBE2, NRBE2L, LRBE2, NRBE2,COMPID_RBE2S);}

void c_h3d_create_rbe2__ (int *ITAB, int *NUMNOD, int *IRBE2, int *NRBE2L, int *LRBE2, int *NRBE2, int *COMPID_RBE2S)
{c_h3d_create_rbe2_ (ITAB, NUMNOD, IRBE2, NRBE2L, LRBE2, NRBE2,COMPID_RBE2S);}

void c_h3d_create_rbe2 (int *ITAB, int *NUMNOD, int *IRBE2, int *NRBE2L, int *LRBE2, int *NRBE2, int *COMPID_RBE2S)
{c_h3d_create_rbe2_ (ITAB, NUMNOD, IRBE2, NRBE2L, LRBE2, NRBE2,COMPID_RBE2S);}


/*=================================================================*/
/*        C_H3D_CREATE_RBE2                                     */
/*=================================================================*/
void c_h3d_create_rbe2_impi_(int *ITAB,int *NRBE2,int *IADRBE2,int *MASTERNODS,int *P0RBE2BUF,
                             int *ID_RBE2, int *COMPID_RBE2S)
{
	
    try {


        // create Elements
        H3D_ID elem_id ;
        H3D_ID RigidElem ;
        int i,j,n,p,nsn,nsn_loc,ptr,iadg;
        //int PTRPO_TMP[*NSPMD + 1];

        char RBE2POOL[] = "Rbe2";
        rc = Hyper3DAddString(h3d_file, RBE2POOL, &rbe2_poolname_id);
        if( !rc ) throw rc;

        char RIGIDPOOL[] = "Rigid";
        rc = Hyper3DAddString(h3d_file, RIGIDPOOL, &rigid_poolname_id);
        if( !rc ) throw rc;
		
        unsigned int *conn;
        double *coef;
        int *dof;
        unsigned int elem_count = 1;


        if(*COMPID_RBE2S != 0 && *NRBE2 != 0)
        {
            rc = Hyper3DElement2Begin(h3d_file, *NRBE2, rbe2_poolname_id, 
                                    H3D_ELEM_CONFIG_RIGIDLINK, *COMPID_RBE2S, 
                                    rbe2_poolname_id, node_poolname_id);
            for(i=0;i<*NRBE2;i++)  
            {
                 elem_id = ID_RBE2[i];
                 nsn =IADRBE2[i+1] - IADRBE2[i];
                 iadg = IADRBE2[i]  ;
      

                 conn =(unsigned int *)malloc(nsn*sizeof(unsigned int));
                 coef =(double *)malloc(nsn*sizeof(double));
                 dof =(int *)malloc(nsn*sizeof(int));
                 unsigned int conn1[1];
                 //double coef[nsn];
                 double coef1[1] = {1.0};
                 //int dof[nsn];
                 int dof1[1] = {6};
                 conn1[0] = MASTERNODS[i];
                 j = 0;
                 for(n=0;n<nsn;n++)
                 {
                     conn[n] = P0RBE2BUF[iadg + n];
                 }

                 if( !rc ) throw rc;
                 rc = Hyper3DElement2Write(h3d_file, elem_id, conn1, dof1 , coef1 , 1 ,conn, dof, coef, nsn);
                 if( !rc ) throw rc;

                 free(conn);
                 free(coef);
                 free(dof);
            }
            rc = Hyper3DElement2End(h3d_file);
            if( !rc ) throw rc;

        }
        else
        {
            for(i=0;i<*NRBE2;i++)  
            {
                 elem_id = ID_RBE2[i];
                 nsn =IADRBE2[i+1] - IADRBE2[i];
                 iadg = IADRBE2[i]  ;
      

                 conn =(unsigned int *)malloc(nsn*sizeof(unsigned int));
                 coef =(double *)malloc(nsn*sizeof(double));
                 dof =(int *)malloc(nsn*sizeof(int));
                 unsigned int conn1[1];
                 //double coef[nsn];
                 double coef1[1] = {1.0};
                 //int dof[nsn];
                 int dof1[1] = {6};
                 conn1[0] = MASTERNODS[i];
                 j = 0;
                 for(n=0;n<nsn;n++)
                 {
                     conn[n] = P0RBE2BUF[iadg + n];
                 }



                 rc = Hyper3DElement2Begin(h3d_file, elem_count, rbe2_poolname_id, 
                                        H3D_ELEM_CONFIG_RIGIDLINK, elem_id, 
                                        rbe2_poolname_id, node_poolname_id);
                 if( !rc ) throw rc;
                 rc = Hyper3DElement2Write(h3d_file, elem_id, conn1, dof1 , coef1 , 1 ,conn, dof, coef, nsn);
                 if( !rc ) throw rc;
                 rc = Hyper3DElement2End(h3d_file);
                 if( !rc ) throw rc;

                 free(conn);
                 free(coef);
                 free(dof);

            }
        }
    } // end of try

    catch(...) {
        Hyper3DExportClearError(h3d_file);
    }

}

void _FCALL C_H3D_CREATE_RBE2_IMPI(int *ITAB,int *NRBE2,int *IADRBE2,int *MASTERNODS,int *P0RBE2BUF,
                                   int *ID_RBE2, int *COMPID_RBE2S)
{c_h3d_create_rbe2_impi_ (ITAB,NRBE2,IADRBE2,MASTERNODS,P0RBE2BUF,ID_RBE2,COMPID_RBE2S);}

void c_h3d_create_rbe2_impi__ (int *ITAB,int *NRBE2,int *IADRBE2,int *MASTERNODS,int *P0RBE2BUF,
                               int *ID_RBE2, int *COMPID_RBE2S)
{c_h3d_create_rbe2_impi_ (ITAB,NRBE2,IADRBE2,MASTERNODS,P0RBE2BUF,ID_RBE2,COMPID_RBE2S);}

void c_h3d_create_rbe2_impi (int *ITAB,int *NRBE2,int *IADRBE2,int *MASTERNODS,int *P0RBE2BUF,
                             int *ID_RBE2, int *COMPID_RBE2S)
{c_h3d_create_rbe2_impi_ (ITAB,NRBE2,IADRBE2,MASTERNODS,P0RBE2BUF,ID_RBE2,COMPID_RBE2S);}


}
