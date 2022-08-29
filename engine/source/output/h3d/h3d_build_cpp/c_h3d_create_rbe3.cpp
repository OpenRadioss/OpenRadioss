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
/*        C_H3D_CREATE_RBE3                                     */
/*=================================================================*/
void c_h3d_create_rbe3_(int *ITAB, int *NUMNOD, int *IRBE3, int *NRBE3L, int *LRBE3, int *NRBE3, int *COMPID_RBE3S)
{
    try {


        // create Elements
        H3D_ID elem_id ;
        H3D_ID RigidElem ;
        int i,j,nsn;

        char RBE3POOL[] = "Rbe3";
        rc = Hyper3DAddString(h3d_file, RBE3POOL, &rbe3_poolname_id);
        if( !rc ) throw rc;

        char RIGIDPOOL[] = "Rigid";
        rc = Hyper3DAddString(h3d_file, RIGIDPOOL, &rigid_poolname_id);
        if( !rc ) throw rc;

        unsigned int elem_count = 1;
        unsigned int *conn;
        double *coef;
        int *dof;

        if(*COMPID_RBE3S != 0 && *NRBE3 != 0)
        {
            rc = Hyper3DElement2Begin(h3d_file, *NRBE3, rbe3_poolname_id, 
                                    H3D_ELEM_CONFIG_RIGIDLINK, *COMPID_RBE3S, 
                                    rbe3_poolname_id, node_poolname_id);
            for(i=0;i<*NRBE3;i++)  
            {
                 elem_id = IRBE3[*NRBE3L * i + 1];
                 nsn = IRBE3[*NRBE3L * i + 4];
                 comp_id = 1;

                 conn =(unsigned int *)malloc(nsn*sizeof(unsigned int));
                 coef =(double *)malloc(nsn*sizeof(double));
                 dof =(int *)malloc(nsn*sizeof(int));

                 for(j=0;j<nsn;j++)  conn[j] = ITAB[LRBE3[IRBE3[*NRBE3L * i ] + j ] - 1 ];

                 RigidElem = IRBE3[*NRBE3L * i + 1] ;

                 unsigned int conn1[1];
                 double coef1[1] = {1.0};
                 int dof1[1] = {6};
                 conn1[0] = ITAB[IRBE3[*NRBE3L * i + 2]-1];

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
            for(i=0;i<*NRBE3;i++)  
            {
                 elem_id = IRBE3[*NRBE3L * i + 1];
                 nsn = IRBE3[*NRBE3L * i + 4];
                 comp_id = 1;

                 conn =(unsigned int *)malloc(nsn*sizeof(unsigned int));
                 coef =(double *)malloc(nsn*sizeof(double));
                 dof =(int *)malloc(nsn*sizeof(int));

                 for(j=0;j<nsn;j++)  conn[j] = ITAB[LRBE3[IRBE3[*NRBE3L * i ] + j ] - 1 ];

                 RigidElem = IRBE3[*NRBE3L * i + 1] ;

                 unsigned int conn1[1];
                 double coef1[1] = {1.0};
                 int dof1[1] = {6};
                 conn1[0] = ITAB[IRBE3[*NRBE3L * i + 2]-1];


                 rc = Hyper3DElement2Begin(h3d_file, elem_count, rbe3_poolname_id, 
                                        H3D_ELEM_CONFIG_RIGIDLINK, RigidElem, 
                                        rbe3_poolname_id, node_poolname_id);
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

void _FCALL C_H3D_CREATE_RBE3(int *ITAB, int *NUMNOD, int *IRBE3, int *NRBE3L, int *LRBE3, int *NRBE3, int *COMPID_RBE3S)
{c_h3d_create_rbe3_ (ITAB, NUMNOD, IRBE3, NRBE3L, LRBE3, NRBE3,COMPID_RBE3S);}

void c_h3d_create_rbe3__ (int *ITAB, int *NUMNOD, int *IRBE3, int *NRBE3L, int *LRBE3, int *NRBE3, int *COMPID_RBE3S)
{c_h3d_create_rbe3_ (ITAB, NUMNOD, IRBE3, NRBE3L, LRBE3, NRBE3,COMPID_RBE3S);}

void c_h3d_create_rbe3 (int *ITAB, int *NUMNOD, int *IRBE3, int *NRBE3L, int *LRBE3, int *NRBE3, int *COMPID_RBE3S)
{c_h3d_create_rbe3_ (ITAB, NUMNOD, IRBE3, NRBE3L, LRBE3, NRBE3,COMPID_RBE3S);}


/*=================================================================*/
/*        C_H3D_CREATE_RBE3                                     */
/*=================================================================*/
void c_h3d_create_rbe3_impi_(int *ITAB,int *NRBE3,int *IADRBE3,int *SLAVENODS,int *P0RBE3BUF,
                             int *ID_RBE3, int *COMPID_RBE3S)
{
	
    try {


        // create Elements
        H3D_ID elem_id ;
        H3D_ID RigidElem ;
        int i,j,n,p,nsn,nsn_loc,ptr,iadg;
        //int PTRPO_TMP[*NSPMD + 1];

        char RBE3POOL[] = "Rbe3";
        rc = Hyper3DAddString(h3d_file, RBE3POOL, &rbe3_poolname_id);
        if( !rc ) throw rc;

        char RIGIDPOOL[] = "Rigid";
        rc = Hyper3DAddString(h3d_file, RIGIDPOOL, &rigid_poolname_id);
        if( !rc ) throw rc;
		
        unsigned int *conn;
        double *coef;
        int *dof;
        unsigned int elem_count = 1;


        if(*COMPID_RBE3S != 0 && *NRBE3 != 0)
        {
             rc = Hyper3DElement2Begin(h3d_file, *NRBE3, rbe3_poolname_id, 
                                    H3D_ELEM_CONFIG_RIGIDLINK, *COMPID_RBE3S, 
                                    rbe3_poolname_id, node_poolname_id);
            for(i=0;i<*NRBE3;i++)  
            {
                 elem_id = ID_RBE3[i];
                 nsn =IADRBE3[i+1] - IADRBE3[i];
                 iadg = IADRBE3[i]  ;
      

                 conn =(unsigned int *)malloc(nsn*sizeof(unsigned int));
                 coef =(double *)malloc(nsn*sizeof(double));
                 dof =(int *)malloc(nsn*sizeof(int));
                 unsigned int conn1[1];
                 //double coef[nsn];
                 double coef1[1] = {1.0};
                 //int dof[nsn];
                 int dof1[1] = {6};
                 conn1[0] = SLAVENODS[i];
                 j = 0;
                 for(n=0;n<nsn;n++)
                 {
                     conn[n] = P0RBE3BUF[iadg + n];
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
            for(i=0;i<*NRBE3;i++)  
            {
                 elem_id = ID_RBE3[i];
                 nsn =IADRBE3[i+1] - IADRBE3[i];
                 iadg = IADRBE3[i]  ;
      

                 conn =(unsigned int *)malloc(nsn*sizeof(unsigned int));
                 coef =(double *)malloc(nsn*sizeof(double));
                 dof =(int *)malloc(nsn*sizeof(int));
                 unsigned int conn1[1];
                 //double coef[nsn];
                 double coef1[1] = {1.0};
                 //int dof[nsn];
                 int dof1[1] = {6};
                 conn1[0] = SLAVENODS[i];
                 j = 0;
                 for(n=0;n<nsn;n++)
                 {
                     conn[n] = P0RBE3BUF[iadg + n];
                 }



                 rc = Hyper3DElement2Begin(h3d_file, elem_count, rbe3_poolname_id, 
                                        H3D_ELEM_CONFIG_RIGIDLINK, elem_id, 
                                        rbe3_poolname_id, node_poolname_id);
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

void _FCALL C_H3D_CREATE_RBE3_IMPI(int *ITAB,int *NRBE3,int *IADRBE3,int *SLAVENODS,int *P0RBE3BUF,
                                   int *ID_RBE3, int *COMPID_RBE3S)
{c_h3d_create_rbe3_impi_ (ITAB,NRBE3,IADRBE3,SLAVENODS,P0RBE3BUF,ID_RBE3,COMPID_RBE3S);}

void c_h3d_create_rbe3_impi__ (int *ITAB,int *NRBE3,int *IADRBE3,int *SLAVENODS,int *P0RBE3BUF,
                               int *ID_RBE3, int *COMPID_RBE3S)
{c_h3d_create_rbe3_impi_ (ITAB,NRBE3,IADRBE3,SLAVENODS,P0RBE3BUF,ID_RBE3,COMPID_RBE3S);}

void c_h3d_create_rbe3_impi (int *ITAB,int *NRBE3,int *IADRBE3,int *SLAVENODS,int *P0RBE3BUF,
                             int *ID_RBE3, int *COMPID_RBE3S)
{c_h3d_create_rbe3_impi_ (ITAB,NRBE3,IADRBE3,SLAVENODS,P0RBE3BUF,ID_RBE3,COMPID_RBE3S);}


}
