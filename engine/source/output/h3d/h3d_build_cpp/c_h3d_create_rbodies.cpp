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
/*        C_H3D_CREATE_RBODIES                                     */
/*=================================================================*/
void c_h3d_create_rbodies_(int *ITAB, int *NUMNOD, int *NPBY, int *NNPBY, int *LPBY, int *NRBODY, int *COMPID_RBODIES)
{
    try {


        // create Elements
        H3D_ID elem_id ;
        H3D_ID RigidElem ;
        int i,j,nsn;

        char RBODYPOOL[] = "Rbody";
        rc = Hyper3DAddString(h3d_file, RBODYPOOL, &rbody_poolname_id);
        if( !rc ) throw rc;

        char RIGIDPOOL[] = "Rigid";
        rc = Hyper3DAddString(h3d_file, RIGIDPOOL, &rigid_poolname_id);
        if( !rc ) throw rc;

        unsigned int elem_count = 1;
        unsigned int *conn;
        double *coef;
        int *dof;

        if(*COMPID_RBODIES != 0 && *NRBODY != 0)
        {
            rc = Hyper3DElement2Begin(h3d_file, *NRBODY, rbody_poolname_id, 
                                   H3D_ELEM_CONFIG_RIGIDLINK, *COMPID_RBODIES, 
                                   rbody_poolname_id, node_poolname_id);
            for(i=0;i<*NRBODY;i++)  
            {
                 elem_id = NPBY[*NNPBY * i + 5];
                 nsn = NPBY[*NNPBY * i + 1];
                 comp_id = 1;

                 conn =(unsigned int *)malloc(nsn*sizeof(unsigned int));
                 coef =(double *)malloc(nsn*sizeof(double));
                 for(int n=0; n < nsn ; n++){coef[n] = 0.0;}

                 dof =(int *)malloc(nsn*sizeof(int));

                 for(j=0;j<nsn;j++)
                 {
                    conn[j] = ITAB[LPBY[NPBY[*NNPBY * i + 10] + j ] - 1 ];
                    dof[j] = 0;
                 }

                 RigidElem = NPBY[*NNPBY * i + 5] ;

                 unsigned int conn1[1];
                 double coef1[1] = {1.0};
                 int dof1[1] = {6};
                 conn1[0] = ITAB[NPBY[*NNPBY * i]-1];


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
            for(i=0;i<*NRBODY;i++)  
            {
                 elem_id = NPBY[*NNPBY * i + 5];
                 nsn = NPBY[*NNPBY * i + 1];
                 comp_id = 1;

                 conn =(unsigned int *)malloc(nsn*sizeof(unsigned int));
                 coef =(double *)malloc(nsn*sizeof(double));
                 for(int n=0; n < nsn ; n++){coef[n] = 0.0;}

                 dof =(int *)malloc(nsn*sizeof(int));

                 for(j=0;j<nsn;j++)
                 {
                    conn[j] = ITAB[LPBY[NPBY[*NNPBY * i + 10] + j ] - 1 ];
                    dof[j] = 0;
                 }

                 RigidElem = NPBY[*NNPBY * i + 5] ;

                 unsigned int conn1[1];
                 double coef1[1] = {1.0};
                 int dof1[1] = {6};
                 conn1[0] = ITAB[NPBY[*NNPBY * i]-1];


                 rc = Hyper3DElement2Begin(h3d_file, elem_count, rbody_poolname_id, 
                                        H3D_ELEM_CONFIG_RIGIDLINK, RigidElem, 
                                        rbody_poolname_id, node_poolname_id);
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

void _FCALL C_H3D_CREATE_RBODIES(int *ITAB, int *NUMNOD, int *NPBY, int *NNPBY, int *LPBY, int *NRBODY, int *COMPID_RBODIES)
{c_h3d_create_rbodies_ (ITAB, NUMNOD, NPBY, NNPBY, LPBY, NRBODY, COMPID_RBODIES);}

void c_h3d_create_rbodies__ (int *ITAB, int *NUMNOD, int *NPBY, int *NNPBY, int *LPBY, int *NRBODY, int *COMPID_RBODIES)
{c_h3d_create_rbodies_ (ITAB, NUMNOD, NPBY, NNPBY, LPBY, NRBODY, COMPID_RBODIES);}

void c_h3d_create_rbodies (int *ITAB, int *NUMNOD, int *NPBY, int *NNPBY, int *LPBY, int *NRBODY, int *COMPID_RBODIES)
{c_h3d_create_rbodies_ (ITAB, NUMNOD, NPBY, NNPBY, LPBY, NRBODY, COMPID_RBODIES);}


/*=================================================================*/
/*        C_H3D_CREATE_RBODIES                                     */
/*=================================================================*/
void c_h3d_create_rbodies_impi_(int *ITAB, int *NRBYKIN, int *MASTERND, int *ID_RBY, int *PTRPO, int *PTRPOO, int *PORBY, int *NSPMD, int *COMPID_RBODIES)
{
	
    int * PTRPO_TMP = new int [*NSPMD + 1];
    try {


        // create Elements
        H3D_ID elem_id ;
        H3D_ID RigidElem ;
        int i,j,n,p,nsn,nsn_loc,ptr;
        //int PTRPO_TMP[*NSPMD + 1];

        char RBODYPOOL[] = "Rbody";
        rc = Hyper3DAddString(h3d_file, RBODYPOOL, &rbody_poolname_id);
        if( !rc ) throw rc;

        char RIGIDPOOL[] = "Rigid";
        rc = Hyper3DAddString(h3d_file, RIGIDPOOL, &rigid_poolname_id);
        if( !rc ) throw rc;
		
        unsigned int *conn;
        double *coef;
        int *dof;
        unsigned int elem_count = 1;

        for(i=0;i<*NSPMD + 1;i++)  
        {
           PTRPO_TMP[i] = PTRPO[i];
        }

        if(*COMPID_RBODIES != 0 && *NRBYKIN != 0)
        {
            rc = Hyper3DElement2Begin(h3d_file, *NRBYKIN, rbody_poolname_id, 
                                   H3D_ELEM_CONFIG_RIGIDLINK, *COMPID_RBODIES, 
                                   rbody_poolname_id, node_poolname_id);
            for(i=0;i<*NRBYKIN;i++)  
            {
                 elem_id = ID_RBY[i];
                 RigidElem = MASTERND[i] + 1 ;
                 nsn = 0;

                 for(p=0;p<*NSPMD;p++)
                 {
                     ptr = PTRPO_TMP[p];
                     if(ptr < PTRPOO[p+1])
                     {
                        if(PORBY[ptr-1] == i+1)
                        {
                           nsn = nsn + PORBY[ptr];
                           ptr = ptr + 2 ;
                           PTRPO_TMP[p]=PTRPO_TMP[p] + nsn + 2;
                        }
                     }
                 }
                             
                 conn =(unsigned int *)malloc(nsn*sizeof(unsigned int));
                 coef =(double *)malloc(nsn*sizeof(double));
                 dof =(int *)malloc(nsn*sizeof(int));

                 for(n=0; n < nsn ; n++){coef[n] = 0.0; dof[n]=0; conn[n]=0;}
                 unsigned int conn1[1];
                 //double coef[nsn];
                 double coef1[1] = {1.0};
                 //int dof[nsn];
                 int dof1[1] = {6};
                 conn1[0] = RigidElem;
                 j = 0;

                 for(p=0;p<*NSPMD;p++)
                 {
                     ptr = PTRPO[p];
                     if(ptr < PTRPOO[p+1])
                     {
                        if(PORBY[ptr - 1] == i+1)
                        {
                           nsn_loc =  PORBY[ptr];
                           ptr = ptr + 2 ;
                           for(n=0;n<nsn_loc;n++)
                           {
                               conn[j] = PORBY[ptr+n-1] + 1;

                               j++;
                           }
                           PTRPO[p]=PTRPO[p] + nsn + 2;
                        }
                     }

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

            for(i=0;i<*NRBYKIN;i++)  
            {
                 elem_id = ID_RBY[i];
                 RigidElem = MASTERND[i] + 1 ;
                 nsn = 0;

                 for(p=0;p<*NSPMD;p++)
                 {
                     ptr = PTRPO_TMP[p];
                     if(ptr < PTRPOO[p+1])
                     {
                        if(PORBY[ptr-1] == i+1)
                        {
                           nsn = nsn + PORBY[ptr];
                           ptr = ptr + 2 ;
                           PTRPO_TMP[p]=PTRPO_TMP[p] + nsn + 2;
                        }
                     }

                 }

                             
                 conn =(unsigned int *)malloc(nsn*sizeof(unsigned int));
                 coef =(double *)malloc(nsn*sizeof(double));
                 dof =(int *)malloc(nsn*sizeof(int));

                 for(n=0; n < nsn ; n++){coef[n] = 0.0; dof[n]=0; conn[n]=0;}
                 unsigned int conn1[1];
                 //double coef[nsn];
                 double coef1[1] = {1.0};
                 //int dof[nsn];
                 int dof1[1] = {6};
                 conn1[0] = RigidElem;
                 j = 0;

                 for(p=0;p<*NSPMD;p++)
                 {
                     ptr = PTRPO[p];
                     if(ptr < PTRPOO[p+1])
                     {
                        if(PORBY[ptr - 1] == i+1)
                        {
                           nsn_loc =  PORBY[ptr];
                           ptr = ptr + 2 ;
                           for(n=0;n<nsn_loc;n++)
                           {
                               conn[j] = PORBY[ptr+n-1] + 1;

                               j++;
                           }
                           PTRPO[p]=PTRPO[p] + nsn + 2;
                        }
                     }

                 }



                 rc = Hyper3DElement2Begin(h3d_file, elem_count, rbody_poolname_id, 
                                        H3D_ELEM_CONFIG_RIGIDLINK, elem_id, 
                                        rbody_poolname_id, node_poolname_id);
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
    delete [] PTRPO_TMP;

}

void _FCALL C_H3D_CREATE_RBODIES_IMPI(int *ITAB ,int *NRBYKIN, int *MASTERND, int *ID_RBY, int *PTRPO, int *PTRPOO, int *PORBY, int *NSPMD, int *COMPID_RBODIES)
{c_h3d_create_rbodies_impi_ (ITAB,NRBYKIN, MASTERND, ID_RBY, PTRPO, PTRPOO, PORBY,NSPMD,COMPID_RBODIES);}

void c_h3d_create_rbodies_impi__ (int *ITAB ,int *NRBYKIN, int *MASTERND, int *ID_RBY, int *PTRPO, int *PTRPOO, int *PORBY, int *NSPMD, int *COMPID_RBODIES)
{c_h3d_create_rbodies_impi_ (ITAB,NRBYKIN, MASTERND, ID_RBY, PTRPO, PTRPOO, PORBY,NSPMD,COMPID_RBODIES);}

void c_h3d_create_rbodies_impi (int *ITAB, int *NRBYKIN, int *MASTERND, int *ID_RBY, int *PTRPO, int *PTRPOO, int *PORBY, int *NSPMD, int *COMPID_RBODIES)
{c_h3d_create_rbodies_impi_ (ITAB,NRBYKIN, MASTERND, ID_RBY, PTRPO, PTRPOO, PORBY,NSPMD,COMPID_RBODIES);}


}
