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

#define PI 3.141592654

extern "C" 
/*=================================================================*/
{
/*=================================================================*/
/*        C_H3D_CREATE_RWALLS                                     */
/*=================================================================*/
void c_h3d_create_rwalls_(int *NOM_OPT, int *LNOPT1, int *I16D, int *NPRW, int *NRWALL, int *MAX_NOD_ID,
                          my_real *XWL, my_real *YWL , my_real *ZWL, my_real *V1, my_real *V2, my_real *V3, 
                          my_real *VV1, my_real *VV2 , my_real *VV3 , my_real *XL , my_real *XN, my_real *YN,
                          my_real *ZN )
{
    try {


        float DX0[6] = { 0., 0., 0., 0., 0., 0.};
        float DY0[6] = { 1.,-1., 0., 0., 0., 0.};
        float DZ0[6] = { 0., 0.,-1., 1., 1.,-1.};
        float DX1[6] = { 1., 1., 1., 1., 0., 0.};
        float DY1[6] = { 0., 0., 0., 0., 1., 1.};
        float DZ1[6] = { 0., 0., 0., 0., 0., 0.};
        float X0[6]  = {-3.,-3.,-3.,-3.,-3., 3.};
        float Y0[6]  = {-3., 3.,-3., 3.,-3.,-3.};
        float Z0[6]  = {-3., 3., 3.,-3.,-3., 3.};

        float node[3],r,xx,yy,zz,xx0,yy0,zz0,d,alpha,s,c; 
        H3D_ID node_id;

        // create Elements
        H3D_ID elem_id ;
        H3D_ID RigidElem ;
        int i,j,nsn,nbelemwrite,n2,n3,n4,ityp;

        char RWALLPOOL[] = "Rwall";
        rc = Hyper3DAddString(h3d_file, RWALLPOOL, &rwall_poolname_id);
        if( !rc ) throw rc;

        char RIGIDPOOL[] = "Rigid";
        rc = Hyper3DAddString(h3d_file, RIGIDPOOL, &rigid_poolname_id);
        if( !rc ) throw rc;

        unsigned int elem_count = 1;

        int id_node = *MAX_NOD_ID;

        elem_id = 0;

        for(i=0;i<*NRWALL;i++)  
        {

             n2 = i  + *NRWALL;
             n3 = n2 + *NRWALL;
             n4 = n3 + *NRWALL;

             ityp = NPRW[n4];
             RigidElem =  NOM_OPT[*I16D + i * *LNOPT1 - 1];

             if( ityp == 1) 
                 {

                 int id_node_0 = id_node + 1 ;

                 rc = Hyper3DPositionBegin(h3d_file, 4, node_poolname_id);
                 if( !rc ) throw rc;

                 node[0] =  XWL[i] + V1[i];
                 node[1] =  YWL[i] + V2[i];
                 node[2] =  ZWL[i] + V3[i];
                 id_node ++ ;
                 node_id= id_node;

                 rc = Hyper3DPositionWrite(h3d_file, node_id, node, H3D_NULL_ID, H3D_NULL_ID); 
                 if( !rc ) throw rc;

                 node[0] =  XWL[i] + VV1[i];
                 node[1] =  YWL[i] + VV2[i];
                 node[2] =  ZWL[i] + VV3[i];
                 id_node ++ ;
                 node_id= id_node;

                 rc = Hyper3DPositionWrite(h3d_file, node_id, node, H3D_NULL_ID, H3D_NULL_ID); 
                 if( !rc ) throw rc;

                 node[0] =  XWL[i] - V1[i];
                 node[1] =  YWL[i] - V2[i];
                 node[2] =  ZWL[i] - V3[i];
                 id_node ++ ;
                 node_id= id_node;

                 rc = Hyper3DPositionWrite(h3d_file, node_id, node, H3D_NULL_ID, H3D_NULL_ID); 
                 if( !rc ) throw rc;

                 node[0] =  XWL[i] - VV1[i];
                 node[1] =  YWL[i] - VV2[i];
                 node[2] =  ZWL[i] - VV3[i];
                 id_node ++ ;
                 node_id= id_node;

                 rc = Hyper3DPositionWrite(h3d_file, node_id, node, H3D_NULL_ID, H3D_NULL_ID); 
                 if( !rc ) throw rc;

                 rc = Hyper3DPositionEnd(h3d_file);
                 if( !rc ) throw rc;

                 unsigned int	 conn[4]; 
                 conn[0] = id_node - 3 ;
                 conn[1] = id_node - 2 ;
                 conn[2] = id_node - 1 ;
                 conn[3] = id_node  ;

                 nbelemwrite = 1;

                 rc = Hyper3DElementBegin(h3d_file, nbelemwrite, rwall_poolname_id, 
                        	   H3D_ELEM_CONFIG_QUAD4, RigidElem, 
                        	   rwall_poolname_id, node_poolname_id);
                 if( !rc ) throw rc;

                 elem_id++;
                 rc = Hyper3DElementWrite(h3d_file, elem_id, conn);
                 if( !rc ) throw rc;

                 rc = Hyper3DElementEnd(h3d_file);
                 if( !rc ) throw rc;
                 }



             else if(ityp == 2)
                 {
                 int cpt = 0;
                 int id_node_0 = id_node + 1 ;
                 alpha = 0.0f ;
                 rc = Hyper3DPositionBegin(h3d_file, 48 , node_poolname_id);
                 if( !rc ) throw rc;

                 for(int ii=0;ii<24;ii++) 
                     {

                     s = sin(alpha);
                     c = cos(alpha);

    		     node[0] =  XWL[i] + VV1[i] * s + V1[i] * c - XN[i] * XL[i];
    		     node[1] =  YWL[i] + VV2[i] * s + V2[i] * c - YN[i] * XL[i];
    		     node[2] =  ZWL[i] + VV3[i] * s + V3[i] * c - ZN[i] * XL[i];

                     id_node ++ ;
     		     node_id = id_node;
     		     rc = Hyper3DPositionWrite(h3d_file, node_id, node, H3D_NULL_ID, H3D_NULL_ID); 
     		     if( !rc ) throw rc;

    		     node[0] =  XWL[i] + VV1[i] * s + V1[i] * c + XN[i] * XL[i];
    		     node[1] =  YWL[i] + VV2[i] * s + V2[i] * c + YN[i] * XL[i];
    		     node[2] =  ZWL[i] + VV3[i] * s + V3[i] * c + ZN[i] * XL[i];

                     id_node ++ ;
     		     node_id = id_node;
     		     rc = Hyper3DPositionWrite(h3d_file, node_id, node, H3D_NULL_ID, H3D_NULL_ID); 
     		     if( !rc ) throw rc;

                     alpha = alpha + PI /12 ;
                     }


                 rc = Hyper3DPositionEnd(h3d_file);
                 if( !rc ) throw rc;


                 unsigned int	 conn[4]; 
                 id_node = id_node_0;

                 for(int ii=0;ii<23;ii++) 
                     {
   		     conn[0] = id_node  ;
   		     conn[1] = id_node + 2 ;
   		     conn[2] = id_node + 3 ;
   		     conn[3] = id_node + 1 ;
                     nbelemwrite = 1;

                     rc = Hyper3DElementBegin(h3d_file, nbelemwrite, rwall_poolname_id, 
                     	   H3D_ELEM_CONFIG_QUAD4, RigidElem, 
                     	   rwall_poolname_id, node_poolname_id);
    		     if( !rc ) throw rc;

                     elem_id++;
    		     rc = Hyper3DElementWrite(h3d_file, elem_id, conn);
    		     if( !rc ) throw rc;

    		     rc = Hyper3DElementEnd(h3d_file);
    		     if( !rc ) throw rc;

                     id_node = id_node + 2;
                     }
   		 conn[0] = id_node  ;
   		 conn[1] = id_node_0 ;
   		 conn[2] = id_node_0 + 1 ;
   		 conn[3] = id_node + 1 ;
                 nbelemwrite = 1;

                 rc = Hyper3DElementBegin(h3d_file, nbelemwrite, rwall_poolname_id, 
                       H3D_ELEM_CONFIG_QUAD4, RigidElem, 
                       rwall_poolname_id, node_poolname_id);
    		 if( !rc ) throw rc;

                 elem_id++;
    		 rc = Hyper3DElementWrite(h3d_file, elem_id, conn);
    		 if( !rc ) throw rc;

    		 rc = Hyper3DElementEnd(h3d_file);
    		 if( !rc ) throw rc;

                 id_node = id_node + 1;


                 }


             else if(ityp == 3)
                 {
                 int cpt = 0;
                 int id_node_0 = id_node + 1 ;
                 r = V1[i] ;
                 rc = Hyper3DPositionBegin(h3d_file, 294 , node_poolname_id);
                 if( !rc ) throw rc;

                 for(int ii=0;ii<6;ii++) 
                     {
                     xx0 = X0[ii] ;
                     yy0 = Y0[ii] ;
                     zz0 = Z0[ii] ; 
                     for(int jj=0;jj<7;jj++) 
                         {
                         xx = xx0;
                         yy = yy0;
                         zz = zz0;
                         for(int ll=0;ll<7;ll++) 
                             {
                             d = r/sqrt(xx*xx+yy*yy+zz*zz);

     		             node[0] =  d*xx + XWL[i];
     		             node[1] =  d*yy + YWL[i];
     		             node[2] =  d*zz + ZWL[i];
                             id_node ++ ;
     		             node_id = id_node;
     		             rc = Hyper3DPositionWrite(h3d_file, node_id, node, H3D_NULL_ID, H3D_NULL_ID); 
     		             if( !rc ) throw rc;


                             xx = xx + DX0[ii];
                             yy = yy + DY0[ii];
                             zz = zz + DZ0[ii];

                             }
                         xx0 = xx0 + DX1[ii];
                         yy0 = yy0 + DY1[ii];
                         zz0 = zz0 + DZ1[ii];
                         }
                     }

                 rc = Hyper3DPositionEnd(h3d_file);
                 if( !rc ) throw rc;


                 unsigned int	 conn[4]; 
                 id_node = id_node_0;

                 for(int ii=0;ii<6;ii++) 
                     {
                     for(int jj=0;jj<6;jj++) 
                         {
                         for(int ll=0;ll<6;ll++) 
                             {
   		             conn[0] = id_node  ;
   		             conn[1] = id_node + 1 ;
   		             conn[2] = id_node + 8 ;
   		             conn[3] = id_node + 7 ;
                             nbelemwrite = 1;

                             rc = Hyper3DElementBegin(h3d_file, nbelemwrite, rwall_poolname_id, 
                        	   H3D_ELEM_CONFIG_QUAD4, RigidElem, 
                        	   rwall_poolname_id, node_poolname_id);
    		             if( !rc ) throw rc;

                             elem_id++;
    		             rc = Hyper3DElementWrite(h3d_file, elem_id, conn);
    		             if( !rc ) throw rc;

    		             rc = Hyper3DElementEnd(h3d_file);
    		             if( !rc ) throw rc;

                             id_node = id_node + 1;
                             }
                         id_node = id_node + 1;
                         }
                     id_node = id_node + 7;
                     }

                 id_node = id_node_0 + 293 ;

                 }
             else if(ityp == 4 ) 
                 {

                 int id_node_0 = id_node + 1 ;

                 rc = Hyper3DPositionBegin(h3d_file, 4, node_poolname_id);
                 if( !rc ) throw rc;

                 node[0] =  XWL[i];
                 node[1] =  YWL[i];
                 node[2] =  ZWL[i];
                 id_node ++ ;
                 node_id= id_node;

                 rc = Hyper3DPositionWrite(h3d_file, node_id, node, H3D_NULL_ID, H3D_NULL_ID); 
                 if( !rc ) throw rc;

                 node[0] =  XWL[i] + V1[i];
                 node[1] =  YWL[i] + V2[i];
                 node[2] =  ZWL[i] + V3[i];
                 id_node ++ ;
                 node_id= id_node;

                 rc = Hyper3DPositionWrite(h3d_file, node_id, node, H3D_NULL_ID, H3D_NULL_ID); 
                 if( !rc ) throw rc;

                 node[0] =  XWL[i] + V1[i] + VV1[i];
                 node[1] =  YWL[i] + V2[i] + VV2[i];
                 node[2] =  ZWL[i] + V3[i] + VV3[i];
                 id_node ++ ;
                 node_id= id_node;

                 rc = Hyper3DPositionWrite(h3d_file, node_id, node, H3D_NULL_ID, H3D_NULL_ID); 
                 if( !rc ) throw rc;

                 node[0] =  XWL[i] + VV1[i];
                 node[1] =  YWL[i] + VV2[i];
                 node[2] =  ZWL[i] + VV3[i];
                 id_node ++ ;
                 node_id= id_node;

                 rc = Hyper3DPositionWrite(h3d_file, node_id, node, H3D_NULL_ID, H3D_NULL_ID); 
                 if( !rc ) throw rc;

                 rc = Hyper3DPositionEnd(h3d_file);
                 if( !rc ) throw rc;

                 unsigned int	 conn[4]; 
                 conn[0] = id_node - 3 ;
                 conn[1] = id_node - 2 ;
                 conn[2] = id_node - 1 ;
                 conn[3] = id_node  ;

                 nbelemwrite = 1;

                 rc = Hyper3DElementBegin(h3d_file, nbelemwrite, rwall_poolname_id, 
                        	   H3D_ELEM_CONFIG_QUAD4, RigidElem, 
                        	   rwall_poolname_id, node_poolname_id);
                 if( !rc ) throw rc;

                 elem_id++;
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

void _FCALL C_H3D_CREATE_RWALLS(int *NOM_OPT, int *LNOPT1, int *I16D, int *NPRW, int *NRWALL, int *MAX_NOD_ID,
                          my_real *XWL, my_real *YWL , my_real *ZWL, my_real *V1, my_real *V2, my_real *V3, 
                          my_real *VV1, my_real *VV2 , my_real *VV3, my_real *XL , my_real *XN, my_real *YN , 
                          my_real *ZN)
{c_h3d_create_rwalls_ (NOM_OPT, LNOPT1, I16D, NPRW, NRWALL, MAX_NOD_ID,
                          XWL, YWL, ZWL, V1, V2, V3, VV1, VV2 , VV3, XL, XN, YN , ZN  );}

void c_h3d_create_rwalls__ (int *NOM_OPT, int *LNOPT1, int *I16D, int *NPRW, int *NRWALL, int *MAX_NOD_ID,
                          my_real *XWL, my_real *YWL , my_real *ZWL, my_real *V1, my_real *V2, my_real *V3, 
                          my_real *VV1, my_real *VV2 , my_real *VV3, my_real *XL, my_real *XN, my_real *YN , 
                          my_real *ZN  )
{c_h3d_create_rwalls_ (NOM_OPT, LNOPT1, I16D, NPRW, NRWALL, MAX_NOD_ID,
                          XWL, YWL, ZWL, V1, V2, V3, VV1, VV2 , VV3, XL, XN, YN , ZN   );}

void c_h3d_create_rwalls (int *NOM_OPT, int *LNOPT1, int *I16D, int *NPRW, int *NRWALL, int *MAX_NOD_ID,
                          my_real *XWL, my_real *YWL , my_real *ZWL, my_real *V1, my_real *V2, my_real *V3, 
                          my_real *VV1, my_real *VV2 , my_real *VV3, my_real *XL, my_real *XN, my_real *YN , 
                          my_real *ZN )
{c_h3d_create_rwalls_ (NOM_OPT, LNOPT1, I16D, NPRW, NRWALL, MAX_NOD_ID,
                          XWL, YWL, ZWL, V1, V2, V3, VV1, VV2 , VV3, XL , XN, YN , ZN  );}

}
