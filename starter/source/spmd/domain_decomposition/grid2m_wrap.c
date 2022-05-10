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
#include <stdlib.h>

#define _FCALL 


int METIS_PartGraphKway(int *NELEM, int  *NCOND, int  *XADJ,  int  *ADJNCY,
                        int *IWD,   int *vsize,  int *ADJWGT2,int *NNODE, float * tpwgts,
                        float *UBVEC, int  *OPTIONS, int *NEC, int *CEP);

int METIS_PartGraphRecursive(int *NELEM, int  *NCOND, int  *XADJ,  int  *ADJNCY,
                        int *IWD,   int *vsize,  int *ADJWGT2,int *NNODE, float * tpwgts,
                        float *UBVEC, int  *OPTIONS, int *NEC, int *CEP);



int wrap_metis_partgraphkway_(int *NELEM, int  *NCOND, int  *XADJ, int  *ADJNCY,
                              int *IWD,  int *NNODE, 
                              float *UBVEC, int  *OPTIONS, int *NEC, int *CEP)
{
  int *vsize=NULL;
  int *ADJWGT2=NULL;
  float *tpwgts=NULL;
  int IERR1;

          IERR1 = METIS_PartGraphKway(
            NELEM,NCOND,XADJ,ADJNCY,
            IWD,vsize,ADJWGT2,NNODE,tpwgts,
            UBVEC,OPTIONS,NEC,CEP);

   if (vsize!=NULL) free(vsize);
   if (ADJWGT2!=NULL) free(ADJWGT2);
   if (tpwgts!=NULL) free(tpwgts);

   return IERR1;

}

int wrap_metis_partgraphkway(int *NELEM, int  *NCOND, int  *XADJ, int  *ADJNCY,
                              int *IWD,  int *NNODE, 
                              float *UBVEC, int  *OPTIONS, int *NEC, int *CEP)
{
  int *vsize=NULL;
  int *ADJWGT2=NULL;
  float *tpwgts=NULL;
  int IERR1;

          IERR1 = METIS_PartGraphKway(
            NELEM,NCOND,XADJ,ADJNCY,
            IWD,vsize,ADJWGT2,NNODE,tpwgts,
            UBVEC,OPTIONS,NEC,CEP);

   if (vsize!=NULL) free(vsize);
   if (ADJWGT2!=NULL) free(ADJWGT2);
   if (tpwgts!=NULL) free(tpwgts);

   return IERR1;
}

int wrap_metis_partgraphkway__(int *NELEM, int  *NCOND, int  *XADJ, int  *ADJNCY,
                              int *IWD,  int *NNODE, 
                              float *UBVEC, int  *OPTIONS, int *NEC, int *CEP)
{
  int *vsize=NULL;
  int *ADJWGT2=NULL;
  float *tpwgts=NULL;
  int IERR1;

          IERR1 = METIS_PartGraphKway(
            NELEM,NCOND,XADJ,ADJNCY,
            IWD,vsize,ADJWGT2,NNODE,tpwgts,
            UBVEC,OPTIONS,NEC,CEP);

   if (vsize!=NULL) free(vsize);
   if (ADJWGT2!=NULL) free(ADJWGT2);
   if (tpwgts!=NULL) free(tpwgts);

   return IERR1;

}

int _FCALL WRAP_METIS_PARTGRAPHKWAY(int *NELEM, int  *NCOND, int  *XADJ, int  *ADJNCY,
                              int *IWD,  int *NNODE, 
                              float *UBVEC, int  *OPTIONS, int *NEC, int *CEP)
{
  int *vsize=NULL;
  int *ADJWGT2=NULL;
  float *tpwgts=NULL;
  int IERR1;

          IERR1 = METIS_PartGraphKway(
            NELEM,NCOND,XADJ,ADJNCY,
            IWD,vsize,ADJWGT2,NNODE,tpwgts,
            UBVEC,OPTIONS,NEC,CEP);

   if (vsize!=NULL) free(vsize);
   if (ADJWGT2!=NULL) free(ADJWGT2);
   if (tpwgts!=NULL) free(tpwgts);

   return IERR1;
}

int wrap_metis_partgraphrecursive_(int *NELEM, int  *NCOND, int  *XADJ, int  *ADJNCY,
                              int *IWD,  int *NNODE, 
                              float *UBVEC, int  *OPTIONS, int *NEC, int *CEP)
{
  int *vsize=NULL;
  int *ADJWGT2=NULL;
  float *tpwgts=NULL;
  int IERR1;

          IERR1 = METIS_PartGraphRecursive(
            NELEM,NCOND,XADJ,ADJNCY,
            IWD,vsize,ADJWGT2,NNODE,tpwgts,
            UBVEC,OPTIONS,NEC,CEP);

   if (vsize!=NULL) free(vsize);
   if (ADJWGT2!=NULL) free(ADJWGT2);
   if (tpwgts!=NULL) free(tpwgts);

   return IERR1;

}

int wrap_metis_partgraphrecursive(int *NELEM, int  *NCOND, int  *XADJ, int  *ADJNCY,
                              int *IWD,  int *NNODE, 
                              float *UBVEC, int  *OPTIONS, int *NEC, int *CEP)
{
  int *vsize=NULL;
  int *ADJWGT2=NULL;
  float *tpwgts=NULL;
  int IERR1;

          IERR1 = METIS_PartGraphRecursive(
            NELEM,NCOND,XADJ,ADJNCY,
            IWD,vsize,ADJWGT2,NNODE,tpwgts,
            UBVEC,OPTIONS,NEC,CEP);

   if (vsize!=NULL) free(vsize);
   if (ADJWGT2!=NULL) free(ADJWGT2);
   if (tpwgts!=NULL) free(tpwgts);

   return IERR1;

}

int wrap_metis_partgraphrecursive__(int *NELEM, int  *NCOND, int  *XADJ, int  *ADJNCY,
                              int *IWD,  int *NNODE, 
                              float *UBVEC, int  *OPTIONS, int *NEC, int *CEP)
{
  int *vsize=NULL;
  int *ADJWGT2=NULL;
  float *tpwgts=NULL;
  int IERR1;

          IERR1 = METIS_PartGraphRecursive(
            NELEM,NCOND,XADJ,ADJNCY,
            IWD,vsize,ADJWGT2,NNODE,tpwgts,
            UBVEC,OPTIONS,NEC,CEP);

   if (vsize!=NULL) free(vsize);
   if (ADJWGT2!=NULL) free(ADJWGT2);
   if (tpwgts!=NULL) free(tpwgts);

   return IERR1;

}

int _FCALL WRAP_METIS_PARTGRAPHRECURSIVE(int *NELEM, int  *NCOND, int  *XADJ, int  *ADJNCY,
                                         int *IWD,  int *NNODE, 
                                         float *UBVEC, int  *OPTIONS, int *NEC, int *CEP)
{
  int *vsize=NULL;
  int *ADJWGT2=NULL;
  float *tpwgts=NULL;
  int IERR1;

          IERR1 = METIS_PartGraphRecursive(
            NELEM,NCOND,XADJ,ADJNCY,
            IWD,vsize,ADJWGT2,NNODE,tpwgts,
            UBVEC,OPTIONS,NEC,CEP);

   if (vsize!=NULL) free(vsize);
   if (ADJWGT2!=NULL) free(ADJWGT2);
   if (tpwgts!=NULL) free(tpwgts);

   return IERR1;

}





