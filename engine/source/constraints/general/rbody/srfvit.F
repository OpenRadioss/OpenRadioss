Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
Copyright>    
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>    
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>    
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>    
Copyright>    
Copyright>        Commercial Alternative: Altair Radioss Software 
Copyright>    
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss 
Copyright>        software under a commercial license.  Contact Altair to discuss further if the 
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.    
Chd|====================================================================
Chd|  SRFVIT                        source/constraints/general/rbody/srfvit.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ROTBMR                        source/tools/skew/rotbmr.F    
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE SRFVIT(X,V,VR,A,AR,
     2            NPBY,RBY  ,MS  ,IN  ,
     3            IGRSURF ,BUFSF )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C==============================================
C
C     * Retour des conditions cinematiques vers les surfaces
C     * Envoi des positions ou devra etre effectue le calcul des forces
C             dans les interfaces, au pas suivant.
C==============================================
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPBY(NNPBY,*)
C     REAL ou REAL*8
      my_real
     .   X(3,*)  ,V(3,*) ,VR(3,*) ,A(3,*) ,AR(3,*) ,
     .   RBY(NRBY,*) ,IN(*) ,MS(*),
     .   BUFSF(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  ISU, ADRSRF, IM, NRBOD
      my_real
     .       VI(3), VM(3), VRM(3)
C

!$OMP DO

      DO ISU=1,NSURF

       IF ( IGRSURF(ISU)%TYPE.EQ.101 ) THEN
        NRBOD=IGRSURF(ISU)%NB_MADYMO
        IF ( NRBOD.NE.0 ) THEN
C         ISURF(9,ISU)==1 (RIGID BODIES) !
Clm       A chaque instant (et a TT=0. dans le Starter), 
C         le Rigid Body transmet la position et les vitesses du noeud main 
C         a la surface.
          IM=NPBY(1,NRBOD)
          ADRSRF=IGRSURF(ISU)%IAD_BUFR
          BUFSF(ADRSRF+16)=X(1,IM)
          BUFSF(ADRSRF+17)=X(2,IM)
          BUFSF(ADRSRF+18)=X(3,IM)
          VM(1) =V(1,IM) +A(1,IM)*DT12
          VM(2) =V(2,IM) +A(2,IM)*DT12
          VM(3) =V(3,IM) +A(3,IM)*DT12
          VRM(1)=VR(1,IM)+AR(1,IM)*DT12
          VRM(2)=VR(2,IM)+AR(2,IM)*DT12
          VRM(3)=VR(3,IM)+AR(3,IM)*DT12
          BUFSF(ADRSRF+19)= VM(1)
          BUFSF(ADRSRF+20)= VM(2)
          BUFSF(ADRSRF+21)= VM(3)
          BUFSF(ADRSRF+22)=VRM(1)
          BUFSF(ADRSRF+23)=VRM(2)
          BUFSF(ADRSRF+24)=VRM(3)
C         Translation et Rotation de la surface.
Clm deplacer apres SORTIES :
          BUFSF(ADRSRF+4)=BUFSF(ADRSRF+4)+VM(1)*DT2
          BUFSF(ADRSRF+5)=BUFSF(ADRSRF+5)+VM(2)*DT2
          BUFSF(ADRSRF+6)=BUFSF(ADRSRF+6)+VM(3)*DT2
          CALL ROTBMR(VRM,BUFSF(ADRSRF+7),DT2)
        END IF
       END IF
      END DO

!$OMP END DO

C-----------------------------------------------
 200  CONTINUE
      RETURN
      END
