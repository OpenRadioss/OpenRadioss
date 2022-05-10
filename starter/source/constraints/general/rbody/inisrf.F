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
C===============================================
C
Chd|====================================================================
Chd|  INISRF                        source/constraints/general/rbody/inisrf.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INISRF(X,V,VR,NPBY,RBY,IGRSURF,BUFSF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
C===============================================
C    * Verifie qu'il n'y a pas deux RIGID BODIES (.../... deux conditions
C      cinematiques)
C    qui definissent le mvt de la meme Surface.
C    * Initialise la position et les vitesses du point ou calculer les forces
C      dans les interfaces.
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPBY(NNPBY,*)
C     REAL ou REAL*8
      my_real
     .   X(3,*),V(3,*),VR(3,*),RBY(NRBY,*),BUFSF(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, ISU, ADRSRF, IM
C-----------------------------------------------
      DO 100 N=1,NRBYKIN
        ISU=NPBY(8,N)
        IF (ISU.EQ.0) GOTO 100
        IF (IGRSURF(ISU)%NB_MADYMO.NE.0) GOTO 999
C
        IGRSURF(ISU)%NB_MADYMO=N
        IGRSURF(ISU)%TYPE_MADYMO=1
C
        IM=NPBY(1,N)
        ADRSRF=IGRSURF(ISU)%IAD_BUFR
        BUFSF(ADRSRF+16)=X(1,IM)
        BUFSF(ADRSRF+17)=X(2,IM)
        BUFSF(ADRSRF+18)=X(3,IM)
        BUFSF(ADRSRF+19)=V(1,IM)
        BUFSF(ADRSRF+20)=V(2,IM)
        BUFSF(ADRSRF+21)=V(3,IM)
        BUFSF(ADRSRF+22)  =VR(1,IM)
        BUFSF(ADRSRF+23)=VR(2,IM)
        BUFSF(ADRSRF+24)=VR(3,IM)
 100  CONTINUE
      GOTO 9999
C-----------------------------------------------
 999  CONTINUE
C      WRITE(ISTDO,*)' ** ERROR : MOTION OF SURFACE MULTIPLY DEFINED'
C      WRITE(IOUT,*) ' ** ERROR : SURFACES MOTION DEFINITION :'
C      WRITE(IOUT,*) ' ** ERROR : MOTION OF SURFACE MULTIPLY DEFINED :'
C      WRITE(IOUT,*) '            MOTION OF SURFACE ',ISURF(1,ISU),
C     .                 ' IS DEFINED BY MORE THAN ONE RIGID BODY.'
C      IERR=IERR+1
      CALL ANCMSG(MSGID=146,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO_BLIND_1,
     .            I1=IGRSURF(ISU)%ID)
C-----------------------------------------------
 9999 CONTINUE
      RETURN
      END
