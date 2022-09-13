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
Chd|  CUTCNT                        source/tools/sect/cutcnt.F    
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CUTCNT(ICUT,XCUT,IXS,XYZ0,D,LEN)
C------------------------------------------
C COMPTAGE DU NOMBRE D'ELEMENTS COUPES
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scrcut_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   XCUT(7,*),XYZ0(3,*),D(3,*)
      INTEGER ICUT(44,*),IXS(NIXS,*),LEN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IC,I,J,K,ITYP
      my_real
     .   DIST,DIST0,DISTMIN,DISTMAX,
     .   DD,DX1,DY1,DZ1,DX2,DY2,DZ2,VNX,VNY,VNZ,X,Y,Z 
C
      LEN=0
      DO 10 IC=1,NCUTS
      ITYP=ICUT(1,IC)
      IF(ITYP==1)THEN
       DD=DTANIM*XCUT(7,IC)
       XCUT(1,IC)=XCUT(1,IC)+DD*XCUT(4,IC)
       XCUT(2,IC)=XCUT(2,IC)+DD*XCUT(5,IC)
       XCUT(3,IC)=XCUT(3,IC)+DD*XCUT(6,IC)   
      ELSEIF(ITYP==3)THEN
       XCUT(1,IC)=XYZ0(1,ICUT(2,IC))
       XCUT(2,IC)=XYZ0(2,ICUT(2,IC))
       XCUT(3,IC)=XYZ0(3,ICUT(2,IC))
       DX1=XYZ0(1,ICUT(3,IC))-XCUT(1,IC)
       DY1=XYZ0(2,ICUT(3,IC))-XCUT(2,IC)
       DZ1=XYZ0(3,ICUT(3,IC))-XCUT(3,IC)
       DX2=XYZ0(1,ICUT(4,IC))-XCUT(1,IC)
       DY2=XYZ0(2,ICUT(4,IC))-XCUT(2,IC)
       DZ2=XYZ0(3,ICUT(4,IC))-XCUT(3,IC)
       VNX=DY1*DZ2-DY2*DZ1
       VNY=DZ1*DX2-DZ2*DX1
       VNZ=DX1*DY2-DX2*DY1
       DD=SQRT(VNX**2+VNY**2+VNZ**2)
       IF(DD>EM10)THEN
         XCUT(4,IC)=VNX/DD
         XCUT(5,IC)=VNY/DD
         XCUT(6,IC)=VNZ/DD
       ENDIF
      ENDIF
C
      DIST0=XCUT(4,IC)*XCUT(1,IC)
     .     +XCUT(5,IC)*XCUT(2,IC)
     .     +XCUT(6,IC)*XCUT(3,IC)
      DO100 I=1,NUMELS
        DISTMIN= EP30
        DISTMAX=-EP30
        DO110 J=1,8
          K=IXS(J+1,I)
          X=XYZ0(1,K)
          Y=XYZ0(2,K)
          Z=XYZ0(3,K)
          IF(ITYP==2)THEN
            X=X-D(1,K)
            Y=Y-D(2,K)
            Z=Z-D(3,K)
          ENDIF
          DIST=X*XCUT(4,IC)+Y*XCUT(5,IC)+Z*XCUT(6,IC)-DIST0
          DISTMIN=MIN(DIST,DISTMIN)
 110      DISTMAX=MAX(DIST,DISTMAX)
        IF(DISTMIN*DISTMAX<= ZERO)LEN=LEN+1
 100  CONTINUE
C
  10  CONTINUE
      RETURN
      END
