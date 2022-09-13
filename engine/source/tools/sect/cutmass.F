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
Chd|  CUTMASS                       source/tools/sect/cutmass.F   
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE CUTMASS(NC,X,AL,NODCUT,NELCUT,VEL,V,IVOIS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   X(3,*),AL(*) ,VEL(3,*),V(3,*)
      INTEGER NC(5,*),NODCUT,NELCUT,IVOIS(2,*)
C-----------------------------------------------
C     REAL
      my_real
     . X13,Y13,Z13,X24,Y24,Z24,XX,YY,ZZ,S,UN6
      INTEGER I,J,N1,N2,N3,N4
      REAL R4
C-----------------------------------------------
      UN6=ONE_OVER_6
C
      DO 10 I=1,NODCUT
      N1=IVOIS(1,I)
      N2=IVOIS(2,I)
      DO 10 J=1,3
 10   V(J,I)=AL(I)*VEL(J,N2)+(1.-AL(I))*VEL(J,N1)
C
      DO 15 I=1,NODCUT
 15   AL(I)=0.
      DO 20 I=1,NELCUT
        N1=NC(1,I)
        N2=NC(2,I)
        N3=NC(3,I)
        N4=NC(4,I)
        X13=X(1,N3)-X(1,N1)
        Y13=X(2,N3)-X(2,N1)
        Z13=X(3,N3)-X(3,N1)
        X24=X(1,N4)-X(1,N2)
        Y24=X(2,N4)-X(2,N2)
        Z24=X(3,N4)-X(3,N2)
        XX=Y13*Z24-Z13*Y24
        YY=Z13*X24-X13*Z24
        ZZ=X13*Y24-Y13*X24
        IF(N3==N4)THEN
           S= UN6
        ELSE
           S=ONE_OVER_8
        ENDIF
C        S=S*SQRT(XX**2+YY**2+ZZ**2)
        AL(N1)=AL(N1)+S*(V(1,N1)*XX+V(2,N1)*YY+V(3,N1)*ZZ)
        AL(N2)=AL(N2)+S*(V(1,N2)*XX+V(2,N2)*YY+V(3,N2)*ZZ)
        AL(N3)=AL(N3)+S*(V(1,N3)*XX+V(2,N3)*YY+V(3,N3)*ZZ)
        IF(N3/=N4)AL(N4)=AL(N4)+S*(V(1,N4)*XX+V(2,N4)*YY+V(3,N4)*ZZ)
 20   CONTINUE
      DO 30 I=1,NODCUT
         R4 =     AL(I)
         CALL WRITE_R_C(R4,1)
 30   CONTINUE
C-----------------------------------------------
      RETURN
      END
