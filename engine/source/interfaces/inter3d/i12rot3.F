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
Chd|  I12ROT3                       source/interfaces/inter3d/i12rot3.F
Chd|-- called by -----------
Chd|        INTTI12A                      source/interfaces/interf/intti12.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I12ROT3(XS,XM,X,SKEW,ISKWN,ILEV,IREF,ICEN,NSN,NMN,
     &                   TETS,TETM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ILEV,IREF,ICEN,ISKWN(LISKN,*),NSN,NMN
      my_real
     .   XS(3,*),XM(3,*),X(3,*),SKEW(LSKEW,*),TETS(*),TETM(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ORIG
      my_real
     . XX(3),XO(3),XR(3),RR,TET,P(9)
C
      IF(ILEV==1)THEN
C        ORIG=ISKWN(1,IREF+1)
       IF(ICEN>0)THEN
         XO(1)=X(1,ICEN)
         XO(2)=X(2,ICEN)
         XO(3)=X(3,ICEN)
       ELSE
         XO(1)=ZERO
         XO(2)=ZERO
         XO(3)=ZERO
C
       ENDIF
       DO I=1,9
         P(I)=SKEW(I,IREF+1)
       ENDDO
       DO I=1,NSN
         XX(1)=XS(1,I)-XO(1)
         XX(2)=XS(2,I)-XO(2)
         XX(3)=XS(3,I)-XO(3)
         XR(1)=P(1)*XX(1)+P(2)*XX(2)+P(3)*XX(3)
         XR(2)=P(4)*XX(1)+P(5)*XX(2)+P(6)*XX(3)
         XR(3)=P(7)*XX(1)+P(8)*XX(2)+P(9)*XX(3)
         RR=SQRT(XR(2)**2+XR(3)**2)
         TET=ATAN2(XR(3),XR(2))
         XS(1,I)=XR(1)
         XS(2,I)=RR
         XS(3,I)=TET
         TETS(I)=TET
       ENDDO
       DO I=1,NMN
         XX(1)=XM(1,I)-XO(1)
         XX(2)=XM(2,I)-XO(2)
         XX(3)=XM(3,I)-XO(3)
         XR(1)=P(1)*XX(1)+P(2)*XX(2)+P(3)*XX(3)
         XR(2)=P(4)*XX(1)+P(5)*XX(2)+P(6)*XX(3)
         XR(3)=P(7)*XX(1)+P(8)*XX(2)+P(9)*XX(3)
         RR=SQRT(XR(2)**2+XR(3)**2)
         TET=ATAN2(XR(3),XR(2))
         XM(1,I)=XR(1)
         XM(2,I)=RR
         XM(3,I)=TET
         TETM(I)=TET
       ENDDO
      ENDIF
      RETURN
      END


