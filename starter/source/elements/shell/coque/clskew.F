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
Chd|  CLSKEW3                       source/elements/shell/coque/clskew.F
Chd|-- called by -----------
Chd|        CDKEVEC3                      source/elements/sh3n/coquedk/cdkevec3.F
Chd|        CNEVECI                       source/elements/shell/coqueba/cneveci.F
Chd|        SPCOOR3                       source/elements/solid/sconnect/spcoor3.F
Chd|        XYZLOC3N                      source/elements/xfem/lslocal.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CLSKEW3(JFT,JLT,IREP,
     .           RX, RY, RZ,SX, SY, SZ, 
     .           E1X, E2X, E3X, E1Y, E2Y, E3Y, E1Z, E2Z, E3Z, DET)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,IREP
C     REAL ou REAL*8
      my_real
     .   RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ), 
     .   SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ),
     .   E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .   E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .   E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ), DET(MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      MY_REAL C1,C2,CC
c=======================================================================
C IREP=0 ->QEPH IREP=1 ->Q4, IREP=2-> E1=R(ISHFRAM=1)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO I=JFT,JLT
C---------E3------------
       E3X(I) = RY(I) * SZ(I) - RZ(I) * SY(I) 
       E3Y(I) = RZ(I) * SX(I) - RX(I) * SZ(I) 
       E3Z(I) = RX(I) * SY(I) - RY(I) * SX(I) 
       DET(I) = SQRT(E3X(I)*E3X(I) + E3Y(I)*E3Y(I) + E3Z(I)*E3Z(I))
       DET(I) = MAX(EM20,DET(I))
       CC = UN / DET(I)
       E3X(I) = E3X(I) * CC 
       E3Y(I) = E3Y(I) * CC 
       E3Z(I) = E3Z(I) * CC 
      ENDDO 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF (IREP.EQ.2) THEN
       DO I=JFT,JLT
        E1X(I) = RX(I)
        E1Y(I) = RY(I)
        E1Z(I) = RZ(I)
       ENDDO 
      ELSEIF (IREP.EQ.1) THEN
       DO I=JFT,JLT
        C2 = SQRT(SX(I)*SX(I) + SY(I)*SY(I) + SZ(I)*SZ(I))
        E1X(I) = RX(I)*C2+(SY(I)*E3Z(I)-SZ(I)*E3Y(I))
        E1Y(I) = RY(I)*C2+(SZ(I)*E3X(I)-SX(I)*E3Z(I))
        E1Z(I) = RZ(I)*C2+(SX(I)*E3Y(I)-SY(I)*E3X(I))
       ENDDO 
      ELSE
       DO I=JFT,JLT
        C1 = SQRT(RX(I)*RX(I) + RY(I)*RY(I) + RZ(I)*RZ(I))
        C2 = SQRT(SX(I)*SX(I) + SY(I)*SY(I) + SZ(I)*SZ(I))
        E1X(I) = RX(I)*C2+(SY(I)*E3Z(I)-SZ(I)*E3Y(I))*C1
        E1Y(I) = RY(I)*C2+(SZ(I)*E3X(I)-SX(I)*E3Z(I))*C1
        E1Z(I) = RZ(I)*C2+(SX(I)*E3Y(I)-SY(I)*E3X(I))*C1
       ENDDO 
      ENDIF 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       DO I=JFT,JLT
        C1 = SQRT(E1X(I)*E1X(I) + E1Y(I)*E1Y(I) + E1Z(I)*E1Z(I))
        IF (C1.NE.ZERO) C1 = UN / C1
        E1X(I) = E1X(I)*C1
        E1Y(I) = E1Y(I)*C1
        E1Z(I) = E1Z(I)*C1
        E2X(I) = E3Y(I) * E1Z(I) - E3Z(I) * E1Y(I)
        E2Y(I) = E3Z(I) * E1X(I) - E3X(I) * E1Z(I)
        E2Z(I) = E3X(I) * E1Y(I) - E3Y(I) * E1X(I)
       ENDDO 
c-----------
      RETURN
      END
