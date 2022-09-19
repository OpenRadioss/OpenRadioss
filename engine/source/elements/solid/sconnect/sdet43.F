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
Chd|  SDET43                        source/elements/solid/sconnect/sdet43.F
Chd|-- called by -----------
Chd|        SCOOR43                       source/elements/solid/sconnect/scoor43.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SDET43(
     .         NEL  ,X    ,Y    ,Z    ,AREA ,AREAP,OFF  ,SOLID_ID,
     .         E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
CC-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "units_c.inc"
#include "comlock.inc"
#include "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      INTEGER SOLID_ID(*)
C     REAL
      my_real
     .   AREA(NEL),AREAP(MVSIZ,4),OFF(NEL),
     .   X(NEL,8) ,Y(NEL,8),Z(NEL,8),RXX(NEL),RYY(NEL),
     .   E1X(*),E2X(*),E3X(*),E1Y(*),E2Y(*),E3Y(*),E1Z(*),E2Z(*),E3Z(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NINDX
      INTEGER INDX(NEL)
C     REAL
      my_real
     .   PG, XX1,XX2,XX3,XX4,YY1,YY2,YY3,YY4,ZZ1,ZZ2,ZZ3,ZZ4,
     .   L1,L2,XL1,XL2,XL3,XL4,YL2,YL3,YL4,
     .   MX13,MY13,MX23,MX34,MY23,MY34,J0,J1,J2
      my_real
     .   VCX(4),VCY(4),
     .   P1X(NEL), P2X(NEL), P3X(NEL), P4X(NEL),
     .   P1Y(NEL), P2Y(NEL), P3Y(NEL), P4Y(NEL),
     .   P1Z(NEL), P2Z(NEL), P3Z(NEL), P4Z(NEL)
      DATA   PG/0.5773502691896257645091488/
C=======================================================================
      NINDX = 0
C     Calcule AREA / P Gauss
      DO I=1,NEL
        P1X(I)=(X(I,1)+X(I,5))*HALF
        P1Y(I)=(Y(I,1)+Y(I,5))*HALF
        P1Z(I)=(Z(I,1)+Z(I,5))*HALF
        P2X(I)=(X(I,2)+X(I,6))*HALF
        P2Y(I)=(Y(I,2)+Y(I,6))*HALF
        P2Z(I)=(Z(I,2)+Z(I,6))*HALF
        P3X(I)=(X(I,3)+X(I,7))*HALF
        P3Y(I)=(Y(I,3)+Y(I,7))*HALF
        P3Z(I)=(Z(I,3)+Z(I,7))*HALF
        P4X(I)=(X(I,4)+X(I,8))*HALF
        P4Y(I)=(Y(I,4)+Y(I,8))*HALF
        P4Z(I)=(Z(I,4)+Z(I,8))*HALF
      ENDDO
C
      DO I=1,NEL
        XX1 = P2X(I)-P1X(I)
        YY1 = P2Y(I)-P1Y(I)
        ZZ1 = P2Z(I)-P1Z(I)
        XL2 = E1X(I)*XX1 + E1Y(I)*YY1 + E1Z(I)*ZZ1
        YL2 = E2X(I)*XX1 + E2Y(I)*YY1 + E2Z(I)*ZZ1
c
        XX3 = P3X(I)-P1X(I)
        YY3 = P3Y(I)-P1Y(I)
        ZZ3 = P3Z(I)-P1Z(I)
        XL3 = E1X(I)*XX3 + E1Y(I)*YY3 + E1Z(I)*ZZ3
        YL3 = E2X(I)*XX3 + E2Y(I)*YY3 + E2Z(I)*ZZ3
c
        XX4 = P4X(I)-P1X(I)
        YY4 = P4Y(I)-P1Y(I)
        ZZ4 = P4Z(I)-P1Z(I)
        XL4 = E1X(I)*XX4 + E1Y(I)*YY4 + E1Z(I)*ZZ4
        YL4 = E2X(I)*XX4 + E2Y(I)*YY4 + E2Z(I)*ZZ4
c
        L1=FOURTH*(XL2+XL3+XL4)
        L2=FOURTH*(YL2+YL3+YL4)
c
        VCX(1)=-L1
        VCX(2)= XL2-L1
        VCX(3)= XL3-L1
        VCX(4)= XL4-L1
        VCY(1)=-L2
        VCY(2)=YL2-L2
        VCY(3)=YL3-L2
        VCY(4)=YL4-L2
c
        MX13=(VCX(1)+VCX(3))*HALF
        MY13=(VCY(1)+VCY(3))*HALF
        MX23=(VCX(2)+VCX(3))*HALF
        MX34=(VCX(3)+VCX(4))*HALF
        MY23=(VCY(2)+VCY(3))*HALF
        MY34=(VCY(3)+VCY(4))*HALF
c
        J1 = (MX23*MY13-MX13*MY23)*PG
        J2 =-(MX13*MY34-MX34*MY13)*PG
        J0 = AREA(I)*FOURTH
C-----------
        AREAP(I,1) = J0+J2-J1
        AREAP(I,2) = J0+J2+J1
        AREAP(I,3) = J0-J2+J1
        AREAP(I,4) = J0-J2-J1
        IF (OFF(I) == ONE .and. ( AREA(I) < ZERO .or.
     .      AREAP(I,1) < ZERO .or. AREAP(I,2) < ZERO .or.
     .      AREAP(I,3) < ZERO .or. AREAP(I,4) < ZERO)) THEN
          NINDX = NINDX+1
          INDX(NINDX) = I
          OFF(I) = ZERO
        ENDIF
      ENDDO
C-----------
      IF (NINDX > 0) THEN
        DO I=1,NINDX
#include "lockon.inc"
          WRITE(IOUT ,1000) SOLID_ID(INDX(I))
          WRITE(ISTDO,1100) SOLID_ID(INDX(I))
#include "lockoff.inc"
        END DO
      ENDIF
C-----------
 1000 FORMAT(5X,'NEGATIVE AREA: DELETE CONNECTION SOLID ELEMENT ',I10)
 1100 FORMAT(5X,'NEGATIVE AREA: DELETE CONNECTION SOLID ELEMENT ',I10)
C-----------
      RETURN
      END




