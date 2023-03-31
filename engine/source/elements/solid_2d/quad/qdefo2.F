Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  QDEFO2                        source/elements/solid_2d/quad/qdefo2.F
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE QDEFO2(
     1   V,       W,       Y1,      Y2,
     2   Y3,      Y4,      Z1,      Z2,
     3   Z3,      Z4,      VY1,     VY2,
     4   VY3,     VY4,     VZ1,     VZ2,
     5   VZ3,     VZ4,     PY1,     PY2,
     6   PZ1,     PZ2,     WYZ,     DYZ,
     7   DZY,     EYY,     EZZ,     ETT,
     8   EYZ,     EYT,     EZT,     RX,
     9   RY,      RZ,      SX,      SY,
     A   SZ,      TX,      TY,      TZ,
     B   VOLN,    AIRE,    AIREM,   NC1,
     C   NC2,     NC3,     NC4,     NEL)
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
      INTEGER, INTENT(IN) :: NEL
      my_real
     .   V(3,*), W(3,*),
     .   Y1(*),Y2(*),Y3(*),Y4(*),Z1(*),Z2(*),Z3(*),Z4(*),
     .   VY1(*),VY2(*),VY3(*),VY4(*),VZ1(*),VZ2(*),VZ3(*),VZ4(*),
     .   PY1(*), PY2(*), PZ1(*), PZ2(*),
     .   WYZ(*), DYZ(*), DZY(*),
     .   EYY(*),EZZ(*), ETT(*), EYZ(*), EYT(*), EZT(*),
     .   RX(*),RY(*), RZ(*), SX(*), SY(*), SZ(*), TX(*), TY(*), TZ(*),
     .   VOLN(*),AIRE(*),AIREM(*)
C
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   VY13(MVSIZ), VY24(MVSIZ), VZ13(MVSIZ),VZ24(MVSIZ),
     .   YM1(MVSIZ), YM2(MVSIZ), YM3(MVSIZ), YM4(MVSIZ),
     .   ZM1(MVSIZ), ZM2(MVSIZ), ZM3(MVSIZ), ZM4(MVSIZ), DTH,
     .   YAVG(MVSIZ),A1(MVSIZ) , A2(MVSIZ)
C-----------------------------------------------
      DTH=HALF*DT1
      
      DO I=1,NEL
        !from qforc2 subroutine : &W  = &V  
        !from bforc2 subroutine : &W /= &V  
        VY1(I)=W(2,NC1(I))
        VZ1(I)=W(3,NC1(I))
        VY2(I)=W(2,NC2(I))
        VZ2(I)=W(3,NC2(I))
        VY3(I)=W(2,NC3(I))
        VZ3(I)=W(3,NC3(I))
        VY4(I)=W(2,NC4(I))
        VZ4(I)=W(3,NC4(I))
      ENDDO

      DO I=1,NEL
        YM1(I)=Y1(I)-DTH*VY1(I)
        YM2(I)=Y2(I)-DTH*VY2(I)
        YM3(I)=Y3(I)-DTH*VY3(I)
        YM4(I)=Y4(I)-DTH*VY4(I)
        ZM1(I)=Z1(I)-DTH*VZ1(I)
        ZM2(I)=Z2(I)-DTH*VZ2(I)
        ZM3(I)=Z3(I)-DTH*VZ3(I)
        ZM4(I)=Z4(I)-DTH*VZ4(I)
      ENDDO

      DO I=1,NEL
        VY1(I)=V(2,NC1(I))
        VZ1(I)=V(3,NC1(I))
        VY2(I)=V(2,NC2(I))
        VZ2(I)=V(3,NC2(I))
        VY3(I)=V(2,NC3(I))
        VZ3(I)=V(3,NC3(I))
        VY4(I)=V(2,NC4(I))
        VZ4(I)=V(3,NC4(I))
      ENDDO

      DO I=1,NEL
        PY1(I)=HALF*(ZM2(I)-ZM4(I))
        PY2(I)=HALF*(ZM3(I)-ZM1(I))
        PZ1(I)=HALF*(YM4(I)-YM2(I))
        PZ2(I)=HALF*(YM1(I)-YM3(I))      
      ENDDO

      DO I=1,NEL
        A1(I) =YM2(I)*(ZM3(I)-ZM4(I))+YM3(I)*(ZM4(I)-ZM2(I))+YM4(I)*(ZM2(I)-ZM3(I))
        A2(I) =YM2(I)*(ZM4(I)-ZM1(I))+YM4(I)*(ZM1(I)-ZM2(I))+YM1(I)*(ZM2(I)-ZM4(I))
        AIREM(I)=(A1(I)+A2(I))*HALF
        !AUTRE FORMULE AIREM(I)=2.*(PZ2(I)*PY1(I)-PZ1(I)*PY2(I))
        YAVG(I) =YM1(I)+YM2(I)+YM3(I)+YM4(I)
      ENDDO

      DO I=1,NEL
        VY13(I)=VY1(I)-VY3(I)
        VY24(I)=VY2(I)-VY4(I)
        VZ13(I)=VZ1(I)-VZ3(I)
        VZ24(I)=VZ2(I)-VZ4(I)
       ENDDO

      DO I=1,NEL
        IF(AIREM(I)>ZERO) THEN
          EYY(I)=(PY1(I)*VY13(I)+PY2(I)*VY24(I))/AIREM(I)
          EZZ(I)=(PZ1(I)*VZ13(I)+PZ2(I)*VZ24(I))/AIREM(I)
          ETT(I)=ZERO
          DZY(I)=(PY1(I)*VZ13(I)+PY2(I)*VZ24(I))/AIREM(I)
          DYZ(I)=(PZ1(I)*VY13(I)+PZ2(I)*VY24(I))/AIREM(I)
          EYT(I)=ZERO
          EZT(I)=ZERO   
        ELSE
          EYY(I)=ZERO
          EZZ(I)=ZERO
          ETT(I)=ZERO
          DZY(I)=ZERO
          DYZ(I)=ZERO
          EYT(I)=ZERO
          EZT(I)=ZERO
        END IF
      ENDDO

      IF(N2D==1) THEN
        DO I=1,NEL
          IF (YAVG(I)<=ZERO) CYCLE
          ETT(I)=(VY1(I)+VY2(I)+VY3(I)+VY4(I))/YAVG(I)
        ENDDO
      ENDIF

      DO I=1,NEL
        EYZ(I)= DZY(I)+DYZ(I)
        WYZ(I)=HALF*DT1*(DZY(I)-DYZ(I))
      ENDDO
C---------------------------------------
C     DERIVEES POUR LE CALCUL DES FORCES
C---------------------------------------
      DO I=1,NEL
        PY1(I)=HALF*(Z2(I)-Z4(I))
        PY2(I)=HALF*(Z3(I)-Z1(I))
        PZ1(I)=HALF*(Y4(I)-Y2(I))
        PZ2(I)=HALF*(Y1(I)-Y3(I))      
      ENDDO
C---------------------------------------
C     REPERE LOCAL POUR MATERIAUX NON ISOTROPES
C---------------------------------------
      DO I=1,NEL
        RX(I)=ONE
        RY(I)=ZERO
        RZ(I)=ZERO
        SX(I)=ZERO    
        SY(I)=-PZ1(I)-PZ2(I)
        SZ(I)=+PY1(I)+PY2(I)
        TX(I)=ZERO
        TY(I)=+PZ1(I)-PZ2(I)
        TZ(I)=-PY1(I)+PY2(I)
      ENDDO

      RETURN
      END
