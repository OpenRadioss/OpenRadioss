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
Chd|  S4COOR3                       source/elements/solid/solide4/s4coor3.F
Chd|-- called by -----------
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S4COOR3(
     1   X,       IXS,     V,       W,
     2   X1,      X2,      X3,      X4,
     3   Y1,      Y2,      Y3,      Y4,
     4   Z1,      Z2,      Z3,      Z4,
     5   VX1,     VX2,     VX3,     VX4,
     6   VY1,     VY2,     VY3,     VY4,
     7   VZ1,     VZ2,     VZ3,     VZ4,
     8   VDX1,    VDX2,    VDX3,    VDX4,
     9   VDY1,    VDY2,    VDY3,    VDY4,
     A   VDZ1,    VDZ2,    VDZ3,    VDZ4,
     B   VDX,     VDY,     VDZ,     VD2,
     C   VIS,     OFFG,    OFF,     SAV,
     D   RHO,     RHOO,    NC1,     NC2,
     E   NC3,     NC4,     NGL,     MXT,
     F   NGEO,    F11,     F21,     F31,
     G   F12,     F22,     F32,     F13,
     H   F23,     F33,     F14,     F24,
     I   F34,     XD1,     XD2,     XD3,
     J   XD4,     YD1,     YD2,     YD3,
     K   YD4,     ZD1,     ZD2,     ZD3,
     L   ZD4,     XDP,     NEL,     JALE,
     M   ISMSTR,  JEUL,    JLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: JALE
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: JEUL
      INTEGER, INTENT(IN) :: JLAG
      INTEGER, INTENT(IN) ::  NEL
      my_real
     .   X(3,*),V(3,*),W(3,*), VIS(MVSIZ),
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ), 
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ), 
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ), 
     .  VX1(MVSIZ), VX2(MVSIZ), VX3(MVSIZ), VX4(MVSIZ), 
     .  VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), VY4(MVSIZ), 
     .  VZ1(MVSIZ), VZ2(MVSIZ), VZ3(MVSIZ), VZ4(MVSIZ), 
     .  VDX1(MVSIZ),VDX2(MVSIZ),VDX3(MVSIZ),VDX4(MVSIZ),
     .  VDY1(MVSIZ),VDY2(MVSIZ),VDY3(MVSIZ),VDY4(MVSIZ),
     .  VDZ1(MVSIZ),VDZ2(MVSIZ),VDZ3(MVSIZ),VDZ4(MVSIZ),
     .  VDX(MVSIZ), VDY(MVSIZ), VDZ(MVSIZ),VD2(MVSIZ),
     .  OFFG(NEL),OFF(MVSIZ),RHO(NEL),
     .  F11(MVSIZ),F21(MVSIZ),F31(MVSIZ),F12(MVSIZ),F22(MVSIZ),F32(MVSIZ),
     .  F13(MVSIZ),F23(MVSIZ),F33(MVSIZ),F14(MVSIZ),F24(MVSIZ),F34(MVSIZ),
     .  RHOO(MVSIZ)
      INTEGER NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ),
     .        MXT(MVSIZ), NGL(MVSIZ),NGEO(MVSIZ)
      INTEGER IXS(NIXS,*)
      
      DOUBLE PRECISION 
     .   XDP(3,*), SAV(NEL,9),
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ)
           
      my_real
     .   OFF_L
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
#include "vectorize.inc"
      DO I=1,NEL
        VIS(I)=ZERO
        NGEO(I)=IXS(10,I)
        NGL(I)=IXS(11,I)
        MXT(I)=IXS(1,I)
        NC1(I)=IXS(2,I)
        NC2(I)=IXS(4,I)
        NC3(I)=IXS(7,I)
        NC4(I)=IXS(6,I)
        RHOO(I)=RHO(I)
      ENDDO
#include "vectorize.inc"
      DO I=1,NEL
        X1(I) =X(1,NC1(I))
        Y1(I) =X(2,NC1(I))
        Z1(I) =X(3,NC1(I))
        X2(I) =X(1,NC2(I))
        Y2(I) =X(2,NC2(I))
        Z2(I) =X(3,NC2(I))
        X3(I) =X(1,NC3(I))
        Y3(I) =X(2,NC3(I))
        Z3(I) =X(3,NC3(I))
        X4(I) =X(1,NC4(I))
        Y4(I) =X(2,NC4(I))
        Z4(I) =X(3,NC4(I))
        VX1(I)=V(1,NC1(I))
        VY1(I)=V(2,NC1(I))
        VZ1(I)=V(3,NC1(I))
        VX2(I)=V(1,NC2(I))
        VY2(I)=V(2,NC2(I))
        VZ2(I)=V(3,NC2(I))
        VX3(I)=V(1,NC3(I))
        VY3(I)=V(2,NC3(I))
        VZ3(I)=V(3,NC3(I))
        VX4(I)=V(1,NC4(I))
        VY4(I)=V(2,NC4(I))
        VZ4(I)=V(3,NC4(I))
      ENDDO

      OFF_L  = ZERO
C----------------------------
C     NODAL COORDINATES     |
C----------------------------
      IF((ISMSTR<=4.AND.JLAG>0).OR.(ISMSTR==12.AND.IDTMIN(1)==3)) THEN

        IF(IRESP == 1) THEN
#include "vectorize.inc"
          DO I=1,NEL
            IF(ABS(OFFG(I))>ONE)THEN
              XD1(I)=SAV(I,1)
              YD1(I)=SAV(I,2)
              ZD1(I)=SAV(I,3)
              XD2(I)=SAV(I,4)
              YD2(I)=SAV(I,5)
              ZD2(I)=SAV(I,6)
              XD3(I)=SAV(I,7)
              YD3(I)=SAV(I,8)
              ZD3(I)=SAV(I,9)
              XD4(I)=ZERO
              YD4(I)=ZERO
              ZD4(I)=ZERO
              OFF(I) = ABS(OFFG(I))-ONE
C             OFF_L  = MIN(OFF_L,OFFG(I))
            ELSE
              XD1(I)=XDP(1,NC1(I))
              YD1(I)=XDP(2,NC1(I))
              ZD1(I)=XDP(3,NC1(I))
              XD2(I)=XDP(1,NC2(I))
              YD2(I)=XDP(2,NC2(I))
              ZD2(I)=XDP(3,NC2(I))
              XD3(I)=XDP(1,NC3(I))
              YD3(I)=XDP(2,NC3(I))
              ZD3(I)=XDP(3,NC3(I))
              XD4(I)=XDP(1,NC4(I))
              YD4(I)=XDP(2,NC4(I))
              ZD4(I)=XDP(3,NC4(I))
              OFF(I) = ABS(OFFG(I))
C             OFF_L  = MIN(OFF_L,OFFG(I))
            ENDIF
          ENDDO
          OFF_L = MIN(OFF_L,MINVAL(OFFG(1:NEL)))
        ELSE
#include "vectorize.inc"
          DO I=1,NEL
            IF(ABS(OFFG(I))>ONE)THEN
              XD1(I)=SAV(I,1)
              YD1(I)=SAV(I,2)
              ZD1(I)=SAV(I,3)
              XD2(I)=SAV(I,4)
              YD2(I)=SAV(I,5)
              ZD2(I)=SAV(I,6)
              XD3(I)=SAV(I,7)
              YD3(I)=SAV(I,8)
              ZD3(I)=SAV(I,9)
              XD4(I)=ZERO
              YD4(I)=ZERO
              ZD4(I)=ZERO
              OFF(I) = ABS(OFFG(I))-ONE
C             OFF_L  = MIN(OFF_L,OFFG(I))
            ELSE
              XD1(I)=X1(I)
              YD1(I)=Y1(I)
              ZD1(I)=Z1(I)
              XD2(I)=X2(I)
              YD2(I)=Y2(I)
              ZD2(I)=Z2(I)
              XD3(I)=X3(I)
              YD3(I)=Y3(I)
              ZD3(I)=Z3(I)
              XD4(I)=X4(I)
              YD4(I)=Y4(I)
              ZD4(I)=Z4(I)
              OFF(I) = ABS(OFFG(I))
C             OFF_L  = MIN(OFF_L,OFFG(I))
            ENDIF
          ENDDO
          OFF_L = MIN(OFF_L,MINVAL(OFFG(1:NEL)))
        ENDIF
C
      ELSE
C
       IF(IRESP==1)THEN 
#include "vectorize.inc"
         DO I=1,NEL
           XD1(I)=XDP(1,NC1(I))  
           YD1(I)=XDP(2,NC1(I))  
           ZD1(I)=XDP(3,NC1(I))  
           XD2(I)=XDP(1,NC2(I))  
           YD2(I)=XDP(2,NC2(I))  
           ZD2(I)=XDP(3,NC2(I))  
           XD3(I)=XDP(1,NC3(I))  
           YD3(I)=XDP(2,NC3(I))  
           ZD3(I)=XDP(3,NC3(I))  
           XD4(I)=XDP(1,NC4(I))  
           YD4(I)=XDP(2,NC4(I))  
           ZD4(I)=XDP(3,NC4(I))  
           OFF(I) = ABS(OFFG(I))
C          OFF_L  = MIN(OFF_L,OFFG(I))                 
         ENDDO
         OFF_L = MIN(OFF_L,MINVAL(OFFG(1:NEL)))
       ELSE  
#include "vectorize.inc"
         DO I=1,NEL
           XD1(I)=X1(I) 
           YD1(I)=Y1(I) 
           ZD1(I)=Z1(I) 
           XD2(I)=X2(I) 
           YD2(I)=Y2(I) 
           ZD2(I)=Z2(I) 
           XD3(I)=X3(I) 
           YD3(I)=Y3(I) 
           ZD3(I)=Z3(I) 
           XD4(I)=X4(I) 
           YD4(I)=Y4(I) 
           ZD4(I)=Z4(I)   
           OFF(I) = ABS(OFFG(I))
C          OFF_L  = MIN(OFF_L,OFFG(I))     
         ENDDO      
         OFF_L = MIN(OFF_L,MINVAL(OFFG(1:NEL)))
       ENDIF     
C
      ENDIF
C
      IF(OFF_L<ZERO)THEN
#include "vectorize.inc"
        DO I=1,NEL
          IF(OFFG(I)<ZERO)THEN
            VX1(I)=ZERO
            VY1(I)=ZERO
            VZ1(I)=ZERO
            VX2(I)=ZERO
            VY2(I)=ZERO
            VZ2(I)=ZERO
            VX3(I)=ZERO
            VY3(I)=ZERO
            VZ3(I)=ZERO
            VX4(I)=ZERO
            VY4(I)=ZERO
            VZ4(I)=ZERO
          ENDIF
        ENDDO
      ENDIF

      F11(1:NEL)=ZERO
      F21(1:NEL)=ZERO
      F31(1:NEL)=ZERO
      F12(1:NEL)=ZERO
      F22(1:NEL)=ZERO
      F32(1:NEL)=ZERO
      F13(1:NEL)=ZERO
      F23(1:NEL)=ZERO
      F33(1:NEL)=ZERO
      F14(1:NEL)=ZERO
      F24(1:NEL)=ZERO
      F34(1:NEL)=ZERO
C
      IF (JLAG/=0)THEN
       VD2(1:NEL)=ZERO
       RETURN
C
      ELSEIF(JALE/=0)THEN
#include "vectorize.inc"
        DO I=1,NEL
          VDX1(I)=VX1(I)-W(1,NC1(I))
          VDY1(I)=VY1(I)-W(2,NC1(I))
          VDZ1(I)=VZ1(I)-W(3,NC1(I))
          VDX2(I)=VX2(I)-W(1,NC2(I))
          VDY2(I)=VY2(I)-W(2,NC2(I))
          VDZ2(I)=VZ2(I)-W(3,NC2(I))
          VDX3(I)=VX3(I)-W(1,NC3(I))
          VDY3(I)=VY3(I)-W(2,NC3(I))
          VDZ3(I)=VZ3(I)-W(3,NC3(I))
          VDX4(I)=VX4(I)-W(1,NC4(I))
          VDY4(I)=VY4(I)-W(2,NC4(I))
          VDZ4(I)=VZ4(I)-W(3,NC4(I))
        ENDDO
      ELSEIF(JEUL/=0)THEN
#include "vectorize.inc"
        DO I=1,NEL
          VDX1(I)=VX1(I)
          VDY1(I)=VY1(I)
          VDZ1(I)=VZ1(I)
          VDX2(I)=VX2(I)
          VDY2(I)=VY2(I)
          VDZ2(I)=VZ2(I)
          VDX3(I)=VX3(I)
          VDY3(I)=VY3(I)
          VDZ3(I)=VZ3(I)
          VDX4(I)=VX4(I)
          VDY4(I)=VY4(I)
          VDZ4(I)=VZ4(I)
        ENDDO
      ENDIF
#include "vectorize.inc"
      DO I=1,NEL
        VDX(I)=FOURTH*(VDX1(I)+VDX2(I)+VDX3(I)+VDX4(I))
        VDY(I)=FOURTH*(VDY1(I)+VDY2(I)+VDY3(I)+VDY4(I))
        VDZ(I)=FOURTH*(VDZ1(I)+VDZ2(I)+VDZ3(I)+VDZ4(I))
        VD2(I)=NINE*(VDX(I)**2+VDY(I)**2+VDZ(I)**2)
      ENDDO
C
      RETURN
      END
