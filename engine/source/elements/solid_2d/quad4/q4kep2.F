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
Chd|  Q4KEP2                        source/elements/solid_2d/quad4/q4kep2.F
Chd|-- called by -----------
Chd|        Q4KE2                         source/elements/solid_2d/quad4/q4ke2.F
Chd|-- calls ---------------
Chd|        Q4KEPIJ2                      source/elements/solid_2d/quad4/q4kep2.F
Chd|====================================================================
      SUBROUTINE Q4KEP2 (
     1               PY1,PY2,PZ1,PZ2,AY,R22,R23,
     2               K11,K12,K13,K14,K22,
     3               K23,K24,K33,K34,K44,
     4               HH,AIR,FAC,ICP,OFF)
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICP
      my_real
     .    PY1(*), PY2(*), PZ1(*), PZ2(*), AY(*), R22(*), R23(*),
     .    K11(2,2,*), K12(2,2,*), K13(2,2,*), K14(2,2,*), K22(2,2,*), 
     .    K23(2,2,*), K24(2,2,*), K33(2,2,*), K34(2,2,*), K44(2,2,*), 
     .    HH(2,*), AIR(*), FAC(*), OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IS
      my_real
     .    BH(MVSIZ), 
     .    PY3(MVSIZ), PY4(MVSIZ), PZ3(MVSIZ), PZ4(MVSIZ)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO I=LFT,LLT
        PY3(I) = -PY1(I)
        PY4(I) = -PY2(I)
        PZ3(I) = -PZ1(I)
        PZ4(I) = -PZ2(I)
      ENDDO
C
      IF (ICP==1) THEN
        DO I=LFT,LLT
          BH(I) = (HH(1,I)+TWO_THIRD*HH(2,I))*AIR(I)
        ENDDO
      ELSE
        DO I=LFT,LLT
          BH(I) = (HH(1,I)*FAC(I)+TWO_THIRD*HH(2,I)*(ONE-FAC(I)))*AIR(I)
        ENDDO
      ENDIF
C
      IS = 1
      CALL Q4KEPIJ2(PY1, PZ1, PY1, PZ1, AY, R22,R23, BH, K11)
      CALL Q4KEPIJ2(PY2, PZ2, PY2, PZ2, AY, R22,R23, BH, K22)
      CALL Q4KEPIJ2(PY3, PZ3, PY3, PZ3, AY, R22,R23, BH, K33)
      CALL Q4KEPIJ2(PY4, PZ4, PY4, PZ4, AY, R22,R23, BH, K44)
      IS = 0
      CALL Q4KEPIJ2(PY1, PZ1, PY2, PZ2, AY, R22,R23, BH, K12)
      CALL Q4KEPIJ2(PY1, PZ1, PY3, PZ3, AY, R22,R23, BH, K13)
      CALL Q4KEPIJ2(PY1, PZ1, PY4, PZ4, AY, R22,R23, BH, K14)
      CALL Q4KEPIJ2(PY2, PZ2, PY3, PZ3, AY, R22,R23, BH, K23)
      CALL Q4KEPIJ2(PY2, PZ2, PY4, PZ4, AY, R22,R23, BH, K24)
      CALL Q4KEPIJ2(PY3, PZ3, PY4, PZ4, AY, R22,R23, BH, K34)
C
      RETURN
      END
Chd|====================================================================
Chd|  Q4KEPIJ2                      source/elements/solid_2d/quad4/q4kep2.F
Chd|-- called by -----------
Chd|        Q4KEP2                        source/elements/solid_2d/quad4/q4kep2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE Q4KEPIJ2 (PYI, PZI, PYJ, PZJ, AY, R22, R23, BH, KIJ)
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
#include      "vect01_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    PYI(*), PZI(*), PYJ(*), PZJ(*), AY(*), R22(*), R23(*), 
     .    BH(*), KIJ(2,2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER EP,IASY
      my_real
     .    AYS(MVSIZ)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO EP=LFT,LLT
        KIJ(1,1,EP) = KIJ(1,1,EP) + BH(EP)*PYI(EP)*PYJ(EP)
        KIJ(1,2,EP) = KIJ(1,2,EP) + BH(EP)*PYI(EP)*PZJ(EP)
        KIJ(2,1,EP) = KIJ(2,1,EP) + BH(EP)*PZI(EP)*PYJ(EP)
        KIJ(2,2,EP) = KIJ(2,2,EP) + BH(EP)*PZI(EP)*PZJ(EP)
      ENDDO
C
      IF (N2D==1) THEN
        DO EP=LFT,LLT
          AYS(EP) = AY(EP)*AY(EP)
        ENDDO
        IF (JCVT==0) THEN
          DO EP=LFT,LLT
            KIJ(1,1,EP) = KIJ(1,1,EP) + BH(EP)*( 
     .                  AY(EP)*(PYI(EP)+PYJ(EP)) + AYS(EP) )
            KIJ(1,2,EP) = KIJ(1,2,EP) + BH(EP)*( AY(EP)*PZJ(EP) )
            KIJ(2,1,EP) = KIJ(2,1,EP) + BH(EP)*( PZI(EP)*AY(EP) )
          ENDDO
C         asymmetric part
C          IASY = 0
C          IF (IASY/=0) THEN
C          ENDIF
        ELSE
          DO EP=LFT,LLT
            KIJ(1,1,EP) = KIJ(1,1,EP) + BH(EP)*( 
     .                  AY(EP)*R22(EP)*(PYI(EP)+PYJ(EP)) +
     .                  R22(EP)*R22(EP)*AYS(EP) )
            KIJ(1,2,EP) = KIJ(1,2,EP) + BH(EP)*( 
     .                  AY(EP)*(R23(EP)*PYI(EP)+R22(EP)*PZJ(EP)) +
     .                  R22(EP)*R23(EP)*AYS(EP) )
            KIJ(2,1,EP) = KIJ(2,1,EP) + BH(EP)*( 
     .                  AY(EP)*(R23(EP)*PYJ(EP)+R22(EP)*PZI(EP)) +
     .                  R23(EP)*R22(EP)*AYS(EP) )
            KIJ(2,2,EP) = KIJ(2,2,EP) + BH(EP)*( 
     .                  AY(EP)*R23(EP)*(PZI(EP)+PZJ(EP)) +
     .                  R23(EP)*R23(EP)*AYS(EP) )
          ENDDO
C         asymmetric part
C          IASY = 0
C          IF (IASY/=0) THEN
C          ENDIF
        ENDIF
      ENDIF
C
      RETURN
      END
