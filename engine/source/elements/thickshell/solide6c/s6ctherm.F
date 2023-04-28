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
      SUBROUTINE S6CTHERM(
     1   PM       ,MAT      ,VOL      ,NC1      ,
     2   NC2      ,NC3      ,NC4      ,NC5      ,
     3   NC6      ,PX1      ,PX2      ,PX3      ,
     4   PX4      ,PY1      ,PY2      ,PY3      ,
     5   PY4      ,PZ1      ,PZ2      ,PZ3      ,
     6   PZ4      ,DT1      ,TEMPNC   ,TEL      ,
     7   DIE      ,FPHI     ,OFFG     ,OFF      ,
     8   NEL      )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "scr_thermal_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: MAT(MVSIZ),NC1(MVSIZ),NC2(MVSIZ),
     .   NC3(MVSIZ),NC4(MVSIZ),NC5(MVSIZ),NC6(MVSIZ)
C     REAL
      my_real, INTENT(IN) ::
     .   VOL(MVSIZ), PX1(MVSIZ), PY1(MVSIZ),PZ1(MVSIZ),
     .   PX2(MVSIZ), PY2(MVSIZ), PZ2(MVSIZ),
     .   PX3(MVSIZ), PY3(MVSIZ),PZ3(MVSIZ),
     .   PX4(MVSIZ), PY4(MVSIZ),PZ4(MVSIZ),
     .   TEMPNC(NUMNOD), PM(NPROPM,NUMMAT),
     .   DT1,DIE(MVSIZ), TEL(MVSIZ),OFF(MVSIZ),OFFG(MVSIZ)
      my_real, INTENT(INOUT) :: FPHI(MVSIZ,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX
C     REAL
      my_real
     .  CA, CB, KC, PHIX, PHIY, PHIZ, A
C----------------------------------------------- 

      MX = MAT(1)
      CA = PM(75,MX)
      CB = PM(76,MX)
      DO I=1,NEL
        IF (OFF(I)/=ZERO .AND. OFFG(I)>ZERO) THEN 
c          
          ! Flux initialization 
          PHIX = ZERO
          PHIY = ZERO
          PHIZ = ZERO
c
          ! Computing flux
          PHIX =  TEMPNC(NC1(I))*(PX1(I)-PX4(I)) + TEMPNC(NC2(I))*(PX2(I)-PX4(I)) +
     .            TEMPNC(NC3(I))*(PX3(I)-PX4(I)) + TEMPNC(NC4(I))*(PX1(I)+PX4(I)) +
     .            TEMPNC(NC5(I))*(PX2(I)+PX4(I)) + TEMPNC(NC6(I))*(PX3(I)+PX4(I))   
          
          PHIY =  TEMPNC(NC1(I))*(PY1(I)-PY4(I)) + TEMPNC(NC2(I))*(PY2(I)-PY4(I)) +
     .            TEMPNC(NC3(I))*(PY3(I)-PY4(I)) + TEMPNC(NC4(I))*(PY1(I)+PY4(I)) +
     .            TEMPNC(NC5(I))*(PY2(I)+PY4(I)) + TEMPNC(NC6(I))*(PY3(I)+PY4(I)) 
      
          PHIZ =  TEMPNC(NC1(I))*(PZ1(I)-PZ4(I)) + TEMPNC(NC2(I))*(PZ2(I)-PZ4(I)) +
     .            TEMPNC(NC3(I))*(PZ3(I)-PZ4(I)) + TEMPNC(NC4(I))*(PZ1(I)+PZ4(I)) +
     .            TEMPNC(NC5(I))*(PZ2(I)+PZ4(I)) + TEMPNC(NC6(I))*(PZ3(I)+PZ4(I)) 
c
          KC = (CA + CB*TEL(I))*DT1*VOL(I)*THEACCFACT           
          PHIX = KC*PHIX
          PHIY = KC*PHIY
          PHIZ = KC*PHIZ 
c
          ! Computing nodal thermic forces
          A = DIE(I)*ONE_OVER_6
          FPHI(I,1) = FPHI(I,1) +
     .        A - (PHIX*(PX1(I)-PX4(I)) + PHIY*(PY1(I)-PY4(I)) + PHIZ*(PZ1(I)-PZ4(I)))
          FPHI(I,2) = FPHI(I,2) +
     .        A - (PHIX*(PX2(I)-PX4(I)) + PHIY*(PY2(I)-PY4(I)) + PHIZ*(PZ2(I)-PZ4(I)))
          FPHI(I,3) = FPHI(I,3) +
     .        A - (PHIX*(PX3(I)-PX4(I)) + PHIY*(PY3(I)-PY4(I)) + PHIZ*(PZ3(I)-PZ4(I)))
          FPHI(I,4) = FPHI(I,4) +
     .        A - (PHIX*(PX1(I)+PX4(I)) + PHIY*(PY1(I)+PY4(I)) + PHIZ*(PZ1(I)+PZ4(I)))
          FPHI(I,5) = FPHI(I,5) +
     .        A - (PHIX*(PX2(I)+PX4(I)) + PHIY*(PY2(I)+PY4(I)) + PHIZ*(PZ2(I)+PZ4(I)))
          FPHI(I,6) = FPHI(I,6) +
     .        A - (PHIX*(PX3(I)+PX4(I)) + PHIY*(PY3(I)+PY4(I)) + PHIZ*(PZ3(I)+PZ4(I)))
        ENDIF
      ENDDO
C 
      END
