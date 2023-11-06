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
Chd|  TBILAN                        source/elements/truss/tbilan.F
Chd|-- called by -----------
Chd|        TFORC3                        source/elements/truss/tforc3.F
Chd|-- calls ---------------
Chd|        GRELEM_SAV                    source/output/th/grelem_sav.F
Chd|        SENSOR_ENERGY_BILAN           source/tools/sensor/sensor_energy_bilan.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F
Chd|====================================================================
      SUBROUTINE TBILAN(
     1   PM,      V,       EINT,    AREA,
     2   PARTSAV, IPARTT,  TANI,    FOR,
     3   GRESAV,  GRTH,    IGRTH,   MAT,
     4   AL,      VX1,     VX2,     VY1,
     5   VY2,     VZ1,     VZ2,     X1,
     6   X2,      Y1,      Y2,      Z1,
     7   Z2,      ITASK,   H3D_DATA,NEL,
     8   IGRE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE H3D_MOD
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
#include      "param_c.inc"
#include      "scr14_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: NEL
        INTEGER, INTENT(IN) :: IGRE
        INTEGER IPARTT(*),GRTH(*),IGRTH(*),MAT(MVSIZ),ITASK
        my_real
     .     PM(NPROPM,*),V(3,*),EINT(*),AREA(*),PARTSAV(NPSAV,*),
     .     TANI(15,*),FOR(*),GRESAV(*),AL(MVSIZ),VX1(MVSIZ),VX2(MVSIZ),
     .     VY1(MVSIZ),VY2(MVSIZ),VZ1(MVSIZ),VZ2(MVSIZ),
     .     X1(MVSIZ),X2(MVSIZ),Y1(MVSIZ),Y2(MVSIZ),
     .     Z1(MVSIZ),Z2(MVSIZ)
        TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, M, J, FLAG
        my_real
     .     VXA(MVSIZ), VYA(MVSIZ),VZA(MVSIZ), VA2(MVSIZ), XMAS(MVSIZ),
     .     EI(MVSIZ), EK(MVSIZ), XM(MVSIZ),
     .     YM(MVSIZ), ZM(MVSIZ),
     .     RBIDON(1),OFF(MVSIZ),RHO(MVSIZ),
     .     XCG(MVSIZ), YCG(MVSIZ), ZCG(MVSIZ),
     .     XX(MVSIZ) , YY(MVSIZ) , ZZ(MVSIZ) ,
     .     REI(MVSIZ), REK(MVSIZ), INEL,
     .     XXM(MVSIZ), YYM(MVSIZ), ZZM(MVSIZ),
     .     IXX(MVSIZ), IYY(MVSIZ), IZZ(MVSIZ),
     .     IXY(MVSIZ), IYZ(MVSIZ), IZX(MVSIZ)
C-----------------------------------------------
        OFF(1:MVSIZ)    = ZERO
c
        DO I=1,NEL
          RHO(I)=PM(1,MAT(I))
        ENDDO
c
        DO I=1,NEL
          XX(I)=X1(I)+X2(I)
          YY(I)=Y1(I)+Y2(I)
          ZZ(I)=Z1(I)+Z2(I)
          VXA(I)=VX1(I)+VX2(I)
          VYA(I)=VY1(I)+VY2(I)
          VZA(I)=VZ1(I)+VZ2(I)
          VA2(I)=VX1(I)*VX1(I)+VX2(I)*VX2(I)
     .          +VY1(I)*VY1(I)+VY2(I)*VY2(I)
     .          +VZ1(I)*VZ1(I)+VZ2(I)*VZ2(I)
        ENDDO
C
        DO I=1,NEL
          XX(I)=XX(I)*HALF
          YY(I)=YY(I)*HALF
          ZZ(I)=ZZ(I)*HALF
          VXA(I)=VXA(I)*HALF
          VYA(I)=VYA(I)*HALF
          VZA(I)=VZA(I)*HALF
          VA2(I)=VA2(I)*HALF
        ENDDO
C
        DO I=1,NEL
          XMAS(I)=RHO(I)*AREA(I)*AL(I)
        ENDDO
c
        DO I=1,NEL
          EI(I)= EINT(I)
          EK(I)= XMAS(I)*VA2(I)*HALF
          XM(I)= XMAS(I)*VXA(I)
          YM(I)= XMAS(I)*VYA(I)
          ZM(I)= XMAS(I)*VZA(I)
        ENDDO

        DO I=1,NEL
          XCG(I)= XMAS(I)*XX(I)
          YCG(I)= XMAS(I)*YY(I)
          ZCG(I)= XMAS(I)*ZZ(I)
          INEL = XMAS(I)*AL(I)*AL(I)*ONE_OVER_12
          IXY(I) = -XCG(I)*YY(I)
          IYZ(I) = -YCG(I)*ZZ(I)
          IZX(I) = -ZCG(I)*XX(I)
          XX(I) = XCG(I)*XX(I)
          YY(I) = YCG(I)*YY(I)
          ZZ(I) = ZCG(I)*ZZ(I)
          IXX(I)= INEL + YY(I) + ZZ(I)
          IYY(I)= INEL + ZZ(I) + XX(I)
          IZZ(I)= INEL + XX(I) + YY(I)
          XXM(I)= VZA(I)*YCG(I)-VYA(I)*ZCG(I)
          YYM(I)= VXA(I)*ZCG(I)-VZA(I)*XCG(I)
          ZZM(I)= VYA(I)*XCG(I)-VXA(I)*YCG(I)
          REI(I)= EI(I)
          REK(I)= INEL*VA2(I)*HALF
        ENDDO
c
        FLAG = 0
        IF (IGRE /= 0) THEN
          DO I=1,NEL
            OFF(I) = ONE
          ENDDO
          CALL GRELEM_SAV(1   ,NEL   ,GRESAV,IGRTH ,GRTH  ,
     2                    OFF   ,EI    ,EK    ,XM    ,YM    ,
     3                    ZM    ,XMAS  ,XCG   ,YCG   ,ZCG   ,
     4                    XXM   ,YYM   ,ZZM   ,IXX   ,IYY   ,
     5                    IZZ   ,IXY   ,IYZ   ,IZX   ,REI   ,
     6                    REK   ,FLAG  )

        ENDIF
c
        DO I=1,NEL
          M = IPARTT(I)
          PARTSAV(1,M)=PARTSAV(1,M) + EINT(I)
          PARTSAV(2,M)=PARTSAV(2,M) + EK(I)
          PARTSAV(3,M)=PARTSAV(3,M) + XM(I)
          PARTSAV(4,M)=PARTSAV(4,M) + YM(I)
          PARTSAV(5,M)=PARTSAV(5,M) + ZM(I)
          PARTSAV(6,M)=PARTSAV(6,M) + XMAS(I)
          PARTSAV(9,M) =PARTSAV(9,M)  + XCG(I)
          PARTSAV(10,M)=PARTSAV(10,M) + YCG(I)
          PARTSAV(11,M)=PARTSAV(11,M) + ZCG(I)
          PARTSAV(12,M)=PARTSAV(12,M) + XXM(I)
          PARTSAV(13,M)=PARTSAV(13,M) + YYM(I)
          PARTSAV(14,M)=PARTSAV(14,M) + ZZM(I)
          PARTSAV(15,M)=PARTSAV(15,M) + IXX(I)
          PARTSAV(16,M)=PARTSAV(16,M) + IYY(I)
          PARTSAV(17,M)=PARTSAV(17,M) + IZZ(I)
          PARTSAV(18,M)=PARTSAV(18,M) + IXY(I)
          PARTSAV(19,M)=PARTSAV(19,M) + IYZ(I)
          PARTSAV(20,M)=PARTSAV(20,M) + IZX(I)
          PARTSAV(21,M)=PARTSAV(21,M) + REI(I)
          PARTSAV(22,M)=PARTSAV(22,M) + REK(I)
        ENDDO
c
        IF (ANIM_FT(1) /= 0 .OR. H3D_DATA%UND_FORC /= 0) THEN
          DO I=1,NEL
            TANI(1,I)= FOR(I)
            TANI(2,I)= ZERO
            TANI(3,I)= ZERO
            TANI(4,I)= ZERO
            TANI(5,I)= ZERO
            TANI(6,I)= ZERO
            TANI(7,I)= ZERO
            TANI(8,I)= ZERO
            TANI(9,I)= ZERO
          ENDDO
        ENDIF
        OFF(1:NEL) = ONE
        CALL SENSOR_ENERGY_BILAN(1,NEL,EINT,EK,OFF,IPARTT,ITASK)
C-----------------------------------------------
        RETURN
      END
