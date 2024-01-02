Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  PBILAN                        source/elements/beam/pbilan.F 
Chd|-- called by -----------
Chd|        PFORC3                        source/elements/beam/pforc3.F 
Chd|-- calls ---------------
Chd|        GRELEM_SAV                    source/output/th/grelem_sav.F 
Chd|        SENSOR_ENERGY_BILAN           source/tools/sensor/sensor_energy_bilan.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE PBILAN(
     1   PM,      V,       EINT,    GEO,
     2   PARTSAV, IPARTP,  TANI,    FOR,
     3   MOM,     GRESAV,  GRTH,    IGRTH,
     4   OFF_DUM, NEL,     AL,      NC1,
     5   NC2,     NC3,     E1X,     E1Y,
     6   E1Z,     E2X,     E2Y,     E2Z,
     7   MAT,     PID,     VX1G,    VX2G,
     8   VY1G,    VY2G,    VZ1G,    VZ2G,
     9   X1G,     X2G,     Y1G,     Y2G,
     A   Z1G,     Z2G,     ITASK,   H3D_DATA,
     B   IGRE)
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
      INTEGER, INTENT(IN) :: IGRE
      INTEGER IPARTP(*),GRTH(*),IGRTH(*),NEL,NC1(MVSIZ),NC2(MVSIZ),
     .   NC3(MVSIZ),MAT(MVSIZ),PID(MVSIZ),ITASK
C     REAL
      my_real
     .   PM(NPROPM,*), V(3,*), EINT(NEL,2), GEO(NPROPG,*),
     .   PARTSAV(NPSAV,*),TANI(15,*),FOR(NEL,3),MOM(NEL,3),GRESAV(*),
     .   OFF_DUM(*),AL(MVSIZ),E1X(MVSIZ),E1Y(MVSIZ),E1Z(MVSIZ),
     .   E2X(MVSIZ),E2Y(MVSIZ),E2Z(MVSIZ),VX1G(MVSIZ),VX2G(MVSIZ),
     .   VY1G(MVSIZ),VY2G(MVSIZ),VZ1G(MVSIZ),VZ2G(MVSIZ),
     .   X1G(MVSIZ),X2G(MVSIZ),Y1G(MVSIZ),Y2G(MVSIZ),
     .   Z1G(MVSIZ),Z2G(MVSIZ)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, M, FLAG
C     REAL
      my_real
     .   VXA(MVSIZ), VYA(MVSIZ),VZA(MVSIZ), VA2(MVSIZ), XMAS(MVSIZ), 
     .   EI(MVSIZ), EK(MVSIZ), XM(MVSIZ),
     .   YM(MVSIZ), ZM(MVSIZ), SECT(MVSIZ),
     .   RBIDON(1),OFF(MVSIZ),RHO(MVSIZ),
     .   XCG(MVSIZ), YCG(MVSIZ), ZCG(MVSIZ),
     .   XX(MVSIZ) , YY(MVSIZ) , ZZ(MVSIZ) ,
     .   REI(MVSIZ), REK(MVSIZ), INEL,
     .   XXM(MVSIZ), YYM(MVSIZ), ZZM(MVSIZ),
     .   IXX(MVSIZ), IYY(MVSIZ), IZZ(MVSIZ),
     .   IXY(MVSIZ), IYZ(MVSIZ), IZX(MVSIZ)
C-----------------------------------------------
      RBIDON = ZERO
      OFF(1:MVSIZ) = ZERO
      FLAG = 0
      DO I=1,NEL
        SECT(I)=GEO(1,PID(I))
        RHO(I) =PM(1,MAT(I))
      ENDDO
C
      DO I=1,NEL
        XX(I)=X1G(I)+X2G(I)
        YY(I)=Y1G(I)+Y2G(I)
        ZZ(I)=Z1G(I)+Z2G(I)
        VXA(I)=VX1G(I)+VX2G(I)
        VYA(I)=VY1G(I)+VY2G(I)
        VZA(I)=VZ1G(I)+VZ2G(I)
        VA2(I)=VX1G(I)*VX1G(I)+VX2G(I)*VX2G(I)
     .        +VY1G(I)*VY1G(I)+VY2G(I)*VY2G(I)
     .        +VZ1G(I)*VZ1G(I)+VZ2G(I)*VZ2G(I)
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
        XMAS(I)=RHO(I)*AL(I)*SECT(I)
      ENDDO
C
      DO I=1,NEL
        EI(I)= EINT(I,1) + EINT(I,2)
        EK(I)= XMAS(I)*VA2(I)*HALF
        XM(I)= XMAS(I)*VXA(I)
        YM(I)= XMAS(I)*VYA(I)
        ZM(I)= XMAS(I)*VZA(I) 
      ENDDO
C
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
C
      IF (IGRE /= 0) THEN
        DO I=1,NEL
          OFF(I) = ONE
        ENDDO
        CALL GRELEM_SAV(1   ,NEL   ,GRESAV,IGRTH ,GRTH  ,
     2                  OFF   ,EI    ,EK    ,XM    ,YM    ,
     3                  ZM    ,XMAS  ,XCG   ,YCG   ,ZCG   ,
     4                  XXM   ,YYM   ,ZZM   ,IXX   ,IYY   ,
     5                  IZZ   ,IXY   ,IYZ   ,IZX   ,REI   ,
     6                  REK   ,FLAG  )
      ENDIF
C
      DO I=1,NEL
        M = IPARTP(I)
        PARTSAV(1,M)=PARTSAV(1,M) + EI(I)
        PARTSAV(2,M)=PARTSAV(2,M) + EK(I)
        PARTSAV(3,M)=PARTSAV(3,M) + XM(I)
        PARTSAV(4,M)=PARTSAV(4,M) + YM(I)
        PARTSAV(5,M)=PARTSAV(5,M) + ZM(I)
        IF (OFF_DUM(I) > ZERO) PARTSAV(6,M)=PARTSAV(6,M) + XMAS(I)
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
C
      IF (ANIM_FT(1) /= 0 .OR. H3D_DATA%UND_FORC /= 0) THEN
        DO I=1,NEL
          TANI(1,I)= FOR(I,1)
          TANI(2,I)= FOR(I,2)
          TANI(3,I)= FOR(I,3)
          TANI(4,I)= MOM(I,1)
          TANI(5,I)= MOM(I,2) + HALF*AL(I)*FOR(I,3)
          TANI(6,I)= MOM(I,3) - HALF*AL(I)*FOR(I,2)
          TANI(7,I)= -MOM(I,1)
          TANI(8,I)= -MOM(I,2) + HALF*AL(I)*FOR(I,3)
          TANI(9,I)= -MOM(I,3) - HALF*AL(I)*FOR(I,2)
C          TANI(10,I)= E1X(I)
C          TANI(11,I)= E1Y(I)
C          TANI(12,I)= E1Z(I)
C          TANI(13,I)= E2X(I)
C          TANI(14,I)= E2Y(I)
C          TANI(15,I)= E2Z(I)
        ENDDO
      ENDIF
      OFF(1:MVSIZ) = ONE
      CALL SENSOR_ENERGY_BILAN(1,NEL,EI,EK,OFF,IPARTP,ITASK) 
C-----------------------------------------------
      RETURN
      END
