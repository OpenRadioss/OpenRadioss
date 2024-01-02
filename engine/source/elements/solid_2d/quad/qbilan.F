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
Chd|  QBILAN                        source/elements/solid_2d/quad/qbilan.F
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|        GRELEM_SAV                    source/output/th/grelem_sav.F 
Chd|        SENSOR_ENERGY_BILAN           source/tools/sensor/sensor_energy_bilan.F
Chd|====================================================================
      SUBROUTINE QBILAN(
     1   PARTSAV, OFF,     EINT,    RHO,
     2   RK,      VOL,     VY1,     VY2,
     3   VY3,     VY4,     VZ1,     VZ2,
     4   VZ3,     VZ4,     VNEW,    IPARTQ,
     5   EHOU,    R22,     R23,     R32,
     6   R33,     GRESAV,  GRTH,    IGRTH,
     7   IEXPAN,  EINTTH,  ITASK,   NEL,
     8   JTUR,    JCVT,    IGRE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C----6------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: JTUR
      INTEGER, INTENT(IN) :: JCVT
      INTEGER, INTENT(IN) :: IGRE
      my_real
     .   PARTSAV(NPSAV,*), OFF(*), EINT(*), RHO(*), RK(*), VOL(*),
     .   VY1(*),VY2(*),VY3(*),VY4(*),VZ1(*),VZ2(*),VZ3(*),VZ4(*),
     .   VNEW(*), EHOU(*), R22(*), R23(*), R32(*), R33(*),GRESAV(*),
     .   EINTTH(*)
      INTEGER IEXPAN,ITASK,
     .   IPARTQ(*),GRTH(*),IGRTH(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .  VYA(MVSIZ), VZA(MVSIZ), VA2(MVSIZ), XMAS(MVSIZ), VY, VZ,
     .  OFF_SAV(MVSIZ),RBIDON(1),EI_SAV(MVSIZ),EK_SAV(MVSIZ),
     .  XM_SAV(MVSIZ),YM_SAV(MVSIZ),ZM_SAV(MVSIZ),EI(MVSIZ),EK(MVSIZ)
      INTEGER I,M, FLAG
C-----------------------------------------------
      RBIDON = ZERO
      OFF_SAV= ZERO
      EI_SAV = ZERO
      EK_SAV = ZERO
      XM_SAV = ZERO
      YM_SAV = ZERO
      ZM_SAV = ZERO

      DO I=1,NEL
        VYA(I)=VY1(I)+VY2(I)+VY3(I)+VY4(I)
        VZA(I)=VZ1(I)+VZ2(I)+VZ3(I)+VZ4(I)
      ENDDO

      IF(JCVT/=0)THEN
        DO I=1,NEL
          VY=R22(I)*VYA(I)+R23(I)*VZA(I)
          VZ=R32(I)*VYA(I)+R33(I)*VZA(I)
          VYA(I)=VY
          VZA(I)=VZ
        END DO
      END IF

      DO I=1,NEL
        VA2(I)=
     1         VY1(I)*VY1(I)+VY2(I)*VY2(I)+VY3(I)*VY3(I)+VY4(I)*VY4(I)
     2        +VZ1(I)*VZ1(I)+VZ2(I)*VZ2(I)+VZ3(I)*VZ3(I)+VZ4(I)*VZ4(I)
      ENDDO

      DO I=1,NEL
        XMAS(I)=RHO(I)*VNEW(I)*OFF(I)  
      ENDDO

      FLAG = 0
      IF (IGRE /= 0) THEN
          DO I=1,NEL
            OFF_SAV(I) = ONE
            EI_SAV(I)  = EINT(I)*VOL(I)
            EK_SAV(I)  = XMAS(I)*VA2(I)*ONE_OVER_8
            XM_SAV(I)  = ZERO
            YM_SAV(I)  = XMAS(I)*VYA(I)*FOURTH
            ZM_SAV(I)  = XMAS(I)*VZA(I)*FOURTH
          ENDDO
          CALL GRELEM_SAV(1   ,NEL   ,GRESAV,IGRTH ,GRTH  ,
     2                    OFF_SAV,EI_SAV,EK_SAV,XM_SAV,YM_SAV,
     3                    ZM_SAV,XMAS  ,RBIDON,RBIDON,RBIDON,
     4                    RBIDON,RBIDON,RBIDON,RBIDON,RBIDON,
     5                    RBIDON,RBIDON,RBIDON,RBIDON,RBIDON,
     6                    RBIDON,FLAG)
      ENDIF

      DO I=1,NEL
        M=IPARTQ(I)
        PARTSAV(1,M)=PARTSAV(1,M) + EINT(I)*VOL(I)*OFF(I)
        PARTSAV(2,M)=PARTSAV(2,M) + XMAS(I)*VA2(I)*ONE_OVER_8
        PARTSAV(3,M)=ZERO
        PARTSAV(4,M)=PARTSAV(4,M) + XMAS(I)*VYA(I)*FOURTH
        PARTSAV(5,M)=PARTSAV(5,M) + XMAS(I)*VZA(I)*FOURTH
        PARTSAV(6,M)=PARTSAV(6,M) + XMAS(I)
      ENDDO

      IF(JTUR/=0)THEN
       DO I=1,NEL
         M=IPARTQ(I)
         PARTSAV(7,M)=PARTSAV(7,M) + RK(I)*VOL(I)*OFF(I)
       ENDDO
      ENDIF

      IF(IEXPAN/=0)THEN
       DO I=1,NEL
        M=IPARTQ(I)
        PARTSAV(27,M)=PARTSAV(27,M) + EINTTH(I)*VOL(I)*OFF(I)
       ENDDO
      ENDIF

      DO I=1,NEL
        M=IPARTQ(I)
        EI(I) = EINT(I)*VOL(I)*OFF(I)
        EK(I) = XMAS(I)*VA2(I)*ONE_OVER_8
      ENDDO
      CALL SENSOR_ENERGY_BILAN(1,NEL,EI,EK,OFF,IPARTQ,ITASK)  

      RETURN
      END
