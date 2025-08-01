Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    spbilan      ../engine/source/elements/sph/spbilan.F
!||--- called by ------------------------------------------------------
!||    spstres      ../engine/source/elements/sph/spstres.F
!||--- calls      -----------------------------------------------------
!||    grelem_sav   ../engine/source/output/th/grelem_sav.F
!||====================================================================
      SUBROUTINE SPBILAN(
     1   V,       PARTSAV, NC1,     EINT,
     2   RHO,     RK,      VOL,     VNEW,
     3   IPARTSP, OFFG,    GRESAV,  GRTH,
     4   IGRTH,   EINTTH,  IEXPAN,  X,
     5   LFT,     LLT,     JTUR,    IGRE)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(INOUT) :: LFT
      INTEGER, INTENT(INOUT) :: LLT
      INTEGER, INTENT(INOUT) :: JTUR
      INTEGER, INTENT(INOUT) :: IGRE
C     REAL
      my_real
     .  V(3,*),X(3,*),
     .  PARTSAV(NPSAV,*), EINT(*), RHO(*), RK(*), VOL(*),
     .  VNEW(*),OFFG(*),GRESAV(*),EINTTH(*)
      INTEGER NC1(*),IPARTSP(*),GRTH(*),IGRTH(*),IEXPAN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, M, FLAG
C     REAL
      my_real
     .   VXA(MVSIZ),VYA(MVSIZ),VZA(MVSIZ),VA2(MVSIZ),XMAS(MVSIZ),
     .   RBIDON(1),OFF(MVSIZ),EI(MVSIZ),EK(MVSIZ),
     .   XM(MVSIZ),YM(MVSIZ),ZM(MVSIZ),
     .   XX(MVSIZ), YY(MVSIZ), ZZ(MVSIZ),
     .   XCG(MVSIZ), YCG(MVSIZ), ZCG(MVSIZ),
     .   XY(MVSIZ) , YZ(MVSIZ) , ZX(MVSIZ),
     .   XX2(MVSIZ), YY2(MVSIZ), ZZ2(MVSIZ),
     .   XXM(MVSIZ), YYM(MVSIZ), ZZM(MVSIZ),
     .   IXX(MVSIZ), IYY(MVSIZ), IZZ(MVSIZ),
     .   IXY(MVSIZ), IYZ(MVSIZ), IZX(MVSIZ),
     .   REI(MVSIZ), REK(MVSIZ)
C-----------------------------------------------
      RBIDON = ZERO
      OFF    = ZERO
      EI = ZERO
      EK = ZERO
      XM = ZERO
      YM = ZERO
      ZM = ZERO
C
C     NODAL VELOCITIES AND COORDINATES
      DO 20 I=LFT,LLT
       VXA(I)=V(1,NC1(I))
       VYA(I)=V(2,NC1(I))
       VZA(I)=V(3,NC1(I))
       VA2(I)=VXA(I)*VXA(I)+VYA(I)*VYA(I)+VZA(I)*VZA(I)
       XX(I) =X(1,NC1(I))
       YY(I) =X(2,NC1(I))
       ZZ(I) =X(3,NC1(I))
       XX2(I)=X(1,NC1(I))**2
       YY2(I)=X(2,NC1(I))**2
       ZZ2(I)=X(3,NC1(I))**2
       XY(I) =X(1,NC1(I))*X(2,NC1(I))
       YZ(I) =X(2,NC1(I))*X(3,NC1(I))
       ZX(I) =X(3,NC1(I))*X(1,NC1(I))
   20 CONTINUE
C
      DO I=LFT,LLT
        XMAS(I)=RHO(I)*VNEW(I)
        EI(I)  = EINT(I)*VOL(I)
        EK(I)  = XMAS(I)*VA2(I)*HALF
        OFF(I) = ONE
        XM(I)  = XMAS(I)*VXA(I)
        YM(I)  = XMAS(I)*VYA(I)
        ZM(I)  = XMAS(I)*VZA(I)
      ENDDO

      DO I=LFT,LLT
        XCG(I)= XMAS(I)*XX(I)
        YCG(I)= XMAS(I)*YY(I)
        ZCG(I)= XMAS(I)*ZZ(I)
        IXY(I)= -XMAS(I)*XY(I)
        IYZ(I)= -XMAS(I)*YZ(I)
        IZX(I)= -XMAS(I)*ZX(I)
        IXX(I)= XMAS(I)*(YY2(I) + ZZ2(I))
        IYY(I)= XMAS(I)*(ZZ2(I) + XX2(I))
        IZZ(I)= XMAS(I)*(XX2(I) + YY2(I))
        XXM(I)= VZA(I)*YCG(I)-VYA(I)*ZCG(I)
        YYM(I)= VXA(I)*ZCG(I)-VZA(I)*XCG(I)
        ZZM(I)= VYA(I)*XCG(I)-VXA(I)*YCG(I)
        REI(I)= ZERO
        REK(I)= ZERO
      ENDDO

      FLAG = 0
      IF (IGRE /= 0) THEN
          CALL GRELEM_SAV(LFT   ,LLT   ,GRESAV,IGRTH ,GRTH  ,
     2                    OFF   ,EI    ,EK    ,XM    ,YM    ,
     3                    ZM    ,XMAS  ,XCG   ,YCG   ,ZCG   ,
     4                    XXM   ,YYM   ,ZZM   ,IXX   ,IYY   ,
     5                    IZZ   ,IXY   ,IYZ   ,IZX   ,REI   ,
     6                    REK   ,FLAG  )
      ENDIF
C
      DO I=LFT,LLT
        M=IPARTSP(I)
        PARTSAV(1,M)=PARTSAV(1,M) + EINT(I)*VOL(I)
        PARTSAV(2,M)=PARTSAV(2,M) + XMAS(I)*VA2(I)*HALF
        PARTSAV(6,M)=PARTSAV(6,M) + XMAS(I)
        PARTSAV(3,M)=PARTSAV(3,M) + XMAS(I)*VXA(I)
        PARTSAV(4,M)=PARTSAV(4,M) + XMAS(I)*VYA(I)
        PARTSAV(5,M)=PARTSAV(5,M) + XMAS(I)*VZA(I)
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
      IF(JTUR/=0)THEN
       DO 50 I=LFT,LLT
       IF(OFFG(I)>ZERO)THEN
        M=IPARTSP(I)
        PARTSAV(7,M)=PARTSAV(7,M) + RK(I)*VOL(I)
       ENDIF
   50  CONTINUE
      ENDIF

Cbm   THERMAL EXPANSION
      IF(IEXPAN/=0)THEN
       DO I=LFT,LLT
         M=IPARTSP(I)
         PARTSAV(27,M)=PARTSAV(27,M) + EINTTH(I)*VOL(I)
       END DO
      ENDIF

C
      RETURN
      END
