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
!||    s8bilan               ../engine/source/elements/solid/solide8/s8bilan.F
!||--- called by ------------------------------------------------------
!||    s8forc3               ../engine/source/elements/solid/solide8/s8forc3.F
!||--- calls      -----------------------------------------------------
!||    grelem_sav            ../engine/source/output/th/grelem_sav.F
!||    sensor_energy_bilan   ../engine/source/tools/sensor/sensor_energy_bilan.F
!||--- uses       -----------------------------------------------------
!||    sensor_mod            ../common_source/modules/sensor_mod.F90
!||====================================================================
      SUBROUTINE S8BILAN(PARTSAV,EINT,RHO,RK,VOL,VNEW,
     .                   VX,VY,VZ,IPARTS,
     .                   GRESAV,GRTH,IGRTH,X,Y,Z,ITASK,IPARG,SENSORS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SENSOR_MOD
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   PARTSAV(NPSAV,*), EINT(*), RHO(*), RK(*), VOL(*),
     .   VNEW(MVSIZ), VX(MVSIZ,8),VY(MVSIZ,8),VZ(MVSIZ,8),
     .   GRESAV(*), X(MVSIZ,8), Y(MVSIZ,8), Z(MVSIZ,8)
      INTEGER 
     .   IPARTS(*),GRTH(*),IGRTH(*),IPARG(*),ITASK
      type (sensors_),INTENT(INOUT) :: SENSORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, M, FLAG
      my_real
     .   VXA(MVSIZ), VYA(MVSIZ) , VZA(MVSIZ), 
     .   VA2(MVSIZ), XMAS(MVSIZ),
     .   OFF(MVSIZ), EI(MVSIZ) , EK(MVSIZ),
     .   XM(MVSIZ) , YM(MVSIZ) , ZM(MVSIZ),
     .   XX(MVSIZ) , YY(MVSIZ) , ZZ(MVSIZ),
     .   XY(MVSIZ) , YZ(MVSIZ) , ZX(MVSIZ),
     .   XX2(MVSIZ), YY2(MVSIZ), ZZ2(MVSIZ),
     .   XCG(MVSIZ), YCG(MVSIZ), ZCG(MVSIZ),
     .   XXM(MVSIZ), YYM(MVSIZ), ZZM(MVSIZ),
     .   IXX(MVSIZ), IYY(MVSIZ), IZZ(MVSIZ),
     .   IXY(MVSIZ), IYZ(MVSIZ), IZX(MVSIZ),
     .   REI(MVSIZ), REK(MVSIZ), OFF_L(MVSIZ)
C-----------------------------------------------
      FLAG = IPARG(80) ! Flag pour sorties additionnelles
C-----------------------------------------------
C     CALCUL BILAN SORTIES DEF 
C-----------------------------------------------
      DO I=LFT,LLT
        VXA(I)=VX(I,1)+VX(I,2)+VX(I,3)+VX(I,4)+VX(I,5)+VX(I,6)+VX(I,7)+VX(I,8)
        VYA(I)=VY(I,1)+VY(I,2)+VY(I,3)+VY(I,4)+VY(I,5)+VY(I,6)+VY(I,7)+VY(I,8)
        VZA(I)=VZ(I,1)+VZ(I,2)+VZ(I,3)+VZ(I,4)+VZ(I,5)+VZ(I,6)+VZ(I,7)+VZ(I,8)
        VA2(I)=VX(I,1)**2+VX(I,2)**2+VX(I,3)**2+VX(I,4)**2
     1        +VX(I,5)**2+VX(I,6)**2+VX(I,7)**2+VX(I,8)**2
     2        +VY(I,1)**2+VY(I,2)**2+VY(I,3)**2+VY(I,4)**2
     3        +VY(I,5)**2+VY(I,6)**2+VY(I,7)**2+VY(I,8)**2
     4        +VZ(I,1)**2+VZ(I,2)**2+VZ(I,3)**2+VZ(I,4)**2
     5        +VZ(I,5)**2+VZ(I,6)**2+VZ(I,7)**2+VZ(I,8)**2
      ENDDO
C
      DO I=LFT,LLT
        VXA(I)=VXA(I)*ONE_OVER_8
        VYA(I)=VYA(I)*ONE_OVER_8
        VZA(I)=VZA(I)*ONE_OVER_8
        VA2(I)=VA2(I)*ONE_OVER_8
      ENDDO
C
      DO I=LFT,LLT
        XMAS(I)=RHO(I)*VNEW(I)
        EI(I)  = EINT(I)*VOL(I)
        EK(I)  = XMAS(I)*VA2(I)*HALF
        XM(I)  = XMAS(I)*VXA(I)
        YM(I)  = XMAS(I)*VYA(I)
        ZM(I)  = XMAS(I)*VZA(I)
      ENDDO
C
      DO I=LFT,LLT
        M=IPARTS(I)
        PARTSAV(1,M)=PARTSAV(1,M) + EI(I)
        PARTSAV(2,M)=PARTSAV(2,M) + EK(I)
        PARTSAV(3,M)=PARTSAV(3,M) + XM(I)
        PARTSAV(4,M)=PARTSAV(4,M) + YM(I)
        PARTSAV(5,M)=PARTSAV(5,M) + ZM(I)
        PARTSAV(6,M)=PARTSAV(6,M) + XMAS(I)
      ENDDO
C
C-----------------------------------------------
C     CALCUL BILAN SORTIES ADDITIONNELLES 
C-----------------------------------------------
      IF(FLAG==1) THEN
        DO I=LFT,LLT
          XX(I)=X(I,1)+X(I,2)+X(I,3)+X(I,4)+X(I,5)+X(I,6)+X(I,7)+X(I,8)
          YY(I)=Y(I,1)+Y(I,2)+Y(I,3)+Y(I,4)+Y(I,5)+Y(I,6)+Y(I,7)+Y(I,8)
          ZZ(I)=Z(I,1)+Z(I,2)+Z(I,3)+Z(I,4)+Z(I,5)+Z(I,6)+Z(I,7)+Z(I,8)
          XX2(I)=X(I,1)**2+X(I,2)**2+X(I,3)**2+X(I,4)**2
     +          +X(I,5)**2+X(I,6)**2+X(I,7)**2+X(I,8)**2
          YY2(I)=Y(I,1)**2+Y(I,2)**2+Y(I,3)**2+Y(I,4)**2
     +          +Y(I,5)**2+Y(I,6)**2+Y(I,7)**2+Y(I,8)**2
          ZZ2(I)=Z(I,1)**2+Z(I,2)**2+Z(I,3)**2+Z(I,4)**2
     +          +Z(I,5)**2+Z(I,6)**2+Z(I,7)**2+Z(I,8)**2
          XY(I)=X(I,1)*Y(I,1)+X(I,2)*Y(I,2)+X(I,3)*Y(I,3)+X(I,4)*Y(I,4)
     +         +X(I,5)*Y(I,5)+X(I,6)*Y(I,6)+X(I,7)*Y(I,7)+X(I,8)*Y(I,8)
          YZ(I)=Y(I,1)*Z(I,1)+Y(I,2)*Z(I,2)+Y(I,3)*Z(I,3)+Y(I,4)*Z(I,4)
     +         +Y(I,5)*Z(I,5)+Y(I,6)*Z(I,6)+Y(I,7)*Z(I,7)+Y(I,8)*Z(I,8)
          ZX(I)=Z(I,1)*X(I,1)+Z(I,2)*X(I,2)+Z(I,3)*X(I,3)+Z(I,4)*X(I,4)
     +         +Z(I,5)*X(I,5)+Z(I,6)*X(I,6)+Z(I,7)*X(I,7)+Z(I,8)*X(I,8)
        ENDDO
C
        DO I=LFT,LLT
          XX(I)=XX(I)*ONE_OVER_8
          YY(I)=YY(I)*ONE_OVER_8
          ZZ(I)=ZZ(I)*ONE_OVER_8
          XY(I)=XY(I)*ONE_OVER_8
          YZ(I)=YZ(I)*ONE_OVER_8
          ZX(I)=ZX(I)*ONE_OVER_8
          XX2(I)=XX2(I)*ONE_OVER_8
          YY2(I)=YY2(I)*ONE_OVER_8
          ZZ2(I)=ZZ2(I)*ONE_OVER_8
        ENDDO
C
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
C
        DO I=LFT,LLT
          M=IPARTS(I)
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
      ENDIF
C
C-----------------------------------------------
      IF (IGRE /= 0) THEN
          DO I=LFT,LLT
            OFF(I)     = ONE
          ENDDO
          CALL GRELEM_SAV(LFT   ,LLT   ,GRESAV,IGRTH ,GRTH  ,
     2                    OFF   ,EI    ,EK    ,XM    ,YM    ,
     3                    ZM    ,XMAS  ,XCG   ,YCG   ,ZCG   ,
     4                    XXM   ,YYM   ,ZZM   ,IXX   ,IYY   ,
     5                    IZZ   ,IXY   ,IYZ   ,IZX   ,REI   ,
     6                    REK   ,FLAG  )
      ENDIF
C-----------------------------------------------
C
      IF(JTUR/=0)THEN
       DO I=LFT,LLT
         M=IPARTS(I)
         PARTSAV(7,M)=PARTSAV(7,M) + RK(I)*VOL(I)
       ENDDO
      ENDIF

      DO I=LFT,LLT
            OFF_L(I) = ONE
      ENDDO
      CALL SENSOR_ENERGY_BILAN(LFT,LLT,EI,EK,OFF_L,IPARTS,ITASK,SENSORS) 
C
      RETURN
      END
