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
      !||====================================================================
      !||    m2lawp       ../engine/source/materials/mat/mat002/m2lawp.F
      !||--- called by ------------------------------------------------------
      !||    main_beam3   ../engine/source/elements/beam/main_beam3.F
      !||====================================================================
      SUBROUTINE M2LAWP(MAT_PARAM,ASRATE  ,TIME    ,DTIME   ,NPROPG  ,
     .                  FOR      ,MOM     ,EINT    ,GEO     ,OFF     ,
     .                  PLA      ,EXX     ,EXY     ,EXZ     ,
     .                  KXX      ,KYY     ,KZZ     ,AL      ,FA1     ,
     .                  FA2      ,FA3     ,MA1     ,MA2     ,MA3     ,
     .                  NEL      ,IPID    ,NGL     ,NUVAR   ,UVAR    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NUVAR,NPROPG
      INTEGER ,INTENT(IN) :: IPID
      INTEGER ,INTENT(IN) :: NGL(NEL)
      my_real ,INTENT(IN) :: ASRATE,TIME,DTIME
      my_real :: FOR(NEL,3), MOM(NEL,3), EINT(NEL,2)
      my_real ,DIMENSION(NEL) :: OFF,PLA,AL,EXX,EXY,EXZ,KXX,KYY,
     .                           KZZ,FA1,FA2,FA3,MA1,MA2,MA3,A1
      my_real :: GEO(NPROPG,*)
      my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) :: UVAR
      TYPE (MATPARAM_STRUCT_) ,INTENT(IN) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: INDX(NEL)
      INTEGER :: I,J,NINDX,IFORM,ICC,VP
      my_real :: Z3,Z4,CC,YOUNG,SHEAR,PLAP1
      my_real, DIMENSION(NEL) :: YLDTMP,B1, B2, B3, DEGMB,
     .   DEGFX,ESP,DMPM, DMPF, SHF, F1, M1, M2, M3, DEGSH, YEQ, YLD, DWPLA,
     .   EPMX, DWELM, DWELF, CA, CB, CN, YMAX, RR, EPDR, EPSP,
     .   SH, YMA2, SH10, SH20, SH0, SH1, SH2,PLAP,DPLA, TMP1, TMP2, TMP3
C-----------------------------------------------
      Z3 = ZERO
      Z4 = ZERO
C
      IF (IMPL_S == 0 .OR. IDYNA > 0) THEN
        DO I=1,NEL                    
          DMPM(I)=GEO(16,IPID)*AL(I)  
          DMPF(I)=GEO(17,IPID)*AL(I)  
        ENDDO
      ELSE
        DO I=1,NEL                    
          DMPM(I)=ZERO  
          DMPF(I)=ZERO  
        ENDDO 
      ENDIF                                 
!
      SHEAR  = MAT_PARAM%SHEAR 
      YOUNG  = MAT_PARAM%YOUNG 
!
      IFORM  = MAT_PARAM%IPARAM(1)
      ICC    = MAT_PARAM%IPARAM(2)
      VP     = MAT_PARAM%IPARAM(3)
      CC     = MAT_PARAM%UPARAM(6)
      IF (IFORM == 1) THEN
        Z3 = MAT_PARAM%UPARAM(10)
        Z4 = MAT_PARAM%UPARAM(11)
      END IF
!
      DO I=1,NEL
        CA(I)  = MAT_PARAM%UPARAM(1)
        CB(I)  = MAT_PARAM%UPARAM(2)
        CN(I)  = MAT_PARAM%UPARAM(3)
        EPMX(I)= MAT_PARAM%UPARAM(4)
        YMAX(I)= MAT_PARAM%UPARAM(5)
        IF (VP == 1)THEN
          EPDR(I) = MAX(EM20,MAT_PARAM%UPARAM(7))
        ELSE               
          EPDR(I) = MAX(EM20,MAT_PARAM%UPARAM(7)*DTIME)
        ENDIF
      END DO
C-----------------------------
      DO I=1,NEL
        A1(I)  = GEO(1,IPID)
        B1(I)  = GEO(2,IPID)
        B2(I)  = GEO(18,IPID)
        B3(I)  = GEO(4,IPID)
        SHF(I) = GEO(37,IPID)
      ENDDO
C-----------------------------
C     DAMPING terms moved to pforce3
C-----------------------------
      DO I=1,NEL
        ESP(I) = (EINT(I,1)+EINT(I,2))/AL(I)/A1(I)
      ENDDO
C
      DO I=1,NEL
        DEGMB(I) = FOR(I,1)*EXX(I)
        DEGSH(I) = FOR(I,2)*EXY(I)+FOR(I,3)*EXZ(I)
        DEGFX(I) = MOM(I,1)*KXX(I)+MOM(I,2)*KYY(I)+MOM(I,3)*KZZ(I)
      ENDDO
C
C     CISAILLEMENT TRANSVERSAL CALCULE AVEC K1=12EI/L**2 K2=5/6GA
C
      DO I=1,NEL
        SH(I)=FIVE_OVER_6*SHEAR*A1(I)
        YMA2(I)=TWELVE*YOUNG/AL(I)**2
        SH10(I)=YMA2(I)*B1(I)
        SH20(I)=YMA2(I)*B2(I)
        SH0(I)=(ONE - SHF(I))*SH(I)
        SH1(I)=SH0(I)*SH10(I)/(SH(I)+SH10(I)) + SHF(I)*SH10(I)
        SH2(I)=SH0(I)*SH20(I)/(SH(I)+SH20(I)) + SHF(I)*SH20(I)
C
        F1(I)   =FOR(I,1)+ EXX(I)*A1(I)*YOUNG
        FOR(I,2)=FOR(I,2)+ EXY(I)*SH2(I)
        FOR(I,3)=FOR(I,3)+ EXZ(I)*SH1(I)
        M1(I)   =MOM(I,1)+ KXX(I)*SHEAR *B3(I)
        M2(I)   =MOM(I,2)+ KYY(I)*YOUNG*B1(I)
        M3(I)   =MOM(I,3)+ KZZ(I)*YOUNG*B2(I)
      ENDDO
C-------------
C     CRITERE
C-------------
      DO I=1,NEL
        YEQ(I)= F1(I)*F1(I) + THREE * A1(I) *
     +                    ( M1(I)*M1(I) / MAX(B3(I),EM20)
     +                    + M2(I)*M2(I) / MAX(B1(I),EM20)
     +                    + M3(I)*M3(I) / MAX(B2(I),EM20) )
        YEQ(I)= SQRT(YEQ(I))/A1(I)
      ENDDO
C-------------
C     STRAIN RATE (JOHNSON-COOK, ZERILLI-ARMSTRONG)
C-------------
      IF (CC > ZERO) THEN
        DO I=1,NEL
          IF(VP == 1)THEN
            PLAP(I) = UVAR(I,1)
            PLAP(I) = MAX(PLAP(I),EPDR(I))
            EPSP(I) = LOG(PLAP(I)/EPDR(I))
          ELSE
            EPSP(I)=ABS(DEGMB(I)+DEGFX(I))/(YEQ(I)+ EM20)/A1(I)
            TMP2(I)=ABS(DEGMB(I)+DEGFX(I))
            TMP3(I)=EPSP(I)
            EPSP(I)= MAX(EPSP(I),EPDR(I))
            EPSP(I)= LOG(EPSP(I)/EPDR(I))
          ENDIF
        ENDDO
        IF (IFORM == 0) THEN  ! J-C
          DO I=1,NEL
            EPSP(I)=(ONE + CC * EPSP(I))
            IF (ICC == 1) YMAX(I) = YMAX(I) * EPSP(I)
          ENDDO
        ELSE IF (IFORM  == 1) THEN  ! Zerilli
          DO I=1,NEL
            EPSP(I)=(ONE + CC * EPSP(I))
            TMP1(I)=EPSP(I)
            EPSP(I)= CC*EXP((-Z3+Z4 * EPSP(I))*ESP(I))
            IF (ICC==1) YMAX(I) = YMAX(I) + EPSP(I)
            CA(I) = CA(I) + EPSP(I)
            EPSP(I)=ONE
          ENDDO
        END IF
      ELSE
        DO I=1,NEL
          EPSP(I)=ONE
        ENDDO
      ENDIF
C-----------------------------------
C     YIELD
C-----------------------------------
      DO I=1,NEL
        YLD(I)= CA(I) + CB(I) * EXP(CN(I) * LOG(PLA(I)+EM30))
        YLDTMP(I)=YLD(I)
        YLD(I)= MIN(YLD(I)*EPSP(I),YMAX(I))
        RR(I) = MIN(ONE,YLD(I)/(YEQ(I)+ EM20))
      ENDDO
C
      DO I=1,NEL
        F1(I)    = F1(I)*RR(I)
        DWELM(I) =(F1(I)+FOR(I,1))*(F1(I)-FOR(I,1))/YOUNG/A1(I)
        DEGMB(I) = DEGMB(I) + F1(I)*EXX(I)
      ENDDO
C
      DO I=1,NEL
        M1(I) = M1(I)*RR(I)
        M2(I) = M2(I)*RR(I)
        M3(I) = M3(I)*RR(I)
        DWELF(I) =(M1(I)+MOM(I,1))*(M1(I)-MOM(I,1))/ SHEAR/B3(I)+
     .            (M2(I)+MOM(I,2))*(M2(I)-MOM(I,2))/YOUNG/B1(I)+
     .            (M3(I)+MOM(I,3))*(M3(I)-MOM(I,3))/YOUNG/B2(I)
        DEGFX(I) = DEGFX(I)+ M1(I)*KXX(I)+M2(I)*KYY(I)+M3(I)*KZZ(I)
      ENDDO
C
      DO I=1,NEL
        DWPLA(I)= DEGMB(I)+DEGFX(I)-DWELM(I)-DWELF(I)
      ENDDO
C-----------------------
C     EPS PLASTIQUE
C-----------------------
      DO I=1,NEL
        TMP1(I)=EPSP(I)*DWPLA(I)/YLD(I)
c       PLA(I)=PLA(I)+OFF(I)*MAX(ZERO,0.5*EPSP(I)*DWPLA(I)/YLD(I)/A1(I))
        DPLA(I) = MAX(ZERO,HALF*TMP1(I)/A1(I))
        PLA(I)  = PLA(I)+OFF(I) * DPLA(I)
      ENDDO
      DO I=1,NEL
        IF (VP == 1) THEN 
          PLAP1     = DPLA(I) / MAX(EM20,DTIME)
          UVAR(I,1) = ASRATE * PLAP1 + (ONE - ASRATE) * PLAP(I)
        ENDIF
      ENDDO

C--------------------------------
C     TEST DE RUPTURE DUCTILE
C--------------------------------
      DO I=1,NEL
        IF (OFF(I) < EM01) OFF(I)=ZERO
        IF (OFF(I) < ONE)  OFF(I)=OFF(I)*FOUR_OVER_5
      ENDDO
C
      NINDX  = 0
      DO I=1,NEL
        IF (OFF(I) < ONE)      CYCLE
        IF (PLA(I) < EPMX(I)) CYCLE
        OFF(I)=FOUR_OVER_5
        IDEL7NOK = 1
        NINDX=NINDX+1
        INDX(NINDX)=I
      ENDDO
C
      IF (NINDX > 0 .AND. IMCONV == 1) THEN
        DO J=1,NINDX
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(INDX(J))
          WRITE(ISTDO,1100) NGL(INDX(J)),TIME
#include "lockoff.inc"
        ENDDO
      ENDIF
C
      DO I=1,NEL
        FA1(I) = F1(I)
        FA2(I) = FOR(I,2) 
        FA3(I) = FOR(I,3)  
        MA1(I) = M1(I) 
        MA2(I) = M2(I) 
        MA3(I) = M3(I) 
      ENDDO               
C
      RETURN
C-----------------------------------------------
 1000 FORMAT(1X,'-- RUPTURE OF BEAM ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'-- RUPTURE OF BEAM ELEMENT :',I10,' AT TIME :',G11.4)
C-----------------------------------------------
      END
