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
Chd|  M2LAWP                        source/materials/mat/mat002/m2lawp.F
Chd|-- called by -----------
Chd|        MAIN_BEAM3                    source/elements/beam/main_beam3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M2LAWP(
     .    PM      ,FOR     ,MOM     ,EINT    ,GEO     ,
     .    OFF     ,PLA     ,EXX     ,EXY     ,EXZ     ,
     .    KXX     ,KYY     ,KZZ     ,AL      ,FA1     ,
     .    FA2     ,FA3     ,MA1     ,MA2     ,MA3     ,
     .    NEL     ,MAT     ,PID     ,NGL     ,IPM     ,
     .    NUMMAT  ,NUVAR   ,UVAR    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NUMMAT,NUVAR
      INTEGER ,INTENT(IN) :: MAT(NEL),PID(NEL),NGL(NEL)
      INTEGER ,DIMENSION(NPROPMI,NUMMAT) ,INTENT(IN) :: IPM
      my_real
     .   PM(NPROPM,*), FOR(NEL,3), MOM(NEL,3), EINT(NEL,2), GEO(NPROPG,*),
     .   OFF(*), PLA(*),
     .   AL(NEL),
     .   EXX(NEL),EXY(NEL),EXZ(NEL),KXX(NEL),KYY(NEL),
     .   KZZ(NEL),FA1(NEL),FA2(NEL),FA3(NEL),
     .   MA1(NEL),MA2(NEL),MA3(NEL),A1(NEL)
      my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER INDX(NEL),ICC(NEL),IRTY(NEL)
      INTEGER  I,NPIF, J, NINDX
      my_real
     .   YLDTMP(NEL),YM(NEL), 
     .   B1(NEL), B2(NEL), B3(NEL), DEGMB(NEL),
     .   DEGFX(NEL),ESP(NEL),RHO(NEL),G(NEL),
     .   DMPM(NEL), DMPF(NEL), SHF(NEL),
     .   F1(NEL), M1(NEL), M2(NEL), M3(NEL),
     .   DEGSH(NEL), YEQ(NEL), YLD(NEL), DWPLA(NEL),
     .   EPMX(NEL), DWELM(NEL), DWELF(NEL),Z3(NEL),Z4(NEL),
     .   CA(NEL), CB(NEL), CN(NEL), YMAX(NEL),
     .   RR(NEL), CC(NEL), EPDR(NEL), EPSP(NEL),
     .   SH(NEL), YMA2(NEL), SH10(NEL), SH20(NEL), SH0(NEL),
     .   SH1(NEL), SH2(NEL), FACT, EPIF,PLAP(NEL),DPLA(NEL),
     .   TMP1(NEL), TMP2(NEL), TMP3(NEL), ETMP,PLAP1,VP,ASRATE,
     .   FACTPHI,YMA2PHI, SH10P, SH20P, SH1P, SH2P,SHP, SH0P,
     .   YS,YSP,GS,GSP,TRM1,TRM1P,TRM2,TRM2P,TRM3,TRM3P,YEQ0,YEQ1
C-----------------------------------------------
      EPIF = ZERO
      NPIF = 0
C
      IF (IMPL_S == 0 .OR. IDYNA > 0) THEN
        DO I=1,NEL                    
          DMPM(I)=GEO(16,PID(I))*AL(I)  
          DMPF(I)=GEO(17,PID(I))*AL(I)  
        ENDDO
      ELSE
        DO I=1,NEL                    
          DMPM(I)=ZERO  
          DMPF(I)=ZERO  
        ENDDO 
      ENDIF                                 
C

      DO I=1,NEL
        VP     = IPM(255,MAT(I)) 
        RHO(I) = PM( 1,MAT(I))
        ASRATE = MIN(ONE, PM(9,MAT(I))*DT1)
        G(I)   =PM(22,MAT(I))
        YM(I)  =PM(20,MAT(I))
C--------------------------
        CA(I)  =PM(38,MAT(I))
        CB(I)  =PM(39,MAT(I))
        CN(I)  =PM(40,MAT(I))
        EPMX(I)=PM(41,MAT(I))
        YMAX(I)=PM(42,MAT(I))
        CC(I)  =PM(43,MAT(I))
        IF(VP == 1)THEN
          EPDR(I) =   MAX(EM20,PM(44,MAT(I)))
        ELSE               
          EPDR(I) = MAX(EM20,PM(44,MAT(I))*DT1)  
        ENDIF
        ICC(I) =NINT(PM(49,MAT(I)))
C-----------------------------
        A1(I)  =GEO(1,PID(I))
        B1(I)  =GEO(2,PID(I))
        B2(I)  =GEO(18,PID(I))
        B3(I)  =GEO(4,PID(I))
C        SHF(I) =GEO(13,PID(I))
        SHF(I) =GEO(37,PID(I))
        EPIF    = MAX(EPIF,CC(I))
        IRTY(I) = NINT(PM(50,MAT(I)))
        NPIF    = NPIF+IRTY(I)
        Z3(I)   =PM(51,MAT(I))
        Z4(I)   =PM(52,MAT(I))
      ENDDO
C-----------------------------
C     DAMPING terms removed to pforce3
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
        SH(I)=FIVE_OVER_6*G(I)*A1(I)
        YMA2(I)=TWELVE*YM(I)/AL(I)**2
        SH10(I)=YMA2(I)*B1(I)
        SH20(I)=YMA2(I)*B2(I)
        SH0(I)=(ONE - SHF(I))*SH(I)
        SH1(I)=SH0(I)*SH10(I)/(SH(I)+SH10(I)) + SHF(I)*SH10(I)
        SH2(I)=SH0(I)*SH20(I)/(SH(I)+SH20(I)) + SHF(I)*SH20(I)
C
        F1(I)   =FOR(I,1)+ EXX(I)*A1(I)*YM(I)
        FOR(I,2)=FOR(I,2)+ EXY(I)*SH2(I)
        FOR(I,3)=FOR(I,3)+ EXZ(I)*SH1(I)
        M1(I)   =MOM(I,1)+ KXX(I)*G(I) *B3(I)
        M2(I)   =MOM(I,2)+ KYY(I)*YM(I)*B1(I)
        M3(I)   =MOM(I,3)+ KZZ(I)*YM(I)*B2(I)
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
      IF (EPIF /= ZERO) THEN
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
        IF (NPIF == ZERO) THEN
          DO I=1,NEL
            EPSP(I)=(ONE + CC(I) * EPSP(I))
            IF (ICC(I) == 1) YMAX(I) = YMAX(I) * EPSP(I)
          ENDDO
        ELSEIF (NPIF == NEL) THEN
          DO I=1,NEL
            EPSP(I)=(ONE + CC(I) * EPSP(I))
            TMP1(I)=EPSP(I)
            EPSP(I)= CC(I)*EXP((-Z3(I)+Z4(I) * EPSP(I))*ESP(I))
            IF(ICC(I)==1)YMAX(I)= YMAX(I) + EPSP(I)
            CA(I) = CA(I) + EPSP(I)
            EPSP(I)=ONE
          ENDDO
        ELSE
          DO I=1,NEL
            IF (IRTY(I) == 0)THEN
              EPSP(I)=(ONE + CC(I) * EPSP(I))
              TMP1(I)=YMAX(I)
              IF(ICC(I)==1) YMAX(I) = YMAX(I) * EPSP(I)
            ELSE
              EPSP(I)=(ONE  + CC(I) * EPSP(I))
              TMP1(I)=EPSP(I)
              EPSP(I)= CC(I)*EXP((-Z3(I)+Z4(I) * EPSP(I))*ESP(I))
              IF(ICC(I)==1)YMAX(I)= YMAX(I) + EPSP(I)
              CA(I) = CA(I) + EPSP(I)
              EPSP(I)=ONE
            ENDIF
          ENDDO
        ENDIF
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
        DWELM(I) =(F1(I)+FOR(I,1))*(F1(I)-FOR(I,1))/YM(I)/A1(I)
        DEGMB(I) = DEGMB(I) + F1(I)*EXX(I)
      ENDDO
C
      DO I=1,NEL
        M1(I) = M1(I)*RR(I)
        M2(I) = M2(I)*RR(I)
        M3(I) = M3(I)*RR(I)
        DWELF(I) =(M1(I)+MOM(I,1))*(M1(I)-MOM(I,1))/ G(I)/B3(I)+
     .            (M2(I)+MOM(I,2))*(M2(I)-MOM(I,2))/YM(I)/B1(I)+
     .            (M3(I)+MOM(I,3))*(M3(I)-MOM(I,3))/YM(I)/B2(I)
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
          PLAP1     = DPLA(I) / MAX(EM20,DT1)
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
          WRITE(ISTDO,1100) NGL(INDX(J)),TT
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
 1000 FORMAT(1X,'-- RUPTURE OF BEAM ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'-- RUPTURE OF BEAM ELEMENT :',I10,' AT TIME :',G11.4)
C-----------------------------------------------
      RETURN
      END
