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
Chd|  SIGEPS44P                     source/materials/mat/mat044/sigeps44p.F
Chd|-- called by -----------
Chd|        PFORC3                        source/elements/beam/pforc3.F 
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS44P(
     .    NEL     ,NGL     ,MAT     ,PID     ,NUPARAM ,UPARAM  ,
     .    IPM     ,GEO     ,OFF     ,PLA     ,EINT    ,AL      ,
     .    EXX     ,EXY     ,EXZ     ,KXX     ,KYY     ,KZZ     ,
     .    FA1     ,FA2     ,FA3     ,MA1     ,MA2     ,MA3     ,
     .    FOR     ,MOM     ,PM      ,NUVAR   ,UVAR    ,NFUNC   , 
     .    IFUNC   ,TF      ,NPF     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN)  :: NEL,NUPARAM,NUVAR,NFUNC,IFUNC(NFUNC),
     .                        NPF(*)
      INTEGER ,DIMENSION(NEL) ,INTENT(IN) :: MAT,PID,NGL
      INTEGER ,DIMENSION(NPROPMI,NUMMAT) ,INTENT(IN) :: IPM
      my_real ,DIMENSION(NPROPM ,NUMMAT) ,INTENT(IN) :: PM
      my_real ,DIMENSION(NPROPG ,NUMGEO) ,INTENT(IN) :: GEO
      my_real ,DIMENSION(NUPARAM)        ,INTENT(IN) :: UPARAM
      my_real ,DIMENSION(NEL,NUVAR) :: UVAR
      my_real ,DIMENSION(NEL,2) :: EINT
      my_real ,DIMENSION(NEL,3) :: FOR,MOM
      my_real ,DIMENSION(NEL)   :: OFF,PLA,AL,EXX,EXY,EXZ,KXX,KYY,KZZ,
     .   FA1,FA2,FA3,MA1,MA2,MA3
      my_real
     .   TF(*),FINTER
      EXTERNAL FINTER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ,DIMENSION(NEL) :: INDX,ICC,ISRATE,VFLAG,IPOS,IAD,ILEN
      INTEGER :: I,J,IPID,IADBUF,NINDX
      my_real :: DMG,FACT,EPIF,FRATE,ALPHA,EPSDOT
      my_real ,DIMENSION(NEL) :: E,G,G3,CA,CB,CN,CP,A1,B1,B2,B3,SHF,
     .   DEGMB,DEGFX,DMPM,DMPF,F1,M1,M2,M3,DEGSH,YEQ,YLD,YMA2,YLDMAX,
     .   DWPLA,EPMAX,EPST,EPSR1,EPSR2,EPDR,EPSP,RHO,EPMX,DWELM,DWELF,RR,
     .   SH,SH10,SH20,SH0,SH1,SH2,ASRATE,YSCALE,DPLA,DFDPLA
c=======================================================================
      EPIF = ZERO
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
        IADBUF   = IPM(7,MAT(I))-1
        IPID     = PID(I)
        E(I)     = UPARAM(IADBUF+1)
        CA(I)    = UPARAM(IADBUF+3)
        YLDMAX(I)= UPARAM(IADBUF+4)
        EPMAX(I) = UPARAM(IADBUF+5)
        EPSR1(I) = UPARAM(IADBUF+6)
        EPSR2(I) = UPARAM(IADBUF+7)
        CB(I)    = UPARAM(IADBUF+8)
        CN(I)    = UPARAM(IADBUF+9)
        ICC(I)   = NINT(UPARAM(IADBUF+10))
        EPDR(I)  = UPARAM(IADBUF+11)
        EPIF     = MAX(EPIF,EPDR(I))              
        CP(I)    = UPARAM(IADBUF+12)
        G(I)     = UPARAM(IADBUF+16)
        G3(I)    = UPARAM(IADBUF+18)
        ISRATE(I)= NINT(UPARAM(IADBUF+13))
        ASRATE(I)= UPARAM(IADBUF+14)
        VFLAG(I) = NINT(UPARAM(IADBUF+23))
        YSCALE(I)= UPARAM(IADBUF+24)
c
        RHO(I)   = PM(1,MAT(I))
c
        A1(I)    = GEO(1 ,IPID)
        B1(I)    = GEO(2 ,IPID)
        B2(I)    = GEO(18,IPID)
        B3(I)    = GEO(4 ,IPID)
        SHF(I)   = GEO(37,IPID)
        DPLA(I)  = ZERO
      ENDDO
C-----------------------------
C     AMORTISSEMENT
C-----------------------------
      DO I=1,NEL
        FA1(I) = EXX(I)*DMPM(I)*A1(I)*SQRT(DEUX*E(I)*RHO(I))
        FA2(I) = EXY(I)*DMPF(I)*12*B2(I)*SQRT(DEUX*E(I)*RHO(I))/AL(I)**2
        FA3(I) = EXZ(I)*DMPF(I)*12*B1(I)*SQRT(DEUX*E(I)*RHO(I))/AL(I)**2
        MA1(I) = KXX(I)*DMPF(I)*B3(I)*SQRT(DEUX*G(I)*RHO(I))
        MA2(I) = KYY(I)*DMPF(I)*B1(I)*SQRT(DEUX*E(I)*RHO(I))
        MA3(I) = KZZ(I)*DMPF(I)*B2(I)*SQRT(DEUX*E(I)*RHO(I))
        FA1(I) = FA1(I)*OFF(I)
        FA2(I) = FA2(I)*OFF(I)
        FA3(I) = FA3(I)*OFF(I)
        MA1(I) = MA1(I)*OFF(I)
        MA2(I) = MA2(I)*OFF(I)
        MA3(I) = MA3(I)*OFF(I)
      ENDDO
C
      DO I=1,NEL
        EXX(I)=EXX(I)*DT1
        EXY(I)=EXY(I)*DT1
        EXZ(I)=EXZ(I)*DT1
        KXX(I)=KXX(I)*DT1
        KYY(I)=KYY(I)*DT1
        KZZ(I)=KZZ(I)*DT1
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
        SH(I)  = CSIX*G(I)*A1(I)
        YMA2(I)= DOUZE*E(I)/AL(I)**2
        SH10(I)= YMA2(I)*B1(I)
        SH20(I)= YMA2(I)*B2(I)
        SH0(I) = (UN - SHF(I))*SH(I)
        SH1(I) = SH0(I)*SH10(I)/(SH(I)+SH10(I)) + SHF(I)*SH10(I)
        SH2(I) = SH0(I)*SH20(I)/(SH(I)+SH20(I)) + SHF(I)*SH20(I)
C
        F1(I)   = FOR(I,1) + EXX(I)*A1(I)*E(I)
        FOR(I,2)= FOR(I,2) + EXY(I)*SH2(I)
        FOR(I,3)= FOR(I,3) + EXZ(I)*SH1(I)
        M1(I)   = MOM(I,1) + KXX(I)*G(I) *B3(I)
        M2(I)   = MOM(I,2) + KYY(I)*E(I)*B1(I)
        M3(I)   = MOM(I,3) + KZZ(I)*E(I)*B2(I)
        EPST(I) = F1(I)/A1(I)/E(I)
      ENDDO
C-------------
C     CRITERE
C-------------
      DO I=1,NEL
        YEQ(I) = F1(I)*F1(I) + TROIS * A1(I) *
     .           (M1(I)*M1(I) / MAX(B3(I),EM20)
     .         +  M2(I)*M2(I) / MAX(B1(I),EM20)
     .         +  M3(I)*M3(I) / MAX(B2(I),EM20) )
        YEQ(I) = MAX(EM20, SQRT(YEQ(I)) / A1(I) )
      ENDDO
C-----------------------------------
C     YIELD
C-----------------------------------
      IF (NFUNC > 0) THEN
        !IPOS(1:NEL) = NINT(VARTMP(1:NEL,1))
        !IAD (1:NEL) = NPF(IFUNC(1)) / 2 + 1
        !ILEN(1:NEL) = NPF(IFUNC(1)+1) / 2 - IAD(1:NEL) - IPOS(1:NEL)
        !CALL VINTER(TF,IAD,IPOS,ILEN,NEL,PLA,DFDPLA,YLD) 
        DO I = 1, NEL
          YLD(I) = YSCALE(I)*FINTER(IFUNC(1),PLA(I),NPF,TF,DFDPLA(I))
        ENDDO
        !VARTMP(1:NEL,1) = IPOS(1:NEL)
      ENDIF
c
c
c
      DO I = 1,NEL
        IF (NFUNC > 0) THEN 
          YLD(I) = YSCALE(I)*YLD(I)
        ELSE
          YLD(I) = CA(I)
        ENDIF
      ENDDO
c
c
c
      DO I = 1,NEL
        IF (PLA(I) > ZERO) THEN
          IF (NFUNC > 0) THEN 
            YLD(I) = YSCALE(I)*YLD(I)
          ELSE
            YLD(I) = CA(I) + CB(I)*EXP(CN(I)*LOG(PLA(I)))
          ENDIF
        ENDIF                                          
      ENDDO  
C-------------
C     STRAIN RATE EFFECT
C-------------
      IF (EPIF > ZERO) THEN
        DO I = 1,NEL
          IF (EPDR(I) > ZERO) THEN
            EPSP(I) = ABS(DEGMB(I) + DEGFX(I))/YEQ(I)/A1(I)
            IF (VFLAG(I) /= 1) THEN
              IF (ISRATE(I) == 1) THEN
                ALPHA = MIN(UN, ASRATE(I)*DT1)
                EPSDOT = ALPHA*ABS(EPSP(I)/MAX(EM20,DT1)) + (UN-ALPHA)*UVAR(I,1)
                UVAR(I,1) = EPSDOT
              ELSE
                EPSDOT = ABS(EPSP(I)/MAX(EM20,DT1))
              ENDIF
            ELSE
             EPSDOT = UVAR(I,1)
            ENDIF
            FRATE   = UN + (EPSDOT*EPDR(I))**CP(I)
            IF (ICC(I)== 1) YLDMAX(I) = YLDMAX(I) * FRATE
            IF ((NFUNC > 0) .AND. (CA(I) /= ZERO)) THEN 
              IF (PLA(I)>ZERO) THEN 
                YLD(I) = YLD(I) + (CA(I) + CB(I)*EXP(CN(I)*LOG(PLA(I))))*(FRATE-UN)
              ELSE
                YLD(I) = YLD(I) + CA(I)*(FRATE-UN)
              ENDIF
            ELSE
              YLD(I) = YLD(I) * FRATE
            ENDIF  
          ENDIF
        ENDDO
      ELSE
        EPSP(1:NEL )= UN
      ENDIF
c-------------------
c     PROJECTION
c-------------------
      DO I=1,NEL
        YLD(I) = MIN(YLD(I),YLDMAX(I))    
        RR(I)  = MIN(UN, YLD(I) / YEQ(I))
        F1(I)  = F1(I)*RR(I)
        DWELM(I) =(F1(I) + FOR(I,1))*(F1(I)-FOR(I,1)) / E(I)/A1(I)
        DEGMB(I) = DEGMB(I)+ F1(I)*EXX(I)
      ENDDO
C
      DO I=1,NEL
        M1(I) = M1(I)*RR(I)
        M2(I) = M2(I)*RR(I)
        M3(I) = M3(I)*RR(I)
        DWELF(I) =(M1(I)+MOM(I,1))*(M1(I)-MOM(I,1))/ G(I)/B3(I)+
     .            (M2(I)+MOM(I,2))*(M2(I)-MOM(I,2))/E(I)/B1(I)+
     .            (M3(I)+MOM(I,3))*(M3(I)-MOM(I,3))/E(I)/B2(I)
        DEGFX(I) = DEGFX(I) + M1(I)*KXX(I) + M2(I)*KYY(I) + M3(I)*KZZ(I)
      ENDDO
C
      DO I=1,NEL
        DWPLA(I) = DEGMB(I) + DEGFX(I) - DWELM(I) - DWELF(I)
        DEGSH(I) = DEGSH(I) + FOR(I,2)*EXY(I) + FOR(I,3)*EXZ(I)
        FACT = UNDEMI*OFF(I)*AL(I)
        EINT(I,1) = EINT(I,1) + (DEGSH(I)+DEGMB(I))*FACT
        EINT(I,2) = EINT(I,2) +  FACT*DEGFX(I)
      ENDDO
C-----------------------
C     EPS PLASTIQUE
C-----------------------
      DO I=1,NEL
        FACT    = DWPLA(I)/YLD(I)
        DPLA(I) = OFF(I)*MAX(ZERO,UNDEMI*FACT/A1(I))
        PLA(I)  = PLA(I) + OFF(I)*MAX(ZERO,UNDEMI*FACT/A1(I))
      ENDDO
c--------------------------------
c     DUCTILE RUPTURE
c--------------------------------
      DO I=1,NEL
        IF (OFF(I) < EM01) OFF(I) = ZERO
        IF (OFF(I) < UN)   OFF(I) = OFF(I)*QCINQ
      ENDDO
c--------------------------------
c     AXIAL TENSION OR PLASTIC STRAIN FAILURE 
c--------------------------------
      NINDX = 0
      DO I = 1,NEL
        IF (OFF(I) == UN) THEN
          DMG = UN
          IF (EPST(I) > EPSR1(I)) THEN
            DMG = (EPSR2(I) - EPST(I)) / (EPSR2(I) - EPSR1(I))
            DMG = MAX(DMG, ZERO)
            FOR(I,1) = F1(I)*DMG
            FOR(I,2) = FOR(I,2)*DMG
            FOR(I,3) = FOR(I,3)*DMG
            MOM(I,1) = M1(I)*DMG
            MOM(I,2) = M2(I)*DMG
            MOM(I,3) = M3(I)*DMG
          ENDIF
c         test strain failure
          IF (DMG == ZERO .or. PLA(I) >= EPMAX(I)) THEN
            OFF(I)   = QCINQ
            IDEL7NOK = 1
            NINDX = NINDX+1
            INDX(NINDX) = I
          ENDIF   
c
          IF (VFLAG(I) == 1) THEN 
            ALPHA   = MIN(UN, ASRATE(I)*DT1)
            EPSDOT  = DPLA(I)/MAX(EM20,DT1)
            UVAR(I,1) = ALPHA*EPSDOT + (UN - ALPHA)*UVAR(I,1)
          ENDIF
c
        ENDIF
      ENDDO                                          
c--------------------------------
      IF (NINDX > 0 .AND. IMCONV == 1) THEN
        DO J=1,NINDX
          I = INDX(J)
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(I)
          WRITE(ISTDO,1100) NGL(I),TT
#include "lockoff.inc"
        ENDDO
      ENDIF
C
      DO I=1,NEL
        FOR(I,1)=F1(I)*OFF(I)
        FOR(I,2)=FOR(I,2)*OFF(I)
        FOR(I,3)=FOR(I,3)*OFF(I)
        MOM(I,1)=M1(I)*OFF(I)
        MOM(I,2)=M2(I)*OFF(I)
        MOM(I,3)=M3(I)*OFF(I)
      ENDDO
C
      DO I=1,NEL
        FA1(I) = FA1(I) + FOR(I,1)
        FA2(I) = FA2(I) + FOR(I,2) 
        FA3(I) = FA3(I) + FOR(I,3)  
        MA1(I) = MA1(I) + MOM(I,1) 
        MA2(I) = MA2(I) + MOM(I,2) 
        MA3(I) = MA3(I) + MOM(I,3) 
      ENDDO               
c-----------------------------------------------
 1000 FORMAT(1X,'-- RUPTURE OF BEAM ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'-- RUPTURE OF BEAM ELEMENT :',I10,' AT TIME :',G11.4)
c-----------------------------------------------
      RETURN
      END
