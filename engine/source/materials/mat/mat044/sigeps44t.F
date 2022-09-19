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
Chd|  SIGEPS44T                     source/materials/mat/mat044/sigeps44t.F
Chd|-- called by -----------
Chd|        TFORC3                        source/elements/truss/tforc3.F
Chd|-- calls ---------------
Chd|        CVMGT                         source/system/machine.F       
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS44T(NEL     ,NGL     ,MAT     ,PID     ,UPARAM  ,
     .                     IPM     ,GEO     ,OFF     ,FOR     ,STI     ,
     .                     PLA     ,EINT    ,AREA    ,AL0     ,AL      ,
     .                     EPSP    ,NUVAR   ,UVAR    ,NPF     ,TF      ,
     .                     NFUNC   ,IFUNC   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN)  :: NEL,NUVAR,NPF(*),NFUNC,IFUNC(NFUNC)
      INTEGER ,DIMENSION(NEL) ,INTENT(IN)  :: MAT,PID,NGL
      INTEGER ,DIMENSION(NPROPMI,NUMMAT) ,INTENT(IN) :: IPM
      my_real ,DIMENSION(NPROPG ,NUMGEO) ,INTENT(IN) :: GEO
      my_real ,DIMENSION(*) ,INTENT(IN) :: UPARAM,TF
      my_real ,DIMENSION(NEL) :: OFF,FOR,EINT,AREA,AL0,AL,PLA,STI,EPSP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,IADBUF,NINDX
      INTEGER ,DIMENSION(NEL) :: INDX,ICC,ISRATE,VFLAG
      my_real :: YMA,EPIF,DMG,FRATE,EPSDOT,ALPHA
      my_real ,DIMENSION(NEL) :: E,NU,CA,CB,CN,CP,YLD,YLDMAX,AA,HH,FF,
     .                           GAP,EPST,EPDR,EPMAX,EPSR1,EPSR2,ASRATE,
     .                           YSCALE,DPLA
      my_real ,DIMENSION(NEL,NUVAR) :: UVAR
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      my_real :: CVMGT,FINTER
      EXTERNAL FINTER
c=======================================================================
      EPIF   = ZERO
c
      DO I=1,NEL
        IADBUF   = IPM(7,MAT(I))-1
        E(I)     = UPARAM(IADBUF+1)
        NU(I)    = UPARAM(IADBUF+2)
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
        ISRATE(I)= NINT(UPARAM(IADBUF+13))
        ASRATE(I)= UPARAM(IADBUF+14)
        VFLAG(I) = NINT(UPARAM(IADBUF+23))
        YSCALE(I)= UPARAM(IADBUF+24)
c
        GAP(I)   = GEO(2,PID(I))
        DPLA(I)  = ZERO
      ENDDO
c
      DO I=1,NEL
        IF (GAP(I) > ZERO .AND. AL(I) <= (AL0(I)-GAP(I))) OFF(I) = ONE
      ENDDO
c
      DO I=1,NEL
        EINT(I) = EINT(I) + FOR(I)*EPSP(I)*AL(I)*DT1*HALF
      ENDDO
c
      DO I=1,NEL
        AREA(I) = AREA(I)*(ONE - TWO*NU(I)*EPSP(I)*DT1*OFF(I))
      ENDDO
c
      DO I=1,NEL
        YMA = E(I)*AREA(I)
        FOR(I)  = FOR(I) + YMA*EPSP(I)*DT1
        EPST(I) = FOR(I) / YMA
        STI(I)  = YMA / AL(I)
      ENDDO
c
      DO I=1,NEL
        IF (NFUNC>0) THEN
          YLD(I) = YSCALE(I)*FINTER(IFUNC(1),PLA(I),NPF,TF,HH(I))
          HH(I)  = YSCALE(I)*HH(I)
        ELSE
          YLD(I) = CA(I) + CB(I)*(PLA(I)**CN(I))
          IF (CN(I) == ONE) THEN
            HH(I) = CB(I)
          ELSE
            IF (PLA(I) > ZERO) THEN
              HH(I) = CB(I)*CN(I)/PLA(I)**(ONE-CN(I))
            ELSE
              HH(I) = ZERO
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C-------------
C     STRAIN RATE EFFECT
C-------------
      IF (EPIF > ZERO) THEN
        DO I = 1,NEL
          IF (EPDR(I) > ZERO) THEN
            IF (VFLAG(I) /= 1) THEN
              IF (ISRATE(I) == 1) THEN
                ALPHA  = MIN(ONE, ASRATE(I)*DT1)
                EPSDOT = ALPHA*ABS(EPSP(I)) + (ONE-ALPHA)*UVAR(I,1)
                UVAR(I,1) = EPSDOT
              ELSE
                EPSDOT = ABS(EPSP(I))
              ENDIF
            ELSE
              EPSDOT = UVAR(I,1)
            ENDIF
            FRATE = ONE + (EPSDOT*EPDR(I))**CP(I)
            IF (ICC(I)== 1) YLDMAX(I) = YLDMAX(I) * FRATE
            IF ((NFUNC > 0) .AND. (CA(I) /= ZERO)) THEN
              YLD(I) = YLD(I) + (CA(I) + CB(I)*(PLA(I)**CN(I)))*(FRATE-ONE)
              IF (CN(I) == ONE) THEN
                HH(I) = HH(I) + CB(I)*(FRATE-ONE)
              ELSE
                IF (PLA(I) > ZERO) THEN
                  HH(I) = HH(I) + CB(I)*CN(I)/PLA(I)**(ONE-CN(I))*(FRATE-ONE)
                ENDIF
              ENDIF
            ELSE
              YLD(I) = YLD(I) * FRATE
              HH(I)  = HH(I)  * FRATE
            ENDIF
          ENDIF
        ENDDO
      ENDIF
c-------------
      DO I=1,NEL
        AA(I)  = (E(I) + HH(I))*AREA(I)
        YLD(I) = MIN(YLD(I),YLDMAX(I))
        FF(I)  = ABS(FOR(I)) - YLD(I)*AREA(I)
        FF(I)  = MAX(ZERO,FF(I))
      ENDDO
c
      DO I=1,NEL
        DPLA(I) = FF(I)/AA(I)
        PLA(I)  = PLA(I) + FF(I)/AA(I)
      ENDDO
c
      DO I=1,NEL
        FOR(I) = CVMGT(SIGN(YLD(I)*AREA(I),FOR(I)),FOR(I),FF(I) > ZERO)
      ENDDO
C--------------------------------
C     TEST DE RUPTURE DUCTILE
C-------------------------------
      DO I=1,NEL
        IF (OFF(I) < EM01) OFF(I) = ZERO
        IF (OFF(I) < ONE)   OFF(I) = OFF(I)*FOUR_OVER_5
      ENDDO
c--------------------------------
c     AXIAL TENSION OR PLASTIC STRAIN FAILURE
c--------------------------------
      NINDX = 0
      DO I = 1,NEL
        IF (OFF(I) == ONE) THEN
          DMG = ONE
          IF (EPST(I) > EPSR1(I)) THEN
            DMG = (EPSR2(I) - EPST(I)) / (EPSR2(I) - EPSR1(I))
            DMG = MAX(DMG, ZERO)
            FOR(I) = FOR(I)*DMG
          ENDIF
c         test strain failure
          IF (DMG == ZERO .or. PLA(I) >= EPMAX(I)) THEN
            OFF(I)   = FOUR_OVER_5
            IDEL7NOK = 1
            NINDX = NINDX+1
            INDX(NINDX) = I
          ENDIF
c
          IF (VFLAG(I)==1) THEN
            ALPHA  = MIN(ONE, ASRATE(I)*DT1)
            EPSDOT = DPLA(I)/MAX(EM20,DT1)
            UVAR(I,1) = ALPHA*EPSDOT + (ONE - ALPHA)*UVAR(I,1)
          ENDIF
        ENDIF
      ENDDO
c
      IF (NINDX > 0) THEN
        DO J=1,NINDX
          I = INDX(J)
#include "lockon.inc"
          WRITE(IOUT,1000)  NGL(I)
          WRITE(ISTDO,1100) NGL(I),TT
#include "lockoff.inc"
        ENDDO
      ENDIF
c
      DO I=1,NEL
        STI(I) = STI(I)*OFF(I)
        FOR(I) = FOR(I)*OFF(I)
      ENDDO
c
      DO I=1,NEL
        EINT(I) = EINT(I) + FOR(I)*EPSP(I)*AL(I)*DT1*HALF
      ENDDO
c-----------------------------------------------
 1000 FORMAT(1X,'-- RUPTURE OF TRUSS ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'-- RUPTURE OF TRUSS ELEMENT :',I10,' AT TIME :',G11.4)
c-----------------------------------------------
      RETURN
      END
