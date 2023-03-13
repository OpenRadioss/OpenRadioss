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
Chd|  FAIL_SNCONNECT                source/materials/fail/snconnect/fail_snconnect.F
Chd|-- called by -----------
Chd|        SUSER43                       source/elements/solid/sconnect/suser43.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE FAIL_SNCONNECT (
     1           NEL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,
     2           NPF     ,TF      ,TIME    ,TIMESTEP,UPARAM  ,
     3           UVAR    ,NGL     ,IPG     ,NPG     ,NDAMF   ,
     4           EPSD    ,PLA     ,OFFG    ,OFFL    ,ISOLID  ,
     5           SIGNZZ  ,SIGNYZ  ,SIGNZX  ,SYM     ,AREA    ,
     6           DMG     ,DAMT    ,DFMAX   ,TDELE   )
C-----------------------------------------------
c  SNCONNECT failure model for solid spotwelds
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF FAILURE ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C NFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW not used
C IFUNC   | NFUNC   | I | R | FUNCTION INDEX not used
C NPF     |  *      | I | R | FUNCTION ARRAY   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER FAILURE PARAMETER ARRAY
C EPSP    | NEL     | F | W | STRAIN RATE
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFFG    | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C-----------------------------------------------
C        Y = FINTER(IFUNC(J),X,NPF,TF,DYDX)
C        Y       : y = f(x)
C        X       : x
C        DYDX    : f'(x) = dy/dx
C        IFUNC(J): FUNCTION INDEX
C              J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
C        NPF,TF  : FUNCTION PARAMETER
CC-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "units_c.inc"
#include "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUPARAM,NUVAR,NFUNC,IPG,NPG,NDAMF,ISOLID
      INTEGER NGL(NEL),NPF(*),IFUNC(*)
      my_real TIME,TIMESTEP
      my_real UPARAM(NUPARAM),UVAR(NEL,NUVAR),DAMT(NEL,NDAMF),TF(*)
      my_real , DIMENSION(NEL) :: OFFG,OFFL,EPSD,PLA,DEIN,DEIT,
     .   SIGNZZ,SIGNYZ,SIGNZX,SYM,DMG,DFMAX,TDELE,AREA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IDEL,IDEV,NINDX,NINDXD,NINDXA,ISYM,FUNN,FUNT,IFUN2N,
     .  IFUN2T,IFUN3N,IFUN3T
      INTEGER INDX(NEL),INDXD(NEL),INDXA(NEL)
      my_real A2,B2,A3,B3,XSCALE2,XSCALE3,SVMN,SVMT,T1,T2,TTN,TTS,FCT,
     .  DYDX,DAMA,SSYM,CPHI,SPHI,AREASCALE,DEFO
      my_real , DIMENSION(NEL) :: FUN2N,FUN2T,FUN3N,FUN3T,DYDX2N,
     .  DYDX2T,DYDX3N,DYDX3T,PHI,PLA1,PLA2
C-----------------------------------------------
C     E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      my_real  FINTER 
      EXTERNAL FINTER
C=======================================================================
c Internal VARIABLES :
c-----------------
c UVAR1  initial plastic elongation 
c UVAR2  final plastic elongation (at rupture)
c-----------------
c Damage output VARIABLES :
c-----------------
c DAMT1  damage factor d,   where (sig = sig *(1-d))
c DAMT2  damage function at damage start
c DAMT3  damage function at rupture 
C=======================================================================
      A2 = UPARAM(1)          
      B2 = UPARAM(2)          
      A3 = UPARAM(3)          
      B3 = UPARAM(4)          
      ISOLID    = NINT(UPARAM(5))          
      XSCALE2   = UPARAM(6)          
      XSCALE3   = UPARAM(7) 
      ISYM      = NINT(UPARAM(8))
      AREASCALE = UPARAM(9)        
      
      IFUN2N  = IFUNC(1)
      IFUN2T  = IFUNC(2)
      IFUN3N  = IFUNC(3)
      IFUN3T  = IFUNC(4)
C-----------------------------------------------
      IF (TIME == ZERO) THEN
        DO I=1,NEL
          UVAR(I,3)= AREA(I)    ! initial area at time = 0
        ENDDO   
      ENDIF
c      
      DO I=1,NEL
        PLA1(I) = UVAR(I,1)
        PLA2(I) = UVAR(I,2)    
      END DO                                 
C-----------------------------------------------
C    RATE FUNCTIONS INERPOLATION
C-----------------------------------------------
      IF (IFUN2N == 0) THEN
        FUN2N(1:NEL) = ONE
      ELSE
        DO I=1,NEL
          FUN2N(I) = FINTER(IFUN2N,EPSD(I)*XSCALE2,NPF,TF,DYDX2N)
        ENDDO
      ENDIF
      IF (IFUN2T == 0) THEN
        FUN2T(1:NEL) = ONE
      ELSE
        DO I=1,NEL
          FUN2T(I) = FINTER(IFUN2T,EPSD(I)*XSCALE2,NPF,TF,DYDX2T)
        ENDDO
      ENDIF
      IF (IFUN3N == 0) THEN
        DO I=1,NEL
          FUN3N(I) = ONE
        ENDDO
      ELSE
        DO I=1,NEL
          FUN3N(I) = FINTER(IFUN3N,EPSD(I)*XSCALE3,NPF,TF,DYDX3N)
        ENDDO
      ENDIF
      IF (IFUN3T == 0) THEN
        DO I=1,NEL
          FUN3T(I) = ONE
        ENDDO
      ELSE
        DO I=1,NEL
          FUN3T(I) = FINTER(IFUN3T,EPSD(I)*XSCALE3,NPF,TF,DYDX3T)
        ENDDO
      ENDIF
C-----------------------------------------------
      NINDX  = 0  
      NINDXD = 0  
      NINDXA = 0  
c
      DO I=1,NEL
        IDEL  = 0
        DAMA  = ZERO
        SSYM  = SIN(SYM(I))
        SVMN  = ABS(SIGNZZ(I))                                                   
        SVMT  = SQRT(SIGNYZ(I)**2 + SIGNZX(I)**2)
        PHI(I)= ATAN(SVMN/MAX(EM20,SVMT))
        SPHI  = SIN(PHI(I))
        CPHI  = COS(PHI(I))
c
        IF (PLA1(I) == ZERO) THEN   ! No damage yet
          IF (ISYM == 1 .AND. SIGNZZ(I)<= ZERO) THEN
            T1 = ZERO
          ELSE
            T1  = SPHI/(ONE-A2*SSYM)/FUN2N(I)
          ENDIF
          T2  = CPHI/FUN2T(I)
          TTN = T1*PLA(I)
          TTS = T2*PLA(I)
          FCT = (TTN**B2 + TTS**B2)**(ONE/B2)
          DAMT(I,2) = MIN(FCT, ONE)
          IF (FCT > ONE ) THEN 
            PLA1(I) = (T1**B2 + T2**B2)**(-ONE/B2)
            IF (ISYM == 1 .AND. SIGNZZ(I) <= ZERO)THEN
              T1 = ZERO
            ELSE
              T1  = SPHI/(ONE-A3*SSYM)/FUN3N(I)
            ENDIF
            T2  = CPHI/FUN3T(I)
            TTN = T1*PLA(I)
            TTS = T2*PLA(I)
            FCT = (TTN**B3 + TTS**B3) **(ONE / B3)
            PLA2(I)   = (T1**B3 + T2**B3)**(-ONE/B3)
            DAMT(I,1) = (PLA(I)-PLA1(I))/MAX(EM20,(PLA2(I) - PLA1(I)))
            DAMT(I,1) = MIN(DAMT(I,1), ONE)
            DAMT(I,3) = MIN(FCT, ONE)
            DAMA = DAMT(I,1)
            NINDXD = NINDXD+1
            INDXD(NINDXD) = I
          ENDIF
c          
        ELSE     ! (PLA1(I) > ZERO) 
          IF (ISYM == 1 .AND. SIGNZZ(I) <= ZERO) THEN
            T1 = ZERO
          ELSE
            T1  = SPHI/(ONE-A3*SSYM)/FUN3N(I)
          ENDIF
          T2  = CPHI/FUN3T(I)
          TTN = T1*PLA(I)                        
          TTS = T2*PLA(I)      
          FCT = (TTN**B3 + TTS**B3) **(ONE / B3)  
          PLA2(I)   = (T1**B3 + T2**B3)**(-ONE/B3)
          DAMT(I,1) = (PLA(I)-PLA1(I))/MAX(EM20,(PLA2(I) - PLA1(I)))
          DAMT(I,1) = MIN(DAMT(I,1), ONE)
          DAMT(I,3) = MIN(FCT, ONE)
          DAMA = DAMT(I,1)
c-----    check if rupture ...
          IF (FCT > ONE) THEN
            IDEL =1
            DAMA = ONE
          ENDIF
        ENDIF
        DMG(I) = MAX(DMG(I),DAMA)
        DMG(I) = MIN(DMG(I),ONE)   
c-----------------------------
        IF (IDEL == 1 .AND. OFFL(I) == ONE) THEN
          OFFL(I) = ZERO       ! local integ point rupture    
          NINDX = NINDX+1
          INDX(NINDX) = I
          TDELE(I) = TIME  
        ENDIF 
C           
        UVAR(I,1) = PLA1(I)
        UVAR(I,2) = PLA2(I)
C-------------  Maximum Damage storing for output : 0 < DFMAX < 1 
        DFMAX(I) = MIN(ONE,MAX(DFMAX(I),DAMT(I,1)))
c        
c---    deformed elements check
        IF (AREASCALE > ZERO ) THEN
          DEFO = UVAR(I,3) * AREASCALE
          IF (AREA(I) > DEFO .AND. OFFG(I) == ONE) THEN
            OFFL(I) = ZERO
            NINDXA = NINDXA+1
            INDXA(NINDXA) = I
            TDELE(I) = TIME  
            ISOLID   = 1
          ENDIF
        ENDIF
c-------------------------------        
      ENDDO     ! I=1,NEL                           
C-----------------------------------------------
      IF (NINDXD > 0) THEN
        DO J=1,NINDXD
          I = INDXD(J)
#include "lockon.inc"
          WRITE(IOUT ,1000) NGL(I),IPG,PLA1(I)
          WRITE(ISTDO,1100) NGL(I),IPG,PLA1(I),TIME
#include "lockoff.inc"
        END DO
      ELSEIF (NINDX > 0) THEN
        DO J=1,NINDX
          I = INDX(J)
#include "lockon.inc"
          WRITE(IOUT ,1200) NGL(I),IPG,PLA2(I)
          WRITE(ISTDO,1300) NGL(I),IPG,PLA2(I),TIME
#include "lockoff.inc"
        END DO
      ELSEIF (NINDXA > 0) THEN
        DO J=1,NINDXA
          I = INDXA(J)
#include "lockon.inc"
          WRITE(IOUT ,1400) NGL(I),AREA(I)
          WRITE(ISTDO,1500) NGL(I),AREA(I),TIME
#include "lockoff.inc"
        END DO
      ENDIF         
C-----------------------------------------------
 1000 FORMAT(5X,'START DAMAGE CONNECTION ELEMENT ',I10,
     .          ' INTEGRATION POINT',I2,', PLASTIC ELONGATION =',1PE16.9)
 1100 FORMAT(5X,'START DAMAGE CONNECTION ELEMENT ',I10,
     .          ' INTEGRATION POINT',I2,', PLASTIC ELONGATION =',1PE16.9,
     .          ' AT TIME ',1PE16.9)
 1200 FORMAT(5X,'FAILURE CONNECTION SOLID ELEMENT ',I10,
     .          ' INTEGRATION POINT',I2,',  PLASTIC ELONGATION=',1PE16.9)
 1300 FORMAT(5X,'FAILURE CONNECTION SOLID ELEMENT ',I10,
     .          ' INTEGRATION POINT',I2,',  PLASTIC ELONGATION :',1PE16.9,
     .          ' AT TIME ',1PE16.9)
 1400 FORMAT(5X,'FAILURE CONNECTION SOLID ELEMENT ',I10,
     .         ',   AREA (LIMIT REACHED) :',1PE16.9)
 1500 FORMAT(5X,'FAILURE CONNECTION SOLID ELEMENT ',I10,
     .          ',  AREA (LIMIT REACHED) :',1PE16.9,' AT TIME ',1PE16.9)
C-----------------------------------------------
      RETURN
      END
