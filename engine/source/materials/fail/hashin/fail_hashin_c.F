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
Chd|  FAIL_HASHIN_C                 source/materials/fail/hashin/fail_hashin_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FAIL_HASHIN_C(
     1           NEL       ,NUPARAM   ,NUVAR     ,UPARAM    ,UVAR      ,
     2           TIME      ,TIMESTEP  ,IPG       ,ILAY      ,IPT       ,
     3           NGL       ,DMG_FLAG  ,DMG_SCALE ,DFMAX     ,TDEL      ,
     4           SIGNXX    ,SIGNYY    ,SIGNXY    ,SIGNYZ    ,SIGNZX    ,
     5           OFF       ,FOFF      ,PTHKF     ,IGTYP     ,PLY_ID    ,
     6           EPSP      ,FWAVE_EL  ,DADV      )
C-----------------------------------------------
c    Hashin failure model
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "units_c.inc"
#include "comlock.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C OFF     | NEL     | F | R | DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C FOFF    | NEL     | I |R/W| DELETED INTEGRATION POINT FLAG (=1 ON, =0 OFF)
C PTHKF   |  1      | F | W | PERCENTAGE OF LAYER THICKNESS TO FAIL
C DFMAX   | NEL     | F |R/W| MAX DAMAGE FACTOR 
C TDEL    | NEL     | F | W | FAILURE TIME
C DMG_FLAG|  1      | I | W | STRESS REDUCTION FLAG DUE TO DAMAGE
C DMG_SCALE| NEL    | F | W | STRESS REDUCTION FACTOR
C---------+---------+---+---+--------------------------------------------
C NGL                         ELEMENT ID
C IPG                         CURRENT GAUSS POINT (in plane)
C ILAY                        CURRENT LAYER
C IPT                         CURRENT INTEGRATION POINT IN THE LAYER
C---------+---------+---+---+--------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NUPARAM,NUVAR,IPG,ILAY,IPT,IGTYP,PLY_ID
      INTEGER ,DIMENSION(NEL) ,INTENT(IN) :: NGL
      my_real ,INTENT(IN) :: TIME
      my_real ,DIMENSION(NEL) ,INTENT(IN)    :: OFF,TIMESTEP
      my_real,DIMENSION(NUPARAM) ,INTENT(IN) :: UPARAM
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER ,INTENT(OUT) ::DMG_FLAG
      INTEGER ,DIMENSION(*) :: FWAVE_EL
      INTEGER ,DIMENSION(NEL) ,INTENT(INOUT) :: FOFF
      my_real ,INTENT(OUT) :: PTHKF
      my_real ,DIMENSION(NEL) ,INTENT(INOUT) :: DFMAX,DADV,
     .   SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX,EPSP
      my_real ,DIMENSION(NEL) ,INTENT(OUT)   :: TDEL,DMG_SCALE
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,JJ,NINDX,IFAIL_SH,IMODEL,IMODE,TMOD,IFRWAVE,FAC
      INTEGER ,DIMENSION(NEL) :: INDX,FS
      INTEGER ,DIMENSION(7,NEL)   :: MODE
      my_real :: SIGT1,SIGT2,SIGC1,SIGC2,CSIG,FSIG12,MSIG12,MSIG23,
     .   MSIG13,ANGLE,SDEL,TMAX,RATIO,F1,F2,F3,F4,F5,SIG,P,F6,F7,
     .   TSIG12,XSIG12,XSIG23,DAMMX, TELEM,K2M,INSTF,K0,K1,K2,K3,
     .   ZEP669741,DTINV,KM,EPSPREF,FILT,ALPHA
c=======================================================================C
      ZEP669741 = SIX*EM01 + SIX*EM02 + NINE*EM3 + SEVEN*EM04 +
     .            FOUR*EM05 + EM06
C      
      IMODEL   = INT(UPARAM(1))
      SIGT1    = UPARAM(2)
      SIGT2    = UPARAM(3)
      SIGC1    = UPARAM(5)
      SIGC2    = UPARAM(6)
      CSIG     = UPARAM(7)
      FSIG12   = UPARAM(8)
      MSIG12   = UPARAM(9)
      MSIG13   = UPARAM(10)
      MSIG23   = UPARAM(11)
      ANGLE    = UPARAM(12)
      SDEL     = UPARAM(13)
      TMAX     = UPARAM(14)
      IFAIL_SH = INT(UPARAM(15))
      RATIO    = UPARAM(17)
      DMG_FLAG = INT(UPARAM(18))
      TMOD    = INT(UPARAM(19))
      EPSPREF = UPARAM(20)
      FILT    = UPARAM(21)
      KM      = UPARAM(22)
      IFRWAVE = NINT(UPARAM(23))
c
      IF (IFAIL_SH == 1) THEN
        PTHKF = EM06
      ELSEIF (IFAIL_SH == 2) THEN
        PTHKF = RATIO
      ELSE   !  IFAIL_SH = 3
        PTHKF = ONE - EM06
      ENDIF
c-------------------------------
      MODE(1,1:NEL) = 0
      MODE(2,1:NEL) = 0
      MODE(3,1:NEL) = 0
      MODE(4,1:NEL) = 0
      MODE(5,1:NEL) = 0
      MODE(6,1:NEL) = 0
      MODE(7,1:NEL) = 0
      NINDX  = 0
c
      IF (TIME == ZERO) then
        DADV(:) = ONE
      ENDIF
c
      IF (IFRWAVE > 1) THEN   ! front wave propagation
        DO I=1,NEL
          IF (FWAVE_EL(I) /= 0) DADV(I) = UPARAM(24)
        END DO
      ENDIF
c-------------------------------
      SELECT CASE (IMODEL)
c-------------------------------
      CASE (1)   !    Matrix or fiber failure mode
c-------------------------------
        DO I=1,NEL
          IF (OFF(I) == ONE .and. FOFF(I) == 1) THEN
            F1 = ZERO
            F2 = ZERO
            F3 = ZERO
            F4 = ZERO
            F5 = ZERO
C alpha = number of cycles                                 
            DTINV = TIMESTEP(I)/MAX(TIMESTEP(I)**2,EM20)
            ALPHA = INT(MAX(ONE,FILT*DTINV))
c alpha coef de moyenne mobile exponentielle
            ALPHA = TWO/(ALPHA + ONE)
            FS(I)=UVAR(I,10)
c smoothed strain rate
            EPSP(I)=(ONE - ALPHA)*UVAR(I,11)  +  ALPHA*EPSP(I)
            UVAR(I,11)=EPSP(I)
            IF (UVAR(I,6) == ZERO) THEN
c
c             fiber criteria
c
              SIG = HALF*(SIGNXX(I) + ABS(SIGNXX(I)))
              F1  = (SIG/SIGT1)**2
     .            + (SIGNXY(I)**2 + SIGNZX(I)**2)/FSIG12**2
              SIG = -HALF*SIGNYY(I)
              SIG = -SIGNXX(I) + HALF*(SIG + ABS(SIG))
              SIG = HALF*(SIG + ABS(SIG))
              F2  = (SIG /SIGC1)**2
              P   = -THIRD*(SIGNXX(I) + SIGNYY(I))
              IF (P > 0) F3 = (P/CSIG)**2
c
c             matrix criteria
c
              F5 = (SIGNYZ(I)/MSIG23)**2 + (SIGNZX(I)/MSIG13)**2
              F5 = SDEL*SDEL*F5
              IF (SIGNYY(I) < ZERO) THEN
                XSIG12 = MSIG12 - SIGNYY(I)*(TAN(ANGLE))
                XSIG23 = MSIG23 - SIGNYY(I)*(TAN(ANGLE))
              ELSE
                XSIG12 = MSIG12
                XSIG23 = MSIG23
              ENDIF
c
              SIG = HALF*(SIGNYY(I) + ABS(SIGNYY(I)))
              F4  = (SIG/SIGT2)**2
     .            + (SIGNYZ(I)/XSIG23)**2 + (SIGNXY(I)/XSIG12)**2
c
              DAMMX    = MIN(ONE,MAX(F1,F2,F3,F4,F5))
              DFMAX(I) = MIN(ONE,DAMMX)
              IF (DAMMX >= DADV(I)) THEN
                NINDX = NINDX+1
                INDX(NINDX) = I
                UVAR(I,6) = TIME
C
                FAC =  HUNDRED*MAX(ABS(SIGNXX(I)),ABS(SIGNYY(I)),
     .                  ABS(SIGNXY(I)),ABS(SIGNYZ(I)),ABS(SIGNZX(I)))
                K0 =  FAC*EPSPREF**2*ZEP669741*TMAX**2 + ZEP9*EPSPREF*TMAX
                K1 =  ZEP9*EPSP(I)
                K2 =  ZEP669741*FAC*EPSP(I)**2
                K2M = ZEP669741*FAC*KM*EPSPREF**2
                K2 = MAX(K2M,K2,EM20)
                TELEM = (SQRT(K1**2+4*K2*K0)-K1)/2/K2
                UVAR(I,12) = TELEM
                IF (F1>=DADV(I)) MODE(1,I) = 1
                IF (F2>=DADV(I)) MODE(2,I) = 1
                IF (F3>=DADV(I)) MODE(3,I) = 1
                IF (F4>=DADV(I)) MODE(4,I) = 1
                IF (F5>=DADV(I)) MODE(5,I) = 1
                IF (DMG_FLAG == 0) THEN
                  UVAR(I,1) = SIGNXX(I)
                  UVAR(I,2) = SIGNYY(I)
                  UVAR(I,3) = SIGNXY(I)
                  UVAR(I,4) = SIGNYZ(I)
                  UVAR(I,5) = SIGNZX(I)
                ENDIF
              ENDIF
            ENDIF
c
            IF (UVAR(I,6) > ZERO .AND. TIME > UVAR(I,6) ) THEN
               IF(TMOD == 1) THEN
                  TELEM = MAX((TIMESTEP(I)*1.0),TMAX*(EPSPREF/EPSP(I))**2)
                  DMG_SCALE(I)= DMG_SCALE(I)*EXP(-TIMESTEP(I)/TELEM)
              ELSEIF(TMOD == 2) THEN
                 TELEM = UVAR(I, 12)
                 DMG_SCALE(I) = EXP(-(TIME - UVAR(I,6))/TELEM)
              ELSE
                DMG_SCALE(I) = EXP(-(TIME - UVAR(I,6))/TMAX)
              ENDIF
              IF (DMG_SCALE(I) < EM02) THEN
                FOFF(I) = 0
                TDEL(I)= TIME
                DMG_SCALE(I) = ZERO
              ENDIF
              IF (DMG_FLAG == 0) THEN
                SIGNXX(I) = UVAR(I,1)*DMG_SCALE(I)
                SIGNYY(I) = UVAR(I,2)*DMG_SCALE(I)
                SIGNXY(I) = UVAR(I,3)*DMG_SCALE(I)
                SIGNYZ(I) = UVAR(I,4)*DMG_SCALE(I)
                SIGNZX(I) = UVAR(I,5)*DMG_SCALE(I)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
c ----------------------------------------------
      CASE (2)   !    Fabric failure mode
c-------------------------------
        DO I=1,NEL
          IF (OFF(I) == ONE .and. FOFF(I) == 1) THEN
            F1 = ZERO
            F2 = ZERO
            F3 = ZERO
            F4 = ZERO
            F5 = ZERO
            F6 = ZERO
            F7 = ZERO
c
C alpha = number of cycles                                 
            DTINV = TIMESTEP(I)/MAX(TIMESTEP(I)**2,EM20)
            ALPHA = INT(MAX(ONE,FILT*DTINV))
c alpha coef de moyenne mobile exponentielle
            ALPHA = TWO/(ALPHA + ONE)
            FS(I)=UVAR(I,10)
c smoothed strain rate
            EPSP(I)=(ONE - ALPHA)*UVAR(I,11)  +  ALPHA*EPSP(I)
            UVAR(I,11)=EPSP(I)
            IF (UVAR(I,6) == ZERO) THEN
c
              SIG = HALF*(SIGNXX(I) + ABS(SIGNXX(I)))
              F1  = (SIG/SIGT1)**2
     .            + (SIGNXY(I)**2 + SIGNZX(I)**2)/FSIG12**2
              SIG = HALF*(SIGNYY(I) + ABS(SIGNYY(I)))
              TSIG12 = FSIG12*SIGT2/SIGT1
              F2 = (SIG/SIGT2)**2
     .           + (SIGNXY(I)**2 + SIGNYZ(I)**2)/TSIG12**2

              IF (SIGNXX(I) < ZERO) F3 = (SIGNXX(I)/SIGC1)**2
              IF (SIGNYY(I) < ZERO) F4 = (SIGNYY(I)/SIGC2)**2
              P = -THIRD*(SIGNXX(I) + SIGNYY(I) )
              IF (P > ZERO) F5 = (P/CSIG)**2
              F6 = (SIGNXY(I)/MSIG12)**2
              F7 = (SIGNYZ(I)/MSIG23)**2 + (SIGNZX(I)/MSIG13)**2
              F7 = SDEL*SDEL*F6
c
              DAMMX    = MIN(ONE,MAX(F1,F2,F3,F4,F5,F6,F7))
              DFMAX(I) = MIN(ONE,DAMMX)
              IF (DAMMX >= DADV(I)) THEN
                NINDX = NINDX+1
                INDX(NINDX) = I
                UVAR(I,6) = TIME
C
                FAC =  HUNDRED*MAX(ABS(SIGNXX(I)),ABS(SIGNYY(I)),
     .                  ABS(SIGNXY(I)),ABS(SIGNYZ(I)),ABS(SIGNZX(I)))
                K0 =  FAC*EPSPREF**2*ZEP669741*TMAX**2 + ZEP9*EPSPREF*TMAX
                K1 =  ZEP9*EPSP(I)
                K2 =  ZEP669741*FAC*EPSP(I)**2
                K2M = ZEP669741*FAC*KM*EPSPREF**2
                K2 = MAX(K2M,K2,EM20)
                TELEM = (SQRT(K1**2+4*K2*K0)-K1)/2/K2
                UVAR(I,12) = TELEM
C                                                                
                IF (F1>=DADV(I)) MODE(1,I) = 1
                IF (F2>=DADV(I)) MODE(2,I) = 1
                IF (F3>=DADV(I)) MODE(3,I) = 1
                IF (F4>=DADV(I)) MODE(4,I) = 1
                IF (F5>=DADV(I)) MODE(5,I) = 1
                IF (F6>=DADV(I)) MODE(6,I) = 1
                IF (F7>=DADV(I)) MODE(7,I) = 1
                IF (DMG_FLAG == 0) THEN
                  UVAR(I,1) = SIGNXX(I)
                  UVAR(I,2) = SIGNYY(I)
                  UVAR(I,3) = SIGNXY(I)
                  UVAR(I,4) = SIGNYZ(I)
                  UVAR(I,5) = SIGNZX(I)
                ENDIF
              ENDIF
            ENDIF
c
            IF (UVAR(I,6) > ZERO .AND. TIME > UVAR(I,6) ) THEN
              DFMAX(I) = ONE
              IF(TMOD == 1) THEN
                  TELEM = MAX((TIMESTEP(I)*1.0),TMAX*(EPSPREF/EPSP(I))**2)
                  DMG_SCALE(I)=DMG_SCALE(I)*EXP(-TIMESTEP(I)/TELEM)
              ELSEIF(TMOD == 2) THEN
                 TELEM = UVAR(I,12)
                 DMG_SCALE(I)=EXP(-(TIME - UVAR(I,6))/TELEM)
              ELSE
                DMG_SCALE(I)= EXP(-(TIME - UVAR(I,6))/TMAX)
              ENDIF
c
              IF (DMG_SCALE(I) < EM02) THEN
                FOFF(I) = 0
                TDEL(I) = TIME
                DMG_SCALE(I) = ZERO
              ENDIF
              IF (DMG_FLAG == 0) THEN
                SIGNXX(I) = UVAR(I,1)*DMG_SCALE(I)
                SIGNYY(I) = UVAR(I,2)*DMG_SCALE(I)
                SIGNXY(I) = UVAR(I,3)*DMG_SCALE(I)
                SIGNYZ(I) = UVAR(I,4)*DMG_SCALE(I)
                SIGNZX(I) = UVAR(I,5)*DMG_SCALE(I)
              ENDIF
            ENDIF
          ELSEIF (FOFF(I) == 0) THEN
            DMG_SCALE(I) = ZERO
          ENDIF    ! FOFF
        ENDDO      ! I=1,NEL
c-------------
      END SELECT
c--------------------------------------------
c-    print
c--------------------------------------------
      IF (NINDX > 0) THEN
        DO J=1,NINDX
          I = INDX(J)
          DO JJ=1,6
            IMODE = MODE(JJ,I)
            IF (IMODE == 1) THEN
              IMODE = JJ
#include      "lockon.inc"
              WRITE(IOUT  ,1000) NGL(I),IPG,ILAY,IPT
              WRITE(ISTDO ,1000) NGL(I),IPG,ILAY,IPT
              WRITE(IOUT  ,2000) IMODE,TIME
              WRITE(ISTDO ,2000) IMODE,TIME
              WRITE(IOUT  ,3000) SIGNXX(I),SIGNYY(I),SIGNXY(I),
     .                           SIGNYZ(I),SIGNZX(I)
              IF (DADV(I) < ONE) WRITE(IOUT,4000) DADV(I)
#include      "lockoff.inc"
            ENDIF
          ENDDO
        END DO
      ENDIF
c--------------------------------------------
 1000 FORMAT(1X,'FAILURE (HASHIN) OF SHELL ELEMENT ',I10,
     .       1X,',GAUSS PT',I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3)
 2000 FORMAT(10X,'HASHIN MODE #',I10,1X, ',TIME #:',1PE12.4)
 3000 FORMAT(10X,'RESPONSIBLE STRESS: SIG_11= ',1PE12.4,
     .       1X,'SIG_22= ',1PE12.4,1X,'SIG_12= ',1PE12.4,/,
     .       10X,'SIG_13= ',1PE12.4,1X,'SIG_23= ',1PE12.4,1X)
 4000 FORMAT(1X,'CRITERIA REDUCTION FACTOR= ',1PE12.4)
c--------------------------------------------
      RETURN
      END
