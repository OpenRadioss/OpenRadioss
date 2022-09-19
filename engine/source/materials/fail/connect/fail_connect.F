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
Chd|  FAIL_CONNECT                  source/materials/fail/connect/fail_connect.F
Chd|-- called by -----------
Chd|        SUSER43                       source/elements/solid/sconnect/suser43.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE FAIL_CONNECT(
     1           NEL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,
     2           NPF     ,TF      ,TIME    ,TIMESTEP,UPARAM  ,
     3           UVAR    ,NGL     ,EPS1    ,EPS2    ,EPS3    ,
     4           EPSP    ,OFFG    ,OFFL    ,IPG     ,ISOLID  ,
     5           SIGNZZ  ,SIGNYZ  ,SIGNZX  ,DEIN    ,DEIT    ,
     6           DFMAX   ,TDELE   ,AREA    ,SOFT    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
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
C---------+---------+---+---+--------------------------------------------
C EPS1    | NEL     | F | W | LOCAL STRAIN1
C EPS2    | NEL     | F | W | LOCAL STRAIN2
C EPS3    | NEL     | F | W | LOCAL STRAIN3
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
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "scr17_c.inc"
#include "units_c.inc"
#include "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUPARAM,NUVAR,NFUNC,IPG,ISOLID
      INTEGER NGL(*),NPF(*),IFUNC(*)
      my_real TIME,TIMESTEP
      my_real UPARAM(NUPARAM),UVAR(NEL,NUVAR),TF(*)
      my_real, DIMENSION(NEL) :: OFFG,OFFL,EPS1,EPS2,EPS3,EPST,EPSP,
     .   SIGNZZ,SIGNYZ,SIGNZX,DEIN,DEIT,DFMAX,TDELE,AREA,SOFT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IDEL,NINDX,NINDXA,NINDXE,NINDXS,FUNN,FUNT,IFAILS,IFAILE,
     .   ISYM
      INTEGER, DIMENSION(NEL) :: INDX,INDXA,INDXE,INDXS
      my_real C1,C2,C3,C4,CM,DYDX,DAMT,IFLAG,DEFO,DTIME,DSOFT,AREASCALE,
     .   MAXDN,MAXDT,MAXEI,MAXEN,MAXET,FAC1,FAC2,ASCALE,EXPN,EXPT,NN,NT
      my_real, DIMENSION(NEL) :: EI,EN,ET,DAM1,DAM2,DAM3,DAM4
C-----------------------------------------------
C     E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      my_real FINTER
      EXTERNAL FINTER
C=======================================================================
      MAXDN  = UPARAM(1)
      MAXDT  = UPARAM(2)
      EXPN   = UPARAM(3)
      EXPT   = UPARAM(4)
      FAC1   = UPARAM(5)
      FAC2   = UPARAM(6)
      ASCALE = UPARAM(7)
      IFAILS = NINT(UPARAM(8))
      IFAILE = NINT(UPARAM(9))
      ISOLID = NINT(UPARAM(10))
      MAXEI  = UPARAM(11)
      MAXEN  = UPARAM(12)
      MAXET  = UPARAM(13)
      NN     = UPARAM(14)
      NT     = UPARAM(15)
      DTIME  = UPARAM(16)
      DSOFT  = UPARAM(17)
      ISYM   = NINT(UPARAM(18))
      AREASCALE = UPARAM(19)
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
      IF (ISIGI == ZERO) THEN
        IF (TIME == ZERO) THEN
          DO I=1,NEL
            UVAR(I,6)= ONE
            UVAR(I,13)= AREA(I)
          ENDDO
        ENDIF
      ENDIF
C-----------------------------------------------
      DO I=1,NEL
        DAM1(I) = UVAR(I,7)
        DAM2(I) = UVAR(I,8)
        DAM3(I) = UVAR(I,9)
        DAM4(I) = UVAR(I,10)
        EN(I)   = UVAR(I,11)
        ET(I)   = UVAR(I,12)
      ENDDO
C-----------------------------------------------
      NINDX  = 0
      NINDXE = 0
      NINDXS = 0
      NINDXA = 0
c
      DO I=1,NEL
        IDEL = 0
        ET(I) = ET(I) + DEIT(I)
        IF (ISYM == 1 .AND. SIGNZZ(I) < ZERO) THEN
          FAC1  = ZERO
        ELSE
          EN(I) = EN(I) + DEIN(I)
        ENDIF
        EI(I) = EN(I) + ET(I)
        C1 = ZERO
        C2 = ZERO
        C3 = ZERO
        C4 = ZERO
        CM = ZERO
c-----------------------------
c-----  Energy criterion
c-----------------------------
        IF (IFAILE == 1) THEN   !  global energy criterion
          C1 = EI(I)/MAXEI
          IF (C1 > ONE) THEN
            IF (DAM1(I) == ZERO) THEN
              NINDXE = NINDXE+1
              INDXE(NINDXE) = I
            ENDIF
            DAM1(I) = DAM1(I) + C1 * TIMESTEP
          ENDIF
c
        ELSEIF (IFAILE == 2) THEN  !  orthotropic energy rupture
          C2 = (ET(I)/MAXET)**NT+ (EN(I)/MAXEN)**NN
          IF (C2 > ONE) THEN
            IF (DAM1(I) == ZERO) THEN
              NINDXE = NINDXE+1
              INDXE(NINDXE) = I
            ENDIF
            DAM1(I) = DAM1(I) + C2*TIMESTEP
          ENDIF
        ELSEIF (IFAILE == 3) THEN  ! total + component
          C1 =  EI(I)/MAXEI
          C2 = (ET(I)/MAXET)**NT+ (EN(I)/MAXEN)**NN
          C1 = MAX(C1,C2)
          IF (C1 > ONE) THEN
            IF (DAM1(I) == ZERO) THEN
              NINDXE = NINDXE+1
              INDXE(NINDXE) = I
            ENDIF
            DAM1(I) = DAM1(I) + C1*TIMESTEP
          ENDIF
        ENDIF
c-----------------------------
c------ max strain criterion
c-----------------------------
        IF (IFAILS > 0) THEN
          FUNN = IFUNC(1)
          FUNT = IFUNC(2)
          ! eps2=epszx et eps3 epsxy
          EPST(I) = SQRT(EPS2(I)**2 + EPS3(I)**2)
          C3 = FAC1*EPS1(I)/MAXDN
          C4 = FAC2*EPST(I)/MAXDT
          IF (FUNN > 0) C3 = C3*FINTER(FUNN,EPSP(I)*ASCALE,NPF,TF,DYDX)
          IF (FUNT > 0) C4 = C4*FINTER(FUNT,EPSP(I)*ASCALE,NPF,TF,DYDX)
          IF (IFAILS == 1) THEN
c           unidirectional rupture in strain
            C3 = MAX(C3,C4)
            IF (C3 > ONE) THEN
              IF (DAM3(I) == ZERO) THEN
                NINDXS = NINDXS+1
                INDXS(NINDXS) = I
              ENDIF
              DAM3(I) = DAM3(I) + C3*TIMESTEP
            ENDIF
          ELSEIF (IFAILS == 2) THEN
c           multidirectional rupture in strain
            CM = ABS(C3)**EXPN + ABS(C4)**EXPT
            IF (CM > ONE) THEN
              IF (DAM3(I) == ZERO) THEN
                NINDXS = NINDXS+1
                INDXS(NINDXS) = I
              ENDIF
              DAM3(I) = DAM3(I) + CM* TIMESTEP
            ENDIF
          ENDIF
        ENDIF
c-----------------------------
        DAMT = MAX(DAM1(I),DAM3(I))
        IF (DAMT > DTIME) THEN
          IDEL =1
          SOFT(I) = ZERO
        ELSEIF (DSOFT == ONE) THEN
          SOFT(I) = ONE - DAMT/MAX(DTIME,EM20)
        ELSEIF (DSOFT > ZERO) THEN
          SOFT(I) = (ONE - DAMT/MAX(DTIME,EM20))**DSOFT
        ENDIF
c-----------------------------
        IF (IDEL == 1 .AND. OFFL(I) == ONE) THEN
          OFFL(I) = ZERO       ! local integ point rupture
          NINDX = NINDX+1
          INDX(NINDX) = I
          TDELE(I) = TIME
        ENDIF
c-----------------------------
c---    deformed elements check
        IF (AREASCALE > ZERO .AND. OFFG(I) == ONE ) THEN
          DEFO = UVAR(I,13) * AREASCALE
          IF (AREA(I) > DEFO) THEN
            OFFL(I) = ZERO
            NINDXA = NINDXA+1
            INDXA(NINDXA) = I
            TDELE(I) = TIME
            ISOLID   = 1
          ENDIF
        ENDIF
c-------------------------------
        UVAR(I,1) = MAX(UVAR(I,1), C1)
        UVAR(I,2) = MAX(UVAR(I,2), C2)
        UVAR(I,3) = MAX(UVAR(I,3), C3)
        UVAR(I,4) = MAX(UVAR(I,4), C4)
        UVAR(I,5) = MAX(UVAR(I,5), CM)
        UVAR(I,6) = SOFT(I)
c
        UVAR(I,7) = DAM1(I)
        UVAR(I,8) = DAM2(I)
        UVAR(I,9) = DAM3(I)
        UVAR(I,10)= DAM4(I)
        UVAR(I,11)= EN(I)
        UVAR(I,12)= ET(I)
c--     Maximum Damage storing for output : 0 < DFMAX < 1
        DFMAX(I) = MIN(ONE,MAX(DFMAX(I),DAMT/ MAX(DTIME,EM20)))
c
      ENDDO     ! I=1,NEL
C-----------------------------------------------
      IF (NINDXE > 0) THEN
        DO J=1,NINDXE
          I = INDXE(J)
#include "lockon.inc"
          WRITE(IOUT ,1000) NGL(I),IPG,EI(I)
          WRITE(ISTDO,1100) NGL(I),IPG,EI(I),TIME
#include "lockoff.inc"
        END DO
      ELSEIF (NINDXS > 0) THEN
        DO J=1,NINDXS
          I = INDXS(J)
#include "lockon.inc"
          WRITE(IOUT ,1600) NGL(I),IPG,EPS1(I),EPST(I)
          WRITE(ISTDO,1700) NGL(I),IPG,EPS1(I),EPST(I),TIME
#include "lockoff.inc"
        END DO
      ELSEIF (NINDX > 0) THEN
        DO J=1,NINDX
          I = INDX(J)
#include "lockon.inc"
          WRITE(IOUT ,1200) NGL(I),IPG
          WRITE(ISTDO,1300) NGL(I),IPG,TIME
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
     .          ' INTEGRATION POINT',I2,', ENERGY=',1PE16.9)
 1100 FORMAT(5X,'START DAMAGE CONNECTION ELEMENT ',I10,
     .          ' INTEGRATION POINT',I2,', ENERGY=',1PE16.9,
     .          ' AT TIME ',1PE16.9)
 1200 FORMAT(5X,'FAILURE CONNECTION SOLID ELEMENT ',I10,
     .          ' INTEGRATION POINT',I2)
 1300 FORMAT(5X,'FAILURE CONNECTION SOLID ELEMENT ',I10,
     .          ' INTEGRATION POINT',I2,' AT TIME ',1PE16.9)
 1400 FORMAT(5X,'FAILURE CONNECTION SOLID ELEMENT ',I10,
     .         ',   AREA(LIMIT REACHED) =',1PE16.9)
 1500 FORMAT(5X,'FAILURE CONNECTION SOLID ELEMENT ',I10,
     .          ',  AREA(LIMIT REACHED) :',1PE16.9,' AT TIME ',1PE16.9)
 1600 FORMAT(5X,'START DAMAGE CONNECTION ELEMENT ',I10,
     .          ' INTEGRATION POINT',I2,', EPSN=',1PE16.9,', EPST=',1PE16.9)
 1700 FORMAT(5X,'START DAMAGE CONNECTION ELEMENT ',I10,
     .          ' INTEGRATION POINT',I2,', EPSN=',1PE16.9,', EPST=',1PE16.9,
     .          ' AT TIME ',1PE16.9)
C-----------------------------------------------
      RETURN
      END
