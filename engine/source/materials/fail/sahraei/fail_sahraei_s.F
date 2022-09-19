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
Chd|  FAIL_SAHRAEI_S                source/materials/fail/sahraei/fail_sahraei_s.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE FAIL_SAHRAEI_S(
     1     NEL   ,NFUNC ,IFUNC ,NPF   ,TF     ,
     2     TIME  ,NGL   ,IPM   ,MAT   ,UPARAM ,
     3     EPSXX ,EPSYY ,EPSZZ ,EPSXY ,EPSYZ  ,
     4     EPSZX ,OFF   ,IP    ,DFMAX ,TDELE  ,
     5     DELTAX,NUVAR ,UVAR  )
C-----------------------------------------------
C   MIT Wierzbicki Sahraei electric battery failure
C   /FAIL/SAHRAEI1 - tabulated rupture criteria for solids
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include    "implicit_f.inc"
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
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
#include "units_c.inc"
#include "param_c.inc"
#include "scr17_c.inc"
#include "scr19_c.inc"
#include "impl1_c.inc"
#include "comlock.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL, NUPARAM,IP,NUVAR
      INTEGER NGL(NEL),IPM(NPROPMI,*),MAT(NEL)
      INTEGER, INTENT(IN) :: NFUNC
      INTEGER, DIMENSION(NFUNC), INTENT(IN) ::  IFUNC
      my_real TIME,UPARAM(*),
     .   EPSXX(NEL) ,EPSYY(NEL) ,EPSZZ(NEL) ,UVAR(NEL,NUVAR),
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,DELTAX(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real OFF(NEL), DFMAX(NEL),TDELE(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*)
      my_real FINTER ,TF(*),DYDX
      EXTERNAL FINTER
C        Y = FINTER(IFUNC(J),X,NPF,TF,DYDX)
C        Y       : y = f(x)
C        X       : x
C        DYDX    : f'(x) = dy/dx
C        IFUNC(J): FUNCTION INDEX
C              J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
C        NPF,TF  : FUNCTION PARAMETER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,II,J,IFAIL,NINDX1,NINDX2,NUM,DEN,
     .          FAIL_ORT,ORDI,COMP_DIR,NINDXOFF,IDEL
        my_real
     .          VOL_STRAIN_LIMIT,MAX_COMP_STRAIN,RATIO_2,EL_REF,DAMAGE_SP(NEL)
        DOUBLE PRECISION
     .          I1,I2,I3,E11,E22,E33,EXX,EYY,EZZ,EXY,EZX,EYZ,
     .          Q,R,R_INTER,PHI,RATIO(NEL),DAMAGE(NEL),E00,FAC,
     .          VOL_STRAIN,NUMERATOR,DENOMINATOR,ORDINATE,
     .          LAMBDA,PLAMAX(NEL)
        INTEGER,DIMENSION(NEL) :: INDX1,INDX2,INDX3,INDXOFF
C-----------------------------------------------
c!#---1----|----2----|----3----|----4----|----5----|----6----|----7----|----8----|----9----|---10----|
c!#
c!# NUMERATOR    =  1  ==> Eps_xx
c!# NUMERATOR    =  2  ==> Eps_yy
c!# NUMERATOR    =  3  ==> Eps_zz
c!# NUMERATOR    =  4  ==> Eps_1   1st. 3d-principal
c!# NUMERATOR    =  5  ==> Eps_2   2nd. 3d-principal
c!# NUMERATOR    =  6  ==> Eps_3   3rd. 3d-principal
c!#
c!# Denominator  =  1  ==> 2D e1 = x-z - plane
c!# Denominator  =  2  ==> 2D e1 = x-y - plane
c!# Denominator  =  3  ==> 2D e1 = y-z - plane
c!# Denominator  =  4  ==> Eps_1   1st. 3d-principal
c!# Denominator  =  5  ==> Eps_2   2nd. 3d-principal
c!# Denominator  =  6  ==> Eps_3   3rd. 3d-principal
c!#
c!# Ordinate     =  1  ==> MAX(Eps_xx,Eps_yy,Eps_zz)
c!# Ordinate     =  2  ==> Eps_xx
c!# Ordinate     =  3  ==> Eps_yy
c!# Ordinate     =  4  ==> Eps_zz
c!# Ordinate     =  5  ==> Eps_1   1st. 3d-principal
c!# Ordinate     =  6  ==> 2D - e1 = x-z - plane
c!# Ordinate     =  7  ==> 2D - e1 = x-y - plane
c!# Ordinate     =  8  ==> 2D - e1 = y-z - plane
c!#
c!# COMP_DIR     = 1   ==> Pressure stress in xx
c!# COMP_DIR     = 2   ==> Pressure stress in yy
c!# COMP_DIR     = 3   ==> Pressure stress in zz
c!#
c!# MAX_COMP_STRAIN = In-plane failure compression strain (not deleting, only damage=1)
c!# if value is negative, the element will NOT be deleted, only DAMAGE set to 1
c!# RATIO        = RATIO of the two other failure strains (default = 1.0)
c!#
c!
C-----------------------------------------------
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering model parameters
      VOL_STRAIN_LIMIT = UPARAM(1)
      NUM              = INT(UPARAM(2))
      DEN              = INT(UPARAM(3))
      ORDI             = INT(UPARAM(4))
      COMP_DIR         = INT(UPARAM(5))
      MAX_COMP_STRAIN  = UPARAM(6)
      RATIO_2          = UPARAM(7)
      IDEL             = INT(UPARAM(8))
      EL_REF           = UPARAM(9)
c
      ! Initialization
      IF ((UVAR(1,1) == ZERO).AND.(NFUNC == 2)) THEN
        DO I=1,NEL
          LAMBDA    = DELTAX(I)/EL_REF
          UVAR(I,1) = FINTER(IFUNC(2),LAMBDA,NPF,TF,DYDX)
        ENDDO
      ENDIF
c
      ! Recovering internal variables
      DO I=1,NEL
        ! Checking failure flag
        IF (OFF(I) < ONE)   OFF(I) = OFF(I)*FOUR_OVER_5
        IF (OFF(I) < EM01) OFF(I) = ZERO
        ! Resetting damage variable
        DAMAGE(I)    = ZERO
        DAMAGE_SP(I) = ZERO
        ! Recovering element size factor if needed
        IF (NFUNC == 2) THEN
          FAC = UVAR(I,1)
        ELSE
          FAC = ONE
        ENDIF
      END DO
c
      ! Initialization of couter
      NINDX1   = 0
      NINDX2   = 0
      NINDXOFF = 0
      DO I = 1,NEL
        IF (OFF(I) == ONE) THEN
          NINDXOFF = NINDXOFF + 1
          INDXOFF(NINDXOFF) = I
        ENDIF
      ENDDO
c
      !=======================================================================
      ! - COMPUTATION OF FAILURE PLASTIC STRAIN
      !=======================================================================
      ! Loop over elements
#include "vectorize.inc"
      DO II = 1,NINDXOFF
c
        ! Number of the element
        I = INDXOFF(II)
C
        ! Initialization of FAIL_ORT
        FAIL_ORT = ORDI
C
        ! Copying strain value
        EXX = EPSXX(I)
        EYY = EPSYY(I)
        EZZ = EPSZZ(I)
        EXY = HALF*EPSXY(I)
        EYZ = HALF*EPSYZ(I)
        EZX = HALF*EPSZX(I)
C
        ! Computation of strain tensor invariants (attention au formalisme : EPSXY = GAMMAXY = 2*DEFXY)
        I1 = EXX + EYY + EZZ
        I2 = EXX*EYY + EYY*EZZ + EZZ*EXX - EXY*EXY - EZX*EZX - EYZ*EYZ
        I3 = EXX*EYY*EZZ - EXX*EYZ*EYZ - EYY*EZX*EZX - EZZ*EXY*EXY + TWO*EXY*EZX*EYZ
        Q  = (THREE*I2 - I1*I1)/NINE
        R  = (TWO*I1*I1*I1-NINE*I1*I2+TWENTY7*I3)/CINQUANTE4
        R_INTER = MIN(R/SQRT(MAX(EM20,(-Q**3))),ONE)
        PHI     = ACOS(MAX(R_INTER,-ONE))
        ! Computation of principal strain
        E11 = TWO*SQRT(MAX(-Q,ZERO))*COS( PHI/THREE           ) + THIRD*I1
        E22 = TWO*SQRT(MAX(-Q,ZERO))*COS((PHI+TWO*PI)/THREE  ) + THIRD*I1
        E33 = TWO*SQRT(MAX(-Q,ZERO))*COS((PHI+FOUR*PI)/THREE) + THIRD*I1
        ! Sorting principal strains
        IF (E11 < E22) THEN
          R_INTER = E11
          E11     = E22
          E22     = R_INTER
        ENDIF
        IF (E22 < E33)THEN
          R_INTER = E22
          E22     = E33
          E33     = R_INTER
        ENDIF
        IF (E11 < E22)THEN
          R_INTER = E11
          E11     = E22
          E22     = R_INTER
        ENDIF
c
        ! Computation of volumetric strain
        VOL_STRAIN = E11 + E22 + E33 + E11*E22 + E11*E33 + E22*E33 + E11*E22*E33
c
        !-----------------------------------------------------------------------------------
        ! Volumetric strain failure
        !-----------------------------------------------------------------------------------
        IF (ABS(VOL_STRAIN) >= VOL_STRAIN_LIMIT) THEN
C                  
          ! Numerator flag for strain-ratio
          IF (NUM == 1) THEN
            NUMERATOR = EXX
          ELSEIF (NUM == 2) THEN
            NUMERATOR = EYY
          ELSEIF (NUM == 3) THEN
            NUMERATOR = EZZ
          ELSEIF (NUM == 4) THEN
            NUMERATOR = E11
          ELSEIF (NUM == 5) THEN
            NUMERATOR = E22
          ELSEIF (NUM == 6) THEN
            NUMERATOR = E33
          ENDIF
C
          ! Denominator flag for strain-ratio
          IF (DEN == 1) THEN
            DENOMINATOR = ((EXX + EZZ)/TWO) + SQRT(((EXX - EZZ)/TWO)**2 + EZX**2)
          ELSEIF (DEN == 2) THEN
            DENOMINATOR = ((EXX + EYY)/TWO) + SQRT(((EXX - EYY)/TWO)**2 + EXY**2)
          ELSEIF (DEN == 3) THEN
            DENOMINATOR = ((EYY + EZZ)/TWO) + SQRT(((EYY - EZZ)/TWO)**2 + EYZ**2)
          ELSEIF (DEN == 4) THEN
            DENOMINATOR = E11
          ELSEIF (DEN == 5) THEN
            DENOMINATOR = E22
          ELSEIF (DEN == 6) THEN
            DENOMINATOR = E33
          ENDIF
C
          ! Checking values
          RATIO(I) = ABS(NUMERATOR/MAX(DENOMINATOR,EM20))
          IF (NUMERATOR > ZERO .AND. DENOMINATOR > ZERO) RATIO(I) = ZERO
          IF (NUMERATOR < ZERO .AND. DENOMINATOR < ZERO) FAIL_ORT = 1000
          PLAMAX(I) = FINTER(IFUNC(1),RATIO(I),NPF,TF,DYDX)
          PLAMAX(I) = PLAMAX(I) * FAC
C             
          ! Case according to the ordinate
          IF (FAIL_ORT == 1) THEN
            ! Computation of DAMAGE
            DAMAGE(I)= MAX(EXX/(MAX(PLAMAX(I),EM20)),
     .                     EYY/(MAX(PLAMAX(I),EM20)),
     .                     EZZ/(MAX(PLAMAX(I),EM20)))
            ! If the maximum strain is reached by one of the strain tensor diagonal components
            IF (EXX >= PLAMAX(I) .OR. EYY >= PLAMAX(I) .OR. EZZ >= PLAMAX(I)) THEN
              TDELE(I)      = TIME
              NINDX1        = NINDX1 + 1
              INDX1(NINDX1) = I
              DAMAGE(I)     = ONE
              OFF(I)        = FOUR_OVER_5
            ENDIF
          ELSEIF (FAIL_ORT == 2) THEN
            DAMAGE(I) = EXX/(MAX(PLAMAX(I),EM20))
            IF (EXX >= PLAMAX(I) .AND. EXX > ZERO) THEN
              TDELE(I)      = TIME
              NINDX1        = NINDX1 + 1
              INDX1(NINDX1) = I
              DAMAGE(I)     = ONE
              OFF(I)        = FOUR_OVER_5
            ENDIF
          ELSEIF (FAIL_ORT == 3) THEN
            DAMAGE(I) = EYY/(MAX(PLAMAX(I),EM20))
            IF (EYY >= PLAMAX(I) .AND. EYY > ZERO) THEN
              TDELE(I)      = TIME
              NINDX1        = NINDX1 + 1
              INDX1(NINDX1) = I
              DAMAGE(I)     = ONE
              OFF(I)        = FOUR_OVER_5
            ENDIF
          ELSEIF (FAIL_ORT == 4) THEN
            DAMAGE(I) = EZZ/(MAX(PLAMAX(I),EM20))
            IF (EZZ >= PLAMAX(I) .AND. EZZ > ZERO) THEN
              TDELE(I)      = TIME
              NINDX1        = NINDX1 + 1
              INDX1(NINDX1) = I
              DAMAGE(I)     = ONE
              OFF(I)        = FOUR_OVER_5
            ENDIF
          ELSEIF (FAIL_ORT == 5) THEN
            DAMAGE(I) = E11/(MAX(PLAMAX(I),EM20))
            IF (E11 >= PLAMAX(I) .AND. E11 > ZERO) THEN
              TDELE(I)      = TIME
              NINDX1        = NINDX1 + 1
              INDX1(NINDX1) = I
              DAMAGE(I)     = ONE
              OFF(I)        = FOUR_OVER_5
            ENDIF
          ELSEIF (FAIL_ORT == 6) THEN
            E00 = ((EXX + EZZ)/TWO) + SQRT(((EXX - EZZ)/TWO)**2 + EZX**2)
            DAMAGE(I) = E00/(MAX(PLAMAX(I),EM20))
            IF (E00 >= PLAMAX(I) .AND. E00 > ZERO) THEN
              TDELE(I)      = TIME
              NINDX1        = NINDX1 + 1
              INDX1(NINDX1) = I
              DAMAGE(I)     = ONE
              OFF(I)        = FOUR_OVER_5
            ENDIF
          ELSEIF (FAIL_ORT == 7) THEN
            E00 = ((EXX + EYY)/TWO) + SQRT(((EXX - EYY)/TWO)**2 + EXY**2)
            DAMAGE(I) = E00/(MAX(PLAMAX(I),EM20))
            IF (E00 >= PLAMAX(I) .AND. E00 > ZERO) THEN
              TDELE(I)      = TIME
              NINDX1        = NINDX1 + 1
              INDX1(NINDX1) = I
              DAMAGE(I)     = ONE
              OFF(I)        = FOUR_OVER_5
            ENDIF
          ELSEIF (FAIL_ORT == 8) THEN
            E00 = ((EYY + EZZ)/TWO) + SQRT(((EYY - EZZ)/TWO)**2 + EYZ**2)
            DAMAGE(I) = E00/(MAX(PLAMAX(I),EM20))
            IF (E00 >= PLAMAX(I) .AND. E00 > ZERO) THEN
              TDELE(I)      = TIME
              NINDX1        = NINDX1 + 1
              INDX1(NINDX1) = I
              DAMAGE(I)     = ONE
              OFF(I)        = FOUR_OVER_5
            ENDIF
          ENDIF
        ENDIF
C
        !-----------------------------------------------------------------------------------
        ! In-plane compression failure
        !-----------------------------------------------------------------------------------
        IF (COMP_DIR > 0 .AND. DFMAX(I) < ONE) THEN
          IF (COMP_DIR == 1) THEN
            IF (EYY < (-ABS(MAX_COMP_STRAIN))) THEN
              IF (IDEL > 0) THEN
                OFF(I)   = FOUR_OVER_5
                TDELE(I) = TIME
              ENDIF
              DAMAGE(I)     = ONE
              NINDX2        = NINDX2 + 1
              INDX2(NINDX2) = I
              INDX3(NINDX2) = 2
            ELSEIF (EZZ < (-ABS(MAX_COMP_STRAIN)*RATIO_2)) THEN
              IF (IDEL > 0) THEN
                OFF(I)   = FOUR_OVER_5
                TDELE(I) = TIME
              ENDIF
              DAMAGE(I)     = ONE
              NINDX2        = NINDX2 + 1
              INDX2(NINDX2) = I
              INDX3(NINDX2) = 3
            ENDIF
          ELSEIF (COMP_DIR == 2) THEN
            IF (EZZ < (-ABS(MAX_COMP_STRAIN))) THEN
              IF (IDEL > 0) THEN
                OFF(I)   = FOUR_OVER_5
                TDELE(I) = TIME
              ENDIF
              DAMAGE(I)     = ONE
              NINDX2        = NINDX2 + 1
              INDX2(NINDX2) = I
              INDX3(NINDX2) = 3
            ELSEIF (EXX < (-ABS(MAX_COMP_STRAIN)*RATIO_2)) THEN
              IF (IDEL > 0) THEN
                OFF(I)   = FOUR_OVER_5
                TDELE(I) = TIME
              ENDIF
              DAMAGE(I)     = ONE
              NINDX2        = NINDX2 + 1
              INDX2(NINDX2) = I
              INDX3(NINDX2) = 1
            ENDIF
          ELSEIF (COMP_DIR == 3) THEN
            IF (EXX < (-ABS(MAX_COMP_STRAIN))) THEN
              IF (IDEL > 0) THEN
                OFF(I)   = FOUR_OVER_5
                TDELE(I) = TIME
              ENDIF
              DAMAGE(I)     = ONE
              NINDX2        = NINDX2 + 1
              INDX2(NINDX2) = I
              INDX3(NINDX2) = 1
            ELSEIF (EYY < (-ABS(MAX_COMP_STRAIN)*RATIO_2)) THEN
              IF (IDEL > 0) THEN
                OFF(I)   = FOUR_OVER_5
                TDELE(I) = TIME
              ENDIF
              DAMAGE(I)     = ONE
              NINDX2        = NINDX2 + 1
              INDX2(NINDX2) = I
              INDX3(NINDX2) = 2
            ENDIF
          ENDIF
        ENDIF
c
        ! Storing damage maximal value
        DAMAGE_SP(I) = DAMAGE(I)
        IF (DFMAX(I) <= ONE) DFMAX(I) = MAX(DFMAX(I),DAMAGE_SP(I))
        DFMAX(I) = MIN(DFMAX(I),ONE)
c
      ENDDO
c
      !=======================================================================
      ! - PRINTOUT ELEMENT FAILURE
      !=======================================================================
      ! Volumetric strain failure
      IF (NINDX1 > 0)THEN
        IDEL7NOK = 1
        DO J=1,NINDX1
          I = INDX1(J)
#include "lockon.inc"
            WRITE(IOUT, 1000) NGL(I),PLAMAX(I),RATIO(I),TIME
            WRITE(ISTDO,1100) NGL(I),PLAMAX(I),RATIO(I),TIME
#include "lockoff.inc"
        ENDDO
      ENDIF
      ! Compression limit failure
      IF (NINDX2 > 0)THEN
        IDEL7NOK = 1
        DO J=1,NINDX2
          I = INDX2(J)
          IF (MAX_COMP_STRAIN > ZERO) THEN
#include "lockon.inc"
            WRITE(IOUT, 2000) NGL(I),TIME
            WRITE(ISTDO,2100) NGL(I),TIME
#include "lockoff.inc"
          ELSE
#include "lockon.inc"
            WRITE(IOUT, 2002) NGL(I),TIME
            WRITE(ISTDO,2102) NGL(I),TIME
#include "lockoff.inc"
          ENDIF
          IF (INDX3(I) == 1) THEN
#include "lockon.inc"
            WRITE(IOUT, 2010) EPSXX(I)
            WRITE(ISTDO,2110) EPSXX(I)
#include "lockoff.inc"
          ENDIF
          IF (INDX3(I) == 2) THEN
#include "lockon.inc"
            WRITE(IOUT, 2020) EPSYY(I)
            WRITE(ISTDO,2120) EPSYY(I)
#include "lockoff.inc"
          ENDIF
          IF (INDX3(I) == 3) THEN
#include "lockon.inc"
            WRITE(IOUT, 2030) EPSZZ(I)
            WRITE(ISTDO,2130) EPSZZ(I)
#include "lockoff.inc"
          ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------
 1000 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER (SAHRAEI) EL :',I10,
     .          '  STRAIN LIMIT OF ',1PE10.3,'REACHED. AT STRAIN RATIO : ',1PE10.3,
     .          ' AT TIME :',1PE20.13)
 1100 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER (SAHRAEI) EL :',I10,
     .          '  STRAIN LIMIT OF ',1PE10.3,' REACHED. AT STRAIN RATIO : ',1PE10.3,
     .          ' AT TIME :',1PE20.13)
 2000 FORMAT(1X,'SOLID ELEMENT NUMBER (SAHRAEI) EL :',I10,
     .          ' MAX. IN-PLANE-COMPRESSION STRAIN REACHED AT TIME :',1PE20.13)
 2100 FORMAT(1X,'SOLID ELEMENT NUMBER (SAHRAEI) EL :',I10,
     .          ' MAX. IN-PLANE-COMPRESSION STRAIN REACHED AT TIME :',1PE20.13)
 2002 FORMAT(1X,'SOLID ELEMENT NUMBER (SAHRAEI) EL :',I10,
     .          ' MAX. IN-PLANE-COMPRESSION STRAIN REACHED AT TIME :',1PE20.13)
 2102 FORMAT(1X,'SOLID ELEMENT NUMBER (SAHRAEI) EL :',I10,
     .          ' MAX. IN-PLANE-COMPRESSION STRAIN REACHED AT TIME :',1PE20.13)
c
 2010 FORMAT(1X,'MAX PRESSURE STRAIN IN LOCAL X-DIRECTION REACHED : EPSXX= ',1PE20.13)
 2110 FORMAT(1X,'MAX PRESSURE STRAIN IN LOCAL X-DIRECTION REACHED : EPSXX= ',1PE20.13)
c
 2020 FORMAT(1X,'MAX PRESSURE STRAIN IN LOCAL Y-DIRECTION REACHED : EPSYY= ',1PE20.13)
 2120 FORMAT(1X,'MAX PRESSURE STRAIN IN LOCAL Y-DIRECTION REACHED : EPSYY= ',1PE20.13)
c
 2030 FORMAT(1X,'MAX PRESSURE STRAIN IN LOCAL Z-DIRECTION REACHED : EPSZZ= ',1PE20.13)
 2130 FORMAT(1X,'MAX PRESSURE STRAIN IN LOCAL Z-DIRECTION REACHED : EPSZZ= ',1PE20.13)
       END
