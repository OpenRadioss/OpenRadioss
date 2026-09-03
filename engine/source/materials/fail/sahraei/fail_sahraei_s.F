Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    fail_sahraei_s   ../engine/source/materials/fail/sahraei/fail_sahraei_s.F
!||--- called by ------------------------------------------------------
!||    mmain            ../engine/source/materials/mat_share/mmain.F90
!||    mulaw            ../engine/source/materials/mat_share/mulaw.F90
!||    mulaw8           ../engine/source/materials/mat_share/mulaw8.F90
!||    usermat_solid    ../engine/source/materials/mat_share/usermat_solid.F
!||--- calls      -----------------------------------------------------
!||    finter           ../engine/source/tools/curve/finter.F
!||====================================================================
      SUBROUTINE FAIL_SAHRAEI_S(FAIL  ,
     1     NEL    ,NUVAR  ,NVARTMP,UVAR   ,VARTMP  ,
     2     TIME   ,NGL    ,EPSXX  ,EPSYY  ,EPSZZ   ,
     3     EPSXY  ,EPSYZ  ,EPSZX  ,OFF    ,DFMAX   ,
     5     TDELE  ,DELTAX )
C-----------------------------------------------
C   MIT Wierzbicki Sahraei electric battery failure
C   /FAIL/SAHRAEI1 - tabulated rupture criteria for solids
C-----------------------------------------------
C   Modules
C-----------------------------------------------
      use fail_param_mod
      use table_mat_vinterp_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include    "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUVAR   |  1      | I | R | NUMBER OF FAILURE ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
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
#include "comlock.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL
      INTEGER ,INTENT(IN) :: NUVAR,NVARTMP
      INTEGER ,DIMENSION(NEL) :: NGL  
      INTEGER ,DIMENSION(NEL,NVARTMP) :: VARTMP
      my_real ,DIMENSION(NEL,NUVAR)   :: UVAR
      my_real :: TIME
      my_real :: EPSXX(NEL) ,EPSYY(NEL) ,EPSZZ(NEL) ,EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,DELTAX(NEL)
      type (fail_param_) ,intent(in) :: fail    !< failure model data structure
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real OFF(NEL), DFMAX(NEL),TDELE(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,II,J,NINDX1,NINDX2,NUM,DEN,ORDI,COMP_DIR,NINDXOFF,IDEL
        INTEGER ,DIMENSION(NEL) :: FAIL_ORT,INDX1,INDX2,INDX3,INDXOFF
        my_real :: MAX_COMP_STRAIN,RATIO_2,EL_REF,DAMAGE_SP(NEL)
        DOUBLE PRECISION :: I1,I2,I3,Q,R,R_INTER,PHI,E00,VOL_STRAIN_LIMIT,NUMERATOR,DENOMINATOR
        DOUBLE PRECISION ,DIMENSION(NEL) :: E11,E22,E33,EXX,EYY,EZZ,EXY,EZX,EYZ,VOL_STRAIN
        my_real ,DIMENSION(NEL) :: LAMBDA,DYDX,Y_LOC,PLAMAX,RATIO,DAMAGE,SIZE_FAC
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
  
      NUM              = FAIL%IPARAM(1)
      DEN              = FAIL%IPARAM(2)
      ORDI             = FAIL%IPARAM(3)
      COMP_DIR         = FAIL%IPARAM(4)
      IDEL             = FAIL%IPARAM(5)
      
      VOL_STRAIN_LIMIT = FAIL%UPARAM(1)
      MAX_COMP_STRAIN  = FAIL%UPARAM(2)
      RATIO_2          = FAIL%UPARAM(3)
      EL_REF           = FAIL%UPARAM(4)
c-----------------------------------------------      
      ! Initialization
      IF (FAIL%TABLE4D(2)%NOTABLE > 0) THEN 
        IF (UVAR(1,1) == ZERO) THEN
          LAMBDA(:) = DELTAX(:)/EL_REF
          call table_mat_vinterp(fail%table4d(2),nel,nel,vartmp,lambda,uvar(1:nel,2:2),dydx)
        ENDIF
        SIZE_FAC(1:nel) = UVAR(1:nel,1)  !  element size factor 
      ELSE
        SIZE_FAC(1:nel) = ONE
      ENDIF
c      
      DO I=1,NEL
        ! Checking failure flag
        IF (OFF(I) < ONE)   OFF(I) = OFF(I)*FOUR_OVER_5
        IF (OFF(I) < EM01) OFF(I) = ZERO
        ! Resetting damage variable
        DAMAGE(I)    = ZERO
        DAMAGE_SP(I) = ZERO
      END DO
c
      ! Initialization of counter
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
        I = INDXOFF(II)
C
        ! Initialization of FAIL_ORT
        FAIL_ORT(I) = ORDI
C
        ! Copying strain value
        EXX(I) = EPSXX(I)
        EYY(I) = EPSYY(I)
        EZZ(I) = EPSZZ(I)
        EXY(I) = HALF*EPSXY(I)
        EYZ(I) = HALF*EPSYZ(I)
        EZX(I) = HALF*EPSZX(I)
C
        ! Computation of strain tensor invariants (attention au formalisme : EPSXY = GAMMAXY = 2*DEFXY)
        I1 = EXX(I) + EYY(I) + EZZ(I)
        I2 = EXX(I)*EYY(I) + EYY(I)*EZZ(I) + EZZ(I)*EXX(I) - EXY(I)*EXY(I) - EZX(I)*EZX(I) - EYZ(I)*EYZ(I)
        I3 = EXX(I)*EYY(I)*EZZ(I) - EXX(I)*EYZ(I)*EYZ(I) - EYY(I)*EZX(I)*EZX(I) - EZZ(I)*EXY(I)*EXY(I) + TWO*EXY(I)*EZX(I)*EYZ(I)
        Q  = (THREE*I2 - I1*I1)/NINE
        R  = (TWO*I1*I1*I1-NINE*I1*I2+TWENTY7*I3)/FIFTY4
        R_INTER = MIN(R/SQRT(MAX(EM20,(-Q**3))),ONE)
        PHI     = ACOS(MAX(R_INTER,-ONE))
        ! Computation of principal strain
        E11(I) = TWO*SQRT(MAX(-Q,ZERO))*COS( PHI/THREE         ) + THIRD*I1
        E22(I) = TWO*SQRT(MAX(-Q,ZERO))*COS((PHI+TWO*PI)/THREE ) + THIRD*I1
        E33(I) = TWO*SQRT(MAX(-Q,ZERO))*COS((PHI+FOUR*PI)/THREE) + THIRD*I1
        ! Sorting principal strains
        IF (E11(I) < E22(I)) THEN 
          R_INTER = E11(I)
          E11(I)     = E22(I)
          E22(I)     = R_INTER
        ENDIF 
        IF (E22(I) < E33(I))THEN
          R_INTER = E22(I)
          E22(I)     = E33(I)
          E33(I)     = R_INTER
        ENDIF
        IF (E11(I) < E22(I))THEN
          R_INTER = E11(I)
          E11(I)     = E22(I)
          E22(I)     = R_INTER
        ENDIF     
c
        ! Computation of volumetric strain 
        VOL_STRAIN(I) = E11(I) + E22(I) + E33(I) + E11(I)*E22(I) + E11(I)*E33(I) + E22(I)*E33(I) + E11(I)*E22(I)*E33(I)
c
        !-----------------------------------------------------------------------------------
        ! Volumetric strain failure
        !-----------------------------------------------------------------------------------
        IF (ABS(VOL_STRAIN(I)) >= VOL_STRAIN_LIMIT) THEN
C                  
          ! Numerator flag for strain-ratio
          IF (NUM == 1) THEN
            NUMERATOR = EXX(I)
          ELSEIF (NUM == 2) THEN
            NUMERATOR = EYY(I)
          ELSEIF (NUM == 3) THEN
            NUMERATOR = EZZ(I)
          ELSEIF (NUM == 4) THEN
            NUMERATOR = E11(I)
          ELSEIF (NUM == 5) THEN
            NUMERATOR = E22(I)
          ELSE !IF (NUM == 6) THEN
            NUMERATOR = E33(I)
          ENDIF
C
          ! Denominator flag for strain-ratio     
          IF (DEN == 1) THEN 
            DENOMINATOR = ((EXX(I) + EZZ(I))/TWO) + SQRT(((EXX(I) - EZZ(I))/TWO)**2 + EZX(I)**2)            
          ELSEIF (DEN == 2) THEN
            DENOMINATOR = ((EXX(I) + EYY(I))/TWO) + SQRT(((EXX(I) - EYY(I))/TWO)**2 + EXY(I)**2)            
          ELSEIF (DEN == 3) THEN
            DENOMINATOR = ((EYY(I) + EZZ(I))/TWO) + SQRT(((EYY(I) - EZZ(I))/TWO)**2 + EYZ(I)**2)       
          ELSEIF (DEN == 4) THEN
            DENOMINATOR = E11(I)
          ELSEIF (DEN == 5) THEN
            DENOMINATOR = E22(I)
          ELSE !IF (DEN == 6) THEN
            DENOMINATOR = E33(I)      
          ENDIF
C
          ! Checking values
          RATIO(II) = ABS(NUMERATOR/MAX(DENOMINATOR,EM20))
          IF (NUMERATOR > ZERO .AND. DENOMINATOR > ZERO) RATIO(I)    = ZERO
          IF (NUMERATOR < ZERO .AND. DENOMINATOR < ZERO) FAIL_ORT(I) = 1000
        END IF
      END DO
        !
      call table_mat_vinterp(fail%table4d(1),nel,nindxoff,vartmp,ratio,y_loc,dydx)
        !
      do ii = 1,nindxoff
        i = indxoff(ii)
        plamax(i) = y_loc(ii) * size_fac(i)
      end do
        !
      DO II = 1,NINDXOFF                                           
        I = INDXOFF(II)
        IF (ABS(VOL_STRAIN(I)) >= VOL_STRAIN_LIMIT) THEN       
          ! Case according to the ordinate
          IF (FAIL_ORT(I) == 1) THEN
            ! Computation of DAMAGE 
            DAMAGE(I)= MAX(EXX(I)/(MAX(PLAMAX(I),EM20)),
     .                     EYY(I)/(MAX(PLAMAX(I),EM20)),
     .                     EZZ(I)/(MAX(PLAMAX(I),EM20)))
            ! If the maximum strain is reached by one of the strain tensor diagonal components
            IF (EXX(I) >= PLAMAX(I) .OR. EYY(I) >= PLAMAX(I) .OR. EZZ(I) >= PLAMAX(I)) THEN
              TDELE(I)      = TIME 
              NINDX1        = NINDX1 + 1  
              INDX1(NINDX1) = I
              DAMAGE(I)     = ONE
              OFF(I)        = FOUR_OVER_5
            ENDIF
          ELSEIF (FAIL_ORT(I) == 2) THEN
            DAMAGE(I) = EXX(I)/(MAX(PLAMAX(I),EM20))
            IF (EXX(I) >= PLAMAX(I) .AND. EXX(I) > ZERO) THEN
              TDELE(I)      = TIME 
              NINDX1        = NINDX1 + 1  
              INDX1(NINDX1) = I
              DAMAGE(I)     = ONE
              OFF(I)        = FOUR_OVER_5
            ENDIF            
          ELSEIF (FAIL_ORT(I) == 3) THEN
            DAMAGE(I) = EYY(I)/(MAX(PLAMAX(I),EM20))
            IF (EYY(I) >= PLAMAX(I) .AND. EYY(I) > ZERO) THEN
              TDELE(I)      = TIME 
              NINDX1        = NINDX1 + 1  
              INDX1(NINDX1) = I
              DAMAGE(I)     = ONE
              OFF(I)        = FOUR_OVER_5
            ENDIF            
          ELSEIF (FAIL_ORT(I) == 4) THEN
            DAMAGE(I) = EZZ(I)/(MAX(PLAMAX(I),EM20))
            IF (EZZ(I) >= PLAMAX(I) .AND. EZZ(I) > ZERO) THEN
              TDELE(I)      = TIME 
              NINDX1        = NINDX1 + 1  
              INDX1(NINDX1) = I
              DAMAGE(I)     = ONE
              OFF(I)        = FOUR_OVER_5
            ENDIF            
          ELSEIF (FAIL_ORT(I) == 5) THEN
            DAMAGE(I) = E11(I)/(MAX(PLAMAX(I),EM20))
            IF (E11(I) >= PLAMAX(I) .AND. E11(I) > ZERO) THEN
              TDELE(I)      = TIME 
              NINDX1        = NINDX1 + 1  
              INDX1(NINDX1) = I
              DAMAGE(I)     = ONE
              OFF(I)        = FOUR_OVER_5
            ENDIF            
          ELSEIF (FAIL_ORT(I) == 6) THEN
            E00 = ((EXX(I) + EZZ(I))/TWO) + SQRT(((EXX(I) - EZZ(I))/TWO)**2 + EZX(I)**2)
            DAMAGE(I) = E00/(MAX(PLAMAX(I),EM20))
            IF (E00 >= PLAMAX(I) .AND. E00 > ZERO) THEN
              TDELE(I)      = TIME 
              NINDX1        = NINDX1 + 1  
              INDX1(NINDX1) = I
              DAMAGE(I)     = ONE
              OFF(I)        = FOUR_OVER_5
            ENDIF            
          ELSEIF (FAIL_ORT(I) == 7) THEN
            E00 = ((EXX(I) + EYY(I))/TWO) + SQRT(((EXX(I) - EYY(I))/TWO)**2 + EXY(I)**2)
            DAMAGE(I) = E00/(MAX(PLAMAX(I),EM20))
            IF (E00 >= PLAMAX(I) .AND. E00 > ZERO) THEN
              TDELE(I)      = TIME 
              NINDX1        = NINDX1 + 1  
              INDX1(NINDX1) = I
              DAMAGE(I)     = ONE
              OFF(I)        = FOUR_OVER_5
            ENDIF            
          ELSEIF (FAIL_ORT(I) == 8) THEN
            E00 = ((EYY(I) + EZZ(I))/TWO) + SQRT(((EYY(I) - EZZ(I))/TWO)**2 + EYZ(I)**2)
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
            IF (EYY(I) < (-ABS(MAX_COMP_STRAIN))) THEN
              IF (IDEL > 0) THEN 
                OFF(I)   = FOUR_OVER_5
                TDELE(I) = TIME 
              ENDIF
              DAMAGE(I)     = ONE
              NINDX2        = NINDX2 + 1  
              INDX2(NINDX2) = I
              INDX3(NINDX2) = 2
            ELSEIF (EZZ(I) < (-ABS(MAX_COMP_STRAIN)*RATIO_2)) THEN
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
            IF (EZZ(I) < (-ABS(MAX_COMP_STRAIN))) THEN
              IF (IDEL > 0) THEN
                OFF(I)   = FOUR_OVER_5
                TDELE(I) = TIME 
              ENDIF
              DAMAGE(I)     = ONE
              NINDX2        = NINDX2 + 1  
              INDX2(NINDX2) = I
              INDX3(NINDX2) = 3
            ELSEIF (EXX(I) < (-ABS(MAX_COMP_STRAIN)*RATIO_2)) THEN
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
            IF (EXX(I) < (-ABS(MAX_COMP_STRAIN))) THEN
              IF (IDEL > 0) THEN
                OFF(I)   = FOUR_OVER_5
                TDELE(I) = TIME 
              ENDIF
              DAMAGE(I)     = ONE
              NINDX2        = NINDX2 + 1  
              INDX2(NINDX2) = I
              INDX3(NINDX2) = 1
            ELSEIF (EYY(I) < (-ABS(MAX_COMP_STRAIN)*RATIO_2)) THEN
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
