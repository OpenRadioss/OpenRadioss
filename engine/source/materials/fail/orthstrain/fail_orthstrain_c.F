Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    fail_orthstrain_c       ../engine/source/materials/fail/orthstrain/fail_orthstrain_c.F
!||--- called by ------------------------------------------------------
!||    mulawc                  ../engine/source/materials/mat_share/mulawc.F90
!||    usermat_shell           ../engine/source/materials/mat_share/usermat_shell.F
!||--- calls      -----------------------------------------------------
!||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
!||--- uses       -----------------------------------------------------
!||    fail_param_mod          ../common_source/modules/mat_elem/fail_param_mod.F90
!||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
      SUBROUTINE FAIL_ORTHSTRAIN_C(FAIL ,
     1     NEL      ,NUVAR    ,UVAR     ,NVARTMP  ,VARTMP   ,
     2     TIME     ,TIMESTEP ,IPG      ,ILAY     ,IPT      , 
     3     EPSXX    ,EPSYY    ,EPSXY    ,DMG_FLAG ,DMG_SCALE,   
     4     EPSPXX   ,EPSPYY   ,EPSPXY   ,ALDT     ,ISMSTR   ,
     5     LF_DAMMX ,NGL      ,
     6     OFF      ,OFFLY    ,FOFF     ,DFMAX    ,TDEL     )
! ---------------------------- ----------------------------------------------------------------------
!         Modules
! --------------------------------------------------------------------------------------------------
          use fail_param_mod
          use table_mat_vinterp_mod
C-----------------------------------------------
C    Orthotropic strain failure model
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "units_c.inc"
#include "comlock.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C---------+---------+---+---+--------------------------------------------
C EPSXX   | NEL     | F | R | STRAIN XX
C EPSYY   | NEL     | F | R | STRAIN YY
C ...     |         |   |   |
C EPSPXX  | NEL     | F | R | STRAIN RATE XX
C EPSPYY  | NEL     | F | R | STRAIN RATE YY
C ...     |         |   |   |
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C OFF     | NEL     | F | R | DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C FOFF    | NEL     | I |R/W| DELETED INTEGRATION POINT FLAG (=1 ON, =0 OFF)
C DFMAX   | NEL     | F |R/W| MAX DAMAGE FACTOR 
C TDEL    | NEL     | F | W | FAILURE TIME
C---------+---------+---+---+--------------------------------------------
C NGL                         ELEMENT ID
C IPG                         CURRENT GAUSS POINT (in plane)
C ILAY                        CURRENT LAYER
C IPT                         CURRENT INTEGRATION POINT IN THE LAYER
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NUVAR,NVARTMP,IPG,ILAY,IPT,ISMSTR
      INTEGER ,INTENT(IN) :: LF_DAMMX
      INTEGER ,DIMENSION(NEL) ,INTENT(IN) :: NGL
      my_real ,INTENT(IN) :: TIME,TIMESTEP
      my_real ,DIMENSION(NEL) ,INTENT(IN) :: OFF,EPSXX,EPSYY,EPSXY,
     .   EPSPXX,EPSPYY,EPSPXY,ALDT
      type (fail_param_) ,intent(in) :: fail    !< failure model data structure
      TARGET :: UVAR
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER ,INTENT(OUT) :: DMG_FLAG
      INTEGER ,DIMENSION(NEL) ,INTENT(INOUT) :: OFFLY,FOFF
      INTEGER ,DIMENSION(NEL,NVARTMP)  ,INTENT(INOUT) :: VARTMP
      my_real ,DIMENSION(NEL,LF_DAMMX) ,INTENT(INOUT) :: DFMAX
      my_real ,DIMENSION(NEL) ,INTENT(OUT)   :: TDEL,DMG_SCALE
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NINDX,NINDX_ON,MODE,XX,XY,YY,STRDEF,STRFLAG
      INTEGER ,DIMENSION(NEL)   :: INDX,INDX_ON,FAILI
      INTEGER ,DIMENSION(NEL,6) :: FMODE
      my_real ODAM(NEL,6),EPSP(NEL,6),
     .  SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),ESCAL(NEL)
      my_real ET1,ET2,ET12,EC1,EC2,EC12,ET3,EC3,EC23,ET23,EC31,
     .  ET31,ET1M,ET2M,ET12M,EC1M,EC2M,EC12M,ET3M,EC3M,EC23M,ET23M,EC31M,
     .  ET31M,DI,ALPHA,DAMXX,DAMYY,DAMXY,ODAMXX,ODAMYY,ODAMXY,THETA,C,S,
     .  EPSPREF,DEPS,DEPSM,FACT,REF_SIZ,EPSD1,EPSD2,
     .  EPST1,EPST2,EPST_A,EPST_B,FCUT
      my_real EPS_1,EPS_2
      my_real ,DIMENSION(NEL) :: EPS11,EPS22,EPS12,EPSP11,EPSP22,EPSP12,
     .                           EPSM1,EPSM2,EPSM3,EPSM4,EPSM5,EPSM6
      my_real ,DIMENSION(NEL) :: EPDOT,DYDX
      my_real ,DIMENSION(NEL) :: FRATE
      my_real ,DIMENSION(:), POINTER :: FSIZE
C=======================================================================
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
      DMG_FLAG = 1   ! flag to apply damage to nodal force contribution
!
      STRDEF  = FAIL%IPARAM(1)
!
      REF_SIZ = FAIL%UPARAM(1) 
      FCUT    = FAIL%UPARAM(2) 
      EPSPREF = FAIL%UPARAM(3) 
      ET1     = FAIL%UPARAM(4) 
      ET1M    = FAIL%UPARAM(5) 
      ET2     = FAIL%UPARAM(6) 
      ET2M    = FAIL%UPARAM(7) 
      EC1     = FAIL%UPARAM(8) 
      EC1M    = FAIL%UPARAM(9) 
      EC2     = FAIL%UPARAM(10)
      EC2M    = FAIL%UPARAM(11)
      ET12    = FAIL%UPARAM(12)
      ET12M   = FAIL%UPARAM(13)
      EC12    = FAIL%UPARAM(14)
      EC12M   = FAIL%UPARAM(15)
      ET3     = FAIL%UPARAM(16)
      ET3M    = FAIL%UPARAM(17)
      EC3     = FAIL%UPARAM(18)
      EC3M    = FAIL%UPARAM(19)
      ET23    = FAIL%UPARAM(20)
      ET23M   = FAIL%UPARAM(21)
      EC23    = FAIL%UPARAM(22)
      EC23M   = FAIL%UPARAM(23)
      ET31    = FAIL%UPARAM(24)
      ET31M   = FAIL%UPARAM(25)
      EC31    = FAIL%UPARAM(26)
      EC31M   = FAIL%UPARAM(27)
!
      ALPHA = MIN(TWO*PI*FCUT*TIMESTEP,ONE)
c
c     et1  = mode1 , et2  = mode 2, ec1 = mode 3, ec2 = mode 4, 
c     et12 = mode 5, ec12 = mode 6
c     xx = dir 1, yy = dir 2, xy = dir 3
c     xx => et1,  ec1
c     yy => et2,  ec2
c     xy => et12, ec12
c
c----------------------------------------------
c     strain transformation following input definition
c-------------------
      STRFLAG = 0
      IF (STRDEF == 2) THEN        ! failure defined as engineering strain
        IF (ISMSTR /= 1 .AND. ISMSTR /= 3 .AND. ISMSTR /= 11) THEN
          STRFLAG = 2
        END IF
      ELSE IF (STRDEF == 3) THEN   ! failure defined as true strain
        IF (ISMSTR == 1 .OR. ISMSTR == 3 .OR. ISMSTR == 11) THEN
          STRFLAG = 3
        END IF
      END IF     
c--------------------------
      SELECT CASE (STRFLAG)
c
        CASE (2)  !  transform true strain to engineering
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN
              THETA  = HALF*ATAN(EPSXY(I) / MAX((EPSXX(I)-EPSYY(I)),EM20))
              C = COS(THETA)
              S = SIN(THETA)
c             strain principal + transformation              
              EPST_A = HALF*(EPSXX(I)+EPSYY(I))
              EPST_B = SQRT( (HALF*(EPSXX(I)-EPSYY(I)))**2 + (HALF*EPSXY(I))**2)
              EPST1  = EPST_A + EPST_B
              EPST2  = EPST_A - EPST_B
              EPST1  = EXP(EPST1) - ONE
              EPST2  = EXP(EPST2) - ONE
              EPS11(I) = C*C*EPST1 + S*S*EPST2
              EPS22(I) = S*S*EPST1 + C*C*EPST2
              EPS12(I) = S*C*(EPST2 - EPST1)
c             strain rate
              EPST_A = HALF*(EPSPXX(I)+EPSPYY(I))
              EPST_B = SQRT( (HALF*(EPSPXX(I)-EPSPYY(I)))**2 + (HALF*EPSPXY(I))**2)
              EPSD1  = EPST_A + EPST_B
              EPSD2  = EPST_A - EPST_B
              EPSD1  = (ONE+EPST1)*EPSD1
              EPSD2  = (ONE+EPST2)*EPSD2
              EPSP11(I) = C*C*EPSD1 + S*S*EPSD2
              EPSP22(I) = S*S*EPSD1 + C*C*EPSD2
              EPSP12(I) = S*C*(EPSD2 - EPSD1) 
            END IF
          ENDDO
c
        CASE (3)  !  transform engineering to true strain
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN
              THETA  = HALF*ATAN(EPSXY(I) / MAX((EPSXX(I)-EPSYY(I)),EM20))
              C = COS(THETA)
              S = SIN(THETA)
c             strain principal + transformation              
              EPST_A = HALF*(EPSXX(I)+EPSYY(I))
              EPST_B = SQRT( (HALF*(EPSXX(I)-EPSYY(I)))**2 + (HALF*EPSXY(I))**2)
              EPST1  = EPST_A + EPST_B
              EPST2  = EPST_A - EPST_B
              EPST1  = LOG(ONE + EPST1)
              EPST2  = LOG(ONE + EPST2)
              EPS11(I) = C*C*EPST1 + S*S*EPST2
              EPS22(I) = S*S*EPST1 + C*C*EPST2
              EPS12(I) = S*C*(EPST2 - EPST1)
c             strain rate
              EPST_A = HALF*(EPSPXX(I)+EPSPYY(I))
              EPST_B = SQRT( (HALF*(EPSPXX(I)-EPSPYY(I)))**2 + (HALF*EPSPXY(I))**2)
              EPSD1  = EPST_A + EPST_B
              EPSD2  = EPST_A - EPST_B
              EPSD1  = (ONE/EXP(EPST1))*EPSD1
              EPSD2  = (ONE/EXP(EPST2))*EPSD2
              EPSP11(I) = C*C*EPSD1 + S*S*EPSD2
              EPSP22(I) = S*S*EPSD1 + C*C*EPSD2
              EPSP12(I) = S*C*(EPSD2 - EPSD1)
            END IF
          ENDDO
c
        CASE DEFAULT
           ! no transformation : failure strain measure is defined by Ismstr
           EPS11(1:NEL)  = EPSXX(1:NEL)
           EPS22(1:NEL)  = EPSYY(1:NEL)
           EPS12(1:NEL)  = HALF*EPSXY(1:NEL)
           EPSP11(1:NEL) = EPSPXX(1:NEL)
           EPSP22(1:NEL) = EPSPYY(1:NEL)
           EPSP12(1:NEL) = HALF*EPSPXY(1:NEL)
      END SELECT 
!----------------------------------------------
      !  Element size scale factor
      FSIZE => UVAR(1:NEL,13)
      IF (FSIZE(1) == ZERO) THEN
        IF (FAIL%TABLE4D(13)%NOTABLE > 0) THEN 
          ESCAL(1:NEL) = ALDT(1:NEL) / REF_SIZ
          call table_mat_vinterp(fail%table4d(13),nel,nel,vartmp(1:nel,13:13),escal,fsize,dydx)
        ELSE
          FSIZE(1:NEL) = ONE
        ENDIF
      ENDIF
!----------------------------------------------
      XX=1
      YY=2
      XY=3
c     variable initialisation, current time
      DO  J=1 ,6
        DO I=1, NEL
          ODAM(I,J) = MIN(DFMAX(I,1+J),ONE-EM3)
          EPSP(I,J) = UVAR(I,J)
        ENDDO
      ENDDO  
      DO I =1, NEL 
        SIGOXX(I) = UVAR(I,6+XX)
        SIGOYY(I) = UVAR(I,6+YY)
        SIGOXY(I) = UVAR(I,6+XY)
      ENDDO
c      
c     strain rate filtering
      DO I=1,NEL
        EPSP(I,XX) = ALPHA * ABS(EPSP11(I)) + (ONE-ALPHA)*EPSP(I,XX) 
        EPSP(I,YY) = ALPHA * ABS(EPSP22(I)) + (ONE-ALPHA)*EPSP(I,YY) 
        EPSP(I,XY) = ALPHA * ABS(EPSP12(I)) + (ONE-ALPHA)*EPSP(I,XY)
      ENDDO 
!---------------------------------------------------------------------
      ! list of active elements
!---------------------------------------------------------------------
      NINDX = 0  
      nindx_on = 0  
      do i=1,nel
        if (off(i) == one .and. offly(i) == one .and. foff(i) == 1) then
          nindx_on = nindx_on + 1
          indx_on(nindx_on) = i  
        end if
      end do
!---------------------------------------------------------------------
      ! failure and damage calculation in each mode (dir)
!---------------------------------------------------------------------
      faili(1:nel)     = 0
      fmode(1:nel,1:6) = 0
!
      mode = 1   ! traction xx
      if (fail%table4d(mode)%notable > 0) then 
        epdot(1:nel) = abs(epsp(1:nel,xx)) / epspref
        call table_mat_vinterp(fail%table4d(mode),nel,nel,vartmp(1:nel,mode:mode),epdot,frate,dydx)
      else
        frate(:) = one
      end if
      do j = 1,nindx_on
        i = indx_on(j)
        FRATE(I) = FRATE(I)*FSIZE(I)
        EPS_2 = FRATE(I) * ET1M
        EPS_1 = FRATE(I) * ET1
        DEPS  = EPS11(I) - EPS_1 
        IF (DEPS > ZERO .AND. DFMAX(I,1+MODE) < ONE) THEN
          DEPSM = EPS_2 - EPS_1
          FACT  = EPS_2 / EPS11(I)
          DI    = FACT * DEPS / DEPSM 
          DFMAX(I,1+MODE) = MAX(DFMAX(I,1+MODE), DI)
          IF (DFMAX(I,1+MODE) >= ONE) THEN
            FAILI(I) = 1
            FMODE(I,MODE) = 1
            DFMAX(I,1+MODE) = ONE
            EPSM1(I) = EPS_2
          ENDIF
        ENDIF
      end do
!
      mode = 2   ! traction yy
      if (fail%table4d(mode)%notable > 0) then 
        epdot(1:nel) = abs(epsp(1:nel,yy)) / epspref
        call table_mat_vinterp(fail%table4d(mode),nel,nel,vartmp(1:nel,mode:mode),epdot,frate,dydx)
      else
        frate(:) = one
      end if
      do j = 1,nindx_on
        i = indx_on(j)
        FRATE(I) = FRATE(I)*FSIZE(I)
        EPS_2 = FRATE(I) * ET2M
        EPS_1 = FRATE(I) * ET2
        DEPS  = MAX(EPS22(I) - EPS_1, ZERO)
        IF (DEPS > ZERO .AND. DFMAX(I,1+MODE) < ONE) THEN
          DEPSM = EPS_2 - EPS_1
          FACT  = EPS_2 / EPS22(I)
          DI    = FACT * DEPS / DEPSM 
          DFMAX(I,1+MODE) = MAX(DFMAX(I,1+MODE), DI)
          IF (DFMAX(I,1+MODE) >= ONE) THEN
            FAILI(I) = 1
            FMODE(I,MODE) = 1
            DFMAX(I,1+MODE) = ONE
            EPSM2(I) = EPS_2
          ENDIF
        ENDIF
      end do
!
      mode = 3   ! traction xy
      if (fail%table4d(mode)%notable > 0) then 
        epdot(1:nel) = abs(epsp(1:nel,xy)) / epspref
        call table_mat_vinterp(fail%table4d(mode),nel,nel,vartmp(1:nel,mode:mode),epdot,frate,dydx)
      else
        frate(:) = one
      end if
      do j = 1,nindx_on
        i = indx_on(j)
        FRATE(I) = FRATE(I)*FSIZE(I)
        EPS_2 = FRATE(I) * ET12M
        EPS_1 = FRATE(I) * ET12
        DEPS  = MAX(EPS12(I) - EPS_1, ZERO)
        IF (DEPS > ZERO .AND. DFMAX(I,1+MODE) < ONE) THEN
          DEPSM = EPS_2 - EPS_1
          FACT  = EPS_2 / EPS12(I)
          DI    = FACT * DEPS / DEPSM
          DFMAX(I,1+MODE) = MAX(DFMAX(I,1+MODE), DI)
          IF (DFMAX(I,1+MODE) >= ONE) THEN
            FAILI(I) = 1
            FMODE(I,MODE) = 1
            DFMAX(I,1+MODE) = ONE
            EPSM3(I) = EPS_2
          ENDIF
        ENDIF
      end do
!
      mode = 4   ! compression xx
      if (fail%table4d(mode)%notable > 0) then 
        epdot(1:nel) = abs(epsp(1:nel,xx)) / epspref
        call table_mat_vinterp(fail%table4d(mode),nel,nel,vartmp(1:nel,mode:mode),epdot,frate,dydx)
      else
        frate(:) = one
      end if
      do j = 1,nindx_on
        i = indx_on(j)
        FRATE(I) = FRATE(I)*FSIZE(I)
        EPS_2 = FRATE(I) * EC1M
        EPS_1 = FRATE(I) * EC1
        DEPS  = -EPS11(I) - EPS_1
        IF (DEPS > ZERO .AND. DFMAX(I,1+MODE) < ONE) THEN
          DEPSM = EPS_2 - EPS_1
          FACT = EPS_2 / ABS(EPS11(I))
          DI = FACT * DEPS / DEPSM
          DFMAX(I,1+MODE) = MAX(DFMAX(I,1+MODE), DI)
          IF (DFMAX(I,1+MODE) >= ONE) THEN
            FAILI(I) = 1
            FMODE(I,MODE) = 1
            DFMAX(I,1+MODE) = ONE
            EPSM4(I) = -EPS_2
          ENDIF
        ENDIF
      end do
!
      mode = 5   ! compression yy
      if (fail%table4d(mode)%notable > 0) then 
        epdot(1:nel) = abs(epsp(1:nel,yy)) / epspref
        call table_mat_vinterp(fail%table4d(mode),nel,nel,vartmp(1:nel,mode:mode),epdot,frate,dydx)
      else
        frate(:) = one
      end if
      do j = 1,nindx_on
        i = indx_on(j)
        FRATE(I) = FRATE(I)*FSIZE(I)
        EPS_2 = FRATE(I) * EC2M
        EPS_1 = FRATE(I) * EC2
        DEPS  = MAX(-EPS22(I) - EPS_1, ZERO)
        IF (DEPS > ZERO .AND. DFMAX(I,1+MODE) < ONE) THEN
          DEPSM = EPS_2 - EPS_1
          FACT  = EPS_2 / ABS(EPS22(I))
          DI = FACT * DEPS / DEPSM
          DFMAX(I,1+MODE) = MAX(DFMAX(I,1+MODE), DI)
          IF (DFMAX(I,1+MODE) >= ONE) THEN
            FAILI(I) = 1
            FMODE(I,MODE) = 1
            DFMAX(I,1+MODE) = ONE
            EPSM5(I) = -EPS_2
          ENDIF
        ENDIF
      end do
!
      mode = 6   ! compression xy
      if (fail%table4d(mode)%notable > 0) then 
        epdot(1:nel) = abs(epsp(1:nel,xy)) / epspref
        call table_mat_vinterp(fail%table4d(mode),nel,nel,vartmp(1:nel,mode:mode),epdot,frate,dydx)
      else
        frate(:) = one
      end if
      do j = 1,nindx_on
        i = indx_on(j)
        FRATE(I) = FRATE(I)*FSIZE(I)
        EPS_2 = FRATE(I) * EC12M
        EPS_1 = FRATE(I) * EC12
        DEPS  = MAX(-EPS12(I) - EPS_1, ZERO)
        IF (DEPS > ZERO .AND. DFMAX(I,1+MODE) < ONE) THEN
          DEPSM = EPS_2 - EPS_1
          FACT  = EPS_2 / ABS(EPS12(I))
          DI = FACT * DEPS / DEPSM
          DFMAX(I,1+MODE) = MAX(DFMAX(I,1+MODE), DI)
          IF (DFMAX(I,1+MODE) >= ONE) THEN
            FAILI(I) = 1
            FMODE(I,MODE) = 1
            DFMAX(I,1+MODE) = ONE
            EPSM6(I) = -EPS_2
          ENDIF
        ENDIF
      end do
!---------------------------------------
      DO I = 1,NEL
        IF (FAILI(I) == 1) THEN
          FOFF(I) = 0
          TDEL(I) = TIME 
          NINDX   = NINDX + 1  
          INDX(NINDX) = I
        ENDIF
      ENDDO  
!---------------------------------------
      IF (NINDX > 0) THEN 
        DO J=1,NINDX    
          I = INDX(J)
#include  "lockon.inc"
          WRITE(IOUT, 1000) NGL(I),IPG,ILAY,IPT
          WRITE(ISTDO,1000) NGL(I),IPG,ILAY,IPT
          IF (FMODE(I,1)==1) WRITE(IOUT, 2000) '1 - TRACTION XX',EPS11(I),EPSM1(I),EPSP(I,XX)
          IF (FMODE(I,2)==1) WRITE(IOUT, 2000) '2 - TRACTION YY',EPS22(I),EPSM2(I),EPSP(I,YY)
          IF (FMODE(I,3)==1) WRITE(IOUT, 2000) '3 - POSITIVE SHEAR XY',EPS12(I),EPSM3(I),EPSP(I,XY)
          IF (FMODE(I,4)==1) WRITE(IOUT, 2000) '4 - COMPRESSION XX',EPS11(I),EPSM4(I),EPSP(I,XX)
          IF (FMODE(I,5)==1) WRITE(IOUT, 2000) '5 - COMPRESSION YY',EPS22(I),EPSM5(I),EPSP(I,YY)
          IF (FMODE(I,6)==1) WRITE(IOUT, 2000) '6 - NEGATIVE SHEAR XY',EPS12(I),EPSM6(I),EPSP(I,XY)
#include  "lockoff.inc"
        END DO
      END IF              
c-----------------------------------------------------------------------
c     damage applied to nodal force contribution
c     damage / direction is equal to the max of damages par mode in the same direction
c-----------------------------------------------------------------------
      DO I = 1,NEL
        DAMXX = MAX(DFMAX(I,2),DFMAX(I,5))
        DAMYY = MAX(DFMAX(I,3),DFMAX(I,6))
        DAMXY = MAX(DFMAX(I,4),DFMAX(I,7))
        DFMAX(I,1) = MAX(DFMAX(I,1), DAMXX)
        DFMAX(I,1) = MAX(DFMAX(I,1), DAMYY)
        DFMAX(I,1) = MAX(DFMAX(I,1), DAMXY)
        DFMAX(I,1) = MIN(ONE,DFMAX(I,1))
        DMG_SCALE(I) = ONE - DFMAX(I,1)
      ENDDO
c     Update state variables
      DO  J=1 ,6
        DO I=1, NEL
          UVAR(I,J) = EPSP(I,J)
        ENDDO
      ENDDO  
c-----------------------------------------------------------------------
 1000 FORMAT(1X,'FAILURE (ORTHSTR) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3)
 2000 FORMAT(1X,'MODE',1X,A,', STRAIN',1PE12.4,', CRITICAL VALUE',1PE12.4,
     .       ', STRAIN RATE',1PE12.4)
c-----------------------------------------------------------------------
      RETURN          
      END
