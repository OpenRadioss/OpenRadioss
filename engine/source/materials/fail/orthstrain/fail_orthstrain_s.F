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
!||    fail_orthstrain         ../engine/source/materials/fail/orthstrain/fail_orthstrain_s.F
!||--- called by ------------------------------------------------------
!||    mmain                   ../engine/source/materials/mat_share/mmain.F90
!||    mulaw                   ../engine/source/materials/mat_share/mulaw.F90
!||    usermat_solid           ../engine/source/materials/mat_share/usermat_solid.F
!||--- calls      -----------------------------------------------------
!||    jacobiew                ../engine/source/materials/mat/mat033/sigeps33.F
!||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
!||--- uses       -----------------------------------------------------
!||    fail_param_mod          ../common_source/modules/mat_elem/fail_param_mod.F90
!||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
              SUBROUTINE FAIL_ORTHSTRAIN (FAIL ,
     1        NEL      ,NUVAR  ,TIME   ,TIMESTEP,ISMSTR ,
     2        EPSPXX   ,EPSPYY ,EPSPZZ ,EPSPXY  ,EPSPYZ ,EPSPZX   , 
     3        EPSXX    ,EPSYY  ,EPSZZ  ,EPSXY   ,EPSYZ  ,EPSZX    ,
     4        SIGNXX   ,SIGNYY ,SIGNZZ ,SIGNXY  ,SIGNYZ ,SIGNZX   ,
     5        UVAR     ,OFF    ,IPT    ,NGL     ,DFMAX  ,TDEL     ,
     6        UELR     ,NPT    ,DELTAX ,LF_DAMMX,NVARTMP,VARTMP   )
! ---------------------------- ----------------------------------------------------------------------
!         Modules
! --------------------------------------------------------------------------------------------------
          use fail_param_mod
          use table_mat_vinterp_mod
C-----------------------------------------------
c    Orthotropic strain failure model
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include  "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include  "units_c.inc"
#include  "comlock.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C EPSPXX  | NEL     | F | R | STRAIN RATE XX
C EPSPYY  | NEL     | F | R | STRAIN RATE YY
C ...     |         |   |   |
C EPSXX   | NEL     | F | R | STRAIN XX
C EPSYY   | NEL     | F | R | STRAIN YY
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C   I N P U T   A r g u m e n t s
C--------------------------------------------
      INTEGER :: NEL,NUVAR,IPT,NPT,ISMSTR,LF_DAMMX
      INTEGER ,INTENT(IN) :: NVARTMP
      INTEGER ,DIMENSION(NEL) :: NGL
      my_real :: TIME,TIMESTEP
      my_real ,DIMENSION(NEL) ,INTENT(IN) :: EPSXX,EPSYY,EPSZZ,
     .  EPSXY,EPSYZ,EPSZX,EPSPXX,EPSPYY,EPSPZZ,EPSPXY,EPSPYZ,EPSPZX
      my_real ,DIMENSION(NEL)     :: UELR,DELTAX,
     .  SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX
      type (fail_param_) ,intent(in) :: fail    !< failure model data structure
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER ,DIMENSION(NEL,NVARTMP) ,INTENT(INOUT) :: VARTMP
      my_real UVAR(NEL,NUVAR),OFF(NEL),TDEL(NEL)
      my_real, DIMENSION(NEL,LF_DAMMX), INTENT(INOUT) :: DFMAX
      TARGET :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,K,L,M
      INTEGER :: NINDX,NINDX_ON,STRDEF,STRFLAG
      INTEGER :: MODE,XX,XY,YY,ZZ,YZ,ZX
      INTEGER :: M11T,M11C,M22T,M22C,M33T,M33C,M12T,M12C,M23T,M23C,M31T,M31C
      INTEGER :: NROT
      INTEGER ,DIMENSION(NEL)    :: INDX,INDX_ON,FAILI
      INTEGER ,DIMENSION(NEL,12) :: FMODE
      my_real  ODAM(NEL,12),EPSP(NEL,6),SIGO(NEL,6),ESCAL(NEL),DYDX(NEL)
      my_real ,DIMENSION(NEL)  :: EPS11,EPS22,EPS33,EPS12,EPS23,EPS31,
     .     EPSP11,EPSP22,EPSP33,EPSP12,EPSP23,EPSP31,
     .     EPSM1,EPSM2,EPSM3,EPSM4,EPSM5,EPSM6,
     .     EPSM7,EPSM8,EPSM9,EPSM10,EPSM11,EPSM12
      my_real  EPSPREF,FCUT,ALPHA,DEPSM,DEPS,FACT,
     .    ET1,ET2,ET12,EC1,EC2,EC12,ET3,EC3,EC23,ET23,EC31,ET31,
     .    ET1M,ET2M,ET12M,EC1M,EC2M,EC12M,ET3M,EC3M,EC23M,ET23M,EC31M,ET31M,
     .    DI,DAMXX,DAMYY,DAMXY,DAMZZ,DAMYZ,DAMZX, 
     .    ODAMXX,ODAMYY,ODAMXY,ODAMZZ,ODAMYZ,ODAMZX,REF_SIZ
      my_real  EPS_1,EPS_2
      my_real  EPSTENS(3,3),EPSTENS2(3,3),EPS_PR(3),VECT_PR(3,3),
     .         EPSPTENS(3,3),EPSPTENS2(3,3),EPSP_PR(3)
      my_real ,DIMENSION(NEL) :: FRATE,EPDOT
      my_real ,DIMENSION(:), POINTER :: FSIZE
C=======================================================================
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
c----------------------------------------------
c     strain transformation following input definition
c-------------------
      STRFLAG = 0
      IF (STRDEF == 2) THEN        ! failure defined as engineering strain
        IF (ISMSTR /=1 .AND. ISMSTR /=3 .AND. ISMSTR /= 11) THEN
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
              ! -> Fill the strain tensor 3x3
              EPSTENS(1,1) = EPSXX(I)
              EPSTENS(2,2) = EPSYY(I)
              EPSTENS(3,3) = EPSZZ(I)
              EPSTENS(1,2) = HALF*EPSXY(I)
              EPSTENS(2,3) = HALF*EPSYZ(I)
              EPSTENS(1,3) = HALF*EPSZX(I)
              EPSTENS(2,1) = EPSTENS(1,2)
              EPSTENS(3,1) = EPSTENS(1,3)
              EPSTENS(3,2) = EPSTENS(2,3)
              ! -> Compute principal strains and vectors
              CALL JACOBIEW(EPSTENS,3,EPS_PR,VECT_PR,NROT)
              ! -> Transform true principal strains to engineering principal strains
              EPS_PR(1) = EXP(EPS_PR(1)) - ONE
              EPS_PR(2) = EXP(EPS_PR(2)) - ONE
              EPS_PR(3) = EXP(EPS_PR(3)) - ONE 
              ! -> Recover engineering strain tensor 
              EPSTENS(1:3,1:3) = ZERO
              EPSTENS(1,1) = EPS_PR(1)
              EPSTENS(2,2) = EPS_PR(2)
              EPSTENS(3,3) = EPS_PR(3)
              !  --> Principal vector*Principal strains*Transpose(Principal vector)
              DO K = 1,3
                DO L = 1,3
                  EPSTENS2(K,L) = ZERO
                  DO M = 1,3
                    EPSTENS2(K,L) = EPSTENS2(K,L) + EPSTENS(K,M)*VECT_PR(L,M)
                  ENDDO
                ENDDO
              ENDDO
              DO K = 1,3
                DO L = 1,3
                  EPSTENS(K,L) = ZERO
                  DO M = 1,3
                    EPSTENS(K,L) = EPSTENS(K,L) + VECT_PR(K,M)*EPSTENS2(M,L)
                  ENDDO
                ENDDO
              ENDDO
              ! -> Save engineering strains
              EPS11(I)  = EPSTENS(1,1)
              EPS22(I)  = EPSTENS(2,2)
              EPS33(I)  = EPSTENS(3,3)
              EPS12(I)  = EPSTENS(1,2)
              EPS23(I)  = EPSTENS(2,3)
              EPS31(I)  = EPSTENS(3,1)
              ! -> Fill the strain rate tensor
              EPSPTENS(1,1) = EPSPXX(I)
              EPSPTENS(2,2) = EPSPYY(I)
              EPSPTENS(3,3) = EPSPZZ(I)
              EPSPTENS(1,2) = HALF*EPSPXY(I)
              EPSPTENS(2,3) = HALF*EPSPYZ(I)
              EPSPTENS(1,3) = HALF*EPSPZX(I)
              EPSPTENS(2,1) = EPSPTENS(1,2)
              EPSPTENS(3,1) = EPSPTENS(1,3)
              EPSPTENS(3,2) = EPSPTENS(2,3)
              ! -> Compute principal strain rates and vectors
              CALL JACOBIEW(EPSPTENS,3,EPSP_PR,VECT_PR,NROT)
              ! -> Transform true principal strain rates to engineering principal strain rates
              EPSP_PR(1) = (EPS_PR(1)+ONE)*EPSP_PR(1)
              EPSP_PR(2) = (EPS_PR(2)+ONE)*EPSP_PR(2)
              EPSP_PR(3) = (EPS_PR(3)+ONE)*EPSP_PR(3)
              ! -> Recover engineering strain rate tensor 
              EPSPTENS(1:3,1:3) = ZERO
              EPSPTENS(1,1) = EPSP_PR(1)
              EPSPTENS(2,2) = EPSP_PR(2)
              EPSPTENS(3,3) = EPSP_PR(3)
              !  --> Principal vector*Principal strains*Transpose(Principal vector)
              DO K = 1,3
                DO L = 1,3
                  EPSPTENS2(K,L) = ZERO
                  DO M = 1,3
                    EPSPTENS2(K,L) = EPSPTENS2(K,L) + EPSPTENS(K,M)*VECT_PR(L,M)
                  ENDDO
                ENDDO
              ENDDO
              DO K = 1,3
                DO L = 1,3
                  EPSPTENS(K,L) = ZERO
                  DO M = 1,3
                    EPSPTENS(K,L) = EPSPTENS(K,L) + VECT_PR(K,M)*EPSPTENS2(M,L)
                  ENDDO
                ENDDO
              ENDDO
              ! -> Save engineering strain rates
              EPSP11(I) = EPSPTENS(1,1)
              EPSP22(I) = EPSPTENS(2,2)
              EPSP33(I) = EPSPTENS(3,3)
              EPSP12(I) = EPSPTENS(1,2)
              EPSP23(I) = EPSPTENS(2,3)
              EPSP31(I) = EPSPTENS(3,1)
            END IF
          ENDDO
c
        CASE (3)  !  transform engineering to true strain
          DO I=1,NEL
            IF (OFF(I) == ONE ) THEN
              ! -> Fill the strain tensor 3x3
              EPSTENS(1,1) = EPSXX(I)
              EPSTENS(2,2) = EPSYY(I)
              EPSTENS(3,3) = EPSZZ(I)
              EPSTENS(1,2) = HALF*EPSXY(I)
              EPSTENS(2,3) = HALF*EPSYZ(I)
              EPSTENS(1,3) = HALF*EPSZX(I)
              EPSTENS(2,1) = EPSTENS(1,2)
              EPSTENS(3,1) = EPSTENS(1,3)
              EPSTENS(3,2) = EPSTENS(2,3)
              ! -> Compute principal strains and vectors
              CALL JACOBIEW(EPSTENS,3,EPS_PR,VECT_PR,NROT)
              ! -> Transform engineering principal strains to true principal strains
              EPS_PR(1) = LOG(MAX(ONE + EPS_PR(1),EM20))
              EPS_PR(2) = LOG(MAX(ONE + EPS_PR(2),EM20))
              EPS_PR(3) = LOG(MAX(ONE + EPS_PR(3),EM20)) 
              ! -> Recover true strain tensor 
              EPSTENS(1:3,1:3) = ZERO
              EPSTENS(1,1) = EPS_PR(1)
              EPSTENS(2,2) = EPS_PR(2)
              EPSTENS(3,3) = EPS_PR(3)
              !  --> Principal vector*Principal strains*Transpose(Principal vector)
              DO K = 1,3
                DO L = 1,3
                  EPSTENS2(K,L) = ZERO
                  DO M = 1,3
                    EPSTENS2(K,L) = EPSTENS2(K,L) + EPSTENS(K,M)*VECT_PR(L,M)
                  ENDDO
                ENDDO
              ENDDO
              DO K = 1,3
                DO L = 1,3
                  EPSTENS(K,L) = ZERO
                  DO M = 1,3
                    EPSTENS(K,L) = EPSTENS(K,L) + VECT_PR(K,M)*EPSTENS2(M,L)
                  ENDDO
                ENDDO
              ENDDO
              ! -> Save true strains
              EPS11(I)  = EPSTENS(1,1)
              EPS22(I)  = EPSTENS(2,2)
              EPS33(I)  = EPSTENS(3,3)
              EPS12(I)  = EPSTENS(1,2)
              EPS23(I)  = EPSTENS(2,3)
              EPS31(I)  = EPSTENS(3,1)
              ! -> Fill the strain rate tensor
              EPSPTENS(1,1) = EPSPXX(I)
              EPSPTENS(2,2) = EPSPYY(I)
              EPSPTENS(3,3) = EPSPZZ(I)
              EPSPTENS(1,2) = HALF*EPSPXY(I)
              EPSPTENS(2,3) = HALF*EPSPYZ(I)
              EPSPTENS(1,3) = HALF*EPSPZX(I)
              EPSPTENS(2,1) = EPSPTENS(1,2)
              EPSPTENS(3,1) = EPSPTENS(1,3)
              EPSPTENS(3,2) = EPSPTENS(2,3)
              ! -> Compute principal strain rates and vectors
              CALL JACOBIEW(EPSPTENS,3,EPSP_PR,VECT_PR,NROT)
              ! -> Transform engineering principal strain rates to true principal strain rates
              EPSP_PR(1) = (ONE/(EXP(EPS_PR(1))))*EPSP_PR(1)
              EPSP_PR(2) = (ONE/(EXP(EPS_PR(2))))*EPSP_PR(2)
              EPSP_PR(3) = (ONE/(EXP(EPS_PR(3))))*EPSP_PR(3)
              ! -> Recover true strain rates tensor 
              EPSPTENS(1:3,1:3) = ZERO
              EPSPTENS(1,1) = EPSP_PR(1)
              EPSPTENS(2,2) = EPSP_PR(2)
              EPSPTENS(3,3) = EPSP_PR(3)
              !  --> Principal vector*Principal strains*Transpose(Principal vector)
              DO K = 1,3
                DO L = 1,3
                  EPSPTENS2(K,L) = ZERO
                  DO M = 1,3
                    EPSPTENS2(K,L) = EPSPTENS2(K,L) + EPSPTENS(K,M)*VECT_PR(L,M)
                  ENDDO
                ENDDO
              ENDDO
              DO K = 1,3
                DO L = 1,3
                  EPSPTENS(K,L) = ZERO
                  DO M = 1,3
                    EPSPTENS(K,L) = EPSPTENS(K,L) + VECT_PR(K,M)*EPSPTENS2(M,L)
                  ENDDO
                ENDDO
              ENDDO
              ! -> Save true strain rates
              EPSP11(I) = EPSPTENS(1,1)
              EPSP22(I) = EPSPTENS(2,2)
              EPSP33(I) = EPSPTENS(3,3)
              EPSP12(I) = EPSPTENS(1,2)
              EPSP23(I) = EPSPTENS(2,3)
              EPSP31(I) = EPSPTENS(3,1)
            END IF
          ENDDO
c
        CASE DEFAULT
          ! no transformation : failure strain measure is defined by Ismstr
          EPS11(1:NEL)  = EPSXX(1:NEL)
          EPS22(1:NEL)  = EPSYY(1:NEL)
          EPS33(1:NEL)  = EPSZZ(1:NEL)
          EPS12(1:NEL)  = HALF*EPSXY(1:NEL)
          EPS23(1:NEL)  = HALF*EPSYZ(1:NEL)
          EPS31(1:NEL)  = HALF*EPSZX(1:NEL)
          EPSP11(1:NEL) = EPSPXX(1:NEL)
          EPSP22(1:NEL) = EPSPYY(1:NEL)
          EPSP33(1:NEL) = EPSPZZ(1:NEL)
          EPSP12(1:NEL) = HALF*EPSPXY(1:NEL)
          EPSP23(1:NEL) = HALF*EPSPYZ(1:NEL)
          EPSP31(1:NEL) = HALF*EPSPZX(1:NEL)
      END SELECT
!----------------------------------------------
      !  Element size scale factor
      FSIZE => UVAR(1:NEL,13)
      IF (FSIZE(1) == ZERO) THEN
        IF (FAIL%TABLE4D(13)%NOTABLE > 0) THEN 
          ESCAL(1:NEL) = DELTAX(1:NEL) / REF_SIZ
          call table_mat_vinterp(fail%table4d(13),nel,nel,vartmp(1:nel,13:13),escal,fsize,dydx)
        ELSE
          FSIZE(1:NEL) = ONE
        ENDIF
      ENDIF
!----------------------------------------------
      XX=1
      YY=2
      ZZ=3
      XY=4
      YZ=5
      ZX=6
      M11T = 1
      M22T = 2 
      M12T = 3
      M11C = 4
      M22C = 5
      M12C = 6
      M33T = 7
      M33C = 8
      M23T = 9
      M23C = 10
      M31T = 11
      M31C = 12
c     current variable initialisation
      DO I=1, NEL
        IF (OFF(I) == ONE) THEN 
          DO J=1 ,12
            ODAM(I,J) = MIN(DFMAX(I,J+1),ONE-EM3)
          ENDDO
          DO J=1 ,6
            SIGO(I,J) = UVAR(I,J)
            EPSP(I,J) = UVAR(I,J+6)
          ENDDO
        ENDIF
      ENDDO  
c     
c strain rate filtering
      DO I=1,NEL
        IF (OFF(I) == ONE) THEN 
          EPSP(I,XX) = ALPHA * ABS(EPSP11(I)) + (ONE-ALPHA)*EPSP(I,XX) 
          EPSP(I,YY) = ALPHA * ABS(EPSP22(I)) + (ONE-ALPHA)*EPSP(I,YY) 
          EPSP(I,ZZ) = ALPHA * ABS(EPSP33(I)) + (ONE-ALPHA)*EPSP(I,ZZ) 
          EPSP(I,XY) = ALPHA * ABS(EPSP12(I)) + (ONE-ALPHA)*EPSP(I,XY)
          EPSP(I,YZ) = ALPHA * ABS(EPSP23(I)) + (ONE-ALPHA)*EPSP(I,YZ)
          EPSP(I,ZX) = ALPHA * ABS(EPSP31(I)) + (ONE-ALPHA)*EPSP(I,ZX)
        ELSE
          EPSP(I,XX) = ZERO
          EPSP(I,YY) = ZERO
          EPSP(I,ZZ) = ZERO
          EPSP(I,XY) = ZERO
          EPSP(I,YZ) = ZERO
          EPSP(I,ZX) = ZERO
        ENDIF
      ENDDO 
!---------------------------------------------------------------------
      ! list of active elements
!---------------------------------------------------------------------
      NINDX = 0  
      nindx_on = 0  
      do i=1,nel
        if (off(i) == one) then
          nindx_on = nindx_on + 1
          indx_on(nindx_on) = i  
        end if
      end do
!---------------------------------------------------------------------
      ! failure and damage calculation in each mode (dir)
!---------------------------------------------------------------------
      faili(1:nel)      = 0
      fmode(1:nel,1:12) = 0
      NINDX = 0  
!
      mode = m11t   ! traction xx
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
        DEPS = MAX(EPS11(I) - EPS_1, ZERO)
        IF (DEPS > ZERO .AND. DFMAX(I,1+MODE) < ONE) THEN
          DEPSM = EPS_2 - EPS_1
          FACT  = EPS_2 / MAX(EPS11(I),EM20)
          DI = FACT*DEPS/MAX(DEPSM,EM20)
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
      mode = m22t   ! traction yy
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
          FACT  = EPS_2 / MAX(EPS22(I),EM20)
          DI    = FACT * DEPS / MAX(DEPSM,EM20) 
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
      mode = m12t   ! traction xy
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
        EPS_1  = FRATE(I) * ET12
        DEPS  = MAX(EPS12(I) - EPS_1, ZERO)
        IF (DEPS > ZERO .AND. DFMAX(I,1+MODE) < ONE) THEN
          DEPSM = EPS_2 - EPS_1
          FACT  = EPS_2 / MAX(EPS12(I),EM20)
          DI    = FACT * DEPS / MAX(DEPSM,EM20)
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
      mode = m11c   ! compression xx
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
        DEPS  = MAX(-EPS11(I) - EPS_1, ZERO)
        IF (DEPS > ZERO .AND. DFMAX(I,1+MODE) < ONE) THEN
          DEPSM = EPS_2 - EPS_1
          FACT  = EPS_2 / MAX(ABS(EPS11(I)),EM20)
          DI    = FACT * DEPS / MAX(DEPSM,EM20)
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
      mode = m22c   ! compression yy
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
          FACT  = EPS_2 / MAX(ABS(EPS22(I)),EM20)
          DI    = FACT * DEPS / MAX(DEPSM,EM20)
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
      mode = m12c   ! compression xy
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
          FACT  = EPS_2 / MAX(ABS(EPS12(I)),EM20)
          DI    = FACT * DEPS / MAX(DEPSM,EM20)
          DFMAX(I,1+MODE) = MAX(DFMAX(I,1+MODE), DI)
          IF (DFMAX(I,1+MODE) >= ONE) THEN
            FAILI(I) = 1
            FMODE(I,MODE) = 1
            DFMAX(I,1+MODE) = ONE
            EPSM6(I) = -EPS_2
          ENDIF
        ENDIF
      end do
!
      mode = m33t   ! traction zz
      if (fail%table4d(mode)%notable > 0) then 
        epdot(1:nel) = abs(epsp(1:nel,zz)) / epspref
        call table_mat_vinterp(fail%table4d(mode),nel,nel,vartmp(1:nel,mode:mode),epdot,frate,dydx)
      else
        frate(:) = one
      end if
      do j = 1,nindx_on
        i = indx_on(j)
        FRATE(I) = FRATE(I)*FSIZE(I)
        EPS_2 = FRATE(I) * ET3M
        EPS_1 = FRATE(I) * ET3
        DEPS  = MAX(EPS33(I) - EPS_1, ZERO)
        IF (DEPS > ZERO .AND. DFMAX(I,1+MODE) < ONE) THEN
          DEPSM = EPS_2 - EPS_1
          FACT  = EPS_2 / MAX(EPS33(I),EM20)
          DI    = FACT * DEPS / MAX(DEPSM,EM20)
          DFMAX(I,1+MODE) = MAX(DFMAX(I,1+MODE), DI)
          IF (DFMAX(I,1+MODE) >= ONE) THEN
            FAILI(I) = 1
            FMODE(I,MODE) = 1
            DFMAX(I,1+MODE) = ONE
            EPSM7(I) = EPS_2
          ENDIF
        ENDIF
      end do
!
      mode = m33c   ! compression zz
      if (fail%table4d(mode)%notable > 0) then 
        epdot(1:nel) = abs(epsp(1:nel,zz)) / epspref
        call table_mat_vinterp(fail%table4d(mode),nel,nel,vartmp(1:nel,mode:mode),epdot,frate,dydx)
      else
        frate(:) = one
      end if
      do j = 1,nindx_on
        i = indx_on(j)
        FRATE(I) = FRATE(I)*FSIZE(I)
        EPS_2 = FRATE(I) * EC3M
        EPS_1 = FRATE(I) * EC3
        DEPS  = MAX(-EPS33(I) - EPS_1, ZERO)
        IF (DEPS > ZERO .AND. DFMAX(I,1+MODE) < ONE) THEN
          DEPSM = EPS_2 - EPS_1
          FACT  = EPS_2 / MAX(ABS(EPS33(I)),EM20)
          DI    = FACT * DEPS / MAX(DEPSM,EM20)
          DFMAX(I,1+MODE) = MAX(DFMAX(I,1+MODE), DI)
          IF (DFMAX(I,1+MODE) >= ONE) THEN
            FAILI(I) = 1
            FMODE(I,MODE) = 1
            DFMAX(I,1+MODE) = ONE
            EPSM8(I) = -EPS_2
          ENDIF
        ENDIF
      end do
!
      mode = m23t   ! traction yz
      if (fail%table4d(mode)%notable > 0) then 
        epdot(1:nel) = abs(epsp(1:nel,yz)) / epspref
        call table_mat_vinterp(fail%table4d(mode),nel,nel,vartmp(1:nel,mode:mode),epdot,frate,dydx)
      else
        frate(:) = one
      end if
      do j = 1,nindx_on
        i = indx_on(j)
        FRATE(I) = FRATE(I)*FSIZE(I)
        EPS_2 = FRATE(I) * ET23M
        EPS_1 = FRATE(I) * ET23
        DEPS  = MAX(EPS23(I) - EPS_1, ZERO)
        IF (DEPS > ZERO .AND. DFMAX(I,1+MODE) < ONE) THEN
          DEPSM = EPS_2 - EPS_1
          FACT  = EPS_2 / MAX(EPS23(I),EM20)
          DI    = FACT * DEPS / MAX(DEPSM,EM20)
          DFMAX(I,1+MODE) = MAX(DFMAX(I,1+MODE), DI)
          IF (DFMAX(I,1+MODE) >= ONE) THEN
            FAILI(I) = 1
            FMODE(I,MODE) = 1
            DFMAX(I,1+MODE) = ONE
            EPSM9(I) = EPS_2
          ENDIF
        ENDIF
      end do
!
      mode = m23c   ! compression yz
      if (fail%table4d(mode)%notable > 0) then 
        epdot(1:nel) = abs(epsp(1:nel,yz)) / epspref
        call table_mat_vinterp(fail%table4d(mode),nel,nel,vartmp(1:nel,mode:mode),epdot,frate,dydx)
      else
        frate(:) = one
      end if
      do j = 1,nindx_on
        i = indx_on(j)
        FRATE(I) = FRATE(I)*FSIZE(I)
        EPS_2 = FRATE(I) * EC23M
        EPS_1 = FRATE(I) * EC23
        DEPS  = MAX(-EPS23(I) - EPS_1, ZERO)
        IF (DEPS > ZERO .AND. DFMAX(I,1+MODE) < ONE) THEN
          DEPSM = EPS_2 - EPS_1
          FACT  = EPS_2 / MAX(ABS(EPS23(I)),EM20)
          DI    = FACT * DEPS / MAX(DEPSM,EM20)
          DFMAX(I,1+MODE) = MAX(DFMAX(I,1+MODE), DI)
          IF (DFMAX(I,1+MODE) >= ONE) THEN
            FAILI(I) = 1
            FMODE(I,MODE) = 1
            DFMAX(I,1+MODE) = ONE
            EPSM10(I) = -EPS_2
          ENDIF
        ENDIF
      end do
!
      mode = m31t   ! traction zx
      if (fail%table4d(mode)%notable > 0) then 
        epdot(1:nel) = abs(epsp(1:nel,zx)) / epspref
        call table_mat_vinterp(fail%table4d(mode),nel,nel,vartmp(1:nel,mode:mode),epdot,frate,dydx)
      else
        frate(:) = one
      end if
      do j = 1,nindx_on
        i = indx_on(j)
        FRATE(I) = FRATE(I)*FSIZE(I)
        EPS_2 = FRATE(I) * ET31M
        EPS_1 = FRATE(I) * ET31
        DEPS  = MAX(EPS31(I) - EPS_1, ZERO)
        IF (DEPS > ZERO .AND. DFMAX(I,1+MODE) < ONE) THEN
          DEPSM = EPS_2 - EPS_1
          FACT  = EPS_2 / MAX(EPS31(I),EM20)
          DI    = FACT * DEPS / MAX(DEPSM,EM20)
          DFMAX(I,1+MODE) = MAX(DFMAX(I,1+MODE), DI)
          IF (DFMAX(I,1+MODE) >= ONE) THEN
            FAILI(I) = 1
            FMODE(I,MODE) = 1
            DFMAX(I,1+MODE) = ONE
            EPSM11(I) = EPS_2
          ENDIF
        ENDIF
      end do
!
      mode = m31c   ! compression zx
      if (fail%table4d(mode)%notable > 0) then 
        epdot(1:nel) = abs(epsp(1:nel,zx)) / epspref
        call table_mat_vinterp(fail%table4d(mode),nel,nel,vartmp(1:nel,mode:mode),epdot,frate,dydx)
      else
        frate(:) = one
      end if
      do j = 1,nindx_on
        i = indx_on(j)
        FRATE(I) = FRATE(I)*FSIZE(I)
        EPS_2 = FRATE(I) * EC31M
        EPS_1 = FRATE(I) * EC31
        DEPS  = MAX(-EPS31(I) - EPS_1, ZERO)
        IF (DEPS > ZERO .AND. DFMAX(I,1+MODE) < ONE) THEN
          DEPSM = EPS_2 - EPS_1
          FACT  = EPS_2 / MAX(ABS(EPS31(I)),EM20)
          DI    = FACT * DEPS / MAX(DEPSM,EM20)
          DFMAX(I,1+MODE) = MAX(DFMAX(I,1+MODE), DI)
          IF (DFMAX(I,1+MODE) >= ONE) THEN
            FAILI(I) = 1
            FMODE(I,MODE) = 1
            DFMAX(I,1+MODE) = ONE
            EPSM12(I) = -EPS_2
          ENDIF
        ENDIF
      end do
!---------------------------------------
      DO I = 1,NEL
        IF (FAILI(I) == 1) THEN
          UELR(I) = UELR(I)+1
          NINDX   = NINDX + 1  
          INDX(NINDX) = I
          IF (UELR(I) >= NPT) THEN
            TDEL(I) = TIME 
            OFF(I)  = ZERO
          ENDIF
        ENDIF
      ENDDO  
c------------------------
c     Apply damage to stress
c     damage / direction is equal to the max of damages par mode in the same direction
c------------------------
      DO I=1, NEL
        IF (OFF(I) == ONE) THEN 
          DAMXX  = MAX(DFMAX(I,2) ,DFMAX(I,5) )
          DAMYY  = MAX(DFMAX(I,3) ,DFMAX(I,6) )
          DAMZZ  = MAX(DFMAX(I,8) ,DFMAX(I,9) )
          DAMXY  = MAX(DFMAX(I,4) ,DFMAX(I,7) )
          DAMYZ  = MAX(DFMAX(I,10),DFMAX(I,11))
          DAMZX  = MAX(DFMAX(I,12),DFMAX(I,13))
c
          ODAMXX = MAX(ODAM(I,1) ,ODAM(I,4) )
          ODAMYY = MAX(ODAM(I,2) ,ODAM(I,5) )
          ODAMZZ = MAX(ODAM(I,7) ,ODAM(I,8) )
          ODAMXY = MAX(ODAM(I,3) ,ODAM(I,6) )
          ODAMYZ = MAX(ODAM(I,9) ,ODAM(I,10))
          ODAMZX = MAX(ODAM(I,11),ODAM(I,12))
c
          DFMAX(I,1) = MAX(DAMXX,DAMYY)
          DFMAX(I,1) = MAX(DAMZZ,DFMAX(I,1))
          DFMAX(I,1) = MAX(DAMXY,DFMAX(I,1))
          DFMAX(I,1) = MAX(DAMYZ,DFMAX(I,1))
          DFMAX(I,1) = MAX(DAMZX,DFMAX(I,1))
c Undamaged sign estimate
          SIGNXX(I) = SIGO(I,XX)/MAX(ONE-ODAMXX,EM20) + (SIGNXX(I) - SIGO(I,XX))
          SIGNYY(I) = SIGO(I,YY)/MAX(ONE-ODAMYY,EM20) + (SIGNYY(I) - SIGO(I,YY))
          SIGNZZ(I) = SIGO(I,ZZ)/MAX(ONE-ODAMZZ,EM20) + (SIGNZZ(I) - SIGO(I,ZZ))
          SIGNXY(I) = SIGO(I,XY)/MAX(ONE-ODAMXY,EM20) + (SIGNXY(I) - SIGO(I,XY))
          SIGNYZ(I) = SIGO(I,YZ)/MAX(ONE-ODAMYZ,EM20) + (SIGNYZ(I) - SIGO(I,YZ))
          SIGNZX(I) = SIGO(I,ZX)/MAX(ONE-ODAMZX,EM20) + (SIGNZX(I) - SIGO(I,ZX))
c Damaged sign
          SIGNXX(I) = SIGNXX(I) * (ONE-DAMXX)*OFF(I)
          SIGNYY(I) = SIGNYY(I) * (ONE-DAMYY)*OFF(I)
          SIGNZZ(I) = SIGNZZ(I) * (ONE-DAMZZ)*OFF(I)
          SIGNXY(I) = SIGNXY(I) * (ONE-DAMXY)*OFF(I)
          SIGNYZ(I) = SIGNYZ(I) * (ONE-DAMYZ)*OFF(I)
          SIGNZX(I) = SIGNZX(I) * (ONE-DAMZX)*OFF(I)
        ELSE
          SIGNXX(I) = ZERO
          SIGNYY(I) = ZERO
          SIGNZZ(I) = ZERO
          SIGNXY(I) = ZERO
          SIGNYZ(I) = ZERO
          SIGNZX(I) = ZERO
        ENDIF
      ENDDO
c------------------------
c     Update UVAR
c------------------------
      DO I=1, NEL
        IF (OFF(I) == ONE) THEN 
          DO  J=1,6
            UVAR(I,J+6)  = EPSP(I,J)
          ENDDO
        ENDIF
      ENDDO  
      DO I =1, NEL 
        IF (OFF(I) == ONE) THEN 
          UVAR(I,XX) = SIGNXX(I)
          UVAR(I,YY) = SIGNYY(I)
          UVAR(I,ZZ) = SIGNZZ(I)
          UVAR(I,XY) = SIGNXY(I)
          UVAR(I,YZ) = SIGNYZ(I)
          UVAR(I,ZX) = SIGNZX(I)
        ENDIF
      ENDDO
c------------------------
c     Output
c------------------------
      IF (NINDX > 0) THEN 
        DO J=1,NINDX    
          I = INDX(J)
#include  "lockon.inc"      
            WRITE(ISTDO,1000) NGL(I),IPT
            WRITE(IOUT, 2000) NGL(I),IPT
            IF (FMODE(I,1)  == 1) WRITE(IOUT,4000) '1 - TRACTION XX',EPS11(I),EPSM1(I),EPSP(I,XX)
            IF (FMODE(I,2)  == 1) WRITE(IOUT,4000) '2 - TRACTION YY',EPS22(I),EPSM2(I),EPSP(I,YY)
            IF (FMODE(I,3)  == 1) WRITE(IOUT,4000) '3 - POSITIVE SHEAR XY',EPS12(I),EPSM3(I),EPSP(I,XY)
            IF (FMODE(I,4)  == 1) WRITE(IOUT,4000) '4 - COMPRESSION XX',EPS11(I),EPSM4(I),EPSP(I,XX)
            IF (FMODE(I,5)  == 1) WRITE(IOUT,4000) '5 - COMPRESSION YY',EPS22(I),EPSM5(I),EPSP(I,YY)
            IF (FMODE(I,6)  == 1) WRITE(IOUT,4000) '6 - NEGATIVE SHEAR XY',EPS12(I),EPSM6(I),EPSP(I,XY)
            IF (FMODE(I,7)  == 1) WRITE(IOUT,4000) '7 - TRACTION ZZ',EPS33(I),EPSM7(I),EPSP(I,ZZ)
            IF (FMODE(I,8)  == 1) WRITE(IOUT,4000) '8 - COMPRESSION ZZ',EPS33(I),EPSM8(I),EPSP(I,ZZ)
            IF (FMODE(I,9)  == 1) WRITE(IOUT,4000) '9 - POSITIVE SHEAR YZ',EPS23(I),EPSM9(I),EPSP(I,YZ)
            IF (FMODE(I,10) == 1) WRITE(IOUT,4000) '10 - NEGATIVE SHEAR YZ',EPS23(I),EPSM10(I),EPSP(I,YZ)
            IF (FMODE(I,11) == 1) WRITE(IOUT,4000) '11 - POSITIVE SHEAR ZX',EPS31(I),EPSM11(I),EPSP(I,ZX)
            IF (FMODE(I,12) == 1) WRITE(IOUT,4000) '12 - NEGATIVE SHEAR ZX',EPS31(I),EPSM12(I),EPSP(I,ZX)
            IF (OFF(I) == ZERO)   WRITE(IOUT, 3000) NGL(I),TIME
#include  "lockoff.inc"
        END DO
      END IF                     
c-----------------------------------------------------------------------
 1000 FORMAT(1X,'FAILURE (ORTHSTR) OF SOLID ELEMENT ',I10,1X,',GAUSS PT ',I5)
 2000 FORMAT(1X,'FAILURE (ORTHSTR) OF SOLID ELEMENT ',I10,1X,',GAUSS PT ',I5)
 3000 FORMAT(1X,'RUPTURE OF ELEMENT #',I10,1X,'AT TIME # ',1PE12.4)   
 4000 FORMAT(1X,'MODE',1X,A,', STRAIN',1PE12.4,', CRITICAL VALUE',1PE12.4,
     .       ', STRAIN RATE',1PE12.4) 
c-----------------------------------------------------------------------
      RETURN          
      END
