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
!||    fail_tab2_c             ../engine/source/materials/fail/tabulated/fail_tab2_c.F
!||--- called by ------------------------------------------------------
!||    mulawc                  ../engine/source/materials/mat_share/mulawc.F90
!||    usermat_shell           ../engine/source/materials/mat_share/usermat_shell.F
!||--- calls      -----------------------------------------------------
!||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
!||--- uses       -----------------------------------------------------
!||    fail_param_mod          ../common_source/modules/mat_elem/fail_param_mod.F90
!||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
      SUBROUTINE FAIL_TAB2_C (FAIL      ,
     1     NEL      ,NUVAR    ,NVARTMP  ,UVAR     ,VARTMP   ,
     2     TIME     ,NGL      ,ALDT     ,DPLA     ,EPSP     ,     
     4     SIGNXX   ,SIGNYY   ,SIGNXY   ,     
     5     TEMP     ,FOFF     ,DFMAX    ,TDELE    ,IPT      ,
     6     IPG      ,DMG_FLAG ,DMG_SCALE,
     7     NOFF     ,OFF      ,NPT      ,GVAR     )   
C---------+---------+---+---+--------------------------------------------
      use fail_param_mod
      use table_mat_vinterp_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL,NUVAR,NGL(NEL),IPT,IPG
      INTEGER, INTENT(IN) :: NPT
      INTEGER, INTENT(IN) :: NVARTMP
      INTEGER, DIMENSION(NEL,NVARTMP), INTENT(INOUT) :: VARTMP
      INTEGER, INTENT(INOUT) :: 
     .        DMG_FLAG,FOFF(NEL)
      my_real, INTENT(IN) :: TIME,ALDT(NEL),DPLA(NEL),EPSP(NEL),TEMP(NEL),
     .        SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL)
      my_real, INTENT(INOUT) :: UVAR(NEL,NUVAR)
      my_real, INTENT(INOUT) :: DFMAX(NEL),TDELE(NEL),DMG_SCALE(NEL)
      INTEGER, INTENT(INOUT) :: NOFF(NEL)
      my_real, INTENT(INOUT) :: OFF(NEL)
      my_real, DIMENSION(NEL), INTENT(INOUT) :: GVAR    !< Global element user variables
      type (fail_param_)     , intent(inout) :: fail    !< failure model data structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,NINDX,IREG,NDIM
      INTEGER :: LOG_SCALE1,LOG_SCALE2,MIDFAIL
      INTEGER ,DIMENSION(NEL) :: INDX,IAD,ILEN
      my_real 
     .        FCRIT  ,DN,DCRIT,ECRIT,EXP_REF,EXPO,EL_REF,
     .        SR_REF1,FSCALE_EL,SHRF,BIAXF  ,SR_REF2,
     .        FSCALE_SR,CJC,FSCALE_DLIM,TEMP_REF, FSCALE_TEMP,
     .        RGTR1  ,RGTR2,RETA,ETA
      my_real
     .        DF,INST(NEL)  ,DC(NEL)     ,L0(NEL)     ,
     .        TRIAX(NEL)  ,XI(NEL)     ,EPSF(NEL)   ,EPSL(NEL)   ,
     .        DEPSF(NEL)  ,DEPSL(NEL)  ,XVEC(NEL,3) ,DPL_DEF     ,
     .        COS3THETA   ,P           ,SVM         ,SIZEFAC(NEL),
     .        RATEFAC(NEL),DSIZE(NEL)  ,SOFTEXP(NEL),DLIM(NEL) 
      my_real ,DIMENSION(NEL) :: LAMBDA,DYDX,TEMPFAC,TEMPFAC2
      LOGICAL IS_MIDPT,COMP_MIDFAIL
C!--------------------------------------------------------------
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering failure criterion parameters
      !< Integer parameters
      MIDFAIL       = FAIL%IPARAM(1)
      !< Real parameters
      FCRIT         = FAIL%UPARAM(1)
      DN            = FAIL%UPARAM(4)
      DCRIT         = FAIL%UPARAM(5) 
      ECRIT         = FAIL%UPARAM(6)
      EXP_REF       = FAIL%UPARAM(7)
      EXPO          = FAIL%UPARAM(8)
      IREG          = NINT(FAIL%UPARAM(9))
      EL_REF        = FAIL%UPARAM(10)
      SR_REF1       = FAIL%UPARAM(11)
      FSCALE_EL     = FAIL%UPARAM(12)
      SHRF          = FAIL%UPARAM(13)
      BIAXF         = FAIL%UPARAM(14)
      SR_REF2       = FAIL%UPARAM(15)
      FSCALE_SR     = FAIL%UPARAM(16)
      CJC           = FAIL%UPARAM(17)
      FSCALE_DLIM   = FAIL%UPARAM(18)
      TEMP_REF      = FAIL%UPARAM(19)
      FSCALE_TEMP   = FAIL%UPARAM(20)
      LOG_SCALE1    = NINT(FAIL%UPARAM(21))
      LOG_SCALE2    = NINT(FAIL%UPARAM(22))
      RGTR1         = FAIL%UPARAM(24)
      RGTR1         = MAX(RGTR1,EM06)
      RGTR2         = FAIL%UPARAM(25)
      RGTR2         = MIN(RGTR2,TWO_THIRD - EM06)
c
      ! Set flag for stress softening
      DMG_FLAG = 1
c
      !< Flag to identify if mid-plane point
      IS_MIDPT = .FALSE.
      ! -> Odd number of integration points
      IF (MOD(NPT,2) == 1) THEN
        IF (IPT == (NPT/2 + 1)) IS_MIDPT = .TRUE.
      ! -> Even number of integration points
      ELSE
        IF (IPT == NPT/2)       IS_MIDPT = .TRUE.
        IF (IPT == (NPT/2 + 1)) IS_MIDPT = .TRUE.
      ENDIF
c
      !< Midfail computation flag
      COMP_MIDFAIL = .TRUE.
      IF ((MIDFAIL == 4) .AND. (.NOT.IS_MIDPT)) COMP_MIDFAIL = .FALSE.
c
      ! Checking element failure and recovering user variable
      DO I=1,NEL
        ! If necking control is activated
        IF (fail%table4d(6)%notable > 0 .OR. ECRIT > ZERO) THEN 
          IF (UVAR(I,2) == ZERO) UVAR(I,2) = ONE
        ELSE
          IF (UVAR(I,2) == ZERO) UVAR(I,2) = DCRIT
        ENDIF
        ! Instability damage
        INST(I) = UVAR(I,1)
        ! Necking critical damage 
        DC(I)   = UVAR(I,2)
      END DO
c      
      !====================================================================
      ! - LOOP OVER THE ELEMENT TO COMPUTE THE STRESS STATE QUANTITIES
      !====================================================================       
      DO I=1,NEL
c
        ! Computation of hydrostatic stress, Von Mises stress, and stress triaxiality
        P = THIRD*(SIGNXX(I) + SIGNYY(I))
        ! Von Mises equivalent stress
        SVM = SIGNXX(I)**2 + SIGNYY(I)**2 - SIGNXX(I)*SIGNYY(I) +
     .           THREE*SIGNXY(I)**2
        SVM = SQRT(MAX(SVM,ZERO))
        TRIAX(I) = P/MAX(EM20,SVM)
        IF (TRIAX(I) < -TWO_THIRD) TRIAX(I) = -TWO_THIRD
        IF (TRIAX(I) >  TWO_THIRD) TRIAX(I) =  TWO_THIRD
c
        ! Computation of Lode parameter
        COS3THETA = -HALF*TWENTY7*TRIAX(I)*(TRIAX(I)**2 - THIRD)
        IF (COS3THETA < -ONE ) COS3THETA = -ONE
        IF (COS3THETA > ONE  ) COS3THETA =  ONE
        XI(I) = ONE - TWO*ACOS(COS3THETA)/PI
c
      ENDDO
c      
      !====================================================================
      ! - COMPUTE FACTORS FOR ELEMENT SIZE, STRAIN RATE AND TEMPERATURE
      !====================================================================
      ! At initial time, save the element size 
      IF (UVAR(1,3) == ZERO) UVAR(1:NEL,3) = ALDT(1:NEL)
      L0(1:NEL) = UVAR(1:NEL,3)
c
      ! compute the softening exponent
      if (fail%table4d(1)%notable > 0) then
        lambda(1:nel) = l0(1:nel) / exp_ref
        call table_mat_vinterp(fail%table4d(1),nel,nel,vartmp(1:nel,1:1),lambda,softexp,dydx)
      else
        softexp(1:nel) = expo
      endif
c
      ! compute the temperature dependency factor
      if (fail%table4d(4)%notable > 0) then
        lambda(1:nel) = temp(1:nel) / temp_ref
        call table_mat_vinterp(fail%table4d(4),nel,nel,vartmp(1:nel,4:4),lambda,tempfac,dydx)
      else
        tempfac(1:nel) = one
      endif
      tempfac2(1:nel) = tempfac(1:nel)
c
      ! compute the element size regularization factor 
      if (fail%table4d(7)%notable > 0) then
        ndim = fail%table4d(7)%ndim
        if (ireg == 1) then
          xvec(1:nel,2:3) = zero
          select case (ndim)
            case(1)            ! scale factor vs element size
              xvec(1:nel,1)   = l0(1:nel)/el_ref
            case(2)            ! scale factor vs element size vs strain rate 
              xvec(1:nel,1)   = l0(1:nel)/el_ref
              if (log_scale1 > 0) then 
                do i = 1,nel
                  xvec(i,2) = log(max(epsp(i),em20)/sr_ref1)
                enddo 
              else
                xvec(1:nel,2) = epsp(1:nel)/sr_ref1
              endif
          end select
        else if (ireg == 2) then
          xvec(1:nel,1)   = l0(1:nel)/el_ref
          xvec(1:nel,2)   = triax(1:nel)
          xvec(1:nel,3)   = xi(1:nel)
        end if  ! ireg
!
        call table_mat_vinterp(fail%table4d(7),nel,nel,vartmp(1:nel,11),xvec,sizefac,dydx)
!
        if (ireg == 1) then 
          do i = 1,nel
            if (triax(i) < third) then 
              reta =  shrf*(one - min(max(triax(i),zero),rgtr1)/rgtr1)
            else    ! triax >= third 
              reta = (three*biaxf/(three*rgtr2 - two)) * (rgtr2 - min(max(triax(i),rgtr2),two_third))
            endif
            reta = max(zero, min(one, reta))
            sizefac(i) = sizefac(i) + reta*(one - sizefac(i))
          enddo
        endif
      else  ! func_size = 0
        sizefac(1:nel) = one
      endif
c
      ! Compute the strain rate dependency factor
      if (fail%table4d(2)%notable > 0) then
        if (log_scale2 > 0) then
          do i = 1,nel 
            lambda(i) = log(max(epsp(i),em20)/sr_ref2)
          enddo 
        else
          lambda(1:nel) = epsp(1:nel)/sr_ref2
        endif
        call table_mat_vinterp(fail%table4d(2),nel,nel,vartmp(1:nel,2:2),lambda,ratefac,dydx)
      else if (cjc > zero) then
        do i=1,nel
          if (epsp(i) > sr_ref2) then 
            ratefac(i) = one + cjc*log(epsp(i)/sr_ref2)
          else
            ratefac(i) = one
          endif
        enddo
      else
        ratefac(1:nel) = one
      end if
c
      ! Compute the damage limit value
      if (fail%table4d(3)%notable > 0) then
        call table_mat_vinterp(fail%table4d(3),nel,nel,vartmp(1:nel,3:3),triax,dlim,dydx)
        do i = 1,nel 
          dlim(i) = min(dlim(i),one)
          dlim(i) = max(dlim(i),zero)
        enddo
      else
        dlim(1:nel) = one
      endif
c
      !====================================================================
      ! - COMPUTATION OF PLASTIC STRAIN AT FAILURE
      !====================================================================
      if (fail%table4d(5)%notable > 0) then
        xvec(1:nel,1)   = triax(1:nel)
        xvec(1:nel,2)   = xi(1:nel)
        xvec(1:nel,3)   = temp(1:nel)/temp_ref
        if (fail%table4d(5)%ndim == 3) tempfac(1:nel)  = one
        call table_mat_vinterp(fail%table4d(5),nel,nel,vartmp(1:nel,5),xvec,epsf,dydx)
      else
        epsf(1:nel) = fcrit
      end if
c
      !====================================================================
      ! - COMPUTATION OF PLASTIC STRAIN AT NECKING
      !====================================================================
      if (fail%table4d(6)%notable > 0) then
        xvec(1:nel,1)   = triax(1:nel)
        xvec(1:nel,2)   = xi(1:nel)
        xvec(1:nel,3)   = temp(1:nel)/temp_ref
        call table_mat_vinterp(fail%table4d(6),nel,nel,vartmp(1:nel,8),xvec,epsl,dydx)
        if (fail%table4d(6)%ndim == 3) tempfac2(1:nel) = one  
      else
        epsl(1:nel) = ecrit
      end if
c
      !====================================================================
      ! - COMPUTATION OF THE DAMAGE VARIABLE EVOLUTION
      !==================================================================== 
      ! Initialization of element failure index
      NINDX = 0  
      INDX(1:NEL) = 0
c
      ! Loop over the elements 
      DO I=1,NEL
c
        ! If the element is not broken
        IF (FOFF(I) /= 0 .AND. DPLA(I) > ZERO .AND. COMP_MIDFAIL) THEN
c
          ! Needs to initialize damage at a very small value the first time
          IF (DFMAX(I) == ZERO) DFMAX(I) = EM20
          IF (INST(I)  == ZERO) INST(I)  = EM20
c
          ! Compute failure strain damage variable
          DPL_DEF  = DPLA(I)/MAX(EPSF(I)*RATEFAC(I)*SIZEFAC(I)*TEMPFAC(I),EM20)
          DFMAX(I) = DFMAX(I) + DPL_DEF*DN*(DFMAX(I)**(ONE-(ONE/DN)))
          DFMAX(I) = MIN(DFMAX(I),DLIM(I))
          IF (DFMAX(I) >= ONE) THEN 
            IF ((MIDFAIL /= 1 .AND. MIDFAIL /= 2 .AND. MIDFAIL /= 3).OR.(IS_MIDPT)) THEN
              NINDX       = NINDX + 1
              INDX(NINDX) = I
              FOFF(I)     = 0
              TDELE(I)    = TIME
            ENDIF
            !< Instant failure of all integration points and element deletion
            IF ((IS_MIDPT).AND.((MIDFAIL == 3).OR.(MIDFAIL == 4))) THEN 
              OFF(I) = FOUR_OVER_5
            ENDIF
          ENDIF
c
          ! Compute the control necking instability damage
          IF (fail%table4d(6)%notable > 0 .OR. ECRIT > ZERO) THEN 
            DPL_DEF = DPLA(I)/MAX(EPSL(I)*RATEFAC(I)*SIZEFAC(I)*TEMPFAC2(I),EM20)
            INST(I) = INST(I) + DPL_DEF*DN*(INST(I)**(ONE-(ONE/DN)))
            INST(I) = MIN(INST(I),ONE)
            IF ((INST(I) >= ONE).AND.(DC(I) == ONE)) THEN 
              DC(I) = DFMAX(I)
            ENDIF
          ENDIF
c
        ENDIF
      ENDDO
c
      !====================================================================
      ! - UPDATE UVAR AND THE STRESS TENSOR
      !====================================================================
      DO I = 1,NEL 
        !< Save necking instability damage
        UVAR(I,1) = INST(I)
        !< Damage criterion for stress softening
        IF (DFMAX(I) >= DC(I)) THEN
          !< Flag for mid-point integration point softening
          IF (MOD(NPT,2) == 1) THEN
            IF ((IS_MIDPT).AND.(NOFF(I) == 0)) NOFF(I) = 1
          ELSE
            IF ((IS_MIDPT).AND.(NOFF(I) == 0)) NOFF(I) = IPT
            IF ((IS_MIDPT).AND.(NOFF(I) /= 0).AND.(IPT /= NOFF(I))) NOFF(I) = 1
          ENDIF
          !< Damage softening scale computation
          IF (DC(I) < ONE) THEN 
            DMG_SCALE(I) = ONE - ((DFMAX(I)-DC(I))/MAX(ONE-DC(I),EM20))**SOFTEXP(I)
          ELSE
            DMG_SCALE(I) = ZERO
          ENDIF
          !< Mid-point integration point softening synchronization
          IF (IS_MIDPT) GVAR(I) = DMG_SCALE(I)
          !< Synchronization of mid-point softening to other breaking integration points
          IF ((MIDFAIL == 1).AND.(NOFF(I) == 1).AND.(.NOT.IS_MIDPT)) THEN
            DMG_SCALE(I) = GVAR(I)
            IF ((DMG_SCALE(I) == ZERO).AND.(FOFF(I) /= 0)) THEN
              NINDX       = NINDX + 1
              INDX(NINDX) = I
              FOFF(I)     = 0
              TDELE(I)    = TIME
            ENDIF
          ENDIF
        ELSE
          DMG_SCALE(I) = ONE
        ENDIF
        !< Synchronization of mid-point softening to all integration points
        IF (((MIDFAIL == 2).OR.(MIDFAIL == 3)).AND.(NOFF(I) == 1).AND.(.NOT.IS_MIDPT)) THEN
          DMG_SCALE(I) = GVAR(I)
          IF ((DMG_SCALE(I) == ZERO).AND.(FOFF(I) /= 0)) THEN
            NINDX       = NINDX + 1
            INDX(NINDX) = I
            FOFF(I)     = 0
            TDELE(I)    = TIME
          ENDIF
        ENDIF
        !< Update necking critical damage
        UVAR(I,2) = DC(I)
      ENDDO
c
      !====================================================================
      ! - PRINTOUT DATA ABOUT FAILED ELEMENTS
      !====================================================================
      IF (NINDX > 0) THEN        
        DO J=1,NINDX             
          I = INDX(J)         
#include "lockon.inc"         
          WRITE(IOUT, 1000) NGL(I),IPG,IPT,TIME
          WRITE(ISTDO,1000) NGL(I),IPG,IPT,TIME
#include "lockoff.inc" 
        END DO                   
      END IF       
c------------------------
 1000 FORMAT(1X,'FOR SHELL ELEMENT NUMBER el#',I10,
     .          ' FAILURE (TAB2) AT GAUSS POINT ',I3,' LAYER ',I3,
     .          ' AT TIME :',1PE12.4)
c
      END
