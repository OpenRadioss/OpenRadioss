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
!||    fail_tab2_s             ../engine/source/materials/fail/tabulated/fail_tab2_s.F
!||--- called by ------------------------------------------------------
!||    mmain                   ../engine/source/materials/mat_share/mmain.F90
!||    mulaw                   ../engine/source/materials/mat_share/mulaw.F90
!||    usermat_solid           ../engine/source/materials/mat_share/usermat_solid.F
!||--- calls      -----------------------------------------------------
!||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
!||--- uses       -----------------------------------------------------
!||    fail_param_mod          ../common_source/modules/mat_elem/fail_param_mod.F90
!||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
      SUBROUTINE FAIL_TAB2_S (FAIL      ,
     1     NEL      ,NUVAR    ,NVARTMP  ,UVAR     ,VARTMP   ,
     2     TIME     ,NGL      ,ALDT     ,DPLA     ,EPSP     ,
     4     SIGNXX   ,SIGNYY   ,SIGNZZ   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,            
     5     TEMP     ,OFF      ,DFMAX    ,TDELE    ,DMG_SCALE,
     6     UELR     ,IPG      ,NPG      ,LOFF     ,
     7     NOFF     ,VOLN     )   
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
      INTEGER, INTENT(IN)    :: NEL,NUVAR,NPG,IPG
      INTEGER, INTENT(IN)    :: NVARTMP
      INTEGER, INTENT(IN)    :: NGL(NEL)
      INTEGER, INTENT(INOUT) :: NOFF(NEL)
      INTEGER, DIMENSION(NEL,NVARTMP) ,INTENT(INOUT) :: VARTMP
      my_real, INTENT(IN) :: TIME,ALDT(NEL),
     .        DPLA(NEL),EPSP(NEL),TEMP(NEL),
     .        SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .        SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),VOLN(NEL)
      my_real, INTENT(INOUT) :: 
     .        UVAR(NEL,NUVAR),DFMAX(NEL),TDELE(NEL),
     .        DMG_SCALE(NEL),OFF(NEL),UELR(NEL),LOFF(NEL)
      type (fail_param_) ,intent(inout) :: fail              !< failure model data structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,INDX(NEL),NINDX,ITAB_EPSF,FAILIP,
     .        ITAB_INST,ITAB_SIZE,IREG,NDIM,
     .        IAD(NEL),ILEN(NEL),
     .        LOG_SCALE1,LOG_SCALE2
      my_real 
     .        FCRIT  ,DN,DCRIT,ECRIT,EXP_REF,EXPO,EL_REF,
     .        SR_REF1,FSCALE_EL,SHRF,BIAXF  ,SR_REF2,
     .        FSCALE_SR,CJC,FSCALE_DLIM,TEMP_REF, FSCALE_TEMP, VOLFRAC,
     .        RGTR1  ,RGTR2, RETA
      my_real
     .        INST(NEL)  ,DC(NEL)     ,L0(NEL)     ,
     .        TRIAX(NEL)  ,XI(NEL)     ,EPSF(NEL)   ,EPSL(NEL)   ,
     .        DEPSF(NEL)  ,DEPSL(NEL)  ,XVEC(NEL,3) ,DPL_DEF     ,
     .        COS3THETA   ,DET,P,SVM   ,SXX,SYY,SZZ ,SIZEFAC(NEL),
     .        RATEFAC(NEL),SOFTEXP(NEL),DLIM(NEL)   ,
     .        TEMPFAC(NEL),TEMPFAC2(NEL),DFT(NEL)   ,VAR(NEL) 
      my_real ,DIMENSION(NEL) :: LAMBDA,DYDX
C!--------------------------------------------------------------
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering failure criterion parameters
      FCRIT         = FAIL%UPARAM(1)
      FAILIP        = MIN(NINT(FAIL%UPARAM(2)),NPG)
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
      VOLFRAC       = FAIL%UPARAM(23)
      RGTR1         = FAIL%UPARAM(24)
      RGTR1         = MAX(RGTR1,EM06)
      RGTR2         = FAIL%UPARAM(25)
      RGTR2         = MIN(RGTR2,TWO_THIRD - EM06)
c
      ! Checking element failure and recovering user variable
      DO I=1,NEL
        ! If necking control is activated
        IF (FAIL%TABLE4D(6)%NOTABLE > 0 .OR. ECRIT > ZERO) THEN 
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
        P   = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
        SXX = SIGNXX(I) - P
        SYY = SIGNYY(I) - P
        SZZ = SIGNZZ(I) - P
        SVM = HALF*(SXX**2 + SYY**2 + SZZ**2)
     .        + SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2
        SVM = SQRT(MAX(THREE*SVM,ZERO))
        TRIAX(I) = P/MAX(EM20,SVM)
        IF (TRIAX(I) < -ONE) TRIAX(I) = -ONE
        IF (TRIAX(I) >  ONE) TRIAX(I) = ONE
c
        ! Computation of Lode parameter
        DET  = SXX*SYY*SZZ + TWO*SIGNXY(I)*SIGNZX(I)*SIGNYZ(I)
     .       - SXX*SIGNYZ(I)**2-SZZ*SIGNXY(I)**2 - SYY*SIGNZX(I)**2
        COS3THETA = HALF*TWENTY7*DET/MAX(EM20,SVM**3)
        IF (COS3THETA < -ONE) COS3THETA = -ONE
        IF (COS3THETA > ONE)  COS3THETA = ONE
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
        lambda(1:nel) = temp(i:nel) / temp_ref
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
            else    !  if (triax >= third
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
      if (fail%table4d(6)%notable > 0) then     ! Instability plastic strain vs triaxiality vs Lode vs temperature
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
        IF (LOFF(I) == ONE .AND. OFF(I) ==  ONE .AND. DPLA(I) > ZERO) THEN
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
            NINDX       = NINDX + 1
            INDX(NINDX) = I
            NOFF(I)     = NOFF(I) + 1
            LOFF(I)     = ZERO
            UELR(I)     = UELR(I) + VOLN(I)
            IF ((NPG > 1).AND.(VOLFRAC > ZERO)) THEN 
              IF (UELR(I)/(NPG*VOLN(I)) >= VOLFRAC) THEN 
                OFF(I)    = ZERO
                TDELE(I)  = TIME
              ENDIF
            ELSE
              IF (NOFF(I) >= FAILIP) THEN 
                OFF(I)    = ZERO
                TDELE(I)  = TIME
              ENDIF
            ENDIF           
          ENDIF
c
          ! Compute the control necking instability damage
          IF ((ITAB_INST > 0).OR.(ECRIT > ZERO)) THEN 
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
          !< Damage softening scale computation
          IF (DC(I) < ONE) THEN 
            DMG_SCALE(I) = ONE - ((DFMAX(I)-DC(I))/MAX(ONE-DC(I),EM20))**SOFTEXP(I)
          ELSE
            DMG_SCALE(I) = ZERO
          ENDIF
        ELSE
          DMG_SCALE(I) = ONE
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
          WRITE(IOUT, 1000) NGL(I),IPG,TIME
          WRITE(ISTDO,1000) NGL(I),IPG,TIME
          IF (OFF(I) == ZERO) THEN 
            WRITE(IOUT, 2000) NGL(I),TIME
            WRITE(ISTDO,2000) NGL(I),TIME
          ENDIF
#include "lockoff.inc"
        END DO
      END IF                          
c-----------------------------------------------------------------------
 1000 FORMAT(1X,'FOR SOLID ELEMENT NUMBER el#',I10,
     .          ' FAILURE (TAB2) AT GAUSS POINT ',I5,
     .          ' AT TIME :',1PE12.4)     
 2000 FORMAT(1X,'-- RUPTURE OF SOLID ELEMENT :',I10,
     .          ' AT TIME :',1PE12.4)     
c
      END
