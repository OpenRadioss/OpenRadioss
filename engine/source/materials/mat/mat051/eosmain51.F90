!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!||====================================================================
!||    eosmain51_mod   ../engine/source/materials/mat/mat051/eosmain51.F90
!||--- called by ------------------------------------------------------
!||    sigeps51        ../engine/source/materials/mat/mat051/sigeps51.F90
!||====================================================================
      module eosmain51_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Here is a small description of the routine, [after the header]
!! \details if needed, more details can be added here
!||====================================================================
!||    eosmain51            ../engine/source/materials/mat/mat051/eosmain51.F90
!||--- called by ------------------------------------------------------
!||    sigeps51             ../engine/source/materials/mat/mat051/sigeps51.F90
!||--- calls      -----------------------------------------------------
!||    compaction           ../common_source/eos/compaction.F90
!||    compaction2          ../common_source/eos/compaction2.F90
!||    compaction_tab       ../common_source/eos/compaction_tab.F90
!||    eosexponential       ../common_source/eos/eosexponential.F90
!||    eoslinear            ../common_source/eos/eoslinear.F
!||    eospolyno            ../common_source/eos/eospolyno.F
!||    gruneisen            ../common_source/eos/gruneisen.F
!||    idealgas             ../common_source/eos/idealgas.F
!||    idealgas_vt          ../common_source/eos/idealgas_vt.F
!||    lszk                 ../common_source/eos/lszk.F
!||    murnaghan            ../common_source/eos/murnaghan.F
!||    nasg                 ../common_source/eos/nasg.F
!||    noble_abel           ../common_source/eos/noble_abel.F
!||    osborne              ../common_source/eos/osborne.F
!||    puff                 ../common_source/eos/puff.F
!||    sesame               ../common_source/eos/sesame.F
!||    stiffgas             ../common_source/eos/stiffgas.F
!||    tabulated            ../common_source/eos/tabulated.F
!||    tillotson            ../common_source/eos/tillotson.F
!||--- uses       -----------------------------------------------------
!||    compaction2_mod      ../common_source/eos/compaction2.F90
!||    compaction_mod       ../common_source/eos/compaction.F90
!||    compaction_tab_mod   ../common_source/eos/compaction_tab.F90
!||    constant_mod         ../common_source/modules/constant_mod.F
!||    eos_param_mod        ../common_source/modules/mat_elem/eos_param_mod.F90
!||    eosexponential_mod   ../common_source/eos/eosexponential.F90
!||    eoslinear_mod        ../common_source/eos/eoslinear.F
!||    eospolyno_mod        ../common_source/eos/eospolyno.F
!||    gruneisen_mod        ../common_source/eos/gruneisen.F
!||    idealgas_mod         ../common_source/eos/idealgas.F
!||    idealgas_vt_mod      ../common_source/eos/idealgas_vt.F
!||    lszk_mod             ../common_source/eos/lszk.F
!||    murnaghan_mod        ../common_source/eos/murnaghan.F
!||    nasg_mod             ../common_source/eos/nasg.F
!||    noble_abel_mod       ../common_source/eos/noble_abel.F
!||    osborne_mod          ../common_source/eos/osborne.F
!||    precision_mod        ../common_source/modules/precision_mod.F90
!||    puff_mod             ../common_source/eos/puff.F
!||    sesame_mod           ../common_source/eos/sesame.F
!||    stiffgas_mod         ../common_source/eos/stiffgas.F
!||    tabulated_mod        ../common_source/eos/tabulated.F
!||    tillotson_mod        ../common_source/eos/tillotson.F
!||====================================================================
         subroutine eosmain51(pmin, off, eint, mu, espe, dvol, df, v, psh, p, dpdm, dpde, rho0, &
                              temp, v0 , sbufmat, bufmat, eos_struct, vareos, &
                              time, dt, npf   ,tf   ,snpf ,stf )


! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only : one, ep03, fifteen
          use eos_param_mod , only : eos_param_
          use precision_mod, only : WP
          use constant_mod , only : zero
          use eos_param_mod ,only : analy_temp
      !--------------------------------------------------!--------------------------!-----------!
      !   EOS MODULE                                     !  IEOS  ! EOS             !  VERSION  !
      !--------------------------------------------------!--------!-----------------!-----------!
          USE EOSPOLYNO_MOD      , ONLY : EOSPOLYNO      !   1    !  POLYNOMIAL     ! 12.0      !
          USE GRUNEISEN_MOD      , ONLY : GRUNEISEN      !   2    !  GRUNEISEN      ! 12.0      !
          USE TILLOTSON_MOD      , ONLY : TILLOTSON      !   3    !  TILLOTSON      ! 12.0      !
          USE PUFF_MOD           , ONLY : PUFF           !   4    !  PUFF           ! 12.0      !
          USE SESAME_MOD         , ONLY : SESAME         !   5    !  SESAME         ! 12.0      !
          USE NOBLE_ABEL_MOD     , ONLY : NOBLE_ABEL     !   6    !  NOBLE-ABEL     ! 2017.0    !
          USE IDEALGAS_MOD       , ONLY : IDEALGAS       !   7    !  IDEAL GAS      ! 2018.0    !
          USE MURNAGHAN_MOD      , ONLY : MURNAGHAN      !   8    !  MUNAGHAN       ! 2018.0    !
          USE OSBORNE_MOD        , ONLY : OSBORNE        !   9    !  OSBORNE        ! 2018.0    !
          USE STIFFGAS_MOD       , ONLY : STIFFGAS       !  10    !  STIFFENED GAS  ! 2018.0    !
          USE LSZK_MOD           , ONLY : LSZK           !  11    !  LSZK           ! 2018.0    !
         !USE POWDER_BURN_MOD    , ONLY : POWDER_BURN    !  12    !  POWDER-BURN    ! 2019.1    !
          USE COMPACTION_MOD     , ONLY : COMPACTION     !  13    !  COMPACTION     ! 2019.1    !
          USE NASG_MOD           , ONLY : NASG           !  14    !  NASG           ! 2020.0    !
         !USE JWL_MOD            , ONLY : JWL            !  15    !  JWL            ! internal use : INIMAP
          USE IDEALGAS_VT_MOD    , ONLY : IDEALGAS_VT    !  16    !  IDEALGAS_VT    ! 2022.0    !
          USE TABULATED_MOD      , ONLY : TABULATED      !  17    !  TABULATED      ! 2022.2    !
          USE EOSLINEAR_MOD      , ONLY : EOSLINEAR      !  18    !  LINEAR         ! 2019.0    !
          USE EOSEXPONENTIAL_MOD , ONLY : EOSEXPONENTIAL !  19    !  EXPONENTIAL    ! 2024.1    !
          USE COMPACTION2_MOD    , ONLY : COMPACTION2    !  20    !  COMPACTION2    ! 2025.1    !
          USE COMPACTION_TAB_MOD , ONLY : COMPACTION_TAB !  21    !  COMPACTION_TAB ! 2026.0    !
      !--------------------------------------------------!--------!-----------------!-----------!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
           integer,intent(in) :: sbufmat
           integer,intent(in) :: snpf, stf
           integer,intent(in) :: npf(snpf)
           real(kind=WP),intent(in) :: tf(stf)
           real(kind=WP),intent(in) :: time
           real(kind=WP), intent(in) :: bufmat(sbufmat)
           real(kind=WP), intent(in) :: PMIN, OFF(1), EINT, MU, ESPE, DVOL, DF, V, PSH, RHO0, V0
           real(kind=WP), intent(out) :: P, DPDM, DPDE, TEMP
           real(kind=WP), intent(inout) :: VAREOS(1,6)
           real(kind=WP), intent(in) :: DT
           type(eos_param_),intent(in) :: eos_struct !< data structure for EoS parameters            
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
           integer :: eostyp
           integer :: isfluid
           integer :: iflag                 !< eos flag
           integer, parameter :: nel = 1    !< array size for eos subroutine
           integer :: nvareos               !< extra elem variables for EoS.
           real(kind=WP),DIMENSION(1) :: EINT_,MU_,MU2_,ESPE_,DVOL_,DF_,V_,PSH_,P_,POLD_,DPDM_,DPDE_,RHO0_,RHO_,TEMP_
           real(kind=WP) :: tmax
           !real(kind=WP) :: Cp, Cv, mcv, qheat, dtemp
           integer, parameter :: nvartmp = 3
           integer :: vartmp(nel, nvartmp)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
           eostyp = eos_struct%eostype
           iflag=2
           nvareos=6 !can be automatically set if BUFEER LAYER enabled for law51 (heavy work)
           tmax = fifteen * ep03

           EINT_(1) = EINT
           MU_(1) = MU
           MU2_(1) = MU*MAX(MU,ZERO)
           ESPE_(1) = ESPE
           DVOL_(1) = DVOL
           DF_(1) = DF
           V_(1) = V
           PSH_(1) = PSH
           RHO0_(1) = RHO0
           RHO_(1) = RHO0*(ONE+MU)
           P_(1) = P
           POLD_(1) = P
           DPDM_(1) = DPDM
           DPDE_(1) = DPDE

           select case (EOSTYP)
             case (1)
               !--------------------------------------------------!
               !         POLYNOMIAL EOS                           !
               !--------------------------------------------------!
               call eospolyno( &
                      iflag   ,nel    ,pmin  ,off        ,eint_     ,mu_   ,mu2_      , &
                      espe_   ,dvol_  ,df_   ,v_         ,psh_      , &
                      p_      ,dpdm_  ,dpde_ ,eos_struct)

             case (2)
               !--------------------------------------------------!
               !         GRUNEISEN EOS                            !
               !--------------------------------------------------!
               call gruneisen( &
                      iflag  ,nel     ,pmin    ,off     ,eint_     ,mu_  ,mu2_, &
                      espe_  ,dvol_   ,df_     ,v_      ,rho0_     , &
                      p_     ,dpdm_   ,dpde_   ,psh_    ,eos_struct)

             case (3)
              !--------------------------------------------------!
              !         TILLOTSON EOS                            !
              !--------------------------------------------------!
              call tillotson( &
                     iflag   ,nel     ,pmin    ,off    ,eint_  ,mu_  ,mu2_, &
                     espe_   ,dvol_   ,df_     ,v_     ,psh_   , &
                     p_      ,dpdm_   ,dpde_   ,vareos ,nvareos,eos_struct)

             case (4)
               !--------------------------------------------------!
               !         PUFF EOS                                 !
               !--------------------------------------------------!
               call puff( &
                      iflag   ,nel     ,pmin    ,off       ,eint_  ,mu_  ,mu2_, &
                      espe_   ,dvol_   ,df_     ,v_        , &
                      p_      ,dpdm_   ,dpde_   ,eos_struct)

             case (5)
               !--------------------------------------------------!
               !      SESAME EOS                                  !
               !--------------------------------------------------!
               temp_(1) = temp
               call sesame( &
                      iflag    ,nel    ,pmin   ,off     ,eint_  ,rho_ ,rho0_ , &
                      espe_    ,dvol_  ,p_     ,dpdm_   ,dpde_, &
                      temp_    ,bufmat ,eos_struct)

             case(6)
               !--------------------------------------------------!
               !         NOBLE-ABEL EOS (COVOLUME EOS)            !
               !--------------------------------------------------!
                call noble_abel( &
                       iflag  , nel   ,off   ,eint_ ,mu_  , &
                       espe_  , dvol_ ,df_   ,v_    ,psh_ , &
                       p_     , dpdm_ ,dpde_ ,eos_struct)

             case (7)
               !--------------------------------------------------!
               !         IDEAL GAS EOS                            !
               !--------------------------------------------------!
               call idealgas( &
                      iflag  ,nel   ,off   ,eint_ ,mu_  , &
                      espe_  ,dvol_ ,df_   ,v_        ,psh_ , &
                      p_     ,dpdm_ ,dpde_ ,eos_struct )

             case(8)
               !--------------------------------------------------!
               !         MURNAGHAN EOS                            !
               !--------------------------------------------------!
                call murnaghan( &
                       iflag  ,nel    ,pmin  ,off         ,eint_ ,mu_  , &
                       dvol_  ,v_     ,psh_  , &
                       p_     ,dpdm_  ,dpde_ ,eos_struct)

             case(9)
               !--------------------------------------------------!
               !         osborne eos                              !
               !--------------------------------------------------!
                call osborne( &
                       iflag   ,nel    ,pmin  ,off    ,eint_ ,mu_  , &
                       espe_   ,dvol_  ,df_   ,v_     ,rho0_ ,psh_ , &
                       p_      ,dpdm_  ,dpde_ ,eos_struct)
               
             case (10)
               !--------------------------------------------------!
               !         STIFFENED GAS EOS                        !
               !--------------------------------------------------!
               call stiffgas( &
                      iflag  , nel    ,pmin  ,off        ,eint_ ,mu_  ,mu2_, &
                      espe_  , dvol_  ,df_   ,v_         ,psh_ , &
                      p_     , dpdm_  ,dpde_ ,eos_struct )

             case(11)
               !--------------------------------------------------!
               !         LSZK EOS                                 !
               !--------------------------------------------------!
                call lszk( &
                       iflag  , nel    ,pmin  ,off  ,eint_  ,mu_  , &
                       espe_  , dvol_  ,df_   ,v_   ,psh_   , &
                       p_     , dpdm_  ,dpde_ ,eos_struct)

             case(13)
               !--------------------------------------------------!
               !         COMPACTION EOS                           !
               !--------------------------------------------------!
                call compaction( &
                       iflag ,nel   ,pmin ,off   ,eint_  ,mu_  ,mu2_, &
                       dvol_  ,psh_  , &
                       p_  ,dpdm_  ,dpde_ ,vareos, &
                       eos_struct)

             case(14)
               !--------------------------------------------------!
               !         NASG EOS                                 !
               !--------------------------------------------------!
                call nasg( &
                       iflag  , nel    ,pmin   ,off      ,eint_ ,mu_  , &
                       espe_  , dvol_  ,v_     ,psh_     , &
                       p_     , dpdm_  ,dpde_ ,eos_struct)

             case(16)
               !--------------------------------------------------!
               !         IDEAL GAS VT EOS                         !
               !--------------------------------------------------!
                call idealgas_vt( &
                       iflag ,nel  ,pmin ,off     ,eint_ ,mu_  ,mu2_, &
                       espe_  ,dvol_ ,df_   ,v_    ,psh_  ,rho0_,rho_, &
                       p_  ,dpdm_ ,dpde_ ,temp_ ,pold_  ,eos_struct)

             case(17)
               !--------------------------------------------------!
               !         TABULATED EOS                            !
               !--------------------------------------------------!
                call tabulated( &
                       iflag   , nel    ,pmin  ,off  ,eint_ ,mu_  , &
                       espe_   , dvol_  ,df_   ,v_   ,psh_  , &
                       p_      , dpdm_  ,dpde_ , &
                       npf     , tf     ,snpf  ,stf  ,eos_struct)

             case (18)
               !--------------------------------------------------!
               !         LINEAR EOS                               !
               !--------------------------------------------------!
               call eoslinear( &
                      iflag   ,nel       ,pmin    ,off    ,eint_  ,mu_  , &
                      dvol_   ,v_        ,psh_    ,p_     ,dpdm_, &
                      dpde_   ,eos_struct, eos_struct%uparam(1), eos_struct%uparam(2))

             case (19)
               !--------------------------------------------------!
               !         EXPONENTIAL EOS                          !
               !--------------------------------------------------!
               call eosexponential( &
                      iflag   ,nel    ,off    ,eint_ , &
                      dvol_   ,v_     ,psh_   , &
                      p_      ,dpdm_  ,dpde_  ,time  , &
                      eos_struct)

             case(20)
               !--------------------------------------------------!
               !         COMPACTION EOS                           !
               !--------------------------------------------------!
                call compaction2( &
                       iflag ,nel  ,pmin   ,off   ,eint_  ,mu_   , &
                       dvol_ ,psh_  , &
                       p_  ,dpdm_ ,dpde_ ,nvareos ,vareos, &
                       npf   ,tf   ,snpf ,stf   , &
                       eos_struct)

             case(21)
               !--------------------------------------------------!
               !         COMPACTION_TAB EOS                       !
               !--------------------------------------------------!
               vartmp(:,:)=1
               call compaction_tab( &
                      iflag   ,nel    ,pmin     ,off    ,eint_ , &
                      dvol_   ,psh_   ,dt       ,rho_   ,rho0_ , &
                      p_      ,dpdm_  ,dpde_    , &
                      nvareos ,vareos ,nvartmp  ,vartmp, &
                      eos_struct )

             case default
             !--------------------------------------------------!
             !        UNSUPPORTED
             !--------------------------------------------------!          
               P_(1) = zero
               DPDM_(1) = zero
               DPDE_(1) = zero
               PSH_(1) = zero
               
           end select
           
           P = P_(1)
           DPDM = abs(DPDM_(1)) ! stability
           DPDE = DPDE_(1)

           !-------------------ISENTROPIC TEMPERATURE CHANGE--------------------
           isfluid = eos_struct%isfluid
           if(eostyp == 5)then
             temp = temp_(1)
           elseif(isfluid == 0 .AND. ANALY_TEMP == 0)then
             if(mu>zero)then
               temp = temp-temp*dpde*dvol/v0
             end if
           else
             temp = temp-temp*dpde*dvol/v0
           end if


           ! IS DONE IN MMAIN AFTER CALL OF MULAW (using global viscosity Q[n] and Q[n+1])
           !
           !  !------------------SHOCK INDUCED TEMPERATURE CHANGE
           !  ! --- TEMPERATURE UPDATE (SHOCK-INDUCED ENTROPY) ---!
           !  ! retrieving Cv parameter
           !  cv = eos_struct%cv
           !  if(cv == zero)then
           !    cp = mat_elem%mat_param(imat)%therm%rhocp / mat_elem%mat_param(imat)%rho0
           !    cv = cp !hypothesis if eos did not provide cv
           !  end if
           !  ! temperature dT = Q/mcv
           !  if(cv > zero)then
           !    if(off(1) == one) then
           !      mcv=lbuf%rho(i)*voln(i)*cv
           !      qheat = -half*(qold(i)+lbuf%qvis(i))*dvol(i) !2nd order integration
           !      dtemp = qheat/mcv ! heat related to entropy deposit
           !      temp = temp + dtemp
           !    end if
           !  end if

           temp = max(temp, zero)
           temp = min(temp, tmax)
           
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine eosmain51
      end module eosmain51_mod
