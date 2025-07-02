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
      !||    granular51_mod   ../engine/source/materials/mat/mat051/granular51.F90
      !||--- called by ------------------------------------------------------
      !||    sigeps51         ../engine/source/materials/mat/mat051/sigeps51.F90
      !||====================================================================
      module granular51_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
      !||====================================================================
      !||    granular51              ../engine/source/materials/mat/mat051/granular51.F90
      !||--- called by ------------------------------------------------------
      !||    sigeps51                ../engine/source/materials/mat/mat051/sigeps51.F90
      !||--- calls      -----------------------------------------------------
      !||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod            ../common_source/modules/constant_mod.F
      !||    matparam_def_mod        ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    precision_mod           ../common_source/modules/precision_mod.F90
      !||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
      !||====================================================================
      subroutine granular51 &
                (nel    ,sigd   ,vol      ,epseq  , &
                 deps   ,uparam ,volume   ,eint   , plas    , &
                 uvar   ,nuvar  ,kk       ,rho0   , &
                 pfrac  ,pp     ,gg1      , &
                 off    ,pext   ,timestep ,de     , nummat , matparam , &
                 nvartmp  ,vartmp,vfrac)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
        use precision_mod, only : WP
        use constant_mod , only : zero, em20, em15, em14, em02, half, one, onep333, two, three
        use matparam_def_mod , only : matparam_struct_
        use table_mat_vinterp_mod , only : table_mat_vinterp
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer,intent(in) :: nel
      integer,intent(in) :: nummat
      real(kind=wp), intent(in) :: rho0
      real(kind=wp), intent(in) :: pfrac !< fracture pressure (pfrac <= 0)
      real(kind=wp), intent(in) :: vfrac(nel) !< volume fractions
      real(kind=wp), intent(in) :: off(nel)
      real(kind=wp), intent(in) :: deps(6,nel)
      real(kind=wp), intent(in) :: vol(nel),uparam(*) ,volume(nel)
      real(kind=wp), intent(in) :: timestep, pext , de(nel)
      real(kind=wp), intent(inout) :: plas(nel)                !< accumulated plastic strain
      real(kind=wp),intent(inout) :: sigd(6,nel),pp(nel),eint(nel)
      real(kind=wp),intent(inout) :: gg1(nel) !< shear modulus (variable with granular constitutive model)
      type(matparam_struct_),intent(in),dimension(nummat) :: matparam
      integer ,intent(in) :: nvartmp                       !< number of temporary internal variables
      integer ,dimension(nel,nvartmp) ,intent(inout) :: vartmp    !< temporary internal variables
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
      integer :: i,kk,nuvar
      real(kind=wp) :: fact,fac2
      real(kind=wp) :: uvar(nel,nuvar)
      real(kind=wp) :: mas, ptot
      real(kind=wp) :: young, nu
      real(kind=wp) :: pold(nel)
      real(kind=wp) :: t1(nel), t2(nel), t3(nel), t4(nel), t5(nel), t6(nel)
      real(kind=wp) :: p(nel), ratio(nel)
      real(kind=wp) :: g(nel), g43(nel), g0(nel), g2(nel)
      real(kind=wp) :: j2(nel),yield2(nel), epseq(nel)
      real(kind=wp) :: rho_new(nel), rho_old(nel),vnew(nel),sigdo(6,nel)
      real(kind=wp) :: einc,vol_avg,vm

      real(kind=WP), dimension(nel,1) :: xvec1 !<temporary array for table interpolation
      real(kind=WP) :: slope(nel,1) !<required for table interpolation
      integer :: mid
      logical, parameter :: opt_extrapolate = .false.
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      YOUNG = UPARAM(02)
      MID   = INT(UPARAM(14))
      NU    = UPARAM(22)

      ! if(timestep==zero)return  !law3 is treeting this also on NC=0   (Einc must be added )
      DO I=1,NEL
        POLD(I)    = UVAR(I,18+KK)
        RHO_OLD(I) = UVAR(I,12+KK)
        VNEW(I)    = UVAR(I,1+KK)*VOLUME(I) -TIMESTEP*UVAR(I,13+KK)
        VNEW(I)    = MIN(MAX(ZERO,VNEW(I)),VOLUME(I))
        IF(VNEW(I)>EM15)THEN
          RHO_NEW(I) = UVAR(I,9+KK) / VNEW(I)  !MASS/VOLUME
        ELSE
          RHO_NEW(I)=RHO_OLD(I)
        ENDIF
      ENDDO

      !========================================================================
      !< Recovering Yield surface value Y = Y(P)
      !========================================================================
      !
      xvec1(1:nel,1) = Pold(1:nel)
      ! matparam%table(1) : G(rho) function
      ! ivartmp(1,1) : backup index poisition to optimize search during next cycle
      ! xvec1 : abscissa for table interpolation (in)
      ! g : ordinate for table interpolation (out)
      ! slope : slope for table interpolation (out)
      call table_mat_vinterp(MATPARAM(MID)%table(2),nel,nel,vartmp(1,1),xvec1,g0,slope,opt_extrapolate)


      !========================================================================
      !< Recovering shear modulus for each element : G = G(rho)
      !========================================================================
      !
      xvec1(1:nel,1) = rho_new(1:nel)
      ! matparam%table(1) : G(rho) function
      ! ivartmp(1,1) : backup index poisition to optimize search during next cycle
      ! xvec1 : abscissa for table interpolation (in)
      ! shear : ordinate for table interpolation (out)
      ! slope : slope for table interpolation (out)
      call table_mat_vinterp(MATPARAM(MID)%table(1),nel,nel,vartmp(1,1),xvec1,g,slope,opt_extrapolate)
      g2(1:nel) = two*g(1:nel)
      g43(1:nel)  = onep333*g(1:nel)
      gg1(1:nel) = g2(1:nel)

      DO I=1,NEL
        MAS     = UVAR(I,9+KK)
        IF(MAS < EM20)THEN
          RHO_NEW(I) = RHO0
        ENDIF

        T1(I)=SIGD(1,I)
        T2(I)=SIGD(2,I)
        T3(I)=SIGD(3,I)
        T4(I)=SIGD(4,I)
        T5(I)=SIGD(5,I)
        T6(I)=SIGD(6,I)

        FAC2 = ONE
        IF(POLD(I) < PFRAC)THEN
          P(I)  = PFRAC
          FAC2 = ZERO
        ENDIF
        PP(I) = P(I)

        SIGDO(1:6,I) = SIGD(1:6,I) * OFF(I)
        FACT = VFRAC(I)
        T1(I) = T1(I) + G2(I)* (DEPS(1,I)-DE(I))*FACT
        T2(I) = T2(I) + G2(I)* (DEPS(2,I)-DE(I))*FACT
        T3(I) = T3(I) + G2(I)* (DEPS(3,I)-DE(I))*FACT
        T4(I) = T4(I) + G(I) * DEPS(4,I)*FACT
        T5(I) = T5(I) + G(I) * DEPS(5,I)*FACT
        T6(I) = T6(I) + G(I) * DEPS(6,I)*FACT

        J2(I)=HALF*(T1(I)**2+T2(I)**2+T3(I)**2)+T4(I)**2+T5(I)**2+T6(I)**2
        VM=SQRT(THREE*J2(I))
        PTOT = POLD(I)+PEXT
        G0(I)= MAX(ZERO,G0(I))
        IF(POLD(I) < PFRAC) G0(I) = ZERO
        YIELD2(I)=VM-G0(I)

        RATIO(I)=ZERO
        IF(YIELD2(I) <= ZERO .AND. G0(I) > ZERO)THEN
          RATIO(I)=ONE
        ELSE
          RATIO(I)=SQRT(G0(I)/(vm+EM14))
        ENDIF

        ! deviatoric stress
        SIGD(1,I)=RATIO(I)*T1(I)*OFF(I)
        SIGD(2,I)=RATIO(I)*T2(I)*OFF(I)
        SIGD(3,I)=RATIO(I)*T3(I)*OFF(I)
        SIGD(4,I)=RATIO(I)*T4(I)*OFF(I)
        SIGD(5,I)=RATIO(I)*T5(I)*OFF(I)
        SIGD(6,I)=RATIO(I)*T6(I)*OFF(I)

        !if(UVAR(I,1+KK)>EM02)then
          plas(I)  = plas(I) +(ONE -RATIO(I))*VM  /MAX(THREE*G(I),EM15)
          epseq(i) = epseq(I)+(ONE -RATIO(I))*VM  /MAX(THREE*G(I),EM15)
        !endif

         VOL_AVG = HALF*(VFRAC(I)*VOLUME(I)+VOL(I))
         EINC    = HALF*VOL_AVG* &
                             ( (SIGDO(1,I)+SIGD(1,I)) * DEPS(1,I) &
                             + (SIGDO(2,I)+SIGD(2,I)) * DEPS(2,I) &
                             + (SIGDO(3,I)+SIGD(3,I)) * DEPS(3,I) &
                             + (SIGDO(4,I)+SIGD(4,I)) * DEPS(4,I) &
                             + (SIGDO(5,I)+SIGD(5,I)) * DEPS(5,I) &
                             + (SIGDO(6,I)+SIGD(6,I)) * DEPS(6,I))
          EINT(I) = EINT(I) + EINC

      ENDDO !next I

      RETURN
      END subroutine granular51
      END module granular51_mod


