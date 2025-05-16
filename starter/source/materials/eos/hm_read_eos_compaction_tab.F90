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
      !||    hm_read_eos_compaction_tab_mod   ../starter/source/materials/eos/hm_read_eos_compaction_tab.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_eos                      ../starter/source/materials/eos/hm_read_eos.F
      !||====================================================================
      module hm_read_eos_compaction_tab_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Compaction EoS Reader (/EOS/COMPACTION)
!! \details  RHOI = PM(89)   -> provided by /MAT
!! \details  RHOR = PM(01)   -> provided by /MAT (can be erased by EOS if present : obsolete)
!! \details  PM(31) = P(MU0,E0) -> will be used to initialize diagonal of stress tensor SIG(1:3,*)
      !||====================================================================
      !||    hm_read_eos_compaction_tab   ../starter/source/materials/eos/hm_read_eos_compaction_tab.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_eos                  ../starter/source/materials/eos/hm_read_eos.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                       ../starter/source/output/message/message.F
      !||    eos_table_copy               ../starter/source/materials/tools/eos_table_copy.F90
      !||    hm_get_floatv                ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_floatv_dim            ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
      !||    hm_get_intv                  ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted       ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||    table_mat_vinterp            ../starter/source/materials/tools/table_mat_vinterp.F
      !||--- uses       -----------------------------------------------------
      !||    elbuftag_mod                 ../starter/share/modules1/elbuftag_mod.F
      !||    eos_table_copy_mod           ../starter/source/materials/tools/eos_table_copy.F90
      !||    message_mod                  ../starter/share/message_module/message_mod.F
      !||    submodel_mod                 ../starter/share/modules1/submodel_mod.F
      !||    table_mat_vinterp_mod        ../starter/source/materials/tools/table_mat_vinterp.F
      !||    table_mod                    ../starter/share/modules1/table_mod.F
      !||====================================================================
      subroutine hm_read_eos_compaction_tab(iout,pm,unitab,lsubmodel,uid,eos_tag,ieos,npropm, &
                                            maxeos,eos_param,ntable,table)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use message_mod
      use unitab_mod , only : unit_type_
      use submodel_mod , only : nsubmod, submodel_data
      use elbuftag_mod , only : eos_tag_
      use constant_mod , only : zero, em20, em12, em10, half, two_third, one, two, three, three100, ep10, ep20
      use eos_param_mod , only : eos_param_
      use table_mod , only : ttable
      use eos_table_copy_mod , only : eos_table_copy
      use table_mat_vinterp_mod , only : table_mat_vinterp
      use names_and_titles_mod , only : ncharline
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer,intent(in) :: npropm, maxeos  !< array sizes
      type (unit_type_),intent(in) ::unitab !< data structure for units (/UNIT)
      integer, intent(in) :: iout !< file units
      my_real, intent(inout) :: pm(npropm)  !< data structure for material laws
      type(submodel_data), dimension(nsubmod), intent(in) :: lsubmodel !< data structure for sumobeling method (//SUBMODEL)
      integer,intent(in) :: uid
      type(eos_tag_),dimension(0:maxeos) ,intent(inout) :: eos_tag !< data structure for EoS
      integer,intent(in) :: ieos !< EoS (internal) identifier
      type(eos_param_), intent(inout) :: eos_param !< eos data structure (specific parameters)
      integer, intent(in) :: ntable
      type(ttable) ,dimension(ntable) ,intent(in) :: table
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      my_real :: p0, psh, rhoi,rhor
      my_real :: ssp0
      logical :: is_encrypted, is_available
      integer :: P_FUNC_ID, C_FUNC_ID, G_FUNC_ID !< user function identifer
      integer :: IPLAS
      integer :: iform
      integer :: ierror
      integer :: npt
      my_real :: PSCALE, CSCALE, GSCALE
      my_real :: GAMMA_TMD,RHO_TMD, C_SOLID
      my_real :: density_unit
      my_real :: rhomax_plastic

      my_real :: x1scale, x2scale, x3scale, x4scale
      my_real :: x2vect(3),x3vect(3),x4vect(3)
      my_real :: fscale(3)

      my_real :: slope_end

      my_real :: puser, ff, df, rho_, tol, residu  !< variable for newton iteration
      integer :: niter, iter !< variable for newton iteration

      my_real, dimension(1,1) :: xvec1 !<temporary array for table interpolation

      integer :: vartmp(1,3)
      my_real :: slope(1)
      my_real :: yy(1)

      integer ipt

      character(len=ncharline) :: mesg2

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      is_encrypted = .false.
      is_available = .false.
      vartmp(1,1:3) = 1

      eos_tag(ieos)%g_mu = 1
      eos_tag(ieos)%l_mu = 1
      eos_tag(ieos)%nvar = 5   !  --> elbuf%bufly%eos%nvar      (LAMBDA, C*C, Pnew, GAMMA, Pc)
      eos_tag(ieos)%nvartmp = 3
           
      call hm_option_is_encrypted(is_encrypted)

      call hm_get_intv('P_FUNC', P_FUNC_ID, is_available,lsubmodel)
      call hm_get_floatv('PSCALE', Pscale, is_available,lsubmodel,unitab)
      call hm_get_intv('c_FUNC', C_FUNC_ID, is_available,lsubmodel)
      call hm_get_floatv('cSCALE', Cscale, is_available,lsubmodel,unitab)
      call hm_get_intv('G_FUNC', G_FUNC_ID, is_available,lsubmodel)
      call hm_get_floatv('GSCALE', Gscale, is_available,lsubmodel,unitab)

      call hm_get_floatv('RHO_TMD', rho_tmd, is_available,lsubmodel,unitab)
      call hm_get_floatv('C_SOLID', c_solid, is_available,lsubmodel,unitab)

      call hm_get_intv('IPLAS', IPLAS, is_available,lsubmodel)

      !dimension / unit
      call hm_get_floatv_dim('RHO_TMD',density_unit,is_available,lsubmodel,unitab)

      rhor = pm(1)
      rhoi = pm(89)
      psh = zero

      if(rho_tmd <= zero)then
         call ancmsg(MSGID=67,MSGTYPE=msgerror,ANMODE=aninfo,I1=uid,C1='/EOS/COMPACTION_TAB',C2='RHO_TMD MUST BE POSITIVE')
      endif

      if(rho_tmd <= rhoi)then
         call ancmsg(MSGID=67,MSGTYPE=msgerror,ANMODE=aninfo,I1=uid,C1='/EOS/COMPACTION_TAB',&
                                                                    C2='RHO_TMD MUST BE GREATER THAN INITIAL DENSITY')
      endif

      if(c_solid <= zero)then
         call ancmsg(MSGID=67,MSGTYPE=msgerror,ANMODE=aninfo,I1=uid,C1='/EOS/COMPACTION_TAB',C2='C_SOLID MUST BE POSITIVE')
      endif

      if(PSCALE == zero) PSCALE = one

      if(cSCALE == zero) cSCALE = one

      if(GSCALE == zero)then
        if(G_FUNC_ID > 0)GSCALE = one  !zero is linear unloading
      else
        ! linear unloading with Gscale=0.0
      end if

      !Default values
      ! Iform : formulation flag for unload behavior
      !  1: linear (if gamma function not provided)
      !  2: non linear (if gamma function provided)
      if(G_FUNC_ID > 0 .and. gscale > em10)then
        iform=2
      else
        iform=1
      endif

      !generate user function in data structure
      eos_param%ntable = 3
      allocate (eos_param%table(eos_param%ntable))
      eos_param%table(1)%notable = P_FUNC_ID
      eos_param%table(2)%notable = C_FUNC_ID
      eos_param%table(3)%notable = G_FUNC_ID
      x1scale   = one * density_unit
      x2scale   = one * density_unit
      x3scale   = one
      x4scale   = one
      x3vect(1) = zero
      x4vect(1) = zero
      fscale(1) = PSCALE
      fscale(2) = CSCALE
      fscale(3) = GSCALE
      call eos_table_copy(eos_param ,x2vect   ,x3vect   ,x4vect   ,         &
                x1scale  ,x2scale  ,x3scale  ,x4scale  ,fscale   ,         &
                ntable   ,table    ,ierror   ,uid)

      ! check mathematical meaning of gamma function (gamma=gscale*g(rho) must be positive)
      if(G_FUNC_ID > 0 .and. gscale /= zero)then
        NPT = size(eos_param%table(3)%X(1)%VALUES)
        ierror = 0
        do ipt =1,npt
          YY(1) = gscale * eos_param%table(3)%Y1D(ipt)
          IF (YY(1) < zero) then
            ierror = 1
            exit
          endif
        end do
        if(ierror == 1)then
          call ancmsg(MSGID=67,MSGTYPE=msgerror,ANMODE=aninfo,I1=uid,C1='/EOS/COMPACTION_TAB', &
                      C2='GAMMA(RHO) MUST BE POSITIVE')
        end if
      end if

      ! GAMMA(RHO_TMD) -> 0 ?
      gamma_tmd = zero
      if(G_FUNC_ID > 0 .and. gscale /= zero)then
         xvec1(1:1,1) = rho_tmd
         call table_mat_vinterp(eos_param%table(3),1,1,vartmp(1,3),xvec1,yy,slope)
         gamma_tmd = yy(1)
         if(gamma_tmd > em10)then
          call ancmsg(MSGID=67,MSGTYPE=msgwarning,ANMODE=aninfo,I1=uid,C1='/EOS/COMPACTION_TAB', &
                      C2='FUNCTION GAMMA(RHO) SHOULD DECAY TO 0.0 WHEN DENSITY APPROACHES RHO_TMD')
         end if
      endif

      !Iplas check and default (plastic expansion flag)
      if(IPLAS /= 0 .and. IPLAS /= 1)then
        IPLAS = 0
      end if

      !default initial temperature
      if(pm(79)==zero)pm(79)=three100

      ! P0=P(rho0)
      xvec1(1:1,1) = rhoi
      call table_mat_vinterp(eos_param%table(1),1,1,vartmp(1,1),xvec1,yy,slope)
      P0 = yy(1)
      ! SSP0 = Cunload(rho_0)
      call table_mat_vinterp(eos_param%table(2),1,1,vartmp(1,2),xvec1,yy,slope)
      SSP0 = yy(1)

      ! RHOMAX_PLASTIC
      ! intersection between
      !  1) the unloading curve (slope c_solid² crossing (rho_tmd,0)) :  eqn : yy1 = Puser(xx) -> automaticaly extrapolated if needed
      !  2) and the compaction curve : yy(rho)                           eqn : yy2 = c_solid**2*xx - c_solid**2*rho_tmd
      ! newton iteration :  solving f(xx) := Pc(xx) - c_solid**2*(xx-rho_tmd) = 0
      NITER = size(eos_param%table(1)%x(1)%values) + 100 ! large value but not inside a loop
      ITER = 0
      TOL = em12
      rho_ = rho_tmd
      residu = ep20
      DO WHILE (ITER <= NITER .AND. RESIDU > TOL)
        xvec1(1:1,1) = rho_
        call table_mat_vinterp(eos_param%table(1),1,1,vartmp(1,1),xvec1,yy,slope)
        Puser = yy(1)
        ff = Puser - c_solid**2 * (rho_-rho_tmd)
        df = slope(1) - c_solid**2
        rho_ = rho_ - ff/df
        residu = abs(ff/df)
      END DO
      rhomax_plastic = rho_ ! intersection point

      ! check pysical meaning : intersection point between compaction curve vs TMD line, dp/dr < c_solid**2
      slope_end = slope(1)
      if(slope_end > c_solid**2)then
        mesg2=''
        write(mesg2, '(A,A,F10.3,A)') 'UNLOAD MODULUS C_SOLID**2 TOO LOW REGARDING COMPACTION CURVE. ',&
        'CHECK P(RHO) SLOPE AT POINT RHO = RHOMAX_PLASTIC (', rhomax_plastic,')'
        call ancmsg(MSGID=67,MSGTYPE=msgerror,ANMODE=aninfo,I1=uid,C1='/EOS/COMPACTION_TAB',C2=mesg2)
      end if


      !integer parameters
      eos_param%nuparam = 5
      eos_param%niparam = 2
      eos_param%nfunc = 0
      !eos_param%ntable = 3
      call eos_param%construct() !allocations

      !real parameters
      eos_param%uparam(1) = rho_tmd
      eos_param%uparam(2) = c_solid
      eos_param%uparam(3) = PSH
      eos_param%uparam(4) = RHOMAX_PLASTIC
      eos_param%uparam(5) = gamma_tmd !user may want to have a non zero value even if 0 is recommended
                                      !used when rho>rhomax_plastic to avoid any useless interpolation of gamma function

      !integer parameters
      eos_param%iparam(1) = iform
      eos_param%iparam(2) = IPLAS

      !ssp0
      pm(27) = max(ssp0, pm(27))
      pm(23) = zero ! e0
      pm(31) = p0-psh
      pm(104)= p0-psh

      write(iout,1000)
      if(is_encrypted)then
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      else
        write(iout,1500) RHO_TMD,C_SOLID,IPLAS
        write(iout,2000) P_FUNC_ID, PSCALE
        write(iout,2500) c_FUNC_ID, cSCALE
        if(iform==1)then
          !linear unloading
          write(iout,2501)
        elseif(iform==2)then
          !non linear unloading
          write(iout,2502)
          write(iout,2600) G_FUNC_ID, GSCALE
        endif
        write(iout,3000) rhomax_plastic
      endif

      return
! ----------------------------------------------------------------------------------------------------------------------

 1000 format(&
      5X,'  COMPACTION TABULATED EOS    ',/,&
      5X,'  ------------------------    ',/)
 1500 format(&
      5X,'RHO_TMD . . . . . . . . . . . . . . . . =',E12.4/&
      5X,'C_SOLID . . . . . . . . . . . . . . . . =',E12.4/&
      5X,'PLASTIC EXPANSION FLAG . . . . . . . .  =',I10)
 2000 format( &
      5X,'PLASTIC COMPACTION CURVE RHO_C(P_C) :',/ &
      5X,'  FUNCTION ID . . . . . . . . . . . . . =',I10/ &
      5X,'  SCALE FACTOR  . . . . . . . . . . . . =',E12.4)
 2500 format( &
      5X,'ELASTIC UNLOADING CURVE RHO_UNLOAD(C_UNLOAD) :',/ &
      5X,'  FUNCTION ID . . . . . . . . . . . . . =',I10/ &
      5X,'  SCALE FACTOR  . . . . . . . . . . . . =',E12.4)
 2501 format(&
      5X,'UNLOADING MODEL : LINEAR')
 2502 format(&
      5X,'UNLOADING MODEL : NON-LINEAR')
 2600 format( &
      5X,'UNLOADING SHAPE FACTOR GAMMA(RHO) :',/ &
      5X,'  FUNCTION ID . . . . . . . . . . . . . =',I10/ &
      5X,'  SCALE FACTOR  . . . . . . . . . . . . =',E12.4)
 3000 format( &
      5X,'COMPUTED FROM COMPACTION CURVE :',/ &
      5X,'  RHOMAX_PLASTIC. . . . . . . . . . . . =',E12.4/)


      end subroutine hm_read_eos_compaction_tab
! ----------------------------------------------------------------------------------------------------------------------

      end module hm_read_eos_compaction_tab_mod
