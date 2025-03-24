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
      !||    hm_read_eos_compaction2_mod   ../starter/source/materials/eos/hm_read_eos_compaction2.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_eos                   ../starter/source/materials/eos/hm_read_eos.F
      !||====================================================================
      module hm_read_eos_compaction2_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Compaction EoS Reader (/EOS/COMPACTION)
!! \details  RHOI = PM(89)   -> provided by /MAT
!! \details  RHOR = PM(01)   -> provided by /MAT (can be erased by EOS if present : obsolete)
!! \details  => MU0 = RHO/RHOR-1.
!! \details  PM(31) = P(MU0,E0) -> will be used to initialize diagonal of stress tensor SIG(1:3,*)
      !||====================================================================
      !||    hm_read_eos_compaction2   ../starter/source/materials/eos/hm_read_eos_compaction2.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_eos               ../starter/source/materials/eos/hm_read_eos.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                    ../starter/source/output/message/message.F
      !||    finter                    ../starter/source/tools/curve/finter.F
      !||    hm_get_floatv             ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_intv               ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted    ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||--- uses       -----------------------------------------------------
      !||    elbuftag_mod              ../starter/share/modules1/elbuftag_mod.F
      !||    message_mod               ../starter/share/message_module/message_mod.F
      !||    submodel_mod              ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      subroutine hm_read_eos_compaction2(iout,pm,unitab,lsubmodel,imideos,eos_tag,ieos,npropm,maxeos,&
                                          eos_param, iunit, nfunc, npc, tf ,snpc ,npts )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use message_mod
      use unitab_mod , only : unit_type_
      use submodel_mod , only : nsubmod, submodel_data
      use elbuftag_mod , only : eos_tag_
      use constant_mod , only : zero, em12, two_third, one, two, three, three100, ep20
      use eos_param_mod , only : eos_param_
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
      integer,intent(in) :: imideos
      type(eos_tag_),dimension(0:maxeos) ,intent(inout) :: eos_tag !< data structure for EoS
      integer,intent(in) :: ieos !< EoS (internal) identifier
      type(eos_param_), intent(inout) :: eos_param !< eos data structure (specific parameters)
      integer,intent(in) :: iunit !< unit identifier
      integer,intent(in) :: snpc, npts, nfunc
      integer,intent(in) :: npc(snpc)
      my_real,intent(in) :: tf(npts)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      my_real  p0, psh, rho0,rhoi,rhor
      my_real  mumin,mumax
      my_real  mu0,ssp0, dpdmu
      integer iform
      logical :: is_encrypted, is_available, is_available_rho0

      integer :: P_FUNC_ID !< user function identifer
      integer :: jfunc !< loop
      my_real :: Fscale, Xscale !< function scale factors
      my_real :: bmin, bmax !< unload modulus
      my_real :: dpdm0 !< total derivative at initial time
      my_real :: tmp, dpdmu_mumax, dpdmu_mumin

      my_real :: FAC_M,FAC_L,FAC_T,FAC_PRES !< factors for unit translation (case iunit > 0)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External
! ----------------------------------------------------------------------------------------------------------------------
      my_real, external :: finter
! In order to get : y <- f(x)  and dydx <- f'(x) :
! Use :             y = FINTER(func_id,x,NPF,TF,dydx)
!                       where - NPF,TF are constant arrays automatically built by Starter (/FUNCT data).
!                               NPF contains the cursors and number of points for each function.
!                               TF contains the abscissa and ordinate values of the functions.
!                             - func_id is internal identifier (example : first function to be read /FUNCT/999 has user id #999 and internal id #1)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      is_encrypted = .false.
      is_available = .false.
      is_available_rho0 = .false.
      iform=0

      eos_tag(ieos)%g_mu = 1
      eos_tag(ieos)%l_mu = 1
           
      call hm_option_is_encrypted(is_encrypted)

      call hm_get_intv('P_FUNC', P_FUNC_ID, is_available,lsubmodel)
      call hm_get_floatv('FSCALE_P', Fscale, is_available,lsubmodel,unitab)
      call hm_get_floatv('XSCALE_P', Xscale, is_available,lsubmodel,unitab)
      call hm_get_intv('IFORM', IFORM, is_available,lsubmodel)

      call hm_get_floatv('EOS_COM_Mue_min', mumin, is_available,lsubmodel,unitab)
      call hm_get_floatv('EOS_COM_Mue_max', mumax, is_available,lsubmodel,unitab)
      call hm_get_floatv('EOS_COM_BT', bmin, is_available,lsubmodel,unitab)
      call hm_get_floatv('EOS_COM_B', bmax, is_available,lsubmodel,unitab)

      call hm_get_floatv('LAW5_PSH', psh, is_available,lsubmodel,unitab)
      call hm_get_floatv('Refer_Rho', rho0, is_available_rho0,lsubmodel,unitab)

      rhor = pm(1)
      rhoi = pm(89)

      if(rho0 > zero) then
        rhor = rho0
        pm(1)= rho0
      else
        rho0=rhor
      endif

      if(bmin <= zero)then
         call ancmsg(MSGID=67,MSGTYPE=msgerror,ANMODE=aninfo,I1=imideos,C1='/EOS/COMPACTION2',C2='BMIN MUST BE POSITIVE')
      endif

      if(bmax <= zero)then
         call ancmsg(MSGID=67,MSGTYPE=msgerror,ANMODE=aninfo,I1=imideos,C1='/EOS/COMPACTION2',C2='BMAX MUST BE POSITIVE')
      endif

      !Default values
      if(iform /= 1 .and. iform /= 2)then
        !iform=1 : constant unload modulus bunl (old Radioss revision)
        !iform=2 : linear uload modulus from c1 to bunl (default)
        iform=2 !default
      endif

      if(mumax == zero) mumax = ep20

      if(Xscale == zero) Xscale = one

       if(iunit > 0)then
         fac_m = unitab%fac_m(iunit)
         fac_l = unitab%fac_l(iunit)
         fac_t = unitab%fac_t(iunit)
         fac_pres = fac_m / (fac_l*fac_t*fac_t)
       else
         fac_pres = one
       endif
       if(Fscale == zero) Fscale = one * fac_pres

      if(pm(79)==zero)pm(79)=three100

      !integer parameters
      eos_param%nuparam = 7
      eos_param%niparam = 1
      eos_param%nuvar = 0
      eos_param%nfunc = 1
      eos_param%ntable = 0
      call eos_param%construct() !allocations

      !real parameters
      eos_param%uparam(1) = bmin
      eos_param%uparam(2) = bmax
      eos_param%uparam(3) = mumin
      eos_param%uparam(4) = mumax
      eos_param%uparam(5) = Fscale
      eos_param%uparam(6) = Xscale
      eos_param%uparam(7) = psh

      !integer parameters
      eos_param%iparam(1) = iform

      !functions
      eos_param%func(1) = P_FUNC_ID ! user function id

      !initial sound speed
      if(rhoi == zero)then
        mu0 = zero ! error 683 already displayed
      else
        if(rhor /= zero)then
          mu0 = rhoi/rhor-one
        else
          mu0 = zero ! error 683 already displayed
        endif
      endif

      !check unload modulus regarding C1
      do jfunc=1,nfunc
        if(npc(nfunc + jfunc + 1) == P_FUNC_ID) then
          tmp = finter(jfunc ,-em12 ,npc,tf,dpdmu_mumin)
          dpdmu_mumin = dpdmu_mumin * fac_pres
          if(bmin < dpdmu_mumin)  then
            call ancmsg(MSGID=67,MSGTYPE=msgerror,ANMODE=aninfo,I1=imideos, &
            C1='/EOS/COMPACTION2',C2='BMIN MUST BE GREATER THAN DERIVATIVE OF P(MU) AT 0.0')
          end if
          exit
        end if
      end do

      !check unload modulus regarding point of maximum compaction
      if(mumax > zero .and. mumax < 1000.)then !possible overflow with unphysical value
        do jfunc=1,nfunc
          if(npc(nfunc + jfunc + 1) == P_FUNC_ID) then
            tmp = finter(jfunc ,mumax,npc,tf,dpdmu_mumax)
            dpdmu_mumax = dpdmu_mumax * fac_pres
            if(bmax < dpdmu_mumax)  then
              call ancmsg(MSGID=67,MSGTYPE=msgerror,ANMODE=aninfo,I1=imideos, &
              C1='/EOS/COMPACTION2',C2='BMAX MUST BE GREATER THAN DERIVATIVE OF P(MU) AT MUMAX')
            end if
            exit
          end if
        end do
      end if

      !ssp0
      ssp0 = zero
      dpdmu=max(bmin,bmax)
      if(rhor > zero) ssp0 = sqrt(dpdmu/rhor)
      pm(27) = max(ssp0, pm(27))
      pm(23) = zero ! e0
      pm(31) = p0-psh
      pm(104)= p0-psh

      write(iout,1000)
      if(is_encrypted)then
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      else
        write(iout,1500) P_FUNC_ID, fscale, xscale, psh, bmin, bmax, mumin, mumax
        if(is_available_rho0)write(iout,1503) pm(1)
        if(iform==1)write(iout,1501)
      endif

      return
! ----------------------------------------------------------------------------------------------------------------------

 1000 FORMAT(&
      5X,'  COMPACTION2 EOS    ',/,&
      5X,'  ---------------    ',/)
 1500 FORMAT(&
      5X,'FUNCTION IDENTIFIER . . . . . . . . . . . .=',I10/,&
      5X,'Y-SCALE FACTOR. . . . . . . . . . . . . . .=',1PG20.13/,&
      5X,'X-SCALE FACTOR. . . . . . . . . . . . . . .=',1PG20.13/,&
      5X,'PRESSURE SHIFT. . . . . . . . . . . . . . .=',1PG20.13/,&
      5X,'BMIN : MINIMUM UNLOADING MODULUS  . . . . .=',1PG20.13/,&
      5X,'BMAX : MAXIMUM UNLOADING MODULUS  . . . . .=',1PG20.13/,&
      5X,'MU_MIN : ELASTIC LIMIT. . . . . . . . . . .=',1PG20.13/,&
      5X,'MU_MAX : MAXIMUM COMPACTION . . . . . . . .=',1PG20.13)
 1501 FORMAT(&
      5X,'CONSTANT UNLOAD MODULUS'/)
 1503 FORMAT(&
      5X,'EOS REFERENCE DENSITY . . . . . . . . . .=',1PG20.13)

      END SUBROUTINE HM_READ_EOS_COMPACTION2
! ----------------------------------------------------------------------------------------------------------------------
      
      END MODULE hm_read_eos_compaction2_mod
