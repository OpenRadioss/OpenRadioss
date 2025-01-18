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
      !||    hm_read_eos_compaction_mod   ../starter/source/materials/eos/hm_read_eos_compaction.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_eos                  ../starter/source/materials/eos/hm_read_eos.F
      !||====================================================================
      module hm_read_eos_compaction_mod
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
      !||    hm_read_eos_compaction   ../starter/source/materials/eos/hm_read_eos_compaction.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_eos              ../starter/source/materials/eos/hm_read_eos.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                   ../starter/source/output/message/message.F
      !||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||--- uses       -----------------------------------------------------
      !||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
      !||    message_mod              ../starter/share/message_module/message_mod.F
      !||    submodel_mod             ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      subroutine hm_read_eos_compaction(iout,pm,unitab,lsubmodel,imideos,eos_tag,ieos,npropm,maxeos)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use message_mod
      use unitab_mod , only : unit_type_
      use submodel_mod , only : nsubmod, submodel_data
      use elbuftag_mod , only : eos_tag_
      use constant_mod , only : zero, two_third, one, two, three, three100
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
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      my_real  p0, e0, psh, rho0,rhoi,rhor
      my_real  c0,c1,c2,c3,bunl,mu,mumin,mumax
      my_real  mu0,ssp0,df, g0, bulk,bulk2, bb, pold, mu2, muold, alpha,dpdmu
      integer iform, ioutp
      logical :: is_encrypted, is_available, is_available_rho0
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      is_encrypted = .false.
      is_available = .false.
      is_available_rho0 = .false.
      iform=0
      ioutp=1

      eos_tag(ieos)%g_mu = 1
      eos_tag(ieos)%l_mu = 1
           
      call hm_option_is_encrypted(is_encrypted)

      call hm_get_floatv('EOS_COM_C0', c0, is_available,lsubmodel,unitab)
      call hm_get_floatv('EOS_COM_C1', c1, is_available,lsubmodel,unitab)
      call hm_get_floatv('EOS_COM_C2', c2, is_available,lsubmodel,unitab)
      call hm_get_floatv('EOS_COM_C3', c3, is_available,lsubmodel,unitab)
      call hm_get_intv('IFORM', IFORM, is_available,lsubmodel)

      call hm_get_floatv('EOS_COM_Mue_min', mumin, is_available,lsubmodel,unitab)
      call hm_get_floatv('EOS_COM_Mue_max', mumax, is_available,lsubmodel,unitab)
      call hm_get_floatv('EOS_COM_B', bunl, is_available,lsubmodel,unitab)

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

      if(c1 <= zero)then
         call ancmsg(MSGID=67,MSGTYPE=msgerror,ANMODE=aninfo,I1=imideos,C1='/EOS/COMPACTION',C2='C1 MUST BE POSITIVE')
      endif

      !iform=1 : constant unload modulus bunl
      !iform=2 : linear uload modulus from c1 to bunl (default)
      if(iform /= 1 .and. iform /= 2)then
        iform=2 !default
        ioutp=0
      endif
      
      mu = rho0/rhor-one
      p0 = c0+min(c1*mu,c1*mu+c2*mu*mu+c3*mu*mu*mu)
      e0 = zero

      pm(49) = c0
      pm(32) = c1
      pm(33) = c2
      pm(34) = c3
      pm(88) = psh
      pm(45) = bunl
      pm(46) = mumax
      pm(47) = mumin
      pm(48) = iform
      if(pm(79)==zero)pm(79)=three100

      pm(23) = e0
      pm(31) = p0-psh
      pm(104)= p0-psh

      if(rhoi == zero)then
        mu0 = zero ! error 683 already displayed
      else
        if(rhor /= zero)then
          mu0 = rhoi/rhor-one
        else
          mu0 = zero ! error 683 already displayed
        endif
      endif

      if(rhoi /= zero)then
        df = rhor/rhoi
      else
        df = zero
      endif

      mu2=mu0*mu0
      muold=mu0
      pold=p0
      bulk = bunl
      bulk2 = bunl

      !ssp0
      ssp0 = zero
      g0 = pm(22)
      rhoi = pm(89)
        if(iform == 1)then
            bb=bunl
        elseif(iform == 2)then
          alpha = one
          if(mumax > zero)then
            alpha=muold/mumax
          endif
          bb = alpha*bunl+(one-alpha)*c1
        endif
        dpdmu = c1 + two*c2*mu0+three*c3*mu2   !can be discussed in expansion...
        dpdmu = max(bb,dpdmu)

      dpdmu=max(zero,dpdmu)
      if(rhor > zero) ssp0 = sqrt((dpdmu + two_third*g0)/rhor)
      pm(27)=ssp0

      write(iout,1000)

      if(is_encrypted)then
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      else
        write(iout,1500)c0,c1,c2,c3,psh,bunl,mumin,mumax
        if(is_available_rho0)write(iout,1503)pm(1)
        if(ioutp == 1)then
          if(iform==1)then
             write(iout,1501)
          elseif(iform==2)then
             write(iout,1502)
          endif
        endif
      endif

      return
! ----------------------------------------------------------------------------------------------------------------------

 1000 FORMAT(&
      5X,'  COMPACTION EOS    ',/,&
      5X,'  --------------    ',/)
 1500 FORMAT(&
      5X,'C0. . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,&
      5X,'C1. . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,&
      5X,'C2. . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,&
      5X,'C3. . . . . . . . . . . . . . . . . . . . .=',1PG20.13/,&
      5X,'PRESSURE SHIFT. . . . . . . . . . . . . . .=',1PG20.13/,&
      5X,'BUNL : UNLOADING MODULUS. . . . . . . . . .=',1PG20.13/,&
      5X,'MU_MIN : ELASTIC LIMIT. . . . . . . . . . .=',1PG20.13/,&
      5X,'MU_MAX : MAXIMUM COMPACTION . . . . . . . .=',1PG20.13)
 1501 FORMAT(&
      5X,'CONSTANT UNLOAD MODULUS'/)
 1502 FORMAT(&
      5X,'CONTINUOUS UNLOAD MODULUS FROM C1 TO BUNL IN RANGE [MUMIN,MUMAX]'/)
 1503 FORMAT(&
      5X,'EOS REFERENCE DENSITY . . . . . . . . . .=',1PG20.13)


  
      END SUBROUTINE HM_READ_EOS_COMPACTION
! ----------------------------------------------------------------------------------------------------------------------
      
      END MODULE hm_read_eos_compaction_mod
