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
      !||    hm_read_visc_plas_mod   ../starter/source/materials/visc/hm_read_visc_plas.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_visc            ../starter/source/materials/visc/hm_read_visc.F
      !||====================================================================
      module hm_read_visc_plas_mod
      contains
! ======================================================================================================================
! \brief Reading material parameters of /VISC/PLAS
! \details Reading material parameters of /VISC/PLAS
! ======================================================================================================================
      !||====================================================================
      !||    hm_read_visc_plas        ../starter/source/materials/visc/hm_read_visc_plas.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_visc             ../starter/source/materials/visc/hm_read_visc.F
      !||--- calls      -----------------------------------------------------
      !||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||--- uses       -----------------------------------------------------
      !||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
      !||    message_mod              ../starter/share/message_module/message_mod.F
      !||    submodel_mod             ../starter/share/modules1/submodel_mod.F
      !||====================================================================
       subroutine hm_read_visc_plas( &
                       visc     ,ivisc      ,iout , unitab  ,lsubmodel)
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use visc_param_mod
      use unitab_mod
      use message_mod
      use submodel_mod
      use hm_option_read_mod 
! -------------------------------------------------------------------------------------------------
      implicit none
! -------------------------------------------------------------------------------------------------
!     included files
! -------------------------------------------------------------------------------------------------
#include "my_real.inc"
! -------------------------------------------------------------------------------------------------
!   d u m m y   a r g u m e n t s
! -------------------------------------------------------------------------------------------------
      integer ,intent(in) :: ivisc
      integer ,intent(in) :: iout
      type (visc_param_)  ,intent(inout) :: visc
      type (unit_type_)   ,intent(in) ::unitab 
      type(submodel_data) ,dimension(nsubmod), intent(in) :: lsubmodel
! -------------------------------------------------------------------------------------------------
!   l o c a l   v a r i a b l e s
! -------------------------------------------------------------------------------------------------
      integer i,nuparam,niparam
      my_real :: g,sigy
!      
      logical :: is_available,is_encrypted
!=======================================================================
      is_encrypted = .false.
      is_available = .false.

      call hm_option_is_encrypted(is_encrypted)
!======================================   
      visc%ilaw = ivisc
!
      ! 1st card 
      call hm_get_floatv ('LSD_G'       ,g    ,is_available,lsubmodel,unitab)  
      call hm_get_floatv ('LSDYNA_SIGF'  ,sigy   ,is_available,lsubmodel,unitab)  
!-------------------------------------------------
!     storing parameters in uparam / iparam tables
!-------------------------------------------------
      
      niparam = 0
      nuparam = 2
      allocate (visc%uparam(nuparam))
      allocate (visc%iparam(niparam))
      visc%nuvar     = 0
      visc%nuparam   = nuparam
      visc%niparam   = niparam
 
      visc%uparam(1) = g
      visc%uparam(2) = sigy
!-----------------------------------------------------------------------
!     output
!-----------------------------------------------------------------------
      if (is_encrypted)then                                
        write(iout,'(5x,a,//)')'confidential data'
      else 
         write(iout,1000) 
         write(iout,1100) g,sigy
      endif                
!-----------        
 1000 format(  &
      5x,'  FREQUENCY INDEPENDENT DAMPING MODEL  :'           ,/, &
      5x,' ---------------------------------------- '         ,/)
 1100 format(   &
       5x,'SHEAR MODULUS FOR FREQUENCY INDEPENDENT DAMPING . . . . . . =',1pg20.13 /  &
       5x,'LIMIT STRESS FOR FREQUENCY INDEPENDENT FRICTIONAL DAMPING . =',1pg20.13 / )     
     
      end subroutine hm_read_visc_plas
!=======================================================================
      end module hm_read_visc_plas_mod

