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
!copyright>        openradioss
! ======================================================================================================================

      !||====================================================================
      !||    hm_read_mat134_mod   ../starter/source/materials/mat/mat134/hm_read_mat134.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat          ../starter/source/materials/mat/hm_read_mat.F90
      !||====================================================================
      module hm_read_mat134_mod
      contains
  
      !||====================================================================
      !||    hm_read_mat134           ../starter/source/materials/mat/mat134/hm_read_mat134.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F90
      !||--- calls      -----------------------------------------------------
      !||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||    init_mat_keyword         ../starter/source/materials/mat/init_mat_keyword.F
      !||--- uses       -----------------------------------------------------
      !||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
      !||    message_mod              ../starter/share/message_module/message_mod.F
      !||    submodel_mod             ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      subroutine hm_read_mat134( mtag, matparam ,                              &
                   parmat   ,nuvar, unitab   ,lsubmodel,                       &
                   mat_id   ,titr     ,     iout       )
  ! ----------------------------------------------------------------------------------------------------------------------
  !                                                   modules
  ! ----------------------------------------------------------------------------------------------------------------------
      use elbuftag_mod
      use matparam_def_mod
      use unitab_mod
      use message_mod
      use submodel_mod
      use constant_mod , only : one ,two, zero,three
! --------------------------------------------------------------------------------------------------------------------
! --------------------------------------------------------------------------------------------------------------------
!                                                 implicit none
! --------------------------------------------------------------------------------------------------------------------
      implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
      
!c-----------------------------------------------
!c   d u m m y   a r g u m e n t s
!c-----------------------------------------------
      integer, intent(in)                          :: mat_id
      integer, intent(in)                          :: iout
      integer, intent(out)                         :: nuvar 
      type (unit_type_),intent(in) ::unitab 
      type(submodel_data), dimension(nsubmod),intent(in) :: lsubmodel
      character(len=nchartitle) ,intent(in)             :: titr
      my_real, dimension(100)       ,intent(inout)   :: parmat  
      type(matparam_struct_) ,intent(inout) :: matparam
      type(mlaw_tag_), intent(inout)  :: mtag
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      logical :: is_available,is_encrypted
      integer :: ilaw
      my_real :: rho0, young, nu , bulk, shear 
      my_real :: e1, n1, e2, v2, n2      
!=======================================================================
      is_encrypted = .false.
      is_available = .false.
      ilaw  = 134
!--------------------------------------------------------
!
      call hm_option_is_encrypted(is_encrypted)
!
!--------------------------------------------------------
!     read input fields
!--------------------------------------------------------
      call hm_get_floatv('Rho'            ,rho0    ,is_available, lsubmodel, unitab)
      !line2
      call hm_get_floatv('LSD_MAT_E1'  ,e1  ,is_available, lsubmodel, unitab)
      call hm_get_floatv('Nu'          ,nu  ,is_available, lsubmodel, unitab)
      call hm_get_floatv('LSD_MAT_N1'  ,n1  ,is_available, lsubmodel, unitab)
      call hm_get_floatv('LSD_MAT_E2'  ,e2  ,is_available, lsubmodel, unitab)
       call hm_get_floatv('LSD_MAT_N2' ,n2  ,is_available, lsubmodel, unitab)
      call hm_get_floatv('LSD_MAT_V2'  ,v2  ,is_available, lsubmodel, unitab)
!-------------------------------------
      young  = e1 + e2
      shear   = young/(two * (one + nu))
      bulk   = young/(three * (one - two * nu))
!-------------------------------------
      nuvar = 6
!-------------------------------------
      
      matparam%niparam = 0
      matparam%nuparam = 6
      matparam%nfunc   = 0
      matparam%ntable  = 0
!          
      allocate (matparam%uparam(matparam%nuparam))
      allocate (matparam%iparam(matparam%niparam))
      allocate (matparam%table(matparam%ntable))
!     
      matparam%uparam(1)  = e1
      matparam%uparam(2)  = n1      
      matparam%uparam(3)  = nu
      matparam%uparam(4)  = e2
      matparam%uparam(5)  = v2
      matparam%uparam(6)  = n2
      !
       !< Real material parameters
      matparam%rho       = rho0
      matparam%rho0      = rho0
      matparam%young      = young
      matparam%nu         = nu
      matparam%shear      = shear
      matparam%bulk       = bulk
      !
      parmat(1) = bulk
      parmat(2) = young
      parmat(3) = nu 
      parmat(16) = 1
      parmat(17) = (one-two*nu)/(one-nu)  !   2G / (bulk + G*4/3)
!------------------------------------------------- 
      mtag%g_epsd = 1
      mtag%l_epsd = 1
!-------------------------------------------------
              !< Properties compatibility  
          call init_mat_keyword(matparam,"SOLID_ISOTROPIC") 
          !< Properties compatibility  
          call init_mat_keyword(matparam ,"INCREMENTAL" )
          call init_mat_keyword(matparam ,"HOOK")
          call init_mat_keyword(matparam ,"ISOTROPIC")   
!-------------------------------------------------
      write(iout,1050) trim(titr),mat_id,134
      write(iout,1000)
      if (is_encrypted) then
        write(iout,'(5x,a,//)')'CONFIDENTIAL DATA'
      else
        write(iout,1060) rho0
        write(iout,1100) e1,n1,e2,v2,n2,nu
      endif       
!     
!-----------
      return
!-----------
 1000 format(                                                                &
     5x,a,/,                                                                 & 
     5x,40h  VISCOUS FOAM  MATERIAL                 ,/,                       & 
     5x,40h  -----------------------------------   ,//)           
 1050 format(/                                                               &
      5x,a,/,                                                                &
      5x,'MATERIAL NUMBER . . . . . . . . . . . . .=',i10/,                  &
      5x,'MATERIAL LAW. . . . . . . . . . . . . . .=',i10/)       
 1060 format(                                                                &
      5x,'INITIAL DENSITY . . . . . . . . . . . . .=',1pg20.13/)  
 1100 format(                                                                & 
      5x,'INITIAL YOUNGS MODULUS . . . . . . . . . . . . . . .=',1PG20.13/,  &
      5x,'EXPONENT IN POWER LAW FOR YOUNGS MODULUS. . . . . . =',1PG20.13/,  &
      5x,'ELASTIC MODULUS FOR VISCOSITY . . .. . . . . . . . .=',1PG20.13/,  &
      5x,'VISCOUS COEFFICIENT  .  . . . . . . . . . . . . . . =',1PG20.13/, &
      5x,'EXPONENT IN POWER LAW FOR VISCOSITY. . . . . . . . .=',1PG20.13/,  &
      5x,'POISSONS RATIO . . . . . . . . . . . . . . . . . . .=',1PG20.13/ )
!-----------------
    end subroutine hm_read_mat134
end module hm_read_mat134_mod             
