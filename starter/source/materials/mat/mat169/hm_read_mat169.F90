!copyright>        openradioss
!copyright>        copyright (c) 1986-2023 altair engineering inc.
!copyright>
!copyright>        this program is free software: you can redistribute it and/or modify
!copyright>        it under the terms of the gnu affero general public license as published by
!copyright>        the free software foundation, either version 3 of the license, or
!copyright>        (at your option) any later version.
!copyright>
!copyright>        this program is distributed in the hope that it will be useful,
!copyright>        but without any warranty; without even the implied warranty of
!copyright>        merchantability or fitness for a particular purpose.  see the
!copyright>        gnu affero general public license for more details.
!copyright>
!copyright>        you should have received a copy of the gnu affero general public license
!copyright>        along with this program.  if not, see <https://www.gnu.org/licenses/>.
!copyright>
!copyright>
!copyright>        commercial alternative: altair radioss software
!copyright>
!copyright>        as an alternative to this open-source version, altair also offers altair radioss
!copyright>        software under a commercial license.  contact altair to discuss further if the
!copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!chd|====================================================================
!chd|  hm_read_mat169                 source/materials/mat/mat169/hm_read_mat169.f
!chd|-- called by -----------
!chd|        hm_read_mat                   source/materials/mat/hm_read_mat.f
!chd|-- calls ---------------
!chd|====================================================================
! ======================================================================================================================

      module hm_read_mat169_arup_mod
      contains
  
      subroutine hm_read_mat169_arup( mtag, matparam ,                         &
                   parmat   ,nuvar, unitab   ,lsubmodel,                       &
                   mat_id   ,titr     ,pm         ,                            &
                   iout     ,npropm   )
  ! ----------------------------------------------------------------------------------------------------------------------
  !                                                   modules
  ! ----------------------------------------------------------------------------------------------------------------------
      use elbuftag_mod
      use matparam_def_mod
      use unitab_mod
      use message_mod
      use submodel_mod
      use constant_mod , only : one ,two, zero
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
      integer, intent(in)                          :: npropm
      integer, intent(out)                         :: nuvar 
      type (unit_type_),intent(in) ::unitab 
      type(submodel_data), dimension(nsubmod),intent(in) :: lsubmodel
      character(len=nchartitle) ,intent(in)             :: titr
      my_real, dimension(100)       ,intent(inout)   :: parmat         
      my_real, dimension(npropm)    ,intent(inout)   :: pm     
      type(matparam_struct_) ,intent(inout) :: matparam
      type(mlaw_tag_), intent(inout)  :: mtag
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      logical :: is_available,is_encrypted
      integer :: ilaw,pwrt, pwrs
      my_real :: rho0, young,shear, nu, thick, tenmax, gcten  ,shrmax, gcshr,             &
                 shrp, sht_sl,unit_l                  
      ! -------------------------
!=======================================================================
      is_encrypted = .false.
      is_available = .false.
      ilaw  = 169
!--------------------------------------------------------
!
      call hm_option_is_encrypted(is_encrypted)
!
!--------------------------------------------------------
!     read input fields
!--------------------------------------------------------

      call hm_get_floatv('Rho'              ,rho0      ,is_available, lsubmodel, unitab)
      !line2
      call hm_get_floatv('E'                ,young     ,is_available, lsubmodel, unitab)
      call hm_get_floatv('Nu'               ,nu        ,is_available, lsubmodel, unitab)
      call hm_get_floatv('thick'            ,thick     ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT169_TENMAX'    ,tenmax    ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT169_GCTEN'     ,gcten     ,is_available, lsubmodel, unitab)
      !line 3 
      call hm_get_floatv('MAT169_SHRMAX'    ,shrmax    ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT169_GCSHR'     ,gcshr     ,is_available, lsubmodel, unitab)
      call hm_get_intv  ('MAT169_PWRT'      ,pwrt      ,is_available, lsubmodel)     
      call hm_get_intv  ('MAT169_PWRS'      ,pwrs      ,is_available, lsubmodel)            
      call hm_get_floatv('MAT169_SHRP'      ,shrp      ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT169_SHT_SL'    ,sht_sl    ,is_available, lsubmodel, unitab)
!-------------------------------------
      if (thick == zero) then
        call hm_get_floatv_dim('thick' ,unit_l ,is_available, lsubmodel, unitab)
        thick = one * unit_l
      endif
      shear   = young/(two * (one + nu))

!-------------------------------------
      nuvar = 4
!-------------------------------------
      
      matparam%niparam = 2
      matparam%nuparam = 10
!          
      allocate (matparam%uparam(matparam%nuparam))
      allocate (matparam%iparam(matparam%niparam))
!     
      matparam%iparam(1) =  pwrt
      matparam%iparam(2) =  pwrs

      matparam%uparam(1) =   young 
      matparam%uparam(2) =   shear      
      matparam%uparam(3) =   nu
      matparam%uparam(4) =   tenmax
      matparam%uparam(5) =   gcten
      matparam%uparam(6) =   shrmax
      matparam%uparam(7) =   gcshr
      matparam%uparam(8) =   shrp
      matparam%uparam(9) =   sht_sl
      matparam%uparam(10) =  thick
!-------------------------------------------------
      pm(1)  = rho0
      pm(89) = rho0
!-------------------------------------------------
      mtag%g_pla  = 1
      mtag%l_pla  = 1
      mtag%l_dmg  = 1
!-------------------------------------------------
      ! properties compatibility  
      call init_mat_keyword(matparam,"SOLID_COHESIVE")       
!-------------------------------------------------
      write(iout,1050) trim(titr),mat_id,169
      write(iout,1000)
      if (is_encrypted) then
        write(iout,'(5x,a,//)')'CONFIDENTIAL DATA'
      else
        write(iout,1060) rho0
        write(iout,1100) young,nu, tenmax,shrmax, pwrt,pwrs,gcten  , gcshr,   & 
        shrp, sht_sl
      endif       
!      parmat(1)  = c1   
      parmat(2)  = young
      parmat(3)  = nu   

!-----------
      return
!-----------
 1000 format(                                                                &
     5x,a,/,                                                                 & 
     5x,40h  ELASTOPLASTIC MATERIAL FOR COHESIVE   ,/,                       & 
     5x,40h  -----------------------------------   ,//)           
 1050 format(/                                                               &
      5x,a,/,                                                                &
      5x,'MATERIAL NUMBER . . . . . . . . . . . . .=',i10/,                  &
      5x,'MATERIAL LAW. . . . . . . . . . . . . . .=',i10/)       
 1060 format(                                                                &
      5x,'INITIAL DENSITY . . . . . . . . . . . . .=',1pg20.13/)  
 1100 format(                                                                & 
      5x,'YOUNG MODULUS PER THICKNESS UNIT IN TENSION. . . . .=',1PG20.13/,  &
      5x,'POISSON RATION . . . . . . . . . . . . . . . . . . .=',1PG20.13/,  &
      5x,'MAXIMAL TENSILE STRESS . . . . . . . . . . . . . . .=',1PG20.13/,  &
      5x,'MAXIMAL SHEAR STRESS . . . . . . . . . . . . . . . .=',1PG20.13/,  &
      5x,'POWER TERM FOR TENSION . . . . . . . . . . . . . . .=',I10/,       &
      5x,'POWER TERM FOR SHEAR . . . . . . . . . . . . . . . .=',I10/,       &
      5x,'ENERGY PER UNIT AREA TO FAIL IN TENSION. . . . . . .=',1PG20.13/,  &
      5x,'ENERGY PER UNIT AREA TO FAIL IN SHEAR. . . . . . . .=',1PG20.13/,  &
      5x,'SHEAR PLATEAU RATIO. . . . . . . . . . . . . . . . .=',1PG20.13/,  &
      5x,'SLOPE OF YIELD SURFACE AT ZERO TENSION . . . . . . .=',1PG20.13/)
!-----------------
    end subroutine hm_read_mat169_arup
end module hm_read_mat169_arup_mod             