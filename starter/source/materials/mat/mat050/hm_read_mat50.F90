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
! =================================================================================================
! =================================================================================================
      !||====================================================================
      !||    hm_read_mat50_mod   ../starter/source/materials/mat/mat050/hm_read_mat50.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat         ../starter/source/materials/mat/hm_read_mat.F90
      !||====================================================================
      module hm_read_mat50_mod
      contains  

      !||====================================================================
      !||    hm_read_mat50            ../starter/source/materials/mat/mat050/hm_read_mat50.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F90
      !||--- calls      -----------------------------------------------------
      !||    func_table_copy          ../starter/source/materials/tools/func_table_copy.F90
      !||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_floatv_dim        ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
      !||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||    init_mat_keyword         ../starter/source/materials/mat/init_mat_keyword.F
      !||--- uses       -----------------------------------------------------
      !||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
      !||    func_table_copy_mod      ../starter/source/materials/tools/func_table_copy.F90
      !||    submodel_mod             ../starter/share/modules1/submodel_mod.F
      !||    table_mod                ../starter/share/modules1/table_mod.F
      !||====================================================================
      subroutine hm_read_mat50(mat_param,mtag     ,parmat   ,nuvar    ,nvartmp  ,    &
                               ntable   ,table    ,mat_id   ,iout     ,titr     ,    &
                               unitab   ,lsubmodel)
  ! -----------------------------------------------------------------------------------------------
  !   modules
  ! -----------------------------------------------------------------------------------------------
      use elbuftag_mod
      use matparam_def_mod
      use table_mod
      use unitab_mod
      use submodel_mod
      use constant_mod , only : pi,one,third,two,zero,em20,ep10,ep20
      use func_table_copy_mod
! -------------------------------------------------------------------------------------------------
      implicit none
! -------------------------------------------------------------------------------------------------
!     included files
! -------------------------------------------------------------------------------------------------
#include "my_real.inc"
      
! -------------------------------------------------------------------------------------------------
!   d u m m y   a r g u m e n t s
! -------------------------------------------------------------------------------------------------
      integer                                ,intent(in)    :: mat_id 
      integer                                ,intent(in)    :: iout   
      integer                                ,intent(in)    :: ntable
      integer                                ,intent(out)   :: nuvar  
      integer                                ,intent(out)   :: nvartmp
      type (unit_type_)                      ,intent(in)    :: unitab
      type(submodel_data), dimension(nsubmod),intent(in)    :: lsubmodel
      character(len=nchartitle)              ,intent(in)    :: titr
      my_real, dimension(128)                ,intent(inout) :: parmat  
      type(matparam_struct_)                 ,intent(inout) :: mat_param
      type(mlaw_tag_)                        ,intent(inout) :: mtag
      type(ttable) ,dimension(ntable)        ,intent(in)    :: table
! -------------------------------------------------------------------------------------------------
!   l o c a l   v a r i a b l e s
! -------------------------------------------------------------------------------------------------
      integer :: i,ilaw,n11,n22,n33,n12,n23,n31,iflag1,iflag2,ierr
      integer :: icompact,irate
      my_real :: rho0,rhor,fcut,dmin,dmax,e11,e22,e33,g12,g23,g31
      my_real :: ecomp,gcomp,bulk,nu,sigy,hcomp,vcomp
      my_real :: emx11,emx22,emx33,emx12,emx23,emx31
      my_real :: x1scale,x2scale,press_scale
      integer ,dimension(5) :: i11,i22,i33,i12,i23,i31
      integer ,dimension(5,6) :: func_id
      my_real ,dimension(5) :: epsp11,epsp22,epsp33,epsp12,epsp23,epsp31
      my_real ,dimension(5) :: fac11,fac22,fac33,fac12,fac23,fac31
      logical :: is_available,is_encrypted
!==================================================================================================
      is_encrypted = .false.
      is_available = .false.
      ilaw     = 50
      icompact = 0   ! no compacted state coupling by default
!
      call hm_option_is_encrypted(is_encrypted)
!
      CALL hm_get_floatv('MAT_RHO'      ,rho0      ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('Refer_Rho'    ,rhor      ,is_available, lsubmodel, unitab)
!      
      CALL hm_get_floatv('MAT_EA'       ,e11       ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EB'       ,e22       ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EC'       ,e33       ,is_available, lsubmodel, unitab)
!
      CALL hm_get_floatv('MAT_GAB'      ,g12       ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_GBC'      ,g23       ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_GCA'      ,g31       ,is_available, lsubmodel, unitab)
!
      CALL hm_get_floatv('MAT_asrate'   ,fcut      ,is_available, lsubmodel, unitab)
      CALL hm_get_intv  ('Irate'        ,irate     ,is_available,lsubmodel)
!    
!     normal direction    
!    
      CALL hm_get_intv  ('Gflag'        ,iflag1    ,is_available,lsubmodel)
      CALL hm_get_floatv('MAT_EPS_max11',emx11     ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS_max22',emx22     ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS_max33',emx33     ,is_available, lsubmodel, unitab)
!
!     direction 11
!
      CALL hm_get_intv  ('MAT_YFUN11_1' ,i11(1)    ,is_available,lsubmodel)
      CALL hm_get_intv  ('MAT_YFUN11_2' ,i11(2)    ,is_available,lsubmodel)
      CALL hm_get_intv  ('MAT_YFUN11_3' ,i11(3)    ,is_available,lsubmodel)
      CALL hm_get_intv  ('MAT_YFUN11_4' ,i11(4)    ,is_available,lsubmodel)
      CALL hm_get_intv  ('MAT_YFUN11_5' ,i11(5)    ,is_available,lsubmodel)
!
      CALL hm_get_floatv('MAT_SFAC11_1' ,fac11(1)  ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SFAC11_2' ,fac11(2)  ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SFAC11_3' ,fac11(3)  ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SFAC11_4' ,fac11(4)  ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SFAC11_5' ,fac11(5)  ,is_available, lsubmodel, unitab)
!
      CALL hm_get_floatv('MAT_EPS11_1'  ,epsp11(1) ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS11_2'  ,epsp11(2) ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS11_3'  ,epsp11(3) ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS11_4'  ,epsp11(4) ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS11_5'  ,epsp11(5) ,is_available, lsubmodel, unitab)
!
!     direction 22
!
      CALL hm_get_intv  ('MAT_YFUN22_1' ,i22(1)    ,is_available,lsubmodel)
      CALL hm_get_intv  ('MAT_YFUN22_2' ,i22(2)    ,is_available,lsubmodel)
      CALL hm_get_intv  ('MAT_YFUN22_3' ,i22(3)    ,is_available,lsubmodel)
      CALL hm_get_intv  ('MAT_YFUN22_4' ,i22(4)    ,is_available,lsubmodel)
      CALL hm_get_intv  ('MAT_YFUN22_5' ,i22(5)    ,is_available,lsubmodel)
!
      CALL hm_get_floatv('MAT_SFAC22_1' ,fac22(1)  ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SFAC22_2' ,fac22(2)  ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SFAC22_3' ,fac22(3)  ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SFAC22_4' ,fac22(4)  ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SFAC22_5' ,fac22(5)  ,is_available, lsubmodel, unitab)
!
      CALL hm_get_floatv('MAT_EPS22_1'  ,epsp22(1) ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS22_2'  ,epsp22(2) ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS22_3'  ,epsp22(3) ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS22_4'  ,epsp22(4) ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS22_5'  ,epsp22(5) ,is_available, lsubmodel, unitab)
!
!     direction 33
!
      CALL hm_get_intv  ('MAT_YFUN33_1' ,i33(1)    ,is_available,lsubmodel)
      CALL hm_get_intv  ('MAT_YFUN33_2' ,i33(2)    ,is_available,lsubmodel)
      CALL hm_get_intv  ('MAT_YFUN33_3' ,i33(3)    ,is_available,lsubmodel)
      CALL hm_get_intv  ('MAT_YFUN33_4' ,i33(4)    ,is_available,lsubmodel)
      CALL hm_get_intv  ('MAT_YFUN33_5' ,i33(5)    ,is_available,lsubmodel)
!
      CALL hm_get_floatv('MAT_SFAC33_1' ,fac33(1)  ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SFAC33_2' ,fac33(2)  ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SFAC33_3' ,fac33(3)  ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SFAC33_4' ,fac33(4)  ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SFAC33_5' ,fac33(5)  ,is_available, lsubmodel, unitab)
!
      CALL hm_get_floatv('MAT_EPS33_1'  ,epsp33(1) ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS33_2'  ,epsp33(2) ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS33_3'  ,epsp33(3) ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS33_4'  ,epsp33(4) ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS33_5'  ,epsp33(5) ,is_available, lsubmodel, unitab)
!    
!     shear
!    
      CALL hm_get_intv  ('Vflag'        ,iflag2    ,is_available,lsubmodel)
      CALL hm_get_floatv('MAT_EPS_max12',emx12     ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS_max23',emx23     ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS_max31',emx31     ,is_available, lsubmodel, unitab)
!
!     direction 12
!
      CALL hm_get_intv  ('MAT_YFUN12_1' ,i12(1)    ,is_available,lsubmodel)
      CALL hm_get_intv  ('MAT_YFUN12_2' ,i12(2)    ,is_available,lsubmodel)
      CALL hm_get_intv  ('MAT_YFUN12_3' ,i12(3)    ,is_available,lsubmodel)
      CALL hm_get_intv  ('MAT_YFUN12_4' ,i12(4)    ,is_available,lsubmodel)
      CALL hm_get_intv  ('MAT_YFUN12_5' ,i12(5)    ,is_available,lsubmodel)
!
      CALL hm_get_floatv('MAT_SFAC12_1' ,fac12(1)  ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SFAC12_2' ,fac12(2)  ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SFAC12_3' ,fac12(3)  ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SFAC12_4' ,fac12(4)  ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SFAC12_5' ,fac12(5)  ,is_available, lsubmodel, unitab)
!
      CALL hm_get_floatv('MAT_EPS12_1'  ,epsp12(1) ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS12_2'  ,epsp12(2) ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS12_3'  ,epsp12(3) ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS12_4'  ,epsp12(4) ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS12_5'  ,epsp12(5) ,is_available, lsubmodel, unitab)
!
!     direction 23
!
      CALL hm_get_intv  ('MAT_YFUN23_1' ,i23(1)    ,is_available,lsubmodel)
      CALL hm_get_intv  ('MAT_YFUN23_2' ,i23(2)    ,is_available,lsubmodel)
      CALL hm_get_intv  ('MAT_YFUN23_3' ,i23(3)    ,is_available,lsubmodel)
      CALL hm_get_intv  ('MAT_YFUN23_4' ,i23(4)    ,is_available,lsubmodel)
      CALL hm_get_intv  ('MAT_YFUN23_5' ,i23(5)    ,is_available,lsubmodel)
!
      CALL hm_get_floatv('MAT_SFAC23_1' ,fac23(1)  ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SFAC23_2' ,fac23(2)  ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SFAC23_3' ,fac23(3)  ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SFAC23_4' ,fac23(4)  ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SFAC23_5' ,fac23(5)  ,is_available, lsubmodel, unitab)
!
      CALL hm_get_floatv('MAT_EPS23_1'  ,epsp23(1) ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS23_2'  ,epsp23(2) ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS23_3'  ,epsp23(3) ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS23_4'  ,epsp23(4) ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS23_5'  ,epsp23(5) ,is_available, lsubmodel, unitab)
!
!     direction 31
!
      CALL hm_get_intv  ('MAT_YFUN31_1' ,i31(1)    ,is_available,lsubmodel)
      CALL hm_get_intv  ('MAT_YFUN31_2' ,i31(2)    ,is_available,lsubmodel)
      CALL hm_get_intv  ('MAT_YFUN31_3' ,i31(3)    ,is_available,lsubmodel)
      CALL hm_get_intv  ('MAT_YFUN31_4' ,i31(4)    ,is_available,lsubmodel)
      CALL hm_get_intv  ('MAT_YFUN31_5' ,i31(5)    ,is_available,lsubmodel)
!
      CALL hm_get_floatv('MAT_SFAC31_1' ,fac31(1)  ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SFAC31_2' ,fac31(2)  ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SFAC31_3' ,fac31(3)  ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SFAC31_4' ,fac31(4)  ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SFAC31_5' ,fac31(5)  ,is_available, lsubmodel, unitab)
!
      CALL hm_get_floatv('MAT_EPS31_1'  ,epsp31(1) ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS31_2'  ,epsp31(2) ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS31_3'  ,epsp31(3) ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS31_4'  ,epsp31(4) ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_EPS31_5'  ,epsp31(5) ,is_available, lsubmodel, unitab)
!
!     compact state coupling parameters
!
      CALL hm_get_floatv('MAT_ECOMP'    ,ecomp     ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_PR'       ,nu        ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_SIGY'     ,sigy      ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_ET'       ,hcomp     ,is_available, lsubmodel, unitab)
      CALL hm_get_floatv('MAT_VCOMP'    ,vcomp     ,is_available, lsubmodel, unitab)
!
!     get pressure unit scale factor
!
      call hm_get_floatv_dim('MAT_EA',press_scale,is_available,lsubmodel,unitab)
!--------------------------------------------------
!     default values
!--------------------------------------------------
      if (irate == 0) irate = 2          ! default value => independent strain rate by direction
                                         ! irate = 1 => equivalent strain rate common for all directions                                 
      if (fcut == zero) fcut = ep20
      x1scale = one
      x2scale = one
      if (emx11  == zero) emx11  = ep10
      if (emx22  == zero) emx22  = ep10
      if (emx33  == zero) emx33  = ep10
      if (emx12  == zero) emx12  = ep10
      if (emx23  == zero) emx23  = ep10
      if (emx31  == zero) emx31  = ep10
      
      if (ecomp*sigy*vcomp > zero) icompact = 1  ! compacted state coupling mode is activated        
!
      do i=1,5
        if (fac11(i) == zero) fac11(i) = one*press_scale
        if (fac22(i) == zero) fac22(i) = one*press_scale
        if (fac33(i) == zero) fac33(i) = one*press_scale
        if (fac12(i) == zero) fac12(i) = one*press_scale
        if (fac23(i) == zero) fac23(i) = one*press_scale
        if (fac31(i) == zero) fac31(i) = one*press_scale
      enddo
!
      n11 = 0
      n22 = 0
      n33 = 0
      n12 = 0
      n23 = 0
      n31 = 0
      if (i11(1) > 0) n11 = 1
      if (i22(1) > 0) n22 = 1
      if (i33(1) > 0) n33 = 1
      if (i12(1) > 0) n12 = 1
      if (i23(1) > 0) n23 = 1
      if (i31(1) > 0) n31 = 1
      do i=2,5
        if ((i11(i)*epsp11(i)) > em20 .and. n11 == i-1) n11=i
        if ((i22(i)*epsp22(i)) > em20 .and. n22 == i-1) n22=i
        if ((i33(i)*epsp33(i)) > em20 .and. n33 == i-1) n33=i
        if ((i12(i)*epsp12(i)) > em20 .and. n12 == i-1) n12=i
        if ((i23(i)*epsp23(i)) > em20 .and. n23 == i-1) n23=i
        if ((i31(i)*epsp31(i)) > em20 .and. n31 == i-1) n31=i
      enddo
!
      do i=1,5
        func_id(i,1) = i11(i)
        func_id(i,2) = i22(i)
        func_id(i,3) = i33(i)
        func_id(i,4) = i12(i)
        func_id(i,5) = i23(i)
        func_id(i,6) = i31(i)
      end do
!-------------------------------------
      ! create local function table from tabulated yield curves
!-------------------------------------
      mat_param%ntable = 6
      allocate (mat_param%table(mat_param%ntable))
!
      mat_param%table(1)%notable = n11
      mat_param%table(2)%notable = n22
      mat_param%table(3)%notable = n33
      mat_param%table(4)%notable = n12
      mat_param%table(5)%notable = n23
      mat_param%table(6)%notable = n31
!
      if (n11 > 0) then
        call func_table_copy(mat_param%table(1),mat_param%title,mat_param%mat_id,     &
                             n11     ,i11     ,epsp11  ,x1scale ,x2scale  ,fac11    , &
                             ntable  ,table   ,ierr    )
      end if
      if (n22 > 0) then
        call func_table_copy(mat_param%table(2),mat_param%title,mat_param%mat_id,     &
                             n22     ,i22     ,epsp22  ,x1scale ,x2scale  ,fac22    , &
                             ntable  ,table   ,ierr    )
      end if
      if (n33 > 0) then
        call func_table_copy(mat_param%table(3),mat_param%title,mat_param%mat_id,     &
                             n33     ,i33     ,epsp33  ,x1scale ,x2scale  ,fac33    , &
                             ntable  ,table   ,ierr    )
      end if
      if (n12 > 0) then
        call func_table_copy(mat_param%table(4),mat_param%title,mat_param%mat_id,     &
                             n12     ,i12     ,epsp12  ,x1scale ,x2scale  ,fac12    , &
                             ntable  ,table   ,ierr    )
      end if
      if (n23 > 0) then
        call func_table_copy(mat_param%table(5),mat_param%title,mat_param%mat_id,     &
                             n23     ,i23     ,epsp23  ,x1scale ,x2scale  ,fac23    , &
                             ntable  ,table   ,ierr    )
      end if
      if (n31 > 0) then
        call func_table_copy(mat_param%table(6),mat_param%title,mat_param%mat_id,     &
                             n31     ,i31     ,epsp31  ,x1scale ,x2scale  ,fac31    , &
                             ntable  ,table   ,ierr    )
      end if
!-------------------------------------
      nuvar   = 6
      nvartmp = 13
!-------------------------------------      
      mat_param%niparam = 4
      if (icompact == 0) then
        mat_param%nuparam = 13
      else
        mat_param%nuparam = 19
      end if
      mat_param%nfunc = 0
!          
      allocate (mat_param%uparam(mat_param%nuparam))
      allocate (mat_param%iparam(mat_param%niparam))
!     
      mat_param%iparam(1)  = iflag1
      mat_param%iparam(2)  = iflag2
      mat_param%iparam(3)  = icompact
      mat_param%iparam(4)  = irate  ! strain rate formulation : 0=strain rate by direction
                                    !                           1=common component strain rate 
      mat_param%uparam(1)  = e11
      mat_param%uparam(2)  = e22
      mat_param%uparam(3)  = e33
      mat_param%uparam(4)  = g12
      mat_param%uparam(5)  = g23
      mat_param%uparam(6)  = g31
      mat_param%uparam(7)  = emx11
      mat_param%uparam(8)  = emx22
      mat_param%uparam(9)  = emx33
      mat_param%uparam(10) = emx12
      mat_param%uparam(11) = emx23
      mat_param%uparam(12) = emx31
      mat_param%uparam(13) = fcut   ! *two*pi
      
      if (icompact == 1) then
        nu = min(nu, 0.495)
        gcomp = ecomp / (one + nu)               ! saving 2G
        bulk  = ecomp * third / (one - two*nu)
        mat_param%uparam(14) = ecomp
        mat_param%uparam(15) = gcomp
        mat_param%uparam(16) = bulk    
        mat_param%uparam(17) = sigy    
        mat_param%uparam(18) = hcomp  
        mat_param%uparam(19) = vcomp  
      end if     
!---------------------------
      mat_param%bulk  = max(e11,e22,e33,g12,g23,g31)
      mat_param%young = zero
      mat_param%shear = zero
      mat_param%nu    = zero

      mat_param%rho   = rhor
      mat_param%rho0  = rho0
!-----------------
      dmin       = min(e11*e22, e22*e33,e11*e33)
      dmax       = max(e11,e22,e33)
!
      parmat(1)  = mat_param%bulk
      parmat(2)  = zero
      parmat(3)  = zero
      parmat(4)  = zero
      parmat(16) = 1
      parmat(17) = dmin/dmax/dmax      
!-----------------
!     element buffer variable allocation
!-----------------
      mtag%g_pla  = 1
      mtag%l_pla  = 1
!--------------------------------------------------
      call init_mat_keyword(mat_param,"HOOK")
      call init_mat_keyword(mat_param,"COMPRESSIBLE")
      call init_mat_keyword(mat_param,"SMALL_STRAIN")
      call init_mat_keyword(mat_param,"ORTHOTROPIC")
      ! properties compatibility 
      call init_mat_keyword(mat_param,"SOLID_ISOTROPIC")      
!--------------------------------------------------
!     starter output
!--------------------------------------------------
      write(iout,1000) trim(titr),mat_id,ilaw
      if (is_encrypted) then
        write(iout,'(5x,a,//)')'confidential data'
      else
        write(iout,1100)
        write(iout,1200) rho0
        write(iout,1300) e11,e22,e33,g12,g23,g31,irate,fcut
!
        write(iout,1001)
        write(iout,1600) epsp11(1),func_id(1,1),fac11(1)
        if (func_id(2,1) > 0) write(iout,1600) epsp11(2),func_id(2,1),fac11(2)
        if (func_id(3,1) > 0) write(iout,1600) epsp11(3),func_id(3,1),fac11(3)
        if (func_id(4,1) > 0) write(iout,1600) epsp11(4),func_id(4,1),fac11(4)
        if (func_id(5,1) > 0) write(iout,1600) epsp11(5),func_id(5,1),fac11(5)
!
        write(iout,1002)
        write(iout,1600) epsp22(1),func_id(1,2),fac22(1)
        if (func_id(2,2) > 0) write(iout,1600) epsp22(2),func_id(2,2),fac22(2)
        if (func_id(3,2) > 0) write(iout,1600) epsp22(3),func_id(3,2),fac22(3)
        if (func_id(4,2) > 0) write(iout,1600) epsp22(4),func_id(4,2),fac22(4)
        if (func_id(5,2) > 0) write(iout,1600) epsp22(5),func_id(5,2),fac22(5)
!
        write(iout,1003)
        write(iout,1600) epsp33(1),func_id(1,3),fac33(1)
        if (func_id(2,3) > 0) write(iout,1600) epsp33(2),func_id(2,3),fac33(2)
        if (func_id(3,3) > 0) write(iout,1600) epsp33(3),func_id(3,3),fac33(3)
        if (func_id(4,3) > 0) write(iout,1600) epsp33(4),func_id(4,3),fac33(4)
        if (func_id(5,3) > 0) write(iout,1600) epsp33(5),func_id(5,3),fac33(5)
!
        write(iout,1004)
        write(iout,1600) epsp12(1),func_id(1,4),fac12(1)
        if (func_id(2,4) > 0) write(iout,1600) epsp12(2),func_id(2,4),fac12(2)
        if (func_id(3,4) > 0) write(iout,1600) epsp12(3),func_id(3,4),fac12(3)
        if (func_id(4,4) > 0) write(iout,1600) epsp12(4),func_id(4,4),fac12(4)
        if (func_id(5,4) > 0) write(iout,1600) epsp12(5),func_id(5,4),fac12(5)
!
        write(iout,1005)
        write(iout,1600) epsp23(1),func_id(1,5),fac23(1)
        if (func_id(2,5) > 0) write(iout,1600) epsp23(2),func_id(2,5),fac23(2)
        if (func_id(3,5) > 0) write(iout,1600) epsp23(3),func_id(3,5),fac23(3)
        if (func_id(4,5) > 0) write(iout,1600) epsp23(4),func_id(4,5),fac23(4)
        if (func_id(5,5) > 0) write(iout,1600) epsp23(5),func_id(5,5),fac23(5)
!
        write(iout,1006)
        write(iout,1600) epsp31(1),func_id(1,6),fac31(1)
        if (func_id(2,6) > 0) write(iout,1600) epsp31(2),func_id(2,6),fac31(2)
        if (func_id(3,6) > 0) write(iout,1600) epsp31(3),func_id(3,6),fac31(3)
        if (func_id(4,6) > 0) write(iout,1600) epsp31(4),func_id(4,6),fac31(4)
        if (func_id(5,6) > 0) write(iout,1600) epsp31(5),func_id(5,6),fac31(5)
!
        if (iflag1+iflag2 /= 0) write(iout,1400) iflag1,iflag2
        if (emx11+emx22+emx33+emx12+emx23+emx31 /= 0) then
          write(iout,1500) emx11,emx22,emx33,emx12,emx23,emx31
        end if
        if (icompact == 1) then
          write(iout,1700) ecomp,nu,sigy,hcomp,vcomp
        end if
      endif     
!
      return
!-----------------------------------------------------------------------
 1000 format(/                                                           &
      5x,a,/,                                                            &
      5x,'   MATERIAL NUMBER. . . . . . . . . . .=',i10/,                &
      5x,'   MATERIAL LAW . . . . . . . . . . . .=',i10/)                 
 1100 format(5x,'MATERIAL MODEL : HONEYCOMB (VISC_HONEY)    ',/,         &
             5x,'-------------------------------------------',/)           
 1200 format(                                                            &
      5x,'INITIAL DENSITY . . . . . . . . . . . . .=',1pg20.13/)          
 1300 format(                                                            &
      5x,'E11 . . . . . . . . . . . . . . . . . . .=',1pg20.13/          &
      5x,'E22 . . . . . . . . . . . . . . . . . . .=',1pg20.13/          &
      5x,'E33 . . . . . . . . . . . . . . . . . . .=',1pg20.13/          &
      5x,'G12 . . . . . . . . . . . . . . . . . . .=',1pg20.13/          &
      5x,'G23 . . . . . . . . . . . . . . . . . . .=',1pg20.13/          &
      5x,'G31 . . . . . . . . . . . . . . . . . . .=',1pg20.13/          &
      5x,'STRAIN RATE DEFINITION FLAG . . . . . . .=',i10/               &          
      5x,'STRAIN RATE FILTERING CUT OFF FREQUENCY .=',1pg20.13)           
 1400 format(                                                            &
      5x,'YIELD FUNCTION 11,22,33 FLAG .  . . . . .=',i10/               &
      5x,'YIELD FUNCTION 12,23,31 FLAG .  . . . . .=',i10//)              
 1500 format(                                                            &
      5x,'TENSION FAILURE STRAIN 11 . . . . . . . .=',1pg20.13/          &
      5x,'TENSION FAILURE STRAIN 22 . . . . . . . .=',1pg20.13/          &
      5x,'TENSION FAILURE STRAIN 33 . . . . . . . .=',1pg20.13/          &
      5x,'SHEAR FAILURE STRAIN 12 . . . . . . . . .=',1pg20.13/          &
      5x,'SHEAR FAILURE STRAIN 23 . . . . . . . . .=',1pg20.13/          &
      5x,'SHEAR FAILURE STRAIN 31 . . . . . . . . .=',1pg20.13//)         
 1600 format(                                                            &
      8x,'STRAIN RATE . . . . . . . . . . . . . . .=',1pg20.13/          &
      8x,'  FUNCTION NUMBER . . . . . . . . . . . .=',i10/               &
      8x,'  SCALE FACTOR. . . . . . . . . . . . . .=',1pg20.13)
 1700 format(                                                            &
      5x,'YOUNG MODULUS IN COMPACTED STATE. . . . .=',1pg20.13/          &
      5x,'POISSON RATIO IN COMPACTED STATE. . . . .=',1pg20.13/          &
      5x,'YELD STRESS IN COMPACTED STATE. . . . . .=',1pg20.13/          &
      5x,'TANGENT MODULUS IN COMPACTED STATE. . . .=',1pg20.13/          &
      5x,'VOLUME FRACTION IN COMPACTED STATE. . . .=',1pg20.13//)
 1001 format(5x,'YIELD STRESS 11')
 1002 format(5x,'YIELD STRESS 22')
 1003 format(5x,'YIELD STRESS 33')
 1004 format(5x,'YIELD STRESS 12')
 1005 format(5x,'YIELD STRESS 23')
 1006 format(5x,'YIELD STRESS 31')
!-----------------------------------------------------------------------
      end subroutine hm_read_mat50

      end module hm_read_mat50_mod
