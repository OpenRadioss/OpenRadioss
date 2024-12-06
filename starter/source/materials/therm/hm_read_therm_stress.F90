!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
! ==================================================================================================
!                                                   PROCEDURES
! ==================================================================================================
!! \brief read input cards of /therm_stress option
!! \details
! ==================================================================================================
      !||====================================================================
      !||    hm_read_therm_stress_mod   ../starter/source/materials/therm/hm_read_therm_stress.F90
      !||--- called by ------------------------------------------------------
      !||    read_material_models       ../starter/source/materials/read_material_models.F
      !||====================================================================
      module hm_read_therm_stress_mod
      contains

      !||====================================================================
      !||    hm_read_therm_stress   ../starter/source/materials/therm/hm_read_therm_stress.F90
      !||--- called by ------------------------------------------------------
      !||    read_material_models   ../starter/source/materials/read_material_models.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||    hm_get_floatv          ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_intv            ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_count        ../starter/source/devtools/hm_reader/hm_option_count.F
      !||    hm_option_read_key     ../starter/source/devtools/hm_reader/hm_option_read_key.F
      !||    hm_option_start        ../starter/source/devtools/hm_reader/hm_option_start.F
      !||--- uses       -----------------------------------------------------
      !||    elbuftag_mod           ../starter/share/modules1/elbuftag_mod.F
      !||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||    submodel_mod           ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      subroutine hm_read_therm_stress(nummat   ,mat_param ,mlaw_tag ,unitab   ,lsubmodel ,   &
                                      iout     ,npropm   ,npropmi   ,ipm      ,pm       )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use unitab_mod
      use message_mod
      use submodel_mod
      use hm_option_read_mod
      use elbuftag_mod
      use names_and_titles_mod , only : nchartitle
      use matparam_def_mod
      use constant_mod, only : zero, one
!============================================================================
!                                                   Implicit none
! --------------------------------------------------------------------------------------------------
      implicit none
! --------------------------------------------------------------------------------------------------
!                                                   Included files
! --------------------------------------------------------------------------------------------------
#include "my_real.inc"
! --------------------------------------------------------------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer ,intent(in) :: nummat
      integer ,intent(in) :: npropm
      integer ,intent(in) :: npropmi
      integer ,intent(in) :: iout
      integer ,dimension(npropmi,nummat)  ,intent(inout) :: ipm
      my_real ,dimension(npropm ,nummat)  ,intent(inout) :: pm
      type (unit_type_)                   ,intent(in)    :: unitab 
      type(mlaw_tag_)        ,dimension(nummat) ,intent(inout) :: mlaw_tag
      type(submodel_data)    ,dimension(*)      ,intent(in)    :: lsubmodel
      type(matparam_struct_) ,dimension(nummat) ,intent(inout) :: mat_param
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i,ith,mat_id,imat,ilaw,jthe,ntherm_st,ifunc_alpha
      integer ,dimension(nummat) :: itherm_for
      my_real ::  fscal_alpha
      character(len=nchartitle) :: titr
      character key*80
      logical :: is_available
!=======================================================================

!--------------------------------------------------
!     count eos models using cfg files
!--------------------------------------------------
!      
      call hm_option_count('/therm_stress',ntherm_st)
!
!--------------------------------------------------
!     start browsing /therm_stress models
!--------------------------------------------------
!
      call hm_option_start('/therm_stress')
!
!--------------------------------------------------
      do ith = 1,ntherm_st
!
        call hm_option_read_key(lsubmodel, option_id = mat_id , option_titr = titr , keyword2 = key )

        if (key(1:3) == 'mat') then   
          call hm_get_intv  ('funct_id'      ,ifunc_alpha    ,is_available, lsubmodel) 
          call hm_get_floatv('cload_scale_y' ,fscal_alpha    ,is_available, lsubmodel, unitab)

          if (fscal_alpha == zero) fscal_alpha=one
          do imat=1,nummat-1
            if (mat_id == mat_param(i)%mat_id) then
              titr = mat_param(imat)%title
              ilaw = mat_param(imat)%ilaw
              jthe = mat_param(imat)%itherm
              if (jthe == 0) then
                 call ancmsg(msgid=1129, msgtype=msgerror, anmode=aninfo, i1=mat_id, c1=titr)
              endif
              mat_param(imat)%iexpan = 1
              mat_param(imat)%therm%func_thexp  = ifunc_alpha
              mat_param(imat)%therm%scale_thexp = fscal_alpha
!              
              mlaw_tag(imat)%g_temp  = 1
              mlaw_tag(imat)%l_temp  = 1
              
              ! tmp double parameter storage - to be cleaned
              ipm(218,imat)  = 1
              ipm(219,imat)  = ifunc_alpha
              pm(191 ,imat)  = fscal_alpha

              !-------------------------------------------------
              write(iout,4000) mat_id,ifunc_alpha,fscal_alpha
              if (pm(72, imat) > one) then
                 ! euler or ale material
                 call ancmsg(msgid=1723,msgtype=msgerror,anmode=aninfo,      &
                             i1=imat,c1=titr)
              endif
              
            endif  
          enddo    ! nummat
        endif
      enddo        ! ith = 1,ntherm_st
!-----------------------------------------      
 4000 format(                                                         &
      5x,'    THERMAL MATERIAL EXPANSION  ',/,                        &
      5x,'    --------------------------  ',/,                        &
      5x,'MATERIAL NUMBER . . . . . . . . . . . . .=',i10/,           &
      5x,'CURVE ID DEFINING THERMAL EXPANSION COEFFICIENT '/,         &
      5x,'   AS A FUNCTION OF TEMPERATURE         .=',i10/,           &
      5x,'THERMAL EXPANSION FUNCTION SCALE FACTOR .=',1pg20.13//)
!-----------------------------------------      
      return
      end
!-----------------------------------------      
      end module hm_read_therm_stress_mod
