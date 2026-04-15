!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    hm_read_yield_criterion_mod   ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion.F90
!||--- called by ------------------------------------------------------
!||    hm_read_elasto_plastic        ../starter/source/materials/mat/mat131/hm_read_elasto_plastic.F90
!||====================================================================
      module hm_read_yield_criterion_mod
        implicit none
! \brief Read yield criterion input data for /MAT/LAW131
! \details Read and dispatch the yield criterion model input data
!          for /MAT/LAW131 (elasto-plastic material law).
      contains
!||====================================================================
!||    hm_read_yield_criterion                  ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion.F90
!||--- called by ------------------------------------------------------
!||    hm_read_elasto_plastic                   ../starter/source/materials/mat/mat131/hm_read_elasto_plastic.F90
!||--- calls      -----------------------------------------------------
!||    hm_read_yield_criterion_barlat1989       ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion_barlat1989.F90
!||    hm_read_yield_criterion_barlat2000       ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion_barlat2000.F90
!||    hm_read_yield_criterion_hershey          ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion_hershey.F90
!||    hm_read_yield_criterion_hill             ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion_hill.F90
!||    hm_read_yield_criterion_vonmises         ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion_vonmises.F90
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod                       ../starter/share/modules1/hm_option_read_mod.F
!||    hm_read_yield_criterion_barlat1989_mod   ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion_barlat1989.F90
!||    hm_read_yield_criterion_barlat2000_mod   ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion_barlat2000.F90
!||    hm_read_yield_criterion_hershey_mod      ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion_hershey.F90
!||    hm_read_yield_criterion_hill_mod         ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion_hill.F90
!||    hm_read_yield_criterion_vonmises_mod     ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion_vonmises.F90
!||    submodel_mod                             ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_yield_criterion(                                    &
          ikey     ,type     ,icrit    ,nupar_crit,upar_crit,is_available,     &
          unitab   ,lsubmodel,iout     ,is_encrypted,mat_id ,titr        )
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
          use unitab_mod
          use submodel_mod
          use hm_option_read_mod
          use constant_mod
          use matparam_def_mod
          use precision_mod, only : WP
          use hm_read_yield_criterion_vonmises_mod
          use hm_read_yield_criterion_hershey_mod
          use hm_read_yield_criterion_hill_mod
          use hm_read_yield_criterion_barlat1989_mod
          use hm_read_yield_criterion_barlat2000_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
          implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
          integer,                 intent(in)    :: ikey                  !< Material key
          character(len=20),       intent(in)    :: type                  !< Keyword type
          integer,                 intent(inout) :: icrit                 !< Yield criterion type
          integer,                 intent(inout) :: nupar_crit            !< Number of yield criterion parameters
          real(kind=WP),dimension(100),intent(inout) :: upar_crit         !< Yield criterion parameters
          logical,                 intent(in)    :: is_available          !< Availability flag
          type(unit_type_),        intent(in)    :: unitab                !< Units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel  !< Submodel data structure
          integer,                 intent(in)    :: iout                  !< Output unit
          logical,                 intent(in)    :: is_encrypted          !< Encryption flag
          integer,                 intent(in)    :: mat_id                !< Material ID
          character(len=nchartitle),intent(in)   :: titr                  !< Material law user title
!===============================================================================
!     
          !=====================================================================
          !< Von Mises criterion parameters
          !=====================================================================
          if (type(1:8) == 'VONMISES') then 
            call hm_read_yield_criterion_vonmises(                             &
              icrit    ,nupar_crit,iout     ,is_encrypted)
          !=====================================================================
          !< Hershey criterion parameters
          !=====================================================================
          elseif (type(1:7) == 'HERSHEY') then 
            call hm_read_yield_criterion_hershey(                              &
              ikey     ,icrit    ,nupar_crit,upar_crit,is_available,unitab ,   &
              lsubmodel,iout     ,is_encrypted) 
          !=====================================================================
          !< Hill criterion parameters
          !=====================================================================
          elseif (type(1:6) == 'HILL_1') then
            call hm_read_yield_criterion_hill(                                 &
              ikey     ,icrit    ,nupar_crit,upar_crit,is_available,unitab ,   &
              lsubmodel,iout     ,is_encrypted,1      )     
          elseif (type(1:6) == 'HILL_2') then
            call hm_read_yield_criterion_hill(                                 &
              ikey     ,icrit    ,nupar_crit,upar_crit,is_available,unitab ,   &
              lsubmodel,iout     ,is_encrypted,2      )    
          elseif (type(1:6) == 'HILL_3') then
            call hm_read_yield_criterion_hill(                                 &
              ikey     ,icrit    ,nupar_crit,upar_crit,is_available,unitab ,   &
              lsubmodel,iout     ,is_encrypted,3      )    
          !=====================================================================
          !< Barlat 1989 criterion parameters
          !=====================================================================  
          elseif (type(1:10) == 'BARLAT1989') then
            call hm_read_yield_criterion_barlat1989(                           &
              ikey     ,icrit    ,nupar_crit,upar_crit,is_available,unitab ,   &
              lsubmodel,iout     ,is_encrypted)         
          !=====================================================================
          !< Barlat 2000 criterion parameters
          !=====================================================================  
          elseif (type(1:12) == 'BARLAT2000_1') then
            call hm_read_yield_criterion_barlat2000(                           &
              ikey     ,icrit    ,nupar_crit,upar_crit,is_available,unitab ,   &
              lsubmodel,iout     ,is_encrypted,mat_id ,titr        ,1      )     
          elseif (type(1:12) == 'BARLAT2000_2') then
            call hm_read_yield_criterion_barlat2000(                           &
              ikey     ,icrit    ,nupar_crit,upar_crit,is_available,unitab ,   &
              lsubmodel,iout     ,is_encrypted,mat_id ,titr        ,2      )     
          endif          
! -------------------------------------------------------------------------------
        end subroutine hm_read_yield_criterion
      end module hm_read_yield_criterion_mod