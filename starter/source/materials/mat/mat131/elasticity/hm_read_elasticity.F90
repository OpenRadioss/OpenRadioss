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
!||    hm_read_elasticity_mod   ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity.F90
!||--- called by ------------------------------------------------------
!||    hm_read_elasto_plastic   ../starter/source/materials/mat/mat131/hm_read_elasto_plastic.F90
!||====================================================================
      module hm_read_elasticity_mod
        implicit none
      contains
!||====================================================================
!||    hm_read_elasticity                   ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity.F90
!||--- called by ------------------------------------------------------
!||    hm_read_elasto_plastic               ../starter/source/materials/mat/mat131/hm_read_elasto_plastic.F90
!||--- calls      -----------------------------------------------------
!||    hm_read_elasticity_isotropic         ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity_isotropic.F90
!||    hm_read_elasticity_orthotropic       ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity_orthotropic.F90
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod                   ../starter/share/modules1/hm_option_read_mod.F
!||    hm_read_elasticity_isotropic_mod     ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity_isotropic.F90
!||    hm_read_elasticity_orthotropic_mod   ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity_orthotropic.F90
!||    submodel_mod                         ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_elasticity(                                         &
          ikey     ,type     ,ielas    ,nupar_elas,upar_elas,is_available,     &
          unitab   ,lsubmodel,matparam ,parmat    ,iout     ,is_encrypted)
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
          use unitab_mod
          use submodel_mod
          use hm_option_read_mod
          use constant_mod
          use matparam_def_mod
          use precision_mod, only : WP
          use hm_read_elasticity_isotropic_mod
          use hm_read_elasticity_orthotropic_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
          implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
          integer,                 intent(in)    :: ikey                  !< material key
          character(len=20),       intent(in)    :: type                  !< keyword type
          integer,                 intent(inout) :: ielas                 !< elastic model type
          integer,                 intent(inout) :: nupar_elas            !< number of elastic parameters
          real(kind=WP),dimension(100),intent(inout) :: upar_elas         !< elastic parameters
          logical,                 intent(in)    :: is_available          !< availability flag
          type(unit_type_),        intent(in)    :: unitab                !< units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel  !< submodel data structure
          type(matparam_struct_),  intent(inout) :: matparam              !< matparam data structure
          real(kind=WP),           intent(inout) :: parmat(100)           !< material parameter global table 1
          integer,                 intent(in)    :: iout                  !< output unit
          logical,                 intent(in)    :: is_encrypted          !< encryption flag
!===============================================================================
! 
          !< Select elasticity type
          select case (type(1:4))
            !===================================================================
            !< Isotropic elasticity parameters
            !===================================================================
            case ('ISOT')
              call hm_read_elasticity_isotropic(                               &
                ikey     ,ielas    ,nupar_elas,upar_elas,is_available,         &
                unitab   ,lsubmodel,matparam  ,parmat   ,iout        ,         &
                is_encrypted)
            !===================================================================
            !< Orthotropic elasticity parameters
            !===================================================================
            case ('ORTH')
              call hm_read_elasticity_orthotropic(                             &
                ikey     ,ielas    ,nupar_elas,upar_elas,is_available,         &
                unitab   ,lsubmodel,matparam  ,parmat   ,iout        ,         &
                is_encrypted)
          end select
! -------------------------------------------------------------------------------
        end subroutine hm_read_elasticity
      end module hm_read_elasticity_mod