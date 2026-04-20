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
! \brief Read elasticity input data for /MAT/LAW131
! \details Read and dispatch the elasticity model input data
!          for /MAT/LAW131 (elasto-plastic material law).
      contains
!||====================================================================
!||    hm_read_elasticity                         ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity.F90
!||--- called by ------------------------------------------------------
!||    hm_read_elasto_plastic                     ../starter/source/materials/mat/mat131/hm_read_elasto_plastic.F90
!||--- calls      -----------------------------------------------------
!||    hm_read_elasticity_anisotropic             ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity_anisotropic.F90
!||    hm_read_elasticity_bimod_isotropic         ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity_bimod_isotropic.F90
!||    hm_read_elasticity_isotropic               ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity_isotropic.F90
!||    hm_read_elasticity_orthotropic             ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity_orthotropic.F90
!||    hm_read_elasticity_temp_isotropic          ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity_temp_isotropic.F90
!||    hm_read_elasticity_viscous_isotropic       ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity_viscous_isotropic.F90
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod                               ../starter/share/modules1/elbuftag_mod.F
!||    hm_option_read_mod                         ../starter/share/modules1/hm_option_read_mod.F
!||    hm_read_elasticity_anisotropic_mod         ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity_anisotropic.F90
!||    hm_read_elasticity_bimod_isotropic_mod     ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity_bimod_isotropic.F90
!||    hm_read_elasticity_isotropic_mod           ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity_isotropic.F90
!||    hm_read_elasticity_orthotropic_mod         ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity_orthotropic.F90
!||    hm_read_elasticity_temp_isotropic_mod      ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity_temp_isotropic.F90
!||    hm_read_elasticity_viscous_isotropic_mod   ../starter/source/materials/mat/mat131/elasticity/hm_read_elasticity_viscous_isotropic.F90
!||    submodel_mod                               ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_elasticity(                                         &
          ikey     ,type     ,ielas    ,nupar_elas,upar_elas,is_available,     &
          unitab   ,lsubmodel,matparam ,parmat    ,iout     ,is_encrypted,     &
          mat_id   ,titr     ,iresp    ,ntab_elas ,itab_elas,x2vect      ,     &
          x3vect   ,x4vect   ,fscale   ,nvartmp   ,israte   ,vpflag      ,     &
          mtag     ,nuvar_elas)
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
          use unitab_mod
          use submodel_mod
          use hm_option_read_mod
          use constant_mod
          use matparam_def_mod
          use precision_mod, only : WP
          use elbuftag_mod
          use hm_read_elasticity_isotropic_mod
          use hm_read_elasticity_orthotropic_mod
          use hm_read_elasticity_anisotropic_mod
          use hm_read_elasticity_viscous_isotropic_mod
          use hm_read_elasticity_temp_isotropic_mod
          use hm_read_elasticity_bimod_isotropic_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
          implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
          integer,                 intent(in)    :: ikey                  !< Material key
          character(len=20),       intent(in)    :: type                  !< Keyword type
          integer,                 intent(inout) :: ielas                 !< Elastic model type
          integer,                 intent(inout) :: nupar_elas            !< Number of elastic parameters
          real(kind=WP),dimension(100),intent(inout) :: upar_elas         !< Elastic parameters
          logical,                 intent(in)    :: is_available          !< Availability flag
          type(unit_type_),        intent(in)    :: unitab                !< Units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel  !< Submodel data structure
          type(matparam_struct_),  intent(inout) :: matparam              !< Matparam data structure
          real(kind=WP),           intent(inout) :: parmat(100)           !< Material parameter global table 1
          integer,                 intent(in)    :: iout                  !< Output unit
          logical,                 intent(in)    :: is_encrypted          !< Encryption flag
          integer, intent(in)                    :: mat_id                !< Material law user ID
          character(len=nchartitle),intent(in)   :: titr                  !< Material law user title
          integer, intent(in)                    :: iresp                 !< Flag for single precision
          integer,                 intent(inout) :: ntab_elas             !< Number of tabulated elasticity dependency functions/tables
          integer,       dimension(100), intent(inout) :: itab_elas       !< Identifiers of tabulated elasticity dependency functions/tables
          real(kind=WP), dimension(100), intent(inout) :: x2vect          !< x2 scale factor for tabulated elasticity dependency
          real(kind=WP), dimension(100), intent(inout) :: x3vect          !< x3 scale factor for tabulated elasticity dependency
          real(kind=WP), dimension(100), intent(inout) :: x4vect          !< x4 scale factor for tabulated elasticity dependency
          real(kind=WP), dimension(100), intent(inout) :: fscale          !< y  scale factor for tabulated elasticity dependency
          integer,                 intent(inout) :: nvartmp               !< Number of variables used in tabulated elasticity dependency
          integer,                 intent(inout) :: israte                !< Strain rate filtering flag
          integer,                 intent(inout) :: vpflag                !< Viscous formulation flag
          type(mlaw_tag_),         intent(inout) :: mtag                  !< Material tag for internal variables in element buffer
          integer,                 intent(inout) :: nuvar_elas            !< Number of user variables for elasticity
!===============================================================================
! 
          !=====================================================================
          !< Isotropic elasticity parameters
          !=====================================================================
          if (type(1:9) == 'ISOTROPIC') then
            call hm_read_elasticity_isotropic(                                 &
              ikey     ,ielas    ,nupar_elas,upar_elas,is_available,           &
              unitab   ,lsubmodel,matparam  ,parmat   ,iout        ,           &
              is_encrypted,mat_id,titr      )
          !=====================================================================
          !< Orthotropic elasticity parameters
          !=====================================================================
          elseif (type(1:11) == 'ORTHOTROPIC') then
            call hm_read_elasticity_orthotropic(                               &
              ikey     ,ielas    ,nupar_elas,upar_elas,is_available,           &
              unitab   ,lsubmodel,matparam  ,parmat   ,iout        ,           &
              is_encrypted,mat_id,titr      )
          !=====================================================================
          !< Anisotropic elasticity parameters
          !=====================================================================
          elseif (type(1:11) == 'ANISOTROPIC') then
            call hm_read_elasticity_anisotropic(                               &
              ikey     ,ielas    ,nupar_elas,upar_elas,is_available,           &
              unitab   ,lsubmodel,matparam  ,parmat   ,iout        ,           &
              is_encrypted,mat_id,titr      ,iresp    )
          !=====================================================================
          !< Viscous isotropic elasticity parameters
          !=====================================================================
          elseif (type(1:14) == 'VISC_ISOTROPIC') then
            call hm_read_elasticity_viscous_isotropic(                         &
              ikey     ,ielas    ,nupar_elas,upar_elas,is_available,           &
              unitab   ,lsubmodel,matparam  ,parmat   ,iout        ,           &
              is_encrypted,mat_id,titr      ,ntab_elas,itab_elas   ,           &
              x2vect   ,x3vect   ,x4vect    ,fscale   ,nvartmp     ,           &
              israte   ,vpflag   )
          !=====================================================================
          !< Temperature-dependent isotropic elasticity parameters
          !=====================================================================
          elseif (type(1:14) == 'TEMP_ISOTROPIC') then
            call hm_read_elasticity_temp_isotropic(                            &
              ikey     ,ielas    ,nupar_elas,is_available,                     &
              unitab   ,lsubmodel,matparam  ,parmat   ,iout        ,           &
              is_encrypted,mat_id,titr      ,ntab_elas,itab_elas   ,           &
              x2vect   ,x3vect   ,x4vect    ,fscale   ,nvartmp     ,           &
              mtag     ,nuvar_elas)
          !=====================================================================
          !< Bimodular isotropic elasticity parameters
          !=====================================================================
          elseif (type(1:15) == 'BIMOD_ISOTROPIC') then
            call hm_read_elasticity_bimod_isotropic(                           &
              ikey     ,ielas    ,nupar_elas,upar_elas,is_available,           &
              unitab   ,lsubmodel,matparam  ,parmat   ,iout        ,           &
              is_encrypted,mat_id,titr      )
          endif
! -------------------------------------------------------------------------------
        end subroutine hm_read_elasticity
      end module hm_read_elasticity_mod