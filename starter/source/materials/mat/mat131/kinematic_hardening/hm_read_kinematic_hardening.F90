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
!||    hm_read_kinematic_hardening_mod   ../starter/source/materials/mat/mat131/kinematic_hardening/hm_read_kinematic_hardening.F90
!||--- called by ------------------------------------------------------
!||    hm_read_elasto_plastic            ../starter/source/materials/mat/mat131/hm_read_elasto_plastic.F90
!||====================================================================
      module hm_read_kinematic_hardening_mod
        implicit none
! \brief Read kinematic hardening input data for /MAT/LAW131
! \details Read and dispatch the kinematic hardening model input data
!          for /MAT/LAW131 (elasto-plastic material law).
      contains
!||====================================================================
!||    hm_read_kinematic_hardening                ../starter/source/materials/mat/mat131/kinematic_hardening/hm_read_kinematic_hardening.F90
!||--- called by ------------------------------------------------------
!||    hm_read_elasto_plastic                     ../starter/source/materials/mat/mat131/hm_read_elasto_plastic.F90
!||--- calls      -----------------------------------------------------
!||    hm_read_kinematic_hardening_chaboche       ../starter/source/materials/mat/mat131/kinematic_hardening/hm_read_kinematic_hardening_chaboche.F90
!||    hm_read_kinematic_hardening_prager         ../starter/source/materials/mat/mat131/kinematic_hardening/hm_read_kinematic_hardening_prager.F90
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod                               ../starter/share/modules1/elbuftag_mod.F
!||    hm_option_read_mod                         ../starter/share/modules1/hm_option_read_mod.F
!||    hm_read_kinematic_hardening_chaboche_mod   ../starter/source/materials/mat/mat131/kinematic_hardening/hm_read_kinematic_hardening_chaboche.F90
!||    hm_read_kinematic_hardening_prager_mod     ../starter/source/materials/mat/mat131/kinematic_hardening/hm_read_kinematic_hardening_prager.F90
!||    submodel_mod                               ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_kinematic_hardening(                                &
          ikey     ,type  ,ikine    ,nupar_kine  ,upar_kine    ,is_available,  &
          unitab,lsubmodel,iout     ,is_encrypted,mtag         ,chard       ) 
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
          use unitab_mod
          use submodel_mod
          use hm_option_read_mod
          use constant_mod
          use matparam_def_mod
          use elbuftag_mod
          use precision_mod, only : WP
          use hm_read_kinematic_hardening_prager_mod
          use hm_read_kinematic_hardening_chaboche_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
          implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
          integer,                 intent(in)    :: ikey                  !< Material key
          character(len=20),       intent(in)    :: type                  !< Keyword type
          integer,                 intent(inout) :: ikine                 !< Kinematic hardening type
          integer,                 intent(inout) :: nupar_kine            !< Number of kinematic hardening parameters
          real(kind=WP),dimension(100),intent(inout) :: upar_kine         !< Kinematic hardening parameters
          logical,                 intent(in)    :: is_available          !< Availability flag
          type(unit_type_),        intent(in)    :: unitab                !< Units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel  !< Submodel data structure
          integer,                 intent(in)    :: iout                  !< Output unit
          logical,                 intent(in)    :: is_encrypted          !< Encryption flag
          type(mlaw_tag_),         intent(inout) :: mtag                  !< Material tag for internal variables in element buffer
          real(kind=WP),           intent(inout) :: chard                 !< Kinematic hardening parameter
!===============================================================================
!     
          !=====================================================================
          !< Prager kinematic hardening parameters
          !=====================================================================
          if (type(1:6) == 'PRAGER') then
            call hm_read_kinematic_hardening_prager(                           &
              ikey     ,ikine    ,nupar_kine,chard      ,is_available,         &
              unitab   ,lsubmodel,iout     ,is_encrypted,mtag      )
          !=====================================================================
          !< Chaboche-Rousselier kinematic hardening parameters
          !=====================================================================
          elseif (type(1:8) == 'CHABOCHE') then
            call hm_read_kinematic_hardening_chaboche(                         &
              ikey     ,ikine    ,nupar_kine,upar_kine  ,chard       ,         &
              is_available,unitab,lsubmodel ,iout       ,is_encrypted,         &
              mtag     )
          endif
! -------------------------------------------------------------------------------
        end subroutine hm_read_kinematic_hardening
      end module hm_read_kinematic_hardening_mod