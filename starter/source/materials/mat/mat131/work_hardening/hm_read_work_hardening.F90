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
      module hm_read_work_hardening_mod
        implicit none
      contains
        subroutine hm_read_work_hardening(                                     &
          ikey     ,type     ,ihard    ,nupar_hard,upar_hard,ntab_hard,        &
          itab_hard,x2vect   ,x3vect   ,x4vect    ,fscale   ,nvartmp  ,        &
          is_available,unitab,lsubmodel,iout      ,is_encrypted,vpflag,        &
          israte   ,parmat   )
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
          use unitab_mod
          use submodel_mod
          use hm_option_read_mod
          use constant_mod
          use matparam_def_mod
          use precision_mod, only : WP
          use hm_read_work_hardening_powerlaw_mod
          use hm_read_work_hardening_voce_mod
          use hm_read_work_hardening_tabulated_mod
          use hm_read_work_hardening_linearvoce_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
          implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
          integer,                 intent(in)    :: ikey                  !< Material key
          character(len=20),       intent(in)    :: type                  !< Keyword type
          integer,                 intent(inout) :: ihard                 !< Work hardening type
          integer,                 intent(inout) :: nupar_hard            !< Number of work hardening parameters
          real(kind=WP),dimension(100),intent(inout) :: upar_hard         !< Work hardening parameters
          integer,                 intent(inout) :: ntab_hard             !< Number of tabulated hardening functions/tables
          integer, dimension(100), intent(inout) :: itab_hard             !< Identifiers of tabulated hardening functions/tables
          real(kind=WP),dimension(100),intent(inout) :: x2vect            !< X2 scale factor for tabulated hardening
          real(kind=WP),dimension(100),intent(inout) :: x3vect            !< X3 scale factor for tabulated hardening
          real(kind=WP),dimension(100),intent(inout) :: x4vect            !< X4 scale factor for tabulated hardening
          real(kind=WP),dimension(100),intent(inout) :: fscale            !< Y  scale factor for tabulated hardening
          integer,                 intent(inout) :: nvartmp               !< Number of variables used in tabulated hardening
          logical,                 intent(in)    :: is_available          !< Availability flag
          type(unit_type_),        intent(in)    :: unitab                !< Units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel  !< Submodel data structure
          integer,                 intent(in)    :: iout                  !< Output unit
          logical,                 intent(in)    :: is_encrypted          !< Encryption flag
          integer,                 intent(inout) :: vpflag                !< Strain rate flag
          integer,                 intent(inout) :: israte                !< Strain rate filtering flag
          real(kind=WP),dimension(100),intent(inout) :: parmat            !< Material parameters
!===============================================================================
!     
          !< Select work hardening type
          select case (type(1:4))
            !===================================================================
            !< Power law hardening parameters
            !===================================================================
            case ('POWE')
              call hm_read_work_hardening_powerlaw(                            &
                ikey     ,ihard    ,nupar_hard,upar_hard,is_available,         &
                unitab   ,lsubmodel,iout     ,is_encrypted)
            !===================================================================
            !< Power law hardening parameters
            !===================================================================
            case ('VOCE')
              call hm_read_work_hardening_voce(                                &
                ikey     ,ihard    ,nupar_hard,upar_hard,is_available,         &
                unitab   ,lsubmodel,iout     ,is_encrypted)
            !===================================================================
            !< Tabulated hardening parameters
            !===================================================================
            case ('TAB ')
              call hm_read_work_hardening_tabulated(                           &
                ikey     ,ihard    ,ntab_hard ,itab_hard   ,x2vect ,x3vect   , &
                x4vect   ,fscale   ,nvartmp   ,is_available,unitab ,lsubmodel, &
                iout     ,is_encrypted,vpflag ,israte      ,parmat ,nupar_hard,&
                upar_hard)
            !===================================================================
            !< Linear-Voce hardening parameters
            !===================================================================
            case ('LINV')
              call hm_read_work_hardening_linearvoce(                          &
                ikey     ,ihard    ,nupar_hard,upar_hard,is_available,         &
                unitab   ,lsubmodel,iout     ,is_encrypted)
            !===================================================================
          end select
! -------------------------------------------------------------------------------
        end subroutine hm_read_work_hardening
      end module hm_read_work_hardening_mod