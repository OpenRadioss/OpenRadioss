!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
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
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
      module law137_init_mod
      contains
        subroutine law137_init(mat_param,nel    ,pla      ) 
! ------------------------------------------------------------------------------
!           Modules
! ------------------------------------------------------------------------------
          use precision_mod, only : wp
          use matparam_def_mod
          use constant_mod
! ------------------------------------------------------------------------------
          implicit none
! ------------------------------------------------------------------------------
!          A r g u m e n t s
! ------------------------------------------------------------------------------
          integer                 ,intent(in)    :: nel
          real(kind=WP)           ,intent(inout) :: pla(nel)
          type (matparam_struct_) ,intent(in)    :: mat_param
! ------------------------------------------------------------------------------
!         Local variables
! ------------------------------------------------------------------------------
          real(kind=WP) :: epsini
!===============================================================================
          epsini = mat_param%uparam(9)
!------------------------------------------
          !< Plastic strain initialization
          if (epsini /= zero) then
            pla(1:nel) = epsini
          endif
!-------------
          return
        end subroutine law137_init
      end module law137_init_mod
