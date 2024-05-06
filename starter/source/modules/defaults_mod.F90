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
      module defaults_mod
!=======================================================================================================================
!!\brief default type : Hosts the variables for /DEF/xxx Starter Deck option
!=======================================================================================================================

        ! Variables from /DEF/SHELL Option.
        type shell_defaults_
          integer ::  ioffset    !< 3: removing offset by projection in starter; 1: offset for contact (Engine)
        end type  shell_defaults_

        ! --------------------------------
        ! Variables from /DEF/SOLID Option.
        type solid_defaults_
          integer ::  dummy    !< dummy variable to create the type - to be removed when /DEF_SOLID is merged in
        end type  solid_defaults_

        ! --------------------------------
        ! Variables from /DEF/INTER Option.
        type interface_defaults_
          integer ::  dummy    !< dummy variable to create the type - to be removed when /DEFAULT/INTER is merged in
        end type  interface_defaults_

        ! --------------------------------
        ! Variables from /DEF/xxx Option.
        type defaults_
          type (shell_defaults_)     :: shell       !< /DEF_SHELL option
          type (solid_defaults_)     :: solid       !< /DEF_SOLID option
          type (interface_defaults_) :: interface   !< /DEFAULT/INTER option
        end type  defaults_

      end module defaults_mod
