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
      module connectivity_size_mod
        integer, parameter :: nixs = 11 !< size of IXS array
        integer, parameter :: nixc = 7  !< size of IXC array
        integer, parameter :: nixq = 7  !< size of IXQ array
        integer, parameter :: nixt = 5  !< size of IXT array
        integer, parameter :: nixp = 6  !< size of IXP array
        integer, parameter :: nixr = 6  !< size of IXR array
        integer, parameter :: nixtg = 6 !< size of IXTG array
        integer, parameter :: nixur = 6 !< size of IXUR array
      end module connectivity_size_mod
