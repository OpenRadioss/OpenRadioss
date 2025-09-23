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
      module element_mod
! ----------------------------------------------------------------------------------------------------------------------
        integer, parameter :: nixs = 11 !< first dimension of ixs array (solid element)
        integer, parameter :: nixs10 = 6 !< first dimension of ixs10 array (tetrahedral element)
        integer, parameter :: nixs16 = 8 !< first dimension of ixs16 array (solid 16-node element)
        integer, parameter :: nixs20 = 12 !< first dimension of ixs20 array (solid 20-node element)
        integer, parameter :: nixc = 7 !< first dimension of ixc array (shell element)        
        integer, parameter :: nixt = 5 !< first dimension of ixt array (truss element)
        integer, parameter :: nixq = 7 !< first dimension of ixq array (quad element)
        integer, parameter :: nixp = 6 !< first dimension of ixp array (beam element)
        integer, parameter :: nixr = 6 !< first dimension of ixr array (spring element)
        integer, parameter :: nixtg = 6 !< first dimension of ixtg array (shell 3n / triangle element)
        integer, parameter :: nixur = 6 !< first dimension of ixur array (user element)     
! ----------------------------------------------------------------------------------------------------------------------        
      end module element_mod
