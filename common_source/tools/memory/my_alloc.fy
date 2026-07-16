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
! ======================================================================================================================
! NOTE: my_alloc.F90 is now hand-written (not fypp-generated).
!
! The generated allocation subroutines have been split into two fypp templates for
! faster parallel compilation:
!
!   my_alloc_impl_idx4.fy  →  my_alloc_impl_idx4.F90  (integer(4) dimension args, 132 subroutines)
!   my_alloc_impl_idx8.fy  →  my_alloc_impl_idx8.F90  (integer(8) dimension args, 132 subroutines)
!
! Both include the shared macro and type definitions from:
!   my_alloc_impl_core.fy  (fypp include only — do NOT invoke fypp on this file directly)
!
! Utility routines (my_alloc_check, record_alloc_addr, etc.) live in:
!   my_alloc_tools.F90     (hand-written, defines my_alloc_tools_mod)
!
! To regenerate after editing the templates:
!   fypp my_alloc_impl_idx4.fy my_alloc_impl_idx4.F90
!   fypp my_alloc_impl_idx8.fy my_alloc_impl_idx8.F90
!
! my_alloc.F90 aggregates all sub-modules and remains the sole file users need to reference.
! ======================================================================================================================
