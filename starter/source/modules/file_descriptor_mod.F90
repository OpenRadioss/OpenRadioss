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
      !||====================================================================
      !||    file_descriptor_mod   ../starter/source/modules/file_descriptor_mod.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_inivol        ../starter/source/initial_conditions/inivol/hm_read_inivol.F90
      !||    hm_read_mat           ../starter/source/materials/mat/hm_read_mat.F90
      !||====================================================================
      module file_descriptor_mod
      !=======================================================================================================================
      !!\brief default type : Hosts the parameters for Starter File Descriptors
      !=======================================================================================================================
         integer, parameter :: istdo = 6
         integer, parameter :: iout = 7
      end module file_descriptor_mod