Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      !||====================================================================
      !||    interface_modification_mod   ../engine/share/modules/interface_modification_mod.F
      !||--- called by ------------------------------------------------------
      !||    intcrit                      ../engine/source/interfaces/intsort/intcrit.F
      !||    mpp_init                     ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_initfi                  ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_savefi                  ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||--- uses       -----------------------------------------------------
      !||    pointerdef                   ../engine/share/modules/pointerdef_mod.F
      !||====================================================================
        MODULE INTERFACE_MODIFICATION_MOD
            USE POINTERDEF
            TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: NSNSI_SAV
        END MODULE INTERFACE_MODIFICATION_MOD
