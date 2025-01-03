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
      !||    intbufscratch_mod   ../starter/source/interfaces/interf1/intbufscratch_mod.F
      !||--- called by ------------------------------------------------------
      !||    inintr              ../starter/source/interfaces/interf1/inintr.F
      !||    inintr2             ../starter/source/interfaces/inter3d1/inintr2.F
      !||    inintr_thkvar       ../starter/source/interfaces/interf1/inintr_thkvar.F
      !||    lectur              ../starter/source/starter/lectur.F
      !||    prescrint           ../starter/source/interfaces/interf1/prescrint.F
      !||    scrint              ../starter/source/interfaces/interf1/scrint.F
      !||====================================================================
      MODULE INTBUFSCRATCH_MOD
      TYPE SCRATCH_STRUCT_
        INTEGER, DIMENSION(:) , POINTER ::  WA    !  
        INTEGER SINSCR
      END TYPE SCRATCH_STRUCT_
      END MODULE INTBUFSCRATCH_MOD
