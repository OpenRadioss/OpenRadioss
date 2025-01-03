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
      !||    fxb_mod      ../engine/share/modules/fxb_mod.F
      !||--- called by ------------------------------------------------------
      !||    fxbresb      ../engine/source/output/restart/rdresb.F
      !||    fxbwrest     ../engine/source/output/restart/wrrest.F
      !||    lectur       ../engine/source/input/lectur.F
      !||    resol_head   ../engine/source/engine/resol_head.F
      !||    restalloc    ../engine/source/output/restart/arralloc.F
      !||====================================================================
      MODULE FXB_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------

      INTEGER, DIMENSION(:,:), ALLOCATABLE :: FXBIPM

      INTEGER, DIMENSION(:), ALLOCATABLE :: FXBNOD, FXBELM,
     .                                      FXBGRVI      
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE ::
     .   FXBMOD

      my_real
     .       , DIMENSION(:), ALLOCATABLE ::
     .   FXBRPM, FXBGLM, FXBCPM, FXBCPS, FXBLM,
     .   FXBFLS, FXBDLS, FXBDEP, FXBVIT, FXBACC, FXBSIG,
     .   FXBGRVR
     
C-----------------------------------------------
      END MODULE FXB_MOD
