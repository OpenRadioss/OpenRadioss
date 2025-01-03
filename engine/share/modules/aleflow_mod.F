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
      !||    aleflow_mod    ../engine/share/modules/aleflow_mod.F
      !||--- called by ------------------------------------------------------
      !||    alelag_rrest   ../engine/source/output/restart/rdresb.F
      !||    alelag_wrest   ../engine/source/output/restart/wrrest.F
      !||    resol          ../engine/source/engine/resol.F
      !||====================================================================
      MODULE ALEFLOW_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
C   G l o b a l   V a r i a b l e s   D e f i n i t i o n
C-----------------------------------------------
        my_real, DIMENSION(:), ALLOCATABLE :: AFLOW,
     .                          DFLOW,VFLOW,FFSKY,WFLOW 
        INTEGER, DIMENSION(:), ALLOCATABLE :: IFOAM,IFOAM_CONT
      END MODULE ALEFLOW_MOD
