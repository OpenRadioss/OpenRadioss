Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|====================================================================
Chd|  ALEFLOW_MOD                   share/modules/aleflow_mod.F   
Chd|-- called by -----------
Chd|        ALELAG_RREST                  source/output/restart/rdresb.F
Chd|        ALELAG_WREST                  source/output/restart/wrrest.F
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
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
