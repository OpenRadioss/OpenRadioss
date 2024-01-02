Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  DSGRAPH_MOD                   share/modules1/dsgraph_mod.F  
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|        DSWREST                       source/restart/ddsplit/wrrest.F
Chd|        DSWRESTP                      source/restart/ddsplit/wrrest.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      MODULE DSGRAPH_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C-----------------------------------------------
      TYPE SUPEL
         INTEGER :: NDDL_I, NDDL_F, NSDMAX
         INTEGER, DIMENSION(:), POINTER :: DDLS, CHILD, IFACM
         INTEGER, DIMENSION(:,:), POINTER :: IFAC
      END TYPE SUPEL
C      
      TYPE PRGRAPH
         INTEGER :: NDDL, NSUPEL, NSLEVEL, NSDEC, NSVMAX, IPRI,
     .              NDDL_GLOB
         my_real
     .           :: CUTFREQ
         INTEGER, DIMENSION(:), POINTER :: LSDDL_GLOB
         INTEGER, DIMENSION(:,:), POINTER :: LSDDL
         TYPE(SUPEL), DIMENSION(:), POINTER :: DGRAPH
      END TYPE PRGRAPH
C
      END MODULE DSGRAPH_MOD
