Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Cf51e6 Nouvelle routine
Chd|====================================================================
Chd|  UINTBUF_MOD                   source/user_interface/uintbuf_mod.F
Chd|-- called by -----------
Chd|        I2RUPT                        source/interfaces/interf/int2rupt.F
Chd|        RUPTINT2                      source/interfaces/interf/ruptint2.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE UINTBUF_MOD
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C-----------------------------------------------
      TYPE UINTBUF
#include "sequence.inc"
	INTEGER ISECND
        my_real AREA
        my_real DT
        my_real DXN
        my_real DXT
        my_real SIGN
        my_real SIGT
        my_real RUPT
        my_real FACN
        my_real FACT
      END TYPE UINTBUF
C-----
C
      END MODULE UINTBUF_MOD

