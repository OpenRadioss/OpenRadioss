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
Chd|====================================================================
Chd|  LOI_FLUID                     source/fluid/loi_fluid.F      
Chd|-- called by -----------
Chd|        QDLEN2                        source/elements/solid_2d/quad/qdlen2.F
Chd|-- calls ---------------
Chd|====================================================================
      LOGICAL FUNCTION LOI_FLUID(MLN)
#include      "implicit_f.inc"
      INTEGER MLN
      IF(MLN.EQ.5   .OR.
     .   MLN.EQ.6   .OR.
     .   MLN.EQ.11  .OR.
     .   MLN.EQ.17  .OR.
     .   MLN.EQ.37  .OR.
     .   MLN.EQ.51  .OR.
     .   MLN.EQ.46  .OR.
     .   MLN.EQ.47  .OR.
     .   MLN.EQ.97  .OR.
     .   MLN.EQ.105 .OR.
     .   MLN.EQ.151)THEN
       LOI_FLUID=.TRUE.
      ELSE
       LOI_FLUID=.FALSE.
      ENDIF
      RETURN
      END
