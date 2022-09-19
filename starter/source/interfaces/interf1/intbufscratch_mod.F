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
Chd|  INTBUFSCRATCH_MOD             source/interfaces/interf1/intbufscratch_mod.F
Chd|-- called by -----------
Chd|        ININTR                        source/interfaces/interf1/inintr.F
Chd|        ININTR2                       source/interfaces/inter3d1/inintr2.F
Chd|        ININTR_THKVAR                 source/interfaces/interf1/inintr_thkvar.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        PRESCRINT                     source/interfaces/interf1/prescrint.F
Chd|        SCRINT                        source/interfaces/interf1/scrint.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE INTBUFSCRATCH_MOD
      TYPE SCRATCH_STRUCT_
        INTEGER, DIMENSION(:) , POINTER ::  WA    !
        INTEGER SINSCR
      END TYPE SCRATCH_STRUCT_
      END MODULE INTBUFSCRATCH_MOD
