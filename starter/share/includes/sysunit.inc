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
C.../SYSUNIT/
      COMMON /SYSUNIT/ FAC_MASS, FAC_LENGTH, FAC_TIME, 
     .         FAC_M_INPUT,FAC_L_INPUT,FAC_T_INPUT, 
     .         FAC_M_WORK,FAC_L_WORK,FAC_T_WORK,
     .         KEYM,KEYL,KEYT,KEYMI,KEYLI,KEYTI
      COMMON /ISYSUNIT/ NUNIT0,NUNITS,LUNIT,UNITID_SUB,
     .         FLAG_KEY_M,FLAG_KEY_L,FLAG_KEY_T
      my_real
     .         FAC_MASS, FAC_LENGTH, FAC_TIME, 
     .         FAC_M_INPUT,FAC_L_INPUT,FAC_T_INPUT, 
     .         FAC_M_WORK,FAC_L_WORK,FAC_T_WORK
      INTEGER  NUNIT0,NUNITS,LUNIT,UNITID_SUB,
     .         FLAG_KEY_M,FLAG_KEY_L,FLAG_KEY_T
      CHARACTER*20  KEYM,KEYL,KEYT,KEYMI,KEYLI,KEYTI
