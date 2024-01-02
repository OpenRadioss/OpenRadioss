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
Chd|  LECLAS1                       source/loads/laser/leclas1.F  
Chd|-- called by -----------
Chd|        LECLAS                        source/loads/laser/leclas.F   
Chd|-- calls ---------------
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE LECLAS1(NL ,LAS ,NC, LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN)    :: NC, NL
      INTEGER,INTENT(INOUT) :: LAS(2,*)
      TYPE(SUBMODEL_DATA),INTENT(IN) ::LSUBMODEL(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NN,NK
      INTEGER J10(NL)
      LOGICAL IS_AVAILABLE
C---------------------------------
C List of plasma elements  
C---------------------------------

      !---READING
      DO J=1,NL
        CALL HM_GET_INT_ARRAY_INDEX('location_unit_element', J10(J), J, IS_AVAILABLE, LSUBMODEL)
        LAS(1,J) = J10(J)
      ENDDO
      
      !---OUTPUT
      N  = 0
      NN = NL / 10
      NK = MOD(NL,10)
      DO J=1,NN
        WRITE(IOUT,FMT_10I)J10((J-1)*10+1:J*10)
      ENDDO
      WRITE(IOUT,FMT_10I)J10(NN*10+1:NN*10+NK) 
      
      !---STORAGE              
      LAS(1,NL+1)=NC

      RETURN
      END
