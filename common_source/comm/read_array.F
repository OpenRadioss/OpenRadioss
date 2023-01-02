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
Chd|  READ_ARRAY                    comm/read_array.F             
Chd|-- called by -----------
Chd|        RDCOMI                        engine/source/output/restart/rdcomm.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE READ_ARRAY(ARRAY,TABVINT, LVARINT, M_CHECK)
C-----------------------------------------------
C   Description
C-----------------------------------------------
C This function is reading array in TAVINT
C TABVINT(LVARINT) = SIZE
C output ARRAY(1:SIZE) = TABVINT(LVARINT+1:LVARINT+SIZE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "scr14_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER,INTENT(INOUT) :: ARRAY(*),LVARINT
       INTEGER,INTENT(IN)    :: TABVINT(*),M_CHECK
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LB,UB
      INTEGER NUM
      INTEGER I
C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
C
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------
      LVARINT=LVARINT+1
      NUM=TABVINT(LVARINT)
      
      IF(M_CHECK/=0)THEN
        DO I=1,NUM
          LVARINT=LVARINT+1
          ARRAY(TABVINT(LVARINT))=1
        ENDDO
      ELSE
        LVARINT=LVARINT+NUM      
      ENDIF

      RETURN 

C-----------------------------------------------
      END SUBROUTINE READ_ARRAY
