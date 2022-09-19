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
Chd|  WRITE_ARRAY                   comm/write_array.F            
Chd|-- called by -----------
Chd|        WRCOMI                        engine/source/output/restart/wrcomm.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE WRITE_ARRAY(ARRAY,IDX1,IDX2,TABVINT, LVARINT)
C-----------------------------------------------
C   Description
C-----------------------------------------------
C This function is recording a given integer array
C from range ARRAY(IDX1,IDX2)
C inside TABVINT array which will we used to transmit
C data from Strater/Engine
C Of course only non zero values are stored.
C Non zero values are first counted. This size is also stored.
C Then array is scan to store only non zero values.
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
       INTEGER,INTENT(IN)    :: ARRAY(*)
       INTEGER,INTENT(IN)    :: IDX1,IDX2
       INTEGER,INTENT(INOUT) :: LVARINT,TABVINT(*)
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
      !IF (SIZE(ARRAY)<=0)RETURN
      !LB=LBOUND(ARRAY)
      !UB=UBOUND(ARRAY)
      !IF(IDX1<LB)IDX1=LB
      !IF(IDX2>UB)IDX2=UB
      !IF (IDX2<IDX1)RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------
      NUM=0
      DO I=IDX1,IDX2
        IF(ARRAY(I)/=0)NUM=NUM+1
      ENDDO
      LVARINT=LVARINT+1
      TABVINT(LVARINT)=NUM

      DO I=IDX1,IDX2
        IF(ARRAY(I)/=0)THEN
          LVARINT=LVARINT+1
          TABVINT(LVARINT)=I
        ENDIF
      ENDDO

      RETURN
C-----------------------------------------------
      END SUBROUTINE WRITE_ARRAY
