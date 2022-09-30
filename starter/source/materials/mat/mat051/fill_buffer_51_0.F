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
Chd|  FILL_BUFFER_51_0                source/materials/mat/mat051/fill_buffer_51.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FILL_BUFFER_51_0(MATPARAM_TAB, IPM, PM, BUFMAT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE MATPARAM_DEF_MOD      
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is filling multimaterial law51 buffer UPARAM(:)
C
C It is done here and not in lecm51 because we need all material cards and eos card to be read for each submaterial
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "param_c.inc"
#include "com04_c.inc"
#include "scr17_c.inc"
#include "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(MATPARAM_STRUCT_) ,DIMENSION(NUMMAT), INTENT(INOUT) :: MATPARAM_TAB
      INTEGER,TARGET :: IPM(NPROPMI,NUMMAT)
      my_real,TARGET :: PM(NPROPM,NUMMAT),BUFMAT(SBUFMAT)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II,ILAW,IFORM,IADBUF,MAT_ID,NPAR
      CHARACTER*nchartitle TITR     
      my_real, DIMENSION(:), POINTER :: UPARAM       
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO II=1,NUMMAT-1
        TITR = MATPARAM_TAB(II)%TITLE        
        ILAW = IPM(2,II)
        IF(ILAW/=51)CYCLE
        IADBUF = IPM(7,II)
        NPAR = IPM(9,II)
        IADBUF = MAX(1,IADBUF)
        UPARAM => BUFMAT(IADBUF:IADBUF+NPAR)
        IFORM = NINT(UPARAM(31))
        IPM(62, II) = IFORM
        MAT_ID = IPM(1,II)
        IF(IFORM == 12)THEN
          CALL FILL_BUFFER_51( IPM, PM, UPARAM, BUFMAT, MAT_ID, TITR, II) 
          PM(38,II) = UPARAM(42)
        ENDIF
      ENDDO
C-----------------------------------------------

      RETURN
      END SUBROUTINE FILL_BUFFER_51_0
