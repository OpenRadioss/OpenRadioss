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
C-----------------------------------------------------------------------
C     REAL
#include "my_real.inc"
Chd|====================================================================
Chd|  RSTR                          source/tools/univ/rstr.F      
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      my_real FUNCTION RSTR(STR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER STR*8
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "ascchr_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IS, IEXP, MANT11, MANT12, MANT21, MANT22, MANT31, MANT32,
     .   ISM, ISE, MANT1, MANT2, MANT3
C
      IS=IASCOD(ICHAR(STR(1:1)))-1
      IEXP=IASCOD(ICHAR(STR(2:2)))
      MANT11=IASCOD(ICHAR(STR(3:3)))
      MANT12=IASCOD(ICHAR(STR(4:4)))
      MANT21=IASCOD(ICHAR(STR(5:5)))
      MANT22=IASCOD(ICHAR(STR(6:6)))
      MANT31=IASCOD(ICHAR(STR(7:7)))
      MANT32=IASCOD(ICHAR(STR(8:8)))
C
      ISM=IS/2
      ISE=IS-2*ISM
      MANT1=MANT11*32+MANT12
      MANT2=MANT21*32+MANT22
      MANT3=MANT31*32+MANT32
C
      IF(ISE==1)IEXP=-IEXP
      RSTR=MANT3+MANT2*1000+MANT1*1000000
      RSTR=RSTR/1000000000.
      RSTR=RSTR*TEN**IEXP
      IF(ISM==1)RSTR=-RSTR
C
      RETURN
      END
