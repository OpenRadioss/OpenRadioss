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
Chd|  S20EOFF                       source/elements/solid/solide20/s20eoff.F
Chd|-- called by -----------
Chd|        S20KE3                        source/elements/solid/solide20/s20ke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S20EOFF(JFT , JLT , IXS ,IXS20, ETAG, OFF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, IXS(NIXS,*), IXS20(12,*), ETAG(*)
      my_real
     .        OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IEL, I,  N1 , N2,  N3, N4, N5, N6, N7, N8,
     .         N9 , N10, N11, N12, N13, N14, N15, N16,
     .         N17, N18, N19, N20, NALL
C
      DO IEL=JFT,JLT
         N1=IXS(2,IEL)
         N2=IXS(3,IEL)
         N3=IXS(4,IEL)
         N4=IXS(5,IEL)
         N5=IXS(6,IEL)
         N6=IXS(7,IEL)
         N7=IXS(8,IEL)
         N8=IXS(9,IEL)
      N9  =IXS20(1,IEL)
      N10 =IXS20(2,IEL)
      N11 =IXS20(3,IEL)
      N12 =IXS20(4,IEL)
      N13 =IXS20(5,IEL)
      N14 =IXS20(6,IEL)
      N15 =IXS20(7,IEL)
      N16 =IXS20(8,IEL)
      N17 =IXS20(9,IEL)
      N18 =IXS20(10,IEL)
      N19 =IXS20(11,IEL)
      N20 =IXS20(12,IEL)
         NALL=ETAG(N1)*ETAG(N2)*ETAG(N3)*ETAG(N4)
     .       *ETAG(N5)*ETAG(N6)*ETAG(N7)*ETAG(N8)
     .       *ETAG(N9)*ETAG(N10)*ETAG(N11)*ETAG(N12)
     .       *ETAG(N13)*ETAG(N14)*ETAG(N15)*ETAG(N16)
     .       *ETAG(N17)*ETAG(N18)*ETAG(N19)*ETAG(N20)
         OFF(IEL)=MIN(NALL,1)
      ENDDO
C
      RETURN
      END

