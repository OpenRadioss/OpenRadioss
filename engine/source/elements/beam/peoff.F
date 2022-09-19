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
Chd|  PEOFF                         source/elements/beam/peoff.F  
Chd|-- called by -----------
Chd|        PKE3                          source/elements/beam/pke3.F   
Chd|        R13KE3                        source/elements/spring/r13ke3.F
Chd|        R4KE3                         source/elements/spring/r4ke3.F
Chd|        R8KE3                         source/elements/spring/r8ke3.F
Chd|        RUSER32KE3                    source/elements/spring/ruser32ke3.F
Chd|        TKE3                          source/elements/truss/tke3.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PEOFF(NIXPL, JFT , JLT , IXP, ETAG,
     .                 OFF )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIXPL, JFT, JLT, IXP(NIXPL,*), ETAG(*)
      my_real
     .        OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IEL, I, N1, N2, NALL
C
      DO IEL=JFT,JLT
         N1=IXP(2,IEL)
         N2=IXP(3,IEL)
         NALL=ETAG(N1)*ETAG(N2)
         OFF(IEL)=MIN(NALL,1)
      ENDDO
C
      RETURN
      END

