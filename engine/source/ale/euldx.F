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
Chd|  EULDX                         source/ale/euldx.F            
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE EULDX(V,D,X,DDP,NALE,NODFT,NODLT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NALE(NUMNOD),NODFT,NODLT
      my_real X(3,NUMNOD), D(3,NUMNOD), V(3,NUMNOD)  
      REAL(kind=8), DIMENSION(3,*), INTENT(INOUT) :: DDP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  N, L
      my_real XLAG
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
        IF(N2D == 1)THEN
          DO N=NODFT,NODLT
            IF(X(2,N)+DT2*V(2,N) < ZERO)V(2,N)=ZERO
          ENDDO
        ENDIF
        DO N=NODFT,NODLT
          XLAG=MIN(1,IABS(IABS(NALE(N))-2))
          D(1,N)=D(1,N)+DT2*V(1,N)*XLAG
          D(2,N)=D(2,N)+DT2*V(2,N)*XLAG
          D(3,N)=D(3,N)+DT2*V(3,N)*XLAG
          X(1,N)=X(1,N)+DT2*V(1,N)*XLAG
          X(2,N)=X(2,N)+DT2*V(2,N)*XLAG
          X(3,N)=X(3,N)+DT2*V(3,N)*XLAG
        ENDDO
!   -------------------------
!   single precision
        IF(IRESP==1) THEN
            DO N=NODFT,NODLT
                DDP(1,N)= D(1,N)
                DDP(2,N)= D(2,N)
                DDP(3,N)= D(3,N)
            ENDDO
        ENDIF    
C-----------------------------------------------       
      RETURN
      END
