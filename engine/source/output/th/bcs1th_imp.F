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
Chd|  BCS1TH_IMP                    source/output/th/bcs1th_imp.F 
Chd|-- called by -----------
Chd|        THBCS_IMP                     source/output/th/thbcs_imp.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BCS1TH_IMP(NINDX,INDX,A,FTHREAC,NODREAC,N,
     &                      FTHDTM,DT3)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NINDX, N, INDX(*),NODREAC(*)
      my_real
     .   A(3,*),FTHREAC(6,*),DT3,FTHDTM(6,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "impl2_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, L
C-----------------------------------------------
C
#include "vectorize.inc"
      DO K = 1, NINDX
        L = INDX(K)
        FTHREAC(N+1,NODREAC(L)) = FTHREAC(N+1,NODREAC(L))
     &                           - A(1,L)*DT3/DEUX
     &                           - FTHDTM(N+1,NODREAC(L))*DT3/DEUX
        FTHREAC(N+2,NODREAC(L)) = FTHREAC(N+2,NODREAC(L))
     &                           - A(2,L)*DT3/DEUX
     &                           - FTHDTM(N+2,NODREAC(L))*DT3/DEUX
        FTHREAC(N+3,NODREAC(L)) = FTHREAC(N+3,NODREAC(L))
     &                           - A(3,L)*DT3/DEUX
     &                           - FTHDTM(N+3,NODREAC(L))*DT3/DEUX
        FTHDTM(N+1,NODREAC(L)) = A(1,L)
        FTHDTM(N+2,NODREAC(L)) = A(2,L)
        FTHDTM(N+3,NODREAC(L)) = A(3,L)
      ENDDO
C
      RETURN
      END

Chd|====================================================================
Chd|  BCS1AN_IMP                    source/output/th/bcs1th_imp.F 
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BCS1AN_IMP(NINDX,INDX,A,FANREAC,N)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NINDX, N, INDX(*)
      my_real
     .   A(3,*),FANREAC(6,*)
C-----------------------------------------------
      INTEGER K, L
C-----------------------------------------------
C
#include "vectorize.inc"
      DO K = 1, NINDX
        L = INDX(K)
        FANREAC(N+1,L) = - A(1,L)
        FANREAC(N+2,L) = - A(2,L)
        FANREAC(N+3,L) = - A(3,L)
      ENDDO
C
      RETURN
      END

