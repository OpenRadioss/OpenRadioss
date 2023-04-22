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
Chd|  TRANSK                        source/elements/solid/solide8s/transk.F
Chd|-- called by -----------
Chd|        S8SKE3                        source/elements/solid/solide8s/s8ske3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TRANSK(
     1   KL,      TRM,     NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
      DOUBLE PRECISION
     .   KL(24,24,NEL),KTEMP(24,24),TRM(NEL,24,24)
      INTEGER I,L,M,N,NEL

      DO I=1,NEL
        !CALL DGEMM('N','N',24,24,24,ONE,KL(1,1,I),24,TRM(I,1,1),24,ZERO,
        KTEMP(1:24,1:24) = ZERO
        DO L=1,24
         DO M=1,24
          DO N=1,24
           KTEMP(L,M) = KTEMP(L,M)+KL(L,N,I)*TRM(I,N,M)
          ENDDO
         ENDDO
        ENDDO
        
        KL(1:24,1:24,I) = ZERO
        DO L=1,24
         DO M=1,24
          DO N=1,24
           KL(L,M,I) = KL(L,M,I)+TRM(I,N,L)*KTEMP(N,M)
          ENDDO
         ENDDO
        ENDDO
       ENDDO 
c       
       RETURN
       END
