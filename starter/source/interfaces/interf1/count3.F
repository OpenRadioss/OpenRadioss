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
C
Chd|====================================================================
Chd|  COUNT3                        source/interfaces/interf1/count3.F
Chd|-- called by -----------
Chd|        INSLIN                        source/interfaces/interf1/inslin.F
Chd|        INSURF                        source/interfaces/interf1/insurf.F
Chd|        INSURF23                      source/interfaces/interf1/insurf23.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|====================================================================
      SUBROUTINE COUNT3(IRECT,MNN,N,NRT,NTAG)
C-----------------------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, NRT
      INTEGER IRECT(*), MNN(*)
      INTEGER, DIMENSION(0:2*NUMNOD), INTENT(INOUT) ::  NTAG
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IWORK(70000)
      INTEGER I, NN, K,NINDEX0
        INTEGER, DIMENSION(:), ALLOCATABLE ::  INDEX0
C-----------------------------------------------
C
      ALLOCATE(INDEX0(2*NUMNOD))
      NTAG(0) = 1      ! special case IRECT=0
      N = 0
      NINDEX0 = 0
      DO I=1,NRT*4     ! 4 : size of IRECT
        NN = IRECT(I)
        IF(NTAG(NN) == 0) THEN
          N=N+1
          MNN(N)=NN
          NTAG(NN)=1
          NINDEX0 = NINDEX0 + 1
          INDEX0(NINDEX0) = NN
        END IF            
      ENDDO
C Need to sort MNM but avoid treatment over NUMNOD if too big
      IF(N>NUMNOD/10)THEN  ! test to check which treatment is best
        N=0
        DO I=1,NUMNOD
          IF(NTAG(I) /= 0) THEN
            N=N+1
            MNN(N)=I
            NTAG(I) = 0
          END IF
        END DO
      ELSE   ! N needs to be at leas < NUMNOD/2
        CALL MY_ORDERS(0,IWORK,MNN,NTAG(1),N,1)
        DO I=1,N
          NTAG(N+I)=MNN(I)
        END DO
        DO I=1,N
          MNN(I)=NTAG(N+NTAG(I))
        END DO
        DO I=1,N
          NTAG(N+NTAG(I)) = 0
        ENDDO
        DO I=1,N
          NTAG(N+I) = 0
          NTAG(I) = 0
        ENDDO
      END IF

#include "vectorize.inc"
      DO K = 1,NINDEX0
        NTAG(INDEX0(K)) = 0
      ENDDO        

      DEALLOCATE(INDEX0)
C
      RETURN
      END
