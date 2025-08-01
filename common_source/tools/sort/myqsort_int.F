Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    myqsort_int                ../common_source/tools/sort/myqsort_int.F
!||--- called by ------------------------------------------------------
!||    find_surface_inter         ../engine/source/interfaces/interf/find_surface_inter.F
!||    get_segment_orientation    ../engine/source/interfaces/interf/get_segment_orientation.F90
!||    init_nodal_state           ../engine/source/interfaces/interf/init_nodal_state.F
!||    nloc_count_solnod          ../engine/source/elements/solid/solide/nloc_count_solnod.F90
!||    nloc_dmg_init              ../starter/source/materials/fail/nloc_dmg_init.F
!||    select_s2s                 ../starter/source/interfaces/inter3d1/select_s2s.F90
!||    spmd_cell_size_exchange    ../engine/source/mpi/interfaces/spmd_cell_size_exchange.F
!||    update_neighbour_segment   ../engine/source/interfaces/interf/update_neighbour_segment.F90
!||====================================================================
      SUBROUTINE MYQSORT_INT(N, A, PERM, ERROR)
C-----------------------------------------------
c     q u i c k s o r t
C Sedgewick algorithm from "Implementing Quicksort Programs" ; int version
C     A: data
C     N: len
C     PERM: permutations
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N,ERROR,PERM(N)
      INTEGER :: A(N)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: STACKLEN
      INTEGER :: TRESHOLD
      INTEGER :: DONE
C the max STACKLEN <= 1 + 2 x log2 (N+1)/(TRESHOLD + 2)
      PARAMETER( STACKLEN = 128 ,
     .           TRESHOLD   =  9 )
C
      INTEGER :: I 
      INTEGER :: IPLUS1
      INTEGER :: J
      INTEGER :: JMINUS1
      INTEGER :: K 
      INTEGER :: LEFT
      INTEGER :: LLEN
      INTEGER :: RIGHT 
      INTEGER :: RLEN
      INTEGER :: TOP  
      INTEGER :: STACK(STACKLEN)
C     REAL ou REAL*8
      my_real
     .        RK, RV
C
      ERROR = 0
C
      IF  (N < 1)  THEN
        ERROR = -1
        RETURN
      ENDIF

      IF  (N == 1)  THEN
         PERM(1)=1
         RETURN
      ENDIF

      DO  I = 1, N
         PERM(I) = I
      ENDDO
C
      TOP = 1
      LEFT = 1
      RIGHT = N
      IF (N <= TRESHOLD) THEN
        DONE = 1
      ELSE
        DONE = 0
      ENDIF

c     QUICKSORT                                                              
c
       DO WHILE (DONE /= 1)
         RK = A((LEFT+RIGHT)/2)
         A((LEFT+RIGHT)/2) = A(LEFT)
         A(LEFT) = RK
C
         K = PERM((LEFT+RIGHT)/2)
         PERM((LEFT+RIGHT)/2) = PERM(LEFT)
         PERM(LEFT) = K

         IF( A(LEFT+1) > A(RIGHT) ) THEN
           RK = A(LEFT+1)
           A(LEFT+1) = A(RIGHT)
           A(RIGHT) = RK
           K = PERM(LEFT+1)
           PERM(LEFT+1) = PERM(RIGHT)
           PERM(RIGHT) = K
         ENDIF
         IF( A(LEFT) > A(RIGHT) ) THEN
           RK = A(LEFT)
           A(LEFT) = A(RIGHT)
           A(RIGHT) = RK
           K = PERM(LEFT)
           PERM(LEFT) = PERM(RIGHT)
           PERM(RIGHT) = K
         ENDIF
         IF( A(LEFT+1) >  A(LEFT) ) THEN
           RK = A(LEFT+1)
           A(LEFT+1) = A(LEFT)
           A(LEFT) = RK
           K = PERM(LEFT+1)
           PERM(LEFT+1) = PERM(LEFT)
           PERM(LEFT) = K
         ENDIF

         RV = A(LEFT)
         I = LEFT+1
         J = RIGHT

         DO WHILE(J >= I)
           I  = I + 1
           DO WHILE(A(I) <  RV) 
             I = I +1
           ENDDO
           J = J - 1
           DO WHILE(A(J) > RV)
             J = J - 1  
           ENDDO
           IF (J >= I) THEN 
             RK = A(I)
             A(I) = A(J)
             A(J) = RK
             K = PERM(I)
             PERM(I) = PERM(J)
             PERM(J) = K
           ENDIF
         ENDDO
C
         RK = A(LEFT)
         A(LEFT) = A(J)
         A(J) = RK
C
         K = PERM(LEFT)
         PERM(LEFT) = PERM(J)
         PERM(J) = K
C
         LLEN = J-LEFT
         RLEN = RIGHT - I + 1

         IF(MAX(LLEN, RLEN) <= TRESHOLD ) THEN
             IF  (TOP == 1) THEN
               DONE = 1
             ELSE
               TOP = TOP - 2
               LEFT = STACK(TOP)
               RIGHT = STACK(TOP+1)
             ENDIF
         ELSE IF(MIN(LLEN, RLEN) <=  TRESHOLD) THEN 
             IF( LLEN > RLEN ) THEN 
               RIGHT = J - 1
             ELSE
               LEFT = I
             ENDIF
         ELSE
           IF( LLEN > RLEN ) THEN 
              STACK(TOP) = LEFT
              STACK(TOP+1) = J-1
              LEFT = I
            ELSE
              STACK(TOP) = I
              STACK(TOP+1) = RIGHT
              RIGHT = J-1
            ENDIF
            TOP = TOP + 2
         ENDIF
       END DO
c
c     INSERTION SORT 
c
      I = N - 1
      IPLUS1 = N
      DO WHILE (I > 0) 
        IF( A(I) > A(IPLUS1) ) THEN 
          RK = A(I)
          K  = PERM(I)
          J = IPLUS1
          JMINUS1 = I
          DO WHILE(A(J) <  RK)
            A(JMINUS1) = A(J) 
            PERM(JMINUS1) = PERM(J)
            JMINUS1 = J
            J = J + 1
            IF  ( J > N )  EXIT
          ENDDO
          A(JMINUS1) = RK
          PERM(JMINUS1) = K
        ENDIF
C
        IPLUS1 = I
        I = I - 1
      ENDDO
c
      RETURN
c
c     -------------------
c
      end
