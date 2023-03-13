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
C
Chd|====================================================================
Chd|  SPMD_EXCH_VOL                 source/mpi/nodes/spmd_exch_vol.F
Chd|-- called by -----------
Chd|        S4ALESFEM                     source/elements/solid/solide4_sfem/s4alesfem.F
Chd|        S4LAGSFEM                     source/elements/solid/solide4_sfem/s4lagsfem.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_VOL(VOLNOD6,VARNOD6,IAD_ELEM, 
     .                         FR_ELEM,LENR )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*),FR_ELEM(*),LENR
      DOUBLE PRECISION
     .        VOLNOD6(6,*), VARNOD6(6,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,IERROR,MSGOFF,
     .        SIZ,J,L,NB_NOD,SIZ6,LEN,
     .        STATUS(MPI_STATUS_SIZE),
     .        IAD_SEND(NSPMD+1),
     .        IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      DATA MSGOFF/231/

      DOUBLE PRECISION,
     . DIMENSION(:,:),ALLOCATABLE :: RBUF, SBUF 
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      ALLOCATE(RBUF(6,LENR))
      ALLOCATE(SBUF(6,LENR))

      LOC_PROC = ISPMD + 1
      SIZ6 = 6

      L = 1
      IAD_RECV(1)  = 1

      DO I=1,NSPMD
        LEN = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        SIZ = SIZ6 * 2 * LEN
        IF(SIZ/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_IRECV(
     S      RBUF(1,L),SIZ,MPI_DOUBLE_PRECISION,
     G      IT_SPMD(I),MSGTYP,MPI_COMM_WORLD,
     H      REQ_R(I),IERROR)
          L = L  + 2*LEN
        ENDIF
        IAD_RECV(I+1)  = L
      END DO

      L  = 1
      IAD_SEND(1)  = L

      DO I=1,NSPMD
        NB_NOD = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
#include      "vectorize.inc"
        DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
          NOD = FR_ELEM(J)
c
          SBUF(1, L)   =  VOLNOD6(1,NOD)
          SBUF(2, L)   =  VOLNOD6(2,NOD)
          SBUF(3, L)   =  VOLNOD6(3,NOD)
          SBUF(4, L)   =  VOLNOD6(4,NOD)
          SBUF(5, L)   =  VOLNOD6(5,NOD)
          SBUF(6, L)   =  VOLNOD6(6,NOD)
c
          SBUF(1, L+ NB_NOD)   =  VARNOD6(1,NOD)
          SBUF(2, L+ NB_NOD)   =  VARNOD6(2,NOD)
          SBUF(3, L+ NB_NOD)   =  VARNOD6(3,NOD)
          SBUF(4, L+ NB_NOD)   =  VARNOD6(4,NOD)
          SBUF(5, L+ NB_NOD)   =  VARNOD6(5,NOD)
          SBUF(6, L+ NB_NOD)   =  VARNOD6(6,NOD)
c
          L  = L  + 1
        END DO

        L  = L  + NB_NOD
        IAD_SEND(I+1)  = L
      ENDDO
C
C   echange messages
C
      DO I=1,NSPMD
C--------------------------------------------------------------------
C envoi a N+I mod P
       NB_NOD = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
       IF(NB_NOD>0)THEN
          MSGTYP = MSGOFF 
          LEN = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)

          SIZ = LEN * 2 * SIZ6       
          L = IAD_SEND(I)
          CALL MPI_ISEND(
     S      SBUF(1,L),SIZ,MPI_DOUBLE_PRECISION,
     G      IT_SPMD(I),MSGTYP,MPI_COMM_WORLD,
     H      REQ_S(I),IERROR)
       ENDIF
C--------------------------------------------------------------------
      ENDDO
C
C decompactage
C
      DO I = 1, NSPMD
        NB_NOD = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        IF(NB_NOD>0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          L  = IAD_RECV(I)

#include        "vectorize.inc"
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            NOD = FR_ELEM(J)
            VOLNOD6(1,NOD) = VOLNOD6(1,NOD) + RBUF(1,L)
            VOLNOD6(2,NOD) = VOLNOD6(2,NOD) + RBUF(2,L)
            VOLNOD6(3,NOD) = VOLNOD6(3,NOD) + RBUF(3,L)
            VOLNOD6(4,NOD) = VOLNOD6(4,NOD) + RBUF(4,L)
            VOLNOD6(5,NOD) = VOLNOD6(5,NOD) + RBUF(5,L)
            VOLNOD6(6,NOD) = VOLNOD6(6,NOD) + RBUF(6,L)
c
            VARNOD6(1,NOD) = VARNOD6(1,NOD) + RBUF(1,L+ NB_NOD) 
            VARNOD6(2,NOD) = VARNOD6(2,NOD) + RBUF(2,L+ NB_NOD) 
            VARNOD6(3,NOD) = VARNOD6(3,NOD) + RBUF(3,L+ NB_NOD) 
            VARNOD6(4,NOD) = VARNOD6(4,NOD) + RBUF(4,L+ NB_NOD) 
            VARNOD6(5,NOD) = VARNOD6(5,NOD) + RBUF(5,L+ NB_NOD) 
            VARNOD6(6,NOD) = VARNOD6(6,NOD) + RBUF(6,L+ NB_NOD) 
c
            L  = L  + 1
          END DO
        ENDIF
      END DO
C
Cel wait terminaison isend
C
      DO I = 1, NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
#endif
      RETURN
      END
