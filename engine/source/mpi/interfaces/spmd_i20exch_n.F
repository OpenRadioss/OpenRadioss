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
Chd|  SPMD_I20EXCH_N                source/mpi/interfaces/spmd_i20exch_n.F
Chd|-- called by -----------
Chd|        I20MAINF                      source/interfaces/int20/i20mainf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_I20EXCH_N(XNORM,IAD_ELEM,FR_ELEM,LENR)
C realise la sommation des normales aux points frontiere
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
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
      INTEGER IAD_ELEM(2,*),FR_ELEM(*), SIZE, LENR
      INTEGER XNORM(3,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,IERROR,MSGOFF,
     .        SIZ,J,K,L,NB_NOD,
     .        STATUS(MPI_STATUS_SIZE),
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      INTEGER SBUF(LENR*3),RBUF(LENR*3)
      DATA MSGOFF/163/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      LOC_PROC = ISPMD+1
      L = 1
      IAD_RECV(1)=1

C   Mise en place des buffers de reception
      DO I=1,NSPMD
         SIZ = 3*(IAD_ELEM(1,I+1)-IAD_ELEM(1,I))
         IF(SIZ.NE.0) THEN
           MSGTYP = MSGOFF 
           CALL MPI_IRECV(
     S      RBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
           L = L+SIZ
         END IF
         IAD_RECV(I+1) = L
      END DO

C   Remplissage des buffers d'envoi et envoi des donnees
      L=1
      IAD_SEND(1)=1
      DO I=1,NSPMD
         DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            NOD = FR_ELEM(J)
            SBUF(L  ) = XNORM(1,NOD)
            SBUF(L+1) = XNORM(2,NOD)
            SBUF(L+2) = XNORM(3,NOD)
            L = L+3
         END DO
        IAD_SEND(I+1) = L
      END DO

      DO I=1,NSPMD

       IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I).GT.0)THEN

          MSGTYP = MSGOFF 
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          L = IAD_SEND(I)
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF
      END DO

C   Reception et traitement
      DO I = 1, NSPMD
C
        NB_NOD = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        IF(NB_NOD.GT.0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          L = IAD_RECV(I)

          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1

             NOD = FR_ELEM(J)
             XNORM(1,NOD) = XNORM(1,NOD) + RBUF(L)
             XNORM(2,NOD) = XNORM(2,NOD) + RBUF(L+1)
             XNORM(3,NOD) = XNORM(3,NOD) + RBUF(L+2)
             L = L+3
          ENDDO
        ENDIF
      ENDDO

      DO I = 1, NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO

#endif
      RETURN
      END
