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
Chd|  SPMD_EXCH_I24_GAP             source/mpi/interfaces/spmd_exch_i24_gap.F
Chd|-- called by -----------
Chd|        I24PXFEM                      source/interfaces/int24/i24pxfem.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_I24_GAP(
     1           WA,IAD_ELEM ,FR_ELEM,SIZE  ,LENR  )
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
#include      "scr06_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*),FR_ELEM(*), SIZE, LENR
      my_real
     .        WA(2,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,IERROR, MSGOFF,
     .        SIZ,J,K,L,NB_NOD,
     .        STATUS(MPI_STATUS_SIZE),
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      DATA MSGOFF/232/

      my_real
     .        RBUF(SIZE*LENR ),
     .        SBUF(SIZE*LENR )
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      L = 1
      IAD_RECV(1) = 1
      DO I=1,NSPMD
        SIZ = SIZE*(IAD_ELEM(1,I+1)-IAD_ELEM(1,I))
        IF(SIZ.NE.0)THEN
          MSGTYP = MSGOFF
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
      END DO
      L = 1
      IAD_SEND(1) = 1
C
      DO I=1,NSPMD
C preparation envoi  partie fixe (elem) a proc I
#include  "vectorize.inc"
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            NOD = FR_ELEM(J)
            SBUF(L     ) =  WA(1,NOD)
            SBUF(L + 1 ) =  WA(2,NOD)
            L = L + SIZE
          END DO
C
         IAD_SEND(I+1) = L
      ENDDO
C
C   echange messages
C
      DO I=1,NSPMD
C--------------------------------------------------------------------
C envoi a N+I mod P
Cel test si msg necessaire a envoyer a completer par test interface
       IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I).GT.0)THEN
          MSGTYP = MSGOFF
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          L = IAD_SEND(I)
          CALL MPI_ISEND(
     .      SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     .      MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF
C--------------------------------------------------------------------
      ENDDO
C
C decompactage
C
      DO I = 1, NSPMD
Cel test si msg necessaire a envoyer a completer par test interface
        NB_NOD = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        IF(NB_NOD.GT.0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          L = IAD_RECV(I)
C
#include    "vectorize.inc"
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              WA(1,NOD) = MAX(WA(1,NOD),RBUF(L))
              WA(2,NOD) = MAX(WA(2,NOD),RBUF(L+1))
              L = L + SIZE
            END DO
C ---
        ENDIF
      ENDDO
C
Cel wait terminaison isend
C
      DO I = 1, NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
#endif
      RETURN
      END
