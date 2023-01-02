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
Chd|  SPMD_EXCH_STIF_SCND           source/mpi/elements/spmd_exch_stif_scnd.F
Chd|-- called by -----------
Chd|        S10STFE_POFF                  source/elements/solid/solide10/s10cndf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_STIF_SCND(
     1            STIFND,FR_CDNS,IAD_CDNS, LCOMM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
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
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LCOMM, FR_CDNS(*), IAD_CDNS(*)
      my_real
     .        STIFND(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,LOC_PROC,NOD,I,J,L,IDEB,IAD,LEN,
     .        NBINDEX,INDEX,MSGOFF,SIZ,IERROR,
     .        STATUS(MPI_STATUS_SIZE),
     .        REQ_S(NSPMD),REQ_R(NSPMD),INDEXI(NSPMD)
      DATA MSGOFF/1179/
      my_real
     .        SBUF(LCOMM),RBUF(LCOMM)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
      IDEB = 1
      L = 0
c      ISIZE = 1
      DO I = 1, NSPMD
        LEN = IAD_CDNS(I+1)-IAD_CDNS(I)
        IF(LEN>0) THEN
          SIZ = LEN
          L=L+1
          INDEXI(L)=I
          MSGTYP = MSGOFF
          CALL MPI_IRECV(
     S      RBUF(IDEB),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(L),IERROR)
          IDEB = IDEB + SIZ
        ENDIF
      ENDDO
      NBINDEX = L
C
      IDEB = 1
      DO L = 1, NBINDEX
        I = INDEXI(L)
        LEN = IAD_CDNS(I+1) - IAD_CDNS(I)
        IAD = IAD_CDNS(I)-1
        DO J = 1, LEN
         NOD = FR_CDNS(IAD+J)
         SBUF(IDEB) = STIFND(NOD)
         IDEB = IDEB + 1
        ENDDO
      ENDDO
C
      IDEB = 1
      DO L=1,NBINDEX
        I = INDEXI(L)
        LEN = IAD_CDNS(I+1)-IAD_CDNS(I)
        SIZ = LEN
        MSGTYP = MSGOFF
        CALL MPI_ISEND(
     S    SBUF(IDEB),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G    MPI_COMM_WORLD,REQ_S(L),IERROR)
        IDEB = IDEB + SIZ
      ENDDO
C
      DO L=1,NBINDEX
        CALL MPI_WAITANY(NBINDEX,REQ_R,INDEX,STATUS,IERROR)
        I = INDEXI(INDEX)
        IDEB = IAD_CDNS(I)
        LEN = IAD_CDNS(I+1)-IAD_CDNS(I)
        IAD = IAD_CDNS(I)-1
        DO J = 1, LEN
         NOD = FR_CDNS(IAD+J)
         STIFND(NOD) = STIFND(NOD) + RBUF(IDEB)
         IDEB = IDEB + 1
        ENDDO
      ENDDO
C
      DO L=1,NBINDEX
        CALL MPI_WAITANY(NBINDEX,REQ_S,INDEX,STATUS,IERROR)
      ENDDO
C
#endif
      RETURN
      END SUBROUTINE
C

