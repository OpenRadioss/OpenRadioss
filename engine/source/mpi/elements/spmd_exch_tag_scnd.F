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
Chd|  SPMD_EXCH_TAG_SCND            source/mpi/elements/spmd_exch_tag_scnd.F
Chd|-- called by -----------
Chd|        S10CNDI2_INI                  source/elements/solid/solide10/s10cndf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_TAG_SCND(
     1            ITAGND   ,FR_CNDS,IAD_CNDS, LCOMM)
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
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LCOMM, ITAGND(*), FR_CNDS(*), IAD_CNDS(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,LOC_PROC,NOD,I,J,L,IDEB,IAD,LEN,
     .        NBINDEX,INDEX,MSGOFF,SIZ,IERROR,
     .        STATUS(MPI_STATUS_SIZE),
     .        REQ_S(NSPMD),REQ_R(NSPMD),INDEXI(NSPMD)
      DATA MSGOFF/1179/
      INTEGER
     .        SBUF(LCOMM),RBUF(LCOMM)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      DO I=1,NSPMD
        SIZ = IAD_CNDS(I+1)-IAD_CNDS(I)
        IF(SIZ>0)THEN
         L = IAD_CNDS(I)
          MSGTYP = MSGOFF 
          CALL MPI_IRECV(
     S      RBUF(L),SIZ  ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
        ENDIF
      END DO
C
C preparation envoi  a proc I
C--------------------------------------------------------------------
      DO I=1,NSPMD
       DO J=IAD_CNDS(I),IAD_CNDS(I+1)-1
        NOD =  FR_CNDS(J)
          IF (IABS(ITAGND(NOD))>NS10E) THEN
         SBUF(J) = 1
          ELSE
         SBUF(J) = 0
          END IF
       ENDDO
      ENDDO
C
C   echange messages
C--------------------------------------------------------------------
      DO I=1,NSPMD
        SIZ = IAD_CNDS(I+1)-IAD_CNDS(I)
        IF(SIZ>0)THEN
          L = IAD_CNDS(I)
          MSGTYP = MSGOFF 
          CALL MPI_ISEND(
     +      SBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     +      MPI_COMM_WORLD,REQ_S(I),IERROR)
        ENDIF        
      ENDDO
C
C assemblage
C--------------------------------------------------------------------
      DO I=1,NSPMD
        SIZ = IAD_CNDS(I+1)-IAD_CNDS(I)
       IF(SIZ>0)THEN
        CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
        DO J=IAD_CNDS(I),IAD_CNDS(I+1)-1
         NOD =  FR_CNDS(J)
         IF (ITAGND(NOD)>0) THEN
          IF (ITAGND(NOD)<=NS10E.AND.RBUF(J)==1)
     +     ITAGND(NOD) = ITAGND(NOD) + NS10E
         ELSEIF(ITAGND(NOD)<0) THEN
          IF (ITAGND(NOD)>=-NS10E.AND.RBUF(J)==1)
     +     ITAGND(NOD) = ITAGND(NOD) - NS10E
         END IF
        ENDDO
       ENDIF
      ENDDO
C
C--------------------------------------------------------------------
      DO I = 1, NSPMD
        IF((IAD_CNDS(I+1)-IAD_CNDS(I))>0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
#endif
      RETURN
      END SUBROUTINE

