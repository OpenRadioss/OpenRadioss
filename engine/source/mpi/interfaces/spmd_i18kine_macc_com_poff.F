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
Chd|  SPMD_I18KINE_MACC_COM_POFF    source/mpi/interfaces/spmd_i18kine_macc_com_poff.F
Chd|-- called by -----------
Chd|        I18MAIN_KINE_1                source/interfaces/int18/i18main_kine.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE  SPMD_I18KINE_MACC_COM_POFF(MTF,A,
     *                                       IAD_ELEM,FR_ELEM,ITAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
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
#include      "task_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*), FR_ELEM(*),ITAB(*)
      my_real
     *    MTF(14,*),A(3,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),
     *        REQ_SI(NSPMD),REQ_RI(NSPMD),MSGTYP,IERROR,MSGOFF
      INTEGER L,NNOD,P,J,LOC_PROC,PTR,SIZ,NB,NOD
      INTEGER IADS(NSPMD+1),IADR(NSPMD+1),LENRV
      my_real
     *   TESTVAL
      my_real ,
     *      DIMENSION(:), ALLOCATABLE :: BBUFS,BBUFR
      DATA MSGOFF/155/
C--------------------------------------------------------------------
      LENRV = (IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1))*4
      ALLOCATE (BBUFS(LENRV),BBUFR(LENRV))
      LOC_PROC=ISPMD+1
C
      BBUFR=-1.
C Preparation du recv
      IADR(1) = 1
      L=1
      DO P=1,NSPMD
        IADR(P) = L
        SIZ = (IAD_ELEM(1,P+1)-IAD_ELEM(1,P))*4
        IF(SIZ/=0)THEN
          MSGTYP = MSGOFF 
          CALL MPI_IRECV(
     S      BBUFR(L),SIZ,REAL,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_RI(P),IERROR)
            L = L + SIZ
        ENDIF
      END DO
      IADR(NSPMD+1) = L


      L=1
      NNOD=0
      DO P=1,NSPMD
         IADS(P)=L
         DO J=IAD_ELEM(1,P),IAD_ELEM(1,P+1)-1
           NOD = FR_ELEM(J)
           BBUFS(L)  = MTF(1,NOD)
           BBUFS(L+1)  = A(1,NOD)
           BBUFS(L+2)  = A(2,NOD)
           BBUFS(L+3)  = A(3,NOD)
           L = L + 4
        ENDDO
      ENDDO
      IADS(NSPMD+1)=L

      DO P=1,NSPMD
        IF(IAD_ELEM(1,P+1)-IAD_ELEM(1,P)>0)THEN
          MSGTYP = MSGOFF
          SIZ=IADS(P+1)-IADS(P)
          L=IADS(P)
          CALL MPI_ISEND(
     S        BBUFS(L),SIZ,REAL,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,REQ_SI(P),IERROR)
        ENDIF
      ENDDO
C--------------------------------------------------------------------
C Recv

      DO P = 1, NSPMD
Cel test si msg necessaire a envoyer a completer par test interface
        NB = IAD_ELEM(1,P+1)-IAD_ELEM(1,P)
        IF(NB>0)THEN
          CALL MPI_WAIT(REQ_RI(P),STATUS,IERROR)
          L=IADR(P)
          DO J=IAD_ELEM(1,P),IAD_ELEM(1,P+1)-1
             NOD = FR_ELEM(J)
             IF (BBUFR(L) > ZERO)THEN
               A(1,NOD)=BBUFR(L+1)
               A(2,NOD)=BBUFR(L+2)
               A(3,NOD)=BBUFR(L+3)
             ENDIF
             L=L+4
          ENDDO
        ENDIF
      ENDDO

C Fin du send
      DO P = 1, NSPMD
          SIZ=IADS(P+1)-IADS(P)
          IF(SIZ>0) THEN
            CALL MPI_WAIT(REQ_SI(P),STATUS,IERROR)
          ENDIF
      ENDDO
#endif
      RETURN
      END
