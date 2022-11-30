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
Chd|  SPMD_EXCH_TAGNCONT            source/mpi/forces/spmd_exch_tagncont.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_TAGNCONT(TAGNCONT,IAD_ELEM ,FR_ELEM,LENR     )
C--------------------------------------
C--------------------------------------
      USE MESSAGE_MOD
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
#include      "sphcom.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "scr18_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*),FR_ELEM(*), TAGNCONT(NLOADP_HYD_INTER,NUMNOD),LENR
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,IERROR,MSGOFF,IERROR2,
     .        SIZ,J,K,L,NB_NOD,NP,
     .        STATUS(MPI_STATUS_SIZE),
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      DATA MSGOFF/120/
      
      INTEGER, DIMENSION(:), ALLOCATABLE :: RBUF,SBUF
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------


      IERROR = 0
      ALLOCATE( RBUF(NLOADP_HYD_INTER*LENR),STAT=IERROR2)
      IERROR = IERROR + IERROR2
      ALLOCATE( SBUF(NLOADP_HYD_INTER*LENR),STAT=IERROR2)
      IERROR = IERROR + IERROR2
              
      IF(IERROR/=0) THEN
         CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
         CALL ARRET(2)
      END IF   
      L = 1
      IAD_RECV(1) = 1
      DO I=1,NSPMD
        SIZ = NLOADP_HYD_INTER*(IAD_ELEM(1,I+1)-IAD_ELEM(1,I))
        IF(SIZ/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
      END DO
C


      L = 1
      IAD_SEND(1) = 1
      DO I=1,NSPMD
         DO NP=1,NLOADP_HYD_INTER
#include      "vectorize.inc"
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SBUF(L  ) =  TAGNCONT(NP,NOD)
              L = L + 1
            END DO
         ENDDO
C
        IAD_SEND(I+1) = L
      ENDDO
C
C   sending messages
C
      DO I=1,NSPMD
C--------------------------------------------------------------------
C Send to N+I mod P 
C-------------------------------------------------------------------
       IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)THEN
          MSGTYP = MSGOFF 
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          L = IAD_SEND(I)
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF
C--------------------------------------------------------------------
      ENDDO
C
C Counting
C

C
      DO I = 1, NSPMD
C
        NB_NOD = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        IF(NB_NOD>0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          L = IAD_RECV(I)

          DO NP=1,NLOADP_HYD_INTER
#include        "vectorize.inc"
              DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                  NOD = FR_ELEM(J)
                  TAGNCONT(NP,NOD) = MAX(TAGNCONT(NP,NOD), RBUF(L))
                  L = L + 1
               END DO
          ENDDO

       ENDIF
C
C

      END DO
C
Cel wait terminaison isend
C

      DO I = 1, NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO

      DEALLOCATE(RBUF,SBUF)
C
#endif
      RETURN
      END
