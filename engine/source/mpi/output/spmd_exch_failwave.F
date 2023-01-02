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
Chd|  SPMD_FAILWAVE_BOUNDARIES      source/mpi/output/spmd_exch_failwave.F
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_FAILWAVE_BOUNDARIES(FAILWAVE,IAD_ELEM,FR_ELEM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAILWAVE_MOD
C      USE DEBUG_MOD
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
      INTEGER IAD_ELEM(2,*), FR_ELEM(*)
      TYPE (FAILWAVE_STR_) ,TARGET :: FAILWAVE 
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER, DIMENSION(:), ALLOCATABLE :: COUNT
      INTEGER P,J,TOTAL_NODES,NCOUNT,NOD
C-----------------------------------------------

      IF (ALLOCATED( FAILWAVE%FWAVE_IAD)) DEALLOCATE (FAILWAVE%FWAVE_IAD) 
      IF (ALLOCATED( FAILWAVE%FWAVE_FR )) DEALLOCATE (FAILWAVE%FWAVE_FR) 

C count the #boundaries failwave nodes
      ALLOCATE(COUNT(NSPMD))
      COUNT(1:NSPMD)=0
      TOTAL_NODES=0
      DO P=1,NSPMD
        DO J=IAD_ELEM(1,P),IAD_ELEM(1,P+1)-1
          NOD = FR_ELEM(J)
          IF (FAILWAVE%IDXI(NOD) > 0)THEN
            COUNT(P)=COUNT(P)+1
          ENDIF
        ENDDO
        TOTAL_NODES = TOTAL_NODES + COUNT(P)
      ENDDO

      ALLOCATE(FAILWAVE%FWAVE_IAD(NSPMD+1))
      ALLOCATE(FAILWAVE%FWAVE_FR(TOTAL_NODES))
      FAILWAVE%FWAVE_IAD(1)=1
      DO P=2,NSPMD+1
         FAILWAVE%FWAVE_IAD(P)=FAILWAVE%FWAVE_IAD(P-1)+COUNT(P-1)
      ENDDO
      
      NCOUNT=0
      DO P=1,NSPMD
         DO J=IAD_ELEM(1,P),IAD_ELEM(1,P+1)-1
            NOD = FR_ELEM(J)
            IF (FAILWAVE%IDXI(NOD) > 0)THEN
              NCOUNT = NCOUNT+1
              FAILWAVE%FWAVE_FR(NCOUNT)=FAILWAVE%IDXI(NOD)
            ENDIF
         ENDDO
      ENDDO


      DEALLOCATE(COUNT)
      END


Chd|====================================================================
Chd|  SPMD_EXCH_FAILWAVE            source/mpi/output/spmd_exch_failwave.F
Chd|-- called by -----------
Chd|        UPDATE_FAILWAVE               source/materials/fail/failwave/update_failwave.F
Chd|-- calls ---------------
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_FAILWAVE(FAILWAVE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAILWAVE_MOD
C      USE DEBUG_MOD
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
      TYPE (FAILWAVE_STR_) ,TARGET :: FAILWAVE 
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I,J,K,L,M,ND
      INTEGER NDDIM

C MPI  VARIABLES

      INTEGER MSGTYP,NOD,LOC_PROC, SIZ,NB_NOD,NB,MAXLEV,VALUE

      INTEGER STATUS(MPI_STATUS_SIZE),IERROR

      INTEGER MSGOFF1,MSGOFF2
      INTEGER IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1)
      INTEGER SEND_SIZ(NSPMD),RECV_SIZ(NSPMD)
      INTEGER SIZ_SEND,SIZ_RECV

      INTEGER REQ_S1(NSPMD), REQ_S2(NSPMD)
      INTEGER REQ_R1(NSPMD), REQ_R2(NSPMD)
      
      INTEGER, DIMENSION(:), ALLOCATABLE :: SBUFI,RBUFI
      INTEGER, DIMENSION(:), ALLOCATABLE :: SEND_BUF,RECV_BUF


      DATA MSGOFF1/1280/
      DATA MSGOFF2/1281/
C=======================================================================
      NDDIM=FAILWAVE%NDDL
      LOC_PROC = ISPMD + 1

C--------------------------------------------------------------------
C 1. SEND MAXLEV (Stacked information to the others)
C--------------------------------------------------------------------
      SIZ = FAILWAVE%FWAVE_IAD(NSPMD+1)-FAILWAVE%FWAVE_IAD(1)
      ALLOCATE(RBUFI(SIZ))
      ALLOCATE(SBUFI(SIZ))

c--------------------------------------------------------------------
c     MPI receive
c--------------------------------------------------------------------
      L = 1
      IAD_RECV(1) = 1
      DO I=1,NSPMD
        SIZ = FAILWAVE%FWAVE_IAD(I+1)-FAILWAVE%FWAVE_IAD(I)
        IF (SIZ > 0)THEN
          MSGTYP = MSGOFF1
          CALL MPI_IRECV(
     S      RBUFI(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R1(I),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
      END DO
c--------------------------------------------------------------------
c     Send buffer & Compute size for next communication
c--------------------------------------------------------------------
      SEND_SIZ(1:NSPMD)=0

      L = 1
      IAD_SEND(1) = 1
      RECV_SIZ(1:NSPMD)=0
      DO I=1,NSPMD
        DO J=FAILWAVE%FWAVE_IAD(I),FAILWAVE%FWAVE_IAD(I+1)-1
          NOD = FAILWAVE%FWAVE_FR(J)
          SBUFI(L) = FAILWAVE%MAXLEV(NOD)
          SEND_SIZ(I) = SEND_SIZ(I)+FAILWAVE%MAXLEV(NOD)*NDDIM
          L = L + 1
        END DO
        IAD_SEND(I+1) = L
      ENDDO
c--------------------------------------------------------------------
c    MPI send
c--------------------------------------------------------------------
      DO I=1,NSPMD
        IF(FAILWAVE%FWAVE_IAD(I+1)-FAILWAVE%FWAVE_IAD(I)>0)THEN
          MSGTYP = MSGOFF1
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          L = IAD_SEND(I)
          CALL MPI_ISEND(
     S      SBUFI(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S1(I),IERROR)
        ENDIF
      ENDDO
c--------------------------------------------------------------------
c     Receive buffer & compute receive_size
c-------------------------------------------------------------------
      RECV_SIZ(1:NSPMD)=0

      DO I = 1, NSPMD
        NB_NOD = FAILWAVE%FWAVE_IAD(I+1)-FAILWAVE%FWAVE_IAD(I)

        IF (NB_NOD > 0)THEN
          CALL MPI_WAIT(REQ_R1(I),STATUS,IERROR)
          L = IAD_RECV(I)

          DO J=FAILWAVE%FWAVE_IAD(I),FAILWAVE%FWAVE_IAD(I+1)-1
             RECV_SIZ(I) = RECV_SIZ(I) + RBUFI(L)*NDDIM
             L = L + 1
          END DO
        ENDIF
      END DO


c--------------------------------------------------------------------
c     Terminate first send
c-------------------------------------------------------------------
      DO I = 1, NSPMD
        IF (FAILWAVE%FWAVE_IAD(I+1)-FAILWAVE%FWAVE_IAD(I) > 0)THEN
          CALL MPI_WAIT(REQ_S1(I),STATUS,IERROR)
        ENDIF
      ENDDO


C---------------------------------------------------------------
C Prepare & send Stacked information
C ---------------------------------------------------------------
      IAD_SEND(1)=1
      IAD_RECV(1)=1
     
      DO I=1,NSPMD
        IAD_SEND(I+1)=IAD_SEND(I)+SEND_SIZ(I)
        IAD_RECV(I+1)=IAD_RECV(I)+RECV_SIZ(I)
      ENDDO

      SIZ_SEND = (IAD_SEND(NSPMD+1)-IAD_SEND(1))
      SIZ_RECV = (IAD_RECV(NSPMD+1)-IAD_RECV(1))

      ALLOCATE(SEND_BUF(SIZ_SEND))
      ALLOCATE(RECV_BUF(SIZ_RECV))

c      print*,ispmd,'send_buf', SIZ_SEND,' recv_buf:', SIZ_RECV
c--------------------------------------------------------------------
c     MPI receive
c--------------------------------------------------------------------
      L = 1
      DO I=1,NSPMD
        SIZ = IAD_RECV(I+1)-IAD_RECV(I)
        IF (SIZ > 0)THEN
          MSGTYP = MSGOFF2
          CALL MPI_IRECV(
     S      RECV_BUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R2(I),IERROR)
          L = L + SIZ
        ENDIF
      END DO


C --------------------------------------------------------------------
C Prepare send buffers
C --------------------------------------------------------------------
      K=1
      DO I=1,NSPMD
         DO J=FAILWAVE%FWAVE_IAD(I),FAILWAVE%FWAVE_IAD(I+1)-1
           ND = FAILWAVE%FWAVE_FR(J)
           DO L=1,FAILWAVE%MAXLEV(ND)
              DO M=1,NDDIM
                SEND_BUF(K) = FAILWAVE%FWAVE_NOD(M,ND,L)
                K = K +1
              ENDDO
           ENDDO
         ENDDO
      ENDDO

c--------------------------------------------------------------------
c    MPI send
c--------------------------------------------------------------------
      DO I=1,NSPMD
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          IF (SIZ > 0)THEN
            L = IAD_SEND(I)
C            print*,'isend=',L
            MSGTYP = MSGOFF2
            CALL MPI_ISEND(
     S        SEND_BUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_S2(I),IERROR)
        ENDIF
      ENDDO

C wait & unstack
c--------------------------------------------------------------------
c     Receive buffer & compute receive_size
c-------------------------------------------------------------------
      RECV_SIZ(1:NSPMD)=0

      DO I = 1, NSPMD
        SIZ = IAD_RECV(I+1)-IAD_RECV(I)
        IF (SIZ > 0)THEN
          CALL MPI_WAIT(REQ_R2(I),STATUS,IERROR)
          L = IAD_RECV(I)

          DO J=FAILWAVE%FWAVE_IAD(I),FAILWAVE%FWAVE_IAD(I+1)-1
C Get the number of Stacked elements
             NB=RBUFI(J)
             IF (NB > 0)THEN
               ND = FAILWAVE%FWAVE_FR(J)
               IF (FAILWAVE%WAVE_MOD == 1) THEN
                 DO M=1,NB
                   DO K=1,NDDIM
                     VALUE = RECV_BUF(L)
                     FAILWAVE%FWAVE_NOD(K,ND,1)=MAX(VALUE,FAILWAVE%FWAVE_NOD(K,ND,1))
c                      print*,ispmd,'VALUE', VALUE,K,ND,MAXLEV
                     L=L+1
                   ENDDO
                 ENDDO
               ELSE
                 DO M=1,NB
                   FAILWAVE%MAXLEV(ND)=FAILWAVE%MAXLEV(ND)+1
                   MAXLEV = FAILWAVE%MAXLEV(ND)
                   DO K=1,NDDIM
                     VALUE = RECV_BUF(L)
                     FAILWAVE%FWAVE_NOD(K,ND,MAXLEV)=VALUE
c                      print*,ispmd,'VALUE', VALUE,K,ND,MAXLEV
                     L=L+1
                   ENDDO
                 ENDDO
               ENDIF
             ENDIF
          END DO
        ENDIF
      END DO


c--------------------------------------------------------------------
c     Terminate second send
c-------------------------------------------------------------------
      DO I = 1, NSPMD
        SIZ = IAD_SEND(I+1)-IAD_SEND(I)
        IF (SIZ > 0)THEN
          CALL MPI_WAIT(REQ_S2(I),STATUS,IERROR)
        ENDIF
      ENDDO
      DEALLOCATE(SEND_BUF)
      DEALLOCATE(RECV_BUF)

      DEALLOCATE(RBUFI)
      DEALLOCATE(SBUFI)

#endif

      RETURN
      END

