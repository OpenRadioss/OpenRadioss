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
Chd|  SPMD_I8_ILOC                  source/mpi/interfaces/spmd_i8tool.F
Chd|-- called by -----------
Chd|        INTVO8                        source/interfaces/inter3d/intvo8.F
Chd|-- calls ---------------
Chd|        INT8_MOD                      ../common_source/modules/interfaces/int8_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_I8_ILOC(ILOC    ,MSR,ITAB,BUFFER,
     .                        DISTANCE)
C-----------------------------------------------
C   I n f o r m a t i o n s            
C-----------------------------------------------
C   This routine computes the global 
C   ILOCS (i.e. main nodes of each secnd).
C   At the end of this routine, only one 
C   processor will have ILOCS(i) > 0
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INT8_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include      "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ::  MSR(*), ILOC(*), ITAB(*)
      my_real :: DISTANCE(*)
      TYPE(BUFT8)  BUFFER(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER, DIMENSION(:), ALLOCATABLE :: BUFR_ID,BUFS_ID
      my_real, DIMENSION(:), ALLOCATABLE :: BUFR_DIST,BUFS_DIST
      INTEGER I,J,K,N,P,IERR,K1,K2
      INTEGER RQS(2*(NSPMD-1))
      INTEGER RQR1(NSPMD-1)
      INTEGER RQR2(NSPMD-1)
      INTEGER STAT(MPI_STATUS_SIZE,2*(NSPMD-1))
      INTEGER STAT2(MPI_STATUS_SIZE)
      INTEGER TAG
      INTEGER TAB_RANK(NSPMD-1),TAB_BUFPOS(NSPMD-1)
      INTEGER BUFLEN,BUFPOS,RMAX_UID_LOCAL,RMAX_UID_REMOTE
       my_real DIST1,DIST2 
      INTEGER MSGOFF,MSGOFF2
      DATA MSGOFF/15000/
      DATA MSGOFF2/15001/

C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      !CALL STARTIME(97,1)
      BUFLEN=0
      DO I = 1,NSPMD
         IF(I-1 /=ISPMD) THEN
         BUFLEN = BUFLEN+BUFFER(I)%NBSECND_TOT
         ENDIF
      ENDDO
      ALLOCATE(BUFR_ID(BUFLEN)) 
      ALLOCATE(BUFS_ID(BUFLEN)) 
      ALLOCATE(BUFR_DIST(BUFLEN)) 
      ALLOCATE(BUFS_DIST(BUFLEN)) 
      BUFR_ID(1:BUFLEN)    = 0 
      BUFS_ID(1:BUFLEN)    = 0
      BUFR_DIST(1:BUFLEN)  = 0
      BUFS_DIST(1:BUFLEN)  = 0
      K = 0
      K1= 0
      K2= 0
      BUFPOS = 1
      DO I = 1,NSPMD
       IF( ISPMD /= I-1) THEN
        DO J = 1,BUFFER(I)%NBSECND_TOT
         N = BUFFER(I)%SECND_ID(J)
C        BUFFER(I)%DISTANCE(J) = DISTANCE(N) 
         BUFS_DIST(BUFPOS + J -1) = DISTANCE(N) 
C        BUFFER(I)%NEW_MAIN_UID(J) = ITAB(MSR(ILOC(N)))
         BUFS_ID(BUFPOS + J -1) = ITAB(MSR(ILOC(N)))

        ENDDO
        N =  BUFFER(I)%NBSECND_TOT
        ! the number of secnds on the frontier has
        ! to be the same on each side of the frontier
        IF( N > 0 ) THEN
        K = K + 1
        K1=K1 + 1

        TAB_RANK(K1) = I
        TAB_BUFPOS(K1) = BUFPOS
        TAG = MSGOFF

        CALL MPI_ISEND(BUFS_ID(BUFPOS),N,
     .    MPI_INT,I-1,TAG,MPI_COMM_WORLD,RQS(K),IERR)
        CALL MPI_IRECV(BUFR_ID(BUFPOS),N,
     .    MPI_INT,I-1,TAG,MPI_COMM_WORLD,RQR1(K1),IERR)

        TAG = MSGOFF2
        K = K + 1
        K2=K2 + 1
        CALL MPI_ISEND(BUFS_DIST(BUFPOS),N,
     .    REAL,I-1,TAG,MPI_COMM_WORLD,RQS(K),IERR)
        CALL MPI_IRECV(BUFR_DIST(BUFPOS),N,
     .    REAL,I-1,TAG,MPI_COMM_WORLD,RQR2(K2),IERR)
            BUFPOS = BUFPOS + N
        ENDIF
       ENDIF
      ENDDO

      IERR=-999
      IF(K > 0)  CALL MPI_WAITALL(K, RQS,STAT,IERR)
      BUFPOS = 0
      DO P = 1,K1
         CALL MPI_WAITANY(K2,RQR2,I,STAT,IERR) 
         CALL MPI_WAIT(RQR1(I),STAT2,IERR)
         BUFPOS = TAB_BUFPOS(I) - 1
         I = TAB_RANK(I)
         IF(ISPMD /= I-1) THEN
         N = BUFFER(I)%NBSECND_TOT
         DO J = 1,N
            K = BUFFER(I)%SECND_ID(J)
C           DIST1 = DISTANCE(K)
            DIST1 = BUFS_DIST(BUFPOS+J)
            DIST2 = BUFR_DIST(BUFPOS+J)
            RMAX_UID_REMOTE=BUFR_ID(BUFPOS+J)

            !IF ISPMD STILL HAS THE SECND
            IF(ILOC(K) > 0) THEN
            RMAX_UID_LOCAL = ITAB(MSR(ILOC(K))) 
            IF(DIST1 > DIST2 .OR.
     .  (DIST1 == DIST2 .AND. RMAX_UID_LOCAL > RMAX_UID_REMOTE) .OR.
     .  (RMAX_UID_LOCAL == RMAX_UID_REMOTE .AND. I-1 < ISPMD)) THEN 
!       IF the main remote is closer than the main local
!       OR the distance is the same, but the user id of the remote
!       main is lower
!       OR the user id of the main remote is the same than the local
!       main (i.e. the new main is also on the boundary between i-1
!       and ISPMD, but ISPMD > i-1
!       THEN we remove the secnd (it will be kept by the Proc i-1)
                DISTANCE(K)=DIST2
                ILOC(k)= -1                                             
!        Here, the secnd will
!        not have a new main internal to the ISPMD           
!        Still it can be put at the boundary of ISPMD by another
!        process
             ENDIF
             ENDIF
         ENDDO
         BUFPOS = BUFPOS + N 
         ENDIF
      ENDDO

!     CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
      DEALLOCATE(BUFR_ID,BUFS_ID) 
      DEALLOCATE(BUFR_DIST,BUFS_DIST) 
      !CALL STOPTIME(97,1)
#endif
      END SUBROUTINE
Chd|====================================================================
Chd|  SPMD_I8_INDEX                 source/mpi/interfaces/spmd_i8tool.F
Chd|-- called by -----------
Chd|        INTVO8                        source/interfaces/inter3d/intvo8.F
Chd|-- calls ---------------
Chd|        INT8_MOD                      ../common_source/modules/interfaces/int8_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_I8_INDEX(NMN,FRONTIER,INDEX_IN_COMM,S_COMM)
C-----------------------------------------------
C   I n f o r m a t i o n s            
C-----------------------------------------------
C Computes INDEX_IN_COMM such that
C if I is the main local id
C INDEX_IN_COMM(i) = 
C                     index of the node in
C   Communication structure
C                     0 if the node is not
C   shared between processors (considering only this
C   interface)            
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INT8_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ::  NMN,S_COMM
      INTEGER ::  INDEX_IN_COMM(NMN)
      TYPE(FRONT8)  FRONTIER(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      INDEX_IN_COMM(1:NMN) = 0
      DO I = 1,S_COMM
        INDEX_IN_COMM(FRONTIER(I)%NUMLOC) = I
      ENDDO

      END SUBROUTINE
Chd|====================================================================
Chd|  SPMD_I8_COMMSLV               source/mpi/interfaces/spmd_i8tool.F
Chd|-- called by -----------
Chd|        INTVO8                        source/interfaces/inter3d/intvo8.F
Chd|-- calls ---------------
Chd|        INT8_MOD                      ../common_source/modules/interfaces/int8_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_I8_COMMSLV(NBSECNDS,ILOC,NSV,
     .            ITAB,BUFFER,FRONTIER,INDEX_IN_COMM)
C-----------------------------------------------
C   I n f o r m a t i o n s            
C-----------------------------------------------
C   Each processor communicates a 
C   secnd nodes that has a main nodes that is shared
C   with another processor 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INT8_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include      "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ::  NSV(*), ILOC(*), ITAB(*)
      INTEGER :: NBSECNDS,INDEX_IN_COMM(*)
      TYPE(FRONT8)  FRONTIER(*)
      TYPE(BUFT8)  BUFFER(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER, DIMENSION(:), ALLOCATABLE :: BUFR_ID,BUFR_IDR
      INTEGER I,J,K,L,N,P,IERR
      INTEGER RQS(2*(NSPMD-1))
      INTEGER RQR(2*(NSPMD-1))
      INTEGER KSENT,KRECV
      INTEGER STAT(MPI_STATUS_SIZE,2*(NSPMD-1))
      INTEGER TAG,NB_SECND_SENT(NSPMD),NB_SECND_RECV(NSPMD)
      INTEGER NB_SECND_TOT
      INTEGER BUFLEN,BUFPOS(NSPMD),RMAX_UID_LOCAL,RMAX_UID_REMOTE
      INTEGER IMAIN,ISECND
      INTEGER MSGOFF,MSGOFF2
      DATA MSGOFF/15002/
      DATA MSGOFF2/15003/

C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      !CALL STARTIME(98,1)
      NB_SECND_SENT(1:NSPMD) = 0
      NB_SECND_TOT = 0
      DO I = 1,NBSECNDS
        ! if the secnd is active, and its main is in the frontier
        IF(ILOC(I) > 0) THEN 
        IF(INDEX_IN_COMM(ILOC(I))>0)THEN
           K = INDEX_IN_COMM(ILOC(I)) 
           DO J = 1,FRONTIER(K)%NBCOM
           P = FRONTIER(K)%PROCLIST(J)
           NB_SECND_SENT(P) = NB_SECND_SENT(P) + 1
           NB_SECND_TOT = NB_SECND_TOT +1
           ENDDO
        ENDIF
        ENDIF
      ENDDO


      ALLOCATE(BUFR_ID(NB_SECND_TOT*2))


      BUFPOS = 0          
      BUFPOS(1) = 1
      DO I = 1,NSPMD-1
        BUFPOS(I+1) = 2*NB_SECND_SENT(I) + BUFPOS(I) 
      ENDDO


      DO I = 1,NBSECNDS
         ! if the secnd is active, and its main is in the frontier
         IF(ILOC(I)>0) THEN 
         IF(INDEX_IN_COMM(ILOC(I))>0)THEN
            K = INDEX_IN_COMM(ILOC(I)) 
            DO J = 1,FRONTIER(K)%NBCOM
            P = FRONTIER(K)%PROCLIST(J)
            N = FRONTIER(K)%BUF_INDEX(J)
            ! we have to send the secnd uid and the position in the
            ! frontier
            ! this position is also the position in the remote domain
            BUFR_ID(BUFPOS(P)) = ITAB(NSV(I)) 
            BUFR_ID(BUFPOS(P)+1) = N 
            BUFPOS(P) = BUFPOS(P) + 2 
            ENDDO
         ENDIF
         ENDIF
      ENDDO
     
      NB_SECND_RECV(1:NSPMD) = 0
      K = 1 
      N = 1

      TAG =  MSGOFF
      K = 0
      DO I = 1,NSPMD
        ! if ispmd shares main nodes with i
        ! then send the number of secnd to exchange
C       WRITE(6,*) __FILE__,__LINE__,ISPMD,I-1
        IF(ISPMD /= I-1 .AND.BUFFER(I)%NBMAIN >0 ) THEN
        K = K +1
        CALL MPI_ISEND(NB_SECND_SENT(I),1,
     .    MPI_INT,I-1,TAG,MPI_COMM_WORLD,RQS(K),IERR)
        CALL MPI_IRECV(NB_SECND_RECV(I),1,
     .    MPI_INT,I-1,TAG,MPI_COMM_WORLD,RQR(K),IERR)
        ENDIF
      ENDDO
 
      IF( K > 0 ) CALL MPI_WAITALL(K,RQR,STAT,IERR)


      ! size of reception buffer
      BUFLEN = 0
      DO I = 1,NSPMD
        IF(ISPMD /= I-1) THEN
        BUFLEN = BUFLEN + NB_SECND_RECV(I) 
        ENDIF
      ENDDO
      !write(6,*) __file__,__line__,"nbsr(:)=",nb_secnd_recv(1:nspmd)
      ALLOCATE(BUFR_IDR(BUFLEN*2))

      IF( K > 0 ) CALL MPI_WAITALL(K, RQS,STAT,IERR)

     
      TAG = MSGOFF2 
      K = 1
      L = 1 
      KSENT = 0
      KRECV = 0
      !send the data corresponding of the secnds to exchange
      DO I = 1,NSPMD
        IF(ISPMD /= I-1) THEN
          J = NB_SECND_SENT(I)*2
          IF(J > 0) THEN
            KSENT = KSENT + 1
            CALL MPI_ISEND(BUFR_ID(N),J,
     .      MPI_INT,I-1,TAG,MPI_COMM_WORLD,RQS(KSENT),IERR)
            N = N + J                
          ENDIF
          J = NB_SECND_RECV(I)*2
          IF(J > 0) THEN
            KRECV = KRECV + 1
            CALL MPI_IRECV(BUFR_IDR(L),J,
     .      MPI_INT,I-1,TAG,MPI_COMM_WORLD,RQR(KRECV),IERR)
            L = L + J       
          ENDIF
        ENDIF
      ENDDO

      IF(KSENT > 0 ) CALL MPI_WAITALL(KSENT, RQS,STAT,IERR)
      IF(KRECV > 0 ) CALL MPI_WAITALL(KRECV, RQR,STAT,IERR)

!                                                    
      K = 1
      L = 1 
      N = 1
      !In the following we suppose that only one SPMD domain
      !has a secnd activated
      DO ISECND=1,NBSECNDS
        RMAX_UID_LOCAL = ITAB(NSV(ISECND))
        DO I = 1,NSPMD
          IF(ISPMD /= I-1) THEN
           J = NB_SECND_RECV(I)
           IF(J > 0) THEN
           ! if ISPMD recieves secnds from proc I
           ! then find out if one these secnds is ISECND
           ! using user ids.
           DO K = L,L+J-1 
            RMAX_UID_REMOTE = BUFR_IDR(2*K-1) 
            IF(RMAX_UID_LOCAL == RMAX_UID_REMOTE) THEN
            IMAIN = BUFR_IDR(2*K)
            ILOC(ISECND) = BUFFER(I)%MAIN_ID(IMAIN)
            ENDIF 
           ENDDO 
           ENDIF
          L = L + J
          ENDIF !ISPMD
        ENDDO !ISPMD
        L = 1
      ENDDO

      DEALLOCATE(BUFR_ID)
      DEALLOCATE(BUFR_IDR)
      !CALL STOPTIME(98,1)
#endif
      END SUBROUTINE

Chd|====================================================================
Chd|  SPMD_I8_UPDBUF                source/mpi/interfaces/spmd_i8tool.F
Chd|-- called by -----------
Chd|        INTVO8                        source/interfaces/inter3d/intvo8.F
Chd|-- calls ---------------
Chd|        INT8_MOD                      ../common_source/modules/interfaces/int8_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_I8_UPDBUF(NBSECNDS,ILOC,NSV,
     .            ITAB,BUFFER,FRONTIER,INDEX_IN_COMM)
C-----------------------------------------------
C   I n f o r m a t i o n s            
C-----------------------------------------------
C   Update the buffer structure that contains
C   the secnd nodes that have a main node
C   which is shared by at least another processor 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INT8_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  ::   NSV(*), ILOC(*), ITAB(*)
      INTEGER  ::  INDEX_IN_COMM(*)
      INTEGER  :: NBSECNDS
      TYPE(FRONT8)  FRONTIER(*)
      TYPE(BUFT8)  BUFFER(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,P
      INTEGER PT(NSPMD)


#ifdef MPI
         !CALL STARTIME(99,1)
         !compute the number of secnd nodes per frontier main node 
         DO P = 1,NSPMD
          IF(ISPMD /= P-1) THEN
          BUFFER(P)%NBSECND = 0 
          ENDIF
         ENDDO
         DO I = 1,NBSECNDS
            ! If the secnd is active, and its main is in the frontier
            IF(ILOC(I)>0) THEN 
            IF(INDEX_IN_COMM(ILOC(I))>0)THEN
               K = INDEX_IN_COMM(ILOC(I)) 
               DO J = 1,FRONTIER(K)%NBCOM
               P = FRONTIER(K)%PROCLIST(J)
               N = FRONTIER(K)%BUF_INDEX(J)
               BUFFER(P)%NBSECND(N) = BUFFER(P)%NBSECND(N) + 1
               ENDDO
            ENDIF
            ENDIF
         ENDDO

         DO I =1, NSPMD
         ! compute the global number of secnd nodes to send to proc i
            BUFFER(I)%NBSECND_TOT = 0
            DO J = 1,BUFFER(I)%NBMAIN          
               BUFFER(I)%NBSECND_TOT =  BUFFER(I)%NBSECND_TOT +
     .         BUFFER(I)%NBSECND(J) 
            ENDDO
C           IF(ASSOCIATED(BUFFER(I)%SECND_UID)) THEN
C          
              DEALLOCATE(BUFFER(I)%SECND_UID)
              DEALLOCATE(BUFFER(I)%SECND_ID)
C             DEALLOCATE(BUFFER(I)%NEW_MAIN_UID)
C             DEALLOCATE(BUFFER(I)%DISTANCE)
C             DEALLOCATE(BUFFER(I)%BUFR)
C             DEALLOCATE(BUFFER(I)%BUFI)
C           ENDIF
            ALLOCATE(BUFFER(I)%SECND_UID(BUFFER(I)%NBSECND_TOT))
            ALLOCATE(BUFFER(I)%SECND_ID(BUFFER(I)%NBSECND_TOT))
C           ALLOCATE(BUFFER(I)%NEW_MAIN_UID(BUFFER(I)%NBSECND_TOT))
C           ALLOCATE(BUFFER(I)%DISTANCE(BUFFER(I)%NBSECND_TOT))
C           ALLOCATE(BUFFER(I)%BUFR(BUFFER(I)%NBSECND_TOT*2))
C           ALLOCATE(BUFFER(I)%BUFI(BUFFER(I)%NBSECND_TOT*9))
C           BUFFER(I)%BUFR(1:BUFFER(I)%NBSECND_TOT)=0
C           BUFFER(I)%BUFI(1:BUFFER(I)%NBSECND_TOT)=0
         ENDDO 
         
         ! Fill the buffer of secnd uid to send                                      
         PT = 0
         DO I = 1,NBSECNDS
            ! if the secnd is active, and its main is in the frontier
           IF(ILOC(I) > 0) THEN
           IF(INDEX_IN_COMM(ILOC(I)) > 0)THEN
               K = INDEX_IN_COMM(ILOC(I)) 
               DO J = 1,FRONTIER(K)%NBCOM
               P = FRONTIER(K)%PROCLIST(J)
               N = FRONTIER(K)%BUF_INDEX(J)
               PT(P) = PT(P) + 1
               BUFFER(P)%SECND_ID(PT(P)) = I
               BUFFER(P)%SECND_UID(PT(P)) = ITAB(NSV(I))

               ENDDO
            ENDIF
            ENDIF
         ENDDO
         !CALL STOPTIME(99,1)
#endif
       END SUBROUTINE


Chd|====================================================================
Chd|  SPMD_I8_IRTL                  source/mpi/interfaces/spmd_i8tool.F
Chd|-- called by -----------
Chd|        INTVO8                        source/interfaces/inter3d/intvo8.F
Chd|-- calls ---------------
Chd|        INT8_MOD                      ../common_source/modules/interfaces/int8_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_I8_IRTL(
     .            IRTL,     HAS_MOVED,
     .            TAB_RMAX, TAB_RMAX_UID,
     .            ITAB,     BUFFER)

C-----------------------------------------------
C   I n f o r m a t i o n s            
C-----------------------------------------------
C   This routine computes the global 
C   IRTL (i.e. main face of each secnd).
C   At the end of this routine, only one processor
C   will have IRTL(i) /= 0.
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INT8_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include      "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: ITAB(*),IRTL(*)
      INTEGER :: TAB_RMAX_UID(4,*),HAS_MOVED(*)
      my_real :: TAB_RMAX(*)
      TYPE(BUFT8)  BUFFER(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI

      INTEGER, DIMENSION(:), ALLOCATABLE :: BUFR_ID,BUFS_ID
      my_real, DIMENSION(:), ALLOCATABLE :: BUFR,BUFS
      INTEGER I,J,K,N,IERR
      INTEGER RQS(2*(NSPMD-1))
      INTEGER RQR(2*(NSPMD-1))
      INTEGER STAT(MPI_STATUS_SIZE,2*(NSPMD-1))
      INTEGER TAG
      INTEGER BUFLEN,IFLAG
      INTEGER RMAX_UID_LOCAL(4),RMAX_UID_REMOTE(4)
      INTEGER BUFPOS1,BUFPOS2,HAS_MOVED_ON_REMOTE
      my_real RMAX1,RMAX2
      INTEGER NBRQ                                   
      INTEGER MSGOFF,MSGOFF2
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER IS_SUP_FACE_ID
      EXTERNAL IS_SUP_FACE_ID

      DATA MSGOFF/15004/
      DATA MSGOFF2/15005/

      !AFTER THE EXCHANGE OF FRONT SECND, AND THE UPDATE OF THE BUFFER
      !THE SECND AT EACH BOUNDARY ARE SHARED 
      BUFLEN=0
      DO I = 1,NSPMD
         BUFLEN = BUFLEN+BUFFER(I)%NBSECND_TOT
      ENDDO

      ALLOCATE(BUFR_ID(BUFLEN*5)) 
      ALLOCATE(BUFR(BUFLEN)) 
      ALLOCATE(BUFS_ID(BUFLEN*5)) 
      ALLOCATE(BUFS(BUFLEN)) 
      BUFR_ID(1:BUFLEN*5) = 0 
      BUFR(1:BUFLEN)      = ZERO
      BUFS_ID(1:BUFLEN*5) = 0 
      BUFS(1:BUFLEN)      = ZERO 

      K = 1
      BUFPOS1 = 1
      BUFPOS2 = 1
      NBRQ = 0
      DO I = 1,NSPMD
       IF( ISPMD /= I-1) THEN
        DO J = 1,BUFFER(I)%NBSECND_TOT
         N = BUFFER(I)%SECND_ID(J)
C
         BUFS(BUFPOS2 - 1+(J-1)+1) = TAB_RMAX(N) 
         BUFS_ID(BUFPOS1 - 1 + (J-1)*5+1) = TAB_RMAX_UID(1,N) 
         BUFS_ID(BUFPOS1 - 1 + (J-1)*5+2) = TAB_RMAX_UID(2,N) 
         BUFS_ID(BUFPOS1 - 1 + (J-1)*5+3) = TAB_RMAX_UID(3,N) 
         BUFS_ID(BUFPOS1 - 1 + (J-1)*5+4) = TAB_RMAX_UID(4,N) 
         BUFS_ID(BUFPOS1 - 1 + (J-1)*5+5) = HAS_MOVED(N) 

        ENDDO
        N =  BUFFER(I)%NBSECND_TOT
        ! The number of secnds on the frontier has
        ! to be the same on each side of the frontier
        IF(N > 0) THEN
          TAG = MSGOFF
          NBRQ = NBRQ + 1
          CALL MPI_ISEND(BUFS_ID(BUFPOS1),N*5,MPI_INT,I-1,TAG,MPI_COMM_WORLD,RQS(NBRQ),IERR)
          CALL MPI_IRECV(BUFR_ID(BUFPOS1),N*5,MPI_INT,I-1,TAG,MPI_COMM_WORLD,RQR(NBRQ),IERR)
          BUFPOS1 = BUFPOS1 + 5*N
          TAG = MSGOFF2 
          NBRQ = NBRQ + 1
          CALL MPI_ISEND(BUFS(BUFPOS2),N,REAL,I-1,TAG,MPI_COMM_WORLD,RQS(NBRQ),IERR)
          CALL MPI_IRECV(BUFR(BUFPOS2),N,REAL,I-1,TAG,MPI_COMM_WORLD,RQR(NBRQ),IERR)
          BUFPOS2 = BUFPOS2 + N
        ENDIF
       ENDIF
      ENDDO !ISPMD
      IF(NBRQ > 0) THEN 
        CALL MPI_WAITALL(NBRQ, RQS,STAT,IERR)
        CALL MPI_WAITALL(NBRQ, RQR,STAT,IERR)
      ENDIF

      BUFPOS1 = 0
      BUFPOS2 = 0

      DO I = 1,NSPMD
        IF(ISPMD /= I-1) THEN
        N = BUFFER(I)%NBSECND_TOT
        DO J = 1,N
          K = BUFFER(I)%SECND_ID(J)
          RMAX2 = BUFR((J-1)+1+BUFPOS2)
          RMAX_UID_REMOTE(1)   = BUFR_ID((J-1)*5+1+BUFPOS1)
          RMAX_UID_REMOTE(2)   = BUFR_ID((J-1)*5+2+BUFPOS1)
          RMAX_UID_REMOTE(3)   = BUFR_ID((J-1)*5+3+BUFPOS1)
          RMAX_UID_REMOTE(4)   = BUFR_ID((J-1)*5+4+BUFPOS1)
          HAS_MOVED_ON_REMOTE  = BUFR_ID((J-1)*5+5+BUFPOS1)

          RMAX1 = TAB_RMAX(K)
          RMAX_UID_LOCAL(1) = TAB_RMAX_UID(1,K)
          RMAX_UID_LOCAL(2) = TAB_RMAX_UID(2,K)
          RMAX_UID_LOCAL(3) = TAB_RMAX_UID(3,K)
          RMAX_UID_LOCAL(4) = TAB_RMAX_UID(4,K)

          IFLAG = IS_SUP_FACE_ID(RMAX_UID_LOCAL,RMAX_UID_REMOTE)

          IF(HAS_MOVED(K) == 1) THEN
          ! the main face has changed on the local proc
          ! or is not on the local proc
          ! current proc not in charge of the face
          ! in the following cases
            IF(HAS_MOVED_ON_REMOTE == 0) THEN
          ! the main face has not changed on the remote proc.
              IRTL(K) =  0        
            ELSEIF( RMAX1 < RMAX2 .OR. (RMAX1 == RMAX2 .AND. IFLAG == 1)) THEN        
              IRTL(K) =  0
            ENDIF
          ENDIF
        ENDDO
        BUFPOS1 = BUFPOS1 + 5*N
        BUFPOS2 = BUFPOS2 + N
        ENDIF
      ENDDO
      DEALLOCATE(BUFR_ID,BUFS_ID) 
      DEALLOCATE(BUFR,BUFS) 

#endif

      END SUBROUTINE


Chd|====================================================================
Chd|  SPMD_I8_REDUCE                source/mpi/interfaces/spmd_i8tool.F
Chd|-- called by -----------
Chd|        INTFOP8                       source/interfaces/interf/intfop8.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_I8_REDUCE(TAB,N,NUM)
C-----------------------------------------------
C   I n f o r m a t i o n s            
C-----------------------------------------------
C   This routine communicates secnd variables  
C   (- Flag = 1 : Send) Commented                                     
C   (- Flag = 2 : Recieve) Commented
C   - Flag = 3 : Synchrone 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include      "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real :: TAB(N)                
      INTEGER :: N
C     INTEGER :: FLAG,RQ(NSPMD)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,MSGOFF,TAG,NUM
      DATA MSGOFF/15010/

#ifdef MPI
      INTEGER IERR,POS
      INTEGER STAT(MPI_STATUS_SIZE)
      my_real, DIMENSION(:), ALLOCATABLE :: BUF
      !CALL STARTIME(101,1)
      TAG = MSGOFF 
c     IF(FLAG == 1) THEN
c       POS = 0
c       DO I=1,NSPMD
c         IF(ISPMD /= I-1) THEN
c           POS = POS + 1
c           CALL MPI_ISEND(TAB,N,REAL,I-1,TAG,MPI_COMM_WORLD,RQ(POS),IERR)
c         ENDIF
c       ENDDO 
c       CALL MPI_WAITALL(NSPMD-1,RQ,MPI_STATUSES_IGNORE,IERR)
c     ELSEIF(FLAG ==2) THEN
c       ALLOCATE(BUF(N),STAT=IERR)
c       BUF(1:N) = 0
c       DO I=1,NSPMD
c         IF(ISPMD /= I-1) THEN
c           CALL MPI_RECV(BUF,N,REAL,I-1,TAG,MPI_COMM_WORLD,IERR)
c           DO J=1,N
c              TAB(J) = TAB(J) + BUF(J)
c           ENDDO
c         ENDIF
c       ENDDO 
c       CALL MPI_WAIT(RQ,STAT,IERR) 
c       DEALLOCATE(BUF)
c     ELSEIF(FLAG == 3) THEN
        ALLOCATE(BUF(N),STAT=IERR)
        BUF(1:N) = TAB(1:N)
        CALL MPI_ALLREDUCE(BUF,TAB,N,REAL,MPI_SUM,
     .  MPI_COMM_WORLD,IERR)
        DEALLOCATE(BUF)
       

c     ENDIF
      !CALL STOPTIME(101,1)
#endif
      END SUBROUTINE


