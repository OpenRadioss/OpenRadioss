Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  SPMD_GET_STIF25_EDG           source/mpi/interfaces/spmd_getstif25_edg.F
Chd|-- called by -----------
Chd|        I25MAIN_FREE                  source/interfaces/intsort/i25main_free.F
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_GET_STIF25_EDG(
     .    STFE,    NEDGE, LEDGE,
     .    NIN ,  ISENDTO, IRCVFROM, COMM, RANK, COMSIZE)
C-----------------------------------------------
C Description: 
C A domain that own an edge warn others when
C - This edge is deleted
C - this edge is now free (one of the segment is
C   deleted)
C Comment:
C The two communications are done using ALLGATHERV
C to remote domain's LEDGE_FIE even when _SI structures
C are not updated yet according to SPMD_IFRONT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI25EBOX
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "i25edge_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEDGE
      INTEGER, INTENT(INOUT) :: LEDGE(NLEDGE,NEDGE)
      INTEGER, INTENT(IN) ::  NIN,
     .        ISENDTO(NINTER+1,*), IRCVFROM(NINTER+1,*)
      INTEGER :: COMM,RANK,COMSIZE
      my_real
     .        STFE(NEDGE)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------

      INTEGER, DIMENSION(:), ALLOCATABLE :: BUFFER_SEND,BUFFER_RECV
      INTEGER :: LOCAL_SIZE(COMSIZE,2), TOTAL_SIZE
      INTEGER :: DISPL(COMSIZE)
      INTEGER :: I,J, UID
      INTEGER :: COMSIZE2,LS
      INTEGER :: S_LEFT,S_RIGHT
      INTEGER :: ID_LEFT,ID_RIGHT
#ifdef MPI
      INTEGER DATA MSGOFF/1001/
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      IF(.NOT. (NSPMD == 1 .OR. COMM == MPI_COMM_NULL)) THEN 
!       Count fully broken edge
        LOCAL_SIZE(1:COMSIZE,1:2) = 0
        LOCAL_SIZE(RANK+1,1) = COUNT(STFE(1:NEDGE) < ZERO)
       
!       count new free edges
        DO I = 1,NEDGE
           IF(LEDGE(LEDGE_GLOBAL_ID,I) < 0) THEN
             LOCAL_SIZE(RANK+1,2) = LOCAL_SIZE(RANK+1,2) + 1 
           ENDIF
        ENDDO
       
        COMSIZE2 = COMSIZE * 2 
        CALL MPI_ALLREDUCE(MPI_IN_PLACE,
     .                   LOCAL_SIZE,
     .                   COMSIZE2,
     .                   MPI_INTEGER,
     .                   MPI_SUM,
     .                   COMM,
     .                   IERROR)


C
C     Send broken edges 
C 
        TOTAL_SIZE = SUM(LOCAL_SIZE(1:COMSIZE,1))
        IF(TOTAL_SIZE > 0) THEN
          ALLOCATE(BUFFER_SEND(LOCAL_SIZE(RANK+1,1)))
          ALLOCATE(BUFFER_RECV(TOTAL_SIZE))
          J = 0
          DO I = 1, NEDGE
            IF( STFE(I) < 0 ) THEN
              J = J + 1 
Cfill with global ID 
            BUFFER_SEND(J) = ABS(LEDGE(LEDGE_GLOBAL_ID,I))
#ifdef D_ES
              IF(ABS(LEDGE(LEDGE_GLOBAL_ID,I)) == D_ES) THEN
                WRITE(6,*) __FILE__,D_ES,"is deleted",STFE(I)
              ENDIF
#endif      
            ENDIF
          ENDDO
          DISPL(1)=0
          DO I=2,COMSIZE
            DISPL(I)=LOCAL_SIZE(I-1,1)+DISPL(I-1)
          ENDDO
          CALL MPI_ALLGATHERV(BUFFER_SEND,
     .                      LOCAL_SIZE(RANK+1,1),
     .                      MPI_INTEGER,
     .                      BUFFER_RECV,
     .                      LOCAL_SIZE(:,1),
     .                      DISPL,
     .                      MPI_INTEGER,
     .                      COMM,
     .                      IERROR)

          DEALLOCATE(BUFFER_SEND)
C hash table would be better here (UID -> index in LEDEG_FIE)
          DO J = 1, TOTAL_SIZE 
            UID = BUFFER_RECV(J)
            DO I = 1,NEDGE_REMOTE
              IF(LEDGE_FIE(NIN)%P(E_GLOBAL_ID,I) == UID) THEN
                STIFIE(NIN)%P(I) = ZERO
#ifdef D_ES
               IF(UID == D_ES) WRITE(6,*) __FILE__,"STF <- 0"
#endif
              ENDIF
            ENDDO
          ENDDO
          DEALLOCATE(BUFFER_RECV)
        ENDIF
C
C     Send Free edges 
C 
        TOTAL_SIZE = SUM(LOCAL_SIZE(1:COMSIZE,2))
        IF(TOTAL_SIZE > 0) THEN
          ALLOCATE(BUFFER_SEND(5*LOCAL_SIZE(RANK+1,2)))
          ALLOCATE(BUFFER_RECV(5*TOTAL_SIZE))
          J = 0
          DO I = 1, NEDGE
            IF( LEDGE(LEDGE_GLOBAL_ID,I) < 0 ) THEN
              J = J + 1 
Cfill with global ID 
              BUFFER_SEND(5*(J-1)+1) = ABS(LEDGE(LEDGE_GLOBAL_ID,I))
              BUFFER_SEND(5*(J-1)+2) = LEDGE(LEDGE_LEFT_SEG,I)
              BUFFER_SEND(5*(J-1)+3) = LEDGE(LEDGE_RIGHT_SEG,I)
              BUFFER_SEND(5*(J-1)+2) = LEDGE(LEDGE_LEFT_ID,I)
              BUFFER_SEND(5*(J-1)+3) = LEDGE(LEDGE_RIGHT_ID,I)
#ifdef D_ES
              IF(ABS(LEDGE(LEDGE_GLOBAL_ID,I)) == D_ES) THEN
                WRITE(6,*) __FILE__,D_ES,"is Free"
              ENDIF
#endif    
            ENDIF
          ENDDO
          DO I=1,COMSIZE
            LOCAL_SIZE(I,2) = LOCAL_SIZE(I,2) * 5 
          ENDDO
          DISPL(1)=0
          DO I=2,COMSIZE
            DISPL(I)=LOCAL_SIZE(I-1,2)+DISPL(I-1)
          ENDDO
          LS = LOCAL_SIZE(RANK+1,2)
          CALL MPI_ALLGATHERV(BUFFER_SEND,
     .                      LS,
     .                      MPI_INTEGER,
     .                      BUFFER_RECV,
     .                      LOCAL_SIZE(:,2),
     .                      DISPL,
     .                      MPI_INTEGER,
     .                      COMM,
     .                      IERROR)

          DEALLOCATE(BUFFER_SEND)
C hash table would be better here (UID -> index in LEDEG_FIE)
          DO J = 1, TOTAL_SIZE 
            UID =      BUFFER_RECV(5*(J-1)+1)
            S_LEFT =   BUFFER_RECV(5*(J-1)+2)
            S_RIGHT =  BUFFER_RECV(5*(J-1)+3)
            ID_LEFT =  BUFFER_RECV(5*(J-1)+4)
            ID_RIGHT = BUFFER_RECV(5*(J-1)+5)
        
            DO I = 1,NEDGE_REMOTE
              IF(LEDGE_FIE(NIN)%P(E_GLOBAL_ID,I) == UID) THEN
                LEDGE_FIE(NIN)%P(E_LEFT_SEG,I)  = S_LEFT
                LEDGE_FIE(NIN)%P(E_RIGHT_SEG,I) = S_RIGHT
                LEDGE_FIE(NIN)%P(E_LEFT_ID,I)   = ID_LEFT 
                LEDGE_FIE(NIN)%P(E_RIGHT_ID,I)  = ID_RIGHT
              ENDIF
            ENDDO
          ENDDO
          DEALLOCATE(BUFFER_RECV)
        ENDIF
      ENDIF ! NSPMD > 0 and MPI_COMM EXIST
#endif

      WHERE(STFE < ZERO) STFE = ZERO
      DO I = 1,NEDGE
         IF(LEDGE(LEDGE_GLOBAL_ID,I) < 0) THEN
            LEDGE(LEDGE_GLOBAL_ID,I) = ABS(LEDGE(LEDGE_GLOBAL_ID,I))
         ENDIF
      ENDDO

C     DO i = 1,nedge
C       IF(STFE(i) < ZERO) STFE(i) = ZERO
C     ENDDO
      RETURN
      END
  
