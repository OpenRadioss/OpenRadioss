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
Chd|  SPMD_EXCH_TAG_TSH             source/mpi/elements/spmd_exch_dttsh.F
Chd|-- called by -----------
Chd|        TSHCDCOM_DIM                  source/elements/thickshell/solidec/tshcdcom_dim.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_TAG_TSH(IAD_ELEM ,FR_ELEM ,ISEND,IRECV ,LEN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
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
      INTEGER  LEN
      INTEGER, INTENT(IN) :: IAD_ELEM(2,*),FR_ELEM(*),ISEND(LEN)
      INTEGER, INTENT(INOUT) :: IRECV(LEN)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
       INTEGER LENCOM,MSGOFF ,MSGTYP
       INTEGER STATUS(MPI_STATUS_SIZE),REQ_R(NSPMD),REQ_S(NSPMD)
       INTEGER IERROR
       INTEGER I,P,J,NOD,SIZ,L,IAD
C-----------------------------------------------
       DATA MSGOFF/430/    
       
      DO I=1,NSPMD
        SIZ = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        IF(SIZ>0)THEN
          MSGTYP = MSGOFF
          L = IAD_ELEM(1,I)
          CALL MPI_IRECV(
     +      IRECV(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     +      MPI_COMM_WORLD,REQ_R(I),IERROR)
        ENDIF
      END DO
C
C   echange messages
C
      DO I=1,NSPMD
       SIZ = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
       IF(SIZ>0)THEN
          MSGTYP = MSGOFF
          L = IAD_ELEM(1,I)
          CALL MPI_ISEND(
     +      ISEND(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     +      MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF
      ENDDO
C
      DO I = 1, NSPMD
       SIZ = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        IF(SIZ>0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
        ENDIF
      ENDDO
      DO I = 1, NSPMD
       SIZ = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        IF(SIZ>0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
#endif
       
       END SUBROUTINE SPMD_EXCH_TAG_TSH
Chd|====================================================================
Chd|  SPMD_EXCH_VMAX                source/mpi/elements/spmd_exch_dttsh.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_VMAX(IAD_STSH ,FR_STSH ,IAD_RTSH ,FR_RTSH ,V_MAX )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
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
      INTEGER, INTENT(IN) :: IAD_STSH(*),FR_STSH(*),IAD_RTSH(*),FR_RTSH(*)
          my_real , INTENT(INOUT) :: V_MAX(NUMNOD)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
       INTEGER LENCOM,MSGOFF ,MSGTYP
       my_real, dimension(:), ALLOCATABLE :: SEND_BUF,REC_BUF
       INTEGER STATUS(MPI_STATUS_SIZE),REQ_R(NSPMD),REQ_S(NSPMD)
       INTEGER IERROR
       INTEGER P,J,NOD,SIZ,L,IAD
C-----------------------------------------------
       DATA MSGOFF/410/    
       MSGTYP=MSGOFF
       
       LENCOM = IAD_STSH(NSPMD+1) - IAD_STSH(1) 

C Allocation des Buffers           
       ALLOCATE(SEND_BUF(LENCOM),STAT=IERROR)
       IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
       ENDIF
       
       LENCOM = IAD_RTSH(NSPMD+1) - IAD_RTSH(1) 
       
       ALLOCATE(REC_BUF(LENCOM),STAT=IERROR)
       IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
       ENDIF   
C 
C ----------
C Set IRECV
C ----------
       L=1
       DO P=1,NSPMD 
          SIZ = IAD_RTSH(P+1)-IAD_RTSH(P)
          IF(SIZ > 0) THEN 
           CALL MPI_IRECV(
     S      REC_BUF(L),SIZ,REAL,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(P),IERROR)
            L=L+SIZ
          ENDIF
       ENDDO
       
C -----------------
C PREPARE SEND_BUF       
C -----------------
       L=1
       DO P=1,NSPMD 
         SIZ = IAD_STSH(P+1)-IAD_STSH(P)
         IF(SIZ > 0)THEN
           IAD = L
           DO J=IAD_STSH(P),IAD_STSH(P+1)-1
              NOD = FR_STSH(J)
              SEND_BUF(L)=V_MAX(NOD)
              L=L+1
           ENDDO         
         
           CALL MPI_ISEND(
     S       SEND_BUF(IAD),SIZ,REAL,IT_SPMD(P),MSGTYP,
     G       MPI_COMM_WORLD,REQ_S(P),IERROR)
     
         ENDIF
       ENDDO

C ------------
C END RECEIVE
C ------------
       L=1
       DO P=1,NSPMD 
         SIZ = IAD_RTSH(P+1)-IAD_RTSH(P)
         IF(SIZ > 0)THEN
           CALL MPI_WAIT(REQ_R(P),STATUS,IERROR)
            
           DO J=IAD_RTSH(P),IAD_RTSH(P+1)-1
             NOD = FR_RTSH(J)
             V_MAX(NOD) = MAX(V_MAX(NOD), REC_BUF(L))
             L = L+1
           ENDDO
         ENDIF
       ENDDO

C ------------
C END SEND
C ------------
      DO P = 1, NSPMD
        IF(IAD_STSH(P+1)-IAD_STSH(P)>0)THEN
          CALL MPI_WAIT(REQ_S(P),STATUS,IERROR)
        ENDIF
      ENDDO
      IF (ALLOCATED(SEND_BUF)) DEALLOCATE(SEND_BUF)
      IF (ALLOCATED(REC_BUF)) DEALLOCATE(REC_BUF)
       
#endif
       
       END SUBROUTINE SPMD_EXCH_VMAX
Chd|====================================================================
Chd|  SPMD_EXCH_FA                  source/mpi/elements/spmd_exch_dttsh.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_FA(IAD_STSH ,FR_STSH ,IAD_RTSH ,FR_RTSH ,A  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
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
      INTEGER, INTENT(IN) :: IAD_STSH(*),FR_STSH(*),IAD_RTSH(*),FR_RTSH(*)
          my_real , INTENT(INOUT) :: A(3,NUMNOD)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
       INTEGER LENCOM,MSGOFF ,MSGTYP
       my_real, dimension(:), ALLOCATABLE :: SEND_BUF,REC_BUF
       INTEGER STATUS(MPI_STATUS_SIZE),REQ_R(NSPMD),REQ_S(NSPMD)
       INTEGER IERROR
       INTEGER P,J,NOD,SIZ,L,IAD
C-----------------------------------------------
       DATA MSGOFF/420/    
       MSGTYP=MSGOFF
       
       LENCOM = IAD_STSH(NSPMD+1) - IAD_STSH(1) 

C Allocation des Buffers           
       ALLOCATE(SEND_BUF(3*LENCOM),STAT=IERROR)
       IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
       ENDIF
       LENCOM = IAD_RTSH(NSPMD+1) - IAD_RTSH(1) 
       
       ALLOCATE(REC_BUF(3*LENCOM),STAT=IERROR)
       IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
       ENDIF   
C 
C ----------
C Set IRECV
C ----------
       L=1
       DO P=1,NSPMD 
          SIZ = 3*(IAD_RTSH(P+1)-IAD_RTSH(P))
          IF(SIZ > 0) THEN 
           CALL MPI_IRECV(
     S      REC_BUF(L),SIZ,REAL,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(P),IERROR)
            L=L+SIZ
          ENDIF
       ENDDO
       
C -----------------
C PREPARE SEND_BUF       
C -----------------
       L=1
       DO P=1,NSPMD 
         SIZ = 3*(IAD_STSH(P+1)-IAD_STSH(P))
         IF(SIZ > 0)THEN
           IAD = L
           DO J=IAD_STSH(P),IAD_STSH(P+1)-1
              NOD = FR_STSH(J)
              SEND_BUF(L)  =A(1,NOD)
              SEND_BUF(L+1)=A(2,NOD)
              SEND_BUF(L+2)=A(3,NOD)
              L=L+3
           ENDDO         
         
           CALL MPI_ISEND(
     S       SEND_BUF(IAD),SIZ,REAL,IT_SPMD(P),MSGTYP,
     G       MPI_COMM_WORLD,REQ_S(P),IERROR)
     
         ENDIF
       ENDDO

C ------------
C END RECEIVE
C ------------
       L=1
       DO P=1,NSPMD 
         SIZ = 3*(IAD_RTSH(P+1)-IAD_RTSH(P))
         IF(SIZ > 0)THEN
           CALL MPI_WAIT(REQ_R(P),STATUS,IERROR)
            
           DO J=IAD_RTSH(P),IAD_RTSH(P+1)-1
             NOD = FR_RTSH(J)
             A(1,NOD) = REC_BUF(L)
             A(2,NOD) = REC_BUF(L+1)
             A(3,NOD) = REC_BUF(L+2)
             L = L+3
           ENDDO
         ENDIF
       ENDDO

C ------------
C END SEND
C ------------
      DO P = 1, NSPMD
        IF(IAD_STSH(P+1)-IAD_STSH(P)>0)THEN
          CALL MPI_WAIT(REQ_S(P),STATUS,IERROR)
        ENDIF
      ENDDO
      IF (ALLOCATED(SEND_BUF)) DEALLOCATE(SEND_BUF)
      IF (ALLOCATED(REC_BUF)) DEALLOCATE(REC_BUF)
       
#endif
       
       END SUBROUTINE SPMD_EXCH_FA
       
