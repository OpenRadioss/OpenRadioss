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
Chd|  SPMD_EXCH_MIN_MAX             source/mpi/ale/spmd_exch_min_max.F
Chd|-- called by -----------
Chd|        ALE51_GRADIENT_RECONSTRUCTION source/ale/alemuscl/ale51_gradient_reconstruction.F
Chd|        ALE51_GRADIENT_RECONSTRUCTION2source/ale/alemuscl/ale51_gradient_reconstruction2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_MIN_MAX(IAD_ELEM ,FR_ELEM ,MIN_ARRAY,MAX_ARRAY )
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
      INTEGER, INTENT(IN) :: IAD_ELEM(2,*),FR_ELEM(*)
          my_real , INTENT(INOUT) :: MIN_ARRAY(NUMNOD),MAX_ARRAY(NUMNOD)
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
       DATA MSGOFF/400/    
       MSGTYP=MSGOFF
       
       LENCOM = ( IAD_ELEM(1,NSPMD+1) - IAD_ELEM(1,1) )

C Allocation des Buffers           
       ALLOCATE(SEND_BUF(2 * LENCOM),STAT=IERROR)
       IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
       ENDIF
       
       ALLOCATE(REC_BUF(2 * LENCOM),STAT=IERROR)
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
              SIZ = IAD_ELEM(1,P+1)-IAD_ELEM(1,P)
          IF(SIZ > 0) THEN 
           CALL MPI_IRECV(
     S      REC_BUF(L),2*SIZ,REAL,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(P),IERROR)
            L=L+2*SIZ
          ENDIF
       ENDDO
       
C -----------------
C PREPARE SEND_BUF       
C -----------------
       L=1
       DO P=1,NSPMD 
         SIZ = IAD_ELEM(1,P+1)-IAD_ELEM(1,P)
         IF(SIZ > 0)THEN
           IAD = L
           DO J=IAD_ELEM(1,P),IAD_ELEM(1,P+1)-1
              NOD = FR_ELEM(J)
              SEND_BUF(L)=MIN_ARRAY(NOD)
              SEND_BUF(L + 1)=MAX_ARRAY(NOD)
              L=L+2
           ENDDO         
         
           CALL MPI_ISEND(
     S       SEND_BUF(IAD),2*SIZ,REAL,IT_SPMD(P),MSGTYP,
     G       MPI_COMM_WORLD,REQ_S(P),IERROR)
     
         ENDIF
       ENDDO

C ------------
C END RECEIVE
C ------------
       L=1
       DO P=1,NSPMD 
         SIZ = IAD_ELEM(1,P+1)-IAD_ELEM(1,P)
         IF(SIZ > 0)THEN
           CALL MPI_WAIT(REQ_R(P),STATUS,IERROR)
            
           DO J=IAD_ELEM(1,P),IAD_ELEM(1,P+1)-1
             NOD = FR_ELEM(J)
             MIN_ARRAY(NOD) = MIN(MIN_ARRAY(NOD), REC_BUF(L))
             MAX_ARRAY(NOD) = MAX(MAX_ARRAY(NOD), REC_BUF(L + 1))
             L = L+2
           ENDDO
         ENDIF
       ENDDO

C ------------
C END SEND
C ------------
      DO P = 1, NSPMD
        IF(IAD_ELEM(1,P+1)-IAD_ELEM(1,P)>0)THEN
          CALL MPI_WAIT(REQ_S(P),STATUS,IERROR)
        ENDIF
      ENDDO
      IF (ALLOCATED(SEND_BUF)) DEALLOCATE(SEND_BUF)
      IF (ALLOCATED(REC_BUF)) DEALLOCATE(REC_BUF)
       
#endif
       
       END
       
