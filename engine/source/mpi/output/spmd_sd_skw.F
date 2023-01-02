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
Chd|  SPMD_SD_SKW                   source/mpi/output/spmd_sd_skw.F
Chd|-- called by -----------
Chd|        NEWSKW                        source/tools/skew/newskw.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_SD_SKW(SKEW,ISKWP_L_SEND,NUMSKW_L_SEND,RECVCOUNT)
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NUMSKW_L_SEND
      INTEGER, DIMENSION(NUMSKW_L_SEND), INTENT(IN) :: ISKWP_L_SEND
      my_real, DIMENSION(LSKEW,*), INTENT(INOUT) :: SKEW
      INTEGER, DIMENSION(NSPMD), INTENT(IN) :: RECVCOUNT
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       NUMSKW_L_SEND : integer 
!                       number of sent SKEW
!       ISKWP_L_SEND : integer ; dimension=NUMSKW_L_SEND
!                      index of sent SKEW
!       SKEW : integer ; dimension=LSKEW*number of SKEW
!              SKEW array
!       RECVCOUNT : integer ; dimension=NSPMD
!                   number of received SKEW
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
        INTEGER :: I,K,N,LOC_PROC,NN
        INTEGER :: IERROR
        INTEGER :: SENDCOUNT,TOTAL_RECV
        INTEGER, DIMENSION(NSPMD) :: DIPSPL
        my_real, DIMENSION(10*NUMSKW) :: SBUF,RBUF

C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
!$COMMENT
!       SPMD_SD_SKW description
!       communication of SKEW array with a ALLGATHERV comm
!       
!       SPMD_SD_SKW organization :
!       - initialize the SBUF buffer with the local SKEW value
!       - initialize the displacement and the total number of 
!         received SKEW
!       - ALLGATHERV comm
!       - fill the SKEW array with the received RBUFF buffer
!
!       proc 1 :
!               SBUF :   1 | 4 | 100          (--> 3 values)
!       proc 2 :
!               SBUF :   2 | 3                (--> 2 values)
!       proc 3 :
!               SBUF :   5 | 99 | 102 | 1000  (--> 4 values)
!       displacement : 
!                       DIPSPL(1) = 0
!                       DIPSPL(2) = 3
!                       DIPSPL(3) = 5
!
!       RBUF :
!               1 | 4 | 100 | 2 | 3 | 5 | 99 | 102 | 1000
!
!$ENDCOMMENT

        LOC_PROC = ISPMD + 1
        DIPSPL(1:NSPMD) = 0
        K = 0
!       initialization of SBUF
        DO NN = 1, NUMSKW_L_SEND
                N = ISKWP_L_SEND(NN)
                K = K + 1
                SBUF(1+(K-1)*10) = SKEW(1,N+1)
                SBUF(2+(K-1)*10) = SKEW(2,N+1)
                SBUF(3+(K-1)*10) = SKEW(3,N+1)
                SBUF(4+(K-1)*10) = SKEW(4,N+1)
                SBUF(5+(K-1)*10) = SKEW(5,N+1)
                SBUF(6+(K-1)*10) = SKEW(6,N+1)
                SBUF(7+(K-1)*10) = SKEW(7,N+1)
                SBUF(8+(K-1)*10) = SKEW(8,N+1)
                SBUF(9+(K-1)*10) = SKEW(9,N+1)
                SBUF(10+(K-1)*10) = N+1
        END DO

!       displacement, number of sent value and total number of received values
        SENDCOUNT = K*10
        DIPSPL(1)=0
        TOTAL_RECV = RECVCOUNT(1)
        DO I=2,NSPMD
                DIPSPL(I)=RECVCOUNT(I-1)+DIPSPL(I-1)
                TOTAL_RECV = TOTAL_RECV + RECVCOUNT(I)
        ENDDO
        TOTAL_RECV = TOTAL_RECV / 10              

!       comm        
        CALL MPI_ALLGATHERV(SBUF,SENDCOUNT,REAL,RBUF,RECVCOUNT,DIPSPL,REAL,MPI_COMM_WORLD,IERROR)
                
!       fill the SKEW array 
        DO I=1,TOTAL_RECV
                K = NINT(RBUF(10+(I-1)*10))
                SKEW(1,K) = RBUF(1+(I-1)*10)
                SKEW(2,K) = RBUF(2+(I-1)*10)
                SKEW(3,K) = RBUF(3+(I-1)*10)
                SKEW(4,K) = RBUF(4+(I-1)*10)
                SKEW(5,K) = RBUF(5+(I-1)*10)
                SKEW(6,K) = RBUF(6+(I-1)*10)
                SKEW(7,K) = RBUF(7+(I-1)*10)
                SKEW(8,K) = RBUF(8+(I-1)*10)
                SKEW(9,K) = RBUF(9+(I-1)*10)
        ENDDO               
C
#endif
      RETURN
      END SUBROUTINE SPMD_SD_SKW
C
Chd|====================================================================
Chd|  SPMD_SD_SKW_ANIM              source/mpi/output/spmd_sd_skw.F
Chd|-- called by -----------
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_SD_SKW_ANIM(SKEW,ISKWP)
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, DIMENSION(*), INTENT(IN) :: ISKWP
      my_real, DIMENSION(LSKEW,*), INTENT(INOUT) :: SKEW
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       ISKWP : integer ; dimension=NUMSKW+1
!               give the location of the SKEW
!               a SKEW can be on several processor 
!       SKEW : integer ; dimension=LSKEW*number of SKEW
!              SKEW array
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
        INTEGER :: I,K,N,LOC_PROC,j
        INTEGER :: IERROR
        INTEGER :: SENDCOUNT,TOTAL_RECV
        INTEGER, DIMENSION(NSPMD) :: RECVCOUNT,DIPSPL
        my_real, DIMENSION(10*NUMSKW) :: SBUF,RBUF

C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
!$COMMENT
!       SPMD_SD_SKW_ANIM description
!       communication of SKEW array with a ALLGATHERV comm
!       on main processor (proc 0) before the animation
!       
!       SPMD_SD_SKW_ANIM organization :
!       - initialize the SBUF buffer with the local SKEW value
!       - initialize the displacement and the total number of 
!         received SKEW
!       - ALLGATHER comm on 0 proc
!       - fill the SKEW array with the received RBUFF buffer
!
!       proc 1 :
!               SBUF :   1 | 4 | 100          (--> 3 values)
!       proc 2 :
!               SBUF :   2 | 3                (--> 2 values)
!       proc 3 :
!               SBUF :   5 | 99 | 102 | 1000  (--> 4 values)
!       displacement : 
!                       DIPSPL(1) = 0
!                       DIPSPL(2) = 3
!                       DIPSPL(3) = 5
!
!       RBUF (on main proc):
!               1 | 4 | 100 | 2 | 3 | 5 | 99 | 102 | 1000
!
!$ENDCOMMENT
        LOC_PROC = ISPMD + 1
        RECVCOUNT(1:NSPMD) = 0
        DIPSPL(1:NSPMD) = 0
        K = 0
!       initialization of SBUF
        DO N = 1, NUMSKW
                IF(ABS(ISKWP(N+1))==LOC_PROC)THEN
                        K = K + 1
                        SBUF(1+(K-1)*10) = SKEW(1,N+1)
                        SBUF(2+(K-1)*10) = SKEW(2,N+1)
                        SBUF(3+(K-1)*10) = SKEW(3,N+1)
                        SBUF(4+(K-1)*10) = SKEW(4,N+1)
                        SBUF(5+(K-1)*10) = SKEW(5,N+1)
                        SBUF(6+(K-1)*10) = SKEW(6,N+1)
                        SBUF(7+(K-1)*10) = SKEW(7,N+1)
                        SBUF(8+(K-1)*10) = SKEW(8,N+1)
                        SBUF(9+(K-1)*10) = SKEW(9,N+1)
                        SBUF(10+(K-1)*10) = N+1
                END IF
                IF(ISKWP(N+1)/=0) RECVCOUNT(ABS(ISKWP(N+1))) = RECVCOUNT(ABS(ISKWP(N+1))) + 10
        END DO

!       displacement, number of sent value and total number of received values
        SENDCOUNT = K*10
        DIPSPL(1)=0
        TOTAL_RECV = RECVCOUNT(1)
        DO I=2,NSPMD
                DIPSPL(I)=RECVCOUNT(I-1)+DIPSPL(I-1)
                TOTAL_RECV = TOTAL_RECV + RECVCOUNT(I)
        ENDDO
        TOTAL_RECV = TOTAL_RECV / 10              

!       comm on proc 0 (main)       
        CALL MPI_GATHERV(SBUF,SENDCOUNT,REAL,RBUF,RECVCOUNT,DIPSPL,REAL,0,MPI_COMM_WORLD,IERROR)
                
!       fill the RBUF array (only main proc)
        IF(ISPMD==0) THEN
                DO I=1,TOTAL_RECV
                        K = NINT(RBUF(10+(I-1)*10))
                        SKEW(1,K) = RBUF(1+(I-1)*10)
                        SKEW(2,K) = RBUF(2+(I-1)*10)
                        SKEW(3,K) = RBUF(3+(I-1)*10)
                        SKEW(4,K) = RBUF(4+(I-1)*10)
                        SKEW(5,K) = RBUF(5+(I-1)*10)
                        SKEW(6,K) = RBUF(6+(I-1)*10)
                        SKEW(7,K) = RBUF(7+(I-1)*10)
                        SKEW(8,K) = RBUF(8+(I-1)*10)
                        SKEW(9,K) = RBUF(9+(I-1)*10)
                ENDDO               
        ENDIF
C
#endif
      RETURN
      END SUBROUTINE SPMD_SD_SKW_ANIM
