Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  SPMD_CELL_SIZE_POST_RCV                     source/mpi/interfaces/spmd_cell_size_post_rcv.F
Chd|-- called by -----------
Chd|        SPMD_CELL_LIST_EXCHANGE                    source/mpi/interfaces/spmd_cell_list_exchange.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE SPMD_CELL_SIZE_POST_RCV(IRCVFROM,ISENDTO,NIN,SORT_COMM,ITIED)
!$COMMENT
!       SPMD_CELL_SIZE_POST_RCV description :
!       prepare the reception of number of remote secondary nodes
!       SPMD_CELL_SIZE_POST_RCV organization :
!       the recv comm is limited to : proc with main nodes + remote proc with secondary nodes
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTER_SORTING_MOD
        USE TRI7BOX
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
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr02_c.inc"
#include      "scr18_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "timeri_c.inc"
#include      "sms_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: NIN
        INTEGER, INTENT(in) :: ITIED
        INTEGER, DIMENSION(NINTER+1,NSPMD+1), INTENT(in) :: ISENDTO,IRCVFROM
        TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
#ifdef MPI        
        INTEGER :: I,J,KK,IJK,KJI
        INTEGER :: P,P_LOC
        INTEGER IERROR1,STATUS(MPI_STATUS_SIZE),IERROR
        INTEGER :: LOC_PROC
        INTEGER :: MSGOFF, MSGOFF2, MSGOFF3, MSGOFF4, MSGOFF5
        INTEGER :: MSGTYP,INFO
        LOGICAL :: NEED_TO_RCV
!   --------------------------------------------------------------------     
        DATA MSGOFF/6021/
        DATA MSGOFF2/6022/
        DATA MSGOFF3/6023/
        DATA MSGOFF4/6024/ 
        DATA MSGOFF5/6025/ 
        
        !   --------------------------------
        LOC_PROC = ISPMD + 1
        IF(IRCVFROM(NIN,LOC_PROC).NE.0) THEN  !   local nmn>0
            !   -----------------------
            !   loop over the proc of the interface
            DO P_LOC = 1,SORT_COMM(NIN)%PROC_NUMBER
                P = SORT_COMM(NIN)%PROC_LIST(P_LOC)
                NSNFI(NIN)%P(P) = 0
                !   -----------------------  
                !   prepare the reception : only for proc with main node & remote proc with secondary nodes     
                IF(ISENDTO(NIN,P).NE.0) THEN  !   remote nsn>0
                    IF(LOC_PROC.NE.P) THEN
                        NEED_TO_RCV = .TRUE.
                        IF(SORT_COMM(NIN)%PROC_NUMBER>NSPMD/2) THEN
                            IF(.NOT.SORT_COMM(NIN)%KEEP_PROC(P_LOC)) THEN
                                NEED_TO_RCV = .FALSE.
                            ENDIF
                        ENDIF
                        IF(ITIED/=0) NEED_TO_RCV = .TRUE.    ! itied option : force the exchnage
                        IF(NEED_TO_RCV) THEN
                            MSGTYP = MSGOFF3 
                            SORT_COMM(NIN)%NBRECV_NB=SORT_COMM(NIN)%NBRECV_NB+1
                            SORT_COMM(NIN)%RECV_NB(SORT_COMM(NIN)%NBRECV_NB)=P   !   proc with nmn>0
                            CALL MPI_IRECV(NSNFI(NIN)%P(P),1,MPI_INTEGER,IT_SPMD(P),
     .                              MSGTYP,MPI_COMM_WORLD,SORT_COMM(NIN)%REQUEST_NB_R(SORT_COMM(NIN)%NBRECV_NB),IERROR)
                        ENDIF
                    ENDIF
                ENDIF
                !   -----------------------          
            ENDDO
            !   -----------------------
        ENDIF
        !   --------------------------------

#endif
        RETURN
        END SUBROUTINE SPMD_CELL_SIZE_POST_RCV
