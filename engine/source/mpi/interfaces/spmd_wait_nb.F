Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    spmd_wait_nb         ../engine/source/mpi/interfaces/spmd_wait_nb.F
!||--- called by ------------------------------------------------------
!||    inter_prepare_sort   ../engine/source/interfaces/generic/inter_prepare_sort.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    intbufdef_mod        ../common_source/modules/interfaces/intbufdef_mod.F90
!||    inter_sorting_mod    ../engine/share/modules/inter_sorting_mod.F
!||    message_mod          ../engine/share/message_module/message_mod.F
!||    tri7box              ../engine/share/modules/tri7box.F
!||====================================================================
        SUBROUTINE SPMD_WAIT_NB(IRCVFROM,ISENDTO,NIN,SORT_COMM)
!$COMMENT
!       SPMD_WAIT_NB description :
!       wait the message "number of secondary nodes needed by remote proc"
!       SPMD_WAIT_NB organization :
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE TRI7BOX
        USE INTBUFDEF_MOD  
        USE MESSAGE_MOD
        USE INTER_SORTING_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: NIN
        INTEGER, DIMENSION(NINTER+1,NSPMD+1), INTENT(in) :: ISENDTO,IRCVFROM
        TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm

#ifdef MPI        
        INTEGER :: I,J,NOD,L,L2,KK,KKK,IJK
        INTEGER :: P,P_LOC

        INTEGER IERROR1,STATUS(MPI_STATUS_SIZE),IERROR

        INTEGER :: LOC_PROC
!   ----------------------------------------

        ! ----------------------------
        ! wait the S comm
        LOC_PROC = ISPMD + 1
        SORT_COMM(NIN)%NBIRECV = 0
        SORT_COMM(NIN)%NSNR = 0
        IF(IRCVFROM(NIN,LOC_PROC)==0.AND.ISENDTO(NIN,LOC_PROC)==0) RETURN !CYCLE
        IF(ISENDTO(NIN,LOC_PROC)/=0) THEN   !   local nsn >0
            DO L=1,SORT_COMM(NIN)%NBSEND_NB
                CALL MPI_WAITANY(SORT_COMM(NIN)%NBSEND_NB,
     .                           SORT_COMM(NIN)%REQUEST_NB_S,P_LOC,STATUS,IERROR)
            ENDDO
        ENDIF   
        ! ----------------------------

        ! ----------------------------
        ! wait the R comm & compute the global number of remote secondary nodes sum( NB(p), p=1:number_of_proc)
        IF(IRCVFROM(NIN,LOC_PROC)/=0) THEN  !   local nmn>0
            IJK = 0

            CALL MPI_WAITALL(SORT_COMM(NIN)%NBRECV_NB,SORT_COMM(NIN)%REQUEST_NB_R,MPI_STATUSES_IGNORE,IERROR)

            L = 0
            SORT_COMM(NIN)%NSNR = 0
            DO P = 1, NSPMD
                IF(ISENDTO(NIN,P)/=0) THEN  !   nsn>0
                    IF(.NOT.ALLOCATED(SORT_COMM(NIN)%ISINDEXI)) THEN
                        ALLOCATE(SORT_COMM(NIN)%ISINDEXI(SORT_COMM(NIN)%PROC_NUMBER) )
                    ENDIF

                    IF(LOC_PROC/=P) THEN
                        IF(NSNFI(NIN)%P(P)>0) THEN
                            L=L+1
                            SORT_COMM(NIN)%ISINDEXI(L)=P
                            SORT_COMM(NIN)%NSNR = SORT_COMM(NIN)%NSNR + NSNFI(NIN)%P(P)
                        ENDIF
                    ENDIF
                ENDIF
            ENDDO
            SORT_COMM(NIN)%NBIRECV=L
        ENDIF
        ! ----------------------------

#endif
        RETURN
        END SUBROUTINE SPMD_WAIT_NB
