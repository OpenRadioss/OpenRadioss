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
Chd|  INTER_DEALLOCATE_WAIT         source/interfaces/generic/inter_deallocate_wait.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTER_SORTING_MOD             share/modules/inter_sorting_mod.F
Chd|        INTER_STRUCT_MOD              share/modules/inter_struct_mod.F
Chd|        METRIC_MOD                    ../common_source/modules/metric_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
        SUBROUTINE INTER_DEALLOCATE_WAIT( ITASK,NB_INTER_SORTED,LIST_INTER_SORTED,IPARI,ISENDTO,
     1                                    IRECVFROM,INTBUF_TAB,SENSOR_TAB,INTER_STRUCT,SORT_COMM )
!$COMMENT
!       INTER_DEALLOCATE_WAIT description
!       wait the message "send the secondary nodes" & deallocation of array
!       INTER_DEALLOCATE_WAIT organization :
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MULTI_FVM_MOD
      USE H3D_MOD
      USE METRIC_MOD
      USE INTBUFDEF_MOD  
      USE INTER_STRUCT_MOD
      USE INTER_SORTING_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(in) :: ITASK    !   omp thread ID
      INTEGER, INTENT(in) :: NB_INTER_SORTED        !   number of interfaces that need to be sorted
      INTEGER, DIMENSION(NB_INTER_SORTED), INTENT(in) :: LIST_INTER_SORTED   !   list of interfaces that need to be sorted
      INTEGER, DIMENSION(NPARI,NINTER), INTENT(in) :: IPARI    !   interface data
      INTEGER, DIMENSION(NINTER+1,NSPMD+1), INTENT(in) :: ISENDTO,IRECVFROM 
      TYPE(INTBUF_STRUCT_),DIMENSION(NINTER), INTENT(inout) :: INTBUF_TAB    ! interface data
      TYPE(inter_struct_type), DIMENSION(NINTER), INTENT(inout) :: INTER_STRUCT   !   structure for interface
      TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: KK,N,ESHIFT,P
        INTEGER :: NTY,INACTI,ISENS
        INTEGER :: NRTM_T,NSN,NMN
        LOGICAL :: TYPE18
#ifdef MPI
        INTEGER :: IERROR,STATUS(MPI_STATUS_SIZE)
#endif
        my_real :: TS
!   ----------------------------------------
        IF(ITASK==0.AND.NB_INTER_SORTED>0) THEN
            DO KK=1,NB_INTER_SORTED
                N = LIST_INTER_SORTED(KK)

                NTY = IPARI(7,N)
                INACTI = IPARI(22,N)
                TYPE18=.FALSE.
                IF(NTY==7.AND.INACTI==7)TYPE18=.TRUE.

                ISENS = 0
                IF(NTY == 7.OR.NTY == 11.OR.NTY == 24.OR.NTY == 25) ISENS = IPARI(64,N)  
                IF (ISENS > 0) THEN    ! IF INTERFACE IS ACTIVATED BY SENSOR 
                    TS = SENSOR_TAB(ISENS)%TSTART
                ELSE
                    TS = TT
                ENDIF

                NSN = IPARI(5,N)
                NMN = IPARI(6,N)
                ! -----------------------------------------------------
                ! type 7
                IF((NTY==7.AND.TT>=TS).AND.(.NOT.TYPE18))THEN
                    ! wait the message only for proc with only secondary nodes 
                    IF(NSN>0.AND.NMN==0) THEN   !   local nsn>0 && local nmn == 0
                        DO P = 1, NSPMD
                            IF(IRECVFROM(N,P)/=0) THEN     !   nmn >0
                                IF(P/=ISPMD+1) THEN
                                    IF(SORT_COMM(N)%NB(P)/=0) THEN
#ifdef MPI
                                        CALL MPI_WAIT(SORT_COMM(N)%REQ_SD2(P),STATUS,IERROR) 
                                        DEALLOCATE(SORT_COMM(N)%DATA_PROC(P)%RBUF) 
                                        CALL MPI_WAIT(SORT_COMM(N)%REQ_SD3(P),STATUS,IERROR) 
                                        DEALLOCATE(SORT_COMM(N)%DATA_PROC(P)%IBUF)  
                                        SORT_COMM(N)%NB(P) = 0 
#endif                     
                                    END IF
                                ENDIF
                            ENDIF
                        ENDDO
                    ENDIF
                ENDIF
                ! -----------------------------------------------------
                ! deallocation
                IF(ALLOCATED(SORT_COMM(N)%COARSE_GRID) )DEALLOCATE( SORT_COMM(N)%COARSE_GRID )
                IF(ALLOCATED(SORT_COMM(N)%MAIN_COARSE_GRID) )DEALLOCATE( SORT_COMM(N)%MAIN_COARSE_GRID )
                IF(ALLOCATED(SORT_COMM(N)%GLOBAL_COARSE_CELL) )DEALLOCATE( SORT_COMM(N)%GLOBAL_COARSE_CELL )

                IF(ALLOCATED(SORT_COMM(N)%SEND_SIZE_COARSE_CELL) ) DEALLOCATE( SORT_COMM(N)%SEND_SIZE_COARSE_CELL )
                IF(ALLOCATED(SORT_COMM(N)%RCV_SIZE_COARSE_CELL) ) DEALLOCATE( SORT_COMM(N)%RCV_SIZE_COARSE_CELL )
                IF(ALLOCATED(SORT_COMM(N)%SEND_DISPLS_COARSE_CELL) ) DEALLOCATE( SORT_COMM(N)%SEND_DISPLS_COARSE_CELL )
                IF(ALLOCATED(SORT_COMM(N)%RCV_DISPLS_COARSE_CELL) ) DEALLOCATE( SORT_COMM(N)%RCV_DISPLS_COARSE_CELL )
               

                IF(ALLOCATED(SORT_COMM(N)%KEEP_PROC) )DEALLOCATE( SORT_COMM(N)%KEEP_PROC )
                IF(ALLOCATED(SORT_COMM(N)%CELL_LIST) )DEALLOCATE( SORT_COMM(N)%CELL_LIST )
                IF(ALLOCATED(SORT_COMM(N)%NB_CELL_PROC) )DEALLOCATE( SORT_COMM(N)%NB_CELL_PROC )
                IF(ALLOCATED(SORT_COMM(N)%CELL) )DEALLOCATE( SORT_COMM(N)%CELL )

                IF(ALLOCATED(SORT_COMM(N)%SEND_NB_CELL) ) DEALLOCATE( SORT_COMM(N)%SEND_NB_CELL )
                IF(ALLOCATED(SORT_COMM(N)%RCV_NB_CELL) ) DEALLOCATE( SORT_COMM(N)%RCV_NB_CELL )
                IF(ALLOCATED(SORT_COMM(N)%SEND_DISPLS_NB_CELL) ) DEALLOCATE( SORT_COMM(N)%SEND_DISPLS_NB_CELL )
                IF(ALLOCATED(SORT_COMM(N)%RCV_DISPLS_NB_CELL) ) DEALLOCATE( SORT_COMM(N)%RCV_DISPLS_NB_CELL )

                IF(ALLOCATED(SORT_COMM(N)%SEND_SIZE_CELL) ) DEALLOCATE( SORT_COMM(N)%SEND_SIZE_CELL )
                IF(ALLOCATED(SORT_COMM(N)%RCV_SIZE_CELL) ) DEALLOCATE( SORT_COMM(N)%RCV_SIZE_CELL )
                IF(ALLOCATED(SORT_COMM(N)%SEND_DISPLS_CELL) ) DEALLOCATE( SORT_COMM(N)%SEND_DISPLS_CELL )
                IF(ALLOCATED(SORT_COMM(N)%RCV_DISPLS_CELL) ) DEALLOCATE( SORT_COMM(N)%RCV_DISPLS_CELL )

                IF(ALLOCATED(SORT_COMM(N)%REQUEST_NB_S) ) DEALLOCATE( SORT_COMM(N)%REQUEST_NB_S )
                IF(ALLOCATED(SORT_COMM(N)%REQUEST_NB_R) ) DEALLOCATE( SORT_COMM(N)%REQUEST_NB_R )
                IF(ALLOCATED(SORT_COMM(N)%NB) ) DEALLOCATE( SORT_COMM(N)%NB )
                IF(ALLOCATED(SORT_COMM(N)%INDEX_RCV) ) DEALLOCATE( SORT_COMM(N)%INDEX_RCV )

                IF(ALLOCATED(SORT_COMM(N)%SEND_NB) ) DEALLOCATE( SORT_COMM(N)%SEND_NB )
                IF(ALLOCATED(SORT_COMM(N)%RECV_NB) ) DEALLOCATE( SORT_COMM(N)%RECV_NB )

                IF(ALLOCATED(SORT_COMM(N)%REQUEST_CELL_SEND) ) DEALLOCATE( SORT_COMM(N)%REQUEST_CELL_SEND )
                IF(ALLOCATED(SORT_COMM(N)%REQUEST_CELL_RCV) ) DEALLOCATE( SORT_COMM(N)%REQUEST_CELL_RCV )

                IF(ALLOCATED(SORT_COMM(N)%IIX) )DEALLOCATE( SORT_COMM(N)%IIX )
                IF(ALLOCATED(SORT_COMM(N)%IIY) )DEALLOCATE( SORT_COMM(N)%IIY )
                IF(ALLOCATED(SORT_COMM(N)%IIZ) )DEALLOCATE( SORT_COMM(N)%IIZ )
                IF(ALLOCATED(SORT_COMM(N)%LAST_NOD) )DEALLOCATE( SORT_COMM(N)%LAST_NOD )
                IF(ALLOCATED(SORT_COMM(N)%NEXT_NOD) )DEALLOCATE( SORT_COMM(N)%NEXT_NOD )
                IF(ALLOCATED(SORT_COMM(N)%VOXEL) )DEALLOCATE( SORT_COMM(N)%VOXEL )

                IF(ALLOCATED(INTER_STRUCT(N)%CURV_MAX)) DEALLOCATE(INTER_STRUCT(N)%CURV_MAX)
                ! -----------------------------------------------------
            ENDDO
            IF(ALLOCATED(NB_LOCAL_CELL) )DEALLOCATE( NB_LOCAL_CELL )
            IF(ALLOCATED(CELL_BOOL) )DEALLOCATE( CELL_BOOL )
        ENDIF
        CALL MY_BARRIER()

        RETURN
        END SUBROUTINE INTER_DEALLOCATE_WAIT
