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
Chd|====================================================================
        SUBROUTINE SPLIT_PCYL(TOTAL_NUMBER_PCYL,LOADS,LOADS_PER_PROC)
!$COMMENT
!       SPLIT_PCYL description
!       SPLIT_PCYL split the node/data of the /load option on the different processors
!       
!       SPLIT_PCYL organization :
!       loop over the nspmd processor :
!           * compute an index global load id --> local load id (local to the processor p)
!           * if the processor p has /load's nodes :
!               - save the data in a local structure LOADS_PER_PROC
!               - convert global node id into local node id
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE LOADS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
! nspmd definition
#include "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: TOTAL_NUMBER_PCYL ! total number of /load/pcyl
        TYPE(LOADS_),INTENT(INOUT) :: LOADS ! initial structure of load cyl
        TYPE(LOADS_),DIMENSION(NSPMD), INTENT(INOUT) :: LOADS_PER_PROC ! structure of load cyl for each processor
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,J,K
        INTEGER :: IJK,OFFSET
        INTEGER :: NODE_ID ! node id
        INTEGER :: PCYL_ID ! id of the /load/cyl 
        INTEGER :: LOCAL_NUMBER_CYL ! local number of /load/cyl on the current proc
        INTEGER :: LOCAL_SEGMENT_NUMBER ! local number of segment
        INTEGER :: SEGMENT_ID ! segment id
        INTEGER, DIMENSION(TOTAL_NUMBER_PCYL) :: OFFSET_CYL ! offset for the global segment id
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
        LOADS%S_GLOBAL_SEGMENT_ID = 0
        DO I=1,TOTAL_NUMBER_PCYL          
            LOADS%S_GLOBAL_SEGMENT_ID = LOADS%S_GLOBAL_SEGMENT_ID + LOADS%LOAD_CYL(I)%NSEG
        ENDDO
        ALLOCATE( LOADS%GLOBAL_SEGMENT_ID(LOADS%S_GLOBAL_SEGMENT_ID,3) )
        OFFSET = 0
        DO I=1,TOTAL_NUMBER_PCYL
            DO J=1,LOADS%LOAD_CYL(I)%NSEG                    
                LOADS%GLOBAL_SEGMENT_ID(J+OFFSET,1) = LOADS%CYL_RESTART(I)%SEGMENT_TO_PROC(J)
                LOADS%GLOBAL_SEGMENT_ID(J+OFFSET,3) = I
            ENDDO
            OFFSET_CYL(I) = OFFSET
            OFFSET = OFFSET + LOADS%LOAD_CYL(I)%NSEG
        ENDDO
        ! -------------------
        DO J=1,NSPMD
            LOCAL_NUMBER_CYL = 0
            ! ------------
            ! loop over the /LOAD/PCYL to count the number of load/pcyl on the processor J
            ALLOCATE( LOADS_PER_PROC(J)%INDEX_LOAD( LOADS%NLOAD_CYL,2 ) )
            DO I=1,LOADS%NLOAD_CYL
                LOCAL_SEGMENT_NUMBER = LOADS%CYL_RESTART(I)%PROC(J)%LOCAL_SEGMENT_NUMBER
                IF(LOCAL_SEGMENT_NUMBER>0) THEN
                    LOCAL_NUMBER_CYL = LOCAL_NUMBER_CYL + 1
                    LOADS_PER_PROC(J)%INDEX_LOAD(LOCAL_NUMBER_CYL,1) = I
                    LOADS_PER_PROC(J)%INDEX_LOAD(I,2) = LOCAL_NUMBER_CYL
                ELSE
                    LOADS_PER_PROC(J)%INDEX_LOAD(I,2) = 0
                ENDIF
            ENDDO
            ! ------------

            LOADS_PER_PROC(J)%NLOAD_CYL = LOCAL_NUMBER_CYL ! save the number of /oad/pcyl on the current proc
            ALLOCATE( LOADS_PER_PROC(J)%LOAD_CYL( LOCAL_NUMBER_CYL ) ) 
            ! ------------
            IF(LOCAL_NUMBER_CYL>0) THEN
                DO I=1,LOCAL_NUMBER_CYL
                    PCYL_ID = LOADS_PER_PROC(J)%INDEX_LOAD(I,1) ! get the id of the /load/cyl 
                    OFFSET = OFFSET_CYL(PCYL_ID)
                    LOADS_PER_PROC(J)%LOAD_CYL(I)%ID = LOADS%LOAD_CYL(PCYL_ID)%ID !  option ID
                    LOADS_PER_PROC(J)%LOAD_CYL(I)%IFRAME = LOADS%LOAD_CYL(PCYL_ID)%IFRAME !  moving frame 
                    LOADS_PER_PROC(J)%LOAD_CYL(I)%ISENS = LOADS%LOAD_CYL(PCYL_ID)%ISENS !  sensor number
                    LOADS_PER_PROC(J)%LOAD_CYL(I)%ITABLE = LOADS%LOAD_CYL(PCYL_ID)%ITABLE !  table number

                    LOADS_PER_PROC(J)%LOAD_CYL(I)%XSCALE_R = LOADS%LOAD_CYL(PCYL_ID)%XSCALE_R 
                    LOADS_PER_PROC(J)%LOAD_CYL(I)%XSCALE_T = LOADS%LOAD_CYL(PCYL_ID)%XSCALE_T 
                    LOADS_PER_PROC(J)%LOAD_CYL(I)%YSCALE = LOADS%LOAD_CYL(PCYL_ID)%YSCALE

                    LOADS_PER_PROC(J)%LOAD_CYL(I)%NSEG = LOADS%CYL_RESTART(PCYL_ID)%PROC(J)%LOCAL_SEGMENT_NUMBER ! number of segment of processor j
                    LOCAL_SEGMENT_NUMBER = LOADS%CYL_RESTART(PCYL_ID)%PROC(J)%LOCAL_SEGMENT_NUMBER
                    ALLOCATE( LOADS_PER_PROC(J)%LOAD_CYL(I)%SEGNOD(LOCAL_SEGMENT_NUMBER,4) )
                    ! ------------
                    DO K=1,LOCAL_SEGMENT_NUMBER
                        SEGMENT_ID = LOADS%CYL_RESTART(PCYL_ID)%PROC(J)%LOCAL_SEGMENT(K) ! get the segment id on the processor j
                        LOADS%GLOBAL_SEGMENT_ID(SEGMENT_ID+OFFSET,2) = K
                        DO IJK=1,4
                            NODE_ID = LOADS%LOAD_CYL(PCYL_ID)%SEGNOD(SEGMENT_ID,IJK) ! get the node id of the segment id on the processor j
                            LOADS_PER_PROC(J)%LOAD_CYL(I)%SEGNOD(K,IJK) = NODE_ID ! save the node id
                        ENDDO
                    ENDDO
                    ! ------------
                    LOADS_PER_PROC(J)%LOAD_CYL(I)%S_SEGMENT_ADRESS = LOCAL_SEGMENT_NUMBER
                    ALLOCATE( LOADS_PER_PROC(J)%LOAD_CYL(I)%SEGMENT_ADRESS(4,LOCAL_SEGMENT_NUMBER) )
                    LOADS_PER_PROC(J)%LOAD_CYL(I)%SEGMENT_ADRESS(1:4,1:LOCAL_SEGMENT_NUMBER) = 0
                ENDDO
            ! ------------
            ENDIF
            ! ------------
        ENDDO
        ! -------------------

        RETURN
        END SUBROUTINE SPLIT_PCYL
