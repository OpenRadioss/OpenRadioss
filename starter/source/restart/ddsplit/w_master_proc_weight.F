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
Chd|  W_MAIN_PROC_WEIGHT            source/restart/ddsplit/w_master_proc_weight.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
        SUBROUTINE W_MAIN_PROC_WEIGHT(NODGLOB,NUMNOD_L,NSPMD,PROC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: NUMNOD_L,NSPMD,PROC
        INTEGER, DIMENSION(*), INTENT(IN) :: NODGLOB
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
!       NUMNOD_L  : integer,
!                   number of local element
!       NSPMD     : integer,
!                   number of processor/domain
!       PROC      : integer,
!                   ID of the current processor
!       NODGLOB   : integer, dimension=NUMNOD_L
!                   gives the global ID of a local element
!                   NODGLOB( local_id) = global_id
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
        INTEGER  NLOCAL
        EXTERNAL NLOCAL    
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,N,P,G
        INTEGER, DIMENSION(:), ALLOCATABLE :: MAIN_PROC 
        INTEGER, DIMENSION(:), ALLOCATABLE :: WEIGHT

C-----------------------------------------------
!       allocate 1d array
        ALLOCATE( MAIN_PROC(NUMNOD_L) )
        ALLOCATE( WEIGHT(NUMNOD_L) )
! -----------------------------------
        MAIN_PROC(1:NUMNOD_L) = 0
        WEIGHT(1:NUMNOD_L) = 0
        DO I = 1,NUMNOD_L
                N = NODGLOB(I)
                DO P = 1, NSPMD 
                        IF(NLOCAL(N,P)==1) THEN
                                MAIN_PROC(I) = P 
                                GOTO 10
                        ENDIF
                ENDDO
 10             CONTINUE
        ENDDO
        
        DO I=1,NUMNOD_L
                IF(MAIN_PROC(I)==PROC) WEIGHT(I) = 1
        ENDDO        

        CALL WRITE_I_C(MAIN_PROC,NUMNOD_L)
        CALL WRITE_I_C(WEIGHT,NUMNOD_L)
! -----------------------------------
!       deallocate 1d array
        DEALLOCATE(MAIN_PROC )
        DEALLOCATE( WEIGHT )
! -----------------------------------
        RETURN

        END SUBROUTINE W_MAIN_PROC_WEIGHT
