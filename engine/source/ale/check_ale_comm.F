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
Chd|  CHECK_ALE_NEIGHBOUR           source/ale/alemain.F
Chd|-- called by -----------
Chd|        RESOL                   source/engine/resol.F                
Chd|-- calls --------------- 
Chd|        SPMD_CHECK_ALE_NEIGHBOUR            source/mpi/fluid/spmd_check_ale_neighbour.F
Chd|====================================================================
        SUBROUTINE CHECK_ALE_COMM(IPARG_L,ELBUF_TAB,GLOBAL_ACTIVE_CFD_ELEMENT)
!$COMMENT
!       CHECK_ALE_COMM description
!           CHECK_ALE_COMM checks if ALE elements are deactivated to
!           avoid several mpi communications in the ALE sovler
!           if all the ALE elements are deactivated, ALE solver is skipped
!       CHECK_ALE_COMM organization
!           The neighbourhood is re-built in this routine
!           * a deactivated element is removed from the list of neighbour --> ACTIVE_ELEMENT = false
!           * exchange of deactivated elements 
!           * build new neighbourhood
!           * neighbourhood array is saved before its modification
!           * saved neighbourhood array is written in the restart file
!$ENDCOMMENT
        USE RESTMOD
        USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C  MPI
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "com01_c.inc"
#include "com04_c.inc"
#include "tabsiz_c.inc"
#include "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------

C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        LOGICAL, INTENT(INOUT) :: GLOBAL_ACTIVE_CFD_ELEMENT
        INTEGER, DIMENSION(NPARG,NGROUP), INTENT(IN) :: IPARG_L
        TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP), INTENT(IN) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        LOGICAL :: ACTIVE_CFD_ELEMENT
        INTEGER :: NG,I,II,NFT
        INTEGER :: FIRST,LAST,LENCOM
        INTEGER :: ITY,MTN,JEUL

        INTEGER :: INDX_RCV,INDX_SD,INDX_RCV_TMP,INDX_SD_TMP
        INTEGER :: NUMBER_NODE
        my_real :: OFF_VALUE
        LOGICAL, DIMENSION(:), ALLOCATABLE :: ACTIVE_ELEMENT
        INTEGER, DIMENSION(:), ALLOCATABLE :: TMP_NERCVOIS,TMP_NESDVOIS
        INTEGER, DIMENSION(:), ALLOCATABLE :: TMP_LERCVOIS,TMP_LESDVOIS

        INTEGER :: IERROR
C-----------------------------------------------
        ! allocation of saved arrays
        ALLOCATE( NESDVOIS_SAVE(SNESDVOIS) )
        ALLOCATE( NERCVOIS_SAVE(SNERCVOIS) )

        NESDVOIS_SAVE(1:SNESDVOIS) = NESDVOIS(1:SNESDVOIS)
        NERCVOIS_SAVE(1:SNERCVOIS) = NERCVOIS(1:SNERCVOIS)

        ALLOCATE( ACTIVE_ELEMENT(NUMELS+NUMELQ) )
        ACTIVE_ELEMENT(1:NUMELS+NUMELQ) = .TRUE.
        ACTIVE_CFD_ELEMENT = .FALSE.

        ! -------------------
        ! loop over the element groups to find the deactivated ALE elements
        DO NG=1,NGROUP
            ITY = IPARG_L(5,NG)    
            MTN = IPARG_L(1,NG) 
            IF(ITY==1.OR.ITY==2)THEN
                JEUL = IPARG_L(11,NG) 
            ELSEIF (MTN == 151 .AND. ITY == 7) THEN
                JEUL = IPARG_L(11,NG) 
            ELSE
                JEUL = 0
            END IF                 
            IF(IPARG_L(7,NG)+JEUL == 0)CYCLE
            FIRST = 1
            LAST = IPARG_L(2,NG)
            NFT = IPARG_L(3,NG)
            DO I=FIRST,LAST
                OFF_VALUE = ELBUF_TAB(NG)%GBUF%OFF(I)
                IF(OFF_VALUE==ZERO) THEN
                    II = I + NFT
                    ACTIVE_ELEMENT(II) = .FALSE.
                ELSE
                    ACTIVE_CFD_ELEMENT = .TRUE.
                ENDIF        
            ENDDO
        ENDDO
        ! -------------------

        LENCOM =NERCVOIS(NSPMD+1)+NESDVOIS(NSPMD+1)
        ALLOCATE( TMP_NERCVOIS(SNERCVOIS) )
        ALLOCATE( TMP_NESDVOIS(SNESDVOIS) )
        ALLOCATE( TMP_LERCVOIS(SLERCVOIS) )
        ALLOCATE( TMP_LESDVOIS(SLESDVOIS) )

        TMP_NERCVOIS(1:SNERCVOIS) = NERCVOIS(1:SNERCVOIS)
        TMP_NESDVOIS(1:SNESDVOIS) = NESDVOIS(1:SNESDVOIS)
        TMP_LERCVOIS(1:SLERCVOIS) = LERCVOIS(1:SLERCVOIS)
        TMP_LESDVOIS(1:SLESDVOIS) = LESDVOIS(1:SLESDVOIS)

        ! -------------------
        ! exchange of deactivated ALE elements to re-build the neighbourhood
        CALL SPMD_CHECK_ALE_NEIGHBOUR(ACTIVE_ELEMENT,NERCVOIS,NESDVOIS,LERCVOIS,LESDVOIS,LENCOM,
     .                                TMP_NERCVOIS,TMP_NESDVOIS,TMP_LERCVOIS,TMP_LESDVOIS)
        ! -------------------

        ! -------------------
        ! save in the NSPMD+1 case the total number of neighbours
        TMP_NERCVOIS(NSPMD+1) = 0
        TMP_NESDVOIS(NSPMD+1) = 0
        DO I=1,NSPMD
            TMP_NERCVOIS(NSPMD+1) = TMP_NERCVOIS(NSPMD+1) + TMP_NERCVOIS(I)
            TMP_NESDVOIS(NSPMD+1) = TMP_NESDVOIS(NSPMD+1) + TMP_NESDVOIS(I)
        ENDDO

        ! -------------------
        ! neighbourhood build 
        INDX_RCV = 0
        INDX_SD = 0
        INDX_RCV_TMP = 0
        INDX_SD_TMP = 0
        DO I=1,NSPMD
            IF(TMP_NERCVOIS(I)>0) THEN                
                LERCVOIS(1+INDX_RCV:TMP_NERCVOIS(I)+INDX_RCV) = TMP_LERCVOIS(1+INDX_RCV_TMP:TMP_NERCVOIS(I)+INDX_RCV_TMP)
                INDX_RCV = INDX_RCV + TMP_NERCVOIS(I)
                INDX_RCV_TMP = INDX_RCV_TMP + NERCVOIS(I)
            ENDIF
            IF(TMP_NESDVOIS(I)>0) THEN                
                LESDVOIS(1+INDX_SD:TMP_NESDVOIS(I)+INDX_SD) = TMP_LESDVOIS(1+INDX_SD_TMP:TMP_NESDVOIS(I)+INDX_SD_TMP)
                INDX_SD = INDX_SD + TMP_NESDVOIS(I)
                INDX_SD_TMP = INDX_SD_TMP + NESDVOIS(I)
            ENDIF
        ENDDO

        NERCVOIS(1:SNERCVOIS) = TMP_NERCVOIS(1:SNERCVOIS)
        NESDVOIS(1:SNESDVOIS) = TMP_NESDVOIS(1:SNESDVOIS)
        ! -------------------

        ! -------------------
        DEALLOCATE( TMP_NERCVOIS )
        DEALLOCATE( TMP_NESDVOIS )
        DEALLOCATE( TMP_LERCVOIS )
        DEALLOCATE( TMP_LESDVOIS )
        DEALLOCATE( ACTIVE_ELEMENT )
        ! -------------------


        IF(NSPMD>0) THEN
#ifdef MPI
            CALL MPI_ALLREDUCE(ACTIVE_CFD_ELEMENT,GLOBAL_ACTIVE_CFD_ELEMENT,1,
     .                         MPI_LOGICAL,MPI_LOR,MPI_COMM_WORLD,IERROR)
#endif
        ELSE
            GLOBAL_ACTIVE_CFD_ELEMENT = ACTIVE_CFD_ELEMENT
        ENDIF

        RETURN
        END SUBROUTINE CHECK_ALE_COMM
