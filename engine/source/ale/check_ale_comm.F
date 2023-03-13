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
Chd|  CHECK_ALE_COMM                source/ale/check_ale_comm.F   
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
        SUBROUTINE CHECK_ALE_COMM(IPARG_L,ELBUF_TAB,GLOBAL_ACTIVE_ALE_ELEMENT)
!$COMMENT
!       CHECK_ALE_COMM description
!           CHECK_ALE_COMM checks if all ALE elements are deactivated to
!           skip ALE solver
!       CHECK_ALE_COMM organization
!$ENDCOMMENT
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
#include "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------

C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        LOGICAL, INTENT(INOUT) :: GLOBAL_ACTIVE_ALE_ELEMENT
        INTEGER, DIMENSION(NPARG,NGROUP), INTENT(IN) :: IPARG_L
        TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP), INTENT(IN) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        LOGICAL :: ACTIVE_ALE_ELEMENT
        INTEGER :: NG,I,II,NFT
        INTEGER :: FIRST,LAST,LENCOM
        INTEGER :: ITY,MTN,JEUL

        INTEGER :: INDX_RCV,INDX_SD,INDX_RCV_TMP,INDX_SD_TMP
        INTEGER :: NUMBER_NODE
        my_real :: OFF_VALUE

        INTEGER :: IERROR
C-----------------------------------------------
        ! ---------------------------------
        IF(IALE+IEULER+ITHERM/=0) THEN
            ACTIVE_ALE_ELEMENT = .FALSE.
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
                    IF(OFF_VALUE/=ZERO) THEN
                        ACTIVE_ALE_ELEMENT = .TRUE.
                    ENDIF        
                ENDDO
            ENDDO
            ! -------------------


            IF(NSPMD>1) THEN
#ifdef MPI
                CALL MPI_ALLREDUCE(ACTIVE_ALE_ELEMENT,GLOBAL_ACTIVE_ALE_ELEMENT,1,
     .                         MPI_LOGICAL,MPI_LOR,MPI_COMM_WORLD,IERROR)
#endif
            ELSE
                GLOBAL_ACTIVE_ALE_ELEMENT = ACTIVE_ALE_ELEMENT
            ENDIF
        ELSE
            GLOBAL_ACTIVE_ALE_ELEMENT = .TRUE.
        ENDIF
        ! ---------------------------------

        RETURN
        END SUBROUTINE CHECK_ALE_COMM
