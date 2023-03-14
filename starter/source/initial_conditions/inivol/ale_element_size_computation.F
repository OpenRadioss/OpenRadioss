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
Chd|  ALE_ELEMENT_SIZE_COMPUTATION  source/initial_conditions/inivol/ale_element_size_computation.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE ALE_ELEMENT_SIZE_COMPUTATION(IPARG,IXS,IXQ,IXTG,
     .                                          ELEMENT_SIZE,MIN_MAX_POSITION,X,
     .                                          ALE_ELEMENT_NUMBER,ALE_NODE_NUMBER,LIST_ALE_NODE)
!$COMMENT
!       ALE_ELEMENT_SIZE_COMPUTATION description
!       ALE_ELEMENT_SIZE_COMPUTATION computes the maximum element size 
!       and the min / max position of nodes
!       the maximum element size & min / max position are used to defined the grid
!       
!       ALE_ELEMENT_SIZE_COMPUTATION organization :
!       - loop over the element group
!           - for each ALE element group, loop over the NEL elements
!               - compute the maximum element size 
!               - compute the min/max node's positions & save the ALE node id
!$ENDCOMMENT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
! com01 : ngroup/n2d
#include "com01_c.inc" 
! com04 : numels/numelq/numeltg
#include "com04_c.inc"
! param : nparg
#include "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(INOUT) :: ALE_ELEMENT_NUMBER ! number of ale element with material 51 or 151
        INTEGER, INTENT(INOUT) :: ALE_NODE_NUMBER ! number of ale node
        INTEGER, DIMENSION(NUMNOD), INTENT(INOUT) :: LIST_ALE_NODE ! list of ale node
        INTEGER, DIMENSION(NPARG,NGROUP), INTENT(IN) ::  IPARG  ! group data
        INTEGER, DIMENSION(NIXS,NUMELS),INTENT(IN), TARGET :: IXS ! solid data
        INTEGER, DIMENSION(NIXQ,NUMELQ),INTENT(IN), TARGET :: IXQ ! quad data
        INTEGER, DIMENSION(NIXTG,NUMELTG),INTENT(IN), TARGET :: IXTG ! triangle data
        my_real, INTENT(INOUT) :: ELEMENT_SIZE ! max element size
        my_real, DIMENSION(6), INTENT(INOUT) :: MIN_MAX_POSITION ! min/max position
        my_real, DIMENSION(3,NUMNOD), INTENT(IN) :: X ! position
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,J,K,NG
        INTEGER :: MTN,NEL,NFT,ITY,ISOLNOD,INIVOL
        INTEGER :: NODE_NUMBER,FIRST
        INTEGER, DIMENSION(:,:), POINTER :: IX
        INTEGER, DIMENSION(:), ALLOCATABLE :: TAG_ARRAY
        my_real :: LOCAL_SIZE
        my_real, DIMENSION(3) :: MAX_NODE,MIN_NODE,DISTANCE
C-----------------------------------------------
        ALLOCATE(TAG_ARRAY(NUMNOD))
        TAG_ARRAY(1:NUMNOD) = 0
        ELEMENT_SIZE = -ONE
        MIN_MAX_POSITION(1:3) = EP30
        MIN_MAX_POSITION(4:6) = -EP30
        ALE_ELEMENT_NUMBER = 0
        ALE_NODE_NUMBER = 0
        ! -----------------------
        ! loop over the solid / quad / triangle elements with 51/151 material
        DO NG=1,NGROUP
            MTN = IPARG(1,NG)   ! material law
            NEL = IPARG(2,NG)   ! number of element 
            NFT = IPARG(3,NG)   ! adress of first element
            ITY = IPARG(5,NG)   ! type of element 
            ISOLNOD = IPARG(28,NG)
            INIVOL = IPARG(53,NG)
            IF(INIVOL<=0) CYCLE
            IF(MTN/=51.AND.MTN/=151) CYCLE
            IF(N2D ==0.AND.ITY/= 1)THEN
              CYCLE
            ELSEIF(N2D>0.AND.ITY/=7.AND.ITY/= 2)THEN
              CYCLE
            ENDIF
            ! total number of ale element with material 51 or 151
            ALE_ELEMENT_NUMBER = ALE_ELEMENT_NUMBER + NEL

            ! ---------------
            ! depending on the king of element
            IF(ITY==1) THEN
                FIRST = 1
                NODE_NUMBER = 8
!                IX(1:NODE_NUMBER+1,1:NEL) => IXS(1:NODE_NUMBER+1,NFT+1:NFT+NEL)
                IX => IXS(1:NODE_NUMBER+1,NFT+1:NFT+NEL)
            ELSEIF(ITY==2) THEN
                FIRST = 2
                NODE_NUMBER = 4
!                IX(1:NODE_NUMBER+1,1:NEL) => IXQ(1:NODE_NUMBER+1,NFT+1:NFT+NEL)
                IX => IXQ(1:NODE_NUMBER+1,NFT+1:NFT+NEL)
            ELSEIF(ITY==7) THEN
                FIRST = 2
                NODE_NUMBER = 3
!                IX(1:NODE_NUMBER+1,1:NEL) => IXTG(1:NODE_NUMBER+1,NFT+1:NFT+NEL)
                IX => IXTG(1:NODE_NUMBER+1,NFT+1:NFT+NEL)
            ENDIF
            ! ---------------

            ! ---------------
            ! loop over the elements of the group to compute 
            ! the max element size and the min/max position
            DO J=1,NEL
                MAX_NODE(1:3) = -EP20
                MIN_NODE(1:3) =  EP20
                ! ---------------
                ! max element size
                DO I=1,NODE_NUMBER
                    MAX_NODE(FIRST:3) = MAX(MAX_NODE(FIRST:3),X(FIRST:3,IX(1+I,J)) )
                    MIN_NODE(FIRST:3) = MIN(MIN_NODE(FIRST:3),X(FIRST:3,IX(1+I,J)) )
                ENDDO
                DISTANCE(FIRST:3) = (ABS(MAX_NODE(1:3)-MIN_NODE(1:3)))**2
                LOCAL_SIZE = SQRT( SUM(DISTANCE(FIRST:3)) )
                ELEMENT_SIZE = MAX(ELEMENT_SIZE,LOCAL_SIZE)
                ! ---------------

                ! ---------------
                ! min / max position & save the ALE node id
                DO K=FIRST,3
                    ! min / max position
                    DO I=1,NODE_NUMBER
                        MIN_MAX_POSITION(K) = MIN(MIN_MAX_POSITION(K),X(K,IX(1+I,J)))
                        MIN_MAX_POSITION(3+K) = MAX(MIN_MAX_POSITION(3+K),X(K,IX(1+I,J)))
                        IF(TAG_ARRAY(IX(1+I,J))==0) THEN
                            TAG_ARRAY(IX(1+I,J)) = 1
                            ALE_NODE_NUMBER = ALE_NODE_NUMBER + 1
                            LIST_ALE_NODE(ALE_NODE_NUMBER) = IX(1+I,J)
                        ENDIF
                    ENDDO
                ENDDO
                ! ---------------
            ENDDO
            ! ---------------
        ENDDO

        DEALLOCATE(TAG_ARRAY)
        ! -----------------------

        RETURN
        END SUBROUTINE ALE_ELEMENT_SIZE_COMPUTATION
