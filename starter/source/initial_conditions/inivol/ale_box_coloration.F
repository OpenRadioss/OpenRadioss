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
Chd|  ALE_BOX_COLORATION            source/initial_conditions/inivol/ale_box_coloration.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ALLOC_3D_ARRAY                ../common_source/modules/array_mod.F
Chd|        ARRAY_MOD                     ../common_source/modules/array_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INIVOL_DEF_MOD                share/modules1/inivol_mod.F   
Chd|====================================================================
        SUBROUTINE ALE_BOX_COLORATION(NB_CELL_X,NB_CELL_Y,NB_CELL_Z,SURFACE_NUMBER,
     .                                MIN_MAX_POSITION,CELL,X,IGRSURF,INIVOL,CELL_POSITION,
     .                                ALE_NODE_NUMBER,LIST_ALE_NODE)
!$COMMENT
!       ALE_BOX_COLORATION description
!       ALE_BOX_COLORATION colors the grid created by ALE_BOX_CREATION routine
!       with the node of the surface
!       
!       ALE_BOX_COLORATION organization :
!       - loop over the surface
!           - loop over the node of the surface
!               - compute the node's position (ix,iy,iz) in the cell
!               - color the cell (ix,iy,iz) & all the cells crossed by a segment of the surface
!       - the position of ALE nodes in the grid are computed 
!$ENDCOMMENT
        USE ARRAY_MOD
        USE INIVOL_DEF_MOD , only:inivol_,conty_
        USE GROUPDEF_MOD , only:surf_
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
! com04 : numnod, nsurf
#include "com04_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: NB_CELL_X,NB_CELL_Y,NB_CELL_Z ! number of cell in x/y/z direction
        INTEGER, INTENT(IN) :: SURFACE_NUMBER ! number of surface
        TYPE(array_type), DIMENSION(SURFACE_NUMBER), INTENT(INOUT) :: CELL ! voxcell
        my_real, DIMENSION(6), INTENT(IN) :: MIN_MAX_POSITION ! min/max position
        my_real, DIMENSION(3,NUMNOD), INTENT(IN) :: X ! position
        INTEGER, DIMENSION(3,NUMNOD), INTENT(INOUT) :: CELL_POSITION ! position of node/cell
        TYPE (SURF_), DIMENSION(NSURF), INTENT(IN) :: IGRSURF ! surface data
        TYPE (INIVOL_), INTENT(IN) :: INIVOL ! inivol data
        INTEGER, INTENT(IN) :: ALE_NODE_NUMBER ! number of ale node
        INTEGER, DIMENSION(ALE_NODE_NUMBER), INTENT(IN) :: LIST_ALE_NODE ! list of ale node
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,J,K
        INTEGER :: II,JJ,KK
        INTEGER, DIMENSION(5) :: IX,IY,IZ
        INTEGER :: SURFACE_ID,SURFACE_NODE_NUMBER,SURFACE_TYPE
        INTEGER, DIMENSION(4) :: NODE_ID
        INTEGER :: LOW_X,UP_X
        INTEGER :: LOW_Y,UP_Y
        INTEGER :: LOW_Z,UP_Z
        my_real :: XMIN,YMIN,ZMIN
        my_real :: XMAX,YMAX,ZMAX
C-----------------------------------------------

        !   ------------------
        XMAX = MIN_MAX_POSITION(4)
        XMIN = MIN_MAX_POSITION(1)
        YMAX = MIN_MAX_POSITION(5)
        YMIN = MIN_MAX_POSITION(2)
        ZMAX = MIN_MAX_POSITION(6)
        ZMIN = MIN_MAX_POSITION(3)
        !   ------------------

        !   ------------------
        ! loop over the surface
        DO I=1,SURFACE_NUMBER
            ! ------------------
            ! allocation of CELL array
            CELL(I)%SIZE_INT_ARRAY_3D(1) = NB_CELL_X
            CELL(I)%SIZE_INT_ARRAY_3D(2) = NB_CELL_Y
            CELL(I)%SIZE_INT_ARRAY_3D(3) = NB_CELL_Z
            CALL ALLOC_3D_ARRAY(CELL(I))
            CELL(I)%INT_ARRAY_3D(1:NB_CELL_X,1:NB_CELL_Y,1:NB_CELL_Z) = 0
            ! ------------------

            ! ------------------
            SURFACE_ID = INIVOL%SURFCONTY(I) ! surface id
            SURFACE_NODE_NUMBER = IGRSURF(SURFACE_ID)%NSEG ! number of segment of the surface
            SURFACE_TYPE = IGRSURF(SURFACE_ID)%TYPE ! type of surface
            IF(SURFACE_TYPE/=200.AND.SURFACE_TYPE/=101) THEN
                DO K=1,SURFACE_NODE_NUMBER
                    ! ------------------
                    ! load the positions of the 4 nodes of the surface K
                    DO J=1,4
                        NODE_ID(J) = IGRSURF(SURFACE_ID)%NODES(K,J) ! node id
                        IX(J)=MAX(1,1+INT(NB_CELL_X*(X(1,NODE_ID(J))-XMIN)/(XMAX-XMIN)))
                        IY(J)=MAX(1,1+INT(NB_CELL_Y*(X(2,NODE_ID(J))-YMIN)/(YMAX-YMIN)))
                        IZ(J)=MAX(1,1+INT(NB_CELL_Z*(X(3,NODE_ID(J))-ZMIN)/(ZMAX-ZMIN)))
                    ENDDO
                    IX(5) = IX(1)
                    IY(5) = IY(1)
                    IZ(5) = IZ(1)
                    ! ------------------
                    ! tag the cells crossed by the segments of the surface K
                    !       1       2       3
                    !   * ----- * ----- * ----- *
                    !   | o_____|__o    |       |
                    ! a |  \    |  |    |       |           a1,b1,c1,a2,b2 are coloured
                    !   * --\-- * -o--- * ----- *
                    !   |    \  | /     |       |
                    ! b |     \ |/      |       |
                    !   * -----o* ----- * ----- *
                    !   |       |       |       |
                    ! c |       |       |       |
                    !   * ----- * ----- * ----- *
                

                    DO J=1,4
                        LOW_Z = MAX(1,IZ(J))
                        UP_Z = MIN(NB_CELL_Z,IZ(J+1))
                        LOW_Y = MAX(1,IY(J))
                        UP_Y = MIN(NB_CELL_Y,IY(J+1))
                        LOW_X = MAX(1,IX(J))
                        UP_X = MIN(NB_CELL_X,IX(J+1))
                        DO KK=LOW_Z,UP_Z
                            DO JJ=LOW_Y,UP_Y
                                DO II=LOW_X,UP_X
                                ! ------------------
                                    ! the cell (ii,jj,kk) contains a node of a surface or is crossed by the segment
                                    IF(CELL(I)%INT_ARRAY_3D(II,JJ,KK)/=2) THEN
                                        CELL(I)%INT_ARRAY_3D(II,JJ,KK) = 2
                                    ENDIF
                                    ! ------------------
                                ENDDO
                            ENDDO
                        ENDDO
                    ENDDO
                    ! ------------------
                ENDDO
                ! ------------------
            ENDIF
        ENDDO
        !   ------------------

        !   ------------------
        ! compute the position of ALE nodes in grid
        DO J=1,ALE_NODE_NUMBER
            I = LIST_ALE_NODE(J)
            CELL_POSITION(1,I) =MAX(1,1+INT(NB_CELL_X*(X(1,I)-XMIN)/(XMAX-XMIN)))
            CELL_POSITION(2,I) =MAX(1,1+INT(NB_CELL_Y*(X(2,I)-YMIN)/(YMAX-YMIN)))
            CELL_POSITION(3,I) =MAX(1,1+INT(NB_CELL_Z*(X(3,I)-ZMIN)/(ZMAX-ZMIN)))
        ENDDO   
        !   ------------------

        RETURN
        END SUBROUTINE ALE_BOX_COLORATION
