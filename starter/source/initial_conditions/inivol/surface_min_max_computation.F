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
!||    surface_min_max_computation   ../starter/source/initial_conditions/inivol/surface_min_max_computation.F
!||--- called by ------------------------------------------------------
!||    init_inivol                   ../starter/source/initial_conditions/inivol/init_inivol.F90
!||--- uses       -----------------------------------------------------
!||    inivol_def_mod                ../starter/share/modules1/inivol_mod.F
!||====================================================================
      SUBROUTINE SURFACE_MIN_MAX_COMPUTATION(
     .                                       NSURF, NUMNOD,
     .                                       SURFACE_NUMBER,MIN_MAX_POSITION,X,IGRSURF,INIVOL)

        USE INIVOL_DEF_MOD , only : inivol_struct_, container_
        USE GROUPDEF_MOD , only : surf_
!$COMMENT
!       SURFACE_MIN_MAX_COMPUTATION description
!       SURFACE_MIN_MAX_COMPUTATION computes the min/max position of surface's nodes
!       
!       SURFACE_MIN_MAX_COMPUTATION organization :
!$ENDCOMMENT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER,INTENT(IN) :: NSURF,NUMNOD
        INTEGER, INTENT(IN) :: SURFACE_NUMBER ! number of surface
        my_real, DIMENSION(6), INTENT(INOUT) :: MIN_MAX_POSITION ! min/max position
        my_real, DIMENSION(3,NUMNOD), INTENT(IN) :: X ! position
        TYPE (SURF_), DIMENSION(NSURF), INTENT(IN) :: IGRSURF ! surface data
        TYPE (INIVOL_STRUCT_), INTENT(IN) :: INIVOL ! inivol data
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,J,K
        INTEGER :: SURFACE_ID,SURFACE_NODE_NUMBER,SURFACE_TYPE
        INTEGER :: NODE_ID
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
        DO I=1,SURFACE_NUMBER
          SURFACE_ID = INIVOL%CONTAINER(I)%SURF_ID ! surface id
          SURFACE_NODE_NUMBER = IGRSURF(SURFACE_ID)%NSEG ! number of segment of the surface
          SURFACE_TYPE = IGRSURF(SURFACE_ID)%TYPE ! type of surface
          IF(SURFACE_TYPE /= 200 .AND. SURFACE_TYPE /= 101) THEN
            DO J=1,4
              DO K=1,SURFACE_NODE_NUMBER
                NODE_ID = IGRSURF(SURFACE_ID)%NODES(K,J)
                MIN_MAX_POSITION(1:3) = MIN(MIN_MAX_POSITION(1:3),X(1:3,NODE_ID))
                MIN_MAX_POSITION(4:6) = MAX(MIN_MAX_POSITION(4:6),X(1:3,NODE_ID))
              ENDDO
            ENDDO
          ENDIF
        ENDDO
        ! --------------------------

        RETURN
        END SUBROUTINE SURFACE_MIN_MAX_COMPUTATION
