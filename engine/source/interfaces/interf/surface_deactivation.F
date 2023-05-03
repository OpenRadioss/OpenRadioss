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
Chd|  SURFACE_DEACTIVATION          source/interfaces/interf/surface_deactivation.F
Chd|-- called by -----------
Chd|        CHECK_REMOTE_SURFACE_STATE    source/interfaces/interf/check_remote_surface_state.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE SURFACE_DEACTIVATION(NRTM,GLOCAL_SURFACE_ID,MSEGLO,MVOISIN)
!$COMMENT
!       SURFACE_DEACTIVATION description
!           deactivation of neighbour surface
!       SURFACE_DEACTIVATION organization
!           - for a given K deactivated surface :
!              loop over the surfaces to deactivate the neighbour K
!$ENDCOMMENT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"

C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------

C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: NRTM ! number of surface
        INTEGER, INTENT(in) :: GLOCAL_SURFACE_ID ! global id of the surface
        INTEGER, DIMENSION(NRTM), INTENT(in) :: MSEGLO ! global id of the k surface 
        INTEGER, DIMENSION(4,NRTM), INTENT(inout) :: MVOISIN ! neighbouring array
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: J
C-----------------------------------------------
        ! ----------------------
        DO J=1,NRTM
            ! --------------
            ! deactivation of neighbouring 
            IF(MVOISIN(1,J)==GLOCAL_SURFACE_ID) MVOISIN(1,J) = 0
            IF(MVOISIN(2,J)==GLOCAL_SURFACE_ID) MVOISIN(2,J) = 0
            IF(MVOISIN(3,J)==GLOCAL_SURFACE_ID) MVOISIN(3,J) = 0
            IF(MVOISIN(4,J)==GLOCAL_SURFACE_ID) MVOISIN(4,J) = 0
            ! --------------
                
            ! --------------
            ! deactivation of the surface SURFACE_ID
            IF(MSEGLO(J)==GLOCAL_SURFACE_ID) THEN
                MVOISIN(1,J) = 0
                MVOISIN(2,J) = 0
                MVOISIN(3,J) = 0
                MVOISIN(4,J) = 0
            ENDIF
            ! --------------
        ENDDO
        ! ----------------------
        RETURN
        END SUBROUTINE SURFACE_DEACTIVATION
