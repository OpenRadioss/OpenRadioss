Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  INTER_CURV_COMPUTATION        source/interfaces/generic/inter_curv_computation.F
Chd|-- called by -----------
Chd|        INTER_COUNT_NODE_CURV         source/interfaces/generic/inter_count_node_curv.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTER_CURV_COMPUTATION(ICURV,NRTM_T,IRECT,C_MAX_LOCAL,CURV_MAX,X)
!$COMMENT
!       INTER_CURV_COMPUTATION description :
!       
!       INTER_CURV_COMPUTATION :
!$ENDCOMMENT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: ICURV,NRTM_T
        INTEGER, DIMENSION(4,NRTM_T), INTENT(in) :: IRECT
        my_real, INTENT(inout) :: C_MAX_LOCAL
        my_real, DIMENSION(NRTM_T), INTENT(inout) :: CURV_MAX
        my_real, DIMENSION(3,NUMNOD), INTENT(in) :: X
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I
        my_real :: XXX,YYY,ZZZ 
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
        IF(ICURV/=0)THEN
            C_MAX_LOCAL = ZERO
            DO I=1,NRTM_T
                XXX=MAX(X(1,IRECT(1,I)),X(1,IRECT(2,I)),
     .                  X(1,IRECT(3,I)),X(1,IRECT(4,I)))
     .               -MIN(X(1,IRECT(1,I)),X(1,IRECT(2,I)),
     .                  X(1,IRECT(3,I)),X(1,IRECT(4,I)))
                YYY=MAX(X(2,IRECT(1,I)),X(2,IRECT(2,I)),
     .                  X(2,IRECT(3,I)),X(2,IRECT(4,I)))
     .               -MIN(X(2,IRECT(1,I)),X(2,IRECT(2,I)),
     .                  X(2,IRECT(3,I)),X(2,IRECT(4,I)))
                ZZZ=MAX(X(3,IRECT(1,I)),X(3,IRECT(2,I)),
     .                  X(3,IRECT(3,I)),X(3,IRECT(4,I)))
     .               -MIN(X(3,IRECT(1,I)),X(3,IRECT(2,I)),
     .                  X(3,IRECT(3,I)),X(3,IRECT(4,I)))
                CURV_MAX(I) = HALF * MAX(XXX,YYY,ZZZ)
                C_MAX_LOCAL = MAX(C_MAX_LOCAL,CURV_MAX(I))
            ENDDO
        ELSE
            DO I=1,NRTM_T
                CURV_MAX(I)=ZERO
            ENDDO
        ENDIF
C
        RETURN
        END SUBROUTINE INTER_CURV_COMPUTATION
