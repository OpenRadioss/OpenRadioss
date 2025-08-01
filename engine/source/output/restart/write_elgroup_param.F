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
!||    write_elgroup_param   ../engine/source/output/restart/write_elgroup_param.F
!||--- called by ------------------------------------------------------
!||    wrrestp               ../engine/source/output/restart/wrrestp.F
!||--- calls      -----------------------------------------------------
!||    write_db              ../common_source/tools/input_output/write_db.F
!||--- uses       -----------------------------------------------------
!||    group_param_mod       ../common_source/modules/mat_elem/group_param_mod.F90
!||====================================================================
      SUBROUTINE WRITE_ELGROUP_PARAM(GROUP_PARAM_TAB,NGROUP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUP_PARAM_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGROUP
      TYPE(GROUP_PARAM_) ,DIMENSION(NGROUP) :: GROUP_PARAM_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG,LEN
      my_real, DIMENSION(:,:), ALLOCATABLE :: RBUF
C=======================================================================
      LEN = 2
      ALLOCATE(RBUF(NGROUP,LEN))
c
      DO NG = 1, NGROUP
        RBUF(NG,1) = GROUP_PARAM_TAB(NG)%VISC_DM
        RBUF(NG,2) = GROUP_PARAM_TAB(NG)%VISC_DN
      ENDDO
C
      CALL WRITE_DB(RBUF,LEN*NGROUP)
      DEALLOCATE( RBUF )
c-----------
      RETURN
      END

