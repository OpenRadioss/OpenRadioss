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
Chd|  READ_ELGROUP_PARAM            source/output/restart/read_elgroup_param.F
Chd|-- called by -----------
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        GROUP_PARAM_MOD               ../common_source/modules/mat_elem/group_param_mod.F
Chd|====================================================================
      SUBROUTINE READ_ELGROUP_PARAM(MAT_ELEM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MAT_ELEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (MAT_ELEM_) ,INTENT(INOUT) :: MAT_ELEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG,NGROUP,LEN
      my_real, DIMENSION(:,:), ALLOCATABLE :: RBUF
C=======================================================================
c     allocation of element group parameters
      NGROUP = MAT_ELEM%NGROUP
      ALLOCATE (MAT_ELEM%GROUP_PARAM(NGROUP) )

      LEN = 2
      ALLOCATE(RBUF(NGROUP,LEN))
      CALL READ_DB(RBUF, LEN*NGROUP)
c
      DO NG = 1, NGROUP
        MAT_ELEM%GROUP_PARAM(NG)%VISC_DM = RBUF(NG,1)
        MAT_ELEM%GROUP_PARAM(NG)%VISC_DN = RBUF(NG,2)
      ENDDO
c
      DEALLOCATE( RBUF )
c-----------
      RETURN
      END
