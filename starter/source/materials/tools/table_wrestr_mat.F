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
Chd|  TABLE_WRESTR_MAT              source/materials/tools/table_wrestr_mat.F
Chd|-- called by -----------
Chd|        WRITE_FAILPARAM               source/materials/fail/write_failparam.F
Chd|        WRITE_MATPARAM                source/materials/mat/write_matparam.F
Chd|        WRITE_VISCPARAM               source/materials/mat/write_viscparam.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        TABLE4D_MOD                   ../common_source/modules/table4d_mod.F
Chd|====================================================================
      SUBROUTINE TABLE_WRESTR_MAT(TABLE, NUMTABL, LENR)
C-----------------------------------------------
      USE TABLE4D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN)    :: NUMTABL
      INTEGER ,INTENT(INOUT) :: LENR
      TYPE(TABLE_4D_) ,DIMENSION(NUMTABL) ,INTENT(IN) :: TABLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, K, NDIM
      INTEGER NXK(4)
c===============================================
      LENR=0
      DO N=1,NUMTABL
        IF (TABLE(N)%NOTABLE > 0) THEN
          NDIM = TABLE(N)%NDIM
          DO K=1,NDIM
            NXK(K) = SIZE(TABLE(N)%X(K)%VALUES)
            LEN = NXK(K)
            CALL WRITE_DB(TABLE(N)%X(K)%VALUES,LEN)
            LENR =  LENR + LEN
          END DO
          IF (NDIM == 1) THEN
            LEN = SIZE( TABLE(N)%Y1D)
            CALL WRITE_DB(TABLE(N)%Y1D,LEN)
          ELSE IF (NDIM == 2) THEN
            LEN = SIZE( TABLE(N)%Y2D)
            CALL WRITE_DB(TABLE(N)%Y2D,LEN)
          ELSE IF (NDIM == 3) THEN
            LEN = SIZE( TABLE(N)%Y3D)
            CALL WRITE_DB(TABLE(N)%Y3D,LEN)
          ELSE IF (NDIM == 4) THEN
            LEN = SIZE( TABLE(N)%Y4D)
            CALL WRITE_DB(TABLE(N)%Y4D,LEN)
          END IF
          LENR=LENR+LEN
        END IF
      END DO
c-----------
      RETURN
      END SUBROUTINE TABLE_WRESTR_MAT
