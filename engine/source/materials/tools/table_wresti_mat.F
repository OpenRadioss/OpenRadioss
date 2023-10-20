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
Chd|  TABLE_WRESTI_MAT              source/tools/curve/table_tools.F
Chd|-- called by -----------
Chd|        WRITE_FAILPARAM               source/materials/fail/write_failparam.F
Chd|        WRITE_MATPARAM                source/materials/mat/write_matparam.F
Chd|        WRITE_VISCPARAM               source/materials/mat/write_viscparam.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE TABLE_WRESTI_MAT(TABLE, NUMTABL)
C-----------------------------------------------
      USE TABLE4D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER         ,INTENT(IN)    :: NUMTABL
      TYPE(TABLE_4D_) ,INTENT(IN)    :: TABLE(NUMTABL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, K, NDIM
c=======================================================================
      LEN = 1
      DO N=1,NUMTABL
        CALL WRITE_I_C(TABLE(N)%NOTABLE,LEN)
        IF (TABLE(N)%NOTABLE > 0) THEN
          NDIM = TABLE(N)%NDIM
          CALL WRITE_I_C(NDIM,LEN)
          DO K=1,NDIM
            CALL WRITE_I_C( SIZE(TABLE(N)%X(K)%VALUES) ,LEN)
          END DO
        END IF
      END DO
c-----------
      RETURN
      END SUBROUTINE TABLE_WRESTI_MAT
