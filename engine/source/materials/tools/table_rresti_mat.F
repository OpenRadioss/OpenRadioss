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
Chd|  TABLE_RRESTI_MAT              source/materials/tools/table_rresti_mat.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE4D_MOD                   ../common_source/modules/table4d_mod.F
Chd|====================================================================
      SUBROUTINE TABLE_RRESTI_MAT(TABLE, NUMTABL)
C-----------------------------------------------
      USE TABLE4D_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER         ,INTENT(IN)    :: NUMTABL
      TYPE(TABLE_4D_) ,INTENT(INOUT) :: TABLE(NUMTABL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, K, STAT, NDIM
      INTEGER NXK(4)
c===============================================
      DO N=1,NUMTABL
        LEN = 1
        CALL READ_I_C(TABLE(N)%NOTABLE,LEN)
        LEN = 1
        IF (TABLE(N)%NOTABLE > 0) THEN
          CALL READ_I_C(NDIM,LEN)
          TABLE(N)%NDIM = NDIM
          ALLOCATE(TABLE(N)%X(NDIM),STAT=stat)
          DO K=1,NDIM
            CALL READ_I_C(NXK(K),LEN)
            ALLOCATE(TABLE(N)%X(K)%VALUES(NXK(K)),STAT=stat)
          END DO
          IF (NDIM == 1) THEN
            ALLOCATE(TABLE(N)%Y1D(NXK(1)),STAT=stat)
          ELSE IF (NDIM == 2) THEN
            ALLOCATE(TABLE(N)%Y2D(NXK(1),NXK(2)),STAT=stat)
          ELSE IF (NDIM == 3) THEN
            ALLOCATE(TABLE(N)%Y3D(NXK(1),NXK(2),NXK(3)),STAT=stat)
          ELSE IF (NDIM == 4) THEN
            ALLOCATE(TABLE(N)%Y4D(NXK(1),NXK(2),NXK(3),NXK(4)),STAT=stat)
          END IF          
        END IF
      END DO
c-----------
      RETURN
      END SUBROUTINE TABLE_RRESTI_MAT
