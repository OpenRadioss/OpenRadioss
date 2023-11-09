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
Chd|  WRITE_MAT_TABLE               source/materials/tools/write_mat_table.F
Chd|-- called by -----------
Chd|        WRITE_FAILPARAM               source/materials/fail/write_failparam.F
Chd|        WRITE_MATPARAM                source/materials/mat/write_matparam.F
Chd|        WRITE_VISCPARAM               source/materials/mat/write_viscparam.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        TABLE4D_MOD                   ../common_source/modules/table4d_mod.F
Chd|====================================================================
      SUBROUTINE WRITE_MAT_TABLE(TABLE, NUMTABL, LENI,  LENR)
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
      INTEGER ,INTENT(INOUT) :: LENI,LENR
      TYPE(TABLE_4D_) ,INTENT(IN) :: TABLE(NUMTABL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,M,N,LEN,NPT,NDIM
      INTEGER LDIM(4)
      my_real, DIMENSION(:) ,ALLOCATABLE :: YTMP
c===============================================
      LENI = 0
      LENR = 0
      DO N=1,NUMTABL
        LEN  = 1
        CALL WRITE_I_C(TABLE(N)%NOTABLE,LEN)
        LENI = LENI + LEN
        NDIM = TABLE(N)%NDIM
        CALL WRITE_I_C(NDIM,LEN)
        LENI = LENI + LEN
        DO I=1,NDIM
          NPT = SIZE(TABLE(N)%X(I)%VALUES)
          CALL WRITE_I_C(NPT , LEN)
          LENI = LENI + LEN
          CALL WRITE_DB(TABLE(N)%X(I)%VALUES,NPT)
          LENR = LENR + NPT
          LDIM(I) = NPT 
        END DO
c
        NPT = LDIM(1)
        IF (NDIM == 1) THEN
          CALL WRITE_DB(TABLE(N)%Y1D,NPT)
          LEN = NPT
        ELSE IF (NDIM == 2) THEN
          LEN = LDIM(1)*LDIM(2)
          ALLOCATE(YTMP(LEN))
          M = 0
          DO J=1,LDIM(2)
            DO I=1,NPT
              M = M+1
              YTMP(M) = TABLE(N)%Y2D(I,J) ! YTMP(M) = YTMP((J-1)*NPT + I)
            END DO
          END DO
          CALL WRITE_DB(YTMP, LEN)
        ELSE IF (NDIM == 3) THEN
          LEN = LDIM(1)*LDIM(2)*LDIM(3)
          ALLOCATE(YTMP(LEN))
          M = 0
          DO K=1,LDIM(3)
            DO J=1,LDIM(2)
              DO I=1,NPT
                M = M+1
                YTMP(M) = TABLE(N)%Y3D(I,J,K)
              END DO
            END DO
          END DO
          CALL WRITE_DB(YTMP, LEN)
        ELSE IF (NDIM == 4) THEN
          LEN = LDIM(1)*LDIM(2)*LDIM(3)*LDIM(4)
          ALLOCATE(YTMP(LEN))
          M = 0
          DO L=1,LDIM(4)
            DO K=1,LDIM(3)
              DO J=1,LDIM(2)
                DO I=1,NPT
                  M = M+1
                  YTMP(M) = TABLE(N)%Y4D(I,J,K,L)
                END DO
              END DO
            END DO
          END DO
          CALL WRITE_DB(YTMP, LEN)
        END IF
        IF (ALLOCATED(YTMP)) DEALLOCATE(YTMP)
        LENR = LENR + LEN
      END DO
c-----------
      RETURN
      END SUBROUTINE WRITE_MAT_TABLE
