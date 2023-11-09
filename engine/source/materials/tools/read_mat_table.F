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
Chd|  READ_MAT_TABLE                source/materials/tools/read_mat_table.F
Chd|-- called by -----------
Chd|        READ_FAILPARAM                source/output/restart/read_failparam.F
Chd|        READ_MATPARAM                 source/output/restart/read_matparam.F
Chd|        READ_VISCPARAM                source/output/restart/read_viscparam.F
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        TABLE4D_MOD                   ../common_source/modules/table4d_mod.F
Chd|====================================================================
      SUBROUTINE READ_MAT_TABLE(TABLE, NUMTABL)
C----------------------------------------------- 
C   M o d u l e s
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
      TYPE(TABLE_4D_) ,INTENT(INOUT) :: TABLE(NUMTABL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,M,N,LEN,NPT,NDIM,STAT
      INTEGER LDIM(4)
      my_real, DIMENSION(:) ,ALLOCATABLE :: YTMP
c===============================================
      DO N=1,NUMTABL
        LEN = 1
        CALL READ_I_C(TABLE(N)%NOTABLE,LEN)
        LEN = 1
        CALL READ_I_C(NDIM,LEN)
        TABLE(N)%NDIM = NDIM
        ALLOCATE(TABLE(N)%X(NDIM),STAT=stat)
c
        DO K=1,NDIM
          CALL READ_I_C(NPT,LEN)
          ALLOCATE(TABLE(N)%X(K)%VALUES(NPT) ,STAT=stat)
          CALL READ_DB(TABLE(N)%X(K)%VALUES,NPT)
          LDIM(K) = NPT
        END DO
        NPT = LDIM(1)
        IF (NDIM == 1) THEN
          ALLOCATE(TABLE(N)%Y1D(NPT) ,STAT=stat)
          CALL READ_DB(TABLE(N)%Y1D,NPT)
        ELSE IF (NDIM == 2) THEN
          LEN = LDIM(1)*LDIM(2)
          ALLOCATE(YTMP(LEN))
          ALLOCATE(TABLE(N)%Y2D(LDIM(1),LDIM(2)) ,STAT=stat)
          CALL READ_DB(YTMP,LEN)
          M = 0
          DO J=1,LDIM(2)
            DO I=1,NPT
              M = M+1
              TABLE(N)%Y2D(I,J) = YTMP(M)
            END DO
          END DO
        ELSE IF (NDIM == 3) THEN
          LEN = LDIM(1)*LDIM(2)*LDIM(3)
          ALLOCATE(YTMP(LEN))
          ALLOCATE(TABLE(N)%Y3D(LDIM(1),LDIM(2),LDIM(3)),STAT=stat)
          CALL READ_DB(YTMP,LEN)
          M = 0
          DO K=1,LDIM(3)
            DO J=1,LDIM(2)
              DO I=1,NPT
                M = M+1
                TABLE(N)%Y3D(I,J,K) = YTMP(M)
              END DO
            END DO
          END DO
        ELSE IF (NDIM == 4) THEN
          LEN = LDIM(1)*LDIM(2)*LDIM(3)*LDIM(4)
          ALLOCATE(YTMP(LEN))
          ALLOCATE(TABLE(N)%Y4D(LDIM(1),LDIM(2),LDIM(3),LDIM(4)),STAT=stat)
          CALL READ_DB(YTMP,LEN)
          M = 0
          DO L=1,LDIM(4)
            DO K=1,LDIM(3)
              DO J=1,LDIM(2)
                DO I=1,NPT
                  M = M+1
                  TABLE(N)%Y4D(I,J,K,L) = YTMP(M)
                END DO
              END DO
            END DO
          END DO
        END IF          
        IF (ALLOCATED(YTMP)) DEALLOCATE(YTMP)
      END DO
c-----------
      RETURN
      END SUBROUTINE READ_MAT_TABLE
