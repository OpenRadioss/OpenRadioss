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
Chd|  C_IRBE3                       source/restart/ddsplit/c_irbe3.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE C_IRBE3(IRBE3,LRBE3,NSKYRBE3_L,PROC  ,
     .                   NRBE3_L,SLMN_L ,NBDDRBE3M)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PROC, NSKYRBE3_L,NRBE3_L,SLMN_L,NBDDRBE3M,
     .        IRBE3(NRBE3L,*), LRBE3(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, NSN, ILP, N,P,M
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGI
C-----------------------------------------------
!     allocate 1d array
      ALLOCATE( TAGI(NUMNOD) )
! ----------------------------
C
      SLMN_L = 0
      DO I = 1, NRBE3
        NSN = IRBE3(5,I)
        K = IRBE3(1,I)
        M = IRBE3(3,I)
       ILP = 0
       DO J = 1, NSN
          N = LRBE3(K+J)
          IF (NLOCAL(N,PROC+1)==1)THEN
            ILP = ILP + 1
          ENDIF
       ENDDO
C--------------pour exchange,meme sans node secnd----
       IF (NLOCAL(M,PROC+1)==1.OR.ILP>0)THEN
        NRBE3_L = NRBE3_L+1
       END IF
       SLMN_L = SLMN_L + ILP
      ENDDO
C
      IF(IPARI0/=0) NSKYRBE3_L = SLMN_L*0
C-------------pour main nodes---------
      DO I = 1, NUMNOD
        TAGI(I) = 0
      END DO
      DO I = 1, SLRBE3/2
          N = LRBE3(I)
            IF(NLOCAL(N,PROC+1)==1) THEN
              IF(TAGI(N)==0) THEN
C decompte des frontieres de main nodes
                DO P = 1, NSPMD
                  IF(NLOCAL(N,P)==1)THEN
                    NBDDRBE3M = NBDDRBE3M + 1
                  END IF
                END DO
C on ne se compte pas soi-meme
                NBDDRBE3M = NBDDRBE3M - 1
C pour ne pas prendre en compte 2 fois des noeuds main de RBE3 differentes ds les frontieres
                TAGI(N) = 1
              END IF
            ENDIF
      END DO
C
! ----------------------------
!     deallocate 1d array
      DEALLOCATE( TAGI )
! ----------------------------
      RETURN
      END
