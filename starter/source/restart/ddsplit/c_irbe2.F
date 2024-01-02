Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  C_IRBE2                       source/restart/ddsplit/c_irbe2.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        PLIST_IFRONT                  source/spmd/node/ddtools.F    
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE C_IRBE2(IRBE2,LRBE2,PROC  ,NRBE2_L,
     .                   SLMN_L ,NBDDRBE2)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PROC, NRBE2_L,SLMN_L,NBDDRBE2,
     .        IRBE2(NRBE2L,*), LRBE2(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, NSN, ILP, N,P,M,NS,PROC1,SPLIST,SLMN_L0
      INTEGER,DIMENSION(:), ALLOCATABLE :: MAIN_NODES,TAG,PLIST

C-----------------------------------------------
      ALLOCATE(MAIN_NODES(NRBE2))
      ALLOCATE(TAG(NUMNOD))
      ALLOCATE(PLIST(NSPMD))
! ------------------------
C
C
C NRBE2_L : Same initialization done in ddsplit
C           but cleaner to have it here once again.
C
      NRBE2_L = 0
C
      PROC1 = PROC + 1
      SLMN_L = 0
      NBDDRBE2 = 0
      DO I = 1, NRBE2
        NSN = IRBE2(5,I)
        K = IRBE2(1,I)
        M = IRBE2(3,I)
C----- only remove zero NSN (global) one to work the output(H3d)
        IF (NSN==0) CYCLE
        DO J = 1, NSN
         NS = LRBE2(K+J)
         IF(NLOCAL(NS,PROC1)==1) SLMN_L = SLMN_L + 1
        ENDDO
C-----remove local nsn_l=0 will get spmd issue in engine (M in front)
        IF (NLOCAL(M,PROC1)==1)THEN
          NRBE2_L = NRBE2_L+1
C Stacking Main nodes.
          MAIN_NODES(NRBE2_L)=M
        END IF
      ENDDO
C
C Memory optimization - avoid NSPMD*NUMNOD array.
C Fill a list with all RBE2 Main present on one domain
C The issue, Main Nodes can appear twice, but one wants to 
C have it only one time.
C One NUMNOD array is acceptable, other treatments in DDSPLIT take more.
C
      TAG(1:NUMNOD)=0
C
      DO I=1,NRBE2_L
         M=MAIN_NODES(I)

         IF(TAG(M)==0)THEN
           TAG(M)=1

C          List of All domains where M is sticked
           CALL PLIST_IFRONT(PLIST,M,SPLIST)
           DO P=1,SPLIST
              IF( PLIST(P) /= PROC1)THEN
                 NBDDRBE2 = NBDDRBE2 + 1
              ENDIF
           ENDDO
         ENDIF
      ENDDO 
C --------------------------------
      DEALLOCATE(MAIN_NODES)
      DEALLOCATE(TAG)
C --------------------------------
      RETURN
      END
