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
Chd|  KINE_SEATBELT_FORCE           source/tools/seatbelts/kine_seatbelt_force.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        SPMD_EXCH_A_SEATBELT          source/mpi/seatbelts/spmd_exch_a_seatbelt.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|====================================================================
      SUBROUTINE KINE_SEATBELT_FORCE(A,STIFN,FLAG_SLIPRING_UPDATE,FLAG_RETRACTOR_UPDATE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE SEATBELT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FLAG_SLIPRING_UPDATE,FLAG_RETRACTOR_UPDATE
      my_real A(3,*),STIFN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,L,NODE2,ANCHOR_NODE
      my_real BUF_EXCH(N_ANCHOR_REMOTE_SEND,4)
C---------------------------------------------------------
C
      L = 1
C
C----------------------------------------------------------
C-    KINEMATIC CONDITION OF SLIPRING - FORCE TRANSFER
C----------------------------------------------------------      
C

      DO I=1,NSLIPRING
        DO J=1,SLIPRING(I)%NFRAM
          IF (ABS(SLIPRING(I)%FRAM(J)%UPDATE)==2) FLAG_SLIPRING_UPDATE = FLAG_SLIPRING_UPDATE + 2
          ANCHOR_NODE = SLIPRING(I)%FRAM(J)%ANCHOR_NODE
          NODE2 = SLIPRING(I)%FRAM(J)%NODE(2)
          A(1,ANCHOR_NODE)=A(1,ANCHOR_NODE)+A(1,NODE2)
          A(2,ANCHOR_NODE)=A(2,ANCHOR_NODE)+A(2,NODE2)
          A(3,ANCHOR_NODE)=A(3,ANCHOR_NODE)+A(3,NODE2)
          STIFN(ANCHOR_NODE)=STIFN(ANCHOR_NODE)+STIFN(NODE2) 
          IF (SLIPRING(I)%FRAM(J)%N_REMOTE_PROC > 0) THEN
C--         Contribution stored to be exchanged to remote anchor node
            BUF_EXCH(L,1) = A(1,NODE2)
            BUF_EXCH(L,2) = A(2,NODE2)
            BUF_EXCH(L,3) = A(3,NODE2)
            BUF_EXCH(L,4) = STIFN(NODE2)
            L = L + 1
          ENDIF
          A(1,NODE2)=ZERO
          A(2,NODE2)=ZERO
          A(3,NODE2)=ZERO
        ENDDO     
      ENDDO

C----------------------------------------------------------
C-    KINEMATIC CONDITION OF RETRACTOR - FORCE TRANSFER
C----------------------------------------------------------

      DO I=1,NRETRACTOR
         IF (ABS(RETRACTOR(I)%UPDATE)==2) FLAG_RETRACTOR_UPDATE = FLAG_RETRACTOR_UPDATE + 2
         ANCHOR_NODE = RETRACTOR(I)%ANCHOR_NODE
         NODE2 = RETRACTOR(I)%NODE(2)
         A(1,ANCHOR_NODE)=A(1,ANCHOR_NODE)+A(1,NODE2)
         A(2,ANCHOR_NODE)=A(2,ANCHOR_NODE)+A(2,NODE2)
         A(3,ANCHOR_NODE)=A(3,ANCHOR_NODE)+A(3,NODE2)
         STIFN(ANCHOR_NODE)=STIFN(ANCHOR_NODE)+STIFN(NODE2)  
         IF (RETRACTOR(I)%N_REMOTE_PROC > 0) THEN
C--        Contribution stored to be exchanged to remote anchor node
            BUF_EXCH(1,L) = A(1,NODE2)
            BUF_EXCH(2,L) = A(2,NODE2)
            BUF_EXCH(3,L) = A(3,NODE2)
            BUF_EXCH(4,L) = STIFN(NODE2)
            L = L + 1
         ENDIF
         A(1,NODE2)=ZERO
         A(2,NODE2)=ZERO
         A(3,NODE2)=ZERO    
      ENDDO

C----------------------------------------------------------
C-    EXCHANGE OF CONTRIBUTION FOR REMOTE ANCHOR NODES
C----------------------------------------------------------
C
C--   PARITH/ON is ensured because anchor node can only be on one slipring or retractor
C
      IF (NSPMD > 1) THEN
        CALL SPMD_EXCH_A_SEATBELT(A,STIFN,BUF_EXCH)
      ENDIF

C----------------------------------------------------------
C

C----------------------------------------------------------      
C
      RETURN
                
      END
