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
Chd|  W_SEATBELTS                   source/restart/ddsplit/w_seatbelts.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|====================================================================
      SUBROUTINE W_SEATBELTS(LEN_IA,LEN_AM,P,NODLOCAL,N_ANCHOR_REMOTE_L,
     .                       N_ANCHOR_REMOTE_SEND_L,ANCHOR_REMOTE_L,ANCHOR_REMOTE_SEND_L)     
C---------------------------------------------     
      USE SEATBELT_MOD
C---------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN_AM,LEN_IA,P,NODLOCAL(*),N_ANCHOR_REMOTE_L,N_ANCHOR_REMOTE_SEND_L
      TYPE(SEATBELT_REMOTE_NODES_STRUCT) ANCHOR_REMOTE_L,ANCHOR_REMOTE_SEND_L
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,OPTIONAL_NODE
C-----------------------------------------------
C
      DO I = 1, NSLIPRING
C
        IF (NLOCAL(SLIPRING(I)%FRAM(1)%NODE(2),P)==1) THEN
C
          CALL WRITE_I_C(SLIPRING(I)%ID, 1)
          CALL WRITE_I_C(SLIPRING(I)%IDG, 1)
          CALL WRITE_I_C(SLIPRING(I)%NFRAM, 1)
          CALL WRITE_I_C(SLIPRING(I)%IFUNC, 4)
          CALL WRITE_I_C(SLIPRING(I)%SENSID, 1)
          CALL WRITE_I_C(SLIPRING(I)%FL_FLAG, 1)
          CALL WRITE_I_C(SLIPRING(I)%RBODY, 1)
          LEN_IA = LEN_IA + 10
C
          CALL WRITE_DB(SLIPRING(I)%DC, 1)
          CALL WRITE_DB(SLIPRING(I)%A, 1)
          CALL WRITE_DB(SLIPRING(I)%FRIC, 1)
          CALL WRITE_DB(SLIPRING(I)%FAC_D, 3)
          CALL WRITE_DB(SLIPRING(I)%FRICS, 1)
          CALL WRITE_DB(SLIPRING(I)%FAC_S, 3)
          LEN_AM = LEN_AM + 10
C
          DO J=1,SLIPRING(I)%NFRAM
C
            CALL WRITE_I_C(SLIPRING(I)%FRAM(J)%UPDATE, 1)
            CALL WRITE_I_C(NODLOCAL(SLIPRING(I)%FRAM(J)%ANCHOR_NODE), 1)          
            CALL WRITE_I_C(NODLOCAL(SLIPRING(I)%FRAM(J)%NODE(1)), 1)
            CALL WRITE_I_C(NODLOCAL(SLIPRING(I)%FRAM(J)%NODE(2)), 1)
            CALL WRITE_I_C(NODLOCAL(SLIPRING(I)%FRAM(J)%NODE(3)), 1)
            CALL WRITE_I_C(SLIPRING(I)%FRAM(J)%NODE_NEXT, 3)
            CALL WRITE_I_C(SLIPRING(I)%FRAM(J)%NODE2_PREV, 1)
            CALL WRITE_I_C(SLIPRING(I)%FRAM(J)%N_REMOTE_PROC, 1)
C
            OPTIONAL_NODE = 0
            IF (SLIPRING(I)%FRAM(J)%ORIENTATION_NODE > 0) THEN
              OPTIONAL_NODE = NODLOCAL(SLIPRING(I)%FRAM(J)%ORIENTATION_NODE)
            ENDIF
            CALL WRITE_I_C(OPTIONAL_NODE, 1)
C
            CALL WRITE_I_C(SLIPRING(I)%FRAM(J)%STRAND_DIRECTION, 2)
            CALL WRITE_I_C(SLIPRING(I)%FRAM(J)%LOCKED, 1)
            LEN_IA = LEN_IA + 14
C
            CALL WRITE_DB(SLIPRING(I)%FRAM(J)%VECTOR, 6)
            CALL WRITE_DB(SLIPRING(I)%FRAM(J)%ORIENTATION_ANGLE, 1)
            CALL WRITE_DB(SLIPRING(I)%FRAM(J)%MATERIAL_FLOW, 1)
            CALL WRITE_DB(SLIPRING(I)%FRAM(J)%DFS, 1)
            CALL WRITE_DB(SLIPRING(I)%FRAM(J)%RESIDUAL_LENGTH, 2)
            CALL WRITE_DB(SLIPRING(I)%FRAM(J)%CURRENT_LENGTH, 2)
            CALL WRITE_DB(SLIPRING(I)%FRAM(J)%RINGSLIP, 1)
            CALL WRITE_DB(SLIPRING(I)%FRAM(J)%BETA, 1)
            CALL WRITE_DB(SLIPRING(I)%FRAM(J)%SLIP_FORCE, 3)
            CALL WRITE_DB(SLIPRING(I)%FRAM(J)%PREV_REF_LENGTH, 1)
            CALL WRITE_DB(SLIPRING(I)%FRAM(J)%INTVAR_STR1, 8)
            CALL WRITE_DB(SLIPRING(I)%FRAM(J)%INTVAR_STR2, 8)
            LEN_AM = LEN_AM + 35
C
          ENDDO
C
        ENDIF
C
      ENDDO
C
      DO I = 1, NRETRACTOR
C
        IF (NLOCAL(RETRACTOR(I)%NODE(2),P)==1) THEN
          CALL WRITE_I_C(RETRACTOR(I)%ID, 1)
          CALL WRITE_I_C(RETRACTOR(I)%IDG, 1)
          CALL WRITE_I_C(RETRACTOR(I)%UPDATE, 1)
          CALL WRITE_I_C(NODLOCAL(RETRACTOR(I)%ANCHOR_NODE), 1)
          CALL WRITE_I_C(NODLOCAL(RETRACTOR(I)%NODE(1)), 1)
          CALL WRITE_I_C(NODLOCAL(RETRACTOR(I)%NODE(2)), 1)
          CALL WRITE_I_C(RETRACTOR(I)%NODE_NEXT, 2)
          CALL WRITE_I_C(RETRACTOR(I)%STRAND_DIRECTION, 1)
          CALL WRITE_I_C(RETRACTOR(I)%IFUNC, 3)
          CALL WRITE_I_C(RETRACTOR(I)%ISENS, 2)
          CALL WRITE_I_C(RETRACTOR(I)%TENS_TYP, 1)
          CALL WRITE_I_C(RETRACTOR(I)%LOCKED, 1)
          CALL WRITE_I_C(RETRACTOR(I)%PRETENS_ACTIV, 1)
          CALL WRITE_I_C(RETRACTOR(I)%INACTI_NNOD, 1)
          CALL WRITE_I_C(RETRACTOR(I)%INACTI_NNOD_MAX, 1)
          DO K=1,RETRACTOR(I)%INACTI_NNOD
            CALL WRITE_I_C(NODLOCAL(RETRACTOR(I)%INACTI_NODE(K)), 1)
          ENDDO
          CALL WRITE_I_C(RETRACTOR(I)%N_REMOTE_PROC, 1)
          LEN_IA = LEN_IA + 20 + RETRACTOR(I)%INACTI_NNOD
          CALL WRITE_DB(RETRACTOR(I)%VECTOR, 3)
          CALL WRITE_DB(RETRACTOR(I)%ELEMENT_SIZE, 1)
          CALL WRITE_DB(RETRACTOR(I)%FORCE, 1)
          CALL WRITE_DB(RETRACTOR(I)%MATERIAL_FLOW, 1)
          CALL WRITE_DB(RETRACTOR(I)%RESIDUAL_LENGTH, 1)
          CALL WRITE_DB(RETRACTOR(I)%FAC, 4)
          CALL WRITE_DB(RETRACTOR(I)%PULLOUT, 1)
          CALL WRITE_DB(RETRACTOR(I)%UNLOCK_FORCE, 1)
          CALL WRITE_DB(RETRACTOR(I)%LOCK_OFFSET, 1)
          CALL WRITE_DB(RETRACTOR(I)%LOCK_PULL, 1)
          CALL WRITE_DB(RETRACTOR(I)%LOCK_YIELD_FORCE, 1)
          CALL WRITE_DB(RETRACTOR(I)%RINGSLIP, 1)
          CALL WRITE_DB(RETRACTOR(I)%PRETENS_TIME, 1)
          CALL WRITE_DB(RETRACTOR(I)%PRETENS_PULL, 1)
          CALL WRITE_DB(RETRACTOR(I)%PRETENS_PULLMAX, 1)
          CALL WRITE_DB(RETRACTOR(I)%RET_FORCE, 1)
          LEN_AM = LEN_AM + 21
        ENDIF
C
      ENDDO
C
      IF (N_ANCHOR_REMOTE_L > 0) THEN
        CALL WRITE_I_C(ANCHOR_REMOTE_L%ADD_PROC, NSPMD+1)
        CALL WRITE_I_C(ANCHOR_REMOTE_L%NODE, N_ANCHOR_REMOTE_L)
        LEN_IA = LEN_IA + NSPMD + 1 + N_ANCHOR_REMOTE_L     
      ENDIF
C
      IF (N_ANCHOR_REMOTE_SEND_L > 0) THEN
        CALL WRITE_I_C(ANCHOR_REMOTE_SEND_L%ADD_PROC, NSPMD+1)
        CALL WRITE_I_C(ANCHOR_REMOTE_SEND_L%NODE, N_ANCHOR_REMOTE_SEND_L)
        LEN_IA = LEN_IA + NSPMD + 1 + N_ANCHOR_REMOTE_SEND_L     
      ENDIF
C
      IF (P ==1) THEN
        CALL WRITE_I_C(NSEATBELT_TH_PROC, 1)
        LEN_IA = LEN_IA + 1
        IF ((NSPMD > 1).AND.(NSEATBELT_TH_PROC > 0)) THEN
          DO I=1,NSEATBELT_TH_PROC
            CALL WRITE_I_C(SEATBELT_TH_EXCH(I)%ID_PROC, 1)
            CALL WRITE_I_C(SEATBELT_TH_EXCH(I)%ADD_PROC, 1)
            CALL WRITE_I_C(SEATBELT_TH_EXCH(I)%NSLIPRING, 1)
            CALL WRITE_I_C(SEATBELT_TH_EXCH(I)%NRETRACTOR, 1)
            LEN_IA = LEN_IA + 4
          ENDDO
        ENDIF
      ENDIF
C                
! --------------------------------------
      RETURN
      END
