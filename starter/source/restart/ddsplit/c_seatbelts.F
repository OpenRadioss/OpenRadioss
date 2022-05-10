Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  C_SEATBELTS                   source/restart/ddsplit/c_seatbelts.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|====================================================================
      SUBROUTINE C_SEATBELTS(N_SLIPRING_L,N_RETRACTOR_L,P,NODLOCAL,ELBUF_TAB,
     .                       IPARG,N_ANCHOR_REMOTE_L,N_ANCHOR_REMOTE_SEND_L,ANCHOR_REMOTE_L,ANCHOR_REMOTE_SEND_L,
     .                       N_SEATBELT_L,N_SEATBELT_2D_L,CEP,OFF)     
C---------------------------------------------     
      USE SEATBELT_MOD
      USE ELBUFDEF_MOD     
C---------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N_SLIPRING_L,N_RETRACTOR_L,P,IPARG(NPARG,*),NODLOCAL(*),N_ANCHOR_REMOTE_L,
     .        N_ANCHOR_REMOTE_SEND_L,N_SEATBELT_L,N_SEATBELT_2D_L,CEP(*),OFF
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP)  :: ELBUF_TAB
      TYPE(SEATBELT_REMOTE_NODES_STRUCT) ANCHOR_REMOTE_L,ANCHOR_REMOTE_SEND_L
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NN,PROC,ITY,MTN,NEL,NG,LOCAL_SLIP(NSLIPRING),LOCAL_RETR(NRETRACTOR),PP
     .        N_ANCHOR_REMOTE_SEND,PP,N_REC_PROC(NSPMD),N_SEND_PROC(NSPMD),COMPT,TAG_PROC(2,NSPMD)
      TYPE(G_BUFEL_)     , POINTER :: GBUF
C-----------------------------------------------
C
      LOCAL_SLIP = 0
      LOCAL_RETR = 0
      N_REC_PROC = 0
      N_SEND_PROC = 0
C
C---  Pass 1 counting ----------------------------------------------------------
C
      DO I = 1, N_SEATBELT
        IF (SEATBELT_TAB(I)%NSPRING > 0) THEN
          IF (CEP(OFF + SEATBELT_TAB(I)%SPRING(1)) == P - 1) THEN
            N_SEATBELT_L = N_SEATBELT_L + 1
            IF (SEATBELT_TAB(I)%NFRAM > 1) N_SEATBELT_2D_L = N_SEATBELT_2D_L + 1
          ENDIF
        ENDIF
      ENDDO
C
      DO I = 1, NSLIPRING   
        SLIPRING(I)%IDG = I
C
        IF (NLOCAL(SLIPRING(I)%FRAM(1)%NODE(2),P)==1) THEN
C--       Proc master of slipring
          N_SLIPRING_L = N_SLIPRING_L + 1
          LOCAL_SLIP(I) = N_SLIPRING_L
        ENDIF 
C
        DO J=1,SLIPRING(I)%NFRAM           
          IF (NLOCAL(SLIPRING(I)%FRAM(J)%NODE(2),P)==1) THEN
            DO PP=1,NSPMD 
              IF ((PP /= P).AND.(NLOCAL(SLIPRING(I)%FRAM(J)%ANCHOR_NODE,PP)==1)) THEN
C--             Count of remote anchor node (send)
                N_ANCHOR_REMOTE_SEND_L = N_ANCHOR_REMOTE_SEND_L + 1
                SLIPRING(I)%FRAM(J)%N_REMOTE_PROC = SLIPRING(I)%FRAM(J)%N_REMOTE_PROC + 1
                N_SEND_PROC(PP) = N_SEND_PROC(PP) + 1
              ENDIF
            ENDDO         
          ELSEIF (NLOCAL(SLIPRING(I)%FRAM(J)%ANCHOR_NODE,P)==1) THEN
C--         Count of remote anchor node (receve)
            N_ANCHOR_REMOTE_L = N_ANCHOR_REMOTE_L + 1
            COMPT = 0
            DO PP=1,NSPMD 
              IF ((PP /= P).AND.(NLOCAL(SLIPRING(I)%FRAM(J)%NODE(2),PP)==1)) THEN
C--             Count of remote anchor node per master proc
                N_REC_PROC(PP) = N_REC_PROC(PP) + 1
              ENDIF
            ENDDO
          ENDIF
        ENDDO
C
      ENDDO
C
      DO I = 1, NRETRACTOR
        RETRACTOR(I)%IDG = I 
        IF (NLOCAL(RETRACTOR(I)%NODE(2),P)==1) THEN
C--       Proc master of retractor
          N_RETRACTOR_L = N_RETRACTOR_L + 1
          LOCAL_RETR(I) = N_RETRACTOR_L
          DO PP=1,NSPMD 
            IF ((PP /= P).AND.(NLOCAL(RETRACTOR(I)%ANCHOR_NODE,PP)==1)) THEN
C--           remote anchor node (send)
              N_ANCHOR_REMOTE_SEND_L = N_ANCHOR_REMOTE_SEND_L + 1
              RETRACTOR(I)%N_REMOTE_PROC = RETRACTOR(I)%N_REMOTE_PROC + 1
              N_SEND_PROC(PP) = N_SEND_PROC(PP) + 1
            ENDIF
          ENDDO
        ELSEIF (NLOCAL(RETRACTOR(I)%ANCHOR_NODE,P)==1) THEN
C--       Remote anchor node
          N_ANCHOR_REMOTE_L = N_ANCHOR_REMOTE_L + 1
          DO PP=1,NSPMD 
            IF ((PP /= P).AND.(NLOCAL(RETRACTOR(I)%NODE(2),PP)==1)) THEN
C--           Count of remote anchor node per master proc
              N_REC_PROC(PP) = N_REC_PROC(PP) + 1
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
C---  Pass 2 : construction of arrays for remote anchor nodes-------------------------
C
      ALLOCATE(ANCHOR_REMOTE_L%ADD_PROC(NSPMD+1))
      ALLOCATE(ANCHOR_REMOTE_L%NODE(N_ANCHOR_REMOTE_L))
      COMPT = 0
C
      ALLOCATE(ANCHOR_REMOTE_SEND_L%ADD_PROC(NSPMD+1))
      ALLOCATE(ANCHOR_REMOTE_SEND_L%NODE(N_ANCHOR_REMOTE_SEND_L))
C
      IF (N_ANCHOR_REMOTE_L > 0) THEN
        ANCHOR_REMOTE_L%ADD_PROC(1) = 1
        DO PP=1,NSPMD
          ANCHOR_REMOTE_L%ADD_PROC(PP+1) = ANCHOR_REMOTE_L%ADD_PROC(PP) + N_REC_PROC(PP)
        ENDDO
      ENDIF
C
      IF (N_ANCHOR_REMOTE_SEND_L > 0) THEN
        ANCHOR_REMOTE_SEND_L%ADD_PROC(1) = 1
        DO PP=1,NSPMD
          ANCHOR_REMOTE_SEND_L%ADD_PROC(PP+1) = ANCHOR_REMOTE_SEND_L%ADD_PROC(PP) + N_SEND_PROC(PP)
        ENDDO
        N_SEND_PROC = 0
      ENDIF         
C
      NN = 0
C
      DO I = 1, NSLIPRING   
        DO J=1,SLIPRING(I)%NFRAM           
          IF (NLOCAL(SLIPRING(I)%FRAM(J)%NODE(2),P)==1) THEN
            IF (SLIPRING(I)%FRAM(J)%N_REMOTE_PROC > 0) THEN
C--           Proc master of slipring
              NN = NN + 1        
              DO PP=1,NSPMD 
                IF ((PP /= P).AND.(NLOCAL(SLIPRING(I)%FRAM(J)%ANCHOR_NODE,PP)==1)) THEN
C--               remote anchor node (send)
                  ANCHOR_REMOTE_SEND_L%NODE(ANCHOR_REMOTE_SEND_L%ADD_PROC(PP)+N_SEND_PROC(PP)) = NN
                  N_SEND_PROC(PP) = N_SEND_PROC(PP) + 1              
                ENDIF
              ENDDO
            ENDIF
          ELSEIF (NLOCAL(SLIPRING(I)%FRAM(J)%ANCHOR_NODE,P)==1) THEN
C--         Proc remote of slipring
            COMPT = COMPT + 1
            ANCHOR_REMOTE_L%NODE(COMPT) = NODLOCAL(SLIPRING(I)%FRAM(J)%ANCHOR_NODE)
          ENDIF
        ENDDO
      ENDDO 
C
      DO I = 1, NRETRACTOR                
        IF (NLOCAL(RETRACTOR(I)%NODE(2),P)==1) THEN
          IF (RETRACTOR(I)%N_REMOTE_PROC > 0) THEN
C--         Proc master of retractor
            NN = NN + 1        
            DO PP=1,NSPMD 
              IF ((PP /= P).AND.(NLOCAL(RETRACTOR(I)%ANCHOR_NODE,PP)==1)) THEN
C--             remote anchor node (send)
                ANCHOR_REMOTE_SEND_L%NODE(ANCHOR_REMOTE_SEND_L%ADD_PROC(PP)+N_SEND_PROC(PP)) = NN
                N_SEND_PROC(PP) = N_SEND_PROC(PP) + 1              
              ENDIF
            ENDDO
          ENDIF
        ELSEIF (NLOCAL(RETRACTOR(I)%ANCHOR_NODE,P)==1) THEN
C--       Proc remote of slipring
          COMPT = COMPT + 1
          ANCHOR_REMOTE_L%NODE(COMPT) = NODLOCAL(RETRACTOR(I)%ANCHOR_NODE)
        ENDIF
      ENDDO
C
C---  COnstruction of arrays for spmd exchange of TH on proc 0
      IF ((NSPMD > 1).AND.(P == 1)) THEN
C
        NSEATBELT_TH_PROC = 0
        TAG_PROC = 0
C
        DO PP=1,NSPMD 
          DO I=1,NSLIPRING
            IF ((PP /= P).AND.(NLOCAL(SLIPRING(I)%FRAM(1)%NODE(2),PP)==1)) TAG_PROC(1,PP) = 1
          ENDDO
          DO I=1,NRETRACTOR
            IF ((PP /= P).AND.(NLOCAL(RETRACTOR(I)%NODE(2),PP)==1)) TAG_PROC(2,PP) = 1
          ENDDO
          IF (TAG_PROC(1,PP) + TAG_PROC(2,PP) > 0) NSEATBELT_TH_PROC = NSEATBELT_TH_PROC + 1
        ENDDO
C
        ALLOCATE(SEATBELT_TH_EXCH(NSEATBELT_TH_PROC+1))
C
        COMPT = 0
        SEATBELT_TH_EXCH(1)%ADD_PROC = 1
        DO PP=1,NSPMD 
          IF (TAG_PROC(1,PP) + TAG_PROC(2,PP) > 0) THEN
            COMPT = COMPT + 1
C--         data must be received from this proc for TH
            SEATBELT_TH_EXCH(COMPT)%ID_PROC = PP
            SEATBELT_TH_EXCH(COMPT)%NSLIPRING = 0
            SEATBELT_TH_EXCH(COMPT)%NRETRACTOR = 0
            DO I=1,NSLIPRING
              IF ((PP /= P).AND.(NLOCAL(SLIPRING(I)%FRAM(1)%NODE(2),PP)==1)) THEN
                SEATBELT_TH_EXCH(COMPT)%NSLIPRING = SEATBELT_TH_EXCH(COMPT)%NSLIPRING + 1
                SEATBELT_TH_EXCH(COMPT+1)%ADD_PROC = SEATBELT_TH_EXCH(COMPT)%ADD_PROC + 6
              ENDIF
            ENDDO
            DO I=1,NRETRACTOR
              IF ((PP /= P).AND.(NLOCAL(RETRACTOR(I)%NODE(2),PP)==1)) THEN
                SEATBELT_TH_EXCH(COMPT)%NRETRACTOR = SEATBELT_TH_EXCH(COMPT)%NRETRACTOR + 1
                SEATBELT_TH_EXCH(COMPT+1)%ADD_PROC = SEATBELT_TH_EXCH(COMPT)%ADD_PROC + 3
              ENDIF
            ENDDO
          ENDIF
        ENDDO
C
      ENDIF

C
C---  Update of add_node in buffer element ----------------------------------------------

      IF (N_SLIPRING_L + N_RETRACTOR_L > 0) THEN
C
        DO NG=1,NGROUP
C
          PROC  = IPARG(32,NG)
          ITY   = IPARG(5,NG)
          MTN   = IPARG(1,NG)
          NEL = IPARG(2,NG)
c----
          IF ((P == PROC + 1).AND.(ITY==6).AND.(MTN==114)) THEN
            GBUF => ELBUF_TAB(NG)%GBUF
            DO I=1,NEL
              IF (GBUF%ADD_NODE(I) > 0) GBUF%ADD_NODE(I) = NODLOCAL(GBUF%ADD_NODE(I))
              IF (GBUF%ADD_NODE(NEL+I) > 0) GBUF%ADD_NODE(NEL+I) = NODLOCAL(GBUF%ADD_NODE(NEL+I))
              IF (GBUF%SLIPRING_ID(I) > 0) GBUF%SLIPRING_ID(I) = LOCAL_SLIP(GBUF%SLIPRING_ID(I))
              IF (GBUF%RETRACTOR_ID(I) > 0) GBUF%RETRACTOR_ID(I) = LOCAL_RETR(GBUF%RETRACTOR_ID(I))
            ENDDO
          ENDIF
C
        ENDDO
C
      ENDIF   
C                
! --------------------------------------
      RETURN
      END
