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
Chd|  I25TRC_EDG                    source/interfaces/intsort/i25trc_edg.F
Chd|-- called by -----------
Chd|        I25MAIN_TRI                   source/interfaces/intsort/i25main_tri.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25TRC_EDG(
     1      NEDGE,I_STOK,CAND_S,CAND_M,
     2      CAND_P,CAND_A,NIN,NEDGE_L,IFQ,
     3      CAND_FX ,CAND_FY,CAND_FZ,IFPEN)
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "assert.inc"
#include      "i25edge_c.inc"
C-----------------------------------------------
C   ROLE DE LA ROUTINE:
C   ===================
C   TRI sur N de CAND_S CAND_M CAND_F
C   et elimination des noeuds en rebond 
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: I_STOK,NEDGE,NIN,NEDGE_L,IFQ
      INTEGER :: CAND_S(I_STOK),CAND_M(I_STOK),CAND_A(*),IFPEN(*)
      my_real
     .        CAND_P(*),CAND_FX(*) ,CAND_FY(*) ,CAND_FZ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I_ST0,N,NN,K,E,CAND_T,
     .        IGET(I_STOK),IPUT(I_STOK)
      my_real
     .        CAND_TF
C=======================================================================
C

      DO N=1,NEDGE+3
        CAND_A(N) = 0
      ENDDO

      DO I=1,I_STOK
        NN = CAND_S(I)
        E  = CAND_M(I)

        ASSERT(CAND_S(I) > 0)
        ASSERT(CAND_S(I) <= NEDGE) ! ici nedge = nedge + nedge_remote


C       IF(NN<=NEDGE_L)THEN
        IF (IFQ == 0) THEN
          IF(CAND_P(I)==ZERO)THEN
            CAND_S(I) = NEDGE+1
          ENDIF
        ELSE
         IF(IFPEN(I)==0.AND.CAND_P(I)==ZERO)THEN
            CAND_S(I) = NEDGE+1
          ENDIF 
        ENDIF
C       ELSE ! remote
C       ENDIF
      ENDDO

C=======================================================================
C     CAND_A : DENOMBREMENT DE CHAQUE NOEUD 
C     APRES 300 CAND_A[3:NEDGE+3] : OCCURENCE DES NOEUDS [1:NEDGE+1] 
C=======================================================================
      DO I=1,I_STOK
        NN = CAND_S(I) + 2
        CAND_A(NN) = CAND_A(NN) + 1
      ENDDO
      
C=======================================================================
C     CAND_A : ADRESSE DE CHAQUE NOEUD 
C     APRES 400 CAND_A[2:NEDGE+2] : ADRESSE DES NOEUDS [1:NEDGE+1] 
C=======================================================================
      CAND_A(1) = 1
      CAND_A(2) = 1
      DO  N=3,NEDGE+2
        CAND_A(N) = CAND_A(N) + CAND_A(N-1)
      ENDDO
C=======================================================================
C     IPUT(I) ADRESSE OU DOIT ALLER I
C     IGET(K) ADRESSE D'OU DOIT VENIR K
C     APRES 500 CAND_A[1:NEDGE+1] : ADRESSE DES NOEUDS [1:NEDGE+1] 
C=======================================================================
      DO I=1,I_STOK
        NN = CAND_S(I) + 1
        K = CAND_A(NN)
        IPUT(I) = K
        IGET(K) = I
        ASSERT(I > 0)
        ASSERT(K > 0)
        ASSERT(NN < NEDGE + 4 )
        CAND_A(NN) = CAND_A(NN) + 1
      ENDDO
C=======================================================================
C     TRI DE CAND_S CAND_M CAND_P
C     SUR N CROISSANT
C     PERMUTATION 1 PASSE
C============================================= 
        DO K=1,I_STOK
          I = IGET(K)
          ASSERT(I > 0)
C
          CAND_T    = CAND_S(K)
          CAND_S(K) = CAND_S(I)
          CAND_S(I) = CAND_T
C
          CAND_T    = CAND_M(K)
          CAND_M(K) = CAND_M(I)
          CAND_M(I) = CAND_T
C
          CAND_TF   = CAND_P(K)
          CAND_P(K) = CAND_P(I)
          CAND_P(I) = CAND_TF
C
          CAND_TF   = CAND_FX(K)
          CAND_FX(K) = CAND_FX(I)
          CAND_FX(I) = CAND_TF
C
          CAND_TF   = CAND_FY(K)
          CAND_FY(K) = CAND_FY(I)
          CAND_FY(I) = CAND_TF
C
          CAND_TF   = CAND_FZ(K)
          CAND_FZ(K) = CAND_FZ(I)
          CAND_FZ(I) = CAND_TF
C
          CAND_T    = IFPEN(K)
          IFPEN(K) = IFPEN(I)
          IFPEN(I) = CAND_T
C 
          IPUT(I) = IPUT(K)
          ASSERT(IPUT(I) > 0)
          ASSERT(IPUT(I) <= I_STOK)
          IGET(IPUT(I)) = I

        ENDDO
C=======================================================================
C     CAND_A[NEDGE+1] : ADRESSE DE NEDGE+1 
C=======================================================================
      I_STOK = CAND_A(NEDGE+1) - 1
      CAND_A(NEDGE+2) = CAND_A(NEDGE+1)
C
      RETURN
      END
