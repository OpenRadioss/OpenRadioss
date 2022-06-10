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
Chd|  I23TRC                        source/interfaces/intsort/i23trc.F
Chd|-- called by -----------
Chd|        I23MAIN_TRI                   source/interfaces/intsort/i23main_tri.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I23TRC(
     1      NSN    ,I_STOK  ,CAND_N ,CAND_E,CAND_P ,
     2      CAND_FX,CAND_FY ,CAND_FZ,CAND_A,IFPEN  )
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
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   ROLE DE LA ROUTINE:
C   ===================
C   TRI sur N de CAND_N CAND_E CAND_F
C   et elimination des noeuds en rebond 
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_STOK,NSN
      INTEGER CAND_N(*),CAND_E(*),CAND_A(*), IFPEN(*),
     .        CAND_T
C     REAL
      my_real
     .   CAND_FX(*),CAND_FY(*),CAND_FZ(*),CAND_P(*),CAND_TF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I_ST0,N,NN,K,
     .        IGET(I_STOK),IPUT(I_STOK)
C=======================================================================
C
      DO 100 N=1,NSN+3
 100  CAND_A(N) = 0
C=======================================================================
C     LES NOEUDS DELETES DEVIENNENT NSN+1 
C=======================================================================
      DO I=1,I_STOK
        IF(IFPEN(I).EQ.0)THEN
          CAND_N(I) = NSN+1
        ENDIF
      ENDDO
C=======================================================================
C     CAND_A : DENOMBREMENT DE CHAQUE NOEUD 
C     APRES 300 CAND_A[3:NSN+3] : OCCURENCE DES NOEUDS [1:NSN+1] 
C=======================================================================
      DO 300 I=1,I_STOK
        NN = CAND_N(I) + 2
        CAND_A(NN) = CAND_A(NN) + 1
 300  CONTINUE
C=======================================================================
C     CAND_A : ADRESSE DE CHAQUE NOEUD 
C     APRES 400 CAND_A[2:NSN+2] : ADRESSE DES NOEUDS [1:NSN+1] 
C=======================================================================
      CAND_A(1) = 1
      CAND_A(2) = 1
      DO 400 N=3,NSN+2
 400  CAND_A(N) = CAND_A(N) + CAND_A(N-1)
C=======================================================================
C     IPUT(I) ADRESSE OU DOIT ALLER I
C     IGET(K) ADRESSE D'OU DOIT VENIR K
C     APRES 500 CAND_A[1:NSN+1] : ADRESSE DES NOEUDS [1:NSN+1] 
C=======================================================================
      DO 500 I=1,I_STOK
        NN = CAND_N(I) + 1
        K = CAND_A(NN)
        IPUT(I) = K
        IGET(K) = I
        CAND_A(NN) = CAND_A(NN) + 1
 500  CONTINUE
C=======================================================================
C     TRI DE CAND_N CAND_E CAND_P
C     SUR N CROISSANT
C     PERMUTATION 1 PASSE
C=======================================================================
      DO K=1,I_STOK
        I = IGET(K)
C
        CAND_T    = CAND_N(K)
        CAND_N(K) = CAND_N(I)
        CAND_N(I) = CAND_T
C
        CAND_T    = CAND_E(K)
        CAND_E(K) = CAND_E(I)
        CAND_E(I) = CAND_T
C------ Fx
        CAND_TF    = CAND_FX(K)
        CAND_FX(K) = CAND_FX(I)
        CAND_FX(I) = CAND_TF
C------ Fy
        CAND_TF    = CAND_FY(K)
        CAND_FY(K) = CAND_FY(I)
        CAND_FY(I) = CAND_TF
C------ Fz
        CAND_TF    = CAND_FZ(K)
        CAND_FZ(K) = CAND_FZ(I)
        CAND_FZ(I) = CAND_TF
C
        CAND_TF   = CAND_P(K)
        CAND_P(K) = CAND_P(I)
        CAND_P(I) = CAND_TF

        CAND_T    = IFPEN(K)
        IFPEN(K)  = IFPEN(I)
        IFPEN(I)  = CAND_T 
C 
        IPUT(I) = IPUT(K)
        IGET(IPUT(I)) = I
      ENDDO
C=======================================================================
C     CAND_A[NSN+1] : ADRESSE DE NSN+1 
C=======================================================================
      I_STOK = CAND_A(NSN+1) - 1
      CAND_A(NSN+2) = CAND_A(NSN+1)
C
      RETURN
      END
