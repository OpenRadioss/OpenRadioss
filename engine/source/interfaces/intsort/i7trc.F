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
Chd|  I7TRC                         source/interfaces/intsort/i7trc.F
Chd|-- called by -----------
Chd|        I20MAIN_TRI                   source/interfaces/intsort/i20main_tri.F
Chd|        I7MAIN_TRI                    source/interfaces/intsort/i7main_tri.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I7TRC(
     1      NSN    ,I_STOK  ,CAND_N ,CAND_E ,
     2      CAND_P ,CAND_FX ,CAND_FY,CAND_FZ,
     3      CAND_A ,IFPEN   ,INACTI ,IFQ    ,
     4      NUM_IMP,IND_IMP ,STFNS  ,NIN    ,
     5      NSNL   ,ITIED   ,CAND_F  )
C-----------------------------------------------
C   M o d u l e s
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
      INTEGER I_STOK,NSN,INACTI,IFQ,NUM_IMP,IND_IMP(*),
     .        NIN, NSNL, ITIED
      INTEGER CAND_N(*),CAND_E(*),CAND_A(*), IFPEN(*),
     .        CAND_T
C     REAL ou REAL*8
      my_real
     .   CAND_FX(*),CAND_FY(*),CAND_FZ(*),CAND_P(*),CAND_TF,
     .   STFNS(*),CAND_F(8,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I_ST0,N,NN,K,NI,
     .        IGET(I_STOK),IPUT(I_STOK)
C=============================================
C
      DO 100 N=1,NSN+3
 100  CAND_A(N) = 0
C=============================================
C     LES NOEUDS DELETES DEVIENNENT NSN+1 
C=============================================
C------for travelling nodes detected in imp_solv----- 
      IF(NUM_IMP.GT.0)THEN
        DO I=1,I_STOK
         IPUT(I)=0
        END DO
        DO N=1,NUM_IMP
	  I= IND_IMP(N)
          IPUT(I)=1
        END DO
       IF(IFQ.GT.0)THEN
C
        IF((INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7)
     .     .AND.TT.EQ.ZERO)THEN
          DO I=1,I_STOK
            IFPEN(I)=1
          END DO
        END IF
C
        DO I=1,I_STOK
          IF(IFPEN(I) == 0.AND.IPUT(I)==0) THEN  
            CAND_N(I) = NSN+1     
          ELSEIF(TT.GT.ZERO)THEN
            !case IDEL>0 + IFQ if element is deleted 
            !at previous cycle IFEN should be updated 
            NI = CAND_N(I)
            IF(NI.GT.NSNL) THEN
              !remote node
              NI = NI-NSNL 
              IF((STIFI(NIN)%P(NI) == 0.0).AND.IPUT(I)==0)THEN
                IFPEN(I)  = 0
                CAND_N(I) = NSN+1
              ENDIF
            ELSE
              !local node
              IF((STFNS(NI) == 0.0).AND.IPUT(I)==0)THEN 
                IFPEN(I)  = 0
                CAND_N(I) = NSN+1
              ENDIF
            ENDIF
          ENDIF
        ENDDO
       ELSEIF(INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7)THEN
        DO I=1,I_STOK
          IF(CAND_P(I).EQ.ZERO.AND.IPUT(I)==0)THEN
            CAND_N(I) = NSN+1
          ENDIF
        ENDDO
       ELSE
        DO I=1,I_STOK
          IF(IPUT(I) == 0) THEN
            CAND_N(I) = NSN+1
          ENDIF
        ENDDO
       ENDIF
      ELSEIF(IFQ.GT.0)THEN
C
        IF((INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7)
     .     .AND.TT.EQ.ZERO)THEN
          DO I=1,I_STOK
            IFPEN(I)=1
          END DO
        END IF

        IF(ITIED==0)THEN
          DO I=1,I_STOK

            IF(IFPEN(I) == 0) THEN  
              CAND_N(I) = NSN+1     
            ELSEIF(TT.GT.ZERO)THEN
              !case IDEL>0 + IFQ if element is deleted   
              !at previous cycle IFEN should be updated 
              NI = CAND_N(I)
              IF(NI.GT.NSNL) THEN
	  	    !remote node
          	NI = NI-NSNL
          	IF(STIFI(NIN)%P(NI) == ZERO)THEN
          	  IFPEN(I)    = 0
          	  CAND_N(I)   = NSN+1
          	ENDIF
              ELSE
	  	    !local node
          	IF(STFNS(NI) == ZERO)THEN 
          	  IFPEN(I)    = 0
          	  CAND_N(I)   = NSN+1
          	ENDIF
              ENDIF
            ENDIF
          ENDDO
        ELSE
          DO I=1,I_STOK

            IF(IFPEN(I) == 0 .AND. CAND_F(1,I) == ZERO) THEN  
              CAND_N(I) = NSN+1     
            ELSEIF(TT.GT.ZERO)THEN
              !case IDEL>0 + IFQ if element is deleted   
              !at previous cycle IFEN should be updated 
              NI = CAND_N(I)
              IF(NI.GT.NSNL) THEN
	  	    !remote node
          	NI = NI-NSNL
          	IF(STIFI(NIN)%P(NI) == ZERO)THEN
          	  IFPEN(I)  = 0
                  CAND_F(1,I) = ZERO
          	  CAND_N(I) = NSN+1
          	ENDIF
              ELSE
	  	    !local node
          	IF(STFNS(NI) == ZERO)THEN 
          	  IFPEN(I)  = 0
                  CAND_F(1,I) = ZERO
          	  CAND_N(I) = NSN+1
          	ENDIF
              ENDIF
            ENDIF
          ENDDO
        END IF

      ELSEIF(INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7)THEN
        IF(ITIED == 0)THEN
          DO I=1,I_STOK
            IF(CAND_P(I).EQ.ZERO)THEN
              CAND_N(I) = NSN+1
            ENDIF
          ENDDO
        ELSE
          DO I=1,I_STOK
            IF(CAND_P(I).EQ.ZERO .AND. CAND_F(1,I) == ZERO)THEN
              CAND_N(I) = NSN+1
            ELSEIF(TT.GT.ZERO)THEN
              !case IDEL>0 + ITIED if element is deleted   
              !at previous cycle CAND_F should be updated 
              NI = CAND_N(I)
              IF(NI.GT.NSNL) THEN
	  	    !remote node
          	NI = NI-NSNL
          	IF(STIFI(NIN)%P(NI) == ZERO)THEN
                  CAND_F(1,I) = ZERO
          	  CAND_N(I) = NSN+1
          	ENDIF
              ELSE
	  	    !local node
          	IF(STFNS(NI) == ZERO)THEN 
                  CAND_F(1,I) = ZERO
          	  CAND_N(I) = NSN+1
          	ENDIF
              ENDIF
            ENDIF
          ENDDO
        END IF
      ELSEIF(ITIED.NE.0)THEN
        DO I=1,I_STOK
          IF(CAND_F(1,I).EQ.ZERO)THEN
            CAND_N(I) = NSN+1
          ELSEIF(TT.GT.ZERO)THEN
            !case IDEL>0 + ITIED if element is deleted   
            !at previous cycle CAND_F should be updated 
            NI = CAND_N(I)
            IF(NI.GT.NSNL) THEN
	          !remote node
              NI = NI-NSNL
              IF(STIFI(NIN)%P(NI) == ZERO)THEN
          	CAND_F(1,I) = ZERO
                CAND_N(I)   = NSN+1
              ENDIF
            ELSE
	          !local node
              IF(STFNS(NI) == ZERO)THEN 
          	CAND_F(1,I) = ZERO
                CAND_N(I) = NSN+1
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ELSE
        DO I=1,I_STOK

            CAND_N(I) = NSN+1
        ENDDO
      ENDIF
C=============================================
C     CAND_A : DENOMBREMENT DE CHAQUE NOEUD 
C     APRES 300 CAND_A[3:NSN+3] : OCCURENCE DES NOEUDS [1:NSN+1] 
C=============================================
      DO 300 I=1,I_STOK
        NN = CAND_N(I) + 2
        CAND_A(NN) = CAND_A(NN) + 1
 300  CONTINUE
C=============================================
C     CAND_A : ADRESSE DE CHAQUE NOEUD 
C     APRES 400 CAND_A[2:NSN+2] : ADRESSE DES NOEUDS [1:NSN+1] 
C=============================================
      CAND_A(1) = 1
      CAND_A(2) = 1
      DO 400 N=3,NSN+2
 400  CAND_A(N) = CAND_A(N) + CAND_A(N-1)
C=============================================
C     IPUT(I) ADRESSE OU DOIT ALLER I
C     IGET(K) ADRESSE D'OU DOIT VENIR K
C     APRES 500 CAND_A[1:NSN+1] : ADRESSE DES NOEUDS [1:NSN+1] 
C=============================================
      DO 500 I=1,I_STOK
        NN = CAND_N(I) + 1
        K = CAND_A(NN)
        IPUT(I) = K
        IGET(K) = I
        CAND_A(NN) = CAND_A(NN) + 1
 500  CONTINUE
C=============================================
C     TRI DE CAND_N CAND_E CAND_P
C     SUR N CROISSANT
C     PERMUTATION 1 PASSE
C=============================================
      DO N=1,NUM_IMP
       K=IND_IMP(N)
       I = IPUT(K)
       IND_IMP(N)=I
      END DO

      IF(IFQ.GT.0.AND.(INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7).AND.ITIED.NE.0)THEN
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
C------   
          CAND_TF = CAND_F(1,K)
          CAND_F(1,K) = CAND_F(1,I)
          CAND_F(1,I) = CAND_TF
C
          CAND_TF = CAND_F(2,K)
          CAND_F(2,K) = CAND_F(2,I)
          CAND_F(2,I) = CAND_TF
C
          CAND_TF = CAND_F(3,K)
          CAND_F(3,K) = CAND_F(3,I)
          CAND_F(3,I) = CAND_TF
C
          CAND_TF = CAND_F(4,K)
          CAND_F(4,K) = CAND_F(4,I)
          CAND_F(4,I) = CAND_TF
C
          CAND_TF = CAND_F(5,K)
          CAND_F(5,K) = CAND_F(5,I)
          CAND_F(5,I) = CAND_TF
C
          CAND_TF = CAND_F(6,K)
          CAND_F(6,K) = CAND_F(6,I)
          CAND_F(6,I) = CAND_TF
C
          CAND_TF = CAND_F(7,K)
          CAND_F(7,K) = CAND_F(7,I)
          CAND_F(7,I) = CAND_TF
C
          CAND_TF = CAND_F(8,K)
          CAND_F(8,K) = CAND_F(8,I)
          CAND_F(8,I) = CAND_TF
C------   Fx
          CAND_TF    = CAND_FX(K)
          CAND_FX(K) = CAND_FX(I)
          CAND_FX(I) = CAND_TF
C------   Fy
          CAND_TF    = CAND_FY(K)
          CAND_FY(K) = CAND_FY(I)
          CAND_FY(I) = CAND_TF
C------   Fz
          CAND_TF    = CAND_FZ(K)
          CAND_FZ(K) = CAND_FZ(I)
          CAND_FZ(I) = CAND_TF
C
          CAND_TF   = CAND_P(K)
          CAND_P(K) = CAND_P(I)
          CAND_P(I) = CAND_TF
C
          CAND_T   = IFPEN(K)
          IFPEN(K) = IFPEN(I)
          IFPEN(I) = CAND_T 
C 
          IPUT(I) = IPUT(K)
          IGET(IPUT(I)) = I
        ENDDO
       ELSEIF(IFQ.GT.0.AND.(INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7))THEN
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
C------   Fx
          CAND_TF    = CAND_FX(K)
          CAND_FX(K) = CAND_FX(I)
          CAND_FX(I) = CAND_TF
C------   Fy
          CAND_TF    = CAND_FY(K)
          CAND_FY(K) = CAND_FY(I)
          CAND_FY(I) = CAND_TF
C------   Fz
          CAND_TF    = CAND_FZ(K)
          CAND_FZ(K) = CAND_FZ(I)
          CAND_FZ(I) = CAND_TF
C
          CAND_TF   = CAND_P(K)
          CAND_P(K) = CAND_P(I)
          CAND_P(I) = CAND_TF
C
          CAND_T   = IFPEN(K)
          IFPEN(K) = IFPEN(I)
          IFPEN(I) = CAND_T 
C 
          IPUT(I) = IPUT(K)
          IGET(IPUT(I)) = I
        ENDDO
       ELSEIF(IFQ.GT.0.AND.ITIED.NE.0)THEN
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
C------   
          CAND_TF = CAND_F(1,K)
          CAND_F(1,K) = CAND_F(1,I)
          CAND_F(1,I) = CAND_TF
C
          CAND_TF = CAND_F(2,K)
          CAND_F(2,K) = CAND_F(2,I)
          CAND_F(2,I) = CAND_TF
C
          CAND_TF = CAND_F(3,K)
          CAND_F(3,K) = CAND_F(3,I)
          CAND_F(3,I) = CAND_TF
C
          CAND_TF = CAND_F(4,K)
          CAND_F(4,K) = CAND_F(4,I)
          CAND_F(4,I) = CAND_TF
C
          CAND_TF = CAND_F(5,K)
          CAND_F(5,K) = CAND_F(5,I)
          CAND_F(5,I) = CAND_TF
C
          CAND_TF = CAND_F(6,K)
          CAND_F(6,K) = CAND_F(6,I)
          CAND_F(6,I) = CAND_TF
C
          CAND_TF = CAND_F(7,K)
          CAND_F(7,K) = CAND_F(7,I)
          CAND_F(7,I) = CAND_TF
C
          CAND_TF = CAND_F(8,K)
          CAND_F(8,K) = CAND_F(8,I)
          CAND_F(8,I) = CAND_TF
C------   Fx
          CAND_TF    = CAND_FX(K)
          CAND_FX(K) = CAND_FX(I)
          CAND_FX(I) = CAND_TF
C------   Fy
          CAND_TF    = CAND_FY(K)
          CAND_FY(K) = CAND_FY(I)
          CAND_FY(I) = CAND_TF
C------   Fz
          CAND_TF    = CAND_FZ(K)
          CAND_FZ(K) = CAND_FZ(I)
          CAND_FZ(I) = CAND_TF
C
          CAND_T   = IFPEN(K)
          IFPEN(K) = IFPEN(I)
          IFPEN(I) = CAND_T 
C
          IPUT(I) = IPUT(K)
          IGET(IPUT(I)) = I
        ENDDO
       ELSEIF(IFQ.GT.0)THEN
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
C------   Fx
          CAND_TF    = CAND_FX(K)
          CAND_FX(K) = CAND_FX(I)
          CAND_FX(I) = CAND_TF
C------   Fy
          CAND_TF    = CAND_FY(K)
          CAND_FY(K) = CAND_FY(I)
          CAND_FY(I) = CAND_TF
C------   Fz
          CAND_TF    = CAND_FZ(K)
          CAND_FZ(K) = CAND_FZ(I)
          CAND_FZ(I) = CAND_TF
          CAND_T   = IFPEN(K)
          IFPEN(K) = IFPEN(I)
          IFPEN(I) = CAND_T 
C
          IPUT(I) = IPUT(K)
          IGET(IPUT(I)) = I
        ENDDO
      ELSEIF((INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7).AND.ITIED.NE.0)THEN
        DO K=1,I_STOK
          I = IGET(K)
C
          CAND_T = CAND_N(K)
          CAND_N(K) = CAND_N(I)
          CAND_N(I) = CAND_T
C
          CAND_T = CAND_E(K)
          CAND_E(K) = CAND_E(I)
          CAND_E(I) = CAND_T
C------   
          CAND_TF = CAND_F(1,K)
          CAND_F(1,K) = CAND_F(1,I)
          CAND_F(1,I) = CAND_TF
C
          CAND_TF = CAND_F(2,K)
          CAND_F(2,K) = CAND_F(2,I)
          CAND_F(2,I) = CAND_TF
C
          CAND_TF = CAND_F(3,K)
          CAND_F(3,K) = CAND_F(3,I)
          CAND_F(3,I) = CAND_TF
C
          CAND_TF = CAND_F(4,K)
          CAND_F(4,K) = CAND_F(4,I)
          CAND_F(4,I) = CAND_TF
C
          CAND_TF = CAND_F(5,K)
          CAND_F(5,K) = CAND_F(5,I)
          CAND_F(5,I) = CAND_TF
C
          CAND_TF = CAND_F(6,K)
          CAND_F(6,K) = CAND_F(6,I)
          CAND_F(6,I) = CAND_TF
C
          CAND_TF = CAND_F(7,K)
          CAND_F(7,K) = CAND_F(7,I)
          CAND_F(7,I) = CAND_TF
C
          CAND_TF = CAND_F(8,K)
          CAND_F(8,K) = CAND_F(8,I)
          CAND_F(8,I) = CAND_TF
C
          CAND_TF = CAND_P(K)
          CAND_P(K) = CAND_P(I)
          CAND_P(I) = CAND_TF
C 
          IPUT(I) = IPUT(K)
          IGET(IPUT(I)) = I
        ENDDO
      ELSEIF(INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7)THEN
        DO K=1,I_STOK
          I = IGET(K)
C
          CAND_T = CAND_N(K)
          CAND_N(K) = CAND_N(I)
          CAND_N(I) = CAND_T
C
          CAND_T = CAND_E(K)
          CAND_E(K) = CAND_E(I)
          CAND_E(I) = CAND_T
C
          CAND_TF = CAND_P(K)
          CAND_P(K) = CAND_P(I)
          CAND_P(I) = CAND_TF
C 
          IPUT(I) = IPUT(K)
          IGET(IPUT(I)) = I
        ENDDO
      ELSEIF(ITIED.NE.0)THEN
        DO K=1,I_STOK
          I = IGET(K)
C
          CAND_T = CAND_N(K)
          CAND_N(K) = CAND_N(I)
          CAND_N(I) = CAND_T
C
          CAND_T = CAND_E(K)
          CAND_E(K) = CAND_E(I)
          CAND_E(I) = CAND_T
C------   
          CAND_TF = CAND_F(1,K)
          CAND_F(1,K) = CAND_F(1,I)
          CAND_F(1,I) = CAND_TF
C
          CAND_TF = CAND_F(2,K)
          CAND_F(2,K) = CAND_F(2,I)
          CAND_F(2,I) = CAND_TF
C
          CAND_TF = CAND_F(3,K)
          CAND_F(3,K) = CAND_F(3,I)
          CAND_F(3,I) = CAND_TF
C
          CAND_TF = CAND_F(4,K)
          CAND_F(4,K) = CAND_F(4,I)
          CAND_F(4,I) = CAND_TF
C
          CAND_TF = CAND_F(5,K)
          CAND_F(5,K) = CAND_F(5,I)
          CAND_F(5,I) = CAND_TF
C
          CAND_TF = CAND_F(6,K)
          CAND_F(6,K) = CAND_F(6,I)
          CAND_F(6,I) = CAND_TF
C
          CAND_TF = CAND_F(7,K)
          CAND_F(7,K) = CAND_F(7,I)
          CAND_F(7,I) = CAND_TF
C
          CAND_TF = CAND_F(8,K)
          CAND_F(8,K) = CAND_F(8,I)
          CAND_F(8,I) = CAND_TF
C 
          IPUT(I) = IPUT(K)
          IGET(IPUT(I)) = I
        ENDDO
      ELSEIF(NUM_IMP.GT.0)THEN
        DO K=1,I_STOK
          I = IGET(K)
C
          CAND_T = CAND_N(K)
          CAND_N(K) = CAND_N(I)
          CAND_N(I) = CAND_T
C
          CAND_T = CAND_E(K)
          CAND_E(K) = CAND_E(I)
          CAND_E(I) = CAND_T
C
          CAND_TF = CAND_P(K)
          CAND_P(K) = CAND_P(I)
          CAND_P(I) = CAND_TF
C 
          IPUT(I) = IPUT(K)
          IGET(IPUT(I)) = I
        ENDDO
      ELSE
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
C 
          IPUT(I) = IPUT(K)
          IGET(IPUT(I)) = I
        ENDDO

      ENDIF
C=============================================
C     CAND_A[NSN+1] : ADRESSE DE NSN+1 
C=============================================
      I_STOK = CAND_A(NSN+1) - 1
      CAND_A(NSN+2) = CAND_A(NSN+1)
C
      RETURN
      END


Chd|====================================================================
Chd|  I24TRC                        source/interfaces/intsort/i7trc.F
Chd|-- called by -----------
Chd|        I24MAIN_TRI                   source/interfaces/intsort/i24main_tri.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24TRC(
     1      NSN,I_STOK,CAND_N,CAND_E,CAND_A,NIN,NSNL,IRTLM,NSV,ITAB,
     2      MSEGLO,MSEGTYP)
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
      INTEGER I_STOK,NSN,CAND_T,NIN
      INTEGER CAND_N(*),CAND_E(*),CAND_A(*),
     *        NSNL,IRTLM(2,*),NSV(*),ITAB(*),MSEGLO(*),MSEGTYP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, I_ST0,N,NN,K,E,
     .        IGET(I_STOK),IPUT(I_STOK),count,i_st_sav,ISH,SYM_SURF
C=============================================
C

      DO N=1,NSN+3
        CAND_A(N) = 0
      ENDDO

      i_st_sav=I_STOK
      count=0
      DO I=1,I_STOK
        NN = CAND_N(I)
        E  = CAND_E(I)

C The symetric surface on shell elements must be kept
C On solids, the symmetric is the same

        ISH = MSEGTYP(E)
        IF (ISH > 0)THEN
           SYM_SURF = MSEGLO(ISH)
        ELSE
           SYM_SURF = MSEGLO(E)
        ENDIF

        IF(NN.LE.NSNL)THEN
          IF( IABS(IRTLM(1,NN))/=MSEGLO(E) .AND. IABS(IRTLM(1,NN)) /= SYM_SURF ) THEN
            CAND_N(I) = NSN+1
          ELSE
            count=count+1
          ENDIF
        ELSE
         IF(IABS(IRTLM_FI(NIN)%P(1,NN-NSNL)) /= MSEGLO(E) .AND. 
     *      IABS(IRTLM_FI(NIN)%P(1,NN-NSNL)) /= SYM_SURF )THEN
            CAND_N(I) = NSN+1
          ELSE
            count=count+1
          ENDIF
        ENDIF
      ENDDO

C=============================================
C     CAND_A : DENOMBREMENT DE CHAQUE NOEUD 
C     APRES 300 CAND_A[3:NSN+3] : OCCURENCE DES NOEUDS [1:NSN+1] 
C=============================================
      DO I=1,I_STOK
        NN = CAND_N(I) + 2
        CAND_A(NN) = CAND_A(NN) + 1
      ENDDO
      
C=============================================
C     CAND_A : ADRESSE DE CHAQUE NOEUD 
C     APRES 400 CAND_A[2:NSN+2] : ADRESSE DES NOEUDS [1:NSN+1] 
C=============================================
      CAND_A(1) = 1
      CAND_A(2) = 1
      DO  N=3,NSN+2
        CAND_A(N) = CAND_A(N) + CAND_A(N-1)
      ENDDO
C=============================================
C     IPUT(I) ADRESSE OU DOIT ALLER I
C     IGET(K) ADRESSE D'OU DOIT VENIR K
C     APRES 500 CAND_A[1:NSN+1] : ADRESSE DES NOEUDS [1:NSN+1] 
C=============================================
      DO I=1,I_STOK
        NN = CAND_N(I) + 1
        K = CAND_A(NN)
        IPUT(I) = K
        IGET(K) = I
        CAND_A(NN) = CAND_A(NN) + 1
      ENDDO
C=============================================
C     TRI DE CAND_N CAND_E CAND_P
C     SUR N CROISSANT
C     PERMUTATION 1 PASSE
C============================================= 
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
C 
          IPUT(I) = IPUT(K)
          IGET(IPUT(I)) = I
        ENDDO
C=============================================
C     CAND_A[NSN+1] : ADRESSE DE NSN+1 
C=============================================
      I_STOK = CAND_A(NSN+1) - 1
      CAND_A(NSN+2) = CAND_A(NSN+1)
C
      RETURN
      END

