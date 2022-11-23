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
C  ------approximate inverse (minimisation:least squares by colonne)------ 
Chd|====================================================================
Chd|  IMP_PC_INV                    source/implicit/imp_pc_inv.F  
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        GET_SUBA                      source/implicit/imp_pc_inv.F  
Chd|        IMP_SAIC                      source/implicit/imp_pc_inv.F  
Chd|        SP_STATIC                     source/implicit/imp_fsa_inv.F 
Chd|====================================================================
      SUBROUTINE IMP_PC_INV(
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,IADM  ,JDIM  ,DIAG_M, LT_M  ,
     3                    PSI   ,NNZM  ,MAXC  ,MAXA  ,MAX_L  ,
     4                    IOPT  ,NNE   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL ,NNZ ,IADK(*),JDIK(*),NNZM ,IADM(*),JDIM(*),
     .         MAXC ,MAXA ,MAX_L,IOPT,NNE
C     REAL
      my_real
     .  DIAG_K(*), DIAG_M(*), LT_K(*)  ,LT_M(*) ,PSI 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--- DIAG_M->[D]^-1, LT_M  strictly upper in c.r.s. format
      INTEGER I,J,K,M,N,NC(NDDL),JM(MAXC,NDDL),I1
      my_real
     .   PSR,TOL,MJ(MAXA),A(MAXA,MAXC)
C-----------------------------
      CALL SP_STATIC(NDDL  ,IADK  ,JDIK  ,DIAG_K ,LT_K  ,
     .               IADM  ,JDIM  ,NNZM  ,NC     ,JM    ,
     .               MAXC  ,PSI   ,IOPT  )
C------------parallelisation ici----
      NNZM = 0
      IADM(1)=1
      NNE=0
      TOL = PSI*PSI 
      DO I=1,NDDL
       CALL GET_SUBA(NDDL  ,IADK  ,JDIK  ,DIAG_K ,LT_K  ,
     .               NC    ,JM    ,A     ,MAXC   ,MAXA  ,
     .               I     ,M     ,MJ     )
       IF (M>MAXA) WRITE(*,*)'M>MAXB',M,MAXA
C       write(*,*)'m,n,i=',m,nc(i),i
       CALL IMP_SAIC(M     ,NC(I) ,A     ,MJ     ,MAXA  )
C------------post-filtrage----
        DO K =1,NC(I)
         J=JM(K,I)
         IF (J==I) THEN
           DIAG_M(I)=MJ(K)
           IF (DIAG_M(I)<=EM20) NNE=NNE+1
         ELSE
          I1=MIN(I,J)
          PSR = TOL*ABS(DIAG_M(I1))
          IF (ABS(MJ(K))>=PSR) THEN
           NNZM = NNZM+1
           IF (NNZM>MAX_L) THEN
             WRITE(*,*)'NNZM>MAX_L',NNZM,MAXC,I
             CALL ARRET(2)
           ENDIF
           JDIM(NNZM)=J
           LT_M(NNZM)=MJ(K)
          ENDIF
         ENDIF
        ENDDO
       IADM(I+1)=NNZM+1
      ENDDO 
C------------eventuellement deuxieme niveau----
C
      RETURN
      END
C-------------resol A(M,N).MJ-I=0  ----
Chd|====================================================================
Chd|  IMP_SAIC                      source/implicit/imp_pc_inv.F  
Chd|-- called by -----------
Chd|        IMP_PC_INV                    source/implicit/imp_pc_inv.F  
Chd|-- calls ---------------
Chd|        IMP_QRF                       source/implicit/imp_pc_inv.F  
Chd|        LT_SOLV                       source/implicit/imp_pc_inv.F  
Chd|        MAV_QT                        source/implicit/imp_pc_inv.F  
Chd|====================================================================
      SUBROUTINE IMP_SAIC(M     ,N     ,A     ,MJ     ,MAXC )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  M,N ,MAXC
C     REAL
      my_real
     . A(MAXC,*),MJ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I
      my_real
     . D_R(N),TAU(N)
C-----------------------------------------------
      CALL IMP_QRF(M     ,N     ,A      ,MAXC   ,D_R   ,
     .                   TAU    )
C 
      CALL MAV_QT(M     ,N     ,A      ,MAXC   ,TAU   ,
     .            MJ     )
C 
      CALL LT_SOLV(N    ,A      ,MAXC   ,D_R   ,MJ     )
C--------------------------------------------
      RETURN
      END
C-------------set submatrix A(M,N) for SAI ----
Chd|====================================================================
Chd|  GET_SUBA                      source/implicit/imp_pc_inv.F  
Chd|-- called by -----------
Chd|        IMP_PC_INV                    source/implicit/imp_pc_inv.F  
Chd|-- calls ---------------
Chd|        GET_KIJS                      source/implicit/imp_pc_inv.F  
Chd|        INTAB2                        source/implicit/imp_fsa_inv.F 
Chd|====================================================================
      SUBROUTINE GET_SUBA(NDDL  ,IADK  ,JDIK  ,DIAG_K ,LT_K  ,
     .                    NC    ,JM    ,A     ,MAXC   ,MAXA  ,
     .                    IM    ,M     ,MJ    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,MAXC,MAXA,IADK(*)  ,JDIK(*),IM,M
      INTEGER  NC(*),JM(MAXC,*)
C     REAL
      my_real
     .  A(MAXA,*),LT_K(*),DIAG_K(*),MJ(*)
C-----------------------------------------------
C   External function
C-----------------------------------------------
      INTEGER INTAB2
      EXTERNAL INTAB2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL EXTN(NDDL)
      INTEGER I,I1,J,K,K0,NL,N,JN,IUN
C--------------------------------------------
      M=0
      DO I=1,NDDL
       EXTN(I)=.TRUE.
      ENDDO
C------d'abord NxN-----------
      DO I1=1,NC(IM)
       I=JM(I1,IM)
       EXTN(I)=.FALSE.
       M=M+1
c       write(*,*)'I1,IM,M=',I1,IM,M
       IF (I==IM) THEN
        N=1
        IUN=M
 100    J=JM(N,IM)
        IF (J<I) THEN
         CALL GET_KIJS(J  ,I ,IADK,JDIK,LT_K ,A(M,N))
         N=N+1
         GOTO 100
        ENDIF 
C------A(M,N)=DIAG_K(I)------
        A(M,N)=DIAG_K(I)
        N=N+1
        DO K=N,NC(IM)
         J=JM(K,IM)
         CALL GET_KIJS(I  ,J ,IADK,JDIK,LT_K ,A(M,K))
        ENDDO
       ELSE
         DO K=1,NC(IM)
          J=JM(K,IM)
          IF (J<I) THEN
           CALL GET_KIJS(J  ,I ,IADK,JDIK,LT_K ,A(M,K))
          ELSEIF (J==I) THEN
           A(M,K)=DIAG_K(I)
          ELSE
           CALL GET_KIJS(I  ,J ,IADK,JDIK,LT_K ,A(M,K))
          ENDIF 
         ENDDO
       ENDIF 
      ENDDO
C------ajoute autres lignes-----------
      DO I=1,NDDL
       IF (EXTN(I)) THEN
        N=INTAB2(NC(IM),JM(1,IM),NC(I),JM(1,I))
        IF (N>0) THEN
         IF(M==MAXA) write(*,*)'mem',N,I
         M=M+1
         DO K=1,N-1
          A(M,K)=ZERO
         ENDDO
         DO K=N,NC(IM)
          J=JM(K,IM)
          IF (J<I) THEN
           CALL GET_KIJS(J  ,I ,IADK,JDIK,LT_K ,A(M,K))
          ELSEIF (J==I) THEN
           A(M,K)=DIAG_K(I)
          ELSE
           CALL GET_KIJS(I  ,J ,IADK,JDIK,LT_K ,A(M,K))
          ENDIF 
         ENDDO
        ENDIF 
       ENDIF 
      ENDDO
C------INITIAL ej------
      DO I=1,M
       MJ(I)=ZERO
      ENDDO
      MJ(IUN)=ONE
C
      RETURN
      END
C-------------QR factorisation ----
Chd|====================================================================
Chd|  IMP_QRF                       source/implicit/imp_pc_inv.F  
Chd|-- called by -----------
Chd|        IMP_SAIC                      source/implicit/imp_pc_inv.F  
Chd|-- calls ---------------
Chd|        PRODUT_V                      source/implicit/produt_v.F    
Chd|====================================================================
      SUBROUTINE IMP_QRF(M     ,N     ,A      ,MAXC   ,D_R   ,
     .                   TAU    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  MAXC,M,N
C     REAL
      my_real
     .  A(MAXC,*),D_R(*),TAU(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C--A(M,N), a la sortie,Qi=1-uu^t/tau est stocke dans triag inf de A
C----------R est stoke dans triag sup,+ diag dans D-------
      INTEGER I,J,K,L
      my_real
     .  S,NORM2,SCAL
C------------Qi=1-u(i)u(i)^t/tau(i)--------------------------------
      DO J=1,N-1
       SCAL=ZERO
       DO I=J,M
        SCAL=MAX(SCAL,ABS(A(I,J)))
       ENDDO 
       IF (SCAL==ZERO) THEN
        TAU(J)=ZERO 
        D_R(J)=ZERO
        WRITE(*,*)'SIGNULAR A' 
       ELSE
        SCAL=ONE/SCAL
        DO I=J,M
         A(I,J)=A(I,J)*SCAL
        ENDDO
        L=M-J+1
        CALL PRODUT_V( L  ,A(J,J)   ,A(J,J)  ,NORM2) 
        S =SIGN(SQRT(NORM2),A(J,J))
        A(J,J)=A(J,J)+S
        TAU(J)=S*A(J,J) 
        D_R(J)=-S/SCAL
C------------R=QA--------------------------------
        DO K=J+1,N
         CALL PRODUT_V( L  ,A(J,J)   ,A(J,K)  ,NORM2) 
         S = NORM2/TAU(J)
         DO I=J,N
          A(I,K)=A(I,K)-S*A(I,J) 
         ENDDO
        ENDDO
       ENDIF 
      ENDDO 
      D_R(N)=A(N,N)
C
      RETURN
      END
C-------------b=Q^tb ---------------------------------
Chd|====================================================================
Chd|  MAV_QT                        source/implicit/imp_pc_inv.F  
Chd|-- called by -----------
Chd|        IMP_SAIC                      source/implicit/imp_pc_inv.F  
Chd|-- calls ---------------
Chd|        PRODUT_V                      source/implicit/produt_v.F    
Chd|====================================================================
      SUBROUTINE MAV_QT(M     ,N     ,A      ,MAXC   ,TAU   ,
     .                  B     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  MAXC,M,N
C     REAL
      my_real
     .  A(MAXC,*),TAU(*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      my_real
     .  S,NORM2
C--------------------------------------------
      DO J=1,N-1
       K=M-J+1
       CALL PRODUT_V( K  ,A(J,J)   ,B(J)  ,NORM2) 
       S = NORM2/TAU(J)
       DO I=J,N
        B(I)=B(I)-S*A(I,J) 
       ENDDO
      ENDDO 
C
      RETURN
      END
C-------------Rx=b ---------------------------------
Chd|====================================================================
Chd|  LT_SOLV                       source/implicit/imp_pc_inv.F  
Chd|-- called by -----------
Chd|        IMP_SAIC                      source/implicit/imp_pc_inv.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LT_SOLV(N     ,A      ,MAXC   ,D_R   ,B      )
C----------R est stoke dans triag_sup de A,+ diag dans D_R-------
C----------X est sortie dans B-------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  MAXC,N
C     REAL
      my_real
     .  A(MAXC,*),D_R(*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      my_real
     .  S
C--------------------------------------------
      B(N)=B(N)/D_R(N)
      DO I=N-1,1,-1
       S = ZERO
       DO J=I+1,N
        S=S+A(I,J)*B(J) 
       ENDDO
       B(I)=(B(I)-S)/D_R(I)
      ENDDO 
C
      RETURN
      END
Chd|====================================================================
Chd|  GET_KIJS                      source/implicit/imp_pc_inv.F  
Chd|-- called by -----------
Chd|        GET_SUBA                      source/implicit/imp_pc_inv.F  
Chd|-- calls ---------------
Chd|        INTAB0                        source/implicit/imp_fsa_inv.F 
Chd|====================================================================
      SUBROUTINE GET_KIJS(I  ,J ,IADK,JDIK,K_LT ,KIJ)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I,J        
      INTEGER IADK(*) , JDIK(*)      
C     REAL
      my_real
     .   K_LT(*) ,KIJ
C-----------------------------------------------
C   External function
C-----------------------------------------------
      INTEGER INTAB0
      EXTERNAL INTAB0
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,K0,NL,N
C----6---------------------------------------------------------------7---------8
        K0=IADK(I)
        NL=IADK(I+1)-K0
        N=INTAB0(NL,JDIK(K0),J)
        IF (N>0) THEN
         KIJ=K_LT(N+K0-1)
        ELSE
         KIJ=ZERO
        ENDIF
C
      RETURN
      END
      
