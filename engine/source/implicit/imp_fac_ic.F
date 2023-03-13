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
C  ------incomplete cholesky using jennings diagnal modif----
Chd|====================================================================
Chd|  IMP_FAC_ICJ                   source/implicit/imp_fac_ic.F  
Chd|-- called by -----------
Chd|        IMP_FSAI                      source/implicit/imp_fsa_inv.F 
Chd|-- calls ---------------
Chd|        ERR_MEM                       source/implicit/lin_solv.F    
Chd|====================================================================
      SUBROUTINE IMP_FAC_ICJ(
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,IADM  ,JDIM  ,DIAG_M,LT_M   ,
     3                    PSI   ,NNZM  ,MAX_L ,ISKY  ,LI     ,
     4                    NNE   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,NNZ  ,IADK(*) ,JDIK(*),NNZM ,IADM(*),JDIM(*),
     .         NNE,ISKY(*),MAX_L
C     REAL
      my_real
     .  DIAG_K(*), DIAG_M(*), LT_K(*)  ,LT_M(*) ,PSI ,LI(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,REJ,MAXK,
     1        K0 ,J0 ,K1, KFT,LSKY
      my_real
     .   AII,AJJ,LKIK,S, FAC
C-----------------------------
C      PSI = SQRT(PSI)
      REJ = 0
      MAXK = 0
      NNE = 0
      NNZM = 0
      IADM(1)=1
      DO I=1,NDDL
       DIAG_M(I) = DIAG_K(I)
       ISKY(I)=I
       LI(I)=ZERO
      ENDDO
C
      DO I=1,NDDL
       AII = DIAG_M(I)
       K0 = IADK(I+1) - 1
       IF (K0>0) THEN
        J0 = JDIK(K0)
        IF (J0>MAXK) MAXK=J0
       ENDIF 
       DO J = IADK(I),K0
        K1= JDIK(J)
        LI(K1) = LT_K(J)
C----------ISKY(J)=I for LT_K(I,J)-----
        IF (ISKY(K1)==K1) ISKY(K1)=I
       ENDDO
       KFT =ISKY(I)
       DO K = KFT,I-1
        LSKY = ISKY(K)
C        IF (LSKY>=IADM(K+1)) write(*,*)'found 1',i       
        IF (LSKY<IADM(K+1).AND.JDIM(LSKY)==I) THEN
         LKIK = LT_M(LSKY)/DIAG_M(K)
         AII = AII -LT_M(LSKY)*LKIK 
         ISKY(K)=LSKY+1
         DO J = ISKY(K),IADM(K+1)-1
          K1= JDIM(J)
C----------voir ici quand ca tombre sur diagonal---
          LI(K1) = LI(K1)-LKIK*LT_M(J)
         ENDDO
        ENDIF 
       ENDDO 
       DO 100 J = I+1 , MAXK
        IF (ISKY(J)==J) GOTO 100
        S = LI(J)
        IF (S/=ZERO) THEN
         LI(J)=ZERO
         AJJ = DIAG_M(J)
         IF (PSI==ZERO) THEN
          NNZM = NNZM +1
          IF (NNZM>MAX_L) CALL ERR_MEM(NNZM)
          LT_M(NNZM)=S
          JDIM(NNZM)=J
         ELSEIF (S*S<PSI*AII*AJJ) THEN
          S =ABS(S)
          FAC = SQRT(AII/AJJ)
          AII = AII +S*FAC 
          DIAG_M(J) = AJJ +S/FAC
          REJ = REJ +1
         ELSE
          NNZM = NNZM +1
          IF (NNZM>MAX_L) CALL ERR_MEM(NNZM)
          LT_M(NNZM)=S
          JDIM(NNZM)=J
         ENDIF 
        ENDIF 
 100   CONTINUE
       IF (AII<EM20) THEN
        NNE=NNE+1
        AII=SIGN(MAX(ABS(AII),EM20),AII)
       ENDIF 
       DIAG_M(I) = ONE/AII
       IADM(I+1)=NNZM+1
       DO J = IADM(I),NNZM
        LT_M(J)=LT_M(J)*DIAG_M(I)
       ENDDO
       ISKY(I)=IADM(I)
       IF (ISKY(I)>NNZM) ISKY(I)= IADM(I)
      ENDDO
c      IF (NNE>0) then
c       write(*,*)'--WARNING: PIVOT PROBLEM',NNE
c      DO I=1,NDDL
c       write(*,*)'diag,i,iadm=',DIAG_M(I),I,IADM(I)
c      ENDDO
c      DO J=1,NNZM
c       write(*,*)'LT_M,i,j=',LT_M(J),J,JDIM(J)
c      ENDDO
c      endif
      RETURN
      END
Chd|====================================================================
Chd|  DIAGMOD                       source/implicit/imp_fac_ic.F  
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DIAGMOD(J0, J1, LI, DIAG_M ,AII,ISKY)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  J0, J1,ISKY(*)
C     REAL
      my_real
     .  DIAG_M(*), LI(*)  ,AII
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J
      my_real
     .   AJJ,S,FAC
C-----------------------------------------------
       DO J = J0, J1
        IF (ISKY(J)/=J) THEN
         S = LI(J)
         IF (S/=ZERO) THEN
          AJJ = DIAG_M(J)
          S =ABS(S)
          FAC = SQRT(ABS(AII/AJJ))
          AII = AII +S*FAC 
          IF (abs(aii)<EP30) THEN
          ELSE
          ENDIF
          DIAG_M(J) = AJJ +S/FAC
         ENDIF 
        ENDIF 
       ENDDO
C
      RETURN
      END
      
