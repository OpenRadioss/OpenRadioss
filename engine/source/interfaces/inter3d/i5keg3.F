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
Chd|  I5KEG3                        source/interfaces/inter3d/i5keg3.F
Chd|-- called by -----------
Chd|        I5KE3                         source/interfaces/inter3d/i5ke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I5KEG3(LFT    ,LLT   ,FRIC   ,SCALK  ,
     3                  TNJ    ,KI11  ,KI12   ,KJ11   ,KJ12  ,
     4                  KK11   ,KK12   ,KL11  ,KL12   ,OFF   ,
     5                  N1     ,N2     ,N3    ,STIF   ,H1    ,
     6                  H2     ,H3     ,H4)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT    ,LLT 
      my_real
     .   FRIC,OFF(*),SCALK,TNJ(3,MVSIZ)
      my_real
     .    KI11(3,3,MVSIZ),KJ11(3,3,MVSIZ),
     .    KK11(3,3,MVSIZ),KL11(3,3,MVSIZ),KI12(3,3,MVSIZ),
     .    KJ12(3,3,MVSIZ),KK12(3,3,MVSIZ),KL12(3,3,MVSIZ)
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: N1,N2,N3,STIF
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: H1,H2,H3,H4
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, J, K,IG,ISF,NN,NS,JLTF,NE,L
      my_real
     .   S2,FAC,FACF, H0, LA1, LA2, LA3, LA4,FACT(MVSIZ),
     .   D1,D2,D3,D4,A1,A2,A3,A4,KN(4,MVSIZ),Q(3,3,MVSIZ)
      my_real
     .   PREC,Q11,Q12,Q13,Q22,Q23,Q33,H00,VTX,VTY,VTZ,VT,
     .   KT1,KT2,KT3,KT4,Q1,Q2
C-----------------------------------------------
C    ----sans frottement d'abord--- 
      DO I=LFT,LLT
        FAC=STIF(I)*SCALK
        KN(1,I)=FAC*H1(I)
        KN(2,I)=FAC*H2(I)
        KN(3,I)=FAC*H3(I)
        KN(4,I)=FAC*H4(I)
      ENDDO
C
      DO I=LFT,LLT
       Q11=N1(I)*N1(I)
       Q12=N1(I)*N2(I)
       Q13=N1(I)*N3(I)
       Q22=N2(I)*N2(I)
       Q23=N2(I)*N3(I)
       Q33=N3(I)*N3(I)
       KI11(1,1,I)=KN(1,I)*Q11
       KI11(1,2,I)=KN(1,I)*Q12
       KI11(1,3,I)=KN(1,I)*Q13
       KI11(2,2,I)=KN(1,I)*Q22
       KI11(2,3,I)=KN(1,I)*Q23
       KI11(3,3,I)=KN(1,I)*Q33
       KJ11(1,1,I)=KN(2,I)*Q11
       KJ11(1,2,I)=KN(2,I)*Q12
       KJ11(1,3,I)=KN(2,I)*Q13
       KJ11(2,2,I)=KN(2,I)*Q22
       KJ11(2,3,I)=KN(2,I)*Q23
       KJ11(3,3,I)=KN(2,I)*Q33
       KK11(1,1,I)=KN(3,I)*Q11
       KK11(1,2,I)=KN(3,I)*Q12
       KK11(1,3,I)=KN(3,I)*Q13
       KK11(2,2,I)=KN(3,I)*Q22
       KK11(2,3,I)=KN(3,I)*Q23
       KK11(3,3,I)=KN(3,I)*Q33
       KL11(1,1,I)=KN(4,I)*Q11
       KL11(1,2,I)=KN(4,I)*Q12
       KL11(1,3,I)=KN(4,I)*Q13
       KL11(2,2,I)=KN(4,I)*Q22
       KL11(2,3,I)=KN(4,I)*Q23
       KL11(3,3,I)=KN(4,I)*Q33
      ENDDO
C    ----avec frottement --- 
      IF (FRIC>ZERO) THEN
C    ----tangent vector --- 
       FAC= FRIC*SCALK
       DO I=LFT,LLT
         Q(1,1,I)=TNJ(1,I)
         Q(1,2,I)=TNJ(2,I)
         Q(1,3,I)=TNJ(3,I)
         Q(3,1,I)=N1(I)
         Q(3,2,I)=N2(I)
         Q(3,3,I)=N3(I)
         Q(2,1,I)=Q(3,2,I)*Q(1,3,I)-Q(3,3,I)*Q(1,2,I)
         Q(2,2,I)=Q(3,3,I)*Q(1,1,I)-Q(3,1,I)*Q(1,3,I)
         Q(2,3,I)=Q(3,1,I)*Q(1,2,I)-Q(3,2,I)*Q(1,1,I)
         FACT(I)=STIF(I)*FAC
       ENDDO
C       
       DO J=1,3 
        DO K=J,3 
         DO I=LFT,LLT
           Q1 =Q(1,J,I)*Q(1,K,I)
           Q2 =Q(2,J,I)*Q(2,K,I)
           FAC=FACT(I)*(Q1+Q2)
           KT1=FAC*H1(I)
           KI11(J,K,I)=KI11(J,K,I)+KT1
           KT2=FAC*H2(I)
           KJ11(J,K,I)=KJ11(J,K,I)+KT2
           KT3=FAC*H3(I)
           KK11(J,K,I)=KK11(J,K,I)+KT3
           KT4=FAC*H4(I)
           KL11(J,K,I)=KL11(J,K,I)+KT4
         ENDDO
        ENDDO
       ENDDO
      END IF !(FRIC>ZERO) THEN
C
       DO J=1,3 
        DO K=J,3 
         DO I=LFT,LLT
          KI12(J,K,I)=-KI11(J,K,I)
          KJ12(J,K,I)=-KJ11(J,K,I)
          KK12(J,K,I)=-KK11(J,K,I)
          KL12(J,K,I)=-KL11(J,K,I)
         ENDDO
        ENDDO
       ENDDO
       DO J=1,3 
        DO K=J+1,3 
         DO I=LFT,LLT
          KI12(K,J,I)=-KI11(J,K,I)
          KJ12(K,J,I)=-KJ11(J,K,I)
          KK12(K,J,I)=-KK11(J,K,I)
          KL12(K,J,I)=-KL11(J,K,I)
         ENDDO
        ENDDO
       ENDDO
C
       DO I=LFT,LLT
        OFF(I)=ONE
       ENDDO
C
       RETURN
      END
Chd|====================================================================
Chd|  I5FRIK3                       source/interfaces/inter3d/i5keg3.F
Chd|-- called by -----------
Chd|        I5KE3                         source/interfaces/inter3d/i5ke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I5FRIK3(LFT   ,LLT   ,I_N  ,I_E   ,IPARI  ,
     2                  X     ,IRECT ,MSR   ,NSV   ,IRTL   ,
     3                  CST   ,IRTLO ,FRIC0 ,FRIC  ,FREQ   ,
     4                  FTSAV ,STFM  ,TNJ   ,XP    ,YP     ,
     5                  ZP    ,N1    ,N2    ,N3    ,ANS    ,
     6                  STIF  )!,FXI   ,FYI   ,FZI   ,FNI)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*),LFT, LLT, NFT,I_N(*),I_E(*)
C     REAL
      my_real
     .   FRIC
      INTEGER IRECT(4,*), MSR(*), NSV(*), IRTL(*), IRTLO(*)
C     REAL
      my_real
     .   X(3,*), CST(2,*), FRIC0(3,*),TNJ(3,*), FREQ, FTSAV(*),STFM(*)
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: XP,YP,ZP,N1,N2,N3
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: ANS
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: STIF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IL, LOLD, JJ, NN, J3,
     .   J2, J1, IG, I3, I2, I1, K, IFQ, MFROT,L
      INTEGER NISKYL
C     REAL
      my_real
     .   H(4), XX1(4), XX2(4), XX3(4),
     .   SS0, TT0, XC, ECONVT, ALPHA, ALPHI,
     .   YC, ZC, XC0, YC0, ZC0, SP, SM, TP, TM, ANSX, ANSY, ANSZ, FMAX,
     .   STF, FTI, FN, TN1, TN2, TN3, TN, DTM, XMU, VX,VY,VZ,VV,V2,P,
     .   VV1,VV2,V21,DMU,AA
      my_real, DIMENSION(MVSIZ) :: FXI,FYI,FZI,FNI
C-----------------------------------------------
      DO I=LFT,LLT
       L=I_E(I)
       STIF(I)=HALF*STFM(L)
      ENDDO
C      
      IF (FRIC==ZERO) RETURN
C      
      DO 300 I=LFT,LLT
       IL=I_N(I)
       LOLD=IABS(IRTLO(IL))
       IF(LOLD==0)THEN
C-------------------------------
C       POINT NON IMPACTE PRECEDEMENT:::diff than explicit
C-------------------------------
c        TN1=ZERO
c        TN2=ZERO
c        TN3=ZERO
         TN3=ZERO
         TN=SQRT(N1(I)*N1(I)+N2(I)*N2(I))
         IF(TN/=ZERO)THEN
          TN2=-N1(I)/TN
          TN1=N2(I)/TN
         ELSE
          TN2=ZERO
          TN1=ONE
         ENDIF
       ELSE
C-------------------------------
C       POINT IMPACTE PRECEDEMENT
C-------------------------------
        FNI(I)=ANS(I)*STIF(I)
        SS0=CST(1,IL)
        TT0=CST(2,IL)
        FXI(I)=FRIC0(1,IL)
        FYI(I)=FRIC0(2,IL)
        FZI(I)=FRIC0(3,IL)
C
        XC=XP(I)
        YC=YP(I)
        ZC=ZP(I)
        DO 100 JJ=1,4
        NN=MSR(IRECT(JJ,LOLD))
        XX1(JJ)=X(1,NN)
        XX2(JJ)=X(2,NN)
 100    XX3(JJ)=X(3,NN)
        XC0=ZERO
        YC0=ZERO
        ZC0=ZERO
        SP=ONE+SS0
        SM=ONE-SS0
        TP= FOURTH*(ONE+TT0)
        TM= FOURTH*(ONE-TT0)
        H(1)=TM*SM
        H(2)=TM*SP
        H(3)=TP*SP
        H(4)=TP*SM
        DO 120 JJ=1,4
        XC0=XC0+H(JJ)*XX1(JJ)
        YC0=YC0+H(JJ)*XX2(JJ)
 120    ZC0=ZC0+H(JJ)*XX3(JJ)
        ANSX= (XC-XC0)
        ANSY= (YC-YC0)
        ANSZ= (ZC-ZC0)
C
        FMAX= -MIN(FRIC*FNI(I),ZERO)
C
        STF=EM01*STIF(I)
        FXI(I)=FXI(I) + ANSX*STF
        FYI(I)=FYI(I) + ANSY*STF
        FZI(I)=FZI(I) + ANSZ*STF
          IFQ = IPARI(31)
          IF (IFQ>0) THEN
	    IF (IFQ==3) FREQ = MAX(ONE,FREQ*DT12)
            ALPHA = FREQ
            ALPHI = ONE - ALPHA
            K = 3*(IL-1)
	    IF (FNI(I)/=ZERO) THEN
              FXI(I)= ALPHA*FXI(I) + ALPHI*FTSAV(K+1)
              FYI(I)= ALPHA*FYI(I) + ALPHI*FTSAV(K+2)
              FZI(I)= ALPHA*FZI(I) + ALPHI*FTSAV(K+3)
            ENDIF
          ENDIF
        FTI=SQRT(FXI(I)*FXI(I)+FYI(I)*FYI(I)+FZI(I)*FZI(I))
C
        FN=FXI(I)*N1(I)+FYI(I)*N2(I)+FZI(I)*N3(I)
        TN1=FXI(I)-N1(I)*FN
        TN2=FYI(I)-N2(I)*FN
        TN3=FZI(I)-N3(I)*FN
        TN=SQRT(TN1*TN1+TN2*TN2+TN3*TN3)
        IF(TN/=ZERO)THEN
         TN1=TN1/TN
         TN2=TN2/TN
         TN3=TN3/TN
        ELSE
         TN3=ZERO
         TN=SQRT(N1(I)*N1(I)+N2(I)*N2(I))
         IF(TN/=ZERO)THEN
          TN2=-N1(I)/TN
          TN1=N2(I)/TN
         ELSE
          TN2=ZERO
          TN1=ONE
         ENDIF
        ENDIF
C
       ENDIF
        TNJ(1,I)=TN1	
        TNJ(2,I)=TN2	
        TNJ(3,I)=TN3	
C
 300  CONTINUE
      RETURN
      END
