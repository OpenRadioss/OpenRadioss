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
Chd|  I10KEG3                       source/interfaces/int10/i10keg3.F
Chd|-- called by -----------
Chd|        I10KE3                        source/interfaces/int10/i10ke3.F
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE I10KEG3(JLT    ,A      ,V      ,MS    ,FRIC   ,
     1                  NX1    ,NX2    ,NX3    ,NX4   ,NY1    ,
     2                  NY2    ,NY3    ,NY4    ,NZ1   ,NZ2    ,
     3                  NZ3    ,NZ4    ,LB1    ,LB2   ,LB3    ,
     4                  LB4    ,LC1    ,LC2    ,LC3   ,LC4    ,
     5                  P1     ,P2     ,P3     ,P4    ,NIN    ,
     6                  IX1    ,IX2    ,IX3    ,IX4   ,NSVG   ,
     7                  GAPV   ,ITIED  ,CAND_F ,INDEX ,STIF   ,
     8                  VXI    ,VYI    ,VZI   ,MSI    ,X1     ,
     9                  X2     ,X3     ,X4    ,Y1     ,Y2     ,
     A                  Y3     ,Y4     ,Z1    ,Z2     ,Z3     ,
     B                  Z4     ,KI11   ,KI12   ,KJ11  ,KJ12   ,
     C                  KK11   ,KK12   ,KL11   ,KL12  ,OFF    ,
     D                  SCALK  ,LREM   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
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
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, ITIED,NIN,LREM
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ), INDEX(MVSIZ)
      my_real
     .   A(3,*), MS(*), V(3,*),X1(*),X2(*),X3(*),X4(*),
     .   Y1(*),Y2(*),Y3(*),Y4(*),Z1(*),Z2(*),Z3(*),Z4(*),
     .   CAND_F(6,*),FRIC,OFF(*),SCALK,
     .   VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ)
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .    P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ), STIF(MVSIZ),
     .    GAPV(MVSIZ),KI11(3,3,MVSIZ),KJ11(3,3,MVSIZ),
     .    KK11(3,3,MVSIZ),KL11(3,3,MVSIZ),KI12(3,3,MVSIZ),
     .    KJ12(3,3,MVSIZ),KK12(3,3,MVSIZ),KL12(3,3,MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, J, K,IG,ISF,NN,NS,JLTF,NE,II
      my_real
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PENE(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   VT1(MVSIZ), VT2(MVSIZ),FNI(MVSIZ), 
     .   NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ),
     .   T1X(MVSIZ),T1Y(MVSIZ),T1Z(MVSIZ),
     .   T2X(MVSIZ),T2Y(MVSIZ),T2Z(MVSIZ),NORMINV,
     .   VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ), 
     .   S2,FAC,FACF, H0, LA1, LA2, LA3, LA4,FACT(MVSIZ),
     .   D1,D2,D3,D4,A1,A2,A3,A4,KN(4,MVSIZ),Q(3,3,MVSIZ),FAC10
      my_real
     .   PREC,Q11,Q12,Q13,Q22,Q23,Q33,H00,VTX,VTY,VTZ,VT,
     .   KT1,KT2,KT3,KT4,Q1,Q2
C-----------------------------------------------
       FRIC =UN
       FAC10 = DIX
      IF (IMP_INT7.EQ.3) THEN
       DO I=1,JLT
        D1 = SQRT(P1(I))
        P1(I) = QUART*GAPV(I) 
        D2 = SQRT(P2(I))
        P2(I) = QUART*GAPV(I)
        D3 = SQRT(P3(I))
        P3(I) = QUART*GAPV(I)
        D4 = SQRT(P4(I))
        P4(I) = QUART*GAPV(I)
       ENDDO
      ELSE
       DO I=1,JLT
C
        D1 = SQRT(P1(I))
        P1(I) = MAX(ZERO, GAPV(I) - D1)
C
        D2 = SQRT(P2(I))
        P2(I) = MAX(ZERO, GAPV(I) - D2)
C
        D3 = SQRT(P3(I))
        P3(I) = MAX(ZERO, GAPV(I) - D3)
C
        D4 = SQRT(P4(I))
        P4(I) = MAX(ZERO, GAPV(I) - D4)
       ENDDO
      ENDIF !(IMP_INT7.EQ.3) 
C
       DO I=1,JLT
        IF(IX3(I).NE.IX4(I))THEN
         PENE(I) = MAX(P1(I),P2(I),P3(I),P4(I))
C
         LA1 = UN - LB1(I) - LC1(I)
         LA2 = UN - LB2(I) - LC2(I)
         LA3 = UN - LB3(I) - LC3(I)
         LA4 = UN - LB4(I) - LC4(I) 
C
         H0    = QUART * 
     .         (P1(I)*LA1 + P2(I)*LA2 + P3(I)*LA3 + P4(I)*LA4)
         H1(I) = H0 + P1(I) * LB1(I) + P4(I) * LC4(I)
         H2(I) = H0 + P2(I) * LB2(I) + P1(I) * LC1(I)
         H3(I) = H0 + P3(I) * LB3(I) + P2(I) * LC2(I)
         H4(I) = H0 + P4(I) * LB4(I) + P3(I) * LC3(I)
         H00    = UN/MAX(EM20,H1(I) + H2(I) + H3(I) + H4(I))
         H1(I) = H1(I) * H00
         H2(I) = H2(I) * H00
         H3(I) = H3(I) * H00
         H4(I) = H4(I) * H00
C
        ELSE
         PENE(I) = P1(I)
         N1(I) = NX1(I)
         N2(I) = NY1(I)
         N3(I) = NZ1(I)
         H1(I) = LB1(I)
         H2(I) = LC1(I)
         H3(I) = UN - LB1(I) - LC1(I)
         H4(I) = ZERO
        ENDIF
       ENDDO
C
Cqz      DO I=1,JLT
Cqz         S2 = UN/MAX(EM30,SQRT(N1(I)**2 + N2(I)**2 + N3(I)**2))
Cqz         N1(I) = N1(I)*S2
Cqz         N2(I) = N2(I)*S2
Cqz         N3(I) = N3(I)*S2
Cqz      ENDDO
C
      DO I=1,JLT
C       correction hourglass
        IF(IX3(I).NE.IX4(I))THEN
          H0 = -QUART*(H1(I) - H2(I) + H3(I) - H4(I))
          H0 = MIN(H0,H2(I),H4(I))
          H0 = MAX(H0,-H1(I),-H3(I))
          H1(I) = H1(I) + H0
          H2(I) = H2(I) - H0
          H3(I) = H3(I) + H0
          H4(I) = H4(I) - H0
        ENDIF
      ENDDO
C-------------------------------------------
      DO I=1,JLT
       II = INDEX(I)
       IF(CAND_F(1,II).EQ.ZERO)THEN
C------------------------------------
C       1ER IMPACT ou PAS d'IMPACT
C------------------------------------
Cqz c'est fait dans i10for3.F        CAND_F(4,II) = H1(I)
Cqz        CAND_F(5,II) = H2(I)
Cqz        CAND_F(6,II) = H3(I)
       ELSE
C------------------------------------
C       IMPACTS SUIVANTS 
C------------------------------------
        H1(I) = CAND_F(4,II)
        H2(I) = CAND_F(5,II)
        H3(I) = CAND_F(6,II)
        H4(I) = UN - H1(I) - H2(I) - H3(I)
       ENDIF
      ENDDO 
C
      DO I=1,JLT
        VX(I) = VXI(I) - H1(I)*V(1,IX1(I)) - H2(I)*V(1,IX2(I))
     .                 - H3(I)*V(1,IX3(I)) - H4(I)*V(1,IX4(I))
        VY(I) = VYI(I) - H1(I)*V(2,IX1(I)) - H2(I)*V(2,IX2(I))
     .                 - H3(I)*V(2,IX3(I)) - H4(I)*V(2,IX4(I))
        VZ(I) = VZI(I) - H1(I)*V(3,IX1(I)) - H2(I)*V(3,IX2(I))
     .                 - H3(I)*V(3,IX3(I)) - H4(I)*V(3,IX4(I))
      ENDDO
C 
      DO I=1,JLT
        T1X(I) = X3(I) - X1(I)
        T1Y(I) = Y3(I) - Y1(I)
        T1Z(I) = Z3(I) - Z1(I)
        NORMINV = UN/SQRT(T1X(I)**2+T1Y(I)**2+T1Z(I)**2)
        T1X(I) = T1X(I)*NORMINV
        T1Y(I) = T1Y(I)*NORMINV
        T1Z(I) = T1Z(I)*NORMINV
C
        T2X(I) = X4(I) - X2(I)
        T2Y(I) = Y4(I) - Y2(I)
        T2Z(I) = Z4(I) - Z2(I)
C
        NX(I) = T1Y(I)*T2Z(I) - T1Z(I)*T2Y(I)
        NY(I) = T1Z(I)*T2X(I) - T1X(I)*T2Z(I)
        NZ(I) = T1X(I)*T2Y(I) - T1Y(I)*T2X(I)
        NORMINV = UN/SQRT(NX(I)**2+NY(I)**2+NZ(I)**2)
        NX(I) = NX(I)*NORMINV
        NY(I) = NY(I)*NORMINV
        NZ(I) = NZ(I)*NORMINV
C
        T2X(I) = NY(I)*T1Z(I) - NZ(I)*T1Y(I)
        T2Y(I) = NZ(I)*T1X(I) - NX(I)*T1Z(I)
        T2Z(I) = NX(I)*T1Y(I) - NY(I)*T1X(I)
C
        VN(I)  = VX(I)*NX(I)  + VY(I)*NY(I)  + VZ(I)*NZ(I)  
        VT1(I) = VX(I)*T1X(I) + VY(I)*T1Y(I) + VZ(I)*T1Z(I)  
        VT2(I) = VX(I)*T2X(I) + VY(I)*T2Y(I) + VZ(I)*T2Z(I)  
      ENDDO
C
      DO I=1,JLT
      IF(PENE(I).EQ.ZERO.AND.CAND_F(1,INDEX(I)).EQ.ZERO)THEN
C------------------------------------
C       PAS ENCORE D'IMPACT OU REBOND
C------------------------------------
        VN(I)  = ZERO
        VT1(I) = ZERO
        VT2(I) = ZERO
      ENDIF
      ENDDO
C
      DO I=1,JLT
        II = INDEX(I)
        FNI(I) = CAND_F(1,II) + VN(I)  * DT1 * STIF(I)
      ENDDO
C
      DO 100 I=1,JLT
      IF(ITIED.EQ.0)THEN
          IF(CAND_F(1,INDEX(I))*FNI(I).LT.ZERO)THEN
C------------------------------------
C           REBOND
C------------------------------------
            FNI(I) = ZERO
            VN(I)  = ZERO
            VT1(I) = ZERO
            VT2(I) = ZERO    
            STIF(I) = ZERO 
          ELSE
C--------
          ENDIF
      ELSE
           STIF(I) = STIF(I) * ABS(VN(I)) * DT1/MAX(PENE(I),EM10) 
      ENDIF
C    
 100  CONTINUE
C
C---------------------------------
C    ----sans frottement d'abord--- 
      DO I=1,JLT
        IF (ABS(VT1(I)).GT.ZERO.OR.ABS(VT2(I)).GT.ZERO) THEN
         Q(1,1,I)=T1X(I)
         Q(1,2,I)=T1Y(I)
         Q(1,3,I)=T1Z(I)
         Q(3,1,I)=NX(I)
         Q(3,2,I)=NY(I)
         Q(3,3,I)=NZ(I)
         Q(2,1,I)=T2X(I)
         Q(2,2,I)=T2Y(I)
         Q(2,3,I)=T2Z(I)
         FACT(I)=FRIC
        ELSE
         FACT(I)=ZERO
        ENDIF
      ENDDO
      IF (SCALK.LT.0) THEN
       ISF=1
      ELSE
       ISF=0
      ENDIF
      FACF=FAC10*ABS(SCALK)
      IF (ISF.EQ.1) THEN
       DO I=1,JLT
        IF (VN(I).GT.ZERO) THEN
         FAC=STIF(I)*FACF
        ELSEIF (VN(I).LT.ZERO) THEN
         FAC=STIF(I)/FACF
        ELSE
         FAC=STIF(I)
        ENDIF
        KN(1,I)=FAC*H1(I)
        KN(2,I)=FAC*H2(I)
        KN(3,I)=FAC*H3(I)
        KN(4,I)=FAC*H4(I)
        FACT(I)=FAC*FACT(I)
       ENDDO
      ELSE
       DO I=1,JLT
        FAC=STIF(I)*FACF
        KN(1,I)=FAC*H1(I)
        KN(2,I)=FAC*H2(I)
        KN(3,I)=FAC*H3(I)
        KN(4,I)=FAC*H4(I)
        FACT(I)=FAC*FACT(I)
       ENDDO
      ENDIF
      DO I=1,JLT
       Q11=NX(I)*NX(I)
       Q12=NX(I)*NY(I)
       Q13=NX(I)*NZ(I)
       Q22=NY(I)*NY(I)
       Q23=NY(I)*NZ(I)
       Q33=NZ(I)*NZ(I)
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
       DO J=1,3 
        DO K=J,3 
         DO I=1,JLT
          IF (FACT(I).GT.ZERO) THEN
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
          ENDIF 
         ENDDO
        ENDDO
       ENDDO
C
       DO J=1,3 
        DO K=J,3 
         DO I=1,JLT
          KI12(J,K,I)=-KI11(J,K,I)
          KJ12(J,K,I)=-KJ11(J,K,I)
          KK12(J,K,I)=-KK11(J,K,I)
          KL12(J,K,I)=-KL11(J,K,I)
         ENDDO
        ENDDO
       ENDDO
       DO J=1,3 
        DO K=J+1,3 
         DO I=1,JLT
          KI12(K,J,I)=-KI11(J,K,I)
          KJ12(K,J,I)=-KJ11(J,K,I)
          KK12(K,J,I)=-KK11(J,K,I)
          KL12(K,J,I)=-KL11(J,K,I)
         ENDDO
        ENDDO
       ENDDO
C
       DO I=1,JLT
        OFF(I)=UN
       ENDDO
       IF (IMACH.EQ.3.AND.NSPMD.GT.1) THEN
       IF ((INTP_D).GT.0) THEN
        DO I=1,JLT
         IF(NSVG(I).LT.0) THEN
	  NN=-NSVG(I)
	  NS=IND_INT(NIN)%P(NN)
C---------pour diag_ss---
	  FFI(1,NS)=ZERO
	  FFI(2,NS)=ZERO
	  FFI(3,NS)=ZERO
	  DFI(1,NS)=ZERO
	  DFI(2,NS)=ZERO
	  DFI(3,NS)=ZERO
	 ENDIF
        ENDDO
       ELSE
        JLTF = 0
        DO I=1,JLT
         IF(NSVG(I).LT.0) THEN
	  NN=-NSVG(I)
          JLTF = JLTF + 1
	  NE=SHF_INT(NIN) + JLTF +LREM
	  NS=IND_INT(NIN)%P(NN)
	  STIFS(NE)=STIF(I)
	  H_E(1,NE)=H1(I)
	  H_E(2,NE)=H2(I)
	  H_E(3,NE)=H3(I)
	  H_E(4,NE)=H4(I)
	  N_E(1,NE)=NX(I)
	  N_E(2,NE)=NY(I)
	  N_E(3,NE)=NZ(I)
C---------pour temporairement diag_ss---
	  FFI(1,NS)=ZERO
	  FFI(2,NS)=ZERO
	  FFI(3,NS)=ZERO
	  DFI(1,NS)=ZERO
	  DFI(2,NS)=ZERO
	  DFI(3,NS)=ZERO
	 ENDIF
        ENDDO
       ENDIF 
       ENDIF 
C
       RETURN
      END
Chd|====================================================================
Chd|  I10FRF3                       source/interfaces/int10/i10keg3.F
Chd|-- called by -----------
Chd|        I10FORCF3                     source/interfaces/int10/i10ke3.F
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE I10FRF3(JLT    ,A      ,V      ,MS     ,FRIC   ,
     1                   N1     ,N2     ,N3     ,T1X    ,T1Y    ,
     2                   T1Z    ,H1     ,H2     ,H3     ,H4     ,
     3                   IX1    ,IX2    ,IX3    ,IX4   ,INDEX   ,
     4                   VXI    ,VYI    ,VZI    ,MSI   ,DXI     ,
     5                   DYI    ,DZI    ,STIF   ,NIN   ,D       ,
     6                   SCALK  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
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
      INTEGER JLT, INACTI,NIN
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        INDEX(MVSIZ)
      my_real
     .   A(3,*), MS(*), V(3,*),D(3,*),
     .   FRIC,SCALK,DXI(MVSIZ),DYI(MVSIZ),DZI(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ)
      my_real
     .     N1(MVSIZ), N2(MVSIZ), N3(MVSIZ),STIF(MVSIZ),
     .     T1X(MVSIZ), T1Y(MVSIZ), T1Z(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, J, K,IG,ISF,NN,NS,NI
      my_real
     .   VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ), 
     .   DX(MVSIZ), DY(MVSIZ), DZ(MVSIZ), DN(MVSIZ), 
     .   DNI(MVSIZ),D1T(MVSIZ),D2T(MVSIZ), DTI1(MVSIZ), 
     .   DTI2(MVSIZ),S2,FACN(MVSIZ),FACF, FACT(MVSIZ),FAC10
      my_real
     .   FX,FY,FZ,FN,FT1,FT2,FNI,FTI1,FTI2,VTX,VTY,VTZ,VT,
     .   T2X(MVSIZ), T2Y(MVSIZ), T2Z(MVSIZ),
     .   KT1,KT2,KT3,KT4,Q1,Q2

C-----------------------------------------------
C
      FAC10 = DIX
      DO I=1,JLT
        VX(I) = VXI(I) - H1(I)*V(1,IX1(I)) - H2(I)*V(1,IX2(I))
     .                 - H3(I)*V(1,IX3(I)) - H4(I)*V(1,IX4(I))
        VY(I) = VYI(I) - H1(I)*V(2,IX1(I)) - H2(I)*V(2,IX2(I))
     .                 - H3(I)*V(2,IX3(I)) - H4(I)*V(2,IX4(I))
        VZ(I) = VZI(I) - H1(I)*V(3,IX1(I)) - H2(I)*V(3,IX2(I))
     .                 - H3(I)*V(3,IX3(I)) - H4(I)*V(3,IX4(I))
        VN(I) = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
        DX(I) = DXI(I) - H1(I)*D(1,IX1(I)) - H2(I)*D(1,IX2(I))
     .                 - H3(I)*D(1,IX3(I)) - H4(I)*D(1,IX4(I))
        DY(I) = DYI(I) - H1(I)*D(2,IX1(I)) - H2(I)*D(2,IX2(I))
     .                 - H3(I)*D(2,IX3(I)) - H4(I)*D(2,IX4(I))
        DZ(I) = DZI(I) - H1(I)*D(3,IX1(I)) - H2(I)*D(3,IX2(I))
     .                 - H3(I)*D(3,IX3(I)) - H4(I)*D(3,IX4(I))
        DN(I) = N1(I)*DX(I) + N2(I)*DY(I) + N3(I)*DZ(I)
        DNI(I) = N1(I)*DXI(I) + N2(I)*DYI(I) + N3(I)*DZI(I)
      ENDDO
C-------------------------------------------
      DO I=1,JLT
        T2X(I) = N2(I)*T1Z(I) - N3(I)*T1Y(I)
        T2Y(I) = N3(I)*T1X(I) - N1(I)*T1Z(I)
        T2Z(I) = N1(I)*T1Y(I) - N2(I)*T1X(I)
        D1T(I) = T1X(I)*DX(I) + T1Y(I)*DY(I) + T1Z(I)*DZ(I)
        D2T(I) = T2X(I)*DX(I) + T2Y(I)*DY(I) + T2Z(I)*DZ(I)
        DTI1(I) = T1X(I)*DXI(I) + T1Y(I)*DYI(I) + T1Z(I)*DZI(I)
        DTI2(I) = T2X(I)*DXI(I) + T2Y(I)*DYI(I) + T2Z(I)*DZI(I)
      ENDDO
      IF (SCALK.LT.0) THEN
       ISF=1
      ELSE
       ISF=0
      ENDIF
      FACF=FAC10*ABS(SCALK)
      IF (ISF.EQ.1) THEN
       DO I=1,JLT
        IF (VN(I).GT.ZERO) THEN
         FACN(I)=STIF(I)*FACF
        ELSEIF (VN(I).LT.ZERO) THEN
         FACN(I)=STIF(I)/FACF
        ELSE
         FACN(I)=STIF(I)
        ENDIF
        FACT(I)=FACN(I)*FRIC
       ENDDO
      ELSE
       DO I=1,JLT
        FACN(I)=STIF(I)*FACF
        FACT(I)=FACN(I)*FRIC
       ENDDO
      ENDIF
C--------partie NML-------
      DO I=1,JLT
        FN = -FACN(I)*DNI(I)
        FX=FN*N1(I)
        FY=FN*N2(I)
        FZ=FN*N3(I)
        FT1 = -FACT(I)*DTI1(I)
        FT2 = -FACT(I)*DTI2(I)
        FX = FX + FT1*T1X(I)+ FT2*T2X(I)
        FY = FY + FT1*T1Y(I)+ FT2*T2Y(I)
        FZ = FZ + FT1*T1Z(I)+ FT2*T2Z(I)
        A(1,IX1(I))=A(1,IX1(I))+FX*H1(I)
        A(1,IX2(I))=A(1,IX2(I))+FX*H2(I)
        A(1,IX3(I))=A(1,IX3(I))+FX*H3(I)
        A(1,IX4(I))=A(1,IX4(I))+FX*H4(I)
        A(2,IX1(I))=A(2,IX1(I))+FY*H1(I)
        A(2,IX2(I))=A(2,IX2(I))+FY*H2(I)
        A(2,IX3(I))=A(2,IX3(I))+FY*H3(I)
        A(2,IX4(I))=A(2,IX4(I))+FY*H4(I)
        A(3,IX1(I))=A(3,IX1(I))+FZ*H1(I)
        A(3,IX2(I))=A(3,IX2(I))+FZ*H2(I)
        A(3,IX3(I))=A(3,IX3(I))+FZ*H3(I)
        A(3,IX4(I))=A(3,IX4(I))+FZ*H4(I)
      ENDDO
C--------partie NSL-------
      DO I=1,JLT
        FNI = FACN(I)*DN(I)
        FX=FNI*N1(I)
        FY=FNI*N2(I)
        FZ=FNI*N3(I)
        FTI1 = FACT(I)*D1T(I)
        FTI2 = FACT(I)*D2T(I)
        FX = FX + FTI1*T1X(I)+ FTI2*T2X(I)
        FY = FY + FTI1*T1Y(I)+ FTI2*T2Y(I)
        FZ = FZ + FTI1*T1Z(I)+ FTI2*T2Z(I)
        NI = INDEX(I)
        FFI(1,NI)=FFI(1,NI)+FX
        FFI(2,NI)=FFI(2,NI)+FY
        FFI(3,NI)=FFI(3,NI)+FZ
      ENDDO
C
       RETURN
      END
Chd|====================================================================
Chd|  I10KFOR3                      source/interfaces/int10/i10keg3.F
Chd|-- called by -----------
Chd|        I10FKU3                       source/interfaces/int10/i10ke3.F
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE I10KFOR3(JLT    ,A      ,V      ,MS    ,CAND_F ,
     1                    STIF   ,ITIED  ,X1     ,X2    ,X3    ,
     2                    X4     ,Y1     ,Y2     ,Y3    ,Y4    ,
     3                    Z1     ,Z2     ,Z3     ,Z4    ,NSVG  ,
     4                    NX1    ,NX2    ,NX3    ,NX4   ,NY1    ,
     5                    NY2    ,NY3    ,NY4    ,NZ1   ,NZ2    ,
     6                    NZ3    ,NZ4    ,LB1    ,LB2   ,LB3    ,
     7                    LB4    ,LC1    ,LC2    ,LC3   ,LC4    ,
     8                    P1     ,P2     ,P3     ,P4    ,NIN    ,
     9                    IX1    ,IX2    ,IX3    ,IX4   ,GAPV   ,
     C                    INDEX  ,VXI    ,VYI    ,VZI   ,MSI    ,
     L                    CN_LOC ,CE_LOC ,XI     ,YI    ,ZI     ,
     H                    DXI    ,DYI    ,DZI    ,D     ,SCALK  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,NIN,ITIED
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ), INDEX(*),CN_LOC(*), CE_LOC(*)
      my_real
     .   A(3,*), MS(*),X1(*),X2(*),X3(*),X4(*),
     .   Y1(*),Y2(*),Y3(*),Y4(*),Z1(*),Z2(*),Z3(*),Z4(*),
     .   CAND_F(6,*), V(3,*),D(3,*),
     .   VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ)
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .   P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ), STIF(MVSIZ),
     .   GAPV(MVSIZ),
     .     DXI(MVSIZ),DYI(MVSIZ), DZI(MVSIZ), 
     .     XI(MVSIZ),YI(MVSIZ),ZI(MVSIZ),SCALK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, IG, II , K0,NBINTER,K1S,K,J,NN,JG
      INTEGER JJ,KK,IN,IE,NSUB,IBID,NI,NS
      my_real
     .   FXI(MVSIZ), FYI(MVSIZ), FZI(MVSIZ), FNI(MVSIZ),
     .   FX1(MVSIZ), FX2(MVSIZ), FX3(MVSIZ), FX4(MVSIZ),
     .   FY1(MVSIZ), FY2(MVSIZ), FY3(MVSIZ), FY4(MVSIZ),
     .   FZ1(MVSIZ), FZ2(MVSIZ), FZ3(MVSIZ), FZ4(MVSIZ),
     .   FT1(MVSIZ), FT2(MVSIZ),
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PENE(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ), 
     .   VT1(MVSIZ), VT2(MVSIZ), 
     .   NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ),
     .   T1X(MVSIZ),T1Y(MVSIZ),T1Z(MVSIZ),
     .   T2X(MVSIZ),T2Y(MVSIZ),T2Z(MVSIZ),
     .   VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ), 
     .   DX(MVSIZ), DY(MVSIZ), DZ(MVSIZ), DN(MVSIZ),
     .   S2,D1,D2,D3,D4,A1,A2,A3,A4,LA1,LA2,LA3,LA4,H0,
     .   NORMINV,BID,GAP2,PENE2,FAC,FX,FY,FZ
      my_real
     .   FXN(MVSIZ), FYN(MVSIZ), FZN(MVSIZ), 
     .   FXT(MVSIZ), FYT(MVSIZ), FZT(MVSIZ) 
C--------------------------------------------------------
C  actualise stif
C--------------------------------------------------------
       DO I=1,JLT
        GAP2=GAPV(I)*GAPV(I)
C
        D1 = MAX(ZERO, GAP2 - P1(I))
        D2 = MAX(ZERO, GAP2 - P2(I))
        D3 = MAX(ZERO, GAP2 - P3(I))
        D4 = MAX(ZERO, GAP2 - P4(I))
        PENE2 = MAX(D1,D2,D3,D4)
        IF (PENE2.LE.ZERO) STIF(I) = ZERO
       ENDDO
C--------------------------------------------------------
C  CAS DES PAQUETS MIXTES
C--------------------------------------------------------
       DO I=1,JLT
        D1 = SQRT(P1(I))
        P1(I) = MAX(ZERO, GAPV(I) - D1)
C
        D2 = SQRT(P2(I))
        P2(I) = MAX(ZERO, GAPV(I) - D2)
C
        D3 = SQRT(P3(I))
        P3(I) = MAX(ZERO, GAPV(I) - D3)
C
        D4 = SQRT(P4(I))
        P4(I) = MAX(ZERO, GAPV(I) - D4)
       ENDDO
C
       DO I=1,JLT
        IF(IX3(I).NE.IX4(I))THEN
         PENE(I) = MAX(P1(I),P2(I),P3(I),P4(I))
C
         LA1 = UN - LB1(I) - LC1(I)
         LA2 = UN - LB2(I) - LC2(I)
         LA3 = UN - LB3(I) - LC3(I)
         LA4 = UN - LB4(I) - LC4(I) 
C
         H0    = QUART * 
     .         (P1(I)*LA1 + P2(I)*LA2 + P3(I)*LA3 + P4(I)*LA4)
         H1(I) = H0 + P1(I) * LB1(I) + P4(I) * LC4(I)
         H2(I) = H0 + P2(I) * LB2(I) + P1(I) * LC1(I)
         H3(I) = H0 + P3(I) * LB3(I) + P2(I) * LC2(I)
         H4(I) = H0 + P4(I) * LB4(I) + P3(I) * LC3(I)
         H0    = UN/MAX(EM20,H1(I) + H2(I) + H3(I) + H4(I))
         H1(I) = H1(I) * H0
         H2(I) = H2(I) * H0
         H3(I) = H3(I) * H0
         H4(I) = H4(I) * H0
C
        ELSE
         PENE(I) = P1(I)
         N1(I) = NX1(I)
         N2(I) = NY1(I)
         N3(I) = NZ1(I)
         H1(I) = LB1(I)
         H2(I) = LC1(I)
         H3(I) = UN - LB1(I) - LC1(I)
         H4(I) = ZERO
        ENDIF
       ENDDO
C      
      DO I=1,JLT
C       correction hourglass
        IF(IX3(I).NE.IX4(I))THEN
          H0 = -QUART*(H1(I) - H2(I) + H3(I) - H4(I))
          H0 = MIN(H0,H2(I),H4(I))
          H0 = MAX(H0,-H1(I),-H3(I))
          H1(I) = H1(I) + H0
          H2(I) = H2(I) - H0
          H3(I) = H3(I) + H0
          H4(I) = H4(I) - H0
        ENDIF
      ENDDO
C      
       DO I=1,JLT
        DX(I) = DXI(I) - H1(I)*D(1,IX1(I)) - H2(I)*D(1,IX2(I))
     .                 - H3(I)*D(1,IX3(I)) - H4(I)*D(1,IX4(I))
        DY(I) = DYI(I) - H1(I)*D(2,IX1(I)) - H2(I)*D(2,IX2(I))
     .                 - H3(I)*D(2,IX3(I)) - H4(I)*D(2,IX4(I))
        DZ(I) = DZI(I) - H1(I)*D(3,IX1(I)) - H2(I)*D(3,IX2(I))
     .                 - H3(I)*D(3,IX3(I)) - H4(I)*D(3,IX4(I))
       ENDDO
C       
      DO I=1,JLT
       II = INDEX(I)
       IF(CAND_F(1,II).EQ.ZERO)THEN
C------------------------------------
C       1ER IMPACT ou PAS d'IMPACT
C------------------------------------
       ELSE
C------------------------------------
C       IMPACTS SUIVANTS 
C------------------------------------
        H1(I) = CAND_F(4,II)
        H2(I) = CAND_F(5,II)
        H3(I) = CAND_F(6,II)
        H4(I) = UN - H1(I) - H2(I) - H3(I)
       ENDIF
      ENDDO 
C      
      DO I=1,JLT
        T1X(I) = X3(I) - X1(I)
        T1Y(I) = Y3(I) - Y1(I)
        T1Z(I) = Z3(I) - Z1(I)
        NORMINV = UN/SQRT(T1X(I)**2+T1Y(I)**2+T1Z(I)**2)
        T1X(I) = T1X(I)*NORMINV
        T1Y(I) = T1Y(I)*NORMINV
        T1Z(I) = T1Z(I)*NORMINV
C
        T2X(I) = X4(I) - X2(I)
        T2Y(I) = Y4(I) - Y2(I)
        T2Z(I) = Z4(I) - Z2(I)
C
        NX(I) = T1Y(I)*T2Z(I) - T1Z(I)*T2Y(I)
        NY(I) = T1Z(I)*T2X(I) - T1X(I)*T2Z(I)
        NZ(I) = T1X(I)*T2Y(I) - T1Y(I)*T2X(I)
        NORMINV = UN/SQRT(NX(I)**2+NY(I)**2+NZ(I)**2)
        NX(I) = NX(I)*NORMINV
        NY(I) = NY(I)*NORMINV
        NZ(I) = NZ(I)*NORMINV
C
        T2X(I) = NY(I)*T1Z(I) - NZ(I)*T1Y(I)
        T2Y(I) = NZ(I)*T1X(I) - NX(I)*T1Z(I)
        T2Z(I) = NX(I)*T1Y(I) - NY(I)*T1X(I)
C
        DN(I) = NX(I)*DX(I) + NY(I)*DY(I) + NZ(I)*DZ(I)
        VT1(I) = DX(I)*T1X(I) + DY(I)*T1Y(I) + DZ(I)*T1Z(I)  
        VT2(I) = DX(I)*T2X(I) + DY(I)*T2Y(I) + DZ(I)*T2Z(I)  
      ENDDO
       FAC =   ABS(SCALK)   
       DO I=1,JLT
        STIF(I)=STIF(I)*FAC
       ENDDO
C
      DO I=1,JLT
       IF(PENE(I).EQ.ZERO.AND.CAND_F(1,INDEX(I)).EQ.ZERO)THEN
C------------------------------------
C       PAS ENCORE D'IMPACT OU REBOND
C------------------------------------
        DN(I)  = ZERO
       ENDIF
      ENDDO
C
      DO I=1,JLT
        II = INDEX(I)
        FNI(I) = CAND_F(1,II) + DN(I)  * STIF(I)
        FT1(I) = CAND_F(2,II) + VT1(I) * STIF(I)
        FT2(I) = CAND_F(3,II) + VT2(I) * STIF(I)
      ENDDO
C
      DO 100 I=1,JLT
      IF(ITIED.EQ.0)THEN
          IF(CAND_F(1,INDEX(I))*FNI(I).LT.ZERO)THEN
C------------------------------------
C           REBOND
C------------------------------------
            FNI(I) = ZERO
            DN(I)  = ZERO
            STIF(I) = ZERO 
            FT1(I) = ZERO
            FT2(I) = ZERO
          ELSE
C--------
          ENDIF
      ENDIF
C    
 100  CONTINUE
C-------------------------------------------
      DO I=1,JLT
        II = INDEX(I)
        FXN(I)= NX(I)*FNI(I)
        FYN(I)= NY(I)*FNI(I)
        FZN(I)= NZ(I)*FNI(I)
        FXT(I)= T1X(I)*FT1(I) + T2X(I)*FT2(I)
        FYT(I)= T1Y(I)*FT1(I) + T2Y(I)*FT2(I)
        FZT(I)= T1Z(I)*FT1(I) + T2Z(I)*FT2(I)
        FXI(I) = FXN(I) + FXT(I)
        FYI(I) = FYN(I) + FYT(I)
        FZI(I) = FZN(I) + FZT(I) 
      ENDDO
C--------main part-------
       DO I=1,JLT
        FX=FXI(I)
        FY=FYI(I)
        FZ=FZI(I)
        A(1,IX1(I))=A(1,IX1(I))+FX*H1(I)
        A(1,IX2(I))=A(1,IX2(I))+FX*H2(I)
        A(1,IX3(I))=A(1,IX3(I))+FX*H3(I)
        A(1,IX4(I))=A(1,IX4(I))+FX*H4(I)
        A(2,IX1(I))=A(2,IX1(I))+FY*H1(I)
        A(2,IX2(I))=A(2,IX2(I))+FY*H2(I)
        A(2,IX3(I))=A(2,IX3(I))+FY*H3(I)
        A(2,IX4(I))=A(2,IX4(I))+FY*H4(I)
        A(3,IX1(I))=A(3,IX1(I))+FZ*H1(I)
        A(3,IX2(I))=A(3,IX2(I))+FZ*H2(I)
        A(3,IX3(I))=A(3,IX3(I))+FZ*H3(I)
        A(3,IX4(I))=A(3,IX4(I))+FZ*H4(I)
       ENDDO
C--------secnd part-------
      IF(IMACH.NE.3) THEN
        DO I=1,JLT
          IG=NSVG(I)
          A(1,IG)=A(1,IG)-FXI(I)
          A(2,IG)=A(2,IG)-FYI(I)
          A(3,IG)=A(3,IG)-FZI(I)
        ENDDO
       ELSE 
        DO I=1,JLT
          IG=NSVG(I)
          IF(IG.GT.0)THEN
           A(1,IG)=A(1,IG)-FXI(I)
           A(2,IG)=A(2,IG)-FYI(I)
           A(3,IG)=A(3,IG)-FZI(I)
	  ELSE
	   NN=-IG
	   NS=IND_INT(NIN)%P(NN)
C---------
	   FFI(1,NS)=FFI(1,NS)-FXI(I)
	   FFI(2,NS)=FFI(2,NS)-FYI(I)
	   FFI(3,NS)=FFI(3,NS)-FZI(I)
          ENDIF
        ENDDO
       END IF
C
      RETURN
      END
