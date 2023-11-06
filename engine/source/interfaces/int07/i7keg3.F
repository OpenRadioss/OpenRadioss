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
Chd|  I7KEG3                        source/interfaces/int07/i7keg3.F
Chd|-- called by -----------
Chd|        I7KE3                         source/interfaces/int07/i7ke3.F
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F
Chd|====================================================================
      SUBROUTINE I7KEG3(JLT    ,A      ,V      ,MS    ,FRIC   ,
     1                  NX1    ,NX2    ,NX3    ,NX4   ,NY1    ,
     2                  NY2    ,NY3    ,NY4    ,NZ1   ,NZ2    ,
     3                  NZ3    ,NZ4    ,LB1    ,LB2   ,LB3    ,
     4                  LB4    ,LC1    ,LC2    ,LC3   ,LC4    ,
     5                  P1     ,P2     ,P3     ,P4    ,NIN    ,
     6                  IX1    ,IX2    ,IX3    ,IX4   ,NSVG   ,
     7                  GAPV   ,INACTI ,CAND_P ,INDEX ,STIGLO ,
     8                  STIF   ,VXI    ,VYI    ,VZI   ,MSI    ,
     9                  KI11   ,KI12   ,KJ11  ,KJ12   ,KK11   ,
     A                  KK12   ,KL11   ,KL12  ,OFF    ,SCALK  ,
     B                  LREM   ,ICURV  ,X      ,
     2                  X1     ,X2     ,X3     ,X4     ,Y1     ,
     3                  Y2     ,Y3     ,Y4     ,Z1     ,Z2     ,
     4                  Z3     ,Z4     ,XI     ,YI     ,ZI     )
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
Ctmp+1
#include      "com01_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER JLT, INACTI,NIN,LREM
        INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .          NSVG(MVSIZ), INDEX(MVSIZ),ICURV(3)
        my_real
     .     A(3,*), MS(*), V(3,*),
     .     STIGLO,CAND_P(*),FRIC,OFF(*),SCALK,
     .     VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ)
        my_real
     .       NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .       NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .       NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .       LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .       LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .      P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ), STIF(MVSIZ),
     .      GAPV(MVSIZ),KI11(3,3,MVSIZ),KJ11(3,3,MVSIZ),
     .      KK11(3,3,MVSIZ),KL11(3,3,MVSIZ),KI12(3,3,MVSIZ),
     .      KJ12(3,3,MVSIZ),KK12(3,3,MVSIZ),KL12(3,3,MVSIZ)
        my_real
     .       X1(MVSIZ),Y1(MVSIZ),Z1(MVSIZ),
     .       X2(MVSIZ),Y2(MVSIZ),Z2(MVSIZ),
     .       X3(MVSIZ),Y3(MVSIZ),Z3(MVSIZ),
     .       X4(MVSIZ),Y4(MVSIZ),Z4(MVSIZ),
     .       XI(MVSIZ),YI(MVSIZ),ZI(MVSIZ),X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J1, J, K,IG,ISF,NN,NS,JLTF,NE
        my_real
     .     N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PENE(MVSIZ),
     .     H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .     VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ),
     .     S2,FAC,FACF, H0, LA1, LA2, LA3, LA4,FACT(MVSIZ),
     .     D1,D2,D3,D4,A1,A2,A3,A4,KN(4,MVSIZ),Q(3,3,MVSIZ)
        my_real
     .     PREC,Q11,Q12,Q13,Q22,Q23,Q33,H00,VTX,VTY,VTZ,VT,
     .     KT1,KT2,KT3,KT4,Q1,Q2
        INTEGER NA1,NA2
        my_real
     .     A0X,A0Y,A0Z,RX,RY,RZ,
     .     ANX,ANY,ANZ,AAN,AAX,AAY,AAZ ,RR,RS,AAA ,TM,TS
C-----------------------------------------------
C  dev021
        IF (IRESP==1) THEN
          PREC = FIVEEM4
        ELSE
          PREC = EM10
        ENDIF
C--------------------------------------------------------
C  CAS DES PAQUETS MIXTES
C--------------------------------------------------------
        IF (IMP_INT7==3) THEN
          DO I=1,JLT
            D1 = SQRT(P1(I))
            P1(I) = FOURTH*GAPV(I)
            D2 = SQRT(P2(I))
            P2(I) = FOURTH*GAPV(I)
            D3 = SQRT(P3(I))
            P3(I) = FOURTH*GAPV(I)
            D4 = SQRT(P4(I))
            P4(I) = FOURTH*GAPV(I)
            A1 = P1(I)/MAX(EM20,D1)
            A2 = P2(I)/MAX(EM20,D2)
            A3 = P3(I)/MAX(EM20,D3)
            A4 = P4(I)/MAX(EM20,D4)
            N1(I) = A1*NX1(I) + A2*NX2(I) + A3*NX3(I) + A4*NX4(I)
            N2(I) = A1*NY1(I) + A2*NY2(I) + A3*NY3(I) + A4*NY4(I)
            N3(I) = A1*NZ1(I) + A2*NZ2(I) + A3*NZ3(I) + A4*NZ4(I)
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
C
            A1 = P1(I)/MAX(EM20,D1)
            A2 = P2(I)/MAX(EM20,D2)
            A3 = P3(I)/MAX(EM20,D3)
            A4 = P4(I)/MAX(EM20,D4)
            N1(I) = A1*NX1(I) + A2*NX2(I) + A3*NX3(I) + A4*NX4(I)
            N2(I) = A1*NY1(I) + A2*NY2(I) + A3*NY3(I) + A4*NY4(I)
            N3(I) = A1*NZ1(I) + A2*NZ2(I) + A3*NZ3(I) + A4*NZ4(I)
          ENDDO
        ENDIF !(IMP_INT7==3)
C
        DO I=1,JLT
          IF(IX3(I)/=IX4(I))THEN
            PENE(I) = MAX(P1(I),P2(I),P3(I),P4(I))
C
            LA1 = ONE - LB1(I) - LC1(I)
            LA2 = ONE - LB2(I) - LC2(I)
            LA3 = ONE - LB3(I) - LC3(I)
            LA4 = ONE - LB4(I) - LC4(I)
C
            H0    = FOURTH *
     .            (P1(I)*LA1 + P2(I)*LA2 + P3(I)*LA3 + P4(I)*LA4)
            H1(I) = H0 + P1(I) * LB1(I) + P4(I) * LC4(I)
            H2(I) = H0 + P2(I) * LB2(I) + P1(I) * LC1(I)
            H3(I) = H0 + P3(I) * LB3(I) + P2(I) * LC2(I)
            H4(I) = H0 + P4(I) * LB4(I) + P3(I) * LC3(I)
            H00    = ONE/MAX(EM20,H1(I) + H2(I) + H3(I) + H4(I))
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
            H3(I) = ONE - LB1(I) - LC1(I)
            H4(I) = ZERO
          ENDIF
        ENDDO
C---------------------
C     COURBURE FIXE
C---------------------
        IF(ICURV(1)==1)THEN
C       spherique (que concave pour le moment)
          NA1 = ICURV(2)
          DO I=1,JLT
            RR = 1.E30
            A0X = X(1,NA1)
            A0Y = X(2,NA1)
            A0Z = X(3,NA1)
C
            RX = X1(I)-A0X
            RY = Y1(I)-A0Y
            RZ = Z1(I)-A0Z
            RR = MIN(RR , RX*RX + RY*RY + RZ*RZ)
            RX = X2(I)-A0X
            RY = Y2(I)-A0Y
            RZ = Z2(I)-A0Z
            RR = MIN(RR , RX*RX + RY*RY + RZ*RZ)
            RX = X3(I)-A0X
            RY = Y3(I)-A0Y
            RZ = Z3(I)-A0Z
            RR = MIN(RR , RX*RX + RY*RY + RZ*RZ)
            IF(IX3(I)/=IX4(I))THEN
              RX = X4(I)-A0X
              RY = Y4(I)-A0Y
              RZ = Z4(I)-A0Z
              RR = MIN(RR , RX*RX + RY*RY + RZ*RZ)
            ENDIF
            RX = XI(I)-A0X
            RY = YI(I)-A0Y
            RZ = ZI(I)-A0Z
            RS = SQRT(RX*RX + RY*RY + RZ*RZ)
            RR = SQRT(RR)
            IF(RS-RR+GAPV(I)<0.)THEN
              STIF(I) = 0.
              PENE(I) = 0.
            ELSEIF(RS-RR+GAPV(I)<PENE(I))THEN
              PENE(I) = RS-RR+GAPV(I)
            ENDIF
            N1(I) = -RX
            N2(I) = -RY
            N3(I) = -RZ
          ENDDO
        ELSEIF(ICURV(1)==2)THEN
C       cylindrique (que concave pour le moment)
          NA1 = ICURV(2)
          NA2 = ICURV(3)
          DO I=1,JLT
            RR = 1.E30
            A0X = X(1,NA1)
            A0Y = X(2,NA1)
            A0Z = X(3,NA1)
            ANX = X(1,NA2)-A0X
            ANY = X(2,NA2)-A0Y
            ANZ = X(3,NA2)-A0Z
            AAN = 1. / (ANX*ANX + ANY*ANY + ANZ*ANZ)
C
            AAX = X1(I)-A0X
            AAY = Y1(I)-A0Y
            AAZ = Z1(I)-A0Z
            AAA = (AAX*ANX + AAY*ANY + AAZ*ANZ) * AAN
            RX = AAX - AAA * ANX
            RY = AAY - AAA * ANY
            RZ = AAZ - AAA * ANZ
            RR = MIN(RR , RX*RX + RY*RY + RZ*RZ)
            AAX = X2(I)-A0X
            AAY = Y2(I)-A0Y
            AAZ = Z2(I)-A0Z
            AAA = (AAX*ANX + AAY*ANY + AAZ*ANZ) * AAN
            RX = AAX - AAA * ANX
            RY = AAY - AAA * ANY
            RZ = AAZ - AAA * ANZ
            RR = MIN(RR , RX*RX + RY*RY + RZ*RZ)
            AAX = X3(I)-A0X
            AAY = Y3(I)-A0Y
            AAZ = Z3(I)-A0Z
            AAA = (AAX*ANX + AAY*ANY + AAZ*ANZ) * AAN
            RX = AAX - AAA * ANX
            RY = AAY - AAA * ANY
            RZ = AAZ - AAA * ANZ
            RR = MIN(RR , RX*RX + RY*RY + RZ*RZ)
            IF(IX3(I)/=IX4(I))THEN
              AAX = X4(I)-A0X
              AAY = Y4(I)-A0Y
              AAZ = Z4(I)-A0Z
              AAA = (AAX*ANX + AAY*ANY + AAZ*ANZ) * AAN
              RX = AAX - AAA * ANX
              RY = AAY - AAA * ANY
              RZ = AAZ - AAA * ANZ
              RR = MIN(RR , RX*RX + RY*RY + RZ*RZ)
            ENDIF
            AAX = XI(I)-A0X
            AAY = YI(I)-A0Y
            AAZ = ZI(I)-A0Z
            AAA = (AAX*ANX + AAY*ANY + AAZ*ANZ) * AAN
            RX = AAX - AAA * ANX
            RY = AAY - AAA * ANY
            RZ = AAZ - AAA * ANZ
            RS = SQRT(RX*RX + RY*RY + RZ*RZ)
            RR = SQRT(RR)
            IF(RS-RR+GAPV(I)<0.)THEN
              STIF(I) = 0.
              PENE(I) = 0.
            ELSEIF(RS-RR+GAPV(I)<PENE(I))THEN
              PENE(I) = RS-RR+GAPV(I)
              N1(I) = -RX
              N2(I) = -RY
              N3(I) = -RZ
            ENDIF
          ENDDO
        ELSEIF(ICURV(1) == 3)THEN
c        print *,'ICURV=3 NOT AVAILABLE WITH IMPLICIT OPTION'
        ENDIF
C
        DO I=1,JLT
          S2 = ONE/MAX(EM30,SQRT(N1(I)**2 + N2(I)**2 + N3(I)**2))
          N1(I) = N1(I)*S2
          N2(I) = N2(I)*S2
          N3(I) = N3(I)*S2
        ENDDO
C
        DO 500 I=1,JLT
          VX(I) = VXI(I) - H1(I)*V(1,IX1(I)) - H2(I)*V(1,IX2(I))
     .                   - H3(I)*V(1,IX3(I)) - H4(I)*V(1,IX4(I))
          VY(I) = VYI(I) - H1(I)*V(2,IX1(I)) - H2(I)*V(2,IX2(I))
     .                   - H3(I)*V(2,IX3(I)) - H4(I)*V(2,IX4(I))
          VZ(I) = VZI(I) - H1(I)*V(3,IX1(I)) - H2(I)*V(3,IX2(I))
     .                   - H3(I)*V(3,IX3(I)) - H4(I)*V(3,IX4(I))
          VN(I) = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
C       correction hourglass
          H0 = -.25*(H1(I) - H2(I) + H3(I) - H4(I))
          H0 = MIN(H0,H2(I),H4(I))
          H0 = MAX(H0,-H1(I),-H3(I))
          IF(IX3(I)==IX4(I))H0 = ZERO
          H1(I) = H1(I) + H0
          H2(I) = H2(I) - H0
          H3(I) = H3(I) + H0
          H4(I) = H4(I) - H0
 500    CONTINUE
C---------------------
C      PENE INITIALE
C---------------------
        IF(INACTI==5)THEN
          DO I=1,JLT
            PENE(I)=MAX(ZERO,PENE(I)-CAND_P(INDEX(I)))
            IF( PENE(I)==ZERO )  STIF(I) = ZERO
            GAPV(I)=GAPV(I)-CAND_P(INDEX(I))
          ENDDO
        ELSE IF(INACTI==6)THEN
          DO I=1,JLT
            CAND_P(INDEX(I))=MIN(CAND_P(INDEX(I)),
     .         ( (ONE-FIVEEM2)*CAND_P(INDEX(I))
     .           +FIVEEM2*(PENE(I)+FIVEEM2*(GAPV(I)-PENE(I))))  )
            PENE(I)=MAX(ZERO,PENE(I)-CAND_P(INDEX(I)))
            IF( PENE(I)==ZERO )  STIF(I) = ZERO
            GAPV(I)=GAPV(I)-CAND_P(INDEX(I))
          ENDDO
        ENDIF
C-------------------------------------------
c      print *,'IMP_INT7=',IMP_INT7,jlt
        IF(IMP_INT7>=2)THEN
          DO I=1,JLT
            IF(STIGLO<=ZERO)THEN
              STIF(I) = HALF*STIF(I)
            ELSEIF(STIF(I)/=ZERO)THEN
              STIF(I) = STIGLO
            ENDIF
          ENDDO
        ELSEIF(IMP_INT7==1)THEN
          DO I=1,JLT
            FAC = GAPV(I)/MAX( EM10,( GAPV(I)-PENE(I) ) )
            IF( (GAPV(I)-PENE(I))/GAPV(I) <PREC .AND.
     .          STIF(I)>0. ) STIF(I) = 0.
            IF(STIGLO<=ZERO)THEN
              STIF(I) = HALF*STIF(I) * FAC
            ELSEIF(STIF(I)/=ZERO)THEN
              STIF(I) = STIGLO * FAC
            ENDIF
          ENDDO
        ELSE
C
          DO I=1,JLT
            FAC = GAPV(I)/MAX( EM10,( GAPV(I)-PENE(I) ) )
            IF( (GAPV(I)-PENE(I))/GAPV(I) <PREC .AND.
     .          STIF(I)>0. ) STIF(I) = 0.
            IF(STIGLO<=ZERO)THEN
              STIF(I) = HALF*STIF(I) * FAC
            ELSEIF(STIF(I)/=ZERO)THEN
              STIF(I) = STIGLO * FAC
            ENDIF
          ENDDO
          DO I=1,JLT
            STIF(I) = STIF(I) * GAPV(I) /
     .              MAX((GAPV(I) - PENE(I)),EM10)
          ENDDO
        ENDIF
C---------------------------------
C    ----sans frottement d'abord---
        DO I=1,JLT
          VTX = VX(I) -VN(I)*N1(I)
          VTY = VY(I) -VN(I)*N2(I)
          VTZ = VZ(I) -VN(I)*N3(I)
          VT  = VTX*VTX+VTY*VTY+VTZ*VTZ
          IF (VT>EM20) THEN
            S2=ONE/SQRT(VT)
            Q(1,1,I)=VTX*S2
            Q(1,2,I)=VTY*S2
            Q(1,3,I)=VTZ*S2
            Q(3,1,I)=N1(I)
            Q(3,2,I)=N2(I)
            Q(3,3,I)=N3(I)
            Q(2,1,I)=Q(3,2,I)*Q(1,3,I)-Q(3,3,I)*Q(1,2,I)
            Q(2,2,I)=Q(3,3,I)*Q(1,1,I)-Q(3,1,I)*Q(1,3,I)
            Q(2,3,I)=Q(3,1,I)*Q(1,2,I)-Q(3,2,I)*Q(1,1,I)
            FACT(I)=FRIC
          ELSE
            FACT(I)=ZERO
          ENDIF
        ENDDO
        IF (SCALK<0) THEN
          ISF=1
        ELSE
          ISF=0
        ENDIF
        FACF=ABS(SCALK)
        IF (ISF==1) THEN
          DO I=1,JLT
            IF (VN(I)>ZERO) THEN
              FAC=STIF(I)/FACF
            ELSEIF (VN(I)<ZERO) THEN
              FAC=STIF(I)*FACF
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
        DO J=1,3
          DO K=J,3
            DO I=1,JLT
              IF (FACT(I)>ZERO) THEN
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
          OFF(I)=ONE
        ENDDO
C
        IF (INTP_D <= 0 .AND. NSPMD > 1) THEN
          JLTF = 0
          DO I=1,JLT
            IF(NSVG(I)<0) THEN
              NN=-NSVG(I)
              JLTF = JLTF + 1
              NE=SHF_INT(NIN) + JLTF +LREM
              NS=IND_INT(NIN)%P(NN)
              STIFS(NE)=STIF(I)
              H_E(1,NE)=H1(I)
              H_E(2,NE)=H2(I)
              H_E(3,NE)=H3(I)
              H_E(4,NE)=H4(I)
              N_E(1,NE)=N1(I)
              N_E(2,NE)=N2(I)
              N_E(3,NE)=N3(I)
            ENDIF
          ENDDO
        ENDIF
C
        RETURN
      END
Chd|====================================================================
Chd|  I7FRF3                        source/interfaces/int07/i7keg3.F
Chd|-- called by -----------
Chd|        I7FORCF3                      source/interfaces/int07/i7ke3.F
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F
Chd|====================================================================
      SUBROUTINE I7FRF3(JLT    ,A      ,V      ,MS     ,FRIC   ,
     1                  N1     ,N2     ,N3     ,H1     ,H2     ,
     2                  H3     ,H4     ,IX1    ,IX2    ,IX3    ,
     2                  IX4    ,INDEX  ,VXI    ,VYI    ,VZI    ,
     3                  MSI    ,DXI    ,DYI    ,DZI    ,STIF   ,
     4                  NIN    ,D      ,SCALK  )
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER JLT, INACTI,NIN
        INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .          INDEX(MVSIZ)
        my_real
     .     A(3,*), MS(*), V(3,*),D(3,*),
     .     FRIC,SCALK,DXI(MVSIZ),DYI(MVSIZ),DZI(MVSIZ),
     .     H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .     VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ)
        my_real
     .       N1(MVSIZ), N2(MVSIZ), N3(MVSIZ),STIF(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J1, J, K,IG,ISF,NN,NS,NI
        my_real
     .     VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ),
     .     DX(MVSIZ), DY(MVSIZ), DZ(MVSIZ), DN(MVSIZ),
     .     DNI(MVSIZ),DT(MVSIZ), DTI(MVSIZ),
     .     S2,FACN(MVSIZ),FACF, FACT(MVSIZ)
        my_real
     .     FX,FY,FZ,FN,FT,FNI,FTI,VTX,VTY,VTZ,VT,
     .     T1(MVSIZ), T2(MVSIZ), T3(MVSIZ),
     .     KT1,KT2,KT3,KT4,Q1,Q2,H0

C-----------------------------------------------
C
        DO I=1,JLT
          VX(I) = VXI(I) - H1(I)*V(1,IX1(I)) - H2(I)*V(1,IX2(I))
     .                   - H3(I)*V(1,IX3(I)) - H4(I)*V(1,IX4(I))
          VY(I) = VYI(I) - H1(I)*V(2,IX1(I)) - H2(I)*V(2,IX2(I))
     .                   - H3(I)*V(2,IX3(I)) - H4(I)*V(2,IX4(I))
          VZ(I) = VZI(I) - H1(I)*V(3,IX1(I)) - H2(I)*V(3,IX2(I))
     .                   - H3(I)*V(3,IX3(I)) - H4(I)*V(3,IX4(I))
          VN(I) = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
          DX(I) = DXI(I) - H1(I)*D(1,IX1(I)) - H2(I)*D(1,IX2(I))
     .                   - H3(I)*D(1,IX3(I)) - H4(I)*D(1,IX4(I))
          DY(I) = DYI(I) - H1(I)*D(2,IX1(I)) - H2(I)*D(2,IX2(I))
     .                   - H3(I)*D(2,IX3(I)) - H4(I)*D(2,IX4(I))
          DZ(I) = DZI(I) - H1(I)*D(3,IX1(I)) - H2(I)*D(3,IX2(I))
     .                   - H3(I)*D(3,IX3(I)) - H4(I)*D(3,IX4(I))
          DN(I) = N1(I)*DX(I) + N2(I)*DY(I) + N3(I)*DZ(I)
          DNI(I) = N1(I)*DXI(I) + N2(I)*DYI(I) + N3(I)*DZI(I)
        ENDDO
C-------------------------------------------
        DO I=1,JLT
          VTX = VX(I) -VN(I)*N1(I)
          VTY = VY(I) -VN(I)*N2(I)
          VTZ = VZ(I) -VN(I)*N3(I)
          VT  = VTX*VTX+VTY*VTY+VTZ*VTZ
          IF (VT>EM20) THEN
            S2=ONE/SQRT(VT)
            T1(I)=VTX*S2
            T2(I)=VTY*S2
            T3(I)=VTZ*S2
            FACT(I)=FRIC
          ELSE
            FACT(I)=ZERO
            T1(I)=ONE
            T2(I)=ZERO
            T3(I)=ZERO
          ENDIF
        ENDDO
        DO I=1,JLT
          DT(I) = T1(I)*DX(I) + T2(I)*DY(I) + T3(I)*DZ(I)
          DTI(I) = T1(I)*DXI(I) + T2(I)*DYI(I) + T3(I)*DZI(I)
        ENDDO
        IF (SCALK<0) THEN
          ISF=1
        ELSE
          ISF=0
        ENDIF
        FACF=ABS(SCALK)
        IF (ISF==1) THEN
          DO I=1,JLT
            IF (VN(I)>ZERO) THEN
              FACN(I)=STIF(I)*FACF
            ELSEIF (VN(I)<ZERO) THEN
              FACN(I)=STIF(I)/FACF
            ELSE
              FACN(I)=STIF(I)
            ENDIF
            FACT(I)=FACN(I)*FACT(I)
          ENDDO
        ELSE
          DO I=1,JLT
            FACN(I)=STIF(I)*FACF
            FACT(I)=FACN(I)*FACT(I)
          ENDDO
        ENDIF
C--------partie NML-------
        DO I=1,JLT
          FN = -FACN(I)*DNI(I)
          FX=FN*N1(I)
          FY=FN*N2(I)
          FZ=FN*N3(I)
          IF (FACT(I)/=ZERO) THEN
            FT = -FACT(I)*DTI(I)
            FX = FX + FT*T1(I)
            FY = FY + FT*T2(I)
            FZ = FZ + FT*T3(I)
          ENDIF
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
          NI = INDEX(I)
          FFI(1,NI)=FFI(1,NI)+FNI*N1(I)
          FFI(2,NI)=FFI(2,NI)+FNI*N2(I)
          FFI(3,NI)=FFI(3,NI)+FNI*N3(I)
          IF (FACT(I)/=ZERO) THEN
            FTI = FACT(I)*DT(I)
            FFI(1,NI)=FFI(1,NI)+FTI*T1(I)
            FFI(2,NI)=FFI(2,NI)+FTI*T2(I)
            FFI(3,NI)=FFI(3,NI)+FTI*T3(I)
          ENDIF
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  I7KFOR3                       source/interfaces/int07/i7keg3.F
Chd|-- called by -----------
Chd|        I7FKU3                        source/interfaces/int07/i7ke3.F
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F
Chd|====================================================================
      SUBROUTINE I7KFOR3(JLT    ,A      ,V      ,MS    ,FRIC   ,
     1                  NX1    ,NX2    ,NX3    ,NX4   ,NY1    ,
     2                  NY2    ,NY3    ,NY4    ,NZ1   ,NZ2    ,
     3                  NZ3    ,NZ4    ,LB1    ,LB2   ,LB3    ,
     4                  LB4    ,LC1    ,LC2    ,LC3   ,LC4    ,
     5                  P1     ,P2     ,P3     ,P4    ,NIN    ,
     6                  IX1    ,IX2    ,IX3    ,IX4   ,NSVG   ,
     7                  GAPV   ,INACTI ,CAND_P ,INDEX ,STIGLO ,
     8                  STIF   ,VXI    ,VYI    ,VZI   ,MSI    ,
     F                  X      ,IRECT  ,CE_LOC ,MFROT ,IFQ    ,
     G                  FROT_P ,CAND_FX,CAND_FY,CAND_FZ,ALPHA0,
     H                  DXI    ,DYI    ,DZI    ,D     ,SCALK  ,
     2                  X1     ,X2     ,X3     ,X4     ,Y1     ,
     3                  Y2     ,Y3     ,Y4     ,Z1     ,Z2     ,
     4                  Z3     ,Z4     ,XI     ,YI     ,ZI     ,
     5                  ICURV  )
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
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER JLT,INACTI,NIN,MFROT, IFQ, NOINT,IRECT(4,*)
        INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .          CE_LOC(MVSIZ),INDEX(MVSIZ),NSVG(MVSIZ)
        my_real
     .     STIGLO,CAND_P(*),FROT_P(*), X(3,*),A(3,*), MS(*), V(3,*),
     .     CAND_FX(*),CAND_FY(*),CAND_FZ(*),ALPHA0,FRIC,SCALK,ICURV(3)
        my_real
     .       NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .       NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .       NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .       LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .       LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .       P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ), STIF(MVSIZ),
     .       GAPV(MVSIZ),VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ),
     .       DXI(MVSIZ),DYI(MVSIZ),DZI(MVSIZ),D(3,*)
        my_real
     .       X1(MVSIZ),Y1(MVSIZ),Z1(MVSIZ),
     .       X2(MVSIZ),Y2(MVSIZ),Z2(MVSIZ),
     .       X3(MVSIZ),Y3(MVSIZ),Z3(MVSIZ),
     .       X4(MVSIZ),Y4(MVSIZ),Z4(MVSIZ),
     .       XI(MVSIZ),YI(MVSIZ),ZI(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J1, IG, J, JG , K0,NBINTER,K1S,K,IL,IE, NN, NI,NS
        my_real
     .     FXI(MVSIZ), FYI(MVSIZ), FZI(MVSIZ), FNI(MVSIZ),
     .     FXT(MVSIZ),FYT(MVSIZ),FZT(MVSIZ),STIF0(MVSIZ),
     .     N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PENE(MVSIZ),
     .     H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .     VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ),XMU(MVSIZ),
     .     DX(MVSIZ), DY(MVSIZ), DZ(MVSIZ), DN0(MVSIZ),
     .
     .     VNX, VNY, VNZ, AA, CRIT,S2,DIST,RDIST,
     .     V2, FM2, FAC,FF,ALPHI,ALPHA,BETA,
     .     FX, FY, FZ, F2, MAS2, M2SK, DTI,FT,FN,FMAX,FTN,
     .     H0, LA1, LA2, LA3, LA4,D1,D2,D3,D4,A1,A2,A3,A4, FF0,
     .     VV,AX1,AX2,AY1,AY2,AZ1,AZ2,AX,AY,AZ,T1,T2,T3,DXT,
     .     AREA,P,VV1,VV2,V21,DMU, DTI2,H00 ,A0X,A0Y,A0Z,RX,RY,RZ,
     .     ANX,ANY,ANZ,AAN,AAX,AAY,AAZ ,RR,RS,AAA ,GAP2 ,PENE2,PREC
        INTEGER NA1,NA2
C-----------------------------------------------
        IF (IRESP==1) THEN
          PREC = FIVEEM4
        ELSE
          PREC = EM10
        ENDIF
C---------------------
C      PENE INITIALE
C---------------------
        IF(INACTI==5.OR.INACTI==6)THEN
          DO I=1,JLT
            IF(STIF(I)==ZERO)THEN
              CAND_P(INDEX(I))=0
            ENDIF
          ENDDO
        ENDIF
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
          IF (PENE2<=ZERO) STIF(I) = ZERO
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
C
          A1 = P1(I)/MAX(EM20,D1)
          A2 = P2(I)/MAX(EM20,D2)
          A3 = P3(I)/MAX(EM20,D3)
          A4 = P4(I)/MAX(EM20,D4)
          N1(I) = A1*NX1(I) + A2*NX2(I) + A3*NX3(I) + A4*NX4(I)
          N2(I) = A1*NY1(I) + A2*NY2(I) + A3*NY3(I) + A4*NY4(I)
          N3(I) = A1*NZ1(I) + A2*NZ2(I) + A3*NZ3(I) + A4*NZ4(I)
        ENDDO
C
        DO I=1,JLT
          IF(IX3(I)/=IX4(I))THEN
            PENE(I) = MAX(P1(I),P2(I),P3(I),P4(I))
C
            LA1 = ONE - LB1(I) - LC1(I)
            LA2 = ONE - LB2(I) - LC2(I)
            LA3 = ONE - LB3(I) - LC3(I)
            LA4 = ONE - LB4(I) - LC4(I)
C
            H0    = FOURTH *
     .            (P1(I)*LA1 + P2(I)*LA2 + P3(I)*LA3 + P4(I)*LA4)
            H1(I) = H0 + P1(I) * LB1(I) + P4(I) * LC4(I)
            H2(I) = H0 + P2(I) * LB2(I) + P1(I) * LC1(I)
            H3(I) = H0 + P3(I) * LB3(I) + P2(I) * LC2(I)
            H4(I) = H0 + P4(I) * LB4(I) + P3(I) * LC3(I)
            H00    = ONE/MAX(EM20,H1(I) + H2(I) + H3(I) + H4(I))
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
            H3(I) = ONE - LB1(I) - LC1(I)
            H4(I) = ZERO
          ENDIF
        ENDDO
C---------------------
C     COURBURE FIXE
C---------------------
        IF(ICURV(1)==1)THEN
C       spherique (que concave pour le moment)
          NA1 = ICURV(2)
          DO I=1,JLT
            RR = 1.E30
            A0X = X(1,NA1)
            A0Y = X(2,NA1)
            A0Z = X(3,NA1)
C
            RX = X1(I)-A0X
            RY = Y1(I)-A0Y
            RZ = Z1(I)-A0Z
            RR = MIN(RR , RX*RX + RY*RY + RZ*RZ)
            RX = X2(I)-A0X
            RY = Y2(I)-A0Y
            RZ = Z2(I)-A0Z
            RR = MIN(RR , RX*RX + RY*RY + RZ*RZ)
            RX = X3(I)-A0X
            RY = Y3(I)-A0Y
            RZ = Z3(I)-A0Z
            RR = MIN(RR , RX*RX + RY*RY + RZ*RZ)
            IF(IX3(I)/=IX4(I))THEN
              RX = X4(I)-A0X
              RY = Y4(I)-A0Y
              RZ = Z4(I)-A0Z
              RR = MIN(RR , RX*RX + RY*RY + RZ*RZ)
            ENDIF
            RX = XI(I)-A0X
            RY = YI(I)-A0Y
            RZ = ZI(I)-A0Z
            RS = SQRT(RX*RX + RY*RY + RZ*RZ)
            RR = SQRT(RR)
            IF(RS-RR+GAPV(I)<0.)THEN
              STIF(I) = 0.
              PENE(I) = 0.
            ELSEIF(RS-RR+GAPV(I)<PENE(I))THEN
              PENE(I) = RS-RR+GAPV(I)
            ENDIF
            N1(I) = -RX
            N2(I) = -RY
            N3(I) = -RZ
          ENDDO
        ELSEIF(ICURV(1)==2)THEN
C       cylindrique (que concave pour le moment)
          NA1 = ICURV(2)
          NA2 = ICURV(3)
          DO I=1,JLT
            RR = 1.E30
            A0X = X(1,NA1)
            A0Y = X(2,NA1)
            A0Z = X(3,NA1)
            ANX = X(1,NA2)-A0X
            ANY = X(2,NA2)-A0Y
            ANZ = X(3,NA2)-A0Z
            AAN = 1. / (ANX*ANX + ANY*ANY + ANZ*ANZ)
C
            AAX = X1(I)-A0X
            AAY = Y1(I)-A0Y
            AAZ = Z1(I)-A0Z
            AAA = (AAX*ANX + AAY*ANY + AAZ*ANZ) * AAN
            RX = AAX - AAA * ANX
            RY = AAY - AAA * ANY
            RZ = AAZ - AAA * ANZ
            RR = MIN(RR , RX*RX + RY*RY + RZ*RZ)
            AAX = X2(I)-A0X
            AAY = Y2(I)-A0Y
            AAZ = Z2(I)-A0Z
            AAA = (AAX*ANX + AAY*ANY + AAZ*ANZ) * AAN
            RX = AAX - AAA * ANX
            RY = AAY - AAA * ANY
            RZ = AAZ - AAA * ANZ
            RR = MIN(RR , RX*RX + RY*RY + RZ*RZ)
            AAX = X3(I)-A0X
            AAY = Y3(I)-A0Y
            AAZ = Z3(I)-A0Z
            AAA = (AAX*ANX + AAY*ANY + AAZ*ANZ) * AAN
            RX = AAX - AAA * ANX
            RY = AAY - AAA * ANY
            RZ = AAZ - AAA * ANZ
            RR = MIN(RR , RX*RX + RY*RY + RZ*RZ)
            IF(IX3(I)/=IX4(I))THEN
              AAX = X4(I)-A0X
              AAY = Y4(I)-A0Y
              AAZ = Z4(I)-A0Z
              AAA = (AAX*ANX + AAY*ANY + AAZ*ANZ) * AAN
              RX = AAX - AAA * ANX
              RY = AAY - AAA * ANY
              RZ = AAZ - AAA * ANZ
              RR = MIN(RR , RX*RX + RY*RY + RZ*RZ)
            ENDIF
            AAX = XI(I)-A0X
            AAY = YI(I)-A0Y
            AAZ = ZI(I)-A0Z
            AAA = (AAX*ANX + AAY*ANY + AAZ*ANZ) * AAN
            RX = AAX - AAA * ANX
            RY = AAY - AAA * ANY
            RZ = AAZ - AAA * ANZ
            RS = SQRT(RX*RX + RY*RY + RZ*RZ)
            RR = SQRT(RR)
            IF(RS-RR+GAPV(I)<0.)THEN
              STIF(I) = 0.
              PENE(I) = 0.
            ELSEIF(RS-RR+GAPV(I)<PENE(I))THEN
              PENE(I) = RS-RR+GAPV(I)
              N1(I) = -RX
              N2(I) = -RY
              N3(I) = -RZ
            ENDIF
          ENDDO
        ELSEIF(ICURV(1) == 3)THEN
c        print *,'ICURV=3 NOT AVAILABLE WITH IMPLICIT OPTION'
        ENDIF
C
        DO I=1,JLT
          S2 = ONE/MAX(EM30,SQRT(N1(I)**2 + N2(I)**2 + N3(I)**2))
          N1(I) = N1(I)*S2
          N2(I) = N2(I)*S2
          N3(I) = N3(I)*S2
        ENDDO
C
        DO I=1,JLT
          DX(I) = DXI(I) - H1(I)*D(1,IX1(I)) - H2(I)*D(1,IX2(I))
     .                   - H3(I)*D(1,IX3(I)) - H4(I)*D(1,IX4(I))
          DY(I) = DYI(I) - H1(I)*D(2,IX1(I)) - H2(I)*D(2,IX2(I))
     .                   - H3(I)*D(2,IX3(I)) - H4(I)*D(2,IX4(I))
          DZ(I) = DZI(I) - H1(I)*D(3,IX1(I)) - H2(I)*D(3,IX2(I))
     .                   - H3(I)*D(3,IX3(I)) - H4(I)*D(3,IX4(I))
          DN0(I) = N1(I)*DX(I) + N2(I)*DY(I) + N3(I)*DZ(I)
        ENDDO
C
        DO 500 I=1,JLT
          VX(I) = VXI(I) - H1(I)*V(1,IX1(I)) - H2(I)*V(1,IX2(I))
     .                   - H3(I)*V(1,IX3(I)) - H4(I)*V(1,IX4(I))
          VY(I) = VYI(I) - H1(I)*V(2,IX1(I)) - H2(I)*V(2,IX2(I))
     .                   - H3(I)*V(2,IX3(I)) - H4(I)*V(2,IX4(I))
          VZ(I) = VZI(I) - H1(I)*V(3,IX1(I)) - H2(I)*V(3,IX2(I))
     .                   - H3(I)*V(3,IX3(I)) - H4(I)*V(3,IX4(I))
          VN(I) = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
C       correction hourglass
          H0 = -.25*(H1(I) - H2(I) + H3(I) - H4(I))
          H0 = MIN(H0,H2(I),H4(I))
          H0 = MAX(H0,-H1(I),-H3(I))
          IF(IX3(I)==IX4(I))H0 = ZERO
          H1(I) = H1(I) + H0
          H2(I) = H2(I) - H0
          H3(I) = H3(I) + H0
          H4(I) = H4(I) - H0
 500    CONTINUE
C---------------------
C      PENE INITIALE
C---------------------
        IF(INACTI==5)THEN
          DO I=1,JLT
C REDUCTION DE LA PENE INITIALE
            CAND_P(INDEX(I))=MIN(CAND_P(INDEX(I)),
     .             ((ONE-FIVEEM2)*CAND_P(INDEX(I))+FIVEEM2*PENE(I))  )
C SOUSTRACTION DE LA PENE INITIALE A LA PENE ET AU GAP
            PENE(I)=MAX(ZERO,PENE(I)-CAND_P(INDEX(I)))
            IF( PENE(I)==ZERO )  STIF(I) = ZERO
            GAPV(I)=GAPV(I)-CAND_P(INDEX(I))
          ENDDO
        ELSE IF(INACTI==6)THEN
          DO I=1,JLT
C REDUCTION DE LA PENE INITIALE
C           CAND_P(INDEX(I))=MIN(CAND_P(INDEX(I)),PENE(I))
            CAND_P(INDEX(I))=MIN(CAND_P(INDEX(I)),
     .         ( (ONE-FIVEEM2)*CAND_P(INDEX(I))
     .           +FIVEEM2*(PENE(I)+FIVEEM2*(GAPV(I)-PENE(I))))  )
C SOUSTRACTION DE LA PENE INITIALE A LA PENE ET AU GAP
            PENE(I)=MAX(ZERO,PENE(I)-CAND_P(INDEX(I)))
            IF( PENE(I)==ZERO )  STIF(I) = ZERO
            GAPV(I)=GAPV(I)-CAND_P(INDEX(I))
          ENDDO
        ENDIF
C
        DO I=1,JLT
          STIF0(I) = STIF(I)
        ENDDO
C-------------------------------------------
        IF(IMP_INT7>=2)THEN
          DO I=1,JLT
            IF(STIGLO<=ZERO)THEN
              STIF(I) = HALF*STIF(I)
            ELSEIF(STIF(I)/=ZERO)THEN
              STIF(I) = STIGLO
            ENDIF
          ENDDO
        ELSEIF(IMP_INT7==1)THEN
          DO I=1,JLT
            FAC = GAPV(I)/MAX( EM10,( GAPV(I)-PENE(I) ) )
            IF( (GAPV(I)-PENE(I))/GAPV(I) <PREC .AND.
     .         STIF(I)>0. ) STIF(I) = 0.
            IF(STIGLO<=ZERO)THEN
              STIF(I) = HALF*STIF(I) * FAC
            ELSEIF(STIF(I)/=ZERO)THEN
              STIF(I) = STIGLO * FAC
            ENDIF
          ENDDO
        ELSE
          DO I=1,JLT
            FAC = GAPV(I)/MAX( EM10,( GAPV(I)-PENE(I) ) )
            IF( (GAPV(I)-PENE(I))/GAPV(I) <PREC .AND.
     .         STIF(I)>0. ) STIF(I) = 0.
            IF(STIGLO<=ZERO)THEN
              STIF(I) = HALF*STIF(I) * FAC
            ELSEIF(STIF(I)/=ZERO)THEN
              STIF(I) = STIGLO * FAC
            ENDIF
          ENDDO
          DO I=1,JLT
            STIF(I) = STIF(I) * GAPV(I) /
     .              MAX((GAPV(I) - PENE(I)),EM10)
          ENDDO
        ENDIF ! (IMP_INT7>=2)
C
        FAC =   ABS(SCALK)
        DO I=1,JLT
          STIF(I)=STIF(I)*FAC
          FNI(I)= -STIF(I) * DN0(I)
          FXI(I)=N1(I)*FNI(I)
          FYI(I)=N2(I)*FNI(I)
          FZI(I)=N3(I)*FNI(I)
        ENDDO
C------------------
C    TANGENT FORCE CALCULATION
C------------------
C---------------------------------
C       NEW FRICTION MODELS
C---------------------------------
        IF (MFROT==0) THEN
C---      Coulomb friction
          DO I=1,JLT
            XMU(I) = FRIC
          ENDDO
        ELSEIF (MFROT==1) THEN
C---      Viscous friction
          DO I=1,JLT
            IE=CE_LOC(I)
            AA = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
            V2 = (VX(I) - N1(I)*AA)**2
     .         + (VY(I) - N2(I)*AA)**2
     .         + (VZ(I) - N3(I)*AA)**2
            VV  = SQRT(MAX(EM30,V2))
            AX1 = X(1,IRECT(3,IE)) - X(1,IRECT(1,IE))
            AY1 = X(2,IRECT(3,IE)) - X(2,IRECT(1,IE))
            AZ1 = X(3,IRECT(3,IE)) - X(3,IRECT(1,IE))
            AX2 = X(1,IRECT(4,IE)) - X(1,IRECT(2,IE))
            AY2 = X(2,IRECT(4,IE)) - X(2,IRECT(2,IE))
            AZ2 = X(3,IRECT(4,IE)) - X(3,IRECT(2,IE))
            AX  = AY1*AZ2 - AZ1*AY2
            AY  = AZ1*AX2 - AX1*AZ2
            AZ  = AX1*AY2 - AY1*AX2
            AREA = HALF*SQRT(AX*AX+AY*AY+AZ*AZ)
            P = -FNI(I)/AREA
            XMU(I) = FRIC + (FROT_P(1) + FROT_P(4)*P ) * P
     .             +(FROT_P(2) + FROT_P(3)*P) * VV + FROT_P(5)*V2
          ENDDO
        ELSEIF(MFROT==2)THEN
C---        Loi Darmstad
          DO I=1,JLT
            IE=CE_LOC(I)
            AA = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
            V2 = (VX(I) - N1(I)*AA)**2
     .         + (VY(I) - N2(I)*AA)**2
     .         + (VZ(I) - N3(I)*AA)**2
            VV = SQRT(MAX(EM30,V2))
            AX1 = X(1,IRECT(3,IE)) - X(1,IRECT(1,IE))
            AY1 = X(2,IRECT(3,IE)) - X(2,IRECT(1,IE))
            AZ1 = X(3,IRECT(3,IE)) - X(3,IRECT(1,IE))
            AX2 = X(1,IRECT(4,IE)) - X(1,IRECT(2,IE))
            AY2 = X(2,IRECT(4,IE)) - X(2,IRECT(2,IE))
            AZ2 = X(3,IRECT(4,IE)) - X(3,IRECT(2,IE))
            AX  = AY1*AZ2 - AZ1*AY2
            AY  = AZ1*AX2 - AX1*AZ2
            AZ  = AX1*AY2 - AY1*AX2
            AREA = HALF*SQRT(AX*AX+AY*AY+AZ*AZ)
            P = -FNI(I)/AREA
            XMU(I) = FROT_P(1)*EXP(FROT_P(2)*VV)*P*P
     .             + FROT_P(3)*EXP(FROT_P(4)*VV)*P
     .             + FROT_P(5)*EXP(FROT_P(6)*VV)
          ENDDO
        ELSEIF (MFROT==3) THEN
C---        Loi Renard
          DO I=1,JLT
            AA = N1(I)*VX(I) + N2(I)*VY(I) + N3(I)*VZ(I)
            V2 = (VX(I) - N1(I)*AA)**2
     .         + (VY(I) - N2(I)*AA)**2
     .         + (VZ(I) - N3(I)*AA)**2
            VV = SQRT(MAX(EM30,V2))
            IF(VV>=0.AND.VV<=FROT_P(5)) THEN
              DMU = FROT_P(3)-FROT_P(1)
              VV1 = VV / FROT_P(5)
              XMU(I) = FROT_P(1)+ DMU*VV1*(TWO-VV1)
            ELSEIF(VV>FROT_P(5).AND.VV<FROT_P(6)) THEN
              DMU = FROT_P(4)-FROT_P(3)
              VV1 = (VV - FROT_P(5))/(FROT_P(6)-FROT_P(5))
              XMU(I) = FROT_P(3)+ DMU * (THREE-TWO*VV1)*VV1**2
            ELSE
              DMU = FROT_P(2)-FROT_P(4)
              VV2 = (VV - FROT_P(6))**2
              XMU(I) = FROT_P(2) - DMU / (ONE + DMU*VV2)
            ENDIF
          ENDDO
        ENDIF
C
C---------------------------------
C       INCREMENTAL (STIFFNESS) FORMULATION
C---------------------------------
        IF (IFQ>=10.AND.SCALK<ZERO) THEN
          IF (IFQ==13) THEN
            ALPHA = MAX(ONE,ALPHA0*DT12)
          ELSE
            ALPHA = ALPHA0
          ENDIF
          DO I=1,JLT
            FX = STIF0(I)*DX(I)
            FY = STIF0(I)*DY(I)
            FZ = STIF0(I)*DZ(I)
            FX = CAND_FX(INDEX(I)) + ALPHA*FX
            FY = CAND_FY(INDEX(I)) + ALPHA*FY
            FZ = CAND_FZ(INDEX(I)) + ALPHA*FZ
            FTN = FX*N1(I) + FY*N2(I) + FZ*N3(I)
            FX = FX - FTN*N1(I)
            FY = FY - FTN*N2(I)
            FZ = FZ - FTN*N3(I)
            FT = FX*FX + FY*FY + FZ*FZ
            FT = MAX(FT,EM30)
            FN = FXI(I)**2+FYI(I)**2+FZI(I)**2
            BETA = MIN(ONE,XMU(I)*SQRT(FN/FT))
            FXT(I) = FX * BETA
            FYT(I) = FY * BETA
            FZT(I) = FZ * BETA
C-------      total force
            FXI(I)=FXI(I) + FXT(I)
            FYI(I)=FYI(I) + FYT(I)
            FZI(I)=FZI(I) + FZT(I)
          ENDDO
        ELSE
C---------------------------------
C       TOTAL (VISCOUS) FORMULATION / NO FRICTION FILTERING
C---------------------------------
          IF (FRIC>0) THEN
            DO I=1,JLT
              VNX = N1(I)*DN0(I)
              VNY = N2(I)*DN0(I)
              VNZ = N3(I)*DN0(I)
              VX(I) = DX(I) - VNX
              VY(I) = DY(I) - VNY
              VZ(I) = DZ(I) - VNZ
              V2 = VX(I)**2 + VY(I)**2 + VZ(I)**2
              DXT = SQRT(V2)
              AA = DXT/MAX(EM30,V2)
              T1 = VX(I)*AA
              T2 = VY(I)*AA
              T3 = VZ(I)*AA
              FTN = -XMU(I)*STIF(I) * DXT
              FXT(I) = FTN * T1
              FYT(I) = FTN * T2
              FZT(I) = FTN * T3
C-------      total force
              FXI(I)=FXI(I) + FXT(I)
              FYI(I)=FYI(I) + FYT(I)
              FZI(I)=FZI(I) + FZT(I)
            ENDDO
          ENDIF
        ENDIF
C
C--------main part-------
c
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
        IF(IMACH/=3) THEN
          DO I=1,JLT
            IG=NSVG(I)
            A(1,IG)=A(1,IG)-FXI(I)
            A(2,IG)=A(2,IG)-FYI(I)
            A(3,IG)=A(3,IG)-FZI(I)
          ENDDO
        ELSE
          DO I=1,JLT
            IG=NSVG(I)
            IF(IG>0)THEN
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
