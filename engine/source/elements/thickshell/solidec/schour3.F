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
Chd|  SCHOUR3                       source/elements/thickshell/solidec/schour3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SCHOUR3(PM,RHO,OFF,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .   F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     .   PX1H1, PX1H2, PX1H3, PX1H4,
     .   PX2H1, PX2H2, PX2H3, PX2H4,
     .   PX3H1, PX3H2, PX3H3, PX3H4,
     .   PX4H1, PX4H2, PX4H3, PX4H4,
     .   HGX1 , HGY2 , HGZ1 , HGZ2 ,
     .   VOL,MAT,CXX,PID,GEO,
     .   FHOUR,RX0,RY0,SX0,SY0,AJ5,EINT,EINTM,VOL0,
     .   SIGY,SIG0,MM,NU,DEFP,ICP,NEL)
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
#include      "param_c.inc"
#include      "cong1_c.inc"
#include      "vect01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scrupw_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(*),PID(*),ICP,NEL
      my_real
     .   PM(NPROPM,*),GEO(NPROPG,*), RHO(*),OFF(*),
     .   VX1(*),VX2(*),VX3(*),VX4(*),VX5(*),VX6(*),VX7(*),VX8(*),
     .   VY1(*),VY2(*),VY3(*),VY4(*),VY5(*),VY6(*),VY7(*),VY8(*),
     .   VZ1(*),VZ2(*),VZ3(*),VZ4(*),VZ5(*),VZ6(*),VZ7(*),VZ8(*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   F17(*),F27(*),F37(*),F18(*),F28(*),F38(*),
     .   PX1H1(*), PX1H2(*), PX1H3(*),PX1H4(*),  
     .   PX2H1(*), PX2H2(*), PX2H3(*),PX2H4(*),  
     .   PX3H1(*), PX3H2(*), PX3H3(*),PX3H4(*),  
     .   PX4H1(*), PX4H2(*), PX4H3(*),PX4H4(*),  
     .   HGX1(*), HGY2(*),HGZ1(*), HGZ2(*),
     .   SIG0(NEL,6),MM(MVSIZ,2),
     .   VOL(*),CXX(*), 
     .   FHOUR(NEL,12),RX0(*),RY0(*),SX0(*),SY0(*),AJ5(*),
     .   EINT(*),EINTM(*),SIGY(*) ,VOL0(*),DEFP(*),NU(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX, J,K
      my_real
     .   CAQ(MVSIZ), FCL(MVSIZ), FCQ(MVSIZ),
     .   HGX3(MVSIZ), HGX4(MVSIZ),HGY3(MVSIZ), HGY4(MVSIZ),
     .   HGZ3(MVSIZ), HGZ4(MVSIZ),THK(MVSIZ),THK_1(MVSIZ),
     .   G11(MVSIZ),G21(MVSIZ),G31(MVSIZ),G41(MVSIZ),
     .   G51(MVSIZ),G61(MVSIZ),G71(MVSIZ),G81(MVSIZ),
     .   G12(MVSIZ),G22(MVSIZ),G32(MVSIZ),G42(MVSIZ),
     .   G52(MVSIZ),G62(MVSIZ),G72(MVSIZ),G82(MVSIZ),
     .   G13(MVSIZ),G23(MVSIZ),G33(MVSIZ),G43(MVSIZ),
     .   G53(MVSIZ),G63(MVSIZ),G73(MVSIZ),G83(MVSIZ),
     .   G14(MVSIZ),G24(MVSIZ),G34(MVSIZ),G44(MVSIZ),
     .   G54(MVSIZ),G64(MVSIZ),G74(MVSIZ),G84(MVSIZ),
     .   NFHX3(MVSIZ),NFHY3(MVSIZ),NFHZ3(MVSIZ),
     .   NFHX4(MVSIZ),NFHY4(MVSIZ),NFHZ4(MVSIZ),
     .   NFHX1(MVSIZ),NFHZ1(MVSIZ),NFHY2(MVSIZ),NFHZ2(MVSIZ),
     .   HHX3(MVSIZ),HHY3(MVSIZ),HXY3(MVSIZ),E0(MVSIZ),
     .   G_3DT(MVSIZ),NU1,NU2,DH_XE1,DH_ZE1 ,DH_YK2, DH_ZK2 ,
     .   DH_XE3,DH_YK3,DH_ZK3 ,DH_ZE3,DH_XE4,DH_YK4,DH_Z4 ,
     .   C11,C22,E,CC,SIGY2,S1,S2,S3,SVM0,SR1,SR2,SR3,SR4,
     .   SR5,RX1,RXY1,SY1,SYX1,VOL_3,VMM0,SVMM,SVM1,SVM2,
     .   SVMR,SVM,SVMRST,FAC,FAC1,FAC2,F_Z,COEF,COEFH,SE,SK
Cqz       coef= (3*0.8)^2 , FZ=0.5*0.95^2
      my_real
     .   A1(MVSIZ), A2(MVSIZ), A3(MVSIZ),FACP
      DATA F_Z/.45/,COEF/5.76/,COEFH/0.5/
C-----------------------------------------------
Cqz      OPEN(UNIT=12,FILE='DEBHC.TMP',STATUS='UNKNOWN',FORM='FORMATTED')
Cqz----: r->ksi; s->eta; t->zeta------------
!      IF(MTN.EQ.11 .OR. MTN.EQ.17 .OR. MTN.EQ.6
!     .   .OR.MTN.EQ.46.OR.MTN.EQ.47.OR.MTN.EQ.37.OR.MTN.EQ.51) RETURN !TS: a tester au Starter.
C
      IF(INVSTR.GE.35)THEN
        DO I=LFT,LLT
          CAQ(I)=QUART*OFF(I)*GEO(13,PID(I))
        ENDDO
      ELSE
        DO I=LFT,LLT
          CAQ(I)=QUART*OFF(I)*PM(4,MAT(I))
        ENDDO
      ENDIF
      DO I=LFT,LLT
        THK(I) =UNHUIT*AJ5(I)
        THK_1(I) =UN/THK(I)
        E0(I)=TROIS*(UN-DEUX*NU(I))*PM(32,MAT(I))
        G_3DT(I)=UNSIX*E0(I)*OFF(I)*DT1/(UN+NU(I))
      ENDDO
C
       DO I=LFT,LLT
         FCL(I)=CAQ(I)*RHO(I)*VOL(I)**QTHIRD
         FCL(I)=ZEP01666666667*FCL(I)*CXX(I)
       ENDDO
C
      DO I=LFT,LLT
Cqz alpha =1 
C 1 1 -1 -1 -1 -1 1 1
        G11(I)= UNHUIT-PX1H1(I)
        G21(I)= UNHUIT-PX2H1(I)
        G31(I)=-UNHUIT-PX3H1(I)
        G41(I)=-UNHUIT-PX4H1(I)
        G51(I)=-UNHUIT+PX3H1(I)
        G61(I)=-UNHUIT+PX4H1(I)
        G71(I)= UNHUIT+PX1H1(I)
        G81(I)= UNHUIT+PX2H1(I)	
      ENDDO
      DO I=LFT,LLT
Cqz alpha =2 
C 1 -1 -1 1 -1 1 1 -1
        G12(I)= UNHUIT-PX1H2(I)
        G22(I)=-UNHUIT-PX2H2(I)
        G32(I)=-UNHUIT-PX3H2(I)
        G42(I)= UNHUIT-PX4H2(I)
        G52(I)=-UNHUIT+PX3H2(I)
        G62(I)= UNHUIT+PX4H2(I)
        G72(I)= UNHUIT+PX1H2(I)
        G82(I)=-UNHUIT+PX2H2(I)	
      ENDDO
      DO I=LFT,LLT
Cqz alpha =3 
C 1 -1 1 -1 1 -1 1 -1
        G13(I)= UNHUIT -PX1H3(I)
        G23(I)=-UNHUIT -PX2H3(I)
        G33(I)= UNHUIT -PX3H3(I)
        G43(I)=-UNHUIT -PX4H3(I)
        G53(I)= UNHUIT +PX3H3(I)
        G63(I)=-UNHUIT +PX4H3(I)
        G73(I)= UNHUIT +PX1H3(I)
        G83(I)=-UNHUIT +PX2H3(I)	
        HGX3(I)=
     &   G13(I)*VX1(I)+G23(I)*VX2(I)+G33(I)*VX3(I)+G43(I)*VX4(I)
     &  +G53(I)*VX5(I)+G63(I)*VX6(I)+G73(I)*VX7(I)+G83(I)*VX8(I)
        HGY3(I)=
     &   G13(I)*VY1(I)+G23(I)*VY2(I)+G33(I)*VY3(I)+G43(I)*VY4(I)
     &  +G53(I)*VY5(I)+G63(I)*VY6(I)+G73(I)*VY7(I)+G83(I)*VY8(I)
        HGZ3(I)=
     &   G13(I)*VZ1(I)+G23(I)*VZ2(I)+G33(I)*VZ3(I)+G43(I)*VZ4(I)
     &  +G53(I)*VZ5(I)+G63(I)*VZ6(I)+G73(I)*VZ7(I)+G83(I)*VZ8(I)
      ENDDO
C
C 1 -1 1 -1 -1 1 -1 1
Cqz alpha =4 
C -1 1 -1 1 1 -1 1 -1
      DO I=LFT,LLT
        G14(I)=-UNHUIT-PX1H4(I)
        G24(I)= UNHUIT-PX2H4(I)
        G34(I)=-UNHUIT-PX3H4(I)
        G44(I)= UNHUIT-PX4H4(I)
        G54(I)= UNHUIT+PX3H4(I)
        G64(I)=-UNHUIT+PX4H4(I)
        G74(I)= UNHUIT+PX1H4(I)
        G84(I)=-UNHUIT+PX2H4(I)	
        HGX4(I)=
     &    G14(I)*VX1(I)+G24(I)*VX2(I)+G34(I)*VX3(I)+G44(I)*VX4(I)
     &   +G54(I)*VX5(I)+G64(I)*VX6(I)+G74(I)*VX7(I)+G84(I)*VX8(I)
        HGY4(I)=
     &    G14(I)*VY1(I)+G24(I)*VY2(I)+G34(I)*VY3(I)+G44(I)*VY4(I)
     &   +G54(I)*VY5(I)+G64(I)*VY6(I)+G74(I)*VY7(I)+G84(I)*VY8(I)
        HGZ4(I)=
     &    G14(I)*VZ1(I)+G24(I)*VZ2(I)+G34(I)*VZ3(I)+G44(I)*VZ4(I)
     &   +G54(I)*VZ5(I)+G64(I)*VZ6(I)+G74(I)*VZ7(I)+G84(I)*VZ8(I)
      ENDDO
      IF (ICP.EQ.1) THEN
       DO I=LFT,LLT
        A1(I) =UNDEMI*(UN-NU(I))
        A2(I) =UNDEMI*(NU(I)-UN) 
        A3(I) = ZERO
       ENDDO 
      ELSEIF (ICP.EQ.2) THEN
       DO I=LFT,LLT
        FACP=UN-DEFP(I)/(SIGY(I)/E0(I)+DEFP(I))
        CC = FACP*(UN+NU(I))
        A1(I) =UNDEMI*(UN-NU(I)+CC)
        A2(I) =UNDEMI*(NU(I)-UN+CC) 
        A3(I) = ZERO
       ENDDO 
      ELSE
       DO I=LFT,LLT
        A1(I) =UN
        A2(I) =NU(I)
        A3(I) = UN
       ENDDO 
      ENDIF
      !-------elstic increament, attention il y a un fac=3----
      DO I=LFT,LLT
       C11 = DEUX*G_3DT(I)/(UN-NU(I))
       CC = G_3DT(I)*THK_1(I)
       DH_XE1 =  CC*HGX1(I)
       DH_YK2 =  CC*HGY2(I)
       !-------reduit 0.1->0.2---------
       E = ZEP4*G_3DT(I)*(UN +NU(I))
       CC =E*THK_1(I)
       DH_ZE1 =  CC*HGZ1(I)
       DH_ZK2 =  CC*HGZ2(I)
       !-------reduit 0.01----------
       DH_Z4  =  EM01*CC*HGZ4(I)
       CC = C11*SY0(I)
       DH_XE3 =  CC*HGX3(I)
       DH_XE4 =  CC*HGX4(I)
       CC = C11*RX0(I)
       DH_YK3 =  CC*HGY3(I)
       DH_YK4 =  CC*HGY4(I)
       DH_ZK3 =  G_3DT(I)*RX0(I)*HGZ3(I)
       DH_ZE3 =  G_3DT(I)*SY0(I)*HGZ3(I)
C
Cqz -----------FHOUR(1-11,I):XE1,ZE1,YK2,ZK2,XE3,YK3,ZK3,ZE3,XE4,YK4,Z4-------
       FHOUR(I,1) =  OFF(I)*FHOUR(I,1)  +  DH_XE1
       FHOUR(I,2) =  OFF(I)*FHOUR(I,2)  +  DH_ZE1
       FHOUR(I,3) =  OFF(I)*FHOUR(I,3)  +  DH_YK2
       FHOUR(I,4) =  OFF(I)*FHOUR(I,4)  +  DH_ZK2
       FHOUR(I,5) =  OFF(I)*FHOUR(I,5)  +  DH_XE3
       FHOUR(I,6) =  OFF(I)*FHOUR(I,6)  +  DH_YK3
       FHOUR(I,7) =  OFF(I)*FHOUR(I,7)  +  DH_ZK3
       FHOUR(I,8) =  OFF(I)*FHOUR(I,8)  +  DH_ZE3
       FHOUR(I,9) =  OFF(I)*FHOUR(I,9)  +  DH_XE4
       FHOUR(I,10) = OFF(I)*FHOUR(I,10) +  DH_YK4
       FHOUR(I,11) = OFF(I)*FHOUR(I,11) +  DH_Z4
       IF (SIGY(I).LT.ZEP9EP30) THEN
        SE  =FHOUR(I,9)*FHOUR(I,9)
        SK  =FHOUR(I,10)*FHOUR(I,10)
        SR1 = (UN +NU(I)*NU(I))*(SE+SK)
        SR2 = SR1 - DEUX*NU(I)*(SE+SK)
        SR3 = ABS((SIX*NU(I)-DEUX)*FHOUR(I,9)*FHOUR(I,10))
        SVMM = F_Z*(SR1+SR2+SR3)
        SE  =(UN-NU(I))*FHOUR(I,5)
        SK  =(UN-NU(I))*FHOUR(I,6)
        SR1 =SE*SE+SK*SK
        SVM =-SE*SK
        SE  =NU(I)*FHOUR(I,5)-FHOUR(I,2)
        SK  =FHOUR(I,6)-FHOUR(I,4)
        SR2 =SE*SE+SK*SK
        SVM = SVM+SE*SK
        SE  =FHOUR(I,5)-FHOUR(I,2)
        SK  =NU(I)*FHOUR(I,6)-FHOUR(I,4)
        SR3 =SE*SE+SK*SK
        SVM = SVM+SE*SK
        SR4 =(FHOUR(I,8)+FHOUR(I,1))*A3(I)
        SR5 =(FHOUR(I,7)+FHOUR(I,3))*A3(I)
        SVMR = 0.5*(SR1+SR2+SR3)+3*(SR4*SR4+SR5*SR5)
        SVM1 = SQRT(MM(I,1)+COEF*(SVMR+SVMM+MAX(SVM,-SVM)))
        SVM2 = SQRT(MM(I,2)+COEF*(SVMR+SVMM+MIN(SVM,-SVM)))
        !-----------ELASTIC-PLASTIC yield criterion------------
        IF (SVM1.GT.SIGY(I)) THEN
        !-----------Moment terms-----------
         FAC1 = SIGY(I)-FHOUR(I,12)
         FAC2 = SVM1-FHOUR(I,12)
         IF (FAC2.LE.EM20) THEN
           FAC=ZERO
         ELSE
           FAC = UN -MAX(EM20,FAC1/FAC2)
         ENDIF
         IF (SVM2.LT.SIGY(I)) THEN
           FAC1 =(SVM1-SIGY(I))/MAX((SVM1-SVM2),EM20)
           FAC1 =UNDEMI +SQRT(FAC1)
           FAC = MIN(FAC1,UN)*FAC
         ENDIF       
         FHOUR(I,9) = FHOUR(I,9) - FAC*DH_XE4
         FHOUR(I,10) = FHOUR(I,10) - FAC*DH_YK4
         SVM = SVM1-SQRT(COEF*SVMM)
         FAC1 =SVM/MAX(SIGY(I),EM20)
         FAC = MIN(SQRT(FAC1),UN)*FAC
         !-----------Membrane terms-----------
          FHOUR(I,1) = FHOUR(I,1) - FAC*DH_XE1*A3(I)
          FHOUR(I,2) = FHOUR(I,2) - FAC*DH_ZE1
          FHOUR(I,3) = FHOUR(I,3) - FAC*DH_YK2*A3(I)
          FHOUR(I,4) = FHOUR(I,4) - FAC*DH_ZK2
          FHOUR(I,5) = FHOUR(I,5) - FAC*DH_XE3
          FHOUR(I,6) = FHOUR(I,6) - FAC*DH_YK3
          FHOUR(I,7) = FHOUR(I,7) - FAC*DH_ZK3*A3(I)
          FHOUR(I,8) = FHOUR(I,8) - FAC*DH_ZE3*A3(I)
          FHOUR(I,11) = FHOUR(I,11) - FAC*DH_Z4
        ENDIF
        FHOUR(I,12) = ZEP9*SVM1+EM01*SVM2
       ENDIF
      ENDDO 
C 
      DO I=LFT,LLT
       CC = THK_1(I)*VOL(I)
       NFHX1(I) = CC*(FHOUR(I,1)+FHOUR(I,8))
       NFHZ1(I) = CC*FHOUR(I,2)
       NFHY2(I) = CC*(FHOUR(I,3)+FHOUR(I,7))
       NFHZ2(I) = CC*FHOUR(I,4)
       RX1  = SX0(I)*SX0(I)/RX0(I)+RX0(I)     
       RXY1 = SX0(I)*SY0(I)/RX0(I)+RY0(I)
       SY1  = RY0(I)*RY0(I)/SY0(I)+SY0(I)
       HHX3(I) = RX1*RX0(I)
       HHY3(I) = SY1*SY0(I)
       HXY3(I) = RXY1*RX0(I)*SQRT(NU(I))
       NFHZ3(I) = VOL(I)*(SY1*FHOUR(I,8)+RX1*FHOUR(I,7)
     .                   +SY0(I)*FHOUR(I,1)+RX0(I)*FHOUR(I,3))
       SY1  = SY1*A1(I)
       RX1  = RX1*A1(I)
       RXY1  = RXY1*A2(I)
       SYX1  = RXY1*RX0(I)/SY0(I)
       VOL_3 = THIRD*VOL(I)
       NFHX3(I) = VOL(I)*(SY1*FHOUR(I,5)+RXY1*FHOUR(I,6))
       NFHY3(I) = VOL(I)*(RX1*FHOUR(I,6)+SYX1*FHOUR(I,5))
       NFHX4(I) = VOL_3*(SY1*FHOUR(I,9)+RXY1*FHOUR(I,10))
       NFHY4(I) = VOL_3*(RX1*FHOUR(I,10)+SYX1*FHOUR(I,9))
       NFHZ4(I) =THK_1(I)*VOL_3*FHOUR(I,11)
      ENDDO
      !--------visco-termes-------
      DO I=LFT,LLT
       CC = FCL(I)*THK_1(I)/SQRT(1+NU(I))
       NFHX1(I) = NFHX1(I)+CC*(THK_1(I)*HGX1(I)+SY0(I)*HGZ3(I))
       NFHY2(I) = NFHY2(I)+CC*(THK_1(I)*HGY2(I)+RX0(I)*HGZ3(I))
       NFHZ3(I) = NFHZ3(I)+CC*(THK(I)*(HHX3(I)+HHY3(I))*HGZ3(I)
     .                         +SY0(I)*HGX1(I)+RX0(I)*HGY2(I))
       CC = QUINZE*FCL(I)*THK_1(I)*THK_1(I)
       NFHZ1(I) = NFHZ1(I)+CC*HGZ1(I)
       NFHZ2(I) = NFHZ2(I)+CC*HGZ2(I)
       NFHZ4(I) = NFHZ4(I)+CC*HGZ4(I)
       CC = FCL(I)/SQRT(1-NU(I)*NU(I))
       NFHX3(I) = NFHX3(I)+CC*(HHX3(I)*HGX3(I)+HXY3(I)*HGY3(I))
       NFHY3(I) = NFHY3(I)+CC*(HHY3(I)*HGY3(I)+HXY3(I)*HGX3(I))
       CC = THIRD*CC
       NFHX4(I) = NFHX4(I)+CC*(HHX3(I)*HGX4(I)+HXY3(I)*HGY4(I))
       NFHY4(I) = NFHY4(I)+CC*(HHY3(I)*HGY4(I)+HXY3(I)*HGX4(I))
      ENDDO
C
      DO I=LFT,LLT
        F11(I) =F11(I)-G11(I)*NFHX1(I)
     .                -G13(I)*NFHX3(I)-G14(I)*NFHX4(I)
        F12(I) =F12(I)-G21(I)*NFHX1(I)
     .                -G23(I)*NFHX3(I)-G24(I)*NFHX4(I)
        F13(I) =F13(I)-G31(I)*NFHX1(I)
     .                -G33(I)*NFHX3(I)-G34(I)*NFHX4(I)
        F14(I) =F14(I)-G41(I)*NFHX1(I)
     .                -G43(I)*NFHX3(I)-G44(I)*NFHX4(I)
        F15(I) =F15(I)-G51(I)*NFHX1(I)
     .                -G53(I)*NFHX3(I)-G54(I)*NFHX4(I)
        F16(I) =F16(I)-G61(I)*NFHX1(I)
     .                -G63(I)*NFHX3(I)-G64(I)*NFHX4(I)
        F17(I) =F17(I)-G71(I)*NFHX1(I)
     .                -G73(I)*NFHX3(I)-G74(I)*NFHX4(I)
        F18(I) =F18(I)-G81(I)*NFHX1(I)
     .                -G83(I)*NFHX3(I)-G84(I)*NFHX4(I)
C
        F21(I) =F21(I)-G12(I)*NFHY2(I)
     .                -G13(I)*NFHY3(I)-G14(I)*NFHY4(I)
        F22(I) =F22(I)-G22(I)*NFHY2(I)
     .                -G23(I)*NFHY3(I)-G24(I)*NFHY4(I)
        F23(I) =F23(I)-G32(I)*NFHY2(I)
     .                -G33(I)*NFHY3(I)-G34(I)*NFHY4(I)
        F24(I) =F24(I)-G42(I)*NFHY2(I)
     .                -G43(I)*NFHY3(I)-G44(I)*NFHY4(I)
        F25(I) =F25(I)-G52(I)*NFHY2(I)
     .                -G53(I)*NFHY3(I)-G54(I)*NFHY4(I)
        F26(I) =F26(I)-G62(I)*NFHY2(I)
     .                -G63(I)*NFHY3(I)-G64(I)*NFHY4(I)
        F27(I) =F27(I)-G72(I)*NFHY2(I)
     .                -G73(I)*NFHY3(I)-G74(I)*NFHY4(I)
        F28(I) =F28(I)-G82(I)*NFHY2(I)
     .                -G83(I)*NFHY3(I)-G84(I)*NFHY4(I)
C
        F31(I) =F31(I)-G11(I)*NFHZ1(I)-G12(I)*NFHZ2(I)
     .                -G13(I)*NFHZ3(I)-G14(I)*NFHZ4(I)
        F32(I) =F32(I)-G21(I)*NFHZ1(I)-G22(I)*NFHZ2(I)
     .                -G23(I)*NFHZ3(I)-G24(I)*NFHZ4(I)
        F33(I) =F33(I)-G31(I)*NFHZ1(I)-G32(I)*NFHZ2(I)
     .                -G33(I)*NFHZ3(I)-G34(I)*NFHZ4(I)
        F34(I) =F34(I)-G41(I)*NFHZ1(I)-G42(I)*NFHZ2(I)
     .                -G43(I)*NFHZ3(I)-G44(I)*NFHZ4(I)
        F35(I) =F35(I)-G51(I)*NFHZ1(I)-G52(I)*NFHZ2(I)
     .                -G53(I)*NFHZ3(I)-G54(I)*NFHZ4(I)
        F36(I) =F36(I)-G61(I)*NFHZ1(I)-G62(I)*NFHZ2(I)
     .                -G63(I)*NFHZ3(I)-G64(I)*NFHZ4(I)
        F37(I) =F37(I)-G71(I)*NFHZ1(I)-G72(I)*NFHZ2(I)
     .                -G73(I)*NFHZ3(I)-G74(I)*NFHZ4(I)
        F38(I) =F38(I)-G81(I)*NFHZ1(I)-G82(I)*NFHZ2(I)
     .                -G83(I)*NFHZ3(I)-G84(I)*NFHZ4(I)
      ENDDO
      !----hourglass energie est inclued in interal energie------
      DO I=LFT,LLT
        EINT(I)= EINT(I)+DT1*(
     .   NFHX1(I)*HGX1(I) + NFHZ1(I)*HGZ1(I) + 
     .   NFHY2(I)*HGY2(I) + NFHZ2(I)*HGZ2(I) + 
     .   NFHZ3(I)*HGZ3(I) + NFHZ4(I)*HGZ4(I) + 
     .   NFHX3(I)*HGX3(I) + NFHX4(I)*HGX4(I) + 
     .   NFHY3(I)*HGY3(I) + NFHY4(I)*HGY4(I) ) 
     .   /MAX(EM20,VOL0(I)) +EINTM(I)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SCHOUR3_1                     source/elements/thickshell/solidec/schour3.F
Chd|-- called by -----------
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SCHOUR3_1(PM,RHO,OFF,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .   F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     .   PX1H1, PX1H2, PX1H3, PX1H4,
     .   PX2H1, PX2H2, PX2H3, PX2H4,
     .   PX3H1, PX3H2, PX3H3, PX3H4,
     .   PX4H1, PX4H2, PX4H3, PX4H4,
     .   HGX1 , HGY2 , HGZ1 , HGZ2 ,
     .   VOL,MAT,CXX,PID,GEO,
     .   FHOUR,RX0,RY0,SX0,SY0,AJ5,EINT,EINTM,VOL0,
     .   SIGY,SIG0,MM,NU,DEFP,ICP,NEL)
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
#include      "param_c.inc"
#include      "cong1_c.inc"
#include      "vect01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scrupw_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(*),PID(*),ICP,NEL
      my_real
     .   PM(NPROPM,*),GEO(NPROPG,*), RHO(*),OFF(*),
     .   VX1(*),VX2(*),VX3(*),VX4(*),VX5(*),VX6(*),VX7(*),VX8(*),
     .   VY1(*),VY2(*),VY3(*),VY4(*),VY5(*),VY6(*),VY7(*),VY8(*),
     .   VZ1(*),VZ2(*),VZ3(*),VZ4(*),VZ5(*),VZ6(*),VZ7(*),VZ8(*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   F17(*),F27(*),F37(*),F18(*),F28(*),F38(*),
     .   PX1H1(*), PX1H2(*), PX1H3(*),PX1H4(*),  
     .   PX2H1(*), PX2H2(*), PX2H3(*),PX2H4(*),  
     .   PX3H1(*), PX3H2(*), PX3H3(*),PX3H4(*),  
     .   PX4H1(*), PX4H2(*), PX4H3(*),PX4H4(*),  
     .   HGX1(*), HGY2(*),HGZ1(*), HGZ2(*),
     .   SIG0(NEL,6),MM(MVSIZ,2),
     .   VOL(*),CXX(*), 
     .   FHOUR(NEL,12),RX0(*),RY0(*),SX0(*),SY0(*),AJ5(*),
     .   EINT(*),EINTM(*),SIGY(*) ,VOL0(*),DEFP(*),NU(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX, J,K
      my_real
     .   CAQ(MVSIZ), FCL(MVSIZ), FCQ(MVSIZ),
     .   HGX3(MVSIZ), HGX4(MVSIZ),HGY3(MVSIZ), HGY4(MVSIZ),
     .   HGZ3(MVSIZ), HGZ4(MVSIZ),THK(MVSIZ),THK_1(MVSIZ),
     .   G11(MVSIZ),G21(MVSIZ),G31(MVSIZ),G41(MVSIZ),
     .   G51(MVSIZ),G61(MVSIZ),G71(MVSIZ),G81(MVSIZ),
     .   G12(MVSIZ),G22(MVSIZ),G32(MVSIZ),G42(MVSIZ),
     .   G52(MVSIZ),G62(MVSIZ),G72(MVSIZ),G82(MVSIZ),
     .   G13(MVSIZ),G23(MVSIZ),G33(MVSIZ),G43(MVSIZ),
     .   G53(MVSIZ),G63(MVSIZ),G73(MVSIZ),G83(MVSIZ),
     .   G14(MVSIZ),G24(MVSIZ),G34(MVSIZ),G44(MVSIZ),
     .   G54(MVSIZ),G64(MVSIZ),G74(MVSIZ),G84(MVSIZ),
     .   NFHX3(MVSIZ),NFHY3(MVSIZ),NFHZ3(MVSIZ),
     .   NFHX4(MVSIZ),NFHY4(MVSIZ),NFHZ4(MVSIZ),
     .   NFHX1(MVSIZ),NFHZ1(MVSIZ),NFHY2(MVSIZ),NFHZ2(MVSIZ),
     .   HHX3(MVSIZ),HHY3(MVSIZ),HXY3(MVSIZ),E0(MVSIZ),
     .   G_3DT(MVSIZ),NU1,NU2,DH_XE1,DH_ZE1 ,DH_YK2, DH_ZK2 ,
     .   DH_XE3,DH_YK3,DH_ZK3 ,DH_ZE3,DH_XE4,DH_YK4,DH_Z4 ,
     .   C11,C22,E,CC,SIGY2,S1,S2,S3,SVM0,SR1,SR2,SR3,SR4,
     .   SR5,RX1,RXY1,SY1,SYX1,VOL_3,VMM0,SVMM,SVM1,SVM2,
     .   SVMR,SVM,SVMRST,FAC,FACM,FACB,F_Z,COEF,COEFH,SE,SK,DP
Cqz       coef= (3*0.8)^2 , FZ=0.5*0.95^2
      my_real
     .   A1(MVSIZ), A2(MVSIZ), A3(MVSIZ),FACP,F_MAX,
     .   SS1,SS2,SS3,FACZM
      DATA F_Z/.45/,COEF/5.76/,COEFH/0.5/,F_MAX/0.998/
C-----------------------------------------------
Cqz      OPEN(UNIT=12,FILE='DEBHC.TMP',STATUS='UNKNOWN',FORM='FORMATTED')
Cqz----: r->ksi; s->eta; t->zeta------------
!      IF(MTN.EQ.11 .OR. MTN.EQ.17 .OR. MTN.EQ.6
!     .   .OR.MTN.EQ.46.OR.MTN.EQ.47.OR.MTN.EQ.37.OR.MTN.EQ.51) RETURN !TS: a tester au Starter.
C
      IF(INVSTR.GE.35)THEN
        DO I=LFT,LLT
          CAQ(I)=QUART*OFF(I)*GEO(13,PID(I))
        ENDDO
      ELSE
        DO I=LFT,LLT
          CAQ(I)=QUART*OFF(I)*PM(4,MAT(I))
        ENDDO
      ENDIF
      DO I=LFT,LLT
        THK(I) =UNHUIT*AJ5(I)
        THK_1(I) =UN/THK(I)
        E0(I)=TROIS*(UN-DEUX*NU(I))*PM(32,MAT(I))
        G_3DT(I)=UNSIX*E0(I)*OFF(I)*DT1/(UN+NU(I))
      ENDDO
C
       DO I=LFT,LLT
         FCL(I)=CAQ(I)*RHO(I)*VOL(I)**QTHIRD
         FCL(I)=ZEP01666666667*FCL(I)*CXX(I)
       ENDDO
C
      DO I=LFT,LLT
Cqz alpha =1 
C 1 1 -1 -1 -1 -1 1 1
        G11(I)= UNHUIT-PX1H1(I)
        G21(I)= UNHUIT-PX2H1(I)
        G31(I)=-UNHUIT-PX3H1(I)
        G41(I)=-UNHUIT-PX4H1(I)
        G51(I)=-UNHUIT+PX3H1(I)
        G61(I)=-UNHUIT+PX4H1(I)
        G71(I)= UNHUIT+PX1H1(I)
        G81(I)= UNHUIT+PX2H1(I)	
      ENDDO
      DO I=LFT,LLT
Cqz alpha =2 
C 1 -1 -1 1 -1 1 1 -1
        G12(I)= UNHUIT-PX1H2(I)
        G22(I)=-UNHUIT-PX2H2(I)
        G32(I)=-UNHUIT-PX3H2(I)
        G42(I)= UNHUIT-PX4H2(I)
        G52(I)=-UNHUIT+PX3H2(I)
        G62(I)= UNHUIT+PX4H2(I)
        G72(I)= UNHUIT+PX1H2(I)
        G82(I)=-UNHUIT+PX2H2(I)	
      ENDDO
      DO I=LFT,LLT
Cqz alpha =3 
C 1 -1 1 -1 1 -1 1 -1
        G13(I)= UNHUIT -PX1H3(I)
        G23(I)=-UNHUIT -PX2H3(I)
        G33(I)= UNHUIT -PX3H3(I)
        G43(I)=-UNHUIT -PX4H3(I)
        G53(I)= UNHUIT +PX3H3(I)
        G63(I)=-UNHUIT +PX4H3(I)
        G73(I)= UNHUIT +PX1H3(I)
        G83(I)=-UNHUIT +PX2H3(I)	
        HGX3(I)=
     &   G13(I)*VX1(I)+G23(I)*VX2(I)+G33(I)*VX3(I)+G43(I)*VX4(I)
     &  +G53(I)*VX5(I)+G63(I)*VX6(I)+G73(I)*VX7(I)+G83(I)*VX8(I)
        HGY3(I)=
     &   G13(I)*VY1(I)+G23(I)*VY2(I)+G33(I)*VY3(I)+G43(I)*VY4(I)
     &  +G53(I)*VY5(I)+G63(I)*VY6(I)+G73(I)*VY7(I)+G83(I)*VY8(I)
        HGZ3(I)=
     &   G13(I)*VZ1(I)+G23(I)*VZ2(I)+G33(I)*VZ3(I)+G43(I)*VZ4(I)
     &  +G53(I)*VZ5(I)+G63(I)*VZ6(I)+G73(I)*VZ7(I)+G83(I)*VZ8(I)
      ENDDO
C
C 1 -1 1 -1 -1 1 -1 1
Cqz alpha =4 
C -1 1 -1 1 1 -1 1 -1
      DO I=LFT,LLT
        G14(I)=-UNHUIT-PX1H4(I)
        G24(I)= UNHUIT-PX2H4(I)
        G34(I)=-UNHUIT-PX3H4(I)
        G44(I)= UNHUIT-PX4H4(I)
        G54(I)= UNHUIT+PX3H4(I)
        G64(I)=-UNHUIT+PX4H4(I)
        G74(I)= UNHUIT+PX1H4(I)
        G84(I)=-UNHUIT+PX2H4(I)	
        HGX4(I)=
     &    G14(I)*VX1(I)+G24(I)*VX2(I)+G34(I)*VX3(I)+G44(I)*VX4(I)
     &   +G54(I)*VX5(I)+G64(I)*VX6(I)+G74(I)*VX7(I)+G84(I)*VX8(I)
        HGY4(I)=
     &    G14(I)*VY1(I)+G24(I)*VY2(I)+G34(I)*VY3(I)+G44(I)*VY4(I)
     &   +G54(I)*VY5(I)+G64(I)*VY6(I)+G74(I)*VY7(I)+G84(I)*VY8(I)
        HGZ4(I)=
     &    G14(I)*VZ1(I)+G24(I)*VZ2(I)+G34(I)*VZ3(I)+G44(I)*VZ4(I)
     &   +G54(I)*VZ5(I)+G64(I)*VZ6(I)+G74(I)*VZ7(I)+G84(I)*VZ8(I)
      ENDDO
c      IF (ICP.EQ.1) THEN
c       DO I=LFT,LLT
c        A1(I) =UNDEMI*(UN-NU(I))
c        A2(I) =UNDEMI*(NU(I)-UN) 
c        A3(I) = ZERO
c       ENDDO 
c      ELSEIF (ICP.EQ.2) THEN
c       DO I=LFT,LLT
c        FACP=UN-DEFP(I)/(SIGY(I)/E0(I)+DEFP(I))
c        CC = FACP*(UN+NU(I))
c        A1(I) =UNDEMI*(UN-NU(I)+CC)
c        A2(I) =UNDEMI*(NU(I)-UN+CC) 
c        A3(I) = ZERO
c       ENDDO 
c      ELSE
c       DO I=LFT,LLT
c        A1(I) =UN
c        A2(I) =NU(I)
c        A3(I) = UN
c       ENDDO 
c      ENDIF
       DO I=LFT,LLT
        A1(I) =UN
        A2(I) =NU(I)
        A3(I) = UN
       ENDDO 
      !-------elstic increament, attention il y a un fac=1/3----
      DO I=LFT,LLT
       C11 = DEUX*G_3DT(I)/(UN-NU(I))
       CC = G_3DT(I)*THK_1(I)
       DH_XE1 =  CC*HGX1(I)
       DH_YK2 =  CC*HGY2(I)
       !-------reduit 0.1->0.2---------
       E = ZEP4*G_3DT(I)*(UN +NU(I))
       CC =E*THK_1(I)
       DH_ZE1 =  CC*HGZ1(I)
       DH_ZK2 =  CC*HGZ2(I)
       !-------reduit 0.01----------
       DH_Z4  =  0.33*CC*HGZ4(I)
c       DH_Z4  =  EM01*CC*HGZ4(I)
       CC = C11*SY0(I)
       DH_XE3 =  CC*HGX3(I)
       DH_XE4 =  CC*HGX4(I)
       CC = C11*RX0(I)
       DH_YK3 =  CC*HGY3(I)
       DH_YK4 =  CC*HGY4(I)
       DH_ZK3 =  G_3DT(I)*RX0(I)*HGZ3(I)
       DH_ZE3 =  G_3DT(I)*SY0(I)*HGZ3(I)
C
Cqz -----------FHOUR(1-11,I):XE1,ZE1,YK2,ZK2,XE3,YK3,ZK3,ZE3,XE4,YK4,Z4-------
       FHOUR(I,1) =  OFF(I)*FHOUR(I,1)  +  DH_XE1
       FHOUR(I,2) =  OFF(I)*FHOUR(I,2)  +  DH_ZE1
       FHOUR(I,3) =  OFF(I)*FHOUR(I,3)  +  DH_YK2
       FHOUR(I,4) =  OFF(I)*FHOUR(I,4)  +  DH_ZK2
       FHOUR(I,5) =  OFF(I)*FHOUR(I,5)  +  DH_XE3
       FHOUR(I,6) =  OFF(I)*FHOUR(I,6)  +  DH_YK3
       FHOUR(I,7) =  OFF(I)*FHOUR(I,7)  +  DH_ZK3
       FHOUR(I,8) =  OFF(I)*FHOUR(I,8)  +  DH_ZE3
       FHOUR(I,9) =  OFF(I)*FHOUR(I,9)  +  DH_XE4
       FHOUR(I,10) = OFF(I)*FHOUR(I,10) +  DH_YK4
       FHOUR(I,11) = OFF(I)*FHOUR(I,11) +  DH_Z4
C------   SIGY(I) : now min(sigy(ip)) and =sigy(e_plas=0) at begining in law2   
       IF (SIGY(I).LT.ZEP9EP30) THEN
        SE  =(UN-NU(I))*FHOUR(I,5)
        SK  =(UN-NU(I))*FHOUR(I,6)
        SR1 =SE*SE+SK*SK
        SVM =-SE*SK
        SE  =NU(I)*FHOUR(I,5)
c        SE  =NU(I)*FHOUR(I,5)-FHOUR(I,2)
        SK  =FHOUR(I,6)
c        SK  =FHOUR(I,6)-FHOUR(I,4)
        SR2 =SE*SE+SK*SK
        SVM = SVM+SE*SK
        SE  =FHOUR(I,5)
c        SE  =FHOUR(I,5)-FHOUR(I,2)
        SK  =NU(I)*FHOUR(I,6)
c        SK  =NU(I)*FHOUR(I,6)-FHOUR(I,4)
        SR3 =SE*SE+SK*SK
        SVM = SVM+SE*SK
        SR4 =(FHOUR(I,8)+FHOUR(I,1))*A3(I)
        SR5 =(FHOUR(I,7)+FHOUR(I,3))*A3(I)
        SVMR = UNDEMI*(SR1+SR2+SR3)+3*(SR4*SR4+SR5*SR5)
        SVM0 =MM(I,2)
        SVMR = SVMR + MAX(SVM,-SVM)
        SVM2 = SQRT(SVM0+COEF*SVMR)
c        SVM2 = SQRT(MM(I,2)+COEF*(SVMR+MIN(SVM,-SVM)))
!-----------memb :ELASTIC-PLASTIC yield criterion, FACM=(1-r_m)------------
        IF (SVM2.GT.SIGY(I)) THEN
          IF (FHOUR(I,12)==ZERO) FHOUR(I,12) = SIGY(I)
          IF (SVM2.GT.FHOUR(I,12)) THEN
            FAC = (SVM2-FHOUR(I,12))/SVM2
          ELSE
            FAC = (SVM2-SIGY(I))/SVM2
          END IF
          FACM = MIN(F_MAX,UN-FAC)
          FHOUR(I,12) = FHOUR(I,12)+FACM*(SVM2-FHOUR(I,12))
        ELSE
          FACM = ZERO
          FHOUR(I,12) = SVM2
        ENDIF
C------bending behavious : 
         SE  =FHOUR(I,9)*FHOUR(I,9)
         SK  =FHOUR(I,10)*FHOUR(I,10)
         SR1 = (UN +NU(I)*NU(I))*(SE+SK)
         SR2 = SR1 - DEUX*NU(I)*(SE+SK)
         SR3 = ABS((SIX*NU(I)-DEUX)*FHOUR(I,9)*FHOUR(I,10))
         SVMM = F_Z*(SR1+SR2+SR3)
         FACZM = SVMR/MAX(EM20,(SVMM+SVMR))        
         IF (MM(I,1)>SIGY(I)*SIGY(I)) THEN
           IF (SVMM>EM20) THEN
            FAC = SVMR/(SVMM+SVMR)
            FACB =UN-MIN(SQRT(FAC),UN-FACM)        
           ELSE
            FACB = FACM
           END IF
           FACB = MIN(FACB,F_MAX)         
         ELSE
           FACB = FACM
         ENDIF 
         FHOUR(I,9) = FHOUR(I,9) - FACB*DH_XE4
         FHOUR(I,10) = FHOUR(I,10) - FACB*DH_YK4
c         DP = THIRD*(UN+NU(I))*(DH_XE3+DH_YK3)
         DP = ZERO
         IF (FACM>ZERO) THEN
         !-----------Membrane terms-----------
          FHOUR(I,1) = FHOUR(I,1) - FACM*DH_XE1*A3(I)
          FHOUR(I,3) = FHOUR(I,3) - FACM*DH_YK2*A3(I)
          FHOUR(I,5) = FHOUR(I,5) - FACM*(DH_XE3-DP)
          FHOUR(I,6) = FHOUR(I,6) - FACM*(DH_YK3-DP)
          FHOUR(I,7) = FHOUR(I,7) - FACM*DH_ZK3*A3(I)
          FHOUR(I,8) = FHOUR(I,8) - FACM*DH_ZE3*A3(I)
C------only elsto-plastic in memb dominate case          
          IF (FACZM >0.99) THEN
            FHOUR(I,2) = FHOUR(I,2) - FACZM*DH_ZE1
            FHOUR(I,4) = FHOUR(I,4) - FACZM*DH_ZK2
          END IF
         ENDIF
       ENDIF
      ENDDO 
C 
      DO I=LFT,LLT
       CC = THK_1(I)*VOL(I)
       NFHX1(I) = CC*(FHOUR(I,1)+FHOUR(I,8))
       NFHZ1(I) = CC*FHOUR(I,2)
       NFHY2(I) = CC*(FHOUR(I,3)+FHOUR(I,7))
       NFHZ2(I) = CC*FHOUR(I,4)
       RX1  = SX0(I)*SX0(I)/RX0(I)+RX0(I)     
       RXY1 = SX0(I)*SY0(I)/RX0(I)+RY0(I)
       SY1  = RY0(I)*RY0(I)/SY0(I)+SY0(I)
       HHX3(I) = RX1*RX0(I)
       HHY3(I) = SY1*SY0(I)
       HXY3(I) = RXY1*RX0(I)*SQRT(NU(I))
       NFHZ3(I) = VOL(I)*(SY1*FHOUR(I,8)+RX1*FHOUR(I,7)
     .                   +SY0(I)*FHOUR(I,1)+RX0(I)*FHOUR(I,3))
       SY1  = SY1*A1(I)
       RX1  = RX1*A1(I)
       RXY1  = RXY1*A2(I)
       SYX1  = RXY1*RX0(I)/SY0(I)
       VOL_3 = THIRD*VOL(I)
       NFHX3(I) = VOL(I)*(SY1*FHOUR(I,5)+RXY1*FHOUR(I,6))
       NFHY3(I) = VOL(I)*(RX1*FHOUR(I,6)+SYX1*FHOUR(I,5))
       NFHX4(I) = VOL_3*(SY1*FHOUR(I,9)+RXY1*FHOUR(I,10))
       NFHY4(I) = VOL_3*(RX1*FHOUR(I,10)+SYX1*FHOUR(I,9))
       NFHZ4(I) =THK_1(I)*VOL_3*FHOUR(I,11)
      ENDDO
      !--------visco-termes-------
      DO I=LFT,LLT
       CC = FCL(I)*THK_1(I)/SQRT(1+NU(I))
       NFHX1(I) = NFHX1(I)+CC*(THK_1(I)*HGX1(I)+SY0(I)*HGZ3(I))
       NFHY2(I) = NFHY2(I)+CC*(THK_1(I)*HGY2(I)+RX0(I)*HGZ3(I))
       NFHZ3(I) = NFHZ3(I)+CC*(THK(I)*(HHX3(I)+HHY3(I))*HGZ3(I)
     .                         +SY0(I)*HGX1(I)+RX0(I)*HGY2(I))
       CC = QUINZE*FCL(I)*THK_1(I)*THK_1(I)
       NFHZ1(I) = NFHZ1(I)+CC*HGZ1(I)
       NFHZ2(I) = NFHZ2(I)+CC*HGZ2(I)
       NFHZ4(I) = NFHZ4(I)+CC*HGZ4(I)
       CC = FCL(I)/SQRT(1-NU(I)*NU(I))
       NFHX3(I) = NFHX3(I)+CC*(HHX3(I)*HGX3(I)+HXY3(I)*HGY3(I))
       NFHY3(I) = NFHY3(I)+CC*(HHY3(I)*HGY3(I)+HXY3(I)*HGX3(I))
       CC = THIRD*CC
       NFHX4(I) = NFHX4(I)+CC*(HHX3(I)*HGX4(I)+HXY3(I)*HGY4(I))
       NFHY4(I) = NFHY4(I)+CC*(HHY3(I)*HGY4(I)+HXY3(I)*HGX4(I))
      ENDDO
C
      DO I=LFT,LLT
        F11(I) =F11(I)-G11(I)*NFHX1(I)
     .                -G13(I)*NFHX3(I)-G14(I)*NFHX4(I)
        F12(I) =F12(I)-G21(I)*NFHX1(I)
     .                -G23(I)*NFHX3(I)-G24(I)*NFHX4(I)
        F13(I) =F13(I)-G31(I)*NFHX1(I)
     .                -G33(I)*NFHX3(I)-G34(I)*NFHX4(I)
        F14(I) =F14(I)-G41(I)*NFHX1(I)
     .                -G43(I)*NFHX3(I)-G44(I)*NFHX4(I)
        F15(I) =F15(I)-G51(I)*NFHX1(I)
     .                -G53(I)*NFHX3(I)-G54(I)*NFHX4(I)
        F16(I) =F16(I)-G61(I)*NFHX1(I)
     .                -G63(I)*NFHX3(I)-G64(I)*NFHX4(I)
        F17(I) =F17(I)-G71(I)*NFHX1(I)
     .                -G73(I)*NFHX3(I)-G74(I)*NFHX4(I)
        F18(I) =F18(I)-G81(I)*NFHX1(I)
     .                -G83(I)*NFHX3(I)-G84(I)*NFHX4(I)
C
        F21(I) =F21(I)-G12(I)*NFHY2(I)
     .                -G13(I)*NFHY3(I)-G14(I)*NFHY4(I)
        F22(I) =F22(I)-G22(I)*NFHY2(I)
     .                -G23(I)*NFHY3(I)-G24(I)*NFHY4(I)
        F23(I) =F23(I)-G32(I)*NFHY2(I)
     .                -G33(I)*NFHY3(I)-G34(I)*NFHY4(I)
        F24(I) =F24(I)-G42(I)*NFHY2(I)
     .                -G43(I)*NFHY3(I)-G44(I)*NFHY4(I)
        F25(I) =F25(I)-G52(I)*NFHY2(I)
     .                -G53(I)*NFHY3(I)-G54(I)*NFHY4(I)
        F26(I) =F26(I)-G62(I)*NFHY2(I)
     .                -G63(I)*NFHY3(I)-G64(I)*NFHY4(I)
        F27(I) =F27(I)-G72(I)*NFHY2(I)
     .                -G73(I)*NFHY3(I)-G74(I)*NFHY4(I)
        F28(I) =F28(I)-G82(I)*NFHY2(I)
     .                -G83(I)*NFHY3(I)-G84(I)*NFHY4(I)
C
        F31(I) =F31(I)-G11(I)*NFHZ1(I)-G12(I)*NFHZ2(I)
     .                -G13(I)*NFHZ3(I)-G14(I)*NFHZ4(I)
        F32(I) =F32(I)-G21(I)*NFHZ1(I)-G22(I)*NFHZ2(I)
     .                -G23(I)*NFHZ3(I)-G24(I)*NFHZ4(I)
        F33(I) =F33(I)-G31(I)*NFHZ1(I)-G32(I)*NFHZ2(I)
     .                -G33(I)*NFHZ3(I)-G34(I)*NFHZ4(I)
        F34(I) =F34(I)-G41(I)*NFHZ1(I)-G42(I)*NFHZ2(I)
     .                -G43(I)*NFHZ3(I)-G44(I)*NFHZ4(I)
        F35(I) =F35(I)-G51(I)*NFHZ1(I)-G52(I)*NFHZ2(I)
     .                -G53(I)*NFHZ3(I)-G54(I)*NFHZ4(I)
        F36(I) =F36(I)-G61(I)*NFHZ1(I)-G62(I)*NFHZ2(I)
     .                -G63(I)*NFHZ3(I)-G64(I)*NFHZ4(I)
        F37(I) =F37(I)-G71(I)*NFHZ1(I)-G72(I)*NFHZ2(I)
     .                -G73(I)*NFHZ3(I)-G74(I)*NFHZ4(I)
        F38(I) =F38(I)-G81(I)*NFHZ1(I)-G82(I)*NFHZ2(I)
     .                -G83(I)*NFHZ3(I)-G84(I)*NFHZ4(I)
      ENDDO
      !----hourglass energie est inclued in interal energie------
      DO I=LFT,LLT
        EINT(I)= EINT(I)+DT1*(
     .   NFHX1(I)*HGX1(I) + NFHZ1(I)*HGZ1(I) + 
     .   NFHY2(I)*HGY2(I) + NFHZ2(I)*HGZ2(I) + 
     .   NFHZ3(I)*HGZ3(I) + NFHZ4(I)*HGZ4(I) + 
     .   NFHX3(I)*HGX3(I) + NFHX4(I)*HGX4(I) + 
     .   NFHY3(I)*HGY3(I) + NFHY4(I)*HGY4(I) ) 
     .   /MAX(EM20,VOL0(I)) +EINTM(I)
      ENDDO
C
      RETURN
      END SUBROUTINE SCHOUR3_1
