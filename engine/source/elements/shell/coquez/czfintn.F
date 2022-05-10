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
Chd|  CZFINTN1                      source/elements/shell/coquez/czfintn.F
Chd|-- called by -----------
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|        CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CZFINTN1(JFT   ,JLT   ,THK  ,C1   ,AA   ,VHG  ,
     2                      X13   ,X24   ,Y13  ,Y24  ,Z1   ,MX23 ,
     3                      MX13  ,MX34  ,MY13 ,MY23 ,MY34 ,VGLAS,
     4                      VSTRE ,MSTRE ,VF   ,VM   ,FAC  ,A11  ,
     5                      A12   ,G     ,SHF  ,SIGY ,OFF  ,FAC1 ,
     6                      RHO   ,AREA  ,DT1  ,EINT ,AMU  ,VHI  ,
     7                      NPT   ,IPARTC,EVIS ,KFTS ,GSR  ,NEL  ,
     8                      A11SR ,A12SR ,NUSR ,SHFSR,MTN  ,FAC58) 
C elasto-plastic hourglass stresses+linear damping
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C        CALCUL VF(3,4),VM(2,4) : FORCES INTERNES GENERALISEES
C        ENTREES : THK,1/AREA,PME,X13,X24,Y13,Y24,MX13,MX34,MY13,MY34
C                  DHG(6)  : INCREMENT DE LA DEF HOURGLASS GENERALISEE    
C                  VGLAS(12): DEF HOURGLASS GENERALISEE 
C                  VGLAS(1-6)->ETA; VGLAS(7-12)->KSI;
C                  FAC=Et/E    
C        SORTIES : VF,VM
C                  VF(3,4),VM(2,4) : 
C                  J=1,2 STOCK LA PARTIE ANTISYM (F(1)=-F(3),F(2)=-F(4))                                  
C                  J=3,4 STOCK LA PARTIE SYM (F(1)=F(3),F(2)=F(4))
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include "implicit_f.inc"
#include "param_c.inc"
#include "mvsiz_p.inc"
#include "parit_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
        INTEGER JFT,JLT,NPT,IPARTC(*),NEL,MTN
        my_real 
     .       THK(*) ,C1(*)  ,AA(*)  ,VHG(MVSIZ,6),
     .       X13(*) ,X24(*) ,Y13(*) ,Y24(*)  ,Z1(*),
     .       MX13(*),MX23(*),MX34(*),MY13(*),MY23(*),MY34(*),
     .       VGLAS(NEL,12),VSTRE(NEL,5),MSTRE(NEL,3),VF(MVSIZ,3,4),VM(MVSIZ,2,4),
     .       FAC(MVSIZ,2),A11(*) ,A12(*) ,G(*),SHF(*),VHI(MVSIZ,3),SIGY(*),
     .       FAC1(*) ,RHO(*) ,AREA(*), DT1,OFF(*),EINT(JLT,2),AMU(*) ,
     .       EVIS(NPSAV,*),
     .       GSR(*), A11SR(*), A12SR(*), NUSR(*), SHFSR(*),FAC58(MVSIZ,2)
         INTEGER KFTS
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I,MX, IC, II, J, JST(MVSIZ+1)
        my_real 
     .  STIER,C2,C3,C4,C5,C7,C8,HSURA,SXX,SYY,SVM,SVMN,
     .  SS1,SS2,SS3,SC6,SC5,CXX_K,CYY_K,HXX,HYY,SF1,SF2,B13,B24,
     .  CXX,CYY,Y13S,X13S,Y34S6,Y23S5,X23S5,X34S6,HXX_K,HYY_K,
     .  BXX,BYY,BXX_K,BYY_K,
     .  C1M,C2M,C1B,C2B,CMM,CNN,UFAC,TOL,COEF,EH1,EH2,
     .  CNNX,CNNY,CNNX_K,CNNY_K,CMMX,CMMY,CMMX_K,CMMY_K,SXY0,MXY0,
     .  SVMC,SIGY2,SVM0,SVMC_K,A_BK,A_B,A_K,UNDOUZSR,AUX,
     .  SSV0,SSV1,SSV2,SSV3,SC6_V,SC5_V,CXX_V,CYY_V,CXY_V,CXZ_V,CYZ_V,
     .  HXX_V,HYY_V,HXY_V,SS1_V,SS2_V,SS3_V,SF1_V,SF2_V,HVL,
     .  EMY,ECX,ECY,ESY,COEF1,COEFH,FBEND,FBEND_V,C12,
     .  C6(MVSIZ),DGLAS(MVSIZ,12),
     .  ESX(MVSIZ),TESY(MVSIZ),EMX(MVSIZ),DHG(MVSIZ,6),ETMP(MVSIZ,2)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
      C7=QTHIRD
      COEFH= ZEP999
      COEF= ZEP85
      STIER  = CINQP333  
      TOL= EM18 
      FBEND_V =TROISP464
C
      IF (NPT.EQ.0) THEN
       COEF1=SEIZE
      ELSE
       COEF1=VINGT5
      ENDIF
      IF (NPT.EQ.1) THEN
       FBEND =ZERO
       FBEND_V =ZERO
      ELSE
       FBEND =UNDOUZE
      ENDIF
      UNDOUZSR=SQRT(UNDOUZE)

      DO I=JFT,JLT
        DHG(I,1)=VHG(I,1)*DT1
        DHG(I,2)=VHG(I,2)*DT1
        DHG(I,3)=VHG(I,3)*DT1
        DHG(I,4)=VHG(I,4)*DT1
        DHG(I,5)=VHG(I,5)*DT1
        DHG(I,6)=VHG(I,6)*DT1
      END DO
      IF(MTN==58) THEN
        IF (NPT==1) THEN
         DO I=JFT,JLT
          C3=QUATRE*AA(I)
          HXX=C3*MY34(I)
          HYY=C3*MX34(I)
          HXX_K=C3*MY23(I)
          HYY_K=C3*MX23(I)
          CXX=HXX*VGLAS(I,11)
          CYY=HYY*VGLAS(I,12)
          CXX_K=HXX_K*VGLAS(I,11)
          CYY_K=HYY_K*VGLAS(I,12)
		  C2 = A11(I)*FAC1(I)
          C1M=C2*FAC58(I,1)
          C2M=C2*FAC58(I,2)
          C12 = EM02*MIN(C1M,C2M)
          DGLAS(I,1) =C1M*CXX-C12*CYY
          DGLAS(I,2) =C2M*CYY-C12*CXX
          DGLAS(I,7) =C1M*CXX_K-C12*CYY_K
          DGLAS(I,8) =C2M*CYY_K-C12*CXX_K
         END DO
c       if (mod(ncycle,10)==0) print *,'FAC58(I,1-2)=',FAC58(nel,1),FAC58(nel,2)	   
		ELSE
         DO I=JFT,JLT
          C3=QUATRE*AA(I)
          HXX=C3*MY34(I)
          HYY=C3*MX34(I)
          HXX_K=C3*MY23(I)
          HYY_K=C3*MX23(I)
          CXX=HXX*DHG(I,1)
          CYY=HYY*DHG(I,2)
          CXX_K=HXX_K*DHG(I,1)
          CYY_K=HYY_K*DHG(I,2)
          BXX=HXX*DHG(I,3)
          BYY=HYY*DHG(I,4)
          BXX_K=HXX_K*DHG(I,3)
          BYY_K=HYY_K*DHG(I,4)
		  C2 = A11(I)*FAC1(I)
          C1M=C2*FAC58(I,1)
          C2M=C2*FAC58(I,2)
          C12 = EM02*MIN(C1M,C2M)
Cqz    -----------for energy calculation-Mean_value--------
          C6(I)=C1(I)*FBEND
          SS1= MY34(I)*VGLAS(I,1) +MY23(I)*VGLAS(I,7)
          SS2= MX23(I)*VGLAS(I,8) +MX34(I)*VGLAS(I,2)
          SF1= MY34(I)*VGLAS(I,3) +MY23(I)*VGLAS(I,9)
          SF2= -MX23(I)*VGLAS(I,10)-MX34(I)*VGLAS(I,4)
          SC5= MY34(I)*VGLAS(I,5) +MX34(I)*VGLAS(I,6) 
          SC6= MY23(I)*VGLAS(I,11)+MX23(I)*VGLAS(I,12)
          C5=UNDEMI*OFF(I)*THK(I)*C7
          ESX(I)= SS1*DHG(I,1)+SS2*DHG(I,2)
          ETMP(I,1) = C5*(ESX(I) +
     .                    QUART*(SC5*DHG(I,5)+SC6*DHG(I,6)))
          EMX(I)= (SF1*DHG(I,3)-SF2*DHG(I,4))*C6(I)
          ETMP(I,2) = C5*EMX(I)
          DGLAS(I,1) =C1M*CXX-C12*CYY
          DGLAS(I,2) =C2M*CYY-C12*CXX
          DGLAS(I,3) =C1M*BXX-C12*BYY
          DGLAS(I,4) =C2M*BYY-C12*BXX
          DGLAS(I,7) =C1M*CXX_K-C12*CYY_K
          DGLAS(I,8) =C2M*CYY_K-C12*CXX_K
          DGLAS(I,9) =C1M*BXX_K-C12*BYY_K
          DGLAS(I,10)=C2M*BYY_K-C12*BXX_K
          C2=FAC1(I)*G(I)*SHF(I)*O64TH
          DGLAS(I,5) =C2*HXX*DHG(I,5)
          DGLAS(I,6) =C2*HYY*DHG(I,5)
          DGLAS(I,11)=C2*HXX_K*DHG(I,6)
          DGLAS(I,12)=C2*HYY_K*DHG(I,6)
         END DO
        END IF !(NPT==1) THEN
      ELSE
      DO I=JFT,JLT
        C3=QUATRE*AA(I)
        HXX=C3*MY34(I)
        HYY=C3*MX34(I)
        HXX_K=C3*MY23(I)
        HYY_K=C3*MX23(I)
        CXX=HXX*DHG(I,1)
        CYY=HYY*DHG(I,2)
        CXX_K=HXX_K*DHG(I,1)
        CYY_K=HYY_K*DHG(I,2)
        BXX=HXX*DHG(I,3)
        BYY=HYY*DHG(I,4)
        BXX_K=HXX_K*DHG(I,3)
        BYY_K=HYY_K*DHG(I,4)
        C1M=A11(I)*FAC1(I)
        C2M=A12(I)*FAC1(I)
Cqz    -----------for energy calculation-Mean_value--------
        C6(I)=C1(I)*FBEND
        SS1= MY34(I)*VGLAS(I,1) +MY23(I)*VGLAS(I,7)
        SS2= MX23(I)*VGLAS(I,8) +MX34(I)*VGLAS(I,2)
        SF1= MY34(I)*VGLAS(I,3) +MY23(I)*VGLAS(I,9)
        SF2= -MX23(I)*VGLAS(I,10)-MX34(I)*VGLAS(I,4)
        SC5= MY34(I)*VGLAS(I,5) +MX34(I)*VGLAS(I,6) 
        SC6= MY23(I)*VGLAS(I,11)+MX23(I)*VGLAS(I,12)
        C5=UNDEMI*OFF(I)*THK(I)*C7
        ESX(I)= SS1*DHG(I,1)+SS2*DHG(I,2)
        ETMP(I,1) = C5*(ESX(I) +
     .              QUART*(SC5*DHG(I,5)+SC6*DHG(I,6)))
        EMX(I)= (SF1*DHG(I,3)-SF2*DHG(I,4))*C6(I)
        ETMP(I,2) = C5*EMX(I)
        DGLAS(I,1) =C1M*CXX-C2M*CYY
        DGLAS(I,2) =C1M*CYY-C2M*CXX
        DGLAS(I,3) =C1M*BXX-C2M*BYY
        DGLAS(I,4) =C1M*BYY-C2M*BXX
        DGLAS(I,7) =C1M*CXX_K-C2M*CYY_K
        DGLAS(I,8) =C1M*CYY_K-C2M*CXX_K
        DGLAS(I,9) =C1M*BXX_K-C2M*BYY_K
        DGLAS(I,10)=C1M*BYY_K-C2M*BXX_K
        C2=FAC1(I)*G(I)*SHF(I)*O64TH
        DGLAS(I,5) =C2*HXX*DHG(I,5)
        DGLAS(I,6) =C2*HYY*DHG(I,5)
        DGLAS(I,11)=C2*HXX_K*DHG(I,6)
        DGLAS(I,12)=C2*HYY_K*DHG(I,6)
      END DO
      END IF !IF(MTN==58 )
      
      IF(MTN==58 .AND.NPT==1) THEN
       DO I=JFT,JLT
Cqz    -----------for energy calculation-Mean_value--------
        C8 = C7*OFF(I)
        SS1= (MY34(I)*DGLAS(I,1) +MY23(I)*DGLAS(I,7))*C8
        SS2= (MX23(I)*DGLAS(I,8) +MX34(I)*DGLAS(I,2))*C8
        HSURA=THK(I)*AA(I)
        HVL = AMU(I)*SQRT(RHO(I)*AREA(I)*FAC1(I))*OFF(I)
        SSV0=MY23(I)*MY23(I)
        SSV1=MY34(I)*MY34(I)
        SSV2=MX23(I)*MX23(I)
        SSV3=MX34(I)*MX34(I)
        HXX_V= STIER*(SSV1+SSV0)
        HXY_V=-STIER*(MY34(I)*MX34(I)+MY23(I)*MX23(I))
        HYY_V= STIER*(SSV2+SSV3)
        AUX=AA(I)*HVL
        C1M = A11SR(I)*AUX
        C2M = A12SR(I)*AUX
        CXX_V=C1M*HXX_V
        CYY_V=C1M*HYY_V
        CXY_V=C2M*HXY_V
        SS1_V=CXX_V*VHG(I,1)+CXY_V*VHG(I,2)
        SS2_V=CYY_V*VHG(I,2)+CXY_V*VHG(I,1)
C--------------------------------------------------
        SS1=SS1+SS1_V
        SS2=SS2+SS2_V
        C2=QUART*THK(I)
C--------NOEUDS 1,3-------
        B13=(MY13(I)*X24(I)-MX13(I)*Y24(I))*HSURA
        VF(I,1,1)=VF(I,1,1)+B13*SS1
        VF(I,1,3)=C2*SS1
        VF(I,2,1)=VF(I,2,1)+B13*SS2
        VF(I,2,3)=C2*SS2
C--------NOEUDS 2,4-------
        B24=(MX13(I)*Y13(I)-MY13(I)*X13(I))*HSURA
        VF(I,1,2)= VF(I,1,2)+B24*SS1
        VF(I,1,4)=-VF(I,1,3)
        VF(I,2,2)= VF(I,2,2)+B24*SS2
        VF(I,2,4)=-VF(I,2,3)
C
        C2=Z1(I)*HSURA
        VF(I,3,1)=VF(I,3,1)+C2*(SS1*Y24(I)-SS2*X24(I))
        VF(I,3,2)=VF(I,3,2)+C2*(-SS1*Y13(I)+SS2*X13(I))
Cqz    -----------for energy calculation-Mean_value--------
        ESY= ((SS1-SS1_V)*DHG(I,1)+(SS2-SS2_V)*DHG(I,2))*THK(I)
        ETMP(I,1) = ESY
        ETMP(I,2) = ZERO
Cqz     ----viscous energy is removed to hourglass energy----
        TESY(I)= (SS1_V*DHG(I,1)+SS2_V*DHG(I,2))*THK(I)
       ENDDO
	  ELSE
      DO I=JFT,JLT
Cqz    ---------elstic increament----------
        VGLAS(I,1) =VGLAS(I,1) +DGLAS(I,1)
        VGLAS(I,2) =VGLAS(I,2) +DGLAS(I,2)
        VGLAS(I,3) =VGLAS(I,3) +DGLAS(I,3)
        VGLAS(I,4) =VGLAS(I,4) +DGLAS(I,4)
        VGLAS(I,7) =VGLAS(I,7) +DGLAS(I,7)
        VGLAS(I,8) =VGLAS(I,8) +DGLAS(I,8)
        VGLAS(I,9) =VGLAS(I,9) +DGLAS(I,9)
        VGLAS(I,10)=VGLAS(I,10)+DGLAS(I,10)
Cqz-----------/4 par rapport formulation-------	  
        VGLAS(I,5) =VGLAS(I,5) +DGLAS(I,5)
        VGLAS(I,6) =VGLAS(I,6) +DGLAS(I,6)
        VGLAS(I,11)=VGLAS(I,11)+DGLAS(I,11)
        VGLAS(I,12)=VGLAS(I,12)+DGLAS(I,12)
C-------------enint------------------
        EINT(I,1) = EINT(I,1)+ETMP(I,1)
        EINT(I,2) = EINT(I,2)+ETMP(I,2)
      END DO
C
      DO I=JFT,JLT
        IF (SIGY(I).LT.ZEP9EP30) THEN
          UFAC=ABS(MIN(FAC(I,1),FAC(I,2))-UN)
          SIGY2 = SIGY(I)*SIGY(I)
          SVM =ZERO
          SXY0=ZERO   
          IF (UFAC.LT.TOL) THEN
            SXY0= VSTRE(I,1)*VSTRE(I,1)+VSTRE(I,2)*VSTRE(I,2)
     .           -VSTRE(I,1)*VSTRE(I,2)+TROIS*VSTRE(I,3)*VSTRE(I,3)
            MXY0= MSTRE(I,1)*MSTRE(I,1)+MSTRE(I,2)*MSTRE(I,2)
     .           -MSTRE(I,1)*MSTRE(I,2)+TROIS*MSTRE(I,3)*MSTRE(I,3)
            CNN=COEF
            CMM=COEF*THK(I)*UNSEIZE
            CNNX=CNN*VGLAS(I,1)
            CNNY=CNN*VGLAS(I,2)
            CNNX_K=CNN*VGLAS(I,7)
            CNNY_K=CNN*VGLAS(I,8)
            CMMX=CMM*VGLAS(I,3)
            CMMY=CMM*VGLAS(I,4)
            CMMX_K=CMM*VGLAS(I,9)
            CMMY_K=CMM*VGLAS(I,10)
            SXY0= SXY0+CNNX*CNNX+CNNY*CNNY-CNNX*CNNY
            MXY0= MXY0+CMMX*CMMX+CMMY*CMMY-CMMX*CMMY
            SXY0= SXY0+CNNX_K*CNNX_K+CNNY_K*CNNY_K-CNNX_K*CNNY_K
            MXY0= MXY0+CMMX_K*CMMX_K+CMMY_K*CMMY_K-CMMX_K*CMMY_K
            SXY0= SXY0+ABS(CNNX*(DEUX*CNNX_K-CNNY_K)
     1                    +CNNY*(DEUX*CNNY_K-CNNX_K))
            MXY0= MXY0+ABS(CMMX*(DEUX*CMMX_K-CMMY_K)
     1                    +CMMY*(DEUX*CMMY_K-CMMX_K))     
            SVM =  SXY0+COEF1*MXY0
          ENDIF 
C-----------ELASTIC-PLASTIC global yield criterion------------
          IF (UFAC.GE.TOL.OR.SVM.GT.SIGY2) THEN
            EH1=MIN(SXY0/MAX(SIGY2,TOL),UN)
            EH1=MAX(COEFH*EH1,(UN-FAC(I,1)))
            EH2=MAX(COEFH,(UN-FAC(I,2)))
            IF (ESX(I).LT.ZERO) EH1=ZERO
            IF (EMX(I).LT.ZERO) EH2=ZERO
C-------------MEMBRANE------------------------
            VGLAS(I,1)=VGLAS(I,1)-EH1*DGLAS(I,1)
            VGLAS(I,2)=VGLAS(I,2)-EH1*DGLAS(I,2)
            VGLAS(I,7)=VGLAS(I,7)-EH1*DGLAS(I,7)
            VGLAS(I,8)=VGLAS(I,8)-EH1*DGLAS(I,8)
C-------------FLEXION------------------------
            VGLAS(I,3) =VGLAS(I,3) -EH2*DGLAS(I,3)
            VGLAS(I,4) =VGLAS(I,4) -EH2*DGLAS(I,4)
            VGLAS(I,9) =VGLAS(I,9) -EH2*DGLAS(I,9)
            VGLAS(I,10)=VGLAS(I,10)-EH2*DGLAS(I,10)
          ENDIF
        ENDIF 
      END DO

      DO I=JFT,JLT
Cqz    -----------for energy calculation-Mean_value--------
        C8 = C7*OFF(I)
        SS1= (MY34(I)*VGLAS(I,1) +MY23(I)*VGLAS(I,7))*C8
        SS2= (MX23(I)*VGLAS(I,8) +MX34(I)*VGLAS(I,2))*C8
        SF1= (MY34(I)*VGLAS(I,3) +MY23(I)*VGLAS(I,9))*C8
        SF2= -(MX23(I)*VGLAS(I,10)+MX34(I)*VGLAS(I,4))*C8
        HSURA=THK(I)*AA(I)
        C2 = C8*THK(I)
        SC5=(MY34(I)*VGLAS(I,5) +MX34(I)*VGLAS(I,6) )*C2
        SC6=(MY23(I)*VGLAS(I,11)+MX23(I)*VGLAS(I,12))*C2
        SS3=SC5+SC6
        HVL = AMU(I)*SQRT(RHO(I)*AREA(I)*FAC1(I))*OFF(I)
        SSV0=MY23(I)*MY23(I)
        SSV1=MY34(I)*MY34(I)
        SSV2=MX23(I)*MX23(I)
        SSV3=MX34(I)*MX34(I)
        HXX_V= STIER*(SSV1+SSV0)
        HXY_V=-STIER*(MY34(I)*MX34(I)+MY23(I)*MX23(I))
        HYY_V= STIER*(SSV2+SSV3)
        C2 =HVL*GSR(I)*SHFSR(I)*UNDOUZSR
        CXZ_V=(SSV1+SSV3)*C2
        CYZ_V=(SSV2+SSV0)*C2
        AUX=AA(I)*HVL
        C1M = A11SR(I)*AUX
        C2M = A12SR(I)*AUX
        CXX_V=C1M*HXX_V
        CYY_V=C1M*HYY_V
        CXY_V=C2M*HXY_V
        SS1_V=CXX_V*VHG(I,1)+CXY_V*VHG(I,2)
        SS2_V=CYY_V*VHG(I,2)+CXY_V*VHG(I,1)
        SF1_V= (CXX_V*VHG(I,3)+CXY_V*VHG(I,4))*FBEND_V
        SF2_V=(-CYY_V*VHG(I,4)-CXY_V*VHG(I,3))*FBEND_V
        SC5_V=CXZ_V*VHG(I,5)*HSURA
        SC6_V=CYZ_V*VHG(I,6)*HSURA
        SS3_V = SC5_V+SC6_V
C--------------------------------------------------
        SS1=SS1+SS1_V
        SS2=SS2+SS2_V
        SS3=SS3+SS3_V
        SC5=SC5+SC5_V
        SC6=SC6+SC6_V
        SF1=SF1+SF1_V
        SF2=SF2+SF2_V
        Y13S= MY13(I)*SS3
        X13S= MX13(I)*SS3
        Y34S6=MY34(I)*SC6
        Y23S5=MY23(I)*SC5
        X23S5=MX23(I)*SC5
        X34S6=MX34(I)*SC6
        C2=QUART*THK(I)
C--------NOEUDS 1,3-------
        B13=(MY13(I)*X24(I)-MX13(I)*Y24(I))*HSURA
        VF(I,1,1)=VF(I,1,1)+B13*SS1
        VF(I,1,3)=C2*SS1
        VF(I,2,1)=VF(I,2,1)+B13*SS2
        VF(I,2,3)=C2*SS2
        VF(I,3,3)=SS3
C--------NOEUDS 2,4-------
        B24=(MX13(I)*Y13(I)-MY13(I)*X13(I))*HSURA
        VF(I,1,2)= VF(I,1,2)+B24*SS1
        VF(I,1,4)=-VF(I,1,3)
        VF(I,2,2)= VF(I,2,2)+B24*SS2
        VF(I,2,4)=-VF(I,2,3)
        VF(I,3,4)=-VF(I,3,3)
C--------NOEUDS 1,3-------
        C3=C6(I)*B13
        C4=C6(I)*C2
        VM(I,1,1)=VM(I,1,1)+C3*SF2+Y23S5+Y34S6
        VM(I,1,3)=VM(I,1,3)+C4*SF2-Y13S
        VM(I,2,1)=VM(I,2,1)+C3*SF1-X23S5-X34S6
        VM(I,2,3)=VM(I,2,3)+C4*SF1+X13S
C--------NOEUDS 2,4-------
        C3=C6(I)*B24
        VM(I,1,2)=VM(I,1,2)+C3*SF2+Y23S5-Y34S6
        VM(I,1,4)=VM(I,1,4)-C4*SF2-Y13S
        VM(I,2,2)=VM(I,2,2)+C3*SF1-X23S5+X34S6
        VM(I,2,4)=VM(I,2,4)-C4*SF1+X13S
C
        C2=Z1(I)*HSURA
        VF(I,3,1)=VF(I,3,1)+C2*(SS1*Y24(I)-SS2*X24(I))
        VF(I,3,2)=VF(I,3,2)+C2*(-SS1*Y13(I)+SS2*X13(I))
C
Cqz    -----------for energy calculation-Mean_value--------
        ESY= ((SS1-SS1_V)*DHG(I,1)+(SS2-SS2_V)*DHG(I,2))*THK(I)+
     .         QUART*((SC5-SC5_V)*DHG(I,5)+(SC6-SC6_V)*DHG(I,6))
        ETMP(I,1) = UNDEMI*ESY
        EMY= (SF1-SF1_V)*DHG(I,3)-(SF2-SF2_V)*DHG(I,4)
        ETMP(I,2) = UNDEMI*C6(I)*EMY*THK(I)
Cqz     ----viscous energy is removed to hourglass energy----
        TESY(I)= (SS1_V*DHG(I,1)+SS2_V*DHG(I,2))*THK(I)+
     .         (SF1_V*DHG(I,3)-SF2_V*DHG(I,4))*THK(I)*C6(I)+
     .         QUART*(SC5_V*DHG(I,5)+SC6_V*DHG(I,6))
      ENDDO 
      END IF!(MTN==58 .AND.NPT==1) THEN
C
      DO I=JFT,JLT
        EINT(I,1) = EINT(I,1)+ETMP(I,1)
        EINT(I,2) = EINT(I,2)+ETMP(I,2)
      END DO

      IC=1
      JST(IC)=JFT
      DO J=JFT+1,JLT
         IF (IPARTC(J).NE.IPARTC(J-1)) THEN
            IC=IC+1
            JST(IC)=J
         ENDIF
      ENDDO
      JST(IC+1)=JLT+1
      IF(IC.EQ.1) THEN
        MX = IPARTC(JFT)
#include "simd.inc"
        DO I=JFT,JLT
          EVIS(8,MX)=EVIS(8,MX) + TESY(I)
        ENDDO
      ELSEIF(IC.EQ.2.AND.KFTS.GT.0)THEN
        MX = IPARTC(JFT)
#include "simd.inc"
        DO I=JFT, KFTS-1
          EVIS(8,MX)=EVIS(8,MX) + TESY(I)
        ENDDO
        MX = IPARTC(JLT)
#include "simd.inc"
        DO I=KFTS,JLT
          EVIS(8,MX)=EVIS(8,MX) + TESY(I)
        ENDDO

      ELSE
        DO II=1,IC
           MX=IPARTC(JST(II))
           IF (JST(II+1)-JST(II).GT.15) THEN
#include "simd.inc"
              DO J=JST(II),JST(II+1)-1
                EVIS(8,MX)=EVIS(8,MX) + TESY(J)
              ENDDO
           ELSE
              DO J=JST(II),JST(II+1)-1
                EVIS(8,MX)=EVIS(8,MX) + TESY(J)
              ENDDO
           ENDIF
        ENDDO
      ENDIF
      RETURN
      END

Cqz -------nouvelle routine----------------------
Chd|====================================================================
Chd|  CZFINTNM                      source/elements/shell/coquez/czfintn.F
Chd|-- called by -----------
Chd|        CZFINTNM1                     source/elements/shell/coquez/czfintn.F
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|        CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CZFINTNM(JFT  ,JLT  ,THK  ,AA   ,VHG  ,
     2                      X13  ,X24  ,Y13  ,Y24  ,VF   ,
     3                      G    ,RHO  ,AREA ,AMU  ,DT1  ,
     4                      OFF  ,IPARTC,EVIS,KFTS  )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C        CALCUL  viscous VF(3,*) due au shear when npt=1
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "mvsiz_p.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
        INTEGER JFT,JLT,IPARTC(*)
        my_real 
     .       THK(*) ,AA(*)  ,VHG(MVSIZ,6),
     .       X13(*) ,X24(*) ,Y13(*) ,Y24(*)  ,
     .       VF(MVSIZ,3,4),G(*),
     .       RHO(*) ,AREA(*), OFF(*),AMU(*) ,
     .       EVIS(NPSAV,*),DT1
         INTEGER KFTS
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I,MX, IC, II, J, JST(MVSIZ+1)
        my_real 
     .  C2,HSURA,MX23,MY23,MX34,MY34,
     .  SC6_V,SC5_V,CXZ_V,CYZ_V,SS3_V,HVL,
     .  TESY(MVSIZ)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
       DO I=JFT,JLT
         MX23=UNDEMI*(X13(I)-X24(I))
         MX34=UNDEMI*(X13(I)+X24(I))
         MY23=UNDEMI*(Y13(I)-Y24(I))
         MY34=UNDEMI*(Y13(I)+Y24(I))
         C2 =UNDOUZE*G(I)*RHO(I)*AREA(I)
         HVL = VINGT5*AMU(I)*SQRT(C2)*OFF(I)
         CXZ_V=(MY34*MY34+MX34*MX34)*HVL
         CYZ_V=(MY23*MY23+MX23*MX23)*HVL
         HSURA=THK(I)*AA(I)
         SC5_V=CXZ_V*VHG(I,5)*HSURA
         SC6_V=CYZ_V*VHG(I,6)*HSURA
         SS3_V = SC5_V+SC6_V
C--------NOEUDS 1,3-------
         VF(I,3,3)=VF(I,3,3)+SS3_V
C--------NOEUDS 2,4-------
         VF(I,3,4)=VF(I,3,4)-SS3_V
         TESY(I)= (SC5_V*VHG(I,5)+SC6_V*VHG(I,6))*DT1
       ENDDO 
C
       MX = IPARTC(JFT)
#include "simd.inc"
       DO I=JFT,JLT
           EVIS(8,MX)=EVIS(8,MX) + TESY(I)
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  CZFINTNRZ                     source/elements/shell/coquez/czfintn.F
Chd|-- called by -----------
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CZFINTNRZ(JFT  ,JLT  ,THK  ,C1   ,AA   ,VHG  ,
     2                      X13  ,X24  ,Y13  ,Y24  ,Z1   ,MX23 ,
     3                      MX13 ,MX34 ,MY13 ,MY23 ,MY34 ,VGLAS,
     4                      VSTRE,MSTRE,VF   ,VM   ,FAC  ,A11  ,
     5                      A12  ,G    ,SHF  ,SIGY ,OFF  ,FAC1 , 
     6                      RHO  ,AREA ,  DT1,EINT ,AMU  ,VHI  ,
     7                      NPT  ,IPARTC,EVIS,KFTS  ,GSR  ,
     8                      A11SR,A12SR ,NUSR,SHFSR,BMKRZ,BMERZ,
     9                      VHGZK,VHGZE ,KRZ ,VMZ  ,NEL  ) 
C elasto-plastic hourglass stresses+linear damping
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C        CALCUL VF(3,4),VM(2,4) : FORCES INTERNES GENERALISEES
C        ENTREES : THK,1/AREA,PME,X13,X24,Y13,Y24,MX13,MX34,MY13,MY34
C                  DHG(6)  : INCREMENT DE LA DEF HOURGLASS GENERALISEE    
C                  VGLAS(12): DEF HOURGLASS GENERALISEE 
C                  VGLAS(1-6)->ETA; VGLAS(7-12)->KSI;
C                  -shear: VGLAS(13-14)->ETA; VGLAS(15-16)->KSI; 
C                  -Sig_rz: VGLAS(17)->ETA; VGLAS(18)->KSI; VGLAS(19)->const
C                  FAC=Et/E    
C        SORTIES : VF,VM
C                  VF(3,4),VM(2,4) : 
C                  J=1,2 STOCK LA PARTIE ANTISYM (F(1)=-F(3),F(2)=-F(4))                                  
C                  J=3,4 STOCK LA PARTIE SYM (F(1)=F(3),F(2)=F(4))
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "mvsiz_p.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
        INTEGER JFT,JLT,NPT,IPARTC(*),NEL
        my_real 
     .       THK(*) ,C1(*)  ,AA(*)  ,VHG(MVSIZ,6),
     .       X13(*) ,X24(*) ,Y13(*) ,Y24(*)  ,Z1(*),
     .       MX13(*),MX23(*),MX34(*),MY13(*),MY23(*),MY34(*),
     .       VGLAS(NEL,19),VSTRE(NEL,5),MSTRE(NEL,3),VF(MVSIZ,3,4),VM(MVSIZ,2,4),
     .       FAC(MVSIZ,2),A11(*) ,A12(*) ,G(*),SHF(*),VHI(MVSIZ,3),SIGY(*),
     .       FAC1(*) ,RHO(*) ,AREA(*), DT1,OFF(*),EINT(JLT,2),AMU(*) ,
     .       EVIS(NPSAV,*),VMZ(MVSIZ,4),
     .       VHGZK(MVSIZ,5),VHGZE(MVSIZ,5),KRZ(*),
     .       BM0RZ(MVSIZ,4,4),BMKRZ(MVSIZ,4,4),BMERZ(MVSIZ,4,4),
     .       GSR(*), A11SR(*), A12SR(*), NUSR(*), SHFSR(*)
         INTEGER KFTS
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I,MX, IC, II, J, JST(MVSIZ+1)
        my_real 
     .  STIER,C2,C3,C4,C5,C7,C8,HSURA,SXX,SYY,SVM,SVMN,
     .  SS1,SS2,SS3,SC6,SC5,CXX_K,CYY_K,HXX,HYY,SF1,SF2,B13,B24,
     .  CXX,CYY,Y13S,X13S,Y34S6,Y23S5,X23S5,X34S6,HXX_K,HYY_K,
     .  BXX,BYY,BXX_K,BYY_K,
     .  C1M,C2M,C1B,C2B,CMM,CNN,UFAC,TOL,COEF,EH1,EH2,
     .  CNNX,CNNY,CNNX_K,CNNY_K,CMMX,CMMY,CMMX_K,CMMY_K,SXY0,MXY0,
     .  SVMC,SIGY2,SVM0,SVMC_K,A_BK,A_B,A_K,UNDOUZSR,AUX,
     .  SSV0,SSV1,SSV2,SSV3,SC6_V,SC5_V,CXX_V,CYY_V,CXY_V,CXZ_V,CYZ_V,
     .  HXX_V,HYY_V,HXY_V,SS1_V,SS2_V,SS3_V,SF1_V,SF2_V,HVL,CRZ,
     .  EMY,ECX,ECY,ESY,COEF1,COEFH,FBEND,FBEND_V,
     .  CNNXY,CNNXY_K,CMMXY,CMMXY_K,SSZ1,SSZ2,C3M,ERZ,
     .  C6(MVSIZ), ESX(MVSIZ), TESY(MVSIZ), EMX(MVSIZ),
     .  DGLAS(MVSIZ,18),
     .  DHG(MVSIZ,6), DHGZK(MVSIZ,5),DHGZE(MVSIZ,5), ETMP(MVSIZ,2)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
      C7=QTHIRD
      COEFH= ZEP999
      COEF= ZEP85
      STIER  = CINQP333  
      TOL= EM18 
      FBEND_V =TROISP464
      IF (NPT.EQ.0) THEN
       COEF1=SEIZE
      ELSE
       COEF1=VINGT5
      ENDIF
      IF (NPT.EQ.1) THEN
       FBEND =ZERO
       FBEND_V =ZERO
      ELSE
       FBEND =UNDOUZE
      ENDIF
      UNDOUZSR=SQRT(UNDOUZE)
C
      DO I=JFT,JLT
         DHG(I,1)=VHG(I,1)*DT1
         DHG(I,2)=VHG(I,2)*DT1
         DHG(I,3)=VHG(I,3)*DT1
         DHG(I,4)=VHG(I,4)*DT1
         DHG(I,5)=VHG(I,5)*DT1
         DHG(I,6)=VHG(I,6)*DT1
         DHGZK(I,1)=VHGZK(I,1)*DT1
         DHGZK(I,2)=VHGZK(I,2)*DT1
         DHGZK(I,3)=VHGZK(I,3)*DT1
         DHGZK(I,4)=VHGZK(I,4)*DT1
         DHGZK(I,5)=VHGZK(I,5)*DT1
         DHGZE(I,1)=VHGZE(I,1)*DT1
         DHGZE(I,2)=VHGZE(I,2)*DT1
         DHGZE(I,3)=VHGZE(I,3)*DT1
         DHGZE(I,4)=VHGZE(I,4)*DT1
         DHGZE(I,5)=VHGZE(I,5)*DT1
         C3=QUATRE*AA(I)
         HXX=C3*MY34(I)
         HYY=C3*MX34(I)
         HXX_K=C3*MY23(I)
         HYY_K=C3*MX23(I)
         CXX=HXX*DHG(I,1)
         CYY=HYY*DHG(I,2)
         CXX_K=HXX_K*DHG(I,1)
         CYY_K=HYY_K*DHG(I,2)
         BXX=HXX*DHG(I,3)
         BYY=HYY*DHG(I,4)
         BXX_K=HXX_K*DHG(I,3)
         BYY_K=HYY_K*DHG(I,4)
         C1M=A11(I)*FAC1(I)
         C2M=A12(I)*FAC1(I)
         C3M=G(I)*FAC1(I)
         CRZ=UNDEMI*KRZ(I)*FAC1(I)
Cqz    -----------for energy calculation-Mean_value--------
         C6(I)=C1(I)*FBEND
         SS1= MY34(I)*VGLAS(I,1) +MY23(I)*VGLAS(I,7)
         SS2= MX23(I)*VGLAS(I,8) +MX34(I)*VGLAS(I,2)
         SS1 = SS1 - MX34(I)*VGLAS(I,13)+MX23(I)*VGLAS(I,15)
     .       +UNDEMI*(MX34(I)*VGLAS(I,17)-MX23(I)*VGLAS(I,18))
         SS2 = SS2 + MY34(I)*VGLAS(I,13) -MY23(I)*VGLAS(I,15)
     .       +UNDEMI*(MY34(I)*VGLAS(I,17)-MY23(I)*VGLAS(I,18))
C-------add Srz---------
         C8 = C7*OFF(I)
         C3= UNDEMI*C8
         SS1= SS1 + C3*(MX34(I)*VGLAS(I,17)-MX23(I)*VGLAS(I,18))    
         SS2= SS2 + C3*(MY34(I)*VGLAS(I,17)-MY23(I)*VGLAS(I,18))    
         SF1= MY34(I)*VGLAS(I,3) +MY23(I)*VGLAS(I,9)
         SF2= -MX23(I)*VGLAS(I,10)-MX34(I)*VGLAS(I,4)
         SF1=SF1+(-MX34(I)*VGLAS(I,14)+MX23(I)*VGLAS(I,16))
         SF2=SF2-(MY34(I)*VGLAS(I,14)-MY23(I)*VGLAS(I,16))
         SC5= MY34(I)*VGLAS(I,5) +MX34(I)*VGLAS(I,6) 
         SC6= MY23(I)*VGLAS(I,11)+MX23(I)*VGLAS(I,12)
         ERZ = VGLAS(I,1)*DHGZE(I,1)-VGLAS(I,2)*DHGZE(I,2)-
     .         VGLAS(I,7)*DHGZK(I,1)+VGLAS(I,8)*DHGZK(I,2)+
     .         VGLAS(I,13)*DHGZE(I,3)+VGLAS(I,15)*DHGZK(I,3)+
     .   UNDEMI*(VGLAS(I,17)*DHGZE(I,4)+VGLAS(I,18)*DHGZK(I,4))
         C5=UNDEMI*OFF(I)*THK(I)*C7
         ESX(I)= SS1*DHG(I,1)+SS2*DHG(I,2)+ERZ*AREA(I)*QUART
         ETMP(I,1) = C5*(ESX(I) +
     .               QUART*(SC5*DHG(I,5)+SC6*DHG(I,6)))
         EMX(I)= (SF1*DHG(I,3)-SF2*DHG(I,4))*C6(I)
         ETMP(I,2) = C5*EMX(I)
C
         DGLAS(I,1) =C1M*CXX-C2M*CYY+C1M*DHGZE(I,1)+C2M*DHGZE(I,2)
         DGLAS(I,2) =C1M*CYY-C2M*CXX-C1M*DHGZE(I,2)-C2M*DHGZE(I,1)
         DGLAS(I,3) =C1M*BXX-C2M*BYY
         DGLAS(I,4) =C1M*BYY-C2M*BXX
         DGLAS(I,7) =C1M*CXX_K-C2M*CYY_K-C1M*DHGZK(I,1)-C2M*DHGZK(I,2)
         DGLAS(I,8) =C1M*CYY_K-C2M*CXX_K+C1M*DHGZK(I,2)+C2M*DHGZK(I,1)
         DGLAS(I,9) =C1M*BXX_K-C2M*BYY_K
         DGLAS(I,10)=C1M*BYY_K-C2M*BXX_K
         C2=FAC1(I)*G(I)*SHF(I)*O64TH
         DGLAS(I,5) =C2*HXX*DHG(I,5)
         DGLAS(I,6) =C2*HYY*DHG(I,5)
         DGLAS(I,11)=C2*HXX_K*DHG(I,6)
         DGLAS(I,12)=C2*HYY_K*DHG(I,6)
Cqz    --------add shear due to rz dof--no sign change like 2,4,7,9-------
         DGLAS(I,13)=C3M*(HXX*DHG(I,2)-HYY*DHG(I,1)+DHGZE(I,3))
         DGLAS(I,14)=C3M*(HXX*DHG(I,4)-HYY*DHG(I,3))
         DGLAS(I,15)=C3M*(-HXX_K*DHG(I,2)+HYY_K*DHG(I,1)+DHGZK(I,3))
         DGLAS(I,16)=C3M*(-HXX_K*DHG(I,4)+HYY_K*DHG(I,3))
Cqz    --------add  srz dof--------
         DGLAS(I,17)=CRZ*DHGZE(I,5)
         DGLAS(I,18)=CRZ*DHGZK(I,5)
      END DO
C
      DO I=JFT,JLT
Cqz    ---------elstic increament----------
        VGLAS(I,1) =VGLAS(I,1) +DGLAS(I,1)
        VGLAS(I,2) =VGLAS(I,2) +DGLAS(I,2)
        VGLAS(I,3) =VGLAS(I,3) +DGLAS(I,3)
        VGLAS(I,4) =VGLAS(I,4) +DGLAS(I,4)
        VGLAS(I,5) =VGLAS(I,5) +DGLAS(I,5)
        VGLAS(I,6) =VGLAS(I,6) +DGLAS(I,6)
        VGLAS(I,7) =VGLAS(I,7) +DGLAS(I,7)
        VGLAS(I,8) =VGLAS(I,8) +DGLAS(I,8)
        VGLAS(I,9) =VGLAS(I,9) +DGLAS(I,9)
        VGLAS(I,10)=VGLAS(I,10)+DGLAS(I,10)
        VGLAS(I,11)=VGLAS(I,11)+DGLAS(I,11)
        VGLAS(I,12)=VGLAS(I,12)+DGLAS(I,12)	  
        VGLAS(I,13)=VGLAS(I,13)+DGLAS(I,13)
        VGLAS(I,14)=VGLAS(I,14)+DGLAS(I,14)
        VGLAS(I,15)=VGLAS(I,15)+DGLAS(I,15)
        VGLAS(I,16)=VGLAS(I,16)+DGLAS(I,16)
        VGLAS(I,17)=VGLAS(I,17)+DGLAS(I,17)
        VGLAS(I,18)=VGLAS(I,18)+DGLAS(I,18)
Cqz    ---------enint----------
        EINT(I,1) = EINT(I,1)+ETMP(I,1)
        EINT(I,2) = EINT(I,2)+ETMP(I,2)
      END DO
C
      DO I=JFT,JLT
        IF (SIGY(I).LT.ZEP9EP30) THEN
           UFAC=ABS(MIN(FAC(I,1),FAC(I,2))-UN)
           SIGY2 = SIGY(I)*SIGY(I)
           SVM =ZERO
           SXY0=ZERO   
           IF (UFAC.LT.TOL) THEN
            SXY0= VSTRE(I,1)*VSTRE(I,1)+VSTRE(I,2)*VSTRE(I,2)
     .           -VSTRE(I,1)*VSTRE(I,2)+TROIS*VSTRE(I,3)*VSTRE(I,3)
            MXY0= MSTRE(I,1)*MSTRE(I,1)+MSTRE(I,2)*MSTRE(I,2)
     .           -MSTRE(I,1)*MSTRE(I,2)+TROIS*MSTRE(I,3)*MSTRE(I,3)
            CNN=COEF
            CMM=COEF*THK(I)*UNSEIZE
            CNNX=CNN*VGLAS(I,1)
            CNNY=CNN*VGLAS(I,2)
            CNNXY=CNN*VGLAS(I,13)
            CNNX_K=CNN*VGLAS(I,7)
            CNNY_K=CNN*VGLAS(I,8)
            CNNXY_K=CNN*VGLAS(I,15)
            CMMX=CMM*VGLAS(I,3)
            CMMY=CMM*VGLAS(I,4)
            CMMXY=CMM*VGLAS(I,14)
            CMMX_K=CMM*VGLAS(I,9)
            CMMY_K=CMM*VGLAS(I,10)
            CMMXY_K=CMM*VGLAS(I,16)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
            SXY0= SXY0+CNNX*CNNX+CNNY*CNNY-CNNX*CNNY+TROIS*CNNXY*CNNXY
            MXY0= MXY0+CMMX*CMMX+CMMY*CMMY-CMMX*CMMY+TROIS*CMMXY*CMMXY
            SXY0= SXY0+CNNX_K*CNNX_K+CNNY_K*CNNY_K-CNNX_K*CNNY_K+
     1            TROIS*CNNXY_K*CNNXY_K
            MXY0= MXY0+CMMX_K*CMMX_K+CMMY_K*CMMY_K-CMMX_K*CMMY_K+
     1            TROIS*CMMXY_K*CMMXY_K
            SXY0= SXY0+ABS(CNNX*(DEUX*CNNX_K-CNNY_K)
     1                    +CNNY*(DEUX*CNNY_K-CNNX_K))
            MXY0= MXY0+ABS(CMMX*(DEUX*CMMX_K-CMMY_K)
     1                    +CMMY*(DEUX*CMMY_K-CMMX_K))     
            SVM =  SXY0+COEF1*MXY0
           ENDIF 
           IF (UFAC.GE.TOL.OR.SVM.GT.SIGY2) THEN
              EH1=MIN(SXY0/MAX(SIGY2,TOL),UN)
              EH1=MAX(COEFH*EH1,(UN-FAC(I,1)))
              EH2=MAX(COEFH,(UN-FAC(I,2)))
              IF (ESX(I).LT.ZERO) EH1=ZERO
              IF (EMX(I).LT.ZERO) EH2=ZERO
C-------------MEMBRANE------------------------
             VGLAS(I,1)=VGLAS(I,1)-EH1*DGLAS(I,1)
             VGLAS(I,2)=VGLAS(I,2)-EH1*DGLAS(I,2)
             VGLAS(I,7)=VGLAS(I,7)-EH1*DGLAS(I,7)
             VGLAS(I,8)=VGLAS(I,8)-EH1*DGLAS(I,8)
             VGLAS(I,13)=VGLAS(I,13)-EH1*DGLAS(I,13)
             VGLAS(I,15)=VGLAS(I,15)-EH1*DGLAS(I,15)
C-------------FLEXION------------------------
             VGLAS(I,3) =VGLAS(I,3) -EH2*DGLAS(I,3)
             VGLAS(I,4) =VGLAS(I,4) -EH2*DGLAS(I,4)
             VGLAS(I,9) =VGLAS(I,9) -EH2*DGLAS(I,9)
             VGLAS(I,10)=VGLAS(I,10)-EH2*DGLAS(I,10)
             VGLAS(I,14)=VGLAS(I,14)-EH1*DGLAS(I,14)
             VGLAS(I,16)=VGLAS(I,16)-EH1*DGLAS(I,16)
           ENDIF
        ENDIF
      END DO
      DO I=JFT,JLT          
Cqz    -----------for energy calculation-Mean_value--------
        C8 = C7*OFF(I)
        SS1= (MY34(I)*VGLAS(I,1) +MY23(I)*VGLAS(I,7))*C8
        SS2= (MX23(I)*VGLAS(I,8) +MX34(I)*VGLAS(I,2))*C8
C-------add shear 
        SS1 = SS1 + C8*(-MX34(I)*VGLAS(I,13)+MX23(I)*VGLAS(I,15))
        SS2 = SS2 + C8*( MY34(I)*VGLAS(I,13) -MY23(I)*VGLAS(I,15))
C-------add Srz---------
        C3= UNDEMI*C8
        SS1= SS1 + C3*(MX34(I)*VGLAS(I,17)-MX23(I)*VGLAS(I,18))    
        SS2= SS2 + C3*(MY34(I)*VGLAS(I,17)-MY23(I)*VGLAS(I,18))    
C-------save for Fz--and add coupling terms(warped)------
        SSZ1 = SS1 
        SSZ2 = SS2 
        SF1= (MY34(I)*VGLAS(I,3) +MY23(I)*VGLAS(I,9))*C8
        SF2= -(MX23(I)*VGLAS(I,10)+MX34(I)*VGLAS(I,4))*C8
C-------add shear SF1->y;SF2->x
        SF1=SF1+(-MX34(I)*VGLAS(I,14)+MX23(I)*VGLAS(I,16))*C8
        SF2=SF2-(MY34(I)*VGLAS(I,14)-MY23(I)*VGLAS(I,16))*C8
        HSURA=THK(I)*AA(I)
        C2 = C8*THK(I)
        SC5=(MY34(I)*VGLAS(I,5) +MX34(I)*VGLAS(I,6) )*C2
        SC6=(MY23(I)*VGLAS(I,11)+MX23(I)*VGLAS(I,12))*C2
        SS3=SC5+SC6
        HVL = AMU(I)*SQRT(RHO(I)*AREA(I)*FAC1(I))*OFF(I)
        SSV0=MY23(I)*MY23(I)
        SSV1=MY34(I)*MY34(I)
        SSV2=MX23(I)*MX23(I)
        SSV3=MX34(I)*MX34(I)
        HXX_V= STIER*(SSV1+SSV0)
        HXY_V=-STIER*(MY34(I)*MX34(I)+MY23(I)*MX23(I))
        HYY_V= STIER*(SSV2+SSV3)
        C2 =HVL*GSR(I)*SHFSR(I)*UNDOUZSR
        CXZ_V=(SSV1+SSV3)*C2
        CYZ_V=(SSV2+SSV0)*C2
        AUX=AA(I)*HVL
        C1M = A11SR(I)*AUX
        C2M = A12SR(I)*AUX
        CXX_V=C1M*HXX_V
        CYY_V=C1M*HYY_V
        CXY_V=C2M*HXY_V
        SS1_V=CXX_V*VHG(I,1)+CXY_V*VHG(I,2)
        SS2_V=CYY_V*VHG(I,2)+CXY_V*VHG(I,1)
        SF1_V= (CXX_V*VHG(I,3)+CXY_V*VHG(I,4))*FBEND_V
        SF2_V=(-CYY_V*VHG(I,4)-CXY_V*VHG(I,3))*FBEND_V
        SC5_V=CXZ_V*VHG(I,5)*HSURA
        SC6_V=CYZ_V*VHG(I,6)*HSURA
        SS3_V = SC5_V+SC6_V
C--------------------------------------------------
        SS1=SS1+SS1_V
        SS2=SS2+SS2_V
        SS3=SS3+SS3_V
        SC5=SC5+SC5_V
        SC6=SC6+SC6_V
        SF1=SF1+SF1_V
        SF2=SF2+SF2_V
        Y13S= MY13(I)*SS3
        X13S= MX13(I)*SS3
        Y34S6=MY34(I)*SC6
        Y23S5=MY23(I)*SC5
        X23S5=MX23(I)*SC5
        X34S6=MX34(I)*SC6
        C2=QUART*THK(I)
C--------NOEUDS 1,3-------
        B13=(MY13(I)*X24(I)-MX13(I)*Y24(I))*HSURA
        VF(I,1,1)=VF(I,1,1)+B13*SS1
        VF(I,1,3)=C2*SS1
        VF(I,2,1)=VF(I,2,1)+B13*SS2
        VF(I,2,3)=C2*SS2
        VF(I,3,3)=SS3
C--------NOEUDS 2,4-------
        B24=(MX13(I)*Y13(I)-MY13(I)*X13(I))*HSURA
        VF(I,1,2)= VF(I,1,2)+B24*SS1
        VF(I,1,4)=-VF(I,1,3)
        VF(I,2,2)= VF(I,2,2)+B24*SS2
        VF(I,2,4)=-VF(I,2,3)
        VF(I,3,4)=-VF(I,3,3)
C--------NOEUDS 1,3-------
        C3=C6(I)*B13
        C4=C6(I)*C2
        VM(I,1,1)=VM(I,1,1)+C3*SF2+Y23S5+Y34S6
        VM(I,1,3)=VM(I,1,3)+C4*SF2-Y13S
        VM(I,2,1)=VM(I,2,1)+C3*SF1-X23S5-X34S6
        VM(I,2,3)=VM(I,2,3)+C4*SF1+X13S
C--------NOEUDS 2,4-------
        C3=C6(I)*B24
        VM(I,1,2)=VM(I,1,2)+C3*SF2+Y23S5-Y34S6
        VM(I,1,4)=VM(I,1,4)-C4*SF2-Y13S
        VM(I,2,2)=VM(I,2,2)+C3*SF1-X23S5+X34S6
        VM(I,2,4)=VM(I,2,4)-C4*SF1+X13S
C       
        C2=Z1(I)*HSURA
        VF(I,3,1)=VF(I,3,1)+C2*(SSZ1*Y24(I)-SSZ2*X24(I))
        VF(I,3,2)=VF(I,3,2)+C2*(-SSZ1*Y13(I)+SSZ2*X13(I))
C-----------non const mz----17,18--
        C2=THK(I)*UNSIX*OFF(I)
        C3=THK(I)*THIRD*OFF(I)
C
        J=1
        VMZ(I,J)= VMZ(I,J)
     .        +C2*(VGLAS(I,17)*BMERZ(I,4,J)+VGLAS(I,18)*BMKRZ(I,4,J))
     .        +C3*(BMERZ(I,1,J)*VGLAS(I,1)-BMKRZ(I,1,J)*VGLAS(I,7)
     .            -BMERZ(I,2,J)*VGLAS(I,2)+BMKRZ(I,2,J)*VGLAS(I,8)
     .            +BMERZ(I,3,J)*VGLAS(I,13)+BMKRZ(I,3,J)*VGLAS(I,15))
        J=2
        VMZ(I,J)= VMZ(I,J)
     .        +C2*(VGLAS(I,17)*BMERZ(I,4,J)+VGLAS(I,18)*BMKRZ(I,4,J))
     .        +C3*(BMERZ(I,1,J)*VGLAS(I,1)-BMKRZ(I,1,J)*VGLAS(I,7)
     .            -BMERZ(I,2,J)*VGLAS(I,2)+BMKRZ(I,2,J)*VGLAS(I,8)
     .            +BMERZ(I,3,J)*VGLAS(I,13)+BMKRZ(I,3,J)*VGLAS(I,15))
        J=3
        VMZ(I,J)= VMZ(I,J)
     .        +C2*(VGLAS(I,17)*BMERZ(I,4,J)+VGLAS(I,18)*BMKRZ(I,4,J))
     .        +C3*(BMERZ(I,1,J)*VGLAS(I,1)-BMKRZ(I,1,J)*VGLAS(I,7)
     .            -BMERZ(I,2,J)*VGLAS(I,2)+BMKRZ(I,2,J)*VGLAS(I,8)
     .            +BMERZ(I,3,J)*VGLAS(I,13)+BMKRZ(I,3,J)*VGLAS(I,15))
        J=4
        VMZ(I,J)= VMZ(I,J)
     .        +C2*(VGLAS(I,17)*BMERZ(I,4,J)+VGLAS(I,18)*BMKRZ(I,4,J))
     .        +C3*(BMERZ(I,1,J)*VGLAS(I,1)-BMKRZ(I,1,J)*VGLAS(I,7)
     .            -BMERZ(I,2,J)*VGLAS(I,2)+BMKRZ(I,2,J)*VGLAS(I,8)
     .            +BMERZ(I,3,J)*VGLAS(I,13)+BMKRZ(I,3,J)*VGLAS(I,15))
C        
        ERZ = VGLAS(I,1)*DHGZE(I,1)-VGLAS(I,2)*DHGZE(I,2)-
     .        VGLAS(I,7)*DHGZK(I,1)+VGLAS(I,8)*DHGZK(I,2)+
     .        VGLAS(I,13)*DHGZE(I,3)+VGLAS(I,15)*DHGZK(I,3)+
     .        UNDEMI*(VGLAS(I,17)*DHGZE(I,4)+VGLAS(I,18)*DHGZK(I,4))
        C3 = THIRD*AREA(I)
Cqz    -----------for energy calculation-Mean_value--------
        ESY= ((SS1-SS1_V)*DHG(I,1)+(SS2-SS2_V)*DHG(I,2)+C3*ERZ)*THK(I)+
     .        QUART*((SC5-SC5_V)*DHG(I,5)+(SC6-SC6_V)*DHG(I,6))
        ETMP(I,1) = UNDEMI*ESY*OFF(I)
        EMY= (SF1-SF1_V)*DHG(I,3)-(SF2-SF2_V)*DHG(I,4)
        ETMP(I,2) = UNDEMI*C6(I)*EMY*THK(I)*OFF(I)
Cqz     ----viscous energy is removed to hourglass energy----
        TESY(I)= (SS1_V*DHG(I,1)+SS2_V*DHG(I,2))*THK(I)+
     .           (SF1_V*DHG(I,3)-SF2_V*DHG(I,4))*THK(I)*C6(I)+
     .            SC5_V*DHG(I,5)+SC6_V*DHG(I,6)
      ENDDO
C
      DO I=JFT,JLT
        EINT(I,1) = EINT(I,1)+ETMP(I,1)
        EINT(I,2) = EINT(I,2)+ETMP(I,2)
      END DO
C
      IC=1
      JST(IC)=JFT
      DO J=JFT+1,JLT
         IF (IPARTC(J).NE.IPARTC(J-1)) THEN
            IC=IC+1
            JST(IC)=J
         ENDIF
      ENDDO
      JST(IC+1)=JLT+1

      IF(IC.EQ.1) THEN
        MX = IPARTC(JFT)
#include "simd.inc"
        DO I=JFT,JLT
          EVIS(8,MX)=EVIS(8,MX) + TESY(i)
        ENDDO
      ELSEIF(IC.EQ.2.AND.KFTS.GT.0)THEN
        MX = IPARTC(JFT)
#include "simd.inc"
        DO I=JFT, KFTS-1
          EVIS(8,MX)=EVIS(8,MX) + TESY(i)
        ENDDO
        MX = IPARTC(JLT)
#include "simd.inc"
        DO I=KFTS,JLT
          EVIS(8,MX)=EVIS(8,MX) + TESY(i)
        ENDDO
      ELSE
        DO II=1,IC
           MX=IPARTC(JST(II))
           IF (JST(II+1)-JST(II).GT.15) THEN
#include "simd.inc"
              DO J=JST(II),JST(II+1)-1
                EVIS(8,MX)=EVIS(8,MX) + TESY(J)
              ENDDO
           ELSE
              DO J=JST(II),JST(II+1)-1
                EVIS(8,MX)=EVIS(8,MX) + TESY(J)
              ENDDO
           ENDIF
        ENDDO
      ENDIF
      RETURN
      END      
Chd|====================================================================
Chd|  CZFINTN_OR                    source/elements/shell/coquez/czfintn.F
Chd|-- called by -----------
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|        CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
Chd|-- calls ---------------
Chd|        CZEHOURV                      source/elements/shell/coquez/czfintn.F
Chd|        CZHOUREP_OR                   source/elements/shell/coquez/czfintn.F
Chd|====================================================================
        SUBROUTINE CZFINTN_OR(JFT  ,JLT  ,THK  ,C1   ,AA   ,VHG  ,
     2                      X13  ,X24  ,Y13  ,Y24  ,Z1   ,MX23 ,
     3                      MX13 ,MX34 ,MY13 ,MY23 ,MY34 ,VGLAS,
     4                      VSTRE,MSTRE,VF   ,VM   ,FAC  ,A11  ,
     5                      A12  ,G    ,GS   ,SIGY ,OFF  ,FAC1 , 
     6                      RHO  ,AREA ,  DT1 ,EINT ,AMU  ,VHI  ,
     7                      NPT  ,IPARTC,EVIS,KFTS  ,GSR  ,
     8                      A11SR,A12SR ,NUSR,SHFSR,IORTH,HM   ,
     9                      HF   ,HC    ,HMFOR,MTN  ,NEL) 
C orthotropic elasto-plastic hourglass stresses+linear damping 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C        CALCUL VF(3,4),VM(2,4) : FORCES INTERNES GENERALISEES
C        ENTREES : THK,1/AREA,PME,X13,X24,Y13,Y24,MX13,MX34,MY13,MY34
C                  DHG(6)  : INCREMENT DE LA DEF HOURGLASS GENERALISEE    
C                  VGLAS(12): DEF HOURGLASS GENERALISEE 
C                  VGLAS(1-6)->ETA; VGLAS(7-12)->KSI;
C                  FAC=Et/E    
C        SORTIES : VF,VM
C                  VF(3,4),VM(2,4) : 
C                  J=1,2 STOCK LA PARTIE ANTISYM (F(1)=-F(3),F(2)=-F(4))                                  
C                  J=3,4 STOCK LA PARTIE SYM (F(1)=F(3),F(2)=F(4))
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "param_c.inc"
#include "mvsiz_p.inc"

C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
        INTEGER JFT,JLT,NPT,IPARTC(*),IORTH,MTN,NEL
        my_real 
     .       THK(*) ,C1(*)  ,AA(*)  ,VHG(MVSIZ,6),
     .       X13(*) ,X24(*) ,Y13(*) ,Y24(*)  ,Z1(*),
     .       MX13(*),MX23(*),MX34(*),MY13(*),MY23(*),MY34(*),
     .       VGLAS(NEL,12),VSTRE(NEL,5),MSTRE(NEL,3),VF(MVSIZ,3,4),VM(MVSIZ,2,4),
     .       FAC(MVSIZ,2),A11(*) ,A12(*) ,G(*),GS(*),VHI(MVSIZ,3),SIGY(*),
     .       FAC1(*) ,RHO(*) ,AREA(*), DT1,OFF(*),EINT(JLT,2),AMU(*) ,
     .       EVIS(NPSAV,*),HM(MVSIZ,6),HF(MVSIZ,6),HC(MVSIZ,2),HMFOR(MVSIZ,6),
     .       GSR(*), A11SR(*), A12SR(*), NUSR(*), SHFSR(*)
         INTEGER KFTS
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I,MX, IC, II, J
        my_real 
     .  STIER,C2,C3,C4,C7,B13,B24,C3M,
     .  Y13S,X13S,Y34S6,Y23S5,X23S5,X34S6,
     .  DGLAS(MVSIZ,12),
     .  C1B,C2B,CMM,CNN,A_BK,A_B,A_K,UNDOUZSR,AUX,
     .  SSV0,SSV1,SSV2,SSV3,SC6_V,SC5_V,CXX_V,CYY_V,CXY_V,CXZ_V,CYZ_V,
     .  HXX_V,HYY_V,HXY_V,SS1_V,SS2_V,SS3_V,SF1_V,SF2_V,HVL,CRZ,
     .  EMY,ECX,ECY,ESY,FBEND,FBEND_V,ERZ,FACM(MVSIZ),FACF(MVSIZ)
        my_real 
     .  C5(MVSIZ),CXX_K(MVSIZ),CYY_K(MVSIZ),HXX(MVSIZ),HYY(MVSIZ),
     .  CXX(MVSIZ),CYY(MVSIZ),HXX_K(MVSIZ),HYY_K(MVSIZ),
     .  BXX(MVSIZ),BYY(MVSIZ),BXX_K(MVSIZ),BYY_K(MVSIZ),
     .  C1M(MVSIZ),C2M(MVSIZ),C6(MVSIZ),TESY(MVSIZ),ESX0(MVSIZ),
     .  SS1(MVSIZ),SS2(MVSIZ),SS3(MVSIZ),SC6(MVSIZ),SC5(MVSIZ),
     .  SF1(MVSIZ),SF2(MVSIZ),HSURA(MVSIZ),C8(MVSIZ),SSZ1(MVSIZ),
     .  SSZ2(MVSIZ),ESX(MVSIZ),EMX(MVSIZ),CM(MVSIZ,3,3),CF(MVSIZ,3,3),
     .  CMF(MVSIZ,3,3),CMH(MVSIZ),CFH(MVSIZ),CMH_K(MVSIZ),CFH_K(MVSIZ)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
       C7=QTHIRD
       STIER  = CINQP333  
      IF (NPT.EQ.1) THEN
       FBEND =ZERO
       FBEND_V =ZERO
      ELSE
       FBEND =UNDOUZE
       FBEND_V =TROISP464
      ENDIF
      UNDOUZSR=SQRT(UNDOUZE)
C------HXX: ETA_i*Y_i,HYY: ETA_i*X_i; HXX_K: Ksi_i*Y_i,HYY_K: Ksi_i*X_i;
       DO I=JFT,JLT
         C3=QUATRE*AA(I)
         HXX(I)=C3*MY34(I)
         HYY(I)=C3*MX34(I)
         HXX_K(I)=C3*MY23(I)
         HYY_K(I)=C3*MX23(I)
         CXX(I)=HXX(I)*VHG(I,1)
         CYY(I)=HYY(I)*VHG(I,2)
         CXX_K(I)=HXX_K(I)*VHG(I,1)
         CYY_K(I)=HYY_K(I)*VHG(I,2)
         BXX(I)=HXX(I)*VHG(I,3)
         BYY(I)=HYY(I)*VHG(I,4)
         BXX_K(I)=HXX_K(I)*VHG(I,3)
         BYY_K(I)=HYY_K(I)*VHG(I,4)
         C5(I)=UNDEMI*OFF(I)*THK(I)*C7*DT1
         C6(I)=C1(I)*FBEND
         HSURA(I)=THK(I)*AA(I)
       END DO
C    -----------for energy calculation-Mean_value--------
       DO I=JFT,JLT
         SS1(I)= MY34(I)*VGLAS(I,1) +MY23(I)*VGLAS(I,7)
         SS2(I)= MX23(I)*VGLAS(I,8) +MX34(I)*VGLAS(I,2)
         SF1(I)= MY34(I)*VGLAS(I,3) +MY23(I)*VGLAS(I,9)
         SF2(I)= -MX23(I)*VGLAS(I,10)-MX34(I)*VGLAS(I,4)
         SC5(I)= MY34(I)*VGLAS(I,5) +MX34(I)*VGLAS(I,6) 
         SC6(I)= MY23(I)*VGLAS(I,11)+MX23(I)*VGLAS(I,12)
         ESX0(I)= SS1(I)*VHG(I,1)+SS2(I)*VHG(I,2)
         ESX(I)= ESX0(I)+
     .           QUART*(SC5(I)*VHG(I,5)+SC6(I)*VHG(I,6))
         EMX(I)= (SF1(I)*VHG(I,3)-SF2(I)*VHG(I,4))*C6(I)
       END DO 
Cqz    -----------for energy calculation-Mean_value--VGLAS(t)------
      DO I=JFT,JLT
         EINT(I,1) = EINT(I,1)+C5(I)*ESX(I)
         EINT(I,2) = EINT(I,2)+C5(I)*EMX(I)
      END DO 
C -----------elastic increment
      IF (IORTH>0) THEN
       IF (MTN==19) THEN
        DO I=JFT,JLT
         C2=FAC1(I)*DT1
         FACM(I)=FAC(I,1)*C2
         FACF(I)=FAC(I,2)*C2
        END DO
       ELSE
C---------coef of integration should be removed, because has been done inside HF       
C---------have to be done here for VGLAS
        DO I=JFT,JLT
         C2=FAC1(I)*DT1
         FACM(I)=C2
         FACF(I)=C2*DOUZE
        END DO
       END IF !(MTN==19) THEN
C-------Modified modulas for damping---    
        DO I=JFT,JLT
         GSR(I) =SQRT(HM(I,4))
         A11SR(I)=SQRT(UNDEMI*(HM(I,1)+HM(I,2)))
         A12SR(I)=SQRT(MAX(EM20,HM(I,3)))
cc         A12SR(I)=SQRT(HM(3,I))
        END DO
        DO I=JFT,JLT
         CM(I,1,1)=FACM(I)*HM(I,1)
         CM(I,2,2)=FACM(I)*HM(I,2)
         CM(I,1,2)=FACM(I)*HM(I,3)
         CM(I,3,3)=FACM(I)*HM(I,4)
         CM(I,1,3)=FACM(I)*HM(I,5)
         CM(I,2,3)=FACM(I)*HM(I,6)
         CF(I,1,1)=FACF(I)*HF(I,1)
         CF(I,2,2)=FACF(I)*HF(I,2)
         CF(I,1,2)=FACF(I)*HF(I,3)
         CF(I,3,3)=FACF(I)*HF(I,4)
         CF(I,1,3)=FACF(I)*HF(I,5)
         CF(I,2,3)=FACF(I)*HF(I,6)
        END DO
C    VGLAS(1-6)->ETA (x:+ y-); VGLAS(7-12)->KSI  (x:- y+);
C-----part iso-commun
        DO I=JFT,JLT
C----- x_ETA, signe +     
          DGLAS(I,1) =CM(I,1,1)*CXX(I)-CM(I,1,2)*CYY(I)
C----- y_ETA, signe -     
          DGLAS(I,2) =CM(I,2,2)*CYY(I)-CM(I,1,2)*CXX(I)
C----- x_ETA, signe+ bending    
          DGLAS(I,3) =CF(I,1,1)*BXX(I)-CF(I,1,2)*BYY(I)
C----- y_ETA, signe - bending    
          DGLAS(I,4) =CF(I,2,2)*BYY(I)-CF(I,1,2)*BXX(I)
C----- x_Ksi, signe -     
          DGLAS(I,7) =CM(I,1,1)*CXX_K(I)-CM(I,1,2)*CYY_K(I)
C----- y_Ksi, signe +     
          DGLAS(I,8) =CM(I,2,2)*CYY_K(I)-CM(I,1,2)*CXX_K(I)
C----- x_Ksi, signe - bending    
          DGLAS(I,9) =CF(I,1,1)*BXX_K(I)-CF(I,1,2)*BYY_K(I)
C----- y_Ksi, signe + bending    
          DGLAS(I,10)=CF(I,2,2)*BYY_K(I)-CF(I,1,2)*BXX_K(I)
C------------         
          C4=FAC1(I)*O64TH*DT1
          C2=C4*HC(I,1)
          C3=C4*HC(I,2)
          DGLAS(I,5) =C2*HXX(I)*VHG(I,5)
          DGLAS(I,6) =C2*HYY(I)*VHG(I,5)
          DGLAS(I,11)=C3*HXX_K(I)*VHG(I,6)
          DGLAS(I,12)=C3*HYY_K(I)*VHG(I,6)
        END DO
C-----part C(1,3),C(2,3)
C-------finally after crash-box tets, constant in-plane shear strain and stress
C--------- like QBAT is used to avoid instability     
       DO I=JFT,JLT
        CMH(I) = ZERO
        CMH_K(I) = ZERO
        CFH(I) = ZERO
        CFH_K(I) = ZERO
       END DO
C-----part CMF
       IF (IORTH>1) THEN
       DO I=JFT,JLT
         C2=FAC1(I)*DT1
         CMF(I,1,1)=C2*HMFOR(I,1)
         CMF(I,2,2)=C2*HMFOR(I,2)
         CMF(I,1,2)=C2*HMFOR(I,3)
         CMF(I,3,3)=C2*HMFOR(I,4)
         CMF(I,1,3)=C2*HMFOR(I,5)
         CMF(I,2,3)=C2*HMFOR(I,6)
C    
        DGLAS(I,1) =DGLAS(I,1)+CMF(I,1,1)*BXX(I)-CMF(I,1,2)*BYY(I)
     +                        -CMF(I,1,3)*CFH(I)  
        DGLAS(I,2) =DGLAS(I,2)+CMF(I,2,2)*BYY(I)-CMF(I,1,2)*BXX(I)
     +                        +CMF(I,2,3)*CFH(I) 
        DGLAS(I,3) =DGLAS(I,3)+CMF(I,1,1)*CXX(I)-CMF(I,1,2)*CYY(I)
     +                        -CMF(I,1,3)*CMH(I) 
        DGLAS(I,4) =DGLAS(I,4)+CMF(I,2,2)*CYY(I)-CMF(I,1,2)*CXX(I)
     +                        +CMF(I,2,3)*CMH(I)
        DGLAS(I,7) =DGLAS(I,7)+CMF(I,1,1)*BXX_K(I)-CMF(I,1,2)*BYY_K(I)
     +                        -CMF(I,1,3)*CFH_K(I)
        DGLAS(I,8) =DGLAS(I,8)+CMF(I,2,2)*BYY_K(I)-CMF(I,1,2)*BXX_K(I)
     +                        +CMF(I,2,3)*CFH_K(I)
        DGLAS(I,9) =DGLAS(I,9)+CMF(I,1,1)*CXX_K(I)-CMF(I,1,2)*CYY_K(I)
     +                        -CMF(I,1,3)*CMH_K(I)
        DGLAS(I,10)=DGLAS(I,10)+CMF(I,2,2)*CYY_K(I)-CMF(I,1,2)*CXX_K(I)
     +                         +CMF(I,2,3)*CMH_K(I)
       END DO
       END IF !(IORTH>1) THEN
      ELSE      
        DO I=JFT,JLT
         C1M(I)=A11(I)*FAC1(I)*DT1
         C2M(I)=A12(I)*FAC1(I)*DT1
        END DO
       DO I=JFT,JLT
          DGLAS(I,1) =C1M(I)*CXX(I)-C2M(I)*CYY(I)
          DGLAS(I,2) =C1M(I)*CYY(I)-C2M(I)*CXX(I)
          DGLAS(I,3) =C1M(I)*BXX(I)-C2M(I)*BYY(I)
          DGLAS(I,4) =C1M(I)*BYY(I)-C2M(I)*BXX(I)
          DGLAS(I,7) =C1M(I)*CXX_K(I)-C2M(I)*CYY_K(I)
          DGLAS(I,8) =C1M(I)*CYY_K(I)-C2M(I)*CXX_K(I)
          DGLAS(I,9) =C1M(I)*BXX_K(I)-C2M(I)*BYY_K(I)
          DGLAS(I,10)=C1M(I)*BYY_K(I)-C2M(I)*BXX_K(I)
C------------          
          C2=FAC1(I)*GS(I)*O64TH*DT1
          DGLAS(I,5) =C2*HXX(I)*VHG(I,5)
          DGLAS(I,6) =C2*HYY(I)*VHG(I,5)
          DGLAS(I,11)=C2*HXX_K(I)*VHG(I,6)
          DGLAS(I,12)=C2*HYY_K(I)*VHG(I,6)
       END DO
      END IF !(IORTH>0) THEN
       DO I=JFT,JLT
          DO J=1,12
           VGLAS(I,J) =VGLAS(I,J) +DGLAS(I,J)
          END DO
       END DO
C-----------elasto-plastic----      
        CALL CZHOUREP_OR(JFT   ,JLT  ,SIGY ,VSTRE ,MSTRE ,
     1                   THK   ,FAC  ,ESX0 ,EMX   ,NPT   ,
     2                   DGLAS ,VGLAS,NEL  ) 
C-----------to use the same coef C5
       DO I=JFT,JLT
         SS1(I)= MY34(I)*VGLAS(I,1) +MY23(I)*VGLAS(I,7)
         SS2(I)= MX23(I)*VGLAS(I,8) +MX34(I)*VGLAS(I,2)
         SF1(I)= MY34(I)*VGLAS(I,3) +MY23(I)*VGLAS(I,9)
         SF2(I)= -(MX23(I)*VGLAS(I,10)+MX34(I)*VGLAS(I,4))
         SC5(I)=MY34(I)*VGLAS(I,5) +MX34(I)*VGLAS(I,6) 
         SC6(I)=MY23(I)*VGLAS(I,11)+MX23(I)*VGLAS(I,12)
       END DO !I=JFT,JLT
C    -------for energy calculation-Mean_value-VGLAS(t+dt)-------
      DO I=JFT,JLT
         ESX(I)= SS1(I)*VHG(I,1)+SS2(I)*VHG(I,2)+
     .           QUART*(SC5(I)*VHG(I,5)+SC6(I)*VHG(I,6))
         EMX(I)= (SF1(I)*VHG(I,3)-SF2(I)*VHG(I,4))*C6(I)
      END DO
      DO I=JFT,JLT
         EINT(I,1) = EINT(I,1)+C5(I)*ESX(I)
         EINT(I,2) = EINT(I,2)+C5(I)*EMX(I)
      END DO !I=JFT,JLT
      DO I=JFT,JLT
         C8(I) = C7*OFF(I)
         SS1(I)= SS1(I)*C8(I)
         SS2(I)= SS2(I)*C8(I)
         SF1(I)= SF1(I)*C8(I)
         SF2(I)= SF2(I)*C8(I)
         C2 = C8(I)*THK(I)
         SC5(I)=SC5(I)*C2
         SC6(I)=SC6(I)*C2
         SS3(I)=SC5(I)+SC6(I)
      END DO !I=JFT,JLT
C--------Fint  :terms for linear damping   
      DO I=JFT,JLT
C-------save for Fz--and add coupling terms(warped)------
         SSZ1(I) = SS1(I) 
         SSZ2(I) = SS2(I) 
         HVL = AMU(I)*SQRT(RHO(I)*AREA(I)*FAC1(I))*OFF(I)
         SSV0=MY23(I)*MY23(I)
         SSV1=MY34(I)*MY34(I)
         SSV2=MX23(I)*MX23(I)
         SSV3=MX34(I)*MX34(I)
         HXX_V= STIER*(SSV1+SSV0)
         HXY_V=-STIER*(MY34(I)*MX34(I)+MY23(I)*MX23(I))
         HYY_V= STIER*(SSV2+SSV3)
         C2 =HVL*GSR(I)*SHFSR(I)*UNDOUZSR
         CXZ_V=(SSV1+SSV3)*C2
         CYZ_V=(SSV2+SSV0)*C2
C
         AUX=AA(I)*HVL
         C1M(I) = A11SR(I)*AUX
         C2M(I) = A12SR(I)*AUX
C
         CXX_V=C1M(I)*HXX_V
         CYY_V=C1M(I)*HYY_V
         CXY_V=C2M(I)*HXY_V
         SS1_V=CXX_V*VHG(I,1)+CXY_V*VHG(I,2)
         SS2_V=CYY_V*VHG(I,2)+CXY_V*VHG(I,1)
         SF1_V= (CXX_V*VHG(I,3)+CXY_V*VHG(I,4))*FBEND_V
         SF2_V=(-CYY_V*VHG(I,4)-CXY_V*VHG(I,3))*FBEND_V
         SC5_V=CXZ_V*VHG(I,5)*HSURA(I)
         SC6_V=CYZ_V*VHG(I,6)*HSURA(I)
         SS3_V = SC5_V+SC6_V
C--------------------------------------------------
         SS1(I)=SS1(I)+SS1_V
         SS2(I)=SS2(I)+SS2_V
         SS3(I)=SS3(I)+SS3_V
         SC5(I)=SC5(I)+SC5_V
         SC6(I)=SC6(I)+SC6_V
         SF1(I)=SF1(I)+SF1_V
         SF2(I)=SF2(I)+SF2_V
         TESY(I)= ((SS1_V*VHG(I,1)+SS2_V*VHG(I,2))*THK(I)+
     .            (SF1_V*VHG(I,3)-SF2_V*VHG(I,4))*THK(I)*C6(I) +
     .            (SC5_V*VHG(I,5)+SC6_V*VHG(I,6))*QUART)*DT1
      END DO !I=JFT,JLT
C--------Fint      
      DO I=JFT,JLT
         Y13S= MY13(I)*SS3(I)
         X13S= MX13(I)*SS3(I)
         Y34S6=MY34(I)*SC6(I)
         Y23S5=MY23(I)*SC5(I)
         X23S5=MX23(I)*SC5(I)
         X34S6=MX34(I)*SC6(I)
         C2=QUART*THK(I)
C--------NOEUDS 1,3-------
        B13=(MY13(I)*X24(I)-MX13(I)*Y24(I))*HSURA(I)
        VF(I,1,1)=VF(I,1,1)+B13*SS1(I)
        VF(I,1,3)=C2*SS1(I)
        VF(I,2,1)=VF(I,2,1)+B13*SS2(I)
        VF(I,2,3)=C2*SS2(I)
        VF(I,3,3)=SS3(I)
C--------NOEUDS 2,4-------
        B24=(MX13(I)*Y13(I)-MY13(I)*X13(I))*HSURA(I)
        VF(I,1,2)= VF(I,1,2)+B24*SS1(I)
        VF(I,1,4)=-VF(I,1,3)
        VF(I,2,2)= VF(I,2,2)+B24*SS2(I)
        VF(I,2,4)=-VF(I,2,3)
        VF(I,3,4)=-VF(I,3,3)
C--------NOEUDS 1,3-------
          C3=C6(I)*B13
          C4=C6(I)*C2
          VM(I,1,1)=VM(I,1,1)+C3*SF2(I)+Y23S5+Y34S6
          VM(I,1,3)=VM(I,1,3)+C4*SF2(I)-Y13S
          VM(I,2,1)=VM(I,2,1)+C3*SF1(I)-X23S5-X34S6
          VM(I,2,3)=VM(I,2,3)+C4*SF1(I)+X13S
C--------NOEUDS 2,4-------
          C3=C6(I)*B24
          VM(I,1,2)=VM(I,1,2)+C3*SF2(I)+Y23S5-Y34S6
          VM(I,1,4)=VM(I,1,4)-C4*SF2(I)-Y13S
          VM(I,2,2)=VM(I,2,2)+C3*SF1(I)-X23S5+X34S6
          VM(I,2,4)=VM(I,2,4)-C4*SF1(I)+X13S
C
        C2=Z1(I)*HSURA(I)
        VF(I,3,1)=VF(I,3,1)+C2*(SSZ1(I)*Y24(I)-SSZ2(I)*X24(I))
        VF(I,3,2)=VF(I,3,2)+C2*(-SSZ1(I)*Y13(I)+SSZ2(I)*X13(I))
      END DO !I=JFT,JLT
C      
      CALL CZEHOURV(JFT  ,JLT  ,IPARTC,EVIS,KFTS  ,TESY ) 
      RETURN
      END      
Chd|====================================================================
Chd|  CZEHOURV                      source/elements/shell/coquez/czfintn.F
Chd|-- called by -----------
Chd|        CZFINTNRZ_OR                  source/elements/shell/coquez/czfintn.F
Chd|        CZFINTN_OR                    source/elements/shell/coquez/czfintn.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CZEHOURV(JFT  ,JLT  ,IPARTC,EVIS,KFTS  ,
     8                      TESY ) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include "implicit_f.inc"
#include "param_c.inc"
#include "mvsiz_p.inc"
#include "parit_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
        INTEGER JFT,JLT,IPARTC(*)
        my_real 
     .       EVIS(NPSAV,*),TESY(*)
         INTEGER KFTS
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I,MX, IC, II, J, JST(MVSIZ+1)
C-----------------------------------------------
      IC=1
      JST(IC)=JFT
      DO J=JFT+1,JLT
         IF (IPARTC(J).NE.IPARTC(J-1)) THEN
            IC=IC+1
            JST(IC)=J
         ENDIF
      ENDDO
      JST(IC+1)=JLT+1
      IF(IC.EQ.1) THEN
        MX = IPARTC(JFT)
#include "simd.inc"
        DO I=JFT,JLT
          EVIS(8,MX)=EVIS(8,MX) + TESY(I)
        ENDDO

      ELSEIF(IC.EQ.2.AND.KFTS.GT.0)THEN
        MX = IPARTC(JFT)
#include "simd.inc"
        DO I=JFT, KFTS-1
          EVIS(8,MX)=EVIS(8,MX) + TESY(I)
        ENDDO
        MX = IPARTC(JLT)
#include "simd.inc"
        DO I=KFTS,JLT
          EVIS(8,MX)=EVIS(8,MX) + TESY(I)
        ENDDO

      ELSE
C
           DO II=1,IC
              MX=IPARTC(JST(II))
              IF (JST(II+1)-JST(II).GT.15) THEN
#include "simd.inc"
                 DO J=JST(II),JST(II+1)-1
                   EVIS(8,MX)=EVIS(8,MX) + TESY(J)
                 ENDDO
              ELSE
                 DO J=JST(II),JST(II+1)-1
                   EVIS(8,MX)=EVIS(8,MX) + TESY(J)
                 ENDDO
              ENDIF
           ENDDO
      ENDIF
C      
      RETURN
      END      
Chd|====================================================================
Chd|  CZFINTNRZ_OR                  source/elements/shell/coquez/czfintn.F
Chd|-- called by -----------
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|        CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
Chd|-- calls ---------------
Chd|        CZEHOURV                      source/elements/shell/coquez/czfintn.F
Chd|        CZHOUREPRZ_OR                 source/elements/shell/coquez/czfintn.F
Chd|====================================================================
        SUBROUTINE CZFINTNRZ_OR(JFT  ,JLT  ,THK  ,C1   ,AA   ,VHG  ,
     2                      X13  ,X24  ,Y13  ,Y24  ,Z1   ,MX23 ,
     3                      MX13 ,MX34 ,MY13 ,MY23 ,MY34 ,VGLAS,
     4                      VSTRE,MSTRE,VF   ,VM   ,FAC  ,A11  ,
     5                      A12  ,G    ,GS   ,SIGY ,OFF  ,FAC1 , 
     6                      RHO  ,AREA ,  DT1,EINT ,AMU  ,VHI  ,
     7                      NPT  ,IPARTC,EVIS,KFTS  ,GSR  ,
     8                      A11SR,A12SR ,NUSR,SHFSR,BMKRZ,BMERZ,
     9                      VHGZK,VHGZE ,KRZ ,VMZ  ,IORTH,HM   ,
     9                      HF   ,HC    ,HMFOR,MTN ,NEL) 
C orthotropic elasto-plastic hourglass stresses+linear damping with drilling dof
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C        CALCUL VF(3,4),VM(2,4) : FORCES INTERNES GENERALISEES
C        ENTREES : THK,1/AREA,PME,X13,X24,Y13,Y24,MX13,MX34,MY13,MY34
C                  DHG(6)  : INCREMENT DE LA DEF HOURGLASS GENERALISEE    
C                  VGLAS(12): DEF HOURGLASS GENERALISEE 
C                  VGLAS(1-6)->ETA; VGLAS(7-12)->KSI;
C                  -shear: VGLAS(13-14)->ETA; VGLAS(15-16)->KSI; 
C                  -Sig_rz: VGLAS(17)->ETA; VGLAS(18)->KSI; VGLAS(19)->const
C                  FAC=Et/E    
C        SORTIES : VF,VM
C                  VF(3,4),VM(2,4) : 
C                  J=1,2 STOCK LA PARTIE ANTISYM (F(1)=-F(3),F(2)=-F(4))                                  
C                  J=3,4 STOCK LA PARTIE SYM (F(1)=F(3),F(2)=F(4))
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "mvsiz_p.inc"
#include      "units_c.inc"

C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
        INTEGER JFT,JLT,NPT,IPARTC(*),IORTH,MTN,NEL
        my_real 
     .       THK(*) ,C1(*)  ,AA(*)  ,VHG(MVSIZ,6),
     .       X13(*) ,X24(*) ,Y13(*) ,Y24(*)  ,Z1(*),
     .       MX13(*),MX23(*),MX34(*),MY13(*),MY23(*),MY34(*),
     .       VGLAS(NEL,19),VSTRE(NEL,5),MSTRE(NEL,3),VF(MVSIZ,3,4),VM(MVSIZ,2,4),
     .       FAC(MVSIZ,2),A11(*) ,A12(*) ,G(*),GS(*),VHI(MVSIZ,3),SIGY(*),
     .       FAC1(*) ,RHO(*) ,AREA(*), DT1,OFF(*),EINT(JLT,2),AMU(*) ,
     .       EVIS(NPSAV,*),VMZ(MVSIZ,4),
     .       VHGZK(MVSIZ,5),VHGZE(MVSIZ,5),KRZ(*),
     .       BM0RZ(MVSIZ,4,4),BMKRZ(MVSIZ,4,4),BMERZ(MVSIZ,4,4),
     .       GSR(*), A11SR(*), A12SR(*), NUSR(*), SHFSR(*),
     .       HM(MVSIZ,6),HF(MVSIZ,6),HC(MVSIZ,2),HMFOR(MVSIZ,6)
         INTEGER KFTS
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I,MX, IC, II, J
        my_real 
     .  STIER,C2,C3,C4,C7,B13,B24,C3M,
     .  Y13S,X13S,Y34S6,Y23S5,X23S5,X34S6,
     .  DGLAS(MVSIZ,18),FACM(MVSIZ),FACF(MVSIZ),
     .  C1B,C2B,CMM,CNN,A_BK,A_B,A_K,UNDOUZSR,AUX,
     .  SSV0,SSV1,SSV2,SSV3,SC6_V,SC5_V,CXX_V,CYY_V,CXY_V,CXZ_V,CYZ_V,
     .  HXX_V,HYY_V,HXY_V,SS1_V,SS2_V,SS3_V,SF1_V,SF2_V,HVL,CRZ,
     .  EMY,ECX,ECY,ESY,FBEND,FBEND_V,ERZ
        my_real 
     .  C5(MVSIZ),CXX_K(MVSIZ),CYY_K(MVSIZ),HXX(MVSIZ),HYY(MVSIZ),
     .  CXX(MVSIZ),CYY(MVSIZ),HXX_K(MVSIZ),HYY_K(MVSIZ),
     .  BXX(MVSIZ),BYY(MVSIZ),BXX_K(MVSIZ),BYY_K(MVSIZ),
     .  C1M(MVSIZ),C2M(MVSIZ),C6(MVSIZ),TESY(MVSIZ),ESX0(MVSIZ),
     .  SS1(MVSIZ),SS2(MVSIZ),SS3(MVSIZ),SC6(MVSIZ),SC5(MVSIZ),
     .  SF1(MVSIZ),SF2(MVSIZ),HSURA(MVSIZ),C8(MVSIZ),SSZ1(MVSIZ),
     .  SSZ2(MVSIZ),ESX(MVSIZ),EMX(MVSIZ),CM(MVSIZ,3,3),CF(MVSIZ,3,3),
     .  CMF(MVSIZ,3,3),CMH(MVSIZ),CFH(MVSIZ),CMH_K(MVSIZ),CFH_K(MVSIZ)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
       C7=QTHIRD
       STIER  = CINQP333  
      IF (NPT.EQ.1) THEN
       FBEND =ZERO
       FBEND_V =ZERO
      ELSE
       FBEND =UNDOUZE
       FBEND_V =TROISP464
      ENDIF
      UNDOUZSR=SQRT(UNDOUZE)
C
       DO I=JFT,JLT
         C3=QUATRE*AA(I)
         HXX(I)=C3*MY34(I)
         HYY(I)=C3*MX34(I)
         HXX_K(I)=C3*MY23(I)
         HYY_K(I)=C3*MX23(I)
         CXX(I)=HXX(I)*VHG(I,1)
         CYY(I)=HYY(I)*VHG(I,2)
         CXX_K(I)=HXX_K(I)*VHG(I,1)
         CYY_K(I)=HYY_K(I)*VHG(I,2)
         BXX(I)=HXX(I)*VHG(I,3)
         BYY(I)=HYY(I)*VHG(I,4)
         BXX_K(I)=HXX_K(I)*VHG(I,3)
         BYY_K(I)=HYY_K(I)*VHG(I,4)
         C5(I)=UNDEMI*OFF(I)*THK(I)*C7*DT1
         C6(I)=C1(I)*FBEND
         HSURA(I)=THK(I)*AA(I)
       END DO
C    -----------for energy calculation-Mean_value--------
       DO I=JFT,JLT
         SS1(I)= MY34(I)*VGLAS(I,1) +MY23(I)*VGLAS(I,7)
         SS2(I)= MX23(I)*VGLAS(I,8) +MX34(I)*VGLAS(I,2)
         SF1(I)= MY34(I)*VGLAS(I,3) +MY23(I)*VGLAS(I,9)
         SF2(I)= -MX23(I)*VGLAS(I,10)-MX34(I)*VGLAS(I,4)
         SC5(I)= MY34(I)*VGLAS(I,5) +MX34(I)*VGLAS(I,6) 
         SC6(I)= MY23(I)*VGLAS(I,11)+MX23(I)*VGLAS(I,12)
         ESX0(I)= SS1(I)*VHG(I,1)+SS2(I)*VHG(I,2)
         ESX(I)= ESX0(I)+
     .           QUART*(SC5(I)*VHG(I,5)+SC6(I)*VHG(I,6))
         EMX(I)= (SF1(I)*VHG(I,3)-SF2(I)*VHG(I,4))*C6(I)
       END DO 
C      
       DO I=JFT,JLT
         SS1(I)=-MX34(I)*VGLAS(I,13)+MX23(I)*VGLAS(I,15)+
     +          UNDEMI*(MX34(I)*VGLAS(I,17)-MX23(I)*VGLAS(I,18))    
         SS2(I)=MY34(I)*VGLAS(I,13) -MY23(I)*VGLAS(I,15)+
     +          UNDEMI*(MY34(I)*VGLAS(I,17)-MY23(I)*VGLAS(I,18))    
         SF1(I)=(-MX34(I)*VGLAS(I,14)+MX23(I)*VGLAS(I,16))
         SF2(I)=-(MY34(I)*VGLAS(I,14)-MY23(I)*VGLAS(I,16))
         ERZ = VGLAS(I,1)*VHGZE(I,1)-VGLAS(I,2)*VHGZE(I,2)-
     .        VGLAS(I,7)*VHGZK(I,1)+VGLAS(I,8)*VHGZK(I,2)+
     .        VGLAS(I,13)*VHGZE(I,3)+VGLAS(I,15)*VHGZK(I,3)+
     .        UNDEMI*(VGLAS(I,17)*VHGZE(I,4)+VGLAS(I,18)*VHGZK(I,4))
C--------     
         ESX(I)=ESX(I)+
     +          SS1(I)*VHG(I,1)+SS2(I)*VHG(I,2)+ERZ*AREA(I)*QUART
         EMX(I)=EMX(I)+(SF1(I)*VHG(I,3)-SF2(I)*VHG(I,4))*C6(I)
       END DO 
Cqz    -----------for energy calculation-Mean_value--VGLAS(t)------
      DO I=JFT,JLT
         EINT(I,1) = EINT(I,1)+C5(I)*ESX(I)
         EINT(I,2) = EINT(I,2)+C5(I)*EMX(I)
      END DO 
C -----------elastic increment     
      IF (IORTH>0) THEN
       IF (MTN==19) THEN
        DO I=JFT,JLT
         C2=FAC1(I)*DT1
         FACM(I)=FAC(I,1)*C2
         FACF(I)=FAC(I,2)*C2
        END DO
       ELSE
        DO I=JFT,JLT
         C2=FAC1(I)*DT1
         FACM(I)=C2
         FACF(I)=C2*DOUZE
        END DO
       END IF !(MTN==19) THEN
C-------Modified modulas for damping---       
        DO I=JFT,JLT
         GSR(I) =SQRT(HM(I,4))
         A11SR(I)=SQRT(UNDEMI*(HM(I,1)+HM(I,2)))
         A12SR(I)=SQRT(HM(I,3))
        END DO
        DO I=JFT,JLT
         CM(I,1,1)=FACM(I)*HM(I,1)
         CM(I,2,2)=FACM(I)*HM(I,2)
         CM(I,1,2)=FACM(I)*HM(I,3)
         CM(I,3,3)=FACM(I)*HM(I,4)
         CM(I,1,3)=FACM(I)*HM(I,5)
         CM(I,2,3)=FACM(I)*HM(I,6)
         CF(I,1,1)=FACF(I)*HF(I,1)
         CF(I,2,2)=FACF(I)*HF(I,2)
         CF(I,1,2)=FACF(I)*HF(I,3)
         CF(I,3,3)=FACF(I)*HF(I,4)
         CF(I,1,3)=FACF(I)*HF(I,5)
         CF(I,2,3)=FACF(I)*HF(I,6)
        END DO
        DO I=JFT,JLT
         CMH(I) = HYY(I)*VHG(I,1)-HXX(I)*VHG(I,2)
         CFH(I) = HYY(I)*VHG(I,3)-HXX(I)*VHG(I,4)
         CMH_K(I) = HYY_K(I)*VHG(I,1)-HXX_K(I)*VHG(I,2)
         CFH_K(I) = HYY_K(I)*VHG(I,3)-HXX_K(I)*VHG(I,4)
        END DO
C    VGLAS(1-6)->ETA (x:+ y-); VGLAS(7-12)->KSI  (x:- y+);
C-----part iso-commun
        DO I=JFT,JLT
C----- x_ETA, signe +     
          DGLAS(I,1) =CM(I,1,1)*CXX(I)-CM(I,1,2)*CYY(I)
C----- y_ETA, signe -     
          DGLAS(I,2) =CM(I,2,2)*CYY(I)-CM(I,1,2)*CXX(I)
C----- x_ETA, signe + bending    
          DGLAS(I,3) =CF(I,1,1)*BXX(I)-CF(I,1,2)*BYY(I)
C----- y_ETA, signe - bending    
          DGLAS(I,4) =CF(I,2,2)*BYY(I)-CF(I,1,2)*BXX(I)
C----- x_Ksi, signe -     
          DGLAS(I,7) =CM(I,1,1)*CXX_K(I)-CM(I,1,2)*CYY_K(I)
C----- y_Ksi, signe +     
          DGLAS(I,8) =CM(I,2,2)*CYY_K(I)-CM(I,1,2)*CXX_K(I)
C----- x_Ksi, signe - bending    
          DGLAS(I,9) =CF(I,1,1)*BXX_K(I)-CF(I,1,2)*BYY_K(I)
C----- y_Ksi, signe + bending    
          DGLAS(I,10)=CF(I,2,2)*BYY_K(I)-CF(I,1,2)*BXX_K(I)
C------------         
          C4=FAC1(I)*O64TH*DT1
          C2=C4*HC(I,1)
          C3=C4*HC(I,2)
          DGLAS(I,5) =C2*HXX(I)*VHG(I,5)
          DGLAS(I,6) =C2*HYY(I)*VHG(I,5)
          DGLAS(I,11)=C3*HXX_K(I)*VHG(I,6)
          DGLAS(I,12)=C3*HYY_K(I)*VHG(I,6)
          C3M=CM(I,3,3)
          CRZ=UNDEMI*KRZ(I)*FAC1(I)*DT1
C    --------add shear due to rz dof--no sign change-------
C----- xy_ETA, signe +     
          DGLAS(I,13)=C3M*(-CMH(I)+VHGZE(I,3))
C----- xy_ETA, signe + bending   
          DGLAS(I,14)=-C3M*CFH(I)
C----- xy_Ksi, signe +     
          DGLAS(I,15)=C3M*(CMH_K(I)+VHGZK(I,3))
C----- xy_Ksi, signe + bending   
          DGLAS(I,16)=C3M*CFH_K(I)
C    --------add  srz dof--------
          DGLAS(I,17)=CRZ*VHGZE(I,5)
          DGLAS(I,18)=CRZ*VHGZK(I,5)
        END DO
C-----part C(1,3),C(2,3)
       DO I=JFT,JLT
C    
        DGLAS(I,1) =DGLAS(I,1)-CM(I,1,3)*CMH(I)
        DGLAS(I,2) =DGLAS(I,2)+CM(I,2,3)*CMH(I)
        DGLAS(I,3) =DGLAS(I,3)-CF(I,1,3)*CFH(I)
        DGLAS(I,4) =DGLAS(I,4)+CF(I,2,3)*CFH(I)
        DGLAS(I,7) =DGLAS(I,7)-CM(I,1,3)*CMH_K(I)
        DGLAS(I,8) =DGLAS(I,8)+CM(I,2,3)*CMH_K(I)
        DGLAS(I,9) =DGLAS(I,9)-CF(I,1,3)*CFH_K(I)
        DGLAS(I,10)=DGLAS(I,10)+CF(I,2,3)*CFH_K(I)
C    --------add shear due to rz dof--no sign change-------
        DGLAS(I,13)=DGLAS(I,13)+
     +              CM(I,1,3)*HXX(I)*VHG(I,1)-CM(I,2,3)*HYY(I)*VHG(I,2)
        DGLAS(I,14)=DGLAS(I,14)+
     +              CF(I,1,3)*HXX(I)*VHG(I,3)-CF(I,2,3)*HYY(I)*VHG(I,4)
        DGLAS(I,15)=DGLAS(I,15)
     +         -CM(I,1,3)*HXX_K(I)*VHG(I,1)+CM(I,2,3)*HYY_K(I)*VHG(I,2)
        DGLAS(I,16)=DGLAS(I,16)
     +         -CF(I,1,3)*HXX_K(I)*VHG(I,3)+CF(I,2,3)*HYY_K(I)*VHG(I,4)
       END DO
C-----part CMF
       IF (IORTH>1) THEN
       DO I=JFT,JLT
         C2=FAC1(I)*DT1
         CMF(I,1,1)=C2*HMFOR(I,1)
         CMF(I,2,2)=C2*HMFOR(I,2)
         CMF(I,1,2)=C2*HMFOR(I,3)
         CMF(I,3,3)=C2*HMFOR(I,4)
         CMF(I,1,3)=C2*HMFOR(I,5)
         CMF(I,2,3)=C2*HMFOR(I,6)
C    
        DGLAS(I,1) =DGLAS(I,1)+CMF(I,1,1)*BXX(I)-CMF(I,1,2)*BYY(I)
     +                        -CMF(I,1,3)*CFH(I)  
        DGLAS(I,2) =DGLAS(I,2)+CMF(I,2,2)*BYY(I)-CMF(I,1,2)*BXX(I)
     +                        +CMF(I,2,3)*CFH(I) 
        DGLAS(I,3) =DGLAS(I,3)+CMF(I,1,1)*CXX(I)-CMF(I,1,2)*CYY(I)
     +                        -CMF(I,1,3)*CMH(I) 
        DGLAS(I,4) =DGLAS(I,4)+CMF(I,2,2)*CYY(I)-CMF(I,1,2)*CXX(I)
     +                        +CMF(I,2,3)*CMH(I)
        DGLAS(I,7) =DGLAS(I,7)+CMF(I,1,1)*BXX_K(I)-CMF(I,1,2)*BYY_K(I)
     +                        -CMF(I,1,3)*CFH_K(I)
        DGLAS(I,8) =DGLAS(I,8)+CMF(I,2,2)*BYY_K(I)-CMF(I,1,2)*BXX_K(I)
     +                        +CMF(I,2,3)*CFH_K(I)
        DGLAS(I,9) =DGLAS(I,9)+CMF(I,1,1)*CXX_K(I)-CMF(I,1,2)*CYY_K(I)
     +                        -CMF(I,1,3)*CMH_K(I)
        DGLAS(I,10)=DGLAS(I,10)+CMF(I,2,2)*CYY_K(I)-CMF(I,1,2)*CXX_K(I)
     +                         +CMF(I,2,3)*CMH_K(I)
        DGLAS(I,13)=DGLAS(I,13)+CMF(I,3,3)*(-CFH(I))+
     +            CMF(I,1,3)*HXX(I)*VHG(I,3)-CMF(I,2,3)*HYY(I)*VHG(I,4)
        DGLAS(I,14)=DGLAS(I,14)-CMF(I,3,3)*CMH(I)+
     +            CMF(I,1,3)*HXX(I)*VHG(I,1)-CMF(I,2,3)*HYY(I)*VHG(I,2)
        DGLAS(I,15)=DGLAS(I,15)+CMF(I,3,3)*CFH_K(I)
     +       -CMF(I,1,3)*HXX_K(I)*VHG(I,3)+CMF(I,2,3)*HYY_K(I)*VHG(I,4)
        DGLAS(I,16)=DGLAS(I,16)+CMF(I,3,3)*CMH_K(I)
     +       -CMF(I,1,3)*HXX_K(I)*VHG(I,1)+CMF(I,2,3)*HYY_K(I)*VHG(I,2)
       END DO
       END IF !(IORTH>1) THEN
      ELSE      
        DO I=JFT,JLT
         C1M(I)=A11(I)*FAC1(I)*DT1
         C2M(I)=A12(I)*FAC1(I)*DT1
        END DO
       DO I=JFT,JLT
          DGLAS(I,1) =C1M(I)*CXX(I)-C2M(I)*CYY(I)+
     +                C1M(I)*VHGZE(I,1)+C2M(I)*VHGZE(I,2)
          DGLAS(I,2) =C1M(I)*CYY(I)-C2M(I)*CXX(I)
     +               -C1M(I)*VHGZE(I,2)-C2M(I)*VHGZE(I,1)
          DGLAS(I,3) =C1M(I)*BXX(I)-C2M(I)*BYY(I)
          DGLAS(I,4) =C1M(I)*BYY(I)-C2M(I)*BXX(I)
          DGLAS(I,7) =C1M(I)*CXX_K(I)-C2M(I)*CYY_K(I)
     +               -C1M(I)*VHGZK(I,1)-C2M(I)*VHGZK(I,2)
          DGLAS(I,8) =C1M(I)*CYY_K(I)-C2M(I)*CXX_K(I)+
     +                C1M(I)*VHGZK(I,2)+C2M(I)*VHGZK(I,1)
          DGLAS(I,9) =C1M(I)*BXX_K(I)-C2M(I)*BYY_K(I)
          DGLAS(I,10)=C1M(I)*BYY_K(I)-C2M(I)*BXX_K(I)
C------------          
          C2=FAC1(I)*GS(I)*O64TH*DT1
          DGLAS(I,5) =C2*HXX(I)*VHG(I,5)
          DGLAS(I,6) =C2*HYY(I)*VHG(I,5)
          DGLAS(I,11)=C2*HXX_K(I)*VHG(I,6)
          DGLAS(I,12)=C2*HYY_K(I)*VHG(I,6)
C
         C3M=G(I)*FAC1(I)*DT1
         CRZ=UNDEMI*KRZ(I)*FAC1(I)*DT1
Cqz    --------add shear due to rz dof--no sign change like 2,4,7,9-------
          DGLAS(I,13)=C3M*(HXX(I)*VHG(I,2)-HYY(I)*VHG(I,1)+VHGZE(I,3))
          DGLAS(I,14)=C3M*(HXX(I)*VHG(I,4)-HYY(I)*VHG(I,3))
          DGLAS(I,15)=C3M*(-HXX_K(I)*VHG(I,2)+HYY_K(I)*VHG(I,1)+
     +                    VHGZK(I,3))
          DGLAS(I,16)=C3M*(-HXX_K(I)*VHG(I,4)+HYY_K(I)*VHG(I,3))
Cqz    --------add  srz dof--------
          DGLAS(I,17)=CRZ*VHGZE(I,5)
          DGLAS(I,18)=CRZ*VHGZK(I,5)
       END DO
      END IF !(IORTH>0) THEN
       DO I=JFT,JLT
          DO J=1,18
           VGLAS(I,J) =VGLAS(I,J) +DGLAS(I,J)
          END DO
       END DO
C-----------elasto-plastic----      
        CALL CZHOUREPRZ_OR(JFT   ,JLT   ,SIGY ,VSTRE ,MSTRE ,
     1                     THK   ,FAC   ,ESX0 ,EMX   ,NPT   ,
     2                     DGLAS ,VGLAS ,NEL  ) 
C    -------for energy calculation-Mean_value-VGLAS(t+dt)-------
C-----------to use the same coef: C5
       DO I=JFT,JLT
         SS1(I)= MY34(I)*VGLAS(I,1) +MY23(I)*VGLAS(I,7)
         SS2(I)= MX23(I)*VGLAS(I,8) +MX34(I)*VGLAS(I,2)
         SF1(I)= MY34(I)*VGLAS(I,3) +MY23(I)*VGLAS(I,9)
         SF2(I)= -(MX23(I)*VGLAS(I,10)+MX34(I)*VGLAS(I,4))
         SC5(I)=MY34(I)*VGLAS(I,5) +MX34(I)*VGLAS(I,6) 
         SC6(I)=MY23(I)*VGLAS(I,11)+MX23(I)*VGLAS(I,12)
       END DO !I=JFT,JLT
C      
       DO I=JFT,JLT
         SS1(I)= SS1(I) -MX34(I)*VGLAS(I,13)+MX23(I)*VGLAS(I,15)+
     +           UNDEMI*(MX34(I)*VGLAS(I,17)-MX23(I)*VGLAS(I,18))    
         SS2(I)= SS2(I) +MY34(I)*VGLAS(I,13) -MY23(I)*VGLAS(I,15)+
     +           UNDEMI*(MY34(I)*VGLAS(I,17)-MY23(I)*VGLAS(I,18))    
         SF1(I)=SF1(I)-MX34(I)*VGLAS(I,14)+MX23(I)*VGLAS(I,16)
         SF2(I)=SF2(I)-MY34(I)*VGLAS(I,14)+MY23(I)*VGLAS(I,16)
         ERZ = VGLAS(I,1)*VHGZE(I,1)-VGLAS(I,2)*VHGZE(I,2)-
     .        VGLAS(I,7)*VHGZK(I,1)+VGLAS(I,8)*VHGZK(I,2)+
     .        VGLAS(I,13)*VHGZE(I,3)+VGLAS(I,15)*VHGZK(I,3)+
     .        UNDEMI*(VGLAS(I,17)*VHGZE(I,4)+VGLAS(I,18)*VHGZK(I,4))
C--------     
         ESX(I)=SS1(I)*VHG(I,1)+SS2(I)*VHG(I,2)+ERZ*AREA(I)*QUART
     +           + QUART*(SC5(I)*VHG(I,5)+SC6(I)*VHG(I,6))
         EMX(I)=(SF1(I)*VHG(I,3)-SF2(I)*VHG(I,4))*C6(I)
       END DO 
      DO I=JFT,JLT
         EINT(I,1) = EINT(I,1)+C5(I)*ESX(I)
         EINT(I,2) = EINT(I,2)+C5(I)*EMX(I)
      END DO 
      DO I=JFT,JLT
         C8(I) = C7*OFF(I)
         SS1(I)= SS1(I)*C8(I)
         SS2(I)= SS2(I)*C8(I)
         SF1(I)= SF1(I)*C8(I)
         SF2(I)= SF2(I)*C8(I)
         C2 = C8(I)*THK(I)
         SC5(I)=SC5(I)*C2
         SC6(I)=SC6(I)*C2
         SS3(I)=SC5(I)+SC6(I)
      END DO !I=JFT,JLT
C-----------non const mz----17,18--
       DO I=JFT,JLT
        C2=THK(I)*UNSIX*OFF(I)
        C3=THK(I)*THIRD*OFF(I)
        DO J=1,4
         VMZ(I,J)= VMZ(I,J)
     .        +C2*(VGLAS(I,17)*BMERZ(I,4,J)+VGLAS(I,18)*BMKRZ(I,4,J))
     .        +C3*(BMERZ(I,1,J)*VGLAS(I,1)-BMKRZ(I,1,J)*VGLAS(I,7)
     .            -BMERZ(I,2,J)*VGLAS(I,2)+BMKRZ(I,2,J)*VGLAS(I,8)
     .            +BMERZ(I,3,J)*VGLAS(I,13)+BMKRZ(I,3,J)*VGLAS(I,15))
        END DO 
       ENDDO 
C--------Fint  :terms for linear damping    
      DO I=JFT,JLT
C-------save for Fz--and add coupling terms(warped)------
         SSZ1(I) = SS1(I) 
         SSZ2(I) = SS2(I) 
         HVL = AMU(I)*SQRT(RHO(I)*AREA(I)*FAC1(I))*OFF(I)
         SSV0=MY23(I)*MY23(I)
         SSV1=MY34(I)*MY34(I)
         SSV2=MX23(I)*MX23(I)
         SSV3=MX34(I)*MX34(I)
         HXX_V= STIER*(SSV1+SSV0)
         HXY_V=-STIER*(MY34(I)*MX34(I)+MY23(I)*MX23(I))
         HYY_V= STIER*(SSV2+SSV3)
         C2 =HVL*GSR(I)*SHFSR(I)*UNDOUZSR
         CXZ_V=(SSV1+SSV3)*C2
         CYZ_V=(SSV2+SSV0)*C2
C
         AUX=AA(I)*HVL
         C1M(I) = A11SR(I)*AUX
         C2M(I) = A12SR(I)*AUX
C
         CXX_V=C1M(I)*HXX_V
         CYY_V=C1M(I)*HYY_V
         CXY_V=C2M(I)*HXY_V
         SS1_V=CXX_V*VHG(I,1)+CXY_V*VHG(I,2)
         SS2_V=CYY_V*VHG(I,2)+CXY_V*VHG(I,1)
         SF1_V= (CXX_V*VHG(I,3)+CXY_V*VHG(I,4))*FBEND_V
         SF2_V=(-CYY_V*VHG(I,4)-CXY_V*VHG(I,3))*FBEND_V
         SC5_V=CXZ_V*VHG(I,5)*HSURA(I)
         SC6_V=CYZ_V*VHG(I,6)*HSURA(I)
         SS3_V = SC5_V+SC6_V
C--------------------------------------------------
         SS1(I)=SS1(I)+SS1_V
         SS2(I)=SS2(I)+SS2_V
         SS3(I)=SS3(I)+SS3_V
         SC5(I)=SC5(I)+SC5_V
         SC6(I)=SC6(I)+SC6_V
         SF1(I)=SF1(I)+SF1_V
         SF2(I)=SF2(I)+SF2_V
         TESY(I)= ((SS1_V*VHG(I,1)+SS2_V*VHG(I,2))*THK(I)+
     .            (SF1_V*VHG(I,3)-SF2_V*VHG(I,4))*THK(I)*C6(I) +
     .             SC5_V*VHG(I,5)+SC6_V*VHG(I,6))*DT1
      END DO !I=JFT,JLT
C--------Fint      
      DO I=JFT,JLT
         Y13S= MY13(I)*SS3(I)
         X13S= MX13(I)*SS3(I)
         Y34S6=MY34(I)*SC6(I)
         Y23S5=MY23(I)*SC5(I)
         X23S5=MX23(I)*SC5(I)
         X34S6=MX34(I)*SC6(I)
         C2=QUART*THK(I)
C--------NOEUDS 1,3-------
        B13=(MY13(I)*X24(I)-MX13(I)*Y24(I))*HSURA(I)
        VF(I,1,1)=VF(I,1,1)+B13*SS1(I)
        VF(I,1,3)=C2*SS1(I)
        VF(I,2,1)=VF(I,2,1)+B13*SS2(I)
        VF(I,2,3)=C2*SS2(I)
        VF(I,3,3)=SS3(I)
C--------NOEUDS 2,4-------
        B24=(MX13(I)*Y13(I)-MY13(I)*X13(I))*HSURA(I)
        VF(I,1,2)= VF(I,1,2)+B24*SS1(I)
        VF(I,1,4)=-VF(I,1,3)
        VF(I,2,2)= VF(I,2,2)+B24*SS2(I)
        VF(I,2,4)=-VF(I,2,3)
        VF(I,3,4)=-VF(I,3,3)
C--------NOEUDS 1,3-------
          C3=C6(I)*B13
          C4=C6(I)*C2
          VM(I,1,1)=VM(I,1,1)+C3*SF2(I)+Y23S5+Y34S6
          VM(I,1,3)=VM(I,1,3)+C4*SF2(I)-Y13S
          VM(I,2,1)=VM(I,2,1)+C3*SF1(I)-X23S5-X34S6
          VM(I,2,3)=VM(I,2,3)+C4*SF1(I)+X13S
C--------NOEUDS 2,4-------
          C3=C6(I)*B24
          VM(I,1,2)=VM(I,1,2)+C3*SF2(I)+Y23S5-Y34S6
          VM(I,1,4)=VM(I,1,4)-C4*SF2(I)-Y13S
          VM(I,2,2)=VM(I,2,2)+C3*SF1(I)-X23S5+X34S6
          VM(I,2,4)=VM(I,2,4)-C4*SF1(I)+X13S
C
        C2=Z1(I)*HSURA(I)
        VF(I,3,1)=VF(I,3,1)+C2*(SSZ1(I)*Y24(I)-SSZ2(I)*X24(I))
        VF(I,3,2)=VF(I,3,2)+C2*(-SSZ1(I)*Y13(I)+SSZ2(I)*X13(I))
      END DO !I=JFT,JLT
C      
      CALL CZEHOURV(JFT  ,JLT  ,IPARTC,EVIS,KFTS  ,TESY ) 
      RETURN
      END      
Chd|====================================================================
Chd|  CZHOUREP_OR                   source/elements/shell/coquez/czfintn.F
Chd|-- called by -----------
Chd|        CZFINTN_OR                    source/elements/shell/coquez/czfintn.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CZHOUREP_OR(JFT   ,JLT  ,SIGY ,VSTRE ,MSTRE ,
     1                         THK   ,FAC  ,ESX  ,EMX   ,NPT   ,
     2                         DGLAS ,VGLAS,NEL  ) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include "implicit_f.inc"
#include "mvsiz_p.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
        INTEGER JFT  ,JLT,NPT,NEL
        my_real 
     .       THK(*) ,VGLAS(NEL,12),VSTRE(NEL,5),MSTRE(NEL,3),
     .       FAC(MVSIZ,2),SIGY(*),ESX(*),EMX(*),DGLAS(MVSIZ,12)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I,MX, IC, II, J,NPT1
        my_real 
     .  SVM,SVMN,C1B,C2B,CMM,CNN,UFAC,TOL,COEF,EH1,EH2,
     .  CNNX,CNNY,CNNX_K,CNNY_K,CMMX,CMMY,CMMX_K,CMMY_K,SXY0,MXY0,
     .  SVMC,SIGY2,SVM0,SVMC_K,A_BK,A_B,A_K,UNDOUZSR,AUX,
     .  COEF1,COEFH,SIGY0,SXYH,MXYH,SIGYH,FACM,FACF,FACT
C-----------------------------------------------
       COEFH= ZEP99
       COEF= ZEP85
       TOL= EM18 
      IF (NPT.EQ.0) THEN
       COEF1=SEIZE
      ELSE
       COEF1=VINGT5
      ENDIF
C---- FAC(1,I)= mean value of Et/G; FAC(2,I)= min(Et_ip/G ), negative value when failure
C---- en Elastic FAC(1,I)=0;FAC(2,I)= 1   
      DO I=JFT,JLT
          IF (ESX(I)<ZERO.AND.EMX(I)<ZERO) CYCLE
          IF (SIGY(I).LT.ZEP9EP30) THEN
           UFAC=ABS(MIN(FAC(I,1),ABS(FAC(I,2)))-UN)
C---------elastic inc on quadrature-----------           
           IF (UFAC.LT.TOL) THEN
            SIGY2 = SIGY(I)*SIGY(I)
            SXY0= VSTRE(I,1)*VSTRE(I,1)+VSTRE(I,2)*VSTRE(I,2)
     .           -VSTRE(I,1)*VSTRE(I,2)+TROIS*VSTRE(I,3)*VSTRE(I,3)
            MXY0= MSTRE(I,1)*MSTRE(I,1)+MSTRE(I,2)*MSTRE(I,2)
     .           -MSTRE(I,1)*MSTRE(I,2)+TROIS*MSTRE(I,3)*MSTRE(I,3)
            SIGY0 =  SXY0+COEF1*MXY0
C------------due to different criteria, SIGY0: Von misese
            SIGY2 = MAX(SIGY2,SIGY0)
            CNN=COEF
            CMM=COEF*THK(I)*UNSEIZE
            CNNX=CNN*VGLAS(I,1)
            CNNY=CNN*VGLAS(I,2)
            CNNX_K=CNN*VGLAS(I,7)
            CNNY_K=CNN*VGLAS(I,8)
            CMMX=CMM*VGLAS(I,3)
            CMMY=CMM*VGLAS(I,4)
            CMMX_K=CMM*VGLAS(I,9)
            CMMY_K=CMM*VGLAS(I,10)
            SXYH= CNNX*CNNX+CNNY*CNNY-CNNX*CNNY
            MXYH= CMMX*CMMX+CMMY*CMMY-CMMX*CMMY
            SXYH= SXYH+CNNX_K*CNNX_K+CNNY_K*CNNY_K-CNNX_K*CNNY_K
            MXYH= MXYH+CMMX_K*CMMX_K+CMMY_K*CMMY_K-CMMX_K*CMMY_K
            SXYH= SXYH+ABS(CNNX*(DEUX*CNNX_K-CNNY_K)
     1                    +CNNY*(DEUX*CNNY_K-CNNX_K))
            MXYH= MXYH+ABS(CMMX*(DEUX*CMMX_K-CMMY_K)
     1                    +CMMY*(DEUX*CMMY_K-CMMX_K))     
            SIGYH =  SXYH+COEF1*MXYH
C            
            SVM=SIGY0+SIGYH
            EH1=ZERO
            EH2=ZERO
C            
            IF (SVM > SIGY2) THEN
             FACT = SVM/MAX(SIGY2,TOL) -UN
             EH1 = MIN(FACT,COEFH)
             EH2 = EH1
            ENDIF
           ELSE
            EH1=UN-FAC(I,1)
            EH2=UN-ABS(FAC(I,2))
           END IF
           IF (ESX(I).LT.ZERO) EH1=ZERO
           IF (EMX(I).LT.ZERO) EH2=ZERO
C-------------MEMBRANE------------------------
             VGLAS(I,1)=VGLAS(I,1)-EH1*DGLAS(I,1)
             VGLAS(I,2)=VGLAS(I,2)-EH1*DGLAS(I,2)
             VGLAS(I,7)=VGLAS(I,7)-EH1*DGLAS(I,7)
             VGLAS(I,8)=VGLAS(I,8)-EH1*DGLAS(I,8)
C-------------FLEXION------------------------
             VGLAS(I,3) =VGLAS(I,3) -EH2*DGLAS(I,3)
             VGLAS(I,4) =VGLAS(I,4) -EH2*DGLAS(I,4)
             VGLAS(I,9) =VGLAS(I,9) -EH2*DGLAS(I,9)
             VGLAS(I,10)=VGLAS(I,10)-EH2*DGLAS(I,10)
          ENDIF 
      END DO !I=JFT,JLT
C-----taking into accound failing in the layers(law25 first)----
       NPT1=MIN(8,NPT)
       FACM = ZEP85  +ZEP015*(NPT1-2)
       FACF = ZEP7 +ZEP015*(NPT1-2)
       FACT = ZEP9999
       DO I=JFT,JLT
          IF (FAC(I,2).LT.ZERO) THEN
            VGLAS(I,1)=FACM*VGLAS(I,1)
            VGLAS(I,2)=FACM*VGLAS(I,2)
            VGLAS(I,7)=FACM*VGLAS(I,7)
            VGLAS(I,8)=FACM*VGLAS(I,8)
            VGLAS(I,5)=FACT*VGLAS(I,5)
            VGLAS(I,6)=FACT*VGLAS(I,6)
            VGLAS(I,11)=FACT*VGLAS(I,11)
            VGLAS(I,12)=FACT*VGLAS(I,12)
            VGLAS(I,3)=FACF*VGLAS(I,3)
            VGLAS(I,4)=FACF*VGLAS(I,4)
            VGLAS(I,9)=FACF*VGLAS(I,9)
            VGLAS(I,10)=FACF*VGLAS(I,10)
          ENDIF 
       END DO 
C      
      RETURN
      END      
Chd|====================================================================
Chd|  CZHOUREPRZ_OR                 source/elements/shell/coquez/czfintn.F
Chd|-- called by -----------
Chd|        CZFINTNRZ_OR                  source/elements/shell/coquez/czfintn.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CZHOUREPRZ_OR(JFT   ,JLT   ,SIGY ,VSTRE ,MSTRE ,
     1                           THK   ,FAC   ,ESX  ,EMX   ,NPT   ,
     2                           DGLAS ,VGLAS ,NEL  ) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include "implicit_f.inc"
#include "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
        INTEGER JFT  ,JLT,NPT,NEL
        my_real 
     .       THK(*) ,VGLAS(NEL,19),VSTRE(NEL,5),MSTRE(NEL,3),
     .       FAC(MVSIZ,2),SIGY(*),ESX(*),EMX(*),DGLAS(MVSIZ,18)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I,MX, IC, II, J,NPT1
        my_real 
     .  SVM,SVMN,C1B,C2B,CMM,CNN,UFAC,TOL,COEF,EH1,EH2,
     .  CNNX,CNNY,CNNX_K,CNNY_K,CMMX,CMMY,CMMX_K,CMMY_K,SXY0,MXY0,
     .  SVMC,SIGY2,SVM0,SVMC_K,A_BK,A_B,A_K,UNDOUZSR,AUX,
     .  COEF1,COEFH,CNNXY,CNNXY_K,CMMXY,CMMXY_K,FACM,FACF,FACT,SIGY0
C-----------------------------------------------
       COEFH= ZEP99
       COEF= ZEP85
       TOL= EM18 
      IF (NPT.EQ.0) THEN
       COEF1=SEIZE
      ELSE
       COEF1=VINGT5
      ENDIF
      DO I=JFT,JLT
          IF (ESX(I)<ZERO.AND.EMX(I)<ZERO) CYCLE
          IF (SIGY(I).LT.ZEP9EP30) THEN
           UFAC=ABS(MIN(FAC(I,1),ABS(FAC(I,2)))-UN)
           SIGY2 = SIGY(I)*SIGY(I)
           SVM =ZERO
           SXY0=ZERO   
           IF (UFAC.LT.TOL) THEN
c            CYCLE
            SXY0= VSTRE(I,1)*VSTRE(I,1)+VSTRE(I,2)*VSTRE(I,2)
     .           -VSTRE(I,1)*VSTRE(I,2)+TROIS*VSTRE(I,3)*VSTRE(I,3)
            MXY0= MSTRE(I,1)*MSTRE(I,1)+MSTRE(I,2)*MSTRE(I,2)
     .           -MSTRE(I,1)*MSTRE(I,2)+TROIS*MSTRE(I,3)*MSTRE(I,3)
            SIGY0 =  SXY0+COEF1*MXY0
            SIGY2 = MAX(SIGY2,SIGY0)
            CNN=COEF
            CMM=COEF*THK(I)*UNSEIZE
            CNNX=CNN*VGLAS(I,1)
            CNNY=CNN*VGLAS(I,2)
            CNNXY=CNN*VGLAS(I,13)
            CNNX_K=CNN*VGLAS(I,7)
            CNNY_K=CNN*VGLAS(I,8)
            CNNXY_K=CNN*VGLAS(I,15)
            CMMX=CMM*VGLAS(I,3)
            CMMY=CMM*VGLAS(I,4)
            CMMXY=CMM*VGLAS(I,14)
            CMMX_K=CMM*VGLAS(I,9)
            CMMY_K=CMM*VGLAS(I,10)
            CMMXY_K=CMM*VGLAS(I,16)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
            SXY0= SXY0+CNNX*CNNX+CNNY*CNNY-CNNX*CNNY+TROIS*CNNXY*CNNXY
            MXY0= MXY0+CMMX*CMMX+CMMY*CMMY-CMMX*CMMY+TROIS*CMMXY*CMMXY
            SXY0= SXY0+CNNX_K*CNNX_K+CNNY_K*CNNY_K-CNNX_K*CNNY_K+
     1            TROIS*CNNXY_K*CNNXY_K
            MXY0= MXY0+CMMX_K*CMMX_K+CMMY_K*CMMY_K-CMMX_K*CMMY_K+
     1            TROIS*CMMXY_K*CMMXY_K
            SXY0= SXY0+ABS(CNNX*(DEUX*CNNX_K-CNNY_K)
     1                    +CNNY*(DEUX*CNNY_K-CNNX_K))
            MXY0= MXY0+ABS(CMMX*(DEUX*CMMX_K-CMMY_K)
     1                    +CMMY*(DEUX*CMMY_K-CMMX_K))     
            SVM =  SXY0+COEF1*MXY0
             EH1 = ZERO
             EH2 = ZERO
            IF (SVM > SIGY2) THEN
             FACT = SVM/MAX(SIGY2,TOL) -UN
             EH1 = MIN(FACT,COEFH)
             EH2 = EH1
            ENDIF
           ELSE
            EH1=UN-FAC(I,1)
            EH2=UN-ABS(FAC(I,2))
           END IF
              IF (ESX(I).LT.ZERO) EH1=ZERO
              IF (EMX(I).LT.ZERO) EH2=ZERO
C-------------MEMBRANE------------------------
             VGLAS(I,1)=VGLAS(I,1)-EH1*DGLAS(I,1)
             VGLAS(I,2)=VGLAS(I,2)-EH1*DGLAS(I,2)
             VGLAS(I,7)=VGLAS(I,7)-EH1*DGLAS(I,7)
             VGLAS(I,8)=VGLAS(I,8)-EH1*DGLAS(I,8)
             VGLAS(I,13)=VGLAS(I,13)-EH1*DGLAS(I,13)
             VGLAS(I,15)=VGLAS(I,15)-EH1*DGLAS(I,15)
C-------------FLEXION------------------------
             VGLAS(I,3) =VGLAS(I,3) -EH2*DGLAS(I,3)
             VGLAS(I,4) =VGLAS(I,4) -EH2*DGLAS(I,4)
             VGLAS(I,9) =VGLAS(I,9) -EH2*DGLAS(I,9)
             VGLAS(I,10)=VGLAS(I,10)-EH2*DGLAS(I,10)
             VGLAS(I,14)=VGLAS(I,14)-EH1*DGLAS(I,14)
             VGLAS(I,16)=VGLAS(I,16)-EH1*DGLAS(I,16)
          ENDIF 
      END DO !I=JFT,JLT
C-----taking into accound failing in the layers(law25 first)----
       NPT1=MIN(8,NPT)
       FACM = ZEP85  +ZEP015*(NPT1-2)
       FACF = ZEP7 +ZEP015*(NPT1-2)
       FACT = ZEP9999
       DO I=JFT,JLT
          IF (FAC(I,2).LT.ZERO) THEN
            VGLAS(I,1)=FACM*VGLAS(I,1)
            VGLAS(I,2)=FACM*VGLAS(I,2)
            VGLAS(I,7)=FACM*VGLAS(I,7)
            VGLAS(I,8)=FACM*VGLAS(I,8)
            VGLAS(I,13)=FACM*VGLAS(I,13)
            VGLAS(I,15)=FACM*VGLAS(I,15)
            VGLAS(I,5)=FACT*VGLAS(I,5)
            VGLAS(I,6)=FACT*VGLAS(I,6)
            VGLAS(I,11)=FACT*VGLAS(I,11)
            VGLAS(I,12)=FACT*VGLAS(I,12)
            VGLAS(I,3)=FACF*VGLAS(I,3)
            VGLAS(I,4)=FACF*VGLAS(I,4)
            VGLAS(I,9)=FACF*VGLAS(I,9)
            VGLAS(I,10)=FACF*VGLAS(I,10)
            VGLAS(I,14)=FACF*VGLAS(I,14)
            VGLAS(I,16)=FACF*VGLAS(I,16)
          ENDIF 
       END DO 
C      
      RETURN
      END      
Chd|====================================================================
Chd|  CZFINTNM1                     source/elements/shell/coquez/czfintn.F
Chd|-- called by -----------
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|-- calls ---------------
Chd|        CZFINTNM                      source/elements/shell/coquez/czfintn.F
Chd|====================================================================
        SUBROUTINE CZFINTNM1(JFT  ,JLT  ,THK  ,AA   ,VHG  ,
     2                       X13  ,X24  ,Y13  ,Y24  ,VF   ,
     3                       MX13 ,MX23 ,MY13 ,MY23 ,
     4                       G    ,RHO  ,AREA ,AMU  ,DT1  ,
     5                       V13  ,V24  ,VHI  ,VGLAS,
     6                       OFF  ,IPARTC,EVIS,KFTS  ,NEL )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C        CALCUL  viscous VF(3,*) due au shear when npt=1
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
        INTEGER JFT,JLT,IPARTC(*),NEL
        my_real 
     .       THK(*) ,AA(*)  ,VHG(MVSIZ,6),
     .       X13(*) ,X24(*) ,Y13(*) ,Y24(*)  ,
     .       MX13(*),MX23(*),MY13(*),MY23(*),
     .       VF(MVSIZ,3,4),G(*),V13(MVSIZ,3),V24(MVSIZ,3),VHI(MVSIZ,3),
     .       RHO(*) ,AREA(*), OFF(*),AMU(*) ,
     .       EVIS(NPSAV,*),DT1,VGLAS(NEL,12)
         INTEGER KFTS
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I,MX, IC, II, J, JST(MVSIZ+1)
        my_real 
     .  C2,HSURA,MX34,MY34,FV,GV(MVSIZ),
     .  SC6_V,SC5_V,CXZ_V,CYZ_V,SS3_V,HVL,HVZ(MVSIZ),
     .   GAMA1(MVSIZ), GAMA2(MVSIZ), GAMA3(MVSIZ), GAMA4(MVSIZ),
     .   GAMAVZ(MVSIZ),PX1(MVSIZ), PX2(MVSIZ), PY1(MVSIZ), PY2(MVSIZ),
     .   PX1V, PX2V, PY1V, PY2V, TESY(MVSIZ),HX(MVSIZ),HY(MVSIZ),HXA,HYA,FAC
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
c gama_i=1/4(h_i-hx*bx_i-hy*byi); bx_i=(y24 -y13 -y24 y13)/A;by_i=(-x24 x13 x24 -x13)/A;
      DO I=JFT,JLT
        PX1(I)  =  Y24(I)
        PX2(I)  = -Y13(I)
        PY1(I)  = -X24(I)
        PY2(I)  =  X13(I)
C----1/4*hx		
	    HX(I) = UNDEMI*(-X13(I)+X24(I))+(MX13(I)-MX23(I))
	    HY(I) = UNDEMI*(-Y13(I)+Y24(I))+(MY13(I)-MY23(I))
      ENDDO
c gama_i*vz_i = vhi-hx*(y24*v13-y13*v24)-hy*(-x24*v13+x13*v24)
      DO I=JFT,JLT
		GAMAVZ(I) = VHI(I,3)-HX(I)*(PX1(I)*V13(I,3)+PX2(I)*V24(I,3))
     .                      -HY(I)*(PY1(I)*V13(I,3)+PY2(I)*V24(I,3))
      ENDDO
      DO I=JFT,JLT
	    HXA =HX(I)*AA(I) 
	    HYA =HY(I)*AA(I) 
        PX1V    = PX1(I)*HXA
        PX2V    = PX2(I)*HXA
        PY1V    = PY1(I)*HYA
        PY2V    = PY2(I)*HYA
C        J=1,2 STOCK LA PARTIE ANTISYM (F(1)=-F(3),F(2)=-F(4))                                  
C        J=3,4 STOCK LA PARTIE SYM (F(1)=F(3),F(2)=F(4))
        GAMA1(I)= - PX1V-PY1V
        GAMA3(I)= QUART
        GAMA2(I)= - PX2V-PY2V
        GAMA4(I)= -QUART
      ENDDO
C----fixing fac as Q4, can be changed w/ G(i) 	  
	  FAC = 0.83*EM02
C--- same as Q4, no more daming but elatic w/ FAC*G 	  
      DO I=JFT,JLT
        HVL=FAC*G(I)*THK(I)*OFF(I)*DT1
        VGLAS(I,10)=VGLAS(I,10)+HVL*GAMAVZ(I)
		C2 = QUATRE*VGLAS(I,10)
        VF(I,3,1)=VF(I,3,1)+C2*GAMA1(I)
        VF(I,3,2)=VF(I,3,2)+C2*GAMA2(I)
        VF(I,3,3)=VF(I,3,3)+C2*GAMA3(I)
        VF(I,3,4)=VF(I,3,4)+C2*GAMA4(I)
C		
		TESY(I) = AREA(I)*VGLAS(I,10)*DT1*(
     .           (GAMA1(I)+GAMA2(I))*V13(I,3)+(GAMA3(I)+GAMA4(I))*V24(I,3))
	  ENDDO
       MX = IPARTC(JFT)
#include "simd.inc"
       DO I=JFT,JLT
          EVIS(8,MX)=EVIS(8,MX) + TESY(I)
       ENDDO
C---- damping 
c      FAC=0.01	  
cc      FAC=0.5	  
c      DO I=JFT,JLT
c        HVZ(I)=QUART*FAC*RHO(I)*THK(I)*SQRT(AREA(I))
c	  ENDDO       
cC---- mode Z1+Z2-(Z3+Z4) 	  
c      DO I=JFT,JLT
c        FV=HVZ(I)*(V13(I,3)+V24(I,3))
c        VF(I,3,1)=VF(I,3,1)+FV
c        VF(I,3,2)=VF(I,3,2)+FV
c	  ENDDO       
cC---- mode Z1+Z4-(Z2+Z3) 	  
c      DO I=JFT,JLT
c        FV=HVZ(I)*(V13(I,3)-V24(I,3))
c        VF(I,3,1)=VF(I,3,1)+FV
c        VF(I,3,2)=VF(I,3,2)-FV
c	  ENDDO       

	   GV(JFT:JLT) = 0.001*G(JFT:JLT)
       CALL CZFINTNM(JFT  ,JLT  ,THK  ,AA   ,VHG  ,
     2               X13  ,X24  ,Y13  ,Y24  ,VF   ,
     3               GV   ,RHO  ,AREA ,AMU  ,DT1  ,
     4               OFF  ,IPARTC,EVIS,KFTS  )
C
      RETURN
      END
