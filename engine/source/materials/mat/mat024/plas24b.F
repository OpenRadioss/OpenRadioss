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
Chd|  PLAS24B                       source/materials/mat/mat024/plas24b.F
Chd|-- called by -----------
Chd|        CONC24                        source/materials/mat/mat024/conc24.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PLAS24B(NEL   ,NINDX ,INDX  ,NGL   ,PM    ,
     .                   SIG   ,DAM   ,CRAK  ,EPSVP ,CDAM  ,
     .                   RHO   ,EINT  ,VK0   ,VK    ,ROB   ,PLA   ,
     .                   DEPS1 ,DEPS2 ,DEPS3 ,DEPS4 ,DEPS5 ,DEPS6 ,
     .                   S1    ,S2    ,S3    ,S4    ,S5    ,S6    ,
     .                   SCAL1 ,SCAL2 ,SCAL3 ,SCLE2 )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "comlock.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NINDX,NGL(NEL),INDX(NINDX)
      my_real PM(NPROPM)
      my_real, DIMENSION(NEL) :: S1,S2,S3,S4,S5,S6,EPSVP,RHO,EINT,VK0,VK,ROB,
     .   SCAL1,SCAL2,SCAL3,SCLE2,DEPS1,DEPS2,DEPS3,DEPS4,DEPS5,DEPS6
      my_real, DIMENSION(NEL,7)   :: PLA
      my_real, DIMENSION(NEL,3,3) :: CDAM
      my_real, DIMENSION(NEL,6)   :: SIG
      my_real, DIMENSION(NEL,3)   :: DAM,CRAK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NITER,ITER
      my_real, DIMENSION(NEL) :: SM,DE1,DE2,DE3,DE4,DE5,DE6,C44,C55,C66
      my_real FC, RT, RC, RCT1, RCT2, AA, BC,
     .   BT, AC, HBP, ALI0, ALF0, VKY, TOL, ROK0, RO0, HV0, VMAX, EXPO,
     .   YOUNG, NU, G, DEN,  RATE, FAC, FAC1,ROK, R2, AJ3, CS3T,
     .   BB, DF, RF, RF2, AJ2, AJJ, DKDSM, DRFDSM, B0, DRF3, B1,
     .   B2, ALPHA, PHI, TO, DFDTO, HPV, HP, ECR,SIGM,
     .   TS1, TS2, TS3, TS4, TS5, TS6, 
     .   DFS1,DFS2,DFS3,DFS4,DFS5,DFS6,DGS1,DGS2,DGS3,DGS4,DGS5,DGS6, 
     .   H1A, H2A, H3A, H4A,H5A, H6A, H1N, H2N, H3N, H4N, H5N, H6N, HH,
     .   LAMBDA,DEPSP1,DEPSP2,DEPSP3,DEPSP4,DEPSP5,DEPSP6,
     .   DEPSEL1,DEPSEL2,DEPSEL3,DEPSEL4,DEPSEL5,DEPSEL6,
     .   DEPSVP, RR,VKMAX, RO, DIV, VKK, NUMER, DENOM,
     .   BULK,DP,RHO0,HVFAC,D1,D2,D3,DEPSV,SEUIL,DFDJ,NORMS,
     .   CP11,CP12,CP13,CP14,CP15,CP16,CP21,CP22,CP23,CP24,CP25,CP26,CP31,
     .   CP32,CP33,CP34,CP35,CP36,CP41,CP42,CP43,CP44,CP45,CP46,CP51,CP52,
     .   CP53,CP54,CP55,CP56,CP61,CP62,CP63,CP64,CP65,CP66,
     .   C11,C12,C13,C22,C23,C33
c=======================================================================
      RHO0  = PM(1)
      YOUNG = PM(20)
      NU    = PM(21)
      G     = PM(22)
      BULK  = PM(32)
      FC    = PM(33)
      RT    = PM(34)
      RC    = PM(35)
      RCT1  = PM(36)
      RCT2  = PM(37)
      AA    = PM(38)
      BC    = PM(39)
      BT    = PM(40)
      AC    = PM(41)
      HBP   = PM(43)
      ALI0  = PM(44)
      ALF0  = PM(45)
      VKY   = PM(46)
      ROK0  = PM(29)
      RO0   = PM(30)
      HV0   = PM(48)
      VMAX  = PM(27)
      EXPO  = PM(49)
      HVFAC = PM(58)
c
      TOL =( RT-RC)/TWENTY
c------------------------------------------------
C  RETOUR SUR LE CRITERE ,SIG ,CDAM,CRAK,...
c------------------------------------------------
      DO J = 1,NINDX
        I = INDX(J)
        CRAK(I,1)=CRAK(I,1)-SCLE2(I)*DEPS1(I)
        CRAK(I,2)=CRAK(I,2)-SCLE2(I)*DEPS2(I)
        CRAK(I,3)=CRAK(I,3)-SCLE2(I)*DEPS3(I)
        DE1(I)=ONE - MAX( ZERO , SIGN(DAM(I,1),CRAK(I,1)) )
        DE2(I)=ONE - MAX( ZERO , SIGN(DAM(I,2),CRAK(I,2)) )
        DE3(I)=ONE - MAX( ZERO , SIGN(DAM(I,3),CRAK(I,3)) )
        SCAL1(I)=HALF+SIGN(HALF,DE1(I)-ONE)
        SCAL2(I)=HALF+SIGN(HALF,DE2(I)-ONE)
        SCAL3(I)=HALF+SIGN(HALF,DE3(I)-ONE)      
        DE4(I)=SCAL1(I)*SCAL2(I)
        DE5(I)=SCAL2(I)*SCAL3(I)
        DE6(I)=SCAL3(I)*SCAL1(I)
c-------
        DEN = ONE - NU**2 *(DE4(I) + DE5(I) + DE6(I)
     .      + TWO*NU*SCAL1(I)*SCAL2(I)*SCAL3(I))
        DEN = ONE / DEN
        CDAM(I,1,1) = YOUNG*DE1(I)*(ONE -NU**2*SCAL2(I)*SCAL3(I))*DEN
        CDAM(I,2,2) = YOUNG*DE2(I)*(ONE -NU**2*SCAL1(I)*SCAL3(I))*DEN
        CDAM(I,3,3) = YOUNG*DE3(I)*(ONE -NU**2*SCAL2(I)*SCAL1(I))*DEN
        CDAM(I,1,2) = NU*YOUNG*SCAL1(I)*SCAL2(I) *(ONE+NU*SCAL3(I))*DEN
        CDAM(I,2,3) = NU*YOUNG*SCAL2(I)*SCAL3(I) *(ONE+NU*SCAL1(I))*DEN
        CDAM(I,1,3) = NU*YOUNG*SCAL1(I)*SCAL3(I) *(ONE+NU*SCAL2(I))*DEN
        CDAM(I,2,1) = CDAM(I,1,2)
        CDAM(I,3,1) = CDAM(I,1,3)
        CDAM(I,3,2) = CDAM(I,2,3)
        C44(I) = G*DE4(I)
        C55(I) = G*DE5(I)
        C66(I) = G*DE6(I)
c-----
        SIG(I,1)=CDAM(I,1,1)*CRAK(I,1)+CDAM(I,1,2)*CRAK(I,2)+CDAM(I,1,3)*CRAK(I,3)
        SIG(I,2)=CDAM(I,2,1)*CRAK(I,1)+CDAM(I,2,2)*CRAK(I,2)+CDAM(I,2,3)*CRAK(I,3)
        SIG(I,3)=CDAM(I,3,1)*CRAK(I,1)+CDAM(I,3,2)*CRAK(I,2)+CDAM(I,3,3)*CRAK(I,3)
        SIG(I,4)=DE4(I)*SIG(I,4)-C44(I)*DEPS4(I)*(ONE-SCLE2(I))
        SIG(I,5)=DE5(I)*SIG(I,5)-C55(I)*DEPS5(I)*(ONE-SCLE2(I))
        SIG(I,6)=DE6(I)*SIG(I,6)-C66(I)*DEPS6(I)*(ONE-SCLE2(I))
        S1(I) = SCAL1(I)*SIG(I,1)
        S2(I) = SCAL2(I)*SIG(I,2)
        S3(I) = SCAL3(I)*SIG(I,3)
        S4(I) = SIG(I,4)*SCAL1(I)*SCAL2(I)
        S5(I) = SIG(I,5)*SCAL2(I)*SCAL3(I)
        S6(I) = SIG(I,6)*SCAL3(I)*SCAL1(I)
        SM(I) = (S1(I)+S2(I)+S3(I)) * THIRD
        S1(I) = S1(I) - SM(I)
        S2(I) = S2(I) - SM(I)
        S3(I) = S3(I) - SM(I)
c----------------------------     
        DENOM = ABS(CRAK(I,1))+ABS(CRAK(I,2))+ABS(CRAK(I,3))
     .        +(ABS(SIG(I,4)) +ABS(SIG(I,5)) +ABS(SIG(I,6)))/G 
c----------------------------     
        IF (DENOM == ZERO) CYCLE
c----------------------------    
        NUMER = ABS(DEPS1(I))+ABS(DEPS2(I))+ABS(DEPS3(I))
     .        + ABS(DEPS4(I))+ABS(DEPS5(I))+ABS(DEPS6(I))
c
        RATE  = NUMER / DENOM
        NITER = NINT(THREE*RATE) + 1
        NITER = MIN0(NITER,10)    
c--------------------------------
        FAC = SCLE2(I)/NITER      
        DEPS1(I) = FAC * DEPS1(I) 
        DEPS2(I) = FAC * DEPS2(I) 
        DEPS3(I) = FAC * DEPS3(I) 
        DEPS4(I) = FAC * DEPS4(I) 
        DEPS5(I) = FAC * DEPS5(I) 
        DEPS6(I) = FAC * DEPS6(I) 
c---------------------------------------------------------
c ...   DEBUT DES ITERATIONS
c---------------------------------------------------------
        DO ITER=1,NITER
c--------------------
          ROK = ROK0+ROB(I)-RO0
          R2  = S1(I)**2 + S2(I)**2 + S3(I)**2
     .        +(S4(I)**2 + S5(I)**2 + S6(I)**2)*TWO
          IF (SM(I) >= RT-TOL) THEN
            VK(I) = ONE
          ELSEIF (SM(I) > RC) THEN
            VK(I) = ONE +(ONE-VK0(I))*(RCT1-TWO*RC*SM(I)+SM(I)**2)/RCT2
          ELSEIF (SM(I) > ROK)THEN
            VK(I) = VK0(I)
C CAPE
          ELSEIF (SM(I) > ROB(I)) THEN
            VK(I) = VK0(I)*(ONE - ((SM(I)-ROK)/(ROB(I)-ROK))**2)
          ELSE
            VK(I) = ZERO
          ENDIF
C ON TRONQUE PRES DE l'AXE des pressions
C CRITERE
          AJ3 = S1(I)*S2(I)*S3(I)
     .        - S1(I)*S5(I)*S5(I)-S2(I)*S6(I)*S6(I)-S3(I)*S4(I)*S4(I)
     .        + TWO*S4(I)*S5(I)*S6(I)
          CS3T= HALF * AJ3*(THREE/HALF*MAX(R2,EM20))**THREE_HALF
          CS3T= MIN(ONE,CS3T)
          CS3T= MAX(-ONE,CS3T)
          BB  = HALF*((ONE - CS3T)*BC+(ONE +CS3T)*BT)
          DF  = SQRT(BB*BB + MAX(-AA*SM(I) + AC,EM9))
          RF  = (-BB+DF)/AA
          RF2 = RF**2
          AJ2 = HALF*R2
          AJJ = SQRT(AJ2)
          RR  = SQR2*AJJ
c
          IF (R2/RF2 > EM04) THEN
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C NOW COMPUTES ALL TERMS TO BUILD UP PLASTIC MATRIX
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C D I M E N S I O N S   RF:CONT       TO:CONT  PHI:ADIM  B0:ADIM  B1:1/CONT
C                       B2:1/CONT**2  DFDTO:ADIM  DRFDSM:ADIM DKDSM:1/CONT
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C PRINCIPALES DERIVEES
            SM(I) = MAX(ROB(I),SM(I))
            IF (SM(I) >= RT-TOL) THEN
              DKDSM =ZERO
            ELSEIF(SM(I)>RC)THEN
              DKDSM = TWO*(ONE -VK0(I))*(SM(I)-RC)/RCT2
            ELSEIF(SM(I)>ROK)THEN
              DKDSM = ZERO
            ELSE
              DKDSM = -TWO*VK0(I)*(SM(I)-ROK)/(ROB(I)-ROK)**2
            ENDIF
            DRFDSM= -HALF / DF
            B0    = -THIRD * VK(I) *DRFDSM - THIRD * RF * DKDSM
            DRF3  =  HALF* (-ONE + BB/DF) * (BT-BC)/AA
            B1    =  HALF*SQR2/AJJ
     .            +  VK(I)*DRF3*FOURTH*AJ3*(THREE/AJ2)**TWOP5
            B2    = -VK(I)*DRF3*HALF*(THREE/AJ2)**THREE_HALF
C           DF/DSIG
            TS1 = S1(I)**2 + S4(I)**2 + S6(I)**2 - TWO*THIRD*AJ2
            TS2 = S2(I)**2 + S4(I)**2 + S5(I)**2 - TWO*THIRD*AJ2
            TS3 = S3(I)**2 + S5(I)**2 + S6(I)**2 - TWO*THIRD*AJ2
            TS4 = TWO* (S5(I)*S6(I)-S4(I)*S3(I))
            TS5 = TWO* (S6(I)*S4(I)-S5(I)*S1(I))
            TS6 = TWO* (S4(I)*S5(I)-S6(I)*S2(I))     
            DFS1= B0 + B1*S1(I) + B2*TS1
            DFS2= B0 + B1*S2(I) + B2*TS2
            DFS3= B0 + B1*S3(I) + B2*TS3
            DFS4= TWO*B1*S4(I) + B2*TS4 
            DFS5= TWO*B1*S5(I) + B2*TS5 
            DFS6= TWO*B1*S6(I) + B2*TS6 
C
            IF (SM(I) > ROK .and. VK(I) > VKY) THEN
              ALPHA = (ONE-VK(I))*ALI0  + (VK(I)-VKY)*ALF0
              ALPHA = ALPHA / (ONE-VKY)
            ELSEIF (VK0(I) > VKY) THEN
              ALPHA = (ONE-VK0(I))*ALI0 + (VK0(I)-VKY)*ALF0
              ALPHA = ALPHA / (ONE-VKY)
            ELSE
              ALPHA = ALI0
            ENDIF
C
c           Definition de la zone de transition
C
c            SEUIL = ALI0
            SEUIL = MIN(-EM01,ALI0)
c            IF(SM(I)<ROK )THEN
c              FAC=(SEUIL-B0)/(SEUIL+THIRD * VK0(I) *DRFDSM)
            IF (SM(I) < ROK .AND. B0 < ZERO) THEN
              FAC = (SEUIL-B0) / SEUIL
              FAC = MAX(ZERO,FAC)
              FAC = MIN(ONE,FAC)
              FAC1= ONE-FAC
            ELSE
              FAC = ONE
              FAC1= ZERO
            ENDIF
c           Securite on ne veut pas que la normale a la loi d'ecoulement soit rentrante
c + on liminte le cos de l'angle max entre citere etloi d'ecoulement a 0.5 
c dans le repere I1 J=sqrt(J2) ng=normale a  G (alpha,1)
c normal a f nf=(B0,DFDJ)
c cos= ng.nf/||nf||||ng||
            DFDJ = TWO*B1*AJJ
cc            NORMS=SQRT((ALPHA**2+ONE)*(B0**2+DFDJ**2))
            NORMS=SQRT((B0**2+DFDJ**2))
            IF (B0 >=ZERO) THEN
              ALPHA = MAX(ALPHA,  (EM01*NORMS - DFDJ)/MAX(EM20,B0))
            ELSE
              ALPHA = MIN(ALPHA,  (EM01*NORMS - DFDJ)/MIN(B0,-EM20))
            ENDIF
c           Dilatation max atteinte
            IF (RHO(I) < RHO0  .OR. EINT(I)<0) ALPHA=ZERO
c           Ecoulement non associe
            DGS1=FAC*ALPHA+S1(I)/(TWO*AJJ)
            DGS2=FAC*ALPHA+S2(I)/(TWO*AJJ)
            DGS3=FAC*ALPHA+S3(I)/(TWO*AJJ)       
            DGS4=S4(I)/AJJ
            DGS5=S5(I)/AJJ
            DGS6=S6(I)/AJJ
c           Transition et associe
            DGS1=FAC*DGS1+FAC1*DFS1
            DGS2=FAC*DGS2+FAC1*DFS2
            DGS3=FAC*DGS3+FAC1*DFS3
            DGS4=FAC*DGS4+FAC1*DFS4
            DGS5=FAC*DGS5+FAC1*DFS5
            DGS6=FAC*DGS6+FAC1*DFS6
c compactance dilatance effective (!= alpha sur la cape)
c correction erreur           ALPHA = DGS1+DGS2+DGS3
            ALPHA = THIRD*(DGS1+DGS2+DGS3)
C ECROUISSAGE
c contribution deviatorique
            IF (SM(I) < ROK) THEN
              TO = SQR3_2*VK0(I)*RF         
            ELSE
              TO = SQR3_2*VK(I)*RF
            ENDIF
            PHI  = MAX(ZERO,ALPHA*THREE*SM(I)+AJJ)/TO
            DFDTO=-SQRT(TWO_THIRD)+B0
            ECR  = PHI*DFDTO*HBP*FAC
c           ECR=  TEN*PHI*DFDTO*HBP*FAC*(ONE-VK0(I))/(ONE-VKY) ! ecrousissage progressif
c            IF (VK0(I)==ONE ) ECR=ZERO
c Contribution volumetrique 
            HPV =HV0*(ONE-HVFAC*VK(I))*MAX(ONE,EXP(EXPO*EPSVP(I)))
            IF (SM(I) >= ROK) THEN
              IF(ALPHA>=ZERO) HPV=ZERO
            ELSE
              IF (ALPHA>=ZERO) THEN ! si dilatant on ne bouge pas la cape
                HPV = ZERO
              ELSE
                ECR = ECR + RF * DKDSM*HPV*ALPHA
              ENDIF
            ENDIF
c plasticite parfaite sur le critere de rupture
c            ECR=ECR*(HALF-SIGN(HALF,VK(I)-ONE)) 
c            write(993,"(A,3E12.3)")'vkvk0  ',VK(I),VK0(I),SQRT(R2/RF2)
c            write(993,"(A,3E12.3)")'SMrobrk',sm(I),ROB(I),ROK
            IF(R2>=RF2 .OR.VK(I)>=ONE )THEN
              HPV=ZERO
              ECR=ZERO
            ENDIF
C PRODUITS TENSORIELS
C *scal car si la fissure est ouverte pas de plasticite dans cette direction
C les termes croises sont deja nul dans cdam 
            DFS1=DFS1*SCAL1(I)
            DFS2=DFS2*SCAL2(I)
            DFS3=DFS3*SCAL3(I)
            DFS4=DFS4*DE4(I)
            DFS5=DFS5*DE5(I)
            DFS6=DFS6*DE6(I)
            DGS1=DGS1*SCAL1(I)
            DGS2=DGS2*SCAL2(I)
            DGS3=DGS3*SCAL3(I)
            DGS4=DGS4*DE4(I)
            DGS5=DGS5*DE5(I)
            DGS6=DGS6*DE6(I)
C
            H1A = CDAM(I,1,1)*DFS1 + CDAM(I,2,1)*DFS2 + CDAM(I,3,1)*DFS3
            H2A = CDAM(I,1,2)*DFS1 + CDAM(I,2,2)*DFS2 + CDAM(I,3,2)*DFS3
            H3A = CDAM(I,1,3)*DFS1 + CDAM(I,2,3)*DFS2 + CDAM(I,3,3)*DFS3
            H4A = C44(I)*DFS4
            H5A = C55(I)*DFS5
            H6A = C66(I)*DFS6
C
            H1N = CDAM(I,1,1)*DGS1 + CDAM(I,1,2)*DGS2 + CDAM(I,1,3)*DGS3
            H2N = CDAM(I,2,1)*DGS1 + CDAM(I,2,2)*DGS2 + CDAM(I,2,3)*DGS3
            H3N = CDAM(I,3,1)*DGS1 + CDAM(I,3,2)*DGS2 + CDAM(I,3,3)*DGS3
            H4N = C44(I)*DGS4
            H5N = C55(I)*DGS5
            H6N = C66(I)*DGS6
C LAMBDA
            HH= DFS1*H1N+DFS2*H2N+DFS3*H3N+DFS4*H4N+DFS5*H5N+DFS6*H6N
     .         - MIN(ZERO,ECR)
C
            LAMBDA= H1A*DEPS1(I)+H2A*DEPS2(I)+H3A*DEPS3(I)
     .             +H4A*DEPS4(I)+H5A*DEPS5(I)+H6A*DEPS6(I)  
            LAMBDA=LAMBDA/HH
!---
! avoid negative plastic strain
            LAMBDA=MAX(LAMBDA,ZERO)
!---
C DEFORMATIONS PLASTIQUES
            DEPSP1=LAMBDA*DGS1
            DEPSP2=LAMBDA*DGS2
            DEPSP3=LAMBDA*DGS3
            DEPSP4=LAMBDA*DGS4
            DEPSP5=LAMBDA*DGS5
            DEPSP6=LAMBDA*DGS6
            DEPSVP=DEPSP1+DEPSP2+DEPSP3
            EPSVP(I)=EPSVP(I)+MIN(ZERO,DEPSVP) ! on cumule seulement la compaction
c
C DEFORMATIONS ELASTIQUES
            DEPSEL1=DEPS1(I)-DEPSP1
            DEPSEL2=DEPS2(I)-DEPSP2
            DEPSEL3=DEPS3(I)-DEPSP3
            DEPSEL4=DEPS4(I)-DEPSP4
            DEPSEL5=DEPS5(I)-DEPSP5
            DEPSEL6=DEPS6(I)-DEPSP6

c           SIG(I,1)=SIG(I,1)+CDAM(I,1,1)*DEPSEL1+CDAM(I,1,2)*DEPSEL2
c    .                   +CDAM(I,1,3)*DEPSEL3
c           SIG(I,2)=SIG(I,2)+CDAM(I,2,1)*DEPSEL1+CDAM(I,2,2)*DEPSEL2
c    .                   +CDAM(I,2,3)*DEPSEL3
c           SIG(I,3)=SIG(I,3)+CDAM(I,3,1)*DEPSEL1+CDAM(I,3,2)*DEPSEL2
c    .                   +CDAM(I,3,3)*DEPSEL3
c           SIG(I,4)=SIG(I,4)+C44(I)*DEPSEL4
c           SIG(I,5)=SIG(I,5)+C55(I)*DEPSEL5
c           SIG(I,6)=SIG(I,6)+C66(I)*DEPSEL6
C
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C RECALCUL DES DEFORMATION ELASTIQUES POUR REOUVERTURE DES FISSURES
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c           C11 = ONE/DE1(I)/YOUNG
c           C12 = -NU*SCAL1(I)*SCAL2(I)/YOUNG
c           C13 = -NU*SCAL1(I)*SCAL3(I)/YOUNG
c           CRAK(I,1) = C11*SIG(I,1)+C12*SIG(I,2)+C13*SIG(I,3)
c           C22 = ONE/DE2(I)/YOUNG
c           C23 = -NU*SCAL2(I)*SCAL3(I)/YOUNG
c           CRAK(I,2) = C12*SIG(I,1)+C22*SIG(I,2)+C23*SIG(I,3)
c           C33 = ONE/DE3(I)/YOUNG
c           CRAK(I,3) = C13*SIG(I,1)+C23*SIG(I,2)+C33*SIG(I,3)
C
            CRAK(I,1) =CRAK(I,1)+DEPSEL1
            CRAK(I,2) =CRAK(I,2)+DEPSEL2
            CRAK(I,3) =CRAK(I,3)+DEPSEL3
c
            DE1(I)=ONE - MAX( ZERO , SIGN(DAM(I,1),CRAK(I,1)) )
            DE2(I)=ONE - MAX( ZERO , SIGN(DAM(I,2),CRAK(I,2)) )
            DE3(I)=ONE - MAX( ZERO , SIGN(DAM(I,3),CRAK(I,3)) )
            SCAL1(I)=HALF+SIGN(HALF,DE1(I)-ONE)
            SCAL2(I)=HALF+SIGN(HALF,DE2(I)-ONE)
            SCAL3(I)=HALF+SIGN(HALF,DE3(I)-ONE)      
            DE4(I)=SCAL1(I)*SCAL2(I)
            DE5(I)=SCAL2(I)*SCAL3(I)
            DE6(I)=SCAL3(I)*SCAL1(I)
c--------------------------
            DEN = ONE - NU**2 *(DE4(I) + DE5(I) + DE6(I)
     .           + TWO*NU*SCAL1(I)*SCAL2(I)*SCAL3(I))
C
            CDAM(I,1,1) = YOUNG*DE1(I)*(ONE-NU**2*SCAL2(I)*SCAL3(I))/DEN
            CDAM(I,2,2) = YOUNG*DE2(I)*(ONE -NU**2*SCAL1(I)*SCAL3(I))/DEN
            CDAM(I,3,3) = YOUNG*DE3(I)*(ONE -NU**2*SCAL2(I)*SCAL1(I))/DEN
            CDAM(I,1,2) = NU*YOUNG*SCAL1(I)*SCAL2(I) *(ONE+NU*SCAL3(I))/DEN
            CDAM(I,2,3) = NU*YOUNG*SCAL2(I)*SCAL3(I) *(ONE+NU*SCAL1(I))/DEN
            CDAM(I,1,3) = NU*YOUNG*SCAL1(I)*SCAL3(I) *(ONE+NU*SCAL2(I))/DEN
            CDAM(I,2,1) = CDAM(I,1,2)
            CDAM(I,3,1) = CDAM(I,1,3)
            CDAM(I,3,2) = CDAM(I,2,3)
            SIG(I,1) = CDAM(I,1,1)*CRAK(I,1)+CDAM(I,1,2)*CRAK(I,2)+CDAM(I,1,3)*CRAK(I,3)
            SIG(I,2) = CDAM(I,2,1)*CRAK(I,1)+CDAM(I,2,2)*CRAK(I,2)+CDAM(I,2,3)*CRAK(I,3)
            SIG(I,3) = CDAM(I,3,1)*CRAK(I,1)+CDAM(I,3,2)*CRAK(I,2)+CDAM(I,3,3)*CRAK(I,3)
            SIG(I,4) = (SIG(I,4)+C44(I)*DEPSEL4)*DE4(I)
            SIG(I,5) = (SIG(I,5)+C55(I)*DEPSEL5)*DE5(I)
            SIG(I,6) = (SIG(I,6)+C66(I)*DEPSEL6)*DE6(I)
c            SIG(I,4) = SIG(I,4)*DE4(I)
c            SIG(I,5) = SIG(I,5)*DE5(I)
c            SIG(I,6) = SIG(I,6)*DE6(I)
c------------------------------------------------
            S1(I) = SCAL1(I)*SIG(I,1)
            S2(I) = SCAL2(I)*SIG(I,2)
            S3(I) = SCAL3(I)*SIG(I,3)
            S4(I) = SIG(I,4)*SCAL1(I)*SCAL2(I)
            S5(I) = SIG(I,5)*SCAL2(I)*SCAL3(I)
            S6(I) = SIG(I,6)*SCAL3(I)*SCAL1(I)
            SM(I) = THIRD * (S1(I)+S2(I)+S3(I))
C
            S1(I) = S1(I)-SM(I)
            S2(I) = S2(I)-SM(I)
            S3(I) = S3(I)-SM(I)
C . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C CALCUL DES PARAMETRES D' ECROUISSAGE COURANTS VK0,ROB
C CORRECTIONS EVENTUELLES
C . . . . . . . . . . . . . . . . . . . . . . . . . . . .
            IF (SM(I) > AC/AA)
     .        SM(I)=SM(I)-THREE*(SM(I)-AC/AA)/(SCAL1(I)+SCAL2(I)+SCAL3(I))
C
            R2  = S1(I)**2 + S2(I)**2 + S3(I)**2
     .          +(S4(I)**2 + S5(I)**2 + S6(I)**2)*TWO
            RR  = SQRT(R2)
            AJ3 = S1(I)*S2(I)*S3(I)
     .          - S1(I)*S5(I)*S5(I) - S2(I)*S6(I)*S6(I) - S3(I)*S4(I)*S4(I)
     .          + TWO*S4(I)*S5(I)*S6(I)
            CS3T= HALF * AJ3*(THREE/HALF*MAX(R2,EM20))**THREE_HALF
            CS3T= MIN(ONE,CS3T)
            CS3T= MAX(-ONE,CS3T)
            BB  = HALF*((ONE-CS3T)*BC + (ONE+CS3T)*BT)
            DF  = SQRT(BB*BB + MAX(-AA*SM(I)+AC, ZERO))
            RF  = (-BB + DF) / AA
c
            IF (RF == ZERO) THEN
              VK(I) = ZERO
            ELSE
              VK(I) = RR / RF
            ENDIF
C
c           on reprojette si en dehors du critere
            IF (VK(I) > ONE) THEN
              FAC = ONE/VK(I)
              IF (SCAL1(I) > ZEP9) SIG(I,1) =  S1(I)*FAC+SM(I)
              IF (SCAL2(I) > ZEP9) SIG(I,2) =  S2(I)*FAC+SM(I)
              IF (SCAL3(I) > ZEP9) SIG(I,3) =  S3(I)*FAC+SM(I)
              IF (SCAL1(I)*SCAL2(I) > ZEP9) SIG(I,4) =  S4(I)*FAC
              IF (SCAL2(I)*SCAL3(I) > ZEP9) SIG(I,5) =  S5(I)*FAC
              IF (SCAL3(I)*SCAL1(I) > ZEP9) SIG(I,6) =  S6(I)*FAC      
              VK(I) = ONE
              C11 = ONE/DE1(I)/YOUNG
              C12 = -NU*SCAL1(I)*SCAL2(I)/YOUNG
              C13 = -NU*SCAL1(I)*SCAL3(I)/YOUNG
              CRAK(I,1) = C11*SIG(I,1)+C12*SIG(I,2)+C13*SIG(I,3)
              C22 = ONE/DE2(I)/YOUNG
              C23 = -NU*SCAL2(I)*SCAL3(I)/YOUNG
              CRAK(I,2) = C12*SIG(I,1)+C22*SIG(I,2)+C23*SIG(I,3)
              C33 = ONE/DE3(I)/YOUNG
              CRAK(I,3) = C13*SIG(I,1)+C23*SIG(I,2)+C33*SIG(I,3)
C
              DE1(I)=ONE - MAX( ZERO , SIGN(DAM(I,1),CRAK(I,1)) )
              DE2(I)=ONE - MAX( ZERO , SIGN(DAM(I,2),CRAK(I,2)) )
              DE3(I)=ONE - MAX( ZERO , SIGN(DAM(I,3),CRAK(I,3)) )
              SCAL1(I)=HALF + SIGN(HALF,DE1(I)-ONE)
              SCAL2(I)=HALF + SIGN(HALF,DE2(I)-ONE)
              SCAL3(I)=HALF + SIGN(HALF,DE3(I)-ONE)        
              DE4(I)=SCAL1(I)*SCAL2(I)
              DE5(I)=SCAL2(I)*SCAL3(I)
              DE6(I)=SCAL3(I)*SCAL1(I)
c---------------------------------
              DEN = ONE - NU**2 *(DE4(I) + DE5(I) + DE6(I)
     .            + TWO*NU*SCAL1(I)*SCAL2(I)*SCAL3(I))
C
              CDAM(I,1,1) = YOUNG*DE1(I)*(ONE -NU**2*SCAL2(I)*SCAL3(I))/DEN
              CDAM(I,2,2) = YOUNG*DE2(I)*(ONE -NU**2*SCAL1(I)*SCAL3(I))/DEN
              CDAM(I,3,3) = YOUNG*DE3(I)*(ONE -NU**2*SCAL2(I)*SCAL1(I))/DEN
              CDAM(I,1,2) = NU*YOUNG*SCAL1(I)*SCAL2(I) *(ONE +NU*SCAL3(I))/DEN
              CDAM(I,2,3) = NU*YOUNG*SCAL2(I)*SCAL3(I) *(ONE +NU*SCAL1(I))/DEN
              CDAM(I,1,3) = NU*YOUNG*SCAL1(I)*SCAL3(I) *(ONE +NU*SCAL2(I))/DEN
              CDAM(I,2,1) = CDAM(I,1,2)
              CDAM(I,3,1) = CDAM(I,1,3)
              CDAM(I,3,2) = CDAM(I,2,3)
              SIG(I,1)=CDAM(I,1,1)*CRAK(I,1)+CDAM(I,1,2)*CRAK(I,2)+CDAM(I,1,3)*CRAK(I,3)
              SIG(I,2)=CDAM(I,2,1)*CRAK(I,1)+CDAM(I,2,2)*CRAK(I,2)+CDAM(I,2,3)*CRAK(I,3)
              SIG(I,3)=CDAM(I,3,1)*CRAK(I,1)+CDAM(I,3,2)*CRAK(I,2)+CDAM(I,3,3)*CRAK(I,3)
              SIG(I,4)=DE4(I)*SIG(I,4)
              SIG(I,5)=DE5(I)*SIG(I,5)
              SIG(I,6)=DE6(I)*SIG(I,6)
            ENDIF
C
            ROB(I) = MIN(ROB(I), ROB(I)+HPV*DEPSVP, SM(I))
            ROK = ROB(I) - RO0 + ROK0
 
            IF (SM(I) >= RT-TOL)THEN
              VKK=ONE
            ELSEIF(SM(I) > RC) THEN
              DIV= MIN(-EM10,RCT1- TWO*RC*SM(I)+SM(I)*SM(I) )
              VKK=ONE +(ONE - VK(I))*RCT2/DIV
            ELSEIF(SM(I)>=ROK) THEN
              VKK=VK(I)
            ELSE
c             si au dela de la cape, on change rob
              VKMAX=ONE- ((SM(I)-ROK)/(ROB(I)-ROK))**2
              IF (VK(I)>VKMAX) THEN
                ROK = SM(I) - SQRT(ONE-VK(I))*(RO0-ROK0)
                ROB(I) = ROK+RO0-ROK0
                VKK = ONE
              ELSE
c               sinon on calcule k0          
                VKK = VK(I)/MAX(VKMAX,EM03)
              ENDIF
            ENDIF
            VKK   = MIN(VKK,ONE)
            VK0(I)= MAX(VKK,VK0(I))
c            write(7,*)'normal'         
c
          ELSE
c           PURE TRIAXIAL TO SOLVE UNDETERMINATION
            DEPSV = (DEPS1(I) + DEPS2(I) + DEPS3(I))
            D1 = DEPS1(I) - DEPSV*THIRD
            D2 = DEPS2(I) - DEPSV*THIRD
            D3 = DEPS3(I) - DEPSV*THIRD
            HPV = HV0*MAX(ONE, EXP(EXPO*EPSVP(I)))
            DEPSVP  = BULK/(BULK+HPV)*DEPSV
            EPSVP(I)= EPSVP(I) + DEPSVP
            RO = MIN( ROB(I),ROB(I) + HPV*DEPSVP )
            DP = RO - ROB(I)
            SIG(I,1) = SIG(I,1) + DP + TWO*G*D1
	           SIG(I,2) = SIG(I,2) + DP + TWO*G*D2
            SIG(I,3) = SIG(I,3) + DP + TWO*G*D3
 	          SIG(I,4) = SIG(I,4) + G*DEPS4(I)
	           SIG(I,5) = SIG(I,5) + G*DEPS5(I)
            SIG(I,6) = SIG(I,6) + G*DEPS6(I)
            ROB(I) = RO
            VK0(I) = ONE  
c            write(7,*)'pure triaxial'         
          ENDIF
c         equivalent plastic strain for output     
          R2  = (S1(I)**2 + S2(I)**2 + S3(I)**2
     .        + (S4(I)**2 + S5(I)**2 + S6(I)**2)*TWO)*THREE_HALF
          SIGM = SIG(I,1) + SIG(I,2) + SIG(I,2)
          RR = ONE/SQR3
          IF (R2 > ZERO) RR = RR + ALPHA*SIGM/SQRT(R2)
!!          PLA(I) = PLA(I) + LAMBDA * RR
          PLA(I,1) = PLA(I,1) + LAMBDA * RR ! --- plastic strain --- scalar
!---
! full plastic strain tensor (for output only)
!---
            PLA(I,2) = PLA(I,2) + LAMBDA*DGS1
            PLA(I,3) = PLA(I,3) + LAMBDA*DGS2
            PLA(I,4) = PLA(I,4) + LAMBDA*DGS3
            PLA(I,5) = PLA(I,5) + LAMBDA*DGS4
            PLA(I,6) = PLA(I,6) + LAMBDA*DGS5
            PLA(I,7) = PLA(I,7) + LAMBDA*DGS6
c-----------
c         FIN DES ITERATIONS
        ENDDO ! ITER
c-----------
      ENDDO ! NINDX
c-----------
      RETURN
      END
