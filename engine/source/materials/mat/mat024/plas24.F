Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  PLAS24                        source/materials/mat/mat024/plas24.F
Chd|-- called by -----------
Chd|        CONC24                        source/materials/mat/mat024/conc24.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PLAS24(NEL   ,NINDX ,INDX  ,NGL   ,PM   ,
     .                  SIG   ,DAM   ,CRAK  ,
     .                  RHO   ,EINT  ,VK0   ,VK   ,ROB  ,CDAM ,
     .                  E1    ,E2    ,E3    ,E4   ,E5   ,E6   ,
     .                  S1    ,S2    ,S3    ,S4   ,S5   ,S6   ,
     .                  SCAL1 ,SCAL2 ,SCAL3 ,SCLE2 )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NINDX,NEL,NGL(NEL),INDX(NINDX)
      my_real PM(NPROPM)
      my_real, DIMENSION(NEL) :: S1,S2,S3,S4,S5,S6,
     .   SCAL1,SCAL2,SCAL3,SCLE2,E1,E2,E3,E4,E5,E6,EINT,RHO,VK0,VK,ROB
      my_real, DIMENSION(NEL,3,3) :: CDAM
      my_real, DIMENSION(NEL,6)   :: SIG
      my_real, DIMENSION(NEL,3)   :: DAM,CRAK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NITER,ITER,ICAP,IBUG
      my_real, DIMENSION(NEL) :: SM,DE1,DE2,DE3,DE4,DE5,DE6,C44,C55,C66
      my_real
     .   FC, RT, RC, RCT1, RCT2, AA, BC,
     .   BT, AC, HBP, ALI0, ALF0, VKY, TOL, ROK0, RO0, HV0, VMAX, EXPO,
     .   YOUNG, NU, G, DEN,  RATE, FAC, ROK, R2, AJ3, CS3T,
     .   BB, DF, RF, RF2, AJ2, AJJ, DKDSM, DRFDSM, B0, DRF3, B1,
     .   B2, ALPHA, PHI, DFDTO1, DFDTO2, TO, SQ32, DFDTO, HPV, HP, ECR,
     .   TS1, TS2, TS3, TS4, TS5, TS6, DFS1, DFS2, DFS3, DFS4, DFS5,
     .   DFS6, DGS1, DGS2, DGS3, DGS4, DGS5, DGS6, H1A, H2A, H3A, H4A,
     .   H5A, H6A, H1N, H2N, H3N, H4N, H5N, H6N, HH, CP11, CP12, CP13,
     .   CP14, CP15, CP16, CP21, CP22, CP23, CP24, CP25, CP26, CP31,
     .   CP32, CP33, CP34, CP35, CP36, CP41, CP42, CP43, CP44, CP45,
     .   CP46, CP51, CP52, CP53, CP54, CP55, CP56, CP61, CP62, CP63,
     .   CP64, CP65, CP66, C11, C12, C13, C22, C23, C33, VKOLD, RR,
     .   VKMAX, RO, DIV, DVK, VKK, NUMER, DENOM,BULK,DP,RHO0,DFDRO     
C=======================================================================
      RHO0  = PM(1)
      YOUNG = PM(20)
      NU    = PM(21)
      G     = PM(22)
      VMAX  = PM(27)
      ROK0  = PM(29)
      RO0   = PM(30)
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
      HV0   = PM(48)
      EXPO  = PM(49)
      ICAP  = NINT(PM(57))
      IBUG  = NINT(PM(59))
c
      TOL   = (RT - RC)/TWENTY
c------------------------------------------------
C  RETOUR SUR LE CRITERE ,SIG ,CDAM,CRAK,...
c------------------------------------------------
      DO J = 1,NINDX
        I = INDX(J)
        CRAK(I,1) = CRAK(I,1)-SCLE2(I)*E1(I)
        CRAK(I,2) = CRAK(I,2)-SCLE2(I)*E2(I)
        CRAK(I,3) = CRAK(I,3)-SCLE2(I)*E3(I)
        DE1(I)  = ONE - MAX( ZERO , SIGN(DAM(I,1),CRAK(I,1)) )
        DE2(I)  = ONE - MAX( ZERO , SIGN(DAM(I,2),CRAK(I,2)) )
        DE3(I)  = ONE - MAX( ZERO , SIGN(DAM(I,3),CRAK(I,3)) )
        SCAL1(I)= HALF+SIGN(HALF,DE1(I)-ONE)
        SCAL2(I)= HALF+SIGN(HALF,DE2(I)-ONE)
        SCAL3(I)= HALF+SIGN(HALF,DE3(I)-ONE)     
        DE4(I)  = SCAL1(I)*SCAL2(I)
        DE5(I)  = SCAL2(I)*SCAL3(I)
        DE6(I)  = SCAL3(I)*SCAL1(I)
c-------
        DEN = ONE - NU**2 *(DE4(I) + DE5(I) + DE6(I)
     .      + TWO*NU*SCAL1(I)*SCAL2(I)*SCAL3(I))
        CDAM(I,1,1) = YOUNG*DE1(I)*(ONE -NU**2*SCAL2(I)*SCAL3(I))/DEN
        CDAM(I,2,2) = YOUNG*DE2(I)*(ONE -NU**2*SCAL1(I)*SCAL3(I))/DEN
        CDAM(I,3,3) = YOUNG*DE3(I)*(ONE -NU**2*SCAL2(I)*SCAL1(I))/DEN
        CDAM(I,1,2) = NU*YOUNG*SCAL1(I)*SCAL2(I) *(ONE+NU*SCAL3(I))/DEN
        CDAM(I,2,3) = NU*YOUNG*SCAL2(I)*SCAL3(I) *(ONE+NU*SCAL1(I))/DEN
        CDAM(I,1,3) = NU*YOUNG*SCAL1(I)*SCAL3(I) *(ONE+NU*SCAL2(I))/DEN
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
        IF (IBUG == 0) THEN 
          SIG(I,4) = DE4(I)*SIG(I,4) - C44(I)*E4(I)*(ONE-SCLE2(I))
          SIG(I,5) = DE5(I)*SIG(I,5) - C55(I)*E5(I)*(ONE-SCLE2(I))
          SIG(I,6) = DE6(I)*SIG(I,6) - C66(I)*E6(I)*(ONE-SCLE2(I))
        ELSE
          SIG(I,4) = DE4(I)*SIG(I,4) + C44(I)*E4(I)*SCLE2(I)
          SIG(I,5) = DE5(I)*SIG(I,5) + C55(I)*E5(I)*SCLE2(I)
          SIG(I,6) = DE6(I)*SIG(I,6) + C66(I)*E6(I)*SCLE2(I)
        ENDIF
        S1(I) = SCAL1(I)*SIG(I,1)
        S2(I) = SCAL2(I)*SIG(I,2)
        S3(I) = SCAL3(I)*SIG(I,3)
        S4(I) = SIG(I,4)*SCAL1(I)*SCAL2(I)
        S5(I) = SIG(I,5)*SCAL2(I)*SCAL3(I)
        S6(I) = SIG(I,6)*SCAL3(I)*SCAL1(I)
        SM(I) = THIRD * (S1(I)+S2(I)+S3(I))
        S1(I) = S1(I) - SM(I)
        S2(I) = S2(I) - SM(I)
        S3(I) = S3(I) - SM(I)
C
        NUMER = ABS(E1(I))+ABS(E2(I))+ABS(E3(I))
     .        + ABS(E4(I))+ABS(E5(I))+ABS(E6(I))
c
        DENOM = (ABS(CRAK(I,1))+ ABS(CRAK(I,2)) + ABS(CRAK(I,3))
     .        + (ABS(SIG(I,4)) + ABS(SIG(I,5))  + ABS(SIG(I,6)))/G )
c------------------------------------------------
        IF (DENOM == ZERO) CYCLE
c------------------------------------------------
        RATE  = NUMER/DENOM
        NITER = NINT(THREE*RATE) + 1
        NITER = MIN0(NITER,10)
c------------------------------------------------
        FAC   = SCLE2(I)/NITER
        E1(I) = FAC * E1(I)
        E2(I) = FAC * E2(I)
        E3(I) = FAC * E3(I)
        E4(I) = FAC * E4(I)
        E5(I) = FAC * E5(I)
        E6(I) = FAC * E6(I)
c---------------------------------------------------------
c ...   DEBUT DES ITERATIONS
c---------------------------------------------------------
        DO ITER=1,NITER
c--------------------
          ROK = ROK0+ROB(I)-RO0
          R2  = S1(I)**2+S2(I)**2+S3(I)**2
     .        + TWO*S4(I)**2+TWO*S5(I)**2+TWO*S6(I)**2
          IF (SM(I) >= RT-TOL)THEN
            VK(I)=ONE
          ELSEIF (SM(I) > RC) THEN
            VK(I)=ONE +(ONE-VK0(I))*(RCT1-TWO*RC*SM(I)+SM(I)**2)/RCT2
          ELSEIF (SM(I) >ROK)THEN
            VK(I)=VK0(I)
          ELSEIF (SM(I) > ROB(I))THEN  ! Cape
            VK(I)=VK0(I)*(ONE- ((SM(I)-ROK)/(ROB(I)-ROK))**2)
          ELSE
            VK(I)=ZERO
          ENDIF
c         CRITERE
          IF (IBUG == 0) THEN 
            AJ3 = S1(I)*S2(I)*S3(I)
     .          - S1(I)*S5(I)*S5(I)-S2(I)*S6(I)*S6(I)-S3(I)*S4(I)*S4(I)
     .          + TWO*S4(I)*S5(I)*S6(I)
          ELSE
            AJ3 = S1(I)*S2(I)*S3(I)
     .          - S1(I)*S5(I)*S5(I)-S2(I)*S6(I)*S6(I)-S3(I)*S4(I)*S4(I)
          ENDIF
          CS3T= HALF * AJ3*(THREE/(HALF*MAX(R2,EM20)))**THREE_HALF
          CS3T= MIN(ONE,CS3T)
          CS3T= MAX(-ONE,CS3T)
          BB  = HALF*((ONE - CS3T)*BC+(ONE +CS3T)*BT)
          DF  = SQRT(BB*BB+ MAX(-AA*SM(I)+AC,EM9))
          RF  = (-BB+DF)/AA
          RF2 = RF**2
          AJ2 = HALF*R2
          AJJ = SQRT(AJ2)
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C NOW COMPUTES ALL TERMS TO BUILD UP PLASTIC MATRIX
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C D I M E N S I O N S   RF:CONT       TO:CONT  PHI:ADIM  B0:ADIM  B1:1/CONT
C                       B2:1/CONT**2  DFDTO:ADIM  DRFDSM:ADIM DKDSM:1/CONT
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C PRINCIPALES DERIVEES
          SM(I) = MAX(ROB(I),SM(I))
          IF(SM(I) >= RT-TOL)THEN
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
          IF(AJJ>EM3*FC)THEN
            DRF3  =  HALF* (-ONE + BB/DF) * (BT-BC)/AA
            B1    =  HALF*SQR2/MAX(AJJ,EM20)
     .            + VK(I)*DRF3*FOURTH*AJ3*(THREE/MAX(AJ2,EM20))**TWOP5
            B2    =-VK(I)*DRF3*HALF*(THREE/MAX(AJ2,EM20))**THREE_HALF
          ELSE
            B1=ZERO
            B2=ZERO
          ENDIF
C DF/DSIG
          TS1 = S1(I)**2 + S4(I)**2 + S6(I)**2 - TWO*THIRD*AJ2
          TS2 = S2(I)**2 + S4(I)**2 + S5(I)**2 - TWO*THIRD*AJ2
          TS3 = S3(I)**2 + S5(I)**2 + S6(I)**2 - TWO*THIRD*AJ2
          IF (IBUG == 0) THEN 
            TS4 = TWO* (S5(I)*S6(I)-S4(I)*S3(I))
            TS5 = TWO* (S6(I)*S4(I)-S5(I)*S1(I))
            TS6 = TWO* (S4(I)*S5(I)-S6(I)*S2(I))
          ELSE
            TS4 = -TWO* S4(I) * S3(I)
            TS5 = -TWO* S5(I) * S1(I)
            TS6 = -TWO* S6(I) * S2(I)     
          ENDIF     
          DFS1=B0+B1*S1(I)+B2*TS1
          DFS2=B0+B1*S2(I)+B2*TS2
          DFS3=B0+B1*S3(I)+B2*TS3
          DFS4=TWO*B1*S4(I)+B2*TS4
          DFS5=TWO*B1*S5(I)+B2*TS5
          DFS6=TWO*B1*S6(I)+B2*TS6
C DG/DSIG
C PLASTICITE VOLUMIQUE COMPACTANCE/DILATANCE
          IF (VK(I) > VKY) THEN
            ALPHA = (ONE-VK(I))*ALI0+(VK(I)-VKY)*ALF0
            ALPHA = ALPHA/(ONE-VKY)
          ELSE
            ALPHA = ALI0
          ENDIF
          IF (ICAP == 1) THEN
            IF (B0 < -EM02) ALPHA = MIN(ALPHA,-ZEP4/B0)
            IF (B0 > EM02)  ALPHA = MAX(ALPHA,-ZEP4/B0)
          ENDIF
          IF (EINT(I)<=ZERO) ALPHA=ZERO
c         AJOUT DILATANCE PLASTIQUE MAX fp 09/16 
          IF (RHO(I) < RHO0) ALPHA = ZERO
          IF (AJJ > EM3*FC) THEN                 
            DGS1=ALPHA+S1(I)/(TWO*AJJ)         
            DGS2=ALPHA+S2(I)/(TWO*AJJ)         
            DGS3=ALPHA+S3(I)/(TWO*AJJ)         
            DGS4=S4(I)/AJJ                      
            DGS5=S5(I)/AJJ                      
            DGS6=S6(I)/AJJ                      
          ELSE                                  
            IF (ICAP == 1) THEN                 
              DGS1=ALPHA                        
              DGS2=ALPHA                        
              DGS3=ALPHA                        
            ELSE                                
              DGS1=-ONE                          
              DGS2=-ONE                          
              DGS3=-ONE                          
            ENDIF                               
            DGS4=ZERO                           
            DGS5=ZERO                           
            DGS6=ZERO                           
          ENDIF                                 
c         ECROUISSAGE
          HPV = HV0*EXP( MIN(FIFTY,(ROB(I)-RO0)*EXPO))
          IF(SM(I)>ROK0)THEN
            HP=HBP
          ELSE
            HP=HPV
          ENDIF
c
          IF (ICAP == 1) THEN
            PHI  = (ALPHA*THREE*SM(I)+AJJ)
            DFDTO1=B0-SQRT(TWO_THIRD)
            DFDTO2=THREE*B0
            IF(DFDTO1<=DFDTO2)THEN
              TO=SQR3_2*VK(I)*RF
              DFDTO=DFDTO1
            ELSE
              TO=ABS(SM(I))
              DFDTO=DFDTO2
            ENDIF
            ECR=PHI*HP*DFDTO/TO*(HALF-SIGN(HALF,VK(I)-ONE))
          ELSE
            DFDTO1=B0-SQRT(TWO_THIRD)
            DFDTO2=THREE*B0
C            IF(DFDTO2>=ZERO)THEN
            IF(DFDTO1<=DFDTO2)THEN
              TO=SQR3_2*VK(I)*RF
              PHI  = (ALPHA*THREE*SM(I)+AJJ)/TO
              DFDTO=DFDTO1
              ECR=PHI*HP*DFDTO*(HALF-SIGN(HALF,VK(I)-ONE))
c              write(IOUT,*)'BASE',SM(I),ROK,VK(I),DFDTO1,DFDTO2,ECR
            ELSE
              DFDRO=-2*VK0(I)*RF*(SM(I)-ROK)/(RO0-ROK0)**2
              ECR=DFDTO2*DFDRO*HPV
c              write(IOUT,*)'CAP',SM(I),ROK,VK(I),DFDTO1,DFDTO2,DFDRO,ECR
              DGS1=DFS1
              DGS2=DFS2
              DGS3=DFS3
              DGS4=DFS4
              DGS5=DFS5
              DGS6=DFS6
            ENDIF
          ENDIF
c         PRODUITS TENSORIELS
          IF (ICAP == 1 .OR. (ICAP == 0 .AND. VK(I) > EM05))THEN
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
c           DENOMINATEUR
            HH= DFS1*H1N+DFS2*H2N+DFS3*H3N+DFS4*H4N+DFS5*H5N+DFS6*H6N
     .         - MIN(ZERO,ECR)
            HH = SIGN(MAX(ABS(HH),EM20),HH)
c            WRITE(IOUT,*)'HH',HH,ECR
C
            CP11=-H1N*H1A/HH*SCAL1(I)
            CP12=-H1N*H2A/HH*SCAL1(I)*SCAL2(I)
            CP13=-H1N*H3A/HH*SCAL1(I)*SCAL3(I)
            CP14=-H1N*H4A/HH*SCAL1(I)*SCAL2(I)
            CP15=-H1N*H5A/HH*SCAL1(I)*SCAL2(I)*SCAL3(I)
            CP16=-H1N*H6A/HH*SCAL1(I)*SCAL3(I)
C
            CP21=-H2N*H1A/HH*SCAL1(I)*SCAL2(I)
            CP22=-H2N*H2A/HH*SCAL2(I)
            CP23=-H2N*H3A/HH*SCAL3(I)*SCAL2(I)
            CP24=-H2N*H4A/HH*SCAL2(I)*SCAL1(I)
            CP25=-H2N*H5A/HH*SCAL2(I)*SCAL3(I)
            CP26=-H2N*H6A/HH*SCAL2(I)*SCAL1(I)*SCAL3(I)
C
            CP31=-H3N*H1A/HH*SCAL3(I)*SCAL1(I)
            CP32=-H3N*H2A/HH*SCAL3(I)*SCAL2(I)
            CP33=-H3N*H3A/HH*SCAL3(I)
            CP34=-H3N*H4A/HH*SCAL3(I)*SCAL1(I)*SCAL2(I)
            CP35=-H3N*H5A/HH*SCAL3(I)*SCAL2(I)
            CP36=-H3N*H6A/HH*SCAL3(I)*SCAL1(I)
C
            CP41=-H4N*H1A/HH*SCAL1(I)
            CP42=-H4N*H2A/HH*SCAL2(I)
            CP43=-H4N*H3A/HH*SCAL3(I)
            CP44=-H4N*H4A/HH*SCAL1(I)*SCAL2(I)
            CP45=-H4N*H5A/HH*SCAL1(I)*SCAL2(I)*SCAL3(I)
            CP46=-H4N*H6A/HH*SCAL1(I)*SCAL2(I)*SCAL3(I)
C
            CP51=-H5N*H1A/HH*SCAL1(I)
            CP52=-H5N*H2A/HH*SCAL2(I)
            CP53=-H5N*H3A/HH*SCAL3(I)
            CP54=-H5N*H4A/HH*SCAL1(I)*SCAL2(I)*SCAL3(I)
            CP55=-H5N*H5A/HH*SCAL2(I)*SCAL3(I)
            CP56=-H5N*H6A/HH*SCAL1(I)*SCAL2(I)*SCAL3(I)
C
            CP61=-H6N*H1A/HH*SCAL1(I)
            CP62=-H6N*H2A/HH*SCAL2(I)
            CP63=-H6N*H3A/HH*SCAL3(I)
            CP64=-H6N*H4A/HH*SCAL1(I)*SCAL2(I)*SCAL3(I)
            CP65=-H6N*H5A/HH*SCAL1(I)*SCAL2(I)*SCAL3(I)
            CP66=-H6N*H6A/HH*SCAL1(I)*SCAL3(I)
C . .       . . . . . . . . . . . . . . . . . . . . . . . . . .
C            NEW STRESS
C            DSIGI=SCLE2*(CIJ+CPIJ)EJ
C . .       . . . . . . . . . . . . . . . . . . . . . . . . . .
            CP11=CDAM(I,1,1)+CP11
            CP12=CDAM(I,1,2)+CP12
            CP13=CDAM(I,1,3)+CP13
            CP21=CDAM(I,2,1)+CP21
            CP22=CDAM(I,2,2)+CP22
            CP23=CDAM(I,2,3)+CP23
            CP31=CDAM(I,3,1)+CP31
            CP32=CDAM(I,3,2)+CP32
            CP33=CDAM(I,3,3)+CP33
            CP44=C44(I)+CP44
            CP55=C55(I)+CP55
            CP66=C66(I)+CP66
            SIG(I,1)=SIG(I,1)+CP11*E1(I)+CP12*E2(I)+CP13*E3(I)
     .                   +CP14*E4(I)+CP15*E5(I)+CP16*E6(I)
            SIG(I,2)=SIG(I,2)+CP21*E1(I)+CP22*E2(I)+CP23*E3(I)
     .                   +CP24*E4(I)+CP25*E5(I)+CP26*E6(I)
            SIG(I,3)=SIG(I,3)+CP31*E1(I)+CP32*E2(I)+CP33*E3(I)
     .                   +CP34*E4(I)+CP35*E5(I)+CP36*E6(I)
            SIG(I,4)=SIG(I,4)+CP41*E1(I)+CP42*E2(I)+CP43*E3(I)
     .                   +CP44*E4(I)+CP45*E5(I)+CP46*E6(I)
            SIG(I,5)=SIG(I,5)+CP51*E1(I)+CP52*E2(I)+CP53*E3(I)
     .                   +CP54*E4(I)+CP55*E5(I)+CP56*E6(I)
            SIG(I,6)=SIG(I,6)+CP61*E1(I)+CP62*E2(I)+CP63*E3(I)
     .                   +CP64*E4(I)+CP65*E5(I)+CP66*E6(I)
          ELSE
c           PURE TRIAXIAL TO SOLVE UNDETERMINATION
                     DP = BULK*HPV/(BULK+HPV)*(E1(I)+E2(I)+E3(I))
                     SIG(I,1)=SIG(I,1)+DP
                     SIG(I,2)=SIG(I,2)+DP
           SIG(I,3)=SIG(I,3)+DP
          ENDIF
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C RECALCUL DES DEFORMATION ELASTIQUES POUR REOUVERTURE DES FISSURES
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
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
          SCAL1(I)=HALF+SIGN(HALF,DE1(I)-ONE)
          SCAL2(I)=HALF+SIGN(HALF,DE2(I)-ONE)
          SCAL3(I)=HALF+SIGN(HALF,DE3(I)-ONE)      
          DE4(I)=SCAL1(I)*SCAL2(I)
          DE5(I)=SCAL2(I)*SCAL3(I)
          DE6(I)=SCAL3(I)*SCAL1(I)
c--------------------------
          DEN = ONE - NU**2 *(DE4(I) + DE5(I) + DE6(I)
     .        + TWO*NU*SCAL1(I)*SCAL2(I)*SCAL3(I))
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
          SIG(I,4) = SIG(I,4)*DE4(I)
          SIG(I,5) = SIG(I,5)*DE5(I)
          SIG(I,6) = SIG(I,6)*DE6(I)
c--------------------------
          S1(I) = SIG(I,1) * SCAL1(I)
          S2(I) = SIG(I,2) * SCAL2(I)
          S3(I) = SIG(I,3) * SCAL3(I)
          S4(I) = SIG(I,4) * DE4(I)
          S5(I) = SIG(I,5) * DE5(I)
          S6(I) = SIG(I,6) * DE6(I)
          SM(I) = THIRD  * (S1(I)+S2(I)+S3(I))
C
          S1(I)= S1(I)-SM(I)
          S2(I)= S2(I)-SM(I)
          S3(I)= S3(I)-SM(I)
C
C . . . . . . . . . . . . . . . . . . . . . . . . . . . .
C CALCUL DES PARAMETRES D' ECROUISSAGE COURANTS VK0,ROB(I)
C CORRECTIONS EVENTUELLES
C . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          VKOLD=VK(I)
          IF (SM(I)>AC/AA) SM(I) = 
     .        SM(I) -THREE*(SM(I)-AC/AA)/(SCAL1(I)+SCAL2(I)+SCAL3(I))
C
          R2  = S1(I)**2+S2(I)**2+S3(I)**2
     .        + TWO*S4(I)**2+TWO*S5(I)**2+TWO*S6(I)**2
          RR  = SQRT(R2)
          IF (IBUG == 0) THEN 
            AJ3 = S1(I)*S2(I)*S3(I)
     .          - S1(I)*S5(I)*S5(I)-S2(I)*S6(I)*S6(I)-S3(I)*S4(I)*S4(I)
     .          + TWO*S4(I)*S5(I)*S6(I)
          ELSE
            AJ3 = S1(I)*S2(I)*S3(I)
     .          - S1(I)*S5(I)*S5(I)-S2(I)*S6(I)*S6(I)-S3(I)*S4(I)*S4(I)
          ENDIF
          CS3T= HALF * AJ3*(THREE/(HALF*MAX(R2,EM20)))**THREE_HALF
          CS3T=  MIN(ONE,CS3T)
          CS3T=  MAX(-ONE,CS3T)
          BB  = HALF*((ONE-CS3T)*BC+(ONE+CS3T)*BT)
          DF  = SQRT(BB*BB+ MAX(-AA*SM(I)+AC,ZERO))
          RF  = (-BB+DF)/AA
          VK(I) = RR/MAX(RF,EM20)
          VKMAX=ONE
C
c         CORRECTION SI EN DEHORS DU CRITERE DE RUPTURE
          IF (VK(I) > VKMAX)THEN
            FAC = VKMAX/VK(I)
            IF(SCAL1(I) > ZEP9) SIG(I,1) =  S1(I)*FAC+SM(I)
            IF(SCAL2(I) > ZEP9) SIG(I,2) =  S2(I)*FAC+SM(I)
            IF(SCAL3(I) > ZEP9) SIG(I,3) =  S3(I)*FAC+SM(I)
            IF(SCAL1(I)*SCAL2(I) > ZEP9)SIG(I,4) =  S4(I)*FAC
            IF(SCAL2(I)*SCAL3(I) > ZEP9)SIG(I,5) =  S5(I)*FAC
            IF(SCAL3(I)*SCAL1(I) > ZEP9)SIG(I,6) =  S6(I)*FAC        
            VK(I)=VKMAX
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
c-------------------------------
            DEN = ONE - NU**2 *(DE4(I) + DE5(I) + DE6(I)
     .          + TWO*NU*SCAL1(I)*SCAL2(I)*SCAL3(I))
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
          RO=ROB(I)
          IF (SM(I) >= RT-TOL)THEN
            VK(I)=ONE
          ELSEIF(SM(I) > RC) THEN
            DIV= MIN(-EM20,RCT1- TWO*RC*SM(I)+SM(I)*SM(I) )
            VK(I)=ONE +(ONE - VK(I))*RCT2/DIV
           ELSEIF(SM(I) > ROK) THEN
             VK(I)=VK(I)
           ELSE
             DVK=VK(I)-VK0(I)*(ONE -(( MAX(SM(I),ROB(I))-ROK)/(RO0-ROK0))**2)
             VKK=VK0(I)+ MAX(DVK,ZERO)
             VKK= MIN(VKK,ONE)
             RO=SM(I)+(ONE -SQRT(ONE -VK(I)/VKK))*(RO0-ROK0)
          ENDIF
          ROB(I)= MIN(RO,ROB(I))
          VK(I) = MIN(VK(I),ONE)
          VK0(I)= MAX(VK(I),VK0(I))
c-----------
c         FIN DES ITERATIONS
        ENDDO ! ITER
c-----------
      ENDDO ! NINDX
c-----------
      RETURN
      END
