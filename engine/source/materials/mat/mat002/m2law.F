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
Chd|  M2LAW                         source/materials/mat/mat002/m2law.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|        M2ITER_IMP                    source/materials/mat/mat002/m2law.F
Chd|        MDTSPH                        source/materials/mat_share/mdtsph.F
Chd|        MQVISCB                       source/materials/mat_share/mqviscb.F
Chd|====================================================================
      SUBROUTINE M2LAW (PM ,OFF  ,SIG     ,EINT   ,RHO     ,
     2           QOLD   ,EPXE    ,EPSD    ,VOL    ,STIFN   ,
     3           DT2T   ,NELTST  ,ITYPTST ,OFFG   ,GEO     ,
     4           PID    ,AMU     ,VOL_AVG,
     6           MUMAX  ,MAT     ,NGL     ,SSP    ,DVOL    ,
     7           AIRE   ,VNEW    ,VD2     ,DELTAX ,VIS     ,  
     8           D1     ,D2      ,D3      ,D4     ,D5      ,
     9           D6     ,PNEW    ,PSH     ,QNEW   ,SSP_EQ  ,
     A           SOLD1  ,SOLD2   ,SOLD3   ,SOLD4  ,SOLD5   ,
     B           SOLD6  ,IPLA    ,SIGY    ,DEFP   ,DPLA1   ,
     C           EPSP   ,TSTAR   ,ETSE    ,MSSA   ,DMELS   ,
     D           TEMPEL ,SIGBAK  ,AL_IMP  ,SIGNOR ,CONDE   ,
     E           DTEL   ,G_DT    ,NEL     ,IPM    ,RHOREF  ,
     F           RHOSP   )
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
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NELTST,ITYPTST,PID(*),G_DT,NEL
      INTEGER MAT(*),NGL(*),IPLA, IPM(NPROPMI,*)
      my_real
     .   DT2T
C
      my_real
     .   PM(NPROPM,*), OFF(*), SIG(NEL,6), EINT(*), RHO(*), QOLD(*),
     .   EPXE(*), EPSD(*), VOL(*), STIFN(*), OFFG(*),GEO(NPROPG,*),
     .   MUMAX(*),AMU(*),VOL_AVG(*)
      my_real
     .   VNEW(*), VD2(*), DELTAX(*), SSP(*), AIRE(*), VIS(*), 
     .   PSH(*), PNEW(*),QNEW(*) ,SSP_EQ(*), DVOL(*), 
     .   D1(*), D2(*), D3(*), D4(*), D5(*), D6(*), 
     .   SOLD1(MVSIZ), SOLD2(MVSIZ), SOLD3(MVSIZ),
     .   SOLD4(MVSIZ), SOLD5(MVSIZ), SOLD6(MVSIZ),
     .   TSTAR(MVSIZ), DPLA1(MVSIZ), EPSP(MVSIZ)
C
      my_real
     .   SIGY(*) ,DEFP(*),ETSE(*), MSSA(*), DMELS(*), TEMPEL(*),
     .   SIGBAK(NEL,6),AL_IMP(*),SIGNOR(MVSIZ,6),CONDE(*),DTEL(*),
     .   RHOREF(*)  ,RHOSP(*)  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ICC,IRTY,ISRATE,
     .   I, MX , NPIF,IBID,IKFLG
      my_real
     .   RHO0, 
     .   EPD(MVSIZ),
     .   G(MVSIZ), AK(MVSIZ),
     .   PC(MVSIZ), QH(MVSIZ), C1,T(MVSIZ),
     .   AJ2(MVSIZ), DAV(MVSIZ),
     .   P, EPMX, 
     .   CA(MVSIZ), CB(MVSIZ), CC, CN,
     .   EPDR, CMX, 
     .   SIGMX(MVSIZ),Z3,Z4,CP,
     .   ASRATE,
     .   E1, E2, E3, E4, E5,E6, EINC, G2, G1,
     .       SIGEXX(MVSIZ),SIGEYY(MVSIZ),SIGEZZ(MVSIZ),
     .       SIGEXY(MVSIZ),SIGEYZ(MVSIZ),SIGEZX(MVSIZ),
     .    SCALE, BID1, BID2, BID3, DTA,TM,MT,EPIF, DPLA,
     .    DSXX,DSYY,DSZZ,DSXY,DSYZ,DSZX,ALPHA,HKIN,
     .    FACQ0,FISOKIN,BETA,VM,VM_1,G3,G3H,NORM_1,
     .    G_1,CA_1,CB_1,SIGMX_1,T_1
      my_real, DIMENSION(MVSIZ) :: BIDMVSIZ
C-----------------------------------------------
C
      EPIF    = ZERO
      NPIF    = 0
      FACQ0   = UN
C
      MX      = MAT(LFT)
      RHO0    = PM( 1,MX)
      C1      = PM(32,MX)
      CN      = PM(40,MX)
      CC      = PM(43,MX)
      ICC     = NINT(PM(49,MX))
      EPMX    = PM(41,MX)
      EPDR    = PM(44,MX)
      EPIF    = MAX(EPIF,CC)
      IRTY    = NINT(PM(50,MX))
      Z3      = PM(51,MX)
      Z4      = PM(52,MX)
      CP      = PM(53,MX)
      CMX     = PM(45,MX)
      ISRATE  = IPM(3,MX)
      ASRATE  = MIN(UN,PM(9,MX)*DT1)
      FISOKIN = PM(55,MX)
      G_1     = PM(22,MX)
      CA_1    = PM(38,MX)
      CB_1    = PM(39,MX)
      SIGMX_1 = PM(42,MX)
      T_1     = PM(54,MX)
      DO I=LFT,LLT
        G(I)    =G_1*OFF(I)
c     --constants a,b,n of the hardening function
c     --sigma_0 = a + b*(eps_p)**n
        CA(I)   =CA_1
        CB(I)   =CB_1
        SIGMX(I)=SIGMX_1
        NPIF    = NPIF+IRTY
        T(I)   =T_1
C
        ETSE(I) = UN
      ENDDO
C
C------------------------------------------
C     ECROUISSAGE CINE
C------------------------------------------
      IKFLG=0
      DO I=LFT,LLT
         IF (FISOKIN /= ZERO ) THEN
          SIG(I,1)=SIG(I,1)-SIGBAK(I,1)
          SIG(I,2)=SIG(I,2)-SIGBAK(I,2)
          SIG(I,3)=SIG(I,3)-SIGBAK(I,3)
          SIG(I,4)=SIG(I,4)-SIGBAK(I,4)
          SIG(I,5)=SIG(I,5)-SIGBAK(I,5)
          SIG(I,6)=SIG(I,6)-SIGBAK(I,6)
          IKFLG = IKFLG + 1
         ENDIF           
      ENDDO
C      
      DO I=LFT,LLT
       P  =-THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
       DAV(I)=-THIRD*(D1(I)+D2(I)+D3(I))
       G1=DT1*G(I)
       G2=DEUX*G1
       SSP(I)=SQRT((UNP333*G(I)+C1)/RHO0)
C-------------------------------
C     CONTRAINTES DEVIATORIQUES
C-------------------------------
c     -- deviatoric elastic stress predictor
       SIG(I,1)=SIG(I,1)+P+G2*(D1(I)+DAV(I))
       SIG(I,2)=SIG(I,2)+P+G2*(D2(I)+DAV(I))
       SIG(I,3)=SIG(I,3)+P+G2*(D3(I)+DAV(I))
       SIG(I,4)=SIG(I,4)+G1*D4(I)
       SIG(I,5)=SIG(I,5)+G1*D5(I)
       SIG(I,6)=SIG(I,6)+G1*D6(I)
C
c     -- von Mises stress at elastic predictor
       AJ2(I)=UNDEMI*(SIG(I,1)**2+SIG(I,2)**2+SIG(I,3)**2)
     1               +SIG(I,4)**2+SIG(I,5)**2+SIG(I,6)**2
       AJ2(I)=SQRT(TROIS*AJ2(I))
      ENDDO
C
c     --  storing elastic predictors for stiffness computation
      IF (IMPL_S.GT.0.OR.IKFLG.GT.0) THEN
        DO I=LFT,LLT
         SIGEXX(I) = SIG(I,1)
         SIGEYY(I) = SIG(I,2)
         SIGEZZ(I) = SIG(I,3)		    
         SIGEXY(I) = SIG(I,4)
         SIGEYZ(I) = SIG(I,5)
         SIGEZX(I) = SIG(I,6)
        ENDDO
      ENDIF
C-------------
C     STRAIN RATE (JOHNSON-COOK, ZERILLI-ARMSTRONG)
C-------------
      IF (ISRATE > 0) THEN
        DO I=LFT,LLT
          EPD(I) = UNDEMI*((D1(I)+DAV(I))**2+(D2(I)+DAV(I))**2
     .                                  +(D3(I)+DAV(I))**2) 
     .           + QUART*(D4(I)**2+D5(I)**2+D6(I)**2)
          EPD(I) = SQRT(TROIS*EPD(I))/TDEMI
          EPSD(I) = ASRATE*EPD(I) + (UN -ASRATE)*EPSD(I)
        ENDDO
      ELSE
        DO I=LFT,LLT
          EPSD(I)=OFF(I)* MAX( ABS(D1(I)), ABS(D2(I)), ABS(D3(I)),
     .      UNDEMI*ABS(D4(I)),UNDEMI*ABS(D5(I)),UNDEMI*ABS(D6(I)))
        ENDDO
      ENDIF   ! israte
      EPSP(1:NEL) = EPSD(1:NEL)                   
c
      IF (EPIF.NE.0.0)THEN
        DO I=LFT,LLT
           EPD(I)= MAX(EPSD(I),EM15)
           EPD(I)= LOG(EPD(I)/EPDR)
        ENDDO
        IF(JTHE >= 0 ) THEN     
          TM=Z4
          IF(TM.EQ.ZERO)TM=EP30   
          DO I=LFT,LLT
            T(I) = T(I) +CP*EINT(I)/MAX(EM15,VOL(I))
            TSTAR(I)=MIN(UN,MAX(ZERO,(T(I)-T_1)/(TM-T_1))) 
          ENDDO
        ENDIF  
  
        IF(NPIF.EQ.ZERO)THEN
          DO I=LFT,LLT
             MT=MAX(EM15,Z3)
c
c epd(i) >= 0
c
             EPD(I)= MAX(ZERO,EPD(I))
             EPD(I)= (UN +CC * EPD(I))*(UN -TSTAR(I)**MT)
             IF(ICC.EQ.1)SIGMX(I)= SIGMX(I)*EPD(I)
          ENDDO
        ELSEIF(NPIF.EQ.LLT)THEN
          DO I=LFT,LLT
             EPD(I)= CC*EXP((-Z3+Z4 * EPD(I))*T(I))
             IF(ICC.EQ.1)SIGMX(I)= SIGMX(I) + EPD(I)
             CA(I) = CA(I) + EPD(I)
             EPD(I)=UN
          ENDDO
        ELSE
          DO I=LFT,LLT
           IF(IRTY.EQ.0)THEN
             MT=Z3
           EPD(I)= MAX(ZERO,EPD(I))
             EPD(I)= (UN +CC * EPD(I))*(UN -TSTAR(I)**MT)
             IF(ICC.EQ.1)SIGMX(I)= SIGMX(I)*EPD(I)
           ELSE
             EPD(I)= CC*EXP((-Z3+Z4 * EPD(I))*T(I))
             IF(ICC.EQ.1)SIGMX(I)= SIGMX(I) + EPD(I)
             CA(I) = CA(I) + EPD(I)
             EPD(I)=UN
           ENDIF
          ENDDO
        ENDIF
      ELSE
        DO I=LFT,LLT
          EPD(I)=UN
        ENDDO
      ENDIF
C-------------
C     CRITERE
C-------------
C--------pure-isotrope----
      IF (IKFLG == 0 ) THEN
      IF(IPLA.NE.2)THEN
        IF(CN.EQ.UN) THEN
                AK(LFT:LLT)= CA(LFT:LLT)+CB(LFT:LLT)*EPXE(LFT:LLT)
                QH(LFT:LLT)= CB(LFT:LLT)*EPD(LFT:LLT)
        ELSE
                DO  I=LFT,LLT
                        IF(EPXE(I).GT.ZERO) THEN
                                AK(I)=CA(I)+CB(I)*EPXE(I)**CN
                                IF(CN.GT.UN) THEN
                                        QH(I)= (CB(I)*CN*EPXE(I)**(CN - UN))*EPD(I)
                                ELSE
                                        QH(I)= (CB(I)*CN/EPXE(I)**(UN -CN))*EPD(I)
                                ENDIF
                        ELSE
                                AK(I)=CA(I)
                                QH(I)=ZERO
                        ENDIF
                ENDDO
        ENDIF
        DO  I=LFT,LLT
                AK(I)=AK(I)*EPD(I)
                IF(SIGMX(I).LT.AK(I))THEN
                        AK(I)=SIGMX(I)
                        QH(I)=ZERO
                ENDIF
                SIGY(I) = AK(I)
                IF(EPXE(I).GT.EPMX)THEN
                        AK(I)=ZERO
                        QH(I)=ZERO
                ENDIF
        ENDDO
      ELSE
       DO 95 I=LFT,LLT
       IF(CN.EQ.UN) THEN
        AK(I)= CA(I)+CB(I)*EPXE(I)
       ELSE
       IF(EPXE(I).GT.ZERO) THEN
        AK(I)=CA(I)+CB(I)*EPXE(I)**CN
       ELSE
         AK(I)=CA(I)
       ENDIF
       ENDIF
       AK(I)= MIN(AK(I)*EPD(I),SIGMX(I))
       SIGY(I) = AK(I)
       IF(EPXE(I).GT.EPMX)AK(I)=ZERO
   95  CONTINUE
      ENDIF
      ELSE
C------------------------------------------
C     ECROUISSAGE CINE&MIXE
C------------------------------------------
       IF(IPLA.NE.2)THEN
        DO I=LFT,LLT
         BETA = UN-FISOKIN
C------------SIGY is used for hourglass stress compute--         
         IF(CN.EQ.UN) THEN
          SIGY(I) = CA(I)+CB(I)*EPXE(I)
          AK(I)= CA(I)+BETA*CB(I)*EPXE(I)
          QH(I)= CB(I)*EPD(I)
         ELSE
          IF(EPXE(I).GT.ZERO) THEN
           SIGY(I)=CA(I)+CB(I)*EPXE(I)**CN
           AK(I)=CA(I)+BETA*CB(I)*EPXE(I)**CN
           IF(CN.GT.UN) THEN
            QH(I)= (CB(I)*CN*EPXE(I)**(CN - UN))*EPD(I)
           ELSE
            QH(I)= (CB(I)*CN/EPXE(I)**(UN -CN))*EPD(I)
           ENDIF
          ELSE
           AK(I)=CA(I)
           SIGY(I)=CA(I)
           QH(I)=ZERO
          ENDIF
         ENDIF
         AK(I)=AK(I)*EPD(I)
         SIGY(I)=SIGY(I)*EPD(I)
         IF(SIGMX(I).LT.AK(I))THEN
          AK(I)=SIGMX(I)
          QH(I)=ZERO
         ENDIF
         SIGY(I)=MIN(SIGY(I),SIGMX(I))
         IF(EPXE(I).GT.EPMX)THEN
          AK(I)=ZERO
          QH(I)=ZERO
         ENDIF
        END DO !I=LFT,LLT
C
       ELSE
        DO I=LFT,LLT
         BETA = UN-FISOKIN
         IF(CN.EQ.UN) THEN
          SIGY(I)= CA(I)+CB(I)*EPXE(I)
          AK(I)= CA(I)+BETA*CB(I)*EPXE(I)
          QH(I)= CB(I)*EPD(I)
         ELSE
          IF(EPXE(I).GT.ZERO) THEN
           SIGY(I)=CA(I)+CB(I)*EPXE(I)**CN
           AK(I)=CA(I)+BETA*CB(I)*EPXE(I)**CN
           IF(CN.GT.UN) THEN
            QH(I)= (CB(I)*CN*EPXE(I)**(CN - UN))*EPD(I)
           ELSE
            QH(I)= (CB(I)*CN/EPXE(I)**(UN -CN))*EPD(I)
           ENDIF
          ELSE
           AK(I)=CA(I)
           SIGY(I)=CA(I)
           QH(I)=ZERO
          ENDIF
         ENDIF
         AK(I)= MIN(AK(I)*EPD(I),SIGMX(I))
         SIGY(I) = MIN(SIGY(I)*EPD(I),SIGMX(I))
         IF(EPXE(I).GT.EPMX)THEN
          AK(I)=ZERO
          QH(I)=ZERO
         ENDIF
        END DO 
       ENDIF
      END IF !(IKFLG == 0 ) THEN
C
      IF(IPLA.EQ.0)THEN
       DO 110 I=LFT,LLT
       SCALE= MIN(UN,AK(I)/ MAX(AJ2(I),EM15))
       SIG(I,1)=SCALE*SIG(I,1)
       SIG(I,2)=SCALE*SIG(I,2)
       SIG(I,3)=SCALE*SIG(I,3)
       SIG(I,4)=SCALE*SIG(I,4)
       SIG(I,5)=SCALE*SIG(I,5)
       SIG(I,6)=SCALE*SIG(I,6)
       EPXE(I)=EPXE(I)+(UN-SCALE)*AJ2(I)/MAX(TROIS*G(I)+QH(I),EM15)
C
       DPLA1(I) = (UN-SCALE)*AJ2(I)/MAX(TROIS*G(I)+QH(I),EM15)     
  110 CONTINUE
C
      ELSEIF(IPLA.EQ.2)THEN
       DO I=LFT,LLT
       SCALE= MIN(UN,AK(I)/ MAX(AJ2(I),EM15))
       SIG(I,1)=SCALE*SIG(I,1)
       SIG(I,2)=SCALE*SIG(I,2)
       SIG(I,3)=SCALE*SIG(I,3)
       SIG(I,4)=SCALE*SIG(I,4)
       SIG(I,5)=SCALE*SIG(I,5)
       SIG(I,6)=SCALE*SIG(I,6)
       EPXE(I)=EPXE(I)+(UN -SCALE)*AJ2(I)/MAX(3.*G(I),EM15)
       DPLA1(I) = (UN -SCALE)*AJ2(I)/MAX(3.*G(I),EM15)
       ENDDO
C
      ELSEIF(IPLA.EQ.1)THEN
C
      IF (IMPL_S.EQ.0) THEN
       DO I=LFT,LLT
       SCALE= MIN(UN,AK(I)/ MAX(AJ2(I),EM15))
Clm    plastic strain increment.
       DPLA=(UN-SCALE)*AJ2(I)/MAX(TROIS*G(I)+QH(I),EM15)
Clm    actual yield stress.
       AK(I)=AK(I)+(UN - FISOKIN)*DPLA*QH(I)
       SCALE= MIN(UN,AK(I)/ MAX(AJ2(I),EM15))
       SIG(I,1)=SCALE*SIG(I,1)
       SIG(I,2)=SCALE*SIG(I,2)
       SIG(I,3)=SCALE*SIG(I,3)
       SIG(I,4)=SCALE*SIG(I,4)
       SIG(I,5)=SCALE*SIG(I,5)
       SIG(I,6)=SCALE*SIG(I,6)
       EPXE(I)=EPXE(I)+DPLA
       DPLA1(I) = DPLA       
       ENDDO
      ELSE
c ---- nonlinear hardening requires iterations in radial return ---
      CALL  M2ITER_IMP( SIG,   EPXE,   AJ2,  G,
     1                  CA,     CB,    CN,
     2                  EPD,  SIGMX,  EPMX,
     3                 DPLA1,   AK,  QH,  SIGY ,FISOKIN,
     4                  NEL)
C
      END IF !(IMPL_S.EQ.0) THEN
      ENDIF 
c-----------------------------------------
      IF (IKFLG > 0 ) THEN
       DO I=LFT,LLT
          DSXX = SIGEXX(I) - SIG(I,1) 
          DSYY = SIGEYY(I) - SIG(I,2)
          DSZZ = SIGEZZ(I) - SIG(I,3)
          DSXY = SIGEXY(I) - SIG(I,4)
          DSYZ = SIGEYZ(I) - SIG(I,5)
          DSZX = SIGEZX(I) - SIG(I,6)
C 
          HKIN = DTIERS*FISOKIN*QH(I)
          ALPHA = HKIN/MAX(DEUX*G(I)+HKIN,EM15)  
C       ..updates back stresses
          SIGBAK(I,1) = SIGBAK(I,1) + ALPHA*DSXX 
          SIGBAK(I,2) = SIGBAK(I,2) + ALPHA*DSYY 
          SIGBAK(I,3) = SIGBAK(I,3) + ALPHA*DSZZ 
          SIGBAK(I,4) = SIGBAK(I,4) + ALPHA*DSXY 
          SIGBAK(I,5) = SIGBAK(I,5) + ALPHA*DSYZ 
          SIGBAK(I,6) = SIGBAK(I,6) + ALPHA*DSZX 
C       ..gets stresses from shifted stresses and back stresses
          SIG(I,1)=SIG(I,1) + SIGBAK(I,1)
          SIG(I,2)=SIG(I,2) + SIGBAK(I,2)
          SIG(I,3)=SIG(I,3) + SIGBAK(I,3)
          SIG(I,4)=SIG(I,4) + SIGBAK(I,4)
          SIG(I,5)=SIG(I,5) + SIGBAK(I,5)
          SIG(I,6)=SIG(I,6) + SIGBAK(I,6)
       ENDDO
      END IF !(IKFLG > 0 ) THEN
C
      BIDMVSIZ(1:MVSIZ) = ZERO
      IF (JSPH.EQ.0)THEN
       CALL MQVISCB(PM,OFF     ,RHO    ,BIDMVSIZ,BIDMVSIZ,SSP   ,
     2        BIDMVSIZ,STIFN   ,DT2T   ,NELTST,ITYPTST,AIRE  ,
     3        OFFG    ,GEO     ,PID    ,VNEW   ,VD2   ,DELTAX ,
     4        VIS     ,D1      ,D2      ,D3     ,PNEW  ,PSH    ,
     5        MAT     ,NGL     ,QNEW    ,SSP_EQ ,VOL   ,MSSA   ,
     6        DMELS   ,IBID    ,FACQ0   ,CONDE  ,DTEL  ,G_DT   ,
     7        IPM     ,RHOREF  ,RHOSP   )
      ELSE
       CALL MDTSPH(PM ,OFF     ,RHO     ,BIDMVSIZ,BIDMVSIZ,
     2        BIDMVSIZ,STIFN   ,DT2T    ,NELTST  ,ITYPTST ,
     3        OFFG    ,GEO     ,PID     ,MUMAX   ,SSP     ,
     4        VNEW    ,VD2     ,DELTAX  ,VIS     ,D1      ,
     5        D2      ,D3      ,PNEW    ,PSH     ,MAT     ,
     6        NGL     ,QNEW    ,SSP_EQ  ,G_DT    ,DTEL)
      ENDIF
C
      DTA =UNDEMI*DT1
C
      DO I=LFT,LLT
       PNEW(I)=C1*AMU(I)
       SIG(I,1)=(SIG(I,1)-PNEW(I))*OFF(I)
       SIG(I,2)=(SIG(I,2)-PNEW(I))*OFF(I)
       SIG(I,3)=(SIG(I,3)-PNEW(I))*OFF(I)
       SIG(I,4)= SIG(I,4)*OFF(I)
       SIG(I,5)= SIG(I,5)*OFF(I)
       SIG(I,6)= SIG(I,6)*OFF(I)
      ENDDO
!       split the loop in two parts in order to improve the vectorization
      DO I=LFT,LLT
       E1=D1(I)*(SOLD1(I)+SIG(I,1))
       E2=D2(I)*(SOLD2(I)+SIG(I,2))
       E3=D3(I)*(SOLD3(I)+SIG(I,3))
       E4=D4(I)*(SOLD4(I)+SIG(I,4))
       E5=D5(I)*(SOLD5(I)+SIG(I,5))
       E6=D6(I)*(SOLD6(I)+SIG(I,6))
C
       EINC = VOL_AVG(I)*(E1+E2+E3+E4+E5+E6)*DTA - UNDEMI*DVOL(I)*(QOLD(I)+QNEW(I))
       EINT(I)=(EINT(I)+EINC*OFF(I)) / MAX(EM15,VOL(I))
      ENDDO
C
      DO I=LFT,LLT
       QOLD(I)=QNEW(I)
       DEFP(I)=EPXE(I)
       SIGY(I)=MAX(SIGY(I),AK(I))
      ENDDO
       DO I=LFT,LLT
         IF(DPLA1(I).GT.0) ETSE(I)= UNDEMI*QH(I)*OFF(I)/MAX(G(I),EM15)
       ENDDO
      IF (IMPL_S.GT.0) THEN
       IF (IKT==0) RETURN
       IF(IPLA.NE.2.AND.IKFLG.EQ.0)THEN
        DO I=LFT,LLT
         IF(DPLA1(I).GT.0)THEN
          IF(CN.EQ.UN) THEN
           QH(I)= CB(I)*EPD(I)
          ELSEIF(CN.GT.UN) THEN
           QH(I)= (CB(I)*CN*EPXE(I)**(CN - UN))*EPD(I)
          ELSE
           QH(I)= (CB(I)*CN/EPXE(I)**(UN -CN))*EPD(I)
          ENDIF
         ENDIF
        ENDDO
       ENDIF
C      -----IKT=4------------
       DO I = LFT,LLT
        IF (DPLA1(I).GT.ZERO) THEN

c ...... Von Mises stress at the elastic predictor (point B)
c ...... SIGEXX, etc. are deviatoric stresses
         VM =UNDEMI*(SIGEXX(I)**2+SIGEYY(I)**2+SIGEZZ(I)**2)
     1              +SIGEXY(I)**2+SIGEYZ(I)**2+SIGEZX(I)**2
         VM_1 =UN/SQRT(TROIS*VM)
         G3 = TROIS*G(I)
         G3H = MAX(G3+QH(I),EM15)
         SCALE = MAX(ZERO,UN-G3H*DPLA1(I)*VM_1)
c ...... NORM_1 normalizes deviatoric stresses, includes consistent
c ...... stiffness matrix parameter beta, von Mises at B, and two_pmi
         NORM_1=G3*VM_1*SQRT(SCALE/G3H)
c ...... Deviatoric stresses "normalized"
         SIGNOR(I,1)=SIGEXX(I)*NORM_1
         SIGNOR(I,2)=SIGEYY(I)*NORM_1
         SIGNOR(I,3)=SIGEZZ(I)*NORM_1
         SIGNOR(I,4)=SIGEXY(I)*NORM_1
         SIGNOR(I,5)=SIGEYZ(I)*NORM_1
         SIGNOR(I,6)=SIGEZX(I)*NORM_1

c ...... Parameter alpha of consistent matrix
         AL_IMP(I)= UN - G3*DPLA1(I)*VM_1
        ELSE
         AL_IMP(I)=UN
        ENDIF
       ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  M2ITER_IMP                    source/materials/mat/mat002/m2law.F
Chd|-- called by -----------
Chd|        M2LAW                         source/materials/mat/mat002/m2law.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M2ITER_IMP( SIG,  EPXE,   AJ2,     G,    CA,
     1                        CB,    CN,   EPD, SIGMX,  EPMX,
     2                     DPLA1,    AK,    QH, SIGY ,FISOKIN,
     3                      NEL )
c--------------------------------------------------------------
c
c    -------------------
c    -- parameters in --
c    -------------------
c
c    SIG   = Elastic Predictor (Deviatoric Stress Tensor)
c    EPXE  = previous time-step equivalent plastic strain  (on input)
c    AJ2   = von Mises stress at elastic predictor
c    G     = Kirhhoff's modulus (elastic shear modulus))
c
c    CA, CB, CN = coefficients of the hardening rule
c                 Sigma_Yield =   CA + CB * (equiv_pl_strn)**CN
c    EPD   = scalling fac due to velocity effect
c    SIGMX = max allowable von Mises stress   
c    EPMX  = max allowable equivalent plastic strain
c    FISOKIN= Mixed hardening ratio
c    --------------------
c    -- parameters out --      
c    --------------------
c
c    DPLA1 = equivalent plastic strain increment
c    EPXE  = updated  equivalent plastic strain (on output)
c    AK    = current yield stress
c    SIGY  = current yield stress (same as AK)
c    QH    = current hardening parameter
c
c--------------------------------------------------------------
c qz !! this subroutine will be optimized after
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NEL
C     REAL ou REAL*8
      my_real
     .   SIG(NEL,6), EPXE(*),EPD(*),
     .   G(*), AK(*),QH(*), AJ2(*), 
     .   CA(*), CB(*), CN,FISOKIN,
     .   SIGMX(*),DPLA1(*),EPMX  ,SIGY(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ITER, NITER
C
C     REAL ou REAL*8
      MY_REAL
     .   XSI,DXSI,LHS,RHS,ALPHA_RADIAL,BETA,G3(MVSIZ),HTOT
C
c ---- max number of iterations ----
       NITER = 10
C       
       DO 100 I=LFT,LLT
C
C------------------------------------------
C
       XSI = ZERO
C
C      -- PREVIOUS TIME-STEP EQUIVALENT PLASTIC STRAIN
       EPXE(I) = MAX(ZERO,EPXE(I))
       G3(I) =MAX(TROIS*G(I),EM15)
C
C      -- PREVIOUS TIME-STEP YIELD STRESS !!!!! if EPXE=0!!!!!
C       AK(I)=CA(I)+CB(I)*EPXE(I)**CN(I) !..SIGMA0(EPS_P)
C
C      -- COMPARE IT WITH ELASTIC PREDICTOR
       IF(AJ2(I).LT.AK(I)) GO TO 90 
C
       BETA = UN-FISOKIN
       DO 80 ITER = 1,NITER ! .. NR ITERATIONS ..
C
       IF(EPXE(I).GT.ZERO) THEN !..TESTING FOR ZERO EQUIV.PL.STRN
         AK(I)=CA(I)+BETA*CB(I)*EPXE(I)**CN !..SIGMA0(EPS_P)
C
C      --COMPUTE THE HARDENING PARAMETER H
C      --(JUST THE DERIVATIVE OF THE SIG-EPS_PL FUNCTION)
C
         IF(CN.GT.UN) THEN
           QH(I) = (CB(I)*CN*EPXE(I)**(CN - UN))*EPD(I)
         ELSEIF(CN.EQ.UN) THEN !.. LINEAR HARDENING
           QH(I)= CB(I)*EPD(I)
         ELSE !.. POWER LESS THAN UNITY
           QH(I) = (CB(I)*CN/EPXE(I)**(UN -CN))*EPD(I)
         ENDIF
C
       ELSEIF(EPXE(I).EQ.ZERO) THEN
C
         AK(I)=CA(I)
C
         IF(CN.GT.UN )THEN
               QH(I) = ZERO
         ELSEIF(CN.EQ.UN) THEN 
               QH(I) = CB(I)*EPD(I)
         ELSE
               QH(I) = EP10 *EPD(I) !.. SHOULD BE +INFINITY
         ENDIF
C
       ELSE !--ERROR
CCC             WRITE(*,*)'M2LAW-NEGATIVE EQUIVALENT PLASTIC STRN',EPXE(I)
CCC             CALL ARRET(2)
C
       ENDIF !--EPXE(I).GT.ZERO ..END TESTING FOR ZERO EQUIV.PL.STRN
C
       HTOT = G3(I) + FISOKIN*QH(I)
       RHS = AJ2(I) -HTOT * XSI - AK(I)
       LHS = G3(I) +  QH(I)
       DXSI = RHS/LHS
       XSI = XSI + DXSI
C
       EPXE(I) = EPXE(I) + DXSI 
       EPXE(I) = MAX(ZERO,EPXE(I))
          IF( ABS(DXSI).LT.EM10.AND.
     $        ABS(RHS ).LT.EM10) GO TO 90
C
 80    CONTINUE
CCC      WRITE(*,*)'M2LAW--NON-CONVERGED ITERATION', ABS(DXSI),ABS(RHS)
 90    CONTINUE
C
       IF(XSI.LT.ZERO) XSI = ZERO
       DPLA1(I) = XSI !.. PLASTIC STRAIN MULTIPLIER "delta lambda"
CCC       IF(I.EQ.1)WRITE(*,*)'D_LAMBDA =',XSI
C
C     .. ENFORCING SOME CUT-OFFS, LEFT UNCHANGED
Cqz----!!!cette partie devrait etre   interieure de boucle ---  tester
       AK(I)=AK(I)*EPD(I)
       IF(SIGMX(I).LT.AK(I))THEN
        AK(I)=SIGMX(I)
        QH(I)=ZERO
       ENDIF
C       SIGY(I) = AK(I)
       IF(EPXE(I).GT.EPMX)THEN
         AK(I)=ZERO
         QH(I)=ZERO
       ENDIF
C
C     -- RADIAL RETURN ------
C     -- ALPHA_RADIAL := PARAMETER ALPHA OF THE RADIAL RETURN
C
       ALPHA_RADIAL= MIN(UN,AK(I)/ MAX(AJ2(I),EM15))
       SIG(I,1)=ALPHA_RADIAL*SIG(I,1)
       SIG(I,2)=ALPHA_RADIAL*SIG(I,2)
       SIG(I,3)=ALPHA_RADIAL*SIG(I,3)
       SIG(I,4)=ALPHA_RADIAL*SIG(I,4)
       SIG(I,5)=ALPHA_RADIAL*SIG(I,5)
       SIG(I,6)=ALPHA_RADIAL*SIG(I,6)
C
 100  CONTINUE                  !..LOOP ON ELEMENTS
C
      RETURN
      END


