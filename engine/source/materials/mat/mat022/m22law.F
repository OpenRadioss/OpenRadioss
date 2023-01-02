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
Chd|  M22LAW                        source/materials/mat/mat022/m22law.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|        MDTSPH                        source/materials/mat_share/mdtsph.F
Chd|        MQVISCB                       source/materials/mat_share/mqviscb.F
Chd|====================================================================
      SUBROUTINE M22LAW (PM ,OFF ,SIG     ,EINT    ,RHO     ,
     2           QOLD   ,EPXE    ,EPD     ,VOL     ,PDAM    ,
     3           STIFN  ,DT2T    ,NELTST  ,ITYPTST ,OFFG    ,
     4           GEO    ,PID     ,AMU     ,VOL_AVG ,
     6           MUMAX  ,MAT     ,NGL     ,SSP     ,DVOL    ,
     7           AIRE   ,VNEW    ,VD2     ,DELTAX  ,VIS     ,  
     8           D1     ,D2      ,D3      ,D4      ,D5      ,
     9           D6     ,PNEW    ,PSH     ,QNEW    ,SSP_EQ  ,
     A           SOLD1  ,SOLD2   ,SOLD3   ,SOLD4   ,SOLD5   ,
     B           SOLD6  ,IPLA    ,SIGY    ,DEFP    ,DPLA    ,
     C           MSSA   ,DMELS   ,CONDE   ,DTEL    ,G_DT    ,
     D           NEL    ,IPM     ,RHOREF  ,RHOSP   )
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
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELTST,ITYPTST ,PID(*), G_DT
      INTEGER MAT(*),NGL(*),IPLA,NEL,IPM(NPROPMI,*)
      my_real
     .   DT2T
      my_real
     .   PDAM
      my_real
     .   PM(NPROPM,*), OFF(*), SIG(NEL,6), EINT(*), RHO(*), QOLD(*),
     .   EPXE(*), EPD(*), VOL(*),STIFN(*), OFFG(*),GEO(NPROPG,*) ,
     .   MUMAX(*), SIGY(*), DEFP(*), DPLA(MVSIZ),
     .   AMU(*), VOL_AVG(*)
      my_real
     .   VNEW(*), VD2(*), DELTAX(*), SSP(*), AIRE(*), VIS(*), 
     .   PSH(*), PNEW(1), QNEW(*) ,SSP_EQ(*), 
     .   D1(*), D2(*), D3(*), D4(*), D5(*), D6(*),
     .   SOLD1(MVSIZ), SOLD2(MVSIZ), SOLD3(MVSIZ),
     .   SOLD4(MVSIZ), SOLD5(MVSIZ), SOLD6(MVSIZ),
     .   MSSA(*), DMELS(*), CONDE(*),DTEL(*),
     .   RHOREF(*)  ,RHOSP(*)  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ICC(MVSIZ), I, MX, II,IBID,ICC_1
      my_real
     .   RHO0(MVSIZ), 
     .   G(MVSIZ),AK(MVSIZ),
     .   QH(MVSIZ), C1(MVSIZ),
     .   P(MVSIZ), EPMX(MVSIZ), THETL(MVSIZ),
     .   CA(MVSIZ), CB(MVSIZ), CC(MVSIZ), CN(MVSIZ), 
     .   EPDR(MVSIZ),
     .   DVOL(MVSIZ), SIGMX(MVSIZ),
     .   CE(MVSIZ),EPSL(MVSIZ), QL(MVSIZ), YLDL(MVSIZ), AJ2(MVSIZ),
     .   G0(MVSIZ),
     .   E1, E2, E3, E4, E5, E6, G1, G2,
     .   EPSP, HL, DEPSL, ALPE, DAV, SCALE, BID1, BID2,
     .   BID3, EINC, DTA,FACQ0,
     .   RHO0_1,C1_1,CA_1,CB_1,CN_1,
     .   EPMX_1,SIGMX_1,CC_1,EPDR_1,EPSL_1,
     .   YLDL_1,QL_1
     
      FACQ0 = ONE
      IF(IPLA==0)THEN
       MX = MAT(LFT)
       RHO0_1 =PM( 1,MX)
       C1_1   =PM(32,MX)
       CA_1   =PM(38,MX)
       CB_1   =PM(39,MX)
       CN_1   =PM(40,MX)
       EPMX_1 =PM(41,MX)
       SIGMX_1=PM(42,MX)
       CC_1   =PM(43,MX)
       EPDR_1 =PM(44,MX)
       EPSL_1 =PM(45,MX)
       YLDL_1 =PM(47,MX)
       QL_1   =PM(48,MX)
       ICC_1  =NINT(PM(49,MX))
       DO 10 I=LFT,LLT
       RHO0(I) =RHO0_1
       G0(I)   =PM(22,MX)*OFF(I)
       C1(I)   =C1_1
       CA(I)   =CA_1
       CB(I)   =CB_1
       CN(I)   =CN_1
       EPMX(I) =EPMX_1
       SIGMX(I)=SIGMX_1
       CC(I)   =CC_1
       EPDR(I) =EPDR_1
       EPSL(I) =EPSL_1
       YLDL(I) =YLDL_1
       QL(I)   =QL_1
       ICC(I)  =ICC_1
   10 CONTINUE
      ELSE

      MX = MAT(LFT)
      RHO0_1 =PM( 1,MX)
      C1_1   =PM(32,MX)
      CA_1   =PM(38,MX)
      CB_1   =PM(39,MX)
      CN_1   =PM(40,MX)
      EPMX_1 =PM(41,MX)
      SIGMX_1=PM(42,MX)
      CC_1   =PM(43,MX)
      EPDR_1 =PM(44,MX)
      EPSL_1 =PM(45,MX)
      QL_1   =PM(46,MX)
      YLDL_1 =PM(47,MX)
      ICC_1  =NINT(PM(49,MX))
      DO 11 I=LFT,LLT
       RHO0(I) =RHO0_1
       G0(I)   =PM(22,MX)*OFF(I)
       C1(I)   =C1_1
       CA(I)   =CA_1
       CB(I)   =CB_1
       CN(I)   =CN_1
       EPMX(I) =EPMX_1
       SIGMX(I)=SIGMX_1
       CC(I)   =CC_1
       EPDR(I) =EPDR_1
       EPSL(I) =EPSL_1
       QL(I)   =QL_1
       YLDL(I) =YLDL_1
       ICC(I)  =ICC_1
   11 CONTINUE
      ENDIF

      DO 15 I=LFT,LLT
      EPD(I)=OFF(I) * MAX(   ABS(D1(I)), ABS(D2(I)), ABS(D3(I)), HALF*ABS(D4(I)),HALF*ABS(D5(I)),HALF*ABS(D6(I)))
      EPSP = MAX(EPD(I),EPDR(I))
      CE(I) = ONE +CC(I) * LOG(EPSP/EPDR(I))
 15   CONTINUE

      IF(IPLA/=2)THEN
       DO 20 I=LFT,LLT
       IF(CN(I)==ONE) THEN
        AK(I)= CA(I)+CB(I)*EPXE(I)
        QH(I)= CB(I)*CE(I)
       ELSE
        IF(EPXE(I)>ZERO) THEN
         AK(I)=CA(I)+CB(I)*EPXE(I)**CN(I)
         IF(CN(I)>ONE) THEN
          QH(I)= (CB(I)*CN(I)*EPXE(I)**(CN(I)-ONE))*CE(I)
         ELSE
          QH(I)= (CB(I)*CN(I)/EPXE(I)**(ONE - CN(I)))*CE(I)
         ENDIF
        ELSE
         AK(I)=CA(I)
         QH(I)=ZERO
        ENDIF
       ENDIF
       SIGY(I) = AK(I)
       IF(EPXE(I)>EPMX(I))AK(I)=ZERO
       IF(EPXE(I)>EPSL(I))QH(I)=QL(I)
 20    CONTINUE
      ELSE
       DO I=LFT,LLT
       IF(CN(I)==ONE) THEN
        AK(I)= CA(I)+CB(I)*EPXE(I)
       ELSE
        IF(EPXE(I)>ZERO) THEN
         AK(I)=CA(I)+CB(I)*EPXE(I)**CN(I)
        ELSE
         AK(I)=CA(I)
        ENDIF
       ENDIF
       SIGY(I) = AK(I)
       IF(EPXE(I)>EPMX(I))AK(I)=ZERO
       ENDDO
      ENDIF

      IF(IPLA==0)THEN
      DO I=LFT,LLT
       AK(I) = MIN(AK(I),SIGMX(I))
       HL    = THREE*G0(I)*QL(I)/(THREE*G0(I)+QL(I))
       DEPSL = MAX(ZERO,EPXE(I)-EPSL(I))
       AK(I) = MIN(AK(I),YLDL(I)+HL*DEPSL)
       AK(I) = MAX(AK(I),ZERO)
       ALPE  = MIN(ONE,AK(I)/MAX(AK(I)+THREE*G0(I)*DEPSL,EM15))
       AK(I) = AK(I)*CE(I)
       IF(ICC(I)==2) AK(I) = MIN(AK(I),SIGMX(I))
       G(I)  = ALPE*G0(I)
       C1(I) = PDAM*ALPE*C1(I) + (1.-PDAM)*C1(I)
       P(I)  =-THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
       G1=DT1*G(I)
       G2=TWO*G1
       SSP(I)=SQRT((ONEP333*G(I)+C1(I))/RHO0(I))
       !-------------------------------
       !     CONTRAINTES DEVIATORIQUES
       !-------------------------------
       DAV     =-THIRD*(D1(I)+D2(I)+D3(I))
       SIG(I,1)=SIG(I,1)+P(I)+G2*(D1(I)+DAV)
       SIG(I,2)=SIG(I,2)+P(I)+G2*(D2(I)+DAV)
       SIG(I,3)=SIG(I,3)+P(I)+G2*(D3(I)+DAV)
       SIG(I,4)=SIG(I,4)+G1*D4(I)
       SIG(I,5)=SIG(I,5)+G1*D5(I)
       SIG(I,6)=SIG(I,6)+G1*D6(I)
       AJ2(I)=HALF*(SIG(I,1)**2+SIG(I,2)**2+SIG(I,3)**2)
     1               +SIG(I,4)**2+SIG(I,5)**2+SIG(I,6)**2
       AJ2(I)=SQRT(THREE*AJ2(I))
      ENDDO
      ELSEIF(IPLA==2)THEN
      DO I=LFT,LLT
       AK(I) = MIN(AK(I),SIGMX(I))
       DEPSL = MAX(ZERO,EPXE(I)-EPSL(I))
       AK(I) = MIN(AK(I),YLDL(I)+QL(I)*DEPSL)
       AK(I) = MAX(AK(I),ZERO)
       ALPE  = MIN(ONE,AK(I)/MAX(AK(I) + THREE*G0(I)*DEPSL,EM15))
       AK(I) = AK(I)*CE(I)
       IF(ICC(I)==2) AK(I) = MIN(AK(I),SIGMX(I))
       G(I)  = ALPE*G0(I)
       C1(I) = PDAM*ALPE*C1(I) + (ONE - PDAM)*C1(I)
       P(I)  =-THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
       G1=DT1*G(I)
       G2=TWO*G1
       SSP(I)=SQRT((ONEP333*G(I)+C1(I))/RHO0(I))
       !-------------------------------
       !     CONTRAINTES DEVIATORIQUES
       !-------------------------------
       IF(EPXE(I)>EPSL(I).AND.THREE*G(I)<EM15)THEN
        SIG(I,1)=ZERO
        SIG(I,2)=ZERO
        SIG(I,3)=ZERO
        SIG(I,4)=ZERO
        SIG(I,5)=ZERO
        SIG(I,6)=ZERO
        AJ2(I)  =ZERO
       ELSE
        DAV     =-THIRD*(D1(I)+D2(I)+D3(I))
        SIG(I,1)=SIG(I,1)+P(I)+G2*(D1(I)+DAV)
        SIG(I,2)=SIG(I,2)+P(I)+G2*(D2(I)+DAV)
        SIG(I,3)=SIG(I,3)+P(I)+G2*(D3(I)+DAV)
        SIG(I,4)=SIG(I,4)+G1*D4(I)
        SIG(I,5)=SIG(I,5)+G1*D5(I)
        SIG(I,6)=SIG(I,6)+G1*D6(I)
        AJ2(I)=HALF*(SIG(I,1)**2+SIG(I,2)**2+SIG(I,3)**2)+
     .                 SIG(I,4)**2+SIG(I,5)**2+SIG(I,6)**2
        AJ2(I)=SQRT(THREE*AJ2(I))
       ENDIF
      ENDDO
      ELSE
      DO I=LFT,LLT
       AK(I) = MIN(AK(I),SIGMX(I))
       DEPSL = MAX(ZERO,EPXE(I)-EPSL(I))
       AK(I) = MIN(AK(I),YLDL(I)+QL(I)*DEPSL)
       AK(I) = MAX(AK(I),ZERO)
       ALPE  = MIN(ONE,AK(I)/MAX(AK(I) + THREE*G0(I)*DEPSL,EM15))
       AK(I) = AK(I)*CE(I)
       IF(ICC(I)==2) AK(I) = MIN(AK(I),SIGMX(I))
       G(I)  = ALPE*G0(I)
       C1(I) = PDAM*ALPE*C1(I) + (ONE - PDAM)*C1(I)
       P(I)  =-THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
       G1=DT1*G(I)
       G2=TWO*G1
       SSP(I)=SQRT((ONEP333*G(I)+C1(I))/RHO0(I))
       !-------------------------------
       !     CONTRAINTES DEVIATORIQUES
       !-------------------------------
       IF(EPXE(I)>EPSL(I).AND.G(I)*(THREE*G0(I)+QH(I))<EM15)THEN
        SIG(I,1)=ZERO
        SIG(I,2)=ZERO
        SIG(I,3)=ZERO
        SIG(I,4)=ZERO
        SIG(I,5)=ZERO
        SIG(I,6)=ZERO
        AJ2(I)  =ZERO
       ELSE
        DAV     =-THIRD*(D1(I)+D2(I)+D3(I))
        SIG(I,1)=SIG(I,1)+P(I)+G2*(D1(I)+DAV)
        SIG(I,2)=SIG(I,2)+P(I)+G2*(D2(I)+DAV)
        SIG(I,3)=SIG(I,3)+P(I)+G2*(D3(I)+DAV)
        SIG(I,4)=SIG(I,4)+G1*D4(I)
        SIG(I,5)=SIG(I,5)+G1*D5(I)
        SIG(I,6)=SIG(I,6)+G1*D6(I)
        AJ2(I)=HALF*(SIG(I,1)**2+SIG(I,2)**2+SIG(I,3)**2)+
     .                 SIG(I,4)**2+SIG(I,5)**2+SIG(I,6)**2
        AJ2(I)=SQRT(THREE*AJ2(I))
       ENDIF
      ENDDO
      ENDIF

      IF(IPLA==0)THEN
        DO I=LFT,LLT
         SCALE= MIN(ONE,AK(I) / MAX(AJ2(I),EM15))
         SIG(I,1)=SCALE*SIG(I,1)
         SIG(I,2)=SCALE*SIG(I,2)
         SIG(I,3)=SCALE*SIG(I,3)
         SIG(I,4)=SCALE*SIG(I,4)
         SIG(I,5)=SCALE*SIG(I,5)
         SIG(I,6)=SCALE*SIG(I,6)
         EPXE(I)=EPXE(I)+(ONE -SCALE)*AJ2(I)/MAX(THREE*G(I)+QH(I),EM15)
         DPLA(I) =  (ONE -SCALE)*AJ2(I)/MAX(THREE*G(I)+QH(I),EM15)      
        ENDDO
        ELSEIF(IPLA==2)THEN
        DO I=LFT,LLT
         SCALE= MIN(ONE,AK(I) / MAX(AJ2(I),EM15))
         SIG(I,1)=SCALE*SIG(I,1)
         SIG(I,2)=SCALE*SIG(I,2)
         SIG(I,3)=SCALE*SIG(I,3)
         SIG(I,4)=SCALE*SIG(I,4)
         SIG(I,5)=SCALE*SIG(I,5)
         SIG(I,6)=SCALE*SIG(I,6)
         EPXE(I)=EPXE(I)+(ONE -SCALE)*AJ2(I)/MAX(THREE*G(I),EM15)
         DPLA(I) = (ONE -SCALE)*AJ2(I)/MAX(THREE*G(I),EM15)       
        ENDDO
        ELSEIF(IPLA==1)THEN
        DO I=LFT,LLT
         SCALE= MIN(ONE,AK(I) / MAX(AJ2(I),EM15))
         DPLA(I)=(ONE -SCALE)*AJ2(I)*G0(I)/ MAX(G(I)*(THREE*G0(I)+QH(I)),EM15)
         AK(I)=MAX(ZERO,AK(I)+DPLA(I)*QH(I))
         SCALE= MIN(ONE,AK(I)/ MAX(AJ2(I),EM15))
         SIG(I,1)=SCALE*SIG(I,1)
         SIG(I,2)=SCALE*SIG(I,2)
         SIG(I,3)=SCALE*SIG(I,3)
         SIG(I,4)=SCALE*SIG(I,4)
         SIG(I,5)=SCALE*SIG(I,5)
         SIG(I,6)=SCALE*SIG(I,6)
         EPXE(I)=EPXE(I)+DPLA(I)      
        ENDDO
        ENDIF
        
      IF (JSPH==0)THEN
       CALL MQVISCB(PM,OFF     ,RHO    ,BID1   ,BID2   ,SSP   ,
     2        BID3    ,STIFN   ,DT2T   ,NELTST ,ITYPTST,AIRE  ,
     3        OFFG    ,GEO     ,PID    ,VNEW   ,VD2    ,DELTAX ,  
     4        VIS     ,D1      ,D2      ,D3     ,PNEW   ,PSH   ,
     5        MAT     ,NGL     ,QNEW    ,SSP_EQ ,VOL    ,MSSA  ,
     6        DMELS   ,IBID    ,FACQ0   ,CONDE  ,DTEL   ,G_DT  ,
     7        IPM     ,RHOREF  ,RHOSP   )
      ELSE
       CALL MDTSPH(PM ,OFF     ,RHO     ,BID1    ,BID2    ,
     2        BID3    ,STIFN   ,DT2T    ,NELTST  ,ITYPTST ,
     3        OFFG    ,GEO     ,PID     ,MUMAX   ,SSP     ,
     4        VNEW    ,VD2     ,DELTAX  ,VIS     ,D1      ,
     5        D2      ,D3      ,PNEW    ,PSH     ,MAT     ,
     6        NGL     ,QNEW    ,SSP_EQ  ,G_DT    ,DTEL)
      ENDIF

      DO 120 I=LFT,LLT
      PNEW(I)=C1(I)*AMU(I)
 120  CONTINUE

C----------------------------
C     TEST DE RUPTURE DUCTILE
C---------------------------
      IF(PDAM==ONE.AND.CODVERS>=42)THEN
        DO I=LFT,LLT
          IF(OFF(I)<EM01) OFF(I)=ZERO
          IF(OFF(I)<ONE) OFF(I)=OFF(I)*FOUR_OVER_5
        ENDDO
C
        DO I=LFT,LLT
          IF(EPMX(I)/=ZERO.AND.OFF(I)>=ONE.AND. EPXE(I)>EPMX(I))THEN
            OFF(I)=OFF(I)*FOUR_OVER_5
            II=I+NFT
#include "lockon.inc"
              WRITE(IOUT,1000) NGL(I)
#include "lockoff.inc"
            IDEL7NOK = 1
          ENDIF
        ENDDO
      ENDIF

      DTA = HALF*DT1

      DO I=LFT,LLT
       SIG(I,1)=(SIG(I,1)-PNEW(I))*OFF(I)
       SIG(I,2)=(SIG(I,2)-PNEW(I))*OFF(I)
       SIG(I,3)=(SIG(I,3)-PNEW(I))*OFF(I)
       SIG(I,4)= SIG(I,4)*OFF(I)
       SIG(I,5)= SIG(I,5)*OFF(I)
       SIG(I,6)= SIG(I,6)*OFF(I)
       E1=D1(I)*(SOLD1(I)+SIG(I,1))
       E2=D2(I)*(SOLD2(I)+SIG(I,2))
       E3=D3(I)*(SOLD3(I)+SIG(I,3))
       E4=D4(I)*(SOLD4(I)+SIG(I,4))
       E5=D5(I)*(SOLD5(I)+SIG(I,5))
       E6=D6(I)*(SOLD6(I)+SIG(I,6))
       EINC= VOL_AVG(I)*(E1+E2+E3+E4+E5+E6)*DTA - HALF*DVOL(I)*(QOLD(I)+QNEW(I))
       EINT(I)=(EINT(I)+EINC*OFF(I)) / MAX(EM15,VOL(I))
       QOLD(I)=QNEW(I)
      ENDDO

      DO I=LFT,LLT
       DEFP(I)=EPXE(I)
       SIGY(I)=MAX(SIGY(I),AK(I))
      ENDDO

 1000 FORMAT(1X,'RUPTURE OF SOLID ELEMENT NUMBER ',I10)
      RETURN
      END
