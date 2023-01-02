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
C|============================================================
C|   M3LAW                           /matera/m3law.F
C|------------------------------------------------------------
C|-- appelee par -----------
C|         MMAIN                           /matera/mmain.F
C|-- appelle ---------------
C|============================================================
Chd|====================================================================
Chd|  M3LAW                         source/materials/mat/mat003/m3law.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M3LAW(PM,OFF     ,SIG     ,EPSEQ   ,MAT     ,
     2           NGL     ,SSP     ,D1      ,D2      ,D3      ,
     3           D4      ,D5      ,D6      ,RHO0    ,DPDM    ,
     4           IPLA    ,SIGY    ,DEFP    ,DPLA1   ,NEL     )
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
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(MVSIZ),NGL(MVSIZ),NEL
      my_real
     .   PM(NPROPM,*), OFF(MVSIZ), SIG(NEL,6), EPSEQ(MVSIZ),
     .   SSP(MVSIZ), D1(MVSIZ), D2(MVSIZ), D3(MVSIZ), D4(MVSIZ),   
     .   D5(MVSIZ), D6(MVSIZ), RHO0(MVSIZ), DPDM(MVSIZ),
     .   SIGY(MVSIZ), DEFP(MVSIZ), DPLA1(MVSIZ)
      INTEGER IPLA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX, II, LIST(MVSIZ), K
      my_real
     .   G(MVSIZ), G1(MVSIZ), G2(MVSIZ), QS(MVSIZ), YLD(MVSIZ), 
     .   QH(MVSIZ), AJ2(MVSIZ), DAV(MVSIZ), SJ2(MVSIZ), P(MVSIZ), 
     .   EPMX(MVSIZ), CA(MVSIZ), CB(MVSIZ), CN(MVSIZ), SIGMX(MVSIZ), 
     .   SCALE, DPLA, G11, CA11, CB11, CN11, EPMX11,GDT,GGDT,
     .   SIGMX11
C-----------------------------------------------
C
      MX=MAT(LFT)
      G11=PM(22,MX)
      CA11=PM(38,MX)
      CB11=PM(39,MX)
      CN11=PM(40,MX)
      EPMX11=PM(41,MX)
      SIGMX11=PM(42,MX)
      
      GDT=DT1*G11
      GGDT=TWO*GDT
       
      ! [D] = [EPS_DOT]
      !   D1 = EPS_DOT(x,x)
      !   D2 = EPS_DOT(y,y)
      !   D3 = EPS_DOT(z,z)
      !   D4 = EPS_DOT(x,y) + EPS_DOT(y,x)
      !   D5 = EPS_DOT(y,z) + EPS_DOT(z,y)
      !   D6 = EPS_DOT(x,z) + EPS_DOT(z,x)
      DO I=LFT,LLT
       P(I)  =-THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
       DAV(I)=-THIRD*(D1(I)+D2(I)+D3(I))
      ENDDO
C
      !ELASTIC PREDICTION : S_prediction = S_old + 2 mu de
      !where [e_dot] = [E_dot]_dev 
      ! [de] = dt.[e_dot]
      ! mu = G
      !
      ! 2mu de_11 = 2G dt.e_dot_11 = 2G dt (EPS_DOT_11-DAV) = 2G dt (D1-DAV)
      ! 2mu de_12 = 2G dt.e_dot_12 = 2G dt D4/2 = G dt D4
      !
      
      DO I=LFT,LLT            
       SIG(I,1)=SIG(I,1)+P(I)+GGDT*(D1(I)+DAV(I))
       SIG(I,2)=SIG(I,2)+P(I)+GGDT*(D2(I)+DAV(I))
       SIG(I,3)=SIG(I,3)+P(I)+GGDT*(D3(I)+DAV(I))
       SIG(I,4)=SIG(I,4)+GDT*D4(I)
       SIG(I,5)=SIG(I,5)+GDT*D5(I)
       SIG(I,6)=SIG(I,6)+GDT*D6(I)
      ENDDO
C---------------------
C     LIMITE PLASTIQUE
C---------------------
      DO I=LFT,LLT
       YLD(I)= MIN(SIGMX11,CA11+CB11*MAX(ZERO,EPSEQ(I))**CN11)
      ENDDO
C-----------------------
C     MODULE ECROUISSAGE
C-----------------------
      IF(CN11==ONE)THEN
        DO I=LFT,LLT
          QH(I)= CB11
        ENDDO   
      ELSEIF(CN11>ONE)THEN
        DO I=LFT,LLT
          QH(I)= CB11*CN11*MAX(ZERO,EPSEQ(I))**(CN11- ONE)
        ENDDO    
      ELSE
        DO I=LFT,LLT
          IF(EPSEQ(I)/=ZERO)THEN
           QH(I)= CB11*CN11/MAX(ZERO,EPSEQ(I))**(ONE-CN11)
          ELSE
           QH(I)=ZERO
          ENDIF              
        ENDDO
      ENDIF

C---------------------------------
C     SOUND SPEED
C---------------------------------
      DO I=LFT,LLT
       DPDM(I)=DPDM(I)+ONEP333*G11
       SSP(I)=SQRT(ABS(DPDM(I))/RHO0(I))
      ENDDO
      DO I=LFT,LLT
       AJ2(I)=HALF*(SIG(I,1)**2+SIG(I,2)**2+SIG(I,3)**2)+SIG(I,4)**2+SIG(I,5)**2+SIG(I,6)**2
       SJ2(I)=SQRT(THREE*AJ2(I))
      ENDDO
      K=0
      ! sublist to avoir CYCLE on YLD(I)==ZERO
      DO I=LFT,LLT
        IF(YLD(I)/=ZERO)THEN
          K=K+1
          LIST(K)=I
        ENDIF
      ENDDO
      IF(IPLA==0)THEN
       DO II=1,K
        I=LIST(II)
        SCALE= MIN(ONE,YLD(I)/ MAX(SJ2(I),EM15))
        SIG(I,1)=SCALE*SIG(I,1)
        SIG(I,2)=SCALE*SIG(I,2)
        SIG(I,3)=SCALE*SIG(I,3)
        SIG(I,4)=SCALE*SIG(I,4)
        SIG(I,5)=SCALE*SIG(I,5)
        SIG(I,6)=SCALE*SIG(I,6)
        DPLA1(I) = (ONE -SCALE)*SJ2(I)/MAX(THREE*G11+QH(I),EM15)    
        EPSEQ(I)=EPSEQ(I)+ DPLA1(I)
       ENDDO
      ELSEIF(IPLA==2)THEN
      DO II=1,K
        I=LIST(II)
        SCALE= MIN(ONE,YLD(I)/ MAX(SJ2(I),EM15))
        SIG(I,1)=SCALE*SIG(I,1)
        SIG(I,2)=SCALE*SIG(I,2)
        SIG(I,3)=SCALE*SIG(I,3)
        SIG(I,4)=SCALE*SIG(I,4)
        SIG(I,5)=SCALE*SIG(I,5)
        SIG(I,6)=SCALE*SIG(I,6)
        DPLA1(I) = (ONE -SCALE)*SJ2(I)/MAX(THREE*G11,EM15)     
        EPSEQ(I)=EPSEQ(I)+DPLA1(I)
       ENDDO
      ELSEIF(IPLA==1)THEN
      DO II=1,K
        I=LIST(II)
        SCALE= MIN(ONE,YLD(I)/ MAX(SJ2(I),EM15))
        !--plastic strain increment.
        DPLA=(ONE -SCALE)*SJ2(I)/MAX(THREE*G11+QH(I),EM15) 
        !--actual yield stress.
        YLD(I)=YLD(I)+DPLA*QH(I)
        SCALE= MIN(ONE,YLD(I)/ MAX(SJ2(I),EM15))
        SIG(I,1)=SCALE*SIG(I,1)
        SIG(I,2)=SCALE*SIG(I,2)
        SIG(I,3)=SCALE*SIG(I,3)
        SIG(I,4)=SCALE*SIG(I,4)
        SIG(I,5)=SCALE*SIG(I,5)
        SIG(I,6)=SCALE*SIG(I,6)
        EPSEQ(I)=EPSEQ(I)+DPLA
        DPLA1(I) = DPLA
       ENDDO
      ENDIF
C----------------------------
C     TEST DE RUPTURE DUCTILE
C---------------------------
      DO I=LFT,LLT
       IF(OFF(I)<EM01) OFF(I)=ZERO
       IF(OFF(I)<ONE) OFF(I)=OFF(I)*FOUR_OVER_5
      ENDDO
      DO I=LFT,LLT
       IF(EPMX11==ZERO .OR. OFF(I)<ONE .OR. EPSEQ(I)<EPMX11) CYCLE
       OFF(I)=OFF(I)*FOUR_OVER_5
       II=I+NFT
#include "lockon.inc"
       WRITE(IOUT,1000) NGL(I)
#include "lockoff.inc"
       IDEL7NOK = 1
      ENDDO
      DO I=LFT,LLT
       SIG(I,1)=SIG(I,1)*OFF(I)
       SIG(I,2)=SIG(I,2)*OFF(I)
       SIG(I,3)=SIG(I,3)*OFF(I)
       SIG(I,4)=SIG(I,4)*OFF(I)
       SIG(I,5)=SIG(I,5)*OFF(I)
       SIG(I,6)=SIG(I,6)*OFF(I)
      ENDDO
      DO I=LFT,LLT
       SIGY(I)=YLD(I)
       DEFP(I)=EPSEQ(I)
      ENDDO 
 1000 FORMAT(1X,'RUPTURE OF SOLID ELEMENT NUMBER ',I10)
      RETURN
      END
