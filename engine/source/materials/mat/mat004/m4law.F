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
C|============================================================
C|   M4LAW                                 /materb/m4law.F
C|------------------------------------------------------------
C|-- appelee par -----------
C|         MMAIN                           /matera/mmain.F
C|-- appelle ---------------
C|============================================================
Chd|====================================================================
Chd|  M4LAW                         source/materials/mat/mat004/m4law.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M4LAW(PM,OFF     ,SIG     ,EPXE    ,MAT     ,
     2           TEMPER  ,SSP     ,D1      ,D2      ,D3      ,
     3           D4      ,D5      ,D6      ,RHO0    ,DPDM    ,
     4           EPD     ,IPLA    ,SIGY    ,DEFP    ,DPLA    ,
     5           EPSP    ,TSTAR   ,TEMPEL  ,NEL     )     
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
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(MVSIZ),NEL
C     REAL ou REAL*8
      my_real
     .   PM(NPROPM,*), OFF(MVSIZ), SIG(NEL,6), EPXE(MVSIZ),
     .   TEMPER(MVSIZ),SSP(MVSIZ), D1(MVSIZ),    D2(MVSIZ), 
     .   D3(MVSIZ),    D4(MVSIZ),  D5(MVSIZ),    D6(MVSIZ), 
     .   RHO0(MVSIZ),  DPDM(MVSIZ),EPD(MVSIZ),   SIGY(MVSIZ),
     .   DEFP(MVSIZ),  DPLA(MVSIZ),TSTAR(MVSIZ), EPSP(MVSIZ),
     .   TEMPEL(MVSIZ)
      INTEGER IPLA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX
C     REAL ou REAL*8
      my_real
     .   G(MVSIZ)    ,G1(MVSIZ)   ,G2(MVSIZ)  ,QS(MVSIZ) ,AK(MVSIZ), 
     .   QH(MVSIZ)   ,TMELT(MVSIZ),AJ2(MVSIZ) ,DAV(MVSIZ),P(MVSIZ) ,
     .   EPMX(MVSIZ) ,THETL(MVSIZ),CA(MVSIZ)  ,CB(MVSIZ) ,CC(MVSIZ), 
     .   CN(MVSIZ)   ,EPXO(MVSIZ) ,EPDR(MVSIZ),CMX(MVSIZ),SIGMX(MVSIZ),
     .   SCALE(MVSIZ),THETA(MVSIZ), T0(MVSIZ),
     .   CT, CE, CH
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C--------------------------------------------------------------
C
        
      DO I=LFT,LLT
        MX =MAT(I)
        G(I)    =PM(22,MX)
        CA(I)   =PM(38,MX)
        CB(I)   =PM(39,MX)
        CN(I)   =PM(40,MX)
        EPMX(I) =PM(41,MX)
        SIGMX(I)=PM(42,MX)
        CC(I)   =PM(43,MX)
        EPDR(I) =PM(44,MX)
        CMX(I)  =PM(45,MX)
        THETL(I)=PM(47,MX)
        TMELT(I)=PM(46,MX)
        T0(I)=PM(79,MX)
      ENDDO
C
      DO I=LFT,LLT
        P(I)  =-THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
        DAV(I)=-THIRD*(D1(I)+D2(I)+D3(I))
        G1(I)=DT1*G(I)*OFF(I)
        G2(I)=DEUX*G1(I)*OFF(I)
      ENDDO
C-----------------------
C     SOUND SPEED
C-----------------------
      DO I=LFT,LLT
        DPDM(I) = DPDM(I) + UNP333*G(I)
        SSP(I)=SQRT(ABS(DPDM(I))/RHO0(I))
      ENDDO
C-------------------------------
C     CONTRAINTES DEVIATORIQUES
C-------------------------------
      DO I=LFT,LLT
        SIG(I,1)=SIG(I,1)+P(I)+G2(I)*(D1(I)+DAV(I))
        SIG(I,2)=SIG(I,2)+P(I)+G2(I)*(D2(I)+DAV(I))
        SIG(I,3)=SIG(I,3)+P(I)+G2(I)*(D3(I)+DAV(I))
        SIG(I,4)=SIG(I,4)+G1(I)*D4(I)
        SIG(I,5)=SIG(I,5)+G1(I)*D5(I)
        SIG(I,6)=SIG(I,6)+G1(I)*D6(I)
      ENDDO
C
      DO I=LFT,LLT
        EPD(I)=OFF(I)*MAX(   ABS(D1(I)),   ABS(D2(I)),   ABS(D3(I)),
     .         UNDEMI*ABS(D4(I)),UNDEMI*ABS(D5(I)),UNDEMI*ABS(D6(I)))
C
        EPSP(I) = EPD(I)     
        AJ2(I)=UNDEMI*(SIG(I,1)**2+SIG(I,2)**2+SIG(I,3)**2)
     .                +SIG(I,4)**2+SIG(I,5)**2+SIG(I,6)**2
        AJ2(I)=SQRT(TROIS*AJ2(I))
      ENDDO
C-----------------
C     TEMPERATURE
C-----------------
      IF(JTHE >= 0) THEN
        DO I=LFT,LLT
           THETA(I)=TEMPER(I)
           IF(THETA(I).GT.T0(I)) TSTAR(I)=(THETA(I)- T0(I))/(TMELT(I)-T0(I))
        ENDDO
      ELSE
        DO I=LFT,LLT
           THETA(I)=TEMPEL(I)
        ENDDO
      ENDIF
C-------------------------------
C     CRITERE
C-------------------------------
      DO I=LFT,LLT
       IF(THETA(I).GE.TMELT(I)) THEN
        AJ2(I)=ZERO
        QH(I)=ZERO
        AK(I)=ZERO
        SCALE(I)=ZERO
        CYCLE
       ELSEIF(THETA(I).GT.T0(I)) THEN
        IF(THETA(I).GT.THETL(I)) CMX(I)=UN
        CT=UN - TSTAR(I)**CMX(I)
       ELSE
        TSTAR(I) = ZERO
        CT=UN
       ENDIF
C
       IF(EPD(I).LE.EPDR(I)) THEN
        CE=UN
       ELSE
        CE=UN + CC(I) * LOG(EPD(I)/EPDR(I))
       ENDIF
C
       IF(EPXE(I).LE.ZERO) THEN
        CH=CA(I)
       ELSEIF(EPXE(I).GT.EPMX(I)) THEN
        CH=ZERO
       ELSE
        CH=CA(I)+CB(I)*EPXE(I)**CN(I)
       ENDIF
C
       AK(I) = MIN(SIGMX(I),CH)*CE*CT
C-----------------------
C     MODULE ECROUISSAGE
C-----------------------
       IF(CN(I).GE.UN) THEN
        QH(I)= (CB(I)*CN(I)*EPXE(I)**(CN(I) - UN))*CE*CT
       ELSEIF(EPXE(I).GT.ZERO)THEN
        QH(I)= (CB(I)*CN(I)/EPXE(I)**(UN -CN(I)))*CE*CT
       ELSE
        QH(I)=ZERO
       ENDIF
C-------------------------------
C     PROJECTION
C-------------------------------
       IF(AJ2(I).LE.AK(I)) THEN
        SCALE(I)=1.
       ELSEIF(AJ2(I).NE.ZERO) THEN
        SCALE(I)=AK(I)/AJ2(I)
       ELSE
        SCALE(I)=ZERO
       ENDIF
      ENDDO
C
      IF(IPLA.EQ.0)THEN
       DO I=LFT,LLT
       SIG(I,1)=SCALE(I)*SIG(I,1)
       SIG(I,2)=SCALE(I)*SIG(I,2)
       SIG(I,3)=SCALE(I)*SIG(I,3)
       SIG(I,4)=SCALE(I)*SIG(I,4)
       SIG(I,5)=SCALE(I)*SIG(I,5)
       SIG(I,6)=SCALE(I)*SIG(I,6)
       DPLA(I) =(UN -SCALE(I))*AJ2(I)/(TROIS*G(I)+QH(I))      
       EPXE(I)=EPXE(I)+ DPLA(I)
       EPXE(I)=EPXE(I)*OFF(I)
      ENDDO
C
      ELSEIF(IPLA.EQ.2)THEN
       DO I=LFT,LLT
       SIG(I,1)=SCALE(I)*SIG(I,1)
       SIG(I,2)=SCALE(I)*SIG(I,2)
       SIG(I,3)=SCALE(I)*SIG(I,3)
       SIG(I,4)=SCALE(I)*SIG(I,4)
       SIG(I,5)=SCALE(I)*SIG(I,5)
       SIG(I,6)=SCALE(I)*SIG(I,6)
       DPLA(I) =(UN -SCALE(I))*AJ2(I)/(TROIS*G(I))      
       EPXE(I)=EPXE(I)+DPLA(I)
       EPXE(I)=EPXE(I)*OFF(I)
       ENDDO
C
      ELSEIF(IPLA.EQ.1)THEN
       DO I=LFT,LLT
Clm    plastic strain increment.
       DPLA(I)=(UN -SCALE(I))*AJ2(I)/(TROIS*G(I)+QH(I))
Clm    actual yeld stress.
       AK(I)=AK(I)+DPLA(I)*QH(I)
       SCALE(I)= MIN(UN,AK(I)/ MAX(AJ2(I),EM15))
       SIG(I,1)=SCALE(I)*SIG(I,1)
       SIG(I,2)=SCALE(I)*SIG(I,2)
       SIG(I,3)=SCALE(I)*SIG(I,3)
       SIG(I,4)=SCALE(I)*SIG(I,4)
       SIG(I,5)=SCALE(I)*SIG(I,5)
       SIG(I,6)=SCALE(I)*SIG(I,6)
       EPXE(I)=EPXE(I)+DPLA(I)
       EPXE(I)=EPXE(I)*OFF(I)
       ENDDO
      ENDIF
C
      DO I=LFT,LLT
       SIGY(I)=AK(I)
       DEFP(I)=EPXE(I)
      ENDDO 
C
      RETURN
      END
