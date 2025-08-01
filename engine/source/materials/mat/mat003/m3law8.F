Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    m3law8         ../engine/source/materials/mat/mat003/m3law8.F
!||--- called by ------------------------------------------------------
!||    mmain8         ../engine/source/materials/mat_share/mmain8.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod   ../common_source/modules/mat_elem/elbufdef_mod.F90
!||====================================================================
      SUBROUTINE M3LAW8(
     1   PM,      OFF,     SIG,     EPSEQ,
     2   EINT,    RHO,     D1,      D2,
     3   D3,      D4,      D5,      D6,
     4   VNEW,    VOLGP,   DVOL,    MAT,
     5   NGL,     IPLA,    DPLA,    EPD,
     6   TSTAR,   BUFLY,   NEL,     NPT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD         
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
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NPT
      INTEGER MAT(MVSIZ),NGL(MVSIZ),IPLA,NEL
C     REAL
      my_real
     .   PM(NPROPM,*), OFF(MVSIZ), SIG(NEL,6),EPSEQ(NEL),
     .   EINT(NEL) , RHO(NEL),
     .   D1(MVSIZ,*), D2(MVSIZ,*), D3(MVSIZ,*) ,
     .   D4(MVSIZ,*), D5(MVSIZ,*), D6(MVSIZ,*) ,
     .   VNEW(MVSIZ), VOLGP(MVSIZ,*),DVOL(MVSIZ),
     .   DPLA(*),TSTAR(*),EPD(*)
      TYPE (BUF_LAY_), TARGET :: BUFLY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,II,IPT,JPT,IWR,MX,JJ(6)
C     REAL
      my_real
     .  SOLD1(MVSIZ,8),SOLD2(MVSIZ,8),SOLD3(MVSIZ,8),
     .  SOLD4(MVSIZ,8),SOLD5(MVSIZ,8),SOLD6(MVSIZ,8),
     .      G,   G1, G2,
     .   EPMX,SIGMX,POLD(MVSIZ),
     .     CA,   CB, CN,   QH(MVSIZ),
     .    AJ2(MVSIZ),  SJ2(MVSIZ),YLD(MVSIZ),
     .   DAV,DTA, SCALE
      my_real,
     .  DIMENSION(:), POINTER :: SIGP, EPLA
      TYPE(L_BUFEL_) ,POINTER :: LBUF
C=======================================================================
      MX  =MAT(1)
      G   =PM(22,MX)
      CA  =PM(38,MX)
      CB  =PM(39,MX)
      CN  =PM(40,MX)
      EPMX=PM(41,MX)
      SIGMX=PM(42,MX)    

C
      DO I=1,NEL
        POLD(I)=(SIG(I,1)+SIG(I,2)+SIG(I,3))*THIRD
      ENDDO
C
      G1=DT1*G
      G2=TWO*G1
      DO I=1,NEL
        EPSEQ(I)=ZERO
        TSTAR(I) = ZERO       
      ENDDO
C
      DO J=1,6
        JJ(J) = NEL*(J-1)
      ENDDO
C--------------------------------------------------
C     BOUCLE 1 SUR LES POINTS DE GAUSS
C--------------------------------------------------
      DO IPT=1,NPT
        LBUF => BUFLY%LBUF(1,1,IPT)
        SIGP => BUFLY%LBUF(1,1,IPT)%SIG(1:NEL*6)
        EPLA => BUFLY%LBUF(1,1,IPT)%PLA(1:NEL)
        JPT=(IPT-1)*NEL
        DO I=1,NEL
         II=I+JPT
         SIGP(JJ(1)+I) = SIGP(JJ(1)+I)-POLD(I) 
         SIGP(JJ(2)+I) = SIGP(JJ(2)+I)-POLD(I) 
         SIGP(JJ(3)+I) = SIGP(JJ(3)+I)-POLD(I) 
         DAV = ONE-DVOL(I)/VNEW(I)
         SOLD1(I,IPT)=SIGP(JJ(1)+I)*DAV
         SOLD2(I,IPT)=SIGP(JJ(2)+I)*DAV
         SOLD3(I,IPT)=SIGP(JJ(3)+I)*DAV
         SOLD4(I,IPT)=SIGP(JJ(4)+I)*DAV
         SOLD5(I,IPT)=SIGP(JJ(5)+I)*DAV
         SOLD6(I,IPT)=SIGP(JJ(6)+I)*DAV
         EPD(II)=OFF(I)* 
     .       MAX( ABS(D1(I,IPT)), ABS(D2(I,IPT)), ABS(D3(I,IPT)),
     .       HALF*ABS(D4(I,IPT)),
     .       HALF*ABS(D5(I,IPT)),HALF*ABS(D6(I,IPT)))        
        ENDDO
C
        DO I=1,NEL
          DAV=-THIRD*(D1(I,IPT)+D2(I,IPT)+D3(I,IPT))
          SIGP(JJ(1)+I)=SIGP(JJ(1)+I)+G2*(D1(I,IPT)+DAV)
          SIGP(JJ(2)+I)=SIGP(JJ(2)+I)+G2*(D2(I,IPT)+DAV)
          SIGP(JJ(3)+I)=SIGP(JJ(3)+I)+G2*(D3(I,IPT)+DAV)
          SIGP(JJ(4)+I)=SIGP(JJ(4)+I)+G1* D4(I,IPT)
          SIGP(JJ(5)+I)=SIGP(JJ(5)+I)+G1* D5(I,IPT)
          SIGP(JJ(6)+I)=SIGP(JJ(6)+I)+G1* D6(I,IPT)
        ENDDO
C---------------------
C     LIMITE PLASTIQUE
C---------------------
        DO I=1,NEL
          YLD(I)= MIN(SIGMX,CA+CB*EPLA(I)**CN)
        ENDDO
C-----------------------
C       MODULE ECROUISSAGE
C-----------------------
        DO I=1,NEL
         II=I+JPT
         IF(CN==ONE) THEN
          QH(I)= CB
         ELSE
          IF(CN>ONE) THEN
           QH(I)= CB*CN*EPLA(I)**(CN-ONE)
          ELSE
           IF(EPLA(I)/=ZERO)THEN
            QH(I)= CB*CN/EPLA(I)**(ONE - CN)
           ELSE
            QH(I)=ZERO
           ENDIF
          ENDIF
         ENDIF
        ENDDO
C
        DO I=1,NEL
          J = (I-1)*6
          AJ2(I)=HALF*(SIGP(JJ(1)+I)**2+SIGP(JJ(2)+I)**2+SIGP(JJ(3)+I)**2)
     .                  +SIGP(JJ(4)+I)**2+SIGP(JJ(5)+I)**2+SIGP(JJ(6)+I)**2
          SJ2(I)=SQRT(THREE*AJ2(I))
        ENDDO
C
      IF(IPLA==0)THEN
       DO I=1,NEL
        II=I+JPT
        IF (YLD(I)==ZERO) CYCLE
          SCALE= MIN(ONE,YLD(I)/ MAX(SJ2(I),EM15))
          SIGP(JJ(1)+I)=SCALE*SIGP(JJ(1)+I)
          SIGP(JJ(2)+I)=SCALE*SIGP(JJ(2)+I)
          SIGP(JJ(3)+I)=SCALE*SIGP(JJ(3)+I)
          SIGP(JJ(4)+I)=SCALE*SIGP(JJ(4)+I)
          SIGP(JJ(5)+I)=SCALE*SIGP(JJ(5)+I)
          SIGP(JJ(6)+I)=SCALE*SIGP(JJ(6)+I)
          EPLA(I) = EPLA(I)+(ONE-SCALE)*SJ2(I)/(THREE*G+QH(I))
          DPLA(II)= (ONE-SCALE)*SJ2(I)/(THREE*G+QH(I))       
        ENDDO
      ELSEIF(IPLA==2)THEN
       DO 110 I=1,NEL
        II=I+JPT
        IF(YLD(I)==ZERO)    GO TO 110
          SCALE= MIN(ONE,YLD(I)/ MAX(SJ2(I),EM15))
          SIGP(JJ(1)+I)=SCALE*SIGP(JJ(1)+I)
          SIGP(JJ(2)+I)=SCALE*SIGP(JJ(2)+I)
          SIGP(JJ(3)+I)=SCALE*SIGP(JJ(3)+I)
          SIGP(JJ(4)+I)=SCALE*SIGP(JJ(4)+I)
          SIGP(JJ(5)+I)=SCALE*SIGP(JJ(5)+I)
          SIGP(JJ(6)+I)=SCALE*SIGP(JJ(6)+I)
          EPLA(I)=EPLA(I)+(ONE-SCALE)*SJ2(I)/(THREE*G)
          DPLA(II)=(ONE-SCALE)*SJ2(I)/(THREE*G)         
  110  CONTINUE
      ELSEIF(IPLA==1)THEN
       DO 120 I=1,NEL
        II=I+JPT
        IF(YLD(I)==ZERO)    GO TO 120
        SCALE= MIN(ONE,YLD(I)/ MAX(SJ2(I),EM15))
C       plastic strain increment.
        DPLA(II)=(ONE - SCALE)*SJ2(I)/(THREE*G+QH(I))
C       actual yield stress.
        YLD(I)=YLD(I)+DPLA(II)*QH(I)
        SCALE= MIN(ONE,YLD(I)/ MAX(SJ2(I),EM15))
        SIGP(JJ(1)+I)=SCALE*SIGP(JJ(1)+I)
        SIGP(JJ(2)+I)=SCALE*SIGP(JJ(2)+I)
        SIGP(JJ(3)+I)=SCALE*SIGP(JJ(3)+I)
        SIGP(JJ(4)+I)=SCALE*SIGP(JJ(4)+I)
        SIGP(JJ(5)+I)=SCALE*SIGP(JJ(5)+I)
        SIGP(JJ(6)+I)=SCALE*SIGP(JJ(6)+I)
        EPLA(I)=EPLA(I)+DPLA(II)      
 120  CONTINUE
      ENDIF
C--------------------------------------------------
C     EPS PLASTIQUE MOYEN (OUTPUT ET RUPTURE)
C--------------------------------------------------
        DO I=1,NEL
          EPSEQ(I)=EPSEQ(I)+ONE_OVER_8*EPLA(I)
        ENDDO
C
      ENDDO ! DO IPT=1,NPT
C FIN BOUCLE 1 PT DE GAUSS
C----------------------------
C     TEST DE RUPTURE DUCTILE
C---------------------------
      DO I=1,NEL
        IF(OFF(I)<EM01) OFF(I)=ZERO
        IF(OFF(I)<ONE) OFF(I)=OFF(I)*FOUR_OVER_5
      ENDDO
C
      IWR=0
      DO I=1,NEL
        IF(EPMX ==ZERO)    CYCLE
        IF(OFF(I)  <ONE)      CYCLE
        IF(EPSEQ(I)<EPMX) CYCLE
        IWR=1
      ENDDO
      IF(IWR==1) THEN
        DO I=1,NEL
          IF(EPMX ==ZERO)    CYCLE
          IF(OFF(I)  <ONE)      CYCLE
          IF(EPSEQ(I)<EPMX) CYCLE
          OFF(I)=OFF(I)*FOUR_OVER_5
#include "lockon.inc"
          WRITE(IOUT,1000) NGL(I)
#include "lockoff.inc"
        ENDDO
      ENDIF
      DTA=HALF*DT1
C--------------------------------------------------
C     BOUCLE 2 SUR LES POINTS DE GAUSS
C--------------------------------------------------
      DO IPT=1,NPT
        LBUF => BUFLY%LBUF(1,1,IPT)
        SIGP => BUFLY%LBUF(1,1,IPT)%SIG(1:NEL*6)
        EPLA => BUFLY%LBUF(1,1,IPT)%PLA(1:NEL)
        JPT=(IPT-1)*NEL
C--------------------------------------------------
C       MISE A OFF AUX POINTS DE GAUSS
C--------------------------------------------------
        DO I=1,NEL
          SIGP(JJ(1)+I)=SIGP(JJ(1)+I)*OFF(I)
          SIGP(JJ(2)+I)=SIGP(JJ(2)+I)*OFF(I)
          SIGP(JJ(3)+I)=SIGP(JJ(3)+I)*OFF(I)
          SIGP(JJ(4)+I)=SIGP(JJ(4)+I)*OFF(I)
          SIGP(JJ(5)+I)=SIGP(JJ(5)+I)*OFF(I)
          SIGP(JJ(6)+I)=SIGP(JJ(6)+I)*OFF(I)
        ENDDO
C--------------------------------------------------
C     ENERGIE INTERNE DEVIATORIQUE
C--------------------------------------------------
        DO I=1,NEL
          DAV=VOLGP(I,IPT)*OFF(I)*DTA
          EINT(I)=EINT(I)+DAV*(D1(I,IPT)*(SOLD1(I,IPT)+SIGP(JJ(1)+I))+
     +                         D2(I,IPT)*(SOLD2(I,IPT)+SIGP(JJ(2)+I))+
     +                         D3(I,IPT)*(SOLD3(I,IPT)+SIGP(JJ(3)+I))+
     +                         D4(I,IPT)*(SOLD4(I,IPT)+SIGP(JJ(4)+I))+
     +                         D5(I,IPT)*(SOLD5(I,IPT)+SIGP(JJ(5)+I))+
     +                         D6(I,IPT)*(SOLD6(I,IPT)+SIGP(JJ(6)+I)))
        ENDDO
C
      ENDDO  ! IPT=1,NPT
C-----------
 1000 FORMAT(1X,'RUPTURE OF SOLID ELEMENT NUMBER ',I10)
C-----------
      RETURN
      END
