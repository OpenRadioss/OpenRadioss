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
Chd|  MHVIS3                        source/airbag/mhvis3.F        
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MHVIS3(JFT,JLT ,PM ,THK,HOUR,
     2                  OFF,PX1 ,PX2,PY1,PY2 ,
     3                  IXC,DT1C,SSP,RHO,STI ,
     4                  EANI,GEO ,PID,STIR,MAT,
     5                  THK0,VISCMX,ALPE,IPARTC ,PARTSAV,
     6                  IHBE  ,NFT ,ISMSTR , RX1,    
     7                  RX2,RX3,RX4,RY1,RY2,
     8                  RY3,RY4,VX1,VX2,VX3,
     9                  VX4,VY1,VY2,VY3,VY4,
     A                  VZ1,VZ2,VZ3,VZ4,B11,
     B                  B12,B13,B14,B21,B22,
     C                  B23,B24,AREA,YM,PR,
     D                  VHX,VHY,H11,H12,H13,
     E                  H14,H21,H22,H23,H24,
     F                  H31,H32,H33,H34,H1 , 
     G                  H2,IGEO,NEL)
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr02_c.inc"
#include      "scr06_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*),IPARTC(*), JFT, JLT,PID(*),
     .        IHBE  ,NFT ,ISMSTR,IGEO(NPROPGI, *),NEL
      INTEGER MAT(MVSIZ) 
C     REAL
      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*), THK(*), HOUR(NEL,5), OFF(*),
     .   PX1(*), PX2(*), PY1(*), PY2(*),DT1C(*),EANI(*),
     .   SSP(MVSIZ), RHO(MVSIZ),STI(MVSIZ),STIR(*),
     .   H1(MVSIZ), H2(MVSIZ),
     .   THK0(MVSIZ),VISCMX(MVSIZ), ALPE(MVSIZ),PARTSAV(NPSAV,*)
C     REAL
      my_real
     .   B11(MVSIZ), B12(MVSIZ), B13(MVSIZ), B14(MVSIZ), B21(MVSIZ),
     .   B22(MVSIZ), B23(MVSIZ), B24(MVSIZ), H11(MVSIZ), H12(MVSIZ),
     .   H13(MVSIZ), H14(MVSIZ), H21(MVSIZ), H22(MVSIZ), H23(MVSIZ),
     .   H24(MVSIZ), H31(MVSIZ), H32(MVSIZ), H33(MVSIZ), H34(MVSIZ),
     .   RX1(MVSIZ), RX2(MVSIZ), RX3(MVSIZ), RX4(MVSIZ), RY1(MVSIZ),
     .   RY2(MVSIZ), RY3(MVSIZ), RY4(MVSIZ), VHX(MVSIZ), VHY(MVSIZ),
     .   VX1(MVSIZ), VX2(MVSIZ), VX3(MVSIZ),
     .   VX4(MVSIZ), VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), VY4(MVSIZ),
     .   VZ1(MVSIZ), VZ2(MVSIZ), VZ3(MVSIZ), VZ4(MVSIZ), AREA(MVSIZ),
     .   A11R(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX,IPID,IGTYP,IGMAT,IPGMAT
C     REAL
      my_real
     .   BB(MVSIZ),
     .   H1L(MVSIZ), H2L(MVSIZ), H1Q(MVSIZ), H2Q(MVSIZ), HG1(MVSIZ),
     .   HG2(MVSIZ), FAC(MVSIZ), YM(MVSIZ), PR(MVSIZ), 
     .   GAMA1(MVSIZ), GAMA2(MVSIZ), GAMA3(MVSIZ), GAMA4(MVSIZ),
     .   H4(MVSIZ), H4L(MVSIZ), H4Q(MVSIZ),THK02(MVSIZ),
     .   G(MVSIZ) ,  B1(MVSIZ),  B2(MVSIZ),A11(MVSIZ),EHOU(MVSIZ), 
     .   PX1V, PX2V, PY1V, PY2V, EHOURT,VV ,SCALE(MVSIZ),FAC1 
C-----------------------------------------------
      IF(ISMSTR/=3.AND.IHBE>=1)THEN
       DO I=JFT,JLT
         PX1V    = PX1(I)*VHX(I)
         PX2V    = PX2(I)*VHX(I)
         PY1V    = PY1(I)*VHY(I)
         PY2V    = PY2(I)*VHY(I)
         GAMA1(I)= OFF(I)*( ONE- PX1V-PY1V)
         GAMA3(I)= OFF(I)*( ONE+ PX1V+PY1V)
         GAMA2(I)= OFF(I)*(-ONE- PX2V-PY2V)
         GAMA4(I)= OFF(I)*(-ONE+ PX2V+PY2V)	 
       ENDDO
      ELSE
       DO I=JFT,JLT
         GAMA1(I)= OFF(I)
         GAMA3(I)= OFF(I)
         GAMA2(I)= -OFF(I)
         GAMA4(I)= -OFF(I)
       ENDDO
      ENDIF
C
      IF(INVSTR>=35)THEN
       MX=MAT(JFT)
       DO I=JFT,JLT
        IPID=PID(I)
        H4(I) =GEO(17,IPID)
       ENDDO
C
      ELSE
       MX=MAT(JFT)
       DO I=JFT,JLT
        H4(I) =PM(91,MX)
       ENDDO
C
      ENDIF
C
      DO 300 I=JFT,JLT
      FAC(I)=FOURTH*RHO(I)*THK0(I)
      H1L(I)= ZERO
      H1Q(I)= ZERO
      H2L(I)= ZERO
      H2Q(I)= ZERO
      H4L(I)=FAC(I)*SQRT(HVISC*H4(I)*AREA(I))
      H4Q(I)=SQRT(HVISC*H4(I))*H4L(I)*HUNDRED
      H4L(I)=H4L(I)*SSP(I)
C
 300  CONTINUE
C
      DO 350 I=JFT,JLT
       THK02(I)= THK0(I)*THK0(I)
       B1(I) = PX1(I)*PX1(I)+PY1(I)*PY1(I)
       B2(I) = PX2(I)*PX2(I)+PY2(I)*PY2(I)
      FAC(I)=FOURTH*YM(I)*THK0(I)*DT1C(I)*HELAS
      H1(I)=H1(I)*FAC(I)
      H2(I)=H2(I)*FAC(I)
 350  CONTINUE
C-----------------------------------
C     TRIANGLES
C-----------------------------------
      DO 360 I=JFT,JLT
      IF(IXC(4,I)/=IXC(5,I))GOTO 360
       H1Q(I)=ZERO
       H1L(I)=ZERO
       H2Q(I)=ZERO
       H2L(I)=ZERO
       H4L(I)=ZERO
       H4Q(I)=ZERO
       H1(I)=ZERO
       H2(I)=ZERO     
 360  CONTINUE
C-----------------------------------
C     RIGIDITE-DT
C-----------------------------------
      DO I=JFT,JLT
         SCALE(I)= ZERO
        ENDDO
      IPID=PID(1)
      IGTYP = IGEO(11,IPID) 
      IGMAT = IGEO(98,IPID)
      IPGMAT  = 700
      IF(NODADT/=0.OR.IDT1SH==1.OR.IDTMINS==2)THEN
C    
        !!IF(IDTMINS == 2) IGMAT = 1
        DO I=JFT,JLT
             SCALE(I)= MAX(GAMA1(I)*GAMA1(I),GAMA2(I)*GAMA2(I),
     .              GAMA3(I)*GAMA3(I),GAMA4(I)*GAMA4(I)) *
     .        DT1C(I)*MAX(H1(I)+H1L(I),H2(I)+H2L(I),H4L(I)) /
     .        MAX(DT1C(I)*DT1C(I),EM20)
              STI(I)=STI(I) + SCALE(I)
        ENDDO
        IF(IGTYP == 11 .AND. IGMAT > 0) THEN
         DO I=JFT,JLT
            A11(I)  = GEO(IPGMAT +5 ,PID(I))
            G(I)    = GEO(IPGMAT+4,PID(I)) 
            A11R(I) = GEO(IPGMAT+7,PID(I))
            IF (OFF(I)==ZERO) THEN
                STI(I) = ZERO
                STIR(I) = ZERO
            ELSE
                VV = VISCMX(I) * VISCMX(I)  * ALPE(I)
                FAC1 = MAX(B1(I),B2(I)) / (AREA(I) * VV)
                STI(I) =  STI(I) + FAC1* THK0(I) * A11(I) 
                STIR(I) = FAC1 *  A11R(I)*ONE_OVER_12*THK0(I)**3 + 
     .                    FAC1 *  A11(I)*THK0(I)*AREA(I)*ONE_OVER_9 +
     .             FAC1*SCALE(I)*(ONE_OVER_12*THK0(I)**2 +  AREA(I)*ONE_OVER_9)
           ENDIF 
         ENDDO        
!!        
        ELSE  !
          MX = MAT(JFT)        
          DO I=JFT,JLT
           A11(I) =PM(24,MX)
           G(I)  =PM(22,MX)
          ENDDO
!!        
          DO I=JFT,JLT
            IF (OFF(I)==ZERO) THEN
                STI(I) = ZERO
                STIR(I) = ZERO
            ELSE
                VV = VISCMX(I) * VISCMX(I)  * ALPE(I)
                STI(I) =  STI(I) + MAX(B1(I),B2(I)) 
     .                          * THK0(I) * A11(I) / (AREA(I) * VV)
              STIR(I) = STI(I)*(THK02(I) * ONE_OVER_12 + AREA(I) * ONE_OVER_9)
           ENDIF 
         ENDDO
       ENDIF
      ENDIF
C-----------------------------------
C     FORCES ANTISABLIER DE MEMBRANE
C-----------------------------------
      IF(ISMSTR==3.OR.IHBE<1)THEN
       DO 400 I=JFT,JLT
       HG1(I)=VX1(I)-VX2(I)+VX3(I)-VX4(I)
       HG2(I)=VY1(I)-VY2(I)+VY3(I)-VY4(I)
 400   CONTINUE
       DO 500 I=JFT,JLT
       HOUR(I,1)=HOUR(I,1)+HG1(I)*H1(I)
       HOUR(I,2)=HOUR(I,2)+HG2(I)*H1(I)
       HG1(I)=HG1(I)*(H1L(I)+H1Q(I)*ABS(HG1(I)))
       HG2(I)=HG2(I)*(H1L(I)+H1Q(I)*ABS(HG2(I)))
       H11(I)= HOUR(I,1)+HG1(I)
       H12(I)=-HOUR(I,1)-HG1(I)
       H13(I)= HOUR(I,1)+HG1(I)
       H14(I)=-HOUR(I,1)-HG1(I)
       H21(I)= HOUR(I,2)+HG2(I)
       H22(I)=-HOUR(I,2)-HG2(I)
       H23(I)= HOUR(I,2)+HG2(I)
       H24(I)=-HOUR(I,2)-HG2(I)
 500   CONTINUE
      ELSE
       DO 540 I=JFT,JLT
       HG1(I)=VX1(I)*GAMA1(I)+VX2(I)*GAMA2(I)
     .       +VX3(I)*GAMA3(I)+VX4(I)*GAMA4(I)
       HG2(I)=VY1(I)*GAMA1(I)+VY2(I)*GAMA2(I)
     .       +VY3(I)*GAMA3(I)+VY4(I)*GAMA4(I)
 540   CONTINUE
       DO 560 I=JFT,JLT
       HOUR(I,1)=HOUR(I,1)+HG1(I)*H1(I)
       HOUR(I,2)=HOUR(I,2)+HG2(I)*H1(I)
       HG1(I)=HG1(I)*(H1L(I)+H1Q(I)*ABS(HG1(I)))
       HG2(I)=HG2(I)*(H1L(I)+H1Q(I)*ABS(HG2(I)))
       H11(I)=(HOUR(I,1)+HG1(I))*GAMA1(I)
       H12(I)=(HOUR(I,1)+HG1(I))*GAMA2(I)
       H13(I)=(HOUR(I,1)+HG1(I))*GAMA3(I)
       H14(I)=(HOUR(I,1)+HG1(I))*GAMA4(I)
       H21(I)=(HOUR(I,2)+HG2(I))*GAMA1(I)
       H22(I)=(HOUR(I,2)+HG2(I))*GAMA2(I)
       H23(I)=(HOUR(I,2)+HG2(I))*GAMA3(I)
       H24(I)=(HOUR(I,2)+HG2(I))*GAMA4(I)
 560   CONTINUE
      ENDIF
      EHOURT = 0.
      DO I=JFT,JLT
       EHOU(I) = 
     .         VX1(I)*H11(I) + VX2(I)*H12(I) 
     .       + VX3(I)*H13(I) + VX4(I)*H14(I)
     .       + VY1(I)*H21(I) + VY2(I)*H22(I) 
     .       + VY3(I)*H23(I) + VY4(I)*H24(I) 
      ENDDO
C----------------------------------------------------------------
C     FORCES ANTISABLIER DE FLEXION W OU PARABOLOIDE HYPERBOLIQUE
C----------------------------------------------------------------
      IF(ISMSTR==3.OR.IHBE<1)THEN
       DO 600 I=JFT,JLT
       HG1(I)=VZ1(I)-VZ2(I)+VZ3(I)-VZ4(I)
 600   CONTINUE
C
       DO 700 I=JFT,JLT
       HOUR(I,3)=HOUR(I,3)+HG1(I)*H2(I)
       HG1(I)=HG1(I)*(H2L(I)+H2Q(I)*ABS(HG1(I)))
       H31(I)= HOUR(I,3)+HG1(I)
       H32(I)=-HOUR(I,3)-HG1(I)
       H33(I)= HOUR(I,3)+HG1(I)
       H34(I)=-HOUR(I,3)-HG1(I)
 700   CONTINUE
      ELSE
       DO 740 I=JFT,JLT
       HG1(I)=VZ1(I)*GAMA1(I)+VZ2(I)*GAMA2(I)
     .       +VZ3(I)*GAMA3(I)+VZ4(I)*GAMA4(I)
 740   CONTINUE
       DO 760 I=JFT,JLT
       HOUR(I,3)=HOUR(I,3)+HG1(I)*H2(I)
       HG1(I)=HG1(I)*(H2L(I)+H2Q(I)*ABS(HG1(I)))
       H31(I)=(HOUR(I,3)+HG1(I))*GAMA1(I)
       H32(I)=(HOUR(I,3)+HG1(I))*GAMA2(I)
       H33(I)=(HOUR(I,3)+HG1(I))*GAMA3(I)
       H34(I)=(HOUR(I,3)+HG1(I))*GAMA4(I)
 760   CONTINUE
      ENDIF
C------------------------------------------------------
C     FORCES ANTISABLIER DE FLEXION  V OU DENTS DE SCIE
C------------------------------------------------------
       DO 780 I=JFT,JLT
       HG1(I)=+VZ1(I)+VZ2(I)-VZ3(I)-VZ4(I)
 780   CONTINUE
C
       DO 785 I=JFT,JLT
C       HOUR(I,3)=HOUR(I,3)+HG1(I)*H2(I)
       HG1(I)=HG1(I)*(H4L(I)+H4Q(I)*ABS(HG1(I)))
       H31(I)=H31(I) +HG1(I)
       H32(I)=H32(I) +HG1(I)
       H33(I)=H33(I) -HG1(I)
       H34(I)=H34(I) -HG1(I)
 785   CONTINUE
C
       DO 790 I=JFT,JLT
       HG1(I)=VZ1(I)-VZ2(I)-VZ3(I)+VZ4(I)
 790   CONTINUE
C
       DO 795 I=JFT,JLT
C       HOUR(I,3)=HOUR(I,3)+HG1(I)*H2(I)
       HG1(I)=HG1(I)*(H4L(I)+H4Q(I)*ABS(HG1(I)))
       H31(I)=H31(I) +HG1(I)
       H32(I)=H32(I) -HG1(I)
       H33(I)=H33(I) -HG1(I)
       H34(I)=H34(I) +HG1(I)
 795   CONTINUE
C------------------------------------------------
      DO I=JFT,JLT
       B11(I)= ZERO
       B12(I)= ZERO
       B13(I)= ZERO
       B14(I)= ZERO
       B21(I)= ZERO
       B22(I)= ZERO
       B23(I)= ZERO
       B24(I)= ZERO
      ENDDO
C------------------------------------------------
      DO I=JFT,JLT
      EHOU(I) = EHOU(I)  
     .       + VZ1(I)*H31(I) + VZ2(I)*H32(I) 
     .       + VZ3(I)*H33(I) + VZ4(I)*H34(I) 
      EHOU(I) = DT1C(I) * EHOU(I)
      EHOURT = EHOURT + EHOU(I)
      ENDDO
C
      DO I=JFT,JLT
         MX = IPARTC(I)
         PARTSAV(8,MX)=PARTSAV(8,MX) + EHOU(I)
      ENDDO
C
#include "atomic.inc"
        EHOUR = EHOUR + EHOURT
#include "atomend.inc"
C
      IF((ANIM_E(25)==1).OR.(ANIM_CE(25)==1).OR.
     &                 (OUTP_CS(25)==1))THEN
        DO I=JFT,JLT
          EANI(NFT+NUMELS+I) = EANI(NFT+NUMELS+I)+EHOU(I)
        ENDDO
      ENDIF
C
      RETURN
      END
