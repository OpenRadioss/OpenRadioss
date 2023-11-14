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
Chd|  RIVET1                        source/elements/rivet/rivet1.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RIVET1(MS    ,IN    ,A    ,AR   ,X   ,
     .                  IXRT  ,RIVET ,GEO  ,V    ,VR  ,
     .                  ITASK )
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
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr11_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXRT(4,*), ITASK
C     REAL
      my_real
     .   MS(*), IN(*), A(3,*), AR(3,*), X(3,*), RIVET(NRIVF,*),
     .   GEO(NPROPG,*), V(3,*), VR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IG,N1,N2,IROT,IMOD,RUPT,LFT,LLT,PROC,
     .   JS, NN, IS, NIND1, NIND2, J, L,
     .   IND1(MVSIZ), IND2(MVSIZ)

C     REAL
      my_real
     .   DA1(3),DA2(3),VT1(3),VT2(3),
     .   DMX2, DT12M1, FN2, FT2, 
     .   XM, AMX, AMY, AMZ, AN, AN2, ANX, ANY, ANZ,
     .   ATX, ATY, ATZ, AT2, ALP,MASS,MASM1,INER,INM1,I1,I2,
     .   XCDG,YCDG,ZCDG,XX1,YY1,ZZ1,XX2,YY2,ZZ2, WW,WT,
     .   VX1,VY1,VZ1,VX2,VY2,VZ2,VMX1,VMY1,VMZ1,VMX2,VMY2,VMZ2,
     .   VX,VY,VZ,VRX1,VRX2,VRY1,VRY2,VRZ1,VRZ2,VXX,VYY,VZZ,
     .   AX,AY,AZ, OFF, S,
     .   XN(MVSIZ), YN(MVSIZ), ZN(MVSIZ), DX2(MVSIZ), ENROT_L
C-----------------------------------------------
      RUPT = 0
      ENROT_L = ZERO
      PROC = ISPMD+1
      LFT = 1 + ITASK*NRIVET / NTHREAD
      LLT = (ITASK+1)*NRIVET / NTHREAD
      JS = LFT-1
      DO L=LFT,LLT,NVSIZ
       NN = MIN(NVSIZ,LLT-JS)
       NIND1 = 0
       NIND2 = 0
       DO IS = 1 , NN
        I = JS+IS 
C test si rivet a traiter sur le proc
        IF(IXRT(2,I)>0)THEN
C----------------------------
C       RUPTURE
C----------------------------
         OFF = RIVET(1,I)
         IF (OFF/=ZERO) THEN
          IG=IXRT(1,I)
          N1=IXRT(2,I)
          N2=IXRT(3,I)
          DMX2= GEO(3,IG)
          IMOD=NINT(GEO(5,IG))
          XN(IS)=X(1,N2)-X(1,N1)
          YN(IS)=X(2,N2)-X(2,N1)
          ZN(IS)=X(3,N2)-X(3,N1)
          DX2(IS)=XN(IS)**2+YN(IS)**2+ZN(IS)**2
          IF(DX2(IS)>DMX2)THEN
            OFF=OFF-EM01
            IF(OFF<=ZERO)THEN
              RIVET(1,I)=-ONE
              RUPT = 1
            ELSE
              RIVET(1,I) = OFF
            ENDIF
          ENDIF
          IF (OFF>ZERO) THEN
            IF(IMOD==1) THEN
              NIND1 = NIND1 + 1
              IND1(NIND1) = IS
            ELSE
              NIND2 = NIND2 + 1
              IND2(NIND2) = IS
            ENDIF
          ENDIF
         ENDIF
        ENDIF
       ENDDO
C
C-----------------------------------------------
C         RIGID BODY formulation
C-----------------------------------------------
#include "vectorize.inc"
       DO J = 1, NIND1
         IS = IND1(J)
         I = JS+IS
         IG=IXRT(1,I)
         N1=IXRT(2,I)
         N2=IXRT(3,I)
         OFF = RIVET(1,I)
         FN2 = GEO(1,IG)
         FT2 = GEO(2,IG)
         DMX2= GEO(3,IG)
         IROT=NINT(GEO(4,IG))
         DT12M1=ONE/DT12
C----------------------------
C        TRANSLATION DU CDG
C----------------------------
         MASS = MS(N1)+MS(N2)
         MASM1= ONE / MASS
         XCDG=(X(1,N1)*MS(N1)+X(1,N2)*MS(N2))*MASM1
         YCDG=(X(2,N1)*MS(N1)+X(2,N2)*MS(N2))*MASM1
         ZCDG=(X(3,N1)*MS(N1)+X(3,N2)*MS(N2))*MASM1
C
         VX1= V(1,N1)+A(1,N1)*DT12
         VY1= V(2,N1)+A(2,N1)*DT12
         VZ1= V(3,N1)+A(3,N1)*DT12
         VX2= V(1,N2)+A(1,N2)*DT12
         VY2= V(2,N2)+A(2,N2)*DT12
         VZ2= V(3,N2)+A(3,N2)*DT12
C
         VMX1= VX1*MS(N1)
         VMY1= VY1*MS(N1)
         VMZ1= VZ1*MS(N1)
         VMX2= VX2*MS(N2)
         VMY2= VY2*MS(N2)
         VMZ2= VZ2*MS(N2)
C
         VX = (VMX1+VMX2)*MASM1
         VY = (VMY1+VMY2)*MASM1
         VZ = (VMZ1+VMZ2)*MASM1
C
         IF(IROT==0) THEN
C----------------------------
C          CALCUL FORCES
C----------------------------
           AX = (-A(1,N1)+(VX-V(1,N1))*DT12M1)*MS(N1)
           AY = (-A(2,N1)+(VY-V(2,N1))*DT12M1)*MS(N1)
           AZ = (-A(3,N1)+(VZ-V(3,N1))*DT12M1)*MS(N1)
C
           IF(DX2(IS)>EM15)THEN
C            POINTS NON CONFONDUS CRITERE FN ET FT
             S  = ONE/SQRT(DX2(IS))
             XN(IS) =XN(IS)*S
             YN(IS) =YN(IS)*S
             ZN(IS) =ZN(IS)*S
             AN =AX*XN(IS)+AY*YN(IS)+AZ*ZN(IS)
             AN2=AN**2
             ANX=AN*XN(IS)
             ANY=AN*YN(IS)
             ANZ=AN*ZN(IS)
             ATX=AX-ANX
             ATY=AY-ANY
             ATZ=AZ-ANZ
             AT2=(ATX**2+ATY**2+ATZ**2)
           ELSE
C            POINTS CONFONDUS CRITERE UNIQUEMENT SUR FN
             AN2=(AX**2+AY**2+AZ**2)
             AT2=0.
           ENDIF
C
           ALP=SQRT((AN2/FN2)+(AT2/FT2))
           ALP=OFF / MAX(ALP,ONE)
           AX=ALP*AX
           AY=ALP*AY
           AZ=ALP*AZ
C----------------------------
C          CALCUL ACCELERATIONS
C----------------------------
           A(1,N1)=A(1,N1)+AX/MS(N1)
           A(2,N1)=A(2,N1)+AY/MS(N1)
           A(3,N1)=A(3,N1)+AZ/MS(N1)
           A(1,N2)=A(1,N2)-AX/MS(N2)
           A(2,N2)=A(2,N2)-AY/MS(N2)
           A(3,N2)=A(3,N2)-AZ/MS(N2)
         ELSE
C----------------------------
C          ROTATION DU CDG
C----------------------------
           XX1=X(1,N1)-XCDG
           YY1=X(2,N1)-YCDG
           ZZ1=X(3,N1)-ZCDG
           XX2=X(1,N2)-XCDG
           YY2=X(2,N2)-YCDG
           ZZ2=X(3,N2)-ZCDG
C
           I1 = (XX1*XX1+YY1*YY1+ZZ1*ZZ1)*MS(N1) + IN(N1)
           I2 = (XX2*XX2+YY2*YY2+ZZ2*ZZ2)*MS(N2) + IN(N2)
           INER = I1 + I2
           INM1 = ONE/INER
C
           VRX1= VR(1,N1)+AR(1,N1)*DT12
           VRY1= VR(2,N1)+AR(2,N1)*DT12
           VRZ1= VR(3,N1)+AR(3,N1)*DT12
           VRX2= VR(1,N2)+AR(1,N2)*DT12
           VRY2= VR(2,N2)+AR(2,N2)*DT12
           VRZ2= VR(3,N2)+AR(3,N2)*DT12
C
           VXX = (VRX1*IN(N1)+YY1*VMZ1-ZZ1*VMY1
     .         +  VRX2*IN(N2)+YY2*VMZ2-ZZ2*VMY2)*INM1
           VYY = (VRY1*IN(N1)+ZZ1*VMX1-XX1*VMZ1
     .         +  VRY2*IN(N2)+ZZ2*VMX2-XX2*VMZ2)*INM1
           VZZ = (VRZ1*IN(N1)+XX1*VMY1-YY1*VMX1
     .         +  VRZ2*IN(N2)+XX2*VMY2-YY2*VMX2)*INM1
C----------------------------
C          CALCUL FORCES
C----------------------------
           VT1(1) = ZZ1*VYY - YY1*VZZ
           VT1(2) = XX1*VZZ - ZZ1*VXX
           VT1(3) = YY1*VXX - XX1*VYY
           VT2(1) = ZZ2*VYY - YY2*VZZ
           VT2(2) = XX2*VZZ - ZZ2*VXX
           VT2(3) = YY2*VXX - XX2*VYY
C
           AX = (-A(1,N2)+(VX+VT2(1)-V(1,N2))*DT12M1)*MS(N2)
           AY = (-A(2,N2)+(VY+VT2(2)-V(2,N2))*DT12M1)*MS(N2)
           AZ = (-A(3,N2)+(VZ+VT2(3)-V(3,N2))*DT12M1)*MS(N2)
C
           AX = (-A(1,N1)+(VX+VT1(1)-V(1,N1))*DT12M1)*MS(N1)
           AY = (-A(2,N1)+(VY+VT1(2)-V(2,N1))*DT12M1)*MS(N1)
           AZ = (-A(3,N1)+(VZ+VT1(3)-V(3,N1))*DT12M1)*MS(N1)
C
           IF(DX2(IS)>EM15)THEN
C            POINTS NON CONFONDUS CRITERE FN ET FT
             S = ONE/SQRT(DX2(IS))
             XN(IS) =XN(IS)*S
             YN(IS) =YN(IS)*S
             ZN(IS) =ZN(IS)*S
             AN =AX*XN(IS)+AY*YN(IS)+AZ*ZN(IS)
             AN2=AN**2
             ANX=AN*XN(IS)
             ANY=AN*YN(IS)
             ANZ=AN*ZN(IS)
             ATX=AX-ANX
             ATY=AY-ANY
             ATZ=AZ-ANZ
             AT2=(ATX**2+ATY**2+ATZ**2)
           ELSE
C            POINTS CONFONDUS CRITERE UNIQUEMENT SUR FN
             AN2=(AX**2+AY**2+AZ**2)
             AT2=ZERO
             AN = SQRT(AN2)
           ENDIF
C
           ALP=SQRT((AN2/FN2)+(AT2/FT2))
           ALP=OFF / MAX(ALP,ONE)
           AX=ALP*AX
           AY=ALP*AY
           AZ=ALP*AZ
C----------------------------
C          CALCUL ACCELERATIONS
C----------------------------
           DA1(1) = HALF*DT2*DT12M1*(VYY*VT1(3) - VZZ*VT1(2))
           DA1(2) = HALF*DT2*DT12M1*(VZZ*VT1(1) - VXX*VT1(3))
           DA1(3) = HALF*DT2*DT12M1*(VXX*VT1(1) - VYY*VT1(2))
           DA2(1) = HALF*DT2*DT12M1*(VYY*VT2(3) - VZZ*VT2(2))
           DA2(2) = HALF*DT2*DT12M1*(VZZ*VT2(1) - VXX*VT2(3))
           DA2(3) = HALF*DT2*DT12M1*(VXX*VT2(1) - VYY*VT2(2))     
C
           A(1,N1)=A(1,N1)+AX/MS(N1)+DA1(1)
           A(2,N1)=A(2,N1)+AY/MS(N1)+DA1(2)
           A(3,N1)=A(3,N1)+AZ/MS(N1)+DA1(3)
           A(1,N2)=A(1,N2)-AX/MS(N2)+DA2(1)
           A(2,N2)=A(2,N2)-AY/MS(N2)+DA2(2)
           A(3,N2)=A(3,N2)-AZ/MS(N2)+DA2(3)
           RIVET(2, I) = AN
           RIVET(3, I) = SQRT(AT2)
           RIVET(4, I) = AX
           RIVET(5, I) = AY
           RIVET(6, I) = AZ
           RIVET(7, I) = A(1,N1)*MS(N1)
           RIVET(8 ,I) = A(2,N1)*MS(N1)
           RIVET(9 ,I) = A(3,N1)*MS(N1)
C
           AMX=-AR(1,N1)+(VXX-VR(1,N1))*DT12M1
           AMY=-AR(2,N1)+(VYY-VR(2,N1))*DT12M1
           AMZ=-AR(3,N1)+(VZZ-VR(3,N1))*DT12M1
           AR(1,N1)=AR(1,N1)+AMX*ALP
           AR(2,N1)=AR(2,N1)+AMY*ALP
           AR(3,N1)=AR(3,N1)+AMZ*ALP
           RIVET(10,I) = AR(1,N1)*I1
           RIVET(11,I) = AR(2,N1)*I1
           RIVET(12,I) = AR(3,N1)*I1
C
           AMX=-AR(1,N2)+(VXX-VR(1,N2))*DT12M1
           AMY=-AR(2,N2)+(VYY-VR(2,N2))*DT12M1
           AMZ=-AR(3,N2)+(VZZ-VR(3,N2))*DT12M1
           AR(1,N2)=AR(1,N2)+AMX*ALP
           AR(2,N2)=AR(2,N2)+AMY*ALP
           AR(3,N2)=AR(3,N2)+AMZ*ALP
C
           RIVET(10,I) = AR(1,N2)*I2
           RIVET(11,I) = AR(2,N2)*I2
           RIVET(12,I) = AR(3,N2)*I2
           RIVET(13,I) = ZERO
C          correction energie cinetique de rotation
           WW = VXX**2+VYY**2+VZZ**2
           WT = (VYY*ZN(IS)-VZZ*YN(IS))**2
     .        + (VZZ*XN(IS)-VXX*ZN(IS))**2
     .        + (VXX*YN(IS)-VYY*XN(IS))**2
            ENROT_L = ENROT_L + HALF*INER*(WW-WT)
         ENDIF
       ENDDO
C-----------------------------------------------
C         OLD RIGID LINK formulation
C-----------------------------------------------
#include      "vectorize.inc"
       DO J = 1, NIND2
         IS = IND2(J)
         I = JS+IS
         IG=IXRT(1,I)
         N1=IXRT(2,I)
         N2=IXRT(3,I)
         OFF = RIVET(1,I)
         FN2 = GEO(1,IG)
         FT2 = GEO(2,IG)
         IROT=NINT(GEO(4,IG))
         XM=MS(N1)*MS(N2)/(MS(N1)+MS(N2))
         AMX=(A(1,N2)-A(1,N1)+(V(1,N2)-V(1,N1))/DT12)*XM
         AMY=(A(2,N2)-A(2,N1)+(V(2,N2)-V(2,N1))/DT12)*XM
         AMZ=(A(3,N2)-A(3,N1)+(V(3,N2)-V(3,N1))/DT12)*XM
         IF(DX2(IS)>EM15)THEN
C          POINTS NON CONFONDUS CRITERE FN ET FT
           S = ONE/SQRT(DX2(IS))
           XN(IS) =XN(IS)*S
           YN(IS) =YN(IS)*S
           ZN(IS) =ZN(IS)*S
           AN =AMX*XN(IS)+AMY*YN(IS)+AMZ*ZN(IS)
           AN2=AN**2
           ANX=AN*XN(IS)
           ANY=AN*YN(IS)
           ANZ=AN*ZN(IS)
           ATX=AMX-ANX
           ATY=AMY-ANY
           ATZ=AMZ-ANZ
           AT2=(ATX**2+ATY**2+ATZ**2)
         ELSE
C          POINTS CONFONDUS CRITERE UNIQUEMENT SUR FN
           AN2=(AMX**2+AMY**2+AMZ**2)
           AT2=ZERO
         ENDIF
         ALP=SQRT((AN2/FN2)+(AT2/FT2))
         ALP=OFF / MAX(ALP,ONE)
         AMX=ALP*AMX
         AMY=ALP*AMY
         AMZ=ALP*AMZ
         A(1,N1)=A(1,N1)+AMX/MS(N1)
         A(2,N1)=A(2,N1)+AMY/MS(N1)
         A(3,N1)=A(3,N1)+AMZ/MS(N1)
         A(1,N2)=A(1,N2)-AMX/MS(N2)
         A(2,N2)=A(2,N2)-AMY/MS(N2)
         A(3,N2)=A(3,N2)-AMZ/MS(N2)
         IF(IROT==1)THEN
           INM1=ONE/(IN(N1)+IN(N2))
           AR(1,N1)=(AR(1,N1)*IN(N1)+AR(1,N2)*IN(N2))*INM1
           AR(2,N1)=(AR(2,N1)*IN(N1)+AR(2,N2)*IN(N2))*INM1
           AR(3,N1)=(AR(3,N1)*IN(N1)+AR(3,N2)*IN(N2))*INM1
           AR(1,N2)=AR(1,N1)
           AR(2,N2)=AR(2,N1)
           AR(3,N2)=AR(3,N1)
         ENDIF
C-----------------------------------------------
       ENDDO
       JS = JS + NN
      ENDDO
C
#include "lockon.inc"
        ENROT = ENROT + ENROT_L 
#include "lockoff.inc"
      IF (RUPT==1) THEN
        DO I=LFT,LLT
          IF(RIVET(1,I)==-ONE)THEN
            RIVET(1,I)=ZERO
#include "lockon.inc"
            WRITE(ISTDO,*)' FAILURE OF RIVET',IXRT(4,I)
            WRITE(IOUT,*) ' FAILURE OF RIVET',IXRT(4,I)
#include "lockoff.inc"
          ENDIF
        ENDDO
      ENDIF
C
      RETURN
      END
