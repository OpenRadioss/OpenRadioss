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
Chd|  CHVIS3                        source/elements/shell/coque/chvis3.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CHVIS3(
     2   JFT ,JLT ,THK ,HOUR,OFF ,PX1 ,PX2 ,PY1 ,PY2 ,
     3   IXC ,DT1C,SSP ,RHO ,STI ,VX1 ,VX2 ,VX3 ,VX4 ,VY1 ,
     4   VY2 ,VY3 ,VY4 ,VZ1 ,VZ2 ,VZ3 ,VZ4 ,AREA,THK0,VHX ,
     5   VHY ,SHF ,Z2  ,EANI,STIR,VISCMX,G    ,A11 ,
     6   H1  ,H2  ,H3  ,YM  ,NU  ,THK02 ,ALPE ,H11  ,
     7   H12 ,H13 ,H21 ,H22 ,H23 ,H31   ,H32  ,H33  ,
     8   B11 ,B12 ,B13 ,B14 ,B21 ,B22   ,B23  ,B24  ,
     9   RX1 ,RX2 ,RX3 ,RX4 ,RY1 ,RY2   ,RY3  ,RY4  ,
     A   IPARTC,PARTSAV,
     B   IHBE    ,NFT   ,ISMSTR,KFTS  ,
     C   SRH1  ,SRH2  ,SRH3   ,IGTYP ,
     D   IGMAT   ,A11R,NEL)
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
#include      "com06_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "remesh_c.inc"
#include      "scr02_c.inc"
#include      "scr06_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*),IPARTC(*), JFT, JLT, 
     .              IHBE   ,NFT     ,ISMSTR,KFTS,IGTYP,
     .              IGMAT,NEL
C     REAL
      my_real
     .   THK(*), HOUR(NEL,5), OFF(*),PARTSAV(NPSAV,*),
     .   PX1(*), PX2(*), PY1(*), PY2(*),DT1C(*),EANI(*),
     .   SSP(MVSIZ), RHO(MVSIZ),STI(MVSIZ),STIR(MVSIZ),
     .   VX1(MVSIZ), VX2(MVSIZ), VX3(MVSIZ), VX4(MVSIZ),
     .   VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), VY4(MVSIZ),
     .   VZ1(MVSIZ), VZ2(MVSIZ), VZ3(MVSIZ), VZ4(MVSIZ),
     .   AREA(MVSIZ),THK0(MVSIZ),VHX(MVSIZ), VHY(MVSIZ),
     .   SHF(MVSIZ),Z2(MVSIZ),VISCMX(MVSIZ),G(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), A11(MVSIZ),
     .   B11(MVSIZ), B12(MVSIZ), B13(MVSIZ), B14(MVSIZ),
     .   B21(MVSIZ), B22(MVSIZ), B23(MVSIZ), B24(MVSIZ),
     .   H11(MVSIZ), H12(MVSIZ), H13(MVSIZ), H14(MVSIZ),
     .   H21(MVSIZ), H22(MVSIZ), H23(MVSIZ), H24(MVSIZ),
     .   H31(MVSIZ), H32(MVSIZ), H33(MVSIZ), H34(MVSIZ),
     .   RX1(MVSIZ), RX2(MVSIZ), RX3(MVSIZ), RX4(MVSIZ),
     .   RY1(MVSIZ), RY2(MVSIZ), RY3(MVSIZ), RY4(MVSIZ),
     .   THK02(MVSIZ),YM(MVSIZ), NU(MVSIZ), ALPE(MVSIZ),
     .    SRH1(*)    ,SRH2(*)   ,SRH3(*),A11R(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,J, II, IC, JST(MVSIZ+1)
C     REAL
      my_real
     .   H1L(MVSIZ), H2L(MVSIZ), H3L(MVSIZ), H1Q(MVSIZ), H2Q(MVSIZ),
     .   H3Q(MVSIZ), HG1(MVSIZ), HG2(MVSIZ),
     .   HH1(MVSIZ), HH2(MVSIZ), HH3(MVSIZ),
     .   A1(MVSIZ),  A2(MVSIZ),  A3(MVSIZ),  A4(MVSIZ),
     .   A5(MVSIZ),  A6(MVSIZ),  A7(MVSIZ),  A8(MVSIZ),
     .   B1(MVSIZ),  B2(MVSIZ),  EHOU(MVSIZ),  
     .   HOUR1(MVSIZ), HOUR2(MVSIZ), HOUR3(MVSIZ), 
     .   GAMA1(MVSIZ), GAMA2(MVSIZ), GAMA3(MVSIZ), GAMA4(MVSIZ)
      my_real
     .   PX1V, PX2V, PY1V, PY2V, SSPHVL, HVISH1, HVISH2,ZZ2,
     .   SHFPR3, FAC, EHOURT, 
     .   R0,R1,T2A,TSA,TD,VV,MM,HOUR1P,HOUR2P,HOUR3P,
     .   PX1VP,PY1VP,PX2VP,PY2VP,HOUR1A,HOUR2A,HOUR3A,TSAPHI,
     .   TDPHI,SHFPR3D,HVISH1D,HVISH2D,R0D,R1D,HH3D,
     .   SR2D2,SRSHFPR3, INV9, INV12,SCALE(MVSIZ),IZ
C=======================================================================
      EHOURT = ZERO
      SR2D2 = SQRT(TWO)* HALF
      INV9 = ONE_OVER_9
      INV12 = ONE_OVER_12
      
!!      IGTYP = IGEO(11,MAT(1))
C
      IF(ISMSTR/=1.AND.ISMSTR/=11.AND.IHBE>=1)THEN
       DO I=JFT,JLT
         PX1V    = PX1(I)*VHX(I)
         PX2V    = PX2(I)*VHX(I)
         PY1V    = PY1(I)*VHY(I)
         PY2V    = PY2(I)*VHY(I)
         GAMA1(I)= OFF(I)*( ONE - PX1V-PY1V)
         GAMA3(I)= OFF(I)*( ONE + PX1V+PY1V)
         GAMA2(I)= OFF(I)*(-ONE - PX2V-PY2V)
         GAMA4(I)= OFF(I)*(-ONE + PX2V+PY2V)         
       ENDDO
      ELSE
       DO I=JFT,JLT
         GAMA1(I)= OFF(I)
         GAMA3(I)= OFF(I)
         GAMA2(I)= -OFF(I)
         GAMA4(I)= -OFF(I)
       ENDDO
      ENDIF
C----------------------------------------
       DO I=JFT,JLT
          SHFPR3 = SHF(I)/(THREE*(ONE + NU(I)))
          HVISH1 = HVISC*H1(I)
          HVISH2 = HVISC*H2(I)
          R0 = FOURTH*RHO(I)
          R1 = R0*HUNDRED
          R0 = R0*HVLIN
          A1(I) = R1*HVISH1 
          A2(I) = R0*SR2D2*SRH1(I)
          SRSHFPR3 = SQRT(SHFPR3)
          A3(I) = R1*HVISH2*SRSHFPR3
          A4(I) = R0*SR2D2*SRH2(I)*SRSHFPR3
          HH3(I) = HELAS*H3(I)
          A5(I) = HH3(I)*R1*ZEP072169
          HH3(I) = SR2D2*SRH3(I)
          A6(I) = HH3(I)*R0*ZEP072169
          R0 = FOURTH*YM(I)*HELAS
          A7(I) = H1(I)*R0
          A8(I) = H2(I)*R0*SHFPR3
       ENDDO
C-------------------------------------
      DO I=JFT,JLT
       T2A    = THK02(I)*AREA(I)
       TSA    = SQRT(T2A)
       H1Q(I) = A1(I)*TSA
       H1L(I) = A2(I)*SSP(I)*TSA
       H2Q(I) = A3(I)*THK02(I)
       H2L(I) = A4(I)*SSP(I)*THK02(I)
       H3Q(I) = A5(I)*T2A
       H3L(I) = A6(I)*SSP(I)*T2A
       TD     = THK0(I)*DT1C(I)
       HH1(I)  = A7(I)*TD
       B1(I) = PX1(I)*PX1(I)+PY1(I)*PY1(I)
       B2(I) = PX2(I)*PX2(I)+PY2(I)*PY2(I)
       HH2(I)  = A8(I)*THK02(I)*TD/(B1(I)+B2(I))
      ENDDO
C-----------------------------------
C     TRIANGLES
C-----------------------------------
      DO I=JFT,JLT
        IF(IXC(4,I)==IXC(5,I))THEN
          H1Q(I)=ZERO
          H1L(I)=ZERO
          H2Q(I)=ZERO
          H2L(I)=ZERO
          H3Q(I)=ZERO
          H3L(I)=ZERO
          HH1(I)=ZERO
          HH2(I)=ZERO
        END IF       
      END DO
C-----------------------------------
C     RIGIDITE-DT
C-----------------------------------
      DO I=JFT,JLT
          SCALE(I) = ZERO
        ENDDO
      IF(NODADT/=0.OR.IDT1SH==1.OR.IDTMINS==2)THEN
         DO I=JFT,JLT
             SCALE(I) = MAX(GAMA1(I)*GAMA1(I),GAMA2(I)*GAMA2(I),
     .              GAMA3(I)*GAMA3(I),GAMA4(I)*GAMA4(I)) *
     .        DT1C(I)*MAX(HH1(I)+H1L(I),HH2(I)+H2L(I),H3L(I))
     .            /MAX(DT1C(I)*DT1C(I),EM20)
             STI(I) = STI(I) + SCALE(I)
          ENDDO
         !! IF(IDTMINS==2 )IGMAT = 1
C     
        IF(IGTYP == 52 .OR. 
     .    ((IGTYP == 11 .OR. IGTYP == 17 .OR. IGTYP == 51)
     .                                   .AND. IGMAT > 0 )) THEN
          IF(NADMESH==0)THEN
             DO I=JFT,JLT
                IF (OFF(I)==ZERO) THEN
                  STI(I) = ZERO
                  STIR(I) = ZERO          
                ELSE
                  VV = VISCMX(I) * VISCMX(I)  * ALPE(I)
                  FAC = MAX(B1(I),B2(I))/(AREA(I) * VV)
                  STI(I) =  STI(I) + FAC * THK0(I) * A11(I)
                  STIR(I) = FAC *  A11R(I)*ONE_OVER_12*THK0(I)**3 + 
     .                      FAC *  A11(I)*THK0(I)*AREA(I)*INV9 +
     .            FAC*SCALE(I)*(ONE_OVER_12*THK0(I)**2 +  AREA(I)*INV9) 
               ENDIF
             ENDDO  
         ELSE
             DO I=JFT,JLT
                IF (OFF(I)==ZERO) THEN
                  STI(I) = ZERO
                  STIR(I) = ZERO          
                ELSE
                  VV = VISCMX(I) * VISCMX(I)  * ALPE(I)
                   FAC = MAX(B1(I),B2(I))/(AREA(I) * VV)
                  STI(I) =  STI(I) + FAC * THK0(I) * A11(I)
                  STIR(I) = FAC *  A11R(I)*ONE_OVER_12*THK0(I)**3 + 
     .                            FAC*SCALE(I)*ONE_OVER_12*THK0(I)**2
                ENDIF 
             ENDDO
          END IF   
        ELSE
          IF(NADMESH==0)THEN
#include   "simd.inc"
             DO I=JFT,JLT
                IF (OFF(I)==ZERO) THEN
                  STI(I) = ZERO
                  STIR(I) = ZERO          
                ELSE
                  VV = VISCMX(I) * VISCMX(I)  * ALPE(I)
                  STI(I) =  STI(I) + MAX(B1(I),B2(I))
     .                 * THK0(I) * A11(I) / (AREA(I) * VV)
                  STIR(I) = STI(I) * (THK02(I)*INV12 + AREA(I)*INV9)
c     ..                   + 0.5 * SHF(I) * AREA(I) * G(I)/A11(I)) 
                ENDIF 
             ENDDO
         ELSE
#include   "simd.inc"
             DO I=JFT,JLT
                IF (OFF(I)==ZERO) THEN
                  STI(I) = ZERO
                  STIR(I) = ZERO          
                ELSE
                  VV = VISCMX(I) * VISCMX(I)  * ALPE(I)
                  STI(I) =  STI(I) + MAX(B1(I),B2(I))
     .                 * THK0(I) * A11(I) / (AREA(I) * VV)
                  STIR(I) = STI(I) * THK02(I)*INV12
                ENDIF 
             ENDDO
        END IF
       ENDIF! IGTYP
      ENDIF 
C-----------------------------------
C     FORCES ANTISABLIER DE MEMBRANE
C-----------------------------------
      IF(ISMSTR==1.OR.ISMSTR==11.OR.IHBE<1)THEN
        DO I=JFT,JLT
         HG1(I)=(VX1(I)-VX2(I)+VX3(I)-VX4(I))*OFF(I)
         HG2(I)=(VY1(I)-VY2(I)+VY3(I)-VY4(I))*OFF(I)
        ENDDO
      ELSE
        DO I=JFT,JLT
         HG1(I)=VX1(I)*GAMA1(I)+VX2(I)*GAMA2(I)
     .         +VX3(I)*GAMA3(I)+VX4(I)*GAMA4(I)
         HG2(I)=VY1(I)*GAMA1(I)+VY2(I)*GAMA2(I)
     .         +VY3(I)*GAMA3(I)+VY4(I)*GAMA4(I)
        ENDDO
      ENDIF
      DO I=JFT,JLT
         HOUR(I,1)=HOUR(I,1)+HG1(I)*HH1(I)
         HOUR(I,2)=HOUR(I,2)+HG2(I)*HH1(I)
         HOUR1A =HOUR(I,1)+HG1(I)*(H1L(I)+H1Q(I)*ABS(HG1(I)))
         H11(I)=HOUR1A*GAMA1(I)
         H12(I)=HOUR1A*GAMA2(I)
         H13(I)=HOUR1A*GAMA3(I)
C         H14(I)=HOUR1A*GAMA4(I)
         HOUR2A =HOUR(I,2)+HG2(I)*(H1L(I)+H1Q(I)*ABS(HG2(I)))
         H21(I)=HOUR2A*GAMA1(I)
         H22(I)=HOUR2A*GAMA2(I)
         H23(I)=HOUR2A*GAMA3(I)
C         H24(I)=HOUR2A*GAMA4(I)
         EHOU(I) = HOUR1A*HG1(I) + HOUR2A*HG2(I) 
      ENDDO
C
C----------------------------------
C     FORCES ANTISABLIER DE FLEXION
C----------------------------------
      IF(ISMSTR==1.OR.ISMSTR==11.OR.IHBE<1)THEN
        DO I=JFT,JLT
          HG1(I)=(VZ1(I)-VZ2(I)+VZ3(I)-VZ4(I))*OFF(I)
        ENDDO
      ELSE
        DO I=JFT,JLT
         HG1(I)=VZ1(I)*GAMA1(I)+VZ2(I)*GAMA2(I)
     .         +VZ3(I)*GAMA3(I)+VZ4(I)*GAMA4(I)
        ENDDO
      ENDIF
      DO I=JFT,JLT
        HOUR(I,3)=HOUR(I,3)+HG1(I)*HH2(I)
        HOUR3A =HOUR(I,3)+HG1(I)*(H2L(I)+H2Q(I)*ABS(HG1(I)))
        H31(I)=HOUR3A*GAMA1(I)
        H32(I)=HOUR3A*GAMA2(I)
        H33(I)=HOUR3A*GAMA3(I)
C         H34(I)=HOUR3(I)*GAMA4(I)
        EHOU(I) = EHOU(I) + HOUR3A*HG1(I) 
      ENDDO
C
C------------------------
C     MOMENTS ANTISABLIER
C------------------------
      DO I=JFT,JLT
       HG1(I)=RX1(I)-RX2(I)+RX3(I)-RX4(I)
       HG2(I)=RY1(I)-RY2(I)+RY3(I)-RY4(I)
      ENDDO
C
      DO I=JFT,JLT
       HOUR(I,4)=HG1(I)*(H3L(I)+H3Q(I)*ABS(HG1(I)))
       HOUR(I,5)=HG2(I)*(H3L(I)+H3Q(I)*ABS(HG2(I)))
       EHOU(I) = EHOU(I) + 
     .           HOUR(I,4)*HG1(I) + HOUR(I,5)*HG2(I) 
       EHOU(I) = DT1C(I) * EHOU(I) * OFF(I)
C       EHOURT  = EHOURT + EHOU(I)
       B11(I)= HOUR(I,4)*OFF(I)
       B12(I)=-HOUR(I,4)*OFF(I)
       B13(I)= HOUR(I,4)*OFF(I)
       B14(I)=-HOUR(I,4)*OFF(I)
       B21(I)= HOUR(I,5)*OFF(I)
       B22(I)=-HOUR(I,5)*OFF(I)
       B23(I)= HOUR(I,5)*OFF(I)
       B24(I)=-HOUR(I,5)*OFF(I)
      ENDDO
C separate loop that does not vectorize w/o simd directive
#include "simd.inc"       
      DO I=JFT,JLT
        EHOURT  = EHOURT + EHOU(I)
      ENDDO
C
      IC=1
      JST(IC)=JFT
      DO J=JFT+1,JLT
         IF (IPARTC(J)/=IPARTC(J-1)) THEN
            IC=IC+1
            JST(IC)=J
         ENDIF
      ENDDO
      
      JST(IC+1)=JLT+1
      IF(IC==1) THEN
        MX = IPARTC(JFT)
#include "simd.inc"
        DO I=JFT,JLT
          PARTSAV(8,MX)=PARTSAV(8,MX) + EHOU(I)
        ENDDO

      ELSEIF(IC==2.AND.KFTS>0)THEN
        MX = IPARTC(JFT)
#include "simd.inc"
        DO I=JFT, KFTS-1
          PARTSAV(8,MX)=PARTSAV(8,MX) + EHOU(I)
        ENDDO
        MX = IPARTC(JLT)
#include "simd.inc"
        DO I=KFTS,JLT
          PARTSAV(8,MX)=PARTSAV(8,MX) + EHOU(I)
        ENDDO

      ELSE
C                 
           DO II=1,IC
              MX=IPARTC(JST(II))
              IF (JST(II+1)-JST(II)>15) THEN
#include "simd.inc"
                 DO J=JST(II),JST(II+1)-1
                    PARTSAV(8,MX)=PARTSAV(8,MX)+EHOU(J)
                 ENDDO
              ELSE
                 DO J=JST(II),JST(II+1)-1
                    PARTSAV(8,MX)=PARTSAV(8,MX)+EHOU(J)
                 ENDDO
              ENDIF
           ENDDO
      ENDIF
C
#include "atomic.inc"
        EHOUR = EHOUR + EHOURT
#include "atomend.inc"
C
      IF((ANIM_E(25)==1).OR.(ANIM_CE(25)==1).OR.
     &             (OUTP_CS(25)==1)) THEN
        DO I=JFT,JLT
          EANI(NFT+NUMELS+I) = EANI(NFT+NUMELS+I) + EHOU(I)
        ENDDO
      ENDIF
C
      RETURN
      END
