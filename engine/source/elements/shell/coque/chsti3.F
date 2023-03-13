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
Chd|  CHSTI3                        source/elements/shell/coque/chsti3.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CHSTI3(JFT ,JLT,THK,HOUR,OFF ,PX1,
     2                  PX2,PY1,PY2 ,SIGY,IXC,DT1C,
     3                  SSP,RHO,STI ,Z2 ,EANI,STIR,
     4                  SHF,THK0,THK02,VISCMX,G  ,A11 ,
     5   H1  ,H2  ,H3 ,YM   ,NU  , ALPE, 
     6   VHX ,VHY ,VX1 ,VX2 ,VX3 ,VX4 ,VY1 ,
     7   VY2 ,VY3 ,VY4,VZ1  ,VZ2  ,VZ3 ,VZ4 ,AREA,
     8   H11 ,H12 ,H13 ,H21 ,H22 ,H23  ,H31  ,H32  ,H33  ,
     9   B11 ,B12 ,B13 ,B14 ,B21 ,B22  ,B23  ,B24  ,
     A   RX1 ,RX2 ,RX3 ,RX4 ,RY1 ,RY2  ,RY3  ,RY4,
     B   IPARTC ,PARTSAV,
     C   IHBE   ,NFT   ,ISMSTR ,SRH3 ,IGTYP ,
     D   IGMAT  ,A11R  ,NEL)
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
#include      "remesh_c.inc"
#include      "scr02_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*),IPARTC(*), JFT, JLT,
     .              IHBE   ,NFT     ,ISMSTR,IGTYP,IGMAT,NEL
C     REAL
      my_real
     .   THK(*), HOUR(NEL,5), OFF(*),EANI(*),PARTSAV(NPSAV,*),
     .   PX1(*), PX2(*), PY1(*), PY2(*), SIGY(*),DT1C(*),STIR(*),
     .   SSP(MVSIZ), RHO(MVSIZ),STI(MVSIZ),Z2(MVSIZ), SHF(MVSIZ),
     .   THK0(MVSIZ),VISCMX(MVSIZ), ALPE(MVSIZ), A11R(*)
C     REAL
      my_real
     .   B11(MVSIZ), B12(MVSIZ), B13(MVSIZ), B14(MVSIZ),
     .   B21(MVSIZ), B22(MVSIZ), B23(MVSIZ), B24(MVSIZ),
     .   H11(MVSIZ), H12(MVSIZ), H13(MVSIZ), H14(MVSIZ),
     .   H21(MVSIZ), H22(MVSIZ), H23(MVSIZ), H24(MVSIZ),
     .   H31(MVSIZ), H32(MVSIZ), H33(MVSIZ), H34(MVSIZ),
     .   RX1(MVSIZ), RX2(MVSIZ), RX3(MVSIZ), RX4(MVSIZ),
     .   RY1(MVSIZ), RY2(MVSIZ), RY3(MVSIZ), RY4(MVSIZ),
     .   VX1(MVSIZ), VX2(MVSIZ), VX3(MVSIZ), VX4(MVSIZ),
     .   VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), VY4(MVSIZ),
     .   VZ1(MVSIZ), VZ2(MVSIZ), VZ3(MVSIZ), VZ4(MVSIZ),
     .   AREA(MVSIZ),VHX(MVSIZ),
     .   H1(MVSIZ),  H2(MVSIZ),  H3(MVSIZ), VHY(MVSIZ),
     .   YM(MVSIZ), NU(MVSIZ), THK02(MVSIZ),SRH3(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX, J
C     REAL
      my_real
     .   GAMA1(MVSIZ), GAMA2(MVSIZ), GAMA3(MVSIZ), GAMA4(MVSIZ),
     .   H3L(MVSIZ), H3Q(MVSIZ),
     .   HG1(MVSIZ), HG2(MVSIZ) ,HP1(MVSIZ), HP2(MVSIZ) , 
     .   G(MVSIZ) ,  B1(MVSIZ),  B2(MVSIZ),  
     .   A11(MVSIZ) ,EHOU(MVSIZ),
     .   PX1V, PX2V, PY1V, PY2V, SSPHVL,ZZ2,VV, 
     .   SHFPR3, FAC, BB, HOUR1(MVSIZ), HOUR2(MVSIZ), HOUR3(MVSIZ), 
     .   EHOURT, PX1VP,PX2VP,PY1VP,PY2VP, FMAX(MVSIZ),
     .   HOUR1P,HOUR2P,HOUR3P,FMAX1,FMAXP1,FMAX2,FMAXP2,FMAXP,
     .   SCALE(MVSIZ)
C-----------------------------------------------
C
      EHOURT = ZERO
C
      IF(ISMSTR/=3.AND.IHBE/=0)THEN
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
C
      DO I=JFT,JLT
       HP1(I) =H1(I)
       HP2(I) =H2(I)
       H3(I) =SRH3(I)
       SHFPR3 = SHF(I)/(THREE*(ONE+NU(I)))
       FAC    = FOURTH*RHO(I)*THK02(I)
       H3L(I) = H3(I)*ZEP072169*FAC*AREA(I)
       H3Q(I) = H3(I)*H3L(I)*HUNDRED
       H3L(I) = H3L(I)*SSP(I)
C
       FAC    = YM(I)*THK0(I)*DT1C(I)*ONE_OVER_8
       H1(I)  = HP1(I) * FAC
       B1(I) = PX1(I)*PX1(I)+PY1(I)*PY1(I)
       B2(I) = PX2(I)*PX2(I)+PY2(I)*PY2(I)
       H2(I)  = HP2(I) * FAC*THK02(I)*SHFPR3/(B1(I)+B2(I))
      ENDDO
C-----------------------------------
C     TRIANGLES
C-----------------------------------
      DO I=JFT,JLT
       IF(IXC(4,I)==IXC(5,I))THEN
         H3Q(I)=ZERO
         H3L(I)=ZERO
         H1(I)=ZERO
         H2(I)=ZERO	 
       ENDIF
      ENDDO
C-----------------------------------
C     RIGIDITE-DT
C-----------------------------------
       DO I=JFT,JLT
          SCALE(I) = ZERO
      ENDDO
      IF(NODADT/=0.OR.IDT1SH==1.OR.IDTMINS==2)THEN
        DO I=JFT,JLT
          SCALE(I) = +MAX(GAMA1(I)*GAMA1(I),GAMA2(I)*GAMA2(I),
     .              GAMA3(I)*GAMA3(I),GAMA4(I)*GAMA4(I)) *
     .    DT1C(I)*MAX(H1(I),H2(I),H3L(I)) /
     .              MAX(DT1C(I)*DT1C(I),EM20)
          STI(I)=STI(I) + SCALE(I) 
        ENDDO
C     
        !!IF(IDTMINS==2)IGMAT = 1
        IF(IGTYP == 52 .OR.
     .    ((IGTYP == 11 .OR. IGTYP == 17 .OR. IGTYP == 51) 
     .                                       .AND. IGMAT > 0)) THEN
          IF(NADMESH==0)THEN
             DO I=JFT,JLT
              IF (OFF(I)==ZERO) THEN
               STI(I) = ZERO
               STIR(I) = ZERO          
             ELSE
               VV = VISCMX(I) * VISCMX(I)  * ALPE(I)
               FAC = MAX(B1(I),B2(I))/(AREA(I) * VV)
               STI(I) =  STI(I) + FAC * THK0(I) * A11(I) 
               STIR(I) = FAC *  A11R(I)*THK0(I)*THK02(I)*ONE_OVER_12 +
     .                   FAC *  A11(I) *THK0(I)*AREA(I) *ONE_OVER_9 +
     .                   SCALE(I)*(THK02(I)* ONE_OVER_12 +  AREA(I)*ONE_OVER_9)
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
               STIR(I) = FAC *  A11R(I)*THK0(I)*THK02(I)* ONE_OVER_12 +
     .                           SCALE(I)*THK02(I)* ONE_OVER_12
            ENDIF 
            ENDDO
         END IF
        ELSE ! IGTYP
          IF(NADMESH==0)THEN
             DO I=JFT,JLT
              IF (OFF(I)==ZERO) THEN
               STI(I) = ZERO
               STIR(I) = ZERO          
             ELSE
               VV = VISCMX(I) * VISCMX(I)  * ALPE(I)
               STI(I) =  STI(I) + MAX(B1(I),B2(I)) 
     .                      * THK0(I) * A11(I) / (AREA(I) * VV)
               STIR(I) = STI(I) * (THK02(I)* ONE_OVER_12 + AREA(I) * ONE_OVER_9)
c     ..                + 0.5 * SHF(I) * AREA(I) * G(I)/A11(I)) 
             ENDIF 
             ENDDO
         ELSE
            DO I=JFT,JLT
             IF (OFF(I)==ZERO) THEN
              STI(I) = ZERO
              STIR(I) = ZERO          
            ELSE
              VV = VISCMX(I) * VISCMX(I)  * ALPE(I)
              STI(I) =  STI(I) + MAX(B1(I),B2(I)) 
     .                     * THK0(I) * A11(I) / (AREA(I) * VV)
              STIR(I) = STI(I) * THK02(I)* ONE_OVER_12
            ENDIF 
            ENDDO
        END IF
       ENDIF! igtyp
      ENDIF ! 
C-----------------------------------
C     FORCES ANTISABLIER DE MEMBRANE
C-----------------------------------
      DO I=JFT,JLT
        HG1(I)=VX1(I)*GAMA1(I)+VX2(I)*GAMA2(I)
     .        +VX3(I)*GAMA3(I)+VX4(I)*GAMA4(I)
        HG2(I)=VY1(I)*GAMA1(I)+VY2(I)*GAMA2(I)
     .        +VY3(I)*GAMA3(I)+VY4(I)*GAMA4(I)
      ENDDO

      DO I=JFT,JLT
        HOUR(I,1)=HOUR(I,1)+HG1(I)*H1(I)
        HOUR(I,2)=HOUR(I,2)+HG2(I)*H1(I)
        FMAX(I) = HALF*SIGY(I)*THK0(I)*SQRT(AREA(I))*HP1(I)
        HOUR(I,1)=SIGN( MIN( ABS(HOUR(I,1)),FMAX(I) ) , HOUR(I,1))
        HOUR(I,2)=SIGN( MIN( ABS(HOUR(I,2)),FMAX(I) ) , HOUR(I,2))
        HOUR1(I)=HOUR(I,1)
        H11(I)=HOUR1(I)*GAMA1(I)
        H12(I)=HOUR1(I)*GAMA2(I)
        H13(I)=HOUR1(I)*GAMA3(I)
        H14(I)=HOUR1(I)*GAMA4(I)
        HOUR2(I)=HOUR(I,2)
        H21(I)=HOUR2(I)*GAMA1(I)
        H22(I)=HOUR2(I)*GAMA2(I)
        H23(I)=HOUR2(I)*GAMA3(I)
        H24(I)=HOUR2(I)*GAMA4(I)
        EHOU(I) = HOUR1(I)*HG1(I) + HOUR2(I)*HG2(I)
      ENDDO
C
C----------------------------------
C     FORCES ANTISABLIER DE FLEXION
C----------------------------------
      DO I=JFT,JLT
        HG1(I)=VZ1(I)*GAMA1(I)+VZ2(I)*GAMA2(I)
     .        +VZ3(I)*GAMA3(I)+VZ4(I)*GAMA4(I)
      ENDDO
      DO I=JFT,JLT
       HOUR(I,3)=HOUR(I,3)+HG1(I)*H2(I)
       FMAX(I) = FOURTH*SIGY(I)*THK0(I)*THK0(I)*HP2(I)
       HOUR(I,3)=SIGN( MIN( ABS(HOUR(I,3)),FMAX(I) ) , HOUR(I,3))
       HOUR3(I) =HOUR(I,3)
       H31(I)=HOUR3(I)*GAMA1(I)
       H32(I)=HOUR3(I)*GAMA2(I)
       H33(I)=HOUR3(I)*GAMA3(I)
       H34(I)=HOUR3(I)*GAMA4(I)
       EHOU(I) = EHOU(I) + HOUR3(I)*HG1(I)
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
       EHOU(I) = EHOU(I) + HOUR(I,4)*HG1(I) + HOUR(I,5)*HG2(I)
       EHOU(I) = DT1C(I) * EHOU(I)*OFF(I)
       EHOURT = EHOURT + EHOU(I)
       B11(I)= HOUR(I,4)*OFF(I)
       B12(I)=-HOUR(I,4)*OFF(I)
       B13(I)= HOUR(I,4)*OFF(I)
       B14(I)=-HOUR(I,4)*OFF(I)
       B21(I)= HOUR(I,5)*OFF(I)
       B22(I)=-HOUR(I,5)*OFF(I)
       B23(I)= HOUR(I,5)*OFF(I)
       B24(I)=-HOUR(I,5)*OFF(I)
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
     &             (OUTP_CS(25)==1)) THEN
        DO I=JFT,JLT
          EANI(NFT+NUMELS+I) = EANI(NFT+NUMELS+I)+EHOU(I)
        ENDDO
      ENDIF
C
      RETURN
      END
