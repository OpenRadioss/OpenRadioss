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
Chd|  M32PLAS                       source/materials/mat/mat032/m32plas.F
Chd|-- called by -----------
Chd|        SIGEPS32C                     source/materials/mat/mat032/sigeps32c.F
Chd|-- calls ---------------
Chd|        UROTOV                        source/airbag/uroto.F         
Chd|====================================================================
      SUBROUTINE M32PLAS(JFT    ,JLT    ,PM     ,OFF    ,EPSEQ  ,
     .                   IMAT   ,DIR    ,EZZ    ,IPLA   ,DT1C   ,
     .                   DPLA1  ,EPSPD  ,NEL    ,NGL    ,HARDM  ,
     .                   SIGNXX ,SIGNYY ,SIGNXY ,SIGNYZ ,SIGNZX ,
     .                   DEPSXX ,DEPSYY ,DEPSXY ,DEPSYZ ,DEPSZX )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "impl1_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "conv_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,IMAT,IPLA,NEL
C     REAL ou REAL*8
      my_real
     .   PM(NPROPM,*), OFF(*), DIR(NEL,2), EPSEQ(*),
     .   EZZ(*),DT1C(*),DPLA1(*),EPSPD(*),HARDM(*),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .   DEPSXX(MVSIZ),DEPSYY(MVSIZ),DEPSXY(MVSIZ),DEPSYZ(MVSIZ),
     .   DEPSZX(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ICC
      INTEGER I,NMAX,J, NINDX, INDEX(MVSIZ),NGL(MVSIZ),
     .        INDX(MVSIZ),N
C     REAL ou REAL*8
      my_real
     .   E,CA, CE, CN, NU,
     .   YMAX,CM,EPDR,EPCHK(MVSIZ),
     .   A11,A22,A1122,A12,
     .   SEQ,YLD,SCALE, EPSP,S11,S22,S12,G3(MVSIZ),NU1,
     .   D11, D22, D12, DPLA, DTINV, P,A,B,C,A1S1,A2S2,Q,UMR,
     .   S_11(MVSIZ),S_22(MVSIZ),S_12(MVSIZ),S1,S2,S3,SEQH(MVSIZ),
     .   H(MVSIZ),DPLA_I(MVSIZ),DPLA_J(MVSIZ),ETSE(MVSIZ),NU2(MVSIZ),
     .   NU3(MVSIZ),NU4(MVSIZ),NU5(MVSIZ),NNU1(MVSIZ),Q12(MVSIZ),
     .   Q21(MVSIZ),JQ(MVSIZ),JQ2(MVSIZ),A_1,A_2,A_3,
     .   ST11(MVSIZ),ST22(MVSIZ),ST12(MVSIZ),AXX(MVSIZ),AYY(MVSIZ),
     .   A_XY(MVSIZ),AXY(MVSIZ),B_1(MVSIZ),B_2(MVSIZ),B_3(MVSIZ),
     .   YLDP(MVSIZ),DR,P_1,P_2,P_3,PP1,PP2,PP3,F,DF,YLD_I,A1,
     .   A2,EPSMAX,SIGE(MVSIZ,5)
      DATA NMAX/3/
C-----------------------------------------------
      E    = PM(20,IMAT)
      NU   = PM(21,IMAT)
      CA   = PM(38,IMAT)
      CE   = PM(39,IMAT)
      CN   = PM(40,IMAT)
      YMAX = PM(42,IMAT)
      CM   = PM(43,IMAT)
      EPDR = PM(44,IMAT)
      A11  = PM(45,IMAT)
      A22  = PM(46,IMAT)
      A1122= PM(47,IMAT)
      A12  = PM(48,IMAT)
      ICC  = NINT(PM(49,IMAT))
      A1   = PM(24,IMAT)
      A2   = PM(25,IMAT) 
      EPSMAX = PM(41,IMAT)
C
      IF (IPLA == 0) THEN
C projection radiale 
C-------------------
C     CONTRAINTE ORTHO
C-------------------
        DO I=JFT,JLT
          D11 = DIR(I,1)*DIR(I,1)
          D22 = DIR(I,2)*DIR(I,2)
          D12 = DIR(I,1)*DIR(I,2)
          S11 = D11*SIGNXX(I) + D22*SIGNYY(I) + TWO*D12*SIGNXY(I)
          S22 = D22*SIGNXX(I) + D11*SIGNYY(I) - TWO*D12*SIGNXY(I)
          S12 = D12*(SIGNYY(I) - SIGNXX(I)) + ( D11 - D22 )*SIGNXY(I)
C----------------------------
C     VITESSE DE DEFORMATION
C----------------------------
          EPSP = MAX(ABS(DEPSXX(I)),ABS(DEPSYY(I)),HALF*ABS(DEPSXY(I)))
          DTINV= DT1C(I)/MAX(DT1C(I)**2,EM20)
          EPSP = EPSP*DTINV
          EPSP = MAX(EPSP ,EPDR)
          EPSPD(I) = EPSP     
          NNU1(I) = NU / (ONE - NU)    
          NU5(I) = ONE-NNU1(I) 
C-------------
C     CRITERE
C-------------
          YLD = CA*(CE+EPSEQ(I))**CN*EPSP**CM
          YLD = MIN(YLD ,YMAX)
C------------------------------------------
C     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C------------------------------------------
          SEQ = SQRT(A11*S11*S11 + A22*S22*S22
     .            -A1122*S11*S22 + A12*S12*S12)
          SCALE    = MIN(ONE,YLD /MAX(SEQ ,EM20))
          SIGNXX(I)=SIGNXX(I)*SCALE
          SIGNYY(I)=SIGNYY(I)*SCALE
          SIGNXY(I)=SIGNXY(I)*SCALE
          DPLA     = OFF(I)* MAX(ZERO,(SEQ -YLD )/E)
          EZZ(I)   = - NU5(I)*DPLA*HALF*(SIGNXX(I)+SIGNYY(I))/MAX(EM20,YLD)
          EPSEQ(I) = EPSEQ(I) + DPLA
          DPLA1(I) = DPLA
        ENDDO
      ELSEIF (IPLA == 2) THEN
C projection avec p=cste + mise a zero de s33 => sig sur le critere 
C-------------------
C     CONTRAINTE ORTHO
C-------------------
        DO I=JFT,JLT
          D11 = DIR(I,1)*DIR(I,1)
          D22 = DIR(I,2)*DIR(I,2)
          D12 = DIR(I,1)*DIR(I,2)
          S11 = D11*SIGNXX(I) + D22*SIGNYY(I) + TWO*D12*SIGNXY(I)
          S22 = D22*SIGNXX(I) + D11*SIGNYY(I) - TWO*D12*SIGNXY(I)
          S12 = D12*(SIGNYY(I) - SIGNXX(I)) + ( D11 - D22 )*SIGNXY(I)
C----------------------------
C     VITESSE DE DEFORMATION
C----------------------------
          EPSP = MAX(ABS(DEPSXX(I)),ABS(DEPSYY(I)),HALF*ABS(DEPSXY(I)))
          DTINV= DT1C(I)/MAX(DT1C(I)**2,EM20)
          EPSP = EPSP*DTINV
          EPSP = MAX(EPSP ,EPDR)
          EPSPD(I) = EPSP          
C-------------
C     CRITERE
C-------------
          YLD =CA*(CE+EPSEQ(I))**CN*EPSP**CM
          YLD = MIN(YLD ,YMAX)
C------------------------------------------
C     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C------------------------------------------
          NU1 = NU/(ONE - NU)
          NU5(I) = ONE-NU1 
          G3(I)  = THREE_HALF*E/(ONE + NU)
          P   = -(S11+S22) * THIRD
          Q   = (ONE -NU1)*P
          S11 = S11+Q
          S22 = S22+Q
          A1S1 = A11*S11
          A2S2 = A22*S22
          A = A1S1*S11 + A2S2*S22 - A1122*S11*S22 + A12*S12*S12
          B = -Q*(A1S1 + A2S2 - HALF*A1122*(S11+S22))
          C = (A11+A22-A1122)*Q*Q
          SEQ = SQRT(A+B+B+C)
          C = C - YLD*YLD
          SCALE = MIN(ONE,(-B + SQRT(MAX(ZERO,B*B-A*C)))/MAX(A ,EM20))
          UMR = ONE - SCALE
          Q = Q*UMR
          SIGNXX(I) = SIGNXX(I)*SCALE - Q 
          SIGNYY(I) = SIGNYY(I)*SCALE - Q
          SIGNXY(I) = SIGNXY(I)*SCALE
          DPLA     = OFF(I)*SEQ*UMR/MAX(EM20,G3(I))
          EZZ(I)   = -NU5(I)*DPLA*HALF*(SIGNXX(I)+SIGNYY(I)) / YLD
          EPSEQ(I) = EPSEQ(I) + DPLA
          DPLA1(I) = DPLA
        ENDDO
C Projection iterative
      ELSEIF (IPLA == 1) THEN
C-- -----------------------
C         CONTRAINTES ORHTO
C-------------------------
        DO I=JFT,JLT
          D11 = DIR(I,1)*DIR(I,1)
          D22 = DIR(I,2)*DIR(I,2)
          D12 = DIR(I,1)*DIR(I,2)
          S_11(I) = D11*SIGNXX(I) + D22*SIGNYY(I) + TWO*D12*SIGNXY(I)
          S_22(I) = D22*SIGNXX(I) + D11*SIGNYY(I) - TWO*D12*SIGNXY(I)
          S_12(I) = D12*(SIGNYY(I) - SIGNXX(I)) + ( D11 - D22 )*SIGNXY(I)
          NNU1(I) = NU / (ONE - NU)
C----------------------------
C     VITESSE DE DEFORMATION
C----------------------------
          EPSP = MAX(ABS(DEPSXX(I)),ABS(DEPSYY(I)),HALF*ABS(DEPSXY(I)))
          DTINV= DT1C(I)/MAX(DT1C(I)**2,EM20)
          EPSP = EPSP*DTINV
          EPSP = MAX(EPSP ,EPDR)
          EPSPD(I) = EPSP
C--------------------------------
C     HILL CRITERION
C--------------------------------
          S1=A11*S_11(I)*S_11(I)
          S2=A22*S_22(I)*S_22(I)
          S3=A1122*S_11(I)*S_22(I)
          AXY(I)=A12*S_12(I)*S_12(I)
          SEQH(I)=SQRT(S1+S2-S3+AXY(I))  
          EZZ(I) = -(DEPSXX(I)+DEPSYY(I))*NNU1(I)
          G3(I)  = THREE_HALF*E/(ONE+NU)
          YLDP(I) = CA*(CE+EPSEQ(I))**CN*EPSP**CM
          YLDP(I) = MIN(YLDP(I) ,YMAX)
          IF (YLDP(I) >= YMAX) THEN
            H(I)=ZERO
          ELSE
            H(I)=CA*CN*(CE+EPSEQ(I))**(CN-ONE)*EPSP**CM
          ENDIF
        ENDDO
C--------------------------------
C     HARDENING MODULUS
C--------------------------------
        DO I=JFT,JLT
          HARDM(I) = H(I)
        ENDDO
C-------------------------
C     GATHER PLASTIC FLOW
C-------------------------
        NINDX=0
        DO I=JFT,JLT
          IF (SEQH(I) > YLDP(I) .AND. OFF(I) == ONE) THEN
            NINDX=NINDX+1
            INDEX(NINDX)=I
          ENDIF
        ENDDO
        IF (NINDX > 0) THEN
C---------------------------
C    DEP EN CONTRAINTE PLANE
C---------------------------
#include "vectorize.inc"
          DO  J=1,NINDX
            I=INDEX(J)
            DPLA_J(I)= (SEQH(I)-YLDP(I))/(G3(I)+H(I))
            ETSE(I)= H(I)/(H(I)+E)
            NU2(I) = ONE-NU*NU
            NU3(I) = NU*HALF
            NU4(I) = HALF*(ONE-NU)
            NU5(I) = ONE-NNU1(I)
            S1=A11*NU*TWO-A1122
            S2=A22*NU*TWO-A1122
            S12=A1122-NU*(A11+A22)
            S3=SQRT(NU2(I)*(A11-A22)*(A11-A22)+S12*S12)
            IF (ABS(S1) < EM20) THEN 
              Q12(I)=ZERO
            ELSE
              Q12(I)=-(A11-A22+S3)/S1
            ENDIF
            IF (ABS(S2) < EM20) THEN 
              Q21(I)=ZERO 
            ELSE
              Q21(I)=(A11-A22+S3)/S2
            ENDIF
            JQ(I)=ONE/(ONE-Q12(I)*Q21(I))
            JQ2(I)=JQ(I)*JQ(I)
            A=A11*Q12(I)
            B=A22*Q21(I)
            A_1=(A11+A1122*Q21(I)+B*Q21(I))*JQ2(I)
            A_2=(A22+A1122*Q12(I)+A*Q12(I))*JQ2(I)
            A_3=(A+B)*JQ2(I)*TWO+A1122*(JQ2(I)*TWO-JQ(I))
            ST11(I)=S_11(I)+S_22(I)*Q12(I)
            ST22(I)=Q21(I)*S_11(I)+S_22(I)
            AXX(I)=A_1*ST11(I)*ST11(I)
            AYY(I)=A_2*ST22(I)*ST22(I)
            A_XY(I)=A_3*ST11(I)*ST22(I)
            A=A1122*NU3(I)
            B=S3*JQ(I)
            B_1(I)=A22-A-B
            B_2(I)=A11-A+B
            B_3(I)=A12*NU4(I)
          ENDDO ! DO  J=1,NINDX
C
          DO N=1,NMAX
#include "vectorize.inc"
            DO  J=1,NINDX
              I=INDEX(J)
              DPLA_I(I)=DPLA_J(I)
              IF (DPLA_I(I) > ZERO) THEN
                YLD_I =MIN(YLDP(I)+H(I)*DPLA_I(I),YMAX)
                DR  =A1*DPLA_I(I)/YLD_I
                P_1=ONE/(ONE+B_1(I)*DR)
                PP1=P_1*P_1
                P_2=ONE/(ONE+B_2(I)*DR)
                PP2=P_2*P_2
                P_3=ONE/(ONE+B_3(I)*DR)
                PP3=P_3*P_3
                F  = AXX(I)*PP1+AYY(I)*PP2-A_XY(I)*P_1*P_2+AXY(I)*PP3
     .              -YLD_I*YLD_I
                DF = -((AXX(I)*P_1-A_XY(I)*P_2*HALF)*PP1*B_1(I)+
     .                 (AYY(I)*P_2-A_XY(I)*P_1*HALF)*PP2*B_2(I)+
     .                  AXY(I)*PP3*P_3*B_3(I))*(A1-DR*H(I))/YLD_I
     .                  -H(I)*YLD_I
                DPLA_J(I)=MAX(ZERO,DPLA_I(I)-F*HALF/DF)
              ELSE
                DPLA_J(I)=ZERO
              ENDIF !IF (DPLA_I(I) > ZERO)
            ENDDO ! DO  J=1,NINDX
          ENDDO ! DO N=1,NMAX
C------------------------------------------
C     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C------------------------------------------
#include "vectorize.inc"
         DO  J=1,NINDX
           I=INDEX(J)
           EPSEQ(I) = EPSEQ(I) + DPLA_J(I)
           YLD_I =YLDP(I)+H(I)*DPLA_J(I)
           YLD_I =MIN(YLD_I ,YMAX)
           DR  =A1*DPLA_J(I)/YLD_I
           P_1=ONE/(ONE+B_1(I)*DR)
           P_2=ONE/(ONE+B_2(I)*DR)
           P_3=ONE/(ONE+B_3(I)*DR)
            S1=ST11(I)*P_1
            S2=ST22(I)*P_2
            S_11(I)=JQ(I)*(S1-S2*Q12(I))
            S_22(I)=JQ(I)*(S2-S1*Q21(I))
            S_12(I)=S_12(I)*P_3
            S1=A11*S_11(I)+A22*S_22(I)
     .        -A1122*(S_11(I)+S_22(I))*HALF
            EZZ(I) = - NU5(I)*DPLA_J(I)*S1/YLD_I
            S1 =A1122*HALF
            P_1=A11*S_11(I)-S1*S_22(I)
            P_2=A22*S_22(I)-S1*S_11(I)
            P_3=A12*S_12(I)
            DPLA1(I) = DPLA_J(I)
         ENDDO
C
         NINDX=0
         DO I=JFT,JLT
            IF (EPSEQ(I) > EPSMAX .AND. OFF(I) == ONE) THEN
             NINDX=NINDX+1
             INDX(NINDX)=I   
             OFF(I)=ZERO
            ENDIF
          ENDDO
C 
          DO I=JFT,JLT
            SIGE(I,1) = S_11(I) 
            SIGE(I,2) = S_22(I) 
            SIGE(I,3) = S_12(I) 
            SIGE(I,4) = ZERO
            SIGE(I,5) = ZERO
          ENDDO
C
          CALL UROTOV(JFT,JLT,SIGE,DIR,NEL)
C
          DO I=JFT,JLT
           SIGNXX(I)= SIGE(I,1)
           SIGNYY(I)= SIGE(I,2)
           SIGNXY(I)= SIGE(I,3)
          ENDDO
        ENDIF  !  IF (NINDX > 0) THEN
c---
        IF (NINDX > 0) THEN
          IDEL7NOK = 1
          IF (IMCONV == 1) THEN
            DO I = 1, NINDX
#include   "lockon.inc"
              WRITE(IOUT, 1000) NGL(INDX(I))
              WRITE(ISTDO,1100) NGL(INDX(I)),TT
#include   "lockoff.inc"
            ENDDO
          ENDIF
        ENDIF ! IF (NINDX > 0)
      ENDIF  !  IF (IPLA == 0) THEN
C 
 1000 FORMAT(1X,'-- RUPTURE OF SHELL ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'-- RUPTURE OF SHELL ELEMENT :',I10,' AT TIME :',G11.4)
C        
      RETURN
      END
