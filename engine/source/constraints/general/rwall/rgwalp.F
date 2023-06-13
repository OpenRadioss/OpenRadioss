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
Chd|  RGWALP                        source/constraints/general/rwall/rgwalp.F
Chd|-- called by -----------
Chd|        RGWAL0                        source/constraints/general/rwall/rgwal0.F
Chd|        RGWAL0_IMP                    source/constraints/general/rwall/rgwal0.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|====================================================================
      SUBROUTINE RGWALP(X    ,A    ,V    ,RWL  ,NSW   ,
     1                  NSN  ,ITIED,MSR  ,MS   ,WEIGHT,
     2                  ICONT,FRWL6,IMP_S,NT_RW,IDDL  ,
     3                  IKC  ,NDOF ,NODNX_SMS ,WEIGHT_MD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr11_c.inc"
#include      "impl1_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, ITIED, MSR,ICONT,IMP_S,NT_RW
      INTEGER NSW(*), WEIGHT(*), IDDL(*),IKC(*),NDOF(*), NODNX_SMS(*),
     .        WEIGHT_MD(*)
C     REAL
      my_real
     .   X(*), A(*), V(*), RWL(*), MS(*)
      DOUBLE PRECISION 
     .   FRWL6(7,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER M3, M2, M1, I, N, N3, N2, N1, K, J,JJ,NINDEX,
     .        INDEX(NSN)
C     REAL
      my_real
     .   XWL, YWL, ZWL, VXW, VYW, VZW, TFXTN, FACT,
     .   TFXT, XL1, YL1, ZL1, XL2, YL2, ZL2, SX12, SY12, SZ12, S12,
     .   VX, VY, VZ, XC, YC, ZC, DP, DN, XCP, YCP, ZCP,
     .   SX1M, SY1M, SZ1M, PS, SM1, SXM2, SYM2, SZM2, SM2, DV, DA, DVT,
     .   FNXN, FNYN, FNZN, FNXT, FNYT, FNZT, FNDFN, FTDFT, FRIC, FRIC2,
     .   FCOE,AX,
     .   XWL0, YWL0, ZWL0, DP0, XC0, YC0, ZC0, TOL, VN, VNOLD, 
     .   DP0DT, DVX, DVY, DVZ, PREC, XPREC,
     .   F1(NSN), F2(NSN), F3(NSN), F4(NSN), F5(NSN), F6(NSN), F7(NSN),
     .   TFXT2, TFXTN2, WEWE2
      DOUBLE PRECISION 
     .   FRWL6_L(7,6)
C-----------------------------------------------
      IF(IRESP==1)THEN
        PREC=EM07
      END IF
      ICONT=0
      CALL MY_BARRIER
!       We need an OMP barrier here because each thread initializes 
!       the variables : without barrier ICONT can be set to 1 by a 
!       thread in the !$OMP DO loop whereas another (late) thread initializes 
!       ICONT to 0
!       ICONT is a shared variable 
      IF(MSR==0)THEN
       XWL=RWL(4)
       YWL=RWL(5)
       ZWL=RWL(6)
       VXW=ZERO
       VYW=ZERO
       VZW=ZERO
       XWL0=XWL
       YWL0=YWL
       ZWL0=ZWL
       VNOLD=ZERO
       VN   =ZERO
      ELSE
       M3=3*MSR
       M2=M3-1
       M1=M2-1
Cel changement formulation : plus d'impasse sur contribution force
       VXW=V(M1)+A(M1)*DT12
       VYW=V(M2)+A(M2)*DT12
       VZW=V(M3)+A(M3)*DT12
       XWL=X(M1)+VXW*DT2
       YWL=X(M2)+VYW*DT2
       ZWL=X(M3)+VZW*DT2
       XWL0=X(M1)
       YWL0=X(M2)
       ZWL0=X(M3)
       VNOLD =RWL(4)
       VN    =VXW*RWL(1)+VYW*RWL(2)+VZW*RWL(3)
       RWL(4)=VN
      ENDIF
      TFXT =ZERO    
      TFXTN=ZERO
      TFXT2 =ZERO    
      TFXTN2=ZERO    
C
      XL1=RWL(7)
      YL1=RWL(8)
      ZL1=RWL(9)
      XL2=RWL(10)
      YL2=RWL(11)
      ZL2=RWL(12)
      SX12=YL1*ZL2-ZL1*YL2
      SY12=ZL1*XL2-XL1*ZL2
      SZ12=XL1*YL2-YL1*XL2
      S12=SX12**2+SY12**2+SZ12**2
      NINDEX = 0
C----  
      IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
!$OMP DO
       DO I=1,NSN
        N=NSW(I)
        N3=3*N
        N2=N3-1
        N1=N2-1
        IF(N2D==1)THEN
         AX=X(N2)
        ELSE
         AX=ONE
        ENDIF   
C
        XC0=X(N1)-XWL0
        YC0=X(N2)-YWL0
        ZC0=X(N3)-ZWL0
        DP0=XC0*RWL(1)+YC0*RWL(2)+ZC0*RWL(3)
C
        VX=V(N1)+A(N1)*DT12
        VY=V(N2)+A(N2)*DT12
        VZ=V(N3)+A(N3)*DT12
        XC=XC0+(VX-VXW)*DT2
        YC=YC0+(VY-VYW)*DT2
        ZC=ZC0+(VZ-VZW)*DT2
        DP=DP0+((VX-VXW)*RWL(1)+(VY-VYW)*RWL(2)+(VZ-VZW)*RWL(3))*DT2
C
        TOL=TWO*DT1*MAX(
     .   ABS( (VX-VXW)*RWL(1)+(VY-VYW)*RWL(2)+(VZ-VZW)*RWL(3) ),
     .   ABS(VN-VNOLD) )
        IF(IRESP==1)THEN
          XPREC=PREC*MAX(ABS(XWL),ABS(YWL),ABS(ZWL),
     .              ABS(X(N1)),ABS(X(N2)),ABS(X(N3)),
     .              ABS(X(N1)-XWL),ABS(X(N2)-YWL),ABS(X(N3)-ZWL))
          TOL=MAX(TOL,XPREC)
        END IF
        IF(DP>ZERO.OR.DP0<=-TOL) CYCLE
C
        XCP=XC-DP*RWL(1)
        YCP=YC-DP*RWL(2)
        ZCP=ZC-DP*RWL(3)
C
        SX1M=YL1*ZCP-ZL1*YCP
        SY1M=ZL1*XCP-XL1*ZCP
        SZ1M=XL1*YCP-YL1*XCP
        PS=SX12*SX1M+SY12*SY1M+SZ12*SZ1M
C
        IF(PS<ZERO) CYCLE
C
        SM1=SX1M**2+SY1M**2+SZ1M**2
C
        IF(SM1>S12) CYCLE
C
        SXM2=YCP*ZL2-ZCP*YL2
        SYM2=ZCP*XL2-XCP*ZL2
        SZM2=XCP*YL2-YCP*XL2
        PS=SX12*SXM2+SY12*SYM2+SZ12*SZM2
C
        IF(PS<ZERO) CYCLE
C
        SM2=SXM2**2+SYM2**2+SZM2**2
C
        IF(SM2>S12) CYCLE
C
        ICONT=1
        IF((VX-VXW)*RWL(1)+(VY-VYW)*RWL(2)+(VZ-VZW)*RWL(3)>ZERO
     .     .AND.DP0>ZERO)CYCLE
        NINDEX = NINDEX + 1
        INDEX(NINDEX) = I
       END DO
!$OMP END DO       
      ELSE
!$OMP DO
       DO I=1,NSN
        N=NSW(I)
        IF(NODNX_SMS(N)/=0)CYCLE
        N3=3*N
        N2=N3-1
        N1=N2-1
        IF(N2D==1)THEN
         AX=X(N2)
        ELSE
         AX=ONE
        ENDIF   
C
        XC0=X(N1)-XWL0
        YC0=X(N2)-YWL0
        ZC0=X(N3)-ZWL0
        DP0=XC0*RWL(1)+YC0*RWL(2)+ZC0*RWL(3)
C
        VX=V(N1)+A(N1)*DT12
        VY=V(N2)+A(N2)*DT12
        VZ=V(N3)+A(N3)*DT12
        XC=XC0+(VX-VXW)*DT2
        YC=YC0+(VY-VYW)*DT2
        ZC=ZC0+(VZ-VZW)*DT2
        DP=DP0+((VX-VXW)*RWL(1)+(VY-VYW)*RWL(2)+(VZ-VZW)*RWL(3))*DT2
C
        TOL=TWO*DT1*MAX(
     .   ABS( (VX-VXW)*RWL(1)+(VY-VYW)*RWL(2)+(VZ-VZW)*RWL(3) ),
     .   ABS(VN-VNOLD) )
        IF(IRESP==1)THEN
          XPREC=PREC*MAX(ABS(XWL),ABS(YWL),ABS(ZWL),
     .              ABS(X(N1)),ABS(X(N2)),ABS(X(N3)),
     .              ABS(X(N1)-XWL),ABS(X(N2)-YWL),ABS(X(N3)-ZWL))
          TOL=MAX(TOL,XPREC)
        END IF
        IF(DP>ZERO.OR.DP0<=-TOL)CYCLE
C
        XCP=XC-DP*RWL(1)
        YCP=YC-DP*RWL(2)
        ZCP=ZC-DP*RWL(3)
C
        SX1M=YL1*ZCP-ZL1*YCP
        SY1M=ZL1*XCP-XL1*ZCP
        SZ1M=XL1*YCP-YL1*XCP
        PS=SX12*SX1M+SY12*SY1M+SZ12*SZ1M
C
        IF(PS<ZERO) CYCLE
C
        SM1=SX1M**2+SY1M**2+SZ1M**2
C
        IF(SM1>S12) CYCLE
C
        SXM2=YCP*ZL2-ZCP*YL2
        SYM2=ZCP*XL2-XCP*ZL2
        SZM2=XCP*YL2-YCP*XL2
        PS=SX12*SXM2+SY12*SYM2+SZ12*SZM2
C
        IF(PS<ZERO) CYCLE
C
        SM2=SXM2**2+SYM2**2+SZM2**2
C
        IF(SM2>S12) CYCLE
C
        ICONT=1
        IF((VX-VXW)*RWL(1)+(VY-VYW)*RWL(2)+(VZ-VZW)*RWL(3)>ZERO
     .     .AND.DP0>ZERO)CYCLE
        NINDEX = NINDEX + 1
        INDEX(NINDEX) = I
       END DO
!$OMP END DO
      END IF
C----  
      FACT=ONE/DT12
      IF(ITIED/=0)THEN
       DO 40 J = 1, NINDEX
       I = INDEX(J)
       N = NSW(I)
       N3=3*N
       N2=N3-1
       N1=N2-1
C
       XC0=X(N1)-XWL0
       YC0=X(N2)-YWL0
       ZC0=X(N3)-ZWL0
       DP0  =XC0*RWL(1)+YC0*RWL(2)+ZC0*RWL(3)
       DP0DT=-MIN(DP0,ZERO)/DT2
       DVX  =DP0DT*RWL(1)
       DVY  =DP0DT*RWL(2)
       DVZ  =DP0DT*RWL(3)
C
       DV=(V(N1)-VXW)*RWL(1)+(V(N2)-VYW)*RWL(2)+(V(N3)-VZW)*RWL(3)
       DA=A(N1)*RWL(1)+A(N2)*RWL(2)+A(N3)*RWL(3)
       DVT=DV+DA*DT12-DP0DT
       FNXN=DVT*RWL(1)*MS(N)
       FNYN=DVT*RWL(2)*MS(N)
       FNZN=DVT*RWL(3)*MS(N)
       TFXTN = TFXTN - WEIGHT_MD(N)*FACT*
     . ((V(N1)-VXW)*FNXN+(V(N2)-VYW)*FNYN+(V(N3)-VZW)*FNZN)
       WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)     
       TFXTN2 = TFXTN2 - WEWE2*FACT*
     . ((V(N1)-VXW)*FNXN+(V(N2)-VYW)*FNYN+(V(N3)-VZW)*FNZN)     
       F1(J) = FNXN*WEIGHT_MD(N)
       F2(J) = FNYN*WEIGHT_MD(N)*AX
       F3(J) = FNZN*WEIGHT_MD(N)*AX
       F4(J) = MS(N)*WEIGHT_MD(N)
       FNXT=((V(N1)-VXW)+A(N1)*DT12-DVX)*MS(N)-FNXN
       FNYT=((V(N2)-VYW)+A(N2)*DT12-DVY)*MS(N)-FNYN
       FNZT=((V(N3)-VZW)+A(N3)*DT12-DVZ)*MS(N)-FNZN
       FNDFN=FNXN**2+FNYN**2+FNZN**2
       FTDFT=FNXT**2+FNYT**2+FNZT**2
       FRIC=RWL(13)
       FRIC2=FRIC**2
       IF(FTDFT<=FRIC2*FNDFN.OR.ITIED==1) THEN
C        POINT SECND TIED
         A(N1)=ZERO
         A(N2)=ZERO
         A(N3)=ZERO 
         V(N1)=VXW+DVX
         V(N2)=VYW+DVY
         V(N3)=VZW+DVZ
         IF (IMP_S==1)THEN
              IF(NDOF(N)>0) THEN
                JJ=IDDL(N)+1
                IF (IKC(JJ)==0)IKC(JJ)=3
                IF (IKC(JJ+1)==0)IKC(JJ+1)=3
                IF (IKC(JJ+2)==0)IKC(JJ+2)=3
              ENDIF
         ENDIF
       ELSE
C        POINT SECND SLIDING
         FCOE=FRIC*SQRT(FNDFN/MAX(EM20,FTDFT))
         FNXT=FCOE*FNXT
         FNYT=FCOE*FNYT
         FNZT=FCOE*FNZT
         A(N1)=A(N1)-(DA*RWL(1)+FNXT/(DT12*MS(N)))
         A(N2)=A(N2)-(DA*RWL(2)+FNYT/(DT12*MS(N)))
         A(N3)=A(N3)-(DA*RWL(3)+FNZT/(DT12*MS(N)))
         V(N1)=V(N1)-DV*RWL(1)+DVX
         V(N2)=V(N2)-DV*RWL(2)+DVY
         V(N3)=V(N3)-DV*RWL(3)+DVZ
          IF (IMP_S==1)THEN
            IF (NDOF(N)>0) THEN
              V(N1)=-DV
              A(N1)=RWL(1)
              A(N2)=RWL(2)
              A(N3)=RWL(3)    
              JJ=IDDL(N)+1
              IF (IKC(JJ)==0)IKC(JJ)=10
            ENDIF
          ENDIF
         TFXT=TFXT-
     .      ((V(N1)-VXW)*FNXT+(V(N2)-VYW)*FNYT+(V(N3)-VZW)*FNZT)
     .       *WEIGHT_MD(N)
         WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)
         TFXT2=TFXT2-
     .      ((V(N1)-VXW)*FNXT+(V(N2)-VYW)*FNYT+(V(N3)-VZW)*FNZT)
     .       *WEWE2         
       ENDIF
       F5(J) = FNXT*WEIGHT_MD(N)
       F6(J) = FNYT*WEIGHT_MD(N)*AX
       F7(J) = FNZT*WEIGHT_MD(N)*AX
 40    CONTINUE
      ELSE
       DO 60 J = 1, NINDEX
       I = INDEX(J)
       N = NSW(I)
       N3=3*N
       N2=N3-1
       N1=N2-1
       XC0=X(N1)-XWL0
       YC0=X(N2)-YWL0
       ZC0=X(N3)-ZWL0
       DP0  =XC0*RWL(1)+YC0*RWL(2)+ZC0*RWL(3)
       DP0DT=-MIN(DP0,ZERO)/DT2
       DVX  =DP0DT*RWL(1)
       DVY  =DP0DT*RWL(2)
       DVZ  =DP0DT*RWL(3)
       DV=(V(N1)-VXW)*RWL(1)+(V(N2)-VYW)*RWL(2)+(V(N3)-VZW)*RWL(3)
       DA=A(N1)*RWL(1)+A(N2)*RWL(2)+A(N3)*RWL(3)
       DVT=DV+DA*DT12-DP0DT
       FNXN=DVT*RWL(1)*MS(N)
       FNYN=DVT*RWL(2)*MS(N)
       FNZN=DVT*RWL(3)*MS(N)
       TFXTN = TFXTN - WEIGHT_MD(N)*FACT*
     . ((V(N1)-VXW)*FNXN+(V(N2)-VYW)*FNYN+(V(N3)-VZW)*FNZN)
       WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)     
       TFXTN2 = TFXTN2 - WEWE2*FACT*
     . ((V(N1)-VXW)*FNXN+(V(N2)-VYW)*FNYN+(V(N3)-VZW)*FNZN)     
       F1(J) = FNXN*WEIGHT_MD(N)
       F2(J) = FNYN*WEIGHT_MD(N)*AX
       F3(J) = FNZN*WEIGHT_MD(N)*AX
       F4(J) = MS(N)*WEIGHT_MD(N)
       F5(J) = ZERO
       F6(J) = ZERO
       F7(J) = ZERO
       A(N1)=A(N1)-DA*RWL(1)
       A(N2)=A(N2)-DA*RWL(2)
       A(N3)=A(N3)-DA*RWL(3)
       V(N1)=V(N1)-DV*RWL(1)+DVX
       V(N2)=V(N2)-DV*RWL(2)+DVY
       V(N3)=V(N3)-DV*RWL(3)+DVZ
        IF(IMP_S==1) V(N1) = -DV+DP0DT
 60    CONTINUE
      ENDIF
C
      IF(IMCONV==1)THEN          
          TFXT=TFXT+HALF*DT1*TFXTN
          TFXT2=TFXT2+HALF*DT1*TFXTN2
#include "atomic.inc"
          TFEXT=TFEXT+TFXT
          TFEXT_MD=TFEXT_MD+TFXT2

          DO K = 1, 6
            FRWL6_L(1,K) = ZERO
            FRWL6_L(2,K) = ZERO
            FRWL6_L(3,K) = ZERO
            FRWL6_L(4,K) = ZERO
            FRWL6_L(5,K) = ZERO
            FRWL6_L(6,K) = ZERO
            FRWL6_L(7,K) = ZERO
          END DO
          CALL SUM_6_FLOAT(1, NINDEX, F1, FRWL6_L(1,1), 7)
          CALL SUM_6_FLOAT(1, NINDEX, F2, FRWL6_L(2,1), 7)
          CALL SUM_6_FLOAT(1, NINDEX, F3, FRWL6_L(3,1), 7)
          CALL SUM_6_FLOAT(1, NINDEX, F4, FRWL6_L(4,1), 7)
          CALL SUM_6_FLOAT(1, NINDEX, F5, FRWL6_L(5,1), 7)
          CALL SUM_6_FLOAT(1, NINDEX, F6, FRWL6_L(6,1), 7)
          CALL SUM_6_FLOAT(1, NINDEX, F7, FRWL6_L(7,1), 7)
          
#include "lockon.inc"
          DO K = 1, 6
            FRWL6(1,K) = FRWL6(1,K)+FRWL6_L(1,K)
            FRWL6(2,K) = FRWL6(2,K)+FRWL6_L(2,K)
            FRWL6(3,K) = FRWL6(3,K)+FRWL6_L(3,K)
            FRWL6(4,K) = FRWL6(4,K)+FRWL6_L(4,K)
            FRWL6(5,K) = FRWL6(5,K)+FRWL6_L(5,K)
            FRWL6(6,K) = FRWL6(6,K)+FRWL6_L(6,K)
            FRWL6(7,K) = FRWL6(7,K)+FRWL6_L(7,K)
          END DO
#include "lockoff.inc"
      ENDIF
      
      IF (IMP_S==1) THEN
        IF(ITIED==0)THEN
          DO J=1,NINDEX
           I = INDEX(J)
           N=NSW(I)
           IF (NDOF(N)>0) THEN
           N3=3*N
           N2=N3-1
           N1=N2-1
           A(N1)=RWL(1)
           A(N2)=RWL(2)
           A(N3)=RWL(3)    
           JJ=IDDL(N)+1
           IF (IKC(JJ)==0)IKC(JJ)=10
           ENDIF 
          ENDDO
        ENDIF
        DO J=1,NINDEX
          I = INDEX(J)
          N=NSW(I)
          IF (NDOF(N)>0) THEN
C to be uncommented the day it is parallel in implicit
#include "atomic.inc"
            NT_RW = NT_RW + 1
          END IF
        ENDDO
      ENDIF
C
      RETURN
      END
