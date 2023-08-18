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
Chd|  RGWALC                        source/constraints/general/rwall/rgwalc.F
Chd|-- called by -----------
Chd|        RGWAL0                        source/constraints/general/rwall/rgwal0.F
Chd|        RGWAL0_IMP                    source/constraints/general/rwall/rgwal0.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|====================================================================
      SUBROUTINE RGWALC(X    ,A    ,V    ,RWL  ,NSW   ,
     .                  NSN  ,ITIED,MSR  ,MS   ,WEIGHT,
     .                  ICONT,FRWL6,IMP_S,NT_RW,IDDL  ,
     .                  IKC  ,NDOF ,NODNX_SMS, WEIGHT_MD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr11_c.inc"
#include      "impl1_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, ITIED, MSR,ICONT,IMP_S,NT_RW
      INTEGER NSW(*),WEIGHT(*), IDDL(*),IKC(*),NDOF(*), NODNX_SMS(*),
     .   WEIGHT_MD(*)      
C     REAL
      my_real
     .   X(*), A(*), V(*), RWL(*), MS(*)
      DOUBLE PRECISION 
     .   FRWL6(7,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER M3, M2, M1, I, N, N3, N2, N1,J,JJ,K,NINDEX,
     .        INDEX(NSN)
C     REAL
      my_real
     .   RA2, XWL, YWL, ZWL, VXW, VYW, VZW, TFXTN, FACT,
     .   TFXT, VX, VY, VZ, UX, UY, UZ, XC, YC, ZC, DD1, DD,
     .   DP, XT, YT, ZT, XX, XN, YN, ZN, DV, DA, DVT, FNXN, FNYN, FNZN,
     .   FNXT, FNYT, FNZT, FNDFN, FTDFT, FRIC, FRIC2, FCOE,
     .   XWLO, YWLO, ZWLO,
     .   F1(NSN), F2(NSN), F3(NSN), F4(NSN), F5(NSN), F6(NSN), F7(NSN),
     .   TFXT2, TFXTN2, WEWE2
      DOUBLE PRECISION 
     .   FRWL6_L(7,6)
C-----------------------------------------------
      ICONT=0
      CALL MY_BARRIER
!       We need an OMP barrier here because each thread initializes 
!       the variables : without barrier ICONT can be set to 1 by a 
!       thread in the !$OMP DO loop whereas another (late) thread initializes 
!       ICONT to 0
!       ICONT is a shared variable 
      RA2=(HALF*RWL(7))**2
      IF(MSR==0)THEN
       XWLO=RWL(4)
       YWLO=RWL(5)
       ZWLO=RWL(6)
       XWL=RWL(4)
       YWL=RWL(5)
       ZWL=RWL(6)
       VXW=ZERO
       VYW=ZERO
       VZW=ZERO
      ELSE
       M3=3*MSR
       M2=M3-1
       M1=M2-1
C   changement formulation : plus d'impasse sur contribution force
       VXW=V(M1)+A(M1)*DT12
       VYW=V(M2)+A(M2)*DT12
       VZW=V(M3)+A(M3)*DT12
       XWLO=X(M1)
       YWLO=X(M2)
       ZWLO=X(M3)
       XWL=X(M1)+VXW*DT2
       YWL=X(M2)+VYW*DT2
       ZWL=X(M3)+VZW*DT2
      ENDIF
      TFXT =ZERO     
      TFXTN=ZERO
      TFXT2 =ZERO     
      TFXTN2=ZERO            
      NINDEX=0     
C----  
      IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
!$OMP DO
       DO I=1,NSN
        N=NSW(I)
        N3=3*N
        N2=N3-1
        N1=N2-1
        VX=V(N1)+A(N1)*DT12
        VY=V(N2)+A(N2)*DT12
        VZ=V(N3)+A(N3)*DT12
        UX=X(N1)+VX*DT2
        UY=X(N2)+VY*DT2
        UZ=X(N3)+VZ*DT2
        XC=UX-XWL
        YC=UY-YWL
        ZC=UZ-ZWL
        DD1=XC**2+YC**2+ZC**2
        DD =XC*RWL(1)+YC*RWL(2)+ZC*RWL(3)
        DP=DD1-DD**2
        IF(DP <= RA2)THEN
          ICONT=1
          NINDEX = NINDEX+1
          INDEX(NINDEX) = I
        END IF
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
        VX=V(N1)+A(N1)*DT12
        VY=V(N2)+A(N2)*DT12
        VZ=V(N3)+A(N3)*DT12
        UX=X(N1)+VX*DT2
        UY=X(N2)+VY*DT2
        UZ=X(N3)+VZ*DT2
        XC=UX-XWL
        YC=UY-YWL
        ZC=UZ-ZWL
        DD1=XC**2+YC**2+ZC**2
        DD =XC*RWL(1)+YC*RWL(2)+ZC*RWL(3)
        DP=DD1-DD**2
        IF(DP <= RA2)THEN
          ICONT=1
          NINDEX = NINDEX+1
          INDEX(NINDEX) = I
        END IF
       END DO
!$OMP END DO
      END IF
C
      FACT=ONE/DT12
      DO J = 1,NINDEX
       I = INDEX(J)
       N=NSW(I)
       N3=3*N
       N2=N3-1
       N1=N2-1
       XC=X(N1)-XWLO
       YC=X(N2)-YWLO
       ZC=X(N3)-ZWLO
       DD1=XC**2+YC**2+ZC**2
       DD =XC*RWL(1)+YC*RWL(2)+ZC*RWL(3)
       DP=DD1-DD**2
       XT=DD*RWL(1)
       YT=DD*RWL(2)
       ZT=DD*RWL(3)
       XX=SQRT(DP)
       XN=(XC-XT)/XX
       YN=(YC-YT)/XX
       ZN=(ZC-ZT)/XX
       DV=(V(N1)-VXW)*XN+(V(N2)-VYW)*YN+(V(N3)-VZW)*ZN
       DA=A(N1)*XN+A(N2)*YN+A(N3)*ZN
       DVT=DV+DA*DT12
       FNXN=DVT*XN*MS(N)
       FNYN=DVT*YN*MS(N)
       FNZN=DVT*ZN*MS(N)
       TFXTN = TFXTN - WEIGHT_MD(N)*FACT*
     . ((V(N1)-VXW)*FNXN+(V(N2)-VYW)*FNYN+(V(N3)-VZW)*FNZN)
       WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)
       TFXTN2 = TFXTN2 - WEWE2*FACT*
     . ((V(N1)-VXW)*FNXN+(V(N2)-VYW)*FNYN+(V(N3)-VZW)*FNZN)     
       F1(J) = FNXN*WEIGHT_MD(N)
       F2(J) = FNYN*WEIGHT_MD(N)
       F3(J) = FNZN*WEIGHT_MD(N)
       F4(J) = MS(N)*WEIGHT_MD(N)
       IF(ITIED/=0)THEN
        FNXT=((V(N1)-VXW)+A(N1)*DT12)*MS(N)-FNXN
        FNYT=((V(N2)-VYW)+A(N2)*DT12)*MS(N)-FNYN
        FNZT=((V(N3)-VZW)+A(N3)*DT12)*MS(N)-FNZN
        FNDFN=FNXN**2+FNYN**2+FNZN**2
        FTDFT=FNXT**2+FNYT**2+FNZT**2
        FRIC=RWL(13)
        FRIC2=FRIC**2
        IF(FTDFT<=FRIC2*FNDFN.OR.ITIED==1) THEN
C         POINT SECND TIED
          A(N1)=ZERO
          A(N2)=ZERO
          A(N3)=ZERO
          V(N1)=VXW
          V(N2)=VYW
          V(N3)=VZW
          IF (IMP_S==1)THEN
            IF(NDOF(N)>0) THEN

              JJ=IDDL(N)+1
              IF (IKC(JJ)==0)IKC(JJ)=3
              IF (IKC(JJ+1)==0)IKC(JJ+1)=3
              IF (IKC(JJ+2)==0)IKC(JJ+2)=3
            ENDIF
          ENDIF 
        ELSE
C         POINT SECND SLIDING
          FCOE=FRIC*SQRT(FNDFN/FTDFT)
          FNXT=FCOE*FNXT
          FNYT=FCOE*FNYT
          FNZT=FCOE*FNZT
          A(N1)=A(N1)-(DA*XN+FNXT/(DT12*MS(N)))
          A(N2)=A(N2)-(DA*YN+FNYT/(DT12*MS(N)))
          A(N3)=A(N3)-(DA*ZN+FNZT/(DT12*MS(N)))
          V(N1)=V(N1)-DV*XN
          V(N2)=V(N2)-DV*YN
          V(N3)=V(N3)-DV*ZN
          IF (IMP_S==1) THEN
            IF(NDOF(N)>0) THEN
              V(N1)=-DV
              A(N1)=XN
              A(N2)=YN
              A(N3)=ZN    
              JJ=IDDL(N)+1
              IF (IKC(JJ)==0)IKC(JJ)=10
            ENDIF 
          ENDIF
          TFXT=TFXT-
     .        ((V(N1)-VXW)*FNXT+(V(N2)-VYW)*FNYT+(V(N3)-VZW)*FNZT)
     .        *WEIGHT_MD(N)
          WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)
          TFXT2=TFXT2-
     .        ((V(N1)-VXW)*FNXT+(V(N2)-VYW)*FNYT+(V(N3)-VZW)*FNZT)
     .        *WEWE2          
        ENDIF
        F5(J) = FNXT*WEIGHT_MD(N)
        F6(J) = FNYT*WEIGHT_MD(N)
        F7(J) = FNZT*WEIGHT_MD(N)
       ELSE
        F5(J) = ZERO
        F6(J) = ZERO
        F7(J) = ZERO
        A(N1)=A(N1)-DA*XN
        A(N2)=A(N2)-DA*YN
        A(N3)=A(N3)-DA*ZN
        V(N1)=V(N1)-DV*XN
        V(N2)=V(N2)-DV*YN
        V(N3)=V(N3)-DV*ZN
        IF (IMP_S==1) THEN
          IF(NDOF(N)>0) THEN
            V(N1)=-DV
            A(N1)=XN
            A(N2)=YN
            A(N3)=ZN    
            JJ=IDDL(N)+1
            IF (IKC(JJ)==0)IKC(JJ)=10
          ENDIF
        ENDIF 
       ENDIF
      ENDDO 
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
