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
Chd|  RGWALL                        source/constraints/general/rwall/rgwall.F
Chd|-- called by -----------
Chd|        RGWAL0                        source/constraints/general/rwall/rgwal0.F
Chd|        RGWAL0_IMP                    source/constraints/general/rwall/rgwal0.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|====================================================================
      SUBROUTINE RGWALL(X     ,A     ,V     ,RWL    ,NSW   ,
     2                  NSN   ,ITIED ,MSR   ,MS     ,WEIGHT,
     3                  ICONT ,RWSAV ,FRWL6 ,IMP_S  ,NT_RW ,
     4                  IDDL  ,IKC   ,NDOF  ,NODNX_SMS, WEIGHT_MD)
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
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr11_c.inc"
#include      "impl1_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, ITIED, MSR,ICONT,IMP_S,NT_RW
      INTEGER NSW(*), WEIGHT(*),IDDL(*),IKC(*),NDOF(*), NODNX_SMS(*)
      INTEGER WEIGHT_MD(*)
C     REAL ou REAL*8
      my_real
     .   X(*), A(*), V(*), RWL(*), MS(*), RWSAV(*)
      DOUBLE PRECISION 
     .   FRWL6(7,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER M3, M2, M1, I, N, N3, N2, N1,NINDEX, IFQ, J, K, JJ,
     .        INDEX(NSN)

C     REAL ou REAL*8
      my_real
     .   XWL, YWL, ZWL, VXW, VYW, VZW, TFXTN, FACT,
     .   TFXT, VX, VY, VZ, UX, UY, UZ, XC, YC, ZC, DP, DV, DA, DVT,
     .   FNXN, FNYN, FNZN, FNXT, FNYT, FNZT, FNDFN, FTDFT, FRIC, FRIC2,
     .   FCOE,MSW,FAC,ALPHA,ALPHI,
     .   F1(NSN), F2(NSN), F3(NSN), F4(NSN), F5(NSN), F6(NSN), F7(NSN),
     .   TFXT2, TFXTN2, WEWE2
      DOUBLE PRECISION 
     .   FRWL6_L(7,6)
C-----------------------------------------------
      IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
C-------------
C       STANDARD CASE
C------------

        ICONT=0
        CALL MY_BARRIER
!       We need an OMP barrier here because each thread initializes 
!       the variables : without barrier ICONT can be set to 1 by a 
!       thread in the !$OMP DO loop whereas another (late) thread initializes 
!       ICONT to 0
!       ICONT is a shared variable 
C
        IF(MSR.EQ.0)THEN
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
Cel changement formulation : plus d'impasse sur contribution force
         VXW=V(M1)+A(M1)*DT12
         VYW=V(M2)+A(M2)*DT12
         VZW=V(M3)+A(M3)*DT12
         XWL=X(M1)+VXW*DT2
         YWL=X(M2)+VYW*DT2
         ZWL=X(M3)+VZW*DT2
        ENDIF
        TFXT =ZERO     
        TFXTN=ZERO
        TFXT2 =ZERO     
        TFXTN2=ZERO     
        NINDEX=0
C    
        IFQ = NINT(RWL(15))
        IF (IFQ > 0) THEN
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
            DP=XC*RWL(1)+YC*RWL(2)+ZC*RWL(3)
            IF(DP > ZERO)THEN
              K = 3*(I-1)
              RWSAV(K+1) = ZERO
              RWSAV(K+2) = ZERO
              RWSAV(K+3) = ZERO
              CYCLE              
            END IF
            ICONT=1
C---        test pour noeuds penetres
            IF((VX-VXW)*RWL(1)+(VY-VYW)*RWL(2)+(VZ-VZW)*RWL(3) > ZERO)
     .        CYCLE
            NINDEX = NINDEX+1
            INDEX(NINDEX) = I
          END DO
!$OMP END DO
        ELSE
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
            DP=XC*RWL(1)+YC*RWL(2)+ZC*RWL(3)
            IF(DP > ZERO) CYCLE
            ICONT=1
C---        test pour noeuds penetres
            IF((VX-VXW)*RWL(1)+(VY-VYW)*RWL(2)+(VZ-VZW)*RWL(3) > ZERO)
     .        CYCLE
            NINDEX = NINDEX+1
            INDEX(NINDEX) = I
          END DO
!$OMP END DO
        ENDIF
      ELSE
C-----------------     
C     AMS     
C-----------------     
        ICONT=0
        CALL MY_BARRIER
!       We need an OMP barrier here because each thread initializes 
!       the variables : without barrier ICONT can be set to 1 by a 
!       thread in the !$OMP DO loop whereas a (late) thread initialize 
!       ICONT to 0
!       ICONT is a shared variable 
C
        IF(MSR.EQ.0)THEN
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
Cel changement formulation : plus d'impasse sur contribution force
         VXW=V(M1)+A(M1)*DT12
         VYW=V(M2)+A(M2)*DT12
         VZW=V(M3)+A(M3)*DT12
         XWL=X(M1)+VXW*DT2
         YWL=X(M2)+VYW*DT2
         ZWL=X(M3)+VZW*DT2
        ENDIF
        TFXT =ZERO     
        TFXTN=ZERO
        TFXT2 =ZERO     
        TFXTN2=ZERO      
        NINDEX=0
C     
        IFQ = NINT(RWL(15))
        IF (IFQ > 0) THEN
!$OMP DO
          DO I=1,NSN
            N=NSW(I)
            IF(NODNX_SMS(N)/=0)CYCLE
C      
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
            DP=XC*RWL(1)+YC*RWL(2)+ZC*RWL(3)
            IF(DP > ZERO)THEN
              K = 3*(I-1)
              RWSAV(K+1) = ZERO
              RWSAV(K+2) = ZERO
              RWSAV(K+3) = ZERO    
              CYCLE
            ENDIF
            ICONT=1
C---        test pour noeuds penetres
            IF((VX-VXW)*RWL(1)+(VY-VYW)*RWL(2)+(VZ-VZW)*RWL(3) > ZERO)
     .        CYCLE
            NINDEX = NINDEX+1
            INDEX(NINDEX) = I
          END DO
!$OMP END DO
        ELSE
!$OMP DO
          DO I=1,NSN
            N=NSW(I)
            IF(NODNX_SMS(N)/=0)CYCLE
C      
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
            DP=XC*RWL(1)+YC*RWL(2)+ZC*RWL(3)
            IF(DP.GT.ZERO) CYCLE
            ICONT=1
C---        test pour noeuds penetres
            IF((VX-VXW)*RWL(1)+(VY-VYW)*RWL(2)+(VZ-VZW)*RWL(3) > ZERO)
     .        CYCLE
            NINDEX = NINDEX+1
            INDEX(NINDEX) = I
          END DO
!$OMP END DO
        ENDIF
      ENDIF
C-----------------     
      FACT=UN/DT12
      IF(ITIED.EQ.0)THEN
C
       DO 40 J = 1,NINDEX
        I = INDEX(J)
        N=NSW(I)
        N3=3*N
        N2=N3-1
        N1=N2-1
        DV=(V(N1)-VXW)*RWL(1)+(V(N2)-VYW)*RWL(2)+(V(N3)-VZW)*RWL(3)
        DA=A(N1)*RWL(1)+A(N2)*RWL(2)+A(N3)*RWL(3)
        DVT=DV+DA*DT12
C
        MSW=MS(N)
        DVT=DVT*MSW
        FNXN=DVT*RWL(1)
        FNYN=DVT*RWL(2)
        FNZN=DVT*RWL(3)
        TFXTN = TFXTN - WEIGHT_MD(N)*FACT*
     .  ((V(N1)-VXW)*FNXN+(V(N2)-VYW)*FNYN+(V(N3)-VZW)*FNZN)
        WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)     
        TFXTN2 = TFXTN2 - WEWE2*FACT*
     .  ((V(N1)-VXW)*FNXN+(V(N2)-VYW)*FNYN+(V(N3)-VZW)*FNZN)        
        F1(J) = FNXN*WEIGHT_MD(N)
        F2(J) = FNYN*WEIGHT_MD(N)
        F3(J) = FNZN*WEIGHT_MD(N)
        F4(J) = MSW*WEIGHT_MD(N) 
        F5(J) = ZERO
        F6(J) = ZERO
        F7(J) = ZERO
        A(N1)=A(N1)-DA*RWL(1)
        A(N2)=A(N2)-DA*RWL(2)
        A(N3)=A(N3)-DA*RWL(3)
        V(N1)=V(N1)-DV*RWL(1)
        V(N2)=V(N2)-DV*RWL(2)
        V(N3)=V(N3)-DV*RWL(3)
        IF(IMP_S.EQ.1) V(N1) = -DV
 40    CONTINUE
C
      ELSEIF(ITIED.EQ.1)THEN
C
       DO 60 J = 1,NINDEX
        I = INDEX(J)
        N=NSW(I)
        N3=3*N
        N2=N3-1
        N1=N2-1
        DV=(V(N1)-VXW)*RWL(1)+(V(N2)-VYW)*RWL(2)+(V(N3)-VZW)*RWL(3)
        DA=A(N1)*RWL(1)+A(N2)*RWL(2)+A(N3)*RWL(3)
        DVT=DV+DA*DT12
C
        MSW=MS(N)
        DVT=DVT*MSW
        FNXN=DVT*RWL(1)
        FNYN=DVT*RWL(2)
        FNZN=DVT*RWL(3)
        TFXTN = TFXTN - WEIGHT_MD(N)*FACT*
     .  ((V(N1)-VXW)*FNXN+(V(N2)-VYW)*FNYN+(V(N3)-VZW)*FNZN)
        WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)
        TFXTN2 = TFXTN2 - WEWE2*FACT*
     .  ((V(N1)-VXW)*FNXN+(V(N2)-VYW)*FNYN+(V(N3)-VZW)*FNZN)        
           F1(J) = FNXN*WEIGHT_MD(N)
           F2(J) = FNYN*WEIGHT_MD(N)
           F3(J) = FNZN*WEIGHT_MD(N)
           F4(J) = MSW*WEIGHT_MD(N)
C
        FNXT=((V(N1)-VXW)+A(N1)*DT12)*MSW-FNXN
        FNYT=((V(N2)-VYW)+A(N2)*DT12)*MSW-FNYN
        FNZT=((V(N3)-VZW)+A(N3)*DT12)*MSW-FNZN
        A(N1)=ZERO
        A(N2)=ZERO
        A(N3)=ZERO
        V(N1)=VXW
        V(N2)=VYW
        V(N3)=VZW
        F5(J) = FNXT*WEIGHT_MD(N)
        F6(J) = FNYT*WEIGHT_MD(N)
        F7(J) = FNZT*WEIGHT_MD(N)
 60    CONTINUE
C
      ELSE
C
       IF (IFQ.GT.0.) THEN
C---     friction filtering
         FRIC = RWL(13)
         ALPHA= RWL(14)
         IF (IFQ.EQ.3) ALPHA = ALPHA * DT12
         ALPHI = UN - ALPHA
         FRIC2 = FRIC**2
         DO J = 1,NINDEX
           I = INDEX(J)
           N=NSW(I)
           N3=3*N
           N2=N3-1
           N1=N2-1
           DV=(V(N1)-VXW)*RWL(1)+(V(N2)-VYW)*RWL(2)+(V(N3)-VZW)*RWL(3)
           DA=A(N1)*RWL(1)+A(N2)*RWL(2)+A(N3)*RWL(3)
           DVT=DV+DA*DT12
           DVT=DVT*MS(N)
           FNXN=DVT*RWL(1)
           FNYN=DVT*RWL(2)
           FNZN=DVT*RWL(3)
           TFXTN = TFXTN - WEIGHT_MD(N)*FACT*
     .     ((V(N1)-VXW)*FNXN+(V(N2)-VYW)*FNYN+(V(N3)-VZW)*FNZN)
           WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)
           TFXTN2 = TFXTN2 - WEWE2*FACT*
     .     ((V(N1)-VXW)*FNXN+(V(N2)-VYW)*FNYN+(V(N3)-VZW)*FNZN)           
           F1(J) = FNXN*WEIGHT_MD(N)
           F2(J) = FNYN*WEIGHT_MD(N)
           F3(J) = FNZN*WEIGHT_MD(N)
           F4(J) = MS(N)*WEIGHT_MD(N) 
           FNXT=((V(N1)-VXW)+A(N1)*DT12)*MS(N)-FNXN
           FNYT=((V(N2)-VYW)+A(N2)*DT12)*MS(N)-FNYN
           FNZT=((V(N3)-VZW)+A(N3)*DT12)*MS(N)-FNZN 
C---       filter
           K = 3*(I-1)+1
           FNXT = FNXT * ALPHA + RWSAV(K)   * ALPHI
           FNYT = FNYT * ALPHA + RWSAV(K+1) * ALPHI
           FNZT = FNZT * ALPHA + RWSAV(K+2) * ALPHI
C---
           FNDFN=FNXN**2+FNYN**2+FNZN**2
           FTDFT=FNXT**2+FNYT**2+FNZT**2
           IF (FNDFN.EQ.0) THEN
             RWSAV(K)   = ZERO
             RWSAV(K+1) = ZERO
             RWSAV(K+2) = ZERO
           ELSE
             RWSAV(K)   = FNXT
             RWSAV(K+1) = FNYT
             RWSAV(K+2) = FNZT
           ENDIF
           FCOE=MIN(UN,FRIC*SQRT(FNDFN/MAX(EM20,FTDFT)))
           FNXT=FCOE*FNXT
           FNYT=FCOE*FNYT
           FNZT=FCOE*FNZT
           FAC=1./(DT12*MS(N))
           A(N1)=A(N1)-(DA*RWL(1)+FNXT*FAC)
           A(N2)=A(N2)-(DA*RWL(2)+FNYT*FAC)
           A(N3)=A(N3)-(DA*RWL(3)+FNZT*FAC)
           V(N1)=V(N1)-DV*RWL(1)
           V(N2)=V(N2)-DV*RWL(2)
           V(N3)=V(N3)-DV*RWL(3)
           TFXT = TFXT - WEIGHT_MD(N)*
     .     ((V(N1)-VXW)*FNXT+(V(N2)-VYW)*FNYT+(V(N3)-VZW)*FNZT)
           WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)
           TFXT2 = TFXT2 - WEWE2*
     .     ((V(N1)-VXW)*FNXT+(V(N2)-VYW)*FNYT+(V(N3)-VZW)*FNZT)           
           F5(J) = FNXT*WEIGHT_MD(N)
           F6(J) = FNYT*WEIGHT_MD(N)
           F7(J) = FNZT*WEIGHT_MD(N)
           IF(IMP_S.EQ.1) V(N1) = -DV
         ENDDO
       ELSE
C---     no friction filtering
         FRIC=RWL(13)
         FRIC2=FRIC**2
         DO J = 1,NINDEX
           I = INDEX(J)
           N=NSW(I)
           N3=3*N
           N2=N3-1
           N1=N2-1
           DV=(V(N1)-VXW)*RWL(1)+(V(N2)-VYW)*RWL(2)+(V(N3)-VZW)*RWL(3)
           DA=A(N1)*RWL(1)+A(N2)*RWL(2)+A(N3)*RWL(3)
           DVT=DV+DA*DT12
           DVT=DVT*MS(N)
           FNXN=DVT*RWL(1)
           FNYN=DVT*RWL(2)
           FNZN=DVT*RWL(3)
           TFXTN = TFXTN - WEIGHT_MD(N)*FACT*
     .     ((V(N1)-VXW)*FNXN+(V(N2)-VYW)*FNYN+(V(N3)-VZW)*FNZN)
           WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N) 
           FNXT=((V(N1)-VXW)+A(N1)*DT12)*MS(N)-FNXN
           FNYT=((V(N2)-VYW)+A(N2)*DT12)*MS(N)-FNYN
           FNZT=((V(N3)-VZW)+A(N3)*DT12)*MS(N)-FNZN  
           TFXTN2 = TFXTN2 - WEWE2*
     .     ((V(N1)-VXW)*FNXT+(V(N2)-VYW)*FNYT+(V(N3)-VZW)*FNZT)
           F1(J) = FNXN*WEIGHT_MD(N)
           F2(J) = FNYN*WEIGHT_MD(N)
           F3(J) = FNZN*WEIGHT_MD(N)
           F4(J) = MS(N)*WEIGHT_MD(N) 
           FNDFN=FNXN**2+FNYN**2+FNZN**2
           FTDFT=FNXT**2+FNYT**2+FNZT**2
C
           IF(FTDFT.LE.FRIC2*FNDFN) THEN
C---         tied secnd point
             A(N1)=ZERO
             A(N2)=ZERO
             A(N3)=ZERO     
             V(N1)=VXW
             V(N2)=VYW
             V(N3)=VZW
            IF (IMP_S.EQ.1) THEN
               IF (NDOF(N).GT.0) THEN
                 JJ=IDDL(N)+1
                 IF (IKC(JJ).EQ.0)IKC(JJ)=3
                 IF (IKC(JJ+1).EQ.0)IKC(JJ+1)=3
                 IF (IKC(JJ+2).EQ.0)IKC(JJ+2)=3
               ENDIF
            ENDIF 
           ELSE
C---         sliding secnd point     
             FCOE=FRIC*SQRT(FNDFN/FTDFT)
             FNXT=FCOE*FNXT
             FNYT=FCOE*FNYT
             FNZT=FCOE*FNZT
             FAC=UN/(DT12*MS(N))
             A(N1)=A(N1)-(DA*RWL(1)+FNXT*FAC)
             A(N2)=A(N2)-(DA*RWL(2)+FNYT*FAC)
             A(N3)=A(N3)-(DA*RWL(3)+FNZT*FAC)
             V(N1)=V(N1)-DV*RWL(1)
             V(N2)=V(N2)-DV*RWL(2)
             V(N3)=V(N3)-DV*RWL(3)
             IF (IMP_S.EQ.1) THEN
              IF (NDOF(N).GT.0) THEN
                V(N1)=-DV
                A(N1)=RWL(1)
                A(N2)=RWL(2)
                A(N3)=RWL(3)    
                JJ=IDDL(N)+1
                IF (IKC(JJ).EQ.0)IKC(JJ)=10
              ENDIF
             ENDIF 
             TFXT = TFXT - WEIGHT_MD(N)*
     .        ((V(N1)-VXW)*FNXT+(V(N2)-VYW)*FNYT+(V(N3)-VZW)*FNZT)
             WEWE2 = (1-WEIGHT_MD(N))*WEIGHT(N)
             TFXT2 = TFXT2 - WEWE2*
     .        ((V(N1)-VXW)*FNXT+(V(N2)-VYW)*FNYT+(V(N3)-VZW)*FNZT)
           ENDIF
           F5(J) = FNXT*WEIGHT_MD(N)
           F6(J) = FNYT*WEIGHT_MD(N)
           F7(J) = FNZT*WEIGHT_MD(N)
         ENDDO
       ENDIF
      ENDIF
C
      IF(IMCONV.EQ.1)THEN
          TFXT=TFXT+UNDEMI*DT1*TFXTN
          TFXT2=TFXT2+UNDEMI*DT1*TFXTN2
#include "atomic.inc"
          TFEXT=TFEXT+TFXT
          TFEXT_MD=TFEXT_MD+TFXT2
C
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
C  
      IF (IMP_S.EQ.1) THEN
        IF(ITIED.EQ.1)THEN
          DO J=1,NINDEX
           I = INDEX(J)
           N=NSW(I)
           IF (NDOF(N).GT.0) THEN
           JJ=IDDL(N)+1
           IF (IKC(JJ).EQ.0)IKC(JJ)=3
           IF (IKC(JJ+1).EQ.0)IKC(JJ+1)=3
           IF (IKC(JJ+2).EQ.0)IKC(JJ+2)=3
           ENDIF 
          ENDDO
        ELSEIF(ITIED.EQ.0.OR.IFQ.GT.0)THEN
          DO J=1,NINDEX
           I = INDEX(J)
           N=NSW(I)
           IF (NDOF(N).GT.0) THEN
           N3=3*N
           N2=N3-1
           N1=N2-1
           A(N1)=RWL(1)
           A(N2)=RWL(2)
           A(N3)=RWL(3)    
           JJ=IDDL(N)+1
           IF (IKC(JJ).EQ.0)IKC(JJ)=10
           ENDIF 
          ENDDO
        ELSE
C--------c'est fait avant-------
        ENDIF
          DO J=1,NINDEX
           I = INDEX(J)
           N=NSW(I)
           IF (NDOF(N).GT.0) THEN
C to be uncommented the day it is parallel in implicit
#include "atomic.inc"
             NT_RW = NT_RW + 1
           END IF
          ENDDO
      ENDIF
C
      RETURN
      END
