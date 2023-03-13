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
Chd|  SMS_RGWALL_IMPACT             source/ams/sms_rgwall.F       
Chd|-- called by -----------
Chd|        SMS_RGWAL_0                   source/ams/sms_rgwal0.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMS_RGWALL_IMPACT
     1                 (X     ,A     ,V     ,RWL    ,NSW   ,
     2                  NSN   ,ITIED ,MSR    ,MS     ,WEIGHT ,
     3                  RWSAV ,NIMPACT,IMPACT ,NSMS  ,NRWL_SMS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, ITIED, MSR, NIMPACT, NSMS
      INTEGER NSW(*), WEIGHT(*), IMPACT(*), NRWL_SMS(*)
C     REAL
      my_real
     .   X(*), A(*), V(*), RWL(*), MS(*), RWSAV(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, N3, N2, N1,IFQ, J, K, M1, M2, M3

C     REAL
      my_real
     .   XWL, YWL, ZWL, VXW, VYW, VZW,
     .   VX, VY, VZ, UX, UY, UZ, XC, YC, ZC, DP
C-----------------------------------------------
C
C     fixed wall only.
C
      IF(MSR==0)THEN
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
        VXW=V(M1)+A(M1)*DT12
        VYW=V(M2)+A(M2)*DT12
        VZW=V(M3)+A(M3)*DT12
        XWL=X(M1)+VXW*DT2
        YWL=X(M2)+VYW*DT2
        ZWL=X(M3)+VZW*DT2
      ENDIF
C
      NIMPACT=0
C
      IFQ = NINT(RWL(15))
      IF (IFQ>0) THEN
        DO 20 J=1,NSMS
          I=NRWL_SMS(J)
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
          IF(DP>ZERO)THEN
            K = 3*(I-1)
            RWSAV(K+1) = ZERO
            RWSAV(K+2) = ZERO
            RWSAV(K+3) = ZERO
            GOTO 20
          ENDIF
C---      test pour noeuds penetres
          IF((VX-VXW)*RWL(1)+(VY-VYW)*RWL(2)+(VZ-VZW)*RWL(3)>ZERO)
     .      GOTO 20
          NIMPACT = NIMPACT+1
          IMPACT(NIMPACT) = I
   20   CONTINUE
      ELSE
        DO 21 J=1,NSMS
          I=NRWL_SMS(J)
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
          IF(DP>ZERO) GOTO 21
C---      test pour noeuds penetres
          IF((VX-VXW)*RWL(1)+(VY-VYW)*RWL(2)+(VZ-VZW)*RWL(3)>ZERO)
     .      GOTO 21
          NIMPACT = NIMPACT+1
          IMPACT(NIMPACT) = I
   21   CONTINUE
      ENDIF
C
      IF(NIMPACT/=0.AND.ITIED==2)IFRICW=1
C
      RETURN
      END
Chd|====================================================================
Chd|  SMS_RGWALL_FRIC               source/ams/sms_rgwall.F       
Chd|-- called by -----------
Chd|        SMS_RGWAL_0                   source/ams/sms_rgwal0.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMS_RGWALL_FRIC
     1                 (X     ,A     ,V     ,RWL    ,NSW   ,
     2                  NSN   ,ITIED ,MSR    ,MS     ,WEIGHT ,
     3                  RWSAV ,NIMPACT,IMPACT ,NSMS  ,NRWL_SMS,
     4                  FSAV  ,FOPT   ,RES    ,R     ,FREA    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, ITIED, MSR, NIMPACT, NSMS
      INTEGER NSW(*), WEIGHT(*), IMPACT(*), NRWL_SMS(*)
C     REAL
      my_real
     .   X(*), A(*), V(*), RWL(*), MS(*), RWSAV(*), FSAV(*),
     .   FOPT(*), RES(*), FREA(*), R(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, N3, N2, N1,IFQ, J, K, M1, M2, M3

C     REAL
      my_real
     .   XWL, YWL, ZWL, VXW, VYW, VZW, VX, VY, VZ,
     .   DV, DA, DVT, FN, MSW,
     .   FNXN, FNYN, FNZN, FNXT, FNYT, FNZT, FNDFN, FTDFT, FRIC, FRIC2,
     .   FCOE, FAC, ALPHA, ALPHI, FXT, FYT, FZT, FXN, FYN, FZN
C-----------------------------------------------
C
      IF(MSR==0)THEN
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
        VXW=V(M1)+A(M1)*DT12
        VYW=V(M2)+A(M2)*DT12
        VZW=V(M3)+A(M3)*DT12
        XWL=X(M1)+VXW*DT2
        YWL=X(M2)+VYW*DT2
        ZWL=X(M3)+VZW*DT2
      ENDIF
C
      IFQ = NINT(RWL(15))
C
      IF(ITIED == 2)THEN
C
        IF (IFQ>0) THEN
C---     friction filtering
          FRIC = RWL(13)
          ALPHA= RWL(14)
          IF (IFQ==3) ALPHA = ALPHA * DT12
          ALPHI = ONE - ALPHA
          FRIC2 = FRIC**2
          FAC=ONE/DT12
          DO J = 1,NIMPACT
            I = ABS(IMPACT(J))
            N=NSW(I)
            N3=3*N
            N2=N3-1
            N1=N2-1
C---
            FN=RES(N1)*RWL(1)+RES(N2)*RWL(2)+RES(N3)*RWL(3)
            FN=FN*DT12
            FNXN=FN*RWL(1)
            FNYN=FN*RWL(2)
            FNZN=FN*RWL(3)
            FNXT=RES(N1)*DT12-FNXN
            FNYT=RES(N2)*DT12-FNYN
            FNZT=RES(N3)*DT12-FNZN
C---       filter
            K = 3*(I-1)+1
            FNXT = FNXT * ALPHA + RWSAV(K)   * ALPHI
            FNYT = FNYT * ALPHA + RWSAV(K+1) * ALPHI
            FNZT = FNZT * ALPHA + RWSAV(K+2) * ALPHI
C---
            FNDFN=FNXN**2+FNYN**2+FNZN**2
            FTDFT=FNXT**2+FNYT**2+FNZT**2
            IF (FNDFN==0) THEN
              RWSAV(K)   = ZERO
              RWSAV(K+1) = ZERO
              RWSAV(K+2) = ZERO
            ELSE
              RWSAV(K)   = FNXT
              RWSAV(K+1) = FNYT
              RWSAV(K+2) = FNZT
            ENDIF
C---
            IF(FTDFT <= FRIC2*FNDFN)THEN
            ELSE
C glissement
              FCOE=FRIC*SQRT(FNDFN/FTDFT)
              FNXT=FCOE*FNXT
              FNYT=FCOE*FNYT
              FNZT=FCOE*FNZT
C
C apply Ft
              FXT=FNXT*FAC
              FYT=FNYT*FAC
              FZT=FNZT*FAC
              R(N1)=R(N1)-FXT
              R(N2)=R(N2)-FYT
              R(N3)=R(N3)-FZT
C
              FREA(N1) = FXT
              FREA(N2) = FYT
              FREA(N3) = FZT
C
              IMPACT(J)=-IMPACT(J)
            END IF
          ENDDO
        ELSE
C---     no friction filtering
          FRIC=RWL(13)
          FRIC2=FRIC**2
          FAC=ONE/DT12
          DO J = 1,NIMPACT
            I = IMPACT(J)
            N=NSW(I)
            N3=3*N
            N2=N3-1
            N1=N2-1
C---
            FN=RES(N1)*RWL(1)+RES(N2)*RWL(2)+RES(N3)*RWL(3)
            FN=FN*DT12
            FNXN=FN*RWL(1)
            FNYN=FN*RWL(2)
            FNZN=FN*RWL(3)
            FNXT=RES(N1)*DT12-FNXN
            FNYT=RES(N2)*DT12-FNYN
            FNZT=RES(N3)*DT12-FNZN
C
C---
            FNDFN=FNXN**2+FNYN**2+FNZN**2
            FTDFT=FNXT**2+FNYT**2+FNZT**2
            IF(FTDFT <= FRIC2*FNDFN)THEN
C adherence
            ELSE
C glissement
              FCOE=FRIC*SQRT(FNDFN/FTDFT)
              FNXT=FCOE*FNXT
              FNYT=FCOE*FNYT
              FNZT=FCOE*FNZT
C
C apply Ft
              FXT=FNXT*FAC
              FYT=FNYT*FAC
              FZT=FNZT*FAC
              R(N1)=R(N1)-FXT
              R(N2)=R(N2)-FYT
              R(N3)=R(N3)-FZT
C
              FXN=FNXN*FAC
              FYN=FNYN*FAC
              FZN=FNZN*FAC
              FREA(N1) = FXT
              FREA(N2) = FYT
              FREA(N3) = FZT
C
              IMPACT(J)=-IMPACT(J)
            END IF
          ENDDO
        ENDIF
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  SMS_RGWALL_BCS_0              source/ams/sms_rgwall.F       
Chd|-- called by -----------
Chd|        SMS_RGWAL_0                   source/ams/sms_rgwal0.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMS_RGWALL_BCS_0
     1                 (X     ,A     ,V     ,RWL      ,NSW     ,
     2                  NSN   ,ITIED ,MSR    ,DIAG_SMS,WEIGHT  ,
     3                  RWSAV ,NIMPACT,IMPACT ,NSMS   ,NRWL_SMS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, ITIED, MSR, NIMPACT, NSMS
      INTEGER NSW(*), WEIGHT(*), IMPACT(*), NRWL_SMS(*)
C     REAL
      my_real
     .   X(*), A(*), V(*), RWL(*), DIAG_SMS(*), RWSAV(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, N3, N2, N1,IFQ, J, K, M1, M2, M3, M

C     REAL
      my_real
     .   XWL, YWL, ZWL, VXW, VYW, VZW,
     .   DV, DA, DVT, MSW, FNXN, FNYN, FNZN
C-----------------------------------------------
C
C     fixed wall only.
      IF(MSR==0)THEN
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
        VXW=V(M1)+A(M1)*DT12
        VYW=V(M2)+A(M2)*DT12
        VZW=V(M3)+A(M3)*DT12
        XWL=X(M1)+VXW*DT2
        YWL=X(M2)+VYW*DT2
        ZWL=X(M3)+VZW*DT2
      ENDIF
C
      IFQ = NINT(RWL(15))
C
      IF(ITIED==0)THEN
C
        DO 40 J = 1,NIMPACT
          I = IMPACT(J)
          N=NSW(I)
          N3=3*N
          N2=N3-1
          N1=N2-1
          DV =(V(N1)-VXW)*RWL(1)+(V(N2)-VYW)*RWL(2)+(V(N3)-VZW)*RWL(3)
          DA =A(N1)*RWL(1)+A(N2)*RWL(2)+A(N3)*RWL(3)
          DA =DV/DT12+DA
C
          A(N1)=A(N1)-DA*RWL(1)
          A(N2)=A(N2)-DA*RWL(2)
          A(N3)=A(N3)-DA*RWL(3)
C
   40   CONTINUE
C
      ELSEIF(ITIED==1)THEN
C
        DO 60 J = 1,NIMPACT
          I = IMPACT(J)
          N=NSW(I)
          N3=3*N
          N2=N3-1
          N1=N2-1
C
          A(N1)=-(V(N1)-VXW)/DT12
          A(N2)=-(V(N2)-VYW)/DT12
          A(N3)=-(V(N3)-VZW)/DT12
   60   CONTINUE
C
      ELSE
C
C---   friction
        DO J = 1,NIMPACT
          I = ABS(IMPACT(J))
          N=NSW(I)
          N3=3*N
          N2=N3-1
          N1=N2-1
C---
C adherence
          A(N1)=-(V(N1)-VXW)/DT12
          A(N2)=-(V(N2)-VYW)/DT12
          A(N3)=-(V(N3)-VZW)/DT12
        ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  SMS_RGWALL_BCS_1              source/ams/sms_rgwall.F       
Chd|-- called by -----------
Chd|        SMS_RGWAL_0                   source/ams/sms_rgwal0.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMS_RGWALL_BCS_1
     1                 (X     ,A     ,V     ,RWL    ,NSW   ,
     2                  NSN   ,ITIED ,MSR    ,MS     ,WEIGHT ,
     3                  RWSAV ,NIMPACT,IMPACT ,NSMS  ,NRWL_SMS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, ITIED, MSR, NIMPACT, NSMS
      INTEGER NSW(*), WEIGHT(*), IMPACT(*), NRWL_SMS(*)
C     REAL
      my_real
     .   X(*), A(*), V(*), RWL(*), MS(*), RWSAV(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, N3, N2, N1,IFQ, J

C     REAL
      my_real
     .       DA
C-----------------------------------------------
C
      IFQ = NINT(RWL(15))
C
      IF(ITIED==0)THEN
C
        DO 40 J = 1,NIMPACT
          I = IMPACT(J)
          N=NSW(I)
          N3=3*N
          N2=N3-1
          N1=N2-1
          DA =A(N1)*RWL(1)+A(N2)*RWL(2)+A(N3)*RWL(3)
C
          A(N1)=A(N1)-DA*RWL(1)
          A(N2)=A(N2)-DA*RWL(2)
          A(N3)=A(N3)-DA*RWL(3)
   40   CONTINUE
C
      ELSEIF(ITIED==1)THEN
C
        DO 60 J = 1,NIMPACT
          I = IMPACT(J)
          N=NSW(I)
          N3=3*N
          N2=N3-1
          N1=N2-1
          A(N1)=ZERO
          A(N2)=ZERO
          A(N3)=ZERO
   60   CONTINUE
C
      ELSE
C
C---   friction
        DO J = 1,NIMPACT
          I = ABS(IMPACT(J))
          N=NSW(I)
          N3=3*N
          N2=N3-1
          N1=N2-1
          DA=A(N1)*RWL(1)+A(N2)*RWL(2)+A(N3)*RWL(3)
C---
          IF(IMPACT(J) > 0)THEN
C adherence
            A(N1)=ZERO
            A(N2)=ZERO
            A(N3)=ZERO
          ELSE
C glissement
            A(N1)=A(N1)-DA*RWL(1)
            A(N2)=A(N2)-DA*RWL(2)
            A(N3)=A(N3)-DA*RWL(3)
          END IF
        ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  SMS_RGWALL_BILAN              source/ams/sms_rgwall.F       
Chd|-- called by -----------
Chd|        SMS_RGWAL_0                   source/ams/sms_rgwal0.F       
Chd|-- calls ---------------
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|====================================================================
      SUBROUTINE SMS_RGWALL_BILAN
     1                 (X     ,FREA  ,V     ,RWL    ,NSW   ,
     2                  NSN   ,ITIED ,MSR    ,MS     ,WEIGHT ,
     3                  RWSAV ,NIMPACT,IMPACT ,NSMS  ,NRWL_SMS,
     4                  FSAV  ,FOPT   ,FRWL6  ,A     )
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, ITIED, MSR, NIMPACT, NSMS
      INTEGER NSW(*), WEIGHT(*), IMPACT(*), NRWL_SMS(*)
C     REAL
      my_real
     .   X(*), V(*), RWL(*), MS(*), RWSAV(*), FSAV(*), FREA(3,*),
     .   FOPT(*), A(*)
      DOUBLE PRECISION
     .   FRWL6(7,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, N3, N2, N1,IFQ, J, K, M1, M2, M3

C     REAL
      my_real
     .   VXW, VYW, VZW, VX, VY, VZ,
     .   TFXT,
     .   FNXN, FNYN, FNZN, FNXT, FNYT, FNZT, FN, MSW,
     .   FXN, FYN, FZN, FXT, FYT, FZT,
     .   F1(NSN), F2(NSN), F3(NSN), F4(NSN), F5(NSN), F6(NSN), F7(NSN)
C-----------------------------------------------
C
C     fixed wall only.
      IF(MSR==0)THEN
        VXW=ZERO
        VYW=ZERO
        VZW=ZERO
      ELSE
        M3=3*MSR
        M2=M3-1
        M1=M2-1
C TFEXT only <=> dt12/2.
        VXW=V(M1)+HALF*A(M1)*DT12
        VYW=V(M2)+HALF*A(M2)*DT12
        VZW=V(M3)+HALF*A(M3)*DT12
      ENDIF
C
      TFXT= ZERO
C
      IFQ = NINT(RWL(15))
C
      IF(ITIED==0)THEN
C
        DO 40 J = 1,NIMPACT
          I = IMPACT(J)
          N=NSW(I)
C
          FN=FREA(1,N)*RWL(1)+FREA(2,N)*RWL(2)+FREA(3,N)*RWL(3)
          FN=WEIGHT(N)*FN
          FXN=FN*RWL(1)
          FYN=FN*RWL(2)
          FZN=FN*RWL(3)
C
          F1(J) = FXN
          F2(J) = FYN
          F3(J) = FZN
          F4(J) = MS(N)
          F5(J) = ZERO
          F6(J) = ZERO
          F7(J) = ZERO
C
C 1er impact (Tfext avec decalage 1/2 cycle)
c        N3=3*N
c        N2=N3-1
c        N1=N2-1
c        VX=V(N1)+HALF*A(N1)*DT12
c        VY=V(N2)+HALF*A(N2)*DT12
c        VZ=V(N3)+HALF*A(N3)*DT12
c        TFXT = TFXT -DT12*((VX-VXW)*FXN+(VY-VYW)*FYN+(VZ-VZW)*FZN)
   40   CONTINUE
C
      ELSE
C
        DO 60 J = 1,NIMPACT
          I = ABS(IMPACT(J))
          N=NSW(I)
C
          FN=FREA(1,N)*RWL(1)+FREA(2,N)*RWL(2)+FREA(3,N)*RWL(3)
          FN=WEIGHT(N)*FN
          FXN=FN*RWL(1)
          FYN=FN*RWL(2)
          FZN=FN*RWL(3)
C
          F1(J) = FXN
          F2(J) = FYN
          F3(J) = FZN
          F4(J) = MS(N)
C
          N3=3*N
          N2=N3-1
          N1=N2-1
          VX=V(N1)+HALF*A(N1)*DT12
          VY=V(N2)+HALF*A(N2)*DT12
          VZ=V(N3)+HALF*A(N3)*DT12
C 1er impact (Tfext avec decalage 1/2 cycle)
c       TFXT = TFXT -DT12*((VX-VXW)*FXN+(VY-VYW)*FYN+(VZ-VZW)*FZN)
C
          FXT=WEIGHT(N)*FREA(1,N)-FXN
          FYT=WEIGHT(N)*FREA(2,N)-FYN
          FZT=WEIGHT(N)*FREA(3,N)-FZN
          F5(J) = FXT
          F6(J) = FYT
          F7(J) = FZT
          TFXT = TFXT -DT12*((VX-VXW)*FXT+(VY-VYW)*FYT+(VZ-VZW)*FZT)
C
   60   CONTINUE
      ENDIF
C
#include "lockon.inc"
      TFEXT= TFEXT+TFXT
#include "lockoff.inc"
C
      CALL SUM_6_FLOAT(1, NIMPACT, F1, FRWL6(1,1), 7)
      CALL SUM_6_FLOAT(1, NIMPACT, F2, FRWL6(2,1), 7)
      CALL SUM_6_FLOAT(1, NIMPACT, F3, FRWL6(3,1), 7)
      CALL SUM_6_FLOAT(1, NIMPACT, F4, FRWL6(4,1), 7)
      CALL SUM_6_FLOAT(1, NIMPACT, F5, FRWL6(5,1), 7)
      CALL SUM_6_FLOAT(1, NIMPACT, F6, FRWL6(6,1), 7)
      CALL SUM_6_FLOAT(1, NIMPACT, F7, FRWL6(7,1), 7)
C
      RETURN
      END
