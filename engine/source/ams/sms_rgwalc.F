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
Chd|  SMS_RGWALC_IMPACT             source/ams/sms_rgwalc.F       
Chd|-- called by -----------
Chd|        SMS_RGWAL_0                   source/ams/sms_rgwal0.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMS_RGWALC_IMPACT(X    ,A    ,V      ,RWL  ,NSW   ,
     1                             NSN  ,ITIED,MSR    ,MS   ,WEIGHT,
     2                             NIMPACT,IMPACT ,NSMS ,NRWL_SMS)
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
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, ITIED, MSR, NIMPACT, NSMS
      INTEGER NSW(*), WEIGHT(*), IMPACT(*), NRWL_SMS(*)
C     REAL ou REAL*8
      my_real
     .   X(*), A(*), V(*), RWL(*), MS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, N3, N2, N1, K, J, M1, M2, M3
C     REAL ou REAL*8
      my_real
     .   RA2, XWL, YWL, ZWL, VXW, VYW, VZW, 
     .   TFXT, VX, VY, VZ, UX, UY, UZ, XC, YC, ZC, DD1, DD,
     .   DP, XT, YT, ZT, XX, XN, YN, ZN, DV, DA, DVT, 
     .   XWL0, YWL0, ZWL0
C-----------------------------------------------
      RA2=(HALF*RWL(7))**2
C
      IF(MSR.EQ.0)THEN
       XWL0=RWL(4)
       YWL0=RWL(5)
       ZWL0=RWL(6)
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
       XWL0=X(M1)
       YWL0=X(M2)
       ZWL0=X(M3)
       XWL=X(M1)+VXW*DT2
       YWL=X(M2)+VYW*DT2
       ZWL=X(M3)+VZW*DT2
      ENDIF
C
      NIMPACT=0
C
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
        DD1=XC**2+YC**2+ZC**2
        DD =XC*RWL(1)+YC*RWL(2)+ZC*RWL(3)
        DP=DD1-DD**2
        IF(DP.GT.RA2)GOTO 20
        NIMPACT = NIMPACT+1
        IMPACT(NIMPACT) = I
 20   CONTINUE
C
      IF(NIMPACT/=0.AND.ITIED==2)IFRICW=1
C
      RETURN
      END
Chd|====================================================================
Chd|  SMS_RGWALC_FRIC               source/ams/sms_rgwalc.F       
Chd|-- called by -----------
Chd|        SMS_RGWAL_0                   source/ams/sms_rgwal0.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMS_RGWALC_FRIC
     1                 (X     ,A     ,V     ,RWL    ,NSW   ,
     2                  NSN   ,ITIED ,MSR    ,MS     ,WEIGHT ,
     3                  NIMPACT,IMPACT ,NSMS  ,NRWL_SMS,FSAV ,
     4                  FOPT   ,RES    ,R     ,FREA    )
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, ITIED, MSR, NIMPACT, NSMS
      INTEGER NSW(*), WEIGHT(*), IMPACT(*), NRWL_SMS(*)
C     REAL ou REAL*8
      my_real
     .   X(*), A(*), V(*), RWL(*), MS(*), FSAV(*), 
     .   FOPT(*), RES(*), FREA(*), R(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, N3, N2, N1, J, K, M1, M2, M3

C     REAL ou REAL*8
      my_real
     .   XWL, YWL, ZWL, VXW, VYW, VZW,
     .   XWL0, YWL0, ZWL0, 
     .   XC, YC, ZC, DD1, DD, DP, XT, YT, ZT, XX, XN, YN, ZN,
     .   TFXT, DV, DA, DVT, FN,
     .   FNXN, FNYN, FNZN, FNXT, FNYT, FNZT, FNDFN, FTDFT, FRIC, FRIC2,
     .   FCOE, FAC, ALPHA, ALPHI, FXT, FYT, FZT
C-----------------------------------------------
C
      IF(MSR.EQ.0)THEN
       XWL0=RWL(4)
       YWL0=RWL(5)
       ZWL0=RWL(6)
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
       XWL0=X(M1)
       YWL0=X(M2)
       ZWL0=X(M3)
       XWL=X(M1)+VXW*DT2
       YWL=X(M2)+VYW*DT2
       ZWL=X(M3)+VZW*DT2
      ENDIF
C
      TFXT=ZERO     
C
      IF(ITIED == 2)THEN
C
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
           XC=X(N1)-XWL0
           YC=X(N2)-YWL0
           ZC=X(N3)-ZWL0
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
C---
           FN=RES(N1)*XN+RES(N2)*YN+RES(N3)*ZN
           FN=FN*DT12
           FNXN=FN*XN
           FNYN=FN*YN
           FNZN=FN*ZN
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
C apply (estimated) Ft
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
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  SMS_RGWALC_BCS_0              source/ams/sms_rgwalc.F       
Chd|-- called by -----------
Chd|        SMS_RGWAL_0                   source/ams/sms_rgwal0.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMS_RGWALC_BCS_0
     1                 (X     ,A     ,V     ,RWL    ,NSW   ,
     2                  NSN   ,ITIED ,MSR    ,MS     ,WEIGHT ,
     3                  NIMPACT,IMPACT ,NSMS  ,NRWL_SMS)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, ITIED, MSR, NIMPACT, NSMS
      INTEGER NSW(*), WEIGHT(*), IMPACT(*), NRWL_SMS(*)
C     REAL ou REAL*8
      my_real
     .   X(*), A(*), V(*), RWL(*), MS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, N3, N2, N1, J, K, M1, M2, M3

C     REAL ou REAL*8
      my_real
     .   XWL, YWL, ZWL, VXW, VYW, VZW,
     .   XWL0, YWL0, ZWL0, 
     .   XC, YC, ZC, DD1, DD, DP, XT, YT, ZT, XX, XN, YN, ZN,
     .   DV, DA, DVT, MS1
C-----------------------------------------------
C
      IF(MSR.EQ.0)THEN
       XWL0=RWL(4)
       YWL0=RWL(5)
       ZWL0=RWL(6)
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
       XWL0=X(M1)
       YWL0=X(M2)
       ZWL0=X(M3)
       XWL=X(M1)+VXW*DT2
       YWL=X(M2)+VYW*DT2
       ZWL=X(M3)+VZW*DT2
      ENDIF
C     
      IF(ITIED.EQ.0)THEN
C
       DO 40 J = 1,NIMPACT
        I = IMPACT(J)
        N=NSW(I)
        N3=3*N
        N2=N3-1
        N1=N2-1
C
        XC=X(N1)-XWL0
        YC=X(N2)-YWL0
        ZC=X(N3)-ZWL0
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
        DA=DV/DT12+DA
C
        A(N1)=A(N1)-DA*XN
        A(N2)=A(N2)-DA*YN
        A(N3)=A(N3)-DA*ZN
 40    CONTINUE
C
      ELSEIF(ITIED.EQ.1)THEN
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
 60    CONTINUE
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
C
         XC=X(N1)-XWL0
         YC=X(N2)-YWL0
         ZC=X(N3)-ZWL0
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
         DA=DV/DT12+DA
C---
         IF(IMPACT(J) > 0)THEN
C adherence
           A(N1)=-(V(N1)-VXW)/DT12
           A(N2)=-(V(N2)-VYW)/DT12
           A(N3)=-(V(N3)-VZW)/DT12
         ELSE
C glissement
           A(N1)=A(N1)-DA*XN
           A(N2)=A(N2)-DA*YN
           A(N3)=A(N3)-DA*ZN
         END IF
       ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  SMS_RGWALC_BCS_1              source/ams/sms_rgwalc.F       
Chd|-- called by -----------
Chd|        SMS_RGWAL_0                   source/ams/sms_rgwal0.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMS_RGWALC_BCS_1
     1                 (X     ,A     ,V     ,RWL    ,NSW   ,
     2                  NSN   ,ITIED ,MSR    ,MS     ,WEIGHT ,
     3                  NIMPACT,IMPACT ,NSMS  ,NRWL_SMS)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, ITIED, MSR, NIMPACT, NSMS
      INTEGER NSW(*), WEIGHT(*), IMPACT(*), NRWL_SMS(*)
C     REAL ou REAL*8
      my_real
     .   X(*), A(*), V(*), RWL(*), MS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, N3, N2, N1, J, M1, M2, M3

C     REAL ou REAL*8
      my_real
     .   XWL, YWL, ZWL, VXW, VYW, VZW,
     .   XWL0, YWL0, ZWL0, 
     .   XC, YC, ZC, DD1, DD, DP, XT, YT, ZT, XX, XN, YN, ZN,
     .   DA
C-----------------------------------------------
C
      IF(MSR.EQ.0)THEN
       XWL0=RWL(4)
       YWL0=RWL(5)
       ZWL0=RWL(6)
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
       XWL0=X(M1)
       YWL0=X(M2)
       ZWL0=X(M3)
       XWL=X(M1)+VXW*DT2
       YWL=X(M2)+VYW*DT2
       ZWL=X(M3)+VZW*DT2
      ENDIF
C
      IF(ITIED.EQ.0)THEN
C
       DO 40 J = 1,NIMPACT
        I = IMPACT(J)
        N=NSW(I)
        N3=3*N
        N2=N3-1
        N1=N2-1
C
        XC=X(N1)-XWL0
        YC=X(N2)-YWL0
        ZC=X(N3)-ZWL0
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
        DA =A(N1)*XN+A(N2)*YN+A(N3)*ZN
C
        A(N1)=A(N1)-DA*XN
        A(N2)=A(N2)-DA*YN
        A(N3)=A(N3)-DA*ZN
 40    CONTINUE
C
      ELSEIF(ITIED.EQ.1)THEN
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
 60    CONTINUE
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
C
          XC=X(N1)-XWL0
          YC=X(N2)-YWL0
          ZC=X(N3)-ZWL0
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
          DA =A(N1)*XN+A(N2)*YN+A(N3)*ZN
C---
          IF(IMPACT(J) > 0)THEN
C adherence
            A(N1)=ZERO
            A(N2)=ZERO
            A(N3)=ZERO
          ELSE
C glissement
            A(N1)=A(N1)-DA*XN
            A(N2)=A(N2)-DA*YN
            A(N3)=A(N3)-DA*ZN
          END IF
        ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  SMS_RGWALC_BILAN              source/ams/sms_rgwalc.F       
Chd|-- called by -----------
Chd|        SMS_RGWAL_0                   source/ams/sms_rgwal0.F       
Chd|-- calls ---------------
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|====================================================================
      SUBROUTINE SMS_RGWALC_BILAN
     1                 (X     ,FREA  ,V     ,RWL    ,NSW   ,
     2                  NSN   ,ITIED ,MSR    ,MS     ,WEIGHT ,
     3                  NIMPACT,IMPACT ,NSMS  ,NRWL_SMS,FSAV ,
     4                  FOPT   ,FRWL6  ,A     )
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, ITIED, MSR, NIMPACT, NSMS
      INTEGER NSW(*), WEIGHT(*), IMPACT(*), NRWL_SMS(*)
C     REAL ou REAL*8
      my_real
     .   X(*), V(*), RWL(*), MS(*), FSAV(*), FREA(3,*),
     .   FOPT(*), A(*)
      DOUBLE PRECISION 
     .   FRWL6(7,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, N3, N2, N1, J, K, M1, M2, M3

C     REAL ou REAL*8
      my_real
     .   VXW, VYW, VZW, VX, VY, VZ, XWL0, YWL0, ZWL0,
     .   XC, YC, ZC, DD1, DD, DP, XT, YT, ZT, XX, XN, YN, ZN,
     .   TFXT,
     .   FNXN, FNYN, FNZN, FNXT, FNYT, FNZT, FN,
     .   FXN, FYN, FZN, FXT, FYT, FZT,
     .   F1(NSN), F2(NSN), F3(NSN), F4(NSN), F5(NSN), F6(NSN), F7(NSN)
C-----------------------------------------------
C
      IF(MSR.EQ.0)THEN
       XWL0=RWL(4)
       YWL0=RWL(5)
       ZWL0=RWL(6)
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
       XWL0=X(M1)
       YWL0=X(M2)
       ZWL0=X(M3)
      ENDIF
C
      TFXT=ZERO
C     
      IF(ITIED.EQ.0)THEN
C
       DO 40 J = 1,NIMPACT
        I = IMPACT(J)
        N=NSW(I)
        N3=3*N
        N2=N3-1
        N1=N2-1
C
        XC=X(N1)-XWL0
        YC=X(N2)-YWL0
        ZC=X(N3)-ZWL0
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
C
        FN=FREA(1,N)*XN+FREA(2,N)*YN+FREA(3,N)*ZN
        FN=WEIGHT(N)*FN
        FXN=FN*XN
        FYN=FN*YN
        FZN=FN*ZN
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
c        VX=V(N1)+HALF*A(N1)*DT12
c        VY=V(N2)+HALF*A(N2)*DT12
c        VZ=V(N3)+HALF*A(N3)*DT12
c        TFXT = TFXT -DT12*((VX-VXW)*FXN+(VY-VYW)*FYN+(VZ-VZW)*FZN)
 40    CONTINUE
C
      ELSE
C
       DO 60 J = 1,NIMPACT
        I = ABS(IMPACT(J))
        N=NSW(I)
        N3=3*N
        N2=N3-1
        N1=N2-1
C
        XC=X(N1)-XWL0
        YC=X(N2)-YWL0
        ZC=X(N3)-ZWL0
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
C
        FN=FREA(1,N)*XN+FREA(2,N)*YN+FREA(3,N)*ZN
        FN=WEIGHT(N)*FN
        FXN=FN*XN
        FYN=FN*YN
        FZN=FN*ZN
C
        F1(J) = FXN
        F2(J) = FYN
        F3(J) = FZN
        F4(J) = MS(N)
C
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
 60    CONTINUE
      ENDIF
C
#include "lockon.inc"
c!$OMP ATOMIC ne marche pas
      TFEXT=TFEXT+TFXT
#include "lockoff.inc"
C
C     IF (MSR.NE.0) THEN
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
