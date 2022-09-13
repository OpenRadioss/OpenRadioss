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
Chd|  RGBODFP                       source/constraints/general/rbody/rgbodfp.F
Chd|-- called by -----------
Chd|        RBYFOR                        source/constraints/general/rbody/rbyfor.F
Chd|-- calls ---------------
Chd|        ROTBMR                        source/tools/skew/rotbmr.F    
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|====================================================================
      SUBROUTINE RGBODFP(AF   ,AM    ,X      ,FS   ,RBY  ,
     2                   NOD  ,M     ,IN     ,VR   ,STIFN,
     3                   STIFR,FOPTA ,WEIGHT ,MS   ,V    ,
     4                   IFLAG,ICODR ,ISKEW  ,SKEW ,RBF6 ,
     5                   NSN  ,NATIV_SMS,FBSAV6,IPARSENS,
     6                   NODREAC,FTHREAC,CPTREAC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr11_c.inc"
#include      "impl1_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOD(*), WEIGHT(*), ICODR(*), ISKEW(*),
     .        IFLAG,NSN,M, NATIV_SMS(*),IPARSENS,
     .        NODREAC(*),CPTREAC
C     REAL
      my_real
     .   AF(3,*), AM(3,*), X(3,*), FS(*), RBY(*), VR(3,*), SKEW(LSKEW,*),
     .   STIFN(*),STIFR(*),FOPTA(6),IN(*),MS(*), V(3,*),
     .   FTHREAC(6,*)
      DOUBLE PRECISION RBF6(8,6)
      DOUBLE PRECISION 
     .   FBSAV6(12,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, LCOD, ISK, K
C     REAL
      my_real
     .   WA1, WA2, WA3, DD, VI(3),II1,II2,II3,II4,II5,II6,II7,II8,II9,
     .   ENROTT,ENCINT,XMASST,XMOMTT,YMOMTT,ZMOMTT,MAS,AFM1,AFM2,AFM3,
     .   ARM1,ARM2,ARM3,STFM,STFRM,
     .   DET, IL1,IL2,IL3,IL4,IL5,IL6,IL7,IL8,IL9,
     .   F1(NSN), F2(NSN), F3(NSN), F4(NSN),
     .   F5(NSN), F6(NSN), F7(NSN), F8(NSN)
C-----------------------------------------------
C
c      M   =NBY(1)
C rajout optimisation pour spmd
      IF (M<0) RETURN
Cel premiere passe : calcul de force sur tous les r.b.
      IF (IFLAG==1) THEN

C CORRECTION DE L'INERTIE DU RIGID BODY POUR DT NODAL
       RBY(10) = MAX(RBY(10),IN(M))
       RBY(11) = MAX(RBY(11),IN(M))
       RBY(12) = MAX(RBY(12),IN(M))
C
c       NSN =NBY(2)
C
       IF(IDTMINS/=2)THEN
c#include "simd.inc"
        DO I=1,NSN
         N = NOD(I)
         IF(WEIGHT(N) == 1)THEN
           DD = (X(1,N)-X(1,M))**2+(X(2,N)-X(2,M))**2+(X(3,N)-X(3,M))**2
           F1(I) = STIFN(N)
           F2(I) = STIFR(N)+DD*STIFN(N)
           F3(I) = AF(1,N)
           F4(I) = AF(2,N)
           F5(I) = AF(3,N)
           F6(I) = AM(1,N)
     .        +(X(2,N)-X(2,M))*AF(3,N)-(X(3,N)-X(3,M))*AF(2,N)
           F7(I) = AM(2,N)
     .        +(X(3,N)-X(3,M))*AF(1,N)-(X(1,N)-X(1,M))*AF(3,N)
           F8(I) = AM(3,N)
     .        +(X(1,N)-X(1,M))*AF(2,N)-(X(2,N)-X(2,M))*AF(1,N)
         ELSE
           F1(I) = ZERO
           F2(I) = ZERO
           F3(I) = ZERO
           F4(I) = ZERO
           F5(I) = ZERO
           F6(I) = ZERO
           F7(I) = ZERO
           F8(I) = ZERO
         END IF
        ENDDO
       ELSE ! IF(IDTMINS/=2)THEN
c#include "simd.inc"
         DO I=1,NSN
          N = NOD(I)
          IF(WEIGHT(N) == 1)THEN
           DD = (X(1,N)-X(1,M))**2+(X(2,N)-X(2,M))**2+(X(3,N)-X(3,M))**2
           F1(I) = STIFN(N)
           F2(I) = STIFR(N)+DD*STIFN(N)
C
           F3(I) = AF(1,N)
           F4(I) = AF(2,N)
           F5(I) = AF(3,N)
           F6(I) = AM(1,N)
     .        +(X(2,N)-X(2,M))*AF(3,N)-(X(3,N)-X(3,M))*AF(2,N)
           F7(I) = AM(2,N)
     .        +(X(3,N)-X(3,M))*AF(1,N)-(X(1,N)-X(1,M))*AF(3,N)
           F8(I) = AM(3,N)
     .        +(X(1,N)-X(1,M))*AF(2,N)-(X(2,N)-X(2,M))*AF(1,N)
          ELSE
           F1(I) = ZERO
           F2(I) = ZERO
           F3(I) = ZERO
           F4(I) = ZERO
           F5(I) = ZERO
           F6(I) = ZERO
           F7(I) = ZERO
           F8(I) = ZERO
          END IF
         ENDDO
       END IF
C
C Traitement Parith/ON avant echange
C
       DO K = 1, 6
         RBF6(1,K) = ZERO
         RBF6(2,K) = ZERO
         RBF6(3,K) = ZERO
         RBF6(4,K) = ZERO
         RBF6(5,K) = ZERO
         RBF6(6,K) = ZERO
         RBF6(7,K) = ZERO
         RBF6(8,K) = ZERO
       END DO
       IF(IPARIT > 0)THEN
         CALL SUM_6_FLOAT(1  ,NSN  ,F1, RBF6(1,1),8)
         CALL SUM_6_FLOAT(1  ,NSN  ,F2, RBF6(2,1),8)
         CALL SUM_6_FLOAT(1  ,NSN  ,F3, RBF6(3,1),8)
         CALL SUM_6_FLOAT(1  ,NSN  ,F4, RBF6(4,1),8)
         CALL SUM_6_FLOAT(1  ,NSN  ,F5, RBF6(5,1),8)
         CALL SUM_6_FLOAT(1  ,NSN  ,F6, RBF6(6,1),8)
         CALL SUM_6_FLOAT(1  ,NSN  ,F7, RBF6(7,1),8)
         CALL SUM_6_FLOAT(1  ,NSN  ,F8, RBF6(8,1),8)
       ELSE
         DO I=1,NSN
           K = 1
           RBF6(1,K) = RBF6(1,K) + F1(I)
           RBF6(2,K) = RBF6(2,K) + F2(I)
           RBF6(3,K) = RBF6(3,K) + F3(I)
           RBF6(4,K) = RBF6(4,K) + F4(I)
           RBF6(5,K) = RBF6(5,K) + F5(I)
           RBF6(6,K) = RBF6(6,K) + F6(I)
           RBF6(7,K) = RBF6(7,K) + F7(I)
           RBF6(8,K) = RBF6(8,K) + F8(I) 
         END DO
       END IF
c
C
C phase 2 : sauvegarde anim + rby
C
      ELSEIF (IFLAG==2) THEN
c       NSN =NBY(2)
C
C Traitement Parith/ON apres echange
C
       STIFN(M) = STIFN(M)+
     +            RBF6(1,1)+RBF6(1,2)+RBF6(1,3)+
     +            RBF6(1,4)+RBF6(1,5)+RBF6(1,6)
       STIFR(M) = STIFR(M)+
     +            RBF6(2,1)+RBF6(2,2)+RBF6(2,3)+
     +            RBF6(2,4)+RBF6(2,5)+RBF6(2,6)
c
       AFM1 = RBF6(3,1)+RBF6(3,2)+RBF6(3,3)+
     +        RBF6(3,4)+RBF6(3,5)+RBF6(3,6)
       AFM2 = RBF6(4,1)+RBF6(4,2)+RBF6(4,3)+
     +        RBF6(4,4)+RBF6(4,5)+RBF6(4,6)
       AFM3 = RBF6(5,1)+RBF6(5,2)+RBF6(5,3)+
     +        RBF6(5,4)+RBF6(5,5)+RBF6(5,6)
       ARM1 = RBF6(6,1)+RBF6(6,2)+RBF6(6,3)+
     +        RBF6(6,4)+RBF6(6,5)+RBF6(6,6)
       ARM2 = RBF6(7,1)+RBF6(7,2)+RBF6(7,3)+
     +        RBF6(7,4)+RBF6(7,5)+RBF6(7,6)
       ARM3 = RBF6(8,1)+RBF6(8,2)+RBF6(8,3)+
     +        RBF6(8,4)+RBF6(8,5)+RBF6(8,6)
c
       AF(1,M)  = AF(1,M)+
     +            RBF6(3,1)+RBF6(3,2)+RBF6(3,3)+
     +            RBF6(3,4)+RBF6(3,5)+RBF6(3,6)
       AF(2,M)  = AF(2,M)+
     +            RBF6(4,1)+RBF6(4,2)+RBF6(4,3)+
     +            RBF6(4,4)+RBF6(4,5)+RBF6(4,6)
       AF(3,M)  = AF(3,M)+
     +            RBF6(5,1)+RBF6(5,2)+RBF6(5,3)+
     +            RBF6(5,4)+RBF6(5,5)+RBF6(5,6)
       AM(1,M)  = AM(1,M)+
     +            RBF6(6,1)+RBF6(6,2)+RBF6(6,3)+
     +            RBF6(6,4)+RBF6(6,5)+RBF6(6,6)
       AM(2,M)  = AM(2,M)+
     +            RBF6(7,1)+RBF6(7,2)+RBF6(7,3)+
     +            RBF6(7,4)+RBF6(7,5)+RBF6(7,6)
       AM(3,M)  = AM(3,M)+
     +            RBF6(8,1)+RBF6(8,2)+RBF6(8,3)+
     +            RBF6(8,4)+RBF6(8,5)+RBF6(8,6)
C
CGW      IF(NBY(3)==1)THEN
      IF(IMCONV==1)THEN
       !---
       ! reaction forces/moments in rbody TH/RBODY
       !---
!!       FS(1)=FS(1)+AF(1,M)*DT1*WEIGHT(M)
!!       FS(2)=FS(2)+AF(2,M)*DT1*WEIGHT(M)
!!       FS(3)=FS(3)+AF(3,M)*DT1*WEIGHT(M)
!!       FS(4)=FS(4)+AM(1,M)*DT1*WEIGHT(M)
!!       FS(5)=FS(5)+AM(2,M)*DT1*WEIGHT(M)
!!       FS(6)=FS(6)+AM(3,M)*DT1*WEIGHT(M)
       FS(1)=FS(1)+AFM1*DT1*WEIGHT(M)
       FS(2)=FS(2)+AFM2*DT1*WEIGHT(M)
       FS(3)=FS(3)+AFM3*DT1*WEIGHT(M)
       FS(4)=FS(4)+ARM1*DT1*WEIGHT(M)
       FS(5)=FS(5)+ARM2*DT1*WEIGHT(M)
       FS(6)=FS(6)+ARM3*DT1*WEIGHT(M)
       !---
       ! reaction forces/moments in "main node" of RBODY for TH/NODE
       !---
       IF (CPTREAC > 0) THEN
         IF(NODREAC(M) > 0) THEN
           FTHREAC(4,NODREAC(M)) = FTHREAC(4,NODREAC(M)) - AM(1,M)*DT1*WEIGHT(M)
           FTHREAC(5,NODREAC(M)) = FTHREAC(5,NODREAC(M)) - AM(2,M)*DT1*WEIGHT(M)
           FTHREAC(6,NODREAC(M)) = FTHREAC(6,NODREAC(M)) - AM(3,M)*DT1*WEIGHT(M)
         ENDIF
       ENDIF ! IF (CPTREAC . and. NODREAC(M) > 0)
      ENDIF
CGW      ENDIF
C
C
       !---
       ! reaction forces/moments in RBODY /ANIM/VECT/FOPT
       !---
!!       FOPTA(1)=AF(1,M)*WEIGHT(M)
!!       FOPTA(2)=AF(2,M)*WEIGHT(M)
!!       FOPTA(3)=AF(3,M)*WEIGHT(M)
!!       FOPTA(4)=AM(1,M)*WEIGHT(M)
!!       FOPTA(5)=AM(2,M)*WEIGHT(M)
!!       FOPTA(6)=AM(3,M)*WEIGHT(M)
       FOPTA(1)=AFM1*WEIGHT(M)
       FOPTA(2)=AFM2*WEIGHT(M)
       FOPTA(3)=AFM3*WEIGHT(M)
       FOPTA(4)=ARM1*WEIGHT(M)
       FOPTA(5)=ARM2*WEIGHT(M)
       FOPTA(6)=ARM3*WEIGHT(M)
C
       IF (IPARSENS /= 0)THEN
          DO I=1,6
            FBSAV6(1,I) = RBF6(3,I)*WEIGHT(M)
            FBSAV6(2,I) = RBF6(4,I)*WEIGHT(M)
            FBSAV6(3,I) = RBF6(5,I)*WEIGHT(M)
            FBSAV6(4,I) = RBF6(6,I)*WEIGHT(M)
            FBSAV6(5,I) = RBF6(7,I)*WEIGHT(M)
            FBSAV6(6,I) = RBF6(8,I)*WEIGHT(M)
          ENDDO
         FBSAV6(7,1) =  AF(1,M)*WEIGHT(M)
         FBSAV6(8,1) =  AF(2,M)*WEIGHT(M)
         FBSAV6(9,1) =  AF(3,M)*WEIGHT(M)
         FBSAV6(10,1) =  AM(1,M)*WEIGHT(M)
         FBSAV6(11,1) =  AM(2,M)*WEIGHT(M)
         FBSAV6(12,1) =  AM(3,M)*WEIGHT(M)
       ENDIF
c
      ISK =ISKEW(M)
      LCOD=ICODR(M)
      IF(LCOD/=0.AND.IMCONV==1)THEN
C
C      rotation de la matrice d'orientation (directions principales)
       IF(N2D==0) THEN
          VI(1)=RBY(1)*VR(1,M)+RBY(2)*VR(2,M)+RBY(3)*VR(3,M)
          VI(2)=RBY(4)*VR(1,M)+RBY(5)*VR(2,M)+RBY(6)*VR(3,M)
          VI(3)=RBY(7)*VR(1,M)+RBY(8)*VR(2,M)+RBY(9)*VR(3,M)
       ELSEIF(N2D==1) THEN
          VI(1)=ZERO
          VI(2)=ZERO
          VI(3)=RBY(9)*VR(3,M)
       ELSE
          VI(1)=RBY(1)*VR(1,M)
          VI(2)=ZERO
          VI(3)=ZERO
       ENDIF

       CALL ROTBMR(VI,RBY,DT1)
C
C      matrice d'inertie en repere global
       IF(N2D==0) THEN
C        ANALYSE 3D
          II1=RBY(10)*RBY(1)
          II2=RBY(10)*RBY(2)
          II3=RBY(10)*RBY(3)
          II4=RBY(11)*RBY(4)
          II5=RBY(11)*RBY(5)
          II6=RBY(11)*RBY(6)
          II7=RBY(12)*RBY(7)
          II8=RBY(12)*RBY(8)
          II9=RBY(12)*RBY(9)
C
          RBY(17)=RBY(1)*II1 + RBY(4)*II4 + RBY(7)*II7
          RBY(18)=RBY(1)*II2 + RBY(4)*II5 + RBY(7)*II8
          RBY(19)=RBY(1)*II3 + RBY(4)*II6 + RBY(7)*II9
          RBY(20)=RBY(2)*II1 + RBY(5)*II4 + RBY(8)*II7
          RBY(21)=RBY(2)*II2 + RBY(5)*II5 + RBY(8)*II8
          RBY(22)=RBY(2)*II3 + RBY(5)*II6 + RBY(8)*II9
          RBY(23)=RBY(3)*II1 + RBY(6)*II4 + RBY(9)*II7
          RBY(24)=RBY(3)*II2 + RBY(6)*II5 + RBY(9)*II8
          RBY(25)=RBY(3)*II3 + RBY(6)*II6 + RBY(9)*II9
C
C      ajout des termes [Iglobal] vr ^ vr
          WA1=RBY(17)*VR(1,M)+RBY(18)*VR(2,M)+RBY(19)*VR(3,M)
          WA2=RBY(20)*VR(1,M)+RBY(21)*VR(2,M)+RBY(22)*VR(3,M)
          WA3=RBY(23)*VR(1,M)+RBY(24)*VR(2,M)+RBY(25)*VR(3,M)
C
       ELSEIF(N2D==1) THEN
C        ANALYSE 2D : Axisymetry  
C             I= A 0 0
C                0 A 0
C                0 0 B 
C
          RBY(17)=RBY(10)
          RBY(21)=RBY(11)
          RBY(25)=RBY(12)
C      ajout des termes [Iglobal] vr ^ vr
          WA1=ZERO
          WA2=ZERO
          WA3=RBY(12)*VR(3,M)
C
       ELSE
C        ANALYSE 2D : Plane strain Inertia matrix 
C             I= A 0 0
C                0 B D
C                0 D C 
          II1=RBY(10)*RBY(1)
          II5=RBY(11)*RBY(5)
          II6=RBY(11)*RBY(6)
          II8=RBY(12)*RBY(8)
          II9=RBY(12)*RBY(9)
C
          RBY(17)=RBY(1)*II1 
          RBY(21)=RBY(5)*II5 + RBY(8)*II8
          RBY(22)=RBY(5)*II6 + RBY(8)*II9
          RBY(23)=ZERO
          RBY(24)=RBY(6)*II5 + RBY(9)*II8
          RBY(25)=RBY(6)*II6 + RBY(9)*II9
C
C      ajout des termes [Iglobal] vr ^ vr
          WA1=RBY(17)*VR(1,M)
          WA2=ZERO
          WA3=ZERO
       ENDIF
C
       IF (IMPL_S==0) THEN
          AM(1,M)=AM(1,M)+(WA2*VR(3,M)-WA3*VR(2,M))
          AM(2,M)=AM(2,M)+(WA3*VR(1,M)-WA1*VR(3,M))
          AM(3,M)=AM(3,M)+(WA1*VR(2,M)-WA2*VR(1,M))
       END IF

C
       IF(ISK==1)THEN
C------------------
C       REPERE GLOBAL :
C       Resolution [Iglobal] gama = M, compte-tenu des conditions aux limites
C       Ex : gamaz=0
C           | Ixx Ixy Ixz | { gamax }   { Mx }
C           | Iyx Iyy Iyz | { gamay } = { My }
C           | Izx Izy Izz | {   0   }   { Mz + DMz }  DMz inconnue
C       equivaut a
C           | Ixx Ixy | { gamax }   { Mx }
C           | Iyx Iyy | { gamay } = { My }
C           et gamaz=0
C------------------
        IF(LCOD==1)THEN
          DET=ONE/(RBY(17)*RBY(21)-RBY(18)*RBY(20))
          WA1=AM(1,M)
          WA2=AM(2,M)
          AM(1,M)=( RBY(21)*WA1-RBY(20)*WA2)*DET
          AM(2,M)=(-RBY(18)*WA1+RBY(17)*WA2)*DET
          AM(3,M)=ZERO
        ELSEIF(LCOD==2)THEN
          DET=ONE/(RBY(17)*RBY(25)-RBY(19)*RBY(23))
          WA1=AM(1,M)
          WA2=AM(3,M)
          AM(1,M)=( RBY(25)*WA1-RBY(23)*WA2)*DET
          AM(2,M)=ZERO
          AM(3,M)=(-RBY(19)*WA1+RBY(17)*WA2)*DET
        ELSEIF(LCOD==3)THEN
          AM(1,M)=AM(1,M)/RBY(17)
          AM(2,M)=ZERO
          AM(3,M)=ZERO
        ELSEIF(LCOD==4)THEN
          DET=ONE/(RBY(21)*RBY(25)-RBY(22)*RBY(24))
          WA1=AM(2,M)
          WA2=AM(3,M)
          AM(1,M)=ZERO
          AM(2,M)=( RBY(25)*WA1-RBY(24)*WA2)*DET
          AM(3,M)=(-RBY(22)*WA1+RBY(21)*WA2)*DET
        ELSEIF(LCOD==5)THEN
          AM(1,M)=ZERO
          AM(2,M)=AM(2,M)/RBY(21)
          AM(3,M)=ZERO
        ELSEIF(LCOD==6)THEN
          AM(1,M)=ZERO
          AM(2,M)=ZERO
          AM(3,M)=AM(3,M)/RBY(25)
        ELSEIF(LCOD==7)THEN
          AM(1,M)=ZERO
          AM(2,M)=ZERO
          AM(3,M)=ZERO
        ENDIF
       ELSE
C-------------------
C       REPERE OBLIQUE
C------------------
C
C       Passage dans le skew : (vitesse), moments, matrice d'inertie.
C
C       WA1=VR(1,M)
C       WA2=VR(2,M)
C       WA3=VR(3,M)
C       VL(1,M)=SKEW(1,ISK)*WA1+SKEW(4,ISK)*WA2+SKEW(7,ISK)*WA3
C       VL(2,M)=SKEW(2,ISK)*WA1+SKEW(5,ISK)*WA2+SKEW(8,ISK)*WA3
C       VL(3,M)=SKEW(3,ISK)*WA1+SKEW(6,ISK)*WA2+SKEW(9,ISK)*WA3
C
        WA1=AM(1,M)
        WA2=AM(2,M)
        WA3=AM(3,M)
        AM(1,M)=SKEW(1,ISK)*WA1+SKEW(2,ISK)*WA2+SKEW(3,ISK)*WA3
        AM(2,M)=SKEW(4,ISK)*WA1+SKEW(5,ISK)*WA2+SKEW(6,ISK)*WA3
        AM(3,M)=SKEW(7,ISK)*WA1+SKEW(8,ISK)*WA2+SKEW(9,ISK)*WA3
C
C       Resolution ds le repere local, compte-tenu des conditions aux limites
C       Ex : v3+gama3*dt12=0
C           | IL1 IL2 IL3 | { gama1    }   { M1 }
C           | IL4 IL5 IL6 | { gama2    } = { M2 }
C           | IL7 IL8 IL9 | { -v3/dt12 }   { M3 + DM3  }  DM3 inconnue
C       equivaut a
C           | IL1 IL2 IL3 | { gama1 }   { M1 }          | IL1 IL2 IL3 | { 0 }
C           | IL4 IL5 IL6 | { gama2 } = { M2 }        + | IL4 IL5 IL6 | { 0 }
C           | IL7 IL8 IL9 | { 0     }   { M3 + DM3  }   | IL7 IL8 IL9 | { v3/dt12 }
C
C       pas de solution => gama3=0, v'3=0 (reimpose dans la condition limite)
C
C           | IL1 IL2 IL3 | { gama1 }   { M1 }            | IL1 IL2 IL3 | { gama1 }   { M1 }
C           | IL4 IL5 IL6 | { gama2 } = { M2 }       <==> | IL4 IL5 IL6 | { gama2 } = { M2 }
C           | IL7 IL8 IL9 | { 0     }   { M3 + DM3  }
C
        IF(LCOD==1)THEN
C12345678901234567890123456789012345678901234567890123456789012345678901
         II1=RBY(17)*SKEW(1,ISK)+RBY(18)*SKEW(2,ISK)+RBY(19)*SKEW(3,ISK)
         II2=RBY(17)*SKEW(4,ISK)+RBY(18)*SKEW(5,ISK)+RBY(19)*SKEW(6,ISK)
         II4=RBY(20)*SKEW(1,ISK)+RBY(21)*SKEW(2,ISK)+RBY(22)*SKEW(3,ISK)
         II5=RBY(20)*SKEW(4,ISK)+RBY(21)*SKEW(5,ISK)+RBY(22)*SKEW(6,ISK)
         II7=RBY(23)*SKEW(1,ISK)+RBY(24)*SKEW(2,ISK)+RBY(25)*SKEW(3,ISK)
         II8=RBY(23)*SKEW(4,ISK)+RBY(24)*SKEW(5,ISK)+RBY(25)*SKEW(6,ISK)
         IL1=SKEW(1,ISK)*II1+SKEW(2,ISK)*II4+SKEW(3,ISK)*II7
         IL2=SKEW(1,ISK)*II2+SKEW(2,ISK)*II5+SKEW(3,ISK)*II8
         IL4=SKEW(4,ISK)*II1+SKEW(5,ISK)*II4+SKEW(6,ISK)*II7
         IL5=SKEW(4,ISK)*II2+SKEW(5,ISK)*II5+SKEW(6,ISK)*II8
C
         DET=ONE/(IL1*IL5-IL2*IL4)
         WA1=AM(1,M)
         WA2=AM(2,M)
         AM(1,M)=( IL5*WA1-IL4*WA2)*DET
         AM(2,M)=(-IL2*WA1+IL1*WA2)*DET
         AM(3,M)=ZERO
C        VL(3)=ZERO
        ELSEIF(LCOD==2)THEN
         II1=RBY(17)*SKEW(1,ISK)+RBY(18)*SKEW(2,ISK)+RBY(19)*SKEW(3,ISK)
         II3=RBY(17)*SKEW(7,ISK)+RBY(18)*SKEW(8,ISK)+RBY(19)*SKEW(9,ISK)
         II4=RBY(20)*SKEW(1,ISK)+RBY(21)*SKEW(2,ISK)+RBY(22)*SKEW(3,ISK)
         II6=RBY(20)*SKEW(7,ISK)+RBY(21)*SKEW(8,ISK)+RBY(22)*SKEW(9,ISK)
         II7=RBY(23)*SKEW(1,ISK)+RBY(24)*SKEW(2,ISK)+RBY(25)*SKEW(3,ISK)
         II9=RBY(23)*SKEW(7,ISK)+RBY(24)*SKEW(8,ISK)+RBY(25)*SKEW(9,ISK)
         IL1=SKEW(1,ISK)*II1+SKEW(2,ISK)*II4+SKEW(3,ISK)*II7
         IL3=SKEW(1,ISK)*II3+SKEW(2,ISK)*II6+SKEW(3,ISK)*II9
         IL7=SKEW(7,ISK)*II1+SKEW(8,ISK)*II4+SKEW(9,ISK)*II7
         IL9=SKEW(7,ISK)*II3+SKEW(8,ISK)*II6+SKEW(9,ISK)*II9
C
         DET=ONE/(IL1*IL9-IL3*IL7)
         WA1=AM(1,M)
         WA2=AM(3,M)
         AM(1,M)=( IL9*WA1-IL7*WA2)*DET
         AM(2,M)=ZERO
         AM(3,M)=(-IL3*WA1+IL1*WA2)*DET
C        VL(2)=ZERO
        ELSEIF(LCOD==3)THEN
         II1=RBY(17)*SKEW(1,ISK)+RBY(18)*SKEW(2,ISK)+RBY(19)*SKEW(3,ISK)
         II4=RBY(20)*SKEW(1,ISK)+RBY(21)*SKEW(2,ISK)+RBY(22)*SKEW(3,ISK)
         II7=RBY(23)*SKEW(1,ISK)+RBY(24)*SKEW(2,ISK)+RBY(25)*SKEW(3,ISK)
         IL1=SKEW(1,ISK)*II1+SKEW(2,ISK)*II4+SKEW(3,ISK)*II7
C
         AM(1,M)=AM(1,M)/IL1
         AM(2,M)=ZERO
         AM(3,M)=ZERO
C        VL(2)=ZERO
C        VL(3)=ZERO
        ELSEIF(LCOD==4)THEN
         II2=RBY(17)*SKEW(4,ISK)+RBY(18)*SKEW(5,ISK)+RBY(19)*SKEW(6,ISK)
         II3=RBY(17)*SKEW(7,ISK)+RBY(18)*SKEW(8,ISK)+RBY(19)*SKEW(9,ISK)
         II5=RBY(20)*SKEW(4,ISK)+RBY(21)*SKEW(5,ISK)+RBY(22)*SKEW(6,ISK)
         II6=RBY(20)*SKEW(7,ISK)+RBY(21)*SKEW(8,ISK)+RBY(22)*SKEW(9,ISK)
         II8=RBY(23)*SKEW(4,ISK)+RBY(24)*SKEW(5,ISK)+RBY(25)*SKEW(6,ISK)
         II9=RBY(23)*SKEW(7,ISK)+RBY(24)*SKEW(8,ISK)+RBY(25)*SKEW(9,ISK)
         IL5=SKEW(4,ISK)*II2+SKEW(5,ISK)*II5+SKEW(6,ISK)*II8
         IL6=SKEW(4,ISK)*II3+SKEW(5,ISK)*II6+SKEW(6,ISK)*II9
         IL8=SKEW(7,ISK)*II2+SKEW(8,ISK)*II5+SKEW(9,ISK)*II8
         IL9=SKEW(7,ISK)*II3+SKEW(8,ISK)*II6+SKEW(9,ISK)*II9
C
         DET=ONE/(IL5*IL9-IL6*IL8)
         WA1=AM(2,M)
         WA2=AM(3,M)
         AM(1,M)=ZERO
         AM(2,M)=( IL9*WA1-IL8*WA2)*DET
         AM(3,M)=(-IL6*WA1+IL5*WA2)*DET
C        VL(1)=ZERO
        ELSEIF(LCOD==5)THEN
         II2=RBY(17)*SKEW(4,ISK)+RBY(18)*SKEW(5,ISK)+RBY(19)*SKEW(6,ISK)
         II5=RBY(20)*SKEW(4,ISK)+RBY(21)*SKEW(5,ISK)+RBY(22)*SKEW(6,ISK)
         II8=RBY(23)*SKEW(4,ISK)+RBY(24)*SKEW(5,ISK)+RBY(25)*SKEW(6,ISK)
         IL5=SKEW(4,ISK)*II2+SKEW(5,ISK)*II5+SKEW(6,ISK)*II8
C
         AM(1,M)=ZERO
         AM(2,M)=AM(2,M)/IL5
         AM(3,M)=ZERO
C        VL(1)=ZERO
C        VL(3)=ZERO
        ELSEIF(LCOD==6)THEN
         II3=RBY(17)*SKEW(7,ISK)+RBY(18)*SKEW(8,ISK)+RBY(19)*SKEW(9,ISK)
         II6=RBY(20)*SKEW(7,ISK)+RBY(21)*SKEW(8,ISK)+RBY(22)*SKEW(9,ISK)
         II9=RBY(23)*SKEW(7,ISK)+RBY(24)*SKEW(8,ISK)+RBY(25)*SKEW(9,ISK)
         IL9=SKEW(7,ISK)*II3+SKEW(8,ISK)*II6+SKEW(9,ISK)*II9
C
         AM(1,M)=ZERO
         AM(2,M)=ZERO
         AM(3,M)=AM(3,M)/IL9
C        VL(1)=ZERO
C        VL(2)=ZERO
        ELSEIF(LCOD==7)THEN
         AM(1,M)=ZERO
         AM(2,M)=ZERO
         AM(3,M)=ZERO
C        VL(1)=ZERO
C        VL(2)=ZERO
C        VL(3)=ZERO
        ENDIF
        WA1=AM(1,M)
        WA2=AM(2,M)
        WA3=AM(3,M)
        AM(1,M)=SKEW(1,ISK)*WA1+SKEW(4,ISK)*WA2+SKEW(7,ISK)*WA3
        AM(2,M)=SKEW(2,ISK)*WA1+SKEW(5,ISK)*WA2+SKEW(8,ISK)*WA3
        AM(3,M)=SKEW(3,ISK)*WA1+SKEW(6,ISK)*WA2+SKEW(9,ISK)*WA3
       ENDIF
C
C      CALCUL D'ONE PSEUDO MOMENT:
C      EN FAIT L'ACCELERATION DE ROTATION * INERTIE DU NOEUD MAIN (IMIN DU RB)
       AM(1,M)=IN(M)*AM(1,M)
       AM(2,M)=IN(M)*AM(2,M)
       AM(3,M)=IN(M)*AM(3,M)
C
      ELSEIF (IMCONV==1) THEN
       IF(N2D==0) THEN
         WA1=AM(1,M)
         WA2=AM(2,M)
         WA3=AM(3,M)
C repere globale -> repere d'inertie principale
         AM(1,M)=RBY(1)*WA1+RBY(2)*WA2+RBY(3)*WA3
         AM(2,M)=RBY(4)*WA1+RBY(5)*WA2+RBY(6)*WA3
         AM(3,M)=RBY(7)*WA1+RBY(8)*WA2+RBY(9)*WA3
C les contributions des vr ne sont ajoutees que sur le processeur main
         VI(1)=RBY(1)*VR(1,M)+RBY(2)*VR(2,M)+RBY(3)*VR(3,M)
         VI(2)=RBY(4)*VR(1,M)+RBY(5)*VR(2,M)+RBY(6)*VR(3,M)
         VI(3)=RBY(7)*VR(1,M)+RBY(8)*VR(2,M)+RBY(9)*VR(3,M)
         CALL ROTBMR(VI,RBY,DT1)
         IF (IMPL_S==0) THEN
           AM(1,M) = AM(1,M) + (RBY(11)-RBY(12))*VI(2)*VI(3)
           AM(2,M) = AM(2,M) + (RBY(12)-RBY(10))*VI(3)*VI(1)
           AM(3,M) = AM(3,M) + (RBY(10)-RBY(11))*VI(1)*VI(2)
         END IF
C
C CALCUL D'ONE PSEUDO MOMENT:
C EN FAIT L'ACCELERATION DE ROTATION * INERTIE DU NOEUD MAIN (IMIN DU RB)
C
         IF (IMPL_S==0) THEN
           WA1 = AM(1,M)*IN(M)/RBY(10)
           WA2 = AM(2,M)*IN(M)/RBY(11)
           WA3 = AM(3,M)*IN(M)/RBY(12)
         ELSE
           WA1 = AM(1,M)
           WA2 = AM(2,M)
           WA3 = AM(3,M)
         END IF
C repere d'inertie principale -> repere globale
         AM(1,M)=RBY(1)*WA1+RBY(4)*WA2+RBY(7)*WA3
         AM(2,M)=RBY(2)*WA1+RBY(5)*WA2+RBY(8)*WA3
         AM(3,M)=RBY(3)*WA1+RBY(6)*WA2+RBY(9)*WA3
C MATRICE d'inertie -> repere globale
         II1=RBY(10)*RBY(1)
         II2=RBY(10)*RBY(2)
         II3=RBY(10)*RBY(3)
         II4=RBY(11)*RBY(4)
         II5=RBY(11)*RBY(5)
         II6=RBY(11)*RBY(6)
         II7=RBY(12)*RBY(7)
         II8=RBY(12)*RBY(8)
         II9=RBY(12)*RBY(9)
C
         RBY(17)=RBY(1)*II1 + RBY(4)*II4 + RBY(7)*II7
         RBY(18)=RBY(1)*II2 + RBY(4)*II5 + RBY(7)*II8
         RBY(19)=RBY(1)*II3 + RBY(4)*II6 + RBY(7)*II9
         RBY(20)=RBY(2)*II1 + RBY(5)*II4 + RBY(8)*II7
         RBY(21)=RBY(2)*II2 + RBY(5)*II5 + RBY(8)*II8
         RBY(22)=RBY(2)*II3 + RBY(5)*II6 + RBY(8)*II9
         RBY(23)=RBY(3)*II1 + RBY(6)*II4 + RBY(9)*II7
         RBY(24)=RBY(3)*II2 + RBY(6)*II5 + RBY(9)*II8
         RBY(25)=RBY(3)*II3 + RBY(6)*II6 + RBY(9)*II9
C
        ELSEIF(N2D==1) THEN
C
C CALCUL D'ONE PSEUDO MOMENT:
C EN FAIT L'ACCELERATION DE ROTATION * INERTIE DU NOEUD MAIN (IMIN DU RB)
C
         IF (IMPL_S==0) THEN
           WA1 = ZERO
           WA2 = ZERO
           WA3 = AM(3,M)*IN(M)/RBY(12)
         ELSE
           WA1 = ZERO
           WA2 = ZERO
           WA3 = AM(3,M)
         END IF
C MATRICE d'inertie -> repere globale
C
          RBY(17)=RBY(10)
          RBY(21)=RBY(11)
          RBY(25)=RBY(12)
C
        ELSEIF(N2D==2) THEN
         WA1=AM(1,M)
         WA2=ZERO
         WA3=ZERO
C repere globale -> repere d'inertie principale
         AM(1,M)=RBY(1)*WA1
         AM(2,M)=ZERO
         AM(3,M)=ZERO
C les contributions des vr ne sont ajoutees que sur le processeur main
          VI(1)=RBY(1)*VR(1,M)
          VI(2)=ZERO
          VI(3)=ZERO
         CALL ROTBMR(VI,RBY,DT1)
C
C CALCUL D'ONE PSEUDO MOMENT:
C EN FAIT L'ACCELERATION DE ROTATION * INERTIE DU NOEUD MAIN (IMIN DU RB)
C
         IF (IMPL_S==0) THEN
           WA1 = AM(1,M)*IN(M)/RBY(10)
           WA2 = ZERO
           WA3 = ZERO
         ELSE
           WA1 = AM(1,M)
           WA2 = AM(2,M)
           WA3 = AM(3,M)
         END IF
C repere d'inertie principale -> repere globale
         AM(1,M)=RBY(1)*WA1
         AM(2,M)=RBY(2)*WA1
         AM(3,M)=RBY(3)*WA1
C MATRICE d'inertie -> repere globale
          II1=RBY(10)*RBY(1)
          II5=RBY(11)*RBY(5)
          II6=RBY(11)*RBY(6)
          II8=RBY(12)*RBY(8)
          II9=RBY(12)*RBY(9)
C
          RBY(17)=RBY(1)*II1 
          RBY(21)=RBY(5)*II5 + RBY(8)*II8
          RBY(22)=RBY(5)*II6 + RBY(8)*II9
          RBY(23)=ZERO
          RBY(24)=RBY(6)*II5 + RBY(9)*II8
          RBY(25)=RBY(6)*II6 + RBY(9)*II9
C
        ENDIF
      ENDIF

       DO I=1,NSN
        N = NOD(I)
C
C       AF, AM does not need to be reset and it will still be used for computing reaction forces...
C       AF(1,N)= ZERO
C       AF(2,N)= ZERO
C       AF(3,N)= ZERO
C
C       AM(1,N)= ZERO
C       AM(2,N)= ZERO
C       AM(3,N)= ZERO
C
        STIFR(N)= EM20
        STIFN(N)= EM20
       ENDDO
      ENDIF
C
      RETURN
      END

C--------------------------------------------------------------------------------
