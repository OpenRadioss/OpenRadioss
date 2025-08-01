Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    fvbag_gpolcut_mod   ../starter/source/airbag/gpolcut.F
!||--- called by ------------------------------------------------------
!||    fvmesh1             ../starter/source/airbag/fvmesh.F
!||====================================================================
      MODULE FVBAG_GPOLCUT_MOD
      CONTAINS
!||====================================================================
!||    fvbag_gpolcut   ../starter/source/airbag/gpolcut.F
!||--- called by ------------------------------------------------------
!||    fvmesh1         ../starter/source/airbag/fvmesh.F
!||====================================================================
      SUBROUTINE FVBAG_GPOLCUT(IPOLY , RPOLY  , IPOLY_OLD, RPOLY_OLD , INEDGE,
     .                         RNEDGE, NBNEDGE, NX       , NY        , NZ    ,
     .                         X0    , Y0     , Z0       , INS       , RNS   ,
     .                         NN    , NHOL   , INZ      , IZ        , NNS3  ,
     .                         NPOLY , NS     , IELNOD   , IELNOD_OLD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NN, NHOL, IPOLY(6+2*NN+1+NHOL,*), IPOLY_OLD(*), 
     .        INEDGE(6,*), NBNEDGE, INS(2,*), INZ, IZ(3,*), NNS3, 
     .        NPOLY, NS, IELNOD(2*NN,*), IELNOD_OLD(*)
      my_real
     .        RPOLY(4+3*2*NN+3*NHOL,*), RPOLY_OLD(*), RNEDGE(6,*), NX,
     .        NY, NZ, X0, Y0, Z0, RNS(4,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NN1, I, IADHOL(NHOL+1), NSEG, ITAG(2*NN+1), II,
     .        TSEG(3,2*NN), NHOL_OLD, ICUT, J, JJ, NSEG_INI, NSEG1,
     .        REDIR(NN), N1, N2, ISTOP, NNP, ICLOSE, ISEG,
     .        POLY(2*NN,NN), IN, IN1, IN2, LENPOLY(NN), K, KK,
     .        THOL(NHOL), JN, NHOLP, IADHOLP(NHOL), KN,
     .        ITAG2(2*NN), ITAGN(NN), ITAGS(NN), ISEG_OLD
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, ZL1, ZL2, ALPHA, NPX, NPY, NPZ,
     .        XP0, YP0, ZP0, VX, VY, VZ, XX, YY, ZZ, XL(NN), XLMIN,
     .        XLC, XX1, YY1, ZZ1, XX2, YY2, ZZ2, VX1, VY1, 
     .        VZ1, VX2, VY2, VZ2, NR1, NR2, SS, VVX, VVY, VVZ, 
     .        SS1, ZL, XNS(3,NN), TOLE, LL, ZLM
C
      TOLE=EPSILON(ZERO)*0.5
C
C Liste des segments
      IF (NHOL==0) THEN
         NN1=NN
      ELSE
         NN1=IPOLY_OLD(6+NN+1+1)-1
         DO I=1,NHOL
            IADHOL(I)=IPOLY_OLD(6+NN+1+I)
         ENDDO
         IADHOL(NHOL+1)=NN+1
      ENDIF
      NS=0
      NSEG=0
C
      DO I=1,2*NN
         ITAG(I)=0
         ITAG2(I)=0
      ENDDO
      ITAG(2*NN+1)=0
      DO I=1,NN
         ITAGN(I)=0
         ITAGS(I)=0
      ENDDO
C Segments du contour exterieur
      DO I=1,NN1
         II=I+1
         IF (I==NN1) II=1
         X1=RPOLY_OLD(4+3*(I-1)+1)
         Y1=RPOLY_OLD(4+3*(I-1)+2)
         Z1=RPOLY_OLD(4+3*(I-1)+3)
         X2=RPOLY_OLD(4+3*(II-1)+1)
         Y2=RPOLY_OLD(4+3*(II-1)+2)
         Z2=RPOLY_OLD(4+3*(II-1)+3)
         ZL1=(X1-X0)*NX+(Y1-Y0)*NY+(Z1-Z0)*NZ
         ZL2=(X2-X0)*NX+(Y2-Y0)*NY+(Z2-Z0)*NZ
         IF (ZL1-ZL2/=ZERO) THEN
            ALPHA=ZL2/(ZL2-ZL1)
            IF (ALPHA>ZERO.AND.ALPHA<ONE) THEN
               NS=NS+1
               INS(1,NS)=IPOLY_OLD(6+I)
               INS(2,NS)=IPOLY_OLD(6+II)
               RNS(1,NS)=ALPHA
               XNS(1,NS)=ALPHA*X1+(ONE-ALPHA)*X2
               XNS(2,NS)=ALPHA*Y1+(ONE-ALPHA)*Y2
               XNS(3,NS)=ALPHA*Z1+(ONE-ALPHA)*Z2
               NSEG=NSEG+1
               TSEG(1,NSEG)=I
               TSEG(2,NSEG)=-NS
               TSEG(3,NSEG)=NSEG+1
               NSEG=NSEG+1
               TSEG(1,NSEG)=-NS
               TSEG(2,NSEG)=II
               TSEG(3,NSEG)=NSEG+1
            ELSEIF (ALPHA==ZERO) THEN
               IF (ITAGN(II)==0) THEN
                  NS=NS+1
                  ITAGN(II)=NS
                  INS(1,NS)=IPOLY_OLD(6+I)
                  INS(2,NS)=IPOLY_OLD(6+II)
                  RNS(1,NS)=ZERO
                  XNS(1,NS)=X2
                  XNS(2,NS)=Y2
                  XNS(3,NS)=Z2
                  ITAGS(NS)=1
                  NSEG=NSEG+1
                  TSEG(1,NSEG)=I
                  TSEG(2,NSEG)=-NS
                  TSEG(3,NSEG)=NSEG+1
                  NSEG=NSEG+1
                  TSEG(1,NSEG)=-NS
                  TSEG(2,NSEG)=II
                  TSEG(3,NSEG)=NSEG+1
               ELSE
                  NSEG=NSEG+1
                  TSEG(1,NSEG)=I
                  TSEG(2,NSEG)=II
                  TSEG(3,NSEG)=NSEG+1
               ENDIF
            ELSEIF (ALPHA==ONE) THEN
               IF (ITAGN(I)==0) THEN
                  NS=NS+1
                  ITAGN(I)=NS
                  INS(1,NS)=IPOLY_OLD(6+I)
                  INS(2,NS)=IPOLY_OLD(6+II)
                  RNS(1,NS)=ONE
                  XNS(1,NS)=X1
                  XNS(2,NS)=Y1
                  XNS(3,NS)=Z1
                  ITAGS(NS)=1
                  NSEG=NSEG+1
                  TSEG(1,NSEG)=I
                  TSEG(2,NSEG)=-NS
                  TSEG(3,NSEG)=NSEG+1
                  NSEG=NSEG+1
                  TSEG(1,NSEG)=-NS
                  TSEG(2,NSEG)=II
                  TSEG(3,NSEG)=NSEG+1
               ELSE
                  NSEG=NSEG+1
                  TSEG(1,NSEG)=I
                  TSEG(2,NSEG)=II
                  TSEG(3,NSEG)=NSEG+1
               ENDIF
            ELSE
               NSEG=NSEG+1
               TSEG(1,NSEG)=I
               TSEG(2,NSEG)=II
               TSEG(3,NSEG)=NSEG+1
            ENDIF
         ELSE
            NSEG=NSEG+1
            TSEG(1,NSEG)=I
            TSEG(2,NSEG)=II
            TSEG(3,NSEG)=NSEG+1
         ENDIF
      ENDDO
      TSEG(3,NSEG)=1
C Segments des trous
      NHOL_OLD=NHOL
      NHOL=0
      DO I=1,NHOL_OLD
         II = I+1
         ICUT=0
         NSEG_INI=NSEG
         DO J=IADHOL(I),IADHOL(I+1)-1
            JJ=J+1
            IF (J==(IADHOL(I+1)-1)) JJ=IADHOL(I)
            X1=RPOLY_OLD(4+3*(J-1)+1)
            Y1=RPOLY_OLD(4+3*(J-1)+2)
            Z1=RPOLY_OLD(4+3*(J-1)+3)
            X2=RPOLY_OLD(4+3*(JJ-1)+1)
            Y2=RPOLY_OLD(4+3*(JJ-1)+2)
            Z2=RPOLY_OLD(4+3*(JJ-1)+3)
            ZL1=(X1-X0)*NX+(Y1-Y0)*NY+(Z1-Z0)*NZ
            ZL2=(X2-X0)*NX+(Y2-Y0)*NY+(Z2-Z0)*NZ
            IF (ZL1-ZL2/=ZERO) THEN
               ALPHA=ZL2/(ZL2-ZL1)
               IF (ALPHA>ZERO.AND.ALPHA<ONE) THEN
                  ICUT=1
                  NS=NS+1
                  INS(1,NS)=IPOLY_OLD(6+J)
                  INS(2,NS)=IPOLY_OLD(6+JJ)
                  RNS(1,NS)=ALPHA
                  XNS(1,NS)=ALPHA*X1+(ONE-ALPHA)*X2
                  XNS(2,NS)=ALPHA*Y1+(ONE-ALPHA)*Y2
                  XNS(3,NS)=ALPHA*Z1+(ONE-ALPHA)*Z2
                  NSEG=NSEG+1
                  TSEG(1,NSEG)=J
                  TSEG(2,NSEG)=-NS
                  TSEG(3,NSEG)=NSEG+1
                  NSEG=NSEG+1
                  TSEG(1,NSEG)=-NS
                  TSEG(2,NSEG)=JJ
                  TSEG(3,NSEG)=NSEG+1
               ELSEIF (ALPHA==ZERO) THEN
                  ICUT=1
                  IF (ITAGN(II)==0) THEN
                     NS=NS+1
                     ITAGN(JJ)=NS
                     INS(1,NS)=IPOLY_OLD(6+J)
                     INS(2,NS)=IPOLY_OLD(6+JJ)
                     RNS(1,NS)=ZERO
                     XNS(1,NS)=X2
                     XNS(2,NS)=Y2
                     XNS(3,NS)=Z2
                     ITAGS(NS)=1
                     NSEG=NSEG+1
                     TSEG(1,NSEG)=J
                     TSEG(2,NSEG)=-NS
                     TSEG(3,NSEG)=NSEG+1
                     NSEG=NSEG+1
                     TSEG(1,NSEG)=-NS
                     TSEG(2,NSEG)=JJ
                     TSEG(3,NSEG)=NSEG+1
                  ELSE
                     NSEG=NSEG+1
                     TSEG(1,NSEG)=J
                     TSEG(2,NSEG)=JJ
                     TSEG(3,NSEG)=NSEG+1
                  ENDIF
               ELSEIF (ALPHA==ONE) THEN
                  ICUT=1
                  IF (ITAGN(I)==0) THEN
                     NS=NS+1
                     ITAGN(J)=NS
                     INS(1,NS)=IPOLY_OLD(6+J)
                     INS(2,NS)=IPOLY_OLD(6+JJ)
                     RNS(1,NS)=ONE
                     XNS(1,NS)=X1
                     XNS(2,NS)=Y1
                     XNS(3,NS)=Z1
                     ITAGS(NS)=1
                     NSEG=NSEG+1
                     TSEG(1,NSEG)=J
                     TSEG(2,NSEG)=-NS
                     TSEG(3,NSEG)=NSEG+1
                     NSEG=NSEG+1
                     TSEG(1,NSEG)=-NS
                     TSEG(2,NSEG)=JJ
                     TSEG(3,NSEG)=NSEG+1
                  ELSE
                     NSEG=NSEG+1
                     TSEG(1,NSEG)=J
                     TSEG(2,NSEG)=JJ
                     TSEG(3,NSEG)=NSEG+1
                  ENDIF
               ELSE
                  NSEG=NSEG+1
                  TSEG(1,NSEG)=J
                  TSEG(2,NSEG)=JJ
                  TSEG(3,NSEG)=NSEG+1
               ENDIF
            ELSE
               NSEG=NSEG+1
               TSEG(1,NSEG)=J
               TSEG(2,NSEG)=JJ
               TSEG(3,NSEG)=NSEG+1
            ENDIF
         ENDDO
         TSEG(3,NSEG)=NSEG_INI+1
C
         IF (ICUT==0) THEN
C Le trou n'est pas coupe
            NHOL=NHOL+1
            DO J=NSEG_INI+1,NSEG
               ITAG(J)=-NHOL
            ENDDO
         ENDIF
      ENDDO
C
      IF(NS <= 1) THEN
        NPOLY=0
        RETURN
      ENDIF
      NSEG1=NSEG
C Creation des nouvelles aretes dans le plan horizontal
      NPX=RPOLY_OLD(2)
      NPY=RPOLY_OLD(3)
      NPZ=RPOLY_OLD(4)
      XP0=RPOLY_OLD(5)
      YP0=RPOLY_OLD(6)
      ZP0=RPOLY_OLD(7)
      VX=NY*NPZ-NZ*NPY
      VY=NZ*NPX-NX*NPZ
      VZ=NX*NPY-NY*NPX
      DO I=1,NS
         XX=XNS(1,I)
         YY=XNS(2,I)
         ZZ=XNS(3,I)
         XL(I)=(XX-XP0)*VX+(YY-YP0)*VY+(ZZ-ZP0)*VZ
      ENDDO
      XLMIN=EP30
      DO I=1,NS
         REDIR(I)=I
      ENDDO
      DO I=1,NS
         XLMIN=XL(REDIR(I))
         DO J=I+1,NS
            XLC=XL(REDIR(J))
            IF (XLC<XLMIN) THEN
               JJ=REDIR(J)
               REDIR(J)=REDIR(I)
               REDIR(I)=JJ
               XLMIN=XLC
            ENDIF
         ENDDO
      ENDDO
      II=0
      DO I=1,NS
         II=II+1
         IF (II==1) THEN
            N1=REDIR(I)
            N2=REDIR(I+1)
            X1=XNS(1,N1)
            Y1=XNS(2,N1)
            Z1=XNS(3,N1)
            X2=XNS(1,N2)
            Y2=XNS(2,N2)
            Z2=XNS(3,N2)
            LL=SQRT((X1-X2)**2+(Y1-Y2)**2+(Z1-Z2)**2)
C
            IF (LL>TOLE) THEN
               NSEG=NSEG+1
               TSEG(1,NSEG)=-N1
               TSEG(2,NSEG)=-N2
               TSEG(3,NSEG)=-1
            ENDIF
         ELSE
            II=0
         ENDIF
      ENDDO
C Nouveaux polygones
      ISTOP=0
      NPOLY=1
      NNP=0
      DO WHILE (ISTOP==0)
         I=1
         DO WHILE (ITAG(I)/=0.AND.I<=NSEG) 
            I=I+1
         ENDDO
         IF (I==NSEG+1) THEN
            ISTOP=1
            CYCLE
         ENDIF
C
         ICLOSE=0
         ISEG=I
         DO WHILE (ICLOSE==0)
            NNP=NNP+1
            POLY(NNP,NPOLY)=TSEG(1,ISEG)
            IN=TSEG(2,ISEG)
            IF (TSEG(3,ISEG)>0) THEN
               ITAG(ISEG)=1
               IF (IN<0) THEN
                  ISEG_OLD=ISEG
                  ISEG=0
                  DO J=NSEG1+1,NSEG
                     IF (ITAG(J)/=0) CYCLE
                     IN1=TSEG(1,J)
                     IN2=TSEG(2,J)
                     IF (IN1==IN) THEN
                        ISEG=J
                     ELSEIF (IN2==IN) THEN
                        ISEG=J
                        TSEG(1,J)=IN2
                        TSEG(2,J)=IN1
                     ENDIF
                  ENDDO
                  IF (ISEG==0) ISEG=TSEG(3,ISEG_OLD)
               ELSE
                  ISEG=TSEG(3,ISEG)
               ENDIF
            ELSE
               IF (ITAG2(ISEG)==1) ITAG(ISEG)=1
               ITAG2(ISEG)=1
               ISEG=0
               DO J=1,NSEG1
                  IN1=TSEG(1,J)
                  IF (IN1==IN) ISEG=J
               ENDDO
            ENDIF
C
            IF (ITAG(ISEG)/=0) THEN
               ICLOSE=1
               LENPOLY(NPOLY)=NNP
               NPOLY=NPOLY+1
               NNP=0
            ENDIF
         ENDDO
      ENDDO
      NPOLY=NPOLY-1
C Attribution des trous
      DO I=1,NHOL
         J=1
         DO WHILE (ITAG(J)/=-I)
            J=J+1
         ENDDO
         N1=TSEG(1,J)
         XX=RPOLY_OLD(4+3*(N1-1)+1)
         YY=RPOLY_OLD(4+3*(N1-1)+2)
         ZZ=RPOLY_OLD(4+3*(N1-1)+3)
         DO J=1,NPOLY
            ALPHA=ZERO
            DO K=1,LENPOLY(J)
               KK=K+1
               IF (K==LENPOLY(J)) KK=1
               N1=POLY(K,I)
               N2=POLY(KK,I)
               IF (N1>0) THEN
                  XX1=RPOLY_OLD(4+3*(N1-1)+1)      
                  YY1=RPOLY_OLD(4+3*(N1-1)+2)      
                  ZZ1=RPOLY_OLD(4+3*(N1-1)+3)
               ELSE
                  XX1=XNS(1,-N1)
                  YY1=XNS(2,-N1)
                  ZZ1=XNS(3,-N1)
               ENDIF
               IF (N2>0) THEN
                  XX2=RPOLY_OLD(4+3*(N2-1)+1)      
                  YY2=RPOLY_OLD(4+3*(N2-1)+2)      
                  ZZ2=RPOLY_OLD(4+3*(N2-1)+3)
               ELSE
                  XX2=XNS(1,-N2)
                  YY2=XNS(2,-N2)
                  ZZ2=XNS(3,-N2)
               ENDIF
               VX1=XX1-XX
               VY1=YY1-YY
               VZ1=ZZ1-ZZ
               VX2=XX2-XX
               VY2=YY2-YY
               VZ2=ZZ2-ZZ
               NR1=SQRT(VX1**2+VY1**2+VZ1**2)
               NR2=SQRT(VX2**2+VY2**2+VZ2**2)
               VX1=VX1/NR1
               VY1=VY1/NR1
               VZ1=VZ1/NR1
               VX2=VX2/NR2
               VY2=VY2/NR2
               VZ2=VZ2/NR2
               SS=VX1*VX2+VY1*VY2+VZ1*VZ2
               VVX=VY1*VZ2-VZ1*VY2
               VVY=VZ1*VX2-VX1*VZ2
               VVZ=VX1*VY2-VY1*VX2
               SS1=NPX*VVX+NPY*VVY+NPZ*VVZ
               IF (SS1>=ZERO) THEN
                  ALPHA=ALPHA+ACOS(SS)
               ELSE
                  ALPHA=ALPHA-ACOS(SS)
               ENDIF
            ENDDO
C
            IF (ABS(ALPHA)>=ONE) THOL(I)=J
         ENDDO
      ENDDO
C Creation des tableaux de sortie
      DO I=1,NS
         RNS(2,I)=XNS(1,I)
         RNS(3,I)=XNS(2,I)
         RNS(4,I)=XNS(3,I)
      ENDDO
C
      DO I=1,NPOLY
         IPOLY(1,I)=IPOLY_OLD(1)
         IPOLY(3,I)=IPOLY_OLD(3)
         IPOLY(4,I)=IPOLY_OLD(4)
         IPOLY(5,I)=IPOLY_OLD(5)
         IPOLY(6,I)=IPOLY_OLD(6)
         RPOLY(1,I)=ZERO
         RPOLY(2,I)=NPX
         RPOLY(3,I)=NPY
         RPOLY(4,I)=NPZ
         NNP=0
         DO J=1,LENPOLY(I)
            JN=POLY(J,I)
            IF (JN>0) THEN
               NNP=NNP+1
               IPOLY(6+NNP,I)=IPOLY_OLD(6+JN)
               RPOLY(4+3*(NNP-1)+1,I)=RPOLY_OLD(4+3*(JN-1)+1)
               RPOLY(4+3*(NNP-1)+2,I)=RPOLY_OLD(4+3*(JN-1)+2)
               RPOLY(4+3*(NNP-1)+3,I)=RPOLY_OLD(4+3*(JN-1)+3)
               IELNOD(NNP,I)=IELNOD_OLD(JN)
            ELSE
               IF (ITAGS(-JN)==0) THEN
                  NNP=NNP+1
                  IPOLY(6+NNP,I)=-NNS3+JN
                  RPOLY(4+3*(NNP-1)+1,I)=XNS(1,-JN)
                  RPOLY(4+3*(NNP-1)+2,I)=XNS(2,-JN)
                  RPOLY(4+3*(NNP-1)+3,I)=XNS(3,-JN)
                  IELNOD(NNP,I)=-1
               ELSE
                  JJ=POLY(J+1,I)
                  IF (J==LENPOLY(I)) JJ=POLY(1,I)
                  X1=XNS(1,-JN)
                  Y1=XNS(2,-JN)
                  Z1=XNS(3,-JN)
                  IF (JJ>0) THEN
                     X2=RPOLY_OLD(4+3*(JJ-1)+1)
                     Y2=RPOLY_OLD(4+3*(JJ-1)+2)
                     Z2=RPOLY_OLD(4+3*(JJ-1)+3)
                  ELSE
                     X2=XNS(1,-JJ)
                     Y2=XNS(2,-JJ)
                     Z2=XNS(3,-JJ)
                  ENDIF
                  LL=SQRT((X1-X2)**2+(Y1-Y2)**2+(Z1-Z2)**2)
                  IF (LL>TOLE) THEN
                     NNP=NNP+1
                     IPOLY(6+NNP,I)=-NNS3+JN
                     RPOLY(4+3*(NNP-1)+1,I)=XNS(1,-JN)
                     RPOLY(4+3*(NNP-1)+2,I)=XNS(2,-JN)
                     RPOLY(4+3*(NNP-1)+3,I)=XNS(3,-JN)
                     IELNOD(NNP,I)=-1
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
C
         NHOLP=0
         
         DO J=1,NHOL
            IF (THOL(J)==I) THEN
               NHOLP=NHOLP+1
               IADHOLP(NHOLP)=NNP+1
               DO K=1,NSEG
                  IF (TSEG(3,K)==-J) THEN
                     NNP=NNP+1
                     KN=TSEG(1,K)
                     IPOLY(6+NNP,I)=KN
                     RPOLY(4+3*(NNP-1)+1,I)=RPOLY_OLD(4+3*(KN-1)+1)
                     RPOLY(4+3*(NNP-1)+2,I)=RPOLY_OLD(4+3*(KN-1)+2)
                     RPOLY(4+3*(NNP-1)+3,I)=RPOLY_OLD(4+3*(KN-1)+3)
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
         IPOLY(2,I)=NNP
         IPOLY(6+NNP+1,I)=NHOLP
         DO J=1,NHOLP
            IPOLY(6+NNP+1+J,I)=IADHOLP(J)
         ENDDO
      ENDDO
C
      DO I=NSEG1+1,NSEG
         NBNEDGE=NBNEDGE+1
         N1=-TSEG(1,I)
         N2=-TSEG(2,I)
         INEDGE(1,NBNEDGE)=IPOLY_OLD(1)
         INEDGE(2,NBNEDGE)=NNS3+N1
         INEDGE(3,NBNEDGE)=NNS3+N2
         INEDGE(4,NBNEDGE)=IPOLY_OLD(3)
         INEDGE(5,NBNEDGE)=IPOLY_OLD(4)
         INEDGE(6,NBNEDGE)=INZ
C
         XX1=XNS(1,N1)
         YY1=XNS(2,N1)
         ZZ1=XNS(3,N1)
C
         XX2=XNS(1,N2)
         YY2=XNS(2,N2)
         ZZ2=XNS(3,N2)
C
         RNEDGE(1,NBNEDGE)=XX1         
         RNEDGE(2,NBNEDGE)=YY1         
         RNEDGE(3,NBNEDGE)=ZZ1         
         RNEDGE(4,NBNEDGE)=XX2         
         RNEDGE(5,NBNEDGE)=YY2         
         RNEDGE(6,NBNEDGE)=ZZ2
      ENDDO
C
      DO I=1,NPOLY
         ZL=ZERO
         ZLM=ZERO
         DO J=1,IPOLY(2,I)
            XX=RPOLY(4+3*(J-1)+1,I)
            YY=RPOLY(4+3*(J-1)+2,I)       
            ZZ=RPOLY(4+3*(J-1)+3,I)
            ZL=(XX-X0)*NX+(YY-Y0)*NY+(ZZ-Z0)*NZ
            IF (ABS(ZL)>ABS(ZLM)) ZLM=ZL
         ENDDO
         IZ(1,I)=1
         IF (ZLM>ZERO) THEN
            IZ(2,I)=INZ+1
         ELSEIF (ZLM<ZERO) THEN
            IZ(2,I)=INZ
         ENDIF
      ENDDO     
C 
      RETURN
      END SUBROUTINE FVBAG_GPOLCUT
      END MODULE FVBAG_GPOLCUT_MOD
