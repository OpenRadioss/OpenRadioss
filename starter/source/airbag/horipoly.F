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
Chd|  HORIEDGE                      source/airbag/horipoly.F      
Chd|-- called by -----------
Chd|        FVMESH1                       source/airbag/fvmesh.F        
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE HORIEDGE(IPOLY  , RPOLY , NX    , NY  , NZ  ,
     .                    NBNEDGE, INEDGE, RNEDGE, X0  , Y0  ,
     .                    Z0     , INZ   , NNS3  , NREF, AREF,
     .                    NNSP   ) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPOLY(*), NBNEDGE, INEDGE(6,*), INZ, NNS3, NREF(2,*), NNSP
      my_real
     .        RPOLY(*), NX, NY, NZ, RNEDGE(6,*), X0, Y0, Z0, AREF(4,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NN, I, II
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, ZL1, ZL2, TOLE, DD 
C
*      TOLE=DLAMCH('Epsmach')
      TOLE=EM10   
      NN=IPOLY(2)
      NNSP=0
      DO I=1,NN
         II=I+1
         IF (I.EQ.NN) II=1
         X1=RPOLY(4+3*(I-1)+1)
         Y1=RPOLY(4+3*(I-1)+2)
         Z1=RPOLY(4+3*(I-1)+3)
         X2=RPOLY(4+3*(II-1)+1)
         Y2=RPOLY(4+3*(II-1)+2)
         Z2=RPOLY(4+3*(II-1)+3)
         DD=(X1-X2)**2+(Y1-Y2)**2+(Z1-Z2)**2
         ZL1=(X1-X0)*NX+(Y1-Y0)*NY+(Z1-Z0)*NZ
         ZL2=(X2-X0)*NX+(Y2-Y0)*NY+(Z2-Z0)*NZ
         IF (ZL1.EQ.ZERO.AND.ZL2.EQ.ZERO.AND.DD.GE.TOLE) THEN
            NBNEDGE=NBNEDGE+1
C
            NNSP=NNSP+1
            NREF(1,NNSP)=IPOLY(6+I)
            NREF(2,NNSP)=IPOLY(6+II)
            AREF(1,NNSP)=ONE
            AREF(2,NNSP)=X1
            AREF(3,NNSP)=Y1
            AREF(4,NNSP)=Z1
            NNSP=NNSP+1
            NREF(1,NNSP)=IPOLY(6+I)
            NREF(2,NNSP)=IPOLY(6+II)
            AREF(1,NNSP)=ZERO
            AREF(2,NNSP)=X2
            AREF(3,NNSP)=Y2
            AREF(4,NNSP)=Z2
C
            INEDGE(1,NBNEDGE)=IPOLY(1)
            INEDGE(2,NBNEDGE)=NNS3+NNSP-1
            INEDGE(3,NBNEDGE)=NNS3+NNSP
            INEDGE(4,NBNEDGE)=IPOLY(3)
            INEDGE(5,NBNEDGE)=IPOLY(4)
            INEDGE(6,NBNEDGE)=INZ
C
            RNEDGE(1,NBNEDGE)=RPOLY(4+3*(I-1)+1)    
            RNEDGE(2,NBNEDGE)=RPOLY(4+3*(I-1)+2)      
            RNEDGE(3,NBNEDGE)=RPOLY(4+3*(I-1)+3)     
            RNEDGE(4,NBNEDGE)=RPOLY(4+3*(II-1)+1)      
            RNEDGE(5,NBNEDGE)=RPOLY(4+3*(II-1)+2)      
            RNEDGE(6,NBNEDGE)=RPOLY(4+3*(II-1)+3)
         ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  HORIPOLY                      source/airbag/horipoly.F      
Chd|-- called by -----------
Chd|        FVMESH1                       source/airbag/fvmesh.F        
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE HORIPOLY(INEDGE, RNEDGE, LEDGE , NEDGE, IPOLY,
     .                    RPOLY , IZ    , IELNOD, NPOLY, NX   ,
     .                    NY    , NZ    , INZ   , IBRIC, NEL  ,
     .                    TAGELA )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER INEDGE(6,*), LEDGE(*), NEDGE, IPOLY(6+2*NEDGE+1+NEDGE,*),
     .        IZ(3,*), IELNOD(NEDGE,*), NPOLY, INZ, IBRIC, NEL,
     .        TAGELA(*)
      my_real
     .        RNEDGE(6,*), RPOLY(4+6*NEDGE+3*NEDGE,*), NX, NY, NZ
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NN, I, II, TNOD(2*NEDGE), TSEG(3,NEDGE), NN_OLD, JFOUND,
     .        J, JJ, REDIR1(2*NEDGE), REDIR2(2*NEDGE), ITAG(2*NEDGE),
     .        ITAGSEG(NEDGE+1), ISTOP, ICLOSE, N1, N2, IN, NNP, 
     .        POLY(2*NEDGE,NEDGE), ISEG, LENPOLY(NEDGE), IFOUND,
     .        IADHOL(NEDGE), NHOL, K, KK, IPOUT, M, MM, NSEC, KSMIN,
     .        NEDGE_OLD, TSEG_OLD(3,NEDGE), REDIR(NEDGE), JSEG, 
     .        JTAGSEG(NEDGE), JTAG(2*NEDGE) 
      my_real
     .        TOLE, XNOD(3,2*NEDGE), XX1, YY1, ZZ1, XX2, YY2, ZZ2,
     .        DD, XLOC(2,NEDGE), XSEC(NEDGE), PHOL(3,NEDGE), ALPHA,
     .        X1, Y1, Z1, VX1, VY1, VZ1, VX2, VY2, VZ2, NR1, NR2,
     .        SS, VVX, VVY, VVZ, SS1, X2, Y2, Z2, YLMIN, YLMAX, XSMIN1,
     .        XSMIN2, XX, YY, ZZ, YLSEC, XS, YS, LMAX, LL
C
      my_real
     .        DLAMCH
      EXTERNAL DLAMCH
C
*      TOLE=DLAMCH('Epsmach')
      TOLE=EM10   
C Creation de la liste des noeuds
      NN=0
      LMAX=ZERO
      DO I=1,NEDGE
         II=LEDGE(I)
         NN=NN+1
         TNOD(NN)=INEDGE(2,II)
         XNOD(1,NN)=RNEDGE(1,II)
         XNOD(2,NN)=RNEDGE(2,II)
         XNOD(3,NN)=RNEDGE(3,II)
         TSEG(1,I)=NN
         NN=NN+1
         TNOD(NN)=INEDGE(3,II)  
         XNOD(1,NN)=RNEDGE(4,II)
         XNOD(2,NN)=RNEDGE(5,II)
         XNOD(3,NN)=RNEDGE(6,II)
         TSEG(2,I)=NN
         TSEG(3,I)=INEDGE(1,II)
         JJ=INEDGE(4,II)
         IF(INEDGE(1,II)==1 .AND. TAGELA(JJ) > NEL) THEN
            TSEG(3,I)=3
         ENDIF
         LL=(RNEDGE(1,II)-RNEDGE(4,II))**2+
     .      (RNEDGE(2,II)-RNEDGE(5,II))**2+
     .      (RNEDGE(3,II)-RNEDGE(6,II))**2
         LMAX=MAX(LMAX,LL)
      ENDDO
*      TOLE=TOLE*LMAX
C Elimination des noeuds doubles
      DO I=1,2*NEDGE
         REDIR1(I)=0
         REDIR2(I)=0
      ENDDO
      NN_OLD=NN
      NN=0
      DO I=1,NN_OLD
         XX1=XNOD(1,I)
         YY1=XNOD(2,I)
         ZZ1=XNOD(3,I)
         JFOUND=0
         DO J=1,NN
            JJ=REDIR1(J)
            XX2=XNOD(1,JJ)
            YY2=XNOD(2,JJ)
            ZZ2=XNOD(3,JJ)
            DD=SQRT((XX1-XX2)**2+(YY1-YY2)**2+(ZZ1-ZZ2)**2)
            IF (DD.LE.TOLE) JFOUND=J
         ENDDO
         IF (JFOUND.EQ.0) THEN
            NN=NN+1
            REDIR1(NN)=I
            REDIR2(I)=NN
         ELSE
            REDIR2(I)=JFOUND
         ENDIF
      ENDDO
C
      DO I=1,NEDGE
         N1=TSEG(1,I)
         N2=TSEG(2,I)
         TSEG(1,I)=REDIR2(N1)
         TSEG(2,I)=REDIR2(N2)
      ENDDO
C Elimination des segments de longueur nulle
      NEDGE_OLD=NEDGE
      NEDGE=0
      DO I=1,NEDGE_OLD
         TSEG_OLD(1,I)=TSEG(1,I)
         TSEG_OLD(2,I)=TSEG(2,I)
         TSEG_OLD(3,I)=TSEG(3,I)
      ENDDO
      DO I=1,NEDGE_OLD
         IF (TSEG_OLD(1,I).NE.TSEG_OLD(2,I)) THEN
            NEDGE=NEDGE+1
            TSEG(1,NEDGE)=TSEG_OLD(1,I)
            TSEG(2,NEDGE)=TSEG_OLD(2,I)
            TSEG(3,NEDGE)=TSEG_OLD(3,I)
         ENDIF
      ENDDO
C Mise des segments internes en tete
      J=0
      DO I=1,NEDGE
         IF(TSEG(3,I) /= 3) CYCLE
         J=J+1
         REDIR(J)=I
      ENDDO
      DO I=1,NEDGE
         IF(TSEG(3,I) == 3) CYCLE
         J=J+1
         REDIR(J)=I
      ENDDO
C Construction des polygones
      DO I=1,NN
         ITAG(I)=0
         JTAG(I)=0
      ENDDO
      DO I=1,NEDGE
         ITAGSEG(I)=0
         JTAGSEG(I)=1
         J=REDIR(I)
         IF(TSEG(3,J)==3) JTAGSEG(I)=2
         N1=TSEG(1,J)
         N2=TSEG(2,J)
         JTAG(N1)=JTAG(N1)+JTAGSEG(I)
         JTAG(N2)=JTAG(N2)+JTAGSEG(I)
      ENDDO
      ITAGSEG(NEDGE+1)=0
      NPOLY=1
      ISTOP=0
      DO WHILE (ISTOP.EQ.0)
         I=1
         DO WHILE (ITAGSEG(I).EQ.1.AND.I.LE.NEDGE)
            I=I+1
         ENDDO
         IF (I.EQ.NEDGE+1) THEN
            ISTOP=1
            CYCLE
         ENDIF
C
         ICLOSE=0
         ISEG=I
         JSEG=REDIR(I)
         ITAGSEG(ISEG)=1
         N1=TSEG(1,JSEG)
         N2=TSEG(2,JSEG)
         ITAG(N1)=1
         ITAG(N2)=1
         IN=N2
         NNP=1
         POLY(1,NPOLY)=REDIR1(N1)
         DO WHILE (ICLOSE.EQ.0)
            IFOUND=0
            I=0
            DO WHILE (IFOUND.EQ.0)
               I=I+1
               IF (ITAGSEG(I) == 1) CYCLE
               J=REDIR(I)
               N1=TSEG(1,J)
               N2=TSEG(2,J)
               IF (N1.EQ.IN) THEN
                  IFOUND=1
                  IF (ITAG(N2) == 1) ICLOSE=1
                  ISEG=I
                  IN=N2
                  NNP=NNP+1
                  POLY(NNP,NPOLY)=REDIR1(N1)
                  ITAG(N2)=1
               ELSEIF (N2.EQ.IN) THEN
                  IFOUND=1
                  IF (ITAG(N1) == 1) ICLOSE=1
                  ISEG=I
                  IN=N1
                  NNP=NNP+1
                  POLY(NNP,NPOLY)=REDIR1(N2)
                  ITAG(N1)=1
               ENDIF
            ENDDO
            ITAGSEG(ISEG)=1
         ENDDO
C
         IF (ICLOSE.EQ.1) THEN
            LENPOLY(NPOLY)=NNP
            NPOLY=NPOLY+1
            DO I=1,NEDGE
               JTAGSEG(I)=JTAGSEG(I)-ITAGSEG(I)
            ENDDO
            DO I=1,NEDGE
               ITAGSEG(I)=0
               IF(JTAGSEG(I) <= 0) ITAGSEG(I)=1
            ENDDO
            DO I=1,NN
               JTAG(I)=JTAG(I)-2*ITAG(I)
            ENDDO
            DO I=1,NN
               ITAG(I)=0
               IF(JTAG(I) <= 0) ITAG(I)=1
            ENDDO
         ENDIF
      ENDDO
      NPOLY=NPOLY-1
C Creation des tableaux de sortie
      DO I=1,NPOLY
         IPOLY(1,I)=2
         IPOLY(2,I)=LENPOLY(I)
         IPOLY(3,I)=IBRIC
         IPOLY(4,I)=IBRIC
         IPOLY(5,I)=0
         IPOLY(6,I)=0
         RPOLY(1,I)=ZERO
         RPOLY(2,I)=NX
         RPOLY(3,I)=NY
         RPOLY(4,I)=NZ
         DO J=1,LENPOLY(I)
            JJ=POLY(J,I)
            IPOLY(6+J,I)=-TNOD(JJ)
            RPOLY(4+3*(J-1)+1,I)=XNOD(1,JJ)
            RPOLY(4+3*(J-1)+2,I)=XNOD(2,JJ)
            RPOLY(4+3*(J-1)+3,I)=XNOD(3,JJ)
            IELNOD(J,I)=-1
         ENDDO
         IPOLY(6+LENPOLY(I)+1,I)=0
C
         IZ(1,I)=2
         IZ(2,I)=INZ
         IZ(3,I)=INZ+1
      ENDDO
C---------------------------------------
C Recherche des trous dans les polygones
C---------------------------------------
      DO I=1,NPOLY
C
         NHOL=0
         DO J=1,NPOLY
            IADHOL(J)=0
         ENDDO
C
         DO J=1,NPOLY
            IF (I.EQ.J) CYCLE
            ALPHA=ZERO
            X1=RPOLY(5,J)
            Y1=RPOLY(6,J)
            Z1=RPOLY(7,J)
            DO K=1,LENPOLY(I)
               KK=K+1
               IF (K.EQ.LENPOLY(I)) KK=1
               XX1=RPOLY(4+3*(K-1)+1,I)
               YY1=RPOLY(4+3*(K-1)+2,I)
               ZZ1=RPOLY(4+3*(K-1)+3,I)
               XX2=RPOLY(4+3*(KK-1)+1,I)
               YY2=RPOLY(4+3*(KK-1)+2,I)
               ZZ2=RPOLY(4+3*(KK-1)+3,I)
               VX1=XX1-X1
               VY1=YY1-Y1
               VZ1=ZZ1-Z1
               VX2=XX2-X1
               VY2=YY2-Y1
               VZ2=ZZ2-Z1
               NR1=SQRT(VX1**2+VY1**2+VZ1**2)
               NR2=SQRT(VX2**2+VY2**2+VZ2**2)
               IF(NR1 > ZERO) THEN
                 VX1=VX1/NR1
                 VY1=VY1/NR1
                 VZ1=VZ1/NR1
               ELSE
                 CYCLE
               ENDIF
               IF(NR2 > ZERO) THEN
                 VX2=VX2/NR2
                 VY2=VY2/NR2
                 VZ2=VZ2/NR2
               ELSE
                 CYCLE
               ENDIF
               SS=VX1*VX2+VY1*VY2+VZ1*VZ2
               VVX=VY1*VZ2-VZ1*VY2
               VVY=VZ1*VX2-VX1*VZ2
               VVZ=VX1*VY2-VY1*VX2
               SS1=NX*VVX+NY*VVY+NZ*VVZ
               IF(SS < -ONE) SS=-ONE
               IF(SS >  ONE) SS= ONE
               IF (SS1.GE.ZERO) THEN
                  ALPHA=ALPHA+ACOS(SS)
               ELSE
                  ALPHA=ALPHA-ACOS(SS)
               ENDIF
            ENDDO
C
            IF (ABS(ALPHA).GE.TWO*PI) THEN
C---------------------------------------------------------------
C Le premier point du polygone j est a l'interieur du polygone i
C On teste tous les autres
C---------------------------------------------------------------
               IPOUT=0
               DO K=2,LENPOLY(J)
                  X2=RPOLY(4+3*(K-1)+1,J)
                  Y2=RPOLY(4+3*(K-1)+2,J)
                  Z2=RPOLY(4+3*(K-1)+3,J)
                  ALPHA=ZERO
                  DO M=1,LENPOLY(I)
                     MM=M+1
                     IF (M.EQ.LENPOLY(I)) MM=1
                     XX1=RPOLY(4+3*(M-1)+1,I)
                     YY1=RPOLY(4+3*(M-1)+2,I)
                     ZZ1=RPOLY(4+3*(M-1)+3,I)
                     XX2=RPOLY(4+3*(MM-1)+1,I)
                     YY2=RPOLY(4+3*(MM-1)+2,I)
                     ZZ2=RPOLY(4+3*(MM-1)+3,I)
                     VX1=XX1-X1
                     VY1=YY1-Y1
                     VZ1=ZZ1-Z1
                     VX2=XX2-X1
                     VY2=YY2-Y1
                     VZ2=ZZ2-Z1
                     NR1=SQRT(VX1**2+VY1**2+VZ1**2)
                     NR2=SQRT(VX2**2+VY2**2+VZ2**2)
                     IF(NR1 > ZERO) THEN
                       VX1=VX1/NR1
                       VY1=VY1/NR1
                       VZ1=VZ1/NR1
                     ELSE
                       CYCLE
                     ENDIF
                     IF(NR2 > ZERO) THEN
                       VX2=VX2/NR2
                       VY2=VY2/NR2
                       VZ2=VZ2/NR2
                     ELSE
                       CYCLE
                     ENDIF
                     SS=VX1*VX2+VY1*VY2+VZ1*VZ2
                     VVX=VY1*VZ2-VZ1*VY2
                     VVY=VZ1*VX2-VX1*VZ2
                     VVZ=VX1*VY2-VY1*VX2
                     SS1=NX*VVX+NY*VVY+NZ*VVZ
                     IF(SS < -ONE) SS=-ONE
                     IF(SS >  ONE) SS= ONE
                     IF (SS1.GE.ZERO) THEN
                        ALPHA=ALPHA+ACOS(SS)
                     ELSE
                        ALPHA=ALPHA-ACOS(SS)
                     ENDIF
                  ENDDO
                  IF (ABS(ALPHA).LT.TWO*PI) IPOUT=1
               ENDDO
C
               IF (IPOUT.EQ.1) CYCLE
C---------------------------------------------
C Le polygone j est un trou dans le polygone i
C---------------------------------------------
               IPOLY(1,J)=-1
               NHOL=NHOL+1
               IADHOL(NHOL)=LENPOLY(I)
               DO K=1,LENPOLY(J)
                  IPOLY(6+IADHOL(NHOL)+K,I)=IPOLY(6+K,J)
                  IELNOD(IADHOL(NHOL)+K,I)=IELNOD(K,J)
                  RPOLY(4+3*IADHOL(NHOL)+3*(K-1)+1,I)=
     .                                   RPOLY(4+3*(K-1)+1,J)
                  RPOLY(4+3*IADHOL(NHOL)+3*(K-1)+2,I)=
     .                                   RPOLY(4+3*(K-1)+2,J)
                  RPOLY(4+3*IADHOL(NHOL)+3*(K-1)+3,I)=
     .                                   RPOLY(4+3*(K-1)+3,J)
               ENDDO
               LENPOLY(I)=LENPOLY(I)+LENPOLY(J)
C Point interieur polygone j
               VX1=RPOLY(5,J)-RPOLY(8,J)
               VY1=RPOLY(6,J)-RPOLY(9,J)
               VZ1=RPOLY(7,J)-RPOLY(10,J)
               SS=SQRT(VX1**2+VY1**2+VZ1**2)
               VX1=VX1/SS
               VY1=VY1/SS
               VZ1=VZ1/SS
               VX2=NY*VZ1-NZ*VY1
               VY2=NZ*VX1-NX*VZ1
               VZ2=NX*VY1-NY*VX1
               X1=RPOLY(5,J)
               Y1=RPOLY(6,J)
               Z1=RPOLY(7,J)
               XLOC(1,1)=ZERO
               XLOC(2,1)=ZERO
               YLMIN=EP30
               YLMAX=-EP30
               DO K=2,LENPOLY(J)
                  XX=RPOLY(4+3*(K-1)+1,J)
                  YY=RPOLY(4+3*(K-1)+2,J)
                  ZZ=RPOLY(4+3*(K-1)+3,J)
                  VVX=XX-X1
                  VVY=YY-Y1
                  VVZ=ZZ-Z1
                  XLOC(1,K)=VVX*VX1+VVY*VY1+VVZ*VZ1
                  XLOC(2,K)=VVX*VX2+VVY*VY2+VVZ*VZ2
                  IF (XLOC(2,K).LT.YLMIN) YLMIN=XLOC(2,K)
                  IF (XLOC(2,K).GT.YLMAX) YLMAX=XLOC(2,K)
               ENDDO
               YLSEC=HALF*(YLMIN+YLMAX)
C
               NSEC=0
               DO K=1,LENPOLY(J)
                  KK=K+1
                  IF (K.EQ.LENPOLY(J)) KK=1
                  X1=XLOC(1,K)
                  Y1=XLOC(2,K)
                  X2=XLOC(1,KK)
                  Y2=XLOC(2,KK)
                  IF (Y1-Y2.NE.ZERO) THEN
                     ALPHA=(YLSEC-Y2)/(Y1-Y2)
                     IF (ALPHA.GE.ZERO.AND.ALPHA.LE.ONE) THEN
                        NSEC=NSEC+1
                        XSEC(NSEC)=ALPHA*X1+(ONE-ALPHA)*X2
                     ENDIF
                  ELSE
                     IF (Y1.EQ.YLSEC) THEN
                        NSEC=NSEC+1
                        XSEC(NSEC)=X1
                        NSEC=NSEC+1
                        XSEC(NSEC)=X2
                     ENDIF
                  ENDIF
               ENDDO
C
               XSMIN1=EP30
               DO K=1,NSEC
                  IF (XSEC(K).LT.XSMIN1) THEN
                     XSMIN1=XSEC(K)
                     KSMIN=K
                  ENDIF
               ENDDO
               XSMIN2=EP30
               DO K=1,NSEC
                  IF (K.EQ.KSMIN) CYCLE
                  IF (XSEC(K).LT.XSMIN2) XSMIN2=XSEC(K)
               ENDDO
C
               XS=HALF*(XSMIN1+XSMIN2)
               YS=YLSEC
               PHOL(1,NHOL)=RPOLY(5,J)+XS*VX1+YS*VX2
               PHOL(2,NHOL)=RPOLY(6,J)+XS*VY1+YS*VY2
               PHOL(3,NHOL)=RPOLY(7,J)+XS*VZ1+YS*VZ2
            ENDIF
         ENDDO
C
         IPOLY(2,I)=LENPOLY(I)
         IPOLY(6+LENPOLY(I)+1,I)=NHOL
         DO J=1,NHOL
            IPOLY(6+LENPOLY(I)+1+J,I)=IADHOL(J)
            RPOLY(4+3*LENPOLY(I)+3*(J-1)+1,I)=PHOL(1,J)
            RPOLY(4+3*LENPOLY(I)+3*(J-1)+2,I)=PHOL(2,J)
            RPOLY(4+3*LENPOLY(I)+3*(J-1)+3,I)=PHOL(3,J)
         ENDDO
      ENDDO
C
      RETURN
      END
