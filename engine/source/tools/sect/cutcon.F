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
Chd|  CUTCON                        source/tools/sect/cutcon.F    
Chd|-- called by -----------
Chd|        CUTMAIN                       source/tools/sect/cutmain.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CUTCON(ITYP,X0,VN,XYZ0,IXS,D,
     .                  NUMTOT,NUMEL,NC,NVOIS,NA,XYZ,AL,NODCUT)
C------------------------------------------
C GENERATION DE LA GEOMETRIE DES COUPES
C------------------------------------------
C
C NUMTOT NOMBRE DE NOEUD DE LA COUPE
C NUMEL  NOMBRE D'ELEMENTS DE LA COUPE
C NUMCON NOMBRE DE CONNECTIVITES DE LA COUPE
C NC     TABLEAU DES CONNECTIVITES
C XYZ    TABLEAU DES COORDONNEES
C NVOIS  TABLEAU DES TWO NOEUDS VOISINS 
C LA     TABLEAU DES COEFFICIENTS D'INTERPOLATION
C------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   X0(3),VN(3),XYZ0(3,*),XYZ(3,*),D(3,*),AL(*)
      INTEGER IXS(NIXS,*),NUMTOT,NUMEL,NUMCON,NC(5,*),NVOIS(2,*),NA(*),
     .        ITYP,NODCUT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IARETE(2,12),NEWL,NUMNEW,I,J,K,L,I1,I2,N1,N2,N3,KK,
     .        NODEL,NVC(2,24),NAC(24),NCA(24),NCB(24),LMIN,KB
C     REAL
      my_real
     .   UNM,DISTMIN,DISTMAX,DIST0,TOL,DIST(8),
     .   XC(24),YC(24),ZC(24),ALC(24),TET(24),DIS,XM,YM,ZM,
     .   X1,Y1,Z1,V1,XI,YI,ZI,VI,XI1,YI1,ZI1,CSI,SSI,TETMIN,
     .   X12,Y12,Z12,X23,Y23,Z23,V12,X2,Y2,Z2
      DATA IARETE/1,2,2,3,3,4,4,1,1,5,2,6,3,7,4,8,5,6,6,7,7,8,8,5/
      UNM = -ONE
C      
      NEWL=0
      NUMNEW=0
      NUMCON=0
      DISTMIN=EP30
      DIST0=VN(1)*X0(1)+VN(2)*X0(2)+VN(3)*X0(3)
C
      DO209 I=1,NUMELS
        I1=IXS(2,I)
        I2=IXS(3,I)
        X1=ABS(XYZ0(1,I1)-XYZ0(1,I2))+ABS(XYZ0(2,I1)-XYZ0(2,I2))
     &    +ABS(XYZ0(3,I1)-XYZ0(3,I2))
  209   DISTMIN=MIN(DISTMIN,ABS(X1))
      TOL=EM3*DISTMIN
C
      DO100 I=1,NUMELS
          DISTMIN= EP30
          DISTMAX=-EP30
          NODEL=0
          DO110 J=1,8
          K=IXS(J+1,I)
          XI=XYZ0(1,K)
          YI=XYZ0(2,K)
          ZI=XYZ0(3,K)
          IF(ITYP.EQ.2)THEN
            XI=XI-D(1,K)
            YI=YI-D(2,K)
            ZI=ZI-D(3,K)
          ENDIF
            DIST(J)=XI*VN(1)+YI*VN(2)+ZI*VN(3)
            DIST(J)=DIST(J)-DIST0
            DISTMIN=MIN(DIST(J),DISTMIN)
 110        DISTMAX=MAX(DIST(J),DISTMAX)
C
          IF(DISTMIN*DISTMAX.GT. ZERO)GOTO100
C-------------------------------------------------------------
C NODEL NOEUDS INTERSECTIONS AVEC LES ARETES DE CHAQUE ELEMENT
C-------------------------------------------------------------
          DO 120 K=1,12
            N1=IARETE(1,K)
            N2=IARETE(2,K)
            IF(DIST(N1)*DIST(N2).GT. ZERO) GOTO 120
            X1  = XYZ0(1,IXS(N1+1,I))
            Y1  = XYZ0(2,IXS(N1+1,I))
            Z1  = XYZ0(3,IXS(N1+1,I))
            X2  = XYZ0(1,IXS(N2+1,I))
            Y2  = XYZ0(2,IXS(N2+1,I))
            Z2  = XYZ0(3,IXS(N2+1,I))
            IF(ITYP.EQ.2)THEN
              X1  =X1  - D(1,IXS(N1+1,I))
              Y1  =Y1  - D(2,IXS(N1+1,I))
              Z1  =Z1  - D(3,IXS(N1+1,I))
              X2  =X2  - D(1,IXS(N2+1,I))
              Y2  =Y2  - D(2,IXS(N2+1,I))
              Z2  =Z2  - D(3,IXS(N2+1,I))
            ENDIF
            IF(ABS(DIST(N1)-DIST(N2)).LT.TOL)THEN
              NODEL=NODEL+1
              XC(NODEL)  = X1
              YC(NODEL)  = Y1
              ZC(NODEL)  = Z1
              NVC(1,NODEL)=IXS(N1+1,I)
              NVC(2,NODEL)=IXS(N1+1,I)
              ALC(NODEL)=1.              
              NODEL=NODEL+1
              XC(NODEL)  = X2
              YC(NODEL)  = Y2
              ZC(NODEL)  = Z2
              NVC(1,NODEL)=IXS(N2+1,I)
              NVC(2,NODEL)=IXS(N2+1,I)
              ALC(NODEL)=1.
            ELSE
              NODEL=NODEL+1
              ALC(NODEL)=DIST(N1)/(DIST(N1)-DIST(N2))
              NVC(1,NODEL)=IXS(N1+1,I)
              NVC(2,NODEL)=IXS(N2+1,I)
              XC(NODEL)  =    ALC(NODEL) *X2
     &                    +(1-ALC(NODEL))*X1
              YC(NODEL)  =    ALC(NODEL) *Y2
     &                    +(1-ALC(NODEL))*Y1
              ZC(NODEL)  =    ALC(NODEL) *Z2
     &                    +(1-ALC(NODEL))*Z1
            ENDIF
 120      CONTINUE
C
C------------------------------------------------
C ELIMINATION DES NOEUDS DOUBLES
C------------------------------------------------
          IF(NODEL.GT.2)THEN
            K=1
            NAC(1)=1
            DO 124 L=2,NODEL
              DO 125 J=1,L-1
                DIS=ABS(XC(L)-XC(J))+
     &              ABS(YC(L)-YC(J))+
     &              ABS(ZC(L)-ZC(J))
                IF(DIS.LE.TOL)THEN
                   NAC(L)=NAC(J)
                   GOTO 124
                ENDIF
 125          CONTINUE
              K=K+1
              NAC(L)=K
 124        CONTINUE
            DO 126 L=1,NODEL
              XC(NAC(L))=XC(L)
              YC(NAC(L))=YC(L)
              ZC(NAC(L))=ZC(L)
              ALC(NAC(L))=ALC(L)
              NVC(1,NAC(L))=NVC(1,L)
              NVC(2,NAC(L))=NVC(2,L)
 126        CONTINUE
            NODEL=K
          ENDIF
C
          IF(NODEL.LE.2)GOTO100
          IF(NODEL.GT.6)GOTO100
C------------------------------------------------
C MISE EN ORDRE DES NOEUDS SELON LEUR COSINUS
C------------------------------------------------
            XM=ZERO
            YM=ZERO
            ZM=ZERO	    
            DO 130 K=1,NODEL
              XM=XM+XC(K)/FLOAT(NODEL)
              YM=YM+YC(K)/FLOAT(NODEL)
  130         ZM=ZM+ZC(K)/FLOAT(NODEL)
C
            X1=XC(1)-XM
            Y1=YC(1)-YM
            Z1=ZC(1)-ZM
            V1=SQRT(X1**2+Y1**2+Z1**2)
            IF(V1.LT.TOL) GOTO100
            X1=X1/V1
            Y1=Y1/V1
            Z1=Z1/V1
            TET(1)=ZERO
C
            DO 140 K=2,NODEL
              XI=XC(K)-XM
              YI=YC(K)-YM
              ZI=ZC(K)-ZM
              VI=SQRT(XI**2+YI**2+ZI**2)
              IF(VI.LT.TOL)  GOTO100
              XI=XI/VI
              YI=YI/VI
              ZI=ZI/VI
              CSI=X1*XI+Y1*YI+Z1*ZI
              CSI=MAX(CSI,UNM)
              CSI=MIN(CSI,ONE)
C              IF(ABS(CSI).LT.1.E-8)CSI=0.
              XI1=Y1*ZI-YI*Z1
              YI1=Z1*XI-ZI*X1
              ZI1=X1*YI-XI*Y1
              SSI=XI1*VN(1)+YI1*VN(2)+ZI1*VN(3)
              SSI=MAX(SSI,UNM)
              SSI=MIN(SSI,ONE)
  140         TET(K)=ATAN2(SSI,CSI)
C
            DO 150 K=1,NODEL
              TETMIN=EP30
              DO 151 L=1,NODEL
                IF(TET(L).LT.TETMIN)THEN
                  LMIN=L
                  TETMIN=TET(L)
                ENDIF
  151         CONTINUE
              NCA(K)=LMIN
              TET(LMIN)=EP30
  150       CONTINUE
C
C------------------------------------------------------------------
C CREATION DES CONNECTIVITES DU POLYGONE ET VERIFICATION CONCAVITE
C------------------------------------------------------------------
            KB=0
            DO 155 K=1,NODEL
              N1=NCA(NODEL)
              IF(K.GT.1) N1=NCA(K-1)
              N2=NCA(K)
              N3=NCA(1)
              IF(K.LT.NODEL)N3=NCA(K+1)
C
              X12=XC(N2)-XC(N1)
              Y12=YC(N2)-YC(N1)
              Z12=ZC(N2)-ZC(N1)
              X23=XC(N3)-XC(N2)
              Y23=YC(N3)-YC(N2)
              Z23=ZC(N3)-ZC(N2)
              V12=SQRT(X12**2+Y12**2+Z12**2)*SQRT(X23**2+Y23**2+Z23**2)
              XI1=(Y12*Z23-Y23*Z12)/V12
              YI1=(Z12*X23-Z23*X12)/V12
              ZI1=(X12*Y23-X23*Y12)/V12
              SSI=XI1*VN(1)+YI1*VN(2)+ZI1*VN(3)
              IF(SSI.GT.EM30)THEN
                IF(KB.EQ.4)THEN
                  KB=KB+1
                  NCB(KB)=NCB(1)
                  KB=KB+1
                  NCB(KB)=NCB(KB-2)
                ENDIF
                KB=KB+1
                NCB(KB)=NCA(K)+NUMNEW
C                IF(KB.EQ.4.OR.KB.EQ.8)NEWL=NEWL+1
              ENDIF
  155       CONTINUE
C
            IF(KB.EQ.3.OR.KB.EQ.7)THEN
              KB=KB+1
              NCB(KB)=NCB(KB-1)
C              NEWL=NEWL+1
            ENDIF
            KB=INT(KB/4)
            DO K=1,KB
              NEWL=NEWL+1
              KK = (K-1)*4+1
              NC(1,NEWL)= NCB(KK)
              NC(2,NEWL)= NCB(KK+1)
              NC(3,NEWL)= NCB(KK+2)
              NC(4,NEWL)= NCB(KK+3)
              NC(5,NEWL)= I
            ENDDO
C--------------------------------
C AFFECTATION DES VALEURS NODALES
C--------------------------------
          IF(ITYP.EQ.2)THEN
            DO260 K=1,NODEL
              N1=NVC(1,K)
              N2=NVC(2,K)
              XC(K)=ALC(K)*XYZ0(1,N2)+(1-ALC(K))*XYZ0(1,N1)
              YC(K)=ALC(K)*XYZ0(2,N2)+(1-ALC(K))*XYZ0(2,N1)
              ZC(K)=ALC(K)*XYZ0(3,N2)+(1-ALC(K))*XYZ0(3,N1)
 260        CONTINUE
          ENDIF
          DO 270 K=1,NODEL
            XYZ(1,NUMNEW+K)=XC(K)
            XYZ(2,NUMNEW+K)=YC(K)
            XYZ(3,NUMNEW+K)=ZC(K)
            AL(NUMNEW+K)=ALC(K)
            NVOIS(1,NUMNEW+K)=NVC(1,K)
            NVOIS(2,NUMNEW+K)=NVC(2,K)
  270     CONTINUE
C
          NUMNEW=NUMNEW+NODEL
C
  100 CONTINUE
C
C--------------------------------
C  SOUDURE DES POLYGONES
C--------------------------------
      K=1
      NA(1)=1
      DO 1240 I=2,NUMNEW
        DO 1250 J=1,I-1
          DIS=ABS(XYZ(1,I)-XYZ(1,J))+
     &        ABS(XYZ(2,I)-XYZ(2,J))+
     &        ABS(XYZ(3,I)-XYZ(3,J))
          IF(DIS.LE.TOL)THEN
            NA(I)=NA(J)
            GOTO 1240
          ENDIF
 1250   CONTINUE
        K=K+1
        NA(I)=K
 1240 CONTINUE
      NUMTOT=K
      NUMEL=NEWL
C
      DO 1260 I=1,NUMNEW
        AL(NA(I))=AL(I)
        NVOIS(1,NA(I))=NVOIS(1,I)
        NVOIS(2,NA(I))=NVOIS(2,I)
        XYZ(1,NA(I))=XYZ(1,I)
        XYZ(2,NA(I))=XYZ(2,I)
        XYZ(3,NA(I))=XYZ(3,I)
 1260 CONTINUE
      DO K=1,NUMEL
        NC(1,K)=NA(NC(1,K))+NODCUT
        NC(2,K)=NA(NC(2,K))+NODCUT
        NC(3,K)=NA(NC(3,K))+NODCUT
        NC(4,K)=NA(NC(4,K))+NODCUT
      ENDDO
      IF(NUMEL.EQ.0)THEN
      X1=0.
      Y1=-VN(3)
      Z1= VN(2)
      V1=SQRT(Y1**2+Z1**2)
      IF(V1.GT.EM10)THEN
        V1=EP04*TOL/V1
        Y1=Y1*V1
        Z1=Z1*V1
        X2= VN(2)*Z1-VN(3)*Y1
        Y2=         -VN(1)*Z1
        Z2= VN(1)*Y1
      ELSE
        X1=ZERO
        Y1=EP04*TOL
        Z1=ZERO
        X2=ZERO
        Y2=ZERO
        Z2=EP04*TOL
      ENDIF
        NUMEL=1
        NUMTOT=1
        XYZ(1,NUMTOT)=X0(1)-X1-X2
        XYZ(2,NUMTOT)=X0(2)-Y1-Y2
        XYZ(3,NUMTOT)=X0(3)-Z1-Z2
        NC(1,1)=NUMTOT+NODCUT
        NVOIS(1,NUMTOT)=1
        NVOIS(2,NUMTOT)=1
        AL(NUMTOT)=ZERO
        NUMTOT=NUMTOT+1
        XYZ(1,NUMTOT)=X0(1)+X1-X2
        XYZ(2,NUMTOT)=X0(2)+Y1-Y2
        XYZ(3,NUMTOT)=X0(3)+Z1-Z2
        NC(2,1)=NUMTOT+NODCUT
        NVOIS(1,NUMTOT)=1
        NVOIS(2,NUMTOT)=1
        AL(NUMTOT)=0.
        NUMTOT=NUMTOT+1
        XYZ(1,NUMTOT)=X0(1)+X1+X2
        XYZ(2,NUMTOT)=X0(2)+Y1+Y2
        XYZ(3,NUMTOT)=X0(3)+Z1+Z2
        NC(3,1)=NUMTOT+NODCUT
        NVOIS(1,NUMTOT)=1
        NVOIS(2,NUMTOT)=1
        AL(NUMTOT)=0.
        NUMTOT=NUMTOT+1
        XYZ(1,NUMTOT)=X0(1)-X1+X2
        XYZ(2,NUMTOT)=X0(2)-Y1+Y2
        XYZ(3,NUMTOT)=X0(3)-Z1+Z2
        NC(4,1)=NUMTOT+NODCUT
        NVOIS(1,NUMTOT)=1
        NVOIS(2,NUMTOT)=1
        AL(NUMTOT)=ZERO
        NC(5,1)=1
      ENDIF
      RETURN
      END
