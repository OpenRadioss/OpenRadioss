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
Chd|  POLYHEDR1                     source/airbag/polyhedr1.F     
Chd|-- called by -----------
Chd|        FVMESH1                       source/airbag/fvmesh.F        
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        C_TRICALL                     stub/fvmbags_stub.F           
Chd|====================================================================
      SUBROUTINE POLYHEDR1(IPOLY, RPOLY   , POLB  , NPOLB, POLH,
     .                     NPOLH, NRPMAX  , NPHMAX, IBRIC, LMIN,
     .                     INFO , NPOLHMAX, NPPMAX, NEL,   INZ ,
     .                     TAGELA )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPPMAX, IPOLY(6+NPPMAX,*), POLB(*), NPOLB, NPHMAX, 
     .        POLH(NPHMAX+2,*),NPOLH, NRPMAX, IBRIC, INFO, NPOLHMAX
      INTEGER TAGELA(*)
      INTEGER NEL, INZ
      my_real
     .        RPOLY(NRPMAX,*), LMIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ICMAX, ICUR, II, J, JJ, K, KK, IEL, M
      INTEGER L, LL, ICUR_OLD, ITY, JMIN, PMIN, POLD, NEDGE, NVERTEX
      INTEGER IMIN, IMAX, I1, I2, I3, I4, N1, N2, N3, K1, K2
      INTEGER M1, M2, L1, L2
      INTEGER ITAG(2,NPOLB), POLEDG(NPOLB,NPPMAX+1), ITYP(NPOLB) 
      INTEGER NTYP(3), REDIR(4), TEMP1(4), TEMP2(4)
      INTEGER NNP, NHOL, NSEG, NELP, JTAG(3), NTRI3(NPOLB,NPPMAX)
      INTEGER, ALLOCATABLE :: EDGPOL(:,:)
      INTEGER, ALLOCATABLE :: EDGVER(:,:)
      INTEGER, ALLOCATABLE :: EDGLOC(:,:)
      INTEGER, ALLOCATABLE :: PSEG(:,:),PTRI(:,:)
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, XX1, YY1, ZZ1, XX2, YY2, ZZ2,
     .        DD11, DD12, DD13, DD21, DD22, TOLE
      my_real
     .        TST12, TST13, TST21, TST32, TST14,
     .        NX, NY, NZ, NX1, NY1, NZ1
      my_real
     .        X0, Y0, Z0, NRM1, VX1, VY1, VZ1, VX2, VY2, VZ2,
     .        VX, VY, VZ
      my_real
     .        ALPHA13, ALPHA14
      my_real, ALLOCATABLE :: XX(:), YY(:), ZZ(:)
      my_real, ALLOCATABLE :: PNODES(:,:), PHOLES(:,:)
C
      TOLE=EPSILON(ZERO)*0.5*LMIN*LMIN
C--------------------------------
C Triangularisation des polygones
C--------------------------------
      DO II=1,NPOLB
         I=POLB(II)
         NNP=IPOLY(2,I)
         NHOL=0
         ALLOCATE(PNODES(2,NNP), PSEG(2,NNP),PTRI(3,2*NNP),PHOLES(2,1))
C
C Coordonnees des sommets dans le plan du polygone
         NX=RPOLY(2,I)
         NY=RPOLY(3,I)
         NZ=RPOLY(4,I)
C
         X0=RPOLY(5,I)
         Y0=RPOLY(6,I)
         Z0=RPOLY(7,I)
         X1=RPOLY(8,I)
         Y1=RPOLY(9,I)
         Z1=RPOLY(10,I)
         NRM1=SQRT((X1-X0)**2+(Y1-Y0)**2+(Z1-Z0)**2)
         VX1=(X1-X0)/NRM1
         VY1=(Y1-Y0)/NRM1
         VZ1=(Z1-Z0)/NRM1
         VX2=NY*VZ1-NZ*VY1
         VY2=NZ*VX1-NX*VZ1
         VZ2=NX*VY1-NY*VX1
C
         PNODES(1,1)=ZERO
         PNODES(2,1)=ZERO
         DO J=2,NNP
            X1=RPOLY(4+3*(J-1)+1,I)
            Y1=RPOLY(4+3*(J-1)+2,I)
            Z1=RPOLY(4+3*(J-1)+3,I)
            VX=X1-X0
            VY=Y1-Y0
            VZ=Z1-Z0
            PNODES(1,J)=VX*VX1+VY*VY1+VZ*VZ1
            PNODES(2,J)=VX*VX2+VY*VY2+VZ*VZ2
            PSEG(1,J-1)=J-1
            PSEG(2,J-1)=J
         ENDDO
         PSEG(1,NNP)=NNP
         PSEG(2,NNP)=1
         NSEG=NNP
C
         CALL C_TRICALL(PNODES, PSEG, PHOLES, PTRI, NNP,
     .                  NSEG,   NHOL, NELP  )
CFA         IF(NELP > NNP) THEN
CFA            WRITE(IOUT,'(A,I5,A,I2,2(I5,A))') 'BRICK',IBRIC,
CFA     .            ' POLYGONE', I, NNP,' SOMMETS',NELP,' TRIANGLES'
CFA            DO J=1,NELP
CFA               WRITE(IOUT,'(4I5)') J,PTRI(1,J),PTRI(2,J),PTRI(3,J)
CFA            ENDDO
CFA         ENDIF
C--------------------------------------------------------------------------------
C Calcul pour chaque arete du 3eme noeud du triangle associe, stockage dans NTRI3
C--------------------------------------------------------------------------------
         DO J=1,NNP
           N1=J
           N2=J+1
           IF(J==NNP) N2=1
           DO K=1,NELP
              JTAG(K)=0
              DO L=1,3
                 IF(PTRI(L,K) == N1) JTAG(K)=JTAG(K)+1
                 IF(PTRI(L,K) == N2) JTAG(K)=JTAG(K)+1
              ENDDO
           ENDDO
           DO K=1,NELP
              IF(JTAG(K) /= 2) CYCLE
              DO L=1,3
                 IF(PTRI(L,K) == N1 .OR. PTRI(L,K) == N2) CYCLE
                 NTRI3(II,J) = PTRI(L,K)
              ENDDO
           ENDDO
         ENDDO ! II=1,NPOLB
C
         DEALLOCATE(PNODES, PSEG, PTRI, PHOLES)
      ENDDO
C--------------------------------------------------------------
C Construction polygone-arete
C POLEDG(i,1) donne le nombre d'aretes connectees au polygone i
C POLEDG(i,j+1) donne le numero de la jeme arete
C EDGVER(i,1) numero du premier sommet de l'arete i
C EDGVER(i,2) numero du deuxieme sommet de l'arete i
C--------------------------------------------------------------
      ALLOCATE (XX(NPOLB*NPPMAX*2))
      ALLOCATE (YY(NPOLB*NPPMAX*2))
      ALLOCATE (ZZ(NPOLB*NPPMAX*2))
      ALLOCATE (EDGVER(NPOLB*NPPMAX,2))
      DO I=1,NPOLB
         DO J=1,NPPMAX+1
            POLEDG(I,J)=0
         ENDDO
      ENDDO
      NEDGE=0
      NVERTEX=0
      DO I=1,NPOLB
         II=POLB(I)
         POLEDG(I,1)=IPOLY(2,II)
         DO J=1,IPOLY(2,II)
            JJ=J+1
            IF(POLEDG(I,JJ) > 0) CYCLE
            NEDGE=NEDGE+1
            POLEDG(I,JJ)=NEDGE
            X1=RPOLY(4+3*(J-1)+1,II)
            Y1=RPOLY(4+3*(J-1)+2,II)
            Z1=RPOLY(4+3*(J-1)+3,II)
            NVERTEX=NVERTEX+1
            EDGVER(NEDGE,1)=NVERTEX
            XX(NVERTEX)=X1
            YY(NVERTEX)=Y1
            ZZ(NVERTEX)=Z1
            IF (J==IPOLY(2,II)) JJ=1
            X2=RPOLY(4+3*(JJ-1)+1,II)
            Y2=RPOLY(4+3*(JJ-1)+2,II)
            Z2=RPOLY(4+3*(JJ-1)+3,II)
            NVERTEX=NVERTEX+1
            EDGVER(NEDGE,2)=NVERTEX
            XX(NVERTEX)=X2
            YY(NVERTEX)=Y2
            ZZ(NVERTEX)=Z2
            DO K=1,NPOLB
               IF (K.EQ.I) CYCLE
               KK=POLB(K)
               DO L=1,IPOLY(2,KK)
                  LL=L+1
                  IF (L==IPOLY(2,KK)) LL=1
                  XX1=RPOLY(4+3*(L-1)+1,KK)
                  YY1=RPOLY(4+3*(L-1)+2,KK)
                  ZZ1=RPOLY(4+3*(L-1)+3,KK)
                  XX2=RPOLY(4+3*(LL-1)+1,KK)
                  YY2=RPOLY(4+3*(LL-1)+2,KK)
                  ZZ2=RPOLY(4+3*(LL-1)+3,KK)
                  DD11=(XX1-X1)**2+(YY1-Y1)**2+(ZZ1-Z1)**2
                  DD21=(XX2-X1)**2+(YY2-Y1)**2+(ZZ2-Z1)**2
                  DD12=(XX1-X2)**2+(YY1-Y2)**2+(ZZ1-Z2)**2
                  DD22=(XX2-X2)**2+(YY2-Y2)**2+(ZZ2-Z2)**2
                  IF ((DD11.LE.TOLE.AND.DD22.LE.TOLE).OR.
     .                (DD21.LE.TOLE.AND.DD12.LE.TOLE)) THEN
                      POLEDG(K,L+1)=NEDGE
                  ENDIF
               ENDDO               
            ENDDO
         ENDDO
      ENDDO
C---------------------------------------------------------------
C Construction arete-polygone
C EDGPOL(i,1) donne le nombre de polygones connectes a l'arete i
C EDGPOL(i,j+1) donne le numero du jeme polygone
C EDGLOC(i,k) donne la position de l'arete i dans POLEDG
C---------------------------------------------------------------
      ALLOCATE(EDGPOL(NEDGE,5))
      ALLOCATE(EDGLOC(NEDGE,4))
      DO I=1,NEDGE
         DO J=1,4
            EDGPOL(I,J)=0
            EDGLOC(I,J)=0
         ENDDO
         EDGPOL(I,5)=0
      ENDDO
      DO I=1,NPOLB
         DO J=1,POLEDG(I,1)
            K=POLEDG(I,J+1)
            IF(K==0) CYCLE
            KK=EDGPOL(K,1)
            KK=KK+1
            IF(KK > 4) THEN
               N1=EDGVER(K,1)
               N2=EDGVER(K,2)
               WRITE(IOUT,'(A,I5,A /A,1P3G20.13,A/A,1P3G20.13,A)') 
     .         ' FVMBAG MESH ERROR : EDGE N1N2 IS CONNECTED TO ',KK,
     .         ' POLYGONS BUT THE LIMIT IS 4',
     .         ' N1 = (',XX(N1),YY(N1),ZZ(N1),')',
     .         ' N2 = (',XX(N2),YY(N2),ZZ(N2),')'
            CALL ARRET(2)
            ENDIF
            EDGPOL(K,1)=KK
            EDGPOL(K,KK+1)=I 
            EDGLOC(K,KK)=J 
         ENDDO
      ENDDO
C
      DO I=1,NPOLB
         ITAG(1,I)=0
         ITAG(2,I)=0
         II=POLB(I)
         ITY=IPOLY(1,II)
         IEL=IPOLY(3,II)
         IF(ITY == 1 .AND. TAGELA(IEL) > NEL) ITY=3
         ITYP(I)=ITY
      ENDDO
C-------
C Checks
C-------
      DO J=1,NEDGE
         N1=EDGVER(J,1)
         N2=EDGVER(J,2)
         DO K=1,3
            NTYP(K)=0
         ENDDO
         L=EDGPOL(J,1)
         DO K=1,L
            II=EDGPOL(J,K+1)
            IF(ITYP(II) == 1) NTYP(1)=NTYP(1)+1
            IF(ITYP(II) == 2) NTYP(2)=NTYP(2)+1         
            IF(ITYP(II) == 3) NTYP(3)=NTYP(3)+1
         ENDDO         
         IF(L == 1) THEN
            WRITE(IOUT,'(/2(A,I5),A,2(/A,1P3G20.13,A))') 
     .        ' FVMBAG MESH ERROR : BRICK ',IBRIC,' LAYER ',INZ,
     .        ' EDGE N1N2 IS CONNECTED TO ONLY 1 POLYGON',
     .        ' N1 = (',XX(N1),YY(N1),ZZ(N1),')',
     .        ' N2 = (',XX(N2),YY(N2),ZZ(N2),')'
            CALL ARRET(2)
         ELSEIF(L == 2 .AND. NTYP(3) == 1) THEN
            WRITE(IOUT,'(/2(A,I5),2A,2(/A,1P3G20.13,A))') 
     .        ' FVMBAG MESH ERROR : BRICK ',IBRIC,' LAYER ',INZ,
     .        ' EDGE N1N2 IS CONNECTED TO 2 POLYGONS BUT ONLY 1',
     .        ' INTERNAL POLYGON',
     .        ' N1 = (',XX(N1),YY(N1),ZZ(N1),')',
     .        ' N2 = (',XX(N2),YY(N2),ZZ(N2),')'
            CALL ARRET(2)
         ELSEIF(L == 3 .AND. NTYP(3) == 2) THEN
            WRITE(IOUT,'(/2(A,I5),2A,2(/A,1P3G20.13,A))') 
     .        ' FVMBAG MESH ERROR : BRICK ',IBRIC,' LAYER ',INZ,
     .        ' EDGE N1N2 IS CONNECTED TO 3 POLYGONS 2 BEING',
     .        ' INTERNAL POLYGONS',
     .        ' N1 = (',XX(N1),YY(N1),ZZ(N1),')',
     .        ' N2 = (',XX(N2),YY(N2),ZZ(N2),')'
            CALL ARRET(2)
         ELSEIF(L == 4) THEN
            IF(NTYP(3) == 1) THEN
              WRITE(IOUT,'(/2(A,I5),2A,2(/A,1P3G20.13,A))') 
     .          ' FVMBAG MESH ERROR : BRICK ',IBRIC,' LAYER ',INZ,
     .          ' EDGE N1N2 IS CONNECTED TO 4 POLYGONS ONLY 1 BEING',
     .          ' INTERNAL POLYGON',
     .          ' N1 = (',XX(N1),YY(N1),ZZ(N1),')',
     .          ' N2 = (',XX(N2),YY(N2),ZZ(N2),')'
              CALL ARRET(2)
            ELSEIF( NTYP(3) >= 3) THEN
              WRITE(IOUT,'(/2(A,I5),2A,2(/A,1P3G20.13,A))') 
     .          ' FVMBAG MESH ERROR : BRICK ',IBRIC,' LAYER ',INZ,
     .          ' EDGE N1N2 IS CONNECTED TO 4 POLYGONS 3 BEING',
     .          ' INTERNAL POLYGONS',
     .          ' N1 = (',XX(N1),YY(N1),ZZ(N1),')',
     .          ' N2 = (',XX(N2),YY(N2),ZZ(N2),')'
              CALL ARRET(2)
            ENDIF
         ELSEIF(L >= 5) THEN
            WRITE(IOUT,'(/2(A,I5),A,2(/A,1P3G20.13,A))') 
     .        ' FVMBAG MESH ERROR : BRICK ',IBRIC,' LAYER ',INZ,
     .        ' EDGE N1N2 IS CONNECTED TO MORE THAN 4 POLYGONS',
     .        ' N1 = (',XX(N1),YY(N1),ZZ(N1),')',
     .        ' N2 = (',XX(N2),YY(N2),ZZ(N2),')'
            CALL ARRET(2)
         ENDIF
      ENDDO ! J=1,NEDGE
C---------------------------------------------------
C Mettre les polygones internes en premiere position
C---------------------------------------------------
      DO J=1,NEDGE
         IF(EDGPOL(J,1) == 2 ) CYCLE
            DO K=1,EDGPOL(J,1)
               TEMP1(K)=EDGPOL(J,K+1)
               TEMP2(K)=EDGLOC(J,K)
            ENDDO
            I=1
            DO K=1,EDGPOL(J,1)
               L=EDGPOL(J,K+1)
               IF(ITYP(L) == 3) THEN
                  REDIR(I)=K
                  I=I+1
               ENDIF
            ENDDO
            DO K=1,EDGPOL(J,1)
               L=EDGPOL(J,K+1)
               IF(ITYP(L) /= 3) THEN
                  REDIR(I)=K
                  I=I+1
               ENDIF
            ENDDO
            DO K=1,EDGPOL(J,1)
               EDGPOL(J,K+1)=TEMP1(REDIR(K))
               EDGLOC(J,K)  =TEMP2(REDIR(K))
            ENDDO
      ENDDO ! J=1,NEDGE
C--------------------------------------------------------------------
C Construction des polyedres
C   ITAG(1,i) le polyedre est du cote oppose a la normale au polygone
C   ITAG(2,i) le polyedre est du cote de la normale au polygone
C--------------------------------------------------------------------
      ICMAX=0
C------------------------------
C Arete connectee a 4 polygones
C------------------------------
      DO J=1,NEDGE
          IF(EDGPOL(J,1) < 4) CYCLE
          I1=EDGPOL(J,2)
          I2=EDGPOL(J,3)
          I3=EDGPOL(J,4)
          I4=EDGPOL(J,5)
          II=POLB(I1)
          NX=RPOLY(2,II)
          NY=RPOLY(3,II)
          NZ=RPOLY(4,II)
          N1=EDGVER(J,1)
          X1=XX(N1)
          Y1=YY(N1)
          Z1=ZZ(N1)
          JJ=EDGLOC(J,2)
          JJ=NTRI3(I2,JJ)
          II=POLB(I2)
          X2=RPOLY(4+3*(JJ-1)+1,II)
          Y2=RPOLY(4+3*(JJ-1)+2,II)
          Z2=RPOLY(4+3*(JJ-1)+3,II)
          TST12=NX*(X2-X1)+NY*(Y2-Y1)+NZ*(Z2-Z1)
C
          JJ=EDGLOC(J,3)
          JJ=NTRI3(I3,JJ)
          II=POLB(I3)
          NX1=RPOLY(2,II)
          NY1=RPOLY(3,II)
          NZ1=RPOLY(4,II)
          X2=RPOLY(4+3*(JJ-1)+1,II)
          Y2=RPOLY(4+3*(JJ-1)+2,II)
          Z2=RPOLY(4+3*(JJ-1)+3,II)
          VX=X2-X1
          VY=Y2-Y1
          VZ=Z2-Z1
          TST13=NX*VX+NY*VY+NZ*VZ
          ALPHA13=ACOS(NX*NX1+NY*NY1+NZ*NZ1)
C
          JJ=EDGLOC(J,4)
          JJ=NTRI3(I4,JJ)
          II=POLB(I4)
          NX1=RPOLY(2,II)
          NY1=RPOLY(3,II)
          NZ1=RPOLY(4,II)
          X2=RPOLY(4+3*(JJ-1)+1,II)
          Y2=RPOLY(4+3*(JJ-1)+2,II)
          Z2=RPOLY(4+3*(JJ-1)+3,II)
          VX=X2-X1
          VY=Y2-Y1
          VZ=Z2-Z1
          TST14=NX*VX+NY*VY+NZ*VZ
          ALPHA14=ACOS(NX*NX1+NY*NY1+NZ*NZ1)
C
          II=POLB(I2)
          NX=RPOLY(2,II)
          NY=RPOLY(3,II)
          NZ=RPOLY(4,II)
          JJ=EDGLOC(J,1)
          JJ=NTRI3(I1,JJ)
          II=POLB(I1)
          X2=RPOLY(4+3*(JJ-1)+1,II)
          Y2=RPOLY(4+3*(JJ-1)+2,II)
          Z2=RPOLY(4+3*(JJ-1)+3,II)
          TST21=NX*(X2-X1)+NY*(Y2-Y1)+NZ*(Z2-Z1)
C			 
          IF(TST12*TST21 < ZERO) THEN
            N2=EDGVER(J,2)
            WRITE(IOUT,'(/2(A,I5),2A,2(/A,1P3G20.13,A))') 
     .        ' FVMBAG MESH ERROR : BRICK ',IBRIC,' LAYER ',INZ,
     .        ' WRONG NORMAL ORIENTATION OF INTERNAL SURFACE ELEMENTS',
     .        ' CONNECTED TO EDGE N1N2',
     .        ' N1 = (',XX(N1),YY(N1),ZZ(N1),')',
     .        ' N2 = (',XX(N2),YY(N2),ZZ(N2),')'
            CALL ARRET(2)
          ELSE
            ICMAX=ICMAX+1
            ITAG(1,I1)=ICMAX
            ITAG(1,I2)=ICMAX
          ENDIF
C
          IF(TST13 > ZERO .AND. TST14 > ZERO) THEN
            IF(ALPHA13 < ALPHA14) THEN
              ICMAX=ICMAX+1
              ITAG(2,I1)=ICMAX
              ITAG(1,I3)=ICMAX
              ICMAX=ICMAX+1
              ITAG(2,I2)=ICMAX
              ITAG(1,I4)=ICMAX
            ELSE
              ICMAX=ICMAX+1
              ITAG(2,I1)=ICMAX
              ITAG(1,I4)=ICMAX
              ICMAX=ICMAX+1
              ITAG(2,I2)=ICMAX
              ITAG(1,I3)=ICMAX
            ENDIF
          ELSEIF(TST13 < ZERO .AND. TST14 < ZERO) THEN
            IF(ALPHA13 < ALPHA14) THEN
              ICMAX=ICMAX+1
              ITAG(2,I1)=ICMAX
              ITAG(1,I4)=ICMAX
              ICMAX=ICMAX+1
              ITAG(2,I2)=ICMAX
              ITAG(1,I3)=ICMAX
            ELSE
              ICMAX=ICMAX+1
              ITAG(2,I1)=ICMAX
              ITAG(1,I3)=ICMAX
              ICMAX=ICMAX+1
              ITAG(2,I2)=ICMAX
              ITAG(1,I4)=ICMAX
            ENDIF
          ELSEIF(TST13 > ZERO .AND. TST14 < ZERO) THEN
            ICMAX=ICMAX+1
            ITAG(2,I1)=ICMAX
            ITAG(1,I3)=ICMAX
            ICMAX=ICMAX+1
            ITAG(2,I2)=ICMAX
            ITAG(1,I4)=ICMAX
          ELSEIF(TST13 < ZERO .AND. TST14 > ZERO) THEN
            ICMAX=ICMAX+1
            ITAG(2,I1)=ICMAX
            ITAG(1,I4)=ICMAX
            ICMAX=ICMAX+1
            ITAG(2,I2)=ICMAX
            ITAG(1,I3)=ICMAX
          ENDIF
      ENDDO ! J=1,NEDGE
C---------------------------------------
C Arete connectee a 3 polygones internes
C---------------------------------------
      DO J=1,NEDGE
          IF(EDGPOL(J,1) /= 3) CYCLE		  
          I1=EDGPOL(J,2)
          I2=EDGPOL(J,3)
          IF(ITYP(I2) /= 3) CYCLE
          I3=EDGPOL(J,4)
          N1=EDGVER(J,1)
          X1=XX(N1)
          Y1=YY(N1)
          Z1=ZZ(N1)
C Position du polygone I2 par rapport au polygone I1
          II=POLB(I1)
          NX=RPOLY(2,II)
          NY=RPOLY(3,II)
          NZ=RPOLY(4,II)
          JJ=EDGLOC(J,2)
          JJ=NTRI3(I2,JJ)
          II=POLB(I2)
          X2=RPOLY(4+3*(JJ-1)+1,II)
          Y2=RPOLY(4+3*(JJ-1)+2,II)
          Z2=RPOLY(4+3*(JJ-1)+3,II)
          TST12=NX*(X2-X1)+NY*(Y2-Y1)+NZ*(Z2-Z1)
          IF(TST12 < ZERO) THEN
             K1=1
             K2=2
          ELSE
             K1=2
             K2=1
          ENDIF
C Position du polygone I1 par rapport au polygone I2
          II=POLB(I2)
          NX=RPOLY(2,II)
          NY=RPOLY(3,II)
          NZ=RPOLY(4,II)
          JJ=EDGLOC(J,2)
          JJ=NTRI3(I1,JJ)
          II=POLB(I1)
          X2=RPOLY(4+3*(JJ-1)+1,II)
          Y2=RPOLY(4+3*(JJ-1)+2,II)
          Z2=RPOLY(4+3*(JJ-1)+3,II)
          TST21=NX*(X2-X1)+NY*(Y2-Y1)+NZ*(Z2-Z1)
          IF(TST21 < ZERO) THEN
             L1=1
             L2=2
          ELSE
             L1=2
             L2=1
          ENDIF
C Position du polygone I2 par rapport au polygone I3
          II=POLB(I3)
          NX=RPOLY(2,II)
          NY=RPOLY(3,II)
          NZ=RPOLY(4,II)
          JJ=EDGLOC(J,2)
          JJ=NTRI3(I2,JJ)
          II=POLB(I2)
          X2=RPOLY(4+3*(JJ-1)+1,II)
          Y2=RPOLY(4+3*(JJ-1)+2,II)
          Z2=RPOLY(4+3*(JJ-1)+3,II)
          TST32=NX*(X2-X1)+NY*(Y2-Y1)+NZ*(Z2-Z1)
          IF(TST32 < ZERO) THEN
             M1=1
             M2=2
          ELSE
             M1=2
             M2=1
          ENDIF
C Premier polyedre
          IF (ITAG(K1,I1) == 0 .AND. ITAG(L1,I2) == 0) THEN
            ICMAX=ICMAX+1
            ITAG(K1,I1)=ICMAX
            ITAG(L1,I2)=ICMAX						
          ELSEIF(ITAG(K1,I1) == 0 .AND. ITAG(L1,I2) /= 0) THEN
            ITAG(K1,I1)=ITAG(L1,I2)
          ELSEIF(ITAG(K1,I1) /= 0. AND. ITAG(L1,I2) == 0) THEN
            ITAG(L1,I2)=ITAG(K1,I1)
          ELSEIF(ITAG(K1,I1) /= 0 .AND. ITAG(L1,I2) /= 0) THEN
            IF(ITAG(K1,I1) /= ITAG(L1,I2)) THEN
               IMIN=MIN(ITAG(L1,I2),ITAG(K1,I1))
               IMAX=MAX(ITAG(L1,I2),ITAG(K1,I1))
               DO I=1,NPOLB
                 IF(ITAG(1,I) == IMAX) ITAG(1,I)=IMIN
                 IF(ITYP(I) /= 3) CYCLE
                 IF(ITAG(2,I) == IMAX) ITAG(2,I)=IMIN
               ENDDO
            ENDIF			
          ENDIF 
C Deuxieme polyedre
          IF(ITAG(M1,I3) == 0 .AND. ITAG(L2,I2) == 0) THEN
            ICMAX=ICMAX+1
            ITAG(M1,I3)=ICMAX
            ITAG(L2,I2)=ICMAX
          ELSEIF(ITAG(M1,I3) /= 0 .AND. ITAG(L2,I2) == 0) THEN
            ITAG(L2,I2)=ITAG(M1,I3)
          ELSEIF(ITAG(M1,I3) == 0 .AND. ITAG(L2,I2) /= 0) THEN
            ITAG(M1,I3)=ITAG(L2,I2)			
          ELSEIF(ITAG(M1,I3) /= 0 .AND. ITAG(L2,I2) /= 0) THEN
            IF(ITAG(M1,I3) /= ITAG(L2,I2)) THEN
               IMIN=MIN(ITAG(M1,I3),ITAG(L2,I2))
               IMAX=MAX(ITAG(M1,I3),ITAG(L2,I2))
               DO I=1,NPOLB
                 IF(ITAG(1,I) == IMAX) ITAG(1,I)=IMIN
                 IF(ITYP(I) /= 3) CYCLE
                 IF(ITAG(2,I) == IMAX) ITAG(2,I)=IMIN
               ENDDO
            ENDIF
          ENDIF
C Troisieme polyedre
          IF(ITAG(M2,I3) == 0 .AND. ITAG(K2,I1) == 0) THEN
            ICMAX=ICMAX+1
            ITAG(M2,I3)=ICMAX
            ITAG(K2,I1)=ICMAX
          ELSEIF(ITAG(M2,I3) /= 0 .AND. ITAG(K2,I1) == 0) THEN
            ITAG(K2,I1)=ITAG(M2,I3)
          ELSEIF(ITAG(M2,I3) == 0 .AND. ITAG(K2,I1) /= 0) THEN
            ITAG(M2,I3)=ITAG(K2,I1)			
          ELSEIF(ITAG(M2,I3) /= 0 .AND. ITAG(K2,I1) /= 0) THEN
            IF(ITAG(M2,I3) /= ITAG(K2,I1)) THEN
               IMIN=MIN(ITAG(M2,I3),ITAG(K2,I1))
               IMAX=MAX(ITAG(M2,I3),ITAG(K2,I1))
               DO I=1,NPOLB
                 IF(ITAG(1,I) == IMAX) ITAG(1,I)=IMIN
                 IF(ITYP(I) /= 3) CYCLE
                 IF(ITAG(2,I) == IMAX) ITAG(2,I)=IMIN
               ENDDO
            ENDIF
          ENDIF		  
      ENDDO ! J=1,NEDGE
C-------------------------------------------------------
C Arete connectee a 3 polygones (1 interne + 2 externes)
C-------------------------------------------------------
      DO J=1,NEDGE
          IF(EDGPOL(J,1) /= 3) CYCLE		  
          I1=EDGPOL(J,2)
          I2=EDGPOL(J,3)
          IF(ITYP(I2) == 3) CYCLE
          I3=EDGPOL(J,4)
          II=POLB(I1)
          NX=RPOLY(2,II)
          NY=RPOLY(3,II)
          NZ=RPOLY(4,II)
          N1=EDGVER(J,1)
          X1=XX(N1)
          Y1=YY(N1)
          Z1=ZZ(N1)
          JJ=EDGLOC(J,2)
          JJ=NTRI3(I2,JJ)
          II=POLB(I2)
          X2=RPOLY(4+3*(JJ-1)+1,II)
          Y2=RPOLY(4+3*(JJ-1)+2,II)
          Z2=RPOLY(4+3*(JJ-1)+3,II)
          TST12=NX*(X2-X1)+NY*(Y2-Y1)+NZ*(Z2-Z1)
          IF(TST12 < ZERO) THEN
             K1=1
             K2=2
          ELSE
             K1=2
             K2=1
          ENDIF
          IF (ITAG(K1,I1) == 0 .AND. ITAG(1,I2) == 0) THEN
            ICMAX=ICMAX+1
            ITAG(K1,I1)=ICMAX
            ITAG(1,I2)=ICMAX
            IF(ITAG(1,I3) == 0) THEN
              ICMAX=ICMAX+1
              ITAG(K2,I1)=ICMAX
              ITAG(1,I3)=ICMAX
            ELSE
              ITAG(K2,I1)=ITAG(1,I3)
            ENDIF
          ELSEIF(ITAG(K1,I1) == 0 .AND. ITAG(1,I2) /= 0) THEN
            ITAG(K1,I1)=ITAG(1,I2)
            IF(ITAG(1,I3) == 0) THEN
              ICMAX=ICMAX+1
              ITAG(K2,I1)=ICMAX
              ITAG(1,I3)=ICMAX
            ELSE
              ITAG(K2,I1)=ITAG(1,I3)
            ENDIF
          ELSEIF(ITAG(K1,I1) /= 0. AND. ITAG(1,I2) == 0) THEN
            ITAG(1,I2)=ITAG(K1,I1)
            IF(ITAG(1,I3) == 0) THEN
               ITAG(1,I3)=ITAG(K2,I1)
            ELSEIF(ITAG(1,I3) /= ITAG(K2,I1)) THEN
               IMIN=MIN(ITAG(1,I3),ITAG(K2,I1))
               IMAX=MAX(ITAG(1,I3),ITAG(K2,I1))
               DO I=1,NPOLB
                 IF(ITAG(1,I) == IMAX) ITAG(1,I)=IMIN
                 IF(ITYP(I) /= 3) CYCLE
                 IF(ITAG(2,I) == IMAX) ITAG(2,I)=IMIN
               ENDDO
            ENDIF
          ELSEIF(ITAG(K1,I1) /= 0 .AND. ITAG(1,I2) /= 0) THEN
            IF(ITAG(K1,I1) /= ITAG(1,I2)) THEN
               IMIN=MIN(ITAG(1,I2),ITAG(K1,I1))
               IMAX=MAX(ITAG(1,I2),ITAG(K1,I1))
               DO I=1,NPOLB
                 IF(ITAG(1,I) == IMAX) ITAG(1,I)=IMIN
                 IF(ITYP(I) /= 3) CYCLE
                 IF(ITAG(2,I) == IMAX) ITAG(2,I)=IMIN
               ENDDO
            ENDIF
            IF(ITAG(1,I3) == 0) THEN
                ITAG(1,I3)=ITAG(K2,I1)
            ELSEIF(ITAG(1,I3) /= ITAG(K2,I1)) THEN
               IMIN=MIN(ITAG(1,I3),ITAG(K2,I1))
               IMAX=MAX(ITAG(1,I3),ITAG(K2,I1))
               DO I=1,NPOLB
                 IF(ITAG(1,I) == IMAX) ITAG(1,I)=IMIN
                 IF(ITYP(I) /= 3) CYCLE
                 IF(ITAG(2,I) == IMAX) ITAG(2,I)=IMIN
               ENDDO
            ENDIF 
          ENDIF 		  
      ENDDO ! J=1,NEDGE
C------------------------------------------------
C Arete connectee a 2 polygones
C Les 2 polygones appartiennent au meme polyedre
C------------------------------------------------
      DO J=1,NEDGE
          IF(EDGPOL(J,1) > 2) CYCLE
          I1=EDGPOL(J,2)
          I2=EDGPOL(J,3)
          IF((ITYP(I1) /= 3 .AND. ITYP(I2) /= 3) .OR.
     .       (ITYP(I1) == 3 .AND. ITYP(I2) == 3) ) THEN
            IF (ITAG(1,I1) == 0 .AND. ITAG(1,I2) == 0) THEN
              ICMAX=ICMAX+1
              ITAG(1,I1)=ICMAX
              ITAG(1,I2)=ICMAX
            ELSEIF(ITAG(1,I1) == 0 .AND. ITAG(1,I2) /= 0) THEN
              ITAG(1,I1)=ITAG(1,I2)
            ELSEIF(ITAG(1,I1) /= 0. AND. ITAG(1,I2) == 0) THEN
              ITAG(1,I2)=ITAG(1,I1)
            ELSEIF(ITAG(1,I1) /= ITAG(1,I2)) THEN
              IMIN=MIN(ITAG(1,I1),ITAG(1,I2))
              IMAX=MAX(ITAG(1,I1),ITAG(1,I2))
              DO I=1,NPOLB
                 IF(ITAG(1,I) == IMAX) ITAG(1,I)=IMIN
                 IF(ITYP(I) /= 3) CYCLE
                 IF(ITAG(2,I) == IMAX) ITAG(2,I)=IMIN
              ENDDO
            ENDIF
          ENDIF
C
          IF(ITYP(I1) == 3 .AND. ITYP(I2) == 3) THEN
            IF (ITAG(2,I1) == 0 .AND. ITAG(2,I2) == 0) THEN
              ICMAX=ICMAX+1
              ITAG(2,I1)=ICMAX
              ITAG(2,I2)=ICMAX
            ELSEIF(ITAG(2,I1) == 0 .AND. ITAG(2,I2) /= 0) THEN
              ITAG(2,I1)=ITAG(2,I2)
            ELSEIF(ITAG(2,I1) /= 0. AND. ITAG(2,I2) == 0) THEN
              ITAG(2,I2)=ITAG(2,I1)
            ELSEIF(ITAG(2,I1) /= ITAG(2,I2)) THEN
              IMIN=MIN(ITAG(2,I1),ITAG(2,I2))
              IMAX=MAX(ITAG(2,I1),ITAG(2,I2))
              DO I=1,NPOLB
                 IF(ITAG(1,I) == IMAX) ITAG(1,I)=IMIN
                 IF(ITYP(I) /= 3) CYCLE
                 IF(ITAG(2,I) == IMAX) ITAG(2,I)=IMIN
              ENDDO
            ENDIF
          ENDIF			
      ENDDO  ! J=1,NEDGE
C
      DEALLOCATE (EDGPOL)
      DEALLOCATE (EDGLOC)
      DEALLOCATE (EDGVER)
      DEALLOCATE (XX,YY,ZZ)
C
      NPOLH=0
      DO I=1,ICMAX
         II=0
         DO J=1,NPOLB
            IF (ITAG(1,J) == I .OR. ITAG(2,J) == I) II=II+1
         ENDDO
         IF (II.NE.0) NPOLH=NPOLH+1
      ENDDO
      IF (NPOLH.GT.NPOLHMAX) THEN
         INFO=1
         NPOLHMAX=NPOLH
         RETURN
      ENDIF
C
      NPOLH=0
      DO I=1,ICMAX
         II=0
         DO J=1,NPOLB
            IF (ITAG(1,J) == I .OR. ITAG(2,J) == I) II=II+1
         ENDDO
         IF (II.NE.0) THEN
            NPOLH=NPOLH+1
            POLH(1,NPOLH)=II
            POLH(2,NPOLH)=IBRIC
            II=0
            DO J=1,NPOLB
               IF (ITAG(1,J) == I .OR. ITAG(2,J) == I) THEN
                  II=II+1
                  JJ=POLB(J)
                  POLH(2+II,NPOLH)=JJ
                  ITY=ITYP(J)
                  IF (ITY == 1) THEN
                     IPOLY(5,JJ)=NPOLH
                  ELSEIF(ITY == 2) THEN
                    IF (IPOLY(5,JJ).EQ.0) THEN
                       IPOLY(5,JJ)=NPOLH
                    ELSE
                       IPOLY(6,JJ)=NPOLH
                    ENDIF
                  ELSEIF(ITY == 3) THEN
                    IF(ITAG(1,J) == I) THEN
                       IPOLY(5,JJ)=NPOLH
                    ELSEIF(ITAG(2,J) == I) THEN
                       IPOLY(6,JJ)=NPOLH
                    ENDIF
                  ENDIF
               ENDIF
            ENDDO
C-------------------------------------------------------
C Tri de POLH par ordre croissant de numero de polygones
C-------------------------------------------------------
            DO J=1,POLH(1,NPOLH)
               JMIN=J
               PMIN=POLH(2+J,NPOLH)
               DO K=J+1,POLH(1,NPOLH)
                  IF (POLH(2+K,NPOLH).LT.PMIN) THEN
                     JMIN=K
                     PMIN=POLH(2+K,NPOLH)
                  ENDIF
               ENDDO
               POLD=POLH(2+J,NPOLH)
               POLH(2+J,NPOLH)=PMIN
               POLH(2+JMIN,NPOLH)=POLD
            ENDDO  !J=1,NPOLB
         ENDIF
      ENDDO  !I=1,ICMAX
C
      INFO=0
      RETURN
      END
