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
Chd|  FACEPOLY                      source/airbag/facepoly.F
Chd|-- called by -----------
Chd|        FVMESH1                       source/airbag/fvmesh.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F
Chd|====================================================================
      SUBROUTINE FACEPOLY(QUAD  , TRI  , NTRI  , IPOLY, RPOLY  ,
     .                    N     , NORMF, NORMT , NSMAX, NNP    ,
     .                    NRPMAX, NB   , NV    , LMIN , NPOLMAX,
     .                    NPPMAX, INFO , IELNOD, X    , IFAC   ,
     .                    ILVOUT, MVID )
C
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
        INTEGER TRI(NSMAX,*), NTRI, NPPMAX, IPOLY(6+NPPMAX+1+NPOLMAX,*),
     .          NSMAX, NRPMAX, NNP, NB, NV, NPOLMAX, INFO,
     .          IELNOD(NPPMAX,*), IFAC, ILVOUT, MVID
        my_real
     .          QUAD(3,*), RPOLY(NRPMAX+3*NPOLMAX,*), N(*), NORMF(3,*),
     .          NORMT(3,*), LMIN, X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, NINTER(4), NP, J, JJ, IDBL, NPI, NSEG, REDIR(5*NTRI),
     .          K, KK, JJ1, NNO, ID1, ID2, ISEG(5,5*NTRI), NSEGF, ISTOP,
     .          ISSEG, IPSEG, ITAGSEG(5*NTRI), ITAG(5*NTRI*2), ICLOSE,
     .          INEXT, NN, NPOLY, IADPOLY(NTRI), LENPOLY(NTRI), IAD, II,
     .          POLY(2,5*NTRI+1), I0, JJJ, REDIR_TMP(5*NTRI), NPI_TMP,
     .          ELSEG(5*NTRI,2), ELINTER(4,NTRI), ELNODE(5*NTRI*2),
     .          NNS, LENMAX, NEDGE, J1, J2, IEDGE, K1, K2,
     .          TEDGE(3*NTRI,3), EDGE(3*NTRI,3), N1, N2, IPOUT, M, MM,
     .          NHOL, IADHOL(NPOLMAX), ISEG3_OLD(5*NTRI), IDIFF, NSEC,
     .          KSMIN,NOK,ISSEGOLD
        INTEGER ID0, KSEG, JSEG(4), J3, ID3, IP0SEG
        INTEGER IADR(4), I1, I2, I3, I4, L, ICMAX, IMIN, IMAX, ALGO
        INTEGER TEMP(5*NTRI),JTAG(2,5*NTRI)
        my_real
     .          TOLE, TOLE2, A(3), X1, Y1, Z1, X2, Y2, Z2, SS1, SS2,
     .          X12, Y12, Z12, XA1, YA1, ZA1, ALPHA, XM, YM, ZM,
     .          STMP(4,3), SEG(5*NTRI,3,2), NX, NY, NZ, XA2, YA2, ZA2,
     .          XA12, YA12, ZA12, XA1P1, YA1P1, ZA1P1, BETA,
     .          PINTER(4,NTRI,4), XL(NTRI), XLREF, XP1, YP1, ZP1, XP2,
     .          YP2, ZP2, NODE(3,5*NTRI*2), XX1, YY1, ZZ1, DD1, DD2,
     .          XLC, LL, TOLEI, T1, T2, DD, XA1P2, YA1P2, ZA1P2,
     .          XEDGE(3*NTRI,4), XX2, YY2, ZZ2, NR1, NR2, VX1, VY1, VZ1,
     .          VX2, VY2, VZ2, SS, VVX, VVY, VVZ, XX, YY, ZZ,
     .          PHOL(3,NPOLMAX), XLOC(2,NPPMAX), XSEC(NPPMAX), YLMIN,
     .          YLMAX, YLSEC, XSMIN1, XSMIN2, XS, YS
        my_real
     .          XP12, YP12, ZP12, XA2P1, YA2P1, ZA2P1
        my_real
     .          XA1A2, YA1A2, ZA1A2, ALPHA1, ALPHA2, NORM

C
        INTEGER, ALLOCATABLE :: NODSEG(:,:)
C
        TOLE2=EPSILON(ZERO)*0.5
        TOLEI=TOLE2*TEN
        TOLE=TOLE2*LMIN
C
C     Initialization to avoid error in debug mode

        IF(NTRI>0) ISEG(1:5,1:5*NTRI) = 0

        A(1)=QUAD(1,1)
        A(2)=QUAD(2,1)
        A(3)=QUAD(3,1)
C Recensement des aretes de triangles
        NEDGE=0
        DO I=1,NTRI
          DO J=1,3
            JJ=J+1
            IF (J==3) JJ=1
            J1=TRI(I,J)
            J2=TRI(I,JJ)
            IEDGE=0
            DO K=1,NEDGE
              K1=EDGE(K,1)
              K2=EDGE(K,2)
              IF ((K1==J1.AND.K2==J2).OR.
     .            (K2==J1.AND.K1==J2)) IEDGE=K
            ENDDO
            IF (IEDGE==0) THEN
              NEDGE=NEDGE+1
              TEDGE(I,J)=NEDGE
              EDGE(NEDGE,1)=J1
              EDGE(NEDGE,2)=J2
            ELSE
              TEDGE(I,J)=IEDGE
            ENDIF
          ENDDO
        ENDDO
C Calcul des intersections des aretes avec le plan de la facette
        DO I=1,NEDGE
          N1=EDGE(I,1)
          N2=EDGE(I,2)
          X1=X(1,N1)
          Y1=X(2,N1)
          Z1=X(3,N1)
          X2=X(1,N2)
          Y2=X(2,N2)
          Z2=X(3,N2)
          SS1=(X1-A(1))*N(1)+(Y1-A(2))*N(2)+(Z1-A(3))*N(3)
          SS2=(X2-A(1))*N(1)+(Y2-A(2))*N(2)+(Z2-A(3))*N(3)
          EDGE(I,3)=0
          IF (ABS(SS1)<=TOLE.OR.ABS(SS2)<=TOLE) THEN
            IF (ABS(SS1)<=TOLE.AND.ABS(SS2)<=TOLE) CYCLE
          ELSEIF (SS1<ZERO.AND.SS2<ZERO) THEN
            CYCLE
          ELSEIF (SS1>=ZERO.AND.SS2>=ZERO) THEN
            CYCLE
          ENDIF
          X12=X2-X1
          Y12=Y2-Y1
          Z12=Z2-Z1
          XA1=X1-A(1)
          YA1=Y1-A(2)
          ZA1=Z1-A(3)
          ALPHA=-(XA1*N(1)+YA1*N(2)+ZA1*N(3))
     .          /(X12*N(1)+Y12*N(2)+Z12*N(3))
          XM=X1+ALPHA*X12
          YM=Y1+ALPHA*Y12
          ZM=Z1+ALPHA*Z12
C
          EDGE(I,3)=1
          XEDGE(I,1)=XM
          XEDGE(I,2)=YM
          XEDGE(I,3)=ZM
          XEDGE(I,4)=SS1*SS2
        ENDDO
C Calcul des intersections des triangles avec le plan de la facette (segments)
        DO I=1,4
          NINTER(I)=0
        ENDDO
        DO I=1,NTRI
          NP=0
          ELSEG(I,1)=I
          ELSEG(I,2)=I
          DO J=1,3
            IEDGE=TEDGE(I,J)
            IF (EDGE(IEDGE,3)==0) CYCLE
            NP=NP+1
            STMP(1,NP)=XEDGE(IEDGE,1)
            STMP(2,NP)=XEDGE(IEDGE,2)
            STMP(3,NP)=XEDGE(IEDGE,3)
            STMP(4,NP)=XEDGE(IEDGE,4)
          ENDDO
          IF (NP==0) THEN
            SEG(I,1,1)=ZERO
            SEG(I,2,1)=ZERO
            SEG(I,3,1)=ZERO
            SEG(I,1,2)=ZERO
            SEG(I,2,2)=ZERO
            SEG(I,3,2)=ZERO
            CYCLE
          ELSEIF (NP==3) THEN
            NP=0
            IDBL=0
            DO J=1,3
              IF (STMP(4,J)<ZERO) THEN
                NP=NP+1
                SEG(I,1,NP)=STMP(1,J)
                SEG(I,2,NP)=STMP(2,J)
                SEG(I,3,NP)=STMP(3,J)
              ELSEIF (STMP(4,J)==ZERO) THEN
                IF (IDBL==0) THEN
                  NP=NP+1
                  SEG(I,1,NP)=STMP(1,J)
                  SEG(I,2,NP)=STMP(2,J)
                  SEG(I,3,NP)=STMP(3,J)
                  IDBL=1
                ENDIF
              ENDIF
            ENDDO
          ELSEIF (NP==2) THEN
            SEG(I,1,1)=STMP(1,1)
            SEG(I,2,1)=STMP(2,1)
            SEG(I,3,1)=STMP(3,1)
            SEG(I,1,2)=STMP(1,2)
            SEG(I,2,2)=STMP(2,2)
            SEG(I,3,2)=STMP(3,2)
          ENDIF
C Intersection (eventuelle) de chaque segment avec les aretes de la facette
          DO J=1,4
            NPI=NINTER(J)
            IF (J/=4) THEN
              JJ=J+1
            ELSE
              JJ=1
            ENDIF
            NX=NORMF(1,J)
            NY=NORMF(2,J)
            NZ=NORMF(3,J)
            X1=SEG(I,1,1)
            Y1=SEG(I,2,1)
            Z1=SEG(I,3,1)
            X2=SEG(I,1,2)
            Y2=SEG(I,2,2)
            Z2=SEG(I,3,2)
            X12=X2-X1
            Y12=Y2-Y1
            Z12=Z2-Z1
            XA1=QUAD(1,J)
            YA1=QUAD(2,J)
            ZA1=QUAD(3,J)
            XA2=QUAD(1,JJ)
            YA2=QUAD(2,JJ)
            ZA2=QUAD(3,JJ)
            XA12=XA2-XA1
            YA12=YA2-YA1
            ZA12=ZA2-ZA1
            XA1P1=X1-XA1
            YA1P1=Y1-YA1
            ZA1P1=Z1-ZA1
            XA1P2=X2-XA1
            YA1P2=Y2-YA1
            ZA1P2=Z2-ZA1
C
            SS1=XA1P1*NX+YA1P1*NY+ZA1P1*NZ
            SS2=XA1P2*NX+YA1P2*NY+ZA1P2*NZ
            IF (SS1>ZERO.AND.SS2>ZERO) THEN
              SEG(I,1,1)=ZERO
              SEG(I,2,1)=ZERO
              SEG(I,3,1)=ZERO
              SEG(I,1,2)=ZERO
              SEG(I,2,2)=ZERO
              SEG(I,3,2)=ZERO
              CYCLE
            ENDIF
C
            SS2=X12*NX+Y12*NY+Z12*NZ
            IF (SS2==ZERO) CYCLE
            ALPHA=-SS1/SS2
            IF (ALPHA<-TOLEI.OR.ALPHA>ONE+TOLEI) CYCLE
            XM=X1+ALPHA*X12
            YM=Y1+ALPHA*Y12
            ZM=Z1+ALPHA*Z12
            BETA=((XM-XA1)*XA12+(YM-YA1)*YA12+(ZM-ZA1)*ZA12)
     .          /(XA12**2+YA12**2+ZA12**2)
            IF (BETA<-TOLEI.OR.BETA>ONE+TOLEI) CYCLE
            SS1=(X1-XA1)*NX+(Y1-YA1)*NY+(Z1-ZA1)*NZ
            SS2=(X2-XA1)*NX+(Y2-YA1)*NY+(Z2-ZA1)*NZ
C
            IF (SS1*SS2>ZERO) THEN
              IF (ABS(SS1)<=ABS(SS2)) THEN
                SEG(I,1,1)=XM
                SEG(I,2,1)=YM
                SEG(I,3,1)=ZM
              ELSE
                SEG(I,1,2)=XM
                SEG(I,2,2)=YM
                SEG(I,3,2)=ZM
              ENDIF
            ELSEIF (SS1*SS2<ZERO) THEN
              IF (SS1<ZERO) THEN
                SEG(I,1,2)=XM
                SEG(I,2,2)=YM
                SEG(I,3,2)=ZM
              ELSEIF (SS2<ZERO) THEN
                SEG(I,1,1)=XM
                SEG(I,2,1)=YM
                SEG(I,3,1)=ZM
              ENDIF
            ELSE
              IF (SS1==ZERO) THEN
                IF (SS2<ZERO) THEN
                  SEG(I,1,1)=XM
                  SEG(I,2,1)=YM
                  SEG(I,3,1)=ZM
                ELSEIF (SS2>=ZERO) THEN
                  SEG(I,1,2)=XM
                  SEG(I,2,2)=YM
                  SEG(I,3,2)=ZM
                ENDIF
              ELSEIF (SS2==ZERO) THEN
                IF (SS1<ZERO) THEN
                  SEG(I,1,2)=XM
                  SEG(I,2,2)=YM
                  SEG(I,3,2)=ZM
                ELSEIF (SS1>=ZERO) THEN
                  SEG(I,1,1)=XM
                  SEG(I,2,1)=YM
                  SEG(I,3,1)=ZM
                ENDIF
              ENDIF
            ENDIF
C
            NPI=NPI+1
            NINTER(J)=NPI
            PINTER(J,NPI,1)=XM
            PINTER(J,NPI,2)=YM
            PINTER(J,NPI,3)=ZM
            ELINTER(J,NPI)=I
            IF(TRI(I,5) == 3) THEN
              PINTER(J,NPI,4)=ZERO
            ELSE
              NX=NORMT(1,I)
              NY=NORMT(2,I)
              NZ=NORMT(3,I)
              SS1=XA12*NX+YA12*NY+ZA12*NZ
              IF (SS1<=ZERO) THEN
                PINTER(J,NPI,4)=ONE
              ELSEIF (SS1>=ZERO) THEN
                PINTER(J,NPI,4)=-ONE
              ENDIF
            ENDIF
          ENDDO  !J=1,4
        ENDDO  !I=1,NTRI
C
        NSEG=NTRI
C
C Intersection eventuelle des segments internes avec les segments externes
        DO I=1,NTRI
          IF(TRI(I,5)/=3) CYCLE
          XA1=SEG(I,1,1)
          YA1=SEG(I,2,1)
          ZA1=SEG(I,3,1)
          XA2=SEG(I,1,2)
          YA2=SEG(I,2,2)
          ZA2=SEG(I,3,2)
          XA12=XA2-XA1
          YA12=YA2-YA1
          ZA12=ZA2-ZA1
          DO J=1,NTRI
            IF(TRI(J,5) == 3) CYCLE
            XP1=SEG(J,1,1)
            YP1=SEG(J,2,1)
            ZP1=SEG(J,3,1)
            XP2=SEG(J,1,2)
            YP2=SEG(J,2,2)
            ZP2=SEG(J,3,2)
            XP12=XP2-XP1
            YP12=YP2-YP1
            ZP12=ZP2-ZP1
C
            XA1P1=XP1-XA1
            YA1P1=YP1-YA1
            ZA1P1=ZP1-ZA1
            X12=YA12*ZA1P1-ZA12*YA1P1
            Y12=ZA12*XA1P1-XA12*ZA1P1
            Z12=XA12*YA1P1-YA12*XA1P1
            SS1=X12*N(1)+Y12*N(2)+Z12*N(3)
C
            XA1P2=XP2-XA1
            YA1P2=YP2-YA1
            ZA1P2=ZP2-ZA1
            X12=YA12*ZA1P2-ZA12*YA1P2
            Y12=ZA12*XA1P2-XA12*ZA1P2
            Z12=XA12*YA1P2-YA12*XA1P2
            SS2=X12*N(1)+Y12*N(2)+Z12*N(3)
C
            IF(SS1*SS2 > ZERO) CYCLE
            X12=YP12*ZA1P1-ZP12*YA1P1
            Y12=ZP12*XA1P1-XP12*ZA1P1
            Z12=XP12*YA1P1-YP12*XA1P1
            SS1=X12*N(1)+Y12*N(2)+Z12*N(3)
C
            XA2P1=XP1-XA2
            YA2P1=YP1-YA2
            ZA2P1=ZP1-ZA2
            X12=YP12*ZA2P1-ZP12*YA2P1
            Y12=ZP12*XA2P1-XP12*ZA2P1
            Z12=XP12*YA2P1-YP12*XA2P1
            SS2=X12*N(1)+Y12*N(2)+Z12*N(3)
            IF(SS1*SS2 > ZERO) CYCLE
            IF(SS1 == ZERO .AND. SS2 /= ZERO) THEN
              XM=XA1
              YM=YA1
              ZM=ZA1
            ELSEIF(SS2 == ZERO .AND. SS1 /= ZERO) THEN
              XM=XA2
              YM=YA2
              ZM=ZA2
            ELSE
C SI SS1=SS2=0 les segments sont colineaires (cas non traite)
              CYCLE
            ENDIF
            SEG(J,1,2)=XM
            SEG(J,2,2)=YM
            SEG(J,3,2)=ZM
            NSEG=NSEG+1
            SEG(NSEG,1,1)=XM
            SEG(NSEG,2,1)=YM
            SEG(NSEG,3,1)=ZM
            ELSEG(NSEG,1)=J
            SEG(NSEG,1,2)=XP2
            SEG(NSEG,2,2)=YP2
            SEG(NSEG,3,2)=ZP2
            ELSEG(NSEG,2)=J
          ENDDO
        ENDDO
C
C Segments additionels sur aretes
        DO I=1,4
          IF (I/=4) THEN
            II=I+1
          ELSE
            II=1
          ENDIF
          XA1=QUAD(1,I)
          YA1=QUAD(2,I)
          ZA1=QUAD(3,I)
          XA2=QUAD(1,II)
          YA2=QUAD(2,II)
          ZA2=QUAD(3,II)
          XA12=XA2-XA1
          YA12=YA2-YA1
          ZA12=ZA2-ZA1
          IF (NINTER(I)==0) CYCLE
C Tri des points sur les bords de l'arete
          NPI=NINTER(I)
          DO J=1,NPI
            XM=PINTER(I,J,1)
            YM=PINTER(I,J,2)
            ZM=PINTER(I,J,3)
            XL(J)=((XM-XA1)*XA12+(YM-YA1)*YA12+(ZM-ZA1)*ZA12)
     .           /(XA12**2+YA12**2+ZA12**2)
          ENDDO
          DO J=1,NPI
            REDIR(J)=J
          ENDDO
          DO J=1,NPI
            XLREF=XL(REDIR(J))
            DO K=J+1,NPI
              XLC=XL(REDIR(K))
              IF (XLC<=XLREF) THEN
                KK=REDIR(K)
                REDIR(K)=REDIR(J)
                REDIR(J)=KK
                XLREF=XLC
              ENDIF
            ENDDO
          ENDDO
C Identification des doubles
          DO J=1,NPI
            JJ=REDIR(J)
            IF (JJ>0) THEN
              X1=PINTER(I,JJ,1)
              Y1=PINTER(I,JJ,2)
              Z1=PINTER(I,JJ,3)
              T1=PINTER(I,JJ,4)
              DO K=J+1,NPI
                KK=REDIR(K)
                IF (KK>0) THEN
                  X2=PINTER(I,KK,1)
                  Y2=PINTER(I,KK,2)
                  Z2=PINTER(I,KK,3)
                  T2=PINTER(I,KK,4)
                  DD=(X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2+(T2-T1)**2
                  IF (DD<=TOLE) REDIR(K)=0
                ENDIF
              ENDDO
            ENDIF
          ENDDO
C Elimination des segments de longueur nulle
          DO J=1,NPI
            JJ=REDIR(J)
            IF (JJ>0) THEN
              X1=PINTER(I,JJ,1)
              Y1=PINTER(I,JJ,2)
              Z1=PINTER(I,JJ,3)
              DO K=J+1,NPI
                KK=REDIR(K)
                IF (KK>0) THEN
                  X2=PINTER(I,KK,1)
                  Y2=PINTER(I,KK,2)
                  Z2=PINTER(I,KK,3)
                  DD=(X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2
                  IF (DD<=TOLE) THEN
                    REDIR(J)=0
                    REDIR(K)=0
c                       print *,'on ne doit pas passer la non plus...',
c     +                 dd,tolem,i,j,k,JJ,KK
c                       call flush(6)
c                       stop
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ENDDO
C Condensation du tableau REDIR
          DO J=1,NPI
            REDIR_TMP(J)=REDIR(J)
          ENDDO
          NPI_TMP=NPI
          NPI=0
          DO J=1,NPI_TMP
            IF (REDIR_TMP(J)>0) THEN
              NPI=NPI+1
              REDIR(NPI)=REDIR_TMP(J)
            ENDIF
          ENDDO
          IF (NPI==0) CYCLE
C Creation des segments sur les aretes
          IF (PINTER(I,REDIR(1),4)==-ONE) THEN
            NSEG=NSEG+1
            SEG(NSEG,1,1)=XA1
            SEG(NSEG,2,1)=YA1
            SEG(NSEG,3,1)=ZA1
            ELSEG(NSEG,1)=0
            SEG(NSEG,1,2)=PINTER(I,REDIR(1),1)
            SEG(NSEG,2,2)=PINTER(I,REDIR(1),2)
            SEG(NSEG,3,2)=PINTER(I,REDIR(1),3)
            ELSEG(NSEG,2)=ELINTER(I,REDIR(1))
          ENDIF
          DO J=1,NPI-1
            JJ=REDIR(J)
            JJ1=REDIR(J+1)
            IF (PINTER(I,JJ,4)==-ONE) CYCLE
            XP1=PINTER(I,JJ,1)
            YP1=PINTER(I,JJ,2)
            ZP1=PINTER(I,JJ,3)
            XP2=PINTER(I,JJ1,1)
            YP2=PINTER(I,JJ1,2)
            ZP2=PINTER(I,JJ1,3)
            NSEG=NSEG+1
            SEG(NSEG,1,1)=XP1
            SEG(NSEG,2,1)=YP1
            SEG(NSEG,3,1)=ZP1
            ELSEG(NSEG,1)=ELINTER(I,JJ)
            SEG(NSEG,1,2)=XP2
            SEG(NSEG,2,2)=YP2
            SEG(NSEG,3,2)=ZP2
            ELSEG(NSEG,2)=ELINTER(I,JJ1)
          ENDDO
          IF (PINTER(I,REDIR(NPI),4)==ONE) THEN
            NSEG=NSEG+1
            SEG(NSEG,1,1)=PINTER(I,REDIR(NPI),1)
            SEG(NSEG,2,1)=PINTER(I,REDIR(NPI),2)
            SEG(NSEG,3,1)=PINTER(I,REDIR(NPI),3)
            ELSEG(NSEG,1)=ELINTER(I,REDIR(NPI))
            SEG(NSEG,1,2)=XA2
            SEG(NSEG,2,2)=YA2
            SEG(NSEG,3,2)=ZA2
            ELSEG(NSEG,2)=0
          ENDIF
        ENDDO
C Recensement des noeuds
        NNO=0
        ALGO=0
        DO I=1,NSEG
          X1=SEG(I,1,1)
          Y1=SEG(I,2,1)
          Z1=SEG(I,3,1)
          X2=SEG(I,1,2)
          Y2=SEG(I,2,2)
          Z2=SEG(I,3,2)
          IF (X1==ZERO.AND.Y1==ZERO.AND.Z1==ZERO.AND.
     .        X2==ZERO.AND.Y2==ZERO.AND.Z2==ZERO) THEN
            ISEG(1,I)=0
            ISEG(2,I)=0
            ISEG(3,I)=1
            ISEG(4,I)=0
            ISEG(5,I)=0
            CYCLE
          ENDIF
          ID1=0
          ID2=0
          DO J=1,NNO
            XX1=NODE(1,J)
            YY1=NODE(2,J)
            ZZ1=NODE(3,J)
            DD1=(XX1-X1)**2+(YY1-Y1)**2+(ZZ1-Z1)**2
            DD2=(XX1-X2)**2+(YY1-Y2)**2+(ZZ1-Z2)**2
            IF (DD1<=TOLE) ID1=J
            IF (DD2<=TOLE) ID2=J
          ENDDO
          LL=(X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2
          IF (ID1==0) THEN
            NNO=NNO+1
            NODE(1,NNO)=X1
            NODE(2,NNO)=Y1
            NODE(3,NNO)=Z1
            ELNODE(NNO)=ELSEG(I,1)
            ID1=NNO
          ENDIF
          IF (LL<=TOLE) ID2=ID1
          IF (ID2==0) THEN
            NNO=NNO+1
            NODE(1,NNO)=X2
            NODE(2,NNO)=Y2
            NODE(3,NNO)=Z2
            ELNODE(NNO)=ELSEG(I,2)
            ID2=NNO
          ENDIF
          ISEG(1,I)=ID1
          ISEG(2,I)=ID2
          ISEG(3,I)=0
          IF (ID1==ID2) ISEG(3,I)=1
          IF(ELSEG(I,1) == ELSEG(I,2)) THEN
            IF(TRI(ELSEG(I,1),5) == 3) THEN
C Segment sur triangle interne
              ISEG(4,I)=2
              ISEG(5,I)=1
              ALGO=1
            ELSE
C Segment sur triangle airbag ou brique additionelle
              ISEG(4,I)=1
              ISEG(5,I)=0
            ENDIF
          ELSE
C Segment sur arete de boite
            ISEG(4,I)=1
            ISEG(5,I)=0
          ENDIF
        ENDDO
C
C Elimination des segments doubles
        DO I=1,NSEG
          IF (ISEG(3,I)==1) CYCLE
          ID1=ISEG(1,I)
          ID2=ISEG(2,I)
          DO J=I+1,NSEG
            IF ((ISEG(1,J)==ID1.AND.ISEG(2,J)==ID2).OR.
     .          (ISEG(1,J)==ID2.AND.ISEG(2,J)==ID1)) ISEG(3,J)=1
          ENDDO
        ENDDO
C
        NSEGF=0
        DO I=1,NSEG
          IF (ISEG(3,I)==0) NSEGF=NSEGF+1
        ENDDO
        IF (NSEGF<=1) RETURN
C
        IF(ALGO == 0) THEN
C-----------------------------
C ALGORITHME V12
C Reconstruction des polygones
C-----------------------------
          ISTOP=0
          NPOLY=0
          NP=0
          ISSEGOLD=0
C
          DO I=1,NSEG
            ISEG3_OLD(I)=ISEG(3,I)
          ENDDO
C
          DO WHILE (ISTOP==0)
            I=1
            IF(I <= NSEG) THEN
              DO WHILE (ISEG(3,I)==1.AND.I<=NSEG)
                I=I+1
              ENDDO
            ENDIF
            IF (I==NSEG+1) THEN
              ISTOP=1
              CYCLE
            ENDIF
            ISSEG=I
            IPSEG=2
            DO J=1,NNO
              ITAG(J)=0
            ENDDO
            DO J=1,NSEG
              ITAGSEG(J)=0
            ENDDO
            I0=1
            ITAG(ISEG(1,ISSEG))=I0
            ITAGSEG(ISSEG)=I0
            ICLOSE=0
            NOK = 0
            DO WHILE (ICLOSE==0.AND.I<NSEG)
              I=I+1
              INEXT=0
              DO J=1,NSEG
                IF (J==ISSEG.OR.ISEG(3,J)==1.OR.INEXT/=0) CYCLE
                ID1=ISEG(1,J)
                ID2=ISEG(2,J)
                IF (ID1==ISEG(IPSEG,ISSEG)) THEN
                  INEXT=1
                  ISSEG=J
                  IPSEG=2
                  I0=I0+1
                  ITAG(ISEG(1,ISSEG))=I0
                  ITAGSEG(J)=I0
                ELSEIF (ID2==ISEG(IPSEG,ISSEG)) THEN
                  INEXT=1
                  ISSEG=J
                  IPSEG=1
                  I0=I0+1
                  ITAG(ISEG(2,ISSEG))=I0
                  ITAGSEG(J)=-I0
                ENDIF
              ENDDO
              IF (INEXT==0) THEN
                ISEG(3,ISSEG)=1
                I=NSEG
                NOK = 1
              ELSE
                NN=ITAG(ISEG(IPSEG,ISSEG))
                IF (NN/=0) ICLOSE=NN
              ENDIF
            ENDDO
            IF (ICLOSE/=0) THEN
              NPOLY=NPOLY+1
              IADPOLY(NPOLY)=NP
              IAD=NP
              DO J=1,NSEG
                JJ=ITAGSEG(J)
                IF (ABS(JJ)>=ICLOSE) THEN
                  NP=NP+1
                  POLY(1,IAD+ABS(JJ)-ICLOSE+1)=J
                  IF (JJ>0) THEN
                    POLY(2,IAD+ABS(JJ)-ICLOSE+1)=2
                  ELSEIF (JJ<0) THEN
                    POLY(2,IAD+ABS(JJ)-ICLOSE+1)=1
                  ENDIF
                  ISEG(3,J)=1
                ENDIF
              ENDDO
              LENPOLY(NPOLY)=NP-IADPOLY(NPOLY)
            ELSEIF(NOK==0.AND.ISSEGOLD==ISSEG)THEN
              ISEG(3,ISSEG)=1
c            print *,'cycle corrige !!!',ISSEG,i,NSEG
c            stop 204
            ENDIF
            ISSEGOLD=ISSEG
C
            IDIFF=0
            DO J=1,NSEG
              IDIFF=MAX(IDIFF,ABS(ISEG3_OLD(J)-ISEG(3,J)))
            ENDDO
            IF (IDIFF==0) THEN
              WRITE(ISTDO,*)
              WRITE(ISTDO,'(A25,I8,A25)')
     .        ' ** MONITORED VOLUME ID: ',MVID,' - INFINITE LOOP DETECTED'
              WRITE(IOUT,'(A25,I8,A25)')
     .        ' ** MONITORED VOLUME ID: ',MVID,' - INFINITE LOOP DETECTED'
              IF (ILVOUT >=1) WRITE(IOUT,'(A26,I8,A16,I8)')
     .        '    CUTTING BRICK NUMBER: ',NB,' - FACE NUMBER: ',IFAC
              CALL ARRET(2)
            ENDIF
          ENDDO  ! DO WHILE (ISTOP==0)
C
          INFO=0
          IF (NPOLY>NPOLMAX) THEN
            NPOLMAX=NPOLY
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
            IF(ILVOUT >=1) WRITE(IOUT,'(A,I10,A,I10)')
     .        ' MONITORED VOLUME ID: ',MVID,' NLAYER  IS RESET TO ',NPOLY
            INFO=1
          ENDIF
          LENMAX=0
          DO I=1,NPOLY
            LENMAX=MAX(LENMAX,LENPOLY(I))
          ENDDO
          IF (LENMAX>NPPMAX) THEN
            NPPMAX=LENMAX
            IF(ILVOUT >=1) WRITE(IOUT,'(A,I10,A,I10)')
     .        ' MONITORED VOLUME ID: ',MVID,' NPPMAX  IS RESET TO ',LENMAX
            INFO=1
          ENDIF
          IF (INFO==1) RETURN
          NNP=NPOLY
C
        ELSE
C-------------------------
C ALGORITME V14
C-------------------------
C Tableau noeuds-segments
C-------------------------
          ALLOCATE (NODSEG(NNO,5))
          DO I=1,NNO
            DO J=1,5
              NODSEG(I,J)=0
            ENDDO
          ENDDO
          DO I=1,NSEG
            IF(ISEG(3,I) == 1) CYCLE
            DO J=1,2
              K=ISEG(J,I)
              M=NODSEG(K,1)
              IF( M <= 3 ) THEN
                NODSEG(K,1)=M+1
                NODSEG(K,M+2)=I
              ELSE
                WRITE(IOUT,'(A,I10,2A,3E12.4,A1)')
     .          ' ** MONITORED VOLUME ID: ',MVID,' MORE THAN 4 SEGMENTS',
     .          ' CONNECTED TO NODE [',NODE(1,K),NODE(2,K),NODE(3,K),']'
                CALL ARRET(2)
              ENDIF
            ENDDO
          ENDDO
C-------------------------------------------
C Tri des noeuds
C     4 segments :  2 internes + 2 externes
C     3 segments :  3 internes
C     3 segments :  1 interne  + 2 externes
C Test et mise des segments internes en tete
C-------------------------------------------
          DO I=1,NNO
            IF(NODSEG(I,1) /= 4) CYCLE
            DO K=1,4
              JSEG(K)=NODSEG(I,K+1)
            ENDDO
            L=0
            DO K=1,4
              IF(ISEG(5,JSEG(K)) == 0) CYCLE
              L=L+1
              IADR(L)=K
            ENDDO
            IF(L /= 2) THEN
              WRITE(IOUT,'(A,I10,A,3E12.4,2A,I2,A)')
     .        ' ** FVMBAG ID: ',MVID,' ERROR IN POLYGON BUILDING : NODE [',
     .         NODE(1,I),NODE(2,I),NODE(3,I),'] IS CONNECTED TO 4',
     .        ' EDGES',L,' BEING INTERNAL EDGES'
            ENDIF
            DO K=1,4
              IF(ISEG(5,JSEG(K)) == 1) CYCLE
              L=L+1
              IADR(L)=K
            ENDDO
            DO K=1,4
              NODSEG(I,K+1)=JSEG(IADR(K))
            ENDDO
          ENDDO
C
          DO I=1,NNO
            IF(NODSEG(I,1) <= 1) THEN
              WRITE(IOUT,'(A,I10,A,3E12.4,A,I2,A)')
     .        ' ** FVMBAG ID ',MVID,' ERROR IN POLYGON BUILDING : NODE [',
     .        NODE(1,I),NODE(2,I),NODE(3,I),'] IS CONNECTED TO ',
     .        NODSEG(I,1),' EDGE'
CFA            CALL ARRET(2)
            ENDIF
            IF(NODSEG(I,1) /= 2) CYCLE
            L=0
            IF(ISEG(5,NODSEG(I,2)) == 1) L=L+1
            IF(ISEG(5,NODSEG(I,3)) == 1) L=L+1
            IF(L == 1) THEN
              WRITE(IOUT,'(A,I10,A,3E12.4,2A)')
     .        ' ** FVMBAG ID ',MVID,' ERROR IN POLYGON BUILDING : NODE [',
     .        NODE(1,I),NODE(2,I),NODE(3,I),'] IS CONNECTED TO 2',
     .        ' EDGES : 1 INTERNAL AND 1 EXTERNAL'
CFA            CALL ARRET(2)
            ENDIF
          ENDDO
C
          DO I=1,NNO
            IF(NODSEG(I,1) /= 3) CYCLE
            DO K=1,3
              JSEG(K)=NODSEG(I,K+1)
            ENDDO
            L=0
            DO K=1,3
              IF(ISEG(5,JSEG(K)) == 0) CYCLE
              L=L+1
              IADR(L)=K
            ENDDO
            IF(L == 2) THEN
              WRITE(IOUT,'(A,I10,A,3E12.4,2A)')
     .        ' ** FVMBAG ID: ',MVID,' ERROR IN POLYGON BUILDING : NODE [',
     .        NODE(1,I),NODE(2,I),NODE(3,I),'] IS CONNECTED TO 3',
     .        ' EDGES 2 BEING INTERNAL EDGES'
CFA            CALL ARRET(2)
            ELSEIF(L == 1) THEN
              DO K=1,3
                IF(ISEG(5,JSEG(K)) == 1) CYCLE
                L=L+1
                IADR(L)=K
              ENDDO
            ELSEIF(L == 3) THEN
              ID1=ISEG(1,JSEG(1))
              ID2=ISEG(2,JSEG(1))
              IF(ID1==I) L=ID2
              IF(ID2==I) L=ID1
              XA1=NODE(1,I)
              YA1=NODE(2,I)
              ZA1=NODE(3,I)
              XA2=NODE(1,L)
              YA2=NODE(2,L)
              ZA2=NODE(3,L)
              XA1A2=XA2-XA1
              YA1A2=YA2-YA1
              ZA1A2=ZA2-ZA1
              NORM=SQRT(XA1A2*XA1A2+YA1A2*YA1A2+ZA1A2*ZA1A2)
              XA1A2=XA1A2/NORM
              YA1A2=YA1A2/NORM
              ZA1A2=ZA1A2/NORM
C
              ID1=ISEG(1,JSEG(2))
              ID2=ISEG(2,JSEG(2))
              IF(ID1==I) L=ID2
              IF(ID2==I) L=ID1
              XP1=NODE(1,L)
              YP1=NODE(2,L)
              ZP1=NODE(3,L)
              XA1P1=XP1-XA1
              YA1P1=YP1-YA1
              ZA1P1=ZP1-ZA1
              NORM=SQRT(XA1P1*XA1P1+YA1P1*YA1P1+ZA1P1*ZA1P1)
              XA1P1=XA1P1/NORM
              YA1P1=YA1P1/NORM
              ZA1P1=ZA1P1/NORM
              SS=XA1A2*XA1P1+YA1A2*YA1P1+ZA1A2*ZA1P1
              IF(SS < -ONE) SS=-ONE
              IF(SS >  ONE) SS= ONE
              ALPHA1=ACOS(SS)
C
              ID1=ISEG(1,JSEG(3))
              ID2=ISEG(2,JSEG(3))
              IF(ID1==I) L=ID2
              IF(ID2==I) L=ID1
              XP1=NODE(1,L)
              YP1=NODE(2,L)
              ZP1=NODE(3,L)
              XA1P1=XP1-XA1
              YA1P1=YP1-YA1
              ZA1P1=ZP1-ZA1
              NORM=SQRT(XA1P1*XA1P1+YA1P1*YA1P1+ZA1P1*ZA1P1)
              XA1P1=XA1P1/NORM
              YA1P1=YA1P1/NORM
              ZA1P1=ZA1P1/NORM
              SS=XA1A2*XA1P1+YA1A2*YA1P1+ZA1A2*ZA1P1
              IF(SS < -ONE) SS=-ONE
              IF(SS >  ONE) SS= ONE
              ALPHA2=ACOS(SS)
C
              IF((ALPHA1 < ZERO .AND. ALPHA2 > ZERO).OR.
     .           (ALPHA2 < ZERO .AND. ALPHA1 > ZERO)) THEN
                IADR(1)=1
                IADR(2)=2
                IADR(3)=3
              ELSEIF(ABS(ALPHA1) < ABS(ALPHA2)) THEN
                IADR(1)=2
                IADR(2)=1
                IADR(3)=3
              ELSEIF(ABS(ALPHA1) > ABS(ALPHA2)) THEN
                IADR(1)=3
                IADR(2)=1
                IADR(3)=2
              ENDIF
            ENDIF
C
            DO K=1,3
              NODSEG(I,K+1)=JSEG(IADR(K))
            ENDDO
          ENDDO ! I=1,NNO
C---------------------------------------------------------------------------
C Construction des polygones (nouvel algorithme v14)
C   JTAG(1,i) le polygone interne est du cote oppose a la normale au segment
C   JTAG(2,i) le polygone interne est du cote de la normale au segment
C---------------------------------------------------------------------------
          DO I=1,NSEG
            JTAG(1,I)=0
            JTAG(2,I)=0
          ENDDO
          ICMAX=0
C-----------------------------
C Noeud connecte a 4 segments
C-----------------------------
          DO I=1,NNO
            IF(NODSEG(I,1) /= 4) CYCLE
            I1=NODSEG(I,2)
            I2=NODSEG(I,3)
            I3=NODSEG(I,4)
            I4=NODSEG(I,5)
C Traitement des 2 segments internes
            IF(JTAG(1,I1)==0.AND.JTAG(1,I2)==0) THEN
              ICMAX=ICMAX+1
              JTAG(1,I1)=ICMAX
              JTAG(1,I2)=ICMAX
            ELSEIF(JTAG(1,I1)/=0.AND.JTAG(1,I2)==0) THEN
              JTAG(1,I2)=JTAG(1,I1)
            ELSEIF(JTAG(1,I1)==0.AND.JTAG(1,I2)/=0) THEN
              JTAG(1,I1)=JTAG(1,I2)
            ELSEIF(JTAG(1,I1) /= JTAG(1,I2)) THEN
              IMIN=MIN(JTAG(1,I1),JTAG(1,I2))
              IMAX=MAX(JTAG(1,I1),JTAG(1,I2))
              DO J=1,NSEG
                IF(JTAG(1,J) == IMAX) JTAG(1,J)=IMIN
                IF(JTAG(2,J) == IMAX) JTAG(2,J)=IMIN
              ENDDO
            ENDIF
C Traitement des 2 segments externes
            ID1=ISEG(1,I3)
            ID2=ISEG(2,I3)
            IF(ID1==I) L=ID2
            IF(ID2==I) L=ID1
            XA1=NODE(1,I)
            YA1=NODE(2,I)
            ZA1=NODE(3,I)
            XA2=NODE(1,L)
            YA2=NODE(2,L)
            ZA2=NODE(3,L)
            XA1A2=XA2-XA1
            YA1A2=YA2-YA1
            ZA1A2=ZA2-ZA1
            NORM=SQRT(XA1A2*XA1A2+YA1A2*YA1A2+ZA1A2*ZA1A2)
            XA1A2=XA1A2/NORM
            YA1A2=YA1A2/NORM
            ZA1A2=ZA1A2/NORM
C
            ID1=ISEG(1,I1)
            ID2=ISEG(2,I1)
            IF(ID1==I) L=ID2
            IF(ID2==I) L=ID1
            XP1=NODE(1,L)
            YP1=NODE(2,L)
            ZP1=NODE(3,L)
            XA1P1=XP1-XA1
            YA1P1=YP1-YA1
            ZA1P1=ZP1-ZA1
            NORM=SQRT(XA1P1*XA1P1+YA1P1*YA1P1+ZA1P1*ZA1P1)
            XA1P1=XA1P1/NORM
            YA1P1=YA1P1/NORM
            ZA1P1=ZA1P1/NORM
            SS=XA1A2*XA1P1+YA1A2*YA1P1+ZA1A2*ZA1P1
            IF(SS < -ONE) SS=-ONE
            IF(SS >  ONE) SS= ONE
            ALPHA1=ACOS(SS)
C
            ID1=ISEG(1,I2)
            ID2=ISEG(2,I2)
            IF(ID1==I) L=ID2
            IF(ID2==I) L=ID1
            XP1=NODE(1,L)
            YP1=NODE(2,L)
            ZP1=NODE(3,L)
            XA1P1=XP1-XA1
            YA1P1=YP1-YA1
            ZA1P1=ZP1-ZA1
            NORM=SQRT(XA1P1*XA1P1+YA1P1*YA1P1+ZA1P1*ZA1P1)
            XA1P1=XA1P1/NORM
            YA1P1=YA1P1/NORM
            ZA1P1=ZA1P1/NORM
            SS=XA1A2*XA1P1+YA1A2*YA1P1+ZA1A2*ZA1P1
            IF(SS < -ONE) SS=-ONE
            IF(SS >  ONE) SS= ONE
            ALPHA2=ACOS(SS)
C
            JSEG(1)=I3
            JSEG(2)=I4
            IF(ALPHA1 < ALPHA2) THEN
              JSEG(3)=I1
              JSEG(4)=I2
            ELSE
              JSEG(3)=I2
              JSEG(4)=I1
            ENDIF
C
            DO M=1,2
              I1=JSEG(M)
              I2=JSEG(M+2)
              IF(JTAG(1,I1)==0.AND.JTAG(2,I2)==0) THEN
                ICMAX=ICMAX+1
                JTAG(1,I1)=ICMAX
                JTAG(2,I2)=ICMAX
              ELSEIF(JTAG(1,I1)/=0.AND.JTAG(2,I2)==0) THEN
                JTAG(2,I2)=JTAG(1,I1)
              ELSEIF(JTAG(1,I1)==0.AND.JTAG(2,I2)/=0) THEN
                JTAG(1,I1)=JTAG(2,I2)
              ELSEIF(JTAG(1,I1) /= JTAG(2,I2)) THEN
                IMIN=MIN(JTAG(1,I1),JTAG(2,I2))
                IMAX=MAX(JTAG(1,I1),JTAG(2,I2))
                DO J=1,NSEG
                  IF(JTAG(1,J) == IMAX) JTAG(1,J)=IMIN
                  IF(JTAG(2,J) == IMAX) JTAG(2,J)=IMIN
                ENDDO
              ENDIF
            ENDDO
          ENDDO
C------------------------------------------------------
C Noeud connecte a 3 segments : 1 interne et 2 externes
C------------------------------------------------------
          DO I=1,NNO
            IF(NODSEG(I,1) /= 3) CYCLE
            JSEG(1)=NODSEG(I,2)
            JSEG(2)=NODSEG(I,3)
            IF(ISEG(5,JSEG(2)) == 1) CYCLE
            JSEG(3)=NODSEG(I,4)
            IF(ISEG(5,JSEG(3)) == 1) CYCLE
C
C Normale au segment interne
            I1=JSEG(1)
            NN=ELSEG(I1,1)
            NX=NORMT(1,NN)
            NY=NORMT(2,NN)
            NZ=NORMT(3,NN)
            XX=NY*N(3)-NZ*N(2)
            YY=NZ*N(1)-NX*N(3)
            ZZ=NX*N(2)-NY*N(1)
            NX=N(2)*ZZ-N(3)*YY
            NY=N(3)*XX-N(1)*ZZ
            NZ=N(1)*YY-N(2)*XX
C
            ID1=ISEG(1,I1)
            ID2=ISEG(2,I1)
            IF(ID1==I) L=ID2
            IF(ID2==I) L=ID1
            XA1=NODE(1,L)
            YA1=NODE(2,L)
            ZA1=NODE(3,L)
C
            I2=JSEG(2)
            ID1=ISEG(1,I2)
            ID2=ISEG(2,I2)
            IF(ID1==I) L=ID2
            IF(ID2==I) L=ID1
            XP1=NODE(1,L)
            YP1=NODE(2,L)
            ZP1=NODE(3,L)
            XA1P1=XP1-XA1
            YA1P1=YP1-YA1
            ZA1P1=ZP1-ZA1
            NORM=SQRT(XA1P1*XA1P1+YA1P1*YA1P1+ZA1P1*ZA1P1)
            XA1P1=XA1P1/NORM
            YA1P1=YA1P1/NORM
            ZA1P1=ZA1P1/NORM
            SS=NX*XA1P1+NY*YA1P1+NZ*ZA1P1
            IF(SS < -ONE) SS=-ONE
            IF(SS >  ONE) SS= ONE
            ALPHA1=ACOS(SS)
C
            I3=JSEG(3)
            ID1=ISEG(1,I3)
            ID2=ISEG(2,I3)
            IF(ID1==I) L=ID2
            IF(ID2==I) L=ID1
            XP1=NODE(1,L)
            YP1=NODE(2,L)
            ZP1=NODE(3,L)
            XA1P1=XP1-XA1
            YA1P1=YP1-YA1
            ZA1P1=ZP1-ZA1
            NORM=SQRT(XA1P1*XA1P1+YA1P1*YA1P1+ZA1P1*ZA1P1)
            XA1P1=XA1P1/NORM
            YA1P1=YA1P1/NORM
            ZA1P1=ZA1P1/NORM
            SS=NX*XA1P1+NY*YA1P1+NZ*ZA1P1
            IF(SS < -ONE) SS=-ONE
            IF(SS >  ONE) SS= ONE
            ALPHA2=ACOS(SS)
C
C Le cas peu probable alpha1=alpha2 est a completer
C
            IF(ALPHA1 < ALPHA2) THEN
              IADR(1)=2
              IADR(2)=1
            ELSE
              IADR(1)=1
              IADR(2)=2
            ENDIF
C
            DO M=1,2
              I1=JSEG(1)
              I2=JSEG(M+1)
              L =IADR(M)
              IF(JTAG(L,I1)==0.AND.JTAG(1,I2)==0) THEN
                ICMAX=ICMAX+1
                JTAG(L,I1)=ICMAX
                JTAG(1,I2)=ICMAX
              ELSEIF(JTAG(L,I1)/=0.AND.JTAG(1,I2)==0) THEN
                JTAG(1,I2)=JTAG(L,I1)
              ELSEIF(JTAG(L,I1)==0.AND.JTAG(1,I2)/=0) THEN
                JTAG(L,I1)=JTAG(1,I2)
              ELSEIF(JTAG(L,I1) /= JTAG(1,I2)) THEN
                IMIN=MIN(JTAG(L,I1),JTAG(1,I2))
                IMAX=MAX(JTAG(L,I1),JTAG(1,I2))
                DO J=1,NSEG
                  IF(JTAG(1,J) == IMAX) JTAG(1,J)=IMIN
                  IF(JTAG(2,J) == IMAX) JTAG(2,J)=IMIN
                ENDDO
              ENDIF
            ENDDO
          ENDDO
C-------------------------------------
C Noeud connecte a 3 segments internes
C-------------------------------------
          DO I=1,NNO
            IF(NODSEG(I,1) /= 3) CYCLE
            JSEG(1)=NODSEG(I,2)
            IF(ISEG(5,JSEG(1)) == 0) CYCLE
            JSEG(2)=NODSEG(I,3)
            IF(ISEG(5,JSEG(2)) == 0) CYCLE
            JSEG(3)=NODSEG(I,4)
            IF(ISEG(5,JSEG(3)) == 0) CYCLE
C
C Normale au 1er segment interne
            I1=JSEG(1)
            NN=ELSEG(I1,1)
            NX=NORMT(1,NN)
            NY=NORMT(2,NN)
            NZ=NORMT(3,NN)
            XX=NY*N(3)-NZ*N(2)
            YY=NZ*N(1)-NX*N(3)
            ZZ=NX*N(2)-NY*N(1)
            NX=N(2)*ZZ-N(3)*YY
            NY=N(3)*XX-N(1)*ZZ
            NZ=N(1)*YY-N(2)*XX
C
            ID1=ISEG(1,I1)
            ID2=ISEG(2,I1)
            IF(ID1==I) L=ID2
            IF(ID2==I) L=ID1
            XA1=NODE(1,L)
            YA1=NODE(2,L)
            ZA1=NODE(3,L)
C
            I2=JSEG(2)
            ID1=ISEG(1,I2)
            ID2=ISEG(2,I2)
            IF(ID1==I) L=ID2
            IF(ID2==I) L=ID1
            XP1=NODE(1,L)
            YP1=NODE(2,L)
            ZP1=NODE(3,L)
            XA1P1=XP1-XA1
            YA1P1=YP1-YA1
            ZA1P1=ZP1-ZA1
            NORM=SQRT(XA1P1*XA1P1+YA1P1*YA1P1+ZA1P1*ZA1P1)
            XA1P1=XA1P1/NORM
            YA1P1=YA1P1/NORM
            ZA1P1=ZA1P1/NORM
            SS=NX*XA1P1+NY*YA1P1+NZ*ZA1P1
            IF(SS < -ONE) SS=-ONE
            IF(SS >  ONE) SS= ONE
            ALPHA=ACOS(SS)
            IF(ABS(ALPHA) < HALF*PI) THEN
              IADR(1)=1
              IADR(2)=2
C              IADR(1)=2
C              IADR(2)=1
            ELSE
              IADR(1)=2
              IADR(2)=1
C              IADR(1)=1
C              IADR(2)=2
            ENDIF
C
            DO M=1,2
              I1=JSEG(1)
              I2=JSEG(M+1)
              L =IADR(M)
              IF(JTAG(L,I1)==0.AND.JTAG(2,I2)==0) THEN
                ICMAX=ICMAX+1
                JTAG(L,I1)=ICMAX
                JTAG(2,I2)=ICMAX
              ELSEIF(JTAG(L,I1)/=0.AND.JTAG(2,I2)==0) THEN
                JTAG(2,I2)=JTAG(L,I1)
              ELSEIF(JTAG(L,I1)==0.AND.JTAG(2,I2)/=0) THEN
                JTAG(L,I1)=JTAG(2,I2)
              ELSEIF(JTAG(L,I1) /= JTAG(2,I2)) THEN
                IMIN=MIN(JTAG(L,I1),JTAG(2,I2))
                IMAX=MAX(JTAG(L,I1),JTAG(2,I2))
                DO J=1,NSEG
                  IF(JTAG(1,J) == IMAX) JTAG(1,J)=IMIN
                  IF(JTAG(2,J) == IMAX) JTAG(2,J)=IMIN
                ENDDO
              ENDIF
            ENDDO
C
            I1=JSEG(2)
            I2=JSEG(3)
            IF(JTAG(1,I1)==0.AND.JTAG(1,I2)==0) THEN
              ICMAX=ICMAX+1
              JTAG(1,I1)=ICMAX
              JTAG(1,I2)=ICMAX
            ELSEIF(JTAG(1,I1)/=0.AND.JTAG(1,I2)==0) THEN
              JTAG(1,I2)=JTAG(1,I1)
            ELSEIF(JTAG(1,I1)==0.AND.JTAG(1,I2)/=0) THEN
              JTAG(1,I1)=JTAG(1,I2)
            ELSEIF(JTAG(1,I1) /= JTAG(1,I2)) THEN
              IMIN=MIN(JTAG(1,I1),JTAG(1,I2))
              IMAX=MAX(JTAG(1,I1),JTAG(1,I2))
              DO J=1,NSEG
                IF(JTAG(1,J) == IMAX) JTAG(1,J)=IMIN
                IF(JTAG(2,J) == IMAX) JTAG(2,J)=IMIN
              ENDDO
            ENDIF
C
          ENDDO
C-------------------------------------
C Noeud connecte a 2 segments internes
C-------------------------------------
          DO I=1,NNO
            IF(NODSEG(I,1) /= 2) CYCLE
            I1=NODSEG(I,2)
            I2=NODSEG(I,3)
            IF(ISEG(5,I1) == 0) CYCLE
C
            IF(JTAG(2,I1)==0.AND.JTAG(2,I2)==0) THEN
              ICMAX=ICMAX+1
              JTAG(2,I1)=ICMAX
              JTAG(2,I2)=ICMAX
            ELSEIF(JTAG(2,I1)/=0.AND.JTAG(2,I2)==0) THEN
              JTAG(2,I2)=JTAG(2,I1)
            ELSEIF(JTAG(2,I1)==0.AND.JTAG(2,I2)/=0) THEN
              JTAG(2,I1)=JTAG(2,I2)
            ELSEIF(JTAG(2,I1) /= JTAG(2,I2)) THEN
              IMIN=MIN(JTAG(2,I1),JTAG(2,I2))
              IMAX=MAX(JTAG(2,I1),JTAG(2,I2))
              DO J=1,NSEG
                IF(JTAG(1,J) == IMAX) JTAG(1,J)=IMIN
                IF(JTAG(2,J) == IMAX) JTAG(2,J)=IMIN
              ENDDO
            ENDIF
C
            IF(JTAG(1,I1)==0.AND.JTAG(1,I2)==0) THEN
              ICMAX=ICMAX+1
              JTAG(1,I1)=ICMAX
              JTAG(1,I2)=ICMAX
            ELSEIF(JTAG(1,I1)/=0.AND.JTAG(1,I2)==0) THEN
              JTAG(1,I2)=JTAG(1,I1)
            ELSEIF(JTAG(1,I1)==0.AND.JTAG(1,I2)/=0) THEN
              JTAG(1,I1)=JTAG(1,I2)
            ELSEIF(JTAG(1,I1) /= JTAG(1,I2)) THEN
              IMIN=MIN(JTAG(1,I1),JTAG(1,I2))
              IMAX=MAX(JTAG(1,I1),JTAG(1,I2))
              DO J=1,NSEG
                IF(JTAG(1,J) == IMAX) JTAG(1,J)=IMIN
                IF(JTAG(2,J) == IMAX) JTAG(2,J)=IMIN
              ENDDO
            ENDIF
          ENDDO
C-------------------------------------
C Noeud connecte a 2 segments externes
C-------------------------------------
          DO I=1,NNO
            IF(NODSEG(I,1) /= 2) CYCLE
            I1=NODSEG(I,2)
            I2=NODSEG(I,3)
            IF(ISEG(5,I1) == 1) CYCLE
C
            IF(JTAG(1,I1)==0.AND.JTAG(1,I2)==0) THEN
              ICMAX=ICMAX+1
              JTAG(1,I1)=ICMAX
              JTAG(1,I2)=ICMAX
            ELSEIF(JTAG(1,I1)/=0.AND.JTAG(1,I2)==0) THEN
              JTAG(1,I2)=JTAG(1,I1)
            ELSEIF(JTAG(1,I1)==0.AND.JTAG(1,I2)/=0) THEN
              JTAG(1,I1)=JTAG(1,I2)
            ELSEIF(JTAG(1,I1) /= JTAG(1,I2)) THEN
              IMIN=MIN(JTAG(1,I1),JTAG(1,I2))
              IMAX=MAX(JTAG(1,I1),JTAG(1,I2))
              DO J=1,NSEG
                IF(JTAG(1,J) == IMAX) JTAG(1,J)=IMIN
              ENDDO
            ENDIF
          ENDDO
C
          DEALLOCATE (NODSEG)
C
          INFO=0
          NPOLY=0
          DO I=1,ICMAX
            II=0
            DO J=1,NSEG
              IF (JTAG(1,J) == I .OR. JTAG(2,J) == I) II=II+1
            ENDDO
            IF (II/=0) THEN
              NPOLY=NPOLY+1
              LENPOLY(NPOLY)=II
            ENDIF
          ENDDO
          IF (NPOLY>NPOLMAX) THEN
            NPOLMAX=NPOLY
            IF(ILVOUT >=1) WRITE(IOUT,'(A,I10,A,I10)')
     .      ' MONITORED VOLUME ID: ',MVID,' NLAYER  IS RESET TO ',NPOLY
            INFO=1
          ENDIF
          LENMAX=0
          DO I=1,NPOLY
            LENMAX=MAX(LENMAX,LENPOLY(I))
          ENDDO
          IF (LENMAX>NPPMAX) THEN
            NPPMAX=LENMAX
            IF(ILVOUT >=1) WRITE(IOUT,'(A,I10,A,I10)')
     .      ' MONITORED VOLUME ID: ',MVID,' NPPMAX  IS RESET TO ',LENMAX
            INFO=1
          ENDIF
          IF (INFO==1) RETURN
C
          NNP=NPOLY
          NPOLY=0
          JJ=0
          DO I=1,ICMAX
            II=0
            DO J=1,NSEG
              IF (JTAG(1,J) == I .OR. JTAG(2,J) == I) II=II+1
            ENDDO
            IF (II/=0) THEN
              NPOLY=NPOLY+1
              LENPOLY(NPOLY)=II
              DO J=1,NSEG
                IF (JTAG(1,J) == I .OR. JTAG(2,J) == I) THEN
                  JJ=JJ+1
                  POLY(1,JJ)=J
                ENDIF
              ENDDO
            ENDIF
          ENDDO
C----------------
C Order polygones
C----------------
          IAD=0
          DO I=1,NPOLY
            IF(LENPOLY(I) < 3) THEN
              WRITE(IOUT,'(A,I8,3(A,I5),A)')
     .        ' CUTTING BRICK NUMBER: ',NB,'  FACE NUMBER: ',IFAC,
     .        ' POLYGONE',I,' HAS ONLY',LENPOLY(I),' EDGES'
            ENDIF
            DO J=1,LENPOLY(I)
              TEMP(J)=POLY(1,IAD+J)
              ITAGSEG(J)=0
            ENDDO
            ISSEG=TEMP(1)
            POLY(2,IAD+1)=1
            ITAGSEG(1)=1
            IP0SEG=2
            I0=ISEG(1,ISSEG)
C           J=0
C           ICLOSE=0
            DO J=1,LENPOLY(I)-1
C              J=J+1
              I1=ISEG(IP0SEG,ISSEG)
              K=0
              ISTOP=0
              DO WHILE (ISTOP ==  0)
                K=K+1
                IF( K > NSEG ) THEN
                  WRITE(IOUT,'(A,I8,2(A,I5),A)')
     .            ' CUTTING BRICK NUMBER: ',NB,'  FACE NUMBER: ',IFAC,
     .            ' POLYGONE',I,' IS NOT CLOSED'
                  CALL ARRET(2)
                ENDIF
C
                IF(TEMP(K) == ISSEG .OR. ITAGSEG(K) == 1) CYCLE
                K1=ISEG(1,TEMP(K))
                K2=ISEG(2,TEMP(K))
                IF(K1 == I1) THEN
                  IP0SEG=2
                  ISSEG=TEMP(K)
                  POLY(1,IAD+J+1)=ISSEG
                  POLY(2,IAD+J+1)=1
                  ISTOP=1
                  ITAGSEG(K)=1
                ELSEIF(K2 == I1) THEN
                  IP0SEG=1
                  ISSEG=TEMP(K)
                  POLY(1,IAD+J+1)=ISSEG
                  POLY(2,IAD+J+1)=2
                  ISTOP=1
                  ITAGSEG(K)=1
                ENDIF
              ENDDO
            ENDDO
            IF(ISEG(IP0SEG,ISSEG) /= I0) THEN
              WRITE(IOUT,'(A,I8,2(A,I5),A)')
     .        ' CUTTING BRICK NUMBER: ',NB,'  FACE NUMBER: ',IFAC,
     .        ' POLYGONE',I,' IS NOT CLOSED'
            ENDIF
            IAD=IAD+LENPOLY(I)
          ENDDO
C
        ENDIF  ! ALGO
C
        NNS=0
        IAD=0
        DO I=1,NPOLY
          IPOLY(1,I)=2
          IPOLY(2,I)=LENPOLY(I)
          IPOLY(3,I)=NB
          IPOLY(4,I)=NV
          IPOLY(5,I)=0
          IPOLY(6,I)=0
          RPOLY(1,I)=ZERO
          RPOLY(2,I)=N(1)
          RPOLY(3,I)=N(2)
          RPOLY(4,I)=N(3)
          DO J=1,LENPOLY(I)
            JJ =POLY(1,IAD+J)
            JJJ=POLY(2,IAD+J)
            ID1=ISEG(JJJ,JJ)
            NNS=NNS+1
            IPOLY(6+J,I)=NNS
            IELNOD(J,I)=ELNODE(ID1)
            RPOLY(4+3*(J-1)+1,I)=NODE(1,ID1)
            RPOLY(4+3*(J-1)+2,I)=NODE(2,ID1)
            RPOLY(4+3*(J-1)+3,I)=NODE(3,ID1)
          ENDDO
          IAD=IAD+LENPOLY(I)
        ENDDO
C
C Recherche des trous
        INFO=0
        DO I=1,NPOLY
C
          NHOL=0
          DO J=1,NPOLY
            IADHOL(J)=0
          ENDDO
C
          DO J=1,NPOLY
            IF (I==J) CYCLE
            ALPHA=ZERO
            X1=RPOLY(5,J)
            Y1=RPOLY(6,J)
            Z1=RPOLY(7,J)
            DO K=1,LENPOLY(I)
              KK=K+1
              IF (K==LENPOLY(I)) KK=1
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
              ENDIF
              IF(NR2 > ZERO) THEN
                VX2=VX2/NR2
                VY2=VY2/NR2
                VZ2=VZ2/NR2
              ENDIF
              SS=VX1*VX2+VY1*VY2+VZ1*VZ2
              IF(SS < -ONE) SS=-ONE
              IF(SS >  ONE) SS= ONE
              VVX=VY1*VZ2-VZ1*VY2
              VVY=VZ1*VX2-VX1*VZ2
              VVZ=VX1*VY2-VY1*VX2
              SS1=N(1)*VVX+N(2)*VVY+N(3)*VVZ
              IF (SS1>ZERO) THEN
                ALPHA=ALPHA+ACOS(SS)
              ELSEIF(SS1<ZERO) THEN
                ALPHA=ALPHA-ACOS(SS)
              ENDIF
            ENDDO
C
            IF (ABS(ALPHA)<TWO*PI) CYCLE
C            IF (ABS(ALPHA)>=ONE) THEN
C Le premier point du polygone i est a l'interieur du polygone j
C Un point est interne si ABS(ALPHA)=2PI!
C On teste tous les autres
            IPOUT=0
            DO K=2,LENPOLY(J)
              X1=RPOLY(4+3*(K-1)+1,J)
              Y1=RPOLY(4+3*(K-1)+2,J)
              Z1=RPOLY(4+3*(K-1)+3,J)
              ALPHA=ZERO
              DO M=1,LENPOLY(I)
                MM=M+1
                IF (M==LENPOLY(I)) MM=1
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
                ENDIF
                IF(NR2 > ZERO) THEN
                  VX2=VX2/NR2
                  VY2=VY2/NR2
                  VZ2=VZ2/NR2
                ENDIF
                SS=VX1*VX2+VY1*VY2+VZ1*VZ2
                IF(SS < -ONE) SS=-ONE
                IF(SS >  ONE) SS= ONE
                VVX=VY1*VZ2-VZ1*VY2
                VVY=VZ1*VX2-VX1*VZ2
                VVZ=VX1*VY2-VY1*VX2
                SS1=N(1)*VVX+N(2)*VVY+N(3)*VVZ
                IF (SS1>ZERO) THEN
                  ALPHA=ALPHA+ACOS(SS)
                ELSEIF(SS1<ZERO) THEN
                  ALPHA=ALPHA-ACOS(SS)
                ENDIF
              ENDDO
C                  IF (ABS(ALPHA)<ONE) IPOUT=1
              IF (ABS(ALPHA)<TWO*PI) IPOUT=1
            ENDDO
C
            IF (IPOUT==1) CYCLE
C Le polygone j est un trou dans le polygone i
            IPOLY(1,J)=-1
            NHOL=NHOL+1
            IADHOL(NHOL)=LENPOLY(I)
            IF (LENPOLY(I)+LENPOLY(J)>NPPMAX) THEN
              NPPMAX=LENPOLY(I)+LENPOLY(J)
              IF(ILVOUT >=1) WRITE(IOUT,'(A,I10,A,I10)')
     .                        ' MONITORED VOLUME ID: ',MVID,
     .                        ' NPPMAX  IS RESET TO ',NPPMAX
              INFO=1
              RETURN
            ELSE
              IF (ILVOUT >=1) THEN
                WRITE(IOUT,'(/A25,I10,A25)')
     .          ' ** MONITORED VOLUME ID: ',MVID,' - HOLE DETECTED'
                WRITE(IOUT,'(A26,I8,A16,I8)')
     .          '    CUTTING BRICK NUMBER: ',NB,'  FACE NUMBER: ',IFAC
              ENDIF
              DO K=1,LENPOLY(J)
                IPOLY(6+IADHOL(NHOL)+K,I)=IPOLY(6+K,J)
                IELNOD(IADHOL(NHOL)+K,I)=IELNOD(K,J)
                RPOLY(4+3*IADHOL(NHOL)+3*(K-1)+1,I)=
     .                                 RPOLY(4+3*(K-1)+1,J)
                RPOLY(4+3*IADHOL(NHOL)+3*(K-1)+2,I)=
     .                                 RPOLY(4+3*(K-1)+2,J)
                RPOLY(4+3*IADHOL(NHOL)+3*(K-1)+3,I)=
     .                                 RPOLY(4+3*(K-1)+3,J)
              ENDDO
            ENDIF
            LENPOLY(I)=LENPOLY(I)+LENPOLY(J)
C Point interieur polygone j
            VX1=QUAD(1,2)-QUAD(1,1)
            VY1=QUAD(2,2)-QUAD(2,1)
            VZ1=QUAD(3,2)-QUAD(3,1)
            SS=SQRT(VX1**2+VY1**2+VZ1**2)
            VX1=VX1/SS
            VY1=VY1/SS
            VZ1=VZ1/SS
            VX2=N(2)*VZ1-N(3)*VY1
            VY2=N(3)*VX1-N(1)*VZ1
            VZ2=N(1)*VY1-N(2)*VX1
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
              IF (XLOC(2,K)<YLMIN) YLMIN=XLOC(2,K)
              IF (XLOC(2,K)>YLMAX) YLMAX=XLOC(2,K)
            ENDDO
            YLSEC=HALF*(YLMIN+YLMAX)
C
            NSEC=0
            DO K=1,LENPOLY(J)
              KK=K+1
              IF (K==LENPOLY(J)) KK=1
              X1=XLOC(1,K)
              Y1=XLOC(2,K)
              X2=XLOC(1,KK)
              Y2=XLOC(2,KK)
              IF (Y1-Y2/=ZERO) THEN
                ALPHA=(YLSEC-Y2)/(Y1-Y2)
                IF (ALPHA>=ZERO.AND.ALPHA<=ONE) THEN
                  NSEC=NSEC+1
                  XSEC(NSEC)=ALPHA*X1+(ONE-ALPHA)*X2
                ENDIF
              ELSE
                IF (Y1==YLSEC) THEN
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
              IF (XSEC(K)<XSMIN1) THEN
                XSMIN1=XSEC(K)
                KSMIN=K
              ENDIF
            ENDDO
            XSMIN2=EP30
            DO K=1,NSEC
              IF (K==KSMIN) CYCLE
              IF (XSEC(K)<XSMIN2) XSMIN2=XSEC(K)
            ENDDO
C
            XS=HALF*(XSMIN1+XSMIN2)
            YS=YLSEC
            PHOL(1,NHOL)=RPOLY(5,J)+XS*VX1+YS*VX2
            PHOL(2,NHOL)=RPOLY(6,J)+XS*VY1+YS*VY2
            PHOL(3,NHOL)=RPOLY(7,J)+XS*VZ1+YS*VZ2
C            ENDIF
          ENDDO
C
          IF (INFO==0) THEN
            IPOLY(2,I)=LENPOLY(I)
            IPOLY(6+LENPOLY(I)+1,I)=NHOL
            DO J=1,NHOL
              IPOLY(6+LENPOLY(I)+1+J,I)=IADHOL(J)
              RPOLY(4+3*LENPOLY(I)+3*(J-1)+1,I)=PHOL(1,J)
              RPOLY(4+3*LENPOLY(I)+3*(J-1)+2,I)=PHOL(2,J)
              RPOLY(4+3*LENPOLY(I)+3*(J-1)+3,I)=PHOL(3,J)
            ENDDO
          ENDIF
        ENDDO  ! I=1,NPOLY
C
        RETURN
      END
