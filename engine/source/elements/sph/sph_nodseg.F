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
Chd|  SPH_NODSEG                    source/elements/sph/sph_nodseg.F
Chd|-- called by -----------
Chd|        SPHREQS                       source/elements/sph/sphreq.F
Chd|        SPONOF2                       source/elements/sph/sponof2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPH_NODSEG(XI,YI,ZI,XX,TFLAG,NP,LONFSPH,IXP,DPS,WNORMAL,FLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER TFLAG,LONFSPH(*),IXP(*),NP,FLAG
        my_real
     .    XI,YI,ZI,XX(12),DPS(*),WNORMAL(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER K,NORMSEG
        my_real
     .         X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,XC,YC,ZC,
     .         X12,Y12,Z12,X23,Y23,Z23,X31,Y31,Z31,
     .         XH,YH,ZH,X1H,Y1H,Z1H,NXH,NYH,NZH,
     .         NN,NX1,NY1,NZ1,NX2,NY2,NZ2,NX3,NY3,NZ3,NX4,NY4,NZ4,
     .         D1,D2,D3,D4,D12,D23,D34,D41,PS,N12,
     .         NX,NY,NZ
C-----------------------------------------------
        IF (FLAG==1) DPS(NP) = 1.E+20
        NORMSEG=0
        X1 = XX(1)
        Y1 = XX(2)
        Z1 = XX(3)
        X2 = XX(4)
        Y2 = XX(5)
        Z2 = XX(6)
        X3 = XX(7)
        Y3 = XX(8)
        Z3 = XX(9)
        X4 = XX(10)
        Y4 = XX(11)
        Z4 = XX(12)
C----------
        IF (TFLAG==0) THEN
          XC=FOURTH*(X1+X2+X3+X4)
          YC=FOURTH*(Y1+Y2+Y3+Y4)
          ZC=FOURTH*(Z1+Z2+Z3+Z4)
        ELSE
          XC=X3
          YC=Y3
          ZC=Z3
        ENDIF
C----------
C          triangle 1,2,C
        K  =0
        X12=X2-X1
        Y12=Y2-Y1
        Z12=Z2-Z1
        X23=XC-X2
        Y23=YC-Y2
        Z23=ZC-Z2
        X31=X1-XC
        Y31=Y1-YC
        Z31=Z1-ZC
        NX1=-Y12*Z31+Z12*Y31
        NY1=-Z12*X31+X12*Z31
        NZ1=-X12*Y31+Y12*X31
        NN =ONE/MAX(EM20,SQRT(NX1*NX1+NY1*NY1+NZ1*NZ1))
        NX1=NN*NX1
        NY1=NN*NY1
        NZ1=NN*NZ1
        D1=NX1*(XI-X1)+NY1*(YI-Y1)+NZ1*(ZI-Z1)
        XH=XI-D1*NX1
        YH=YI-D1*NY1
        ZH=ZI-D1*NZ1
        X1H=XH-X1
        Y1H=YH-Y1
        Z1H=ZH-Z1
        NXH=Y12*Z1H-Z12*Y1H
        NYH=Z12*X1H-X12*Z1H
        NZH=X12*Y1H-Y12*X1H
        IF(NXH*NX1+NYH*NY1+NZH*NZ1>=ZERO)K=K+1
        X1H=XH-X2
        Y1H=YH-Y2
        Z1H=ZH-Z2
        NXH=Y23*Z1H-Z23*Y1H
        NYH=Z23*X1H-X23*Z1H
        NZH=X23*Y1H-Y23*X1H
        IF(NXH*NX1+NYH*NY1+NZH*NZ1>=ZERO)K=K+1
        X1H=XH-XC
        Y1H=YH-YC
        Z1H=ZH-ZC
        NXH=Y31*Z1H-Z31*Y1H
        NYH=Z31*X1H-X31*Z1H
        NZH=X31*Y1H-Y31*X1H
        IF(NXH*NX1+NYH*NY1+NZH*NZ1>=ZERO)K=K+1
        IF(K==3)THEN
C           XH interieur a 1,2,C
          IF(ABS(D1)<ABS(DPS(NP)))THEN
            DPS(NP)=D1
            NORMSEG=1
C             WPROJ(1,LONFSPH(IXP(NP)))=XH
C             WPROJ(2,LONFSPH(IXP(NP)))=YH
C             WPROJ(3,LONFSPH(IXP(NP)))=ZH
          ENDIF
          GOTO 101
        ENDIF
C
        IF (TFLAG==0) THEN
C----------
C          triangle 2,3,C
          K  =0
          X12=X3-X2
          Y12=Y3-Y2
          Z12=Z3-Z2
          X23=XC-X3
          Y23=YC-Y3
          Z23=ZC-Z3
          X31=X2-XC
          Y31=Y2-YC
          Z31=Z2-ZC
          NX2=-Y12*Z31+Z12*Y31
          NY2=-Z12*X31+X12*Z31
          NZ2=-X12*Y31+Y12*X31
          NN =ONE/MAX(EM20,SQRT(NX2*NX2+NY2*NY2+NZ2*NZ2))
          NX2=NN*NX2
          NY2=NN*NY2
          NZ2=NN*NZ2
          D2=NX2*(XI-X2)+NY2*(YI-Y2)+NZ2*(ZI-Z2)
          XH=XI-D2*NX2
          YH=YI-D2*NY2
          ZH=ZI-D2*NZ2
          X1H=XH-X2
          Y1H=YH-Y2
          Z1H=ZH-Z2
          NXH=Y12*Z1H-Z12*Y1H
          NYH=Z12*X1H-X12*Z1H
          NZH=X12*Y1H-Y12*X1H
          IF(NXH*NX2+NYH*NY2+NZH*NZ2>=ZERO)K=K+1
          X1H=XH-X3
          Y1H=YH-Y3
          Z1H=ZH-Z3
          NXH=Y23*Z1H-Z23*Y1H
          NYH=Z23*X1H-X23*Z1H
          NZH=X23*Y1H-Y23*X1H
          IF(NXH*NX2+NYH*NY2+NZH*NZ2>=ZERO)K=K+1
          X1H=XH-XC
          Y1H=YH-YC
          Z1H=ZH-ZC
          NXH=Y31*Z1H-Z31*Y1H
          NYH=Z31*X1H-X31*Z1H
          NZH=X31*Y1H-Y31*X1H
          IF(NXH*NX2+NYH*NY2+NZH*NZ2>=ZERO)K=K+1
          IF(K==3)THEN
C           XH interieur a 2,3,C
            IF(ABS(D2)<ABS(DPS(NP)))THEN
              DPS(NP)=D2
              NORMSEG=1
C             WPROJ(1,LONFSPH(IXP(NP)))=XH
C             WPROJ(2,LONFSPH(IXP(NP)))=YH
C             WPROJ(3,LONFSPH(IXP(NP)))=ZH
            ENDIF
            GOTO 101
          ENDIF
C----------
C          triangle 3,4,C
          K  =0
          X12=X4-X3
          Y12=Y4-Y3
          Z12=Z4-Z3
          X23=XC-X4
          Y23=YC-Y4
          Z23=ZC-Z4
          X31=X3-XC
          Y31=Y3-YC
          Z31=Z3-ZC
          NX3=-Y12*Z31+Z12*Y31
          NY3=-Z12*X31+X12*Z31
          NZ3=-X12*Y31+Y12*X31
          NN =ONE/MAX(EM20,SQRT(NX3*NX3+NY3*NY3+NZ3*NZ3))
          NX3=NN*NX3
          NY3=NN*NY3
          NZ3=NN*NZ3
          D3=NX3*(XI-X3)+NY3*(YI-Y3)+NZ3*(ZI-Z3)
          XH=XI-D3*NX3
          YH=YI-D3*NY3
          ZH=ZI-D3*NZ3
          X1H=XH-X3
          Y1H=YH-Y3
          Z1H=ZH-Z3
          NXH=Y12*Z1H-Z12*Y1H
          NYH=Z12*X1H-X12*Z1H
          NZH=X12*Y1H-Y12*X1H
          IF(NXH*NX3+NYH*NY3+NZH*NZ3>=ZERO)K=K+1
          X1H=XH-X4
          Y1H=YH-Y4
          Z1H=ZH-Z4
          NXH=Y23*Z1H-Z23*Y1H
          NYH=Z23*X1H-X23*Z1H
          NZH=X23*Y1H-Y23*X1H
          IF(NXH*NX3+NYH*NY3+NZH*NZ3>=ZERO)K=K+1
          X1H=XH-XC
          Y1H=YH-YC
          Z1H=ZH-ZC
          NXH=Y31*Z1H-Z31*Y1H
          NYH=Z31*X1H-X31*Z1H
          NZH=X31*Y1H-Y31*X1H
          IF(NXH*NX3+NYH*NY3+NZH*NZ3>=ZERO)K=K+1
          IF(K==3)THEN
C           XH interieur a 3,4,C
            IF(ABS(D3)<ABS(DPS(NP)))THEN
              DPS(NP)=D3
              NORMSEG=1
C             WPROJ(1,LONFSPH(IXP(NP)))=XH
C             WPROJ(2,LONFSPH(IXP(NP)))=YH
C             WPROJ(3,LONFSPH(IXP(NP)))=ZH
            ENDIF
            GOTO 101
          ENDIF
C----------
C          triangle 4,1,C
          K  =0
          X12=X1-X4
          Y12=Y1-Y4
          Z12=Z1-Z4
          X23=XC-X1
          Y23=YC-Y1
          Z23=ZC-Z1
          X31=X4-XC
          Y31=Y4-YC
          Z31=Z4-ZC
          NX4=-Y12*Z31+Z12*Y31
          NY4=-Z12*X31+X12*Z31
          NZ4=-X12*Y31+Y12*X31
          NN =ONE/MAX(EM20,SQRT(NX4*NX4+NY4*NY4+NZ4*NZ4))
          NX4=NN*NX4
          NY4=NN*NY4
          NZ4=NN*NZ4
          D4=NX4*(XI-X4)+NY4*(YI-Y4)+NZ4*(ZI-Z4)
          XH=XI-D4*NX4
          YH=YI-D4*NY4
          ZH=ZI-D4*NZ4
          X1H=XH-X4
          Y1H=YH-Y4
          Z1H=ZH-Z4
          NXH=Y12*Z1H-Z12*Y1H
          NYH=Z12*X1H-X12*Z1H
          NZH=X12*Y1H-Y12*X1H
          IF(NXH*NX4+NYH*NY4+NZH*NZ4>=ZERO)K=K+1
          X1H=XH-X1
          Y1H=YH-Y1
          Z1H=ZH-Z1
          NXH=Y23*Z1H-Z23*Y1H
          NYH=Z23*X1H-X23*Z1H
          NZH=X23*Y1H-Y23*X1H
          IF(NXH*NX4+NYH*NY4+NZH*NZ4>=ZERO)K=K+1
          X1H=XH-XC
          Y1H=YH-YC
          Z1H=ZH-ZC
          NXH=Y31*Z1H-Z31*Y1H
          NYH=Z31*X1H-X31*Z1H
          NZH=X31*Y1H-Y31*X1H
          IF(NXH*NX4+NYH*NY4+NZH*NZ4>=ZERO)K=K+1
          IF(K==3)THEN
C           XH interieur a 4,1,C
            IF(ABS(D4)<ABS(DPS(NP)))THEN
              DPS(NP)=D4
              NORMSEG=1
C             WPROJ(1,LONFSPH(IXP(NP)))=XH
C             WPROJ(2,LONFSPH(IXP(NP)))=YH
C             WPROJ(3,LONFSPH(IXP(NP)))=ZH
            ENDIF
            GOTO 101
          ENDIF
C
        ENDIF
C----------
C          distances aux cotes.
        X12=X2-X1
        Y12=Y2-Y1
        Z12=Z2-Z1
        N12=SQRT(X12*X12+Y12*Y12+Z12*Z12)
        NN =ONE/MAX(EM20,N12)
        X12=X12*NN
        Y12=Y12*NN
        Z12=Z12*NN
        PS=(XI-X1)*X12+(YI-Y1)*Y12+(ZI-Z1)*Z12
        IF(PS>=ZERO.AND.PS<=N12)THEN
          XH =X1+PS*X12
          YH =Y1+PS*Y12
          ZH =Z1+PS*Z12
          X1H=XI-XH
          Y1H=YI-YH
          Z1H=ZI-ZH
        ELSEIF(PS<ZERO)THEN
          X1H=XI-X1
          Y1H=YI-Y1
          Z1H=ZI-Z1
        ELSE
          X1H=XI-X2
          Y1H=YI-Y2
          Z1H=ZI-Z2
        ENDIF
        D12=SQRT(X1H*X1H+Y1H*Y1H+Z1H*Z1H)
        IF(D12<=ONEP001*ABS(DPS(NP)).AND.D1/=ZERO)THEN
          IF(D12<ZEP999*ABS(DPS(NP)))THEN
            NORMSEG=1
          ELSEIF (FLAG==0) THEN
            NORMSEG=2
          ENDIF
          IF(D12<ABS(DPS(NP)))DPS(NP)=SIGN(D12,D1)
C            WPROJ(1,LONFSPH(IXP(NP)))=XI-X1H
C            WPROJ(2,LONFSPH(IXP(NP)))=YI-Y1H
C            WPROJ(3,LONFSPH(IXP(NP)))=ZI-Z1H
        ENDIF
C
        X12=X3-X2
        Y12=Y3-Y2
        Z12=Z3-Z2
        N12=SQRT(X12*X12+Y12*Y12+Z12*Z12)
        NN =ONE/MAX(EM20,N12)
        X12=X12*NN
        Y12=Y12*NN
        Z12=Z12*NN
        PS=(XI-X2)*X12+(YI-Y2)*Y12+(ZI-Z2)*Z12
        IF(PS>=ZERO.AND.PS<=N12)THEN
          XH =X2+PS*X12
          YH =Y2+PS*Y12
          ZH =Z2+PS*Z12
          X1H=XI-XH
          Y1H=YI-YH
          Z1H=ZI-ZH
        ELSEIF(PS<ZERO)THEN
          X1H=XI-X2
          Y1H=YI-Y2
          Z1H=ZI-Z2
        ELSE
          X1H=XI-X3
          Y1H=YI-Y3
          Z1H=ZI-Z3
        ENDIF
        D23=SQRT(X1H*X1H+Y1H*Y1H+Z1H*Z1H)
        IF(D23<=ONEP001*ABS(DPS(NP)).AND.D2/=ZERO)THEN
          IF(D23<ZEP999*ABS(DPS(NP)))THEN
            NORMSEG=1
          ELSEIF (FLAG==0) THEN
            NORMSEG=2
          ENDIF
          IF(D23<ABS(DPS(NP)))DPS(NP)=SIGN(D23,D2)
C            WPROJ(1,LONFSPH(IXP(NP)))=XI-X1H
C            WPROJ(2,LONFSPH(IXP(NP)))=YI-Y1H
C            WPROJ(3,LONFSPH(IXP(NP)))=ZI-Z1H
        ENDIF
C
        IF (TFLAG==0) THEN
          X12=X4-X3
          Y12=Y4-Y3
          Z12=Z4-Z3
          N12=SQRT(X12*X12+Y12*Y12+Z12*Z12)
          NN =ONE/MAX(EM20,N12)
          X12=X12*NN
          Y12=Y12*NN
          Z12=Z12*NN
          PS=(XI-X3)*X12+(YI-Y3)*Y12+(ZI-Z3)*Z12
          IF(PS>=ZERO.AND.PS<=N12)THEN
            XH =X3+PS*X12
            YH =Y3+PS*Y12
            ZH =Z3+PS*Z12
            X1H=XI-XH
            Y1H=YI-YH
            Z1H=ZI-ZH
          ELSEIF(PS<ZERO)THEN
            X1H=XI-X3
            Y1H=YI-Y3
            Z1H=ZI-Z3
          ELSE
            X1H=XI-X4
            Y1H=YI-Y4
            Z1H=ZI-Z4
          ENDIF
          D34=SQRT(X1H*X1H+Y1H*Y1H+Z1H*Z1H)
          IF(D34<=ONEP001*ABS(DPS(NP)).AND.D3/=ZERO)THEN
            IF(D34<ZEP999*ABS(DPS(NP)))THEN
              NORMSEG=1
            ELSEIF (FLAG==0) THEN
              NORMSEG=2
            ENDIF
            IF(D34<ABS(DPS(NP)))DPS(NP)=SIGN(D34,D3)
C            WPROJ(1,LONFSPH(IXP(NP)))=XI-X1H
C            WPROJ(2,LONFSPH(IXP(NP)))=YI-Y1H
C            WPROJ(3,LONFSPH(IXP(NP)))=ZI-Z1H
          ENDIF
        ENDIF
C
        X12=X1-X4
        Y12=Y1-Y4
        Z12=Z1-Z4
        N12=SQRT(X12*X12+Y12*Y12+Z12*Z12)
        NN =ONE/MAX(EM20,N12)
        X12=X12*NN
        Y12=Y12*NN
        Z12=Z12*NN
        PS=(XI-X4)*X12+(YI-Y4)*Y12+(ZI-Z4)*Z12
        IF(PS>=ZERO.AND.PS<=N12)THEN
          XH =X4+PS*X12
          YH =Y4+PS*Y12
          ZH =Z4+PS*Z12
          X1H=XI-XH
          Y1H=YI-YH
          Z1H=ZI-ZH
        ELSEIF(PS<ZERO)THEN
          X1H=XI-X4
          Y1H=YI-Y4
          Z1H=ZI-Z4
        ELSE
          X1H=XI-X1
          Y1H=YI-Y1
          Z1H=ZI-Z1
        ENDIF
        D41=SQRT(X1H*X1H+Y1H*Y1H+Z1H*Z1H)
        IF(D41<=ONEP001*ABS(DPS(NP)).AND.D4/=ZERO)THEN
          IF(D41<ZEP999*ABS(DPS(NP)))THEN
            NORMSEG=1
          ELSEIF (FLAG==0) THEN
            NORMSEG=2
          ENDIF
          IF(D41<ABS(DPS(NP)))DPS(NP)=SIGN(D41,D4)
C            WPROJ(1,LONFSPH(IXP(NP)))=XI-X1H
C            WPROJ(2,LONFSPH(IXP(NP)))=YI-Y1H
C            WPROJ(3,LONFSPH(IXP(NP)))=ZI-Z1H
        ENDIF
 101    CONTINUE
        IF(NORMSEG==1)THEN
C             normale sortante au segment.
          X12=HALF*(X2+X3-X1-X4)
          Y12=HALF*(Y2+Y3-Y1-Y4)
          Z12=HALF*(Z2+Z3-Z1-Z4)
          X23=HALF*(X3+X4-X1-X2)
          Y23=HALF*(Y3+Y4-Y1-Y2)
          Z23=HALF*(Z3+Z4-Z1-Z2)
          NX = Y12*Z23-Z12*Y23
          NY =-X12*Z23+Z12*X23
          NZ = X12*Y23-Y12*X23
          NN =ONE/MAX(EM20,SQRT(NX*NX+NY*NY+NZ*NZ))
          WNORMAL(1,LONFSPH(IXP(NP)))=-NN*NX
          WNORMAL(2,LONFSPH(IXP(NP)))=-NN*NY
          WNORMAL(3,LONFSPH(IXP(NP)))=-NN*NZ
        ELSEIF(NORMSEG==2)THEN
C             cumule normale sortante au segment.
          X12=HALF*(X2+X3-X1-X4)
          Y12=HALF*(Y2+Y3-Y1-Y4)
          Z12=HALF*(Z2+Z3-Z1-Z4)
          X23=HALF*(X3+X4-X1-X2)
          Y23=HALF*(Y3+Y4-Y1-Y2)
          Z23=HALF*(Z3+Z4-Z1-Z2)
          NX = Y12*Z23-Z12*Y23
          NY =-X12*Z23+Z12*X23
          NZ = X12*Y23-Y12*X23
          NN =ONE/MAX(EM20,SQRT(NX*NX+NY*NY+NZ*NZ))
          WNORMAL(1,LONFSPH(IXP(NP)))=
     .        WNORMAL(1,LONFSPH(IXP(NP)))-NN*NX
          WNORMAL(2,LONFSPH(IXP(NP)))=
     .        WNORMAL(2,LONFSPH(IXP(NP)))-NN*NY
          WNORMAL(3,LONFSPH(IXP(NP)))=
     .        WNORMAL(3,LONFSPH(IXP(NP)))-NN*NZ
        ENDIF

C-----------------------------------------------
        RETURN
      END
