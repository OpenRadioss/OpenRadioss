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
Chd|  I2_DTN_27                     source/interfaces/inter3d1/i2_dtn_27.F
Chd|-- called by -----------
Chd|        ININTR2                       source/interfaces/inter3d1/inintr2.F
Chd|-- calls ---------------
Chd|        I2_DTN_27_CIN                 source/interfaces/inter3d1/i2_dtn_27.F
Chd|        I2_DTN_27_PEN                 source/interfaces/inter3d1/i2_dtn_27.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I2_DTN_27(X,INTBUF_TAB,IPARI,STIFN,MS,IN,N,NSN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C=======================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*),NSN,N
C     REAL
      my_real
     .   X(3,*),STIFN(*),MS(*),IN(*)
C
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NI,NINDXC,NINDXP
C=======================================================================
      NINDXC = 0
      NINDXP = 0
C----------------
      DO I=1,NSN
        IF (INTBUF_TAB(N)%IRUPT(I) == 0) THEN
          NINDXC = NINDXC + 1
        ELSE
          NINDXP = NINDXP + 1
        ENDIF
      ENDDO
c-----------
      IF (NINDXC > 0) THEN
        CALL I2_DTN_27_CIN(X,INTBUF_TAB(N)%IRECTM,INTBUF_TAB(N)%CSTS,INTBUF_TAB(N)%CSTS_BIS,INTBUF_TAB(N)%NSV,
     .                     INTBUF_TAB(N)%IRTLM ,IPARI(1,N),INTBUF_TAB(N)%MSEGTYP2, STIFN, STIFN(NUMNOD+1),
     .                     MS,IN,INTBUF_TAB(N)%IRUPT)
      ENDIF
c-----------
      IF (NINDXP > 0) THEN
        CALL I2_DTN_27_PEN(X,INTBUF_TAB(N)%IRECTM,INTBUF_TAB(N)%CSTS,INTBUF_TAB(N)%NSV,INTBUF_TAB(N)%IRTLM,
     .                     IPARI(1,N),INTBUF_TAB(N)%MSEGTYP2,STIFN,STIFN(NUMNOD+1),INTBUF_TAB(N)%SPENALTY,
     .                     INTBUF_TAB(N)%STFR_PENALTY,INTBUF_TAB(N)%VARIABLES(14),IN,INTBUF_TAB(N)%IRUPT)
      ENDIF
C-----------
      RETURN
      END

Chd|====================================================================
Chd|  I2_DTN_27_CIN                 source/interfaces/inter3d1/i2_dtn_27.F
Chd|-- called by -----------
Chd|        I2_DTN_27                     source/interfaces/inter3d1/i2_dtn_27.F
Chd|-- calls ---------------
Chd|        I2CIN_ROT27                   ../common_source/interf/i2cin_rot27.F
Chd|        I2LOCEQ_27                    ../common_source/interf/i2loceq.F
Chd|        I2REP                         ../common_source/interf/i2rep.F
Chd|====================================================================
      SUBROUTINE I2_DTN_27_CIN(X,IRECT ,CRST    ,CSTS_BIS, NSV  ,
     2                         IRTL  ,IPARI, MSEGTYP2, STIFN, STIFR,
     2                         MS, IN, IRUPT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), NSV(*),IRTL(*),
     .        IPARI(*),MSEGTYP2(*),IRUPT(*)
      my_real
     .   X(3,*),CRST(2,*),STIFN(*),STIFR(*), MS(*),CSTS_BIS(2,*),IN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II,I,J,JJ,L,IX1,IX2,IX3,IX4,NIR,NRTM,NSN,NMN,K
      my_real
     .   BID,BID4(4),BID9(9),X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,XS(3),XM(3),X0,Y0,Z0,BETAX,BETAY,BETAZ,
     .   E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,H(4),H2(4),SS,ST,SP,SM,TP,TM,RS(3),RM(3),RX(4),RY(4),RZ(4),
     .   FAC_TRIANG,STBRK,DWDU,STIFMR,STIFM,INS,STF,AA
C=======================================================================
      NRTM   = IPARI(4)
      NSN    = IPARI(5)
      NMN    = IPARI(6)
C
      BID = ZERO
      BID4(1:4)=ZERO
      BID9(1:9)=ZERO
C
      DO II=1,NSN
        IF (IRUPT(II) /= 0) CYCLE
        I = NSV(II)
        L = IRTL(II)
C
        IX1 = IRECT(1,L)
        IX2 = IRECT(2,L)
        IX3 = IRECT(3,L)
        IX4 = IRECT(4,L)
C
        IF (IX3 == IX4) THEN
C--        Shape functions of triangles
           NIR = 3
           H(1) = CRST(1,II)
           H(2) = CRST(2,II)
           H(3) = ONE-CRST(1,II)-CRST(2,II)
           H(4) = ZERO
           H2(1) = CSTS_BIS(1,II)
           H2(2) = CSTS_BIS(2,II)
           H2(3) = ONE-CSTS_BIS(1,II)-CSTS_BIS(2,II)
           H2(4) = ZERO
        ELSE
C--        Shape functions of quadrangles
           NIR = 4
           SS=CRST(1,II)
           ST=CRST(2,II)
           SP=ONE + SS
           SM=ONE - SS
           TP=FOURTH*(ONE + ST)
           TM=FOURTH*(ONE - ST)
           H(1)=TM*SM
           H(2)=TM*SP
           H(3)=TP*SP
           H(4)=TP*SM

C          Additional form functions for distribution of mass / inertia - to avoid negative masses for projection outside of the element
           SS=CSTS_BIS(1,II)
           ST=CSTS_BIS(2,II)
           SP=ONE + SS
           SM=ONE - SS
           TP=FOURTH*(ONE + ST)
           TM=FOURTH*(ONE - ST)
           H2(1)=TM*SM
           H2(2)=TM*SP
           H2(3)=TP*SP
           H2(4)=TP*SM
        ENDIF
C
        IF (MSEGTYP2(L)==0) THEN
C---- rep local facette main
C
            X1  = X(1,IX1)
            Y1  = X(2,IX1)
            Z1  = X(3,IX1)
            X2  = X(1,IX2)
            Y2  = X(2,IX2)
            Z2  = X(3,IX2)
            X3  = X(1,IX3)
            Y3  = X(2,IX3)
            Z3  = X(3,IX3)
            X4  = X(1,IX4)
            Y4  = X(2,IX4)
            Z4  = X(3,IX4)
            XS(1)  = X(1,I)
            XS(2)  = X(2,I)
            XS(3)  = X(3,I)
C
            CALL I2REP(X1     ,X2     ,X3     ,X4     ,
     .               Y1     ,Y2     ,Y3     ,Y4     ,
     .               Z1     ,Z2     ,Z3     ,Z4     ,
     .               E1X    ,E1Y    ,E1Z    ,
     .               E2X    ,E2Y    ,E2Z    ,
     .               E3X    ,E3Y    ,E3Z    ,NIR)

C
            IF (NIR == 4) THEN
              FAC_TRIANG = ONE
              X0  = FOURTH*(X1 + X2 + X3 + X4)
              Y0  = FOURTH*(Y1 + Y2 + Y3 + Y4)
              Z0  = FOURTH*(Z1 + Z2 + Z3 + Z4)
            ELSE
              FAC_TRIANG = ZERO
              X0  = THIRD*(X1 + X2 + X3)
              Y0  = THIRD*(Y1 + Y2 + Y3)
              Z0  = THIRD*(Z1 + Z2 + Z3)
            ENDIF
C
            XS(1) = XS(1) - X0
            XS(2) = XS(2) - Y0
            XS(3) = XS(3) - Z0
C
            X1 = X1 - X0
            Y1 = Y1 - Y0
            Z1 = Z1 - Z0
            X2 = X2 - X0
            Y2 = Y2 - Y0
            Z2 = Z2 - Z0
            X3 = X3 - X0
            Y3 = Y3 - Y0
            Z3 = Z3 - Z0
            X4 = X4 - X0
            Y4 = Y4 - Y0
            Z4 = Z4 - Z0
C
            IF (NIR==3) THEN
              X4 = ZERO
              Y4 = ZERO
              Z4 = ZERO
            END IF
C
            XM(1) = X1*H(1) + X2*H(2) + X3*H(3) + X4*H(4)
            XM(2) = Y1*H(1) + Y2*H(2) + Y3*H(3) + Y4*H(4)
            XM(3) = Z1*H(1) + Z2*H(2) + Z3*H(3) + Z4*H(4)

C---- computation of local coordinates
C
            RS(1) = XS(1)*E1X + XS(2)*E1Y + XS(3)*E1Z
            RS(2) = XS(1)*E2X + XS(2)*E2Y + XS(3)*E2Z
            RS(3) = XS(1)*E3X + XS(2)*E3Y + XS(3)*E3Z
C
            RM(1) = XM(1)*E1X + XM(2)*E1Y + XM(3)*E1Z
            RM(2) = XM(1)*E2X + XM(2)*E2Y + XM(3)*E2Z
            RM(3) = XM(1)*E3X + XM(2)*E3Y + XM(3)*E3Z
c
            RX(1) = E1X*X1 + E1Y*Y1 + E1Z*Z1
            RY(1) = E2X*X1 + E2Y*Y1 + E2Z*Z1
            RZ(1) = E3X*X1 + E3Y*Y1 + E3Z*Z1
            RX(2) = E1X*X2 + E1Y*Y2 + E1Z*Z2
            RY(2) = E2X*X2 + E2Y*Y2 + E2Z*Z2
            RZ(2) = E3X*X2 + E3Y*Y2 + E3Z*Z2
            RX(3) = E1X*X3 + E1Y*Y3 + E1Z*Z3
            RY(3) = E2X*X3 + E2Y*Y3 + E2Z*Z3
            RZ(3) = E3X*X3 + E3Y*Y3 + E3Z*Z3
            RX(4) = E1X*X4 + E1Y*Y4 + E1Z*Z4
            RY(4) = E2X*X4 + E2Y*Y4 + E2Z*Z4
            RZ(4) = E3X*X4 + E3Y*Y4 + E3Z*Z4
C
C---- computation of kinematic parameters and stbrk - local coordinates
            CALL I2CIN_ROT27(STBRK,RS,RM,RX(1),RY(1),RZ(1),RX(2),RY(2),RZ(2),RX(3),RY(3),RZ(3),
     .                       RX(4),RY(4),RZ(4),BID9,DWDU,E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,
     .                       NIR,BETAX,BETAY)
C
C---- update main forces (moment balance) - local coordinates RX
          IF (IRODDL==1) THEN
C--
              CALL I2LOCEQ_27(NIR    ,RS     ,RX     ,RY     ,RZ      ,
     .                        BID4   ,BID4   ,BID4   ,H      ,STIFM   ,
     .                        BID4   ,BID4   ,BID4   ,STIFMR ,BETAX   ,
     .                        BETAY)
          ELSE
C--           moment balance
              CALL I2LOCEQ_27(NIR    ,RS     ,RX     ,RY     ,RZ      ,
     .                        BID4   ,BID4   ,BID4   ,H      ,STIFM   ,
     .                        BID4   ,BID4   ,BID4   ,STIFMR ,BETAX   ,
     .                        BETAY)
              STIFMR = ZERO
          ENDIF
C
        ELSE
          STIFM=ZERO
          STBRK=ZERO
          STIFMR=ZERO
          DWDU=ZERO
        ENDIF
C
        IF ((IRODDL/=0).AND.(MSEGTYP2(L)==0)) THEN
C
          AA =(XM(1)-XS(1))*(XM(1)-XS(1))+(XM(2)-XS(2))*(XM(2)-XS(2))+(XM(3)-XS(3))*(XM(3)-XS(3))
          INS = IN(I) + AA * MS(I)
          STF = STIFR(I) + AA * STIFN(I)
C
          DO JJ=1,NIR
            J=IRECT(JJ,L)
            MS(J)=MS(J)+MS(I)*H2(JJ)
            STIFN(J)=STIFN(J)+STIFN(I)*(ONE+STBRK)*(ABS(H(JJ))+STIFM)+STIFR(I)*STIFMR*DWDU
            IN(J)=IN(J)+INS*H2(JJ)
            STIFR(J)=STIFR(J)+ABS(STF*H(JJ))
          ENDDO
C
          STIFN(I) = ZERO
          STIFR(I) = ZERO
          MS(I) = ZERO
          IN(I) = ZERO
C
        ELSE
          DO JJ=1,NIR
            J=IRECT(JJ,L)
            MS(J)=MS(J)+MS(I)*H2(JJ)
            STIFN(J)=STIFN(J)+STIFN(I)*(ONE+STBRK)*(ABS(H(JJ))+STIFM)
          ENDDO
C
          STIFN(I)=ZERO
          MS(I)=ZERO
C
        END IF
C
      ENDDO
C
C-----------
      RETURN
      END

Chd|====================================================================
Chd|  I2_DTN_27_PEN                 source/interfaces/inter3d1/i2_dtn_27.F
Chd|-- called by -----------
Chd|        I2_DTN_27                     source/interfaces/inter3d1/i2_dtn_27.F
Chd|-- calls ---------------
Chd|        I2LOCEQ                       ../common_source/interf/i2loceq.F
Chd|        I2PEN_ROT27                   ../common_source/interf/i2pen_rot.F
Chd|        I2REP                         ../common_source/interf/i2rep.F
Chd|====================================================================
      SUBROUTINE I2_DTN_27_PEN(X,IRECT ,CRST    ,NSV  ,IRTL,
     2                         IPARI,MSEGTYP2,STIFN, STIFR,STFN,
     3                         STFR,VISC,IN,IRUPT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IRECT(4,*),NSV(*),IRTL(*),MSEGTYP2(*),IPARI(*),IRUPT(*)
C     REAL
      my_real
     .   X(3,*),IN(*),STIFN(*),STIFR(*),STFN(*),STFR(*),CRST(2,*),VISC
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR,I,J,II,JJ,L,W,NN,KK,LLT,
     .        IX1, IX2, IX3, IX4,NSVG,NSN
C     REAL
      my_real
     .   S,T,SP,SM,TP,TM,E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,
     .   XSM,YSM,ZSM,XM,YM,ZM,X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,X0,Y0,Z0,XS,YS,ZS,STIFM,
     .   STF,STR,STBRK,B1,B2,B3,C1,C2,C3,DET
      my_real
     .   H(4),RX(4),RY(4),RZ(4),RM(3),RS(3),STIF, VIS
      my_real
     .   LEN2,FAC_TRIANG,IROT,SKEW(9),TT,BID,BID3(4)
C=======================================================================
      NSN    = IPARI(5)
C
      BID = ZERO
      BID3(1:4)=ZERO
      TT = ZERO
C
      DO II=1,NSN
        IF (IRUPT(II) == 0) CYCLE
        I = NSV(II)
        L = IRTL(II)
C
        IX1 = IRECT(1,L)
        IX2 = IRECT(2,L)
        IX3 = IRECT(3,L)
        IX4 = IRECT(4,L)
C
        IF (I > 0) THEN
          S = CRST(1,II)
          T = CRST(2,II)
          L = IRTL(II)
C
          IX1 = IRECT(1,L)
          IX2 = IRECT(2,L)
          IX3 = IRECT(3,L)
          IX4 = IRECT(4,L)
C
          IROT = ZERO
          IF(IRODDL > 0) THEN
            IF ((MSEGTYP2(L)==1).AND.(IN(I)>EM20)) THEN
C-- shell main segment --
              IROT = ONE
            ENDIF
          ENDIF
C
          IF (IX3 == IX4) THEN
C--         Shape functions of triangles
            NIR = 3
            H(1) = S
            H(2) = T
            H(3) = ONE-S-T
            H(4) = ZERO
          ELSE
C--         Shape functions of quadrangles
            NIR = 4
            SP=ONE + S
            SM=ONE - S
            TP=FOURTH*(ONE + T)
            TM=FOURTH*(ONE - T)
            H(1)=TM*SM
            H(2)=TM*SP
            H(3)=TP*SP
            H(4)=TP*SM
          ENDIF

C------------------------------------------------
C         rep local facette main
C------------------------------------------------
          X1  = X(1,IX1)
          Y1  = X(2,IX1)
          Z1  = X(3,IX1)
          X2  = X(1,IX2)
          Y2  = X(2,IX2)
          Z2  = X(3,IX2)
          X3  = X(1,IX3)
          Y3  = X(2,IX3)
          Z3  = X(3,IX3)
          X4  = X(1,IX4)
          Y4  = X(2,IX4)
          Z4  = X(3,IX4)
          XS  = X(1,I)
          YS  = X(2,I)
          ZS  = X(3,I)

C---------------------
          CALL I2REP(X1     ,X2     ,X3     ,X4     ,
     .               Y1     ,Y2     ,Y3     ,Y4     ,
     .               Z1     ,Z2     ,Z3     ,Z4     ,
     .               E1X    ,E1Y    ,E1Z    ,
     .               E2X    ,E2Y    ,E2Z    ,
     .               E3X    ,E3Y    ,E3Z    ,NIR    )
C------------------------------------------------
        IF (NIR == 4) THEN
          FAC_TRIANG = ONE
C
          XM = X1*H(1) + X2*H(2) + X3*H(3) + X4*H(4)
          YM = Y1*H(1) + Y2*H(2) + Y3*H(3) + Y4*H(4)
          ZM = Z1*H(1) + Z2*H(2) + Z3*H(3) + Z4*H(4)
          X0  = (X1 + X2 + X3 + X4)/NIR
          Y0  = (Y1 + Y2 + Y3 + Y4)/NIR
          Z0  = (Z1 + Z2 + Z3 + Z4)/NIR

          XM = XM - X0
          YM = YM - Y0
          ZM = ZM - Z0
          XS = XS - X0
          YS = YS - Y0
          ZS = ZS - Z0
          XSM = XS - XM
          YSM = YS - YM
          ZSM = ZS - ZM
C
        ELSE
          FAC_TRIANG = ZERO
C
          X0  = (X1 + X2 + X3)/NIR
          Y0  = (Y1 + Y2 + Y3)/NIR
          Z0  = (Z1 + Z2 + Z3)/NIR

          XM = X1*H(1) + X2*H(2) + X3*H(3)
          YM = Y1*H(1) + Y2*H(2) + Y3*H(3)
          ZM = Z1*H(1) + Z2*H(2) + Z3*H(3)

          XM = XM - X0
          YM = YM - Y0
          ZM = ZM - Z0
          XS = XS - X0
          YS = YS - Y0
          ZS = ZS - Z0
          XSM = XS - XM
          YSM = YS - YM
          ZSM = ZS - ZM
        ENDIF
C
        X1 = X1 - X0
        Y1 = Y1 - Y0
        Z1 = Z1 - Z0
        X2 = X2 - X0
        Y2 = Y2 - Y0
        Z2 = Z2 - Z0
        X3 = X3 - X0
        Y3 = Y3 - Y0
        Z3 = Z3 - Z0
        X4 = X4 - X0
        Y4 = Y4 - Y0
        Z4 = Z4 - Z0
C
c       global -> local
c
        RS(1) = XS*E1X + YS*E1Y + ZS*E1Z
        RS(2) = XS*E2X + YS*E2Y + ZS*E2Z
        RS(3) = XS*E3X + YS*E3Y + ZS*E3Z
        RM(1) = XM*E1X + YM*E1Y + ZM*E1Z
        RM(2) = XM*E2X + YM*E2Y + ZM*E2Z
        RM(3) = XM*E3X + YM*E3Y + ZM*E3Z
c
        RX(1) = E1X*X1 + E1Y*Y1 + E1Z*Z1
        RY(1) = E2X*X1 + E2Y*Y1 + E2Z*Z1
        RZ(1) = E3X*X1 + E3Y*Y1 + E3Z*Z1
        RX(2) = E1X*X2 + E1Y*Y2 + E1Z*Z2
        RY(2) = E2X*X2 + E2Y*Y2 + E2Z*Z2
        RZ(2) = E3X*X2 + E3Y*Y2 + E3Z*Z2
        RX(3) = E1X*X3 + E1Y*Y3 + E1Z*Z3
        RY(3) = E2X*X3 + E2Y*Y3 + E2Z*Z3
        RZ(3) = E3X*X3 + E3Y*Y3 + E3Z*Z3
        RX(4) = E1X*X4 + E1Y*Y4 + E1Z*Z4
        RY(4) = E2X*X4 + E2Y*Y4 + E2Z*Z4
        RZ(4) = E3X*X4 + E3Y*Y4 + E3Z*Z4
C
        IF (NIR==3) THEN
          RX(4)=ZERO
          RY(4)=ZERO
          RZ(4)=ZERO
        END IF
C
C---------
          CALL I2PEN_ROT27(
     .    SKEW ,TT   ,BID  ,STBRK,
     .    RS   ,RM   ,BID3 ,BID3 ,BID3 ,
     .    RX   ,RY   ,RZ   ,BID3 ,BID3 ,
     .    BID3 ,BID3 ,BID3 ,BID3 ,DET  ,
     .    B1   ,B2   ,B3   ,C1   ,C2   ,
     .    C3   ,IROT)
C
C------------------------------------------------
C
          STF     = STFN(II)*(VISC + SQRT(VISC**2 + (ONE+STBRK)))**2
          STIFM=ZERO

C------------------------------------------------
C
          IF (IROT > ZERO) THEN
C
C--   Secnd node shell of  spring
C
            LEN2 = XSM**2+YSM**2+ZSM**2
            STR = (STFR(II)+STFN(II)*LEN2)*(VISC + SQRT(VISC**2 + ONE))**2
C
          ELSE
C
C--   Secnd node of solids
C
            STR = ZERO
c           update main forces (moment balance)
            CALL I2LOCEQ( NIR    ,RS     ,RX     ,RY     ,RZ      ,
     .                  BID3   ,BID3   ,BID3    ,H(1) ,STIFM)
C
          ENDIF
C
C----------------------------------------------------
C
           STIFN(IX1) = STIFN(IX1)+ABS(STF*H(1))+STIFM*STF
           STIFN(IX2) = STIFN(IX2)+ABS(STF*H(2))+STIFM*STF
           STIFN(IX3) = STIFN(IX3)+ABS(STF*H(3))+STIFM*STF
           STIFN(IX4) = STIFN(IX4)+ABS(STF*H(4))+STIFM*STF*FAC_TRIANG
c
           IF (IRODDL == 1) THEN
             IF (IROT > 0) THEN

                STIFR(IX1) = STIFR(IX1)+ABS(STR*H(1))
                STIFR(IX2) = STIFR(IX2)+ABS(STR*H(2))
                STIFR(IX3) = STIFR(IX3)+ABS(STR*H(3))
                STIFR(IX4) = STIFR(IX4)+ABS(STR*H(4))
c
             ENDIF
           ENDIF
C
        END IF
C
      ENDDO
C
C-----------
      RETURN
      END
