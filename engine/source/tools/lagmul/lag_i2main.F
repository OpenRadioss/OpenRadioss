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
Chd|  LAG_I2MAIN                    source/tools/lagmul/lag_i2main.F
Chd|-- called by -----------
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|        LAG_MULTP                     source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I2LAGM                        source/tools/lagmul/lag_i2main.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LAG_I2MAIN(
     1      IPARI    ,INTBUF_TAB,IADLL    ,LLL      ,JLL      ,    
     2      SLL      ,XLL      ,COMNTAG  ,LTSM     ,ICFTAG   ,
     3      JCFTAG   ,IN       ,MS       ,X        ,V        ,
     4      VR       ,A        ,AR       ,ISKIP    ,NCF_S    ,
     5      N_MULT   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N_MULT,ISKIP,NCF_S
      INTEGER IPARI(NPARI,NINTER), IADLL(*),
     .        LLL(*), JLL(*), SLL(*),COMNTAG(*),ICFTAG(*),JCFTAG(*)
C     REAL
      my_real
     .   LTSM(*),XLL(*),MS(*),IN(*),X(3,*),V(3,*),VR(3,*),
     .   A(3,*), AR(3,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NTY,ILAGM,NSN,NMN,NRTS,NRTM,NINT
C======================================================================|
      DO NINT=1,NINTER
C ---------------------
        NTY   = IPARI(7,NINT)
        ILAGM = IPARI(33,NINT)
        IF(NTY==2 .AND. ILAGM==1)THEN
          IF(ISPMD==0 .AND. NSPMD > 1)THEN
            CALL ANCMSG(MSGID=113,ANMODE=ANINFO,
     .                  C1='INT 2')
            CALL ARRET(2)
          END IF
          NRTS  =IPARI(3,NINT)
          NRTM  =IPARI(4,NINT)
          NSN   =IPARI(5,NINT)
          NMN   =IPARI(6,NINT)
C
          CALL I2LAGM(
     1      INTBUF_TAB(NINT)%IRECTM,INTBUF_TAB(NINT)%NSV,INTBUF_TAB(NINT)%IRTLM,
     2      IADLL     ,LLL       ,
     3      JLL       ,SLL       ,XLL       ,LTSM      ,COMNTAG   ,
     4      ICFTAG    ,JCFTAG    ,MS        ,IN        ,X         ,
     5      V         ,VR        ,A         ,AR        ,NINT      ,
     6      NSN       ,ISKIP     ,NCF_S     ,N_MULT    )
C
        ENDIF
      ENDDO
C---
      RETURN
      END
Chd|====================================================================
Chd|  I2LAGM                        source/tools/lagmul/lag_i2main.F
Chd|-- called by -----------
Chd|        LAG_I2MAIN                    source/tools/lagmul/lag_i2main.F
Chd|-- calls ---------------
Chd|        LAG_DIRECT                    source/tools/lagmul/lag_direct.F
Chd|====================================================================
      SUBROUTINE I2LAGM(
     1   IRECT    ,NSV      ,IRTL     ,
     2   IADLL    ,LLL      ,
     3   JLL      ,SLL      ,XLL      ,LTSM     ,COMNTAG  ,
     4   ICFTAG   ,JCFTAG   ,MS       ,IN       ,X        ,
     5   V        ,VR       ,A        ,AR       ,NINT     ,
     6   NSN      ,ISKIP    ,NCF_S    ,NC       )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NINT, NC, ISKIP, NCF_S
      INTEGER LLL(*), JLL(*), SLL(*), IADLL(*),
     .        IRECT(4,*),NSV(*),IRTL(*),COMNTAG(*),ICFTAG(*),JCFTAG(*)
C     REAL
      my_real
     .   LTSM(*),XLL(*),X(3,*),MS(*),IN(*),V(3,*),VR(3,*),
     .   A(3,*),AR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER INOD(5), I,J,K,L,IC,JC,II,JJ,IK,ISK,IAD,NIR,NDL, 
     .   NC_INI,NCL,COMFLAG
C     REAL
      my_real
     .   RX(4),RY(4),RZ(4),HLOC(6,6),LLOC(6),
     .   B1,B2,B3,C1,C2,C3,DET,S,HIJ,
     .   X0,X1,X2,X3,X4,XS,Y0,Y1,Y2,Y3,Y4,YS,Z0,Z1,Z2,Z3,Z4,ZS,
     .   X12,X22,X32,X42,Y12,Y22,Y32,Y42,Z12,Z22,Z32,Z42,
     .   XX,YY,ZZ,XXX,YYY,ZZZ,XY,YZ,ZX,XY2,YZ2,ZX2,FACT
C-----------------------------------------------
      NCL = 6
      DO II=1,NSN
        L=IRTL(II)
        NIR = 4
        DO JJ=1,NIR
          INOD(JJ) = IRECT(JJ,L)
        ENDDO
        IF(INOD(4)==INOD(3)) NIR=3
        FACT = ONE / NIR
        INOD(NIR+1) = NSV(II)
        NDL = 3*NIR+1
        NC_INI  = NC
        COMFLAG = 0
        DO JJ=1,NIR+1
          IF (COMNTAG(INOD(JJ))>1) COMFLAG = 1
        ENDDO
C------------------------------
        IF (NIR==4) THEN
          X1=X(1,INOD(1))
          Y1=X(2,INOD(1))
          Z1=X(3,INOD(1))
          X2=X(1,INOD(2))
          Y2=X(2,INOD(2))
          Z2=X(3,INOD(2))
          X3=X(1,INOD(3))
          Y3=X(2,INOD(3))
          Z3=X(3,INOD(3))
          X4=X(1,INOD(4))
          Y4=X(2,INOD(4))
          Z4=X(3,INOD(4))
          X0=FOURTH*(X1+X2+X3+X4)
          Y0=FOURTH*(Y1+Y2+Y3+Y4)
          Z0=FOURTH*(Z1+Z2+Z3+Z4)
          X1=X1-X0
          Y1=Y1-Y0
          Z1=Z1-Z0
          X2=X2-X0
          Y2=Y2-Y0
          Z2=Z2-Z0
          X3=X3-X0
          Y3=Y3-Y0
          Z3=Z3-Z0
          X4=X4-X0
          Y4=Y4-Y0
          Z4=Z4-Z0
C------------------------------
          RX(1) = X1
          RX(2) = X2
          RX(3) = X3
          RX(4) = X4
          RY(1) = Y1
          RY(2) = Y2
          RY(3) = Y3
          RY(4) = Y4
          RZ(1) = Z1
          RZ(2) = Z2
          RZ(3) = Z3
          RZ(4) = Z4
C------------------------------
          X12=X1*X1
          X22=X2*X2
          X32=X3*X3
          X42=X4*X4
          Y12=Y1*Y1
          Y22=Y2*Y2
          Y32=Y3*Y3
          Y42=Y4*Y4
          Z12=Z1*Z1
          Z22=Z2*Z2
          Z32=Z3*Z3
          Z42=Z4*Z4
          XX=X12 + X22 + X32 + X42
          YY=Y12 + Y22 + Y32 + Y42
          ZZ=Z12 + Z22 + Z32 + Z42
          XY=X1*Y1 + X2*Y2 + X3*Y3 + X4*Y4
          YZ=Y1*Z1 + Y2*Z2 + Y3*Z3 + Y4*Z4
          ZX=Z1*X1 + Z2*X2 + Z3*X3 + Z4*X4
        ELSEIF (NIR==3) THEN
          X1=X(1,INOD(1))
          Y1=X(2,INOD(1))
          Z1=X(3,INOD(1))
          X2=X(1,INOD(2))
          Y2=X(2,INOD(2))
          Z2=X(3,INOD(2))
          X3=X(1,INOD(3))
          Y3=X(2,INOD(3))
          Z3=X(3,INOD(3))
          X0=THIRD*(X1+X2+X3)
          Y0=THIRD*(Y1+Y2+Y3)
          Z0=THIRD*(Z1+Z2+Z3)
          X1=X1-X0
          Y1=Y1-Y0
          Z1=Z1-Z0
          X2=X2-X0
          Y2=Y2-Y0
          Z2=Z2-Z0
          X3=X3-X0
          Y3=Y3-Y0
          Z3=Z3-Z0
C------------------------------
          RX(1) = X1
          RX(2) = X2
          RX(3) = X3
          RY(1) = Y1
          RY(2) = Y2
          RY(3) = Y3
          RZ(1) = Z1
          RZ(2) = Z2
          RZ(3) = Z3
C------------------------------
          X12=X1*X1
          X22=X2*X2
          X32=X3*X3
          Y12=Y1*Y1
          Y22=Y2*Y2
          Y32=Y3*Y3
          Z12=Z1*Z1
          Z22=Z2*Z2
          Z32=Z3*Z3
          XX=X12 + X22 + X32
          YY=Y12 + Y22 + Y32
          ZZ=Z12 + Z22 + Z32
          XY=X1*Y1 + X2*Y2 + X3*Y3
          YZ=Y1*Z1 + Y2*Z2 + Y3*Z3
          ZX=Z1*X1 + Z2*X2 + Z3*X3
        ENDIF
        XS=X(1,INOD(NIR+1))-X0
        YS=X(2,INOD(NIR+1))-Y0
        ZS=X(3,INOD(NIR+1))-Z0
        ZZZ=XX+YY
        XXX=YY+ZZ
        YYY=ZZ+XX
        XY2=XY*XY
        YZ2=YZ*YZ
        ZX2=ZX*ZX
        DET=XXX*YYY*ZZZ - XXX*YZ2 - YYY*ZX2 - ZZZ*XY2 - TWO*XY*YZ*ZX
        DET=ONE/DET
        B1=ZZZ*YYY-YZ2
        B2=XXX*ZZZ-ZX2
        B3=YYY*XXX-XY2
        C3=ZZZ*XY+YZ*ZX
        C1=XXX*YZ+ZX*XY
        C2=YYY*ZX+XY*YZ
       
C======================================================================|
C ---   ic = 1 (vx)
        NC = NC + 1
        IADLL(NC+1) = IADLL(NC) + NDL
        IAD = IADLL(NC) -1
        DO JJ=1,NIR
          IK = IAD+JJ
          LLL(IK) = INOD(JJ)
          JLL(IK) = 1
          SLL(IK) = 0
          XLL(IK) = FACT
     .            + DET*ZS*(B2*RZ(JJ) - C1*RY(JJ))
     .            - DET*YS*(C1*RZ(JJ) - B3*RY(JJ))
        ENDDO
        IAD = IAD + NIR
        DO JJ=1,NIR
          IK = IAD+JJ
          LLL(IK) = INOD(JJ)
          JLL(IK) = 2
          SLL(IK) = 0
          XLL(IK) = DET*ZS*(C1*RX(JJ) - C3*RZ(JJ))
     .            - DET*YS*(B3*RX(JJ) - C2*RZ(JJ))
        ENDDO
        IAD = IAD + NIR
        DO JJ=1,NIR
          IK = IAD+JJ
          LLL(IK) = INOD(JJ)
          JLL(IK) = 3
          SLL(IK) = 0
          XLL(IK) = DET*ZS*(C3*RY(JJ) - B2*RX(JJ))
     .            - DET*YS*(C2*RY(JJ) - C1*RX(JJ))
        ENDDO
        IK = IAD + NIR+1
        LLL(IK) = INOD(NIR + 1)
        JLL(IK) = 1
        SLL(IK) = NINT
        XLL(IK) = -ONE
C ---  ic = 2 (vy)
        NC = NC + 1
        IADLL(NC+1) = IADLL(NC) + NDL
        IAD = IADLL(NC) -1
        DO JJ=1,NIR
          IK = IAD+JJ
          LLL(IK) = INOD(JJ)
          JLL(IK) = 1
          SLL(IK) = 0
          XLL(IK) = DET*XS*(C1*RZ(JJ) - B3*RY(JJ))
     .            - DET*ZS*(C3*RZ(JJ) - C2*RY(JJ))
        ENDDO
        IAD = IAD + NIR
        DO JJ=1,NIR
          IK = IAD+JJ
          LLL(IK) = INOD(JJ)
          JLL(IK) = 2
          SLL(IK) = 0
          XLL(IK) = FACT
     .            + DET*XS*(B3*RX(JJ) - C2*RZ(JJ))
     .            - DET*ZS*(C2*RX(JJ) - B1*RZ(JJ))
        ENDDO
        IAD = IAD + NIR
        DO JJ=1,NIR
          IK = IAD+JJ
          LLL(IK) = INOD(JJ)
          JLL(IK) = 3
          SLL(IK) = 0
          XLL(IK) = DET*XS*(C2*RY(JJ) - C1*RX(JJ))
     .            - DET*ZS*(B1*RY(JJ) - C3*RX(JJ))
        ENDDO
        IK = IAD + NIR+1
        LLL(IK) = INOD(NIR + 1)
        JLL(IK) = 2
        SLL(IK) = NINT
        XLL(IK) = -ONE
C ---  ic = 3 (vz)
        NC = NC + 1
        IADLL(NC+1) = IADLL(NC) + NDL
        IAD = IADLL(NC) -1
        DO JJ=1,NIR
          IK = IAD+JJ
          LLL(IK) = INOD(JJ)
          JLL(IK) = 1
          SLL(IK) = 0
          XLL(IK) = DET*YS*(C3*RZ(JJ) - C2*RY(JJ))
     .            - DET*XS*(B2*RZ(JJ) - C1*RY(JJ))
        ENDDO
        IAD = IAD + NIR
        DO JJ=1,NIR
          IK = IAD+JJ
          LLL(IK) = INOD(JJ)
          JLL(IK) = 2
          SLL(IK) = 0
          XLL(IK) = DET*YS*(C2*RX(JJ) - B1*RZ(JJ))
     .            - DET*XS*(C1*RX(JJ) - C3*RZ(JJ))
        ENDDO
        IAD = IAD + NIR
        DO JJ=1,NIR
          IK = IAD+JJ
          LLL(IK) = INOD(JJ)
          JLL(IK) = 3
          SLL(IK) = 0
          XLL(IK) = FACT
     .            + DET*YS*(B1*RY(JJ) - C3*RX(JJ))
     .            - DET*XS*(C3*RY(JJ) - B2*RX(JJ))
        ENDDO
        IK = IAD + NIR+1
        LLL(IK) = INOD(NIR+1)
        JLL(IK) = 3
        SLL(IK) = NINT
        XLL(IK) = -ONE
C
c       begin rotational dof of secnd
C ---   ic = 4 (wx)
        NC = NC + 1
        IADLL(NC+1) = IADLL(NC) + NDL
        IAD = IADLL(NC) -1
        DO JJ=1,NIR
          IK = IAD+JJ
          LLL(IK) = INOD(JJ)
          JLL(IK) = 1
          SLL(IK) = 0
          XLL(IK) = DET*(C3*RZ(JJ) - C2*RY(JJ))
        ENDDO
        IAD = IAD + NIR
        DO JJ=1,NIR
          IK = IAD+JJ
          LLL(IK) = INOD(JJ)
          JLL(IK) = 2
          SLL(IK) = 0
          XLL(IK) = DET*(C2*RX(JJ) - B1*RZ(JJ))
        ENDDO
        IAD = IAD + NIR
        DO JJ=1,NIR
          IK = IAD+JJ
          LLL(IK) = INOD(JJ)
          JLL(IK) = 3
          SLL(IK) = 0
          XLL(IK) = DET*(B1*RY(JJ) - C3*RX(JJ))
        ENDDO
        IK = IAD + NIR + 1
        LLL(IK) = INOD(NIR+1)
        JLL(IK) = 4
        SLL(IK) = NINT
        XLL(IK) = -ONE
C ---  ic = 5 (wy)
        NC = NC + 1
        IADLL(NC+1) = IADLL(NC) + NDL
        IAD = IADLL(NC) -1
        DO JJ=1,NIR
          IK = IAD+JJ
          LLL(IK) = INOD(JJ)
          JLL(IK) = 1
          SLL(IK) = 0
          XLL(IK) = DET*(B2*RZ(JJ) - C1*RY(JJ))
        ENDDO
        IAD = IAD + NIR
        DO JJ=1,NIR
          IK = IAD+JJ
          LLL(IK) = INOD(JJ)
          JLL(IK) = 2
          SLL(IK) = 0
          XLL(IK) = DET*(C1*RX(JJ) - C3*RZ(JJ))
        ENDDO
        IAD = IAD + NIR
        DO JJ=1,NIR
          IK = IAD+JJ
          LLL(IK) = INOD(JJ)
          JLL(IK) = 3
          SLL(IK) = 0
          XLL(IK) = DET*(C3*RY(JJ) - B2*RX(JJ))
        ENDDO
        IK = IAD + NIR + 1
        LLL(IK) = INOD(NIR+1)
        JLL(IK) = 5
        SLL(IK) = NINT
        XLL(IK) = -ONE
C ---   ic = 6 (wz)
        NC = NC + 1
        IADLL(NC+1) = IADLL(NC) + NDL
        IAD = IADLL(NC) -1
        DO JJ=1,NIR
          IK = IAD+JJ
          LLL(IK) = INOD(JJ)
          JLL(IK) = 1
          SLL(IK) = 0
          XLL(IK) = DET*(C1*RZ(JJ) - B3*RY(JJ))
        ENDDO
        IAD = IAD + NIR
        DO JJ=1,NIR
          IK = IAD+JJ
          LLL(IK) = INOD(JJ)
          JLL(IK) = 2
          SLL(IK) = 0
          XLL(IK) = DET*(B3*RX(JJ) - C2*RZ(JJ))
        ENDDO
        IAD = IAD + NIR
        DO JJ=1,NIR
          IK = IAD+JJ
          LLL(IK) = INOD(JJ)
          JLL(IK) = 3
          SLL(IK) = 0
          XLL(IK) = DET*(C2*RY(JJ) - C1*RX(JJ))
        ENDDO
        IK = IAD + NIR + 1
        LLL(IK) = INOD(NIR+1)
        JLL(IK) = 6
        SLL(IK) = NINT
        XLL(IK) = -ONE
C---    Solving local Lagrange multipliers
        CALL LAG_DIRECT(
     1           IADLL    ,LLL      ,JLL      ,XLL      ,LTSM     ,
     2           V        ,VR       ,A        ,AR       ,MS       ,
     3           IN       ,NC_INI   ,NCL      )
        IF (COMFLAG==0) THEN
          ISKIP   = ISKIP + NCL
          NC = NC_INI
        ELSE
          IC = NC_INI - NCF_S
          DO K=1,NCL
            IC = IC + 1
            ICFTAG(IC) = IC + ISKIP
            JCFTAG(IC+ISKIP) = NC_INI + K
          ENDDO
        ENDIF
      ENDDO
C---
      RETURN
      END
