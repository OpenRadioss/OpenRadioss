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
Chd|  CSUBC3                        source/elements/shell/coque/csubc3.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|-- calls ---------------
Chd|        CBILAN                        source/elements/shell/coque/cbilan.F
Chd|====================================================================
      SUBROUTINE CSUBC3(JFT  ,JLT ,X   ,IXC ,GEO ,OFFG ,
     2                  OFF ,SIGY,PID ,D   ,DR   ,D0  ,
     3                  DR0 , VL1, VL2, VL3, VL4,
     3                  VRL1, VRL2, VRL3, VRL4 ,NDT  ,PM  ,V ,
     4                  THK ,EINT,PARTSAV,T0  ,DTC ,AREA,
     5                  VR  ,MAT  ,NGL,DT1C,DT2C,THKE,THK0,
     6                  X1  ,X2  ,X3  ,X4  ,Y1  ,Y2    ,
     7                  Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4    ,
     8                  IPARTC ,IADC ,FSKY ,FSKYV,GRESAV,
     9                  GRTH,IGRTH,ITASK)
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
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr07_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGL(MVSIZ), NDT(*),PID(*),MAT(MVSIZ), IXC(NIXC,*),
     .                  IPARTC(*),JFT, JLT, IADC(4,*),GRTH(*),
     .                  IGRTH(*),ITASK
C     REAL
      my_real
     .   X(3,*), GEO(NPROPG,*), OFFG(*), OFF(*), SIGY(*),
     .   D(3,*),DR(3,*),D0(MVSIZ,3,4),DR0(MVSIZ,3,4),PM(*),PARTSAV(*),
     .   VL1(MVSIZ,3),VL2(MVSIZ,3),VL3(MVSIZ,3),VL4(MVSIZ,3),
     .   VRL1(MVSIZ,3),VRL2(MVSIZ,3),VRL3(MVSIZ,3),VRL4(MVSIZ,3),
     .   THK(*), EINT(JLT,2),V(3,*),T0(*),
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ), Y1(MVSIZ),
     .   Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ), Z1(MVSIZ), Z2(MVSIZ),
     .   Z3(MVSIZ), Z4(MVSIZ), THK0(MVSIZ),
     .   DTC(*),AREA(*),VR(3,*),DT1C(*),DT2C(*),THKE(*),
     .   FSKYV(LSKY,8), FSKY(8,LSKY),GRESAV(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, JFTN, JLTN, IFLAG, K, IBID
C     REAL
      my_real
     .     TTN, FAC, BID, ZEROUN
      INTEGER IXCTMP2, IXCTMP3, IXCTMP4, IXCTMP5
       ZEROUN = EM01
       IBID = 0
       BID = ZERO
C-----------------------------------------------
      IF(ISUB.EQ.1)THEN
C-------------------------------
C       DT PAR GROUPE D'ELEMENT
C-------------------------------
        IF(DT1.NE.0.0)THEN
         IF(T0(1)+DTC(1).GT.TT + 1.2*DT1)THEN
C
          IFLAG=MOD(NCYCLE,NCPRI)
          IF(IFLAG.EQ.0.OR.TT.GE.THIS.OR.TT.GE.TDES.
     +        OR.TT.GE.TPAT.OR.MDESS.NE.0.
     +   OR.TT.GE.THIS1(1).OR.TT.GE.THIS1(2).
     +   OR.TT.GE.THIS1(3).OR.TT.GE.THIS1(4).OR.TT.GE.THIS1(5).
     +   OR.TT.GE.THIS1(6).OR.TT.GE.THIS1(7).OR.TT.GE.THIS1(8).
     +   OR.TT.GE.THIS1(9).OR.NTH.NE.0.OR.NANIM.NE.0          .
     +   OR.TT.GE.TABFIS(1).OR.TT.GE.TABFIS(2).
     +   OR.TT.GE.TABFIS(3).OR.TT.GE.TABFIS(4).OR.TT.GE.TABFIS(5).
     +   OR.TT.GE.TABFIS(6).OR.TT.GE.TABFIS(7).OR.TT.GE.TABFIS(8).
     +   OR.TT.GE.TABFIS(10))THEN
            DO I=JFT,JLT
              MAT(I)=IXC(1,I)
            ENDDO
            CALL CBILAN(JFT,JLT,PM   ,V   ,IXC,
     2                  THK,EINT  ,PARTSAV,AREA,MAT,
     3                  IPARTC ,BID,BID,BID,BID,
     4                  BID,BID, 0,OFF,
     5                  IBID,GRESAV,GRTH,IGRTH,
     6                  VL1 ,VL2  ,VL3  ,VL4 ,VRL1 , 
     7                  VRL2,VRL3 ,VRL4 ,X1  ,X2   , 
     8                  X3  ,X4   ,Y1   ,Y2  ,Y3   , 
     9                  Y4  ,Z1   ,Z2   ,Z3  ,Z4   ,
     A                  IBID,IBID ,BID  ,ITASK)
          ENDIF
          IF(IPARIT.NE.0)THEN
           IF (IVECTOR.EQ.1) THEN
#include "vectorize.inc"
            DO I=JFT,JLT
             FSKYV(IADC(1,I),1)=ZERO
             FSKYV(IADC(1,I),2)=ZERO
             FSKYV(IADC(1,I),3)=ZERO
             FSKYV(IADC(1,I),4)=ZERO
             FSKYV(IADC(1,I),5)=ZERO
             FSKYV(IADC(1,I),6)=ZERO
             FSKYV(IADC(1,I),7)=ZERO
             FSKYV(IADC(1,I),8)=ZERO
C
             FSKYV(IADC(2,I),1)=ZERO
             FSKYV(IADC(2,I),2)=ZERO
             FSKYV(IADC(2,I),3)=ZERO
             FSKYV(IADC(2,I),4)=ZERO
             FSKYV(IADC(2,I),5)=ZERO
             FSKYV(IADC(2,I),6)=ZERO
             FSKYV(IADC(2,I),7)=ZERO
             FSKYV(IADC(2,I),8)=ZERO
C
             FSKYV(IADC(3,I),1)=ZERO
             FSKYV(IADC(3,I),2)=ZERO
             FSKYV(IADC(3,I),3)=ZERO
             FSKYV(IADC(3,I),4)=ZERO
             FSKYV(IADC(3,I),5)=ZERO
             FSKYV(IADC(3,I),6)=ZERO
             FSKYV(IADC(3,I),7)=ZERO
             FSKYV(IADC(3,I),8)=ZERO
C
             FSKYV(IADC(4,I),1)=ZERO
             FSKYV(IADC(4,I),2)=ZERO
             FSKYV(IADC(4,I),3)=ZERO
             FSKYV(IADC(4,I),4)=ZERO
             FSKYV(IADC(4,I),5)=ZERO
             FSKYV(IADC(4,I),6)=ZERO
             FSKYV(IADC(4,I),7)=ZERO
             FSKYV(IADC(4,I),8)=ZERO     
            ENDDO
           ELSE
            DO I=JFT,JLT
             K = IADC(1,I)
             FSKY(1,K)=ZERO
             FSKY(2,K)=ZERO
             FSKY(3,K)=ZERO
             FSKY(4,K)=ZERO
             FSKY(5,K)=ZERO
             FSKY(6,K)=ZERO
             FSKY(7,K)=ZERO
             FSKY(8,K)=ZERO
C
             K = IADC(2,I)
             FSKY(1,K)=ZERO
             FSKY(2,K)=ZERO
             FSKY(3,K)=ZERO
             FSKY(4,K)=ZERO
             FSKY(5,K)=ZERO
             FSKY(6,K)=ZERO
             FSKY(7,K)=ZERO
             FSKY(8,K)=ZERO
C
             K = IADC(3,I)
             FSKY(1,K)=ZERO
             FSKY(2,K)=ZERO
             FSKY(3,K)=ZERO
             FSKY(4,K)=ZERO
             FSKY(5,K)=ZERO
             FSKY(6,K)=ZERO
             FSKY(7,K)=ZERO
             FSKY(8,K)=ZERO
C
             K = IADC(4,I)
             FSKY(1,K)=ZERO
             FSKY(2,K)=ZERO
             FSKY(3,K)=ZERO
             FSKY(4,K)=ZERO
             FSKY(5,K)=ZERO
             FSKY(6,K)=ZERO
             FSKY(7,K)=ZERO
             FSKY(8,K)=ZERO     
            ENDDO
           ENDIF
          ENDIF
C
          JLT = 0
          RETURN
C
         ENDIF
        ENDIF
      ELSEIF(ISUB.EQ.2)THEN
C-------------------------
C       DT PAR ELEMENT
C-------------------------
        IF(DT1.NE.0.0)THEN
          JFTN = MVSIZ
          JLTN = 0
          TTN = TT + 1.2*DT1
          DO I=JFT,JLT
           IF(T0(I)+DTC(I).LE.TTN)THEN
            JFTN = MIN(JFTN,I)
            JLTN = I
           ENDIF
          ENDDO
C
          IFLAG=MOD(NCYCLE,NCPRI)
          IF(IFLAG.EQ.0.OR.TT.GE.THIS.OR.TT.GE.TDES.OR.TT.GE.TPAT.
     +   OR.MDESS.NE.0.OR.TT.GE.THIS1(1).OR.TT.GE.THIS1(2).
     +   OR.TT.GE.THIS1(3).OR.TT.GE.THIS1(4).OR.TT.GE.THIS1(5).
     +   OR.TT.GE.THIS1(6).OR.TT.GE.THIS1(7).OR.TT.GE.THIS1(8).
     +   OR.TT.GE.THIS1(9).OR.NTH.NE.0.OR.NANIM.NE.0          .
     +   OR.TT.GE.TABFIS(1).OR.TT.GE.TABFIS(2).
     .   OR.TT.GE.TABFIS(3).OR.TT.GE.TABFIS(4).OR.TT.GE.TABFIS(5).
     .   OR.TT.GE.TABFIS(6).OR.TT.GE.TABFIS(7).OR.TT.GE.TABFIS(8).
     .   OR.TT.GE.TABFIS(10))THEN
C
            IF(JLTN.LT.JFTN)THEN
              DO I=JFT,JLT
                MAT(I)=IXC(1,I)
              ENDDO
              CALL CBILAN(JFT,JLT,PM   ,V   ,IXC,
     2                  THK,EINT  ,PARTSAV,AREA,MAT,
     3                  IPARTC ,BID,BID,BID,BID,
     4                  BID,BID, 0,OFF,
     5                  IBID,GRESAV,GRTH,IGRTH,
     6                  VL1 ,VL2  ,VL3  ,VL4 ,VRL1 , 
     7                  VRL2,VRL3 ,VRL4 ,X1  ,X2   , 
     8                  X3  ,X4   ,Y1   ,Y2  ,Y3   , 
     9                  Y4  ,Z1   ,Z2   ,Z3  ,Z4   ,
     A                  IBID,IBID ,BID  ,ITASK)
            ELSE
             IF(JFTN.GT.JFT)THEN
              DO I=JFT,JFTN-1
                MAT(I)=IXC(1,I)
              ENDDO
              CALL CBILAN(JFT,JFTN-1,PM   ,V   ,IXC,
     2                  THK,EINT  ,PARTSAV,AREA,MAT,
     3                  IPARTC ,BID,BID,BID,BID,
     4                  BID,BID, 0,OFF,
     5                  IBID,GRESAV,GRTH,IGRTH,
     6                  VL1 ,VL2  ,VL3  ,VL4 ,VRL1 , 
     7                  VRL2,VRL3 ,VRL4 ,X1  ,X2   , 
     8                  X3  ,X4   ,Y1   ,Y2  ,Y3   , 
     9                  Y4  ,Z1   ,Z2   ,Z3  ,Z4   ,
     A                  IBID,IBID ,BID  ,ITASK)
             ENDIF
C
             IF(JLT .GT.JLTN)THEN
              DO I=JLTN+1,JLT
                MAT(I)=IXC(1,I)
              ENDDO
              CALL CBILAN(JLTN+1,JLT ,PM   ,V   ,IXC,
     2                  THK   ,EINT,PARTSAV,AREA,MAT,
     3                  IPARTC ,BID,BID,BID,BID,
     4                  BID,BID, 0,OFF,
     5                  IBID,GRESAV,GRTH,IGRTH,
     6                  VL1 ,VL2  ,VL3  ,VL4 ,VRL1 , 
     7                  VRL2,VRL3 ,VRL4 ,X1  ,X2   , 
     8                  X3  ,X4   ,Y1   ,Y2  ,Y3   , 
     9                  Y4  ,Z1   ,Z2   ,Z3  ,Z4   ,
     A                  IBID,IBID ,BID  ,ITASK)
             ENDIF
            ENDIF
C
          ENDIF
C
          JFT = JFTN
          JLT = JLTN
          IF(JLT.LT.JFT) RETURN
        ENDIF
      ENDIF
C
      DO I=JFT,JLT
       MAT(I)=IXC(1,I)
       IXCTMP2=IXC(2,I)
       IXCTMP3=IXC(3,I)
       IXCTMP4=IXC(4,I)
       IXCTMP5=IXC(5,I)
       NGL(I)=IXC(7,I)
       PID(I)=IXC(6,I)
       THK0(I) = THKE(I)
C----------------------------
C     COORDONNEES
C----------------------------
       X1(I)=X(1,IXCTMP2)
       Y1(I)=X(2,IXCTMP2)
       Z1(I)=X(3,IXCTMP2)
       X2(I)=X(1,IXCTMP3)
       Y2(I)=X(2,IXCTMP3)
       Z2(I)=X(3,IXCTMP3)
       X3(I)=X(1,IXCTMP4)
       Y3(I)=X(2,IXCTMP4)
       Z3(I)=X(3,IXCTMP4)
       X4(I)=X(1,IXCTMP5)
       Y4(I)=X(2,IXCTMP5)
       Z4(I)=X(3,IXCTMP5)
      ENDDO
C
      DO I=JFT,JLT
       OFF(I) = MIN(ONE,OFFG(I))
       OFF(I) = MAX(ZERO,OFF(I))
       SIGY(I) = EP30
      ENDDO
C
      IF(DT1.EQ.ZERO)THEN
        DO I=JFT,JLT
         IXCTMP2=IXC(2,I)
         IXCTMP3=IXC(3,I)
         IXCTMP4=IXC(4,I)
         IXCTMP5=IXC(5,I)
         VL1(I,1)=V(1,IXCTMP2)
         VL1(I,2)=V(2,IXCTMP2)
         VL1(I,3)=V(3,IXCTMP2)
         VL2(I,1)=V(1,IXCTMP3)
         VL2(I,2)=V(2,IXCTMP3)
         VL2(I,3)=V(3,IXCTMP3)
         VL3(I,1)=V(1,IXCTMP4)
         VL3(I,2)=V(2,IXCTMP4)
         VL3(I,3)=V(3,IXCTMP4)
         VL4(I,1)=V(1,IXCTMP5)
         VL4(I,2)=V(2,IXCTMP5)
         VL4(I,3)=V(3,IXCTMP5)
         VRL1(I,1)=VR(1,IXCTMP2)
         VRL1(I,2)=VR(2,IXCTMP2)
         VRL1(I,3)=VR(3,IXCTMP2)
         VRL2(I,1)=VR(1,IXCTMP3)
         VRL2(I,2)=VR(2,IXCTMP3)
         VRL2(I,3)=VR(3,IXCTMP3)
         VRL3(I,1)=VR(1,IXCTMP4)
         VRL3(I,2)=VR(2,IXCTMP4)
         VRL3(I,3)=VR(3,IXCTMP4)
         VRL4(I,1)=VR(1,IXCTMP5)
         VRL4(I,2)=VR(2,IXCTMP5)
         VRL4(I,3)=VR(3,IXCTMP5)
         NDT(I)  = 1
         DT1C(I) = ZERO
         DT2C(I) = EP06
        ENDDO
      ELSEIF(ISUB.EQ.1)THEN
C-------------------------------
C       DT PAR GROUPE D'ELEMENT
C-------------------------------
        DT1C(1) = TT-T0(1)
        DT2C(1) = ONEP1*DTC(1)
        NDT(1)  = NINT(DT1C(1) / DT1)
        T0(1)   = TT
        IF(NDT(1).EQ.1)THEN
         DO I=JFT,JLT
          IXCTMP2=IXC(2,I)
          IXCTMP3=IXC(3,I)
          IXCTMP4=IXC(4,I)
          IXCTMP5=IXC(5,I)
          DT1C(I) = DT1
          VL1(I,1)=V(1,IXCTMP2)
          VL1(I,2)=V(2,IXCTMP2)
          VL1(I,3)=V(3,IXCTMP2)
          VL2(I,1)=V(1,IXCTMP3)
          VL2(I,2)=V(2,IXCTMP3)
          VL2(I,3)=V(3,IXCTMP3)
          VL3(I,1)=V(1,IXCTMP4)
          VL3(I,2)=V(2,IXCTMP4)
          VL3(I,3)=V(3,IXCTMP4)
          VL4(I,1)=V(1,IXCTMP5)
          VL4(I,2)=V(2,IXCTMP5)
          VL4(I,3)=V(3,IXCTMP5)
          VRL1(I,1)=VR(1,IXCTMP2)
          VRL1(I,2)=VR(2,IXCTMP2)
          VRL1(I,3)=VR(3,IXCTMP2)
          VRL2(I,1)=VR(1,IXCTMP3)
          VRL2(I,2)=VR(2,IXCTMP3)
          VRL2(I,3)=VR(3,IXCTMP3)
          VRL3(I,1)=VR(1,IXCTMP4)
          VRL3(I,2)=VR(2,IXCTMP4)
          VRL3(I,3)=VR(3,IXCTMP4)
          VRL4(I,1)=VR(1,IXCTMP5)
          VRL4(I,2)=VR(2,IXCTMP5)
          VRL4(I,3)=VR(3,IXCTMP5)
          D0(I,1,1)  =  D(1,IXCTMP2)
          D0(I,2,1)  =  D(2,IXCTMP2)
          D0(I,3,1)  =  D(3,IXCTMP2)
          D0(I,1,2)  =  D(1,IXCTMP3)
          D0(I,2,2)  =  D(2,IXCTMP3)
          D0(I,3,2)  =  D(3,IXCTMP3)
          D0(I,1,3)  =  D(1,IXCTMP4)
          D0(I,2,3)  =  D(2,IXCTMP4)
          D0(I,3,3)  =  D(3,IXCTMP4)
          D0(I,1,4)  =  D(1,IXCTMP5)
          D0(I,2,4)  =  D(2,IXCTMP5)
          D0(I,3,4)  =  D(3,IXCTMP5)
          DR0(I,1,1) =  DR(1,IXCTMP2)
          DR0(I,2,1) =  DR(2,IXCTMP2)
          DR0(I,3,1) =  DR(3,IXCTMP2)
          DR0(I,1,2) =  DR(1,IXCTMP3)
          DR0(I,2,2) =  DR(2,IXCTMP3)
          DR0(I,3,2) =  DR(3,IXCTMP3)
          DR0(I,1,3) =  DR(1,IXCTMP4)
          DR0(I,2,3) =  DR(2,IXCTMP4)
          DR0(I,3,3) =  DR(3,IXCTMP4)
          DR0(I,1,4) =  DR(1,IXCTMP5)
          DR0(I,2,4) =  DR(2,IXCTMP5)
          DR0(I,3,4) =  DR(3,IXCTMP5)
         ENDDO
        ELSE
         FAC   = ONE/DT1C(1)
         DO I=JFT,JLT
          IXCTMP2=IXC(2,I)
          IXCTMP3=IXC(3,I)
          IXCTMP4=IXC(4,I)
          IXCTMP5=IXC(5,I)
          DT1C(I) = DT1C(1)
          VL1(I,1)  = (D(1,IXCTMP2) -D0(I,1,1) )*FAC
          D0(I,1,1)  =  D(1,IXCTMP2)
          VL1(I,2)  = (D(2,IXCTMP2) -D0(I,2,1) )*FAC
          D0(I,2,1)  =  D(2,IXCTMP2)
          VL1(I,3)  = (D(3,IXCTMP2) -D0(I,3,1) )*FAC
          D0(I,3,1)  =  D(3,IXCTMP2)
          VL2(I,1)  = (D(1,IXCTMP3) -D0(I,1,2) )*FAC
          D0(I,1,2)  =  D(1,IXCTMP3)
          VL2(I,2)  = (D(2,IXCTMP3) -D0(I,2,2) )*FAC
          D0(I,2,2)  =  D(2,IXCTMP3)
          VL2(I,3)  = (D(3,IXCTMP3) -D0(I,3,2) )*FAC
          D0(I,3,2)  =  D(3,IXCTMP3)
          VL3(I,1)  = (D(1,IXCTMP4) -D0(I,1,3) )*FAC
          D0(I,1,3)  =  D(1,IXCTMP4)
          VL3(I,2)  = (D(2,IXCTMP4) -D0(I,2,3) )*FAC
          D0(I,2,3)  =  D(2,IXCTMP4)
          VL3(I,3)  = (D(3,IXCTMP4) -D0(I,3,3) )*FAC
          D0(I,3,3)  =  D(3,IXCTMP4)
          VL4(I,1)  = (D(1,IXCTMP5) -D0(I,1,4) )*FAC
          D0(I,1,4)  =  D(1,IXCTMP5)
          VL4(I,2)  = (D(2,IXCTMP5) -D0(I,2,4) )*FAC
          D0(I,2,4)  =  D(2,IXCTMP5)
          VL4(I,3)  = (D(3,IXCTMP5) -D0(I,3,4) )*FAC
          D0(I,3,4)  =  D(3,IXCTMP5)
          VRL1(I,1) = (DR(1,IXCTMP2)-DR0(I,1,1))*FAC
          DR0(I,1,1) =  DR(1,IXCTMP2)
          VRL1(I,2) = (DR(2,IXCTMP2)-DR0(I,2,1))*FAC
          DR0(I,2,1) =  DR(2,IXCTMP2)
          VRL1(I,3) = (DR(3,IXCTMP2)-DR0(I,3,1))*FAC
          DR0(I,3,1) =  DR(3,IXCTMP2)
          VRL2(I,1) = (DR(1,IXCTMP3)-DR0(I,1,2))*FAC
          DR0(I,1,2) =  DR(1,IXCTMP3)
          VRL2(I,2) = (DR(2,IXCTMP3)-DR0(I,2,2))*FAC
          DR0(I,2,2) =  DR(2,IXCTMP3)
          VRL2(I,3) = (DR(3,IXCTMP3)-DR0(I,3,2))*FAC
          DR0(I,3,2) =  DR(3,IXCTMP3)
          VRL3(I,1) = (DR(1,IXCTMP4)-DR0(I,1,3))*FAC
          DR0(I,1,3) =  DR(1,IXCTMP4)
          VRL3(I,2) = (DR(2,IXCTMP4)-DR0(I,2,3))*FAC
          DR0(I,2,3) =  DR(2,IXCTMP4)
          VRL3(I,3) = (DR(3,IXCTMP4)-DR0(I,3,3))*FAC
          DR0(I,3,3) =  DR(3,IXCTMP4)
          VRL4(I,1) = (DR(1,IXCTMP5)-DR0(I,1,4))*FAC
          DR0(I,1,4) =  DR(1,IXCTMP5)
          VRL4(I,2) = (DR(2,IXCTMP5)-DR0(I,2,4))*FAC
          DR0(I,2,4) =  DR(2,IXCTMP5)
          VRL4(I,3) = (DR(3,IXCTMP5)-DR0(I,3,4))*FAC
          DR0(I,3,4) =  DR(3,IXCTMP5)
         ENDDO
        ENDIF
      ELSEIF(ISUB.EQ.2)THEN
C-------------------------
C       DT PAR ELEMENT
C-------------------------
        TTN = TT + SIX_OVER_5*DT1
        DO I=JFT,JLT
         IF(T0(I)+DTC(I).LE.TTN)THEN
          IXCTMP2=IXC(2,I)
          IXCTMP3=IXC(3,I)
          IXCTMP4=IXC(4,I)
          IXCTMP5=IXC(5,I)
          DT1C(I) = TT-T0(I)
          DT2C(I) = ONEP1*DTC(I)
          NDT(I)  = NINT(DT1C(I) / DT1)
          T0(I)   = TT
          FAC = ONE/DT1C(I)
          VL1(I,1)  = (D(1,IXCTMP2) -D0(I,1,1) )*FAC
          D0(I,1,1)  =  D(1,IXCTMP2)
          VL1(I,2)  = (D(2,IXCTMP2) -D0(I,2,1) )*FAC
          D0(I,2,1)  =  D(2,IXCTMP2)
          VL1(I,3)  = (D(3,IXCTMP2) -D0(I,3,1) )*FAC
          D0(I,3,1)  =  D(3,IXCTMP2)
          VL2(I,1)  = (D(1,IXCTMP3) -D0(I,1,2) )*FAC
          D0(I,1,2)  =  D(1,IXCTMP3)
          VL2(I,2)  = (D(2,IXCTMP3) -D0(I,2,2) )*FAC
          D0(I,2,2)  =  D(2,IXCTMP3)
          VL2(I,3)  = (D(3,IXCTMP3) -D0(I,3,2) )*FAC
          D0(I,3,2)  =  D(3,IXCTMP3)
          VL3(I,1)  = (D(1,IXCTMP4) -D0(I,1,3) )*FAC
          D0(I,1,3)  =  D(1,IXCTMP4)
          VL3(I,2)  = (D(2,IXCTMP4) -D0(I,2,3) )*FAC
          D0(I,2,3)  =  D(2,IXCTMP4)
          VL3(I,3)  = (D(3,IXCTMP4) -D0(I,3,3) )*FAC
          D0(I,3,3)  =  D(3,IXCTMP4)
          VL4(I,1)  = (D(1,IXCTMP5) -D0(I,1,4) )*FAC
          D0(I,1,4)  =  D(1,IXCTMP5)
          VL4(I,2)  = (D(2,IXCTMP5) -D0(I,2,4) )*FAC
          D0(I,2,4)  =  D(2,IXCTMP5)
          VL4(I,3)  = (D(3,IXCTMP5) -D0(I,3,4) )*FAC
          D0(I,3,4)  =  D(3,IXCTMP5)
          VRL1(I,1) = (DR(1,IXCTMP2)-DR0(I,1,1))*FAC
          DR0(I,1,1) =  DR(1,IXCTMP2)
          VRL1(I,2) = (DR(2,IXCTMP2)-DR0(I,2,1))*FAC
          DR0(I,2,1) =  DR(2,IXCTMP2)
          VRL1(I,3) = (DR(3,IXCTMP2)-DR0(I,3,1))*FAC
          DR0(I,3,1) =  DR(3,IXCTMP2)
          VRL2(I,1) = (DR(1,IXCTMP3)-DR0(I,1,2))*FAC
          DR0(I,1,2) =  DR(1,IXCTMP3)
          VRL2(I,2) = (DR(2,IXCTMP3)-DR0(I,2,2))*FAC
          DR0(I,2,2) =  DR(2,IXCTMP3)
          VRL2(I,3) = (DR(3,IXCTMP3)-DR0(I,3,2))*FAC
          DR0(I,3,2) =  DR(3,IXCTMP3)
          VRL3(I,1) = (DR(1,IXCTMP4)-DR0(I,1,3))*FAC
          DR0(I,1,3) =  DR(1,IXCTMP4)
          VRL3(I,2) = (DR(2,IXCTMP4)-DR0(I,2,3))*FAC
          DR0(I,2,3) =  DR(2,IXCTMP4)
          VRL3(I,3) = (DR(3,IXCTMP4)-DR0(I,3,3))*FAC
          DR0(I,3,3) =  DR(3,IXCTMP4)
          VRL4(I,1) = (DR(1,IXCTMP5)-DR0(I,1,4))*FAC
          DR0(I,1,4) =  DR(1,IXCTMP5)
          VRL4(I,2) = (DR(2,IXCTMP5)-DR0(I,2,4))*FAC
          DR0(I,2,4) =  DR(2,IXCTMP5)
          VRL4(I,3) = (DR(3,IXCTMP5)-DR0(I,3,4))*FAC
          DR0(I,3,4) =  DR(3,IXCTMP5)
         ELSE
          VL1(I,1)  = ZERO
          VL1(I,2)  = ZERO
          VL1(I,3)  = ZERO
          VL2(I,1)  = ZERO
          VL2(I,2)  = ZERO
          VL2(I,3)  = ZERO
          VL3(I,1)  = ZERO
          VL3(I,2)  = ZERO
          VL3(I,3)  = ZERO
          VL4(I,1)  = ZERO
          VL4(I,2)  = ZERO
          VL4(I,3)  = ZERO
          VRL1(I,1) = ZERO
          VRL1(I,2) = ZERO
          VRL1(I,3) = ZERO
          VRL2(I,1) = ZERO
          VRL2(I,2) = ZERO
          VRL2(I,3) = ZERO
          VRL3(I,1) = ZERO
          VRL3(I,2) = ZERO
          VRL3(I,3) = ZERO
          VRL4(I,1) = ZERO
          VRL4(I,2) = ZERO
          VRL4(I,3) = ZERO
          NDT(I)  = 0
          DT1C(I) = ZERO
         ENDIF
        ENDDO
      ENDIF
C
      RETURN
C
      END
