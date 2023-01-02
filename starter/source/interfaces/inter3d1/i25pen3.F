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
C
Chd|====================================================================
Chd|  I25PEN3                       source/interfaces/inter3d1/i25pen3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I25PEN3(
     1                  JLT    ,CAND_N ,CAND_E ,PENMIN ,PENMAX,
     2                  X1     ,X2     ,X3     ,X4     ,X0     ,
     3                  Y1     ,Y2     ,Y3     ,Y4     ,Y0     ,
     4                  Z1     ,Z2     ,Z3     ,Z4     ,Z0     ,
     5                  XI     ,YI     ,ZI     ,NSN    ,IX1    ,
     6                  IX2    ,IX3    ,IX4    ,NSVG   ,NRTM   ,
     7                  MSEGLO ,GAPS   ,IRECT  ,IRTLM  ,
     9                  TIME_S ,PENE_OLD,ITAB   ,MSEGTYP,ISHARP,
     A                  NNX     ,NNY    ,NNZ   ,GAP_NM  ,MVOISN,
     B                  GAPMXL  ,IVIS2  ,IBOUND,VTX_BISECTOR)
C
      USE MESSAGE_MOD
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "vect07_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
      INTEGER JLT, NSVG(*), NSN, MSEGLO(*), MSEGTYP(*), NRTM, IVIS2, ISHARP
C     REAL
      my_real
     .   GAPS(*), GAP_NM(4,*), GAPMXL(*)
      INTEGER IRECT(4,*), ITAB(*),CAND_E(*),CAND_N(*),IRTLM(4,*)
      INTEGER IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),
     .        MVOISN(MVSIZ,4), IBOUND(4,MVSIZ)
C     REAL
      my_real
     .   PENMAX,PENMIN,PENE_OLD(5,NSN),TIME_S(NSN)
      my_real
     .     X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ), X0(MVSIZ),
     .     Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ), Y0(MVSIZ),
     .     Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ), Z0(MVSIZ),
     .     XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ),
     .     NNX(MVSIZ,5), NNY(MVSIZ,5), NNZ(MVSIZ,5)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, L, N, IT, N4N, MGLOB, ETYP(MVSIZ),
     .        NINDX, INDX(MVSIZ), I4N(MVSIZ), 
     .        FAR(MVSIZ,4), SUBTRIA(MVSIZ), I1, I2, ITRIA(2,4), JJ
      INTEGER IB1, IB2, IB3, IBX, IX, IY, IZ, NBORD, KBORD(MVSIZ)
C     REAL
      my_real
     .   UNP,ZEROM,EPS,UNPT,ZEROMT,AAA,BB(MVSIZ,4)
      my_real
     .     XIJ(MVSIZ,4),  XI0V(MVSIZ), 
     .     YIJ(MVSIZ,4),  YI0V(MVSIZ), 
     .     ZIJ(MVSIZ,4),  ZI0V(MVSIZ), 
     .     LBH(MVSIZ,4), LCH(MVSIZ,4), LAH(MVSIZ,4), 
     .     DX, DY, DZ, DD(MVSIZ,4), 
     .     XP, YP, ZP,
     .     GAP_MM(MVSIZ), GAPV(MVSIZ,4), GAP2
      my_real
     .     PENE, 
     .     SX1,SX2,SX3,SX4,
     .     SY1,SY2,SY3,SY4,
     .     SZ1,SZ2,SZ3,SZ4,
     .     X0N(MVSIZ,4), Y0N(MVSIZ,4), Z0N(MVSIZ,4), 
     .     XN(MVSIZ,4),YN(MVSIZ,4),ZN(MVSIZ,4),HH(MVSIZ),
     .     SIDE(MVSIZ,4), 
     .     X12(MVSIZ), Y12(MVSIZ), Z12(MVSIZ), 
     .     PX, PY, PZ, PP, LL, NN, PN, VX, VY, VZ,
     .     NY1, NY2, NY3, NY4,
     .     NZ1, NZ2, NZ3, NZ4,
     .     XX(MVSIZ,5),YY(MVSIZ,5),ZZ(MVSIZ,5), 
     .     PENT(MVSIZ,4), DIST(MVSIZ), LB(MVSIZ,4), LC(MVSIZ,4),
     .     LAP, LBP(MVSIZ,4), LCP(MVSIZ,4), HLA, HLB(MVSIZ,4), HLC(MVSIZ,4),
     .     AL(MVSIZ,4),
     .     NN1(MVSIZ), NN2(MVSIZ), NN3(MVSIZ),
     .     LX, LAX, LBX, LCX, LD(MVSIZ), DMIN,
     .     NNE(MVSIZ), XH(MVSIZ), YH(MVSIZ), ZH(MVSIZ), XC, YC, ZC, DC, P1, P2, D1, D2, D3, GAPM
      REAL*4 VTX_BISECTOR(3,2,*)
      DATA ITRIA/1,2,2,3,3,4,4,1/
C-----------------------------------------------
      NINDX=0       
      DO I=1,JLT
        L       = CAND_E(I)
        ETYP(I) =MSEGTYP(L)
        IF(ETYP(I)==0)THEN
C
C         Computing initial penetrations vs solids
          NINDX=NINDX+1
          INDX(NINDX)=I
        ELSEIF((ETYP(I) /= 0 .AND. IABS(ETYP(I)) <= NRTM) .OR. ETYP(I) > NRTM)THEN
C
C         Computing initial penetrations vs shells except opposite segments to the coating shells
          NINDX=NINDX+1
          INDX(NINDX)=I
        END IF
      END DO
C-----------------------------------------------
      IF (IRESP==1.AND.PENMIN<=EM06) PENMIN = TWO*EM06
C--------
      FAR(1:JLT,1:4)    = 0
      DD  (1:JLT,1:4)   = EP20
      DIST(1:JLT)       = EP20
      LD(1:JLT)         = EP20
C--------
      DO K=1,NINDX
        I=INDX(K)

        PENT(I,1:4)=ZERO
C    
        XX(I,1) = X1(I)
        YY(I,1) = Y1(I)
        ZZ(I,1) = Z1(I)
C
        XX(I,2) = X2(I)
        YY(I,2) = Y2(I)
        ZZ(I,2) = Z2(I)
C
        XX(I,3) = X3(I)
        YY(I,3) = Y3(I)
        ZZ(I,3) = Z3(I)
C
        XX(I,4) = X4(I)
        YY(I,4) = Y4(I)
        ZZ(I,4) = Z4(I)
C
        XX(I,5) = X0(I)
        YY(I,5) = Y0(I)
        ZZ(I,5) = Z0(I)

      ENDDO
C--------------------------------------------------------
      DO K=1,NINDX
        I=INDX(K)
C    
        X0N(I,1) = XX(I,1) - XX(I,5)
        Y0N(I,1) = YY(I,1) - YY(I,5)
        Z0N(I,1) = ZZ(I,1) - ZZ(I,5)
C
        X0N(I,2) = XX(I,2) - XX(I,5)
        Y0N(I,2) = YY(I,2) - YY(I,5)
        Z0N(I,2) = ZZ(I,2) - ZZ(I,5)
C
        X0N(I,3) = XX(I,3) - XX(I,5)
        Y0N(I,3) = YY(I,3) - YY(I,5)
        Z0N(I,3) = ZZ(I,3) - ZZ(I,5)
C
        X0N(I,4) = XX(I,4) - XX(I,5)
        Y0N(I,4) = YY(I,4) - YY(I,5)
        Z0N(I,4) = ZZ(I,4) - ZZ(I,5)
C    
        IF(IX3(I)/=IX4(I))THEN
          GAP_MM(I)=FOURTH*(GAP_NM(1,I)+GAP_NM(2,I)+GAP_NM(3,I)+GAP_NM(4,I))
        ELSE
          GAP_MM(I)=GAP_NM(3,I)
        END IF
C
      ENDDO
C--------------------------------------------------------
      DO K=1,NINDX
        I=INDX(K)
C
        XI0V(I) = XX(I,5) - XI(I)
        YI0V(I) = YY(I,5) - YI(I)
        ZI0V(I) = ZZ(I,5) - ZI(I)
C
        XIJ(I,1) = XX(I,1) - XI(I)
        YIJ(I,1) = YY(I,1) - YI(I)
        ZIJ(I,1) = ZZ(I,1) - ZI(I)
C
        XIJ(I,2) = XX(I,2) - XI(I)
        YIJ(I,2) = YY(I,2) - YI(I)
        ZIJ(I,2) = ZZ(I,2) - ZI(I)
C
        XIJ(I,3) = XX(I,3) - XI(I)
        YIJ(I,3) = YY(I,3) - YI(I)
        ZIJ(I,3) = ZZ(I,3) - ZI(I)
C
        XIJ(I,4) = XX(I,4) - XI(I)
        YIJ(I,4) = YY(I,4) - YI(I)
        ZIJ(I,4) = ZZ(I,4) - ZI(I)
C
        SX1 = YI0V(I)*ZIJ(I,1) - ZI0V(I)*YIJ(I,1)
        SY1 = ZI0V(I)*XIJ(I,1) - XI0V(I)*ZIJ(I,1)
        SZ1 = XI0V(I)*YIJ(I,1) - YI0V(I)*XIJ(I,1)
C
        SX2 = YI0V(I)*ZIJ(I,2) - ZI0V(I)*YIJ(I,2)
        SY2 = ZI0V(I)*XIJ(I,2) - XI0V(I)*ZIJ(I,2)
        SZ2 = XI0V(I)*YIJ(I,2) - YI0V(I)*XIJ(I,2)
C
        XN(I,1) = Y0N(I,1)*Z0N(I,2) - Z0N(I,1)*Y0N(I,2)
        YN(I,1) = Z0N(I,1)*X0N(I,2) - X0N(I,1)*Z0N(I,2)
        ZN(I,1) = X0N(I,1)*Y0N(I,2) - Y0N(I,1)*X0N(I,2)
        NN = ONE/
     .       MAX(EM30,SQRT(XN(I,1)*XN(I,1)+ YN(I,1)*YN(I,1)+ ZN(I,1)*ZN(I,1)))
        XN(I,1)=XN(I,1)*NN
        YN(I,1)=YN(I,1)*NN
        ZN(I,1)=ZN(I,1)*NN
        LB(I,1) = -(XN(I,1)*SX2 + YN(I,1)*SY2 + ZN(I,1)*SZ2)*NN
        LC(I,1) =  (XN(I,1)*SX1 + YN(I,1)*SY1 + ZN(I,1)*SZ1)*NN
C
        SX3 = YI0V(I)*ZIJ(I,3) - ZI0V(I)*YIJ(I,3)
        SY3 = ZI0V(I)*XIJ(I,3) - XI0V(I)*ZIJ(I,3)
        SZ3 = XI0V(I)*YIJ(I,3) - YI0V(I)*XIJ(I,3)
C
        XN(I,2) = Y0N(I,2)*Z0N(I,3) - Z0N(I,2)*Y0N(I,3)
        YN(I,2) = Z0N(I,2)*X0N(I,3) - X0N(I,2)*Z0N(I,3)
        ZN(I,2) = X0N(I,2)*Y0N(I,3) - Y0N(I,2)*X0N(I,3)
        NN = ONE/
     .       MAX(EM30,SQRT(XN(I,2)*XN(I,2)+ YN(I,2)*YN(I,2)+ ZN(I,2)*ZN(I,2)))
        XN(I,2)=XN(I,2)*NN
        YN(I,2)=YN(I,2)*NN
        ZN(I,2)=ZN(I,2)*NN
        LB(I,2) = -(XN(I,2)*SX3 + YN(I,2)*SY3 + ZN(I,2)*SZ3)*NN
        LC(I,2) =  (XN(I,2)*SX2 + YN(I,2)*SY2 + ZN(I,2)*SZ2)*NN
C
        SX4 = YI0V(I)*ZIJ(I,4) - ZI0V(I)*YIJ(I,4)
        SY4 = ZI0V(I)*XIJ(I,4) - XI0V(I)*ZIJ(I,4)
        SZ4 = XI0V(I)*YIJ(I,4) - YI0V(I)*XIJ(I,4)
C
        XN(I,3) = Y0N(I,3)*Z0N(I,4) - Z0N(I,3)*Y0N(I,4)
        YN(I,3) = Z0N(I,3)*X0N(I,4) - X0N(I,3)*Z0N(I,4)
        ZN(I,3) = X0N(I,3)*Y0N(I,4) - Y0N(I,3)*X0N(I,4)
        NN = ONE/
     .       MAX(EM30,SQRT(XN(I,3)*XN(I,3)+ YN(I,3)*YN(I,3)+ ZN(I,3)*ZN(I,3)))
        XN(I,3)=XN(I,3)*NN
        YN(I,3)=YN(I,3)*NN
        ZN(I,3)=ZN(I,3)*NN
        LB(I,3) = -(XN(I,3)*SX4 + YN(I,3)*SY4 + ZN(I,3)*SZ4)*NN
        LC(I,3) =  (XN(I,3)*SX3 + YN(I,3)*SY3 + ZN(I,3)*SZ3)*NN
C
        XN(I,4) = Y0N(I,4)*Z0N(I,1) - Z0N(I,4)*Y0N(I,1)
        YN(I,4) = Z0N(I,4)*X0N(I,1) - X0N(I,4)*Z0N(I,1)
        ZN(I,4) = X0N(I,4)*Y0N(I,1) - Y0N(I,4)*X0N(I,1)
        NN = ONE/
     .       MAX(EM30,SQRT(XN(I,4)*XN(I,4)+ YN(I,4)*YN(I,4)+ ZN(I,4)*ZN(I,4)))
        XN(I,4)=XN(I,4)*NN
        YN(I,4)=YN(I,4)*NN
        ZN(I,4)=ZN(I,4)*NN
        LB(I,4) = -(XN(I,4)*SX1 + YN(I,4)*SY1 + ZN(I,4)*SZ1)*NN
        LC(I,4) =  (XN(I,4)*SX4 + YN(I,4)*SY4 + ZN(I,4)*SZ4)*NN
C
      END DO
C--------------------------------------------------------
      DO K=1,NINDX
        I=INDX(K)
C
        AAA      = ONE/MAX(EM30,X0N(I,1)*X0N(I,1)+Y0N(I,1)*Y0N(I,1)+Z0N(I,1)*Z0N(I,1))
        HLC(I,1) = LC(I,1)*ABS(LC(I,1))*AAA
        HLB(I,4) = LB(I,4)*ABS(LB(I,4))*AAA
        AL(I,1)  = -(XI0V(I)*X0N(I,1)+YI0V(I)*Y0N(I,1)+ZI0V(I)*Z0N(I,1))*AAA
        AL(I,1)  = MAX(ZERO,MIN(ONE,AL(I,1)))
        AAA      = ONE/MAX(EM30,X0N(I,2)*X0N(I,2)+Y0N(I,2)*Y0N(I,2)+Z0N(I,2)*Z0N(I,2))
        HLC(I,2) = LC(I,2)*ABS(LC(I,2))*AAA
        HLB(I,1) = LB(I,1)*ABS(LB(I,1))*AAA
        AL(I,2)  = -(XI0V(I)*X0N(I,2)+YI0V(I)*Y0N(I,2)+ZI0V(I)*Z0N(I,2))*AAA
        AL(I,2)  = MAX(ZERO,MIN(ONE,AL(I,2)))
        AAA      = ONE/MAX(EM30,X0N(I,3)*X0N(I,3)+Y0N(I,3)*Y0N(I,3)+Z0N(I,3)*Z0N(I,3))
        HLC(I,3) = LC(I,3)*ABS(LC(I,3))*AAA
        HLB(I,2) = LB(I,2)*ABS(LB(I,2))*AAA
        AL(I,3)  = -(XI0V(I)*X0N(I,3)+YI0V(I)*Y0N(I,3)+ZI0V(I)*Z0N(I,3))*AAA
        AL(I,3)  = MAX(ZERO,MIN(ONE,AL(I,3)))
        AAA      = ONE/MAX(EM30,X0N(I,4)*X0N(I,4)+Y0N(I,4)*Y0N(I,4)+Z0N(I,4)*Z0N(I,4))
        HLC(I,4) = LC(I,4)*ABS(LC(I,4))*AAA
        HLB(I,3) = LB(I,3)*ABS(LB(I,3))*AAA
        AL(I,4)  = -(XI0V(I)*X0N(I,4)+YI0V(I)*Y0N(I,4)+ZI0V(I)*Z0N(I,4))*AAA
        AL(I,4)  = MAX(ZERO,MIN(ONE,AL(I,4)))
C
      END DO
C--------------------------------------------------------
      IT=1
      I1=ITRIA(1,IT)
      I2=ITRIA(2,IT)
      DO K=1,NINDX
        I=INDX(K)
C
        X12(I) = XX(I,I2) - XX(I,I1)
        Y12(I) = YY(I,I2) - YY(I,I1)
        Z12(I) = ZZ(I,I2) - ZZ(I,I1)
C
        LBP(I,IT) = LB(I,IT)
        LCP(I,IT) = LC(I,IT)
        LAP       = ONE-LBP(I,IT)-LCP(I,IT)
C       HLA, HLB, HLC necessaires pour triangle angle obtu
        AAA = ONE / MAX(EM20,X12(I)*X12(I)+Y12(I)*Y12(I)+Z12(I)*Z12(I))
        HLA= LAP*ABS(LAP) * AAA
        IF(LAP<ZERO.AND.
     +     HLA<=HLB(I,IT).AND.HLA<=HLC(I,IT))THEN
         LBP(I,IT) = (XIJ(I,I2)*X12(I)+YIJ(I,I2)*Y12(I)+ZIJ(I,I2)*Z12(I)) * AAA
         LBP(I,IT) = MAX(ZERO,MIN(ONE,LBP(I,IT)))
         LCP(I,IT) = ONE - LBP(I,IT)
        ELSEIF(LBP(I,IT)<ZERO.AND.
     +         HLB(I,IT)<=HLC(I,IT).AND.HLB(I,IT)<=HLA)THEN
         LBP(I,IT) = ZERO
         LCP(I,IT) = AL(I,I2)
        ELSEIF(LCP(I,IT)<ZERO.AND.
     +         HLC(I,IT)<=HLA.AND.HLC(I,IT)<=HLB(I,IT))THEN
         LCP(I,IT) = ZERO
         LBP(I,IT) = AL(I,I1)
        ENDIF

        LAP = ONE-LBP(I,IT)-LCP(I,IT)
        XP  =LAP*XX(I,5)+LBP(I,IT)*XX(I,I1) + LCP(I,IT)*XX(I,I2)
        YP  =LAP*YY(I,5)+LBP(I,IT)*YY(I,I1) + LCP(I,IT)*YY(I,I2)
        ZP  =LAP*ZZ(I,5)+LBP(I,IT)*ZZ(I,I1) + LCP(I,IT)*ZZ(I,I2)
        DX  =XI(I)-XP
        DY  =YI(I)-YP
        DZ  =ZI(I)-ZP
        DD(I,IT)=DX*DX+DY*DY+DZ*DZ

      END DO
C--------------------------------------------------------
      DO K=1,NINDX
        I=INDX(K)
C          
        LAP  = ONE-LBP(I,IT)-LCP(I,IT)
C
        GAPV(I,IT)  = MIN(GAPS(I)+LAP*GAP_MM(I)+LBP(I,IT)*GAP_NM(I1,I)+LCP(I,IT)*GAP_NM(I2,I),GAPMXL(I))
        GAP2 = GAPV(I,IT)**2
C
        BB(I,IT) =((XX(I,5)-XI(I))*XN(I,IT)+(YY(I,5)-YI(I))*YN(I,IT)+(ZZ(I,5)-ZI(I))*ZN(I,IT))

        IF(ETYP(I)==0)THEN
          IF(BB(I,IT) > ZERO)THEN
            PENT(I,IT)=GAPV(I,IT)+BB(I,IT)
          ELSE
            PENT(I,IT)=ZERO
          END IF
        ELSEIF(ETYP(I) > NRTM)THEN ! Coating shell (same orientation than solid)
          PENT(I,IT)=MAX(GAPV(I,IT)+BB(I,IT),ZERO)
        ELSE
          IF(BB(I,IT) > ZERO)THEN
C
C           penetration approximee (cf zone limite interpolation des normales)
            PENT(I,IT)=ZERO
          ELSE
            PENT(I,IT)=MAX(ZERO,GAPV(I,IT)-SQRT(DD(I,IT)))          
          END IF
        END IF
C
      ENDDO
C--------------------------------------------------------
       N4N=0
       DO K=1,NINDX
        I=INDX(K)
C    
        IF(IX3(I)/=IX4(I))THEN
          N4N = N4N+1
          I4N(N4N)=I
        ENDIF
       ENDDO
C--------------------------------------------------------
       DO IT=2,4

         I1=ITRIA(1,IT)
         I2=ITRIA(2,IT)

         DO K=1,N4N
          I=I4N(K)
C
          X12(I) = XX(I,I2) - XX(I,I1)
          Y12(I) = YY(I,I2) - YY(I,I1)
          Z12(I) = ZZ(I,I2) - ZZ(I,I1)
C
          LBP(I,IT) = LB(I,IT)
          LCP(I,IT) = LC(I,IT)
          LAP       = ONE-LBP(I,IT)-LCP(I,IT)
C         HLA, HLB, HLC necessaires pour triangle angle obtu
          AAA = ONE / MAX(EM20,X12(I)*X12(I)+Y12(I)*Y12(I)+Z12(I)*Z12(I))
          HLA= LAP*ABS(LAP) * AAA
          IF(LAP<ZERO.AND.
     +       HLA<=HLB(I,IT).AND.HLA<=HLC(I,IT))THEN
           LBP(I,IT) = (XIJ(I,I2)*X12(I)+YIJ(I,I2)*Y12(I)+ZIJ(I,I2)*Z12(I)) * AAA
           LBP(I,IT) = MAX(ZERO,MIN(ONE,LBP(I,IT)))
           LCP(I,IT) = ONE - LBP(I,IT)
          ELSEIF(LBP(I,IT)<ZERO.AND.
     +           HLB(I,IT)<=HLC(I,IT).AND.HLB(I,IT)<=HLA)THEN
           LBP(I,IT) = ZERO
           LCP(I,IT) = AL(I,I2)
          ELSEIF(LCP(I,IT)<ZERO.AND.
     +           HLC(I,IT)<=HLA.AND.HLC(I,IT)<=HLB(I,IT))THEN
           LCP(I,IT) = ZERO
           LBP(I,IT) = AL(I,I1)
          ENDIF

          LAP = ONE-LBP(I,IT)-LCP(I,IT)
          XP  =LAP*XX(I,5)+LBP(I,IT)*XX(I,I1) + LCP(I,IT)*XX(I,I2)
          YP  =LAP*YY(I,5)+LBP(I,IT)*YY(I,I1) + LCP(I,IT)*YY(I,I2)
          ZP  =LAP*ZZ(I,5)+LBP(I,IT)*ZZ(I,I1) + LCP(I,IT)*ZZ(I,I2)
          DX  =XI(I)-XP
          DY  =YI(I)-YP
          DZ  =ZI(I)-ZP
          DD(I,IT)=DX*DX+DY*DY+DZ*DZ

         END DO

         DO K=1,N4N
          I=I4N(K)
C
          LAP  = ONE-LBP(I,IT)-LCP(I,IT)
C
          GAPV(I,IT) = MIN(GAPS(I)+LAP*GAP_MM(I)+LBP(I,IT)*GAP_NM(I1,I)+LCP(I,IT)*GAP_NM(I2,I),GAPMXL(I))
          GAP2 = GAPV(I,IT)**2
C
          BB(I,IT) =((XX(I,5)-XI(I))*XN(I,IT)+(YY(I,5)-YI(I))*YN(I,IT)+(ZZ(I,5)-ZI(I))*ZN(I,IT))

          L = CAND_E(I)
          IF(ETYP(I)==0)THEN
            IF(BB(I,IT) > ZERO)THEN
              PENT(I,IT)=GAPV(I,IT)+BB(I,IT)
            ELSE
              PENT(I,IT)=ZERO
            END IF
          ELSEIF(ETYP(I) > NRTM)THEN ! Coating shell (same orientation than solid)
            PENT(I,IT)=MAX(GAPV(I,IT)+BB(I,IT),ZERO)
          ELSE
            IF(BB(I,IT) > ZERO)THEN
C
C             penetration approximee (cf zone limite interpolation des normales)
              PENT(I,IT)=ZERO
            ELSE
              PENT(I,IT)=MAX(ZERO,GAPV(I,IT)-SQRT(DD(I,IT)))          
            END IF
          END IF
C
         ENDDO
       END DO !  DO IT=2,4
C--------------------------------------------------------
       DO K=1,NINDX
         I=INDX(K)
C
C        Erase Subtria (cf intersections)
         SUBTRIA(I)=0
C
         IF(IX3(I)/=IX4(I))THEN
           DMIN=MIN(DD(I,1),DD(I,2),DD(I,3),DD(I,4))
           DO JJ=1,4
             IF(DD(I,JJ) <= ONEP03*DMIN)THEN
               LBX = LB(I,JJ)
               LCX = LC(I,JJ)
               LAX = ONE-LB(I,JJ)-LC(I,JJ)
C
C              Privilegier secteur dans lequel on se trouve.
               IF(LBX >= ZERO .AND. LCX >= ZERO)THEN
                 LX=ZERO
               ELSE
                 LX=MAX(ZERO,DD(I,JJ)-BB(I,JJ)*BB(I,JJ))
               END IF

               IF(LX < LD(I)) THEN
                 SUBTRIA(I)= JJ
                 DIST(I)     = DD(I,JJ)
                 LD(I)=LX
               END IF

             END IF
           END DO
         ELSE
           IF(DD(I,1) <= DIST(I))THEN
             SUBTRIA(I)= 1
             DIST(I)   = DD(I,1)
           END IF
         END IF
C
         IT = SUBTRIA(I)
         DO J=1,4
           IF(J /= IT) PENT(I,J)=ZERO
         END DO
C
       END DO
C--------------------------------------------------------
C     Look for nodes projecting out of the free edges <=> Zero Penetration
C--------------------------------------------------------
      DO K=1,NINDX
        I=INDX(K)
C
        IT=SUBTRIA(I)
C       IF(PENT(I,IT)==ZERO) CYCLE ! In Starter, needs to check for FAR=2

        I1=ITRIA(1,IT)
        I2=ITRIA(2,IT)
C
        XH(I)=XI(I)+BB(I,IT)*XN(I,IT)
        YH(I)=YI(I)+BB(I,IT)*YN(I,IT)
        ZH(I)=ZI(I)+BB(I,IT)*ZN(I,IT)
C
C       Projection en dehors du triangle
        IF(LB(I,IT) < -EM03 .OR. LC(I,IT) < -EM03 .OR.
     .      LB(I,IT)+LC(I,IT) > ONE+EM03) FAR(I,IT)=1
C
        IF(IX3(I) /= IX4(I))THEN
C
          IB1=IBOUND(I1,I)
          IB2=IBOUND(I2,I)
          IF(MVOISN(I,IT)==0)THEN     
C
            IF( (XH(I)-XX(I,I1))* NNX(I,IT)+
     .          (YH(I)-YY(I,I1))* NNY(I,IT)+
     .          (ZH(I)-ZZ(I,I1))* NNZ(I,IT) >= GAPS(I)) FAR(I,IT) =2
C
          ELSEIF((IB1 /= 0 .AND. IB2 == 0).OR.
     .           (IB2 /= 0 .AND. IB1 == 0))THEN
C
            IBX=MAX(IB1,IB2)
            IF(IB1/=0)THEN
              IX =I1
            ELSEIF(IB2/=0)THEN
              IX =I2
            END IF
C
            IF(VTX_BISECTOR(1,1,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(2,1,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(3,1,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(1,2,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(2,2,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(3,2,IBX)/=ZERO)THEN
              P1 = (XH(I)-XX(I,IX))* VTX_BISECTOR(1,1,IBX)+
     .             (YH(I)-YY(I,IX))* VTX_BISECTOR(2,1,IBX)+
     .             (ZH(I)-ZZ(I,IX))* VTX_BISECTOR(3,1,IBX)
              P2 = (XH(I)-XX(I,IX))* VTX_BISECTOR(1,2,IBX)+
     .             (YH(I)-YY(I,IX))* VTX_BISECTOR(2,2,IBX)+
     .             (ZH(I)-ZZ(I,IX))* VTX_BISECTOR(3,2,IBX)
              IF(P1 >= GAPS(I) .AND. P2 >= GAPS(I)) FAR(I,IT) =2
            ELSE
              VX = X0N(I,IX) ! fake bisector of angle at vertex IX
              VY = Y0N(I,IX)
              VZ = Z0N(I,IX)
              NN = ONE/MAX(EM20,SQRT(VX*VX+VY*VY+VZ*VZ))
              PN = ((XH(I)-XX(I,IX))*VX+(YH(I)-YY(I,IX))*VY+(ZH(I)-ZZ(I,IX))*VZ)*NN
              IF(PN >= GAPS(I)) FAR(I,IT) =2
            END IF
C
          END IF
C
          IF(FAR(I,IT)==1 .OR. BB(I,IT) <= ZERO)THEN     
C          
            X12(I)=XX(I,I2)-XX(I,I1)
            Y12(I)=YY(I,I2)-YY(I,I1)
            Z12(I)=ZZ(I,I2)-ZZ(I,I1)
C
C           normal to the bisecting plane (pointing toward the inside)
            PX = Z12(I)*NNY(I,IT)-Y12(I)*NNZ(I,IT)  
            PY = X12(I)*NNZ(I,IT)-Z12(I)*NNX(I,IT)  
            PZ = Y12(I)*NNX(I,IT)-X12(I)*NNY(I,IT)  
            PP = PX*PX+PY*PY+PZ*PZ
C
            LL  = XIJ(I,I1)*XIJ(I,I1)+YIJ(I,I1)*YIJ(I,I1)+ZIJ(I,I1)*ZIJ(I,I1)
C
            SIDE(I,IT)=-(XIJ(I,I1)*PX+YIJ(I,I1)*PY+ZIJ(I,I1)*PZ)*SQRT(ONE/MAX(EM30,LL*PP))
C
C           Projette a l'exterieur et Hors secteur angulaire (cf bissectrices) <=> Glissement
            IF(SIDE(I,IT) < -ZEP01) FAR(I,IT) =2
C
          END IF
C
        ELSE
C
          IB1=IBOUND(1,I)
          IB2=IBOUND(2,I)
          IB3=IBOUND(3,I)
C
          D1=(XH(I)-XX(I,1))* NNX(I,1)+
     .       (YH(I)-YY(I,1))* NNY(I,1)+
     .       (ZH(I)-ZZ(I,1))* NNZ(I,1)
          D2=(XH(I)-XX(I,2))* NNX(I,2)+
     .       (YH(I)-YY(I,2))* NNY(I,2)+
     .       (ZH(I)-ZZ(I,2))* NNZ(I,2)
          D3=(XH(I)-XX(I,3))* NNX(I,4)+
     .       (YH(I)-YY(I,3))* NNY(I,4)+
     .       (ZH(I)-ZZ(I,3))* NNZ(I,4)
C
          IF( (MVOISN(I,1) == 0 .AND. D1 >= GAPS(I)).OR.
     .        (MVOISN(I,2) == 0 .AND. D2 >= GAPS(I)).OR.
     .        (MVOISN(I,4) == 0 .AND. D3 >= GAPS(I)) )THEN
C
            FAR(I,IT)=2
C
          ELSEIF((IB1/=0 .AND. IB2==0 .AND. IB3==0).OR.
     .           (IB2/=0 .AND. IB3==0 .AND. IB1==0).OR.
     .           (IB3/=0 .AND. IB1==0 .AND. IB2==0))THEN
C
            IBX=MAX(IB1,IB2,IB3)
            IF(IB1/=0)THEN
              IX =1
              IY =2
              IZ =3
            ELSEIF(IB2/=0)THEN
              IX =2
              IY =3
              IZ =1
            ELSEIF(IB3/=0)THEN
              IX =3
              IY =1
              IZ =2
            END IF
C
            IF(VTX_BISECTOR(1,1,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(2,1,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(3,1,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(1,2,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(2,2,IBX)/=ZERO.OR.
     .         VTX_BISECTOR(3,2,IBX)/=ZERO)THEN
              P1 = (XH(I)-XX(I,IX))* VTX_BISECTOR(1,1,IBX)+
     .             (YH(I)-YY(I,IX))* VTX_BISECTOR(2,1,IBX)+
     .             (ZH(I)-ZZ(I,IX))* VTX_BISECTOR(3,1,IBX)
              P2 = (XH(I)-XX(I,IX))* VTX_BISECTOR(1,2,IBX)+
     .             (YH(I)-YY(I,IX))* VTX_BISECTOR(2,2,IBX)+
     .             (ZH(I)-ZZ(I,IX))* VTX_BISECTOR(3,2,IBX)
              IF(P1 >= GAPS(I) .AND. P2 >= GAPS(I)) FAR(I,IT) =2
            ELSE
              VX = TWO*XX(I,IX)-(XX(I,IY)+XX(I,IZ)) ! fake bisector of angle 2,1,3
              VY = TWO*YY(I,IX)-(YY(I,IY)+YY(I,IZ))
              VZ = TWO*ZZ(I,IX)-(ZZ(I,IY)+ZZ(I,IZ))
              NN = ONE/MAX(EM20,SQRT(VX*VX+VY*VY+VZ*VZ))
              PN = ((XH(I)-XX(I,IX))*VX+(YH(I)-YY(I,IX))*VY+(ZH(I)-ZZ(I,IX))*VZ)*NN
              IF(PN >= GAPS(I)) FAR(I,IT) =2
            END IF
C
          END IF
C
          IF(FAR(I,IT)==1 .OR. BB(I,IT) <= ZERO)THEN

            IF(MVOISN(I,1) /= 0)THEN
C
C             INGAP = 1 => tjrs regarder si l'on est dans le cone (permet de determiner dans quel cone on se trouve) 
C
C             Glissement => utiliser le cone pour choisir entre les segments voisins
C                           mais garder le contact si le nd a glisse de plus d'un elt (cf supersonic)
C          
              X12(I)=XX(I,2)-XX(I,1)
              Y12(I)=YY(I,2)-YY(I,1)
              Z12(I)=ZZ(I,2)-ZZ(I,1)
C
C             normal to the bisecting plane (pointing toward the inside)
              PX = Z12(I)*NNY(I,1)-Y12(I)*NNZ(I,1)  
              PY = X12(I)*NNZ(I,1)-Z12(I)*NNX(I,1)  
              PZ = Y12(I)*NNX(I,1)-X12(I)*NNY(I,1)  
              PP = PX*PX+PY*PY+PZ*PZ
C
              LL  = XIJ(I,1)*XIJ(I,1)+YIJ(I,1)*YIJ(I,1)+ZIJ(I,1)*ZIJ(I,1)
C
              SIDE(I,1)=-(XIJ(I,1)*PX+YIJ(I,1)*PY+ZIJ(I,1)*PZ)*SQRT(ONE/MAX(EM30,LL*PP))             
              IF(SIDE(I,1) < -ZEP01) FAR(I,IT) =2
C
            END IF

            IF(MVOISN(I,2) /= 0)THEN
C
C             INGAP = 1 => tjrs regarder si l'on est dans le cone (permet de determiner dans quel cone on se trouve) 
C
C             Glissement => utiliser le cone pour choisir entre les segments voisins
C                           mais garder le contact si le nd a glisse de plus d'un elt (cf supersonic)
C            
              X12(I)=XX(I,3)-XX(I,2)
              Y12(I)=YY(I,3)-YY(I,2)
              Z12(I)=ZZ(I,3)-ZZ(I,2)
C
C             normal to the bisecting plane (pointing toward the inside)
              PX = Z12(I)*NNY(I,2)-Y12(I)*NNZ(I,2)  
              PY = X12(I)*NNZ(I,2)-Z12(I)*NNX(I,2)  
              PZ = Y12(I)*NNX(I,2)-X12(I)*NNY(I,2)  
              PP = PX*PX+PY*PY+PZ*PZ
C
              LL  = XIJ(I,2)*XIJ(I,2)+YIJ(I,2)*YIJ(I,2)+ZIJ(I,2)*ZIJ(I,2)
C
              SIDE(I,2)=-(XIJ(I,2)*PX+YIJ(I,2)*PY+ZIJ(I,2)*PZ)*SQRT(ONE/MAX(EM30,LL*PP))             
              IF(SIDE(I,2) < -ZEP01) FAR(I,IT) =2
C
            END IF

            IF(MVOISN(I,4) /= 0)THEN
C
C               INGAP = 1 => tjrs regarder si l'on est dans le cone (permet de determiner dans quel cone on se trouve) 
C
C               Glissement => utiliser le cone pour choisir entre les segments voisins
C                             mais garder le contact si le nd a glisse de plus d'un elt (cf supersonic)
C            
              X12(I)=XX(I,1)-XX(I,3)
              Y12(I)=YY(I,1)-YY(I,3)
              Z12(I)=ZZ(I,1)-ZZ(I,3)
C
C             normal to the bisecting plane (pointing toward the inside)
              PX = Z12(I)*NNY(I,4)-Y12(I)*NNZ(I,4)  
              PY = X12(I)*NNZ(I,4)-Z12(I)*NNX(I,4)  
              PZ = Y12(I)*NNX(I,4)-X12(I)*NNY(I,4)  
              PP = PX*PX+PY*PY+PZ*PZ
C
              LL  =  XIJ(I,3)*XIJ(I,3)+YIJ(I,3)*YIJ(I,3)+ZIJ(I,3)*ZIJ(I,3)
C
              SIDE(I,4)=-(XIJ(I,3)*PX+YIJ(I,3)*PY+ZIJ(I,3)*PZ)*SQRT(ONE/MAX(EM30,LL*PP))             
              IF(SIDE(I,4) < -ZEP01) FAR(I,IT) =2
C
            END IF
          END IF
C
        END IF
        IF(FAR(I,IT)==2) PENT(I,IT)=ZERO
C
C-------------------------------------------
c        if(itab(nsvg(i))==2810875.or.
c     .     itab(ix1(i))==2810875.or.itab(ix2(i))==2810875.or.itab(ix3(i))==2810875.or.itab(ix4(i))==2810875)
c     . print *,'avant',itab(nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),mvoisn(i,1:4),
c     .   pent(i,it)
C-------------------------------------------

      ENDDO
C--------------------------------------------------------
C     Exact value of the penetration close to the edges still needs to be computed
C--------------------------------------------------------
      NBORD=0
      DO K=1,NINDX
        I=INDX(K)
C
        IT=SUBTRIA(I)
        I1=ITRIA(1,IT)
        I2=ITRIA(2,IT)
C
        IF(PENT(I,IT)==ZERO) CYCLE
C
        IF(IX3(I) /= IX4(I))THEN
C
          IB1=IBOUND(I1,I)
          IB2=IBOUND(I2,I)
C
          IF(MVOISN(I,IT)==0)THEN     
C
C           Distance signee a l'arete
C
C upper skin      --------------------
C                                     |
C                            I        |
C                       -BB: |        |
C                            |   NNE  |
C neutral fiber - - - C - -  H < - - >
C                     <-------------->|
C                          Gap        |
C                                     |
C                 --------------------
C
            NN1(I)=NNX(I,IT)
            NN2(I)=NNY(I,IT)
            NN3(I)=NNZ(I,IT)
            NNE(I)= (XH(I)-XX(I,I1))*NN1(I)+ (YH(I)-YY(I,I1))*NN2(I)+ (ZH(I)-ZZ(I,I1))*NN3(I)
C
            NBORD=NBORD+1
            KBORD(NBORD)=I
C
          ELSEIF((IB1/=0.AND.IB2==0).OR.
     .           (IB2/=0.AND.IB1==0))THEN
C
            IBX=MAX(IB1,IB2)
            IF(IB1/=0)THEN
              IX =I1
            ELSEIF(IB2/=0)THEN
              IX =I2
            END IF
C
            P1 = (XH(I)-XX(I,IX))* VTX_BISECTOR(1,1,IBX)+
     .           (YH(I)-YY(I,IX))* VTX_BISECTOR(2,1,IBX)+
     .           (ZH(I)-ZZ(I,IX))* VTX_BISECTOR(3,1,IBX)
            P2 = (XH(I)-XX(I,IX))* VTX_BISECTOR(1,2,IBX)+
     .           (YH(I)-YY(I,IX))* VTX_BISECTOR(2,2,IBX)+
     .           (ZH(I)-ZZ(I,IX))* VTX_BISECTOR(3,2,IBX)
C
            IF(P1 < GAPS(I) .AND. P2 < GAPS(I))THEN
C
              NN1(I)=VTX_BISECTOR(1,1,IBX)+VTX_BISECTOR(1,2,IBX)
              NN2(I)=VTX_BISECTOR(2,1,IBX)+VTX_BISECTOR(2,2,IBX)
              NN3(I)=VTX_BISECTOR(3,1,IBX)+VTX_BISECTOR(3,2,IBX)
C
              NN=SQRT(NN1(I)*NN1(I)+NN2(I)*NN2(I)+NN3(I)*NN3(I))
              NN = ONE/MAX(EM30,NN)
              NN1(I)=NN1(I)*NN
              NN2(I)=NN2(I)*NN
              NN3(I)=NN3(I)*NN
C
              NNE(I)= (XH(I)-XX(I,IX))*NN1(I)+ (YH(I)-YY(I,IX))*NN2(I)+ (ZH(I)-ZZ(I,IX))*NN3(I)
C
              NBORD=NBORD+1
              KBORD(NBORD)=I
C
            ELSEIF(P1 < GAPS(I))THEN
              
              NN1(I)= VTX_BISECTOR(1,1,IBX)
              NN2(I)= VTX_BISECTOR(2,1,IBX)
              NN3(I)= VTX_BISECTOR(3,1,IBX)
              NNE(I)= (XH(I)-XX(I,IX))*NN1(I)+ (YH(I)-YY(I,IX))*NN2(I)+ (ZH(I)-ZZ(I,IX))*NN3(I)
C
              NBORD=NBORD+1
              KBORD(NBORD)=I

            ELSEIF(P2 < GAPS(I))THEN 
              
              NN1(I)= VTX_BISECTOR(1,2,IBX)
              NN2(I)= VTX_BISECTOR(2,2,IBX)
              NN3(I)= VTX_BISECTOR(3,2,IBX)
              NNE(I)= (XH(I)-XX(I,IX))*NN1(I)+ (YH(I)-YY(I,IX))*NN2(I)+ (ZH(I)-ZZ(I,IX))*NN3(I)
C
              NBORD=NBORD+1
              KBORD(NBORD)=I

            ELSE

c              print *,' ** possible internal error wrt p1,p2 in i25pen3',ib1,ib2,
c     .        itab(nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),p1,p2

            END IF

          END IF
C
        ELSE ! IF(IX3(I) /= IX4(I))THEN

          IB1=IBOUND(1,I)
          IB2=IBOUND(2,I)
          IB3=IBOUND(3,I)
C
C         Projection sur le plan et Distance signee au plan
          XH(I)=XI(I)+BB(I,IT)*XN(I,IT)
          YH(I)=YI(I)+BB(I,IT)*YN(I,IT)
          ZH(I)=ZI(I)+BB(I,IT)*ZN(I,IT)
C
          IF(MVOISN(I,1)==0.OR.
     .       MVOISN(I,2)==0.OR.
     .       MVOISN(I,4)==0)THEN

            NNE(I)=GAPS(I)
            IF(MVOISN(I,1)==0)THEN
C
              NN = (XH(I)-XX(I,I1))*NNX(I,I1)+(YH(I)-YY(I,I1))*NNY(I,I1)+(ZH(I)-ZZ(I,I1))*NNZ(I,I1)

              IF(NN < NNE(I)) THEN
                NNE(I)=NN
                NN1(I)=NNX(I,I1)
                NN2(I)=NNY(I,I1)
                NN3(I)=NNZ(I,I1)                
C
                NBORD=NBORD+1
                KBORD(NBORD)=I

              END IF
C
            END IF
C
            IF(MVOISN(I,2)==0)THEN
C
C             Distance signee a l'arete
              NN = (XH(I)-XX(I,I2))*NNX(I,I2)+(YH(I)-YY(I,I2))*NNY(I,I2)+(ZH(I)-ZZ(I,I2))*NNZ(I,I2)

              IF(NN < NNE(I)) THEN
                NNE(I)=NN
                NN1(I)=NNX(I,I2)
                NN2(I)=NNY(I,I2)
                NN3(I)=NNZ(I,I2)                
C
                NBORD=NBORD+1
                KBORD(NBORD)=I

              END IF
C
            END IF
C
            IF(MVOISN(I,4)==0)THEN
C
C             Distance signee a l'arete
              NN = (XH(I)-XX(I,5))*NNX(I,5)+(YH(I)-YY(I,5))*NNY(I,5)+(ZH(I)-ZZ(I,5))*NNZ(I,5)

              IF(NN < NNE(I)) THEN
                NNE(I)=NN
                NN1(I)=NNX(I,5)
                NN2(I)=NNY(I,5)
                NN3(I)=NNZ(I,5)                
C
                NBORD=NBORD+1
                KBORD(NBORD)=I

              END IF
C
            END IF
C
          ELSEIF((IB1/=0 .AND. IB2==0 .AND. IB3==0).OR.
     .           (IB2/=0 .AND. IB3==0 .AND. IB1==0).OR.
     .           (IB3/=0 .AND. IB1==0 .AND. IB2==0))THEN
C
            IBX=MAX(IB1,IB2,IB3)
            IF(IB1/=0)THEN
              IX =1
            ELSEIF(IB2/=0)THEN
              IX =2
            ELSEIF(IB3/=0)THEN
              IX =3
            END IF
C
            P1 = (XH(I)-XX(I,IX))* VTX_BISECTOR(1,1,IBX)+
     .           (YH(I)-YY(I,IX))* VTX_BISECTOR(2,1,IBX)+
     .           (ZH(I)-ZZ(I,IX))* VTX_BISECTOR(3,1,IBX)
            P2 = (XH(I)-XX(I,IX))* VTX_BISECTOR(1,2,IBX)+
     .           (YH(I)-YY(I,IX))* VTX_BISECTOR(2,2,IBX)+
     .           (ZH(I)-ZZ(I,IX))* VTX_BISECTOR(3,2,IBX)

            IF(P1 < GAPS(I) .AND. P2 < GAPS(I))THEN
C
              NN1(I)=VTX_BISECTOR(1,1,IBX)+VTX_BISECTOR(1,2,IBX)
              NN2(I)=VTX_BISECTOR(2,1,IBX)+VTX_BISECTOR(2,2,IBX)
              NN3(I)=VTX_BISECTOR(3,1,IBX)+VTX_BISECTOR(3,2,IBX)
C
              NN=SQRT(NN1(I)*NN1(I)+NN2(I)*NN2(I)+NN3(I)*NN3(I))
              NN = ONE/MAX(EM30,NN)
              NN1(I)=NN1(I)*NN
              NN2(I)=NN2(I)*NN
              NN3(I)=NN3(I)*NN
C
              NNE(I)= (XH(I)-XX(I,IX))*NN1(I)+ (YH(I)-YY(I,IX))*NN2(I)+ (ZH(I)-ZZ(I,IX))*NN3(I)
C
              NBORD=NBORD+1
              KBORD(NBORD)=I
C
            ELSEIF(P1 < GAPS(I))THEN
              
              NN1(I)= VTX_BISECTOR(1,1,IBX)
              NN2(I)= VTX_BISECTOR(2,1,IBX)
              NN3(I)= VTX_BISECTOR(3,1,IBX)
              NNE(I)= (XH(I)-XX(I,IX))*NN1(I)+ (YH(I)-YY(I,IX))*NN2(I)+ (ZH(I)-ZZ(I,IX))*NN3(I)
C
              NBORD=NBORD+1
              KBORD(NBORD)=I

            ELSEIF(P2 < GAPS(I))THEN 
              
              NN1(I)= VTX_BISECTOR(1,2,IBX)
              NN2(I)= VTX_BISECTOR(2,2,IBX)
              NN3(I)= VTX_BISECTOR(3,2,IBX)
              NNE(I)= (XH(I)-XX(I,IX))*NN1(I)+ (YH(I)-YY(I,IX))*NN2(I)+ (ZH(I)-ZZ(I,IX))*NN3(I)
C
              NBORD=NBORD+1
              KBORD(NBORD)=I

            ELSE

c              print *,' ** possible internal error wrt p1,p2 in i25pen3',ib1,ib2,
c     .        itab(nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),p1,p2

            END IF
          END IF

        END IF ! IF(IX3(I) /= IX4(I))THEN
      END DO 
C-------------------------------------------
      IF(ISHARP==1)THEN

        DO K=1,NBORD
          I=KBORD(K)

          IT=SUBTRIA(I)
          I1=ITRIA(1,IT)
          I2=ITRIA(2,IT)

          GAPM = GAPV(I,IT)-GAPS(I)
          IF(NNE(I) > ZERO .AND. BB(I,IT)+GAPM < ZERO)THEN
C
C           ___________________________ xxx
C                                      |xxxxxx     
C            GapS                      |xxxxxxxx   <=> Upper Round Corner
C           ___________________________|xxxxxxxx
C                                      |       |
C            GapM                      |       |   <=> Straight shape of the edge
C           ---------------------------M-------
C                                      <------> 
C                                         GapS
C

C
            LAP  = ONE-LBP(I,IT)-LCP(I,IT)
            XP=LAP*XX(I,5)+LBP(I,IT)*XX(I,I1) + LCP(I,IT)*XX(I,I2)
            YP=LAP*YY(I,5)+LBP(I,IT)*YY(I,I1) + LCP(I,IT)*YY(I,I2)
            ZP=LAP*ZZ(I,5)+LBP(I,IT)*ZZ(I,I1) + LCP(I,IT)*ZZ(I,I2)
            XC=XP+GAPM*XN(I,IT)
            YC=YP+GAPM*YN(I,IT)
            ZC=ZP+GAPM*ZN(I,IT)
            NN1(I) =XI(I)-XC
            NN2(I) =YI(I)-YC
            NN3(I) =ZI(I)-ZC
            DC = SQRT(NN1(I)*NN1(I)+ NN2(I)*NN2(I)+ NN3(I)*NN3(I))

            IF(DC > EM04) THEN
C             NN = ONE/DC
C             N1(I) = N1(I)*NN
C             N2(I) = N2(I)*NN
C             N3(I) = N3(I)*NN
              PENT(I,IT)=MAX(ZERO,GAPS(I)-DC)
            ELSE
C
              NNE(I)=NNE(I)-GAPS(I)
              IF(-BB(I,IT) < GAPV(I,IT)+NNE(I))THEN ! Test 45 degres !
C
C               Normale horizonale
C               N1(I) = NN1(I)
C               N2(I) = NN2(I)
C               N3(I) = NN3(I)
C
                PENT(I,IT)=MAX(ZERO,-NNE(I))
C                
              ELSE
C
C               Normale verticale !
C               N1(I) = XN(I,IT)
C               N2(I) = YN(I,IT)
C               N3(I) = ZN(I,IT)
C
                PENT(I,IT)=MAX(ZERO,GAPV(I,IT)+BB(I,IT))
              END IF
            END IF

          ELSE !IF(NNE(I) > ZERO .AND. BB(I)+GAPM < ZERO)THEN
C
            NNE(I)=NNE(I)-GAPS(I)
            IF(NNE(I) >= ZERO)THEN
C
C             Must remain a roundoff error <=> NNE ~ 0
c             IF(NSVG(I) > 0)THEN
c               print *,' ** possible internal error in i25dst3-3, nne should be negative',
c     .         itab(nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),'nne =',nne(I),pent(i,it)
c             ELSE
c               print *,' ** possible internal error in i25dst3-3, nne should be negative',
c     .         itafi(nin)%p(-nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),'nne =',nne(I),pent(i,it)
c             END IF

              PENT(I,IT)=ZERO
              CYCLE

            END IF

            IF(GAPV(I,IT)+NNE(I) > ZERO)THEN
C
              IF(-BB(I,IT) < GAPV(I,IT)+NNE(I))THEN
C
C               Normale horizonale
C               N1(I) = NN1(I)
C               N2(I) = NN2(I)
C               N3(I) = NN3(I)
C
                PENT(I,IT)=-NNE(I)
C                
              ELSE
C
C               Normale verticale !
C               N1(I) = XN(I,IT)
C               N2(I) = YN(I,IT)
C               N3(I) = ZN(I,IT)
C
                PENT(I,IT)=MAX(ZERO,GAPV(I,IT)+BB(I,IT))
              END IF

            END IF

          END IF

        END DO

      ELSEIF(ISHARP==2)THEN

        DO K=1,NBORD
          I=KBORD(K)

          IT=SUBTRIA(I)
          I1=ITRIA(1,IT)
          I2=ITRIA(2,IT)

          NNE(I)=NNE(I)-GAPS(I)
          IF(NNE(I) >= ZERO)THEN
C
C           Must remain a roundoff error <=> NNE ~ 0
c            IF(NSVG(I) > 0)THEN
c              print *,' ** possible internal error in i25dst3-3, nne should be negative',
c     .        itab(nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),'nne =',nne(I)
c            ELSE
c              print *,' ** possible internal error in i25dst3-3, nne should be negative',
c     .        itafi(nin)%p(-nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),'nne =',nne(I)
c            END IF

            CYCLE

          END IF
C
          IF(GAPV(I,IT)+NNE(I) > ZERO)THEN
C
C           Normale radiale shiftee
            XC =XH(I)-(GAPV(I,IT)+NNE(I))*NN1(I)
            YC =YH(I)-(GAPV(I,IT)+NNE(I))*NN2(I)
            ZC =ZH(I)-(GAPV(I,IT)+NNE(I))*NN3(I)
            NN1(I) =XI(I)-XC
            NN2(I) =YI(I)-YC
            NN3(I) =ZI(I)-ZC
            DC = SQRT(NN1(I)*NN1(I)+ NN2(I)*NN2(I)+ NN3(I)*NN3(I))
            IF(DC > EM04) THEN
C             NN = ONE/DC
C             N1(I) = NN1(I)*NN
C             N2(I) = NN2(I)*NN
C             N3(I) = NN3(I)*NN
              PENT(I,IT)=MAX(ZERO,GAPV(I,IT)-DC)
            ELSE
C
C             Keep what was done in the general case : Normale ~ verticale.
            END IF

          END IF
C-------------------------------------------
c        if(itab(nsvg(i))==2810875.or.
c     .     itab(ix1(i))==2810875.or.itab(ix2(i))==2810875.or.itab(ix3(i))==2810875.or.itab(ix4(i))==2810875)
c     . print *,'apres',itab(nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),mvoisn(i,1:4),
c     .   it,kbord,gapv(i,it),nne(i),bb(i,it),pent(i,it)
C-------------------------------------------
        END DO

      END IF ! IF(ISHARP==1)THEN
C--------------------------------------------------------
      IF(IVIS2/=-1) THEN

        DO K=1,NINDX
          I=INDX(K)

          IT  = SUBTRIA(I)

          IF(ETYP(I) > NRTM)THEN
            IF(FAR(I,IT)==2) CYCLE ! retain closest segment whose cone includes the SECONDARY node
                                   ! => same impact (no sliding) at time=0 in RD Engine
            ! Case of coating shell, penetration may be larger than the gap
          ELSEIF(ETYP(I)/=0)THEN
            IF(FAR(I,IT)==2) CYCLE ! retain closest segment whose cone includes the SECONDARY node
                                   ! => same impact (no sliding) at time=0 in RD Engine
            IF(BB(I,IT) > ZERO) CYCLE
          END IF

          N=CAND_N(I)
          L=CAND_E(I)

          MGLOB = MSEGLO(L)

          PENE=PENT(I,IT) ! PENE /= ZERO .OR. DIST(I) < PENMIN**2
C       if(itab(nsvg(i))==299344.or.itab(ix1(i))==299344.or.itab(ix2(i))==299344
C    .   .or.itab(ix3(i))==299344.or.itab(ix4(i))==299344)
C    .      print *,itab(nsvg(i)),itab(ix1(i)),itab(ix2(i)),itab(ix3(i)),itab(ix4(i)),
C    .      it,etyp(i),nrtm,pent(i,it),far(i,it),dist(i)
C 
C         Looking for penetration vs closest segment (cf initial penetrations vs solids)
          IF(DIST(I) < TIME_S(N) .OR.
     *             (DIST(I) == TIME_S(N) .AND. IRTLM(1,N) < MGLOB))THEN
            IF(FAR(I,IT)==0 .AND. ABS(DD(I,IT)-GAPV(I,IT)*GAPV(I,IT)) < PENMIN**2)THEN
              IRTLM(1,N)=MGLOB
              IRTLM(2,N)=IT
              IRTLM(3,N)=L
              TIME_S(N)    =DIST(I)
              PENE_OLD(5,N)=PENE
            ELSE
              IRTLM(1,N)=MGLOB
              IRTLM(2,N)=IT
              IRTLM(3,N)=L
              TIME_S(N)    =DIST(I)
              PENE_OLD(5,N)=PENE
            END IF
          END IF  
        END DO
      ELSE ! adhesion case, pene(5,n) datum is at 0.5*gapv, if pene < 0.5*gapv then pene_old(5,n) = 0 

        DO K=1,NINDX
          I=INDX(K)

          IT  = SUBTRIA(I)
          IF(FAR(I,IT)==2)THEN ! retain closest segment whose cone includes the SECONDARY node
                               ! => same impact (no sliding) at time=0 in RD Engine
            IT = 0
            SUBTRIA(I) = IT
          END IF
          IF(IT == 0)CYCLE
 
          N=CAND_N(I)
          L=CAND_E(I)
 
          MGLOB = MSEGLO(L)
 
          IF(.NOT.(ETYP(I)==0.OR.MSEGTYP(L) > NRTM))THEN
            IF(PENT(I,IT)==ZERO) CYCLE
          END IF

          PENE=MAX(ZERO,PENT(I,IT)-HALF*GAPV(I,IT)) ! PENE /= ZERO .OR. DIST(I) < PENMIN**2
C 
C         Looking for penetration vs closest segment (cf initial penetrations vs solids)
          IF(DIST(I) < TIME_S(N) .OR.
     *             (DIST(I) == TIME_S(N) .AND. IRTLM(1,N) < MGLOB))THEN
            IF(FAR(I,IT)==0 .AND. ABS(DD(I,IT)-GAPV(I,IT)*GAPV(I,IT)) < PENMIN**2)THEN
              IRTLM(1,N)=MGLOB
              IRTLM(2,N)=IT
              IRTLM(3,N)=L
              TIME_S(N)    =PENE
              PENE_OLD(5,N)=PENE
            ELSE
              IRTLM(1,N)=MGLOB
              IRTLM(2,N)=IT
              IRTLM(3,N)=L
              TIME_S(N)    =PENE
              PENE_OLD(5,N)=PENE
            END IF
          END IF     
        END DO
      ENDIF ! ivis2 if
C
      RETURN
      END
