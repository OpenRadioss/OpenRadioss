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
Chd|  I25PEN3E                      source/interfaces/inter3d1/i25pen3e.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I25PEN3E(
     1   JLT   ,IEDGE  ,CAND_S,CAND_M,
     2   N1    ,N2     ,M1    ,M2    ,
     3   XXS1  ,XXS2   ,XYS1  ,XYS2  ,
     4   XZS1   ,XZS2  ,XXM1  ,XXM2  ,XYM1  ,
     5   XYM2   ,XZM1  ,XZM2  ,GAPVE ,PENE  ,
     6   EX    ,EY     ,EZ    ,FX    ,FY    ,
     7   FZ    ,LEDGE  ,IRECT ,X     ,ITAB  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, IEDGE
      INTEGER CAND_S(*), CAND_M(*), IRECT(4,*), LEDGE(NLEDGE,*), ITAB(*),
     .        N1(MVSIZ),N2(MVSIZ),M1(MVSIZ),M2(MVSIZ)
      my_real
     .     XXS1(*), XXS2(*), XYS1(*), XYS2(*), XZS1(*) , XZS2(*), 
     .     XXM1(*), XXM2(*) , XYM1(*), XYM2(*), XZM1(*), XZM2(*),
     .     GAPVE(*), PENE(*), X(3,*),
     .     EX(MVSIZ), EY(MVSIZ), EZ(MVSIZ), FX(MVSIZ), FY(MVSIZ), FZ(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IA, JA, IB, JB, SOL_EDGE, SH_EDGE, K, NJNDX, N4A, N4B,
     .        IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        JX1(MVSIZ), JX2(MVSIZ), JX3(MVSIZ), JX4(MVSIZ),
     .        JNDX(MVSIZ), I4A(MVSIZ), I4B(MVSIZ)
      my_real
     .     H1S(MVSIZ),H2S(MVSIZ),H1M(MVSIZ),H2M(MVSIZ),NX(MVSIZ),NY(MVSIZ),NZ(MVSIZ),
     .     XS12,YS12,ZS12,XM12,YM12,ZM12,XA,XB,
     .     XS2,XM2,XSM,XS2M2,YS2,YM2,YSM,YS2M2,ZS2,ZM2,ZSM,ZS2M2,
     .     XX,YY,ZZ,ALS,ALM,DET,AAA,BBB,NN(MVSIZ),P1,P2
      my_real
     .     XA0(MVSIZ),XA1(MVSIZ),XA2(MVSIZ),XA3(MVSIZ),XA4(MVSIZ),
     .     YA0(MVSIZ),YA1(MVSIZ),YA2(MVSIZ),YA3(MVSIZ),YA4(MVSIZ),
     .     ZA0(MVSIZ),ZA1(MVSIZ),ZA2(MVSIZ),ZA3(MVSIZ),ZA4(MVSIZ),
     .     XB0(MVSIZ),XB1(MVSIZ),XB2(MVSIZ),XB3(MVSIZ),XB4(MVSIZ),
     .     YB0(MVSIZ),YB1(MVSIZ),YB2(MVSIZ),YB3(MVSIZ),YB4(MVSIZ),
     .     ZB0(MVSIZ),ZB1(MVSIZ),ZB2(MVSIZ),ZB3(MVSIZ),ZB4(MVSIZ)
      my_real
     .     X0A(MVSIZ,4),Y0A(MVSIZ,4),Z0A(MVSIZ,4),
     .     X0B(MVSIZ,4),Y0B(MVSIZ,4),Z0B(MVSIZ,4),
     .     XNA(MVSIZ,4), YNA(MVSIZ,4), ZNA(MVSIZ,4), XNB(MVSIZ,4), YNB(MVSIZ,4), ZNB(MVSIZ,4), 
     .     XS, YS, ZS, XM, YM, ZM, DA, DB, CNVX, DA1, DB1, DA2, DB2,
     .     RZERO, RUN, RDIX, REM30, REP30,
     .     ALP,XXS,XYS,XZS,
     .     XI0,YI0,ZI0,XI1,YI1,ZI1,XI2,YI2,ZI2,
     .     SX1,SY1,SZ1,SX2,SY2,SZ2
      INTEGER NTRIA(3,4)
      DATA NTRIA/1,2,4,2,4,1,0,0,0,4,1,2/
C-----------------------------------------------
C       F = [A*X1+(1-A)*X2-B*X3-(1-B)*X4]^2 + [..Y..]^2 + [..Z..]^2
C       DF/DA = 0 = (X1-X2)(A(X1-X2)+X2-X4 +B(X4-X3))+...
C       DF/DA = 0 = A(X1-X2)^2 +X2-X4 + B(X1-X2)(X4-X3))+...
C       DF/DA = 0 = A[(X1-X2)^2 + (Y1-Y2)^2 + (Z1-Z2)^2] 
C                 + B[(X1-X2)(X4-X3) + (Y1-Y2)(Y4-Y3) + (Z1-Z2)(Z4-Z3)]
C                 +   (X1-X2)(X2-X4) + (Y1-Y2)(Y2-Y4) + (Z1-Z2)(Z2-Z4) 
C       DF/DB = 0 = (X4-X3)(A(X1-X2)+X2-X4 +B(X4-X3))+...
C       DF/DB = 0 = B[(X4-X3)^2 + (Y4-Y3)^2 + (Z4-Z3)^2] 
C                 + A[(X1-X2)(X4-X3) + (Y1-Y2)(Y4-Y3) + (Z1-Z2)(Z4-Z3)]
C                 +   (X4-X3)(X2-X4) + (Y4-Y3)(Y2-Y4) + (Z4-Z3)(Z2-Z4) 
C       XS2 = [(X1-X2)^2 + (Y1-Y2)^2 + (Z1-Z2)^2]
C       XM2 = [(X4-X3)^2 + (Y4-Y3)^2 + (Z4-Z3)^2]
C       XSM = [(X1-X2)(X4-X3) + (Y1-Y2)(Y4-Y3) + (Z1-Z2)(Z4-Z3)]
C       XA = (X1-X2)(X2-X4) + (Y1-Y2)(Y2-Y4) + (Z1-Z2)(Z2-Z4)
C       XB = (X4-X3)(X2-X4) + (Y4-Y3)(Y2-Y4) + (Z4-Z3)(Z2-Z4)
C       A XS2 + B XSM +   XA = 0
C       A XSM + B XM2 +   XB = 0
C
C       A = -(XA + B XSM)/XS2
C       -(XA + B XSM)*XSM + B XM2*XS2 +   XB*XS2 = 0
C       -B XSM*XSM + B XM2*XS2 +   XB*XS2-XA*XSM  = 0
C       B*(XM2*XS2 - XSM*XSM) = -XB*XS2+XA*XSM  
C       B = (XA*XSM-XB*XS2) / (XM2*XS2 - XSM*XSM)
C       A = (XB*XSM-XA*XM2) / (XM2*XS2 - XSM*XSM)
C
C IF B<0 => B=0
C
C       XS2 = [(X1-X2)^2 + (Y1-Y2)^2 + (Z1-Z2)^2]
C       XA = (X1-X2)(X2-X4) + (Y1-Y2)(Y2-Y4) + (Z1-Z2)(Z2-Z4)
C       A = - XA /XS2
C       B = 0
C
C ELSEIF B>1 => B=1
C
C       B = 1
C       XS2 = [(X1-X2)^2 + (Y1-Y2)^2 + (Z1-Z2)^2]
C       XSM = [(X1-X2)(X4-X3) + (Y1-Y2)(Y4-Y3) + (Z1-Z2)(Z4-Z3)]
C       XA = (X1-X2)(X2-X4) + (Y1-Y2)(Y2-Y4) + (Z1-Z2)(Z2-Z4)
C       A = -(XA + XSM)/XS2
C
C IF A<0 => A=0
C
C
C ELSEIF A>1 => A=1
C
C
      PENE(1:JLT)=EP20
C
      DO I=1,JLT

       XM12 = XXM2(I)-XXM1(I)
       YM12 = XYM2(I)-XYM1(I)
       ZM12 = XZM2(I)-XZM1(I)
       XM2 = XM12*XM12 + YM12*YM12 + ZM12*ZM12

       XS12 = XXS2(I)-XXS1(I)
       YS12 = XYS2(I)-XYS1(I)
       ZS12 = XZS2(I)-XZS1(I)
       XS2  = XS12*XS12 + YS12*YS12 + ZS12*ZS12
       XSM = - (XS12*XM12 + YS12*YM12 + ZS12*ZM12)
       XS2M2 = XXM2(I)-XXS2(I)
       YS2M2 = XYM2(I)-XYS2(I)
       ZS2M2 = XZM2(I)-XZS2(I)

       XA =  XS12*XS2M2 + YS12*YS2M2 + ZS12*ZS2M2
       XB = -XM12*XS2M2 - YM12*YS2M2 - ZM12*ZS2M2 
       DET = XM2*XS2 - XSM*XSM
       DET = MAX(EM20,DET)
C
       H1M(I) = (XA*XSM-XB*XS2) / DET
       XS2 = MAX(XS2,EM20)
       XM2 = MAX(XM2,EM20)
       H1M(I)=MIN(ONE,MAX(ZERO,H1M(I)))
       H1S(I) = -(XA + H1M(I)*XSM) / XS2
       H1S(I)=MIN(ONE,MAX(ZERO,H1S(I)))
       H1M(I) = -(XB + H1S(I)*XSM) / XM2
       H1M(I)=MIN(ONE,MAX(ZERO,H1M(I)))

       H2S(I) = ONE -H1S(I)
       H2M(I) = ONE -H1M(I)
C !!!!!!!!!!!!!!!!!!!!!!!
C  PENE = GAP^2 - DIST^2 UTILISE POUR TESTER SI NON NUL
C!!!!!!!!!!!!!!!!!!!!!!!!
       NX(I) = H1S(I)*XXS1(I) + H2S(I)*XXS2(I)
     .       - H1M(I)*XXM1(I) - H2M(I)*XXM2(I)
       NY(I) = H1S(I)*XYS1(I) + H2S(I)*XYS2(I)
     .       - H1M(I)*XYM1(I) - H2M(I)*XYM2(I)
       NZ(I) = H1S(I)*XZS1(I) + H2S(I)*XZS2(I)
     .       - H1M(I)*XZM1(I) - H2M(I)*XZM2(I)

       NN(I) = SQRT(NX(I)**2 + NY(I)**2 + NZ(I)**2)

       PENE(I) = MAX(ZERO,GAPVE(I) - NN(I))

      ENDDO
C
      SOL_EDGE =IEDGE/10 ! solids
      SH_EDGE  =IEDGE-10*SOL_EDGE ! shells
C
      IF(SH_EDGE/=0)THEN
        DO I=1,JLT

C
C         Free edges, looking vs positive normal only
C
C                                  /     S
C                                /     x 
C                            M /           
C                      <------x        Sector with Zero force
C                      n(M)    \     
C                                \
C                                  \      

          P1=ZERO
          IF(LEDGE(3,CAND_M(I))==0)
     .      P1=-(NX(I)*EX(I)+NY(I)*EY(I)+NZ(I)*EZ(I)) ! (n(M),SM) > 45 degrees

          P2=ZERO
          IF(LEDGE(3,CAND_S(I))==0)
     .      P2=  NX(I)*FX(I)+NY(I)*FY(I)+NZ(I)*FZ(I)  ! (n(S),MS) > 45 degrees

C         NN(I)=SQRT(NN(I)) ! already done above
          IF(P1 > EM04*NN(I) .OR. P2 > EM04*NN(I))PENE(I)=ZERO ! Tolerance EM04

        ENDDO
      END IF
C
C---------------------------------------
      RETURN
      END
