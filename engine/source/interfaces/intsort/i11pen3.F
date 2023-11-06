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
Chd|  I11PEN3_VOX                   source/interfaces/intsort/i11pen3.F
Chd|-- called by -----------
Chd|        I11STO_VOX                    source/interfaces/intsort/i11sto.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F
Chd|====================================================================
      SUBROUTINE I11PEN3_VOX(JLT   ,CAND_S,CAND_M,GAPMIN,DRAD,
     .                    MARGE  ,GAP_S ,GAP_M   ,GAP_S_L,GAP_M_L ,
     .                    IGAP   ,X     ,IRECTS ,IRECTM  ,PENE   ,
     .                    NRTS   ,DGAPLOAD)

C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER JLT, NRTS,IGAP
        INTEGER IRECTS(2,*), IRECTM(2,*),CAND_S(*),CAND_M(*)
        my_real
     .       GAPMIN,MARGE
        my_real
     .       X(3,*), PENE(MVSIZ),
     .       GAP_S(*), GAP_M(*), GAP_S_L(*), GAP_M_L(*)
        my_real , INTENT(IN) :: DGAPLOAD,DRAD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, IG,N1,N2,M1,M2,NI,L,J
        my_real
     .       XS12,YS12,ZS12,XM12,YM12,ZM12,XA,XB,
     .       XS2,XM2,XSM,XS2M2,YS2,YM2,YSM,YS2M2,ZS2,ZM2,ZSM,ZS2M2,
     .       XX,YY,ZZ,ALS,ALM,DET,
     .       GAP2, X11, X12, X13, X21, X22, X23,
     .       XMAX1,YMAX1,ZMAX1,XMAX2,YMAX2,ZMAX2,
     .       XMIN1,YMIN1,ZMIN1,XMIN2,YMIN2,ZMIN2,DD,GAPV(MVSIZ)
        my_real :: MAXGAPV
C-----------------------------------------------
        IF(IGAP==0)THEN
          DO I=1,JLT
            GAPV(I)=MAX(DRAD,GAPMIN+DGAPLOAD)+MARGE
          ENDDO
        ELSE
          DO I=1,JLT
            L = CAND_S(I)
            IF( L <= NRTS) THEN
              GAPV(I)=GAP_S(L)+GAP_M(CAND_M(I))
              IF(IGAP == 3)
     .           GAPV(I)=MIN(GAP_S_L(L)+GAP_M_L(CAND_M(I)),GAPV(I))
              GAPV(I)=MAX(DRAD,MAX(GAPMIN,GAPV(I))+DGAPLOAD)+MARGE
            ELSE
              GAPV(I)=XREM(16,L-NRTS )+GAP_M(CAND_M(I))
              IF(IGAP == 3)
     .          GAPV(I)=MIN(XREM(17,L-NRTS)+GAP_M_L(CAND_M(I)),GAPV(I))
              GAPV(I)=MAX(DRAD,MAX(GAPMIN,GAPV(I))+DGAPLOAD)+MARGE
            ENDIF ! L <= NRTS
          ENDDO ! JLT
        ENDIF ! IGAP ==0

c     MAXGAPV = GAPV(1)
c     DO I = 2,JLT
c       MAXGAPV = MAX(MAXGAPV,GAPV(I))
c     ENDDO
c     WRITE(6,*) __FILE__,__LINE__,"MAXGAPV=",MAXGAPV
C--------------------------------------------------------
C
C--------------------------------------------------------
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
C--------------------------------------------------------

C
        DO I=1,JLT
          L = CAND_S(I)
          IF(L<=NRTS) THEN
            NI=0
            N1=IRECTS(1,CAND_S(I))
            N2=IRECTS(2,CAND_S(I))
            X11 = X(1,N1)
            X12 = X(2,N1)
            X13 = X(3,N1)
            X21 = X(1,N2)
            X22 = X(2,N2)
            X23 = X(3,N2)
          ELSE
            NI = L - NRTS
            X11 = XREM(1,NI)
            X12 = XREM(2,NI)
            X13 = XREM(3,NI)
            X21 = XREM(8,NI)
            X22 = XREM(9,NI)
            X23 = XREM(10,NI)
          END IF
          M1=IRECTM(1,CAND_M(I))
          M2=IRECTM(2,CAND_M(I))


c         calcul d'un minorant de la distance

          XMAX1 = MAX(X11,X21)
          YMAX1 = MAX(X12,X22)
          ZMAX1 = MAX(X13,X23)
          XMAX2 = MAX(X(1,M1),X(1,M2))
          YMAX2 = MAX(X(2,M1),X(2,M2))
          ZMAX2 = MAX(X(3,M1),X(3,M2))
          XMIN1 = MIN(X11,X21)
          YMIN1 = MIN(X12,X22)
          ZMIN1 = MIN(X13,X23)
          XMIN2 = MIN(X(1,M1),X(1,M2))
          YMIN2 = MIN(X(2,M1),X(2,M2))
          ZMIN2 = MIN(X(3,M1),X(3,M2))
          DD = MAX(XMIN1-XMAX2,YMIN1-YMAX2,ZMIN1-ZMAX2,
     .             XMIN2-XMAX1,YMIN2-YMAX1,ZMIN2-ZMAX1)
          IF(DD > GAPV(I))THEN
            PENE(I) = ZERO
            CYCLE
          ENDIF

c         calcul de la distance^2

          XS12 = X21-X11
          YS12 = X22-X12
          ZS12 = X23-X13
          XS2M2 = X(1,M2)-X21
          YS2M2 = X(2,M2)-X22
          ZS2M2 = X(3,M2)-X23
          XS2 = XS12*XS12 + YS12*YS12 + ZS12*ZS12
          XM12 = X(1,M2)-X(1,M1)
          YM12 = X(2,M2)-X(2,M1)
          ZM12 = X(3,M2)-X(3,M1)
          XM2 = XM12*XM12 + YM12*YM12 + ZM12*ZM12
          XSM = - (XS12*XM12 + YS12*YM12 + ZS12*ZM12)
          XA =  XS12*XS2M2 + YS12*YS2M2 + ZS12*ZS2M2
          XB = -XM12*XS2M2 - YM12*YS2M2 - ZM12*ZS2M2
          DET = XM2*XS2 - XSM*XSM
          DET = MAX(EM20,DET)
C
          ALM = (XA*XSM-XB*XS2) / DET
          XS2 = MAX(XS2,EM20)
          XM2 = MAX(XM2,EM20)
          ALM=MIN(ONE,MAX(ZERO,ALM))
          ALS = -(XA + ALM*XSM) / XS2
          ALS=MIN(ONE,MAX(ZERO,ALS))
          ALM = -(XB + ALS*XSM) / XM2
          ALM=MIN(ONE,MAX(ZERO,ALM))

C  PENE = GAP^2 - DIST^2 UTILISE POUR TESTER SI NON NUL

          XX =  ALS*X11 + (ONE-ALS)*X21
     .        - ALM*X(1,M1) - (ONE-ALM)*X(1,M2)
          YY =  ALS*X12 + (ONE-ALS)*X22
     .        - ALM*X(2,M1) - (ONE-ALM)*X(2,M2)
          ZZ =  ALS*X13 + (ONE-ALS)*X23
     .        - ALM*X(3,M1) - (ONE-ALM)*X(3,M2)
          GAP2=GAPV(I)*GAPV(I)
          PENE(I) = GAP2- XX*XX - YY*YY - ZZ*ZZ
C
        END DO


C
        RETURN
      END
C===============================================================================
Chd|====================================================================
Chd|  I11PEN3                       source/interfaces/intsort/i11pen3.F
Chd|-- called by -----------
Chd|        I11STO                        source/interfaces/intsort/i11sto.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F
Chd|====================================================================
      SUBROUTINE I11PEN3(JLT   ,CAND_S,CAND_M,GAP   ,X     ,
     .                   IRECTS,IRECTM,PENE  ,NRTS )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER JLT, NRTS
        INTEGER IRECTS(2,*), IRECTM(2,*),CAND_S(*),CAND_M(*)
        my_real
     .       GAP
        my_real
     .       X(3,*), PENE(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, IG,N1,N2,M1,M2,NI,L,J
        my_real
     .       XS12,YS12,ZS12,XM12,YM12,ZM12,XA,XB,
     .       XS2,XM2,XSM,XS2M2,YS2,YM2,YSM,YS2M2,ZS2,ZM2,ZSM,ZS2M2,
     .       XX,YY,ZZ,ALS,ALM,DET,
     .       GAP2, X11, X12, X13, X21, X22, X23,
     .       XMAX1,YMAX1,ZMAX1,XMAX2,YMAX2,ZMAX2,
     .       XMIN1,YMIN1,ZMIN1,XMIN2,YMIN2,ZMIN2,DD
C-----------------------------------------------
        GAP2=GAP*GAP
C--------------------------------------------------------
C
C--------------------------------------------------------
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
C--------------------------------------------------------
C
        DO I=1,JLT
          L = CAND_S(I)
          IF(L<=NRTS) THEN
            NI=0
            N1=IRECTS(1,CAND_S(I))
            N2=IRECTS(2,CAND_S(I))
            X11 = X(1,N1)
            X12 = X(2,N1)
            X13 = X(3,N1)
            X21 = X(1,N2)
            X22 = X(2,N2)
            X23 = X(3,N2)
          ELSE
            NI = L - NRTS
            X11 = XREM(1,NI)
            X12 = XREM(2,NI)
            X13 = XREM(3,NI)
            X21 = XREM(8,NI)
            X22 = XREM(9,NI)
            X23 = XREM(10,NI)
          END IF
          M1=IRECTM(1,CAND_M(I))
          M2=IRECTM(2,CAND_M(I))


c         calcul d'un minorant de la distance

          XMAX1 = MAX(X11,X21)
          YMAX1 = MAX(X12,X22)
          ZMAX1 = MAX(X13,X23)
          XMAX2 = MAX(X(1,M1),X(1,M2))
          YMAX2 = MAX(X(2,M1),X(2,M2))
          ZMAX2 = MAX(X(3,M1),X(3,M2))
          XMIN1 = MIN(X11,X21)
          YMIN1 = MIN(X12,X22)
          ZMIN1 = MIN(X13,X23)
          XMIN2 = MIN(X(1,M1),X(1,M2))
          YMIN2 = MIN(X(2,M1),X(2,M2))
          ZMIN2 = MIN(X(3,M1),X(3,M2))
          DD = MAX(XMIN1-XMAX2,YMIN1-YMAX2,ZMIN1-ZMAX2,
     .             XMIN2-XMAX1,YMIN2-YMAX1,ZMIN2-ZMAX1)
          IF(DD > GAP)THEN
            PENE(I) = ZERO
            CYCLE
          ENDIF

c         calcul de la distance^2

          XS12 = X21-X11
          YS12 = X22-X12
          ZS12 = X23-X13
          XS2M2 = X(1,M2)-X21
          YS2M2 = X(2,M2)-X22
          ZS2M2 = X(3,M2)-X23
          XS2 = XS12*XS12 + YS12*YS12 + ZS12*ZS12
          XM12 = X(1,M2)-X(1,M1)
          YM12 = X(2,M2)-X(2,M1)
          ZM12 = X(3,M2)-X(3,M1)
          XM2 = XM12*XM12 + YM12*YM12 + ZM12*ZM12
          XSM = - (XS12*XM12 + YS12*YM12 + ZS12*ZM12)
          XA =  XS12*XS2M2 + YS12*YS2M2 + ZS12*ZS2M2
          XB = -XM12*XS2M2 - YM12*YS2M2 - ZM12*ZS2M2
          DET = XM2*XS2 - XSM*XSM
          DET = MAX(EM20,DET)
C
          ALM = (XA*XSM-XB*XS2) / DET
          XS2 = MAX(XS2,EM20)
          XM2 = MAX(XM2,EM20)
          ALM=MIN(ONE,MAX(ZERO,ALM))
          ALS = -(XA + ALM*XSM) / XS2
          ALS=MIN(ONE,MAX(ZERO,ALS))
          ALM = -(XB + ALS*XSM) / XM2
          ALM=MIN(ONE,MAX(ZERO,ALM))

C  PENE = GAP^2 - DIST^2 UTILISE POUR TESTER SI NON NUL

          XX =  ALS*X11 + (ONE-ALS)*X21
     .        - ALM*X(1,M1) - (ONE-ALM)*X(1,M2)
          YY =  ALS*X12 + (ONE-ALS)*X22
     .        - ALM*X(2,M1) - (ONE-ALM)*X(2,M2)
          ZZ =  ALS*X13 + (ONE-ALS)*X23
     .        - ALM*X(3,M1) - (ONE-ALM)*X(3,M2)
          PENE(I) = GAP2- XX*XX - YY*YY - ZZ*ZZ
C
        END DO
C
        RETURN
      END
C===============================================================================
