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
Chd|  I25PEN3_EDG                   source/interfaces/inter3d1/i25pen3_edg.F
Chd|-- called by -----------
Chd|        I25STO_EDG                    source/interfaces/inter3d1/i25sto_edg.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules1/tri7box.F      
Chd|====================================================================
      SUBROUTINE I25PEN3_EDG(JLT ,CAND_S ,CAND_M  ,DRAD  ,IGAP0   ,
     .                    NEDGE  ,LEDGE  ,MARGE   ,GAPE  ,GAP_E_L ,
     .                    IGAP   ,X      ,IRECT   ,PENE  ,ADMSR   ,
     .                    EDG_BISECTOR,VTX_BISECTOR,ITAB )

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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr05_c.inc"
C#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, IGAP0, NEDGE, IGAP
      INTEGER IRECT(4,*), CAND_S(*), CAND_M(*), LEDGE(NLEDGE,*), ADMSR(4,*), ITAB(*)
      my_real
     .     DRAD, MARGE,
     .     X(3,*), GAPE(*), GAP_E_L(*), PENE(MVSIZ)
      REAL*4 EDG_BISECTOR(3,4,*), VTX_BISECTOR(3,2,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IG,N1,N2,M1,M2,NI,L,J, IE, JE, IL, JL
      my_real
     .     XXS1(MVSIZ) ,XXS2(MVSIZ) ,XYS1(MVSIZ) ,XYS2(MVSIZ) ,XZS1(MVSIZ) ,XZS2(MVSIZ) ,
     .     XXM1(MVSIZ) ,XXM2(MVSIZ) ,XYM1(MVSIZ) ,XYM2(MVSIZ) ,XZM1(MVSIZ) ,XZM2(MVSIZ) ,
     .     XS12,YS12,ZS12,XM12,YM12,ZM12,XA,XB,
     .     XS2,XM2,XSM,XS2M2,YS2,YM2,YSM,YS2M2,ZS2,ZM2,ZSM,ZS2M2,
     .     XX,YY,ZZ,ALS,ALM,DET,GAP2, 
     .     XMAX1,YMAX1,ZMAX1,XMAX2,YMAX2,ZMAX2,
     .     XMIN1,YMIN1,ZMIN1,XMIN2,YMIN2,ZMIN2,GAPV(MVSIZ),
     .     AAA, DX, DY, DZ, DD, EX, EY ,EZ, NNI, NI2, INVCOS
      my_real 
     .     MAXGAPV
C-----------------------------------------------
      DO I=1,JLT
        IE = CAND_M(I)

        IF(CAND_S(I) <= NEDGE) THEN
          JE = CAND_S(I)
          GAPV(I)=GAPE(IE)+GAPE(JE)
C
          IF(IGAP0 == 0) THEN
            GAPV(I)=DEUX*GAPE(IE)+GAPE(JE)
          ELSE
            GAPV(I)=DEUX*(GAPE(IE)+GAPE(JE))
          END IF
C
          IF(IGAP == 3)
     .       GAPV(I)=MIN(GAP_E_L(IE)+GAP_E_L(JE),GAPV(I)) ! under-estimated ...
C
          GAPV(I)=MAX(DRAD,GAPV(I))+MARGE
        ENDIF
      ENDDO

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
      DO I=1,JLT
        IF(CAND_S(I).LE.NEDGE) THEN
          N1 = LEDGE(5,CAND_S(I))
          N2 = LEDGE(6,CAND_S(I))

          XXS1(I) = X(1,N1)
          XYS1(I) = X(2,N1)
          XZS1(I) = X(3,N1)
          XXS2(I) = X(1,N2)
          XYS2(I) = X(2,N2)
          XZS2(I) = X(3,N2)

        END IF

        M1 = LEDGE(5,CAND_M(I))
        M2 = LEDGE(6,CAND_M(I))

        XXM1(I) = X(1,M1)
        XYM1(I) = X(2,M1)
        XZM1(I) = X(3,M1)
        XXM2(I) = X(1,M2)
        XYM2(I) = X(2,M2)
        XZM2(I) = X(3,M2)

      END DO
C
C--------------------------------------------------------
C     calcul d'un minorant de la distance
C--------------------------------------------------------
        DO I=1,JLT
          XMAX1 = MAX(XXS1(I),XXS2(I))
          YMAX1 = MAX(XYS1(I),XYS2(I))
          ZMAX1 = MAX(XZS1(I),XZS2(I))
          XMAX2 = MAX(XXM1(I),XXM2(I))
          YMAX2 = MAX(XYM1(I),XYM2(I))
          ZMAX2 = MAX(XZM1(I),XZM2(I))
          XMIN1 = MIN(XXS1(I),XXS2(I))
          YMIN1 = MIN(XYS1(I),XYS2(I))
          ZMIN1 = MIN(XZS1(I),XZS2(I))
          XMIN2 = MIN(XXM1(I),XXM2(I))
          YMIN2 = MIN(XYM1(I),XYM2(I))
          ZMIN2 = MIN(XZM1(I),XZM2(I))
          DD = MAX(XMIN1-XMAX2,YMIN1-YMAX2,ZMIN1-ZMAX2,
     .             XMIN2-XMAX1,YMIN2-YMAX1,ZMIN2-ZMAX1)
          IF(DD > GAPV(I))THEN
            PENE(I) = ZERO
            CYCLE
          ENDIF

c         calcul de la distance^2

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
          ALM = (XA*XSM-XB*XS2) / DET
          XS2 = MAX(XS2,EM20)
          XM2 = MAX(XM2,EM20)
          ALM=MIN(UN,MAX(ZERO,ALM))
          ALS = -(XA + ALM*XSM) / XS2
          ALS=MIN(UN,MAX(ZERO,ALS))
          ALM = -(XB + ALS*XSM) / XM2
          ALM=MIN(UN,MAX(ZERO,ALM))

C  PENE = GAP^2 - DIST^2 UTILISE POUR TESTER SI NON NUL

          XX =  ALS*XXS1(I) + (UN-ALS)*XXS2(I)
     .        - ALM*XXM1(I) - (UN-ALM)*XXM2(I)
          YY =  ALS*XYS1(I) + (UN-ALS)*XYS2(I)
     .        - ALM*XYM1(I) - (UN-ALM)*XYM2(I)
          ZZ =  ALS*XZS1(I) + (UN-ALS)*XZS2(I)
     .        - ALM*XZM1(I) - (UN-ALM)*XZM2(I)

          GAP2=GAPV(I)*GAPV(I)
          PENE(I) = MAX(ZERO,GAP2- XX*XX - YY*YY - ZZ*ZZ)
C
        END DO
C
      RETURN
      END
