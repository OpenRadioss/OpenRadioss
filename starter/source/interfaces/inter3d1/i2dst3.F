Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  I2DST3                        source/interfaces/inter3d1/i2dst3.F
Chd|-- called by -----------
Chd|        I2BUC1                        source/interfaces/inter3d1/i2buc1.F
Chd|        I2TRI                         source/interfaces/inter3d1/i2tri.F
Chd|-- calls ---------------
Chd|        I2BAR3                        source/interfaces/inter3d1/i2dst3.F
Chd|====================================================================
      SUBROUTINE I2DST3(GAPV,CAND_E  ,CAND_N,TZINF,IRTL,ST,DMIN,IGNORE,
     .                  THK  ,KNOD2ELS,KNOD2ELC,KNOD2ELTG,NOD2ELS,
     .                  NOD2ELC,NOD2ELTG,X,IRECT,
     .                  NINT,IXC ,IXTG  ,THK_PART,IPARTC,GEO   , 
     .                  NOINT,IXS,IXS10 ,PM,IX3,
     1                  IX4,X1 ,X2 ,X3 ,X4 ,
     1                  Y1 ,Y2 ,Y3 ,Y4 ,Z1 ,
     2                  Z2 ,Z3 ,Z4 ,XI ,YI ,
     3                  ZI ,X0 ,Y0 ,Z0 ,NX1,
     4                  NY1,NZ1,NX2,NY2,NZ2,
     5                  NX3,NY3,NZ3,NX4,NY4,
     6                  NZ4,P1 ,P2 ,P3 ,P4 ,
     7                  LB1,LB2,LB3,LB4,LC1,
     8                  LC2,LC3,LC4,S  ,T  )
C============================================================================
C  cette routine est appelee par : I2TRI(/inter3d1/i2tri.F)
C                                  I2BUC1(/inter3d1/i2buc1.F)
C----------------------------------------------------------------------------
C  cette routine appelle : I7BAR3(/inter3d1/i7bar3.F)
C============================================================================
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
      INTEGER CAND_E(*),CAND_N(*),IRTL(*),IGNORE,
     .    KNOD2ELS(*), KNOD2ELC(*),KNOD2ELTG(*), NOD2ELS(*), NOD2ELC(*), 
     .    NOD2ELTG(*),IRECT(4,*),NINT,
     .    IXC(NIXC,*),IXTG(NIXTG,*),IPARTC(*),NOINT,IXS(NIXS,*),
     .    IXS10(*)
      my_real
     .   GAPV(*),TZINF,ST(2,*),DMIN(*),THK(*),X(3,*),THK_PART(*),
     .   GEO(NPROPG,*),PM(*)
      INTEGER, DIMENSION(MVSIZ), INTENT(IN) :: IX3,IX4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: X1,X2,X3,X4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: Y1,Y2,Y3,Y4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: Z1,Z2,Z3,Z4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: XI,YI,ZI
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: X0,Y0,Z0
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: NX1,NY1,NZ1
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: NX2,NY2,NZ2
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: NX3,NY3,NZ3
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: NX4,NY4,NZ4
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: P1,P2,P3,P4
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: LB1,LB2,LB3,LB4
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: LC1,LC2,LC3,LC4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: S,T
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "vect07_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER TFLAG(MVSIZ)
      INTEGER I, II
      my_real PENE(MVSIZ)
C-----------------------------------------------
C=======================================================================
      DO I=LFT,LLT
       X0(I) = FOURTH*(X1(I)+X2(I)+X3(I)+X4(I))
       Y0(I) = FOURTH*(Y1(I)+Y2(I)+Y3(I)+Y4(I))
       Z0(I) = FOURTH*(Z1(I)+Z2(I)+Z3(I)+Z4(I))
      ENDDO
C
      DO I=LFT,LLT
        IF (IX3(I) == IX4(I)) THEN
          X0(I) = X3(I)
          Y0(I) = Y3(I)
          Z0(I) = Z3(I)
          TFLAG(I) = 1
        ELSE
          TFLAG(I) = 0
        ENDIF
      ENDDO
C
      CALL I2BAR3(XI  ,YI  ,ZI ,X0 ,Y0 ,
     .            Z0  ,X1  ,Y1 ,Z1 ,X2 ,
     .            Y2  ,Z2  ,NX1,NY1,NZ1,
     .            LB1 ,LC1 ,P1 ,GAPV, TFLAG )
C
      CALL I2BAR3(XI  ,YI  ,ZI ,X0 ,Y0 ,
     .            Z0  ,X2  ,Y2 ,Z2 ,X3 ,
     .            Y3  ,Z3  ,NX2,NY2,NZ2,
     .            LB2 ,LC2 ,P2 ,GAPV, TFLAG )
C
      CALL I2BAR3(XI  ,YI  ,ZI ,X0 ,Y0 ,
     .            Z0  ,X3  ,Y3 ,Z3 ,X4 ,
     .            Y4  ,Z4  ,NX3,NY3,NZ3,
     .            LB3 ,LC3 ,P3 ,GAPV, TFLAG )
C
      CALL I2BAR3(XI  ,YI  ,ZI ,X0 ,Y0 ,
     .            Z0  ,X4  ,Y4 ,Z4 ,X1 ,
     .            Y1  ,Z1  ,NX4,NY4,NZ4,
     .            LB4 ,LC4 ,P4 ,GAPV, TFLAG )
C
      DO I=LFT,LLT
       PENE(I) = MAX(P1(I),P2(I),P3(I),P4(I))
C
       IF(P1(I)==PENE(I))THEN
         S(I) = -LB1(I) + LC1(I)
         T(I) = -LB1(I) - LC1(I)
       ELSEIF(P2(I)==PENE(I))THEN
         S(I) =  LB2(I) + LC2(I)
         T(I) = -LB2(I) + LC2(I)
       ELSEIF(P3(I)==PENE(I))THEN
         S(I) =  LB3(I) - LC3(I)
         T(I) =  LB3(I) + LC3(I)
       ELSEIF(P4(I)==PENE(I))THEN
         S(I) = -LB4(I) - LC4(I)
         T(I) =  LB4(I) - LC4(I)
       ELSE
         S(I) = ZERO
         T(I) = ZERO
       ENDIF
      ENDDO
C
      DO I=LFT,LLT
       IF (TFLAG(I) ==  1) THEN
          PENE(I) = P1(I)
          T(I)= ONE - TWO*LB1(I) - TWO*LC1(I)
          IF (T(I) < ONE-EM10) THEN
              S(I)= (LC1(I)-LB1(I))/(LC1(I)+LB1(I))            
          ELSEIF (LB1(I) < -EM10) THEN
            S(I)= TWO
          ELSEIF (LC1(I) < -EM10) THEN
            S(I)= -TWO
          ELSE
            S(I)= ZERO
          ENDIF
       ENDIF
      ENDDO
C   
      IF(IGNORE==2 .OR. IGNORE == 3)THEN
        DO I=LFT,LLT
          IF(PENE(I)>ZERO  .AND.
     .          (S(I) < ONEP5 .AND. 
     .           T(I) < ONEP5 .AND.
     .           S(I) >-ONEP5 .AND.
     .           T(I) >-ONEP5))THEN
            II=CAND_N(I)
            IF(GAPV(I) - PENE(I)<DMIN(II))THEN
              DMIN(II)=GAPV(I)-PENE(I)
              IRTL(II)=CAND_E(I)
              ST(1,II) = S(I)
              ST(2,II) = T(I)
            ELSEIF(GAPV(I) - PENE(I)==DMIN(II))THEN
              IF(MAX(ABS(S(I))    ,ABS(T(I)    ))<
     .           MAX(ABS(ST(1,II)),ABS(ST(2,II)))    )THEN
                IRTL(II)=CAND_E(I)
                ST(1,II) = S(I)
                ST(2,II) = T(I)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ELSEIF(IGNORE==1)THEN
        DO I=LFT,LLT
C
        IF(PENE(I)>ZERO  .AND.
     .          (S(I) < ONEP5 .AND. 
     .           T(I) < ONEP5 .AND.
     .           S(I) >-ONEP5 .AND.
     .           T(I) >-ONEP5))  THEN
          II=CAND_N(I)
          
          IF(TZINF - PENE(I)<DMIN(II))THEN
            DMIN(II)=TZINF - PENE(I)
            IRTL(II)=CAND_E(I)
            ST(1,II) = S(I)
            ST(2,II) = T(I)
          ELSEIF(TZINF - PENE(I)==DMIN(II))THEN
            IF(MAX(ABS(S(I))    ,ABS(T(I)    ))<
     .         MAX(ABS(ST(1,II)),ABS(ST(2,II)))    )THEN
              IRTL(II)=CAND_E(I)
              ST(1,II) = S(I)
              ST(2,II) = T(I)
            ENDIF
          ENDIF
         ENDIF
        ENDDO
      ELSE
        DO I=LFT,LLT
C
        IF(PENE(I)>ZERO)  THEN
          II=CAND_N(I)
          
          IF(TZINF - PENE(I)<DMIN(II))THEN
            DMIN(II)=TZINF - PENE(I)
            IRTL(II)=CAND_E(I)
            ST(1,II) = S(I)
            ST(2,II) = T(I)
          ELSEIF(TZINF - PENE(I)==DMIN(II))THEN
            IF(MAX(ABS(S(I))    ,ABS(T(I)    ))<
     .         MAX(ABS(ST(1,II)),ABS(ST(2,II)))    )THEN
              IRTL(II)=CAND_E(I)
              ST(1,II) = S(I)
              ST(2,II) = T(I)
            ENDIF
          ENDIF
         ENDIF
        ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  I2BAR3                        source/interfaces/inter3d1/i2dst3.F
Chd|-- called by -----------
Chd|        I2DST3                        source/interfaces/inter3d1/i2dst3.F
Chd|        I2DST3_27                     source/interfaces/inter3d1/i2dst3_27.F
Chd|-- calls ---------------
Chd|        I7LIN3                        source/interfaces/inter3d1/i7lin3.F
Chd|====================================================================
      SUBROUTINE I2BAR3(XI,YI,ZI,XA,YA,
     .                  ZA,XB,YB,ZB,XC,
     .                  YC,ZC,NX,NY,NZ,
     .                  LB,LC,P,GAPV, TFLAG)
C============================================================================
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TFLAG(*)
C     REAL
      my_real
     .    XI(*),YI(*),ZI(*),XA(*),YA(*),ZA(*),
     .    XB(*),YB(*),ZB(*),XC(*),YC(*),ZC(*),
     .    NX(*),NY(*),NZ(*),LB(*),LC(*),P(*),GAPV(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect07_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .    XPA,YPA,ZPA,XPB,YPB,ZPB,XPC,YPC,ZPC,
     .    XAB,YAB,ZAB,XAC,YAC,ZAC,ALP,
     .    S2,SX,SY,SZ,XP,YP,ZP
C--------1---------2---------3---------4---------5---------6---------7--
      DO I = LFT , LLT
       XAB = XB(I) - XA(I)
       YAB = YB(I) - YA(I)
       ZAB = ZB(I) - ZA(I)
C
       XAC = XC(I) - XA(I)
       YAC = YC(I) - YA(I)
       ZAC = ZC(I) - ZA(I)
C
       NX(I) = YAB*ZAC - ZAB*YAC
       NY(I) = ZAB*XAC - XAB*ZAC
       NZ(I) = XAB*YAC - YAB*XAC
C
       S2 = MAX(EM20,SQRT(NX(I)**2 + NY(I)**2 + NZ(I)**2))
       NX(I) = NX(I) / S2
       NY(I) = NY(I) / S2
       NZ(I) = NZ(I) / S2
C
       P(I) = NX(I) * (XI(I) - XA(I)) 
     .      + NY(I) * (YI(I) - YA(I)) 
     .      + NZ(I) * (ZI(I) - ZA(I))
C
       XP = XI(I) - NX(I) * P(I)
       YP = YI(I) - NY(I) * P(I)
       ZP = ZI(I) - NZ(I) * P(I)
C
       XPA = XA(I)-XP
       YPA = YA(I)-YP
       ZPA = ZA(I)-ZP
C
       XPB = XB(I)-XP
       YPB = YB(I)-YP
       ZPB = ZB(I)-ZP
C
       XPC = XC(I)-XP
       YPC = YC(I)-YP
       ZPC = ZC(I)-ZP
C
       SX = YPC*ZPA - ZPC*YPA
       SY = ZPC*XPA - XPC*ZPA
       SZ = XPC*YPA - YPC*XPA
C
       LB(I) = (NX(I)*SX + NY(I)*SY + NZ(I)*SZ) / S2
C
       SX = YPA*ZPB - ZPA*YPB
       SY = ZPA*XPB - XPA*ZPB
       SZ = XPA*YPB - YPA*XPB
C
       LC(I) = (NX(I)*SX + NY(I)*SY + NZ(I)*SZ) / S2
      ENDDO
C
      DO I=LFT,LLT
       IF(ONE-LB(I)-LC(I)<ZERO)THEN
         CALL I7LIN3(XI(I),YI(I),ZI(I),XB(I),YB(I),
     .               ZB(I),XC(I),YC(I),ZC(I),NX(I),
     .               NY(I),NZ(I),P(I),ALP)
        ELSEIF(LB(I)<ZERO)THEN
         CALL I7LIN3(XI(I),YI(I),ZI(I),XC(I),YC(I),
     .               ZC(I),XA(I),YA(I),ZA(I),NX(I),
     .               NY(I),NZ(I),P(I),ALP)
          IF (TFLAG(I) == 0) THEN   ! only necessary for warped 4 node segments
            LC(I) = ONE - ALP
            LB(I) = ZERO
          ENDIF
        ELSEIF(LC(I)<ZERO)THEN
         CALL I7LIN3(XI(I),YI(I),ZI(I),XA(I),YA(I),
     .               ZA(I),XB(I),YB(I),ZB(I),NX(I),
     .               NY(I),NZ(I),P(I),ALP)
          IF (TFLAG(I) == 0) THEN   ! only necessary for warped 4 node segments
            LB(I) = ALP
            LC(I) = ZERO
          ENDIF
        ELSEIF(P(I)<ZERO)THEN

         NX(I) = -NX(I)
         NY(I) = -NY(I)
         NZ(I) = -NZ(I)
         P(I)  = -P(I)
       ENDIF
      ENDDO
C
      DO I=LFT,LLT
       P(I) = MAX(ZERO, GAPV(I) - P(I))
      ENDDO
C
      RETURN
      END
