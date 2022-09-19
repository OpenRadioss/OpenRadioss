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
Chd|  I2DST3_27                     source/interfaces/inter3d1/i2dst3_27.F
Chd|-- called by -----------
Chd|        I2BUC1                        source/interfaces/inter3d1/i2buc1.F
Chd|        I2TRI                         source/interfaces/inter3d1/i2tri.F
Chd|-- calls ---------------
Chd|        CHOOSE_MAIN_SEGMENT           source/interfaces/inter3d1/i2dst3_27.F
Chd|        I2BAR3                        source/interfaces/inter3d1/i2dst3.F
Chd|====================================================================
      SUBROUTINE I2DST3_27(GAPV,CAND_E  ,CAND_N,TZINF,IRTL,ST,DMIN,IGNORE,
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
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: NX1,NY1,NZ1
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: NX2,NY2,NZ2
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: NX3,NY3,NZ3
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: NX4,NY4,NZ4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: P1,P2,P3,P4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: LB1,LB2,LB3,LB4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: LC1,LC2,LC3,LC4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: S,T
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr08_c.inc"
#include      "vect07_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER TFLAG(MVSIZ)
      INTEGER I, II
      my_real PENE(MVSIZ)
C-----------------------------------------------
C=======================================================================
C
C-----------------------------------------------
C     DERVIED FROM I2DST3 - improved projection on triagnles
C-----------------------------------------------
C
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
        IF (TFLAG(I) ==  1) THEN
          PENE(I) = P1(I)
          S(I) = LB1(I)
          T(I) = LC1(I)
       ELSE
          PENE(I) = MAX(P1(I),P2(I),P3(I),P4(I))
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
              CALL CHOOSE_MAIN_SEGMENT(IRECT,IRTL(II),CAND_E(I),S(I),T(I),ST(1,II),ST(2,II),TFLAG(I),II)
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
             CALL CHOOSE_MAIN_SEGMENT(IRECT,IRTL(II),CAND_E(I),S(I),T(I),ST(1,II),ST(2,II),TFLAG(I),II)
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
             CALL CHOOSE_MAIN_SEGMENT(IRECT,IRTL(II),CAND_E(I),S(I),T(I),ST(1,II),ST(2,II),TFLAG(I),II)
          ENDIF
         ENDIF
        ENDDO
      ENDIF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  CHOOSE_MAIN_SEGMENT           source/interfaces/inter3d1/i2dst3_27.F
Chd|-- called by -----------
Chd|        I2DST3_27                     source/interfaces/inter3d1/i2dst3_27.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CHOOSE_MAIN_SEGMENT(IRECT,M_OLD,M_NEW,S_NEW,T_NEW,S,T,TFLAG,II)
C============================================================================
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER IRECT(4,*),M_OLD,M_NEW,TFLAG,II
      my_real
     .    S_NEW,T_NEW,S,T
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect07_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  TFLAG_OLD,INTERIOR_OLD,INTERIOR,SWITCH
      my_real
     .    S1,T1,S2,T2
C-----------------------------------------------
C
      IF (IRECT(3,M_OLD)==IRECT(4,M_OLD)) THEN
        TFLAG_OLD = 1
        IF ((S>=ZERO).AND.(T>=ZERO).AND.(ONE-S-T>=ZERO)) THEN
          INTERIOR_OLD = 1
        ELSE
          INTERIOR_OLD = 0
        ENDIF
      ELSE
        TFLAG_OLD = 0
        IF ((ABS(S)<=ONE).AND.(ABS(T)<=ONE)) THEN
          INTERIOR_OLD = 1
        ELSE
          INTERIOR_OLD = 0
        ENDIF
      ENDIF
C
      IF (TFLAG==1) THEN
        IF ((S_NEW>=ZERO).AND.(T_NEW>=ZERO).AND.(ONE-S_NEW-T_NEW>=ZERO)) THEN
          INTERIOR = 1
        ELSE
          INTERIOR = 0
        ENDIF
      ELSE
        IF ((ABS(S_NEW)<=ONE).AND.(ABS(T_NEW)<=ONE)) THEN
          INTERIOR = 1
        ELSE
          INTERIOR = 0
        ENDIF
      ENDIF
C
C--  Choose segement to keep as main segment
      SWITCH = 0
      IF ((TFLAG_OLD==0).AND.(TFLAG==0)) THEN
C--     two quadrangles
        IF (MAX(ABS(S_NEW),ABS(T_NEW))<MAX(ABS(S),ABS(T))) SWITCH = 1
      ELSE
C--     At least on segment is a triangle
        IF (INTERIOR_OLD < INTERIOR) THEN
          SWITCH = 1
        ELSEIF (INTERIOR_OLD == INTERIOR) THEN
C--     The segment with projection clostet to center is retained ((1/3,1/3) for triangles and (0,0) for quadrangles)
          IF (((S_NEW-THIRD*TFLAG)**2+(T_NEW-THIRD*TFLAG)**2)<
     .        ((S-THIRD*TFLAG_OLD)**2+(T-THIRD*TFLAG_OLD)**2)) SWITCH = 1
        ENDIF
      ENDIF
C
      IF (SWITCH == 1) THEN
        M_OLD = M_NEW
        S = S_NEW
        T = T_NEW
      ENDIF
C
      RETURN
      END