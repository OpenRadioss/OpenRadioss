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
Chd|  I18DST3                       source/interfaces/int18/i18dst3.F
Chd|-- called by -----------
Chd|        I18MAIN_KINE_I                source/interfaces/int18/i18main_kine.F
Chd|        I7MAINF                       source/interfaces/int07/i7mainf.F
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE I18DST3(
     1                  JLT    ,CAND_N   , CAND_E ,CN_LOC ,CE_LOC        ,
     2                  X1     ,X2       , X3     ,X4     ,Y1            ,
     3                  Y2     ,Y3       , Y4     ,Z1     ,Z2            ,
     4                  Z3     ,Z4       , XI     ,YI     ,ZI            ,
     5                  NX1    ,NX2      , NX3    ,NX4    ,NY1           ,
     6                  NY2    ,NY3      , NY4    ,NZ1    ,NZ2           ,
     7                  NZ3    ,NZ4      , LB1    ,LB2    ,LB3           ,
     8                  LB4    ,LC1      , LC2    ,LC3    ,LC4           ,
     9                  P1     ,P2       , P3     ,P4     ,IX1           ,
     A                  IX2    ,IX3      , IX4    ,NSVG   ,STIF          ,
     B                  JLT_NEW,GAPV     , INACTI ,CAND_P ,ALE_NE_CONNECT,
     C                  INDEX  ,VXI      , VYI    ,ITAB   ,XCELL         ,
     D                  VZI    ,MSI      , KINI   ,SURF   ,IBAG          ,
     E                  IGAP   ,MULTI_FVM)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is computing GAP value (if not constant)
C and also penetration into the gap.
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALE_CONNECTIVITY_MOD
      USE MULTI_FVM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "com04_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, JLT_NEW,INACTI, CAND_N(*),CN_LOC(MVSIZ),
     .        CAND_E(*),CE_LOC(MVSIZ), NSVG(MVSIZ), KINI(*),ICURV
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        INDEX(MVSIZ),IBAG
      INTEGER, INTENT(IN) :: ITAB(NUMNOD),IGAP
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .     KB1(MVSIZ), KB2(MVSIZ), KB3(MVSIZ), KB4(MVSIZ),
     .     KC1(MVSIZ), KC2(MVSIZ), KC3(MVSIZ), KC4(MVSIZ),
     .     X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .     Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .     Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .     XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .     P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ), 
     .     GAPV(MVSIZ), CAND_P(*),
     .     VXI(MVSIZ), VYI(MVSIZ), VZI(MVSIZ), MSI(MVSIZ),
     .     SURF(3,*),XCELL(3,SXCELL)
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM      
      TYPE(t_connectivity), INTENT(IN) :: ALE_NE_CONNECT
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IG, J, IEL, IAD1, IAD2, ELEM_ID
      my_real
     .     X0(MVSIZ), Y0(MVSIZ), Z0(MVSIZ),
     .     AL1(MVSIZ),  AL2(MVSIZ),  AL3(MVSIZ), AL4(MVSIZ),
     .     X01(MVSIZ),  X02(MVSIZ),  X03(MVSIZ), X04(MVSIZ),
     .     Y01(MVSIZ),  Y02(MVSIZ),  Y03(MVSIZ), Y04(MVSIZ),
     .     Z01(MVSIZ),  Z02(MVSIZ),  Z03(MVSIZ), Z04(MVSIZ),
     .     XI1(MVSIZ),  XI2(MVSIZ),  XI3(MVSIZ), XI4(MVSIZ),
     .     YI1(MVSIZ),  YI2(MVSIZ),  YI3(MVSIZ), YI4(MVSIZ),
     .     ZI1(MVSIZ),  ZI2(MVSIZ),  ZI3(MVSIZ), ZI4(MVSIZ),
     .     PENE2(MVSIZ),
     .     HLB1(MVSIZ), HLC1(MVSIZ), HLB2(MVSIZ),HLC2(MVSIZ),
     .     HLB3(MVSIZ), HLC3(MVSIZ), HLB4(MVSIZ),HLC4(MVSIZ)
      my_real
     .     S2,D1,D2,D3,D4,DL,
     .     X12,X23,X34,X41,XI0,SX1,SX2,SX3,SX4,SX0,
     .     Y12,Y23,Y34,Y41,YI0,SY1,SY2,SY3,SY4,SY0,
     .     Z12,Z23,Z34,Z41,ZI0,SZ1,SZ2,SZ3,SZ4,SZ0,
     .     GAP2(MVSIZ), DS2,T1,T2,T3,
     .     AL1NUM,AL2NUM,AL3NUM,AL4NUM,AL1DEN,AL2DEN,AL3DEN,AL4DEN,
     .     X23D,Y23D,Z23D,X34D,Y34D,Z34D,X41D,Y41D,Z41D,
     .     X12D,Y12D,Z12D,GAP2D,XI0D,YI0D,ZI0D,S2D, LA, HLA, AAA,
     .     XI0V(MVSIZ),  YI0V(MVSIZ),  ZI0V(MVSIZ)
      my_real
     .     NNX1(MVSIZ), NNX2(MVSIZ), NNX3(MVSIZ), NNX4(MVSIZ),
     .     NNY1(MVSIZ), NNY2(MVSIZ), NNY3(MVSIZ), NNY4(MVSIZ),
     .     NNZ1(MVSIZ), NNZ2(MVSIZ), NNZ3(MVSIZ), NNZ4(MVSIZ)
     
      LOGICAL IS_GAP_CONSTANT, IS_GAP_VARIABLE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

       IS_GAP_CONSTANT = .TRUE.
       IS_GAP_VARIABLE = .FALSE.
       IF(IGAP == 1)THEN
         IS_GAP_CONSTANT = .FALSE.
         IS_GAP_VARIABLE = .TRUE.         
       ENDIF

       !Reference point for triangular decomposition
       DO I=1,JLT
        IF(IX3(I) /= IX4(I))THEN
        !mean point is the top of each 4 triangles composing the 4-nodes face
         X0(I) = FOURTH*(X1(I)+X2(I)+X3(I)+X4(I))
         Y0(I) = FOURTH*(Y1(I)+Y2(I)+Y3(I)+Y4(I))
         Z0(I) = FOURTH*(Z1(I)+Z2(I)+Z3(I)+Z4(I)) 
        ELSE
         !already a triangle
         X0(I) = X3(I)
         Y0(I) = Y3(I)
         Z0(I) = Z3(I)
        ENDIF
       ENDDO
       
       !COMPUTING VECTORS using each top of the face (from salve point I and from reference point 0 on the face)
       DO I=1,JLT
       ! Vector 01 : from centroid to 1st point of the MAINface
        X01(I) = X1(I) - X0(I)
        Y01(I) = Y1(I) - Y0(I)
        Z01(I) = Z1(I) - Z0(I)
       ! Vector 02 : from centroid to 2nd point of the MAIN face
        X02(I) = X2(I) - X0(I)
        Y02(I) = Y2(I) - Y0(I)
        Z02(I) = Z2(I) - Z0(I)
       ! Vector 03 : from centroid to 3rd point of the MAIN face
        X03(I) = X3(I) - X0(I)
        Y03(I) = Y3(I) - Y0(I)
        Z03(I) = Z3(I) - Z0(I)
       ! Vector 04 : from centroid to 4th point of the MAIN face
        X04(I) = X4(I) - X0(I)
        Y04(I) = Y4(I) - Y0(I)
        Z04(I) = Z4(I) - Z0(I)
       ! Vector IOV : from SECONDARY node to centroid of the MAIN face
        XI0V(I) = X0(I) - XI(I)
        YI0V(I) = Y0(I) - YI(I)
        ZI0V(I) = Z0(I) - ZI(I)
       ! Vector 1I : from salve node to 1st point of the face
        XI1(I) = X1(I) - XI(I)
        YI1(I) = Y1(I) - YI(I)
        ZI1(I) = Z1(I) - ZI(I)
       ! Vector 2I : from salve node to 2nd point of the face
        XI2(I) = X2(I) - XI(I)
        YI2(I) = Y2(I) - YI(I)
        ZI2(I) = Z2(I) - ZI(I)
       ! Vector 3I : from salve node to 3rd point of the face
        XI3(I) = X3(I) - XI(I)
        YI3(I) = Y3(I) - YI(I)
        ZI3(I) = Z3(I) - ZI(I)
       ! Vector 4I : from salve node to 4th point of the face
        XI4(I) = X4(I) - XI(I)
        YI4(I) = Y4(I) - YI(I)
        ZI4(I) = Z4(I) - ZI(I)
       ENDDO
       
       DO I=1,JLT
        ! IO (x) I1
        SX1 = YI0V(I)*ZI1(I) - ZI0V(I)*YI1(I)
        SY1 = ZI0V(I)*XI1(I) - XI0V(I)*ZI1(I)
        SZ1 = XI0V(I)*YI1(I) - YI0V(I)*XI1(I)
        ! IO (x) I2
        SX2 = YI0V(I)*ZI2(I) - ZI0V(I)*YI2(I)
        SY2 = ZI0V(I)*XI2(I) - XI0V(I)*ZI2(I)
        SZ2 = XI0V(I)*YI2(I) - YI0V(I)*XI2(I)
        ! IO (x) I3
        SX3 = YI0V(I)*ZI3(I) - ZI0V(I)*YI3(I)
        SY3 = ZI0V(I)*XI3(I) - XI0V(I)*ZI3(I)
        SZ3 = XI0V(I)*YI3(I) - YI0V(I)*XI3(I)       
        ! IO (x) I4
        SX4 = YI0V(I)*ZI4(I) - ZI0V(I)*YI4(I)
        SY4 = ZI0V(I)*XI4(I) - XI0V(I)*ZI4(I)
        SZ4 = XI0V(I)*YI4(I) - YI0V(I)*XI4(I)

        !---TRIANGLE_1-----------------------                
        ! normal vector (2Sn, where ||n||=1 )
        SX0 = Y01(I)*Z02(I) - Z01(I)*Y02(I)
        SY0 = Z01(I)*X02(I) - X01(I)*Z02(I)
        SZ0 = X01(I)*Y02(I) - Y01(I)*X02(I)
        S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
        NNX1(I) = SX0
        NNY1(I) = SY0
        NNZ1(I) = SZ0        
        LB1(I) = -(SX0*SX2 + SY0*SY2 + SZ0*SZ2) * S2
        LC1(I) =  (SX0*SX1 + SY0*SY1 + SZ0*SZ1) * S2
        
        !---TRIANGLE_2-----------------------        
        ! normal vector (2Sn, where ||n||=1 )
        SX0 = Y02(I)*Z03(I) - Z02(I)*Y03(I)
        SY0 = Z02(I)*X03(I) - X02(I)*Z03(I)
        SZ0 = X02(I)*Y03(I) - Y02(I)*X03(I)
        S2 = 1./MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
        NNX2(I) = SX0
        NNY2(I) = SY0
        NNZ2(I) = SZ0        
        LB2(I) = -(SX0*SX3 + SY0*SY3 + SZ0*SZ3) * S2
        LC2(I) =  (SX0*SX2 + SY0*SY2 + SZ0*SZ2) * S2
               
        !---TRIANGLE_3-----------------------        
        ! normal vector (2Sn, where ||n||=1 )
        SX0 = Y03(I)*Z04(I) - Z03(I)*Y04(I)
        SY0 = Z03(I)*X04(I) - X03(I)*Z04(I)
        SZ0 = X03(I)*Y04(I) - Y03(I)*X04(I)
        S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
        NNX3(I) = SX0
        NNY3(I) = SY0
        NNZ3(I) = SZ0        
        LB3(I) = -(SX0*SX4 + SY0*SY4 + SZ0*SZ4) * S2
        LC3(I) =  (SX0*SX3 + SY0*SY3 + SZ0*SZ3) * S2

        !---TRIANGLE_3-----------------------                
        ! normal vector (2Sn, where ||n||=1 )
        SX0 = Y04(I)*Z01(I) - Z04(I)*Y01(I)
        SY0 = Z04(I)*X01(I) - X04(I)*Z01(I)
        SZ0 = X04(I)*Y01(I) - Y04(I)*X01(I)
        S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
        NNX4(I) = SX0
        NNY4(I) = SY0
        NNZ4(I) = SZ0        
        LB4(I) = -(SX0*SX1 + SY0*SY1 + SZ0*SZ1) * S2
        LC4(I) =  (SX0*SX4 + SY0*SY4 + SZ0*SZ4) * S2
       ENDDO
       
       IF(IGAP == 1)THEN !---VARIABLE GAP, CALCULATED HERE
        IF (.NOT.MULTI_FVM%IS_USED) THEN
         DO I=1,JLT
           IAD1 = ALE_NE_CONNECT%IAD_CONNECT(I)                         
           IAD2 = ALE_NE_CONNECT%IAD_CONNECT(I + 1) - 1 
           DL = ZERO                
           DO IEL = IAD1, IAD2                                          
              ELEM_ID = ALE_NE_CONNECT%CONNECTED(IEL)                   
              DL=MAX(DL, XCELL(1,ELEM_ID))      
           ENDDO                                                   
           GAPV(I) = THREE_HALF*DL
         ENDDO!next I
        ELSEIF(MULTI_FVM%IS_USED) THEN
         DO I=1,JLT
           ELEM_ID = NSVG(I) - NUMNOD   !ou NSVG(I) ?
           DL=XCELL(1,ELEM_ID)                                                         
           !gap value
           GAPV(I) = THREE_HALF*DL
         ENDDO!next I
        ENDIF
       ELSE !---CONSTANT GAP INPUT BY USER (IGAP=0=
         GAPV(1:JLT)=GAPV(1)
       ENDIF  
       
       IF(IS_GAP_CONSTANT)THEN
       !Gap is constant for interface 18 : !GAPV(1:JLT)=GAP
         AAA =GAPV(1)*GAPV(1)
         DO I=1,JLT
           GAP2(I)=AAA  
         ENDDO
       ELSE
         DO I=1,JLT
           GAP2(I)=GAPV(I)*GAPV(I)   
         ENDDO
       ENDIF             

       IF (IBAG/=0) THEN
#include "lockon.inc"
        DO I=1,JLT
          SURF(1,CAND_E(I))= NNX1(I) + NNX2(I) + NNX3(I) + NNX4(I)
          SURF(2,CAND_E(I))= NNY1(I) + NNY2(I) + NNY3(I) + NNY4(I)
          SURF(3,CAND_E(I))= NNZ1(I) + NNZ2(I) + NNZ3(I) + NNZ4(I)
        ENDDO
#include "lockoff.inc"
       ENDIF

       DO I=1,JLT
        AAA    = ONE/MAX(EM30,X01(I)*X01(I)+Y01(I)*Y01(I)+Z01(I)*Z01(I))
        HLC1(I)= LC1(I)*ABS(LC1(I))*AAA
        HLB4(I)= LB4(I)*ABS(LB4(I))*AAA
        AL1(I) = -(XI0V(I)*X01(I)+YI0V(I)*Y01(I)+ZI0V(I)*Z01(I))*AAA
        AL1(I) = MAX(ZERO,MIN(ONE,AL1(I)))
        AAA    = ONE/MAX(EM30,X02(I)*X02(I)+Y02(I)*Y02(I)+Z02(I)*Z02(I))
        HLC2(I)= LC2(I)*ABS(LC2(I))*AAA
        HLB1(I)= LB1(I)*ABS(LB1(I))*AAA
        AL2(I) = -(XI0V(I)*X02(I)+YI0V(I)*Y02(I)+ZI0V(I)*Z02(I))*AAA
        AL2(I) = MAX(ZERO,MIN(ONE,AL2(I)))
        AAA    = ONE/MAX(EM30,X03(I)*X03(I)+Y03(I)*Y03(I)+Z03(I)*Z03(I))
        HLC3(I)= LC3(I)*ABS(LC3(I))*AAA
        HLB2(I)= LB2(I)*ABS(LB2(I))*AAA
        AL3(I) = -(XI0V(I)*X03(I)+YI0V(I)*Y03(I)+ZI0V(I)*Z03(I))*AAA
        AL3(I) = MAX(ZERO,MIN(ONE,AL3(I)))
        AAA    = ONE/MAX(EM30,X04(I)*X04(I)+Y04(I)*Y04(I)+Z04(I)*Z04(I))
        HLC4(I)= LC4(I)*ABS(LC4(I))*AAA
        HLB3(I)= LB3(I)*ABS(LB3(I))*AAA
        AL4(I) = -(XI0V(I)*X04(I)+YI0V(I)*Y04(I)+ZI0V(I)*Z04(I))*AAA
        AL4(I) = MAX(ZERO,MIN(ONE,AL4(I)))
       ENDDO

       !computes local coordinates (LB1,LC1) on triangle_1 which minimize the distance from SECONDARY point I.
       !  I(XI,YI,ZI) is SECONDARY point.
       !  let (LB1,LV1) be a point of triangle_1  :   0 < LB1 < 1  and  0 < LC1 < 1
       !  find (LB1,LC1) which minimize the distance from SECONDARY point I
       DO I=1,JLT
        X12 = X2(I) - X1(I)
        Y12 = Y2(I) - Y1(I)
        Z12 = Z2(I) - Z1(I)
        LA = ONE - LB1(I) - LC1(I)
        AAA = ONE / MAX(EM20,X12*X12+Y12*Y12+Z12*Z12)
        HLA= LA*ABS(LA) * AAA
        IF(LA < ZERO.AND.HLA <= HLB1(I).AND.HLA <= HLC1(I))THEN
         LB1(I) = (XI2(I)*X12+YI2(I)*Y12+ZI2(I)*Z12) * AAA
         LB1(I) = MAX(ZERO,MIN(ONE,LB1(I)))
         LC1(I) = ONE - LB1(I)
        ELSEIF(LB1(I) < ZERO.AND.HLB1(I) <= HLC1(I).AND.HLB1(I) <= HLA)THEN
         LB1(I) = ZERO
         LC1(I) = AL2(I)
        ELSEIF(LC1(I) < ZERO.AND.HLC1(I) <= HLA.AND.HLC1(I) <= HLB1(I))THEN
         LC1(I) = ZERO
         LB1(I) = AL1(I)
        ENDIF
       ENDDO

       !computes local coordinates (LB2,LC2) on triangle_2 which minimize the distance from SECONDARY point I.
       !  I(XI,YI,ZI) is SECONDARY point.
       !  let (LB2,LV2) be a point of triangle_2  :   0 < LB2 < 1  and  0 < LC2 < 1
       !  find (LB2,LC2) which minimize the distance from SECONDARY point I
       DO I=1,JLT
        X23 = X3(I) - X2(I)
        Y23 = Y3(I) - Y2(I)
        Z23 = Z3(I) - Z2(I)
        LA = ONE - LB2(I) - LC2(I)
        AAA = ONE / MAX(EM20,X23*X23+Y23*Y23+Z23*Z23)
        HLA= LA*ABS(LA) * AAA
        IF(LA < ZERO.AND.HLA <= HLB2(I).AND.HLA <= HLC2(I))THEN
         LB2(I) = (XI3(I)*X23+YI3(I)*Y23+ZI3(I)*Z23)*AAA
         LB2(I) = MAX(ZERO,MIN(ONE,LB2(I)))
         LC2(I) = ONE - LB2(I)
        ELSEIF(LB2(I) < ZERO.AND.HLB2(I) <= HLC2(I).AND.HLB2(I) <= HLA)THEN
         LB2(I) = ZERO
         LC2(I) = AL3(I)
        ELSEIF(LC2(I) < ZERO.AND.HLC2(I) <= HLA.AND.HLC2(I) <= HLB2(I))THEN
         LC2(I) = ZERO
         LB2(I) = AL2(I)
        ENDIF
       ENDDO

       !computes local coordinates (LB3,LC3) on triangle_3 which minimize the distance from SECONDARY point I.
       !  I(XI,YI,ZI) is SECONDARY point.
       !  let (LB3,LV3) be a point of triangle_3  :   0 < LB3 < 1  and  0 < LC3 < 1
       !  find (LB3,LC3) which minimize the distance from SECONDARY point I
       DO I=1,JLT
        X34 = X4(I) - X3(I)
        Y34 = Y4(I) - Y3(I)
        Z34 = Z4(I) - Z3(I)
        LA = ONE - LB3(I) - LC3(I)
        AAA = ONE / MAX(EM20,X34*X34+Y34*Y34+Z34*Z34)
        HLA= LA*ABS(LA) * AAA
        IF(LA < ZERO.AND.HLA <= HLB3(I).AND.HLA <= HLC3(I))THEN
         LB3(I) = (XI4(I)*X34+YI4(I)*Y34+ZI4(I)*Z34)*AAA
         LB3(I) = MAX(ZERO,MIN(ONE,LB3(I)))
         LC3(I) = ONE - LB3(I)
        ELSEIF(LB3(I) < ZERO.AND.HLB3(I) <= HLC3(I).AND.HLB3(I) <= HLA)THEN
         LB3(I) = ZERO
         LC3(I) = AL4(I)
        ELSEIF(LC3(I) < ZERO.AND.HLC3(I) <= HLA.AND.HLC3(I) <= HLB3(I))THEN
         LC3(I) = ZERO
         LB3(I) = AL3(I)
        ENDIF
       ENDDO

       !computes local coordinates (LB4,LC4) on triangle_4 which minimize the distance from SECONDARY point I.
       !  I(XI,YI,ZI) is SECONDARY point.
       !  let (LB4,LV4) be a point of triangle_4  :   0 < LB4 < 1  and  0 < LC4 < 1
       !  find (LB4,LC4) which minimize the distance from SECONDARY point I
       DO I=1,JLT
        X41 = X1(I) - X4(I)
        Y41 = Y1(I) - Y4(I)
        Z41 = Z1(I) - Z4(I)
        LA = ONE - LB4(I) - LC4(I)
        AAA = ONE / MAX(EM20,X41*X41+Y41*Y41+Z41*Z41)
        HLA= LA*ABS(LA) * AAA
        IF(LA < ZERO.AND.HLA <= HLB4(I).AND.HLA <= HLC4(I))THEN
         LB4(I) = (XI1(I)*X41+YI1(I)*Y41+ZI1(I)*Z41)*AAA
         LB4(I) = MAX(ZERO,MIN(ONE,LB4(I)))
         LC4(I) = ONE - LB4(I)
        ELSEIF(LB4(I) < ZERO.AND.HLB4(I) <= HLC4(I).AND.HLB4(I) <= HLA)THEN
         LB4(I) = ZERO
         LC4(I) = AL1(I)
        ELSEIF(LC4(I) < ZERO.AND.HLC4(I) <= HLA.AND.HLC4(I) <= HLB4(I))THEN
         LC4(I) = ZERO
         LB4(I) = AL4(I)
        ENDIF
       ENDDO
              
       !Use minimal distance from each triangle to compute penetration
       DO I=1,JLT
        !----MINIMAL DISTANCE FROM TRIANGLE_1
        NX1(I) = XI(I)-(X0(I) + LB1(I)*X01(I) + LC1(I)*X02(I))
        NY1(I) = YI(I)-(Y0(I) + LB1(I)*Y01(I) + LC1(I)*Y02(I))
        NZ1(I) = ZI(I)-(Z0(I) + LB1(I)*Z01(I) + LC1(I)*Z02(I))
        P1(I) = NX1(I)*NX1(I) + NY1(I)*NY1(I) +NZ1(I)*NZ1(I)
        !----MINIMAL DISTANCE FROM TRIANGLE_2        
        NX2(I) = XI(I)-(X0(I) + LB2(I)*X02(I) + LC2(I)*X03(I))
        NY2(I) = YI(I)-(Y0(I) + LB2(I)*Y02(I) + LC2(I)*Y03(I))
        NZ2(I) = ZI(I)-(Z0(I) + LB2(I)*Z02(I) + LC2(I)*Z03(I))
        P2(I) = NX2(I)*NX2(I) + NY2(I)*NY2(I) +NZ2(I)*NZ2(I)
        !----MINIMAL DISTANCE FROM TRIANGLE_3        
        NX3(I) = XI(I)-(X0(I) + LB3(I)*X03(I) + LC3(I)*X04(I))
        NY3(I) = YI(I)-(Y0(I) + LB3(I)*Y03(I) + LC3(I)*Y04(I))
        NZ3(I) = ZI(I)-(Z0(I) + LB3(I)*Z03(I) + LC3(I)*Z04(I))
        P3(I) = NX3(I)*NX3(I) + NY3(I)*NY3(I) +NZ3(I)*NZ3(I)
        !----MINIMAL DISTANCE FROM TRIANGLE_4        
        NX4(I) = XI(I)-(X0(I) + LB4(I)*X04(I) + LC4(I)*X01(I))
        NY4(I) = YI(I)-(Y0(I) + LB4(I)*Y04(I) + LC4(I)*Y01(I))
        NZ4(I) = ZI(I)-(Z0(I) + LB4(I)*Z04(I) + LC4(I)*Z01(I))
        P4(I) = NX4(I)*NX4(I) + NY4(I)*NY4(I) +NZ4(I)*NZ4(I)
        !---------estimation calculated to skip node out of gap
        D1 = MAX(ZERO, GAP2(I) - P1(I))
        D2 = MAX(ZERO, GAP2(I) - P2(I))
        D3 = MAX(ZERO, GAP2(I) - P3(I))
        D4 = MAX(ZERO, GAP2(I) - P4(I))
        PENE2(I) = MAX(D1,D2,D3,D4) !PENE2 = GAP^2 - DIST^2 :    PENE**2 =0 => PENE=0
        
       ENDDO


       !Store local coordinates on each triangles for retaines nodes (PENE**2 >0)
       DO I=1,JLT
          IF(PENE2(I) == ZERO.OR.STIF(I) == ZERO)THEN
            CAND_P(INDEX(I))=0
          ENDIF
       ENDDO
#include   "vectorize.inc"
        DO I=1,JLT
         IF(PENE2(I) /= ZERO.AND.STIF(I) /= ZERO)THEN
          JLT_NEW         = JLT_NEW + 1
          CN_LOC(JLT_NEW) = CAND_N(I)
          CE_LOC(JLT_NEW) = CAND_E(I)
          IX1(JLT_NEW)    = IX1(I)
          IX2(JLT_NEW)    = IX2(I)
          IX3(JLT_NEW)    = IX3(I)
          IX4(JLT_NEW)    = IX4(I)
          NSVG(JLT_NEW)   = NSVG(I)
          NX1(JLT_NEW)    = NNX1(I)
          NX2(JLT_NEW)    = NNX2(I)
          NX3(JLT_NEW)    = NNX3(I)
          NX4(JLT_NEW)    = NNX4(I)
          NY1(JLT_NEW)    = NNY1(I)
          NY2(JLT_NEW)    = NNY2(I)
          NY3(JLT_NEW)    = NNY3(I)
          NY4(JLT_NEW)    = NNY4(I)
          NZ1(JLT_NEW)    = NNZ1(I)
          NZ2(JLT_NEW)    = NNZ2(I)
          NZ3(JLT_NEW)    = NNZ3(I)
          NZ4(JLT_NEW)    = NNZ4(I)
          P1(JLT_NEW)     = P1(I)
          P2(JLT_NEW)     = P2(I)
          P3(JLT_NEW)     = P3(I)
          P4(JLT_NEW)     = P4(I)
          LB1(JLT_NEW)    = LB1(I)
          LB2(JLT_NEW)    = LB2(I)
          LB3(JLT_NEW)    = LB3(I)
          LB4(JLT_NEW)    = LB4(I)
          LC1(JLT_NEW)    = LC1(I)
          LC2(JLT_NEW)    = LC2(I)
          LC3(JLT_NEW)    = LC3(I)
          LC4(JLT_NEW)    = LC4(I)
          STIF(JLT_NEW)   = STIF(I)
          GAPV(JLT_NEW)   = GAPV(I)
          INDEX(JLT_NEW)  = INDEX(I)
          KINI(JLT_NEW)   = KINI(I)
          VXI(JLT_NEW)    = VXI(I)
          VYI(JLT_NEW)    = VYI(I)
          VZI(JLT_NEW)    = VZI(I)
          MSI(JLT_NEW)    = MSI(I)
         ENDIF
       ENDDO
C---------------------
      RETURN
      END
C===============================================================================
