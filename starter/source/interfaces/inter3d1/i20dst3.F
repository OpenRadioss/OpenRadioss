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
Chd|  I20DST3                       source/interfaces/inter3d1/i20dst3.F
Chd|-- called by -----------
Chd|        I20INI3                       source/interfaces/inter3d1/i20ini3.F
Chd|-- calls ---------------
Chd|        I20CGAP0                      source/interfaces/inter3d1/i20dst3.F
Chd|        I20CGAP1                      source/interfaces/inter3d1/i20dst3.F
Chd|        BITGET                        source/interfaces/inter3d1/bitget.F
Chd|====================================================================
      SUBROUTINE I20DST3(IGAP,GAP_SH,CAND_E,CAND_N,GAPV,
     2                   GAP ,GAP_S ,GAP_M ,GAPMAX,GAPMIN,
     3                   IRECT  ,NLN    ,NLG  ,SOLIDN_NORMAL,NSV,
     4                   NBINFLG,TAG    ,IX3  ,IX4          ,X1 ,
     5                   X2,  X3, X4 ,Y1 ,Y2 ,
     6                   Y3,  Y4, Z1 ,Z2 ,Z3 ,
     7                   Z4,  XI, YI ,ZI ,X0 ,
     8                   Y0,  Z0, NX1,NY1,NZ1,
     9                   NX2,NY2, NZ2,NX3,NY3,
     1                   NZ3,NX4, NY4,NZ4,P1 ,
     2                   P2 ,P3 ,P4  ,LB1,LB2,
     3                   LB3,LB4,LC1 ,LC2,LC3,
     4                   LC4)
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
      INTEGER IGAP,CAND_N(*),CAND_E(*),IRECT(4,*),NLN,NLG(NLN),NSV(*),
     . NBINFLG(*),TAG(*)
      my_real
     . GAP,GAP_SH(*),GAPV(MVSIZ),GAP_S(*),GAP_M(*),GAPMAX,GAPMIN,
     . SOLIDN_NORMAL(3,*)
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
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect07_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,I1,I2,I3,I4,IG,IM,IS,IB1,IB2,IB3,IB4
C     REAL ou REAL*8
      my_real
     .     AL1(MVSIZ), AL2(MVSIZ), AL3(MVSIZ), AL4(MVSIZ),
     .     X01(MVSIZ),  X02(MVSIZ),  X03(MVSIZ), X04(MVSIZ),
     .     Y01(MVSIZ),  Y02(MVSIZ),  Y03(MVSIZ), Y04(MVSIZ),
     .     Z01(MVSIZ),  Z02(MVSIZ),  Z03(MVSIZ), Z04(MVSIZ),
     .     XI1(MVSIZ),  XI2(MVSIZ),  XI3(MVSIZ), XI4(MVSIZ),
     .     YI1(MVSIZ),  YI2(MVSIZ),  YI3(MVSIZ), YI4(MVSIZ),
     .     ZI1(MVSIZ),  ZI2(MVSIZ),  ZI3(MVSIZ), ZI4(MVSIZ),
     .     HLB1(MVSIZ), HLC1(MVSIZ), HLB2(MVSIZ),HLC2(MVSIZ),
     .     HLB3(MVSIZ), HLC3(MVSIZ), HLB4(MVSIZ),HLC4(MVSIZ)
      my_real
     .     S2,A1,A2,A3,A4,D1,D2,D3,D4,
     .     X12,X23,X34,X41,XI0,SX1,SX2,SX3,SX4,SX0,SXI,
     .     Y12,Y23,Y34,Y41,YI0,SY1,SY2,SY3,SY4,SY0,SYI,
     .     Z12,Z23,Z34,Z41,ZI0,SZ1,SZ2,SZ3,SZ4,SZ0,SZI,
     .     X10,Y10,Z10,X20,Y20,Z20,X30,Y30,Z30,X40,Y40,Z40,
     .     LA, HLA, AAA, UNSSQR3
C-----------------------------------------------
      INTEGER BITUNSET,BITGET,BITSET
      EXTERNAL BITUNSET,BITGET,BITSET

!      EQUIVALENCE (IX1,IV(1,1)) ,(IX2,IV(1,2)) ,(IX3,IV(1,3))
!      EQUIVALENCE (IX4,IV(1,4)) ,(NSVG,IV(1,5))
C
!      EQUIVALENCE (X1,BV(1,1)) ,(Y1,BV(1,2)) ,(Z1,BV(1,3))
!      EQUIVALENCE (X2,BV(1,4)) ,(Y2,BV(1,5)) ,(Z2,BV(1,6))
!      EQUIVALENCE (X3,BV(1,7)) ,(Y3,BV(1,8)) ,(Z3,BV(1,9))
!      EQUIVALENCE (X4,BV(1,10)),(Y4,BV(1,11)),(Z4,BV(1,12))
!      EQUIVALENCE (XI,BV(1,13)),(YI,BV(1,14)),(ZI,BV(1,15))
!      EQUIVALENCE (X0,BV(1,31)) ,(Y0,BV(1,32)) ,(Z0,BV(1,33))
!      EQUIVALENCE (NX1,BV(1,58)),(NY1,BV(1,59)),(NZ1,BV(1,60))
!      EQUIVALENCE (NX2,BV(1,61)),(NY2,BV(1,62)),(NZ2,BV(1,63))
!      EQUIVALENCE (NX3,BV(1,64)),(NY3,BV(1,65)),(NZ3,BV(1,66))
!      EQUIVALENCE (NX4,BV(1,67)),(NY4,BV(1,68)),(NZ4,BV(1,69))
!      EQUIVALENCE (P1,BV(1,71)) ,(P2,BV(1,72)) ,(P3,BV(1,73))
!      EQUIVALENCE (P4,BV(1,74)) ,(LB1,BV(1,75)),(LB2,BV(1,76))
!      EQUIVALENCE (LB3,BV(1,77)),(LB4,BV(1,78)),(LC1,BV(1,79))
!      EQUIVALENCE (LC2,BV(1,80)),(LC3,BV(1,81)),(LC4,BV(1,82))
C
C--------------------------------------------------------
C  SHIFT DU GAP POUR SOLIDES AVEC GAP NUL
C--------------------------------------------------------
c                                    
c         --------------------------           
c                                 ^                          
c                                 |                          
c                                 |                          
c                                 |                          
c                                 |                          
c                                 |   gap_search(search algorithm)                        
c                                 |                          
c                                 |                          
c                                 |                          
c                   0 Secnd       |                          
c                   |             |  ^                       
c                   | gap_s       |  |                       
c                   |             |  |                       
c                   v             |  |
c         --------------------    |  | gapv(penetration     
c            ^                    |  |      computation)
c            | gap_m              |  |
c            v                    v  | gapv = min(gapmax,
c Main1 0------------------0 M2    |            gap_s+gap_m)
c         |                       ^  |      + gap_sh
c         | gap_sh                |  | 
c         |                       |  |
c         v                       |  v
cM1-shift o------------------o    |
c            ^                    | gap_search(search algorithm)
c            |                    |
c            |                    | gap_search_engine = gapv + gap_sh
c            | gapv(penetration   |            = min(gapmax, 
c            |      computation)  |                  gap_s+gap_m) 
c            |                    |            + 2*gap_sh     
c            | gapv = min(gapmax, | 
c            |       gap_s+gap_m) | gap_search_starter = 
c            |      + gap_sh      |         gap_s + gap_m + 2*gap_sh
c            |                    |         (overestimate) 
c         ----------------------------
c
c
c=====================================================================
      UNSSQR3 = ONE/SQR3
      IF(IGAP /= 0)THEN
         DO I=LFT,LLT 
           IM = CAND_E(I)
           IS = CAND_N(I)
c           IG = NLG(NSV(IS))
           IG = NSV(IS)
           GAPV(I) = GAP_S(IS) + GAP_M(IM)
           GAPV(I) = MIN(GAPV(I),GAPMAX)
           GAPV(I) = GAPV(I) + GAP_SH(IM)*(ONE-EM5)
           GAPV(I) = MAX(GAPMIN,GAPV(I))
c           I1 = NLG(IRECT(1,IM))
c           I2 = NLG(IRECT(2,IM))
c           I3 = NLG(IRECT(3,IM))
c           I4 = NLG(IRECT(4,IM))
           I1 = IRECT(1,IM)
           I2 = IRECT(2,IM)
           I3 = IRECT(3,IM)
           I4 = IRECT(4,IM)

           IB1 = BITGET(NBINFLG(TAG(I1)),7)
           IB2 = BITGET(NBINFLG(TAG(I2)),7)
           IB3 = BITGET(NBINFLG(TAG(I3)),7)
           IB4 = BITGET(NBINFLG(TAG(I4)),7)

           IF(IB1+IB2+IB3+IB4 == 0)THEN
            IF(GAP_SH(IM)/= ZERO)THEN
C
             SX0=(Y3(I)-Y1(I))*(Z4(I)-Z2(I))-(Z3(I)-Z1(I))*(Y4(I)-Y2(I))
             SY0=(Z3(I)-Z1(I))*(X4(I)-X2(I))-(X3(I)-X1(I))*(Z4(I)-Z2(I))
             SZ0=(X3(I)-X1(I))*(Y4(I)-Y2(I))-(Y3(I)-Y1(I))*(X4(I)-X2(I))
             AAA = ONE / SQRT(MAX(EM20,SX0*SX0+SY0*SY0+SZ0*SZ0))
             SX0=SX0*AAA
             SY0=SY0*AAA
             SZ0=SZ0*AAA
c normale second:
c  la normale second. est enleve de la normale main
c  ce qui est equivalent a distribuer le shift
c  sur l'esclace et les mains
             SXI = SOLIDN_NORMAL(1,IG)
             SYI = SOLIDN_NORMAL(2,IG)
             SZI = SOLIDN_NORMAL(3,IG)
             AAA = SXI*SX0+SYI*SY0+SZI*SZ0
             IF(AAA > ZERO)THEN
               SXI = ZERO
               SYI = ZERO
               SZI = ZERO
             ENDIF

             SX1 = SOLIDN_NORMAL(1,I1)-SXI
             SY1 = SOLIDN_NORMAL(2,I1)-SYI
             SZ1 = SOLIDN_NORMAL(3,I1)-SZI
             AAA = ONE / SQRT(MAX(EM20,SX1*SX1+SY1*SY1+SZ1*SZ1))
             SX1 = SX1*AAA
             SY1 = SY1*AAA
             SZ1 = SZ1*AAA
             AAA = SX0*SX1 + SY0*SY1 + SZ0*SZ1
             IF(AAA > UNSSQR3)THEN
               AAA = GAP_SH(IM)/AAA
             ELSE
               AAA = UNSSQR3 - AAA
               SX1 = SX1 + AAA*SX0
               SY1 = SY1 + AAA*SY0
               SZ1 = SZ1 + AAA*SZ0
               AAA = GAP_SH(IM)*SQR3
             ENDIF               
             SX1 = SX1*AAA
             SY1 = SY1*AAA
             SZ1 = SZ1*AAA
             SX2 = SOLIDN_NORMAL(1,I2)-SXI
             SY2 = SOLIDN_NORMAL(2,I2)-SYI
             SZ2 = SOLIDN_NORMAL(3,I2)-SZI
             AAA = ONE / SQRT(MAX(EM20,SX2*SX2+SY2*SY2+SZ2*SZ2))
             SX2 = SX2*AAA
             SY2 = SY2*AAA
             SZ2 = SZ2*AAA
             AAA = SX0*SX2 + SY0*SY2 + SZ0*SZ2
             IF(AAA > UNSSQR3)THEN
               AAA = GAP_SH(IM)/AAA
             ELSE
               AAA = UNSSQR3 - AAA
               SX2 = SX2 + AAA*SX0
               SY2 = SY2 + AAA*SY0
               SZ2 = SZ2 + AAA*SZ0
               AAA = GAP_SH(IM)*SQR3
             ENDIF               
             SX2 = SX2*AAA
             SY2 = SY2*AAA
             SZ2 = SZ2*AAA
             SX3 = SOLIDN_NORMAL(1,I3)-SXI
             SY3 = SOLIDN_NORMAL(2,I3)-SYI
             SZ3 = SOLIDN_NORMAL(3,I3)-SZI
             AAA = ONE / SQRT(MAX(EM20,SX3*SX3+SY3*SY3+SZ3*SZ3))
             SX3 = SX3*AAA
             SY3 = SY3*AAA
             SZ3 = SZ3*AAA
             AAA = SX0*SX3 + SY0*SY3 + SZ0*SZ3
             IF(AAA > UNSSQR3)THEN
               AAA = GAP_SH(IM)/AAA
             ELSE
               AAA = UNSSQR3 - AAA
               SX3 = SX3 + AAA*SX0
               SY3 = SY3 + AAA*SY0
               SZ3 = SZ3 + AAA*SZ0
               AAA = GAP_SH(IM)*SQR3
             ENDIF               
             SX3 = SX3*AAA
             SY3 = SY3*AAA
             SZ3 = SZ3*AAA
             SX4 = SOLIDN_NORMAL(1,I4)-SXI
             SY4 = SOLIDN_NORMAL(2,I4)-SYI
             SZ4 = SOLIDN_NORMAL(3,I4)-SZI
             AAA = ONE / SQRT(MAX(EM20,SX4*SX4+SY4*SY4+SZ4*SZ4))
             SX4 = SX4*AAA
             SY4 = SY4*AAA
             SZ4 = SZ4*AAA
             AAA = SX0*SX4  + SY0*SY4 + SZ0*SZ4
             IF(AAA > UNSSQR3)THEN
               AAA = GAP_SH(IM)/AAA
             ELSE
               AAA = UNSSQR3 - AAA
               SX4 = SX4 + AAA*SX0
               SY4 = SY4 + AAA*SY0
               SZ4 = SZ4 + AAA*SZ0
               AAA = GAP_SH(IM)*SQR3
             ENDIF               
             SX4 = SX4*AAA
             SY4 = SY4*AAA
             SZ4 = SZ4*AAA
             X1(I) = X1(I) - SX1
             Y1(I) = Y1(I) - SY1
             Z1(I) = Z1(I) - SZ1
             X2(I) = X2(I) - SX2
             Y2(I) = Y2(I) - SY2
             Z2(I) = Z2(I) - SZ2
             X3(I) = X3(I) - SX3
             Y3(I) = Y3(I) - SY3
             Z3(I) = Z3(I) - SZ3
             X4(I) = X4(I) - SX4
             Y4(I) = Y4(I) - SY4
             Z4(I) = Z4(I) - SZ4
            ENDIF
           ELSEIF(I3 == I4)THEN
c            bord de coque3N
             X10 = X1(I)
             Y10 = Y1(I)
             Z10 = Z1(I)
             X20 = X2(I)
             Y20 = Y2(I)
             Z20 = Z2(I)
             X30 = X3(I)
             Y30 = Y3(I)
             Z30 = Z3(I)
             IF(IB1 == 1 .and. IB2 == 1)THEN
               CALL I20CGAP0(X10,Y10,Z10,X20,Y20,Z20,
     .                       X30,Y30,Z30,X30,Y30,Z30,
     .                       X1(I),Y1(I),Z1(I),X2(I),Y2(I),Z2(I),
     .                       X3(I),Y3(I),Z3(I),X3(I),Y3(I),Z3(I),
     .                       GAP_M(IM))
             ENDIF
             IF(IB2 == 1 .and. IB3 == 1)THEN
               CALL I20CGAP0(X20,Y20,Z20,X30,Y30,Z30,
     .                       X10,Y10,Z10,X10,Y10,Z10,
     .                       X2(I),Y2(I),Z2(I),X3(I),Y3(I),Z3(I),
     .                       X1(I),Y1(I),Z1(I),X1(I),Y1(I),Z1(I),
     .                       GAP_M(IM))
             ENDIF
             IF(IB3 == 1 .and. IB1 == 1)THEN
               CALL I20CGAP0(X30,Y30,Z30,X10,Y10,Z10,
     .                       X20,Y20,Z20,X20,Y20,Z20,
     .                       X3(I),Y3(I),Z3(I),X1(I),Y1(I),Z1(I),
     .                       X2(I),Y2(I),Z2(I),X2(I),Y2(I),Z2(I),
     .                       GAP_M(IM))
             ENDIF
             IF(IB1 == 1 .and. IB2+IB3 == 0)THEN
               CALL I20CGAP1(X10,Y10,Z10,X20,Y20,Z20,
     .                       X30,Y30,Z30,
     .                       X1(I),Y1(I),Z1(I),X2(I),Y2(I),Z2(I),
     .                       X3(I),Y3(I),Z3(I),GAP_M(IM))
             ENDIF
             IF(IB2 == 1 .and. IB3+IB1 == 0)THEN
               CALL I20CGAP1(X20,Y20,Z20,X30,Y30,Z30,
     .                       X10,Y10,Z10,
     .                       X2(I),Y2(I),Z2(I),X3(I),Y3(I),Z3(I),
     .                       X1(I),Y1(I),Z1(I),GAP_M(IM))
             ENDIF
             IF(IB3 == 1 .and. IB1+IB2 == 0)THEN
               CALL I20CGAP1(X30,Y30,Z30,X10,Y10,Z10,
     .                       X20,Y20,Z20,
     .                       X3(I),Y3(I),Z3(I),X1(I),Y1(I),Z1(I),
     .                       X2(I),Y2(I),Z2(I),GAP_M(IM))
             ENDIF
             X4(I)=X3(I)
             Y4(I)=Y3(I)
             Z4(I)=Z3(I)
           ELSE
c            bord de coque
             X10 = X1(I)
             Y10 = Y1(I)
             Z10 = Z1(I)
             X20 = X2(I)
             Y20 = Y2(I)
             Z20 = Z2(I)
             X30 = X3(I)
             Y30 = Y3(I)
             Z30 = Z3(I)
             X40 = X4(I)
             Y40 = Y4(I)
             Z40 = Z4(I)
             IF(IB1 == 1 .and. IB2 == 1)THEN
               CALL I20CGAP0(X10,Y10,Z10,X20,Y20,Z20,
     .                       X30,Y30,Z30,X40,Y40,Z40,
     .                       X1(I),Y1(I),Z1(I),X2(I),Y2(I),Z2(I),
     .                       X3(I),Y3(I),Z3(I),X4(I),Y4(I),Z4(I),
     .                       GAP_M(IM))
             ENDIF
             IF(IB2 == 1 .and. IB3 == 1)THEN
               CALL I20CGAP0(X20,Y20,Z20,X30,Y30,Z30,
     .                       X40,Y40,Z40,X10,Y10,Z10,
     .                       X2(I),Y2(I),Z2(I),X3(I),Y3(I),Z3(I),
     .                       X4(I),Y4(I),Z4(I),X1(I),Y1(I),Z1(I),
     .                       GAP_M(IM))
             ENDIF
             IF(IB3 == 1 .and. IB4 == 1)THEN
               CALL I20CGAP0(X30,Y30,Z30,X40,Y40,Z40,
     .                       X10,Y10,Z10,X20,Y20,Z20,
     .                       X3(I),Y3(I),Z3(I),X4(I),Y4(I),Z4(I),
     .                       X1(I),Y1(I),Z1(I),X2(I),Y2(I),Z2(I),
     .                       GAP_M(IM))
             ENDIF
             IF(IB4 == 1 .and. IB1 == 1)THEN
               CALL I20CGAP0(X40,Y40,Z40,X10,Y10,Z10,
     .                       X20,Y20,Z20,X30,Y30,Z30,
     .                       X4(I),Y4(I),Z4(I),X1(I),Y1(I),Z1(I),
     .                       X2(I),Y2(I),Z2(I),X3(I),Y3(I),Z3(I),
     .                       GAP_M(IM))
             ENDIF
             IF(IB1 == 1 .and. IB2+IB4 == 0)THEN
               CALL I20CGAP1(X10,Y10,Z10,X20,Y20,Z20,
     .                       X40,Y40,Z40,
     .                       X1(I),Y1(I),Z1(I),X2(I),Y2(I),Z2(I),
     .                       X4(I),Y4(I),Z4(I),GAP_M(IM))
             ENDIF
             IF(IB2 == 1 .and. IB3+IB1 == 0)THEN
               CALL I20CGAP1(X20,Y20,Z20,X30,Y30,Z30,
     .                       X10,Y10,Z10,
     .                       X2(I),Y2(I),Z2(I),X3(I),Y3(I),Z3(I),
     .                       X1(I),Y1(I),Z1(I),GAP_M(IM))
             ENDIF
             IF(IB3 == 1 .and. IB4+IB2 == 0)THEN
               CALL I20CGAP1(X30,Y30,Z30,X40,Y40,Z40,
     .                       X10,Y10,Z10,
     .                       X3(I),Y3(I),Z3(I),X4(I),Y4(I),Z4(I),
     .                       X2(I),Y2(I),Z2(I),GAP_M(IM))
             ENDIF
             IF(IB4 == 1 .and. IB1+IB3 == 0)THEN
               CALL I20CGAP1(X40,Y40,Z40,X10,Y10,Z10,
     .                       X30,Y30,Z30,
     .                       X4(I),Y4(I),Z4(I),X1(I),Y1(I),Z1(I),
     .                       X3(I),Y3(I),Z3(I),GAP_M(IM))
             ENDIF
           ENDIF
         ENDDO                           
      ENDIF

      DO I=LFT,LLT
       X0(I) = FOURTH*(X1(I)+X2(I)+X3(I)+X4(I))
       Y0(I) = FOURTH*(Y1(I)+Y2(I)+Y3(I)+Y4(I))
       Z0(I) = FOURTH*(Z1(I)+Z2(I)+Z3(I)+Z4(I))
      ENDDO
C
      DO I=LFT,LLT
       IF(IX3(I).EQ.IX4(I))THEN
         X0(I) = X3(I)
         Y0(I) = Y3(I)
         Z0(I) = Z3(I)
       ENDIF
      ENDDO
C
      DO I=LFT,LLT
C
        X01(I) = X1(I) - X0(I)
        Y01(I) = Y1(I) - Y0(I)
        Z01(I) = Z1(I) - Z0(I)
C
        X02(I) = X2(I) - X0(I)
        Y02(I) = Y2(I) - Y0(I)
        Z02(I) = Z2(I) - Z0(I)
C
        X03(I) = X3(I) - X0(I)
        Y03(I) = Y3(I) - Y0(I)
        Z03(I) = Z3(I) - Z0(I)
C
        X04(I) = X4(I) - X0(I)
        Y04(I) = Y4(I) - Y0(I)
        Z04(I) = Z4(I) - Z0(I)
C
        XI0 = X0(I) - XI(I)
        YI0 = Y0(I) - YI(I)
        ZI0 = Z0(I) - ZI(I)
C
        XI1(I) = X1(I) - XI(I)
        YI1(I) = Y1(I) - YI(I)
        ZI1(I) = Z1(I) - ZI(I)
C
        XI2(I) = X2(I) - XI(I)
        YI2(I) = Y2(I) - YI(I)
        ZI2(I) = Z2(I) - ZI(I)
C
        XI3(I) = X3(I) - XI(I)
        YI3(I) = Y3(I) - YI(I)
        ZI3(I) = Z3(I) - ZI(I)
C
        XI4(I) = X4(I) - XI(I)
        YI4(I) = Y4(I) - YI(I)
        ZI4(I) = Z4(I) - ZI(I)
C
        SX1 = YI0*ZI1(I) - ZI0*YI1(I)
        SY1 = ZI0*XI1(I) - XI0*ZI1(I)
        SZ1 = XI0*YI1(I) - YI0*XI1(I)
C
        SX2 = YI0*ZI2(I) - ZI0*YI2(I)
        SY2 = ZI0*XI2(I) - XI0*ZI2(I)
        SZ2 = XI0*YI2(I) - YI0*XI2(I)
C
        SX0 = Y01(I)*Z02(I) - Z01(I)*Y02(I)
        SY0 = Z01(I)*X02(I) - X01(I)*Z02(I)
        SZ0 = X01(I)*Y02(I) - Y01(I)*X02(I)
        S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
C
        LB1(I) = -(SX0*SX2 + SY0*SY2 + SZ0*SZ2) * S2
        LC1(I) =  (SX0*SX1 + SY0*SY1 + SZ0*SZ1) * S2
C
        SX3 = YI0*ZI3(I) - ZI0*YI3(I)
        SY3 = ZI0*XI3(I) - XI0*ZI3(I)
        SZ3 = XI0*YI3(I) - YI0*XI3(I)
C
        SX0 = Y02(I)*Z03(I) - Z02(I)*Y03(I)
        SY0 = Z02(I)*X03(I) - X02(I)*Z03(I)
        SZ0 = X02(I)*Y03(I) - Y02(I)*X03(I)
        S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
C
        LB2(I) = -(SX0*SX3 + SY0*SY3 + SZ0*SZ3) * S2
        LC2(I) =  (SX0*SX2 + SY0*SY2 + SZ0*SZ2) * S2
C
        SX4 = YI0*ZI4(I) - ZI0*YI4(I)
        SY4 = ZI0*XI4(I) - XI0*ZI4(I)
        SZ4 = XI0*YI4(I) - YI0*XI4(I)
C
        SX0 = Y03(I)*Z04(I) - Z03(I)*Y04(I)
        SY0 = Z03(I)*X04(I) - X03(I)*Z04(I)
        SZ0 = X03(I)*Y04(I) - Y03(I)*X04(I)
        S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
C
        LB3(I) = -(SX0*SX4 + SY0*SY4 + SZ0*SZ4) * S2
        LC3(I) =  (SX0*SX3 + SY0*SY3 + SZ0*SZ3) * S2
C
        SX0 = Y04(I)*Z01(I) - Z04(I)*Y01(I)
        SY0 = Z04(I)*X01(I) - X04(I)*Z01(I)
        SZ0 = X04(I)*Y01(I) - Y04(I)*X01(I)
        S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
C
        LB4(I) = -(SX0*SX1 + SY0*SY1 + SZ0*SZ1) * S2
        LC4(I) =  (SX0*SX4 + SY0*SY4 + SZ0*SZ4) * S2
C
        AAA    = ONE/MAX(EM30,X01(I)*X01(I)+Y01(I)*Y01(I)+Z01(I)*Z01(I))
        HLC1(I)= LC1(I)*ABS(LC1(I))*AAA
        HLB4(I)= LB4(I)*ABS(LB4(I))*AAA
        AL1(I) = -(XI0*X01(I)+YI0*Y01(I)+ZI0*Z01(I))*AAA
        AL1(I) = MAX(ZERO,MIN(ONE,AL1(I)))
        AAA    = ONE/MAX(EM30,X02(I)*X02(I)+Y02(I)*Y02(I)+Z02(I)*Z02(I))
        HLC2(I)= LC2(I)*ABS(LC2(I))*AAA
        HLB1(I)= LB1(I)*ABS(LB1(I))*AAA
        AL2(I) = -(XI0*X02(I)+YI0*Y02(I)+ZI0*Z02(I))*AAA
        AL2(I) = MAX(ZERO,MIN(ONE,AL2(I)))
        AAA    = ONE/MAX(EM30,X03(I)*X03(I)+Y03(I)*Y03(I)+Z03(I)*Z03(I))
        HLC3(I)= LC3(I)*ABS(LC3(I))*AAA
        HLB2(I)= LB2(I)*ABS(LB2(I))*AAA
        AL3(I) = -(XI0*X03(I)+YI0*Y03(I)+ZI0*Z03(I))*AAA
        AL3(I) = MAX(ZERO,MIN(ONE,AL3(I)))
        AAA    = ONE/MAX(EM30,X04(I)*X04(I)+Y04(I)*Y04(I)+Z04(I)*Z04(I))
        HLC4(I)= LC4(I)*ABS(LC4(I))*AAA
        HLB3(I)= LB3(I)*ABS(LB3(I))*AAA
        AL4(I) = -(XI0*X04(I)+YI0*Y04(I)+ZI0*Z04(I))*AAA
        AL4(I) = MAX(ZERO,MIN(ONE,AL4(I)))
C
      ENDDO
C
      DO I=LFT,LLT
        X12 = X2(I) - X1(I)
        Y12 = Y2(I) - Y1(I)
        Z12 = Z2(I) - Z1(I)
        LA = ONE - LB1(I) - LC1(I)
C       HLA, HLB1, HLC1 necessaires pour triangle angle obtu
        AAA = ONE / MAX(EM20,X12*X12+Y12*Y12+Z12*Z12)
        HLA= LA*ABS(LA) * AAA
        IF(LA.LT.ZERO.AND.
     +     HLA.LE.HLB1(I).AND.HLA.LE.HLC1(I))THEN
         LB1(I) = (XI2(I)*X12+YI2(I)*Y12+ZI2(I)*Z12) * AAA
         LB1(I) = MAX(ZERO,MIN(ONE,LB1(I)))
         LC1(I) = ONE - LB1(I)
        ELSEIF(LB1(I).LT.ZERO.AND.
     +         HLB1(I).LE.HLC1(I).AND.HLB1(I).LE.HLA)THEN
         LB1(I) = ZERO
         LC1(I) = AL2(I)
        ELSEIF(LC1(I).LT.ZERO.AND.
     +         HLC1(I).LE.HLA.AND.HLC1(I).LE.HLB1(I))THEN
         LC1(I) = ZERO
         LB1(I) = AL1(I)
        ENDIF
      ENDDO
C
      DO I=LFT,LLT
        X23 = X3(I) - X2(I)
        Y23 = Y3(I) - Y2(I)
        Z23 = Z3(I) - Z2(I)
        LA = ONE - LB2(I) - LC2(I)
C       HLA, HLB1, HLC1 necessaires pour triangle angle obtu
        AAA = ONE / MAX(EM20,X23*X23+Y23*Y23+Z23*Z23)
        HLA= LA*ABS(LA) * AAA
        IF(LA.LT.ZERO.AND.
     +     HLA.LE.HLB2(I).AND.HLA.LE.HLC2(I))THEN
         LB2(I) = (XI3(I)*X23+YI3(I)*Y23+ZI3(I)*Z23)*AAA
         LB2(I) = MAX(ZERO,MIN(ONE,LB2(I)))
         LC2(I) = ONE - LB2(I)
        ELSEIF(LB2(I).LT.ZERO.AND.
     +         HLB2(I).LE.HLC2(I).AND.HLB2(I).LE.HLA)THEN
         LB2(I) = ZERO
         LC2(I) = AL3(I)
        ELSEIF(LC2(I).LT.ZERO.AND.
     +         HLC2(I).LE.HLA.AND.HLC2(I).LE.HLB2(I))THEN
         LC2(I) = ZERO
         LB2(I) = AL2(I)
        ENDIF
      ENDDO
C
      DO I=LFT,LLT
        X34 = X4(I) - X3(I)
        Y34 = Y4(I) - Y3(I)
        Z34 = Z4(I) - Z3(I)
        LA = ONE - LB3(I) - LC3(I)
C       HLA, HLB1, HLC1 necessaires pour triangle angle obtu
        AAA = ONE / MAX(EM20,X34*X34+Y34*Y34+Z34*Z34)
        HLA= LA*ABS(LA) * AAA
        IF(LA.LT.ZERO.AND.
     +     HLA.LE.HLB3(I).AND.HLA.LE.HLC3(I))THEN
         LB3(I) = (XI4(I)*X34+YI4(I)*Y34+ZI4(I)*Z34)*AAA
         LB3(I) = MAX(ZERO,MIN(ONE,LB3(I)))
         LC3(I) = ONE - LB3(I)
        ELSEIF(LB3(I).LT.ZERO.AND.
     +         HLB3(I).LE.HLC3(I).AND.HLB3(I).LE.HLA)THEN
         LB3(I) = ZERO
         LC3(I) = AL4(I)
        ELSEIF(LC3(I).LT.ZERO.AND.
     +         HLC3(I).LE.HLA.AND.HLC3(I).LE.HLB3(I))THEN
         LC3(I) = ZERO
         LB3(I) = AL3(I)
        ENDIF
      ENDDO
C
      DO I=LFT,LLT
        X41 = X1(I) - X4(I)
        Y41 = Y1(I) - Y4(I)
        Z41 = Z1(I) - Z4(I)
        LA = ONE - LB4(I) - LC4(I)
C       HLA, HLB1, HLC1 necessaires pour triangle angle obtu
        AAA = ONE / MAX(EM20,X41*X41+Y41*Y41+Z41*Z41)
        HLA= LA*ABS(LA) * AAA
        IF(LA.LT.ZERO.AND.
     +     HLA.LE.HLB4(I).AND.HLA.LE.HLC4(I))THEN
         LB4(I) = (XI1(I)*X41+YI1(I)*Y41+ZI1(I)*Z41)*AAA
         LB4(I) = MAX(ZERO,MIN(ONE,LB4(I)))
         LC4(I) = ONE - LB4(I)
        ELSEIF(LB4(I).LT.ZERO.AND.
     +         HLB4(I).LE.HLC4(I).AND.HLB4(I).LE.HLA)THEN
         LB4(I) = ZERO
         LC4(I) = AL1(I)
        ELSEIF(LC4(I).LT.ZERO.AND.
     +         HLC4(I).LE.HLA.AND.HLC4(I).LE.HLB4(I))THEN
         LC4(I) = ZERO
         LB4(I) = AL4(I)
        ENDIF       
      ENDDO
C
      DO I=LFT,LLT
C
        NX1(I) = XI(I)-(X0(I) + LB1(I)*X01(I) + LC1(I)*X02(I))
        NY1(I) = YI(I)-(Y0(I) + LB1(I)*Y01(I) + LC1(I)*Y02(I))
        NZ1(I) = ZI(I)-(Z0(I) + LB1(I)*Z01(I) + LC1(I)*Z02(I))
        P1(I) = NX1(I)*NX1(I) + NY1(I)*NY1(I) +NZ1(I)*NZ1(I)
C
        NX2(I) = XI(I)-(X0(I) + LB2(I)*X02(I) + LC2(I)*X03(I))
        NY2(I) = YI(I)-(Y0(I) + LB2(I)*Y02(I) + LC2(I)*Y03(I))
        NZ2(I) = ZI(I)-(Z0(I) + LB2(I)*Z02(I) + LC2(I)*Z03(I))
        P2(I) = NX2(I)*NX2(I) + NY2(I)*NY2(I) +NZ2(I)*NZ2(I)
C
        NX3(I) = XI(I)-(X0(I) + LB3(I)*X03(I) + LC3(I)*X04(I))
        NY3(I) = YI(I)-(Y0(I) + LB3(I)*Y03(I) + LC3(I)*Y04(I))
        NZ3(I) = ZI(I)-(Z0(I) + LB3(I)*Z03(I) + LC3(I)*Z04(I))
        P3(I) = NX3(I)*NX3(I) + NY3(I)*NY3(I) +NZ3(I)*NZ3(I)
C
        NX4(I) = XI(I)-(X0(I) + LB4(I)*X04(I) + LC4(I)*X01(I))
        NY4(I) = YI(I)-(Y0(I) + LB4(I)*Y04(I) + LC4(I)*Y01(I))
        NZ4(I) = ZI(I)-(Z0(I) + LB4(I)*Z04(I) + LC4(I)*Z01(I))
        P4(I) = NX4(I)*NX4(I) + NY4(I)*NY4(I) +NZ4(I)*NZ4(I)
C
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  I20CGAP0                      source/interfaces/inter3d1/i20dst3.F
Chd|-- called by -----------
Chd|        I20DST3                       source/interfaces/inter3d1/i20dst3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I20CGAP0(X10,Y10,Z10,X20,Y20,Z20,
     .                    X30,Y30,Z30,X40,Y40,Z40,
     .                    X1,Y1,Z1,X2,Y2,Z2,
     .                    X3,Y3,Z3,X4,Y4,Z4,GAP_M)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     . X10,Y10,Z10,X20,Y20,Z20,X30,Y30,Z30,X40,Y40,Z40,
     . X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,GAP_M
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     . L12,L12X,L12Y,L12Z,
     . L23,L23X,L23Y,L23Z,L14,L14X,L14Y,L14Z,AAA,BBB,L1214,L1223
c=====================================================================
      L12X = X20-X10
      L12Y = Y20-Y10
      L12Z = Z20-Z10
      L12 = SQRT(L12X*L12X+L12Y*L12Y+L12Z*L12Z)
      AAA = ONE/MAX(L12,EM30)
      L12X = L12X*AAA
      L12Y = L12Y*AAA
      L12Z = L12Z*AAA

      L14X = X40-X10
      L14Y = Y40-Y10
      L14Z = Z40-Z10
      L14 = SQRT(L14X*L14X+L14Y*L14Y+L14Z*L14Z)
      AAA = ONE/MAX(L14,EM30)
      L14X = L14X*AAA
      L14Y = L14Y*AAA
      L14Z = L14Z*AAA

      L23X = X30-X20
      L23Y = Y30-Y20
      L23Z = Z30-Z20
      L23 = SQRT(L23X*L23X+L23Y*L23Y+L23Z*L23Z)
      AAA = ONE/MAX(L23,EM30)
      L23X = L23X*AAA
      L23Y = L23Y*AAA
      L23Z = L23Z*AAA

      L1214 = L12X*L14X+L12Y*L14Y+L12Z*L14Z

      BBB = GAP_M*(ONE+EM5)/MAX(SQRT(ONE-L1214*L1214),EM30)
      AAA = MIN(L12/THREE,BBB)

      X1 = X1 + L14X*AAA
      Y1 = Y1 + L14Y*AAA
      Z1 = Z1 + L14Z*AAA

      L1223 = L12X*L23X+L12Y*L23Y+L12Z*L23Z

      BBB = GAP_M*(ONE+EM5)/MAX(SQRT(ONE-L1223*L1223),EM30)
      AAA = MIN(L14/THREE,BBB)

      X2 = X2 + L23X*AAA
      Y2 = Y2 + L23Y*AAA
      Z2 = Z2 + L23Z*AAA

      RETURN
      END
Chd|====================================================================
Chd|  I20CGAP1                      source/interfaces/inter3d1/i20dst3.F
Chd|-- called by -----------
Chd|        I20DST3                       source/interfaces/inter3d1/i20dst3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I20CGAP1(X10,Y10,Z10,X20,Y20,Z20,
     .                    X40,Y40,Z40,
     .                    X1,Y1,Z1,X2,Y2,Z2,
     .                    X4,Y4,Z4,GAP_M)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     . X10,Y10,Z10,X20,Y20,Z20,X40,Y40,Z40,
     . X1,Y1,Z1,X2,Y2,Z2,X4,Y4,Z4,GAP_M
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     . L12,L12X,L12Y,L12Z,L14,L14X,L14Y,L14Z,AAA,BBB,L1214
c=====================================================================
      L12X = X20-X10
      L12Y = Y20-Y10
      L12Z = Z20-Z10
      L12 = SQRT(L12X*L12X+L12Y*L12Y+L12Z*L12Z)
      AAA = ONE/MAX(L12,EM30)
      L12X = L12X*AAA
      L12Y = L12Y*AAA
      L12Z = L12Z*AAA

      L14X = X40-X10
      L14Y = Y40-Y10
      L14Z = Z40-Z10
      L14 = SQRT(L14X*L14X+L14Y*L14Y+L14Z*L14Z)
      AAA = ONE/MAX(L14,EM30)
      L14X = L14X*AAA
      L14Y = L14Y*AAA
      L14Z = L14Z*AAA

      L1214 = L12X*L14X+L12Y*L14Y+L12Z*L14Z

      BBB = GAP_M*(ONE+EM5)/MAX(SQRT(ONE-L1214*L1214),EM30)
      AAA = MIN(L12/THREE,BBB)

      X1 = X1 + L12X*AAA
      Y1 = Y1 + L12Y*AAA
      Z1 = Z1 + L12Z*AAA

      AAA = MIN(L14/THREE,BBB)

      X1 = X1 + L14X*AAA
      Y1 = Y1 + L14Y*AAA
      Z1 = Z1 + L14Z*AAA

      RETURN
      END
Chd|====================================================================
Chd|  I20GAP1                       source/interfaces/inter3d1/i20dst3.F
Chd|-- called by -----------
Chd|        I20INI3                       source/interfaces/inter3d1/i20ini3.F
Chd|-- calls ---------------
Chd|        BITGET                        source/interfaces/inter3d1/bitget.F
Chd|====================================================================
      SUBROUTINE I20GAP1(NRTM,NSN,NLN,GAP_M,GAP_SH,
     2                   GAP_S,NBINFLG,NSV,NLG,TAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM,NSN,NLN,NBINFLG(NLN),NSV(NSN),NLG(NLN),TAG(NUMNOD)
      my_real
     . GAP_SH(NRTM),GAP_M(NRTM),GAP_S(NSN)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J

      INTEGER BITUNSET,BITGET,BITSET
      EXTERNAL BITUNSET,BITGET,BITSET
c=====================================================================

      DO I=1,NRTM
        GAP_M(I) = GAP_M(I)-TWO*GAP_SH(I)
      ENDDO
c mise a zero du gap secnd sur le bord des coques
c ici NVS donne encore le noeud global, NSV sera modifie dans I20NLG 
      DO I=1,NLN
        J = NLG(I)
        TAG(J)=I
      ENDDO
      DO I=1,NSN
        IF(BITGET(NBINFLG(TAG(NSV(I))),7)/=0)THEN
          GAP_S(I) = ZERO
        ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  I20NORM                       source/interfaces/inter3d1/i20dst3.F
Chd|-- called by -----------
Chd|        I20INI3                       source/interfaces/inter3d1/i20ini3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I20NORM(NRTM,IRECT,NUMNOD,X,SOLIDN_NORMAL,
     .                  NMN ,MSR  ,NLN,NLG,GAP_SH)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
c#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
c#include      "param_c.inc"
c#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM,NUMNOD,IRECT(4,NRTM),NMN,MSR(*),NLN,NLG(NLN)
C     REAL ou REAL*8
      my_real
     .   X(3,NUMNOD), SOLIDN_NORMAL(3,NUMNOD),GAP_SH(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J ,N1,N2,N3,N4
      my_real
     .   SURFX,SURFY,SURFZ,X12,Y12,Z12,X23,Y23,Z23,
     .   X34,Y34,Z34,X41,Y41,Z41,AAA,SI,CO
C-----------------------------------------------

      DO I=1,NMN
c          N1 = NLG(MSR(I))
          N1 = MSR(I)
          SOLIDN_NORMAL(1,N1) = ZERO
          SOLIDN_NORMAL(2,N1) = ZERO
          SOLIDN_NORMAL(3,N1) = ZERO
      END DO

      DO I=1,NRTM
c test pour ne prendre en compte que les solides 
        IF(GAP_SH(I)/=ZERO)THEN
c          N1 = NLG(IRECT(1,I))
c          N2 = NLG(IRECT(2,I))
c          N3 = NLG(IRECT(3,I))
c          N4 = NLG(IRECT(4,I))
          N1 = IRECT(1,I)
          N2 = IRECT(2,I)
          N3 = IRECT(3,I)
          N4 = IRECT(4,I)

          X41 = X(1,N1) - X(1,N4)
          Y41 = X(2,N1) - X(2,N4)
          Z41 = X(3,N1) - X(3,N4)
c          AAA=ONE/MAX(EM30,SQRT(X41*X41+Y41*Y41+Z41*Z41))
c          X41 = X41 * AAA
c          Y41 = Y41 * AAA
c          Z41 = Z41 * AAA

          X12 = X(1,N2) - X(1,N1)
          Y12 = X(2,N2) - X(2,N1)
          Z12 = X(3,N2) - X(3,N1)
c          AAA=ONE/MAX(EM30,SQRT(X12*X12+Y12*Y12+Z12*Z12))
c          X12 = X12 * AAA
c          Y12 = Y12 * AAA
c          Z12 = Z12 * AAA

          X23 = X(1,N3) - X(1,N2)
          Y23 = X(2,N3) - X(2,N2)
          Z23 = X(3,N3) - X(3,N2)
c          AAA=ONE/MAX(EM30,SQRT(X23*X23+Y23*Y23+Z23*Z23))
c          X23 = X23 * AAA
c          Y23 = Y23 * AAA
c          Z23 = Z23 * AAA

          SURFX = Y41*Z12 - Z41*Y12
          SURFY = Z41*X12 - X41*Z12
          SURFZ = X41*Y12 - Y41*X12

          CO = X41*X12 + Y41*Y12 + Z41*Z12
          SI = SQRT(SURFX*SURFX+SURFY*SURFY+SURFZ*SURFZ)
          AAA = ATAN2(SI,-CO)*TWO/PI/MAX(EM30,SI)
          SOLIDN_NORMAL(1,N1) = SOLIDN_NORMAL(1,N1) + SURFX*AAA
          SOLIDN_NORMAL(2,N1) = SOLIDN_NORMAL(2,N1) + SURFY*AAA
          SOLIDN_NORMAL(3,N1) = SOLIDN_NORMAL(3,N1) + SURFZ*AAA

          SURFX = Y12*Z23 - Z12*Y23
          SURFY = Z12*X23 - X12*Z23
          SURFZ = X12*Y23 - Y12*X23
          CO = X12*X23 + Y12*Y23 + Z12*Z23
          SI = SQRT(SURFX*SURFX+SURFY*SURFY+SURFZ*SURFZ)
          AAA = ATAN2(SI,-CO)*TWO/PI/MAX(EM30,SI)
          SOLIDN_NORMAL(1,N2) = SOLIDN_NORMAL(1,N2) + SURFX*AAA
          SOLIDN_NORMAL(2,N2) = SOLIDN_NORMAL(2,N2) + SURFY*AAA
          SOLIDN_NORMAL(3,N2) = SOLIDN_NORMAL(3,N2) + SURFZ*AAA

          IF(N3 /= N4)THEN
            X34 = X(1,N4) - X(1,N3)
            Y34 = X(2,N4) - X(2,N3)
            Z34 = X(3,N4) - X(3,N3)
c            AAA=ONE/MAX(EM30,SQRT(X34*X34+Y34*Y34+Z34*Z34))
c            X34 = X34 * AAA
c            Y34 = Y34 * AAA
c            Z34 = Z34 * AAA
            SURFX = Y23*Z34 - Z23*Y34
            SURFY = Z23*X34 - X23*Z34
            SURFZ = X23*Y34 - Y23*X34
            CO = X23*X34 + Y23*Y34 + Z23*Z34
            SI = SQRT(SURFX*SURFX+SURFY*SURFY+SURFZ*SURFZ)
            AAA = ATAN2(SI,-CO)*TWO/PI/MAX(EM30,SI)
            SOLIDN_NORMAL(1,N3) = SOLIDN_NORMAL(1,N3) + SURFX*AAA
            SOLIDN_NORMAL(2,N3) = SOLIDN_NORMAL(2,N3) + SURFY*AAA
            SOLIDN_NORMAL(3,N3) = SOLIDN_NORMAL(3,N3) + SURFZ*AAA

            SURFX = Y34*Z41 - Z34*Y41
            SURFY = Z34*X41 - X34*Z41
            SURFZ = X34*Y41 - Y34*X41
            CO = X34*X41 + Y34*Y41 + Z34*Z41
            SI = SQRT(SURFX*SURFX+SURFY*SURFY+SURFZ*SURFZ)
            AAA = ATAN2(SI,-CO)*TWO/PI/MAX(EM30,SI)
            SOLIDN_NORMAL(1,N4) = SOLIDN_NORMAL(1,N4) + SURFX*AAA
            SOLIDN_NORMAL(2,N4) = SOLIDN_NORMAL(2,N4) + SURFY*AAA
            SOLIDN_NORMAL(3,N4) = SOLIDN_NORMAL(3,N4) + SURFZ*AAA
          ELSE
            SURFX = Y23*Z41 - Z23*Y41
            SURFY = Z23*X41 - X23*Z41
            SURFZ = X23*Y41 - Y23*X41
            CO = X41*X23 + Y41*Y23 + Z41*Z23
            SI = SQRT(SURFX*SURFX+SURFY*SURFY+SURFZ*SURFZ)
            AAA = ATAN2(SI,-CO)*TWO/PI/MAX(EM30,SI)
            SOLIDN_NORMAL(1,N3) = SOLIDN_NORMAL(1,N3) + SURFX*AAA
            SOLIDN_NORMAL(2,N3) = SOLIDN_NORMAL(2,N3) + SURFY*AAA
            SOLIDN_NORMAL(3,N3) = SOLIDN_NORMAL(3,N3) + SURFZ*AAA
          ENDIF
        ENDIF
      ENDDO

c      DO I=1,NMN
cc          N1 = NLG(MSR(I))
c          N1 = MSR(I)
c          AAA = SOLIDN_NORMAL(1,N1)*SOLIDN_NORMAL(1,N1)
c     +        + SOLIDN_NORMAL(2,N1)*SOLIDN_NORMAL(2,N1)
c     +        + SOLIDN_NORMAL(3,N1)*SOLIDN_NORMAL(3,N1)
c          AAA = ONE/MAX(EM20,SQRT(AAA))
c          SOLIDN_NORMAL(1,N1) = SOLIDN_NORMAL(1,N1)*AAA
c          SOLIDN_NORMAL(2,N1) = SOLIDN_NORMAL(2,N1)*AAA
c          SOLIDN_NORMAL(3,N1) = SOLIDN_NORMAL(3,N1)*AAA
c      END DO

      RETURN
      END
Chd|====================================================================
Chd|  I20DST3E                      source/interfaces/inter3d1/i20dst3.F
Chd|-- called by -----------
Chd|        I20INI3                       source/interfaces/inter3d1/i20ini3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I20DST3E(JLT   ,GAP   ,CAND_S,CAND_M,IRECTS  ,
     .                  IRECTM ,NX,NY,NZ,
     .                  N1,N2,M1,M2,JLT_NEW,
     .                  X ,IGAP,GAP_S,GAP_M, GAPV2,
     .                  NLN    ,NLG  ,SOLIDN_NORMAL  )
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
      INTEGER JLT,JLT_NEW,IGAP
      INTEGER IRECTS(2,*), IRECTM(2,*),CAND_S(*),CAND_M(*),
     . N1(*),N2(*),M1(*),M2(*)
      my_real
     .     GAP,
     . NX(*),NY(*),NZ(*),X(3,*),GAP_S(*),GAP_M(*), GAPV2(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NLN,NLG(NLN)
      my_real
     .     XS12,YS12,ZS12,XM12,YM12,ZM12,XA,XB,
     .     XS2,XM2,XSM,XS2M2,YS2,YM2,YSM,YS2M2,ZS2,ZM2,ZSM,ZS2M2,
     .     XX,YY,ZZ,ALS,ALM,DET,H1S,H2S,H1M,H2M,
     .     GAP2,XXS1,YYS1,ZZS1,XXS2,YYS2,ZZS2,
     .     XXM1,YYM1,ZZM1,XXM2,YYM2,ZZM2,
     .     PENE2(MVSIZ),SOLIDN_NORMAL(3,*),AAA,
     .     SX1,SX2,SX3,SX4,SY1,SY2,SY3,SY4,SZ1,SZ2,SZ3,SZ4

C-----------------------------------------------
       GAP2=GAP*GAP
C
      IF(IGAP.EQ.0)THEN
        DO I=1,JLT
          GAPV2(I)=GAP2
        ENDDO
      ELSE
        DO I=1,JLT
          GAPV2(I)=GAP_S(CAND_S(I))+GAP_M(CAND_M(I))
          GAPV2(I)=MAX(GAPV2(I)*GAPV2(I),GAP2)
        ENDDO
      ENDIF
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
      DO I=1,JLT
       N1(I)=IRECTS(1,CAND_S(I))
       N2(I)=IRECTS(2,CAND_S(I))
       M1(I)=IRECTM(1,CAND_M(I))
       M2(I)=IRECTM(2,CAND_M(I))
       XXS1 = X(1,N1(I))
       YYS1 = X(2,N1(I))
       ZZS1 = X(3,N1(I))
       XXS2 = X(1,N2(I))
       YYS2 = X(2,N2(I))
       ZZS2 = X(3,N2(I))
       XXM1 = X(1,M1(I))
       YYM1 = X(2,M1(I))
       ZZM1 = X(3,M1(I))
       XXM2 = X(1,M2(I))
       YYM2 = X(2,M2(I))
       ZZM2 = X(3,M2(I))
       
       IF(IGAP/=0)THEN
         AAA = GAP_S(CAND_S(I))
         SX1 = SOLIDN_NORMAL(1,N1(I))*AAA
         SY1 = SOLIDN_NORMAL(2,N1(I))*AAA
         SZ1 = SOLIDN_NORMAL(3,N1(I))*AAA
         SX2 = SOLIDN_NORMAL(1,N2(I))*AAA
         SY2 = SOLIDN_NORMAL(2,N2(I))*AAA
         SZ2 = SOLIDN_NORMAL(3,N2(I))*AAA
         AAA = GAP_M(CAND_M(I))
         SX3 = SOLIDN_NORMAL(1,M1(I))*AAA
         SY3 = SOLIDN_NORMAL(2,M1(I))*AAA
         SZ3 = SOLIDN_NORMAL(3,M1(I))*AAA
         SX4 = SOLIDN_NORMAL(1,M2(I))*AAA
         SY4 = SOLIDN_NORMAL(2,M2(I))*AAA
         SZ4 = SOLIDN_NORMAL(3,M2(I))*AAA
         XXS1 = XXS1 - SX1
         YYS1 = YYS1 - SY1
         ZZS1 = ZZS1 - SZ1
         XXS2 = XXS2 - SX2 
         YYS2 = YYS2 - SY2
         ZZS2 = ZZS2 - SZ2
         XXM1 = XXM1 - SX3
         YYM1 = YYM1 - SY3
         ZZM1 = ZZM1 - SZ3
         XXM2 = XXM2 - SX4
         YYM2 = YYM2 - SY4
         ZZM2 = ZZM2 - SZ4
       ENDIF

       XS12 = XXS2-XXS1
       YS12 = YYS2-YYS1
       ZS12 = ZZS2-ZZS1
       XS2 = XS12*XS12 + YS12*YS12 + ZS12*ZS12
       XM12 = XXM2-XXM1
       YM12 = YYM2-YYM1
       ZM12 = ZZM2-ZZM1
       XM2 = XM12*XM12 + YM12*YM12 + ZM12*ZM12
       XSM = - (XS12*XM12 + YS12*YM12 + ZS12*ZM12)

       XS2M2 = XXM2-XXS2
       YS2M2 = YYM2-YYS2
       ZS2M2 = ZZM2-ZZS2

       XA =  XS12*XS2M2 + YS12*YS2M2 + ZS12*ZS2M2
       XB = -XM12*XS2M2 - YM12*YS2M2 - ZM12*ZS2M2 
       DET = XM2*XS2 - XSM*XSM
       DET = MAX(EM20,DET)
C
       H1M = (XA*XSM-XB*XS2) / DET
C
       XS2 = MAX(XS2,EM20)
       XM2 = MAX(XM2,EM20)
       H1M=MIN(ONE,MAX(ZERO,H1M))
       H1S = -(XA + H1M*XSM) / XS2
       H1S=MIN(ONE,MAX(ZERO,H1S))
       H1M = -(XB + H1S*XSM) / XM2
       H1M=MIN(ONE,MAX(ZERO,H1M))
C
       H2S = ONE - H1S
       H2M = ONE - H1M
C !!!!!!!!!!!!!!!!!!!!!!!
C  PENE = GAP^2 - DIST^2 UTILISE POUR TESTER SI NON NUL
C!!!!!!!!!!!!!!!!!!!!!!!!
       NX(I) =  H1S*XXS1 + H2S*XXS2
     .        - H1M*XXM1 - H2M*XXM2
       NY(I) =  H1S*YYS1 + H2S*YYS2
     .        - H1M*YYM1 - H2M*YYM2
       NZ(I) =  H1S*ZZS1 + H2S*ZZS2
     .        - H1M*ZZM1 - H2M*ZZM2
       PENE2(I) = GAPV2(I) - NX(I)*NX(I) - NY(I)*NY(I) - NZ(I)*NZ(I)
       PENE2(I) = MAX(ZERO,PENE2(I)) 
C
      ENDDO
      DO I=1,JLT
        IF(PENE2(I).NE.ZERO)THEN
          JLT_NEW = JLT_NEW + 1
          CAND_S(JLT_NEW) = CAND_S(I)
          CAND_M(JLT_NEW) = CAND_M(I)
          N1(JLT_NEW)  = N1(I)
          N2(JLT_NEW)  = N2(I)
          M1(JLT_NEW)  = M1(I)
          M2(JLT_NEW)  = M2(I)
          NX(JLT_NEW)  = NX(I)
          NY(JLT_NEW)  = NY(I)
          NZ(JLT_NEW)  = NZ(I)
          GAPV2(JLT_NEW)  = GAPV2(I)
        ENDIF
      ENDDO
C-----------
      RETURN
      END
