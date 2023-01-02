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
Chd|  I21OPTCD                      source/interfaces/int21/i21optcd.F
Chd|-- called by -----------
Chd|        I21MAIN_OPT_TRI               source/interfaces/intsort/i21main_opt_tri.F
Chd|-- calls ---------------
Chd|        I21DST3                       source/interfaces/int21/i21dst3.F
Chd|        STARTIME                      source/system/timer.F         
Chd|====================================================================
      SUBROUTINE I21OPTCD(CAND_E ,CAND_N ,XLOC  ,I_STOK  ,IRECT  ,
     2                   GAP    ,GAP_S  ,IGAP    ,NSN    ,STFN   ,
     3                   ITASK  ,STF    ,INACTI  ,IFPEN  ,FTXSAV ,
     4                   FTYSAV ,FTZSAV ,PENI    ,NIN    ,GAPMAX ,
     5                   ICURV  ,IRTLM   ,CSTS ,DEPTH ,NOD_NORMAL,
     6                   XM0    ,DRAD    ,NB_STOK_N,NB_JLT,DGAPLOAD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "warn_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), CAND_E(*), CAND_N(*), IFPEN(*),
     .        I_STOK,NIN,IGAP ,ITASK, NSN, INACTI,ICURV,
     .        IRTLM(2,*),NB_STOK_N(*),NB_JLT(*)
      my_real
     .        XLOC(3,*),GAP,GAP_S(*),STFN(*),STF(*),
     .        FTXSAV(*), FTYSAV(*), FTZSAV(*), PENI(*),
     .        GAPMAX, CSTS(2,*), DEPTH, NOD_NORMAL(3,*),
     .        XM0(3,*)
      my_real , INTENT(IN) :: DGAPLOAD,DRAD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,L,IS,JS,LS,NLS,NLT,NSEG,SG,FIRST,LAST,MSEG,NLF,II,J
      INTEGER LIST(MVSIZ),IG(MVSIZ),IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),
     .        IX4(MVSIZ), LISTI(MVSIZ),COUNT_CAND,
     .        IX1_L(MVSIZ), IX2_L(MVSIZ), IX3_L(MVSIZ),IX4_L(MVSIZ)
      my_real
     .        XI,X1,X2,X3,X4,YI,Y1,Y2,Y3,Y4,ZI,Z1,Z2,Z3,Z4,
     .        XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,V12,V22,V32,V42
      my_real
     .        GAPV(MVSIZ),NOD_NORMAL_L(12,MVSIZ),XE_L(12,MVSIZ),
     .        XI_L(MVSIZ), YI_L(MVSIZ), ZI_L(MVSIZ),
     .        X1_L(MVSIZ), X2_L(MVSIZ), X3_L(MVSIZ), X4_L(MVSIZ),
     .        Y1_L(MVSIZ), Y2_L(MVSIZ), Y3_L(MVSIZ), Y4_L(MVSIZ),
     .        Z1_L(MVSIZ), Z2_L(MVSIZ), Z3_L(MVSIZ), Z4_l(MVSIZ),
     .        NNX1(MVSIZ), NNX2(MVSIZ), NNX3(MVSIZ), NNX4(MVSIZ),
     .        NNY1(MVSIZ), NNY2(MVSIZ), NNY3(MVSIZ), NNY4(MVSIZ),
     .        NNZ1(MVSIZ), NNZ2(MVSIZ), NNZ3(MVSIZ), NNZ4(MVSIZ)
      my_real
     .        X0,Y0,Z0,XXX,YYY,ZZZ,CURV_MAX,DEPTH2,DRAD2,
     .        XX1 ,XX2 ,XX3 ,XX4 ,XX5 ,XX6 ,XX7 ,XX8 ,
     .        YY1 ,YY2 ,YY3 ,YY4 ,YY5 ,YY6 ,YY7 ,YY8 ,
     .        ZZ1 ,ZZ2 ,ZZ3 ,ZZ4 ,ZZ5 ,ZZ6 ,ZZ7 ,ZZ8 ,
     .        NX1 ,NX2 ,NX3 ,NX4 , NY1 ,NY2 ,NY3 ,NY4 ,
     .        NZ1 ,NZ2 ,NZ3 ,NZ4 ,GAPF, MARJ
C-----------------------------------------------
      COUNT_CAND=0
C-----------------------------------------------
      DEPTH2=DEPTH*DEPTH
      DRAD2 =DRAD*DRAD
C-----------------------------------------------
      DO J=ITASK+1,NSN,NTHREAD
        IRTLM(1,J)=0
      ENDDO
C
      call my_barrier
C
C-----------------------------------------------
      MSEG = NVSIZ
      FIRST = 1 + I_STOK*ITASK / NTHREAD
      LAST = I_STOK*(ITASK+1) / NTHREAD
      JS = FIRST-1
      DO SG = FIRST,LAST,MSEG
       NSEG = MIN(MSEG,LAST-JS)

       NLS = NSEG
       IF(IGAP==0)THEN
        DO IS=1,NSEG
          GAPV(IS)=GAP
          LISTI(IS)=IS
        ENDDO
       ELSE
        DO IS=1,NSEG
          I=JS+IS
          GAPV(IS)=GAP_S(CAND_N(I))
          IF(GAPMAX/=ZERO)GAPV(IS)=MIN(GAPV(IS),GAPMAX)
          GAPV(IS)=MAX(GAPV(IS),GAP)
          LISTI(IS)=IS
        ENDDO
       ENDIF
C
       IF (DEBUG(3)>=1) NB_JLT(ITASK+1) = NB_JLT(ITASK+1) + NLS
C
       NLF = 1
       NLT = NLS
       NLS=0
       IF(ICURV==3)THEN
#include      "vectorize.inc"
        DO LS = NLF, NLT
          IS = LISTI(LS)
          I=JS+IS
          L  = CAND_E(I)
          IF(STF(L)/=ZERO.AND.STFN(CAND_N(I))/=ZERO) THEN 
           IG(IS) = CAND_N(I)
           XI = XLOC(1,IG(IS))
           YI = XLOC(2,IG(IS))
           ZI = XLOC(3,IG(IS))
           IX1(IS)=IRECT(1,L)
           IX2(IS)=IRECT(2,L)
           IX3(IS)=IRECT(3,L)
           IX4(IS)=IRECT(4,L)
           X1=XM0(1,IX1(IS))
           X2=XM0(1,IX2(IS))
           X3=XM0(1,IX3(IS))
           X4=XM0(1,IX4(IS))
           Y1=XM0(2,IX1(IS))
           Y2=XM0(2,IX2(IS))
           Y3=XM0(2,IX3(IS))
           Y4=XM0(2,IX4(IS))
           Z1=XM0(3,IX1(IS))
           Z2=XM0(3,IX2(IS))
           Z3=XM0(3,IX3(IS))
           Z4=XM0(3,IX4(IS))
           X0 = FOURTH*(X1+X2+X3+X4)
           Y0 = FOURTH*(Y1+Y2+Y3+Y4)
           Z0 = FOURTH*(Z1+Z2+Z3+Z4)
           XXX=MAX(X1,X2,X3,X4)-MIN(X1,X2,X3,X4)
           YYY=MAX(Y1,Y2,Y3,Y4)-MIN(Y1,Y2,Y3,Y4)
           ZZZ=MAX(Z1,Z2,Z3,Z4)-MIN(Z1,Z2,Z3,Z4)
           CURV_MAX = HALF * MAX(XXX,YYY,ZZZ)
           XMIN = X0-CURV_MAX-GAPV(IS)
           YMIN = Y0-CURV_MAX-GAPV(IS)
           ZMIN = Z0-CURV_MAX-GAPV(IS)
           XMAX = X0+CURV_MAX+GAPV(IS)
           YMAX = Y0+CURV_MAX+GAPV(IS)
           ZMAX = Z0+CURV_MAX+GAPV(IS)
           IF (XMIN <= XI.AND.XMAX >= XI.AND.
     .         YMIN <= YI.AND.YMAX >= YI.AND.
     .         ZMIN <= ZI.AND.ZMAX >= ZI) THEN
                 CAND_N(I) = -CAND_N(I)
                 COUNT_CAND = COUNT_CAND+1
           ENDIF
          ENDIF
        ENDDO
       ELSE
#include      "vectorize.inc"
        DO LS = NLF, NLT
Cel conserver LISTI et LIST pour optimiser le code genere (IA64)
          IS = LISTI(LS)
          I=JS+IS
          L  = CAND_E(I)
          IF(STF(L)/=ZERO.AND.STFN(CAND_N(I))/=ZERO) THEN ! Contact sorting optimization
           IG(IS) = CAND_N(I)
           GAPF = MAX(GAPV(IS)+DGAPLOAD,DRAD)
C          
           XI = XLOC(1,IG(IS)) ! Secnd node
           YI = XLOC(2,IG(IS))
           ZI = XLOC(3,IG(IS))
C
           IX1(IS)=IRECT(1,L) ! Main Segment
           IX2(IS)=IRECT(2,L)
           IX3(IS)=IRECT(3,L)
           IX4(IS)=IRECT(4,L)
C
           X1=XM0(1,IX1(IS))
           X2=XM0(1,IX2(IS))
           X3=XM0(1,IX3(IS))
           X4=XM0(1,IX4(IS))
C
           Y1=XM0(2,IX1(IS))
           Y2=XM0(2,IX2(IS))
           Y3=XM0(2,IX3(IS))
           Y4=XM0(2,IX4(IS))
C
           Z1=XM0(3,IX1(IS))
           Z2=XM0(3,IX2(IS))
           Z3=XM0(3,IX3(IS))
           Z4=XM0(3,IX4(IS))
C
           NX1 = NOD_NORMAL(1,IX1(IS))
           NY1 = NOD_NORMAL(2,IX1(IS))
           NZ1 = NOD_NORMAL(3,IX1(IS))
C
           NX2 = NOD_NORMAL(1,IX2(IS))
           NY2 = NOD_NORMAL(2,IX2(IS))
           NZ2 = NOD_NORMAL(3,IX2(IS))
C
           NX3 = NOD_NORMAL(1,IX3(IS))
           NY3 = NOD_NORMAL(2,IX3(IS))
           NZ3 = NOD_NORMAL(3,IX3(IS))
C
           NX4 = NOD_NORMAL(1,IX4(IS))
           NY4 = NOD_NORMAL(2,IX4(IS))
           NZ4 = NOD_NORMAL(3,IX4(IS))
C
           XX1 = X1 + GAPF*NX1
           XX2 = X2 + GAPF*NX2
           XX3 = X3 - DEPTH*NX3
           XX4 = X4 - DEPTH*NX4
           XX5 = X1 - DEPTH*NX1
           XX6 = X2 - DEPTH*NX2
           XX7 = X3 + GAPF*NX3
           XX8 = X4 + GAPF*NX4
C
           YY1 = Y1 + GAPF*NY1
           YY2 = Y2 + GAPF*NY2
           YY3 = Y3 - DEPTH*NY3
           YY4 = Y4 - DEPTH*NY4
           YY5 = Y1 - DEPTH*NY1
           YY6 = Y2 - DEPTH*NY2
           YY7 = Y3 + GAPF*NY3
           YY8 = Y4 + GAPF*NY4
C
           ZZ1 = Z1 + GAPF*NZ1
           ZZ2 = Z2 + GAPF*NZ2
           ZZ3 = Z3 - DEPTH*NZ3
           ZZ4 = Z4 - DEPTH*NZ4
           ZZ5 = Z1 - DEPTH*NZ1
           ZZ6 = Z2 - DEPTH*NZ2
           ZZ7 = Z3 + GAPF*NZ3
           ZZ8 = Z4 + GAPF*NZ4
C
           XMIN = MIN(XX1,XX2,XX3,XX4,XX5,XX6,XX7,XX8)
           YMIN = MIN(YY1,YY2,YY3,YY4,YY5,YY6,YY7,YY8)
           ZMIN = MIN(ZZ1,ZZ2,ZZ3,ZZ4,ZZ5,ZZ6,ZZ7,ZZ8)
           XMAX = MAX(XX1,XX2,XX3,XX4,XX5,XX6,XX7,XX8)
           YMAX = MAX(YY1,YY2,YY3,YY4,YY5,YY6,YY7,YY8)
           ZMAX = MAX(ZZ1,ZZ2,ZZ3,ZZ4,ZZ5,ZZ6,ZZ7,ZZ8)
C
           MARJ = EM02*(XMAX-XMIN)
           XMIN = XMIN - MARJ
           XMAX = XMAX + MARJ
           MARJ = EM02*(YMAX-YMIN)
           YMIN = YMIN - MARJ
           YMAX = YMAX + MARJ
           MARJ = EM02*(ZMAX-ZMIN)
           ZMIN = ZMIN - MARJ
           ZMAX = ZMAX + MARJ
C
           IF (XMIN <= XI.AND.XMAX >= XI.AND.   ! If a node is in the box limited by GAP and DEPTH
     .         YMIN <= YI.AND.YMAX >= YI.AND.   ! it is selected
     .         ZMIN <= ZI.AND.ZMAX >= ZI) THEN  
             NLS=NLS+1
             LIST(NLS)=IS
C
             XI_L(NLS) = XI
             YI_L(NLS) = YI
             ZI_L(NLS) = ZI
C 
             IX1_L(NLS) = IX1(IS)
             IX2_L(NLS) = IX2(IS)
             IX3_L(NLS) = IX3(IS)
             IX4_L(NLS) = IX4(IS)
C
             X1_L(NLS) = X1
             Y1_L(NLS) = Y1
             Z1_L(NLS) = Z1
             X2_L(NLS) = X2
             Y2_L(NLS) = Y2
             Z2_L(NLS) = Z2
             X3_L(NLS) = X3
             Y3_L(NLS) = Y3
             Z3_L(NLS) = Z3
             X4_L(NLS) = X4
             Y4_L(NLS) = Y4
             Z4_L(NLS) = Z4
C       
             NNX1(NLS) = NX1
             NNY1(NLS) = NY1
             NNZ1(NLS) = NZ1
             NNX2(NLS) = NX2
             NNY2(NLS) = NY2
             NNZ2(NLS) = NZ2
             NNX3(NLS) = NX3
             NNY3(NLS) = NY3
             NNZ3(NLS) = NZ3
             NNX4(NLS) = NX4
             NNY4(NLS) = NY4
             NNZ4(NLS) = NZ4
C
           ENDIF
          ENDIF
        ENDDO
C        
        IF (DEBUG(3)>=1) NB_STOK_N(ITASK+1) = NB_STOK_N(ITASK+1) + NLS
C
        IF (IMONM > 0 .AND. ITASK+1 == 1) CALL STARTIME(77,1) ! Counting DST time
C
        NLT=NLS
        NLS=0
        CALL I21DST3(
     .       NLT     ,LIST    ,CAND_N(JS+1) ,CAND_E(JS+1) , IX1_L ,
     .       IX2_L   ,IX3_L   ,IX4_L        ,GAPV         ,XI_L   , 
     .       YI_L    ,ZI_L    ,IRTLM        ,CSTS         ,DEPTH2 ,
     .       NNX1    ,NNY1    ,NNZ1         ,NNX2         ,NNY2   , 
     .       NNZ2    ,NNX3    ,NNY3         ,NNZ3         ,NNX4   ,
     .       NNY4    ,NNZ4    ,X1_L         ,Y1_L         ,Z1_L   ,
     .       X2_L    ,Y2_L    ,Z2_L         ,X3_L         ,Y3_L   ,
     .       Z3_L    ,X4_L    ,Y4_L         ,Z4_L         ,DRAD2  ,
     .       DGAPLOAD)
       ENDIF
       JS = JS + NSEG
      ENDDO
      
C-----------------------------------------------
C
      call my_barrier
C
      DO J=ITASK+1,NSN,NTHREAD
        IF(IRTLM(1,J) > 0)THEN
ccc        irtlm < 0 if gap < dist < dradiation
ccc        impact=impact+1
           IFPEN(J)=IFPEN(J)+1
        ELSEIF(IFPEN(J)/=0)THEN
           FTXSAV(J)=ZERO
           FTYSAV(J)=ZERO
           FTZSAV(J)=ZERO
           PENI(J)  =ZERO
           IFPEN(J) =0
        END IF
      ENDDO
C
#include "lockon.inc"
      LSKYI_COUNT=LSKYI_COUNT+COUNT_CAND*5
#include "lockoff.inc"
      call my_barrier
C
C-----------------------------------------------
      RETURN
      END
