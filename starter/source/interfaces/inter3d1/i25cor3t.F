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
Chd|  I25COR3T                      source/interfaces/inter3d1/i25cor3t.F
Chd|-- called by -----------
Chd|        I25STO                        source/interfaces/inter3d1/i25sto.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules1/tri7box.F      
Chd|====================================================================
      SUBROUTINE I25COR3T(JLT    ,X     ,IRECT   ,NSV   ,CAND_E ,
     1                   CAND_N  ,X1    ,X2      ,
     2                   X3      ,X4    ,Y1      ,Y2    ,Y3     ,
     3                   Y4      ,Z1    ,Z2      ,Z3    ,Z4     ,
     4                   XI      ,YI    ,ZI      ,STIF  ,IX1    ,
     5                   IX2     ,IX3   ,IX4     ,NSN   ,NRTM   ,
     6                   MARGE   ,GAP_S ,GAP_M   ,GAPV  ,ITYP   ,
     7                   IGAP  ,GAP_S_L ,GAP_M_L ,EDGE_L2,MSEGTYP,
     8                   ETYP    ,ICODE ,ISKEW   ,IBC    ,DRAD  )
C============================================================================
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
#include      "scr02_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*),
     .    JLT,IDT, NOINT, NSN , NRTM,ITYP, IGAP, MSEGTYP(*), ICODE(*), ISKEW(*)
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        ETYP(MVSIZ), IBC(MVSIZ)
C     REAL ou REAL*8
      my_real
     .   X(3,*), GAPV(*), GAP_S(*), GAP_M(*), MARGE,DRAD,
     .   GAP_S_L(*), GAP_M_L(*), EDGE_L2(*)
C     REAL ou REAL*8
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J  ,IL, L, NN, IG,JFT,IADD,
     .        IBCS, ISKS, IBCM(4), ISKM(4) 
C-----------------------------------------------
C
      IF(IGAP /= 3)THEN
        DO I=1,JLT
          J = CAND_N(I)
          L = CAND_E(I)
          GAPV(I)=MAX(GAP_S(J) + GAP_M(L),DRAD)
          IF(MSEGTYP(L)==0.OR.MSEGTYP(L)>NRTM)THEN
            GAPV(I)=MAX(GAPV(I)+MARGE,GAPV(I)+EDGE_L2(J))
          ELSE
            GAPV(I)=GAPV(I)+MARGE
          END IF
        END DO
      ELSE
        DO I=1,JLT
          J = CAND_N(I)
          L = CAND_E(I)
          GAPV(I)=MAX(GAP_S(J) + GAP_M(L),DRAD)
          GAPV(I)=MIN(GAP_S_L(J)+GAP_M_L(L),GAPV(I))
          IF(MSEGTYP(L)==0.OR.MSEGTYP(L)>NRTM)THEN
            GAPV(I)=MAX(GAPV(I)+MARGE,GAPV(I)+EDGE_L2(J))
          ELSE
            GAPV(I)=GAPV(I)+MARGE
          END IF
        END DO
      END IF
      DO I=1,JLT
        J = CAND_N(I)
        IG = NSV(J)
        XI(I) = X(1,IG)
        YI(I) = X(2,IG)
        ZI(I) = X(3,IG)
C
        L       = CAND_E(I)
        ETYP(I) =MSEGTYP(L)
C
        IX1(I)=IRECT(1,L)
        X1(I)=X(1,IX1(I))
        Y1(I)=X(2,IX1(I))
        Z1(I)=X(3,IX1(I))
C
        IX2(I)=IRECT(2,L)
        X2(I)=X(1,IX2(I))
        Y2(I)=X(2,IX2(I))
        Z2(I)=X(3,IX2(I))
C
        IX3(I)=IRECT(3,L)
        X3(I)=X(1,IX3(I))
        Y3(I)=X(2,IX3(I))
        Z3(I)=X(3,IX3(I))
C
        IX4(I)=IRECT(4,L)
        X4(I)=X(1,IX4(I))
        Y4(I)=X(2,IX4(I))
        Z4(I)=X(3,IX4(I))
C
      ENDDO
C
      IBC(1:JLT)=0
      DO I=1,JLT

        J=CAND_N(I)
        IBCS   =ICODE(NSV(J))/512
        IBCM(1)=ICODE(IX1(I))/512
        IBCM(2)=ICODE(IX2(I))/512
        IBCM(3)=ICODE(IX3(I))/512
        IBCM(4)=ICODE(IX4(I))/512

        IF((IBCS   ==1.OR.IBCS   ==3.OR.IBCS   ==5.OR.IBCS   ==7).AND.
     .     (IBCM(1)==1.OR.IBCM(1)==3.OR.IBCM(1)==5.OR.IBCM(1)==7).AND.
     .     (IBCM(2)==1.OR.IBCM(2)==3.OR.IBCM(2)==5.OR.IBCM(2)==7).AND.
     .     (IBCM(3)==1.OR.IBCM(3)==3.OR.IBCM(3)==5.OR.IBCM(3)==7).AND.
     .     (IBCM(4)==1.OR.IBCM(4)==3.OR.IBCM(4)==5.OR.IBCM(4)==7))THEN
          IBC(I)=IBC(I)+1
        END IF
        IF((IBCS   ==2.OR.IBCS   ==3.OR.IBCS   ==6.OR.IBCS   ==7).AND.
     .     (IBCM(1)==2.OR.IBCM(1)==3.OR.IBCM(1)==6.OR.IBCM(1)==7).AND.
     .     (IBCM(2)==2.OR.IBCM(2)==3.OR.IBCM(2)==6.OR.IBCM(2)==7).AND.
     .     (IBCM(3)==2.OR.IBCM(3)==3.OR.IBCM(3)==6.OR.IBCM(3)==7).AND.
     .     (IBCM(4)==2.OR.IBCM(4)==3.OR.IBCM(4)==6.OR.IBCM(4)==7))THEN
          IBC(I)=IBC(I)+2
        END IF
        IF((IBCS   ==4.OR.IBCS   ==5.OR.IBCS   ==6.OR.IBCS   ==7).AND.
     .     (IBCM(1)==4.OR.IBCM(1)==5.OR.IBCM(1)==6.OR.IBCM(1)==7).AND.
     .     (IBCM(2)==4.OR.IBCM(2)==5.OR.IBCM(2)==6.OR.IBCM(2)==7).AND.
     .     (IBCM(3)==4.OR.IBCM(3)==5.OR.IBCM(3)==6.OR.IBCM(3)==7).AND.
     .     (IBCM(4)==4.OR.IBCM(4)==5.OR.IBCM(4)==6.OR.IBCM(4)==7))THEN
          IBC(I)=IBC(I)+4
        END IF
      END DO

      RETURN
      END
