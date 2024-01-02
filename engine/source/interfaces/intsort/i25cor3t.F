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
Chd|  I25COR3T                      source/interfaces/intsort/i25cor3t.F
Chd|-- called by -----------
Chd|        I25STO                        source/interfaces/intsort/i25sto.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25COR3T(JLT    ,X     ,IRECT   ,NSV   ,CAND_E ,
     1                   CAND_N  ,X1    ,X2     ,
     2                   X3      ,X4    ,Y1      ,Y2    ,Y3     ,
     3                   Y4      ,Z1    ,Z2      ,Z3    ,Z4     ,
     4                   XI      ,YI    ,ZI      ,STIF  ,IX1    ,
     5                   IX2     ,IX3   ,IX4     ,NSN   ,GAP_S  ,
     6                   GAP_M   ,GAPV  ,CURV_MAX,ITYP  ,NIN    ,
     7                   V       ,IGAP  ,GAP_S_L ,GAP_M_L,MSEGTYP,
     8                   ETYP    ,ICODT ,ISKEW   ,IBC    ,DRAD  ,
     9                   DGAPLOAD)        
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
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*), 
     .    JLT,IDT, NOINT, NSN ,ITYP, NIN, IGAP, MSEGTYP(*), ICODT(*), ISKEW(*)
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        ETYP(MVSIZ), IBC(MVSIZ)
C     REAL
      my_real , INTENT(IN) :: DGAPLOAD ,DRAD
      my_real
     .   X(3,*), V(3,*), GAPV(*), GAP_S(*), GAP_M(*),CURV_MAX(*),
     .   GAP_S_L(*), GAP_M_L(*)
C     REAL
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
      my_real
     .    VDT(MVSIZ)
      my_real
     .        VX,VY,VZ,VXI,VYI,VZI,
     .        VX1,VX2,VX3,VX4,VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,PMAX
C-----------------------------------------------
C
      VDT = ZERO
      IF(IGAP /= 3)THEN
        DO I=1,JLT
          J = CAND_N(I)
          IF(J<=NSN) THEN
            GAPV(I)=GAP_S(J) + GAP_M(CAND_E(I))
          ELSE
            IG = J-NSN
            GAPV(I)=XREM(9,IG) + GAP_M(CAND_E(I))
          END IF
          GAPV(I)=MAX(DRAD,GAPV(I)+DGAPLOAD)
        END DO
      ELSE
        DO I=1,JLT
          J = CAND_N(I)
          IF(J<=NSN) THEN
            GAPV(I)=GAP_S(J) + GAP_M(CAND_E(I))
            GAPV(I)=MIN(GAP_S_L(J)+GAP_M_L(CAND_E(I)),GAPV(I))
          ELSE
            IG = J-NSN
            GAPV(I)=XREM(9,IG) + GAP_M(CAND_E(I))
            GAPV(I)=MIN(XREM(10,IG)+GAP_M_L(CAND_E(I)),GAPV(I))
          END IF
          GAPV(I)=MAX(DRAD,GAPV(I)+DGAPLOAD)
        END DO
      END IF
      DO I=1,JLT
        J = CAND_N(I)
        IF(J<=NSN) THEN
          IG = NSV(J)
          XI(I) = X(1,IG)
          YI(I) = X(2,IG)
          ZI(I) = X(3,IG)
          VXI   = V(1,IG)
          VYI   = V(2,IG)
          VZI   = V(3,IG)
        ELSE
          IG = J-NSN
          XI(I) = XREM(1,IG)
          YI(I) = XREM(2,IG)
          ZI(I) = XREM(3,IG)
          VXI   = XREM(4,IG)
          VYI   = XREM(5,IG)
          VZI   = XREM(6,IG)
        ENDIF
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
        VX1 = V(1,IX1(I))
        VX2 = V(1,IX2(I))
        VX3 = V(1,IX3(I))
        VX4 = V(1,IX4(I))
        VX=MAX(MAX(VX1,VX2,VX3,VX4)-VXI,VXI-MIN(VX1,VX2,VX3,VX4))
        VY1 = V(2,IX1(I))
        VY2 = V(2,IX2(I))
        VY3 = V(2,IX3(I))
        VY4 = V(2,IX4(I))
        VY=MAX(MAX(VY1,VY2,VY3,VY4)-VYI,VYI-MIN(VY1,VY2,VY3,VY4))
        VZ1 = V(3,IX1(I))
        VZ2 = V(3,IX2(I))
        VZ3 = V(3,IX3(I))
        VZ4 = V(3,IX4(I))
        VZ=MAX(MAX(VZ1,VZ2,VZ3,VZ4)-VZI,VZI-MIN(VZ1,VZ2,VZ3,VZ4))
        VDT(I) = (VX+VY+VZ)*DT1
      ENDDO

      DO I=1,JLT
        GAPV(I) = GAPV(I) + CURV_MAX(CAND_E(I)) + VDT(I)
        GAPV(I) = ONEP01*GAPV(I)
      END DO
C
      IBC(1:JLT)=0
      DO I=1,JLT

        J = CAND_N(I)
        IF(J <= NSN)THEN
          IBCS =ICODT(NSV(J))
        ELSE
          IG = J - NSN
          ! ICODT 
          IBCS = IREM(7,IG)
        END IF

        IBCM(1)=ICODT(IX1(I))
        IBCM(2)=ICODT(IX2(I))
        IBCM(3)=ICODT(IX3(I))
        IBCM(4)=ICODT(IX4(I))

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
C
      RETURN
      END
