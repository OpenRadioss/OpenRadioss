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
Chd|  I24COR3T                      source/interfaces/intsort/i24cor3t.F
Chd|-- called by -----------
Chd|        I24STO                        source/interfaces/intsort/i24sto.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24COR3T(JLT    ,X     ,IRECT   ,NSV   ,CAND_E ,
     1                   CAND_N  ,X1    ,X2     ,
     2                   X3      ,X4    ,Y1      ,Y2    ,Y3     ,
     3                   Y4      ,Z1    ,Z2      ,Z3    ,Z4     ,
     4                   XI      ,YI    ,ZI      ,STIF  ,IX1    ,
     5                   IX2     ,IX3   ,IX4     ,NSN   ,GAP_S  ,
     6                   GAP_M   ,GAPV  ,CURV_MAX,ITYP  ,NIN    ,
     7                   V       ,PENE_OLD,GAPVE ,EDGE_L2,IEDGE ,
     8                   DGAPLOAD)
C============================================================================
C  cette routine est appelee par : I10STO(/int10/i10sto.F)
C                                  I7STO(/int7/i7sto.F)
C----------------------------------------------------------------------------
C  cette routine appelle : -
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
#include      "com08_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*),
     .    JLT,IDT, NOINT, NSN ,ITYP, NIN,IEDGE
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ)
C     REAL
      my_real , INTENT(IN) :: DGAPLOAD
      my_real
     .   X(3,*), V(3,*), GAPV(*), GAP_S(*), GAP_M(*),CURV_MAX(*),
     .   PENE_OLD(5,NSN), GAPVE(*),EDGE_L2(*)
C     REAL
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J  ,IL, L, NN, IG,JFT,IADD
      my_real
     .    VDT(MVSIZ),PENE_L(MVSIZ)
      my_real
     .        VX,VY,VZ,VXI,VYI,VZI,
     .        VX1,VX2,VX3,VX4,VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,PMAX
C-----------------------------------------------
C
      VDT = ZERO
      GAPVE(1:JLT) = ZERO

      IF(IMACH/=3) THEN
       DO I=1,JLT
         GAPV(I)=GAP_S(CAND_N(I)) + GAP_M(CAND_E(I))
         GAPV(I) =GAPV(I) + DGAPLOAD
       END DO
       DO I=1,JLT
         IG = NSV(CAND_N(I))
         XI(I) = X(1,IG)
         YI(I) = X(2,IG)
         ZI(I) = X(3,IG)
         L  = CAND_E(I)
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
         VXI = V(1,IG)
         VX1 = V(1,IX1(I))
         VX2 = V(1,IX2(I))
         VX3 = V(1,IX3(I))
         VX4 = V(1,IX4(I))
         VX=MAX(MAX(VX1,VX2,VX3,VX4)-VXI,VXI-MIN(VX1,VX2,VX3,VX4))
         VYI = V(2,IG)
         VY1 = V(2,IX1(I))
         VY2 = V(2,IX2(I))
         VY3 = V(2,IX3(I))
         VY4 = V(2,IX4(I))
         VY=MAX(MAX(VY1,VY2,VY3,VY4)-VYI,VYI-MIN(VY1,VY2,VY3,VY4))
         VZI = V(3,IG)
         VZ1 = V(3,IX1(I))
         VZ2 = V(3,IX2(I))
         VZ3 = V(3,IX3(I))
         VZ4 = V(3,IX4(I))
         VZ=MAX(MAX(VZ1,VZ2,VZ3,VZ4)-VZI,VZI-MIN(VZ1,VZ2,VZ3,VZ4))
         VDT(I) = (VX+VY+VZ)*DT1/TWO
       END DO
      ELSE
       DO I=1,JLT
           J = CAND_N(I)
           IF(J<=NSN) THEN
             GAPV(I)=GAP_S(J) + GAP_M(CAND_E(I))
           ELSE
             IG = J-NSN
             GAPV(I)=XREM(9,IG) + GAP_M(CAND_E(I))
           END IF
           GAPV(I) =GAPV(I) + DGAPLOAD
       END DO
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
           PENE_L(I) = PENE_OLD(3,J)
         ELSE
           IG = J-NSN
           XI(I) = XREM(1,IG)
           YI(I) = XREM(2,IG)
           ZI(I) = XREM(3,IG)
           VXI   = XREM(4,IG)
           VYI   = XREM(5,IG)
           VZI   = XREM(6,IG)
           PENE_L(I) = XREM(I24XREMP+6,IG)
         ENDIF
C
         L  = CAND_E(I)
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
      ENDIF

      IF(IEDGE/=0)THEN
        DO I=1,JLT
          GAPVE(I) = GAPV(I) + VDT(I) + EDGE_L2(CAND_N(I))
        END DO
      ENDIF

      DO I=1,JLT
        GAPV(I) = MAX(GAPV(I),PENE_L(I)) + CURV_MAX(CAND_E(I)) + VDT(I)
        GAPV(I) = ONEP01*GAPV(I)

      END DO
C
      RETURN
      END
