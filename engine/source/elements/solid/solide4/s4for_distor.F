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
      SUBROUTINE S4FOR_DISTOR(
     .     X1,        X2,        X3,        X4,
     .     Y1,        Y2,        Y3,        Y4,
     .     Z1,        Z2,        Z3,        Z4,
     .     VX1,       VX2,       VX3,       VX4,
     .     VY1,       VY2,       VY3,       VY4,
     .     VZ1,       VZ2,       VZ3,       VZ4,
     .     F11,       F12,       F13,       F14,
     .     F21,       F22,       F23,       F24,
     .     F31,       F32,       F33,       F34,
     .     STI,       FLD,     STI_C,        LL,
     .      MU,     FQMAX,     ISTAB,      NEL )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, DIMENSION(MVSIZ), INTENT(IN) :: ISTAB
      my_real, INTENT(IN) :: MU,FQMAX
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: FLD,
     .   X1,X2,X3,X4,
     .   Y1,Y2,Y3,Y4,
     .   Z1,Z2,Z3,Z4,
     .   VX1,VX2,VX3,VX4,
     .   VY1,VY2,VY3,VY4,
     .   VZ1,VZ2,VZ3,VZ4,LL,STI_C
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) ::
     .   F11,F21,F31,F12,F22,F32,
     .   F13,F23,F33,F14,F24,F34,STI
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   XC(MVSIZ),YC(MVSIZ),ZC(MVSIZ),STIF(MVSIZ),
     .   VC(MVSIZ,3),FORC_N(MVSIZ,3),FOR_T1(MVSIZ,3),
     .   FOR_T2(MVSIZ,3),FOR_T3(MVSIZ,3),FOR_T4(MVSIZ,3),
     .   FCX,FCY,FCZ,FAC,GAP_MAX,GAP_MIN,TOL_T,TOL_C,TOL_V,
     .   PENMIN(MVSIZ),PENREF(MVSIZ),MARGE(MVSIZ)
      INTEGER I,J,NCTL,IFCTL,IFC1(MVSIZ)
C-----------------------------------------------
C   S o u r c e   C o d e
C-----------------------------------------------
         TOL_C= ZEP2
         TOL_V = EIGHT
C---- element center
         DO I=1,NEL
           VC(I,1) = FOURTH*(VX1(I)+VX2(I)+VX3(I)+VX4(I))
           VC(I,2) = FOURTH*(VY1(I)+VY2(I)+VY3(I)+VY4(I))
           VC(I,3) = FOURTH*(VZ1(I)+VZ2(I)+VZ3(I)+VZ4(I))
           STIF(I) = STI_C(I)
           IFC1(I) = ISTAB(I)
         ENDDO
C         
        NCTL = 0
        FORC_N = ZERO
        FOR_T1 = ZERO
        FOR_T2 = ZERO
        FOR_T3 = ZERO
        FOR_T4 = ZERO
       CALL SFOR_VISN4(VC ,    FLD,   TOL_V,    IFC1,
     .                 VX1,    VX2,     VX3,     VX4,
     .                 VY1,    VY2,     VY3,     VY4,
     .                 VZ1,    VZ2,     VZ3,     VZ4,
     .              FOR_T1, FOR_T2,  FOR_T3,  FOR_T4,
     .               IFCTL, STIF  ,  MU    ,   NEL  )
         NCTL = NCTL + IFCTL
C---- element center
         DO I=1,NEL
           XC(I) = FOURTH*(X1(I)+X2(I)+X3(I)+X4(I))
           YC(I) = FOURTH*(Y1(I)+Y2(I)+Y3(I)+Y4(I))
           ZC(I) = FOURTH*(Z1(I)+Z2(I)+Z3(I)+Z4(I))
         ENDDO
! -- sorting for each 4 big seg. :
         GAP_MIN = TOL_C*EM02  !percentage
         GAP_MAX = FIVE*GAP_MIN
         PENMIN(1:NEL) = GAP_MIN*LL(1:NEL)
         PENREF(1:NEL) = GAP_MAX*LL(1:NEL)
!         MARGE(1:NEL) = TWO*GAP_MAX*LL(1:NEL)
C---- seg 1 : 1,2,3 (normal will be towords to inside)
         CALL SFOR_N2STRIA(XC,      YC,     ZC, 
     .                     X1,      X2,     X3,
     .                     Y1,      Y2,     Y3,
     .                     Z1,      Z2,     Z3,
     .                  FOR_T1, FOR_T2, FOR_T3,
     .                  FORC_N,  STI_C,   STIF, 
     .                  FQMAX , PENMIN, PENREF,
     .                    LL  ,  IFCTL,   NEL )
        NCTL = NCTL + IFCTL
C---- seg 2 : 1,4,2
        CALL SFOR_N2STRIA(XC,      YC,     ZC, 
     .                    X1,      X4,     X2,
     .                    Y1,      Y4,     Y2,
     .                    Z1,      Z4,     Z2,
     .                 FOR_T1, FOR_T4, FOR_T2,
     .                 FORC_N,  STI_C,   STIF, 
     .                 FQMAX , PENMIN, PENREF,
     .                   LL  ,  IFCTL,   NEL )
        NCTL = NCTL + IFCTL
C---- seg 3 : 2,4,3
        CALL SFOR_N2STRIA(XC,      YC,     ZC, 
     .                     X2,     X4,     X3,
     .                     Y2,     Y4,     Y3,
     .                     Z2,     Z4,     Z3,
     .                 FOR_T2, FOR_T4, FOR_T3,
     .                 FORC_N,  STI_C,   STIF, 
     .                 FQMAX , PENMIN, PENREF,
     .                   LL  ,  IFCTL,   NEL )
        NCTL = NCTL + IFCTL
C---- seg 4 : 1,3,4
        CALL SFOR_N2STRIA(XC,      YC,     ZC, 
     .                    X1,      X3,     X4,
     .                    Y1,      Y3,     Y4,
     .                    Z1,      Z3,     Z4,
     .                 FOR_T1, FOR_T3, FOR_T4,
     .                 FORC_N,  STI_C,   STIF, 
     .                 FQMAX , PENMIN, PENREF,
     .                   LL  ,  IFCTL,   NEL )
        NCTL = NCTL + IFCTL
C---- force assembalge and STI update (dt)
        IF (NCTL >0) THEN
          DO I=1,NEL
             IF (STI_C(I)==ZERO) CYCLE
             FCX = FOURTH*FORC_N(I,1)
             FCY = FOURTH*FORC_N(I,2)
             FCZ = FOURTH*FORC_N(I,3)
             F11(I)=F11(I) + FOR_T1(I,1) + FCX
             F21(I)=F21(I) + FOR_T1(I,2) + FCY
             F31(I)=F31(I) + FOR_T1(I,3) + FCZ
             F12(I)=F12(I) + FOR_T2(I,1) + FCX
             F22(I)=F22(I) + FOR_T2(I,2) + FCY
             F32(I)=F32(I) + FOR_T2(I,3) + FCZ
             F13(I)=F13(I) + FOR_T3(I,1) + FCX
             F23(I)=F23(I) + FOR_T3(I,2) + FCY
             F33(I)=F33(I) + FOR_T3(I,3) + FCZ
             F14(I)=F14(I) + FOR_T4(I,1) + FCX
             F24(I)=F24(I) + FOR_T4(I,2) + FCY
             F34(I)=F34(I) + FOR_T4(I,3) + FCZ
C             
             STI(I) = MAX(STI(I),STIF(I))  ! elem dt will be looked later
          END DO 
        END IF !(NCTL >0) THEN
      
      
       

      RETURN
      END
      
