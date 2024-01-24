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
Chd|  S8FOR_DISTOR                  source/elements/solid/solide/s8for_distor.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        SFOR_N2S4                     source/elements/solid/solide/sfor_n2s4.F
Chd|        SFOR_VISN8                    source/elements/solid/solide/sfor_visn8.F
Chd|====================================================================
      SUBROUTINE S8FOR_DISTOR(
     .   X1,      X2,      X3,      X4,      
     .   X5,      X6,      X7,      X8,      
     .   Y1,      Y2,      Y3,      Y4,      
     .   Y5,      Y6,      Y7,      Y8,      
     .   Z1,      Z2,      Z3,      Z4,      
     .   Z5,      Z6,      Z7,      Z8,      
     .   VX1,     VX2,     VX3,     VX4,
     .   VX5,     VX6,     VX7,     VX8,
     .   VY1,     VY2,     VY3,     VY4,
     .   VY5,     VY6,     VY7,     VY8,
     .   VZ1,     VZ2,     VZ3,     VZ4,
     .   VZ5,     VZ6,     VZ7,     VZ8,
     .   F11,     F12,     F13,     F14,
     .   F15,     F16,     F17,     F18,
     .   F21,     F22,     F23,     F24,
     .   F25,     F26,     F27,     F28,
     .   F31,     F32,     F33,     F34,
     .   F35,     F36,     F37,     F38,
     .   STI,   STI_C,     FLD,     MU ,
     .   LL ,   ISTAB,     FQMAX,   NEL)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, DIMENSION(MVSIZ), INTENT(IN) :: ISTAB
      my_real, DIMENSION(NEL), INTENT(IN) :: STI_C
      my_real, INTENT(IN) :: MU,FQMAX
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: FLD,LL,
     1   X1,      X2,      X3,      X4,      
     2   X5,      X6,      X7,      X8,      
     3   Y1,      Y2,      Y3,      Y4,      
     4   Y5,      Y6,      Y7,      Y8,      
     5   Z1,      Z2,      Z3,      Z4,      
     6   Z5,      Z6,      Z7,      Z8,      
     4   VX1,     VX2,     VX3,     VX4,
     5   VX5,     VX6,     VX7,     VX8,
     6   VY1,     VY2,     VY3,     VY4,
     7   VY5,     VY6,     VY7,     VY8,
     8   VZ1,     VZ2,     VZ3,     VZ4,
     9   VZ5,     VZ6,     VZ7,     VZ8
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: STI,
     4   F11,     F12,     F13,     F14,
     5   F15,     F16,     F17,     F18,
     6   F21,     F22,     F23,     F24,
     7   F25,     F26,     F27,     F28,
     8   F31,     F32,     F33,     F34,
     9   F35,     F36,     F37,     F38
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   XC(MVSIZ),YC(MVSIZ),ZC(MVSIZ),STIF(MVSIZ),
     .   VC(MVSIZ,3),FORC_N(MVSIZ,3),FOR_T1(MVSIZ,3),
     .   FOR_T2(MVSIZ,3),FOR_T3(MVSIZ,3),FOR_T4(MVSIZ,3),
     .   FOR_T5(MVSIZ,3),FOR_T6(MVSIZ,3),FOR_T7(MVSIZ,3),
     .   FOR_T8(MVSIZ,3),FCX,FCY,FCZ,FAC,GAP_MAX,GAP_MIN,
     .   PENMIN(MVSIZ),PENREF(MVSIZ),MARGE(MVSIZ),
     .   TOL_T,TOL_C,TOL_V
      INTEGER I,J,NCTL,IFCTL,IFC1(MVSIZ)
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------
C-----------------------------------------------
C   S o u r c e   C o d e
C-----------------------------------------------
         TOL_C= ZEP2
         TOL_V = TEN
         DO I=1,NEL
           VC(I,1) = ONE_OVER_8*(VX1(I)+VX2(I)+VX3(I)+VX4(I)+
     .                           VX5(I)+VX6(I)+VX7(I)+VX8(I))
           VC(I,2) = ONE_OVER_8*(VY1(I)+VY2(I)+VY3(I)+VY4(I)+
     .                           VY5(I)+VY6(I)+VY7(I)+VY8(I))
           VC(I,3) = ONE_OVER_8*(VZ1(I)+VZ2(I)+VZ3(I)+VZ4(I)+
     .                           VZ5(I)+VZ6(I)+VZ7(I)+VZ8(I))
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
        FOR_T5 = ZERO
        FOR_T6 = ZERO
        FOR_T7 = ZERO
        FOR_T8 = ZERO
       CALL SFOR_VISN8(VC ,    FLD,   TOL_V,    IFC1,
     .                 VX1,    VX2,     VX3,     VX4,
     .                 VX5,    VX6,     VX7,     VX8,
     .                 VY1,    VY2,     VY3,     VY4,
     .                 VY5,    VY6,     VY7,     VY8,
     .                 VZ1,    VZ2,     VZ3,     VZ4,
     .                 VZ5,    VZ6,     VZ7,     VZ8,
     .              FOR_T1, FOR_T2,  FOR_T3,  FOR_T4,
     .              FOR_T5, FOR_T6,  FOR_T7,  FOR_T8,
     .               IFCTL, STIF  ,  MU    ,   NEL  )
       IF (IFCTL >0) THEN
         NCTL = NCTL + IFCTL
C---- element center
         DO I=1,NEL
           XC(I) = ONE_OVER_8*(X1(I)+X2(I)+X3(I)+X4(I)+
     .                           X5(I)+X6(I)+X7(I)+X8(I))
           YC(I) = ONE_OVER_8*(Y1(I)+Y2(I)+Y3(I)+Y4(I)+
     .                           Y5(I)+Y6(I)+Y7(I)+Y8(I))
           ZC(I) = ONE_OVER_8*(Z1(I)+Z2(I)+Z3(I)+Z4(I)+
     .                           Z5(I)+Z6(I)+Z7(I)+Z8(I))
         ENDDO
!    IFC1 is used for contact        
         GAP_MIN = TOL_C*EM02  !percentage
         GAP_MAX = FIVE*GAP_MIN
         PENMIN(1:NEL) = GAP_MIN*LL(1:NEL)
         PENREF(1:NEL) = GAP_MAX*LL(1:NEL)
         MARGE(1:NEL) = TWO*GAP_MAX*LL(1:NEL)
C---- seg 1 : 2,1,4, 3 normal towords to outside like tetra
         CALL SFOR_N2S4(   XC,      YC,     ZC,   STIF,     
     .                     X2,     X1,     X4,     X3,
     .                     Y2,     Y1,     Y4,     Y3,
     .                     Z2,     Z1,     Z4,     Z3,
     .                 FOR_T2, FOR_T1, FOR_T4, FOR_T3,
     .                 FORC_N,    LL ,  IFCTL,  IFC1 ,
     .                 PENMIN, PENREF,  MARGE,  FQMAX,
     .                  STI_C,   NEL )
         NCTL = NCTL + IFCTL
C---- seg 2 : 1,2,6,5
         CALL SFOR_N2S4(   XC,      YC,     ZC,   STIF,     
     .                    X1,      X2,     X6,     X5,
     .                    Y1,      Y2,     Y6,     Y5,
     .                    Z1,      Z2,     Z6,     Z5,
     .                 FOR_T1, FOR_T2, FOR_T6, FOR_T5,
     .                 FORC_N,    LL ,  IFCTL,  IFC1 ,
     .                 PENMIN, PENREF,  MARGE,  FQMAX,
     .                  STI_C,   NEL )
         NCTL = NCTL + IFCTL
C---- seg 3 : 2,3,7,6
         CALL SFOR_N2S4(   XC,      YC,     ZC,   STIF,     
     .                     X2,     X3,     X7,     X6,
     .                     Y2,     Y3,     Y7,     Y6,
     .                     Z2,     Z3,     Z7,     Z6,
     .                 FOR_T2, FOR_T3, FOR_T7, FOR_T6,
     .                 FORC_N,    LL ,  IFCTL,  IFC1 ,
     .                 PENMIN, PENREF,  MARGE,  FQMAX,
     .                  STI_C,   NEL )
         NCTL = NCTL + IFCTL
C---- seg 4 : 1,5,8,4
         CALL SFOR_N2S4(   XC,      YC,     ZC,   STIF,     
     .                    X1,      X5,     X8,     X4,
     .                    Y1,      Y5,     Y8,     Y4,
     .                    Z1,      Z5,     Z8,     Z4,
     .                 FOR_T1, FOR_T5, FOR_T8, FOR_T4,
     .                 FORC_N,    LL ,  IFCTL,  IFC1 ,
     .                 PENMIN, PENREF,  MARGE,  FQMAX,
     .                  STI_C,   NEL )
         NCTL = NCTL + IFCTL
C---- seg 5 : 4,8,7,3
         CALL SFOR_N2S4(   XC,      YC,     ZC,   STIF,     
     .                     X4,     X8,     X7,     X3,
     .                     Y4,     Y8,     Y7,     Y3,
     .                     Z4,     Z8,     Z7,     Z3,
     .                 FOR_T4, FOR_T8, FOR_T7, FOR_T3,
     .                 FORC_N,    LL ,  IFCTL,  IFC1 ,
     .                 PENMIN, PENREF,  MARGE,  FQMAX,
     .                  STI_C,   NEL )
         NCTL = NCTL + IFCTL
C---- seg 6 : 5,6,7,8
         CALL SFOR_N2S4(   XC,      YC,     ZC,   STIF,     
     .                     X5,     X6,     X7,     X8,
     .                     Y5,     Y6,     Y7,     Y8,
     .                     Z5,     Z6,     Z7,     Z8,
     .                 FOR_T5, FOR_T6, FOR_T7, FOR_T8,
     .                 FORC_N,    LL ,  IFCTL,  IFC1 ,
     .                 PENMIN, PENREF,  MARGE,  FQMAX,
     .                  STI_C,   NEL )
         NCTL = NCTL + IFCTL
C---- force assembalge and STI update (dt)
          DO I=1,NEL
             FCX = ONE_OVER_8*FORC_N(I,1)
             FCY = ONE_OVER_8*FORC_N(I,2)
             FCZ = ONE_OVER_8*FORC_N(I,3)
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
             F15(I)=F15(I) + FOR_T5(I,1) + FCX
             F25(I)=F25(I) + FOR_T5(I,2) + FCY
             F35(I)=F35(I) + FOR_T5(I,3) + FCZ
             F16(I)=F16(I) + FOR_T6(I,1) + FCX
             F26(I)=F26(I) + FOR_T6(I,2) + FCY
             F36(I)=F36(I) + FOR_T6(I,3) + FCZ
             F17(I)=F17(I) + FOR_T7(I,1) + FCX
             F27(I)=F27(I) + FOR_T7(I,2) + FCY
             F37(I)=F37(I) + FOR_T7(I,3) + FCZ
             F18(I)=F18(I) + FOR_T8(I,1) + FCX
             F28(I)=F28(I) + FOR_T8(I,2) + FCY
             F38(I)=F38(I) + FOR_T8(I,3) + FCZ
C             
             STI(I) = MAX(STI(I),STIF(I))  ! elem dt will be looked later
          END DO 
        ENDIF !(IFCTL >0) THEN
      

      RETURN
      END
      
