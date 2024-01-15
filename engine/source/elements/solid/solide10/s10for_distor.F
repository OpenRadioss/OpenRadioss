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
      SUBROUTINE S10FOR_DISTOR(
     1              STI,   FLD  ,   STI_C,   
     2              XX ,     YY ,     ZZ ,   
     3              VX ,     VY ,     VZ ,     
     4              FX ,     FY ,     FZ ,     
     5              XX0,     YY0,     ZZ0,
     7              MU ,   ISTAB,     LL ,
     8            FQMAX,    NEL )
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
      my_real, INTENT(IN)                   :: MU,FQMAX
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: FLD,LL
      DOUBLE PRECISION, DIMENSION(MVSIZ,10), INTENT(IN) :: 
     .                      XX,YY,ZZ,XX0,YY0,ZZ0
      my_real, DIMENSION(MVSIZ,10), INTENT(IN) ::
     .                                VX,VY,VZ
      my_real, DIMENSION(MVSIZ,10), INTENT(INOUT) ::
     .                                FX,FY,FZ
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) ::STI,STI_C
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   XC(MVSIZ,5),YC(MVSIZ,5),ZC(MVSIZ,5),STIF(MVSIZ),
     .   VC(MVSIZ,3),FORC_N(MVSIZ,3),FOR_T1(MVSIZ,3),
     .   FOR_T2(MVSIZ,3),FOR_T3(MVSIZ,3),FOR_T4(MVSIZ,3),
     .   FOR_T5(MVSIZ,3),FOR_T6(MVSIZ,3),FOR_T7(MVSIZ,3),
     .   FOR_T8(MVSIZ,3),FOR_T9(MVSIZ,3),FOR_T10(MVSIZ,3),
     .   FORC_N1(MVSIZ,3),FORC_N2(MVSIZ,3),FORC_N3(MVSIZ,3),
     .   PENMIN(MVSIZ),PENREF(MVSIZ),MARGE(MVSIZ),
     .   FORC_N4(MVSIZ,3),FCX,FCY,FCZ,FAC,GAP_MAX,GAP_MIN,
     .   TOL_T,TOL_C, TOL_V, TOL_E    
      INTEGER I,J,NCTL,IFCTL,IFC1(MVSIZ),IFC2(MVSIZ)
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------
C-----------------------------------------------
C   S o u r c e   C o d e
C-----------------------------------------------
         TOL_V = TEN
!   velocity gradient and istab as 1er sorting
        VC = ZERO
        DO J =1,10
         DO I=1,NEL
           VC(I,1) = VC(I,1)+VX(I,J)
           VC(I,2) = VC(I,2)+VY(I,J)
           VC(I,3) = VC(I,3)+VZ(I,J)
         ENDDO
        END DO 
        DO I=1,NEL
           VC(I,1) = VC(I,1)*EM01
           VC(I,2) = VC(I,2)*EM01
           VC(I,3) = VC(I,3)*EM01
           STIF(I) = STI_C(I)
           IFC1(I) = ISTAB(I)
        ENDDO
        FORC_N  = ZERO
        FORC_N1 = ZERO
        FORC_N2 = ZERO
        FORC_N3 = ZERO
        FORC_N4 = ZERO
        FOR_T1 = ZERO
        FOR_T2 = ZERO
        FOR_T3 = ZERO
        FOR_T4 = ZERO
        FOR_T5 = ZERO
        FOR_T6 = ZERO
        FOR_T7 = ZERO
        FOR_T8 = ZERO
        FOR_T9 = ZERO
        FOR_T10 = ZERO
        NCTL = 0
        CALL SFOR_VISN10(VC ,    FLD,   TOL_V,      MU,    
     .                   VX ,     VY,      VZ,   IFCTL,
     .                FOR_T1, FOR_T2,  FOR_T3,  FOR_T4,
     .                FOR_T5, FOR_T6,  FOR_T7,  FOR_T8,
     .                FOR_T9, FOR_T10, STIF  ,    IFC1,
     .                NEL  )
        NCTL = NCTL + IFCTL
C---- element center
        XC = ZERO
        YC = ZERO
        ZC = ZERO
        DO J =1,10
         DO I=1,NEL
           XC(I,5) = XC(I,5)+XX(I,J)
           YC(I,5) = YC(I,5)+YY(I,J)
           ZC(I,5) = ZC(I,5)+ZZ(I,J)
         ENDDO
        END DO 
        DO I=1,NEL
           XC(I,5) = XC(I,5)*EM01
           YC(I,5) = YC(I,5)*EM01
           ZC(I,5) = ZC(I,5)*EM01
        ENDDO
C---- middle node treatment first
        TOL_E = EM01 ! %
        CALL S10FOR_EDGEC(
     .              STI,    LL  ,   STI_C,   
     .              XX ,     YY ,     ZZ ,   
     .              XX0,     YY0,     ZZ0,   
     .           FOR_T1,  FOR_T2,  FOR_T3,
     .           FOR_T4,  FOR_T5,  FOR_T6,
     .           FOR_T7,  FOR_T8,  FOR_T9,
     .          FOR_T10,   TOL_E,   IFC2 ,     
     .           IFCTL ,   NEL  )
        NCTL = NCTL + IFCTL
c-- contact by sub-segs       
c-- sorting for each 4 big seg; IFC1 is used for contact only       
        TOL_C= ZEP2
        GAP_MIN = TOL_C*EM02  !percentage
        GAP_MAX = FIVE*GAP_MIN
        MARGE(1:NEL) = GAP_MAX*LL(1:NEL)
        PENMIN(1:NEL) = GAP_MIN*LL(1:NEL)*HALF
        PENREF(1:NEL) = GAP_MAX*LL(1:NEL)*HALF
        DO I=1,NEL
           XC(I,1) = FOURTH*(XX(I,1)+XX(I,5)+XX(I,7)+XX(I,8))
           YC(I,1) = FOURTH*(YY(I,1)+YY(I,5)+YY(I,7)+YY(I,8))
           ZC(I,1) = FOURTH*(ZZ(I,1)+ZZ(I,5)+ZZ(I,7)+ZZ(I,8))
           XC(I,2) = FOURTH*(XX(I,2)+XX(I,5)+XX(I,6)+XX(I,9))
           YC(I,2) = FOURTH*(YY(I,2)+YY(I,5)+YY(I,6)+YY(I,9))
           ZC(I,2) = FOURTH*(ZZ(I,2)+ZZ(I,5)+ZZ(I,6)+ZZ(I,9))
           XC(I,3) = FOURTH*(XX(I,3)+XX(I,7)+XX(I,6)+XX(I,10))
           YC(I,3) = FOURTH*(YY(I,3)+YY(I,7)+YY(I,6)+YY(I,10))
           ZC(I,3) = FOURTH*(ZZ(I,3)+ZZ(I,7)+ZZ(I,6)+ZZ(I,10))
           XC(I,4) = FOURTH*(XX(I,4)+XX(I,8)+XX(I,9)+XX(I,10))
           YC(I,4) = FOURTH*(YY(I,4)+YY(I,8)+YY(I,9)+YY(I,10))
           ZC(I,4) = FOURTH*(ZZ(I,4)+ZZ(I,8)+ZZ(I,9)+ZZ(I,10))
        ENDDO
C         
C---- seg 1 : 1,2,3 (5 6 7) 
        CALL SFOR_N2STRIA2(
     .                XC(1,5),    YC(1,5),    ZC(1,5),
     .                XC(1,1),    YC(1,1),    ZC(1,1),
     .                XC(1,2),    YC(1,2),    ZC(1,2),
     .                XC(1,3),    YC(1,3),    ZC(1,3),
     .                XX(1,1),    XX(1,2),    XX(1,3),
     .                YY(1,1),    YY(1,2),    YY(1,3),
     .                ZZ(1,1),    ZZ(1,2),    ZZ(1,3),
     .                 FOR_T1,     FOR_T2,     FOR_T3,
     .                XX(1,5),    XX(1,6),    XX(1,7),
     .                YY(1,5),    YY(1,6),    YY(1,7),
     .                ZZ(1,5),    ZZ(1,6),    ZZ(1,7),
     .                 FOR_T5,     FOR_T6,     FOR_T7,
     .                FORC_N1,    FORC_N2,    FORC_N3,     
     .                 FORC_N,       STIF,      STI_C,     
     .                  FQMAX,      IFCTL,         LL,
     .                 PENMIN,     PENREF,      MARGE,
     .                   IFC1,      NEL )
        NCTL = NCTL + IFCTL
C---- seg 2 : 1,4,2 (8 9 5)
        CALL SFOR_N2STRIA2(
     .                XC(1,5),    YC(1,5),    ZC(1,5),
     .                XC(1,1),    YC(1,1),    ZC(1,1),
     .                XC(1,4),    YC(1,4),    ZC(1,4),
     .                XC(1,2),    YC(1,2),    ZC(1,2),
     .                XX(1,1),    XX(1,4),    XX(1,2),
     .                YY(1,1),    YY(1,4),    YY(1,2),
     .                ZZ(1,1),    ZZ(1,4),    ZZ(1,2),
     .                 FOR_T1,     FOR_T4,     FOR_T2,
     .                XX(1,8),    XX(1,9),    XX(1,5),
     .                YY(1,8),    YY(1,9),    YY(1,5),
     .                ZZ(1,8),    ZZ(1,9),    ZZ(1,5),
     .                 FOR_T8,     FOR_T9,     FOR_T5,
     .                FORC_N1,    FORC_N4,    FORC_N2,     
     .                 FORC_N,       STIF,      STI_C,     
     .                  FQMAX,      IFCTL,         LL,
     .                 PENMIN,     PENREF,      MARGE,
     .                   IFC1,      NEL )
        NCTL = NCTL + IFCTL
C---- seg 3 : 2,4,3 (9 10 6)
        CALL SFOR_N2STRIA2(
     .                XC(1,5),    YC(1,5),    ZC(1,5),
     .                XC(1,2),    YC(1,2),    ZC(1,2),
     .                XC(1,4),    YC(1,4),    ZC(1,4),
     .                XC(1,3),    YC(1,3),    ZC(1,3),
     .                XX(1,2),    XX(1,4),    XX(1,3),
     .                YY(1,2),    YY(1,4),    YY(1,3),
     .                ZZ(1,2),    ZZ(1,4),    ZZ(1,3),
     .                 FOR_T2,     FOR_T4,     FOR_T3,
     .                XX(1,9),   XX(1,10),    XX(1,6),
     .                YY(1,9),   YY(1,10),    YY(1,6),
     .                ZZ(1,9),   ZZ(1,10),    ZZ(1,6),
     .                 FOR_T9,    FOR_T10,     FOR_T6,
     .                FORC_N2,    FORC_N4,    FORC_N3,     
     .                 FORC_N,       STIF,      STI_C,     
     .                  FQMAX,      IFCTL,         LL,
     .                 PENMIN,     PENREF,      MARGE,
     .                   IFC1,      NEL )
        NCTL = NCTL + IFCTL
C---- seg 4 : 1,3,4 (7 10 8)
        CALL SFOR_N2STRIA2(
     .                XC(1,5),    YC(1,5),    ZC(1,5),
     .                XC(1,1),    YC(1,1),    ZC(1,1),
     .                XC(1,3),    YC(1,3),    ZC(1,3),
     .                XC(1,4),    YC(1,4),    ZC(1,4),
     .                XX(1,1),    XX(1,3),    XX(1,4),
     .                YY(1,1),    YY(1,3),    YY(1,4),
     .                ZZ(1,1),    ZZ(1,3),    ZZ(1,4),
     .                 FOR_T1,     FOR_T3,     FOR_T4,
     .                XX(1,7),   XX(1,10),    XX(1,8),
     .                YY(1,7),   YY(1,10),    YY(1,8),
     .                ZZ(1,7),   ZZ(1,10),    ZZ(1,8),
     .                 FOR_T7,    FOR_T10,     FOR_T8,
     .                FORC_N1,    FORC_N3,    FORC_N4,     
     .                 FORC_N,       STIF,      STI_C,     
     .                  FQMAX,      IFCTL,         LL,
     .                 PENMIN,     PENREF,      MARGE,
     .                   IFC1,      NEL )
        NCTL = NCTL + IFCTL
C---- force assembalge and STI update (dt)
        IF (NCTL >0) THEN
          DO I=1,NEL
            IF (STI_C(I) ==ZERO) CYCLE
             FCX = EM01*FORC_N(I,1)
             FCY = EM01*FORC_N(I,2)
             FCZ = EM01*FORC_N(I,3)
             FX(I,1) = FX(I,1) + FOR_T1(I,1) + FCX
             FY(I,1) = FY(I,1) + FOR_T1(I,2) + FCY
             FZ(I,1) = FZ(I,1) + FOR_T1(I,3) + FCZ
             FX(I,2) = FX(I,2) + FOR_T2(I,1) + FCX
             FY(I,2) = FY(I,2) + FOR_T2(I,2) + FCY
             FZ(I,2) = FZ(I,2) + FOR_T2(I,3) + FCZ
             FX(I,3) = FX(I,3) + FOR_T3(I,1) + FCX
             FY(I,3) = FY(I,3) + FOR_T3(I,2) + FCY
             FZ(I,3) = FZ(I,3) + FOR_T3(I,3) + FCZ
             FX(I,4) = FX(I,4) + FOR_T4(I,1) + FCX
             FY(I,4) = FY(I,4) + FOR_T4(I,2) + FCY
             FZ(I,4) = FZ(I,4) + FOR_T4(I,3) + FCZ
             FX(I,5) = FX(I,5) + FOR_T5(I,1) + FCX
             FY(I,5) = FY(I,5) + FOR_T5(I,2) + FCY
             FZ(I,5) = FZ(I,5) + FOR_T5(I,3) + FCZ
             FX(I,6) = FX(I,6) + FOR_T6(I,1) + FCX
             FY(I,6) = FY(I,6) + FOR_T6(I,2) + FCY
             FZ(I,6) = FZ(I,6) + FOR_T6(I,3) + FCZ
             FX(I,7) = FX(I,7) + FOR_T7(I,1) + FCX
             FY(I,7) = FY(I,7) + FOR_T7(I,2) + FCY
             FZ(I,7) = FZ(I,7) + FOR_T7(I,3) + FCZ
             FX(I,8) = FX(I,8) + FOR_T8(I,1) + FCX
             FY(I,8) = FY(I,8) + FOR_T8(I,2) + FCY
             FZ(I,8) = FZ(I,8) + FOR_T8(I,3) + FCZ
             FX(I,9) = FX(I,9) + FOR_T9(I,1) + FCX
             FY(I,9) = FY(I,9) + FOR_T9(I,2) + FCY
             FZ(I,9) = FZ(I,9) + FOR_T9(I,3) + FCZ
             FX(I,10)=FX(I,10) + FOR_T10(I,1) + FCX
             FY(I,10)=FY(I,10) + FOR_T10(I,2) + FCY
             FZ(I,10)=FZ(I,10) + FOR_T10(I,3) + FCZ
C             
             STI(I) = MAX(STI(I),STIF(I))
          END DO 
          DO I=1,NEL  ! 1 5 7 8
            IF (FORC_N1(I,1)==ZERO.AND.FORC_N1(I,2)==ZERO
     .            .AND.FORC_N1(I,3)==ZERO) CYCLE
             FCX = FOURTH*FORC_N1(I,1)
             FCY = FOURTH*FORC_N1(I,2)
             FCZ = FOURTH*FORC_N1(I,3)
             FX(I,1) = FX(I,1) +  FCX
             FY(I,1) = FY(I,1) +  FCY
             FZ(I,1) = FZ(I,1) +  FCZ
             FX(I,5) = FX(I,5) +  FCX
             FY(I,5) = FY(I,5) +  FCY
             FZ(I,5) = FZ(I,5) +  FCZ
             FX(I,7) = FX(I,7) +  FCX
             FY(I,7) = FY(I,7) +  FCY
             FZ(I,7) = FZ(I,7) +  FCZ
             FX(I,8) = FX(I,8) +  FCX
             FY(I,8) = FY(I,8) +  FCY
             FZ(I,8) = FZ(I,8) +  FCZ
          END DO 
          DO I=1,NEL  ! 2 5 6 9
            IF (FORC_N2(I,1)==ZERO.AND.FORC_N2(I,2)==ZERO
     .            .AND.FORC_N2(I,3)==ZERO) CYCLE
             FCX = FOURTH*FORC_N2(I,1)
             FCY = FOURTH*FORC_N2(I,2)
             FCZ = FOURTH*FORC_N2(I,3)
             FX(I,2) = FX(I,2) +  FCX
             FY(I,2) = FY(I,2) +  FCY
             FZ(I,2) = FZ(I,2) +  FCZ
             FX(I,5) = FX(I,5) +  FCX
             FY(I,5) = FY(I,5) +  FCY
             FZ(I,5) = FZ(I,5) +  FCZ
             FX(I,6) = FX(I,6) +  FCX
             FY(I,6) = FY(I,6) +  FCY
             FZ(I,6) = FZ(I,6) +  FCZ
             FX(I,9) = FX(I,9) +  FCX
             FY(I,9) = FY(I,9) +  FCY
             FZ(I,9) = FZ(I,9) +  FCZ
          END DO 
          DO I=1,NEL  ! 3 6 7 10
            IF (FORC_N3(I,1)==ZERO.AND.FORC_N3(I,2)==ZERO
     .            .AND.FORC_N3(I,3)==ZERO) CYCLE
             FCX = FOURTH*FORC_N3(I,1)
             FCY = FOURTH*FORC_N3(I,2)
             FCZ = FOURTH*FORC_N3(I,3)
             FX(I,3) = FX(I,3) + FCX
             FY(I,3) = FY(I,3) + FCY
             FZ(I,3) = FZ(I,3) + FCZ
             FX(I,6) = FX(I,6) + FCX
             FY(I,6) = FY(I,6) + FCY
             FZ(I,6) = FZ(I,6) + FCZ
             FX(I,7) = FX(I,7) + FCX
             FY(I,7) = FY(I,7) + FCY
             FZ(I,7) = FZ(I,7) + FCZ
             FX(I,10)=FX(I,10) + FCX
             FY(I,10)=FY(I,10) + FCY
             FZ(I,10)=FZ(I,10) + FCZ
          END DO 
          DO I=1,NEL  ! 4 8 9 10
            IF (FORC_N4(I,1)==ZERO.AND.FORC_N4(I,2)==ZERO
     .            .AND.FORC_N4(I,3)==ZERO) CYCLE
             FCX = FOURTH*FORC_N4(I,1)
             FCY = FOURTH*FORC_N4(I,2)
             FCZ = FOURTH*FORC_N4(I,3)
             FX(I,4) = FX(I,4) + FCX
             FY(I,4) = FY(I,4) + FCY
             FZ(I,4) = FZ(I,4) + FCZ
             FX(I,8) = FX(I,8) + FCX
             FY(I,8) = FY(I,8) + FCY
             FZ(I,8) = FZ(I,8) + FCZ
             FX(I,9) = FX(I,9) + FCX
             FY(I,9) = FY(I,9) + FCY
             FZ(I,9) = FZ(I,9) + FCZ
             FX(I,10)=FX(I,10) + FCX
             FY(I,10)=FY(I,10) + FCY
             FZ(I,10)=FZ(I,10) + FCZ
          END DO 
        ENDIF 
C      
      RETURN
      END
