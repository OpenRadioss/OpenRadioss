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
      SUBROUTINE S10FOR_EDGEC(
     .              STI,    LL  ,   STI_C,   
     .              XX ,     YY ,     ZZ ,   
     .              XX0,     YY0,     ZZ0,   
     .           FOR_T1,  FOR_T2,  FOR_T3,
     .           FOR_T4,  FOR_T5,  FOR_T6,
     .           FOR_T7,  FOR_T8,  FOR_T9,
     .          FOR_T10,   TOL_E,   IFCE ,     
     .           IFCTL ,   NEL  )
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
      INTEGER, INTENT (OUT) :: IFCTL
      INTEGER, DIMENSION(MVSIZ), INTENT(OUT) :: IFCE
      my_real, INTENT(IN) :: TOL_E
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: LL
      DOUBLE PRECISION, DIMENSION(MVSIZ,10), INTENT(IN) :: 
     .                       XX, YY, ZZ,
     .                      XX0,YY0,ZZ0
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) ::STI,STI_C
      my_real, DIMENSION(MVSIZ,3), INTENT (INOUT) :: FOR_T10,
     .                          FOR_T1, FOR_T2, FOR_T3,
     .                          FOR_T4, FOR_T5, FOR_T6,
     .                          FOR_T7, FOR_T8, FOR_T9
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   STIF(MVSIZ),DMIN(MVSIZ),DMIN2(MVSIZ),FN(MVSIZ,6),
     .   XM(MVSIZ,6),YM(MVSIZ,6),ZM(MVSIZ,6),KS(MVSIZ),
     .   NJX(MVSIZ,6),NJY(MVSIZ,6),NJZ(MVSIZ,6),FACT,
     .   DX,DY,DZ,D_MAX,LAM,S2,NORM,LAM0,F_Q,FAC,LENG,
     .   NX,NY,NZ,FX,FY,FZ,F_T,DS2(MVSIZ,6),TOL2,KTS,FNJ
      INTEGER I,J,JJ,IFC2(MVSIZ,6),IFF
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------
C-----------------------------------------------
C   S o u r c e   C o d e
C-----------------------------------------------
C---- 5(1,2),6(2,3),7(1,3),8(1,4),9(2,4),10(3,4)
         IFCTL = 0
         DO I=1,NEL
           IF (STI_C(I)==ZERO) CYCLE
           XM(I,1) = HALF*(XX(I,1)+XX(I,2))
           YM(I,1) = HALF*(YY(I,1)+YY(I,2))
           ZM(I,1) = HALF*(ZZ(I,1)+ZZ(I,2))
           XM(I,2) = HALF*(XX(I,3)+XX(I,2))
           YM(I,2) = HALF*(YY(I,3)+YY(I,2))
           ZM(I,2) = HALF*(ZZ(I,3)+ZZ(I,2))
           XM(I,3) = HALF*(XX(I,3)+XX(I,1))
           YM(I,3) = HALF*(YY(I,3)+YY(I,1))
           ZM(I,3) = HALF*(ZZ(I,3)+ZZ(I,1))
           XM(I,4) = HALF*(XX(I,4)+XX(I,1))
           YM(I,4) = HALF*(YY(I,4)+YY(I,1))
           ZM(I,4) = HALF*(ZZ(I,4)+ZZ(I,1))
           XM(I,5) = HALF*(XX(I,4)+XX(I,2))
           YM(I,5) = HALF*(YY(I,4)+YY(I,2))
           ZM(I,5) = HALF*(ZZ(I,4)+ZZ(I,2))
           XM(I,6) = HALF*(XX(I,4)+XX(I,3))
           YM(I,6) = HALF*(YY(I,4)+YY(I,3))
           ZM(I,6) = HALF*(ZZ(I,4)+ZZ(I,3))
           STIF(I) = STI_C(I)
           DMIN(I) = TOL_E*LL(I)
           DMIN2(I) = DMIN(I)*DMIN(I)
           IFCE(I)=0
         ENDDO
C---- first sorting
         IFF = 0
         IFC2 = 0
        DO J=1,6
          JJ = J + 4
          DO I=1,NEL
           IF (STI_C(I)==ZERO) CYCLE
            DX = XX(I,JJ) - XM(I,J)
            DY = YY(I,JJ) - YM(I,J)
            DZ = ZZ(I,JJ) - ZM(I,J)
            D_MAX = TWO*MAX(ABS(DX),ABS(DY),ABS(DZ))
            IF (D_MAX>DMIN(I)) THEN
                IFC2(I,J) = 1
                IFF = 1
            END IF
          ENDDO
        ENDDO
        IF (IFF >0) THEN
          TOL2 = TOL_E*TOL_E
C          
         DO I=1,NEL
           IF (IFC2(I,1)==0) CYCLE
           DX = XX0(I,1)-XX0(I,2)
           DY = YY0(I,1)-YY0(I,2)
           DZ = ZZ0(I,1)-ZZ0(I,2)
           DS2(I,1) =  DX*DX+DY*DY+DZ*DZ
         ENDDO
         DO I=1,NEL
           IF (IFC2(I,2)==0) CYCLE
           DX = XX0(I,3)-XX0(I,2)
           DY = YY0(I,3)-YY0(I,2)
           DZ = ZZ0(I,3)-ZZ0(I,2)
           DS2(I,2) =  DX*DX+DY*DY+DZ*DZ
         ENDDO
         DO I=1,NEL
           IF (IFC2(I,3)==0) CYCLE
           DX = XX0(I,1)-XX0(I,3)
           DY = YY0(I,1)-YY0(I,3)
           DZ = ZZ0(I,1)-ZZ0(I,3)
           DS2(I,3) =  DX*DX+DY*DY+DZ*DZ
         ENDDO
         DO I=1,NEL
           IF (IFC2(I,4)==0) CYCLE
           DX = XX0(I,1)-XX0(I,4)
           DY = YY0(I,1)-YY0(I,4)
           DZ = ZZ0(I,1)-ZZ0(I,4)
           DS2(I,4) =  DX*DX+DY*DY+DZ*DZ
         ENDDO
         DO I=1,NEL
           IF (IFC2(I,5)==0) CYCLE
           DX = XX0(I,4)-XX0(I,2)
           DY = YY0(I,4)-YY0(I,2)
           DZ = ZZ0(I,4)-ZZ0(I,2)
           DS2(I,5) =  DX*DX+DY*DY+DZ*DZ
         ENDDO
         DO I=1,NEL
           IF (IFC2(I,6)==0) CYCLE
           DX = XX0(I,3)-XX0(I,4)
           DY = YY0(I,3)-YY0(I,4)
           DZ = ZZ0(I,3)-ZZ0(I,4)
           DS2(I,6) =  DX*DX+DY*DY+DZ*DZ
         END DO
C---- 
         FN = ZERO
         F_T = THREE
         F_Q = 0.67
         LAM0 = TOL_E
         DO J=1,6
          JJ = J + 4
          DO I=1,NEL
            IF (IFC2(I,J)==0) CYCLE
            DX = XX(I,JJ) - XM(I,J)
            DY = YY(I,JJ) - YM(I,J)
            DZ = ZZ(I,JJ) - ZM(I,J)
            S2 = DX*DX+DY*DY+DZ*DZ
            IF (S2>TOL2*DS2(I,J)) THEN
              IFCTL = 1
              LAM = SQRT(S2/DS2(I,J))
              NORM=ONE/SQRT(S2)
              NJX(I,J) = DX*NORM 
              NJY(I,J) = DY*NORM
              NJZ(I,J) = DZ*NORM
              LENG  = SQRT(DS2(I,J))
              FAC = F_Q*(LAM+ONE)*(LAM+ONE)
              KTS = F_T*(FAC+ONE)*STI_C(I)
              FN(I,J) = KTS*(LAM-LAM0)*LENG
              FACT = F_T*(THREE*FAC+ONE)
              STIF(I) = MAX(STIF(I),FACT*STI_C(I)) ! will be added to STI not max()
            END IF 
          ENDDO
         ENDDO
C  ---- 5 (1,2) 
         J = 1
         DO I=1,NEL
           IF (FN(I,J)==ZERO) CYCLE
           FNJ = HALF*FN(I,J)
           NX = NJX(I,J)
           NY = NJY(I,J)
           NZ = NJZ(I,J)
           FX = FNJ*NX
           FY = FNJ*NY
           FZ = FNJ*NZ
           FOR_T1(I,1) = FOR_T1(I,1) + FX
           FOR_T1(I,2) = FOR_T1(I,2) + FY
           FOR_T1(I,3) = FOR_T1(I,3) + FZ
           FOR_T2(I,1) = FOR_T2(I,1) + FX
           FOR_T2(I,2) = FOR_T2(I,2) + FY
           FOR_T2(I,3) = FOR_T2(I,3) + FZ
!           
           FOR_T5(I,1) = FOR_T5(I,1) - TWO*FX
           FOR_T5(I,2) = FOR_T5(I,2) - TWO*FY
           FOR_T5(I,3) = FOR_T5(I,3) - TWO*FZ
           IFCE(I)=J
         ENDDO
C---- 6(2,3)
         J = 2
         DO I=1,NEL
           IF (FN(I,J)==ZERO) CYCLE
           FNJ = HALF*FN(I,J)
           NX = NJX(I,J)
           NY = NJY(I,J)
           NZ = NJZ(I,J)
           FX = FNJ*NX
           FY = FNJ*NY
           FZ = FNJ*NZ
           FOR_T3(I,1) = FOR_T3(I,1) + FX
           FOR_T3(I,2) = FOR_T3(I,2) + FY
           FOR_T3(I,3) = FOR_T3(I,3) + FZ
           FOR_T2(I,1) = FOR_T2(I,1) + FX
           FOR_T2(I,2) = FOR_T2(I,2) + FY
           FOR_T2(I,3) = FOR_T2(I,3) + FZ
!            
           FOR_T6(I,1) = FOR_T6(I,1) - TWO*FX
           FOR_T6(I,2) = FOR_T6(I,2) - TWO*FY
           FOR_T6(I,3) = FOR_T6(I,3) - TWO*FZ
           IFCE(I)=J
         ENDDO
C---- 7(1,3)
         J = 3
         DO I=1,NEL
           IF (FN(I,J)==ZERO) CYCLE
           FNJ = HALF*FN(I,J)
           NX = NJX(I,J)
           NY = NJY(I,J)
           NZ = NJZ(I,J)
           FX = FNJ*NX
           FY = FNJ*NY
           FZ = FNJ*NZ
           FOR_T3(I,1) = FOR_T3(I,1) + FX
           FOR_T3(I,2) = FOR_T3(I,2) + FY
           FOR_T3(I,3) = FOR_T3(I,3) + FZ
           FOR_T1(I,1) = FOR_T1(I,1) + FX
           FOR_T1(I,2) = FOR_T1(I,2) + FY
           FOR_T1(I,3) = FOR_T1(I,3) + FZ
           FOR_T7(I,1) = FOR_T7(I,1) - TWO*FX
           FOR_T7(I,2) = FOR_T7(I,2) - TWO*FY
           FOR_T7(I,3) = FOR_T7(I,3) - TWO*FZ
           IFCE(I)=J
         ENDDO
C---- 8(1,4)
         J = 4
         DO I=1,NEL
           IF (FN(I,J)==ZERO) CYCLE
           FNJ = HALF*FN(I,J)
           NX = NJX(I,J)
           NY = NJY(I,J)
           NZ = NJZ(I,J)
           FX = FNJ*NX
           FY = FNJ*NY
           FZ = FNJ*NZ
           FOR_T1(I,1) = FOR_T1(I,1) + FX
           FOR_T1(I,2) = FOR_T1(I,2) + FY
           FOR_T1(I,3) = FOR_T1(I,3) + FZ
           FOR_T4(I,1) = FOR_T4(I,1) + FX
           FOR_T4(I,2) = FOR_T4(I,2) + FY
           FOR_T4(I,3) = FOR_T4(I,3) + FZ
           FOR_T8(I,1) = FOR_T8(I,1) - TWO*FX
           FOR_T8(I,2) = FOR_T8(I,2) - TWO*FY
           FOR_T8(I,3) = FOR_T8(I,3) - TWO*FZ
           IFCE(I)=J
         ENDDO
C---- 9(2,4)
         J = 5
         DO I=1,NEL
           IF (FN(I,J)==ZERO) CYCLE
           FNJ = HALF*FN(I,J)
           NX = NJX(I,J)
           NY = NJY(I,J)
           NZ = NJZ(I,J)
           FX = FNJ*NX
           FY = FNJ*NY
           FZ = FNJ*NZ
           FOR_T4(I,1) = FOR_T4(I,1) + FX
           FOR_T4(I,2) = FOR_T4(I,2) + FY
           FOR_T4(I,3) = FOR_T4(I,3) + FZ
           FOR_T2(I,1) = FOR_T2(I,1) + FX
           FOR_T2(I,2) = FOR_T2(I,2) + FY
           FOR_T2(I,3) = FOR_T2(I,3) + FZ
           FOR_T9(I,1) = FOR_T9(I,1) - TWO*FX
           FOR_T9(I,2) = FOR_T9(I,2) - TWO*FY
           FOR_T9(I,3) = FOR_T9(I,3) - TWO*FZ
           IFCE(I)=J
         ENDDO
C----10(3,4)
         J = 6
         DO I=1,NEL
           IF (FN(I,J)==ZERO) CYCLE
           FNJ = HALF*FN(I,J)
           NX = NJX(I,J)
           NY = NJY(I,J)
           NZ = NJZ(I,J)
           FX = FNJ*NX
           FY = FNJ*NY
           FZ = FNJ*NZ
           FOR_T4(I,1) = FOR_T4(I,1) + FX
           FOR_T4(I,2) = FOR_T4(I,2) + FY
           FOR_T4(I,3) = FOR_T4(I,3) + FZ
           FOR_T3(I,1) = FOR_T3(I,1) + FX
           FOR_T3(I,2) = FOR_T3(I,2) + FY
           FOR_T3(I,3) = FOR_T3(I,3) + FZ
           FOR_T10(I,1) = FOR_T10(I,1) - TWO*FX
           FOR_T10(I,2) = FOR_T10(I,2) - TWO*FY
           FOR_T10(I,3) = FOR_T10(I,3) - TWO*FZ
           IFCE(I)=J
         ENDDO
         DO I=1,NEL
           IF (STI_C(I)==ZERO) CYCLE
           STI(I) = MAX(STI(I),STIF(I))
         ENDDO
        END IF !(IFF >0) THEN
       

      RETURN
      END
