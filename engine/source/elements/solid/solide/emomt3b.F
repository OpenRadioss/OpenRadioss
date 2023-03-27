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
Chd|  EMOMT3B                       source/elements/solid/solide/emomt3b.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE EMOMT3B(PM,RHO,VOLGP,
     .   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .   F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     .   PX1,PX2,PX3,PX4,
     .   PY1,PY2,PY3,PY4,
     .   PZ1,PZ2,PZ3,PZ4,
     .   PX5,PX6,PX7,PX8,
     .   PY5,PY6,PY7,PY8,
     .   PZ5,PZ6,PZ7,PZ8,
     .   DXX,DXY,DXZ,DYX,DYY,DYZ,DZX,DZY,DZZ,
     .   VDX,VDY,VDZ,
     .   MAT,  QMV,BUFMAT,
     .   VX1,VX2,VX3,VX4,VX5,VX6,VX7,VX8,
     .   VY1,VY2,VY3,VY4,VY5,VY6,VY7,VY8,
     .   VZ1,VZ2,VZ3,VZ4,VZ5,VZ6,VZ7,VZ8,
     .   IPARG1)
C-----------------------------------------------
C   M o d u l e  s
C-----------------------------------------------
      USE ALE_MOD     
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
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   PM(NPROPM,NUMMAT), RHO(*),VOLGP(LVEUL,*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   F17(*),F27(*),F37(*),F18(*),F28(*),F38(*),
     .   PX1(*),PX2(*),PX3(*),PX4(*),
     .   PY1(*),PY2(*),PY3(*),PY4(*),
     .   PZ1(*),PZ2(*),PZ3(*),PZ4(*),    
     .   PX5(*),PX6(*),PX7(*),PX8(*),
     .   PY5(*),PY6(*),PY7(*),PY8(*),
     .   PZ5(*),PZ6(*),PZ7(*),PZ8(*),    
     .   DXX(*),DXY(*),DXZ(*),DYX(*),DYY(*),DYZ(*),DZX(*),DZY(*),DZZ(*),
     .   VDX(*),VDY(*),VDZ(*)
      my_real
     .   QMV(12,*),BUFMAT(*),
     .   VX1(*),VX2(*),VX3(*),VX4(*),VX5(*),VX6(*),VX7(*),VX8(*),
     .   VY1(*),VY2(*),VY3(*),VY4(*),VY5(*),VY6(*),VY7(*),VY8(*),
     .   VZ1(*),VZ2(*),VZ3(*),VZ4(*),VZ5(*),VZ6(*),VZ7(*),VZ8(*)
      INTEGER IFL, MX
      INTEGER MAT(*),IPARG1(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   F1(MVSIZ),F2(MVSIZ),F3(MVSIZ),GAM(MVSIZ),
     .   A1(MVSIZ),A2(MVSIZ),A3(MVSIZ),A4(MVSIZ),
     .   A5(MVSIZ),A6(MVSIZ),A7(MVSIZ),A8(MVSIZ),FAC
      my_real AAA,DM,DMX,DMY,DMZ
      INTEGER IFLG,IADBUF
      INTEGER I
       
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------       
      IF(ALE%UPWIND%UPWM==0.AND.IPARG1(64)==1)RETURN   !silent boundary
             
C-----------------------------------------------
C     Forces de transports calcul es par volume fini
C-----------------------------------------------
C   DMi = 0.5*DT1*QMV(i,I)  masse entrant par la face i
      IF(MTN==51 .AND. ALE%UPWIND%UPWM/=3 .AND. IPARG1(64)==0)THEN
        IADBUF = NINT(PM(27,MAT(1)))
        IFLG = NINT(BUFMAT(31+IADBUF-1))
C
        IF(IFLG>1)RETURN
C
        IF(ALE%UPWIND%UPWM==0.)THEN
         MX = MAT(LFT)
         DO I=LFT,LLT
          GAM(I)= PM(15,MX)
         ENDDO
        ELSE
         DO I=LFT,LLT
          GAM(I)= ALE%UPWIND%CUPWM
         ENDDO
        ENDIF
C
        DO I=LFT,LLT
          AAA = QMV(1,I)+QMV(2,I)+QMV(3,I)+QMV(4,I)+QMV(5,I)+QMV(6,I)
          AAA = ONE_OVER_6 * AAA

          QMV(1,I) = ONE_OVER_8 * (QMV(1,I) - AAA)   
          QMV(2,I) = ONE_OVER_8 * (QMV(2,I) - AAA)   
          QMV(3,I) = ONE_OVER_8 * (QMV(3,I) - AAA)   
          QMV(4,I) = ONE_OVER_8 * (QMV(4,I) - AAA)   
          QMV(5,I) = ONE_OVER_8 * (QMV(5,I) - AAA)   
          QMV(6,I) = ONE_OVER_8 * (QMV(6,I) - AAA)   
          DMX = ZERO
          DMY = ZERO
          DMZ = ZERO
          DM = QMV(1,I)+QMV(6,I)+QMV(4,I)
          DMX = DMX + VX1(I)*DM
          DMY = DMY + VY1(I)*DM
          DMZ = DMZ + VZ1(I)*DM
          DM = QMV(1,I)+QMV(4,I)+QMV(5,I)
          DMX = DMX + VX2(I)*DM
          DMY = DMY + VY2(I)*DM
          DMZ = DMZ + VZ2(I)*DM
          DM = QMV(1,I)+QMV(5,I)+QMV(2,I)
          DMX = DMX + VX3(I)*DM
          DMY = DMY + VY3(I)*DM
          DMZ = DMZ + VZ3(I)*DM
          DM = QMV(1,I)+QMV(2,I)+QMV(6,I)
          DMX = DMX + VX4(I)*DM
          DMY = DMY + VY4(I)*DM
          DMZ = DMZ + VZ4(I)*DM
          DM = QMV(3,I)+QMV(6,I)+QMV(4,I)
          DMX = DMX + VX5(I)*DM
          DMY = DMY + VY5(I)*DM
          DMZ = DMZ + VZ5(I)*DM
          DM = QMV(3,I)+QMV(4,I)+QMV(5,I)
          DMX = DMX + VX6(I)*DM
          DMY = DMY + VY6(I)*DM
          DMZ = DMZ + VZ6(I)*DM
          DM = QMV(3,I)+QMV(5,I)+QMV(2,I)
          DMX = DMX + VX7(I)*DM
          DMY = DMY + VY7(I)*DM
          DMZ = DMZ + VZ7(I)*DM
          DM = QMV(3,I)+QMV(2,I)+QMV(6,I)
          DMX = DMX + VX8(I)*DM
          DMY = DMY + VY8(I)*DM
          DMZ = DMZ + VZ8(I)*DM
C
          DMX = -0.125 * DMX
          DMY = -0.125 * DMY
          DMZ = -0.125 * DMZ
C
          A1(I) = PX1(I)*VDX(I)+PY1(I)*VDY(I)+PZ1(I)*VDZ(I)
          A2(I) = PX2(I)*VDX(I)+PY2(I)*VDY(I)+PZ2(I)*VDZ(I)
          A3(I) = PX3(I)*VDX(I)+PY3(I)*VDY(I)+PZ3(I)*VDZ(I)
          A4(I) = PX4(I)*VDX(I)+PY4(I)*VDY(I)+PZ4(I)*VDZ(I)
          A1(I) = SIGN(GAM(I),A1(I))
          A2(I) = SIGN(GAM(I),A2(I))
          A3(I) = SIGN(GAM(I),A3(I))
          A4(I) = SIGN(GAM(I),A4(I))
C
          F11(I) = F11(I) - (ONE+A1(I))*DMX
          F12(I) = F12(I) - (ONE+A2(I))*DMX
          F13(I) = F13(I) - (ONE+A3(I))*DMX
          F14(I) = F14(I) - (ONE+A4(I))*DMX
          F15(I) = F15(I) - (ONE-A3(I))*DMX
          F16(I) = F16(I) - (ONE-A4(I))*DMX
          F17(I) = F17(I) - (ONE-A1(I))*DMX
          F18(I) = F18(I) - (ONE-A2(I))*DMX
C
          F21(I) = F21(I) - (ONE+A1(I))*DMY
          F22(I) = F22(I) - (ONE+A2(I))*DMY
          F23(I) = F23(I) - (ONE+A3(I))*DMY
          F24(I) = F24(I) - (ONE+A4(I))*DMY
          F25(I) = F25(I) - (ONE-A3(I))*DMY
          F26(I) = F26(I) - (ONE-A4(I))*DMY
          F27(I) = F27(I) - (ONE-A1(I))*DMY
          F28(I) = F28(I) - (ONE-A2(I))*DMY
C
          F31(I) = F31(I) - (ONE+A1(I))*DMZ
          F32(I) = F32(I) - (ONE+A2(I))*DMZ
          F33(I) = F33(I) - (ONE+A3(I))*DMZ
          F34(I) = F34(I) - (ONE+A4(I))*DMZ
          F35(I) = F35(I) - (ONE-A3(I))*DMZ
          F36(I) = F36(I) - (ONE-A4(I))*DMZ
          F37(I) = F37(I) - (ONE-A1(I))*DMZ
          F38(I) = F38(I) - (ONE-A2(I))*DMZ
        ENDDO
        RETURN
      ENDIF
C
C 'REYNOLDS THEOREM' : TERM CALCULATED AT CENTROID
C  STANDARD UPWIND WITH MATERIAL PARAMETERS
C
       MX = MAT(LFT) 
       DO I=LFT,LLT
        GAM(I)= PM(15,MX)
       ENDDO
       DO I=LFT,LLT
         A1(I) = PX1(I)*VDX(I)+PY1(I)*VDY(I)+PZ1(I)*VDZ(I)
         A2(I) = PX2(I)*VDX(I)+PY2(I)*VDY(I)+PZ2(I)*VDZ(I)
         A3(I) = PX3(I)*VDX(I)+PY3(I)*VDY(I)+PZ3(I)*VDZ(I)
         A4(I) = PX4(I)*VDX(I)+PY4(I)*VDY(I)+PZ4(I)*VDZ(I)
         A5(I) = PX5(I)*VDX(I)+PY5(I)*VDY(I)+PZ5(I)*VDZ(I)
         A6(I) = PX6(I)*VDX(I)+PY6(I)*VDY(I)+PZ6(I)*VDZ(I)
         A7(I) = PX7(I)*VDX(I)+PY7(I)*VDY(I)+PZ7(I)*VDZ(I)
         A8(I) = PX8(I)*VDX(I)+PY8(I)*VDY(I)+PZ8(I)*VDZ(I)
         A1(I) = SIGN(GAM(I),A1(I))
         A2(I) = SIGN(GAM(I),A2(I))
         A3(I) = SIGN(GAM(I),A3(I))
         A4(I) = SIGN(GAM(I),A4(I))
         A5(I) = SIGN(GAM(I),A5(I))
         A6(I) = SIGN(GAM(I),A6(I))
         A7(I) = SIGN(GAM(I),A7(I))
         A8(I) = SIGN(GAM(I),A8(I))
       ENDDO
       DO I=LFT,LLT
        FAC = RHO(I)
        F1(I) = (VDX(I)*DXX(I)+VDY(I)*DXY(I)+VDZ(I)*DXZ(I))*FAC
        F2(I) = (VDX(I)*DYX(I)+VDY(I)*DYY(I)+VDZ(I)*DYZ(I))*FAC
        F3(I) = (VDX(I)*DZX(I)+VDY(I)*DZY(I)+VDZ(I)*DZZ(I))*FAC
       ENDDO
       DO I=LFT,LLT
        F11(I) = F11(I) - (ONE+A1(I))*F1(I)*VOLGP(1,I)
        F12(I) = F12(I) - (ONE+A2(I))*F1(I)*VOLGP(2,I)
        F13(I) = F13(I) - (ONE+A3(I))*F1(I)*VOLGP(3,I)
        F14(I) = F14(I) - (ONE+A4(I))*F1(I)*VOLGP(4,I)
        F15(I) = F15(I) - (ONE+A5(I))*F1(I)*VOLGP(5,I)
        F16(I) = F16(I) - (ONE+A6(I))*F1(I)*VOLGP(6,I)
        F17(I) = F17(I) - (ONE+A7(I))*F1(I)*VOLGP(7,I)
        F18(I) = F18(I) - (ONE+A8(I))*F1(I)*VOLGP(8,I)
C
        F21(I) = F21(I) - (ONE+A1(I))*F2(I)*VOLGP(1,I)
        F22(I) = F22(I) - (ONE+A2(I))*F2(I)*VOLGP(2,I)
        F23(I) = F23(I) - (ONE+A3(I))*F2(I)*VOLGP(3,I)
        F24(I) = F24(I) - (ONE+A4(I))*F2(I)*VOLGP(4,I)
        F25(I) = F25(I) - (ONE+A5(I))*F2(I)*VOLGP(5,I)
        F26(I) = F26(I) - (ONE+A6(I))*F2(I)*VOLGP(6,I)
        F27(I) = F27(I) - (ONE+A7(I))*F2(I)*VOLGP(7,I)
        F28(I) = F28(I) - (ONE+A8(I))*F2(I)*VOLGP(8,I)
C
        F31(I) = F31(I) - (ONE+A1(I))*F3(I)*VOLGP(1,I)
        F32(I) = F32(I) - (ONE+A2(I))*F3(I)*VOLGP(2,I)
        F33(I) = F33(I) - (ONE+A3(I))*F3(I)*VOLGP(3,I)
        F34(I) = F34(I) - (ONE+A4(I))*F3(I)*VOLGP(4,I)
        F35(I) = F35(I) - (ONE+A5(I))*F3(I)*VOLGP(5,I)
        F36(I) = F36(I) - (ONE+A6(I))*F3(I)*VOLGP(6,I)
        F37(I) = F37(I) - (ONE+A7(I))*F3(I)*VOLGP(7,I)
        F38(I) = F38(I) - (ONE+A8(I))*F3(I)*VOLGP(8,I)	
       ENDDO
C
      RETURN
      END
C
