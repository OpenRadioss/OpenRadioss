Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    amomt3    ../engine/source/elements/solid/solide/amomt3.F
!||--- called by ------------------------------------------------------
!||    sforc3    ../engine/source/elements/solid/solide/sforc3.F
!||    szforc3   ../engine/source/elements/solid/solidez/szforc3.F
!||--- calls      -----------------------------------------------------
!||    ajac3_v   ../engine/source/elements/solid/solide/ajac3.F
!||    amomtn3   ../engine/source/elements/solid/solide/amomtn3.F
!||--- uses       -----------------------------------------------------
!||    ale_mod   ../common_source/modules/ale/ale_mod.F
!||====================================================================
      SUBROUTINE AMOMT3(
     1   PM,      RHO,     VOL,     X1,
     2   X2,      X3,      X4,      X5,
     3   X6,      X7,      X8,      Y1,
     4   Y2,      Y3,      Y4,      Y5,
     5   Y6,      Y7,      Y8,      Z1,
     6   Z2,      Z3,      Z4,      Z5,
     7   Z6,      Z7,      Z8,      VX1,
     8   VX2,     VX3,     VX4,     VX5,
     9   VX6,     VX7,     VX8,     VY1,
     A   VY2,     VY3,     VY4,     VY5,
     B   VY6,     VY7,     VY8,     VZ1,
     C   VZ2,     VZ3,     VZ4,     VZ5,
     D   VZ6,     VZ7,     VZ8,     F11,
     E   F21,     F31,     F12,     F22,
     F   F32,     F13,     F23,     F33,
     G   F14,     F24,     F34,     F15,
     H   F25,     F35,     F16,     F26,
     I   F36,     F17,     F27,     F37,
     J   F18,     F28,     F38,     PX1,
     K   PX2,     PX3,     PX4,     PY1,
     L   PY2,     PY3,     PY4,     PZ1,
     M   PZ2,     PZ3,     PZ4,     DXX,
     N   DXY,     DXZ,     DYX,     DYY,
     O   DYZ,     DZX,     DZY,     DZZ,
     P   VDX1,    VDX2,    VDX3,    VDX4,
     Q   VDX5,    VDX6,    VDX7,    VDX8,
     R   VDY1,    VDY2,    VDY3,    VDY4,
     S   VDY5,    VDY6,    VDY7,    VDY8,
     T   VDZ1,    VDZ2,    VDZ3,    VDZ4,
     U   VDZ5,    VDZ6,    VDZ7,    VDZ8,
     V   VDX,     VDY,     VDZ,     DELTAX,
     W   VIS,     MAT,     QMV,     BUFMAT,
     X   IPARG1,  IXS,     IAD22,   NC1,
     Y   NC2,     NC3,     NC4,     NC5,
     Z   NC6,     NC7,     NC8,     NALE,
     1   NEL,     NFT,     MTN,     IPM)
C-----------------------------------------------
C   M o d u l e s
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
#include      "param_c.inc"
#include      "inter22.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NFT
      INTEGER, INTENT(IN) :: MTN
      INTEGER, INTENT(IN) :: NEL
      my_real
     .   PM(NPROPM,NUMMAT), RHO(*),VOL(*),
     .   X1(*),X2(*),X3(*),X4(*),X5(*),X6(*),X7(*),X8(*),
     .   Y1(*),Y2(*),Y3(*),Y4(*),Y5(*),Y6(*),Y7(*),Y8(*),
     .   Z1(*),Z2(*),Z3(*),Z4(*),Z5(*),Z6(*),Z7(*),Z8(*),
     .   VX1(*),VX2(*),VX3(*),VX4(*),VX5(*),VX6(*),VX7(*),VX8(*),
     .   VY1(*),VY2(*),VY3(*),VY4(*),VY5(*),VY6(*),VY7(*),VY8(*),
     .   VZ1(*),VZ2(*),VZ3(*),VZ4(*),VZ5(*),VZ6(*),VZ7(*),VZ8(*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   F17(*),F27(*),F37(*),F18(*),F28(*),F38(*),
     .   PX1(*),PX2(*),PX3(*),PX4(*),
     .   PY1(*),PY2(*),PY3(*),PY4(*),
     .   PZ1(*),PZ2(*),PZ3(*),PZ4(*),    
     .   DXX(*),DXY(*),DXZ(*),
     .   DYX(*),DYY(*),DYZ(*),
     .   DZX(*),DZY(*),DZZ(*),
     .   VDX1(*),VDX2(*),VDX3(*),VDX4(*),
     .   VDX5(*),VDX6(*),VDX7(*),VDX8(*),
     .   VDY1(*),VDY2(*),VDY3(*),VDY4(*),
     .   VDY5(*),VDY6(*),VDY7(*),VDY8(*),
     .   VDZ1(*),VDZ2(*),VDZ3(*),VDZ4(*),
     .   VDZ5(*),VDZ6(*),VDZ7(*),VDZ8(*),
     .   VDX(*),VDY(*),VDZ(*),
     .   DELTAX(*),VIS(*),QMV(12,*),BUFMAT(*),IAD22(*)
      INTEGER MAT(*),IPARG1(*),IXS(NIXS,*), 
     .   NC1(*), NC2(*), NC3(*), NC4(*), NC5(*), NC6(*), NC7(*), NC8(*), 
     .   NALE(*)
      INTEGER, INTENT(IN) :: IPM(NPROPMI,NUMMAT)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   V12(MVSIZ,3),V23(MVSIZ,3),V43(MVSIZ,3),V14(MVSIZ,3),
     .   V56(MVSIZ,3),V67(MVSIZ,3),V87(MVSIZ,3),V58(MVSIZ,3),
     .   V15(MVSIZ,3),V26(MVSIZ,3),V37(MVSIZ,3),V48(MVSIZ,3),
     .   F1(MVSIZ),F2(MVSIZ),F3(MVSIZ),
     .   A1(MVSIZ),A2(MVSIZ),A3(MVSIZ),A4(MVSIZ),GAM(MVSIZ),
     .   R(MVSIZ,3),S(MVSIZ,3),T(MVSIZ,3),FAC,AAA,DM,DMX,DMY,DMZ,
     .   AJV(MVSIZ),AJ0(MVSIZ,3),AJ1(MVSIZ,3),AJ2(MVSIZ,3),AJ3(MVSIZ,3),
     .   V43a(MVSIZ,3), V43b(MVSIZ,3),
     .   V87a(MVSIZ,3), V87b(MVSIZ,3)
      INTEGER I,J,IADBUF,IFLG, MX
C-----------------------------------------------
      IF(ALE%UPWIND%UPWM == 0. AND. IPARG1(64) == 1)RETURN   !silent boundary
C-----------------------------------------------
C  MULTIMATERIAL LAW 51
C-----------------------------------------------
      IF(MTN == 51 .AND. ALE%UPWIND%UPWM /= 3 .AND. IPARG1(64) == 0)THEN
      !-------------------------------------------------------!
      !     Reynolds theorem ('transportation force')         !
      !   DMi = 0.5*DT1*QMV(i,I)  incoming mass from face i   !
      !-------------------------------------------------------!
      IADBUF = IPM(27,MAT(1))
      IFLG = NINT(BUFMAT(31+IADBUF-1))
 
       IF(IFLG>1)RETURN

       !momentum advection with STD upwind & TG
       MX = MAT(1)       
       DO I=1,NEL
        GAM(I)= PM(15,MX)
       ENDDO

        DO I=1,NEL
          IF(INT22>0)THEN
            IF(NINT(IAD22(I))/=0)CYCLE
          ENDIF
          AAA = QMV(1,I)+QMV(2,I)+QMV(3,I)+QMV(4,I)+QMV(5,I)+QMV(6,I)
          AAA = AAA/SIX
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


          !DM.x = -1/8 SUM(DM_i*Vi.x, i=1..8)  where DM_i = SUM(QMV(f), f face such as Ni in face)
          !DM.y = -1/8 SUM(DM_i*Vi.y, i=1..8)  where DM_i = SUM(QMV(f), f face such as Ni in face)
          !DM.z = -1/8 SUM(DM_i*Vi.z, i=1..8)  where DM_i = SUM(QMV(f), f face such as Ni in face)
          DMX = -ONE_OVER_8 * DMX
          DMY = -ONE_OVER_8 * DMY
          DMZ = -ONE_OVER_8 * DMZ

          A1(I) = PX1(I)*VDX(I)+PY1(I)*VDY(I)+PZ1(I)*VDZ(I)
          A2(I) = PX2(I)*VDX(I)+PY2(I)*VDY(I)+PZ2(I)*VDZ(I)
          A3(I) = PX3(I)*VDX(I)+PY3(I)*VDY(I)+PZ3(I)*VDZ(I)
          A4(I) = PX4(I)*VDX(I)+PY4(I)*VDY(I)+PZ4(I)*VDZ(I)
          A1(I) = SIGN(GAM(1),A1(I))
          A2(I) = SIGN(GAM(1),A2(I))
          A3(I) = SIGN(GAM(1),A3(I))
          A4(I) = SIGN(GAM(1),A4(I))

          F11(I) = F11(I) - (ONE+A1(I))*DMX
          F12(I) = F12(I) - (ONE+A2(I))*DMX
          F13(I) = F13(I) - (ONE+A3(I))*DMX
          F14(I) = F14(I) - (ONE+A4(I))*DMX
          F15(I) = F15(I) - (ONE-A3(I))*DMX
          F16(I) = F16(I) - (ONE-A4(I))*DMX
          F17(I) = F17(I) - (ONE-A1(I))*DMX
          F18(I) = F18(I) - (ONE-A2(I))*DMX

          F21(I) = F21(I) - (ONE+A1(I))*DMY
          F22(I) = F22(I) - (ONE+A2(I))*DMY
          F23(I) = F23(I) - (ONE+A3(I))*DMY
          F24(I) = F24(I) - (ONE+A4(I))*DMY
          F25(I) = F25(I) - (ONE-A3(I))*DMY
          F26(I) = F26(I) - (ONE-A4(I))*DMY
          F27(I) = F27(I) - (ONE-A1(I))*DMY
          F28(I) = F28(I) - (ONE-A2(I))*DMY

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



C-----------------------------------------------
C  OTHER FLUID MATERIAL
C-----------------------------------------------


      !--------------------!
      !     SUPG & TG      !
      !    R,S,T frame     !
      !--------------------!
      IF(ALE%UPWIND%UPWM>1)THEN
       DO I=1,NEL
        V12(I,1)=X2(I)-X1(I)
        V12(I,2)=Y2(I)-Y1(I)
        V12(I,3)=Z2(I)-Z1(I)
        V23(I,1)=X3(I)-X2(I)
        V23(I,2)=Y3(I)-Y2(I)
        V23(I,3)=Z3(I)-Z2(I)
        V43(I,1)=X3(I)-X4(I)
        V43(I,2)=Y3(I)-Y4(I)
        V43(I,3)=Z3(I)-Z4(I)
        V14(I,1)=X4(I)-X1(I)
        V14(I,2)=Y4(I)-Y1(I)
        V14(I,3)=Z4(I)-Z1(I)
        V15(I,1)=X5(I)-X1(I)
        V15(I,2)=Y5(I)-Y1(I)
        V15(I,3)=Z5(I)-Z1(I)
        V26(I,1)=X6(I)-X2(I)
        V26(I,2)=Y6(I)-Y2(I)
        V26(I,3)=Z6(I)-Z2(I)
        V37(I,1)=X7(I)-X3(I)
        V37(I,2)=Y7(I)-Y3(I)
        V37(I,3)=Z7(I)-Z3(I)
        V48(I,1)=X8(I)-X4(I)
        V48(I,2)=Y8(I)-Y4(I)
        V48(I,3)=Z8(I)-Z4(I)
        V56(I,1)=X6(I)-X5(I)
        V56(I,2)=Y6(I)-Y5(I)
        V56(I,3)=Z6(I)-Z5(I)
        V67(I,1)=X7(I)-X6(I)
        V67(I,2)=Y7(I)-Y6(I)
        V67(I,3)=Z7(I)-Z6(I)
        V87(I,1)=X7(I)-X8(I)
        V87(I,2)=Y7(I)-Y8(I)
        V87(I,3)=Z7(I)-Z8(I)
        V58(I,1)=X8(I)-X5(I)
        V58(I,2)=Y8(I)-Y5(I)
        V58(I,3)=Z8(I)-Z5(I)
       ENDDO
       
       ! NODE 1 : V12,V14,V15
       ! NODE 2 : V12,V23,V26
       ! NODE 3 : V43,V23,V37
       ! NODE 4 : V43,V14,V48
       ! NODE 5 : V56,V58,V15
       ! NODE 6 : V56,V67,V26
       ! NODE 7 : V87,V67,V37
       ! NODE 8 : V87,V58,V48
              
       !PENTA6DG CASE
       !V43 is change to V43a for frame on node 3
       !V43 is change to V43b for frame on node 4 
       !V87 is change to V87a for frame on node 3
       !V87 is change to V87b for frame on node 4            
       DO I=1,NEL
         
         IF(IXS(5,I+NFT)==IXS(4,I+NFT))THEN
           V43a(I,1)=X3(I)-X1(I)
           V43a(I,2)=Y3(I)-Y1(I)
           V43a(I,3)=Z3(I)-Z1(I) 
           V43b(I,1)=X2(I)-X3(I)
           V43b(I,2)=Y2(I)-Y3(I)
           V43b(I,3)=Z2(I)-Z3(I)            
         ELSE
           V43a(I,1) = V43(I,1)  
           V43b(I,1) = V43(I,1)            
           V43a(I,2) = V43(I,2)  
           V43b(I,2) = V43(I,2)            
           V43a(I,3) = V43(I,3)  
           V43b(I,3) = V43(I,3)            
         ENDIF
         
         IF(IXS(9,I+NFT)==IXS(8,I+NFT))THEN
           V87a(I,1)=X7(I)-X5(I)
           V87a(I,2)=Y7(I)-Y5(I)
           V87a(I,3)=Z7(I)-Z5(I)
           V87b(I,1)=X6(I)-X7(I)
           V87b(I,2)=Y6(I)-Y7(I)
           V87b(I,3)=Z6(I)-Z7(I)           
          ELSE
           V87a(I,1) = V87(I,1)  
           V87b(I,1) = V87(I,1)
           V87a(I,2) = V87(I,2)  
           V87b(I,2) = V87(I,2)
           V87a(I,3) = V87(I,3)  
           V87b(I,3) = V87(I,3)
         ENDIF         
       ENDDO
       
       DO J=1,3
        DO I=1,NEL
         R(I,J)=FOURTH*(V12(I,J)+V56(I,J)+V43(I,J)+V87(I,J))
         S(I,J)=FOURTH*(V23(I,J)+V14(I,J)+V58(I,J)+V67(I,J))
         T(I,J)=FOURTH*(V15(I,J)+V26(I,J)+V37(I,J)+V48(I,J))   
        ENDDO
       ENDDO
      ENDIF

      !----------------------------------------------------!
      ! UPWIND STANDARD (upwm<2)                           !
      ! TRANSPORTATION FORCE AT CENTROID                   !
      !----------------------------------------------------!
      IF(ALE%UPWIND%UPWM<=1 .OR.IPARG1(64)==1)THEN
      
       IF((IPARG1(64)==1))THEN
       ! VDX,VDY,VDZ IS CALCULATED IN M11VS3 OU M51VS3
        DO I=1,NEL
         A1(I) =(PX1(I)*VDX(I)+PY1(I)*VDY(I)+PZ1(I)*VDZ(I))
         A2(I) =(PX2(I)*VDX(I)+PY2(I)*VDY(I)+PZ2(I)*VDZ(I))
         A3(I) =(PX3(I)*VDX(I)+PY3(I)*VDY(I)+PZ3(I)*VDZ(I))
         A4(I) =(PX4(I)*VDX(I)+PY4(I)*VDY(I)+PZ4(I)*VDZ(I))
         A1(I) = SIGN(ONE,A1(I))
         A2(I) = SIGN(ONE,A2(I))
         A3(I) = SIGN(ONE,A3(I))
         A4(I) = SIGN(ONE,A4(I))        
        ENDDO
       ELSE
        IF(ALE%UPWIND%UPWM==ZERO)THEN
         MX = MAT(1)
         DO I=1,NEL
          GAM(I)= PM(15,MX)
         ENDDO
        ELSE
         DO I=1,NEL
          GAM(I)= ALE%UPWIND%CUPWM
         ENDDO
        ENDIF
        DO I=1,NEL
         A1(I) = PX1(I)*VDX(I)+PY1(I)*VDY(I)+PZ1(I)*VDZ(I)
         A2(I) = PX2(I)*VDX(I)+PY2(I)*VDY(I)+PZ2(I)*VDZ(I)
         A3(I) = PX3(I)*VDX(I)+PY3(I)*VDY(I)+PZ3(I)*VDZ(I)
         A4(I) = PX4(I)*VDX(I)+PY4(I)*VDY(I)+PZ4(I)*VDZ(I)
         A1(I) = SIGN(GAM(1),A1(I))
         A2(I) = SIGN(GAM(1),A2(I))
         A3(I) = SIGN(GAM(1),A3(I))
         A4(I) = SIGN(GAM(1),A4(I))
        ENDDO
       ENDIF
       
       DO I=1,NEL
        FAC = ONE_OVER_8*RHO(I)*VOL(I)
        F1(I) = (VDX(I)*DXX(I)+VDY(I)*DXY(I)+VDZ(I)*DXZ(I))*FAC
        F2(I) = (VDX(I)*DYX(I)+VDY(I)*DYY(I)+VDZ(I)*DYZ(I))*FAC
        F3(I) = (VDX(I)*DZX(I)+VDY(I)*DZY(I)+VDZ(I)*DZZ(I))*FAC
       ENDDO

       !inter22
       IF(INT22>0)THEN       
         DO I=1,NEL
          !currently no momentum transport force for polyedra
           IF(NINT(IAD22(I))==0)CYCLE
          !no force for brick in cut cell buffer
           F1(I) = ZERO
           F2(I) = ZERO
           F3(I) = ZERO                      
          ENDDO
       ENDIF
        
       !expand to nodes
       DO I=1,NEL
        F11(I) = F11(I) - (ONE+A1(I))*F1(I)
        F12(I) = F12(I) - (ONE+A2(I))*F1(I)
        F13(I) = F13(I) - (ONE+A3(I))*F1(I)
        F14(I) = F14(I) - (ONE+A4(I))*F1(I)
        F15(I) = F15(I) - (ONE-A3(I))*F1(I)
        F16(I) = F16(I) - (ONE-A4(I))*F1(I)
        F17(I) = F17(I) - (ONE-A1(I))*F1(I)
        F18(I) = F18(I) - (ONE-A2(I))*F1(I)
C
        F21(I) = F21(I) - (ONE+A1(I))*F2(I)
        F22(I) = F22(I) - (ONE+A2(I))*F2(I)
        F23(I) = F23(I) - (ONE+A3(I))*F2(I)
        F24(I) = F24(I) - (ONE+A4(I))*F2(I)
        F25(I) = F25(I) - (ONE-A3(I))*F2(I)
        F26(I) = F26(I) - (ONE-A4(I))*F2(I)
        F27(I) = F27(I) - (ONE-A1(I))*F2(I)
        F28(I) = F28(I) - (ONE-A2(I))*F2(I)
C
        F31(I) = F31(I) - (ONE+A1(I))*F3(I)
        F32(I) = F32(I) - (ONE+A2(I))*F3(I)
        F33(I) = F33(I) - (ONE+A3(I))*F3(I)
        F34(I) = F34(I) - (ONE+A4(I))*F3(I)
        F35(I) = F35(I) - (ONE-A3(I))*F3(I)
        F36(I) = F36(I) - (ONE-A4(I))*F3(I)
        F37(I) = F37(I) - (ONE-A1(I))*F3(I)
        F38(I) = F38(I) - (ONE-A2(I))*F3(I)
       ENDDO

      ELSE

      !-----------------------------------------------------------------------!
      ! SUPG OU TG                                                            !
      ! TRANSPORTATION FORCE WHICH MINIMIZE HOURGLASS                         !
      !  <PHI,UJ*DUI/DXJ> ESTIMATED AT NODES                                  !
      !-----------------------------------------------------------------------!
      ! NODE 1 (1,2,4,5)     
       CALL AJAC3_V(
     1   V12,     V14,     V15,     AJ1,
     2   AJ2,     AJ3,     AJV,     NEL)
       DO I=1,NEL
        AJ0(I,1)=-(AJ1(I,1)+AJ2(I,1)+AJ3(I,1))
        AJ0(I,2)=-(AJ1(I,2)+AJ2(I,2)+AJ3(I,2))
        AJ0(I,3)=-(AJ1(I,3)+AJ2(I,3)+AJ3(I,3))
       ENDDO
       CALL AMOMTN3(
     1   RHO,     DELTAX,  VIS,     VOL,
     2   AJ0,     AJ1,     AJ2,     AJ3,
     3   AJV,     R,       S,       T,
     4   VDX1,    VDY1,    VDZ1,    VX1,
     5   VY1,     VZ1,     VX2,     VY2,
     6   VZ2,     VX4,     VY4,     VZ4,
     7   VX5,     VY5,     VZ5,     F11,
     8   F21,     F31,     F12,     F22,
     9   F32,     F14,     F24,     F34,
     A   F15,     F25,     F35,     F13,
     B   F23,     F33,     F16,     F26,
     C   F36,     F17,     F27,     F37,
     D   F18,     F28,     F38,     IAD22,
     E   NC1,     NC2,     NC3,     NC4,
     F   NC5,     NC6,     NC7,     NC8,
     G   NALE,    NEL)
       ! NODE 2 (2,1,3,6)     
       CALL AJAC3_V(
     1   V12,     V23,     V26,     AJ1,
     2   AJ2,     AJ3,     AJV,     NEL)
       DO I=1,NEL
        AJ1(I,1)=-AJ1(I,1)
        AJ1(I,2)=-AJ1(I,2)
        AJ1(I,3)=-AJ1(I,3)
        AJ0(I,1)=-(AJ1(I,1)+AJ2(I,1)+AJ3(I,1))
        AJ0(I,2)=-(AJ1(I,2)+AJ2(I,2)+AJ3(I,2))
        AJ0(I,3)=-(AJ1(I,3)+AJ2(I,3)+AJ3(I,3))
       ENDDO
       CALL AMOMTN3(
     1   RHO,     DELTAX,  VIS,     VOL,
     2   AJ0,     AJ1,     AJ2,     AJ3,
     3   AJV,     R,       S,       T,
     4   VDX2,    VDY2,    VDZ2,    VX2,
     5   VY2,     VZ2,     VX1,     VY1,
     6   VZ1,     VX3,     VY3,     VZ3,
     7   VX6,     VY6,     VZ6,     F12,
     8   F22,     F32,     F11,     F21,
     9   F31,     F13,     F23,     F33,
     A   F16,     F26,     F36,     F14,
     B   F24,     F34,     F15,     F25,
     C   F35,     F17,     F27,     F37,
     D   F18,     F28,     F38,     IAD22,
     E   NC1,     NC2,     NC3,     NC4,
     F   NC5,     NC6,     NC7,     NC8,
     G   NALE,    NEL)
       ! NODE 3  (3,4,2,8)    
       CALL AJAC3_V(
     1   V43a,    V23,     V37,     AJ1,
     2   AJ2,     AJ3,     AJV,     NEL)
       DO I=1,NEL
        AJ1(I,1)=-AJ1(I,1)
        AJ1(I,2)=-AJ1(I,2)
        AJ1(I,3)=-AJ1(I,3)
        AJ2(I,1)=-AJ2(I,1)
        AJ2(I,2)=-AJ2(I,2)
        AJ2(I,3)=-AJ2(I,3)
        AJ0(I,1)=-(AJ1(I,1)+AJ2(I,1)+AJ3(I,1))
        AJ0(I,2)=-(AJ1(I,2)+AJ2(I,2)+AJ3(I,2))
        AJ0(I,3)=-(AJ1(I,3)+AJ2(I,3)+AJ3(I,3))
       ENDDO
       CALL AMOMTN3(
     1   RHO,     DELTAX,  VIS,     VOL,
     2   AJ0,     AJ1,     AJ2,     AJ3,
     3   AJV,     R,       S,       T,
     4   VDX3,    VDY3,    VDZ3,    VX3,
     5   VY3,     VZ3,     VX4,     VY4,
     6   VZ4,     VX2,     VY2,     VZ2,
     7   VX7,     VY7,     VZ7,     F13,
     8   F23,     F33,     F14,     F24,
     9   F34,     F12,     F22,     F32,
     A   F17,     F27,     F37,     F11,
     B   F21,     F31,     F15,     F25,
     C   F35,     F16,     F26,     F36,
     D   F18,     F28,     F38,     IAD22,
     E   NC1,     NC2,     NC3,     NC4,
     F   NC5,     NC6,     NC7,     NC8,
     G   NALE,    NEL)
       ! NODE 4 (4,3,1,8)     
       CALL AJAC3_V(
     1   V43b,    V14,     V48,     AJ1,
     2   AJ2,     AJ3,     AJV,     NEL)
       DO I=1,NEL
        AJ2(I,1)=-AJ2(I,1)
        AJ2(I,2)=-AJ2(I,2)
        AJ2(I,3)=-AJ2(I,3)
        AJ0(I,1)=-(AJ1(I,1)+AJ2(I,1)+AJ3(I,1))
        AJ0(I,2)=-(AJ1(I,2)+AJ2(I,2)+AJ3(I,2))
        AJ0(I,3)=-(AJ1(I,3)+AJ2(I,3)+AJ3(I,3))
       ENDDO
       CALL AMOMTN3(
     1   RHO,     DELTAX,  VIS,     VOL,
     2   AJ0,     AJ1,     AJ2,     AJ3,
     3   AJV,     R,       S,       T,
     4   VDX4,    VDY4,    VDZ4,    VX4,
     5   VY4,     VZ4,     VX3,     VY3,
     6   VZ3,     VX1,     VY1,     VZ1,
     7   VX8,     VY8,     VZ8,     F14,
     8   F24,     F34,     F13,     F23,
     9   F33,     F11,     F21,     F31,
     A   F18,     F28,     F38,     F12,
     B   F22,     F32,     F15,     F25,
     C   F35,     F16,     F26,     F36,
     D   F17,     F27,     F37,     IAD22,
     E   NC1,     NC2,     NC3,     NC4,
     F   NC5,     NC6,     NC7,     NC8,
     G   NALE,    NEL)
       ! NODE 5  (5,6,8,1)    
       CALL AJAC3_V(
     1   V56,     V58,     V15,     AJ1,
     2   AJ2,     AJ3,     AJV,     NEL)
       DO I=1,NEL
        AJ3(I,1)=-AJ3(I,1)
        AJ3(I,2)=-AJ3(I,2)
        AJ3(I,3)=-AJ3(I,3)
        AJ0(I,1)=-(AJ1(I,1)+AJ2(I,1)+AJ3(I,1))
        AJ0(I,2)=-(AJ1(I,2)+AJ2(I,2)+AJ3(I,2))
        AJ0(I,3)=-(AJ1(I,3)+AJ2(I,3)+AJ3(I,3))
       ENDDO
       CALL AMOMTN3(
     1   RHO,     DELTAX,  VIS,     VOL,
     2   AJ0,     AJ1,     AJ2,     AJ3,
     3   AJV,     R,       S,       T,
     4   VDX5,    VDY5,    VDZ5,    VX5,
     5   VY5,     VZ5,     VX6,     VY6,
     6   VZ6,     VX8,     VY8,     VZ8,
     7   VX1,     VY1,     VZ1,     F15,
     8   F25,     F35,     F16,     F26,
     9   F36,     F18,     F28,     F38,
     A   F11,     F21,     F31,     F12,
     B   F22,     F32,     F13,     F23,
     C   F33,     F14,     F24,     F34,
     D   F17,     F27,     F37,     IAD22,
     E   NC1,     NC2,     NC3,     NC4,
     F   NC5,     NC6,     NC7,     NC8,
     G   NALE,    NEL)
       ! NODE 6  (6,5,7,2)    
       CALL AJAC3_V(
     1   V56,     V67,     V26,     AJ1,
     2   AJ2,     AJ3,     AJV,     NEL)
       DO I=1,NEL
        AJ3(I,1)=-AJ3(I,1)
        AJ3(I,2)=-AJ3(I,2)
        AJ3(I,3)=-AJ3(I,3)
        AJ1(I,1)=-AJ1(I,1)
        AJ1(I,2)=-AJ1(I,2)
        AJ1(I,3)=-AJ1(I,3)
        AJ0(I,1)=-(AJ1(I,1)+AJ2(I,1)+AJ3(I,1))
        AJ0(I,2)=-(AJ1(I,2)+AJ2(I,2)+AJ3(I,2))
        AJ0(I,3)=-(AJ1(I,3)+AJ2(I,3)+AJ3(I,3))
      ENDDO
       CALL AMOMTN3(
     1   RHO,     DELTAX,  VIS,     VOL,
     2   AJ0,     AJ1,     AJ2,     AJ3,
     3   AJV,     R,       S,       T,
     4   VDX6,    VDY6,    VDZ6,    VX6,
     5   VY6,     VZ6,     VX5,     VY5,
     6   VZ5,     VX7,     VY7,     VZ7,
     7   VX2,     VY2,     VZ2,     F16,
     8   F26,     F36,     F15,     F25,
     9   F35,     F17,     F27,     F37,
     A   F12,     F22,     F32,     F11,
     B   F21,     F31,     F13,     F23,
     C   F33,     F14,     F24,     F34,
     D   F18,     F28,     F38,     IAD22,
     E   NC1,     NC2,     NC3,     NC4,
     F   NC5,     NC6,     NC7,     NC8,
     G   NALE,    NEL)
       ! NODE 7 (7,8,6,3)     
       CALL AJAC3_V(
     1   V87a,    V67,     V37,     AJ1,
     2   AJ2,     AJ3,     AJV,     NEL)
       DO I=1,NEL
        AJ1(I,1)=-AJ1(I,1)
        AJ1(I,2)=-AJ1(I,2)
        AJ1(I,3)=-AJ1(I,3)
        AJ2(I,1)=-AJ2(I,1)
        AJ2(I,2)=-AJ2(I,2)
        AJ2(I,3)=-AJ2(I,3)
        AJ3(I,1)=-AJ3(I,1)
        AJ3(I,2)=-AJ3(I,2)
        AJ3(I,3)=-AJ3(I,3)
        AJ0(I,1)=-(AJ1(I,1)+AJ2(I,1)+AJ3(I,1))
        AJ0(I,2)=-(AJ1(I,2)+AJ2(I,2)+AJ3(I,2))
        AJ0(I,3)=-(AJ1(I,3)+AJ2(I,3)+AJ3(I,3))
       ENDDO
       CALL AMOMTN3(
     1   RHO,     DELTAX,  VIS,     VOL,
     2   AJ0,     AJ1,     AJ2,     AJ3,
     3   AJV,     R,       S,       T,
     4   VDX7,    VDY7,    VDZ7,    VX7,
     5   VY7,     VZ7,     VX8,     VY8,
     6   VZ8,     VX6,     VY6,     VZ6,
     7   VX3,     VY3,     VZ3,     F17,
     8   F27,     F37,     F18,     F28,
     9   F38,     F16,     F26,     F36,
     A   F13,     F23,     F33,     F11,
     B   F21,     F31,     F12,     F22,
     C   F32,     F14,     F24,     F34,
     D   F15,     F25,     F35,     IAD22,
     E   NC1,     NC2,     NC3,     NC4,
     F   NC5,     NC6,     NC7,     NC8,
     G   NALE,    NEL)
       ! NODE 8      
       CALL AJAC3_V(
     1   V87b,    V58,     V48,     AJ1,
     2   AJ2,     AJ3,     AJV,     NEL)
       DO I=1,NEL
        AJ2(I,1)=-AJ2(I,1)
        AJ2(I,2)=-AJ2(I,2)
        AJ2(I,3)=-AJ2(I,3)
        AJ3(I,1)=-AJ3(I,1)
        AJ3(I,2)=-AJ3(I,2)
        AJ3(I,3)=-AJ3(I,3)
        AJ0(I,1)=-(AJ1(I,1)+AJ2(I,1)+AJ3(I,1))
        AJ0(I,2)=-(AJ1(I,2)+AJ2(I,2)+AJ3(I,2))
        AJ0(I,3)=-(AJ1(I,3)+AJ2(I,3)+AJ3(I,3))
       ENDDO
       CALL AMOMTN3(
     1   RHO,     DELTAX,  VIS,     VOL,
     2   AJ0,     AJ1,     AJ2,     AJ3,
     3   AJV,     R,       S,       T,
     4   VDX8,    VDY8,    VDZ8,    VX8,
     5   VY8,     VZ8,     VX7,     VY7,
     6   VZ7,     VX5,     VY5,     VZ5,
     7   VX4,     VY4,     VZ4,     F18,
     8   F28,     F38,     F17,     F27,
     9   F37,     F15,     F25,     F35,
     A   F14,     F24,     F34,     F11,
     B   F21,     F31,     F12,     F22,
     C   F32,     F13,     F23,     F33,
     D   F16,     F26,     F36,     IAD22,
     E   NC1,     NC2,     NC3,     NC4,
     F   NC5,     NC6,     NC7,     NC8,
     G   NALE,    NEL)
      ENDIF
      RETURN
      END
C
