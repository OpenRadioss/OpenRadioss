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
!||    upwind4    ../engine/source/elements/solid/solide4/upwind4.F
!||--- called by ------------------------------------------------------
!||    a4momt3    ../engine/source/elements/solid/solide4/a4momt3.F
!||    a4momtn3   ../engine/source/elements/solid/solide4/a4momtn3.F
!||--- uses       -----------------------------------------------------
!||    ale_mod    ../common_source/modules/ale/ale_mod.F
!||====================================================================
      SUBROUTINE UPWIND4(
     1   RHO,     VIS,     VDX,     VDY,
     2   VDZ,     R,       S,       T,
     3   GAM,     NEL)
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
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      my_real
     .   R(MVSIZ,3),S(MVSIZ,3),T(MVSIZ,3),
     .   VDX(*),VDY(*),VDZ(*),RHO(*),VIS(*),GAM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .  FAC,PE,D1,D2,D3,DIST,CH1,CH2,
     .  DV(MVSIZ),V(MVSIZ)
      INTEGER I
C-----------------------------------------------
C   S o u r c e   C o d e
C-----------------------------------------------

      !-------------!
      !   /UPWM/TG  !
      !-------------!
      IF(ALE%UPWIND%UPWM==2)THEN
        FAC=ALE%UPWIND%CUPWM*HALF*DT1
        DO I=1,NEL
          GAM(I)=FAC
        END DO
        
      !-------------!
      !  /UPWM/SUPG !
      !-------------!
      ELSEIF(ALE%UPWIND%UPWM==3)THEN
        DO  I=1,NEL
          V(I)=VDX(I)**2+VDY(I)**2+VDZ(I)**2
          D1=ABS(R(I,1)*VDX(I)+R(I,2)*VDY(I)+R(I,3)*VDZ(I)) 
          D2=ABS(S(I,1)*VDX(I)+S(I,2)*VDY(I)+S(I,3)*VDZ(I)) 
          D3=ABS(T(I,1)*VDX(I)+T(I,2)*VDY(I)+T(I,3)*VDZ(I)) 
          GAM(I)=MAX(D1,D2,D3) !dominant direction
        ENDDO 

        CH2=ALE%UPWIND%CUPWM*HALF
        CH1=CH2*THIRD
       
       !SU/PG weighting function for current node.
       !SUPG is a Finite Element Method !
        DO  I=1,NEL
          FAC=HALF*RHO(I)/MAX(EM20,VIS(I))
          PE=FAC*GAM(I)
          IF(PE<=EM3)THEN
            GAM(I)=ZERO
          ELSEIF(PE<THREE)THEN
            GAM(I)=CH1*FAC*GAM(I)**2/V(I)
          ELSE
            GAM(I)=CH2*GAM(I)/V(I)
          ENDIF
        ENDDO
      ENDIF

      RETURN
      END
