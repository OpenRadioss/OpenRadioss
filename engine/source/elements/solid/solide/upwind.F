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
Chd|  UPWIND                        source/elements/solid/solide/upwind.F
Chd|-- called by -----------
Chd|        AMOMT2                        source/ale/ale2d/amomt2.F     
Chd|        EMOMT2                        source/ale/euler2d/emomt2.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPWIND(RHO,VIS,VDX,VDY,VDZ,R,S,T,DELTAX,GAM)
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
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real R(3,*),S(3,*),T(3,*),VDX(*),VDY(*),VDZ(*),DELTAX(*),RHO(*),VIS(*),GAM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real FAC,PE,D1,D2,D3,DIST,CH1,CH2,DV(MVSIZ),V(MVSIZ)
      INTEGER I
C-----------------------------------------------

      !--------------------------------------------!
      ! /UPWM/TG                                   !
      !   Taylor Galerkine                         !
      !--------------------------------------------!
      IF(ALE%UPWIND%UPWM==2)THEN
        FAC=ALE%UPWIND%CUPWM*HALF*DT1
        DO I=LFT,LLT
         GAM(I)=FAC
        END DO

      !--------------------------------------------!
      ! /UPWM/SUPG                                 !
      !   Streaminline Upwind Petrov Galerkine     !
      !--------------------------------------------!
      ELSEIF(ALE%UPWIND%UPWM==3)THEN
        IF(N2D==0)THEN
          DO  I=LFT,LLT
            V(I)=VDX(I)**2+VDY(I)**2+VDZ(I)**2
            D1=R(1,I)*VDX(I)+R(2,I)*VDY(I)+R(3,I)*VDZ(I) 
            D2=S(1,I)*VDX(I)+S(2,I)*VDY(I)+S(3,I)*VDZ(I) 
            D3=T(1,I)*VDX(I)+T(2,I)*VDY(I)+T(3,I)*VDZ(I) 
            GAM(I)=SQRT(D1**2+D2**2+D3**2)
          ENDDO 
        ELSE
          DO  I=LFT,LLT
            D2=S(2,I)*VDY(I)+S(3,I)*VDZ(I) 
            D3=T(2,I)*VDY(I)+T(3,I)*VDZ(I) 
            GAM(I)=SQRT(D2**2+D3**2)
            V(I)=VDY(I)**2+VDZ(I)**2
          ENDDO 
        ENDIF               
        CH2=ALE%UPWIND%CUPWM*HALF
        CH1=CH2*THIRD
        DO  I=LFT,LLT
          FAC=HALF*RHO(I)/MAX(EM20,VIS(I))
          PE=FAC*GAM(I)
          IF(PE<=EM3)THEN
            GAM(I)=CH1*FAC*DELTAX(I)**2
          ELSEIF(PE<THREE)THEN
            GAM(I)=CH1*FAC*GAM(I)**2/V(I)
          ELSE
            GAM(I)=CH2*GAM(I)/V(I)
          ENDIF
        ENDDO
      ENDIF

      RETURN
      END

Chd|====================================================================
Chd|  UPWIND_V                      source/elements/solid/solide/upwind.F
Chd|-- called by -----------
Chd|        AMOMTN3                       source/elements/solid/solide/amomtn3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPWIND_V(RHO,VIS,VDX,VDY,VDZ,R,S,T,DELTAX,GAM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
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
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   R(MVSIZ,3),S(MVSIZ,3),T(MVSIZ,3),
     .   VDX(*),VDY(*),VDZ(*),DELTAX(*),RHO(*),VIS(*),GAM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .  FAC,PE,D1,D2,D3,DIST,CH1,CH2,
     .  DV(MVSIZ),V(MVSIZ)
      INTEGER I
C-----------------------------------------------

      !--------------------------------------------!
      ! /UPWM/TG                                   !
      !   Taylor Galerkine                         !
      !--------------------------------------------!
      IF(ALE%UPWIND%UPWM==2)THEN
        FAC=ALE%UPWIND%CUPWM*HALF*DT1
        DO I=LFT,LLT
         GAM(I)=FAC
        END DO

      !--------------------------------------------!
      ! /UPWM/SUPG                                 !
      !   Streaminline Upwind Petrov Galerkine     !
      !--------------------------------------------!
      ELSEIF(ALE%UPWIND%UPWM==3)THEN
        IF(N2D==0)THEN
          DO  I=LFT,LLT
            V(I)=VDX(I)**2+VDY(I)**2+VDZ(I)**2
            D1=R(I,1)*VDX(I)+R(I,2)*VDY(I)+R(I,3)*VDZ(I) 
            D2=S(I,1)*VDX(I)+S(I,2)*VDY(I)+S(I,3)*VDZ(I) 
            D3=T(I,1)*VDX(I)+T(I,2)*VDY(I)+T(I,3)*VDZ(I) 
            GAM(I)=SQRT(D1**2+D2**2+D3**2)
          ENDDO 
        ELSE
          DO  I=LFT,LLT
            D2=S(I,2)*VDY(I)+S(I,3)*VDZ(I) 
            D3=T(I,2)*VDY(I)+T(I,3)*VDZ(I) 
            GAM(I)=SQRT(D2**2+D3**2)
            V(I)=VDY(I)**2+VDZ(I)**2
          ENDDO 
        ENDIF               
        CH2=ALE%UPWIND%CUPWM*HALF
        CH1=CH2*THIRD
        DO  I=LFT,LLT
          FAC=HALF*RHO(I)/MAX(EM20,VIS(I))
          PE=FAC*GAM(I)
          IF(PE<=EM3)THEN
            GAM(I)=CH1*FAC*DELTAX(I)**2
          ELSEIF(PE<THREE)THEN
            GAM(I)=CH1*FAC*GAM(I)**2/V(I)
          ELSE
            GAM(I)=CH2*GAM(I)/V(I)
          ENDIF
        ENDDO
      ENDIF

      RETURN
      END
