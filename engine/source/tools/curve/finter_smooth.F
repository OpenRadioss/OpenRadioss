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
#include "my_real.inc"
Chd|====================================================================
Chd|  FINTER_SMOOTH                 source/tools/curve/finter_smooth.F
Chd|-- called by -----------
Chd|        DAASOLV                       source/fluid/daasolv.F        
Chd|        DAASOLVP                      source/fluid/daasolvp.F       
Chd|        FORCE                         source/loads/general/force.F  
Chd|        FORCEFINGEO                   source/loads/general/forcefingeo.F
Chd|        FORCEPINCH                    source/loads/general/forcepinch.F
Chd|        GRAVIT                        source/loads/general/grav/gravit.F
Chd|        GRAVIT_FVM_FEM                source/loads/general/grav/gravit_fvm_fem.F
Chd|        GRAVIT_IMP                    source/loads/general/grav/gravit_imp.F
Chd|        H3D_PRE_SKIN_SCALAR           source/output/h3d/h3d_results/h3d_skin_scalar.F
Chd|        H3D_SKIN_VECTOR               source/output/h3d/h3d_results/h3d_skin_vector.F
Chd|        INCPFLOW                      source/fluid/incpflow.F       
Chd|        LAG_FXV                       source/tools/lagmul/lag_fxv.F 
Chd|        LAG_FXVP                      source/tools/lagmul/lag_fxv.F 
Chd|-- calls ---------------
Chd|====================================================================
      my_real FUNCTION FINTER_SMOOTH(IFUNC,XX,NPF,TF,DERI)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      INTEGER IFUNC,NPF(*),I
      my_real TF(*),DERI,XX,DX,DERI1,DERI2,DERI3,DX1,DX2,DIV0,DIV,
     . XX_FIRST,XX_LAST
C-----------------------------------------------
!
!  smooth interpolation
!
      DX2 = TF(NPF(IFUNC)) - XX
      XX_FIRST = TF(NPF(IFUNC))
      XX_LAST  = TF(NPF(IFUNC+1)-2)
!
      IF ((NPF(IFUNC+1)-NPF(IFUNC)) == 2) THEN
!     constant function
        FINTER_SMOOTH = TF(NPF(IFUNC)+1)
        RETURN
      ELSE
!--------------
        DO I=NPF(IFUNC)+2,NPF(IFUNC+1)-2,2
          IF (XX <= XX_FIRST) THEN
            FINTER_SMOOTH = TF(NPF(IFUNC)+1)
            RETURN
          ELSEIF (XX >= XX_LAST) THEN
            FINTER_SMOOTH = TF(NPF(IFUNC+1)-1)
            RETURN
          ELSEIF (XX_FIRST < XX .AND. XX < XX_LAST) THEN
! within interval
!!            DX1 = -DX2
            DX1 = XX - TF(I-2)
            DX2 = TF(I) - XX
            IF (DX2 >= ZERO .OR. I == NPF(IFUNC+1)-2) THEN
              DIV0 = TF(I) - TF(I-2)
              DIV = MAX(ABS(DIV0),EM16)
              DIV = SIGN(DIV,DIV0)
              IF (DX1 <= DX2) THEN
!!                DERI= DX1/(TF(I)-TF(I-2))
                DERI= DX1 / DIV
                DERI1 = DERI
                DERI2 = DERI1*DERI1
                DERI3 = DERI1*DERI2
                FINTER_SMOOTH  = TF(I-1) + (TF(I+1)-TF(I-1))*DERI3*
     .                           (10. - 15.*DERI1 + 6.*DERI2)
              ELSE
!!                DERI= DX2/(TF(I)-TF(I-2))
                DERI= DX2 / DIV
                DERI1 = DERI
                DERI2 = DERI1*DERI1
                DERI3 = DERI1*DERI2
                FINTER_SMOOTH  = TF(I+1) - (TF(I+1)-TF(I-1))*DERI3*
     .                           (10. - 15.*DERI1 + 6.*DERI2)
              ENDIF ! IF (DX1 <= DX2)
              RETURN
            ENDIF ! IF (DX2 >= ZERO .OR. I == NPF(IFUNC+1)-2)
          ENDIF ! IF (XX <= XX_FIRST)
        ENDDO ! DO I=NPF(IFUNC)+2,NPF(IFUNC+1)-2,2
      ENDIF ! IF ((NPF(IFUNC+1)-NPF(IFUNC)) == 2)
!---
      RETURN
      END
Chd|====================================================================
Chd|  FINTER2_SMOOTH                source/tools/curve/finter_smooth.F
Chd|-- called by -----------
Chd|        FIXFINGEO                     source/constraints/general/impvel/fixfingeo.F
Chd|-- calls ---------------
Chd|====================================================================
      my_real FUNCTION FINTER2_SMOOTH(TF,IAD,IPOS,ILEN,XX,DYDX)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      INTEGER J,J1,J2,ICONT,ILEN,IPOS,IAD
      my_real TF(2,*),DYDX,XX,DYDX1,DYDX2,DYDX3
C-----------------------------------------------
      J = 0
      ICONT = 1
      DO WHILE (ICONT == 1)
        J = J+1
        ICONT = 0
        J1 = IPOS+IAD+1
        IF (J <= ILEN-1 .AND. XX > TF(1,J1)) THEN
          IPOS = IPOS + 1
          ICONT = 1
        ELSEIF (IPOS >= 1 .AND. XX < TF(1,J1-1)) THEN
          IPOS = IPOS - 1
          ICONT = 1
        ENDIF
      ENDDO ! DO WHILE (ICONT == 1)
!
!  smooth interpolation
!
      J1 = IAD + IPOS
      J2 = J1+1
      DYDX = (XX-TF(1,J1))/(TF(1,J2)-TF(1,J1))
!
      DYDX1 = DYDX
      DYDX2 = DYDX1*DYDX1
      DYDX3 = DYDX1*DYDX2
!
      FINTER2_SMOOTH = TF(2,J1) + (TF(2,J2)-TF(2,J1))*DYDX3*
     .                            (10. - 15.*DYDX1 + 6.*DYDX2)
!
!!      FINTER2_SMOOTH = TF(2,J1) + (TF(2,J2)-TF(2,J1))*DYDX**3*
!!     .                            (10. - 15.*DYDX + 6.*DYDX**2)
!---
      RETURN
      END
