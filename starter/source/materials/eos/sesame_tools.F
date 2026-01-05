Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    tb2si1               ../starter/source/materials/eos/sesame_tools.F
!||--- called by ------------------------------------------------------
!||    hm_read_eos_sesame   ../starter/source/materials/eos/hm_read_eos_sesame.F
!||====================================================================
      SUBROUTINE TB2SI1(DENS,PRES,ENER,NR,NT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NR,NT
      my_real
     .   DENS(*),PRES(NR,*),ENER(NR,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real
     .   DTAB2SI,PTAB2SI,ETAB2SI
C
C---------------------------------------------------------------
C Passage of units Table Sesame Type 301-306 with units if
C    1 Mg/m^3 --> 1000 Kg/m^3
C    1 GPa    --> 1E9 Kg/(m.s^2)
C    1 MJ/Kg  --> 1E6 m^2/s^2
C
C DTAB2SI: Coef.passage of the volume masses tables with units if
C PTAB2SI: Coef.Passage of table pressure to units if
C ETAB2SI: Coef.passage from free energy tables to units if
C
      DTAB2SI = 1000.
      PTAB2SI = 1.E09
      ETAB2SI = 1.E06
C
      DO I=1,NR
          DENS(I)=DENS(I)*DTAB2SI
      ENDDO
C
      DO  I=1,NR
        DO  J=1,NT
          PRES(I,J)=PRES(I,J)*PTAB2SI
          ENER(I,J)=ENER(I,J)*ETAB2SI
        ENDDO
      ENDDO
C
      RETURN
      END



!||====================================================================
!||    tbusr1               ../starter/source/materials/eos/sesame_tools.F
!||--- called by ------------------------------------------------------
!||    hm_read_eos_sesame   ../starter/source/materials/eos/hm_read_eos_sesame.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE TBUSR1(R,P,E,NR,NT,UNITAB)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      USE UNITAB_MOD
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NR,NT
      TYPE(UNIT_TYPE_), INTENT(IN) :: UNITAB
      my_real
     .   R(*),P(NR,*),E(NR,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real ALSCAL,AMSCAL,ATSCAL,DSCAL,PSCAL,ESCAL
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      AMSCAL=UNITAB%FAC_M_WORK
      ALSCAL=UNITAB%FAC_L_WORK
      ATSCAL=UNITAB%FAC_T_WORK
C----------------------------------------------------------------
C Passage of units Table Sesame Type 301-306 to units
C user must necessarily pass before calling this
C routine through the TB2SI1 routine
C conversion coefficients from SI units to user units
C M --> masse
C -> Length
C T --> temps
C density: M.L^-3
C Pressure: M.L^-1.t^-2
C energie         : L^2.T^-2
C L'energy array est en fait equivalent a la chaleur specifique 
C phase transition. See Physics Handbook, B. YAVORSKI
C ET A.DETLAF, p 927.
C
      DSCAL = (ALSCAL**3)/AMSCAL
      PSCAL = ALSCAL*(ATSCAL**2)/AMSCAL
      ESCAL = (ATSCAL**2)/(ALSCAL**2)
C
      DO  I=1,NR
          R(I)=R(I)*DSCAL
      ENDDO
C
      DO  I=1,NR
        DO  J=1,NT
          P(I,J)=P(I,J)*PSCAL
          E(I,J)=E(I,J)*ESCAL
        ENDDO
      ENDDO
C
      RETURN
      END
