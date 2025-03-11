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
      !||    adiff3                 ../engine/source/ale/ale3d/adiff3.F
      !||--- called by ------------------------------------------------------
      !||    aeturb                 ../engine/source/ale/turbulence/aeturb.F
      !||    akturb                 ../engine/source/ale/turbulence/akturb.F
      !||    atherm                 ../engine/source/ale/atherm.F
      !||--- uses       -----------------------------------------------------
      !||    ale_connectivity_mod   ../common_source/modules/ale/ale_connectivity_mod.F
      !||====================================================================
      SUBROUTINE ADIFF3(PHIN,PHI,GRAD,COEF,ALE_CONNECT,VOL,TEMP,RHOCP,NEL)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Solving Heat equation
C    alpha = k/rhocp is thermal diffusivity
c    coeff is k (factor simplification since originally only EINT/V was updated)
c       PHIN is EINT/V : updated at the end of the subroutine
C       TEMP : updated at the end of the subroutine.
C              Since temperature is calculated incrementally, both energy and temperature must be consistently updated.
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NEL
      my_real,INTENT(INOUT) :: TEMP(NEL)
      my_real,INTENT(INOUT) :: PHIN(NEL) ! EINT/V
      my_real,INTENT(IN) :: RHOCP
      my_real,INTENT(IN) :: PHI(NEL), GRAD(6,NEL), VOL(NEL)
      my_real,INTENT(IN) :: COEF(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IE, IV1, IV2, IV3, IV4, IV5, IV6, IAD2  
      my_real DPHI(MVSIZ)
      my_real :: COEFF(0:6) ! k = alpha * rhocp
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO I=1,NEL
        IE =NFT+I
        IAD2 = ALE_CONNECT%ee_connect%iad_connect(IE)
        IV1 = ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
        IV2 = ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
        IV3 = ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
        IV4 = ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
        IV5 = ALE_CONNECT%ee_connect%connected(IAD2 + 5 - 1)
        IV6 = ALE_CONNECT%ee_connect%connected(IAD2 + 6 - 1)

        ! adjacent cells (IV=0 => ghost cell with same value as IE)
        IF(IV1 <= 0)IV1=IE
        IF(IV2 <= 0)IV2=IE
        IF(IV3 <= 0)IV3=IE
        IF(IV4 <= 0)IV4=IE
        IF(IV5 <= 0)IV5=IE
        IF(IV6 <= 0)IV6=IE
        COEFF(0) = COEF(IE)
        COEFF(1) = COEF(IV1)
        COEFF(2) = COEF(IV2)
        COEFF(3) = COEF(IV3)
        COEFF(4) = COEF(IV4)
        COEFF(5) = COEF(IV5)
        COEFF(6) = COEF(IV6)
        IF(COEFF(1) == ZERO) COEFF(1)=COEFF(0)
        IF(COEFF(2) == ZERO) COEFF(2)=COEFF(0)
        IF(COEFF(3) == ZERO) COEFF(3)=COEFF(0)
        IF(COEFF(4) == ZERO) COEFF(4)=COEFF(0)
        IF(COEFF(5) == ZERO) COEFF(5)=COEFF(0)
        IF(COEFF(6) == ZERO) COEFF(6)=COEFF(0)

       DPHI(I) =
     1           (COEFF(0)*COEFF(1))*(PHI(IV1)-PHI(IE))*GRAD(1,I) / MAX(EM20,(COEFF(0)+COEFF(1)))
     2         + (COEFF(0)*COEFF(2))*(PHI(IV2)-PHI(IE))*GRAD(2,I) / MAX(EM20,(COEFF(0)+COEFF(2)))
     3         + (COEFF(0)*COEFF(3))*(PHI(IV3)-PHI(IE))*GRAD(3,I) / MAX(EM20,(COEFF(0)+COEFF(3)))
     4         + (COEFF(0)*COEFF(4))*(PHI(IV4)-PHI(IE))*GRAD(4,I) / MAX(EM20,(COEFF(0)+COEFF(4)))
     5         + (COEFF(0)*COEFF(5))*(PHI(IV5)-PHI(IE))*GRAD(5,I) / MAX(EM20,(COEFF(0)+COEFF(5)))
     6         + (COEFF(0)*COEFF(6))*(PHI(IV6)-PHI(IE))*GRAD(6,I) / MAX(EM20,(COEFF(0)+COEFF(6)))
      ENDDO!next I
C-----------------------------------------------------------
        ! time integration for Eint/V
      DO I=1,NEL
          DPHI(I)=TWO*DPHI(I)*DT1/MAX(VOL(I),EM20)
      ENDDO
C-----------------------------------------------------------
      ! Eint/V updated
      DO I=1,NEL
        PHIN(I)=PHIN(I)+DPHI(I)
      ENDDO
C-----------------------------------------------------------
      ! temperature updated
      IF(RHOCP > ZERO)THEN
        DO I=1,NEL
          TEMP(I) = TEMP(I) + DPHI(I)/RHOCP
        ENDDO
      ENDIF
C-----------------------------------------------------------
      RETURN
      END
