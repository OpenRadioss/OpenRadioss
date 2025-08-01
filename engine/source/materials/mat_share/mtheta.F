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
!||    mtheta   ../engine/source/materials/mat_share/mtheta.F
!||--- called by ------------------------------------------------------
!||    mmain    ../engine/source/materials/mat_share/mmain.F90
!||====================================================================
      SUBROUTINE MTHETA(
     1   PM,      EINT,    THETA,   AMU,
     2   C1,      C2,      DF,      PSH,
     3   PC,      MAT,     NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER MAT(*)
C     REAL
      my_real
     .   PM(NPROPM,*), EINT(*), THETA(*),
     .   AMU(*), C1(*), C2(*), DF(*), PSH(*), PC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX
      my_real EINT0, SPH, DMU, PLIM, C2MU, C1PSH, ECOLD      
C=======================================================================
      MX = MAT(1)
      DO I=1,NEL
       SPH     =PM(69,MX)
       EINT0   =PM(23,MX)
       THETA(I)=PM(79,MX)
       PLIM    =PC(I)-C1(I)-PSH(I)
       IF(C2(I)>ABS(PLIM/EP36))THEN
         DMU = MIN(AMU(I)-PLIM/C2(I),ZERO)
       ELSE
         DMU =ZERO
       ENDIF
C
       C2MU    =C2(I)*AMU(I)
       C1PSH   =C1(I)+PSH(I)
C              ENERGIE ELASTIQUE
       ECOLD   =(C1PSH + HALF*C2MU)*AMU(I)
C              ENERGIE DE RUPTURE EN TRITENSION (OU CAVITATION)
C              HYP: NE CREEE PAR D'ECHAUFFEMENT
       ECOLD   =ECOLD + HALF*DMU*(C1PSH+C2MU-PC(I))
C              ENERGIE INITIALE
       ECOLD   =ECOLD+EINT0
       THETA(I)=THETA(I) + (EINT(I)*DF(I)-ECOLD)/SPH
      ENDDO
c-----------
      RETURN
      END
