Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  MTHETA                        source/materials/mat_share/mtheta.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MTHETA(PM,EINT,THETA,AMU, C1, C2,
     2                  DF,PSH ,PC   ,MAT)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
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
      MX = MAT(LFT)
      DO I=LFT,LLT
       SPH     =PM(69,MX)
       EINT0   =PM(23,MX)
       THETA(I)=PM(79,MX)
       PLIM    =PC(I)-C1(I)-PSH(I)
       IF(C2(I).GT.ABS(PLIM/EP36))THEN
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
