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
Chd|  MMAT_H1                       source/elements/solid/solide8z/s8zke3.F
Chd|-- called by -----------
Chd|        Q4KE2                         source/elements/solid_2d/quad4/q4ke2.F
Chd|        S8CKE3                        source/elements/thickshell/solide8c/s8cke3.F
Chd|        S8ZKE3                        source/elements/solid/solide8z/s8zke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MMAT_H1(
     1   HH,      HH1,     FAC,     ICP,
     2   IPREDU,  NEL,     MTN,     ISMSTR,
     3   JHBE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: MTN
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: JHBE
      INTEGER ICP,IPREDU
C     REAL
      my_real
     .   HH(2,*),HH1(2,*),FAC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IFP
C     REAL
C-----------------------------------------------
       IPREDU=0
       IF (ICP==1.OR.(ICP==2.AND.JHBE==17)) IPREDU=1
C-----exclude nonlinear pressure laws (don't use p=c1*rho/(rho0-1)--hyperelastic first       
       IF (MTN==42.OR.MTN==62.OR.MTN==82) IPREDU=0
       IFP = 0
       IF (ICP==1.AND.JHBE==17.AND.ISMSTR==10) IFP=1
       IF (IFP==1) IPREDU=1
C       
        IF (IPREDU == 0) THEN
          DO I=1,NEL
           HH1(1,I)= HH(1,I)
           HH1(2,I) =HH(2,I)
          ENDDO
        ELSEIF (ICP==2.AND.JHBE==17.OR.IFP==1) THEN
C-----------constant pressure-----  
         DO I=1,NEL
          HH1(1,I)=HH(2,I)*(ONE-FAC(I))-TWO_THIRD*HH(2,I)*FAC(I)
          HH1(2,I) = HH(2,I)
         ENDDO
        ELSEIF (ICP == 1) THEN
C-----------constant pressure-----  
         DO I=1,NEL
          HH1(1,I)=-TWO_THIRD*HH(2,I)
          HH1(2,I) = HH(2,I)
         ENDDO
        END IF !(IPREDU == 0) THEN
C
      RETURN
      END
