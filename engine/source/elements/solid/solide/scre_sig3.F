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
!||    scre_sig3     ../engine/source/elements/solid/solide/scre_sig3.F
!||--- called by ------------------------------------------------------
!||    sdistor_ini   ../engine/source/elements/solid/solide/sdistror_ini.F90
!||====================================================================
      SUBROUTINE SCRE_SIG3(SIG,   SIG_C,  ISTAB,  OFFG , ISMSTR , NEL)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL,ISMSTR
      my_real, INTENT(IN) :: SIG_C
      my_real, DIMENSION(NEL,6), INTENT(IN)  :: SIG
      my_real, DIMENSION(NEL), INTENT(IN)    :: OFFG
      INTEGER, DIMENSION(MVSIZ), INTENT(OUT) :: ISTAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real
     .   SMIN
C-----------------------------------------------
      ISTAB(1:NEL) = 0
      IF (ISMSTR==12) THEN
        DO I=1,NEL
          IF (OFFG(I)==ZERO) CYCLE
          SMIN = MIN(SIG(I,1),SIG(I,2),SIG(I,3),
     1               SIG(I,4),SIG(I,5),SIG(I,6))
          IF (OFFG(I)>ONE.OR.SIG_C<-SMIN) ISTAB(I) = 1
        END DO              
      ELSE
        DO I=1,NEL
          IF (OFFG(I)==ZERO) CYCLE
          SMIN = MIN(SIG(I,1),SIG(I,2),SIG(I,3),
     1               SIG(I,4),SIG(I,5),SIG(I,6))
          IF (SIG_C<-SMIN) ISTAB(I) = 1
        END DO              
      END IF 

      RETURN
      END
