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
Chd|  S10SIGP3                      source/elements/solid/solide4_sfem/s10sigp3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10SIGP3(SIG  ,E0  ,DEFP, FAC  ,G_PLA,NEL  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER G_PLA,NEL
C     REAL
      my_real ,DIMENSION(NEL) ,INTENT(IN) :: DEFP
      my_real ,DIMENSION(NEL,6) ,INTENT(IN) :: SIG
      my_real ,DIMENSION(MVSIZ) ,INTENT(IN) :: E0
      my_real ,DIMENSION(MVSIZ) ,INTENT(OUT) :: FAC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   S1,S2,S3,SIGY,DEFT,RFAC,BASE,FMAX
C-----------------------------------------------
      RFAC = ONE/FIFTEEN
      FMAX = ZEP995
      IF (G_PLA == 0) THEN
        FAC(1:NEL)=ZERO
      ELSE
        DO I=1,NEL
         IF (DEFP(I) <= ZERO) THEN
           FAC(I)=ZERO
         ELSE
           S1 =SIG(I,1)-SIG(I,2)
           S2 =SIG(I,2)-SIG(I,3)
           S3 =SIG(I,1)-SIG(I,3)
           SIGY = (S1*S1+S2*S2+S3*S3)*HALF +THREE*(SIG(I,4)*SIG(I,4)
     .                   +SIG(I,5)*SIG(I,5)+SIG(I,6)*SIG(I,6))
           DEFT = SQRT(SIGY)/E0(I)+DEFP(I)
           BASE = DEFP(I)/DEFT
           FAC(I) =EXP(RFAC*LOG(BASE))
           FAC(I) =MIN(FMAX,FAC(I))
         ENDIF
        ENDDO
      ENDIF
C-----------
      RETURN
      END SUBROUTINE S10SIGP3

