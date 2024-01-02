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
c
Chd|====================================================================
Chd|  S8ZSIGP3                      source/elements/solid/solide8z/s8zsigp3.F
Chd|-- called by -----------
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|        Q4KE2                         source/elements/solid_2d/quad4/q4ke2.F
Chd|        S6CKE3                        source/elements/thickshell/solide6c/s6cke3.F
Chd|        S8CKE3                        source/elements/thickshell/solide8c/s8cke3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        S8ZKE3                        source/elements/solid/solide8z/s8zke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZSIGP3(LFT  ,LLT  ,SIG  ,E0  ,DEFP,
     2                    FAC  ,G_PLA,NEL  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT,G_PLA,NEL
C     REAL
      my_real
     .   SIG(NEL,6),E0(*),DEFP(*),FAC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   S1,S2,S3,SIGY,DEFT
C-----------------------------------------------
      IF (G_PLA == 0) THEN
        DO I=LFT,LLT
          FAC(I)=ZERO
        ENDDO
      ELSE
        DO I=LFT,LLT
         IF (DEFP(I) <= ZERO) THEN
           FAC(I)=ZERO
         ELSE
           S1 =SIG(I,1)-SIG(I,2)
           S2 =SIG(I,2)-SIG(I,3)
           S3 =SIG(I,1)-SIG(I,3)
           SIGY = (S1*S1+S2*S2+S3*S3)*HALF +THREE*(SIG(I,4)*SIG(I,4)
     .                   +SIG(I,5)*SIG(I,5)+SIG(I,6)*SIG(I,6))
           DEFT = SQRT(SIGY)/E0(I)+DEFP(I)
           FAC(I)=DEFP(I)/DEFT
         ENDIF
        ENDDO
      ENDIF
C-----------
      RETURN
      END
