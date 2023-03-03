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
Chd|  R1BUF3                        source/elements/spring/r1buf3.F
Chd|-- called by -----------
Chd|        RINIT3                        source/elements/spring/rinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE R1BUF3(OFF   ,GEO   ,X   ,AL  ,IX,
     .                  IPOS  ,IGEO  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IX(NIXR,*),IGEO(NPROPGI,*)
      my_real
     .   OFF(*), GEO(NPROPG,*), X(3,*), AL(*),IPOS(5,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, I1, I2, K, USENS
C     REAL
      my_real
     .   EX, EY, EZ
C-----------------------------------------------
      DO I=LFT,LLT
        J=I+NFT
        USENS=IGEO(3,IX(1,J))
        IF (USENS <= 0) THEN
C         no sensor or Isflag=1
          OFF(I)=ONE
        ELSE
          OFF(I)=-TEN
        ENDIF
      ENDDO
C
      DO J=1,5
        DO I=LFT,LLT
         IPOS(J,I)= ZERO
        ENDDO
      ENDDO
C
      DO I=LFT,LLT
        J=I+NFT
        I1=IX(2,J)
        I2=IX(3,J)
        EX=X(1,I2)-X(1,I1)
        EY=X(2,I2)-X(2,I1)
        EZ=X(3,I2)-X(3,I1)
        AL(I)=SQRT(EX*EX+EY*EY+EZ*EZ)
      ENDDO
C
      RETURN
      END
