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
C
!||====================================================================
!||    r2buf3   ../starter/source/elements/spring/r2buf3.F
!||--- called by ------------------------------------------------------
!||    rinit3   ../starter/source/elements/spring/rinit3.F
!||====================================================================
      SUBROUTINE R2BUF3(OFF  ,GEO   ,X     ,X0    ,Y0   ,
     2                  Z0   ,IX    ,SKEW  ,IPOSX ,IPOSY,
     3                  IPOSZ,IPOSXX,IPOSYY,IPOSZZ,IGEO,
     4                  SKEW_ID)
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
      INTEGER IX(NIXR,*),IGEO(NPROPGI,*),SKEW_ID(*)
C     REAL
      my_real
     .   OFF(*), GEO(NPROPG,*), X(3,*), X0(*), Y0(*), Z0(*),
     .   SKEW(LSKEW,*),IPOSX(5,*) ,IPOSY(5,*),
     .   IPOSZ(5,*),IPOSXX(5,*),IPOSYY(5,*),IPOSZZ(5,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NG, I1, I2, ISK, K, USENS
C     REAL
      my_real
     .   X1, Y1, Z1, EX, EY, EZ
C-----------------------------------------------
      DO I=LFT,LLT
        J=I+NFT
        USENS=IGEO(3,IX(1,J))
        IF(USENS<=0)THEN
C         no sensor or Isflag=1
          OFF(I)=ONE
        ELSE
          OFF(I)=-TEN
        END IF
      ENDDO
C
      DO J=1,5
        DO I=LFT,LLT
          IPOSX(J,I)=ZERO
          IPOSY(J,I)=ZERO
          IPOSZ(J,I)=ZERO
          IPOSXX(J,I)=ZERO
          IPOSYY(J,I)=ZERO
          IPOSZZ(J,I)=ZERO
        ENDDO
      ENDDO
C
      DO I=LFT,LLT
        J=I+NFT
        NG=IX(1,J)
        ISK=SKEW_ID(I)
        I1=IX(2,J)
        I2=IX(3,J)
        X1=X(1,I2)-X(1,I1)
        Y1=X(2,I2)-X(2,I1)
        Z1=X(3,I2)-X(3,I1)
        EX=SKEW(1,ISK)
        EY=SKEW(2,ISK)
        EZ=SKEW(3,ISK)
        X0(I)=EX*X1+EY*Y1+EZ*Z1
        EX=SKEW(4,ISK)
        EY=SKEW(5,ISK)
        EZ=SKEW(6,ISK)
        Y0(I)=EX*X1+EY*Y1+EZ*Z1
        EX=SKEW(7,ISK)
        EY=SKEW(8,ISK)
        EZ=SKEW(9,ISK)
        Z0(I)=EX*X1+EY*Y1+EZ*Z1
      ENDDO
C-----------------------------------------------
      RETURN
      END
