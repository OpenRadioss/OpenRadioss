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
!||    coqini     ../engine/source/elements/shell/coqini.F
!||--- called by ------------------------------------------------------
!||    radioss2   ../engine/source/engine/radioss2.F
!||====================================================================
      SUBROUTINE COQINI
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com20_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .   Z01(11,11), WF1(11,11), WM1(11,11), ZN1(12,11)
C-----------------------------------------------
      DATA  Z01/
     1 0.       ,0.       ,0.       ,0.       ,0.       ,
     1 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     2 -.5      ,0.5      ,0.       ,0.       ,0.       ,
     2 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     3 -.5      ,0.       ,0.5      ,0.       ,0.       ,
     3 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     4 -.5      ,-.1666667,0.1666667,0.5      ,0.       ,
     4 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     5 -.5      ,-.25     ,0.       ,0.25     ,0.5      ,
     5 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     6 -.5      ,-.3      ,-.1      ,0.1      ,0.3      ,
     6 0.5      ,0.       ,0.       ,0.       ,0.       ,0.       ,
     7 -.5      ,-.3333333,-.1666667,0.0      ,0.1666667,
     7 0.3333333,0.5      ,0.       ,0.       ,0.       ,0.       ,
     8 -.5      ,-.3571429,-.2142857,-.0714286,0.0714286,
     8 0.2142857,0.3571429,0.5      ,0.       ,0.       ,0.       ,
     9 -.5      ,-.375    ,-.25     ,-.125    ,0.0      ,
     9 0.125    ,0.25     ,0.375    ,0.5      ,0.       ,0.       ,
     A -.5      ,-.3888889,-.2777778,-.1666667,-.0555555,
     A 0.0555555,0.1666667,0.2777778,0.3888889,0.5      ,0.       ,
     B -.5      ,-.4      ,-.3      ,-.2      ,-.1      ,
     B 0.       ,0.1      ,0.2      ,0.3      ,0.4      ,0.5      /
C
      DATA  WF1/
     1 1.       ,0.       ,0.       ,0.       ,0.       ,
     1 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     2 0.5      ,0.5      ,0.       ,0.       ,0.       ,
     2 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     3 0.25     ,0.5      ,0.25     ,0.       ,0.       ,
     3 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     4 0.1666667,0.3333333,0.3333333,0.1666667,0.       ,
     4 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     5 0.125    ,0.25     ,0.25     ,0.25     ,0.125    ,
     5 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     6 0.1      ,0.2      ,0.2      ,0.2      ,0.2      ,
     6 0.1      ,0.       ,0.       ,0.       ,0.       ,0.       ,
     7 0.0833333,0.1666667,0.1666667,0.1666667,0.1666667,
     7 0.1666667,0.0833333,0.       ,0.       ,0.       ,0.       ,
     8 0.0714286,0.1428571,0.1428571,0.1428571,0.1428571,
     8 0.1428571,0.1428571,0.0714286,0.       ,0.       ,0.       ,
     9 0.0625   ,0.125    ,0.125    ,0.125    ,0.125    ,
     9 0.125    ,0.125    ,0.125    ,0.0625   ,0.       ,0.       ,
     A 0.0555556,0.1111111,0.1111111,0.1111111,0.1111111,
     A 0.1111111,0.1111111,0.1111111,0.1111111,0.0555556,0.       ,
     B 0.05     ,0.1      ,0.1      ,0.1      ,0.1      ,
     B 0.1      ,0.1      ,0.1      ,0.1      ,0.1      ,0.05     /
C 
C  022
C
C    WM1i = Z01i / (n-1)    if |Z01i| # 0.5
C    WM1i =  1/2(n-1) [1/4 - 1/3(n-1)]    if  Z01i =  0.5
C    WM1i = -1/2(n-1) [1/4 - 1/3(n-1)]    if  Z01i = -0.5
C
      DATA  WM1/
     1 0.       ,0.       ,0.       ,0.       ,0.       ,
     1 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     2 -.0833333,0.0833333,0.       ,0.       ,0.       ,
     2 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     3 -.0833333,0.       ,0.0833333,0.       ,0.       ,
     3 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     4 -.0648148,-.0555556,0.0555556,0.0648148,0.       ,
     4 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     5 -.0520833,-.0625   ,0.       ,0.0625   ,0.0520833,
     5 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     6 -.0433333,-.0600000,-.0200000,0.0200000,0.0600000,
     6 0.0433333,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,
     7 -.0370370,-.0555556,-.0277778,0.0000000,0.0277778,
     7 0.0555556,0.0370370,0.0000000,0.0000000,0.0000000,0.0000000,
     8 -.0323129,-.0510204,-.0306122,-.0102041,0.0102041,
     8 0.0306122,0.0510204,0.0323129,0.0000000,0.0000000,0.0000000,
     9 -.0286458,-.0468750,-.0312500,-.0156250,0.0000000,
     9 0.0156250,0.0312500,0.0468750,0.0286458,0.0000000,0.0000000,
     A -.0257202,-.0432099,-.0308642,-.0185185,-.0061728,
     A 0.0061728,0.0185185,0.0308642,0.0432099,0.0257202,0.0000000,
     B -.0233333,-.0400000,-.0300000,-.0200000,-.0100000,
     B 0.0000000,0.0100000,0.0200000,0.0300000,0.0400000,0.0233333/
C
      DATA  ZN1/
     1 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     1 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     2 -.5      ,0.5      ,0.       ,0.       ,0.       ,0.       ,
     2 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     3 -.5      ,-.25     ,0.25     ,0.5      ,0.       ,0.       ,
     3 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     4 -.5      ,-.3333333,0.       ,0.3333333,0.5      ,0.       ,
     4 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     5 -.5      ,-.375    ,-0.125   ,0.125    ,0.375    ,0.5      ,
     5 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     6 -.5      ,-.4      ,-.2      ,0.0      ,0.2      ,0.4      ,
     6 0.5      ,0.       ,0.       ,0.       ,0.       ,0.       ,
     7 -.5      ,-.4166667,-.25     ,-.0833333,0.0833333,0.25     ,
     7 0.4166667,0.5      ,0.       ,0.       ,0.       ,0.       ,
     8 -.5      ,-.4285715,-.2857143,-.1428572,0.0      ,0.1428572,
     8 0.2857143,0.4285715,0.5      ,0.       ,0.       ,0.       ,     
     9 -.5      ,-.4375   ,-.3125   ,-.1875   ,-.0625   ,0.0625   ,
     9 0.1875   ,0.3125   ,0.4375   ,0.5      ,0.       ,0.       ,
     A -.5      ,-.4444444,-.3333333,-.2222222,-.1111111,0.       ,
     A 0.1111111,0.2222222,0.3333333,0.4444444,0.5      ,0.       ,
     B -.5      ,-.45     ,-.35     ,-.25     ,-.15     ,-.05     ,
     B 0.05     ,0.15     ,0.25     ,0.35     ,0.45     ,0.5      /
     
      DO I=1,11
       DO J=1,11
        Z0(I,J)=Z01(I,J)
        WF(I,J)=WF1(I,J)
        WM(I,J)=WM1(I,J)
       ENDDO
      ENDDO
      DO I = 1,12
        DO J = 1, 11
          ZTH(I,J)=ZN1(I,J)
        ENDDO
      ENDDO
      RETURN
      END

!||====================================================================
!||    coqini_wm   ../engine/source/elements/shell/coqini.F
!||--- called by ------------------------------------------------------
!||    mulawc      ../engine/source/materials/mat_share/mulawc.F90
!||====================================================================
      SUBROUTINE COQINI_WM(WM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
            my_real, intent(out):: WM(11,11)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .    WM1(11,11)
C----------------------------------------------- 
C  022
C
C    WM1i = Z01i / (n-1)    if |Z01i| # 0.5
C    WM1i =  1/2(n-1) [1/4 - 1/3(n-1)]    if  Z01i =  0.5
C    WM1i = -1/2(n-1) [1/4 - 1/3(n-1)]    if  Z01i = -0.5
C
      DATA  WM1/
     1 0.       ,0.       ,0.       ,0.       ,0.       ,
     1 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     2 -.0833333,0.0833333,0.       ,0.       ,0.       ,
     2 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     3 -.0833333,0.       ,0.0833333,0.       ,0.       ,
     3 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     4 -.0648148,-.0555556,0.0555556,0.0648148,0.       ,
     4 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     5 -.0520833,-.0625   ,0.       ,0.0625   ,0.0520833,
     5 0.       ,0.       ,0.       ,0.       ,0.       ,0.       ,
     6 -.0433333,-.0600000,-.0200000,0.0200000,0.0600000,
     6 0.0433333,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,
     7 -.0370370,-.0555556,-.0277778,0.0000000,0.0277778,
     7 0.0555556,0.0370370,0.0000000,0.0000000,0.0000000,0.0000000,
     8 -.0323129,-.0510204,-.0306122,-.0102041,0.0102041,
     8 0.0306122,0.0510204,0.0323129,0.0000000,0.0000000,0.0000000,
     9 -.0286458,-.0468750,-.0312500,-.0156250,0.0000000,
     9 0.0156250,0.0312500,0.0468750,0.0286458,0.0000000,0.0000000,
     A -.0257202,-.0432099,-.0308642,-.0185185,-.0061728,
     A 0.0061728,0.0185185,0.0308642,0.0432099,0.0257202,0.0000000,
     B -.0233333,-.0400000,-.0300000,-.0200000,-.0100000,
     B 0.0000000,0.0100000,0.0200000,0.0300000,0.0400000,0.0233333/
                 
      DO I=1,11
           DO J=1,11
                 WM(I,J)=WM1(I,J)
           ENDDO
      ENDDO

      RETURN
                  END
            