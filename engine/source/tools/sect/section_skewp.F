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
Chd|====================================================================
Chd|  SECTION_SKEWP                 source/tools/sect/section_skewp.F
Chd|-- called by -----------
Chd|        SECTION                       source/tools/sect/section.F   
Chd|        SECTION_3N                    source/tools/sect/section_3n.F
Chd|        SECTION_C                     source/tools/sect/section_c.F 
Chd|        SECTION_P                     source/tools/sect/section_p.F 
Chd|        SECTION_R                     source/tools/sect/section_r.F 
Chd|        SECTION_S                     source/tools/sect/section_s.F 
Chd|        SECTION_S4                    source/tools/sect/section_s4.F
Chd|        SECTION_S6                    source/tools/sect/section_s6.F
Chd|        SECTION_T                     source/tools/sect/section_t.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SECTION_SKEWP(
     1   XXC, YYC, ZZC, XX4  , YY4 , ZZ4 ,
     2   XX5, YY5, ZZ5, XX6  , YY6 , ZZ6 ,
     3   XXN, YYN, ZZN, IFRAM, N1  , XSEC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFRAM, N1
      my_real XXC, YYC, ZZC,
     .        XX4, YY4, ZZ4, XX5, YY5, ZZ5, XX6, YY6, ZZ6,
     .        XXN, YYN, ZZN, XSEC(4,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N
      my_real XX1, YY1, ZZ1, XX2, YY2, ZZ2, XX3, YY3, ZZ3,
     .        AL4, AL6, AL5, D13, MAS, X1N, Y1N, Z1N
C
C
      IF(IFRAM<10)THEN
C      CALCUL DES MOMEMTS/REPERE N1 N2 N3
       XX1=XSEC(1,1)
       YY1=XSEC(1,2)
       ZZ1=XSEC(1,3)
       XX2=XSEC(2,1)
       YY2=XSEC(2,2)
       ZZ2=XSEC(2,3)
       XX3=XSEC(3,1)
       YY3=XSEC(3,2)
       ZZ3=XSEC(3,3)
       XX4=XX2-XX1
       YY4=YY2-YY1
       ZZ4=ZZ2-ZZ1
       AL4=SQRT(XX4**2+YY4**2+ZZ4**2)
       XX4=XX4/MAX(AL4,EM20)
       YY4=YY4/MAX(AL4,EM20)
       ZZ4=ZZ4/MAX(AL4,EM20)
       XX5=XX3-XX1
       YY5=YY3-YY1
       ZZ5=ZZ3-ZZ1
       XX6=YY4*ZZ5-ZZ4*YY5
       YY6=ZZ4*XX5-XX4*ZZ5
       ZZ6=XX4*YY5-YY4*XX5
       AL6=SQRT(XX6**2+YY6**2+ZZ6**2)
       XX6=XX6/MAX(AL6,EM20)
       YY6=YY6/MAX(AL6,EM20)
       ZZ6=ZZ6/MAX(AL6,EM20)
       XX5=YY6*ZZ4-ZZ6*YY4
       YY5=ZZ6*XX4-XX6*ZZ4
       ZZ5=XX6*YY4-YY6*XX4
       AL5=SQRT(XX5**2+YY5**2+ZZ5**2)
       XX5=XX5/MAX(AL5,EM20)
       YY5=YY5/MAX(AL5,EM20)
       ZZ5=ZZ5/MAX(AL5,EM20)
C
       XXN=XX6
       YYN=YY6
       ZZN=ZZ6
C
       ELSEIF (N1/=0) THEN
C      CALCUL DES MOMEMTS/REPERE GLOBAL
       XX4=XSEC(2,1)-XSEC(1,1)
       YY4=XSEC(2,2)-XSEC(1,2)
       ZZ4=XSEC(2,3)-XSEC(1,3)
       XX5=XSEC(3,1)-XSEC(1,1)
       YY5=XSEC(3,2)-XSEC(1,2)
       ZZ5=XSEC(3,3)-XSEC(1,3)
       XXN=YY4*ZZ5-ZZ4*YY5
       YYN=ZZ4*XX5-XX4*ZZ5
       ZZN=XX4*YY5-YY4*XX5
       AL6=SQRT(XXN**2+YYN**2+ZZN**2)
       XXN=XXN/MAX(AL6,EM20)
       YYN=YYN/MAX(AL6,EM20)
       ZZN=ZZN/MAX(AL6,EM20)
C
       XX4=ONE
       YY4=ZERO
       ZZ4=ZERO
       XX5=ZERO
       YY5=ONE
       ZZ5=ZERO
       XX6=ZERO
       YY6=ZERO
       ZZ6=ONE
       ELSE
       XXN=ZERO
       YYN=ZERO
       ZZN=ONE
       XX4=ONE
       YY4=ZERO
       ZZ4=ZERO
       XX5=ZERO
       YY5=ONE
       ZZ5=ZERO
       XX6=ZERO
       YY6=ZERO
       ZZ6=ONE
C
      ENDIF
C
C     CALCUL DE L'ORIGINE DU REPERE
C
      IF(IFRAM==0)THEN
        D13=(XX3-XX1)*XX4+(YY3-YY1)*YY4+(ZZ3-ZZ1)*ZZ4
        XXC=XX1+D13*XX4
        YYC=YY1+D13*YY4
        ZZC=ZZ1+D13*ZZ4
      ELSEIF(IFRAM==10)THEN
        X1N=XSEC(2,1)-XSEC(1,1)
        Y1N=XSEC(2,2)-XSEC(1,2)
        Z1N=XSEC(2,3)-XSEC(1,3)
        AL4=SQRT(X1N**2+Y1N**2+Z1N**2)
        X1N=X1N/MAX(AL4,EM20)
        Y1N=Y1N/MAX(AL4,EM20)
        Z1N=Z1N/MAX(AL4,EM20)
        D13=(XSEC(3,1)-XSEC(1,1))*X1N
     .     +(XSEC(3,2)-XSEC(1,2))*Y1N
     .     +(XSEC(3,3)-XSEC(1,3))*Z1N
        XXC=XSEC(1,1)+D13*X1N
        YYC=XSEC(1,2)+D13*Y1N
        ZZC=XSEC(1,3)+D13*Z1N
      ELSEIF(MOD(IFRAM,10)==1)THEN
        XXC=0.
        YYC=0.
        ZZC=0.
C        DO I=1,NNOD
C             N=NOD(I)
C             XXC=XXC+X(1,N)
C             YYC=YYC+X(2,N)
C             ZZC=ZZC+X(3,N)
C        ENDDO
C Deja fait dans spmd_exch_sec
        XXC=XSEC(4,1)
        YYC=XSEC(4,2)
        ZZC=XSEC(4,3)
      ELSEIF(MOD(IFRAM,10)==2)THEN
        XXC=0.
        YYC=0.
        ZZC=0.
        MAS=1.E-30
C        DO I=1,NNOD
C             N=NOD(I)
C             XXC=XXC+X(1,N)*MS(N)
C             YYC=YYC+X(2,N)*MS(N)
C             ZZC=ZZC+X(3,N)*MS(N)
C             MAS=MAS+MS(N)
C        ENDDO
C Deja fait dans spmd_exch_sec
        XXC=XSEC(4,1)
        YYC=XSEC(4,2)
        ZZC=XSEC(4,3)
      ELSEIF(MOD(IFRAM,10)==3)THEN
        XXC=0.
        YYC=0.
        ZZC=0.
      ENDIF
C
      RETURN
      END
