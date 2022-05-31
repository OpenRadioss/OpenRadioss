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
C===========================================
C
C     UNIV.FOR
C
C===========================================
C-------------------------------------------
C
C     ROUTINES DE CODAGE ET DECODAGE
C
C-------------------------------------------
Chd|====================================================================
Chd|  STRR                          source/tools/univ/strr.F      
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        RINIT3                        source/elements/spring/rinit3.F
Chd|        SPMD_MSIN                     source/elements/initia/spmd_msin.F
Chd|        WRTSQR                        source/output/tools/wrtsqr.F  
Chd|        STRR                          source/tools/univ/strr.F      
Chd|-- calls ---------------
Chd|        STRR                          source/tools/univ/strr.F      
Chd|====================================================================
      CHARACTER*8 FUNCTION STRR(Y)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      my_real
     .   Y
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "chrasc_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IEXP, ISE, ISM, MANT, MANT1, MANT2, MANT3, IS, MANT11,
     .   MANT12, MANT21, MANT22, MANT31, MANT32
C     REAL ou REAL*8
      my_real
     .   X, A, XMAN, P99
C
      P99 = ZEP999EP31
      X=Y
      X= MIN(X,P99)
      X= MAX(X,-P99)
      A=ABS(X)
      IF(A.LT.ONEP001EM32)THEN
       STRR='10000000'
       RETURN
      ENDIF
C
      IEXP=LOG10(A)
      ISE=MAX(0,-ISIGN(1,IEXP))
      ISM=SIGN(ONE,X)
      ISM=MAX(0,-ISM)
      XMAN =A*10.**(-IEXP)
      XMAN =XMAN*1000000000.
      IF(XMAN.GE.1000000000.)THEN
       XMAN=XMAN/10.
       IEXP=IEXP+1
      ENDIF
      MANT =XMAN
      IF(MANT.GE.1000000000)THEN
       MANT=MANT/10
       IEXP=IEXP+1
      ENDIF
      IEXP=IABS(IEXP)
C
      MANT1=MANT/1000000
      MANT=MANT-1000000*MANT1
      MANT2=MANT/1000
      MANT3=MANT-1000*MANT2
      IS=2*ISM+ISE
C
      MANT11=MANT1/32
      MANT12=MANT1-32*MANT11
      MANT21=MANT2/32
      MANT22=MANT2-32*MANT21
      MANT31=MANT3/32
      MANT32=MANT3-32*MANT31
C
      STRR(1:1)=CODAS(IS+1)
      STRR(2:2)=CODAS(IEXP)
      STRR(3:3)=CODAS(MANT11)
      STRR(4:4)=CODAS(MANT12)
      STRR(5:5)=CODAS(MANT21)
      STRR(6:6)=CODAS(MANT22)
      STRR(7:7)=CODAS(MANT31)
      STRR(8:8)=CODAS(MANT32)
C
      RETURN
      END
