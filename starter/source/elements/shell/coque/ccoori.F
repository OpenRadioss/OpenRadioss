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
Chd|  CCOORI                        source/elements/shell/coque/ccoori.F
Chd|-- called by -----------
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|        INIRIG_MAT                    source/elements/initia/inirig_mat.F
Chd|        INIVOID                       source/elements/initia/inivoid.F
Chd|        LSLOCAL                       source/elements/xfem/lslocal.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE CCOORI(X,XREFC,IXC,
     .           X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  , 
     .           Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .           IX1 ,IX2 ,IX3 ,IX4 ,NGL )
C-----------------------------------------------
      USE MESSAGE_MOD
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
#include      "scr03_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*)
      INTEGER , DIMENSION(MVSIZ), INTENT(OUT)  :: NGL,
     .                     IX1,IX2,IX3,IX4
      my_real X(3,*),XREFC(4,3,*)
      my_real , DIMENSION(MVSIZ), INTENT(OUT)  :: 
     .                           X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, TEST0, TEST1, TEST2 
      my_real XN14, XN13, XN24, XN23, XN12, X31,
     .   Y31, Z31, X42, Y42, Z42, E3X, E3Y, E3Z, SURF
C=======================================================================
C     CONNECTIVITES ET SHELL ID                   
C----------------------------
      DO I=LFT,LLT
        IX1(I)=IXC(2,I)
        IX2(I)=IXC(3,I)
        IX3(I)=IXC(4,I)
        IX4(I)=IXC(5,I)
        NGL(I)=IXC(NIXC,I)
      ENDDO 
C----------------------------
C     COORDONNEES           
C----------------------------
      IF (NXREF == 0) THEN
        DO I=LFT,LLT
          X1(I)=X(1,IX1(I))
          Y1(I)=X(2,IX1(I))
          Z1(I)=X(3,IX1(I))
          X2(I)=X(1,IX2(I))
          Y2(I)=X(2,IX2(I))
          Z2(I)=X(3,IX2(I))
          X3(I)=X(1,IX3(I))
          Y3(I)=X(2,IX3(I))
          Z3(I)=X(3,IX3(I))
          X4(I)=X(1,IX4(I))
          Y4(I)=X(2,IX4(I))
          Z4(I)=X(3,IX4(I))
        ENDDO
      ELSE
        DO I=LFT,LLT
          X1(I)=XREFC(1,1,I)
          Y1(I)=XREFC(1,2,I)
          Z1(I)=XREFC(1,3,I)
          X2(I)=XREFC(2,1,I)
          Y2(I)=XREFC(2,2,I)
          Z2(I)=XREFC(2,3,I)
          X3(I)=XREFC(3,1,I)
          Y3(I)=XREFC(3,2,I)
          Z3(I)=XREFC(3,3,I)
          X4(I)=XREFC(4,1,I)
          Y4(I)=XREFC(4,2,I)
          Z4(I)=XREFC(4,3,I)
        ENDDO
      ENDIF
c
      DO I=LFT,LLT
        XN14=(X1(I)-X4(I))**2+(Y1(I)-Y4(I))**2+(Z1(I)-Z4(I))**2
        XN13=(X1(I)-X3(I))**2+(Y1(I)-Y3(I))**2+(Z1(I)-Z3(I))**2
        XN24=(X2(I)-X4(I))**2+(Y2(I)-Y4(I))**2+(Z2(I)-Z4(I))**2
        XN23=(X2(I)-X3(I))**2+(Y2(I)-Y3(I))**2+(Z2(I)-Z3(I))**2
        XN12=(X1(I)-X2(I))**2+(Y1(I)-Y2(I))**2+(Z1(I)-Z2(I))**2
        X31=X3(I)-X1(I)
        Y31=Y3(I)-Y1(I)
        Z31=Z3(I)-Z1(I)
        X42=X4(I)-X2(I)
        Y42=Y4(I)-Y2(I)
        Z42=Z4(I)-Z2(I)
        E3X=Y31*Z42-Z31*Y42
        E3Y=Z31*X42-X31*Z42
        E3Z=X31*Y42-Y31*X42
        SURF=SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
C
        TEST0=0
        TEST1=0
        TEST2=0
        IF (XN13+XN24+XN12 < EM20) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1=IXC(NIXC,I))
          TEST0=1
        ELSE IF(SURF<=EM20) THEN
          CALL ANCMSG(MSGID=21,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1=IXC(NIXC,I))
          TEST1=1
        ELSE IF(XN12<=1.E-20.OR.XN23<=1.E-20.OR.XN14<=1.E-20) THEN
          CALL ANCMSG(MSGID=22,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1=IXC(NIXC,I))
           TEST2=1
        ENDIF
        IF (TEST0 == 1 .OR. TEST1 == 1 .OR. TEST2 == 1) THEN
          X1(I)=ZERO
          Y1(I)=ZERO
          Z1(I)=ZERO
          X2(I)=ONE
          Y2(I)=ZERO
          Z2(I)=ZERO
          X3(I)=ONE
          Y3(I)=ONE
          Z3(I)=ZERO
          X4(I)=ZERO
          Y4(I)=ONE
          Z4(I)=ZERO 
        ENDIF
      ENDDO
C-----------
      RETURN
      END
