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
Chd|  C3COORI                       source/elements/sh3n/coque3n/c3coori.F
Chd|-- called by -----------
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        CDKINIT3                      source/elements/sh3n/coquedk/cdkinit3.F
Chd|        INIRIG_MAT                    source/elements/initia/inirig_mat.F
Chd|        INIVOID                       source/elements/initia/inivoid.F
Chd|        LSLOCAL                       source/elements/xfem/lslocal.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE C3COORI(X ,XREFTG,IXP ,NGL ,
     .                   X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .                   Z1  ,Z2  ,Z3  ,IX1 ,IX2 ,IX3 )
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
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C      V a r i a b l e s
C-----------------------------------------------
      INTEGER  NGL(MVSIZ),IXP(NIXTG,*),IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ)
      my_real
     .    X(3,*),XREFTG(3,3,*),
     .    X1(MVSIZ),X2(MVSIZ),X3(MVSIZ),
     .    Y1(MVSIZ),Y2(MVSIZ),Y3(MVSIZ),
     .    Z1(MVSIZ),Z2(MVSIZ),Z3(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, TEST0, TEST1, TEST2
      my_real E3X, E3Y, E3Z, SURF,
     .    XN13, XN23, XN12, X13, Y13, Z13, X12, Y12, Z12
C=======================================================================
C     CONNECTIVITES ET MATERIEL                  
C--------------------------------------------------
      DO I=LFT,LLT
        NGL(I)=IXP(6,I)
        IX1(I)=IXP(2,I)
        IX2(I)=IXP(3,I)
        IX3(I)=IXP(4,I)
      ENDDO
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
        ENDDO
      ELSE
        DO I=LFT,LLT
          X1(I)=XREFTG(1,1,I)
          Y1(I)=XREFTG(1,2,I)
          Z1(I)=XREFTG(1,3,I)
          X2(I)=XREFTG(2,1,I)
          Y2(I)=XREFTG(2,2,I)
          Z2(I)=XREFTG(2,3,I)
          X3(I)=XREFTG(3,1,I)
          Y3(I)=XREFTG(3,2,I)
          Z3(I)=XREFTG(3,3,I)
        ENDDO
      ENDIF
c
      DO I=LFT,LLT
        XN13=(X1(I)-X3(I))**2+(Y1(I)-Y3(I))**2+(Z1(I)-Z3(I))**2
        XN23=(X2(I)-X3(I))**2+(Y2(I)-Y3(I))**2+(Z2(I)-Z3(I))**2
        XN12=(X1(I)-X2(I))**2+(Y1(I)-Y2(I))**2+(Z1(I)-Z2(I))**2
        X13=X3(I)-X1(I)
        Y13=Y3(I)-Y1(I)
        Z13=Z3(I)-Z1(I)
        X12=X2(I)-X1(I)
        Y12=Y2(I)-Y1(I)
        Z12=Z2(I)-Z1(I)
        E3X=Y12*Z13-Z12*Y13
        E3Y=Z12*X13-X12*Z13
        E3Z=X12*Y13-Y12*X13
        SURF=HALF*SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
C
        TEST0=0
        TEST1=0
        TEST2=0
        IF(XN13+XN12<EM20) THEN
           CALL ANCMSG(MSGID=35,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=IXP(NIXTG,I))
           TEST0=1
         ELSE IF(SURF<=EM20) THEN
           CALL ANCMSG(MSGID=35,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=IXP(NIXTG,I))
           TEST1=1
        ELSE IF(XN12<=EM20.OR.XN23<=EM20.OR.XN13<=EM20) THEN
           CALL ANCMSG(MSGID=36,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=IXP(NIXTG,I))
           TEST2=1
        ENDIF
        IF(TEST0==1.OR.TEST1==1.OR.TEST2==1) THEN
          X1(I)=ZERO
          Y1(I)=ZERO
          Z1(I)=ZERO
          X2(I)=ONE
          Y2(I)=ZERO
          Z2(I)=ZERO
          X3(I)=ONE
          Y3(I)=ONE
          Z3(I)=ZERO
        ENDIF
      ENDDO
C-----------                                                                               
      RETURN
      END
