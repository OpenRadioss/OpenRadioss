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
Chd|  SPCOOR3                       source/elements/solid/sconnect/spcoor3.F
Chd|-- called by -----------
Chd|        SUINIT3                       source/elements/elbuf_init/suinit3.F
Chd|-- calls ---------------
Chd|        CLSKEW3                       source/elements/shell/coque/clskew.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPCOOR3(
     .         X    ,IXS  ,GEO  ,MXT  ,PID ,NGL  ,
     .         IX1  ,IX2  ,IX3  ,IX4  ,IX5  ,IX6  ,IX7  ,IX8 ,
     .         X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8  ,   
     .         Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8  ,   
     .         Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8  ,   
     .         E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .         VOLU )
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),MXT(*),NGL(*),PID(*),
     .   IX1(*),IX2(*),IX3(*),IX4(*),IX5(*),IX6(*),IX7(*),IX8(*)        
C     REAL
      my_real
     .   X(3,*),GEO(*),VOLU(*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*), 
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),
     .   E1X(*),E2X(*),E3X(*),E1Y(*),E2Y(*),E3Y(*),E1Z(*),E2Z(*),E3Z(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IREP
C     REAL
      my_real
     .   XL,YL,ZL,SUM
      my_real 
     .   P1X(MVSIZ), P2X(MVSIZ), P3X(MVSIZ), P4X(MVSIZ),
     .   P1Y(MVSIZ), P2Y(MVSIZ), P3Y(MVSIZ), P4Y(MVSIZ),
     .   P1Z(MVSIZ), P2Z(MVSIZ), P3Z(MVSIZ), P4Z(MVSIZ),
     .   RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),SX(MVSIZ),SY(MVSIZ),SZ(MVSIZ)
C-----------------------------------------------
C   E x t e r n a l  F u n c t i o n s
C-----------------------------------------------
      my_real
     .    CHECKVOLUME_8N
C=======================================================================
C     CONNECTIVITES ET NUMERO DE MATERIAU ET PID
C--------------------------------------------------
      DO I=LFT,LLT
        MXT(I) =IXS(1,I)
        IX1(I) =IXS(2,I)
        IX2(I) =IXS(3,I)
        IX3(I) =IXS(4,I)
        IX4(I) =IXS(5,I)
        IX5(I) =IXS(6,I)
        IX6(I) =IXS(7,I)
        IX7(I) =IXS(8,I)
        IX8(I) =IXS(9,I)
        PID(I) =IXS(NIXS-1,I)
        NGL(I) =IXS(NIXS,I)
        IF (CHECKVOLUME_8N(X ,IXS(1,I)) < ZERO) THEN
C         renumber connectivity
          IX1(I)=IXS(6,I)
          IX2(I)=IXS(7,I)
          IX3(I)=IXS(8,I)
          IX4(I)=IXS(9,I)
          IX5(I)=IXS(2,I)
          IX6(I)=IXS(3,I)
          IX7(I)=IXS(4,I)
          IX8(I)=IXS(5,I)
          IXS(2,I)=IX1(I)
          IXS(3,I)=IX2(I)
          IXS(4,I)=IX3(I)
          IXS(5,I)=IX4(I)
          IXS(6,I)=IX5(I)
          IXS(7,I)=IX6(I)
          IXS(8,I)=IX7(I)
          IXS(9,I)=IX8(I)
        ENDIF
      ENDDO
C----------------------------
C     COORDONNEES
C----------------------------
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
        X5(I)=X(1,IX5(I))  
        Y5(I)=X(2,IX5(I))  
        Z5(I)=X(3,IX5(I))  
        X6(I)=X(1,IX6(I))  
        Y6(I)=X(2,IX6(I))  
        Z6(I)=X(3,IX6(I))  
        X7(I)=X(1,IX7(I))  
        Y7(I)=X(2,IX7(I))  
        Z7(I)=X(3,IX7(I))  
        X8(I)=X(1,IX8(I))  
        Y8(I)=X(2,IX8(I))  
        Z8(I)=X(3,IX8(I))
      ENDDO                
      DO I=LFT,LLT 
        P1X(I)=(X1(I)+X5(I))*HALF
        P1Y(I)=(Y1(I)+Y5(I))*HALF
        P1Z(I)=(Z1(I)+Z5(I))*HALF
        P2X(I)=(X2(I)+X6(I))*HALF
        P2Y(I)=(Y2(I)+Y6(I))*HALF
        P2Z(I)=(Z2(I)+Z6(I))*HALF
        P3X(I)=(X3(I)+X7(I))*HALF
        P3Y(I)=(Y3(I)+Y7(I))*HALF
        P3Z(I)=(Z3(I)+Z7(I))*HALF
        P4X(I)=(X4(I)+X8(I))*HALF
        P4Y(I)=(Y4(I)+Y8(I))*HALF
        P4Z(I)=(Z4(I)+Z8(I))*HALF
        RX(I)=X2(I)-X1(I)
        RY(I)=Y2(I)-Y1(I)
        RZ(I)=Z2(I)-Z1(I)
        SX(I)=X3(I)-X1(I)
        SY(I)=Y3(I)-Y1(I)
        SZ(I)=Z3(I)-Z1(I)
      ENDDO
C----------------------------
C     LOCAL SYSTEM
C----------------------------
      IREP = 0
      CALL CLSKEW3(LFT,LLT ,IREP,
     .             RX, RY, RZ,SX, SY, SZ, 
     .             E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z,VOLU)
C-----------
C     REPERE CONVECTE
C-----------
      DO I=LFT,LLT                                                
        XL=E1X(I)*X1(I)+E1Y(I)*Y1(I)+E1Z(I)*Z1(I)                 
        YL=E2X(I)*X1(I)+E2Y(I)*Y1(I)+E2Z(I)*Z1(I)                 
        ZL=E3X(I)*X1(I)+E3Y(I)*Y1(I)+E3Z(I)*Z1(I)                 
        X1(I)=XL                                                  
        Y1(I)=YL                                                  
        Z1(I)=ZL                                                  
        XL=E1X(I)*X2(I)+E1Y(I)*Y2(I)+E1Z(I)*Z2(I)                 
        YL=E2X(I)*X2(I)+E2Y(I)*Y2(I)+E2Z(I)*Z2(I)                 
        ZL=E3X(I)*X2(I)+E3Y(I)*Y2(I)+E3Z(I)*Z2(I)                 
        X2(I)=XL                                                  
        Y2(I)=YL                                                  
        Z2(I)=ZL                                                  
        XL=E1X(I)*X3(I)+E1Y(I)*Y3(I)+E1Z(I)*Z3(I)                 
        YL=E2X(I)*X3(I)+E2Y(I)*Y3(I)+E2Z(I)*Z3(I)                 
        ZL=E3X(I)*X3(I)+E3Y(I)*Y3(I)+E3Z(I)*Z3(I)                 
        X3(I)=XL                                                  
        Y3(I)=YL                                                  
        Z3(I)=ZL                                                  
        XL=E1X(I)*X4(I)+E1Y(I)*Y4(I)+E1Z(I)*Z4(I)                 
        YL=E2X(I)*X4(I)+E2Y(I)*Y4(I)+E2Z(I)*Z4(I)                 
        ZL=E3X(I)*X4(I)+E3Y(I)*Y4(I)+E3Z(I)*Z4(I)                 
        X4(I)=XL                                                  
        Y4(I)=YL                                                  
        Z4(I)=ZL                                                  
        XL=E1X(I)*X5(I)+E1Y(I)*Y5(I)+E1Z(I)*Z5(I)                 
        YL=E2X(I)*X5(I)+E2Y(I)*Y5(I)+E2Z(I)*Z5(I)                 
        ZL=E3X(I)*X5(I)+E3Y(I)*Y5(I)+E3Z(I)*Z5(I)                 
        X5(I)=XL                                                  
        Y5(I)=YL                                                  
        Z5(I)=ZL                                                  
        XL=E1X(I)*X6(I)+E1Y(I)*Y6(I)+E1Z(I)*Z6(I)                 
        YL=E2X(I)*X6(I)+E2Y(I)*Y6(I)+E2Z(I)*Z6(I)                 
        ZL=E3X(I)*X6(I)+E3Y(I)*Y6(I)+E3Z(I)*Z6(I)                 
        X6(I)=XL                                                  
        Y6(I)=YL                                                  
        Z6(I)=ZL                                                  
        XL=E1X(I)*X7(I)+E1Y(I)*Y7(I)+E1Z(I)*Z7(I)                 
        YL=E2X(I)*X7(I)+E2Y(I)*Y7(I)+E2Z(I)*Z7(I)                 
        ZL=E3X(I)*X7(I)+E3Y(I)*Y7(I)+E3Z(I)*Z7(I)                 
        X7(I)=XL                                                  
        Y7(I)=YL                                                  
        Z7(I)=ZL                                                  
        XL=E1X(I)*X8(I)+E1Y(I)*Y8(I)+E1Z(I)*Z8(I)                 
        YL=E2X(I)*X8(I)+E2Y(I)*Y8(I)+E2Z(I)*Z8(I)                 
        ZL=E3X(I)*X8(I)+E3Y(I)*Y8(I)+E3Z(I)*Z8(I)                 
        X8(I)=XL                                                  
        Y8(I)=YL                                                  
        Z8(I)=ZL                                                  
      ENDDO                                                       
C-----------
      RETURN
      END
