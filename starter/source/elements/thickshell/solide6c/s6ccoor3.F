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
Chd|  S6CCOOR3                      source/elements/thickshell/solide6c/s6ccoor3.F
Chd|-- called by -----------
Chd|        S6CINIT3                      source/elements/thickshell/solide6c/s6cinit3.F
Chd|-- calls ---------------
Chd|        S6CORTHO3                     source/elements/thickshell/solide6c/s6cortho3.F
Chd|        CHECKVOLUME_6N                source/elements/solid/solide/checksvolume.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S6CCOOR3(X       ,IXS  ,GEO  ,NGL  ,MXT  ,NGEO ,                            
     .     RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .     R11  ,R21  ,R31  ,R12  ,R22  ,R32  ,R13  ,R23  ,R33  ,
     .     F1X  ,F1Y  ,F1Z  ,F2X  ,F2Y  ,F2Z  ,TEMP0, TEMP, 
     .     IX1, IX2, IX3, IX4, IX5, IX6, 
     .     X1, X2, X3, X4, X5, X6, 
     .     Y1, Y2, Y3, Y4, Y5, Y6,
     .     Z1, Z2, Z3, Z4, Z5, Z6)
      USE MESSAGE_MOD
C
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
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),MXT(*),NGL(*),NGEO(*)
      my_real
     .   X(3,*),GEO(*),
     .   RX(*) ,RY(*) ,RZ(*) ,SX(*) ,SY(*) ,SZ(*) ,TX(*) ,TY(*) ,TZ(*),
     .   R11(*),R12(*),R13(*),R21(*),R22(*),R23(*),R31(*),R32(*),R33(*),
     .   F1X(*),F1Y(*),F1Z(*),F2X(*),F2Y(*),F2Z(*),TEMP0(MVSIZ), TEMP(*)
      INTEGER IX1(*), IX2(*), IX3(*), IX4(*), IX5(*), IX6(*)
      my_real 
     .     X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), 
     .     Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), 
     .     Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
      my_real
     .   XL,YL,ZL,O6TH
C-----------------------------------------------
C   E x t e r n a l  F u n c t i o n s
C-----------------------------------------------
      my_real
     .   CHECKVOLUME_6N
C=======================================================================
C     CONNECTIVITES ET NUMERO DE MATERIAU ET PID
C--------------------------------------------------
      O6TH = UNSIX
      DO I=LFT,LLT
        MXT(I)=IXS(1,I)
        IX1(I)=IXS(2,I)
        IX2(I)=IXS(3,I)
        IX3(I)=IXS(4,I)
        IX4(I)=IXS(6,I)
        IX5(I)=IXS(7,I)
        IX6(I)=IXS(8,I)
        NGEO(I)=IXS(NIXS-1,I)
        NGL(I)=IXS(NIXS,I)
        IF (CHECKVOLUME_6N(X ,IXS(1,I)) < ZERO) THEN
C         renumber connectivity
          IX1(I)=IXS(6,I)
          IX2(I)=IXS(7,I)
          IX3(I)=IXS(8,I)
          IX4(I)=IXS(2,I)
          IX5(I)=IXS(3,I)
          IX6(I)=IXS(4,I)
          IXS(2,I)=IX1(I)
          IXS(3,I)=IX2(I)
          IXS(4,I)=IX3(I)
          IXS(6,I)=IX4(I)
          IXS(7,I)=IX5(I)
          IXS(8,I)=IX6(I)
        ENDIF    
      ENDDO
C
C----------------------------
C     COORDONNEES
C----------------------------
      DO 20 I=LFT,LLT
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
   20 CONTINUE
C
      DO I=LFT,LLT
        F1X(I) = X2(I) - X1(I)
        F1Y(I) = Y2(I) - Y1(I)
        F1Z(I) = Z2(I) - Z1(I)
        F2X(I) = X3(I) - X1(I)
        F2Y(I) = Y3(I) - Y1(I)
        F2Z(I) = Z3(I) - Z1(I)
      ENDDO
C
      DO I=LFT,LLT
        XL=O6TH*(X1(I)+X2(I)+X3(I)+X4(I)+X5(I)+X6(I))
        YL=O6TH*(Y1(I)+Y2(I)+Y3(I)+Y4(I)+Y5(I)+Y6(I))
        ZL=O6TH*(Z1(I)+Z2(I)+Z3(I)+Z4(I)+Z5(I)+Z6(I))
        X1(I)=X1(I)-XL
        Y1(I)=Y1(I)-YL
        Z1(I)=Z1(I)-ZL
        X2(I)=X2(I)-XL
        Y2(I)=Y2(I)-YL
        Z2(I)=Z2(I)-ZL
        X3(I)=X3(I)-XL
        Y3(I)=Y3(I)-YL
        Z3(I)=Z3(I)-ZL
        X4(I)=X4(I)-XL
        Y4(I)=Y4(I)-YL
        Z4(I)=Z4(I)-ZL
        X5(I)=X5(I)-XL
        Y5(I)=Y5(I)-YL
        Z5(I)=Z5(I)-ZL
        X6(I)=X6(I)-XL
        Y6(I)=Y6(I)-YL
        Z6(I)=Z6(I)-ZL
      ENDDO
C-----------
C     REPERE CONVECTE .
C-----------
      CALL S6CORTHO3(                                    
     .  X1, X2, X3, X4, X5, X6,                          
     .  Y1, Y2, Y3, Y4, Y5, Y6,                          
     .  Z1, Z2, Z3, Z4, Z5, Z6,                          
     .  RX  ,RY  ,RZ  ,SX  ,SY  ,SZ  ,TX  ,TY  ,TZ  ,    
     .  R11 ,R21 ,R31 ,R12 ,R22 ,R32 ,R13, R23,  R33)    
C
       DO I=LFT,LLT
        XL=R11(I)*X1(I)+R21(I)*Y1(I)+R31(I)*Z1(I)
        YL=R12(I)*X1(I)+R22(I)*Y1(I)+R32(I)*Z1(I)
        ZL=R13(I)*X1(I)+R23(I)*Y1(I)+R33(I)*Z1(I)
        X1(I)=XL
        Y1(I)=YL
        Z1(I)=ZL
        XL=R11(I)*X2(I)+R21(I)*Y2(I)+R31(I)*Z2(I)
        YL=R12(I)*X2(I)+R22(I)*Y2(I)+R32(I)*Z2(I)
        ZL=R13(I)*X2(I)+R23(I)*Y2(I)+R33(I)*Z2(I)
        X2(I)=XL
        Y2(I)=YL
        Z2(I)=ZL
        XL=R11(I)*X3(I)+R21(I)*Y3(I)+R31(I)*Z3(I)
        YL=R12(I)*X3(I)+R22(I)*Y3(I)+R32(I)*Z3(I)
        ZL=R13(I)*X3(I)+R23(I)*Y3(I)+R33(I)*Z3(I)
        X3(I)=XL
        Y3(I)=YL
        Z3(I)=ZL
        XL=R11(I)*X4(I)+R21(I)*Y4(I)+R31(I)*Z4(I)
        YL=R12(I)*X4(I)+R22(I)*Y4(I)+R32(I)*Z4(I)
        X4(I)=XL
        Y4(I)=YL
        Z4(I)=-Z1(I)
        XL=R11(I)*X5(I)+R21(I)*Y5(I)+R31(I)*Z5(I)
        YL=R12(I)*X5(I)+R22(I)*Y5(I)+R32(I)*Z5(I)
        X5(I)=XL
        Y5(I)=YL
        Z5(I)=-Z2(I)
        XL=R11(I)*X6(I)+R21(I)*Y6(I)+R31(I)*Z6(I)
        YL=R12(I)*X6(I)+R22(I)*Y6(I)+R32(I)*Z6(I)
        X6(I)=XL
        Y6(I)=YL
        Z6(I)=-Z3(I)
       ENDDO
C
      IF(JTHE < 0 ) THEN                                      
        IF(NINTEMP > 0 ) THEN                                 
         DO I= LFT,LLT                                        
           IF(TEMP(IX1(I))== ZERO) TEMP(IX1(I)) = TEMP0(I)    
           IF(TEMP(IX2(I))== ZERO) TEMP(IX2(I)) = TEMP0(I)    
           IF(TEMP(IX3(I))== ZERO) TEMP(IX3(I)) = TEMP0(I)    
           IF(TEMP(IX4(I))== ZERO) TEMP(IX4(I)) = TEMP0(I)    
           IF(TEMP(IX5(I))== ZERO) TEMP(IX5(I)) = TEMP0(I)    
           IF(TEMP(IX6(I))== ZERO) TEMP(IX6(I)) = TEMP0(I)    
         ENDDO                                                
        ELSE                                                  
         DO I=LFT,LLT                                         
           TEMP(IX1(I))=TEMP0(I)                              
           TEMP(IX2(I))=TEMP0(I)                              
           TEMP(IX3(I))=TEMP0(I)                              
           TEMP(IX4(I))=TEMP0(I)                              
           TEMP(IX5(I))=TEMP0(I)                              
           TEMP(IX6(I))=TEMP0(I)                              
         ENDDO                                                
        ENDIF                                                 
      ENDIF                                                   
C-----------
      RETURN
      END
