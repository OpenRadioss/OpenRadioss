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
Chd|  SRCOOR3                       source/elements/solid/solide/srcoor3.F
Chd|-- called by -----------
Chd|        INISOLDIST                    source/initial_conditions/inivol/inisoldist.F
Chd|        MULTIFLUID_INIT3              source/multifluid/multifluid_init3.F
Chd|        S8CINIT3                      source/elements/thickshell/solide8c/s8cinit3.F
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|        SUINIT3                       source/elements/elbuf_init/suinit3.F
Chd|-- calls ---------------
Chd|        MOD_CLOSE                     source/elements/solid/solide/mod_close.F
Chd|        SORTHO3                       source/elements/solid/solide/sortho3.F
Chd|        SREPISO3                      source/elements/solid/solide/srepiso3.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SRCOOR3(
     .         X    ,XREFS,IXS  ,GEO  ,MXT  ,NGEO ,NGL  ,JHBE  ,
     .         IX1  ,IX2  ,IX3  ,IX4  ,IX5  ,IX6  ,IX7  ,IX8 ,
     .         X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8  ,   
     .         Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8  ,   
     .         Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8  ,   
     .         RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .         E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .         F1X  ,F1Y  ,F1Z  ,F2X  ,F2Y  ,F2Z  ,TEMP0, TEMP,
     .         XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .         YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .         ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   )
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
#include      "vect01_c.inc"
#include      "scr03_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),MXT(*),NGL(*),NGEO(*),JHBE,
     .   IX1(*),IX2(*),IX3(*),IX4(*),IX5(*),IX6(*),IX7(*),IX8(*)        
C     REAL
      my_real
     .   X(3,*),GEO(*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*), 
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),
     .   E1X(*),E2X(*),E3X(*),E1Y(*),E2Y(*),E3Y(*),E1Z(*),E2Z(*),E3Z(*),
     .   RX(*) ,RY(*) ,RZ(*) ,SX(*) ,SY(*) ,SZ(*) ,TX(*) ,TY(*) ,TZ(*),
     .   F1X(*),F1Y(*),F1Z(*),F2X(*),F2Y(*),F2Z(*),TEMP0(MVSIZ),
     .   TEMP(*),XREFS(8,3,*)
      DOUBLE PRECISION 
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ)     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      DOUBLE PRECISION 
     .   XL,YL,ZL
C-----------------------------------------------
C   E x t e r n a l  F u n c t i o n s
C-----------------------------------------------
      my_real
     .    CHECKVOLUME_8N
C=======================================================================
C     CONNECTIVITES ET NUMERO DE MATERIAU ET PID
C--------------------------------------------------
      DO I=LFT,LLT
        MXT(I)=IXS(1,I)
        IX1(I)=IXS(2,I)
        IX2(I)=IXS(3,I)
        IX3(I)=IXS(4,I)
        IX4(I)=IXS(5,I)
        IX5(I)=IXS(6,I)
        IX6(I)=IXS(7,I)
        IX7(I)=IXS(8,I)
        IX8(I)=IXS(9,I)
        NGEO(I)=IXS(NIXS-1,I)
        NGL(I)=IXS(NIXS,I)
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
C
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
      ELSE
        DO I=LFT,LLT
          IF (CHECKVOLUME_8N(X ,IXS(1,I)) < ZERO) THEN
            X1(I)=XREFS(5,1,I)
            Y1(I)=XREFS(5,2,I)
            Z1(I)=XREFS(5,3,I)
            X2(I)=XREFS(6,1,I)
            Y2(I)=XREFS(6,2,I)                    
            Z2(I)=XREFS(6,3,I)                    
            X3(I)=XREFS(7,1,I)                    
            Y3(I)=XREFS(7,2,I)                    
            Z3(I)=XREFS(7,3,I)                    
            X4(I)=XREFS(8,1,I)                    
            Y4(I)=XREFS(8,2,I)                    
            Z4(I)=XREFS(8,3,I)                    
            X5(I)=XREFS(1,1,I)                                
            Y5(I)=XREFS(1,2,I)
            Z5(I)=XREFS(1,3,I)
            X6(I)=XREFS(2,1,I)
            Y6(I)=XREFS(2,2,I)
            Z6(I)=XREFS(2,3,I)
            X7(I)=XREFS(3,1,I)
            Y7(I)=XREFS(3,2,I)
            Z7(I)=XREFS(3,3,I)
            X8(I)=XREFS(4,1,I)
            Y8(I)=XREFS(4,2,I)
            Z8(I)=XREFS(4,3,I)
            XREFS(1,1,I)=X1(I)
            XREFS(1,2,I)=Y1(I)
            XREFS(1,3,I)=Z1(I)
            XREFS(2,1,I)=X2(I)
            XREFS(2,2,I)=Y2(I)
            XREFS(2,3,I)=Z2(I)
            XREFS(3,1,I)=X3(I)
            XREFS(3,2,I)=Y3(I)
            XREFS(3,3,I)=Z3(I)
            XREFS(4,1,I)=X4(I)
            XREFS(4,2,I)=Y4(I)
            XREFS(4,3,I)=Z4(I)
            XREFS(5,1,I)=X5(I)
            XREFS(5,2,I)=Y5(I)
            XREFS(5,3,I)=Z5(I)
            XREFS(6,1,I)=X6(I)
            XREFS(6,2,I)=Y6(I)
            XREFS(6,3,I)=Z6(I)
            XREFS(7,1,I)=X7(I)
            XREFS(7,2,I)=Y7(I)
            XREFS(7,3,I)=Z7(I)
            XREFS(8,1,I)=X8(I)
            XREFS(8,2,I)=Y8(I)
            XREFS(8,3,I)=Z8(I)
          ELSE
            X1(I)=XREFS(1,1,I)
            Y1(I)=XREFS(1,2,I)
            Z1(I)=XREFS(1,3,I)
            X2(I)=XREFS(2,1,I)
            Y2(I)=XREFS(2,2,I)
            Z2(I)=XREFS(2,3,I)
            X3(I)=XREFS(3,1,I)
            Y3(I)=XREFS(3,2,I)
            Z3(I)=XREFS(3,3,I)
            X4(I)=XREFS(4,1,I)
            Y4(I)=XREFS(4,2,I)
            Z4(I)=XREFS(4,3,I)
            X5(I)=XREFS(5,1,I)
            Y5(I)=XREFS(5,2,I)
            Z5(I)=XREFS(5,3,I)
            X6(I)=XREFS(6,1,I)
            Y6(I)=XREFS(6,2,I)
            Z6(I)=XREFS(6,3,I)
            X7(I)=XREFS(7,1,I)
            Y7(I)=XREFS(7,2,I)
            Z7(I)=XREFS(7,3,I)
            X8(I)=XREFS(8,1,I)
            Y8(I)=XREFS(8,2,I)
            Z8(I)=XREFS(8,3,I)
          ENDIF
        ENDDO
      ENDIF
C      
        DO I=LFT,LLT
          XD1(I) = X1(I)
          YD1(I) = Y1(I)
          ZD1(I) = Z1(I)
          XD2(I) = X2(I)
          YD2(I) = Y2(I)
          ZD2(I) = Z2(I)
          XD3(I) = X3(I)
          YD3(I) = Y3(I)
          ZD3(I) = Z3(I)
          XD4(I) = X4(I)
          YD4(I) = Y4(I)
          ZD4(I) = Z4(I)
          XD5(I) = X5(I)
          YD5(I) = Y5(I)
          ZD5(I) = Z5(I)
          XD6(I) = X6(I)
          YD6(I) = Y6(I)
          ZD6(I) = Z6(I)
          XD7(I) = X7(I)
          YD7(I) = Y7(I)
          ZD7(I) = Z7(I)
          XD8(I) = X8(I)
          YD8(I) = Y8(I)
          ZD8(I) = Z8(I)
        ENDDO
C
       IF(JCLOS/=0) CALL MOD_CLOSE(GEO,NGEO,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8)
C
C     Repere isoparametrique conforme manuel pour les briques
      CALL SREPISO3(
     .       X1, X2, X3, X4, X5, X6, X7, X8,
     .       Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .       Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .       RX, RY, RZ, SX, SY, SZ, TX, TY,
     .       TZ ,F1X  ,F1Y  ,F1Z  ,F2X  ,F2Y  ,F2Z   )
C-----------
C     REPERE CONVECTE
C-----------
      IF (JHBE == 14 .OR. JHBE == 24) THEN
C       HA8 / HEPH
        CALL SORTHO3(
     .      RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,   
     .      E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,E1X  ,E1Y  ,E1Z  )
      ELSE
        CALL SORTHO3(
     .      RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,   
     .      E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  )
      END IF 
C-----------
C   PASSAGE to Local_SYS in DP, add case JHBE==1(ISORTH==0) but Xj remain in Global_sys as before
C-----------
      IF (JHBE==14 .OR. JHBE==24 .OR. JHBE==17 .OR.(JHBE==1 .AND. ISORTH==0)) THEN
        DO I=LFT,LLT
          XL=E1X(I)*XD1(I)+E1Y(I)*YD1(I)+E1Z(I)*ZD1(I)
          YL=E2X(I)*XD1(I)+E2Y(I)*YD1(I)+E2Z(I)*ZD1(I)
          ZL=E3X(I)*XD1(I)+E3Y(I)*YD1(I)+E3Z(I)*ZD1(I)
          XD1(I)=XL
          YD1(I)=YL
          ZD1(I)=ZL
          XL=E1X(I)*XD2(I)+E1Y(I)*YD2(I)+E1Z(I)*ZD2(I)
          YL=E2X(I)*XD2(I)+E2Y(I)*YD2(I)+E2Z(I)*ZD2(I)
          ZL=E3X(I)*XD2(I)+E3Y(I)*YD2(I)+E3Z(I)*ZD2(I)
          XD2(I)=XL
          YD2(I)=YL
          ZD2(I)=ZL
          XL=E1X(I)*XD3(I)+E1Y(I)*YD3(I)+E1Z(I)*ZD3(I)
          YL=E2X(I)*XD3(I)+E2Y(I)*YD3(I)+E2Z(I)*ZD3(I)
          ZL=E3X(I)*XD3(I)+E3Y(I)*YD3(I)+E3Z(I)*ZD3(I)
          XD3(I)=XL
          YD3(I)=YL
          ZD3(I)=ZL
          XL=E1X(I)*XD4(I)+E1Y(I)*YD4(I)+E1Z(I)*ZD4(I)
          YL=E2X(I)*XD4(I)+E2Y(I)*YD4(I)+E2Z(I)*ZD4(I)
          ZL=E3X(I)*XD4(I)+E3Y(I)*YD4(I)+E3Z(I)*ZD4(I)
          XD4(I)=XL
          YD4(I)=YL
          ZD4(I)=ZL
          XL=E1X(I)*XD5(I)+E1Y(I)*YD5(I)+E1Z(I)*ZD5(I)
          YL=E2X(I)*XD5(I)+E2Y(I)*YD5(I)+E2Z(I)*ZD5(I)
          ZL=E3X(I)*XD5(I)+E3Y(I)*YD5(I)+E3Z(I)*ZD5(I)
          XD5(I)=XL
          YD5(I)=YL
          ZD5(I)=ZL
          XL=E1X(I)*XD6(I)+E1Y(I)*YD6(I)+E1Z(I)*ZD6(I)
          YL=E2X(I)*XD6(I)+E2Y(I)*YD6(I)+E2Z(I)*ZD6(I)
          ZL=E3X(I)*XD6(I)+E3Y(I)*YD6(I)+E3Z(I)*ZD6(I)
          XD6(I)=XL
          YD6(I)=YL
          ZD6(I)=ZL
          XL=E1X(I)*XD7(I)+E1Y(I)*YD7(I)+E1Z(I)*ZD7(I)
          YL=E2X(I)*XD7(I)+E2Y(I)*YD7(I)+E2Z(I)*ZD7(I)
          ZL=E3X(I)*XD7(I)+E3Y(I)*YD7(I)+E3Z(I)*ZD7(I)
          XD7(I)=XL
          YD7(I)=YL
          ZD7(I)=ZL
          XL=E1X(I)*XD8(I)+E1Y(I)*YD8(I)+E1Z(I)*ZD8(I)
          YL=E2X(I)*XD8(I)+E2Y(I)*YD8(I)+E2Z(I)*ZD8(I)
          ZL=E3X(I)*XD8(I)+E3Y(I)*YD8(I)+E3Z(I)*ZD8(I)
          XD8(I)=XL
          YD8(I)=YL
          ZD8(I)=ZL
        ENDDO
      END IF !(JHBE==14 .OR. JHBE==24 .OR. JHBE==17 .OR.(JHBE==1 .AND. ISORTH==0)) THEN
      IF (JHBE==14 .OR. JHBE==24 .OR. JHBE==17 ) THEN
        DO I=LFT,LLT
          X1(I) = XD1(I)  
          Y1(I) = YD1(I)  
          Z1(I) = ZD1(I)  
          X2(I) = XD2(I)  
          Y2(I) = YD2(I)  
          Z2(I) = ZD2(I)  
          X3(I) = XD3(I)  
          Y3(I) = YD3(I)  
          Z3(I) = ZD3(I)  
          X4(I) = XD4(I)  
          Y4(I) = YD4(I)  
          Z4(I) = ZD4(I)  
          X5(I) = XD5(I)  
          Y5(I) = YD5(I)  
          Z5(I) = ZD5(I)  
          X6(I) = XD6(I)  
          Y6(I) = YD6(I)  
          Z6(I) = ZD6(I)  
          X7(I) = XD7(I)  
          Y7(I) = YD7(I)  
          Z7(I) = ZD7(I)  
          X8(I) = XD8(I)  
          Y8(I) = YD8(I)  
          Z8(I) = ZD8(I)  
        ENDDO
      END IF !(JHBE==14 .OR. JHBE==24 .OR. JHBE==17 ) THEN
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
              IF(TEMP(IX7(I))== ZERO) TEMP(IX7(I)) = TEMP0(I)
              IF(TEMP(IX8(I))== ZERO) TEMP(IX8(I)) = TEMP0(I)
            ENDDO        
           ELSE
            DO I=LFT,LLT
              TEMP(IX1(I))=TEMP0(I)
              TEMP(IX2(I))=TEMP0(I)
              TEMP(IX3(I))=TEMP0(I)
              TEMP(IX4(I))=TEMP0(I)
              TEMP(IX5(I))=TEMP0(I)
              TEMP(IX6(I))=TEMP0(I)
              TEMP(IX7(I))=TEMP0(I)
              TEMP(IX8(I))=TEMP0(I)
            ENDDO              
           ENDIF
         ENDIF   
C-----
      RETURN
      END
