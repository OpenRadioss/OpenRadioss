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
Chd|  SCOOR3                        source/elements/solid/solide/scoor3.F
Chd|-- called by -----------
Chd|        INIRIG_MAT                    source/elements/initia/inirig_mat.F
Chd|        INISOLDIST                    source/initial_conditions/inivol/inisoldist.F
Chd|        INIVOID                       source/elements/initia/inivoid.F
Chd|        MULTIFLUID_INIT3              source/multifluid/multifluid_init3.F
Chd|        S8CINIT3                      source/elements/thickshell/solide8c/s8cinit3.F
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|        SUINIT3                       source/elements/elbuf_init/suinit3.F
Chd|-- calls ---------------
Chd|        MOD_CLOSE                     source/elements/solid/solide/mod_close.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SCOOR3(
     .         X    ,XREFS,IXS  ,GEO  ,MXT  ,NGEO ,NGL  ,
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
      INTEGER IXS(NIXS,*), MXT(*), NGL(*), NGEO(*),
     .   IX1(*),IX2(*),IX3(*),IX4(*),IX5(*),IX6(*),IX7(*),IX8(*)
      my_real
     .   X(3,*),GEO(*),TEMP0(MVSIZ),
     .   TEMP(*),XREFS(8,3,*)
      my_real, DIMENSION(MVSIZ), INTENT(OUT) ::
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   RX ,RY ,RZ ,SX ,SY ,SZ ,TX ,TY ,TZ,
     .   E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,
     .   F1X,F1Y,F1Z,F2X,F2Y,F2Z
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
      my_real
     .   SUMA
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
c
C     Repere isoparametrique
C
      DO I=LFT,LLT
        F1X(I) = X2(I)+X3(I)-X1(I)-X4(I)
        F1Y(I) = Y2(I)+Y3(I)-Y1(I)-Y4(I)
        F1Z(I) = Z2(I)+Z3(I)-Z1(I)-Z4(I)
        F2X(I) = X3(I)+X4(I)-X1(I)-X2(I)
        F2Y(I) = Y3(I)+Y4(I)-Y1(I)-Y2(I)
        F2Z(I) = Z3(I)+Z4(I)-Z1(I)-Z2(I)
        RX(I)  = F2X(I)+X7(I)+X8(I)-X5(I)-X6(I)
        RY(I)  = F2Y(I)+Y7(I)+Y8(I)-Y5(I)-Y6(I)
        RZ(I)  = F2Z(I)+Z7(I)+Z8(I)-Z5(I)-Z6(I)
        TX(I)  = F1X(I)+X6(I)+X7(I)-X5(I)-X8(I)
        TY(I)  = F1Y(I)+Y6(I)+Y7(I)-Y5(I)-Y8(I)
        TZ(I)  = F1Z(I)+Z6(I)+Z7(I)-Z5(I)-Z8(I)
        SX(I)  = X5(I)+X6(I)+X7(I)+X8(I)-X1(I)-X2(I)-X3(I)-X4(I)
        SY(I)  = Y5(I)+Y6(I)+Y7(I)+Y8(I)-Y1(I)-Y2(I)-Y3(I)-Y4(I)
        SZ(I)  = Z5(I)+Z6(I)+Z7(I)+Z8(I)-Z1(I)-Z2(I)-Z3(I)-Z4(I)
      ENDDO
C     Orthogonalisation du repere
C       E1=R, E3=R^S, E2=E3^E1                            
C
      DO I=LFT,LLT
        SUMA = SQRT(RX(I)**2+RY(I)**2+RZ(I)**2)
        IF (SUMA > ZERO) SUMA=ONE/SUMA
        E1X(I) = RX(I)*SUMA
        E1Y(I) = RY(I)*SUMA
        E1Z(I) = RZ(I)*SUMA
        E3X(I) = E1Y(I)*SZ(I) - E1Z(I)*SY(I)
        E3Y(I) = E1Z(I)*SX(I) - E1X(I)*SZ(I)
        E3Z(I) = E1X(I)*SY(I) - E1Y(I)*SX(I)
        SUMA = SQRT(E3X(I)**2+E3Y(I)**2+E3Z(I)**2)
        IF (SUMA > ZERO) SUMA=ONE/SUMA
        E3X(I) = E3X(I)*SUMA
        E3Y(I) = E3Y(I)*SUMA
        E3Z(I) = E3Z(I)*SUMA
        E2X(I) = E3Y(I)*E1Z(I) - E3Z(I)*E1Y(I)
        E2Y(I) = E3Z(I)*E1X(I) - E3X(I)*E1Z(I)
        E2Z(I) = E3X(I)*E1Y(I) - E3Y(I)*E1X(I)
        SUMA = SQRT(E2X(I)**2+E2Y(I)**2+E2Z(I)**2)
        IF (SUMA > ZERO) SUMA=ONE/SUMA
        E2X(I) = E2X(I)*SUMA
        E2Y(I) = E2Y(I)*SUMA
        E2Z(I) = E2Z(I)*SUMA
      ENDDO
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
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  SGSAVINI                      source/elements/solid/solide/scoor3.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SGSAVINI(NPE,X,IXS,SAV,NEL)
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPE,NEL
      INTEGER IXS(NIXS,*)
      my_real
     .  X(3,*)
      DOUBLE PRECISION
     .  SAV(NEL,3*(NPE-1))
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NPE1,N
      INTEGER NC(MVSIZ,NPE)
C     REAL
c      my_real
      DOUBLE PRECISION
     .   XL(MVSIZ),YL(MVSIZ),ZL(MVSIZ)
C-----------------------------------------------
C
      NPE1=NPE-1
      IF (NPE==4) THEN
       DO I=LFT,LLT
        NC(I,1)=IXS(2,I)
        NC(I,2)=IXS(4,I)
        NC(I,3)=IXS(7,I)
        NC(I,4)=IXS(6,I)
       ENDDO
      ELSE
       DO N=1,NPE
        DO I=LFT,LLT
         NC(I,N)=IXS(N+1,I)
        ENDDO
       ENDDO
      ENDIF
C
       DO I=LFT,LLT
        XL(I)=X(1,NC(I,NPE))
        YL(I)=X(2,NC(I,NPE))
        ZL(I)=X(3,NC(I,NPE))
       ENDDO
       DO N=1,NPE1
        DO I=LFT,LLT
         SAV(I,N)       = X(1,NC(I,N))-XL(I)
         SAV(I,N+NPE1)  = X(2,NC(I,N))-YL(I)
         SAV(I,N+2*NPE1)= X(3,NC(I,N))-ZL(I)
        ENDDO
       ENDDO
C
      RETURN
      END
