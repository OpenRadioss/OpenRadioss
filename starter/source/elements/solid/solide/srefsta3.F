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
Chd|  SREFSTA3                      source/elements/solid/solide/srefsta3.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        SDEFO3                        source/elements/solid/solide/sdefo3.F
Chd|        SDEFOT3                       source/elements/solid/solide/srefsta3.F
Chd|        SJAC_I                        source/elements/solid/solide/sderi3.F
Chd|        SORDEFT3                      source/elements/solid/solide/srefsta3.F
Chd|        SORTHO3                       source/elements/solid/solide/sortho3.F
Chd|        SREFDERI3                     source/elements/solid/solide/srefsta3.F
Chd|        SREPISO3                      source/elements/solid/solide/srepiso3.F
Chd|        SRHO3                         source/elements/solid/solide/srho3.F
Chd|        SRORTH3                       source/elements/solid/solide/srorth3.F
Chd|        SRROTA3                       source/elements/solid/solide/srrota3.F
Chd|        STORTH3                       source/elements/solid/solidez/szorth3.F
Chd|        SZORDEF3                      source/elements/solid/solidez/szorth3.F
Chd|        SZREFDERI3                    source/elements/solid/solide/srefsta3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SREFSTA3(ELBUF_STR,IXS     ,PM     ,GEO     ,IPARG  ,
     .                    IPM      ,IGEO    ,SKEW   ,X       ,XREFS  ,
     .                    NEL      ,IPARTS  ,IPART  ,BUFMAT  ,
     .                    NPF      ,TF      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr01_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
#include      "warn_c.inc"
#include      "titr_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IPARG(*),IPARTS(*), IGEO(*),
     .    IPM(NPROPMI,*),IPART(LIPART1,*), NEL, NPF(*)
C     REAL ou REAL*8
      my_real
     .   PM(NPROPM,*), X(3,*), XREFS(8,3,*), GEO(NPROPG,*),
     .   SKEW(LSKEW,*), BUFMAT(*), TF(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NF1, I,II(6), N, JHBE, IREP, IGTYP, NVAUX, ITRS, IBID,
     .        NITSAV,J,I1,I2,ID,MAT_ID
      INTEGER MAT(MVSIZ), PID(MVSIZ), NGL(MVSIZ),
     .   IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),
     .   IX5(MVSIZ),IX6(MVSIZ),IX7(MVSIZ),IX8(MVSIZ)
      my_real
     .   X1(MVSIZ),X2(MVSIZ),X3(MVSIZ),X4(MVSIZ),X5(MVSIZ),X6(MVSIZ),
     .   X7(MVSIZ),X8(MVSIZ),Y1(MVSIZ),Y2(MVSIZ),Y3(MVSIZ),Y4(MVSIZ),
     .   Y5(MVSIZ),Y6(MVSIZ),Y7(MVSIZ),Y8(MVSIZ),Z1(MVSIZ),Z2(MVSIZ),
     .   Z3(MVSIZ),Z4(MVSIZ),Z5(MVSIZ),Z6(MVSIZ),Z7(MVSIZ),Z8(MVSIZ),
     .   RX(MVSIZ) ,RY(MVSIZ) ,RZ(MVSIZ) ,VOLU(MVSIZ),
     .   SX(MVSIZ) ,SY(MVSIZ) ,SZ(MVSIZ) ,
     .   TX(MVSIZ) ,TY(MVSIZ) ,TZ(MVSIZ) ,
     .   F1X(MVSIZ) ,F1Y(MVSIZ) ,F1Z(MVSIZ) ,
     .   F2X(MVSIZ) ,F2Y(MVSIZ) ,F2Z(MVSIZ),
     .   E1X(MVSIZ),E1Y(MVSIZ),E1Z(MVSIZ),
     .   E2X(MVSIZ),E2Y(MVSIZ),E2Z(MVSIZ),
     .   E3X(MVSIZ),E3Y(MVSIZ),E3Z(MVSIZ),
     .   PX1(MVSIZ) ,PX2(MVSIZ) ,PX3(MVSIZ), PX4(MVSIZ),
     .   PY1(MVSIZ) ,PY2(MVSIZ) ,PY3(MVSIZ), PY4(MVSIZ),
     .   PZ1(MVSIZ) ,PZ2(MVSIZ) ,PZ3(MVSIZ), PZ4(MVSIZ),
     .   MFXX(MVSIZ), MFXY(MVSIZ), MFYX(MVSIZ),
     .   MFYY(MVSIZ), MFYZ(MVSIZ), MFZY(MVSIZ),
     .   MFZZ(MVSIZ), MFZX(MVSIZ), MFXZ(MVSIZ),
     .   VOLN(MVSIZ), DVOL(MVSIZ),
     .   XR(MVSIZ,8) ,YR(MVSIZ,8) ,ZR(MVSIZ,8) ,
     .   VXL(MVSIZ,8),VYL(MVSIZ,8),VZL(MVSIZ,8),
     .   VX1(MVSIZ),VX2(MVSIZ),VX3(MVSIZ),VX4(MVSIZ),
     .   VX5(MVSIZ),VX6(MVSIZ),VX7(MVSIZ),VX8(MVSIZ),
     .   VY1(MVSIZ),VY2(MVSIZ),VY3(MVSIZ),VY4(MVSIZ),
     .   VY5(MVSIZ),VY6(MVSIZ),VY7(MVSIZ),VY8(MVSIZ),
     .   VZ1(MVSIZ),VZ2(MVSIZ),VZ3(MVSIZ),VZ4(MVSIZ),
     .   VZ5(MVSIZ),VZ6(MVSIZ),VZ7(MVSIZ),VZ8(MVSIZ),
     .   DXX(MVSIZ),DXY(MVSIZ),DXZ(MVSIZ),
     .   DYX(MVSIZ),DYY(MVSIZ),DYZ(MVSIZ),
     .   DZX(MVSIZ),DZY(MVSIZ),DZZ(MVSIZ),
     .   D4(MVSIZ) ,D5(MVSIZ) ,D6(MVSIZ) ,
     .   S1(MVSIZ) , S2(MVSIZ), S3(MVSIZ),
     .   S4(MVSIZ) , S5(MVSIZ), S6(MVSIZ),
     .   WXX(MVSIZ), WYY(MVSIZ), WZZ(MVSIZ),
     .   G1X(MVSIZ),G2X(MVSIZ),G3X(MVSIZ),
     .   G1Y(MVSIZ),G2Y(MVSIZ),G3Y(MVSIZ),
     .   G1Z(MVSIZ),G2Z(MVSIZ),G3Z(MVSIZ),
     .   VBID(LVEUL,MVSIZ)
      my_real
     .   FAC, XT, YT, ZT
C-----
      TYPE(G_BUFEL_) ,POINTER :: GBUF 
      CHARACTER*nchartitle ,
     .     TITR
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================
      GBUF  => ELBUF_STR%GBUF
      NVAUX = IPARG(18)
      JHBE  = IPARG(23)
      IREP  = IPARG(35)
      JCVT  = IPARG(37)
      IGTYP = IPARG(38)
      NF1=NFT+1
C
      NITSAV=NITRS
C-----Case total strain, rather for computing Eint, stress will also be calculated at T=0 of Engine
C-----but NITRS=1 (before), for nonlinear elastic, Eint is too approximative
      IF (ISMSTR >= 10) NITRS=10
      IBID   = 0
      DO J=1,6
        II(J) = NEL*(J-1)
      ENDDO
C--------------------------------------------------
C REFERENCE METRICS
C--------------------------------------------------
      IF (NXREF > 0 .AND. JLAG/=0 .AND. JSPH==0)THEN

        IF(MTN /= 35 .AND.MTN /= 38 .AND. MTN /= 42 .AND.
     .     MTN /= 70 .AND. MTN /= 90)THEN
          NITRS=NITSAV
C         message moved to hm_read_xref
          RETURN
        END IF

        IF (JCVT <= 0 .OR. (JHBE/=1.AND.JHBE/=2.AND.JHBE/=24))THEN
          NITRS=NITSAV
C         message
          RETURN
        END IF

C--------------------------------------------------
C     CONNECTIVITES ET NUMERO DE MATERIAU ET PID
C--------------------------------------------------
        DO I=LFT,LLT
          N=NFT+I
          MAT(I)=IXS(1,N)
          IX1(I)=IXS(2,N)
          IX2(I)=IXS(3,N)
          IX3(I)=IXS(4,N)
          IX4(I)=IXS(5,N)
          IX5(I)=IXS(6,N)
          IX6(I)=IXS(7,N)
          IX7(I)=IXS(8,N)
          IX8(I)=IXS(9,N)
          PID(I)=IXS(NIXS-1,N)
          NGL(I)=IXS(NIXS,N)
        END DO
C----------------------------
C     COORDONNEES
C----------------------------
        DO I=LFT,LLT
          XT      = XREFS(8,1,NFT+I)
          YT      = XREFS(8,2,NFT+I)
          ZT      = XREFS(8,3,NFT+I)
          XR(I,1) = XREFS(1,1,NFT+I)-XT
          YR(I,1) = XREFS(1,2,NFT+I)-YT
          ZR(I,1) = XREFS(1,3,NFT+I)-ZT
          XR(I,2) = XREFS(2,1,NFT+I)-XT
          YR(I,2) = XREFS(2,2,NFT+I)-YT
          ZR(I,2) = XREFS(2,3,NFT+I)-ZT
          XR(I,3) = XREFS(3,1,NFT+I)-XT
          YR(I,3) = XREFS(3,2,NFT+I)-YT
          ZR(I,3) = XREFS(3,3,NFT+I)-ZT
          XR(I,4) = XREFS(4,1,NFT+I)-XT
          YR(I,4) = XREFS(4,2,NFT+I)-YT
          ZR(I,4) = XREFS(4,3,NFT+I)-ZT
          XR(I,5) = XREFS(5,1,NFT+I)-XT
          YR(I,5) = XREFS(5,2,NFT+I)-YT
          ZR(I,5) = XREFS(5,3,NFT+I)-ZT
          XR(I,6) = XREFS(6,1,NFT+I)-XT
          YR(I,6) = XREFS(6,2,NFT+I)-YT
          ZR(I,6) = XREFS(6,3,NFT+I)-ZT
          XR(I,7) = XREFS(7,1,NFT+I)-XT
          YR(I,7) = XREFS(7,2,NFT+I)-YT
          ZR(I,7) = XREFS(7,3,NFT+I)-ZT
          XR(I,8) = ZERO
          YR(I,8) = ZERO
          ZR(I,8) = ZERO
        END DO
C       Repere isoparametrique ,rep convecte, rep orthotrope
        CALL SREPISO3(
     .    XR(1,1) ,XR(1,2) ,XR(1,3) ,XR(1,4) ,
     .    XR(1,5) ,XR(1,6) ,XR(1,7) ,XR(1,8) ,
     .    YR(1,1) ,YR(1,2) ,YR(1,3) ,YR(1,4) ,
     .    YR(1,5) ,YR(1,6) ,YR(1,7) ,YR(1,8) ,
     .    ZR(1,1) ,ZR(1,2) ,ZR(1,3) ,ZR(1,4) ,
     .    ZR(1,5) ,ZR(1,6) ,ZR(1,7) ,ZR(1,8) ,
     .    RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,
     .    TZ   ,F1X  ,F1Y  ,F1Z  ,F2X  ,F2Y  ,F2Z  )

        IF (JHBE == 24) THEN
C         HEPH
          CALL SORTHO3(
     .      RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .      E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,E1X  ,E1Y  ,E1Z  )
        ELSE
          CALL SORTHO3(
     .      RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .      E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  )
        ENDIF

C---------useless to change XR,YR,ZR
C       Reference en coordonnees locales
c        CALL SRROTA3(E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
c     .               XR(1,1) ,XR(1,2) ,XR(1,3) ,XR(1,4) ,
c     .               XR(1,5) ,XR(1,6) ,XR(1,7) ,XR(1,8) ,
c     .               YR(1,1) ,YR(1,2) ,YR(1,3) ,YR(1,4) ,
c     .               YR(1,5) ,YR(1,6) ,YR(1,7) ,YR(1,8) ,
c     .               ZR(1,1) ,ZR(1,2) ,ZR(1,3) ,ZR(1,4) ,
c     .               ZR(1,5) ,ZR(1,6) ,ZR(1,7) ,ZR(1,8) )
c
C       IF (IGTYP == 6 .OR. IGTYP == 21)THEN
        IF (IGTYP == 6)THEN
          IF(JHBE /=24)THEN
            CALL SRORTH3(JHBE ,GBUF%GAMA ,
     .         E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .         XR(1,1) ,XR(1,2) ,XR(1,3) ,XR(1,4) ,
     .         XR(1,5) ,XR(1,6) ,XR(1,7) ,XR(1,8) ,
     .         YR(1,1) ,YR(1,2) ,YR(1,3) ,YR(1,4) ,
     .         YR(1,5) ,YR(1,6) ,YR(1,7) ,YR(1,8) ,
     .         ZR(1,1) ,ZR(1,2) ,ZR(1,3) ,ZR(1,4) ,
     .         ZR(1,5) ,ZR(1,6) ,ZR(1,7) ,ZR(1,8) ,NEL)
          ELSE
          END IF
        END IF

        DO I=LFT,LLT
          XT=X(1,IX8(I))
          YT=X(2,IX8(I))
          ZT=X(3,IX8(I))
          X1(I)=X(1,IX1(I))-XT
          Y1(I)=X(2,IX1(I))-YT
          Z1(I)=X(3,IX1(I))-ZT
          X2(I)=X(1,IX2(I))-XT
          Y2(I)=X(2,IX2(I))-YT
          Z2(I)=X(3,IX2(I))-ZT
          X3(I)=X(1,IX3(I))-XT
          Y3(I)=X(2,IX3(I))-YT
          Z3(I)=X(3,IX3(I))-ZT
          X4(I)=X(1,IX4(I))-XT
          Y4(I)=X(2,IX4(I))-YT
          Z4(I)=X(3,IX4(I))-ZT
          X5(I)=X(1,IX5(I))-XT
          Y5(I)=X(2,IX5(I))-YT
          Z5(I)=X(3,IX5(I))-ZT
          X6(I)=X(1,IX6(I))-XT
          Y6(I)=X(2,IX6(I))-YT
          Z6(I)=X(3,IX6(I))-ZT
          X7(I)=X(1,IX7(I))-XT
          Y7(I)=X(2,IX7(I))-YT
          Z7(I)=X(3,IX7(I))-ZT
          X8(I)=ZERO
          Y8(I)=ZERO
          Z8(I)=ZERO
        END DO
C
C       Repere isoparametrique ,rep convecte, rep orthotrope
        CALL SREPISO3(
     .    X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .    Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .    Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .    RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,
     .    TZ   ,F1X  ,F1Y  ,F1Z  ,F2X  ,F2Y  ,F2Z  )

        IF (JHBE == 24) THEN
C         HEPH
          CALL SORTHO3(
     .      RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .      E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,E1X  ,E1Y  ,E1Z  )
        ELSE
          CALL SORTHO3(
     .      RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .      E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  )
        ENDIF

        CALL SRROTA3(E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .       X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .       Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .       Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   )

C       IF (IGTYP == 6 .OR. IGTYP == 21)THEN
        IF (IGTYP == 6)THEN
          IF(JHBE /=24)THEN
            CALL SRORTH3(JHBE ,GBUF%GAMA ,
     .         E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y  ,E1Z  ,E2Z  ,E3Z ,
     .         X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .         Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .         Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,NEL)
          ELSE
          END IF
        END IF
C
C       Retourne la reference en coordonnees globales.
c        CALL SRROTA3(E1X ,E1Y ,E1Z ,E2X ,E2Y ,E2Z ,E3X ,E3Y ,E3Z ,
c     .       XR(1,1)   ,XR(1,2)   ,XR(1,3)   ,XR(1,4)   ,
c     .       XR(1,5)   ,XR(1,6)   ,XR(1,7)   ,XR(1,8)   ,
c     .       YR(1,1)   ,YR(1,2)   ,YR(1,3)   ,YR(1,4)   ,
c     .       YR(1,5)   ,YR(1,6)   ,YR(1,7)   ,YR(1,8)   ,
c     .       ZR(1,1)   ,ZR(1,2)   ,ZR(1,3)   ,ZR(1,4)   ,
c     .       ZR(1,5)   ,ZR(1,6)   ,ZR(1,7)   ,ZR(1,8)   )
C
        DO I=LFT,LLT
          XT      = XREFS(8,1,NFT+I)
          YT      = XREFS(8,2,NFT+I)
          ZT      = XREFS(8,3,NFT+I)
          XR(I,1) = XREFS(1,1,NFT+I)-XT
          YR(I,1) = XREFS(1,2,NFT+I)-YT
          ZR(I,1) = XREFS(1,3,NFT+I)-ZT
          XR(I,2) = XREFS(2,1,NFT+I)-XT
          YR(I,2) = XREFS(2,2,NFT+I)-YT
          ZR(I,2) = XREFS(2,3,NFT+I)-ZT
          XR(I,3) = XREFS(3,1,NFT+I)-XT
          YR(I,3) = XREFS(3,2,NFT+I)-YT
          ZR(I,3) = XREFS(3,3,NFT+I)-ZT
          XR(I,4) = XREFS(4,1,NFT+I)-XT
          YR(I,4) = XREFS(4,2,NFT+I)-YT
          ZR(I,4) = XREFS(4,3,NFT+I)-ZT
          XR(I,5) = XREFS(5,1,NFT+I)-XT
          YR(I,5) = XREFS(5,2,NFT+I)-YT
          ZR(I,5) = XREFS(5,3,NFT+I)-ZT
          XR(I,6) = XREFS(6,1,NFT+I)-XT
          YR(I,6) = XREFS(6,2,NFT+I)-YT
          ZR(I,6) = XREFS(6,3,NFT+I)-ZT
          XR(I,7) = XREFS(7,1,NFT+I)-XT
          YR(I,7) = XREFS(7,2,NFT+I)-YT
          ZR(I,7) = XREFS(7,3,NFT+I)-ZT
          XR(I,8) = ZERO
          YR(I,8) = ZERO
          ZR(I,8) = ZERO
        END DO
        FAC=UN/FLOAT(NITRS)
        DO I=LFT,LLT
          XT=X(1,IX8(I))
          YT=X(2,IX8(I))
          ZT=X(3,IX8(I))
          VX1(I)=(X(1,IX1(I))-XT-XR(I,1))*FAC
          VY1(I)=(X(2,IX1(I))-YT-YR(I,1))*FAC
          VZ1(I)=(X(3,IX1(I))-ZT-ZR(I,1))*FAC
          VX2(I)=(X(1,IX2(I))-XT-XR(I,2))*FAC
          VY2(I)=(X(2,IX2(I))-YT-YR(I,2))*FAC
          VZ2(I)=(X(3,IX2(I))-ZT-ZR(I,2))*FAC
          VX3(I)=(X(1,IX3(I))-XT-XR(I,3))*FAC
          VY3(I)=(X(2,IX3(I))-YT-YR(I,3))*FAC
          VZ3(I)=(X(3,IX3(I))-ZT-ZR(I,3))*FAC
          VX4(I)=(X(1,IX4(I))-XT-XR(I,4))*FAC
          VY4(I)=(X(2,IX4(I))-YT-YR(I,4))*FAC
          VZ4(I)=(X(3,IX4(I))-ZT-ZR(I,4))*FAC
          VX5(I)=(X(1,IX5(I))-XT-XR(I,5))*FAC
          VY5(I)=(X(2,IX5(I))-YT-YR(I,5))*FAC
          VZ5(I)=(X(3,IX5(I))-ZT-ZR(I,5))*FAC
          VX6(I)=(X(1,IX6(I))-XT-XR(I,6))*FAC
          VY6(I)=(X(2,IX6(I))-YT-YR(I,6))*FAC
          VZ6(I)=(X(3,IX6(I))-ZT-ZR(I,6))*FAC
          VX7(I)=(X(1,IX7(I))-XT-XR(I,7))*FAC
          VY7(I)=(X(2,IX7(I))-YT-YR(I,7))*FAC
          VZ7(I)=(X(3,IX7(I))-ZT-ZR(I,7))*FAC
          VX8(I)=ZERO
          VY8(I)=ZERO
          VZ8(I)=ZERO
        END DO
C
        DO ITRS=1,NITRS

          FAC=FLOAT(ITRS)
C-----Case total strain first in global sys
         IF (ISMSTR >= 10 ) THEN
C------should be global system
          IF (ISMSTR == 10.OR.ISMSTR == 12) THEN
            CALL SJAC_I(
     .              XR(1,1),XR(1,2),XR(1,3),XR(1,4),XR(1,5),XR(1,6),XR(1,7),XR(1,8),
     .              YR(1,1),YR(1,2),YR(1,3),YR(1,4),YR(1,5),YR(1,6),YR(1,7),YR(1,8),
     .              ZR(1,1),ZR(1,2),ZR(1,3),ZR(1,4),ZR(1,5),ZR(1,6),ZR(1,7),ZR(1,8),
     .              GBUF%JAC_I)
          END IF
          IF (JHBE == 24) THEN
           CALL SZREFDERI3(
     .        VOLN ,VBID ,GEO  ,IGEO ,
     .        XR(1,1)   ,XR(1,2)   ,XR(1,3)   ,XR(1,4)   ,
     .        XR(1,5)   ,XR(1,6)   ,XR(1,7)   ,XR(1,8)   ,
     .        YR(1,1)   ,YR(1,2)   ,YR(1,3)   ,YR(1,4)   ,
     .        YR(1,5)   ,YR(1,6)   ,YR(1,7)   ,YR(1,8)   ,
     .        ZR(1,1)   ,ZR(1,2)   ,ZR(1,3)   ,ZR(1,4)   ,
     .        ZR(1,5)   ,ZR(1,6)   ,ZR(1,7)   ,ZR(1,8)   ,
     .        PX1  ,PX2  ,PX3  ,PX4  ,
     .        PY1  ,PY2  ,PY3  ,PY4  ,
     .        PZ1  ,PZ2  ,PZ3  ,PZ4  ,
     .        RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TZ   ,
     .        NGL  ,PID  ,VOLU )
          ELSE
           CALL SREFDERI3(
     .        VOLN ,VBID ,GEO  ,IGEO ,
     .        XR(1,1)   ,XR(1,2)   ,XR(1,3)   ,XR(1,4)   ,
     .        XR(1,5)   ,XR(1,6)   ,XR(1,7)   ,XR(1,8)   ,
     .        YR(1,1)   ,YR(1,2)   ,YR(1,3)   ,YR(1,4)   ,
     .        YR(1,5)   ,YR(1,6)   ,YR(1,7)   ,YR(1,8)   ,
     .        ZR(1,1)   ,ZR(1,2)   ,ZR(1,3)   ,ZR(1,4)   ,
     .        ZR(1,5)   ,ZR(1,6)   ,ZR(1,7)   ,ZR(1,8)   ,
     .        RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,NGL  ,PID  ,
     .        PX1  ,PX2  ,PX3  ,PX4  ,PY1  ,PY2  ,PY3  ,PY4  ,
     .        PZ1  ,PZ2  ,PZ3  ,PZ4, VOLU)
          ENDIF
C         
          CALL SDEFOT3(
     .     PX1, PX2, PX3, PX4,
     .     PY1, PY2, PY3, PY4,
     .     PZ1, PZ2, PZ3, PZ4,
     .     VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8, 
     .     VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8, 
     .     VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8, 
     .     MFXX, MFXY, MFXZ, MFYX, MFYY, MFYZ, MFZX, MFZY, MFZZ)
          DO I=LFT,LLT
            MFXX(I)=FAC*MFXX(I)
            MFYY(I)=FAC*MFYY(I)
            MFZZ(I)=FAC*MFZZ(I)
            MFXY(I)=FAC*MFXY(I)
            MFXZ(I)=FAC*MFXZ(I)
            MFYX(I)=FAC*MFYX(I)
            MFYZ(I)=FAC*MFYZ(I)
            MFZX(I)=FAC*MFZX(I)
            MFZY(I)=FAC*MFZY(I)
          ENDDO
C     
         END IF !(ISMSTR == 10 ) THEN
C---ref stat for total strain     
          DO I=LFT,LLT
            X1(I)=XR(I,1)+FAC*VX1(I)
            Y1(I)=YR(I,1)+FAC*VY1(I)
            Z1(I)=ZR(I,1)+FAC*VZ1(I)
            X2(I)=XR(I,2)+FAC*VX2(I)
            Y2(I)=YR(I,2)+FAC*VY2(I)
            Z2(I)=ZR(I,2)+FAC*VZ2(I)
            X3(I)=XR(I,3)+FAC*VX3(I)
            Y3(I)=YR(I,3)+FAC*VY3(I)
            Z3(I)=ZR(I,3)+FAC*VZ3(I)
            X4(I)=XR(I,4)+FAC*VX4(I)
            Y4(I)=YR(I,4)+FAC*VY4(I)
            Z4(I)=ZR(I,4)+FAC*VZ4(I)
            X5(I)=XR(I,5)+FAC*VX5(I)
            Y5(I)=YR(I,5)+FAC*VY5(I)
            Z5(I)=ZR(I,5)+FAC*VZ5(I)
            X6(I)=XR(I,6)+FAC*VX6(I)
            Y6(I)=YR(I,6)+FAC*VY6(I)
            Z6(I)=ZR(I,6)+FAC*VZ6(I)
            X7(I)=XR(I,7)+FAC*VX7(I)
            Y7(I)=YR(I,7)+FAC*VY7(I)
            Z7(I)=ZR(I,7)+FAC*VZ7(I)
            X8(I)=XR(I,8)+FAC*VX8(I)
            Y8(I)=YR(I,8)+FAC*VY8(I)
            Z8(I)=ZR(I,8)+FAC*VZ8(I)
          END DO
C
C         Repere isoparametrique ,rep convecte, rep orthotrope
          CALL SREPISO3(
     .      X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .      Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .      Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .      RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,
     .      TZ   ,F1X  ,F1Y  ,F1Z  ,F2X  ,F2Y  ,F2Z  )

          IF (JHBE == 24) THEN
C           HEPH
            CALL SORTHO3(
     .        RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .        E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,E1X  ,E1Y  ,E1Z  )
          ELSE
            CALL SORTHO3(
     .        RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .        E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  )
          ENDIF
          IF (ISMSTR == 1 .OR. ISMSTR == 11) THEN
           DO I=LFT,LLT
            X1(I)=XR(I,1)
            Y1(I)=YR(I,1)
            Z1(I)=ZR(I,1)
            X2(I)=XR(I,2)
            Y2(I)=YR(I,2)
            Z2(I)=ZR(I,2)
            X3(I)=XR(I,3)
            Y3(I)=YR(I,3)
            Z3(I)=ZR(I,3)
            X4(I)=XR(I,4)
            Y4(I)=YR(I,4)
            Z4(I)=ZR(I,4)
            X5(I)=XR(I,5)
            Y5(I)=YR(I,5)
            Z5(I)=ZR(I,5)
            X6(I)=XR(I,6)
            Y6(I)=YR(I,6)
            Z6(I)=ZR(I,6)
            X7(I)=XR(I,7)
            Y7(I)=YR(I,7)
            Z7(I)=ZR(I,7)
            X8(I)=XR(I,8)
            Y8(I)=YR(I,8)
            Z8(I)=ZR(I,8)
           END DO
          END IF

          CALL SRROTA3(E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .         X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .         Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .         Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   )

          IF (IGTYP == 6)THEN
            IF(JHBE /=24)THEN
              CALL SRORTH3(JHBE ,GBUF%GAMA ,
     .           E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y  ,E1Z  ,E2Z  ,E3Z ,
     .           X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .           Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .           Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,NEL)
            ELSE
            END IF
          END IF
         IF (ISMSTR >= 10 ) THEN
           CALL SORDEFT3(LFT,LLT,MFXX, MFXY, MFXZ, MFYX, MFYY, MFYZ,
     .                  MFZX, MFZY, MFZZ,
     .                  E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z)
         END IF
C-----------
          IF (JHBE == 24) THEN
           CALL SZREFDERI3(
     .        VOLN ,VBID ,GEO  ,IGEO ,
     .        X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .        Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .        Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .        PX1  ,PX2  ,PX3  ,PX4  ,
     .        PY1  ,PY2  ,PY3  ,PY4  ,
     .        PZ1  ,PZ2  ,PZ3  ,PZ4  ,
     .        RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TZ   ,
     .        NGL  ,PID  ,VOLU )
          ELSE
           CALL SREFDERI3(
     .        VOLN ,VBID ,GEO  ,IGEO ,
     .        X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .        Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .        Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .        RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,NGL  ,PID  ,
     .        PX1  ,PX2  ,PX3  ,PX4  ,PY1  ,PY2  ,PY3  ,PY4  ,
     .        PZ1  ,PZ2  ,PZ3  ,PZ4, VOLU)
          ENDIF
C-----------
          DO I=LFT,LLT
            VXL(I,1)=VX1(I)
            VYL(I,1)=VY1(I)
            VZL(I,1)=VZ1(I)
            VXL(I,2)=VX2(I)
            VYL(I,2)=VY2(I)
            VZL(I,2)=VZ2(I)
            VXL(I,3)=VX3(I)
            VYL(I,3)=VY3(I)
            VZL(I,3)=VZ3(I)
            VXL(I,4)=VX4(I)
            VYL(I,4)=VY4(I)
            VZL(I,4)=VZ4(I)
            VXL(I,5)=VX5(I)
            VYL(I,5)=VY5(I)
            VZL(I,5)=VZ5(I)
            VXL(I,6)=VX6(I)
            VYL(I,6)=VY6(I)
            VZL(I,6)=VZ6(I)
            VXL(I,7)=VX7(I)
            VYL(I,7)=VY7(I)
            VZL(I,7)=VZ7(I)
            VXL(I,8)=VX8(I)
            VYL(I,8)=VY8(I)
            VZL(I,8)=VZ8(I)
          END DO
          CALL SRROTA3(E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .         VXL(1,1)   ,VXL(1,2)   ,VXL(1,3)   ,VXL(1,4)   ,
     .         VXL(1,5)   ,VXL(1,6)   ,VXL(1,7)   ,VXL(1,8)   ,
     .         VYL(1,1)   ,VYL(1,2)   ,VYL(1,3)   ,VYL(1,4)   ,
     .         VYL(1,5)   ,VYL(1,6)   ,VYL(1,7)   ,VYL(1,8)   ,
     .         VZL(1,1)   ,VZL(1,2)   ,VZL(1,3)   ,VZL(1,4)   ,
     .         VZL(1,5)   ,VZL(1,6)   ,VZL(1,7)   ,VZL(1,8)   )

          CALL SDEFO3(
     .      PX1, PX2, PX3, PX4,
     .      PY1, PY2, PY3, PY4,
     .      PZ1, PZ2, PZ3, PZ4,
     .      VXL(1,1), VXL(1,2), VXL(1,3), VXL(1,4),
     .      VXL(1,5), VXL(1,6), VXL(1,7), VXL(1,8),
     .      VYL(1,1), VYL(1,2), VYL(1,3), VYL(1,4),
     .      VYL(1,5), VYL(1,6), VYL(1,7), VYL(1,8),
     .      VZL(1,1), VZL(1,2), VZL(1,3), VZL(1,4),
     .      VZL(1,5), VZL(1,6), VZL(1,7), VZL(1,8),
     .      DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY, DZZ, D4, D5, D6,
     .      WXX, WYY, WZZ)
C-----------
C         IF (IGTYP == 6 .OR. IGTYP == 21)THEN
          IF (IGTYP == 6)THEN
            IF(JHBE /=24)THEN
            ELSE
              CALL STORTH3(LFT,LLT,NEL,
     .              G1X, G1Y, G1Z, G2X, G2Y, G2Z, G3X, G3Y, G3Z,
     .              GBUF%GAMA)
              CALL SZORDEF3(LFT,LLT,DXX,DYY,DZZ,D4,D5,D6,
     .              G1X, G1Y, G1Z, G2X, G2Y, G2Z, G3X, G3Y, G3Z)
            END IF
          END IF
C-----------
          CALL SRHO3(PM, GBUF%VOL, GBUF%RHO, GBUF%EINT, DXX,
     .               DYY, DZZ, VOLN, DVOL, MAT)
          DO I=LFT,LLT
            S1(I) = GBUF%SIG(II(1) + I)
            S2(I) = GBUF%SIG(II(2) + I)
            S3(I) = GBUF%SIG(II(3) + I)
            S4(I) = GBUF%SIG(II(4) + I)
            S5(I) = GBUF%SIG(II(5) + I)
            S6(I) = GBUF%SIG(II(6) + I)
          END DO
C-----------
          CALL MMAIN(PM   ,ELBUF_STR,IXS    ,NIXS    ,X      ,
     2             GEO    ,IPARG    ,NEL    ,SKEW    ,BUFMAT ,
     3             IPART   ,IPARTS ,
     4             MAT    ,IPM      ,NGL    ,PID     ,NPF    ,
     5             TF     ,MFXX     ,MFXY   ,MFXZ    ,MFYX   ,
     6             MFYY   ,MFYZ     ,MFZX   ,MFZY    ,MFZZ   ,
     7             RX     ,RY       ,RZ     ,SX      ,SY     ,
     8             SZ     ,GBUF%GAMA,VOLN   ,DVOL    ,S1     ,
     B             S2     ,S3       ,S4     ,S5      ,S6     ,
     9             DXX    ,DYY      ,DZZ    ,D4      ,D5     ,
     A             D6     ,WXX      ,WYY    ,WZZ     )
        END DO
      END IF
C ======================================================================
      NITRS=NITSAV
      RETURN
      END SUBROUTINE SREFSTA3
c
Chd|====================================================================
Chd|  SDEFOT3                       source/elements/solid/solide/srefsta3.F
Chd|-- called by -----------
Chd|        SREFSTA3                      source/elements/solid/solide/srefsta3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SDEFOT3(
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY, DZZ)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J
      my_real
     .   VX17(MVSIZ), VY17(MVSIZ), VZ17(MVSIZ),
     .   VX28(MVSIZ), VY28(MVSIZ), VZ28(MVSIZ),
     .   VX35(MVSIZ), VY35(MVSIZ), VZ35(MVSIZ),
     .   VX46(MVSIZ), VY46(MVSIZ), VZ46(MVSIZ)
C-----------------------------------------------
      DO I=LFT,LLT
       VX17(I)=VX1(I)-VX7(I)
       VX28(I)=VX2(I)-VX8(I)
       VX35(I)=VX3(I)-VX5(I)
       VX46(I)=VX4(I)-VX6(I)
       VY17(I)=VY1(I)-VY7(I)
       VY28(I)=VY2(I)-VY8(I)
       VY35(I)=VY3(I)-VY5(I)
       VY46(I)=VY4(I)-VY6(I)
       VZ17(I)=VZ1(I)-VZ7(I)
       VZ28(I)=VZ2(I)-VZ8(I)
       VZ35(I)=VZ3(I)-VZ5(I)
       VZ46(I)=VZ4(I)-VZ6(I)
      ENDDO

C                                                                     12
      DO I=LFT,LLT
        DXX(I)=PX1(I)*VX17(I)+PX2(I)*VX28(I)+
     .         PX3(I)*VX35(I)+PX4(I)*VX46(I)
        DYY(I)=PY1(I)*VY17(I)+PY2(I)*VY28(I)+
     .         PY3(I)*VY35(I)+PY4(I)*VY46(I)
        DZZ(I)=PZ1(I)*VZ17(I)+PZ2(I)*VZ28(I)+
     .         PZ3(I)*VZ35(I)+PZ4(I)*VZ46(I)
        DXY(I)=PY1(I)*VX17(I)+PY2(I)*VX28(I)+
     .         PY3(I)*VX35(I)+PY4(I)*VX46(I)
        DXZ(I)=PZ1(I)*VX17(I)+PZ2(I)*VX28(I)+
     .         PZ3(I)*VX35(I)+PZ4(I)*VX46(I)
        DYX(I)=PX1(I)*VY17(I)+PX2(I)*VY28(I)+
     .         PX3(I)*VY35(I)+PX4(I)*VY46(I)
        DYZ(I)=PZ1(I)*VY17(I)+PZ2(I)*VY28(I)+
     .         PZ3(I)*VY35(I)+PZ4(I)*VY46(I)
        DZX(I)=PX1(I)*VZ17(I)+PX2(I)*VZ28(I)+
     .         PX3(I)*VZ35(I)+PX4(I)*VZ46(I)
        DZY(I)=PY1(I)*VZ17(I)+PY2(I)*VZ28(I)+
     .         PY3(I)*VZ35(I)+PY4(I)*VZ46(I)
      ENDDO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  SORDEFT3                      source/elements/solid/solide/srefsta3.F
Chd|-- called by -----------
Chd|        SREFSTA3                      source/elements/solid/solide/srefsta3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SORDEFT3(JFT,JLT,MXX, MXY, MXZ,  
     .                     MYX, MYY, MYZ,MZX, MZY, MZZ,
     .                     G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
      my_real
     .   MXX(*), MXY(*), MXZ(*),MYX(*), MYY(*), MYZ(*),  
     .   MZX(*), MZY(*), MZZ(*),G1X(*),G1Y(*),G1Z(*),
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   SX,SY,SZ,FXX,FXY,FXZ,FYX,FYY,FYZ,FZX,FZY,FZZ
C-----------------------------------------------
      DO I=JFT,JLT
         SX = MXX(I)*G1X(I)+MYX(I)*G1Y(I)+MZX(I)*G1Z(I)
         SY = MXY(I)*G1X(I)+MYY(I)*G1Y(I)+MZY(I)*G1Z(I)
         SZ = MXZ(I)*G1X(I)+MYZ(I)*G1Y(I)+MZZ(I)*G1Z(I)
         FXX = SX*G1X(I)+SY*G1Y(I)+SZ*G1Z(I)
         FXY = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         FXZ = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         SX = MXX(I)*G2X(I)+MYX(I)*G2Y(I)+MZX(I)*G2Z(I)
         SY = MXY(I)*G2X(I)+MYY(I)*G2Y(I)+MZY(I)*G2Z(I)
         SZ = MXZ(I)*G2X(I)+MYZ(I)*G2Y(I)+MZZ(I)*G2Z(I)
         FYX = SX*G1X(I)+SY*G1Y(I)+SZ*G1Z(I)
         FYY = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         FYZ = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         SX = MXX(I)*G3X(I)+MYX(I)*G3Y(I)+MZX(I)*G3Z(I)
         SY = MXY(I)*G3X(I)+MYY(I)*G3Y(I)+MZY(I)*G3Z(I)
         SZ = MXZ(I)*G3X(I)+MYZ(I)*G3Y(I)+MZZ(I)*G3Z(I)
         FZX = SX*G1X(I)+SY*G1Y(I)+SZ*G1Z(I)
         FZY = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         FZZ = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         MXX(I)=FXX
         MXY(I)=FXY
         MXZ(I)=FXZ
         MYX(I)=FYX
         MYY(I)=FYY
         MYZ(I)=FYZ
         MZX(I)=FZX
         MZY(I)=FZY
         MZZ(I)=FZZ
      ENDDO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  SREFDERI3                     source/elements/solid/solide/srefsta3.F
Chd|-- called by -----------
Chd|        SREFSTA3                      source/elements/solid/solide/srefsta3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SREFDERI3(
     .              VOL  ,VEUL ,GEO  ,IGEO , 
     .              X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .              Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .              Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .              JJ1  ,JJ2  ,JJ3  ,JJ4  ,JJ5  ,JJ6  ,NGL  ,NGEO ,
     .              PX1  ,PX2  ,PX3  ,PX4  ,PY1  ,PY2  ,PY3  ,PY4  ,
     .              PZ1  ,PZ2  ,PZ3  ,PZ4, DET)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C     SOUS PROGRAMME DERIVED FROM SDERI3
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
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGEO(NPROPGI,*),NGL(*),NGEO(*)
C     REAL ou REAL*8
      my_real
     .   VOL(*), VEUL(LVEUL,*),GEO(NPROPG,*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*),
     .   X7(*), X8(*), Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*),
     .   Y8(*), Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),
     .   JJ1(*), JJ2(*), JJ3(*), JJ4(*), JJ5(*), JJ6(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), DET(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      
      DOUBLE PRECISION
     .   X1_COPY(MVSIZ), X2_COPY(MVSIZ), X3_COPY(MVSIZ), X4_COPY(MVSIZ),
     .   X5_COPY(MVSIZ), X6_COPY(MVSIZ), X7_COPY(MVSIZ), X8_COPY(MVSIZ),
     .   Y1_COPY(MVSIZ), Y2_COPY(MVSIZ), Y3_COPY(MVSIZ), Y4_COPY(MVSIZ),
     .   Y5_COPY(MVSIZ), Y6_COPY(MVSIZ), Y7_COPY(MVSIZ), Y8_COPY(MVSIZ),
     .   Z1_COPY(MVSIZ), Z2_COPY(MVSIZ), Z3_COPY(MVSIZ), Z4_COPY(MVSIZ),
     .   Z5_COPY(MVSIZ), Z6_COPY(MVSIZ), Z7_COPY(MVSIZ), Z8_COPY(MVSIZ)
C     REAL ou REAL*8
      my_real
     .   JJ7(MVSIZ), JJ8(MVSIZ) , JJ9(MVSIZ),
     .   A17(MVSIZ) , A28(MVSIZ) ,
     .   B17(MVSIZ) , B28(MVSIZ) ,
     .   C17(MVSIZ) , C28(MVSIZ) 
      my_real
     .   DETT(MVSIZ), 
     .   INV_JAC1(MVSIZ), INV_JAC2(MVSIZ), INV_JAC3(MVSIZ), 
     .   INV_JAC4(MVSIZ), INV_JAC5(MVSIZ), INV_JAC6(MVSIZ),
     .   INV_JAC7(MVSIZ), INV_JAC8(MVSIZ), INV_JAC9(MVSIZ),
     .   X17(MVSIZ), X28(MVSIZ), X35(MVSIZ), X46(MVSIZ),
     .   Y17(MVSIZ), Y28(MVSIZ), Y35(MVSIZ), Y46(MVSIZ),
     .   Z17(MVSIZ), Z28(MVSIZ), Z35(MVSIZ), Z46(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JJ12(MVSIZ),   JJ45(MVSIZ),   JJ78(MVSIZ)
C=======================================================================
      DO I=LFT,LLT
        X1_COPY(I)=X1(I)
        X2_COPY(I)=X2(I)
        X3_COPY(I)=X3(I)
        X4_COPY(I)=X4(I)
        X5_COPY(I)=X5(I)
        X6_COPY(I)=X6(I)
        X7_COPY(I)=X7(I)
        X8_COPY(I)=X8(I)
C
        Y1_COPY(I)=Y1(I)
        Y2_COPY(I)=Y2(I)
        Y3_COPY(I)=Y3(I)
        Y4_COPY(I)=Y4(I)
        Y5_COPY(I)=Y5(I)
        Y6_COPY(I)=Y6(I)
        Y7_COPY(I)=Y7(I)
        Y8_COPY(I)=Y8(I)
C
        Z1_COPY(I)=Z1(I)
        Z2_COPY(I)=Z2(I)
        Z3_COPY(I)=Z3(I)
        Z4_COPY(I)=Z4(I)
        Z5_COPY(I)=Z5(I)
        Z6_COPY(I)=Z6(I)
        Z7_COPY(I)=Z7(I)
        Z8_COPY(I)=Z8(I)
      ENDDO     

      DO I=LFT,LLT
        X17(I)=X7_COPY(I)-X1_COPY(I)
        X28(I)=X8_COPY(I)-X2_COPY(I)
        X35(I)=X5_COPY(I)-X3_COPY(I)
        X46(I)=X6_COPY(I)-X4_COPY(I)
        Y17(I)=Y7_COPY(I)-Y1_COPY(I)
        Y28(I)=Y8_COPY(I)-Y2_COPY(I)
        Y35(I)=Y5_COPY(I)-Y3_COPY(I)
        Y46(I)=Y6_COPY(I)-Y4_COPY(I)
        Z17(I)=Z7_COPY(I)-Z1_COPY(I)
        Z28(I)=Z8_COPY(I)-Z2_COPY(I)
        Z35(I)=Z5_COPY(I)-Z3_COPY(I)
        Z46(I)=Z6_COPY(I)-Z4_COPY(I)
      ENDDO
C
      DO I=LFT,LLT               
        JJ1(I)=X17(I)+X28(I)-X35(I)-X46(I)
        JJ2(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
        JJ3(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
        A17(I)=X17(I)+X46(I)
        A28(I)=X28(I)+X35(I)
        B17(I)=Y17(I)+Y46(I)
        B28(I)=Y28(I)+Y35(I)
        C17(I)=Z17(I)+Z46(I)
        C28(I)=Z28(I)+Z35(I)
      ENDDO
         
      DO I=LFT,LLT
        JJ4(I)=A17(I)+A28(I)
        JJ5(I)=B17(I)+B28(I)
        JJ6(I)=C17(I)+C28(I)
        JJ7(I)=A17(I)-A28(I)
        JJ8(I)=B17(I)-B28(I)
        JJ9(I)=C17(I)-C28(I)
      ENDDO
C
C     JACOBIAN
C
      DO I=LFT,LLT
        JAC_59_68(I)=JJ5(I)*JJ9(I)-JJ6(I)*JJ8(I)
        JAC_67_49(I)=JJ6(I)*JJ7(I)-JJ4(I)*JJ9(I)
        JAC_48_57(I)=JJ4(I)*JJ8(I)-JJ5(I)*JJ7(I)
      ENDDO
C
      DO I=LFT,LLT
       DET(I)=O64TH*(JJ1(I)*JAC_59_68(I)+JJ2(I)*JAC_67_49(I)+JJ3(I)*JAC_48_57(I))
       VOL(I)=DET(I)
      ENDDO     
C
      IF(JEUL /= 0)THEN
        DO I=LFT,LLT
          VEUL(32,I) = VOL(I)
        ENDDO
      ENDIF             
C
      DO I=LFT,LLT
        IF (DET(I) > ZERO) CYCLE
        IF (IGEO(11,NGEO(I)).NE.0 .AND. IGEO(11,NGEO(I)).NE.43) THEN
          CALL ANCMSG(MSGID=245,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=NGL(I))
        ELSE
          CALL ANCMSG(MSGID=635,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO,
     .               I1=NGL(I))
        ENDIF
      ENDDO
C
      IF( JEUL==0 .AND. NXREF==0) RETURN
C
      DO I=LFT,LLT
        DETT(I)=O64TH/MAX(DET(I),EM20)
      ENDDO
C
C     INVERSE DE LA MATRICE JACOBIENNE
C
      DO I=LFT,LLT
        INV_JAC1(I)=DETT(I)*JAC_59_68(I)
        INV_JAC4(I)=DETT(I)*JAC_67_49(I)
        INV_JAC7(I)=DETT(I)*JAC_48_57(I)
        INV_JAC2(I)=DETT(I)*(-JJ2(I)*JJ9(I)+JJ3(I)*JJ8(I))
        INV_JAC5(I)=DETT(I)*( JJ1(I)*JJ9(I)-JJ3(I)*JJ7(I))
        INV_JAC8(I)=DETT(I)*(-JJ1(I)*JJ8(I)+JJ2(I)*JJ7(I))
        INV_JAC3(I)=DETT(I)*( JJ2(I)*JJ6(I)-JJ3(I)*JJ5(I))
        INV_JAC6(I)=DETT(I)*(-JJ1(I)*JJ6(I)+JJ3(I)*JJ4(I))
        INV_JAC9(I)=DETT(I)*( JJ1(I)*JJ5(I)-JJ2(I)*JJ4(I))
      ENDDO
C
      DO I=LFT,LLT
        JJ12(I)=INV_JAC1(I)-INV_JAC2(I)
        JJ45(I)=INV_JAC4(I)-INV_JAC5(I)
        JJ78(I)=INV_JAC7(I)-INV_JAC8(I)
      ENDDO

      DO I=LFT,LLT
        PX3(I)= JJ12(I)+INV_JAC3(I)
        PY3(I)= JJ45(I)+INV_JAC6(I)
        PZ3(I)= JJ78(I)+INV_JAC9(I)
        PX4(I)= JJ12(I)-INV_JAC3(I)
        PY4(I)= JJ45(I)-INV_JAC6(I)
        PZ4(I)= JJ78(I)-INV_JAC9(I)
      ENDDO

      DO I=LFT,LLT
        JJ12(I)=INV_JAC1(I)+INV_JAC2(I)
        JJ45(I)=INV_JAC4(I)+INV_JAC5(I)
        JJ78(I)=INV_JAC7(I)+INV_JAC8(I)
      ENDDO

      DO I=LFT,LLT
        PX1(I)=-JJ12(I)-INV_JAC3(I)
        PY1(I)=-JJ45(I)-INV_JAC6(I)
        PZ1(I)=-JJ78(I)-INV_JAC9(I)
        PX2(I)=-JJ12(I)+INV_JAC3(I)
        PY2(I)=-JJ45(I)+INV_JAC6(I)
        PZ2(I)=-JJ78(I)+INV_JAC9(I)
      ENDDO
C---
      IF(JEUL /= 0)THEN
        DO I=LFT,LLT
          VEUL(1,I) = PX1(I)
          VEUL(2,I) = PX2(I)
          VEUL(3,I) = PX3(I)
          VEUL(4,I) = PX4(I)
          VEUL(5,I) = PY1(I)
          VEUL(6,I) = PY2(I)
          VEUL(7,I) = PY3(I)
          VEUL(8,I) = PY4(I)
          VEUL(9,I) = PZ1(I)
          VEUL(10,I)= PZ2(I)
          VEUL(11,I)= PZ3(I)
          VEUL(12,I)= PZ4(I)
        END DO
        IF (IGEO(11,NGEO(LFT)) == 15) THEN
          DO I=LFT,LLT
            VOL(I)=VOL(I)*GEO(1,NGEO(I))      
          END DO              
        END IF
      END IF
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  SZREFDERI3                    source/elements/solid/solide/srefsta3.F
Chd|-- called by -----------
Chd|        SREFSTA3                      source/elements/solid/solide/srefsta3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SZREFDERI3(
     .              VOL  ,VEUL ,GEO  ,IGEO , 
     .              X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   , 
     .              Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   , 
     .              Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .              PX1  ,PX2  ,PX3  ,PX4  ,
     .              PY1  ,PY2  ,PY3  ,PY4  ,
     .              PZ1  ,PZ2  ,PZ3  ,PZ4  , 
     .              JJ1  ,JJ2  ,JJ3  ,JJ4  ,JJ5  ,JJ6  ,JJ9  ,
     .              NGL  ,NGEO ,DET  )
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
#include      "com06_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGEO(NPROPGI,*),NGL(*),NGEO(*)
      my_real
     .   VOL(*), VEUL(LVEUL,*),GEO(NPROPG,*),
     .   JJ1(*), JJ2(*), JJ3(*), JJ4(*), JJ5(*), JJ6(*), JJ9(*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*), 
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*), 
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),DET(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      
      DOUBLE PRECISION
     .   X1_COPY(MVSIZ), X2_COPY(MVSIZ), X3_COPY(MVSIZ), X4_COPY(MVSIZ),
     .   X5_COPY(MVSIZ), X6_COPY(MVSIZ), X7_COPY(MVSIZ), X8_COPY(MVSIZ),
     .   Y1_COPY(MVSIZ), Y2_COPY(MVSIZ), Y3_COPY(MVSIZ), Y4_COPY(MVSIZ),
     .   Y5_COPY(MVSIZ), Y6_COPY(MVSIZ), Y7_COPY(MVSIZ), Y8_COPY(MVSIZ),
     .   Z1_COPY(MVSIZ), Z2_COPY(MVSIZ), Z3_COPY(MVSIZ), Z4_COPY(MVSIZ),
     .   Z5_COPY(MVSIZ), Z6_COPY(MVSIZ), Z7_COPY(MVSIZ), Z8_COPY(MVSIZ)
           
      my_real
     .   DETT(MVSIZ),  JJ7(MVSIZ), JJ8(MVSIZ) , 
     .   INV_JAC1(MVSIZ), INV_JAC2(MVSIZ), INV_JAC3(MVSIZ), INV_JAC4(MVSIZ), 
     .   INV_JAC5(MVSIZ),INV_JAC6(MVSIZ),INV_JAC7(MVSIZ), INV_JAC8(MVSIZ), INV_JAC9(MVSIZ), 
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ), JJ12(MVSIZ), 
     .   JJ45(MVSIZ),JJ78(MVSIZ),A17(MVSIZ),A28(MVSIZ),B17(MVSIZ),
     .   B28(MVSIZ),C17(MVSIZ),C28(MVSIZ), 
     .   X17(MVSIZ) , X28(MVSIZ) , X35(MVSIZ) , X46(MVSIZ),
     .   Y17(MVSIZ) , Y28(MVSIZ) , Y35(MVSIZ) , Y46(MVSIZ),
     .   Z17(MVSIZ) , Z28(MVSIZ) , Z35(MVSIZ) , Z46(MVSIZ)
C=======================================================================
      DO I=LFT,LLT
        X1_COPY(I)=X1(I)
        X2_COPY(I)=X2(I)
        X3_COPY(I)=X3(I)
        X4_COPY(I)=X4(I)
        X5_COPY(I)=X5(I)
        X6_COPY(I)=X6(I)
        X7_COPY(I)=X7(I)
        X8_COPY(I)=X8(I)
C
        Y1_COPY(I)=Y1(I)
        Y2_COPY(I)=Y2(I)
        Y3_COPY(I)=Y3(I)
        Y4_COPY(I)=Y4(I)
        Y5_COPY(I)=Y5(I)
        Y6_COPY(I)=Y6(I)
        Y7_COPY(I)=Y7(I)
        Y8_COPY(I)=Y8(I)
C
        Z1_COPY(I)=Z1(I)
        Z2_COPY(I)=Z2(I)
        Z3_COPY(I)=Z3(I)
        Z4_COPY(I)=Z4(I)
        Z5_COPY(I)=Z5(I)
        Z6_COPY(I)=Z6(I)
        Z7_COPY(I)=Z7(I)
        Z8_COPY(I)=Z8(I)
      ENDDO     
      
      DO I=LFT,LLT
        X17(I)=X7_COPY(I)-X1_COPY(I)
        X28(I)=X8_COPY(I)-X2_COPY(I)
        X35(I)=X5_COPY(I)-X3_COPY(I)
        X46(I)=X6_COPY(I)-X4_COPY(I)
        Y17(I)=Y7_COPY(I)-Y1_COPY(I)
        Y28(I)=Y8_COPY(I)-Y2_COPY(I)
        Y35(I)=Y5_COPY(I)-Y3_COPY(I)
        Y46(I)=Y6_COPY(I)-Y4_COPY(I)
        Z17(I)=Z7_COPY(I)-Z1_COPY(I)
        Z28(I)=Z8_COPY(I)-Z2_COPY(I)
        Z35(I)=Z5_COPY(I)-Z3_COPY(I)
        Z46(I)=Z6_COPY(I)-Z4_COPY(I)
      ENDDO
C
      DO I=LFT,LLT
        JJ4(I)=X17(I)+X28(I)-X35(I)-X46(I)
        JJ5(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
        JJ6(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
        A17(I)=X17(I)+X46(I)
        A28(I)=X28(I)+X35(I)
        B17(I)=Y17(I)+Y46(I)
        B28(I)=Y28(I)+Y35(I)
        C17(I)=Z17(I)+Z46(I)
        C28(I)=Z28(I)+Z35(I)
      ENDDO
C
      DO I=LFT,LLT
        JJ7(I)=A17(I)+A28(I)
        JJ8(I)=B17(I)+B28(I)
        JJ9(I)=C17(I)+C28(I)
        JJ1(I)=A17(I)-A28(I)
        JJ2(I)=B17(I)-B28(I)
        JJ3(I)=C17(I)-C28(I)
      ENDDO
C
C
      DO I=LFT,LLT
        JAC_59_68(I)=JJ5(I)*JJ9(I)-JJ6(I)*JJ8(I)
        JAC_67_49(I)=JJ6(I)*JJ7(I)-JJ4(I)*JJ9(I)
        JAC_48_57(I)=JJ4(I)*JJ8(I)-JJ5(I)*JJ7(I)
      ENDDO
C
C
      DO I=LFT,LLT
       DET(I)=O64TH*(JJ1(I)*JAC_59_68(I)+JJ2(I)*JAC_67_49(I)+JJ3(I)*JAC_48_57(I))
       VOL(I)=DET(I)
      ENDDO
C
      DO I=LFT,LLT
        IF(DET(I) <= ZERO) THEN
          CALL ANCMSG(MSGID=245,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=NGL(I))
        ENDIF
      ENDDO
C
      IF (JEUL == 0 .AND. NXREF == 0) RETURN
C
      DO I=LFT,LLT
        DETT(I)=O64TH/DET(I)
      ENDDO
C
C     INVERSE DE LA MATRICE JACOBIENNE
C
      DO I=LFT,LLT
        INV_JAC1(I)=DETT(I)*JAC_59_68(I)
        INV_JAC4(I)=DETT(I)*JAC_67_49(I)
        INV_JAC7(I)=DETT(I)*JAC_48_57(I)
        INV_JAC2(I)=DETT(I)*(-JJ2(I)*JJ9(I)+JJ3(I)*JJ8(I))
        INV_JAC5(I)=DETT(I)*( JJ1(I)*JJ9(I)-JJ3(I)*JJ7(I))
        INV_JAC8(I)=DETT(I)*(-JJ1(I)*JJ8(I)+JJ2(I)*JJ7(I))
        INV_JAC3(I)=DETT(I)*( JJ2(I)*JJ6(I)-JJ3(I)*JJ5(I))
        INV_JAC6(I)=DETT(I)*(-JJ1(I)*JJ6(I)+JJ3(I)*JJ4(I))
        INV_JAC9(I)=DETT(I)*( JJ1(I)*JJ5(I)-JJ2(I)*JJ4(I))
      ENDDO
C
      DO I=LFT,LLT
        JJ12(I)=INV_JAC1(I)-INV_JAC2(I)
        JJ45(I)=INV_JAC4(I)-INV_JAC5(I)
        JJ78(I)=INV_JAC7(I)-INV_JAC8(I)
      ENDDO
C
      DO I=LFT,LLT
        PX2(I)= JJ12(I)-INV_JAC3(I)
        PY2(I)= JJ45(I)-INV_JAC6(I)
        PZ2(I)= JJ78(I)-INV_JAC9(I)
        PX4(I)=-JJ12(I)-INV_JAC3(I)
        PY4(I)=-JJ45(I)-INV_JAC6(I)
        PZ4(I)=-JJ78(I)-INV_JAC9(I)
      ENDDO
C
      DO I=LFT,LLT
        JJ12(I)=INV_JAC1(I)+INV_JAC2(I)
        JJ45(I)=INV_JAC4(I)+INV_JAC5(I)
        JJ78(I)=INV_JAC7(I)+INV_JAC8(I)
      ENDDO
C
      DO I=LFT,LLT
        PX1(I)=-JJ12(I)-INV_JAC3(I)
        PY1(I)=-JJ45(I)-INV_JAC6(I)
        PZ1(I)=-JJ78(I)-INV_JAC9(I)
        PX3(I)=JJ12(I)-INV_JAC3(I)
        PY3(I)=JJ45(I)-INV_JAC6(I)
        PZ3(I)=JJ78(I)-INV_JAC9(I)
      ENDDO
C
      IF(JEUL /= 0)THEN
        DO I=LFT,LLT
          VEUL(3,I) = PX3(I)
          VEUL(4,I) = PY3(I)
          VEUL(7,I) = PZ3(I)
          VEUL(8,I) = PX4(I)
          VEUL(11,I)= PY4(I)
          VEUL(12,I)= PZ4(I)
          VEUL(1,I) = PX1(I)
          VEUL(2,I) = PY1(I)
          VEUL(5,I) = PZ1(I)
          VEUL(6,I) = PX2(I)
          VEUL(9,I) = PY2(I)
          VEUL(10,I)= PZ2(I)
        END DO
        IF (IGEO(11,NGEO(LFT)) == 15) THEN
          DO I=LFT,LLT
            VOL(I)=VOL(I)*GEO(1,NGEO(I))      
          ENDDO              
        ENDIF
      ENDIF
C-----------
      RETURN
      END
