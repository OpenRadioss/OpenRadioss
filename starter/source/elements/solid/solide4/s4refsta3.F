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
Chd|  S4REFSTA3                     source/elements/solid/solide4/s4refsta3.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        S4DEFO3                       source/elements/solid/solide4/s4defo3.F
Chd|        S4DEFOT3                      source/elements/solid/solide4/s4refsta3.F
Chd|        S4DERI3                       source/elements/solid/solide4/s4deri3.F
Chd|        S4ORTH3                       source/elements/solid/solide4/s4orth3.F
Chd|        S4ORTHT3                      source/elements/solid/solide4/s4orth3.F
Chd|        S4REPISO3                     source/elements/solid/solide4/s4repiso3.F
Chd|        S4REPISOT3                    source/elements/solid/solide4/s4repiso3.F
Chd|        S4ROTA3                       source/elements/solid/solide4/s4rota3.F
Chd|        S4ROTAT3                      source/elements/solid/solide4/s4rota3.F
Chd|        SORTHO3                       source/elements/solid/solide/sortho3.F
Chd|        SRHO3                         source/elements/solid/solide/srho3.F
Chd|        MAT_ELEM_MOD                  ../common_source/modules/mat_elem/mat_elem_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S4REFSTA3(ELBUF_STR,IXS     ,PM     ,GEO     ,IPARG   ,
     .                     IPM    ,IGEO    ,SKEW   ,X      ,XREFS   ,
     .                     NEL      ,IPARTS  ,IPART  ,BUFMAT  ,MATPARAM,
     .                     NPF      ,TF      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MAT_ELEM_MOD
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
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IPARG(*),IPARTS(*),IGEO(NPROPGI,*),
     .    IPM(NPROPMI,*),IPART(LIPART1,*), NPF(*)
      INTEGER NEL
      my_real
     .   PM(NPROPM,*), X(3,*), XREFS(8,3,*), GEO(NPROPG,*),
     .   SKEW(LSKEW,*), BUFMAT(*), TF(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II, N, JHBE, IGTYP, ITRS, ISTRA, IPID, IBID,
     .        NITSAV,JJ(6)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      INTEGER MAT(MVSIZ), PID(MVSIZ), NGL(MVSIZ),
     .    IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ)
      DOUBLE PRECISION
     .   XR(MVSIZ,8) ,YR(MVSIZ,8) ,ZR(MVSIZ,8) ,
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ), 
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),VOLDP(MVSIZ)     
      my_real
     .   RX(MVSIZ) ,RY(MVSIZ) ,RZ(MVSIZ) ,
     .   SX(MVSIZ) ,SY(MVSIZ) ,SZ(MVSIZ) ,
     .   TX(MVSIZ) ,TY(MVSIZ) ,TZ(MVSIZ) ,
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
     .   VXL(MVSIZ,8),VYL(MVSIZ,8),VZL(MVSIZ,8),
     .   VX1(MVSIZ),VX2(MVSIZ),VX3(MVSIZ),VX4(MVSIZ),
     .   VY1(MVSIZ),VY2(MVSIZ),VY3(MVSIZ),VY4(MVSIZ),
     .   VZ1(MVSIZ),VZ2(MVSIZ),VZ3(MVSIZ),VZ4(MVSIZ),
     .   DXX(MVSIZ),DXY(MVSIZ),DXZ(MVSIZ),
     .   DYX(MVSIZ),DYY(MVSIZ),DYZ(MVSIZ),
     .   DZX(MVSIZ),DZY(MVSIZ),DZZ(MVSIZ),
     .   D1(MVSIZ),D2(MVSIZ),D3(MVSIZ),D4(MVSIZ),D5(MVSIZ),D6(MVSIZ),
     .   S1(MVSIZ),S2(MVSIZ),S3(MVSIZ),S4(MVSIZ),S5(MVSIZ),S6(MVSIZ),
     .   WXX(MVSIZ), WYY(MVSIZ), WZZ(MVSIZ),VBID(LVEUL,MVSIZ)
      my_real
     .   FAC, XT, YT, ZT, A11, A12, A13, A21, A22, A23, A31, A32, A33,JAC(10,MVSIZ)
      my_real
     .   DELTAX(MVSIZ), VOLU(MVSIZ)
c----
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
C-----------------------------------------------
C   S o u r c e  L i n e s
C=======================================================================
      GBUF  => ELBUF_STR%GBUF
      LBUF  => ELBUF_STR%BUFLY(1)%LBUF(1,1,1)
      JHBE  = IPARG(23)
      JCVT  = IPARG(37)
      IGTYP = IPARG(38)
      ISTRA  = IPARG(44)
      IBID   = 0
C
      NITSAV=NITRS
!
      DO I=1,6
        JJ(I) = NEL*(I-1)
      ENDDO
!
C-----Case total strain, rather for computing Eint, stress will also be calculated at T=0 of Engine
C-----but NITRS=1, for nonlinear elastic, Eint is too approximative
      IF (ISMSTR >= 10 ) NITRS=10
C----------------------------
      IF (MTN == 35 .OR. MTN == 38 .OR. MTN == 42 .OR. MTN == 70)THEN
        IF (JCVT > 0) THEN
           IPID = IXS(NIXS-1,1)
           ID = IGEO(1,IPID)
           CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,IPID),LTITR)
           CALL ANCMSG(MSGID=906,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=ID,
     .                 C1=TITR)
        ENDIF
C----------------------------
C        Connectivities, PID, MID N
C----------------------------
        DO I=LFT,LLT
          N = NFT+I
          MAT(I)=IXS(1,N)
          PID(I)=IXS(NIXS-1,N)
          NGL(I)=IXS(NIXS,N)
          IX1(I)=IXS(2,N)
          IX2(I)=IXS(4,N)
          IX3(I)=IXS(7,N)
          IX4(I)=IXS(6,N)
        END DO
C----------------------------
C        Coordinates
C----------------------------
        DO I=LFT,LLT
          N = NFT+I
          XT      = XREFS(4,1,N)
          YT      = XREFS(4,2,N)
          ZT      = XREFS(4,3,N)
          XR(I,1) = XREFS(1,1,N)-XT
          YR(I,1) = XREFS(1,2,N)-YT
          ZR(I,1) = XREFS(1,3,N)-ZT
          XR(I,2) = XREFS(2,1,N)-XT
          YR(I,2) = XREFS(2,2,N)-YT
          ZR(I,2) = XREFS(2,3,N)-ZT
          XR(I,3) = XREFS(3,1,N)-XT
          YR(I,3) = XREFS(3,2,N)-YT
          ZR(I,3) = XREFS(3,3,N)-ZT
          XR(I,4) = ZERO
          YR(I,4) = ZERO
          ZR(I,4) = ZERO
        END DO
C-----Case total strain calculated in global sys
C       Repere isoparametrique -> rep convecte -> rep orthotrope
        CALL S4REPISOT3(XR(1,1) ,XR(1,2) ,XR(1,3) ,XR(1,4) ,
     .                 YR(1,1) ,YR(1,2) ,YR(1,3) ,YR(1,4) ,
     .                 ZR(1,1) ,ZR(1,2) ,ZR(1,3) ,ZR(1,4) ,
     .                 RX   ,RY   ,RZ   ,
     .                 SX   ,SY   ,SZ   ,
     .                 TX   ,TY   ,TZ   )
c        CALL SREPLOC3(
        CALL SORTHO3(
     .       RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .       E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y  ,E1Z  ,E2Z  ,E3Z  )

        IF (IGTYP == 6 .AND. JHBE /=24) THEN
          CALL S4ORTHT3(JHBE ,GBUF%GAMA,NEL,
     .         E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .         XR(1,1) ,XR(1,2) ,XR(1,3) ,XR(1,4) ,
     .         YR(1,1) ,YR(1,2) ,YR(1,3) ,YR(1,4) ,
     .         ZR(1,1) ,ZR(1,2) ,ZR(1,3) ,ZR(1,4) )
        ENDIF
C----------------------------
C       Config initiale
C----------------------------
        DO I=LFT,LLT
          XT = X(1,IX4(I))
          YT = X(2,IX4(I))
          ZT = X(3,IX4(I))
          X1(I)=X(1,IX1(I))-XT
          Y1(I)=X(2,IX1(I))-YT
          Z1(I)=X(3,IX1(I))-ZT
          X2(I)=X(1,IX2(I))-XT
          Y2(I)=X(2,IX2(I))-YT
          Z2(I)=X(3,IX2(I))-ZT
          X3(I)=X(1,IX3(I))-XT
          Y3(I)=X(2,IX3(I))-YT
          Z3(I)=X(3,IX3(I))-ZT
          X4(I)=ZERO
          Y4(I)=ZERO
          Z4(I)=ZERO
        END DO
C       Repere isoparametrique -> rep convecte -> rep orthotrope
        CALL S4REPISO3(X1   ,X2   ,X3   ,X4   ,
     .                 Y1   ,Y2   ,Y3   ,Y4   ,
     .                 Z1   ,Z2   ,Z3   ,Z4   ,
     .                 RX   ,RY   ,RZ   ,
     .                 SX   ,SY   ,SZ   ,
     .                 TX   ,TY   ,TZ   )
c        CALL SREPLOC3(
        CALL SORTHO3(
     .       RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .       E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y  ,E1Z  ,E2Z  ,E3Z  )
c        CALL S4ROTA3(E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
c     .             X1   ,X2   ,X3   ,X4   ,
c     .             Y1   ,Y2   ,Y3   ,Y4   ,
c     .             Z1   ,Z2   ,Z3   ,Z4   )
        IF (IGTYP == 6 .AND. JHBE /=24) THEN
          CALL S4ORTH3(JHBE ,GBUF%GAMA,NEL,
     .         E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z    ,
     .         X1   ,X2   ,X3   ,X4   ,
     .         Y1   ,Y2   ,Y3   ,Y4   ,
     .         Z1   ,Z2   ,Z3   ,Z4   )
        ENDIF
C----------------------------
C       Retourne la reference en repere global.
C----------------------------
c        CALL S4ROTAT3(E1X ,E1Y ,E1Z ,E2X ,E2Y ,E2Z ,E3X ,E3Y ,E3Z ,
c     .       XR(1,1)   ,XR(1,2)   ,XR(1,3)   ,XR(1,4)   ,
c     .       YR(1,1)   ,YR(1,2)   ,YR(1,3)   ,YR(1,4)   ,
c     .       ZR(1,1)   ,ZR(1,2)   ,ZR(1,3)   ,ZR(1,4)   )
C------     
        DO I=LFT,LLT
          N = NFT+I
          XT      = XREFS(4,1,N)
          YT      = XREFS(4,2,N)
          ZT      = XREFS(4,3,N)
          XR(I,1) = XREFS(1,1,N)-XT
          YR(I,1) = XREFS(1,2,N)-YT
          ZR(I,1) = XREFS(1,3,N)-ZT
          XR(I,2) = XREFS(2,1,N)-XT
          YR(I,2) = XREFS(2,2,N)-YT
          ZR(I,2) = XREFS(2,3,N)-ZT
          XR(I,3) = XREFS(3,1,N)-XT
          YR(I,3) = XREFS(3,2,N)-YT
          ZR(I,3) = XREFS(3,3,N)-ZT
          XR(I,4) = ZERO
          YR(I,4) = ZERO
          ZR(I,4) = ZERO
        END DO
C
        FAC = ONE/FLOAT(NITRS)
        DO I=LFT,LLT
          XT=X(1,IX4(I))
          YT=X(2,IX4(I))
          ZT=X(3,IX4(I))
          VX1(I)=(X(1,IX1(I))-XT-XR(I,1))*FAC
          VY1(I)=(X(2,IX1(I))-YT-YR(I,1))*FAC
          VZ1(I)=(X(3,IX1(I))-ZT-ZR(I,1))*FAC
          VX2(I)=(X(1,IX2(I))-XT-XR(I,2))*FAC
          VY2(I)=(X(2,IX2(I))-YT-YR(I,2))*FAC
          VZ2(I)=(X(3,IX2(I))-ZT-ZR(I,2))*FAC
          VX3(I)=(X(1,IX3(I))-XT-XR(I,3))*FAC
          VY3(I)=(X(2,IX3(I))-YT-YR(I,3))*FAC
          VZ3(I)=(X(3,IX3(I))-ZT-ZR(I,3))*FAC
          VX4(I)=ZERO
          VY4(I)=ZERO
          VZ4(I)=ZERO
        END DO
C----------------------------
C       deformation : reference -> initial
C----------------------------
        DO ITRS=1,NITRS
C
          FAC = FLOAT(ITRS)
C--------in  global sys	
        IF (ISMSTR >= 10) THEN
           CALL S4DERI3(VOLN   ,VBID   ,GEO    ,IGEO   ,RX    ,
     .          RY     ,RZ     ,SX     ,SY     ,
     .          SZ     ,TX     ,TY     ,TZ     ,
     .          XR(1,1)   ,XR(1,2)   ,XR(1,3)   ,XR(1,4)   ,
     .          YR(1,1)   ,YR(1,2)   ,YR(1,3)   ,YR(1,4)   ,
     .          ZR(1,1)   ,ZR(1,2)   ,ZR(1,3)   ,ZR(1,4)   ,
     .          PX1    ,PX2    ,PX3    ,PX4    ,
     .          PY1    ,PY2    ,PY3    ,PY4    ,
     .          PZ1    ,PZ2    ,PZ3    ,PZ4    ,GBUF%JAC_I, 
     .          DELTAX ,VOLU   ,NGL    ,PID    ,MAT       ,
     .          PM     ,VOLDP  )
          CALL S4DEFOT3(
     .        PX1, PX2, PX3, PX4,
     .        PY1, PY2, PY3, PY4,
     .        PZ1, PZ2, PZ3, PZ4,
     .        VX1, VX2, VX3, VX4, 
     .        VY1, VY2, VY3, VY4, 
     .        VZ1, VZ2, VZ3, VZ4, 
     .        MFXX, MFXY, MFXZ, MFYX, MFYY, MFYZ, MFZX, MFZY, MFZZ) 
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
C ------ to be done for orth, for the moment no orth law is available with total strain    
C         IF (IGTYP == 6) THEN
C         ENDIF
        END IF !(ISMSTR == 10 .OR. ISMSTR == 12) THEN
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
          END DO
C-----------
C         Repere isoparametrique -> rep convecte -> rep orthotrope
          CALL S4REPISO3(X1   ,X2   ,X3   ,X4   ,
     .                   Y1   ,Y2   ,Y3   ,Y4   ,
     .                   Z1   ,Z2   ,Z3   ,Z4   ,
     .                   RX   ,RY   ,RZ   ,
     .                   SX   ,SY   ,SZ   ,
     .                   TX   ,TY   ,TZ   )
C
c        CALL SREPLOC3(
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
           END DO
         END IF
         IF (ISMSTR >= 10) THEN
C----------- in global system for total strain
           DO I=LFT,LLT
            E1X(I)   = ONE
            E1Y(I)   = ZERO
            E1Z(I)   = ZERO
            E2X(I)   = ZERO
            E2Y(I)   = ONE
            E2Z(I)   = ZERO
            E3X(I)   = ZERO
            E3Y(I)   = ZERO
            E3Z(I)   = ONE
           END DO
          ELSE
            CALL SORTHO3(
     .         RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .         E1X  ,E2X  ,E3X  ,E1Y  ,E2Y  ,E3Y  ,E1Z  ,E2Z  ,E3Z  )
C
           CALL S4ROTA3(E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .                 X1   ,X2   ,X3   ,X4   ,
     .                 Y1   ,Y2   ,Y3   ,Y4   ,
     .                 Z1   ,Z2   ,Z3   ,Z4   )
          END IF !(ISMSTR >= 10) THEN
C
          IF (IGTYP == 6 .AND. JHBE /=24) THEN
            CALL S4ORTH3(JHBE ,GBUF%GAMA ,NEL,
     .           E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z    ,
     .           X1   ,X2   ,X3   ,X4   ,
     .           Y1   ,Y2   ,Y3   ,Y4   ,
     .           Z1   ,Z2   ,Z3   ,Z4   )
          ENDIF
C-----------
           CALL S4DERI3(VOLN   ,VBID   ,GEO    ,IGEO   ,RX    ,
     .         RY     ,RZ     ,SX     ,SY     ,
     .         SZ     ,TX     ,TY     ,TZ     ,
     .         X1     ,X2     ,X3     ,X4     ,Y1    ,Y2     ,
     .         Y3     ,Y4     ,Z1     ,Z2     ,Z3    ,Z4     ,
     .         PX1    ,PX2    ,PX3    ,PX4    ,
     .         PY1    ,PY2    ,PY3    ,PY4    ,
     .         PZ1    ,PZ2    ,PZ3    ,PZ4    ,JAC,
     .         DELTAX ,VOLU   ,NGL    ,PID    ,MAT,
     .         PM     ,VOLDP  )
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
          ENDDO
          CALL S4ROTAT3(E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .         VXL(1,1)   ,VXL(1,2)   ,VXL(1,3)   ,VXL(1,4)   ,
     .         VYL(1,1)   ,VYL(1,2)   ,VYL(1,3)   ,VYL(1,4)   ,
     .         VZL(1,1)   ,VZL(1,2)   ,VZL(1,3)   ,VZL(1,4)   )
C-----calculate Dij anyway to get EINT 
C-----(small strain is not taken account here, if do it, should also modify SRHO3)	
          CALL S4DEFO3(
     .      PX1, PX2, PX3, PX4,
     .      PY1, PY2, PY3, PY4,
     .      PZ1, PZ2, PZ3, PZ4,
     .      VXL(1,1), VXL(1,2), VXL(1,3), VXL(1,4),
     .      VYL(1,1), VYL(1,2), VYL(1,3), VYL(1,4),
     .      VZL(1,1), VZL(1,2), VZL(1,3), VZL(1,4),
     .      DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY, DZZ,
     .      D4, D5, D6 ,WXX, WYY, WZZ)
C-----------
          CALL SRHO3(PM, GBUF%VOL, GBUF%RHO, GBUF%EINT, DXX,
     .               DYY, DZZ, VOLN, DVOL, MAT)
        DO I=LFT,LLT
          S1(I) = GBUF%SIG(JJ(1) + I)
          S2(I) = GBUF%SIG(JJ(2) + I)
          S3(I) = GBUF%SIG(JJ(3) + I)
          S4(I) = GBUF%SIG(JJ(4) + I)
          S5(I) = GBUF%SIG(JJ(5) + I)
          S6(I) = GBUF%SIG(JJ(6) + I)
        END DO
C-----------
          CALL MMAIN(PM     ,ELBUF_STR,IXS     ,NIXS   ,X      ,
     2               GEO    ,IPARG  ,NEL     ,SKEW   ,BUFMAT ,
     3               IPART  ,IPARTS ,MATPARAM,
     4               MAT    ,IPM    ,NGL     ,PID    ,NPF    ,
     5               TF     ,MFXX   ,MFXY   ,MFXZ    ,MFYX   ,
     6               MFYY   ,MFYZ   ,MFZX   ,MFZY    ,MFZZ   ,
     7               RX     ,RY     ,RZ     ,SX      ,SY     ,
     8               SZ     ,GBUF%GAMA,VOLN ,DVOL    ,S1     ,
     B               S2     ,S3      ,S4     ,S5     ,S6     ,
     9               DXX    ,DYY     ,DZZ    ,D4     ,D5     ,
     A               D6     ,WXX     ,WYY    ,WZZ    )
        ENDDO
C-----------
c       stress -> global coordinates
        DO I=LFT,LLT
          S1(I) = GBUF%SIG(JJ(1) + I)
          S2(I) = GBUF%SIG(JJ(2) + I)
          S3(I) = GBUF%SIG(JJ(3) + I)
          S4(I) = GBUF%SIG(JJ(4) + I)
          S5(I) = GBUF%SIG(JJ(5) + I)
          S6(I) = GBUF%SIG(JJ(6) + I)
          A11 = S1(I)*E1X(I)+S4(I)*E2X(I)+S6(I)*E3X(I)
          A12 = S1(I)*E1Y(I)+S4(I)*E2Y(I)+S6(I)*E3Y(I)
          A13 = S1(I)*E1Z(I)+S4(I)*E2Z(I)+S6(I)*E3Z(I)
          A21 = S4(I)*E1X(I)+S2(I)*E2X(I)+S5(I)*E3X(I)
          A22 = S4(I)*E1Y(I)+S2(I)*E2Y(I)+S5(I)*E3Y(I)
          A23 = S4(I)*E1Z(I)+S2(I)*E2Z(I)+S5(I)*E3Z(I)
          A31 = S6(I)*E1X(I)+S5(I)*E2X(I)+S3(I)*E3X(I)
          A32 = S6(I)*E1Y(I)+S5(I)*E2Y(I)+S3(I)*E3Y(I)
          A33 = S6(I)*E1Z(I)+S5(I)*E2Z(I)+S3(I)*E3Z(I)
          S1(I) = E1X(I)*A11 + E2X(I)*A21 + E3X(I)*A31
          S2(I) = E1Y(I)*A12 + E2Y(I)*A22 + E3Y(I)*A32
          S3(I) = E1Z(I)*A13 + E2Z(I)*A23 + E3Z(I)*A33
          S4(I) = E1X(I)*A12 + E2X(I)*A22 + E3X(I)*A32
          S5(I) = E1Y(I)*A13 + E2Y(I)*A23 + E3Y(I)*A33
          S6(I) = E1X(I)*A13 + E2X(I)*A23 + E3X(I)*A33
          GBUF%SIG(JJ(1) + I) = S1(I)
          GBUF%SIG(JJ(2) + I) = S2(I)
          GBUF%SIG(JJ(3) + I) = S3(I)
          GBUF%SIG(JJ(4) + I) = S4(I)
          GBUF%SIG(JJ(5) + I) = S5(I)
          GBUF%SIG(JJ(6) + I) = S6(I)
        END DO
c       strain -> global coordinates
        IF (ISTRA > 0) THEN
          DO I=LFT,LLT
            D1(I) = LBUF%STRA(JJ(1) + I)
            D2(I) = LBUF%STRA(JJ(2) + I)
            D3(I) = LBUF%STRA(JJ(3) + I)
            D4(I) = LBUF%STRA(JJ(4) + I)*HALF
            D5(I) = LBUF%STRA(JJ(5) + I)*HALF
            D6(I) = LBUF%STRA(JJ(6) + I)*HALF
            A11 = D1(I)*E1X(I)+D4(I)*E2X(I)+D6(I)*E3X(I)
            A12 = D1(I)*E1Y(I)+D4(I)*E2Y(I)+D6(I)*E3Y(I)
            A13 = D1(I)*E1Z(I)+D4(I)*E2Z(I)+D6(I)*E3Z(I)
            A21 = D4(I)*E1X(I)+D2(I)*E2X(I)+D5(I)*E3X(I)
            A22 = D4(I)*E1Y(I)+D2(I)*E2Y(I)+D5(I)*E3Y(I)
            A23 = D4(I)*E1Z(I)+D2(I)*E2Z(I)+D5(I)*E3Z(I)
            A31 = D6(I)*E1X(I)+D5(I)*E2X(I)+D3(I)*E3X(I)
            A32 = D6(I)*E1Y(I)+D5(I)*E2Y(I)+D3(I)*E3Y(I)
            A33 = D6(I)*E1Z(I)+D5(I)*E2Z(I)+D3(I)*E3Z(I)
            D1(I) = E1X(I)*A11 + E2X(I)*A21 + E3X(I)*A31
            D2(I) = E1Y(I)*A12 + E2Y(I)*A22 + E3Y(I)*A32
            D3(I) = E1Z(I)*A13 + E2Z(I)*A23 + E3Z(I)*A33
            D4(I) = E1X(I)*A12 + E2X(I)*A22 + E3X(I)*A32
            D5(I) = E1Y(I)*A13 + E2Y(I)*A23 + E3Y(I)*A33
            D6(I) = E1X(I)*A13 + E2X(I)*A23 + E3X(I)*A33
            LBUF%STRA(JJ(1) + I) = D1(I)
            LBUF%STRA(JJ(2) + I) = D2(I)
            LBUF%STRA(JJ(3) + I) = D3(I)
            LBUF%STRA(JJ(4) + I) = D4(I)*TWO
            LBUF%STRA(JJ(5) + I) = D5(I)*TWO
            LBUF%STRA(JJ(6) + I) = D6(I)*TWO
          ENDDO
        ENDIF
C-----
      ENDIF
C ======================================================================
      NITRS=NITSAV
      RETURN
      END SUBROUTINE S4REFSTA3
c
Chd|====================================================================
Chd|  S4DEFOT3                      source/elements/solid/solide4/s4refsta3.F
Chd|-- called by -----------
Chd|        S4REFSTA3                     source/elements/solid/solide4/s4refsta3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S4DEFOT3(
     .   PX1, PX2, PX3, PX4, PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4, VX1, VX2, VX3, VX4,
     .   VY1, VY2, VY3, VY4, VZ1, VZ2, VZ3, VZ4,
     .   DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY,
     .   DZZ)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   VX1(*), VX2(*), VX3(*), VX4(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I
C-----------------------------------------------
C                                                                     12
      DO I=LFT,LLT
        DXX(I)=PX1(I)*VX1(I)+PX2(I)*VX2(I)+
     .         PX3(I)*VX3(I)+PX4(I)*VX4(I)
        DYY(I)=PY1(I)*VY1(I)+PY2(I)*VY2(I)+
     .         PY3(I)*VY3(I)+PY4(I)*VY4(I)
        DZZ(I)=PZ1(I)*VZ1(I)+PZ2(I)*VZ2(I)+
     .         PZ3(I)*VZ3(I)+PZ4(I)*VZ4(I)
        DXY(I)=PY1(I)*VX1(I)+PY2(I)*VX2(I)+
     .         PY3(I)*VX3(I)+PY4(I)*VX4(I)
        DXZ(I)=PZ1(I)*VX1(I)+PZ2(I)*VX2(I)+
     .         PZ3(I)*VX3(I)+PZ4(I)*VX4(I)
        DYX(I)=PX1(I)*VY1(I)+PX2(I)*VY2(I)+
     .         PX3(I)*VY3(I)+PX4(I)*VY4(I)
        DYZ(I)=PZ1(I)*VY1(I)+PZ2(I)*VY2(I)+
     .         PZ3(I)*VY3(I)+PZ4(I)*VY4(I)
        DZX(I)=PX1(I)*VZ1(I)+PX2(I)*VZ2(I)+
     .         PX3(I)*VZ3(I)+PX4(I)*VZ4(I)
        DZY(I)=PY1(I)*VZ1(I)+PY2(I)*VZ2(I)+
     .         PY3(I)*VZ3(I)+PY4(I)*VZ4(I)
      ENDDO
C-----------
      RETURN
      END
