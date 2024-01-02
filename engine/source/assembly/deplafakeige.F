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
Chd|  DEPLAFAKEIGE                  source/assembly/deplafakeige.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        IG3DONEBASIS                  source/elements/ige3d/ig3donebasis.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DEPLAFAKEIGE(X     ,V    ,INTBUF_TAB,KXIG3D, 
     .                        IXIG3D,IGEO ,KNOT      ,WIGE  ,
     .                        KNOTLOCPC,KNOTLOCEL  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXIG3D(NIXIG3D,*),IGEO(NPROPGI,*), IXIG3D(*)
C     REAL
      my_real
     .   X(3,*) ,V(3,*), KNOT(*), WIGE(*),KNOTLOCPC(DEG_MAX,3,*),
     .   KNOTLOCEL(2,3,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,K,N,J,JS,NCTRL, IPID, N1, N2, N3,
     .         PX, PY, PZ, IDX, IDY, IDZ, IDX2, IDY2, IDZ2, IAD_KNOT
      my_real
     .   XI, YI, ZI, VXI, VYI, VZI,ZR, ZS, ZT
      INTEGER  NKNOT1,NKNOT2,NKNOT3
      my_real
     .   X_IGEO(64),Y_IGEO(64),Z_IGEO(64),
     .   VX_IGEO(64),VY_IGEO(64),VZ_IGEO(64),
     .   W_IGEO(64),R(64)
      my_real
     .   KNOTLOCELX(2),
     .   KNOTLOCELY(2),KNOTLOCELZ(2)
      my_real, DIMENSION(:,:),ALLOCATABLE ::
     .   KNOTLOCX,KNOTLOCY,KNOTLOCZ
C-------------------------------------------------------------
C         CALCUL DES DEPLACEMENTS TRANSLATIONS DES POINTS FICTIFS IGEO
C-------------------------------------------------------------
c
      DO N=1,NINTER
        DO I=1,INTBUF_TAB(N)%S_NIGE

          JS=INTBUF_TAB(N)%NIGE(I)  ! numero local de l'element IGE

          KNOTLOCELX(1) = KNOTLOCEL(1,1,JS)
          KNOTLOCELY(1) = KNOTLOCEL(1,2,JS)
          KNOTLOCELZ(1) = KNOTLOCEL(1,3,JS)
          KNOTLOCELX(2) = KNOTLOCEL(2,1,JS)
          KNOTLOCELY(2) = KNOTLOCEL(2,2,JS)
          KNOTLOCELZ(2) = KNOTLOCEL(2,3,JS)

          NCTRL = KXIG3D(3,JS)
          IPID=KXIG3D(2,JS)
          IAD_KNOT = IGEO(40,IPID)
          PX = IGEO(41,IPID)
          PY = IGEO(42,IPID)
          PZ = IGEO(43,IPID)

          ALLOCATE (KNOTLOCX(PX+1,NCTRL),KNOTLOCY(PY+1,NCTRL),KNOTLOCZ(PZ+1,NCTRL))

          DO J=1,NCTRL
            X_IGEO(J)=X(1,IXIG3D(KXIG3D(4,JS)+J-1))
            Y_IGEO(J)=X(2,IXIG3D(KXIG3D(4,JS)+J-1))
            Z_IGEO(J)=X(3,IXIG3D(KXIG3D(4,JS)+J-1))
            VX_IGEO(J)=V(1,IXIG3D(KXIG3D(4,JS)+J-1))
            VY_IGEO(J)=V(2,IXIG3D(KXIG3D(4,JS)+J-1))
            VZ_IGEO(J)=V(3,IXIG3D(KXIG3D(4,JS)+J-1))
            W_IGEO(J)=WIGE(IXIG3D(KXIG3D(4,JS)+J-1))

            DO K=1,PX+1
              KNOTLOCX(K,J)=KNOTLOCPC(K,1,(IPID-1)*NUMNOD+IXIG3D(KXIG3D(4,JS)+J-1))
            ENDDO
            DO K=1,PY+1
              KNOTLOCY(K,J)=KNOTLOCPC(K,2,(IPID-1)*NUMNOD+IXIG3D(KXIG3D(4,JS)+J-1))
            ENDDO
            DO K=1,PZ+1
              KNOTLOCZ(K,J)=KNOTLOCPC(K,3,(IPID-1)*NUMNOD+IXIG3D(KXIG3D(4,JS)+J-1))
            ENDDO

          ENDDO
          N1 = IGEO(44,IPID)
          N2 = IGEO(45,IPID)
          N3 = IGEO(46,IPID)
          NKNOT1 = N1+PX
          NKNOT2 = N2+PY
          NKNOT3 = N3+PZ
          IDX = KXIG3D(6,JS)
          IDY = KXIG3D(7,JS)
          IDZ = KXIG3D(8,JS)
          IDX2 = KXIG3D(9,JS)
          IDY2 = KXIG3D(10,JS)
          IDZ2 = KXIG3D(11,JS)
C
          XI = ZERO
          YI = ZERO
          ZI = ZERO
          VXI = ZERO
          VYI = ZERO
          VZI = ZERO
          ZR = INTBUF_TAB(N)%RIGE(3*(I-1)+1)
          ZS = INTBUF_TAB(N)%RIGE(3*(I-1)+2)
          ZT = INTBUF_TAB(N)%RIGE(3*(I-1)+3)
C
c         CALL IGE3DBASIS(
c     1        JS        ,N         ,X_IGEO    ,Y_IGEO  ,
c     2        Z_IGEO    ,W_IGEO    ,IDX       ,IDY     ,
c     3        IDZ       ,R         ,
c     4        NCTRL     ,ZR        ,ZS        ,ZT      ,
c     5        KNOT(IAD_KNOT+1) ,KNOT(IAD_KNOT+NKNOT1+1),
c     6        KNOT(IAD_KNOT+NKNOT1+NKNOT2+1)  ,PX-1    , 
c     7        PY-1      ,PZ-1      ,0)

          CALL IG3DONEBASIS(
     1      JS     ,N        ,X_IGEO  ,Y_IGEO,
     2      Z_IGEO,W_IGEO    ,IDX   ,IDY ,
     3      IDZ ,KNOTLOCX ,KNOTLOCY,KNOTLOCZ,
     4      R          ,NCTRL  ,
     5      ZR  ,ZS      ,ZT   ,KNOT(IAD_KNOT+1),
     6      KNOT(IAD_KNOT+NKNOT1+1),KNOT(IAD_KNOT+NKNOT1+NKNOT2+1),PX-1, 
     7      PY-1   ,PZ-1       ,0        ,
     8      IDX2,IDY2    ,IDZ2   ,
     9      KNOTLOCELX,KNOTLOCELY,KNOTLOCELZ)
C
         DO J=1,NCTRL
c           print*,'R', R(J)
           XI = XI + R(J)*X_IGEO(J)
           YI = YI + R(J)*Y_IGEO(J)
           ZI = ZI + R(J)*Z_IGEO(J)
           VXI = VXI + R(J)*VX_IGEO(J)
           VYI = VYI + R(J)*VY_IGEO(J)
           VZI = VZI + R(J)*VZ_IGEO(J)
         ENDDO
         INTBUF_TAB(N)%XIGE(3*(I-1)+1)=XI
         INTBUF_TAB(N)%XIGE(3*(I-1)+2)=YI
         INTBUF_TAB(N)%XIGE(3*(I-1)+3)=ZI
         INTBUF_TAB(N)%VIGE(3*(I-1)+1)=VXI
         INTBUF_TAB(N)%VIGE(3*(I-1)+2)=VYI
         INTBUF_TAB(N)%VIGE(3*(I-1)+3)=VZI
         DEALLOCATE (KNOTLOCX,KNOTLOCY,KNOTLOCZ)
         ENDDO
      ENDDO
C
      RETURN
      END
