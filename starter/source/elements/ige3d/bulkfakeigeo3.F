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
Chd|  BULKFAKEIGEO3                 source/elements/ige3d/bulkfakeigeo3.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE BULKFAKEIGEO3(ELBUF_TAB,IPARG,PM,KXIG3D,IGRSURF,STIFINT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
#include      "scr17_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,NGROUP),
     .        KXIG3D(NIXIG3D,*)
C     REAL
      my_real
     .   PM(NPROPM,*),STIFINT(*)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, N, NG, IEL, ITEL, NUMFAKECP,
     .        OFFSET_IGBUFSSG, OFFSET_NUMFAKENOD, ITSURF
C     REAL
      my_real
     .   AV1,CV1, VOL(NUMELIG3D),
     .   VNIGE(NUMFAKENODIGEO), BNIGE(NUMFAKENODIGEO)
      TYPE(G_BUFEL_) ,POINTER :: GBUF
C-----------------------------------------------------------------------
C 
C     isogeometric elements :
c
      DO NG=1,NGROUP
       IF (IPARG(5,NG) == 101) THEN
        DO ITEL=1,IPARG(2,NG)
          GBUF => ELBUF_TAB(NG)%GBUF
          VOL(ITEL+IPARG(3,NG)) = GBUF%VOL(ITEL)
        ENDDO
       ENDIF
      ENDDO

      VNIGE = 0
      BNIGE = 0
c
      DO ITSURF=1,NSURF
        IF(IGRSURF(ITSURF)%NSEG_IGE>0) THEN !! on est sur une surface isogeometrique
          DO J=1,IGRSURF(ITSURF)%NSEG_IGE
            IEL=IGRSURF(ITSURF)%ELEM_IGE(J)
            AV1 = VOL(IEL)/56
            CV1 = PM(32,KXIG3D(1,IEL))*AV1
            DO K=1,4
              NUMFAKECP=IGRSURF(ITSURF)%NODES_IGE(J,K)
              VNIGE(NUMFAKECP-NUMNOD)=VNIGE(NUMFAKECP-NUMNOD)+AV1
              BNIGE(NUMFAKECP-NUMNOD)=BNIGE(NUMFAKECP-NUMNOD)+CV1
            ENDDO
          ENDDO
        ENDIF
      ENDDO
c
      DO N=1,NUMFAKENODIGEO
        BNIGE(N)=BNIGE(N)/MAX(EM30,VNIGE(N))
        STIFINT(NUMNOD+N)=STIFINT(NUMNOD+N)+BNIGE(N)*(2.*VNIGE(N))**THIRD
      ENDDO

CC NB : on peut repartir le volume differemment selon si les points sont dans les coins, au milieu
cC      ou sur une arrete, avec PX,PY,PZ on peut retrouver la place de chacun
Cc pour les elements 20 neouds, il y a 3/192 * vol aux coins et 14/192 * vol aux points milieux
C
CC  NB: il me semble que numfakenodigeo est trop grand ici et donc les tableaux sont trop grands et se terminent par 0

      RETURN
      END
