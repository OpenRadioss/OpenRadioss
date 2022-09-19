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
Chd|  REBUILD_IG3D                  source/elements/ige3d/rebuild_ig3d.F
Chd|-- called by -----------
Chd|        PRERAFIG3D                    source/elements/ige3d/prerafig3d.F
Chd|-- calls ---------------
Chd|        MESHSURFIG3D_MOD              source/elements/ige3d/meshsurfig3d_mod.F
Chd|====================================================================
      SUBROUTINE REBUILD_IG3D(IXIG3D, KXIG3D,DIR,DEG,DEGTANG1,
     .                        DEGTANG2,KNOTLOCPC,KNOTLOCEL,
     .                        TAB_ELCUT,L_TAB_ELCUT,
     .                        TAB_NEWEL,L_TAB_NEWEL,
     .                        TAB_FCTCUT,L_TAB_FCTCUT,
     .                        TAB_REMOVE,TAB_NEWFCT,EL_CONNECT,
     .                        TABCONPATCH,IDFILS,FLAG_PRE,FLAG_DEBUG)
C----------------------------------------------------------------------
C   ROUTINE QUI ENLEVE DES TABLES DE CONNECTIVITE LES POINTS SUPPRIMES
C   ET Y INSERE LES NOUVEAUX POINTS
C   LA ROUTINE NE REMET PAS CETTE CONNECTIVITE DANS LE BON ORDRE : 
C   C'EST LE ROLE DE REORDER_IG3D.F
C----------------------------------------------------------------------
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESHSURFIG3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "analyse_name.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXIG3D(*),KXIG3D(NIXIG3D,*),TAB_NEWFCT(*),TAB_REMOVE(*),
     .        TAB_ELCUT(*),TAB_NEWEL(*),
     .        TAB_FCTCUT(*),EL_CONNECT(*),
     .        IDFILS(NBFILSMAX,*)
      TYPE(TABCONPATCH_IG3D_) TABCONPATCH
      INTEGER L_TAB_FCTCUT,L_TAB_NEWEL,L_TAB_ELCUT,
     .        DEG,DEGTANG1,DEGTANG2,DIR,FLAG_PRE,FLAG_DEBUG
      my_real KNOTLOCPC(DEG_MAX,3,*),KNOTLOCEL(2,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,IAD_IXIG3D,OFFSET_KNOT,DIRTANG1,DIRTANG2,
     .        INCTRL,IOUT,DECALGEO,ITNCTRL,IEL,JEL,ITFILS
      my_real TOL
C=======================================================================
c
      TOL = EM06
c
      IF(DIR==1) THEN
        DIRTANG1 = 2
        DIRTANG2 = 3
      ELSEIF(DIR==2) THEN
        DIRTANG1 = 3
        DIRTANG2 = 1
      ELSEIF(DIR==3) THEN
        DIRTANG1 = 1
        DIRTANG2 = 2
      ENDIF
cc
CC----------------------------------------------------------------------------------------------
cc   SUPPRESSION DES FONCTIONS RAFFINEES DES TABLES DE CONNECTIVITE DES ELEMENTS DU PATCH
cc   ET DES FILS QU'ON RAFFINE : 0 A LA PLACE
CC----------------------------------------------------------------------------------------------
cc
      DO I= 1,L_TAB_FCTCUT
        INCTRL=TAB_FCTCUT(I)
        DO J=1,TABCONPATCH%L_TAB_IG3D
          IEL=TABCONPATCH%TAB_IG3D(J)
          DO ITNCTRL=1,KXIG3D(3,IEL)
            IF(IXIG3D(KXIG3D(4,IEL)+ITNCTRL-1)==INCTRL) THEN
              IXIG3D(KXIG3D(4,IEL)+ITNCTRL-1) = 0
            ENDIF
          ENDDO
          DO K=1,IDFILS(1,IEL)
            JEL=IDFILS(K+1,IEL)
            DO ITNCTRL=1,KXIG3D(3,JEL)
              IF(IXIG3D(KXIG3D(4,JEL)+ITNCTRL-1)==INCTRL) THEN
                IXIG3D(KXIG3D(4,JEL)+ITNCTRL-1) = 0
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
cc
CC----------------------------------------------------------------------------------------------
cc   TRAITEMENT DES TABLES DE CONNECTIVITES DES ELEMENTS DU PATCH :
cc   RAJOUT DES NOUVELLES FONCTIONS CREES PAR LE RAFFINEMENT
cc   NB : POURRAIT ETRE LARGEMENT AMELIORE (BOUCLES DO WHILE)
CC----------------------------------------------------------------------------------------------
cc
      DO I=1,TABCONPATCH%L_TAB_IG3D
        IEL=TABCONPATCH%TAB_IG3D(I)
        J=1
        K=OFFSET_NEWFCT
        DECALGEO=(TABCONPATCH%PID-1)*(NUMNOD+NBNEWX_TMP)
        DO WHILE(J<=KXIG3D(3,IEL))
          DO WHILE (IXIG3D(KXIG3D(4,IEL)+J-1)==0.AND.J<=KXIG3D(3,IEL))
            DO WHILE (IXIG3D(KXIG3D(4,IEL)+J-1)==0.AND.K<=L_TAB_NEWFCT-1)
c
              EL_CONNECT(IEL)=1 ! On devra reactualiser la table de connectivite de cet element
c
1000          K=K+1
c              IF(K>L_TAB_NEWFCT) CYCLE ! permet de securiser la boucle
              INCTRL = TAB_NEWFCT(K)
              DO L=1,L_TAB_REMOVE
                IF(TAB_REMOVE(L)==INCTRL) GOTO 1000
              ENDDO
              DO L=1,KXIG3D(3,IEL)
                IF(IXIG3D(KXIG3D(4,IEL)+L-1)==INCTRL) GOTO 1000
              ENDDO
c
              IF(KNOTLOCEL(1,DIR,IEL)<(KNOTLOCPC(1,DIR,DECALGEO+INCTRL)-TOL).OR.
     .           KNOTLOCEL(2,DIR,IEL)>(KNOTLOCPC(DEG+1,DIR,DECALGEO+INCTRL)+TOL)) CYCLE
              IF(KNOTLOCEL(1,DIRTANG1,IEL)<(KNOTLOCPC(1,DIRTANG1,DECALGEO+INCTRL)-TOL).OR.
     .           KNOTLOCEL(2,DIRTANG1,IEL)>(KNOTLOCPC(DEGTANG1+1,DIRTANG1,DECALGEO+INCTRL)+TOL)) CYCLE
              IF(KNOTLOCEL(1,DIRTANG2,IEL)<(KNOTLOCPC(1,DIRTANG2,DECALGEO+INCTRL)-TOL).OR.
     .           KNOTLOCEL(2,DIRTANG2,IEL)>(KNOTLOCPC(DEGTANG2+1,DIRTANG2,DECALGEO+INCTRL)+TOL)) CYCLE
              IXIG3D(KXIG3D(4,IEL)+J-1) = INCTRL
            ENDDO
            J=J+1
          ENDDO
          J=J+1
        ENDDO
cc
CC----------------------------------------------------------------------------------------------
cc    TRAITEMENT SIMILAIRE DES FILS DE CES ELEMENTS (ON POURRAIT COMPACTER LES TWO ENSEMBLES EN ONE)
CC----------------------------------------------------------------------------------------------
cc
        DO ITFILS=1,IDFILS(1,IEL)
          JEL=IDFILS(ITFILS+1,IEL)
          J=1
          K=OFFSET_NEWFCT
          DECALGEO=(TABCONPATCH%PID-1)*(NUMNOD+NBNEWX_TMP)
          DO WHILE(J<=KXIG3D(3,JEL))
            DO WHILE (IXIG3D(KXIG3D(4,JEL)+J-1)==0.AND.J<=KXIG3D(3,JEL))
              DO WHILE (IXIG3D(KXIG3D(4,JEL)+J-1)==0.AND.K<=L_TAB_NEWFCT-1)
c
                EL_CONNECT(JEL)=1 ! On devra reactualiser la table de connectivite de cet element
c
2000            K=K+1
c                IF(K>L_TAB_NEWFCT) CYCLE ! permet de securiser la boucle
                INCTRL = TAB_NEWFCT(K)
                DO L=1,L_TAB_REMOVE
                  IF(TAB_REMOVE(L)==INCTRL) GOTO 2000
                ENDDO
                DO L=1,KXIG3D(3,JEL)
                  IF(IXIG3D(KXIG3D(4,JEL)+L-1)==INCTRL) GOTO 2000
                ENDDO
c
                IF(KNOTLOCEL(1,DIR,JEL)<(KNOTLOCPC(1,DIR,DECALGEO+INCTRL)-TOL).OR.
     .             KNOTLOCEL(2,DIR,JEL)>(KNOTLOCPC(DEG+1,DIR,DECALGEO+INCTRL)+TOL)) CYCLE
                IF(KNOTLOCEL(1,DIRTANG1,JEL)<(KNOTLOCPC(1,DIRTANG1,DECALGEO+INCTRL)-TOL).OR.
     .             KNOTLOCEL(2,DIRTANG1,JEL)>(KNOTLOCPC(DEGTANG1+1,DIRTANG1,DECALGEO+INCTRL)+TOL)) CYCLE
                IF(KNOTLOCEL(1,DIRTANG2,JEL)<(KNOTLOCPC(1,DIRTANG2,DECALGEO+INCTRL)-TOL).OR.
     .             KNOTLOCEL(2,DIRTANG2,JEL)>(KNOTLOCPC(DEGTANG2+1,DIRTANG2,DECALGEO+INCTRL)+TOL)) CYCLE
                IXIG3D(KXIG3D(4,JEL)+J-1) = INCTRL
              ENDDO
              J=J+1
            ENDDO
            J=J+1
          ENDDO
        ENDDO
      ENDDO
cc
CC----------------------------------------------------------------------------------------------
cc   VERIFICATION QU'IL N'Y AIT PLUS DE 0 DANS LA TABLE DE CONNECTIVITE
cc   SINON C'EST QUE LE RAFFINEMENT N'EST PAS CORRECT (RISQUE DE SURNOMBRE
cc   DE FONCTIONS PAR ELEMENT)
CC----------------------------------------------------------------------------------------------
cc
      IF(FLAG_DEBUG==1) THEN
        DO I=1,SIXIG3D+ADDSIXIG3D
          IF(IXIG3D(I)==0) PRINT*,'IL Y A ONE ZERO', IXIG3D(I), I
        ENDDO
      ENDIF
c
      RETURN
      END


