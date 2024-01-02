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
Chd|  RAFIG3D                       source/elements/ige3d/rafig3d.F
Chd|-- called by -----------
Chd|        PRERAFIG3D                    source/elements/ige3d/prerafig3d.F
Chd|-- calls ---------------
Chd|        MESHSURFIG3D_MOD              source/elements/ige3d/meshsurfig3d_mod.F
Chd|====================================================================
      SUBROUTINE RAFIG3D(KNOTLOCPC,DEG,DEGTANG1,DEGTANG2,IAD_KNOT,
     .                   NKNOT1,NKNOT2,NKNOT3,GAMA,DIR,
     .                   NEWKNOT,X,D,V,MS,
     .                   WIGE,TAB_FCTCUT,L_TAB_FCTCUT,TAB_REMOVE,TAB_NEWFCT,
     .                   DECALGEO,TABCONPATCH,NUMPATCH,KXIG3D,
     .                   IXIG3D,TAB_STAY,FLAG_PRE)
C----------------------------------------------------------------------
C   ROUTINE QUI VA INSERER LE NOUVEAU KNOT DANS LE PATCH EN CREANT DE
C   NOUVELLES FONCTIONS, EN EN SUPPRIMANT CERTAINES, TOUT EN TENANT 
C   COMPTE DES FONCTIONS EXISTANTES, SUR LE PATCHS OU SUR LES AUTRES PATCHS
C   ET QUI MODIFIE LES COORDONNES X, V, D DES POINTS SI BESOIN
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXIG3D(*),KXIG3D(NIXIG3D,*),IAD_KNOT,NKNOT1,NKNOT2,NKNOT3,
     .        DEG,DEGTANG1,DEGTANG2,IEL,DIR,
     .        TAB_REMOVE(*),TAB_STAY(*),TAB_FCTCUT(*),
     .        L_TAB_FCTCUT,DECALGEO,TAB_NEWFCT(*),
     .        FLAG_PRE,NUMPATCH
      TYPE(TABCONPATCH_IG3D_), DIMENSION(*) :: TABCONPATCH
      my_real KNOTLOCPC(DEG_MAX,3,*),GAMA(*),NEWKNOT
      my_real X(3,*),V(3,*),D(3,*),MS(*),WIGE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ITFCT,OFFSET_KNOT,DIRTANG1,DIRTANG2,
     .        IDFCTCOUPEE,IDFCTCOUPEE1,IDFCTCOUPEE2,FLAG_REMOVE,ITNCTRL
      my_real ALPHA1,ALPHA2,NEWKNOTLOC(DEG+1,2),TOL
C=======================================================================
c  
      NEWKNOTLOC = 0
      TOL = EM06
c
      IF(DIR==1) THEN
        DIRTANG1 = 2
        DIRTANG2 = 3
        OFFSET_KNOT = IAD_KNOT
      ELSEIF(DIR==2) THEN
        DIRTANG1 = 3
        DIRTANG2 = 1
        OFFSET_KNOT = IAD_KNOT+NKNOT1
      ELSEIF(DIR==3) THEN
        DIRTANG1 = 1
        DIRTANG2 = 2
        OFFSET_KNOT = IAD_KNOT+NKNOT1+NKNOT2
      ENDIF
c
      DO ITFCT=1,L_TAB_FCTCUT

        IDFCTCOUPEE = TAB_FCTCUT(ITFCT)
c
        IF(NEWKNOT<KNOTLOCPC(1,DIR,DECALGEO+IDFCTCOUPEE).OR.
     .     NEWKNOT>KNOTLOCPC(DEG+1,DIR,DECALGEO+IDFCTCOUPEE)) CYCLE
c
C       CALCUL DES COEFFICIENTS ALPHA1 ET ALPHA2
c
        IF(NEWKNOT>=KNOTLOCPC(DEG,DIR,DECALGEO+IDFCTCOUPEE)) THEN 
          ALPHA1=1
        ELSE
          ALPHA1=(NEWKNOT-KNOTLOCPC(1,DIR,DECALGEO+IDFCTCOUPEE))/
     /           (KNOTLOCPC(DEG,DIR,DECALGEO+IDFCTCOUPEE)-KNOTLOCPC(1,DIR,DECALGEO+IDFCTCOUPEE))
        ENDIF
        IF(NEWKNOT<=KNOTLOCPC(2,DIR,DECALGEO+IDFCTCOUPEE)) THEN 
          ALPHA2=1
        ELSE
          ALPHA2=(KNOTLOCPC(DEG+1,DIR,DECALGEO+IDFCTCOUPEE)-NEWKNOT)/
     /           (KNOTLOCPC(DEG+1,DIR,DECALGEO+IDFCTCOUPEE)-KNOTLOCPC(2,DIR,DECALGEO+IDFCTCOUPEE))
        ENDIF
c
c       ASSEMBLAGE DES NOUVEAUX KNOT
c        
        I=DEG+1
        DO WHILE (KNOTLOCPC(I-1,DIR,DECALGEO+IDFCTCOUPEE)>=NEWKNOT)
          NEWKNOTLOC(I,1)=KNOTLOCPC(I-1,DIR,DECALGEO+IDFCTCOUPEE)
          NEWKNOTLOC(I,2)=KNOTLOCPC(I,DIR,DECALGEO+IDFCTCOUPEE)
          I=I-1
        ENDDO
        NEWKNOTLOC(I,1)=NEWKNOT
        NEWKNOTLOC(I,2)=KNOTLOCPC(I,DIR,DECALGEO+IDFCTCOUPEE)
        I=I-1
        NEWKNOTLOC(I,1)=KNOTLOCPC(I,DIR,DECALGEO+IDFCTCOUPEE)
        NEWKNOTLOC(I,2)=NEWKNOT
        I=I-1
        DO WHILE (I>2)
          NEWKNOTLOC(I,1)=KNOTLOCPC(I-1,DIR,DECALGEO+IDFCTCOUPEE)
          NEWKNOTLOC(I,2)=KNOTLOCPC(I,DIR,DECALGEO+IDFCTCOUPEE)
          I=I-1
        ENDDO
        DO WHILE (I>=1)
          NEWKNOTLOC(I,1)=KNOTLOCPC(I,DIR,DECALGEO+IDFCTCOUPEE)
          NEWKNOTLOC(I,2)=KNOTLOCPC(I+1,DIR,DECALGEO+IDFCTCOUPEE)
          I=I-1
        ENDDO
c
c       Recherche si la fonction rajoutee existe deja dans les anciens points
c
        DO J=1,L_TAB_FCTCUT
          IDFCTCOUPEE1 = TAB_FCTCUT(J)
          DO I=1,DEGTANG1+1
            IF(ABS(KNOTLOCPC(I,DIRTANG1,DECALGEO+IDFCTCOUPEE)-KNOTLOCPC(I,DIRTANG1,DECALGEO+IDFCTCOUPEE1))>TOL) EXIT
          ENDDO
          IF(I>DEGTANG1+1) THEN
           DO I=1,DEGTANG2+1
             IF(ABS(KNOTLOCPC(I,DIRTANG2,DECALGEO+IDFCTCOUPEE)-KNOTLOCPC(I,DIRTANG2,DECALGEO+IDFCTCOUPEE1))>TOL) EXIT
           ENDDO
          ENDIF
          IF(I>DEGTANG2+1) THEN
           DO I=1,DEG+1
             IF(ABS(NEWKNOTLOC(I,1)-KNOTLOCPC(I,DIR,DECALGEO+IDFCTCOUPEE1))>TOL) EXIT
           ENDDO
          ENDIF
          IF(I>DEG+1) EXIT
         ENDDO
         IF(I>DEG+1) THEN  ! La fonction existe deja
c
c      modification d'un ancien point existant
c
           IF(FLAG_PRE==1) THEN
            X(:,IDFCTCOUPEE1)=(X(:,IDFCTCOUPEE1)*GAMA(IDFCTCOUPEE1)+
     .                      X(:,IDFCTCOUPEE)*GAMA(IDFCTCOUPEE)*ALPHA1)/
     .                     (GAMA(IDFCTCOUPEE1)+ALPHA1*GAMA(IDFCTCOUPEE))
            D(:,IDFCTCOUPEE1)=(D(:,IDFCTCOUPEE1)*GAMA(IDFCTCOUPEE1)+
     .                      D(:,IDFCTCOUPEE)*GAMA(IDFCTCOUPEE)*ALPHA1)/
     .                     (GAMA(IDFCTCOUPEE1)+ALPHA1*GAMA(IDFCTCOUPEE))
            V(:,IDFCTCOUPEE1)=(V(:,IDFCTCOUPEE1)*GAMA(IDFCTCOUPEE1)+
     .                      V(:,IDFCTCOUPEE)*GAMA(IDFCTCOUPEE)*ALPHA1)/
     .                     (GAMA(IDFCTCOUPEE1)+ALPHA1*GAMA(IDFCTCOUPEE))
            MS(IDFCTCOUPEE1)=(MS(IDFCTCOUPEE1)*GAMA(IDFCTCOUPEE1)+
     .                      MS(IDFCTCOUPEE)*GAMA(IDFCTCOUPEE)*ALPHA1)/
     .                     (GAMA(IDFCTCOUPEE1)+ALPHA1*GAMA(IDFCTCOUPEE))
            WIGE(IDFCTCOUPEE1)=(WIGE(IDFCTCOUPEE1)*GAMA(IDFCTCOUPEE1)+
     .                      WIGE(IDFCTCOUPEE)*GAMA(IDFCTCOUPEE)*ALPHA1)/
     .                     (GAMA(IDFCTCOUPEE1)+ALPHA1*GAMA(IDFCTCOUPEE))
          ENDIF
          GAMA(IDFCTCOUPEE1) = GAMA(IDFCTCOUPEE1)+ALPHA1*GAMA(IDFCTCOUPEE)
          KNOTLOCPC(:,DIR,DECALGEO+IDFCTCOUPEE1) = NEWKNOTLOC(:,1)
        ELSEIF(I<=DEG+1) THEN  ! Il faut chercher dans les nouveaux points
         DO J=1,NEWFCT  ! les nouvelles fonctions de cette meshsurf
          IDFCTCOUPEE1 = NUMNODIGE0+OFFSET_NEWFCT+J
          DO I=1,DEGTANG1+1
            IF(ABS(KNOTLOCPC(I,DIRTANG1,DECALGEO+IDFCTCOUPEE)-KNOTLOCPC(I,DIRTANG1,DECALGEO+IDFCTCOUPEE1))>TOL) EXIT
          ENDDO
          IF(I>DEGTANG1+1) THEN
           DO I=1,DEGTANG2+1
             IF(ABS(KNOTLOCPC(I,DIRTANG2,DECALGEO+IDFCTCOUPEE)-KNOTLOCPC(I,DIRTANG2,DECALGEO+IDFCTCOUPEE1))>TOL) EXIT
           ENDDO
          ENDIF
          IF(I>DEGTANG2+1) THEN
           DO I=1,DEG+1
             IF(ABS(NEWKNOTLOC(I,1)-KNOTLOCPC(I,DIR,DECALGEO+IDFCTCOUPEE1))>TOL) EXIT
           ENDDO
          ENDIF
          IF(I>DEG+1) EXIT
         ENDDO
        ENDIF
        IF(I>DEG+1) THEN  ! La fonction existe deja
c
c      modification d'un point existant
c
          IF(FLAG_PRE==1) THEN
            X(:,NUMNODIGE0+OFFSET_NEWFCT+J)=(X(:,NUMNODIGE0+OFFSET_NEWFCT+J)*GAMA(IDFCTCOUPEE1)+
     .                      X(:,IDFCTCOUPEE)*GAMA(IDFCTCOUPEE)*ALPHA1)/
     .                     (GAMA(IDFCTCOUPEE1)+ALPHA1*GAMA(IDFCTCOUPEE))
            D(:,NUMNODIGE0+OFFSET_NEWFCT+J)=(D(:,NUMNODIGE0+OFFSET_NEWFCT+J)*GAMA(IDFCTCOUPEE1)+
     .                      D(:,IDFCTCOUPEE)*GAMA(IDFCTCOUPEE)*ALPHA1)/
     .                     (GAMA(IDFCTCOUPEE1)+ALPHA1*GAMA(IDFCTCOUPEE))
            V(:,NUMNODIGE0+OFFSET_NEWFCT+J)=(V(:,NUMNODIGE0+OFFSET_NEWFCT+J)*GAMA(IDFCTCOUPEE1)+
     .                      V(:,IDFCTCOUPEE)*GAMA(IDFCTCOUPEE)*ALPHA1)/
     .                     (GAMA(IDFCTCOUPEE1)+ALPHA1*GAMA(IDFCTCOUPEE))
            MS(NUMNODIGE0+OFFSET_NEWFCT+J)=(MS(NUMNODIGE0+OFFSET_NEWFCT+J)*GAMA(IDFCTCOUPEE1)+
     .                      MS(IDFCTCOUPEE)*GAMA(IDFCTCOUPEE)*ALPHA1)/
     .                     (GAMA(IDFCTCOUPEE1)+ALPHA1*GAMA(IDFCTCOUPEE))
            WIGE(NUMNODIGE0+OFFSET_NEWFCT+J)=(WIGE(NUMNODIGE0+OFFSET_NEWFCT+J)*GAMA(IDFCTCOUPEE1)+
     .                      WIGE(IDFCTCOUPEE)*GAMA(IDFCTCOUPEE)*ALPHA1)/
     .                     (GAMA(IDFCTCOUPEE1)+ALPHA1*GAMA(IDFCTCOUPEE))
          ENDIF
          GAMA(IDFCTCOUPEE1) = GAMA(IDFCTCOUPEE1)+ALPHA1*GAMA(IDFCTCOUPEE)
          KNOTLOCPC(:,DIR,DECALGEO+IDFCTCOUPEE1) = NEWKNOTLOC(:,1)
        ELSE
          NEWFCT = NEWFCT+1
          L_TAB_NEWFCT = L_TAB_NEWFCT+1
          IF(FLAG_PRE==1) THEN
            X(:,NUMNODIGE0+OFFSET_NEWFCT+NEWFCT)  = X(:,IDFCTCOUPEE)
            D(:,NUMNODIGE0+OFFSET_NEWFCT+NEWFCT)  = D(:,IDFCTCOUPEE)
            V(:,NUMNODIGE0+OFFSET_NEWFCT+NEWFCT)  = V(:,IDFCTCOUPEE)
            MS(NUMNODIGE0+OFFSET_NEWFCT+NEWFCT)   = MS(IDFCTCOUPEE)
            WIGE(NUMNODIGE0+OFFSET_NEWFCT+NEWFCT) = WIGE(IDFCTCOUPEE)
          ENDIF
          GAMA(NUMNODIGE0+OFFSET_NEWFCT+NEWFCT) = ALPHA1*GAMA(IDFCTCOUPEE)
          KNOTLOCPC(:,DIR,DECALGEO+NUMNODIGE0+OFFSET_NEWFCT+NEWFCT) = NEWKNOTLOC(:,1)
          KNOTLOCPC(:,DIRTANG1,DECALGEO+NUMNODIGE0+OFFSET_NEWFCT+NEWFCT) = KNOTLOCPC(:,DIRTANG1,DECALGEO+IDFCTCOUPEE)
          KNOTLOCPC(:,DIRTANG2,DECALGEO+NUMNODIGE0+OFFSET_NEWFCT+NEWFCT) = KNOTLOCPC(:,DIRTANG2,DECALGEO+IDFCTCOUPEE)
          TAB_NEWFCT(L_TAB_NEWFCT) = NUMNODIGE0+OFFSET_NEWFCT+NEWFCT 
        ENDIF
c
        DO J=1,L_TAB_FCTCUT
          IDFCTCOUPEE2 = TAB_FCTCUT(J)
          DO I=1,DEGTANG1+1
            IF(ABS(KNOTLOCPC(I,DIRTANG1,DECALGEO+IDFCTCOUPEE)-KNOTLOCPC(I,DIRTANG1,DECALGEO+IDFCTCOUPEE2))>TOL) EXIT
          ENDDO
          IF(I>DEGTANG1+1) THEN
           DO I=1,DEGTANG2+1
             IF(ABS(KNOTLOCPC(I,DIRTANG2,DECALGEO+IDFCTCOUPEE)-KNOTLOCPC(I,DIRTANG2,DECALGEO+IDFCTCOUPEE2))>TOL) EXIT
           ENDDO
          ENDIF
          IF(I>DEGTANG2+1) THEN
            DO I=1,DEG+1
             IF(ABS(NEWKNOTLOC(I,2)-KNOTLOCPC(I,DIR,DECALGEO+IDFCTCOUPEE2))>TOL) EXIT
           ENDDO
          ENDIF
          IF(I>DEG+1) EXIT
        ENDDO
        IF(I>DEG+1) THEN  ! La fonction existe deja
c
c      modification d'un point existant
c
          IF(FLAG_PRE==1) THEN
            X(:,IDFCTCOUPEE2)=(X(:,IDFCTCOUPEE2)*GAMA(IDFCTCOUPEE2)+
     .                      X(:,IDFCTCOUPEE)*GAMA(IDFCTCOUPEE)*ALPHA2)/
     .                      (GAMA(IDFCTCOUPEE2)+ALPHA2*GAMA(IDFCTCOUPEE))
            D(:,IDFCTCOUPEE2)=(D(:,IDFCTCOUPEE2)*GAMA(IDFCTCOUPEE2)+
     .                      D(:,IDFCTCOUPEE)*GAMA(IDFCTCOUPEE)*ALPHA2)/
     .                      (GAMA(IDFCTCOUPEE2)+ALPHA2*GAMA(IDFCTCOUPEE))
            V(:,IDFCTCOUPEE2)=(V(:,IDFCTCOUPEE2)*GAMA(IDFCTCOUPEE2)+
     .                      V(:,IDFCTCOUPEE)*GAMA(IDFCTCOUPEE)*ALPHA2)/
     .                      (GAMA(IDFCTCOUPEE2)+ALPHA2*GAMA(IDFCTCOUPEE))
            MS(IDFCTCOUPEE2)=(MS(IDFCTCOUPEE2)*GAMA(IDFCTCOUPEE2)+
     .                      MS(IDFCTCOUPEE)*GAMA(IDFCTCOUPEE)*ALPHA2)/
     .                      (GAMA(IDFCTCOUPEE2)+ALPHA2*GAMA(IDFCTCOUPEE))
            WIGE(IDFCTCOUPEE2)=(WIGE(IDFCTCOUPEE2)*GAMA(IDFCTCOUPEE2)+
     .                      WIGE(IDFCTCOUPEE)*GAMA(IDFCTCOUPEE)*ALPHA2)/
     .                      (GAMA(IDFCTCOUPEE2)+ALPHA2*GAMA(IDFCTCOUPEE))
          ENDIF
          GAMA(IDFCTCOUPEE2) = GAMA(IDFCTCOUPEE2)+ALPHA2*GAMA(IDFCTCOUPEE)
          KNOTLOCPC(:,DIR,DECALGEO+IDFCTCOUPEE2) = NEWKNOTLOC(:,2)
        ELSEIF(I<=DEG+1) THEN  ! Il faut chercher dans les nouveaux points
         DO J=1,NEWFCT  ! les nouvelles fonctions de cette meshsurf
          IDFCTCOUPEE2 = NUMNODIGE0+OFFSET_NEWFCT+J
          DO I=1,DEGTANG1+1
            IF(ABS(KNOTLOCPC(I,DIRTANG1,DECALGEO+IDFCTCOUPEE)-KNOTLOCPC(I,DIRTANG1,DECALGEO+IDFCTCOUPEE2))>TOL) EXIT
          ENDDO
          IF(I>DEGTANG1+1) THEN
           DO I=1,DEGTANG2+1
             IF(ABS(KNOTLOCPC(I,DIRTANG2,DECALGEO+IDFCTCOUPEE)-KNOTLOCPC(I,DIRTANG2,DECALGEO+IDFCTCOUPEE2))>TOL) EXIT
           ENDDO
          ENDIF
          IF(I>DEGTANG2+1) THEN
           DO I=1,DEG+1
             IF(ABS(NEWKNOTLOC(I,2)-KNOTLOCPC(I,DIR,DECALGEO+IDFCTCOUPEE2))>TOL) EXIT
           ENDDO
          ENDIF
          IF(I>DEG+1) EXIT
         ENDDO
        ENDIF
        IF(I>DEG+1) THEN  ! La fonction existe deja
c
c      modification d'un point existant
c
          IF(FLAG_PRE==1) THEN
            X(:,NUMNODIGE0+OFFSET_NEWFCT+J)=(X(:,NUMNODIGE0+OFFSET_NEWFCT+J)*GAMA(IDFCTCOUPEE2)+
     .                      X(:,IDFCTCOUPEE)*GAMA(IDFCTCOUPEE)*ALPHA2)/
     .                      (GAMA(IDFCTCOUPEE2)+ALPHA2*GAMA(IDFCTCOUPEE))
            D(:,NUMNODIGE0+OFFSET_NEWFCT+J)=(D(:,NUMNODIGE0+OFFSET_NEWFCT+J)*GAMA(IDFCTCOUPEE2)+
     .                      D(:,IDFCTCOUPEE)*GAMA(IDFCTCOUPEE)*ALPHA2)/
     .                      (GAMA(IDFCTCOUPEE2)+ALPHA2*GAMA(IDFCTCOUPEE))
            V(:,NUMNODIGE0+OFFSET_NEWFCT+J)=(V(:,NUMNODIGE0+OFFSET_NEWFCT+J)*GAMA(IDFCTCOUPEE2)+
     .                      V(:,IDFCTCOUPEE)*GAMA(IDFCTCOUPEE)*ALPHA2)/
     .                      (GAMA(IDFCTCOUPEE2)+ALPHA2*GAMA(IDFCTCOUPEE))
            MS(NUMNODIGE0+OFFSET_NEWFCT+J)=(MS(NUMNODIGE0+OFFSET_NEWFCT+J)*GAMA(IDFCTCOUPEE2)+
     .                      MS(IDFCTCOUPEE)*GAMA(IDFCTCOUPEE)*ALPHA2)/
     .                      (GAMA(IDFCTCOUPEE2)+ALPHA2*GAMA(IDFCTCOUPEE))
            WIGE(NUMNODIGE0+OFFSET_NEWFCT+J)=(WIGE(NUMNODIGE0+OFFSET_NEWFCT+J)*GAMA(IDFCTCOUPEE2)+
     .                      WIGE(IDFCTCOUPEE)*GAMA(IDFCTCOUPEE)*ALPHA2)/
     .                      (GAMA(IDFCTCOUPEE2)+ALPHA2*GAMA(IDFCTCOUPEE))
          ENDIF
          GAMA(IDFCTCOUPEE2) = GAMA(IDFCTCOUPEE2)+ALPHA2*GAMA(IDFCTCOUPEE)
          KNOTLOCPC(:,DIR,DECALGEO+IDFCTCOUPEE2) = NEWKNOTLOC(:,2)
        ELSE
          NEWFCT = NEWFCT+1
          L_TAB_NEWFCT = L_TAB_NEWFCT+1
          IF(FLAG_PRE==1) THEN
            X(:,NUMNODIGE0+OFFSET_NEWFCT+NEWFCT)  = X(:,IDFCTCOUPEE)
            D(:,NUMNODIGE0+OFFSET_NEWFCT+NEWFCT)  = D(:,IDFCTCOUPEE)
            V(:,NUMNODIGE0+OFFSET_NEWFCT+NEWFCT)  = V(:,IDFCTCOUPEE)
            MS(NUMNODIGE0+OFFSET_NEWFCT+NEWFCT)   = MS(IDFCTCOUPEE)
            WIGE(NUMNODIGE0+OFFSET_NEWFCT+NEWFCT) = WIGE(IDFCTCOUPEE)
          ENDIF
          GAMA(NUMNODIGE0+OFFSET_NEWFCT+NEWFCT) = ALPHA2*GAMA(IDFCTCOUPEE)
          KNOTLOCPC(:,DIR,DECALGEO+NUMNODIGE0+OFFSET_NEWFCT+NEWFCT) = NEWKNOTLOC(:,2)
          KNOTLOCPC(:,DIRTANG1,DECALGEO+NUMNODIGE0+OFFSET_NEWFCT+NEWFCT) = KNOTLOCPC(:,DIRTANG1,DECALGEO+IDFCTCOUPEE)
          KNOTLOCPC(:,DIRTANG2,DECALGEO+NUMNODIGE0+OFFSET_NEWFCT+NEWFCT) = KNOTLOCPC(:,DIRTANG2,DECALGEO+IDFCTCOUPEE)
          TAB_NEWFCT(L_TAB_NEWFCT) = NUMNODIGE0+OFFSET_NEWFCT+NEWFCT 
        ENDIF
c
ccc si la fonction est sur un autre patch aussi, elle ne doit pas etre supprimee. 
ccc Elle sera supprimee de la connectivite du patch raffine mais restera
ccc avec son numero de fct pour le patch voisin
c
        FLAG_REMOVE=1
        DO I=1,NBPART_IG3D
          IF(I==NUMPATCH) CYCLE ! pas sur le patch sur lequel on travaille 
          DO J=1,TABCONPATCH(I)%L_TAB_IG3D
            DO ITNCTRL=1,KXIG3D(3,TABCONPATCH(I)%TAB_IG3D(J))
              IF(IXIG3D(KXIG3D(4,TABCONPATCH(I)%TAB_IG3D(J))+ITNCTRL-1)==IDFCTCOUPEE) THEN
c                print*,'finallement on garde', IDFCTCOUPEE
                FLAG_REMOVE=0
              ENDIF
            ENDDO
          ENDDO
        ENDDO

        IF(FLAG_REMOVE==1) THEN
          L_TAB_REMOVE = L_TAB_REMOVE+1
          TAB_REMOVE(L_TAB_REMOVE) = IDFCTCOUPEE
        ELSE
          L_TAB_STAY = L_TAB_STAY+1
          TAB_STAY(L_TAB_STAY) = IDFCTCOUPEE
        ENDIF
c
      ENDDO
c
      RETURN
      END
c

