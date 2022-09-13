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
Chd|  REORDER_IG3D                  source/elements/ige3d/reorder_ig3d.F
Chd|-- called by -----------
Chd|        PRERAFIG3D                    source/elements/ige3d/prerafig3d.F
Chd|-- calls ---------------
Chd|        MYQSORT3D                     source/elements/ige3d/searchigeo3d.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|====================================================================
      SUBROUTINE REORDER_IG3D(IXIG3D, KXIG3D,KNOTLOCPC,KNOTLOCEL,
     .                        X_TMP,D_TMP,V_TMP,MS_TMP,WIGE_TMP,
     .                        TAB_REMOVE,TAB_NEWFCT,EL_CONNECT,
     .                        IPARTIG3D,IGEO,TAB_STAY,FLAG_PRE,FLAG_DEBUG)
C----------------------------------------------------------------------
C   ROUTINE QUI REORDONNE LES TABLEAUX DE POINTS X, KNOTLOCPC, D, V ETC 
C   EN COMPACTANT : ENLEVE LES POINTS SUPPRIMES, RAJOUTE LES NOUVEAUX,
C   SANS LAISSER DE TROU
C   LA ROUTINE TRIE EGALEMENT LES DOUBLONS EN BORD DE PATCHS, MAIS NE 
C   SUPPRIME PAS COMPLETEMENT LES POINTS (DESACTIVATION)
C   ELLE REORDONNE EN FIN LES TABLES DE CONNECTIVITES DE TOUT LES ELEMENTS
C   QUI ONT ETE MODIFIES
C----------------------------------------------------------------------
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
      INTEGER IXIG3D(*),KXIG3D(NIXIG3D,*),IGEO(NPROPGI,*),
     .        IPARTIG3D(*),TAB_NEWFCT(*),TAB_REMOVE(*),
     .        TAB_STAY(*),FLAG_PRE,EL_CONNECT(*),FLAG_DEBUG
      my_real KNOTLOCPC(DEG_MAX,3,*),KNOTLOCEL(2,3,*)
      my_real X_TMP(3,*),V_TMP(3,*),D_TMP(3,*),MS_TMP(*),WIGE_TMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,M,IAD_IXIG3D,INCTRL,L_TABWORK,WORK(70000),
     .        NDOUBLON_REMOVE, NDOUBLON_NEWFCT, L_REAL_REMOVE, L_REAL_NEWFCT,NVALEURS,
     .        DECALIXIG3D,DECALGEO,DECALGEOFINAL,
     .        NZERO_REMOVE,NZERO_NEWFCT,ITPATCH,NUMPCSTAY,NUMPCLEAVE,
     .        NCTRL,IPID,PX,PY,PZ,NDOUBLONIGE,ITNCTRL,INCTRL2,INCTRL3,INCTRL4
      INTEGER TMPZ(4),TMPZY(4),TABPOSZ(64),TABPOSZY(64),TABPOSZYX(64)
      my_real TOL
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX, TAB_REMOVE_TRI, TAB_NEWFCT_TRI
      INTEGER, DIMENSION(:), ALLOCATABLE :: PERMIGE
      my_real, DIMENSION(:,:), ALLOCATABLE :: X_TRIE
C=======================================================================
C
      TOL=EM06
c
c ------------------------------------------------------------------------------------------
CC   TRI DE MANIERE CROISSANTE LES TABLEAUX REMOVE ET NEWFCT
c ------------------------------------------------------------------------------------------
c
      ALLOCATE(TAB_REMOVE_TRI(L_TAB_REMOVE)) 
      TAB_REMOVE_TRI(:) = 0
      ALLOCATE(INDEX(2*L_TAB_REMOVE))
      CALL MY_ORDERS(0, WORK, TAB_REMOVE, INDEX, L_TAB_REMOVE , 1)
      DO I=1,L_TAB_REMOVE
        TAB_REMOVE_TRI(I)=TAB_REMOVE(INDEX(I))
      ENDDO
      DEALLOCATE(INDEX)
      DO I=1,L_TAB_REMOVE
        TAB_REMOVE(I) = 0
      ENDDO
C
      ALLOCATE(TAB_NEWFCT_TRI(L_TAB_NEWFCT)) 
      TAB_NEWFCT_TRI(:) = 0
      ALLOCATE(INDEX(2*L_TAB_NEWFCT))
      CALL MY_ORDERS(0, WORK, TAB_NEWFCT, INDEX, L_TAB_NEWFCT , 1)
      DO I=1,L_TAB_NEWFCT
        TAB_NEWFCT_TRI(I)=TAB_NEWFCT(INDEX(I))
      ENDDO
      DEALLOCATE(INDEX)
      DO I=1,L_TAB_NEWFCT
        TAB_NEWFCT(I) = 0
      ENDDO
c
c ------------------------------------------------------------------------------------------
CC   ELIMINATION DES DOUBLONS S'IL Y A PLUSIEURS PATCHS EN CONTACT 
CC   (ON PEUT FAIRE ONE FLAG QUI LE REND OPTIONNEL)
c ------------------------------------------------------------------------------------------
c
      NDOUBLON_REMOVE = 0
      IF(NBPART_IG3D>1) THEN
       I = 1
       DO WHILE (I<=L_TAB_REMOVE-NDOUBLON_REMOVE-1)
         NVALEURS = 0
         TAB_REMOVE(I) = TAB_REMOVE_TRI(I+NDOUBLON_REMOVE)
         DO WHILE (((I+NDOUBLON_REMOVE+NVALEURS+1)<=L_TAB_REMOVE)
     .      .AND. (TAB_REMOVE(I+NDOUBLON_REMOVE)==TAB_REMOVE(I+NDOUBLON_REMOVE+NVALEURS+1)))
           NVALEURS = NVALEURS + 1
         ENDDO
         NDOUBLON_REMOVE = NDOUBLON_REMOVE + NVALEURS
         I = I + 1
       ENDDO
      ENDIF
c
      L_REAL_REMOVE = L_TAB_REMOVE - NDOUBLON_REMOVE
c   
      NDOUBLON_NEWFCT = 0
      IF(NBPART_IG3D>1) THEN
       I = 1
       DO WHILE (I<=L_TAB_NEWFCT-NDOUBLON_NEWFCT-1)
         NVALEURS = 0
         TAB_NEWFCT(I) = TAB_NEWFCT_TRI(I+NDOUBLON_NEWFCT)
         DO WHILE (((I+NDOUBLON_NEWFCT+NVALEURS+1)<=L_TAB_NEWFCT)
     .      .AND. (TAB_NEWFCT(I+NDOUBLON_NEWFCT)==TAB_NEWFCT(I+NDOUBLON_NEWFCT+NVALEURS+1)))
           NVALEURS = NVALEURS + 1
         ENDDO
         NDOUBLON_NEWFCT = NDOUBLON_NEWFCT + NVALEURS
         I = I + 1
       ENDDO
      ENDIF
c
      L_REAL_NEWFCT = L_TAB_NEWFCT - NDOUBLON_NEWFCT
c     
c ------------------------------------------------------------------------------------------
C   SUPPRESSION DES VALEURS COMMUNES AUX TWO LISTES (LES POINTS CREES PUIS SUPPRIMES)
c ------------------------------------------------------------------------------------------
c
      I=1
      DO WHILE (I<=L_REAL_REMOVE)
        J=1
        DO WHILE (J<=L_REAL_NEWFCT)
          IF(TAB_REMOVE_TRI(I)==TAB_NEWFCT_TRI(J)) THEN
            TAB_REMOVE_TRI(I) = 0
            TAB_NEWFCT_TRI(J) = 0
          ENDIF
          J=J+1
        ENDDO
        I=I+1
      ENDDO
c
      I = 1
      NZERO_REMOVE = 0
      DO WHILE (I<=L_REAL_REMOVE-NZERO_REMOVE)
        NVALEURS = 0
        DO WHILE ((I+NZERO_REMOVE+NVALEURS)<=L_REAL_REMOVE.AND.
     .             TAB_REMOVE_TRI(I+NZERO_REMOVE+NVALEURS)==0)
          NVALEURS=NVALEURS+1
        ENDDO
        NZERO_REMOVE = NZERO_REMOVE + NVALEURS
        TAB_REMOVE_TRI(I) = TAB_REMOVE_TRI(I+NZERO_REMOVE)
        I = I + 1 
      ENDDO
c
      I = 1
      NZERO_NEWFCT = 0
      DO WHILE (I<=L_REAL_NEWFCT-NZERO_NEWFCT)
        NVALEURS = 0
        DO WHILE ((I+NZERO_NEWFCT+NVALEURS)<=L_REAL_NEWFCT.AND.
     .             TAB_NEWFCT_TRI(I+NZERO_NEWFCT+NVALEURS)==0)
          NVALEURS=NVALEURS+1
        ENDDO
        NZERO_NEWFCT = NZERO_NEWFCT + NVALEURS
        TAB_NEWFCT_TRI(I) = TAB_NEWFCT_TRI(I+NZERO_NEWFCT)
        I = I + 1 
      ENDDO
c
      L_REAL_REMOVE = L_TAB_REMOVE - NZERO_REMOVE
      L_REAL_NEWFCT = L_TAB_NEWFCT - NZERO_NEWFCT
      NBNEWX_FINAL = L_REAL_NEWFCT - L_REAL_REMOVE  !  attention il faudra s'il y avait des 0 concatener
c
c ------------------------------------------------------------------------------------------
C   PUIS IL FAUT REMPLACER CHAQUE POINT DE REMOVE PAR LES POINTS DE NEWS ET QUAND IL N'Y A PLUS DE PLACE
C   ON COMPTE LE NOMBRE REEL DE POINT AJOUTES ET ON REMPLIT AVEC NUMNODO + ITNBNEWX
C
CC  MODIFICATION DES CONNECTIVITES DES ELEMENTS ET DES DONNES DES POINTS DE CONTROLE
c ------------------------------------------------------------------------------------------
c
      IF(FLAG_PRE==1) THEN

        I=1
        DO WHILE (I<=L_REAL_REMOVE)
          X_TMP(:,TAB_REMOVE_TRI(I)) = X_TMP(:,TAB_NEWFCT_TRI(I))  ! X a pile poil la bonne taille
          D_TMP(:,TAB_REMOVE_TRI(I)) = D_TMP(:,TAB_NEWFCT_TRI(I))
          V_TMP(:,TAB_REMOVE_TRI(I)) = V_TMP(:,TAB_NEWFCT_TRI(I))
          MS_TMP(TAB_REMOVE_TRI(I)) = MS_TMP(TAB_NEWFCT_TRI(I))
          WIGE_TMP(TAB_REMOVE_TRI(I)) = WIGE_TMP(TAB_NEWFCT_TRI(I))
          DO ITPATCH=1,NUMGEO  ! SI ON DECALE ONE POINT, C'EST QU'IL EST SUR ONE SEUL PATCH, DONC ON VA DECALER TOUT LES KNOT
cc DE CE POINT SUR TOUT LES PATCH POUR ETRE SUR, MAIS IL AURAIT FALU JUSTE SAVOIR SUR LEQUEL IL EST POU RNE PAS 
C DECALER DES 0
            DECALGEO=(ITPATCH-1)*(NUMNOD+NBNEWX_TMP) ! nb total de point ajoute avec ceux qu'on supprime
            KNOTLOCPC(:,1,DECALGEO+TAB_REMOVE_TRI(I)) = KNOTLOCPC(:,1,DECALGEO+TAB_NEWFCT_TRI(I))
            KNOTLOCPC(:,2,DECALGEO+TAB_REMOVE_TRI(I)) = KNOTLOCPC(:,2,DECALGEO+TAB_NEWFCT_TRI(I))
            KNOTLOCPC(:,3,DECALGEO+TAB_REMOVE_TRI(I)) = KNOTLOCPC(:,3,DECALGEO+TAB_NEWFCT_TRI(I))
          ENDDO
          J=1
          DO WHILE(J<=SIXIG3D+ADDSIXIG3D)
            DO WHILE(IXIG3D(J)==TAB_NEWFCT_TRI(I).AND.J<=SIXIG3D+ADDSIXIG3D)
c              print*,'IXIG3D = ', IXIG3D(J),'IXIG3D final = ', TAB_REMOVE_TRI(I)
              IXIG3D(J)=TAB_REMOVE_TRI(I)
              J=J+1
            ENDDO
            J=J+1
          ENDDO
          I=I+1
        ENDDO
c
        J=1      
        DO WHILE (I<=L_REAL_NEWFCT)
          X_TMP(:,NUMNODIGE0+J) = X_TMP(:,TAB_NEWFCT_TRI(I))
          D_TMP(:,NUMNODIGE0+J) = D_TMP(:,TAB_NEWFCT_TRI(I))
          V_TMP(:,NUMNODIGE0+J) = V_TMP(:,TAB_NEWFCT_TRI(I))
          MS_TMP(NUMNODIGE0+J) = MS_TMP(TAB_NEWFCT_TRI(I))
          WIGE_TMP(NUMNODIGE0+J) = WIGE_TMP(TAB_NEWFCT_TRI(I))
          DO ITPATCH=1,NUMGEO
            DECALGEO=(ITPATCH-1)*(NUMNOD+NBNEWX_TMP) ! nb total de point ajoute avec ceux qu'on supprime
            KNOTLOCPC(:,1,DECALGEO+NUMNODIGE0+J) = KNOTLOCPC(:,1,DECALGEO+TAB_NEWFCT_TRI(I))
            KNOTLOCPC(:,2,DECALGEO+NUMNODIGE0+J) = KNOTLOCPC(:,2,DECALGEO+TAB_NEWFCT_TRI(I))
            KNOTLOCPC(:,3,DECALGEO+NUMNODIGE0+J) = KNOTLOCPC(:,3,DECALGEO+TAB_NEWFCT_TRI(I))
          ENDDO
          K=1
          DO WHILE(K<=SIXIG3D+ADDSIXIG3D)
            DO WHILE(IXIG3D(K)==TAB_NEWFCT_TRI(I).AND.K<=SIXIG3D+ADDSIXIG3D)
c              print*,'IXIG3D = ', IXIG3D(K),'IXIG3D final = ', NUMNODIGE0+J
              IXIG3D(K)=NUMNODIGE0+J
              K=K+1
            ENDDO
            K=K+1
          ENDDO
          I=I+1
          J=J+1
        ENDDO
c
c ------------------------------------------------------------------------------------------
c  TEST DE DOUBLON GEOMETRIQUE (CAUSE PAR DU RAFFINEMENT DE MULTIPATCH COLLES)
C  MERGE EN CAS DE BESOIN
C  ATTENTION A BIEN RAMENER LES INFORMATIONS SUR CHACUN DES PATCHS
C
C  ATTENTION : TWO PATCHS PEUVENT AVOIR DES POINTS AU MEME ENDROIT,
C  MAIS POURRAIENT NE PAS AVOIR A ETRE CONNECTES
C  LE MERGE DE CES TWO POINTS CAUSERAIT ONE SOUCIS
C  ATTENTION2 : NE SUPPRIME PAS LES DOUBLONS DU MODELE : ILS SONT SEULEMENT DESACTIVES
c ------------------------------------------------------------------------------------------
c
        IF(NBPART_IG3D>1) THEN !  ET SI ON A GARDE DES POINTS (CONDITION QUI VIENT DE RAFIG3D.F)
          ALLOCATE(PERMIGE(NUMNOD))
          ALLOCATE(X_TRIE(3,NUMNOD))
          DO I=1,NUMNOD
            X_TRIE(:,I) = X_TMP(:,I)
          ENDDO      
          CALL MYQSORT3D(NUMNOD,X_TRIE,PERMIGE)
c
          I = 1
          NDOUBLONIGE = 0
          DO WHILE (I <= NUMNOD-NDOUBLONIGE-1)
            NVALEURS = 0
            DO WHILE (((I+NDOUBLONIGE+NVALEURS+1) <= NUMNOD)
     .       .AND. (ABS(X_TRIE(1,I+NDOUBLONIGE)-X_TRIE(1,I+NDOUBLONIGE+NVALEURS+1)) <= TOL)
     .       .AND. (ABS(X_TRIE(2,I+NDOUBLONIGE)-X_TRIE(2,I+NDOUBLONIGE+NVALEURS+1)) <= TOL)
     .       .AND. (ABS(X_TRIE(3,I+NDOUBLONIGE)-X_TRIE(3,I+NDOUBLONIGE+NVALEURS+1)) <= TOL))
c
c ------------------------------------------------------------------------------------------
CC TEST POUR SAVOIR LEQUEL DES POINTS DOUBLONS EST A GARDER
CC ET MODIFICATION DES DONNES DES POINTS DE CONTROLE ET DES TABLES
CC DE CONNECTIVITES
c ------------------------------------------------------------------------------------------
c
              NUMPCSTAY =0
              NUMPCLEAVE=0
              DO K=1,L_TAB_STAY
                IF(PERMIGE(I+NDOUBLONIGE)==TAB_STAY(K)) THEN
                  NUMPCSTAY = PERMIGE(I+NDOUBLONIGE)
                  NUMPCLEAVE = PERMIGE(I+NDOUBLONIGE+NVALEURS+1)
                  EXIT
                ENDIF
                IF(PERMIGE(I+NDOUBLONIGE+NVALEURS+1)==TAB_STAY(K)) THEN
                  NUMPCSTAY = PERMIGE(I+NDOUBLONIGE+NVALEURS+1)
                  NUMPCLEAVE = PERMIGE(I+NDOUBLONIGE)
                  EXIT
                ENDIF
              ENDDO
              IF(NUMPCSTAY==0.AND.NUMPCLEAVE==0) THEN ! LES TWO POINTS SONT A SUPPRIMER
                NUMPCSTAY = PERMIGE(I+NDOUBLONIGE)
                NUMPCLEAVE = PERMIGE(I+NDOUBLONIGE+NVALEURS+1)
c                print*,'jai supprime au hasard ',NUMPCLEAVE
c              ELSE
c                print*,'jai supprime',NUMPCLEAVE,' et j ai garde',NUMPCSTAY
              ENDIF
c
              J=1
              DO WHILE(J<=SIXIG3D+ADDSIXIG3D)
                DO WHILE(IXIG3D(J)==NUMPCLEAVE.AND.J<=SIXIG3D+ADDSIXIG3D)
                  IXIG3D(J)=NUMPCSTAY
                  J=J+1
                ENDDO
                J=J+1
              ENDDO

ccc il faut savoir de quel patch vient le point qu'on supprime et de quel patch est le point qui va le remplacer 

              DO ITPATCH=1,NBPART_IG3D
                DECALGEO=(ITPATCH-1)*(NUMNOD+NBNEWX_TMP)
c  on ramene les knotlocpc des patch lorsque l'etendue est non nulle pour ce patch
                DO K=1,4 ! test des etendues knot (vide ou non)
                  IF(KNOTLOCPC(K,1,DECALGEO+NUMPCLEAVE)/=0) THEN
                    KNOTLOCPC(:,1,DECALGEO+NUMPCSTAY)=KNOTLOCPC(:,1,DECALGEO+NUMPCLEAVE)
                    KNOTLOCPC(:,2,DECALGEO+NUMPCSTAY)=KNOTLOCPC(:,2,DECALGEO+NUMPCLEAVE)
                    KNOTLOCPC(:,3,DECALGEO+NUMPCSTAY)=KNOTLOCPC(:,3,DECALGEO+NUMPCLEAVE)
                    EXIT
                  ENDIF
                ENDDO
              ENDDO

              NVALEURS = NVALEURS + 1

            ENDDO
            NDOUBLONIGE = NDOUBLONIGE + NVALEURS
            I = I + 1
          ENDDO

C  changer pour ne pas ecrire de facon croissante mais simplement eliminer les points doublons
c
c        IF(NDOUBLONIGE>0) print*, 'J AI TROUVE ET SUPPRIME DES CONNECTIVITES :',NDOUBLONIGE,' DOUBLONS !'
c
        DEALLOCATE(PERMIGE)
        DEALLOCATE(X_TRIE)
      ENDIF
c ------------------------------------------------------------------------------------------
      ENDIF
c
      DEALLOCATE(TAB_REMOVE_TRI)
      DEALLOCATE(TAB_NEWFCT_TRI)
c
      IF(FLAG_PRE==1) THEN   
c
C-----------------------------------------------
CC ON RE-ORDONNE LA TABLE DE CONNECTIVITE DES ELEMENTS : 
CC PEUT TRAITER DES ELEMENTS MAXI DE DEGRE 3*3*3
CC (TMPZ(4),TMPZY(4),TABPOSZ(64),TABPOSZY(64),TABPOSZYX(64))
C-----------------------------------------------
c
      DO I=1,NUMELIG3D0+ADDELIG3D ! TAILLE DE EL_CONNECT
c
        IF(EL_CONNECT(I)/=1) CYCLE
c
        DECALIXIG3D=KXIG3D(4,I)
        DECALGEO=(KXIG3D(2,I)-1)*(NUMNOD+NBNEWX_TMP)
        NCTRL=KXIG3D(3,I)
        IPID=IPARTIG3D(I)
        PX = IGEO(41,IPID)
        PY = IGEO(42,IPID)
        PZ = IGEO(43,IPID)
C 
        TABPOSZ(:)=0
        TMPZ(:)=0
        DO J=1,NCTRL ! POUR CHACUNE DES (PX*PY*PZ) FONCTIONS
          DO K=1,PZ
            IF(KNOTLOCPC(K,3,DECALGEO+IXIG3D(DECALIXIG3D+J-1))<KNOTLOCEL(1,3,I)+TOL.AND.
     .         KNOTLOCPC(K+1,3,DECALGEO+IXIG3D(DECALIXIG3D+J-1))>KNOTLOCEL(2,3,I)-TOL) THEN
c la position en Z de cette fonction est K : 1 la fonction est en haut, PZ la fonction est en bas
               TMPZ(K)=TMPZ(K)+1
               TABPOSZ((K-1)*(PX*PY)+TMPZ(K)) = IXIG3D(DECALIXIG3D+J-1)
            ENDIF
          ENDDO
        ENDDO
C
        TABPOSZY(:)=0
        DO J=1,PZ ! sur chacun des PZ niveaux
          TMPZY(:)=0
          DO K=1,PX*PY  ! POUR CHACUNE DES (PX*PY) FONCTIONS
            DO L=1,PY
              IF(KNOTLOCPC(L,2,DECALGEO+TABPOSZ((J-1)*(PX*PY)+K))<KNOTLOCEL(1,2,I)+TOL.AND.
     .           KNOTLOCPC(L+1,2,DECALGEO+TABPOSZ((J-1)*(PX*PY)+K))>KNOTLOCEL(2,2,I)-TOL) THEN
c la position en Y de cette fonction est L : 1 la fonction est a droite, PY la fonction est a gauche
                TMPZY(L)=TMPZY(L)+1
                TABPOSZY((J-1)*(PX*PY)+(L-1)*PY+TMPZY(L)) = TABPOSZ((J-1)*(PX*PY)+K)
              ENDIF
            ENDDO
          ENDDO
        ENDDO
C
        TABPOSZYX(:)=0
        DO J=1,PZ ! sur chacun des PZ niveaux
          DO K=1,PY ! sur chacune des PY lignes
            DO L=1,PX ! POUR CHACUNE DES PX FONCTIONS
              DO M=1,PX
                IF(KNOTLOCPC(M,1,DECALGEO+TABPOSZY((J-1)*(PX*PY)+(K-1)*PY+L))<KNOTLOCEL(1,1,I)+TOL.AND.
     .             KNOTLOCPC(M+1,1,DECALGEO+TABPOSZY((J-1)*(PX*PY)+(K-1)*PY+L))>KNOTLOCEL(2,1,I)-TOL) THEN
c la position en X de cette fonction est M : 1 la fonction est devant, PX la fonction est derriere
                  TABPOSZYX((J-1)*(PX*PY)+(K-1)*PY+M) = TABPOSZY((J-1)*(PX*PY)+(K-1)*PY+L)
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDDO
C
C-----------------------------------------------
CC REECRICUTE DANS LA TABLE DE CONNECTIVITE GLOBALE DES 
CC ELEMENTS ISOGEOMETRIQUES
C-----------------------------------------------
C
        DO J=1,NCTRL
          IXIG3D(DECALIXIG3D+J-1)=TABPOSZYX(J)
        ENDDO
C
C-----------------------------------------------
CC VERIFICATION DE LA REORGANISATION DES TABLES DE CONNECTIVITE
CC EN FONCTION DES ETENDUES KNOT DE L'ELEMENTS ET DES PC
C-----------------------------------------------
C
        IF(FLAG_DEBUG==1) THEN
         DO J=1,KXIG3D(3,I)
           INCTRL=IXIG3D(KXIG3D(4,I)+J-1)
           IF(KNOTLOCEL(1,1,I)<KNOTLOCPC(1,1,DECALGEO+INCTRL)-EM06 .OR.
     .        KNOTLOCEL(2,1,I)>KNOTLOCPC(4,1,DECALGEO+INCTRL)+EM06 .OR.
     .        KNOTLOCEL(1,2,I)<KNOTLOCPC(1,2,DECALGEO+INCTRL)-EM06 .OR.
     .        KNOTLOCEL(2,2,I)>KNOTLOCPC(4,2,DECALGEO+INCTRL)+EM06 .OR.
     .        KNOTLOCEL(1,3,I)<KNOTLOCPC(1,3,DECALGEO+INCTRL)-EM06 .OR.
     .        KNOTLOCEL(2,3,I)>KNOTLOCPC(4,3,DECALGEO+INCTRL)+EM06) THEN
             print*,'DECALAGE : element : ',I,'point',INCTRL
             print*,'*************'
             DO K=1,KXIG3D(3,I)
              print*,IXIG3D(DECALIXIG3D+K-1)
             ENDDO
           ENDIF
          ENDDO

C-----------------------------------------------
C VERIFICATION PARTIEL DE L'ORDRE DE LA CONNECTIVITE
C-----------------------------------------------

         DO J=1,PZ-1
           DO K=1,PY-1
             DO L=1,PX-1

               ITNCTRL=(PX*PY)*(J-1)+PX*(K-1)+L

               INCTRL=IXIG3D(KXIG3D(4,I)+ITNCTRL-1)  !! IXIG3D range a l'envers 27->1
               INCTRL2=IXIG3D(KXIG3D(4,I)+ITNCTRL-1+1)  
               IF(KNOTLOCPC(1,1,DECALGEO+INCTRL)<KNOTLOCPC(1,1,DECALGEO+INCTRL2)) THEN
                 print*,'MAUVAIS RANGEMENT DANS IXIG3D : element : ',I,'point',INCTRL
               ENDIF

               INCTRL3=IXIG3D(KXIG3D(4,I)+ITNCTRL-1+PX)  
               IF(KNOTLOCPC(1,2,DECALGEO+INCTRL)<KNOTLOCPC(1,2,DECALGEO+INCTRL3)) THEN
                 print*,'MAUVAIS RANGEMENT DANS IXIG3D : element : ',I,'point',INCTRL
               ENDIF

               INCTRL4=IXIG3D(KXIG3D(4,I)+ITNCTRL-1+PX*PY)  
               IF(KNOTLOCPC(1,3,DECALGEO+INCTRL)<KNOTLOCPC(1,3,DECALGEO+INCTRL4)) THEN
                 print*,'MAUVAIS RANGEMENT DANS IXIG3D : element : ',I,'point',INCTRL
               ENDIF

             ENDDO

           ENDDO
         ENDDO

        ENDIF
C
      ENDDO
C
      ENDIF
C
      RETURN
      END






























