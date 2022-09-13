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
Chd|  COMPUT_COINKNOT               source/elements/ige3d/comput_coinknot.F
Chd|-- called by -----------
Chd|        PRERAFIG3D                    source/elements/ige3d/prerafig3d.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|====================================================================
      SUBROUTINE COMPUT_COINKNOT(IEL ,IXIG3D,KXIG3D ,MESHIGE,PTANG1 ,PTANG2 ,IDDIR,
     .                           IDTANG1,IDTANG2,NELDIR,NELTANG1,NELTANG2,DIR,
     .                           TAB_COINKNOT,L_TAB_COINKNOT,TAB_ELCUT,L_TAB_ELCUT,TAB_NEWEL,L_TAB_NEWEL,
     .                           KNOT,IAD_KNOT,NKNOT1,NKNOT2,NKNOT3,IDFILS,KNOTLOCEL,NEWKNOT,IPARTIG3D,
     .                           TAB_OLDIDCUT,IDCUT,FLAG)
C----------------------------------------------------------------------
C   ROUTINE QUI DEFINIT LES FORMES DES MESHSURF EN FONCTIONS DES ELEMENTS
C   VOISINS A RAFFINER DANS LE PLAN DE COUPE
C   CETTE ROUTINE REMPLIT EGALEMENT TOUTES LES DONNES ELEMENTAIRES DES 
C   ELEMENTS ET DES NOUVEAUX ELEMENTS (KXIG3D + IXIG3D)
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
#include      "param_c.inc"
#include      "tabsiz_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXIG3D(*),KXIG3D(NIXIG3D,*),MESHIGE(NELTANG1,NELTANG2,NELDIR),
     .        IDDIR(*),IDTANG1(*),IDTANG2(*),TAB_ELCUT(L_TAB_ELCUT),
     .        TAB_NEWEL(L_TAB_NEWEL),IDFILS(NBFILSMAX,*),IPARTIG3D(*),TAB_OLDIDCUT(3,*)
      INTEGER IDCUT,IDCUT_VOISIN,IEL,DIR,PTANG1,PTANG2,NELDIR,NELTANG1,NELTANG2,
     .        L_TAB_ELCUT,L_TAB_NEWEL,L_TAB_COINKNOT,FLAG,IAD_KNOT,
     .        NKNOT1,NKNOT2,NKNOT3
      my_real TAB_COINKNOT(2,*),KNOT(*)
      my_real KNOTLOCEL(2,3,*),NEWKNOT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,M,IDMESHIGE,NBSEGMENTS,DIRTANG1,DIRTANG2
      INTEGER IDNBCUT,ID1KNOT1,ID2KNOT1,ID1KNOT2,ID2KNOT2, IDNEXTG,
     .        IDNEXTD, IDNEXTEL,IAD_IXIG3D
      INTEGER COINDEP(2),COIN(2),DIRECTION,WORK(70000),
     .        L_TABWORKEL, COINENGLOB(2,2),NB_NEWFILS
      INTEGER L_TABWORK, NEXT, REFNBCUT, IOUT, INTERSEC
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX, TABWORKEL, TABWORKELTRI
      INTEGER OFFSET_KNOT,OFFSET_TANG1,OFFSET_TANG2,BORDHAUT,BORDDROIT
      my_real COIN_TMP(2,20), DET, T1, T2, XA(5),YA(5),
     .        XB, YB, XC, YC, XD, YD, TOL
C=======================================================================
C----------------------------------------------------------
C     ELEMENT ISO GEOMETRIQUE
C-----------------------------------------------
C     KXIG3D(6,*)   :   index of 1st knot in the Xknot vector corresponding to the element
C     KXIG3D(7,*)   :   index of 1st knot in the Yknot vector corresponding to the element
C     KXIG3D(8,*)   :   index of 1st knot in the Zknot vector corresponding to the element
C     KXIG3D(9,*)   :   index of 2nd knot in the Xknot vector corresponding to the element
C     KXIG3D(10,*)  :   index of 2nd knot in the Yknot vector corresponding to the element
C     KXIG3D(11,*)  :   index of 2nd knot in the Zknot vector corresponding to the element
C     KXIG3D(12,*)  :   number of element's cuts needed in the X direction
C     KXIG3D(13,*)  :   number of element's cuts needed in the Y direction
C     KXIG3D(14,*)  :   number of element's cuts needed in the Z direction
C=======================================================================
C
C     DANS L'ESPACE PARAMETRIQUE
C
C     RAFFINEMENT X :      RAFFINEMENT Y :      RAFFINEMENT Z :   
C
C          Z                    X                    Y         
C          |                    |                    |
C          |_ _ Y               |_ _ Z               |_ _ X
C         /                    /                    /
C        X                    Y                    Z
C
C=======================================================================
C                                           
C        TANG2                              _______  
C          |                               |       |
C          |_ _ TANG1                      |       | 
C         /                                |_______|
C      DIR                      COIN(1,2) /   
C                                           
C=======================================================================
C----------------------------------------------------------------------
C     INITIALISATION DES VARIABLES EN FONCTION DE LA DIRECTION DE COUPE
C----------------------------------------------------------------------
C
      IF(DIR==1) THEN
        DIRTANG1 = 2
        DIRTANG2 = 3
        IDNBCUT=12
        ID1KNOT1=7  ! index of 1nd knot in the Yknot vector corresponding to the element
        ID2KNOT1=10 ! index of 2nd knot in the Yknot vector corresponding to the element
        ID1KNOT2=8  ! index of 1st knot in the Zknot vector corresponding to the element
        ID2KNOT2=11 ! index of 2nd knot in the Zknot vector corresponding to the element
        IDMESHIGE=IDDIR(KXIG3D(6,IEL)) 
        OFFSET_KNOT = IAD_KNOT
        OFFSET_TANG1 = IAD_KNOT + NKNOT1
        OFFSET_TANG2 = IAD_KNOT + NKNOT1 + NKNOT2
      ELSEIF(DIR==2) THEN
        DIRTANG1 = 3
        DIRTANG2 = 1
        IDNBCUT=13
        ID1KNOT1=8  ! index of 1st knot in the Zknot vector corresponding to the element
        ID2KNOT1=11 ! index of 2nd knot in the Zknot vector corresponding to the element
        ID1KNOT2=6  ! index of 1st knot in the Xknot vector corresponding to the element
        ID2KNOT2=9  ! index of 2nd knot in the Xknot vector corresponding to the element
        IDMESHIGE=IDDIR(KXIG3D(7,IEL))
        OFFSET_KNOT = IAD_KNOT + NKNOT1
        OFFSET_TANG1 = IAD_KNOT + NKNOT1 + NKNOT2
        OFFSET_TANG2 = IAD_KNOT
      ELSEIF(DIR==3) THEN
        DIRTANG1 = 1
        DIRTANG2 = 2
        IDNBCUT=14
        ID1KNOT1=6  ! index of 1st knot in the Xknot vector corresponding to the element
        ID2KNOT1=9  ! index of 2nd knot in the Xknot vector corresponding to the element
        ID1KNOT2=7  ! index of 1st knot in the Yknot vector corresponding to the element
        ID2KNOT2=10 ! index of 2nd knot in the Yknot vector corresponding to the element
        IDMESHIGE=IDDIR(KXIG3D(8,IEL))
        OFFSET_KNOT = IAD_KNOT + NKNOT1 + NKNOT2
        OFFSET_TANG1 = IAD_KNOT
        OFFSET_TANG2 = IAD_KNOT + NKNOT1
      ENDIF

      L_TABWORK = 50000
      TOL = EM06

C----------------------------------------------------------------------
C     FIRST STEP : ON PART DE L'ELEMENT INITIAL A RAFFINER
C----------------------------------------------------------------------

      COINDEP(1)=IDTANG1(KXIG3D(ID1KNOT1,IEL))
      COINDEP(2)=IDTANG2(KXIG3D(ID1KNOT2,IEL))

      COIN(:)=COINDEP(:)

      COINENGLOB(1,1)= MIN(COIN(1),10000)
      COINENGLOB(2,1)= MIN(COIN(2),10000)
      COINENGLOB(1,2)= MAX(COIN(1),0)
      COINENGLOB(2,2)= MAX(COIN(2),0)

      NBSEGMENTS = 0
      L_TAB_COINKNOT = 0
      L_TAB_ELCUT = 0
      L_TAB_NEWEL = 0
      L_TABWORKEL = 0

      ALLOCATE(TABWORKEL(L_TABWORK))
      ALLOCATE(TABWORKELTRI(L_TABWORK)) 
      TABWORKELTRI(:) = 0
      TABWORKEL(:) = EP06
      COIN_TMP(:,:) = 0 

      REFNBCUT = KXIG3D(IDNBCUT,IEL)

c
c     PREMIER COIN
c
      L_TAB_COINKNOT = L_TAB_COINKNOT + 1

      COINENGLOB(1,1)= MIN(COIN(1),COINENGLOB(1,1))
      COINENGLOB(2,1)= MIN(COIN(2),COINENGLOB(2,1))
      COINENGLOB(1,2)= MAX(COIN(1),COINENGLOB(1,2))
      COINENGLOB(2,2)= MAX(COIN(2),COINENGLOB(2,2))
      COIN_TMP(1,L_TAB_COINKNOT) = KNOT(OFFSET_TANG1+KXIG3D(ID1KNOT1,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
      COIN_TMP(2,L_TAB_COINKNOT) = KNOT(OFFSET_TANG2+KXIG3D(ID1KNOT2,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
      IF(FLAG==1) THEN
        TAB_COINKNOT(1,L_TAB_COINKNOT) = KNOT(OFFSET_TANG1+KXIG3D(ID1KNOT1,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
        TAB_COINKNOT(2,L_TAB_COINKNOT) = KNOT(OFFSET_TANG2+KXIG3D(ID1KNOT2,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
      ENDIF
c
      BORDDROIT  = 0
      BORDHAUT   = 0

C----------------------------------------------------------------------
C     ON ATTAQUE TJRS PAR LA CASE DU BAS GAUCHE, DONC ON CHERCHE VERS LE HAUT
C----------------------------------------------------------------------
c
      DIRECTION = 1

      DO WHILE (DIRECTION==1) 

c     PROLONGATION DU SEGMENT VERS LE HAUT

        IDNEXTD=MESHIGE(COIN(1),COIN(2),IDMESHIGE)
        IF(KXIG3D(IDNBCUT,IDNEXTD)==1) THEN 
          IDCUT_VOISIN = 0
        ELSE
          IDCUT_VOISIN = TAB_OLDIDCUT(DIR,IDNEXTD)-KXIG3D(IDNBCUT,IDNEXTD)+1
        ENDIF
        IF(IDCUT_VOISIN/=IDCUT) THEN
          DIRECTION=2 ! on doit tourner a droite
          CYCLE
        ELSE
          L_TABWORKEL = L_TABWORKEL + 1
          TABWORKEL(L_TABWORKEL) = IDNEXTD
        ENDIF

        IF(COIN(2)<NELTANG2) THEN
          IF(COIN(1)>1) THEN 
           IDNEXTG=MESHIGE(COIN(1)-1,COIN(2),IDMESHIGE)
        IF(KXIG3D(IDNBCUT,IDNEXTG)==1) THEN 
          IDCUT_VOISIN = 0
        ELSE
          IDCUT_VOISIN = TAB_OLDIDCUT(DIR,IDNEXTG)-KXIG3D(IDNBCUT,IDNEXTG)+1
        ENDIF
           IF(IDCUT_VOISIN==IDCUT) THEN
             DIRECTION=4  ! On doit tourner a gauche
             CYCLE
           ELSE
             COIN(2) = COIN(2)+1 ! on monte 
           ENDIF
          ELSE
           COIN(2) = COIN(2)+1 ! on monte
          ENDIF
        ELSE
          IF(COIN(1)>1) THEN 
           IDNEXTG=MESHIGE(COIN(1)-1,COIN(2),IDMESHIGE)
        IF(KXIG3D(IDNBCUT,IDNEXTG)==1) THEN 
          IDCUT_VOISIN = 0
        ELSE
          IDCUT_VOISIN = TAB_OLDIDCUT(DIR,IDNEXTG)-KXIG3D(IDNBCUT,IDNEXTG)+1
        ENDIF
           IF(IDCUT_VOISIN==IDCUT) THEN
             DIRECTION=4  ! On doit tourner a gauche
             CYCLE
           ELSE
             BORDHAUT=1
             DIRECTION=2 ! On doit tourner a droite
             CYCLE
           ENDIF
          ELSE
            BORDHAUT=1
            DIRECTION=2 ! On doit tourner a droite
            CYCLE
          ENDIF
        ENDIF

      ENDDO
c
c     NOUVEAU COIN
c
      NBSEGMENTS = NBSEGMENTS + 1
      L_TAB_COINKNOT = L_TAB_COINKNOT + 1

      COINENGLOB(1,1)= MIN(COIN(1),COINENGLOB(1,1))
      COINENGLOB(2,1)= MIN(COIN(2),COINENGLOB(2,1))
      COINENGLOB(1,2)= MAX(COIN(1),COINENGLOB(1,2))
      COINENGLOB(2,2)= MAX(COIN(2),COINENGLOB(2,2))
      IF(BORDHAUT==0) THEN
        COIN_TMP(1,L_TAB_COINKNOT) = KNOT(OFFSET_TANG1+KXIG3D(ID1KNOT1,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
        COIN_TMP(2,L_TAB_COINKNOT) = KNOT(OFFSET_TANG2+KXIG3D(ID1KNOT2,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
        IF(FLAG==1) THEN
          TAB_COINKNOT(1,L_TAB_COINKNOT) = KNOT(OFFSET_TANG1+KXIG3D(ID1KNOT1,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
          TAB_COINKNOT(2,L_TAB_COINKNOT) = KNOT(OFFSET_TANG2+KXIG3D(ID1KNOT2,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
        ENDIF
      ELSE
        COIN_TMP(1,L_TAB_COINKNOT) = KNOT(OFFSET_TANG1+KXIG3D(ID1KNOT1,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
        COIN_TMP(2,L_TAB_COINKNOT) = KNOT(OFFSET_TANG2+KXIG3D(ID2KNOT2,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
        IF(FLAG==1) THEN
          TAB_COINKNOT(1,L_TAB_COINKNOT) = KNOT(OFFSET_TANG1+KXIG3D(ID1KNOT1,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
          TAB_COINKNOT(2,L_TAB_COINKNOT) = KNOT(OFFSET_TANG2+KXIG3D(ID2KNOT2,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
        ENDIF
      ENDIF
C
      DO WHILE (.NOT.(COIN(1)==COINDEP(1).AND.COIN(2)==COINDEP(2)).OR.L_TAB_COINKNOT<5)
    
        SELECT CASE (DIRECTION)

          CASE(1)

            DO WHILE (DIRECTION==1) 

c     PROLONGATION DU SEGMENT VERS LE HAUT

              IDNEXTD=MESHIGE(COIN(1),COIN(2),IDMESHIGE)
              IF(KXIG3D(IDNBCUT,IDNEXTD)==1) THEN 
                IDCUT_VOISIN = 0
              ELSE
                IDCUT_VOISIN = TAB_OLDIDCUT(DIR,IDNEXTD)-KXIG3D(IDNBCUT,IDNEXTD)+1
              ENDIF
              IF(IDCUT_VOISIN==IDCUT) THEN
                L_TABWORKEL = L_TABWORKEL + 1
                TABWORKEL(L_TABWORKEL) = IDNEXTD
              ELSE
                DIRECTION=2 ! on doit tourner a droite
                CYCLE
              ENDIF
                
              IF(COIN(2)<NELTANG2) THEN

                IF(COIN(1)>1) THEN 
                 IDNEXTG=MESHIGE(COIN(1)-1,COIN(2),IDMESHIGE)
        IF(KXIG3D(IDNBCUT,IDNEXTG)==1) THEN 
          IDCUT_VOISIN = 0
        ELSE
          IDCUT_VOISIN = TAB_OLDIDCUT(DIR,IDNEXTG)-KXIG3D(IDNBCUT,IDNEXTG)+1
        ENDIF
                 IF(IDCUT_VOISIN==IDCUT) THEN
                   DIRECTION=4 ! On doit tourner a gauche
                   CYCLE
                 ELSE
                   COIN(2) = COIN(2)+1
                 ENDIF
                ELSE
                 COIN(2) = COIN(2)+1
                ENDIF
              ELSE
                IF(COIN(1)>1) THEN 
                 IDNEXTG=MESHIGE(COIN(1)-1,COIN(2),IDMESHIGE)
        IF(KXIG3D(IDNBCUT,IDNEXTG)==1) THEN 
          IDCUT_VOISIN = 0
        ELSE
          IDCUT_VOISIN = TAB_OLDIDCUT(DIR,IDNEXTG)-KXIG3D(IDNBCUT,IDNEXTG)+1
        ENDIF
                 IF(IDCUT_VOISIN==IDCUT) THEN
                   DIRECTION=4  ! On doit tourner a gauche
                   CYCLE
                 ELSE
                   BORDHAUT=1
                   DIRECTION=2 ! On doit tourner a droite
                   CYCLE
                 ENDIF
                ELSE
                  BORDHAUT=1
                  DIRECTION=2 ! On doit tourner a droite
                  CYCLE
                ENDIF
              ENDIF

            ENDDO
c
c     NOUVEAU COIN
c
      NBSEGMENTS = NBSEGMENTS + 1
      L_TAB_COINKNOT = L_TAB_COINKNOT + 1

      IF(BORDHAUT==0) THEN
        COIN_TMP(1,L_TAB_COINKNOT) = KNOT(OFFSET_TANG1+KXIG3D(ID1KNOT1,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
        COIN_TMP(2,L_TAB_COINKNOT) = KNOT(OFFSET_TANG2+KXIG3D(ID1KNOT2,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
        IF(FLAG==1) THEN
          TAB_COINKNOT(1,L_TAB_COINKNOT) = KNOT(OFFSET_TANG1+KXIG3D(ID1KNOT1,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
          TAB_COINKNOT(2,L_TAB_COINKNOT) = KNOT(OFFSET_TANG2+KXIG3D(ID1KNOT2,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
        ENDIF
      ELSE
        COIN_TMP(1,L_TAB_COINKNOT) = KNOT(OFFSET_TANG1+KXIG3D(ID1KNOT1,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
        COIN_TMP(2,L_TAB_COINKNOT) = KNOT(OFFSET_TANG2+KXIG3D(ID2KNOT2,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
        IF(FLAG==1) THEN
          TAB_COINKNOT(1,L_TAB_COINKNOT) = KNOT(OFFSET_TANG1+KXIG3D(ID1KNOT1,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
          TAB_COINKNOT(2,L_TAB_COINKNOT) = KNOT(OFFSET_TANG2+KXIG3D(ID2KNOT2,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
        ENDIF
      ENDIF
c
          CASE(2)

            DO WHILE (DIRECTION==2) 

c     PROLONGATION DU SEGMENT VERS LA DROITE

              IF(BORDHAUT==1) THEN ! Si on est au bord haut
               IDNEXTG=MESHIGE(COIN(1),COIN(2),IDMESHIGE)
        IF(KXIG3D(IDNBCUT,IDNEXTG)==1) THEN 
          IDCUT_VOISIN = 0
        ELSE
          IDCUT_VOISIN = TAB_OLDIDCUT(DIR,IDNEXTG)-KXIG3D(IDNBCUT,IDNEXTG)+1
        ENDIF
               IF(IDCUT_VOISIN==IDCUT) THEN
                 L_TABWORKEL = L_TABWORKEL + 1
                 TABWORKEL(L_TABWORKEL) = IDNEXTG
               ELSE
                 DIRECTION=3 ! On doit tourner en bas
                 CYCLE
               ENDIF
               IF(COIN(1)<NELTANG1) THEN ! si on n'est pas au bord droit
                 COIN(1)=COIN(1)+1
                 CYCLE
               ELSE
                 DIRECTION=3 ! On doit tourner en bas
                 BORDDROIT=1
                 CYCLE
               ENDIF
              ENDIF

              IF(BORDHAUT==0) THEN

               IF(COIN(2)>1) THEN
                IDNEXTD=MESHIGE(COIN(1),COIN(2)-1,IDMESHIGE)
                IF(KXIG3D(IDNBCUT,IDNEXTD)==1) THEN 
                  IDCUT_VOISIN = 0
                ELSE
                  IDCUT_VOISIN = TAB_OLDIDCUT(DIR,IDNEXTD)-KXIG3D(IDNBCUT,IDNEXTD)+1
                ENDIF
                IF(IDCUT_VOISIN==IDCUT) THEN
                  L_TABWORKEL = L_TABWORKEL + 1
                  TABWORKEL(L_TABWORKEL) = IDNEXTD
                ELSE
                  DIRECTION=3 ! On doit tourner en bas
                  CYCLE
                ENDIF

                IF(COIN(1)<NELTANG1) THEN ! si on n'est pas au bord droit
                  IDNEXTG=MESHIGE(COIN(1),COIN(2),IDMESHIGE)
        IF(KXIG3D(IDNBCUT,IDNEXTG)==1) THEN 
          IDCUT_VOISIN = 0
        ELSE
          IDCUT_VOISIN = TAB_OLDIDCUT(DIR,IDNEXTG)-KXIG3D(IDNBCUT,IDNEXTG)+1
        ENDIF
                  IF(IDCUT_VOISIN==IDCUT) THEN
                    L_TABWORKEL = L_TABWORKEL + 1
                    TABWORKEL(L_TABWORKEL) = IDNEXTG  
                    DIRECTION=1 ! On doit tourner en haut
                    CYCLE
                  ELSE
                    COIN(1)=COIN(1)+1 ! On avance
                    CYCLE
                  ENDIF
                ELSE ! Si on est au bord droit
                  IDNEXTG=MESHIGE(COIN(1),COIN(2),IDMESHIGE)
        IF(KXIG3D(IDNBCUT,IDNEXTG)==1) THEN 
          IDCUT_VOISIN = 0
        ELSE
          IDCUT_VOISIN = TAB_OLDIDCUT(DIR,IDNEXTG)-KXIG3D(IDNBCUT,IDNEXTG)+1
        ENDIF
                  IF(IDCUT_VOISIN==IDCUT) THEN
                    L_TABWORKEL = L_TABWORKEL + 1
                    TABWORKEL(L_TABWORKEL) = IDNEXTG  
                    DIRECTION=1 ! On doit tourner en bas
                    CYCLE
                  ELSE
                    DIRECTION=3 ! On doit tourner en bas
                    BORDDROIT=1
                    CYCLE
                  ENDIF
                ENDIF
 
               ENDIF

              ENDIF

            ENDDO
c
c     NOUVEAU COIN
c
      NBSEGMENTS = NBSEGMENTS + 1
      L_TAB_COINKNOT = L_TAB_COINKNOT + 1

       IF(BORDDROIT==0) THEN
         IF(BORDHAUT==0) THEN
           COIN_TMP(1,L_TAB_COINKNOT) = KNOT(OFFSET_TANG1+KXIG3D(ID1KNOT1,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
           COIN_TMP(2,L_TAB_COINKNOT) = KNOT(OFFSET_TANG2+KXIG3D(ID1KNOT2,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
           IF(FLAG==1) THEN
             TAB_COINKNOT(1,L_TAB_COINKNOT) = KNOT(OFFSET_TANG1+KXIG3D(ID1KNOT1,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
             TAB_COINKNOT(2,L_TAB_COINKNOT) = KNOT(OFFSET_TANG2+KXIG3D(ID1KNOT2,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
           ENDIF
         ELSE
           COIN_TMP(1,L_TAB_COINKNOT) = KNOT(OFFSET_TANG1+KXIG3D(ID1KNOT1,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
           COIN_TMP(2,L_TAB_COINKNOT) = KNOT(OFFSET_TANG2+KXIG3D(ID2KNOT2,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
           IF(FLAG==1) THEN
             TAB_COINKNOT(1,L_TAB_COINKNOT) = KNOT(OFFSET_TANG1+KXIG3D(ID1KNOT1,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
             TAB_COINKNOT(2,L_TAB_COINKNOT) = KNOT(OFFSET_TANG2+KXIG3D(ID2KNOT2,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
           ENDIF
         ENDIF
       ELSE
         IF(BORDHAUT==0) THEN
           COIN_TMP(1,L_TAB_COINKNOT) = KNOT(OFFSET_TANG1+KXIG3D(ID2KNOT1,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
           COIN_TMP(2,L_TAB_COINKNOT) = KNOT(OFFSET_TANG2+KXIG3D(ID1KNOT2,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
           IF(FLAG==1) THEN
             TAB_COINKNOT(1,L_TAB_COINKNOT) = KNOT(OFFSET_TANG1+KXIG3D(ID2KNOT1,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
             TAB_COINKNOT(2,L_TAB_COINKNOT) = KNOT(OFFSET_TANG2+KXIG3D(ID1KNOT2,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
           ENDIF
         ELSE
           COIN_TMP(1,L_TAB_COINKNOT) = KNOT(OFFSET_TANG1+KXIG3D(ID2KNOT1,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
           COIN_TMP(2,L_TAB_COINKNOT) = KNOT(OFFSET_TANG2+KXIG3D(ID2KNOT2,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
           IF(FLAG==1) THEN
             TAB_COINKNOT(1,L_TAB_COINKNOT) = KNOT(OFFSET_TANG1+KXIG3D(ID2KNOT1,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
             TAB_COINKNOT(2,L_TAB_COINKNOT) = KNOT(OFFSET_TANG2+KXIG3D(ID2KNOT2,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
           ENDIF
         ENDIF
       ENDIF
c
          CASE(3)

            DO WHILE (DIRECTION==3) 

c     PROLONGATION DU SEGMENT VERS LE BAS

             IF(BORDHAUT==1) THEN
               IF(BORDDROIT==1) THEN
                 BORDHAUT=0
                 CYCLE
               ELSE
                 IF(COIN(2)>1) THEN
                   BORDHAUT=0
                   CYCLE
                 ELSE
                   DIRECTION=4 ! On doit tourner a gauche
                   BORDHAUT=0
                   CYCLE
                 ENDIF
               ENDIF
             ELSE
             
              IF(BORDDROIT==1) THEN
                IF(COIN(2)>1) THEN
                  IDNEXTG=MESHIGE(COIN(1),COIN(2)-1,IDMESHIGE)
        IF(KXIG3D(IDNBCUT,IDNEXTG)==1) THEN 
          IDCUT_VOISIN = 0
        ELSE
          IDCUT_VOISIN = TAB_OLDIDCUT(DIR,IDNEXTG)-KXIG3D(IDNBCUT,IDNEXTG)+1
        ENDIF
                  IF(IDCUT_VOISIN==IDCUT) THEN
                    L_TABWORKEL = L_TABWORKEL + 1
                    TABWORKEL(L_TABWORKEL) = IDNEXTG
                    COIN(2)=COIN(2)-1
                    CYCLE
                  ELSE
                    DIRECTION=4 ! On doit tourner vers la gauche
                    CYCLE
                  ENDIF
                ELSE
                  DIRECTION=4
                  CYCLE
                ENDIF
              ENDIF

              IF(COIN(2)>1) THEN 
                IF(COIN(1)>1) THEN
                  IDNEXTD=MESHIGE(COIN(1)-1,COIN(2)-1,IDMESHIGE)
                  IF(KXIG3D(IDNBCUT,IDNEXTD)==1) THEN 
                    IDCUT_VOISIN = 0
                  ELSE
                    IDCUT_VOISIN = TAB_OLDIDCUT(DIR,IDNEXTD)-KXIG3D(IDNBCUT,IDNEXTD)+1
                  ENDIF
                  IF(IDCUT_VOISIN==IDCUT) THEN
                    L_TABWORKEL = L_TABWORKEL + 1
                    TABWORKEL(L_TABWORKEL) = IDNEXTD
                  ELSE
                    DIRECTION=4 ! On doit tourner vers la gauche
                    CYCLE
                  ENDIF
                  IDNEXTG=MESHIGE(COIN(1),COIN(2)-1,IDMESHIGE)
        IF(KXIG3D(IDNBCUT,IDNEXTG)==1) THEN 
          IDCUT_VOISIN = 0
        ELSE
          IDCUT_VOISIN = TAB_OLDIDCUT(DIR,IDNEXTG)-KXIG3D(IDNBCUT,IDNEXTG)+1
        ENDIF
                  IF(IDCUT_VOISIN==IDCUT) THEN
                    IF(BORDDROIT==0) THEN
                      DIRECTION=2 ! On doit tourner vers la droite
                      CYCLE
                    ELSE
                      L_TABWORKEL = L_TABWORKEL + 1
                      TABWORKEL(L_TABWORKEL) = IDNEXTG                    
                      COIN(2) = COIN(2)-1 
                      BORDHAUT=0  
                    ENDIF
                  ELSE
                   COIN(2) = COIN(2)-1
                   BORDHAUT=0
                  ENDIF
                ELSE ! on est au bord gauche
                  IDNEXTG=MESHIGE(COIN(1),COIN(2)-1,IDMESHIGE)
        IF(KXIG3D(IDNBCUT,IDNEXTG)==1) THEN 
          IDCUT_VOISIN = 0
        ELSE
          IDCUT_VOISIN = TAB_OLDIDCUT(DIR,IDNEXTG)-KXIG3D(IDNBCUT,IDNEXTG)+1
        ENDIF
                  IF(IDCUT_VOISIN/=IDCUT) THEN
                    DIRECTION=2 ! On doit tourner vers la droite
                    CYCLE
                  ELSE
                   COIN(2) = COIN(2)-1
                   BORDHAUT=0
                  ENDIF
                ENDIF
              ELSE ! on est au bord bas
                DIRECTION=4 ! On doit tourner vers la gauche
              ENDIF
             ENDIF
            ENDDO
c
c     NOUVEAU COIN
c
      NBSEGMENTS = NBSEGMENTS + 1
      L_TAB_COINKNOT = L_TAB_COINKNOT + 1

       IF(BORDDROIT==0) THEN
         COIN_TMP(1,L_TAB_COINKNOT) = KNOT(OFFSET_TANG1+KXIG3D(ID1KNOT1,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
         COIN_TMP(2,L_TAB_COINKNOT) = KNOT(OFFSET_TANG2+KXIG3D(ID1KNOT2,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
         IF(FLAG==1) THEN
           TAB_COINKNOT(1,L_TAB_COINKNOT) = KNOT(OFFSET_TANG1+KXIG3D(ID1KNOT1,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
           TAB_COINKNOT(2,L_TAB_COINKNOT) = KNOT(OFFSET_TANG2+KXIG3D(ID1KNOT2,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
         ENDIF
       ELSE
         COIN_TMP(1,L_TAB_COINKNOT) = KNOT(OFFSET_TANG1+KXIG3D(ID2KNOT1,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
         COIN_TMP(2,L_TAB_COINKNOT) = KNOT(OFFSET_TANG2+KXIG3D(ID1KNOT2,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
         IF(FLAG==1) THEN
           TAB_COINKNOT(1,L_TAB_COINKNOT) = KNOT(OFFSET_TANG1+KXIG3D(ID2KNOT1,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
           TAB_COINKNOT(2,L_TAB_COINKNOT) = KNOT(OFFSET_TANG2+KXIG3D(ID1KNOT2,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
         ENDIF
       ENDIF
c
          CASE(4)

            DO WHILE (DIRECTION==4) 

c     PROLONGATION DU SEGMENT VERS LA GAUCHE

              IF(BORDDROIT==1) THEN
                IF(COIN(1)>1) THEN
                  BORDDROIT=0
                  CYCLE
                ELSE
                  BORDDROIT=0
                  DIRECTION=1
                  CYCLE
                ENDIF
              ENDIF
              
              IF(COIN(1)>1) THEN 
                IDNEXTD=MESHIGE(COIN(1)-1,COIN(2),IDMESHIGE)
                IF(KXIG3D(IDNBCUT,IDNEXTD)==1) THEN 
                  IDCUT_VOISIN = 0
                ELSE
                  IDCUT_VOISIN = TAB_OLDIDCUT(DIR,IDNEXTD)-KXIG3D(IDNBCUT,IDNEXTD)+1
                ENDIF
                IF(IDCUT_VOISIN==IDCUT) THEN
                  L_TABWORKEL = L_TABWORKEL + 1
                  TABWORKEL(L_TABWORKEL) = IDNEXTD
                ELSE
                  DIRECTION=1 ! On doit tourner vers le haut
                  CYCLE
                ENDIF

                IF(COIN(2)>1) THEN 
                 IDNEXTG=MESHIGE(COIN(1)-1,COIN(2)-1,IDMESHIGE)
        IF(KXIG3D(IDNBCUT,IDNEXTG)==1) THEN 
          IDCUT_VOISIN = 0
        ELSE
          IDCUT_VOISIN = TAB_OLDIDCUT(DIR,IDNEXTG)-KXIG3D(IDNBCUT,IDNEXTG)+1
        ENDIF
                 IF(IDCUT_VOISIN==IDCUT) THEN
                   DIRECTION=3 ! On doit tourner vers le bas
                   CYCLE
                 ELSE
                   COIN(1) = COIN(1)-1
                   BORDDROIT=0
                 ENDIF
                ELSE
                 COIN(1) = COIN(1)-1
                 BORDDROIT=0
                ENDIF
              ELSE
                DIRECTION=1 ! On doit tourner vers le haut
              ENDIF

            ENDDO
c
c     NOUVEAU COIN
c
      NBSEGMENTS = NBSEGMENTS + 1
      L_TAB_COINKNOT = L_TAB_COINKNOT + 1

      COIN_TMP(1,L_TAB_COINKNOT) = KNOT(OFFSET_TANG1+KXIG3D(ID1KNOT1,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
      COIN_TMP(2,L_TAB_COINKNOT) = KNOT(OFFSET_TANG2+KXIG3D(ID1KNOT2,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
      IF(FLAG==1) THEN
        TAB_COINKNOT(1,L_TAB_COINKNOT) = KNOT(OFFSET_TANG1+KXIG3D(ID1KNOT1,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
        TAB_COINKNOT(2,L_TAB_COINKNOT) = KNOT(OFFSET_TANG2+KXIG3D(ID1KNOT2,MESHIGE(COIN(1),COIN(2),IDMESHIGE)))
      ENDIF
c
        END SELECT

        COINENGLOB(1,1)= MIN(COIN(1),COINENGLOB(1,1))
        COINENGLOB(2,1)= MIN(COIN(2),COINENGLOB(2,1))
        COINENGLOB(1,2)= MAX(COIN(1),COINENGLOB(1,2))
        COINENGLOB(2,2)= MAX(COIN(2),COINENGLOB(2,2))

      ENDDO

C----------------------------------------------------------------------
C     TRAITEMENT DES ELEMENTS DU CONTOUR DEFINI ET DE CEUX A L'INTERIEUR
C----------------------------------------------------------------------

      ALLOCATE(INDEX(2*L_TABWORK))
      CALL MY_ORDERS(0, WORK, TABWORKEL, INDEX, L_TABWORK , 1)

      DO I=1,L_TABWORK
        TABWORKELTRI(I)=TABWORKEL(INDEX(I))
      ENDDO

      DEALLOCATE(INDEX)

      IF(FLAG==0) THEN
        DO I=1,L_TABWORK
          IF(TABWORKELTRI(I)==EP06)  EXIT
          IF(I/=1) THEN
            IF(TABWORKELTRI(I-1)==TABWORKELTRI(I)) CYCLE
          ENDIF
c
          NB_NEWFILS=0
c
          IF(NEWKNOT>=KNOTLOCEL(1,DIR,TABWORKELTRI(I)).AND.
     .       NEWKNOT<=KNOTLOCEL(2,DIR,TABWORKELTRI(I))) THEN
            L_TAB_ELCUT = L_TAB_ELCUT + 1
            L_TAB_NEWEL = L_TAB_NEWEL + 1
          ENDIF
          DO J=1,IDFILS(1,TABWORKELTRI(I))-NB_NEWFILS
            IF(NEWKNOT>=KNOTLOCEL(1,DIR,IDFILS(J+1,TABWORKELTRI(I))).AND.
     .         NEWKNOT<=KNOTLOCEL(2,DIR,IDFILS(J+1,TABWORKELTRI(I)))) THEN
              L_TAB_ELCUT = L_TAB_ELCUT + 1
              L_TAB_NEWEL = L_TAB_NEWEL + 1
            ENDIF
          ENDDO
c
        ENDDO
      ELSE
        DO I=1,L_TABWORK
          IF(TABWORKELTRI(I)==EP06)  EXIT
          IF(I/=1) THEN
            IF(TABWORKELTRI(I-1)==TABWORKELTRI(I)) CYCLE
          ENDIF
c
          NB_NEWFILS=0
c
          IF(NEWKNOT>=KNOTLOCEL(1,DIR,TABWORKELTRI(I)).AND.
     .       NEWKNOT<=KNOTLOCEL(2,DIR,TABWORKELTRI(I))) THEN
            L_TAB_ELCUT = L_TAB_ELCUT + 1
            TAB_ELCUT(L_TAB_ELCUT) = TABWORKELTRI(I)

            ADDELIG3D=ADDELIG3D+1

            L_TAB_NEWEL = L_TAB_NEWEL + 1
            TAB_NEWEL(L_TAB_NEWEL) = NUMELIG3D0+ADDELIG3D

            IDFILS(1,TABWORKELTRI(I))=IDFILS(1,TABWORKELTRI(I))+1
            IDFILS(IDFILS(1,TABWORKELTRI(I))+1,TABWORKELTRI(I))=NUMELIG3D0+ADDELIG3D
C  Comptage les nouveaux fils 
            NB_NEWFILS=NB_NEWFILS+1

            KXIG3D(:,NUMELIG3D0+ADDELIG3D) = KXIG3D(:,TABWORKELTRI(I))
            IAD_IXIG3D = SIXIG3D + ADDSIXIG3D + 1
            KXIG3D(4,NUMELIG3D0+ADDELIG3D) = IAD_IXIG3D
            DO K=1,KXIG3D(3,TABWORKELTRI(I))
              IXIG3D(IAD_IXIG3D+K-1) = IXIG3D(KXIG3D(4,TABWORKELTRI(I))+K-1)
            ENDDO
            ADDSIXIG3D = ADDSIXIG3D + KXIG3D(3,TABWORKELTRI(I))

            KXIG3D(5,NUMELIG3D0+ADDELIG3D) = NUMELIG3D0+ADDELIG3D ! n'empeche pas les id doubles
            IPARTIG3D(NUMELIG3D0+ADDELIG3D) = IPARTIG3D(TABWORKELTRI(I))

            KXIG3D(15,NUMELIG3D0+ADDELIG3D) = INOD_IGE
            INOD_IGE = INOD_IGE + 64

            KXIG3D(IDNBCUT,NUMELIG3D0+ADDELIG3D) = 1

            KNOTLOCEL(:,:,NUMELIG3D0+ADDELIG3D) = KNOTLOCEL(:,:,TABWORKELTRI(I))
            KNOTLOCEL(1,DIR,NUMELIG3D0+ADDELIG3D) = NEWKNOT

            KNOTLOCEL(2,DIR,TABWORKELTRI(I)) = NEWKNOT

          ENDIF

C  Boucle sur les anciens fils de cet element
          DO J=1,IDFILS(1,TABWORKELTRI(I))-NB_NEWFILS
            IF(NEWKNOT>=KNOTLOCEL(1,DIR,IDFILS(J+1,TABWORKELTRI(I))).AND.
     .         NEWKNOT<=KNOTLOCEL(2,DIR,IDFILS(J+1,TABWORKELTRI(I)))) THEN
              L_TAB_ELCUT = L_TAB_ELCUT + 1
              TAB_ELCUT(L_TAB_ELCUT) = IDFILS(J+1,TABWORKELTRI(I))

              ADDELIG3D=ADDELIG3D+1

              L_TAB_NEWEL = L_TAB_NEWEL + 1
              TAB_NEWEL(L_TAB_NEWEL) = NUMELIG3D0+ADDELIG3D

              IDFILS(1,TABWORKELTRI(I))=IDFILS(1,TABWORKELTRI(I))+1
              IDFILS(IDFILS(1,TABWORKELTRI(I))+1,TABWORKELTRI(I))=NUMELIG3D0+ADDELIG3D
              NB_NEWFILS=NB_NEWFILS+1


              KXIG3D(:,NUMELIG3D0+ADDELIG3D) = KXIG3D(:,IDFILS(J+1,TABWORKELTRI(I)))
              IAD_IXIG3D = SIXIG3D + ADDSIXIG3D + 1
              KXIG3D(4,NUMELIG3D0+ADDELIG3D) = IAD_IXIG3D
              DO K=1,KXIG3D(3,IDFILS(J+1,TABWORKELTRI(I)))
                IXIG3D(IAD_IXIG3D+K-1) = IXIG3D(KXIG3D(4,IDFILS(J+1,TABWORKELTRI(I)))+K-1)
              ENDDO
              ADDSIXIG3D = ADDSIXIG3D + KXIG3D(3,IDFILS(J+1,TABWORKELTRI(I)))

              KXIG3D(5,NUMELIG3D0+ADDELIG3D) = NUMELIG3D0+ADDELIG3D ! n'empeche pas les id doubles
              IPARTIG3D(NUMELIG3D0+ADDELIG3D) = IPARTIG3D(IDFILS(J+1,TABWORKELTRI(I)))

              KXIG3D(15,NUMELIG3D0+ADDELIG3D) = INOD_IGE
              INOD_IGE = INOD_IGE + 64

              KXIG3D(IDNBCUT,NUMELIG3D0+ADDELIG3D) = 1

              KNOTLOCEL(:,:,NUMELIG3D0+ADDELIG3D) = KNOTLOCEL(:,:,IDFILS(J+1,TABWORKELTRI(I)))
              KNOTLOCEL(1,DIR,NUMELIG3D0+ADDELIG3D) = NEWKNOT

              KNOTLOCEL(2,DIR,IDFILS(J+1,TABWORKELTRI(I))) = NEWKNOT

            ENDIF
          ENDDO
c
          KXIG3D(IDNBCUT,TABWORKELTRI(I)) = KXIG3D(IDNBCUT,TABWORKELTRI(I)) - 1 ! c'est le parent qui porte le nb de raffinement

        ENDDO
      ENDIF

      DEALLOCATE(TABWORKEL)

C----------------------------------------------------------------------
C   TRAITEMENT ADDITIONNEL POUR LES ELEMENTS DU MILIEU, NON FRONTALIERS AUX SEGMENTS
C----------------------------------------------------------------------

      DO I=COINENGLOB(1,1),COINENGLOB(1,2)
        DO J=COINENGLOB(2,1),COINENGLOB(2,2)
          IDNEXTEL=MESHIGE(I,J,IDMESHIGE)
          NEXT = 0
          DO K=1,L_TABWORK ! Verifie que l'element n'est pas deja liste
            IF(IDNEXTEL==TABWORKELTRI(K)) THEN
              NEXT = 1
              EXIT
            ENDIF
          ENDDO
          IF(NEXT==1) CYCLE

          IOUT = 0

          XA(1) = KNOTLOCEL(1,DIRTANG1,IDNEXTEL) + TOL
          XA(2) = KNOTLOCEL(2,DIRTANG1,IDNEXTEL) - TOL
          XA(3) = KNOTLOCEL(2,DIRTANG1,IDNEXTEL) - TOL
          XA(4) = KNOTLOCEL(1,DIRTANG1,IDNEXTEL) + TOL
          XA(5) = XA(1)

          YA(1) = KNOTLOCEL(1,DIRTANG2,IDNEXTEL) + TOL
          YA(2) = KNOTLOCEL(1,DIRTANG2,IDNEXTEL) + TOL
          YA(3) = KNOTLOCEL(2,DIRTANG2,IDNEXTEL) - TOL
          YA(4) = KNOTLOCEL(2,DIRTANG2,IDNEXTEL) - TOL
          YA(5) = YA(1)


          XB=XA(1)-1000 
          YB=YA(1)-2000

          DO K=1,4  ! BOUCLE SUR LES 4 COINS DE L'ETENDUE DE l'ELEMENT
            INTERSEC=0
            DO L=1,L_TAB_COINKNOT-1

              XC=COIN_TMP(1,L)
              YC=COIN_TMP(2,L)
              XD=COIN_TMP(1,L+1)
              YD=COIN_TMP(2,L+1)

              DET = (XB-XA(K))*(YC-YD) - (XC-XD)*(YB-YA(K))
              IF(DET==0) THEN
              ELSE
                T1 = ((XC-XA(K))*(YC-YD)-(XC-XD)*(YC-YA(K)))/DET
                T2 = ((XB-XA(K))*(YC-YA(K))-(XC-XA(K))*(YB-YA(K)))/DET
                IF(T1>1.OR.T1<0.OR.T2>1.OR.T2<=0) THEN 
                ELSE
                  INTERSEC = INTERSEC + 1
                ENDIF
              ENDIF
            ENDDO
            IF(MOD(INTERSEC,2)==0) IOUT=1
          ENDDO

          IF(IOUT==1) CYCLE

          IF(KXIG3D(IDNBCUT,IDNEXTEL)==1) THEN 
            IDCUT_VOISIN = 0
          ELSE
            IDCUT_VOISIN = TAB_OLDIDCUT(DIR,IDNEXTEL)-KXIG3D(IDNBCUT,IDNEXTEL)+1
          ENDIF
          IF(IDCUT_VOISIN==IDCUT) THEN

            IF(FLAG==0) THEN

              NB_NEWFILS=0

              IF(NEWKNOT>=KNOTLOCEL(1,DIR,IDNEXTEL).AND.
     .           NEWKNOT<=KNOTLOCEL(2,DIR,IDNEXTEL)) THEN
                L_TAB_ELCUT = L_TAB_ELCUT + 1
                L_TAB_NEWEL = L_TAB_NEWEL + 1
              ENDIF
              DO K=1,IDFILS(1,IDNEXTEL)-NB_NEWFILS
                IF(NEWKNOT>=KNOTLOCEL(1,DIR,IDFILS(K+1,IDNEXTEL)).AND.
     .             NEWKNOT<=KNOTLOCEL(2,DIR,IDFILS(K+1,IDNEXTEL))) THEN
                  L_TAB_ELCUT = L_TAB_ELCUT + 1
                  L_TAB_NEWEL = L_TAB_NEWEL + 1
                ENDIF
              ENDDO

            ELSE

              NB_NEWFILS=0

              IF(NEWKNOT>=KNOTLOCEL(1,DIR,IDNEXTEL).AND.
     .           NEWKNOT<=KNOTLOCEL(2,DIR,IDNEXTEL)) THEN
                L_TAB_ELCUT = L_TAB_ELCUT + 1
                TAB_ELCUT(L_TAB_ELCUT) = IDNEXTEL

                ADDELIG3D=ADDELIG3D+1

                L_TAB_NEWEL = L_TAB_NEWEL + 1
                TAB_NEWEL(L_TAB_NEWEL) = NUMELIG3D0+ADDELIG3D

                IDFILS(1,IDNEXTEL)=IDFILS(1,IDNEXTEL)+1
                IDFILS(IDFILS(1,IDNEXTEL)+1,IDNEXTEL)=NUMELIG3D0+ADDELIG3D
                NB_NEWFILS=NB_NEWFILS+1

                KXIG3D(:,NUMELIG3D0+ADDELIG3D) = KXIG3D(:,IDNEXTEL)
                IAD_IXIG3D = SIXIG3D + ADDSIXIG3D + 1
                KXIG3D(4,NUMELIG3D0+ADDELIG3D) = IAD_IXIG3D
                DO M=1,KXIG3D(3,IDNEXTEL)
                  IXIG3D(IAD_IXIG3D+M-1) = IXIG3D(KXIG3D(4,IDNEXTEL)+M-1)
                ENDDO
                ADDSIXIG3D = ADDSIXIG3D + KXIG3D(3,IDNEXTEL)

                KXIG3D(5,NUMELIG3D0+ADDELIG3D) = NUMELIG3D0+ADDELIG3D ! n'empeche pas les id doubles
                IPARTIG3D(NUMELIG3D0+ADDELIG3D) = IPARTIG3D(IDNEXTEL)

                KXIG3D(15,NUMELIG3D0+ADDELIG3D) = INOD_IGE
                INOD_IGE = INOD_IGE + 64


                KXIG3D(IDNBCUT,NUMELIG3D0+ADDELIG3D) = 1

                KNOTLOCEL(:,:,NUMELIG3D0+ADDELIG3D) = KNOTLOCEL(:,:,IDNEXTEL)
                KNOTLOCEL(1,DIR,NUMELIG3D0+ADDELIG3D) = NEWKNOT

                KNOTLOCEL(2,DIR,IDNEXTEL) = NEWKNOT

              ENDIF

              DO K=1,IDFILS(1,IDNEXTEL)-NB_NEWFILS
                IF(NEWKNOT>=KNOTLOCEL(1,DIR,IDFILS(K+1,IDNEXTEL)).AND.
     .             NEWKNOT<=KNOTLOCEL(2,DIR,IDFILS(K+1,IDNEXTEL))) THEN
                  L_TAB_ELCUT = L_TAB_ELCUT + 1
                  TAB_ELCUT(L_TAB_ELCUT) = IDFILS(K+1,IDNEXTEL)

                  ADDELIG3D=ADDELIG3D+1

                  L_TAB_NEWEL = L_TAB_NEWEL + 1
                  TAB_NEWEL(L_TAB_NEWEL) = NUMELIG3D0+ADDELIG3D

                  IDFILS(1,IDNEXTEL)=IDFILS(1,IDNEXTEL)+1
                  IDFILS(IDFILS(1,IDNEXTEL)+1,IDNEXTEL)=NUMELIG3D0+ADDELIG3D
                  NB_NEWFILS=NB_NEWFILS+1

                  KXIG3D(:,NUMELIG3D0+ADDELIG3D) = KXIG3D(:,IDFILS(K+1,IDNEXTEL))
                  IAD_IXIG3D = SIXIG3D + ADDSIXIG3D + 1
                  KXIG3D(4,NUMELIG3D0+ADDELIG3D) = IAD_IXIG3D
                  DO M=1,KXIG3D(3,IDFILS(K+1,IDNEXTEL))
                    IXIG3D(IAD_IXIG3D+M-1) = IXIG3D(KXIG3D(4,IDFILS(K+1,IDNEXTEL))+M-1)
                  ENDDO
                  ADDSIXIG3D = ADDSIXIG3D + KXIG3D(3,IDFILS(K+1,IDNEXTEL))

                  KXIG3D(5,NUMELIG3D0+ADDELIG3D) = NUMELIG3D0+ADDELIG3D ! n'empeche pas les id doubles
                  IPARTIG3D(NUMELIG3D0+ADDELIG3D) = IPARTIG3D(IDFILS(K+1,IDNEXTEL))

                  KXIG3D(15,NUMELIG3D0+ADDELIG3D) = INOD_IGE
                  INOD_IGE = INOD_IGE + 64

                  KXIG3D(IDNBCUT,NUMELIG3D0+ADDELIG3D) = 1

                  KNOTLOCEL(:,:,NUMELIG3D0+ADDELIG3D) = KNOTLOCEL(:,:,IDFILS(K+1,IDNEXTEL))
                  KNOTLOCEL(1,DIR,NUMELIG3D0+ADDELIG3D) = NEWKNOT

                  KNOTLOCEL(2,DIR,IDFILS(K+1,IDNEXTEL)) = NEWKNOT

                ENDIF
              ENDDO

              KXIG3D(IDNBCUT,IDNEXTEL) = KXIG3D(IDNBCUT,IDNEXTEL) - 1 ! c'est le parent qui porte le nb de raffinement

            ENDIF
          ENDIF
        ENDDO
      ENDDO

      DEALLOCATE(TABWORKELTRI)

      RETURN
      END
