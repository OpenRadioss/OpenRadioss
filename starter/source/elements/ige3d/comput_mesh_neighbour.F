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
Chd|  COMPUT_MESH_NEIGHBOUR         source/elements/ige3d/comput_mesh_neighbour.F
Chd|-- called by -----------
Chd|        PRERAFIG3D                    source/elements/ige3d/prerafig3d.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE COMPUT_MESH_NEIGHBOUR(DIR,DIR2,IPID,IPID2,ID_MESHSURF,
     .                                 TAB_COINKNOT,L_TAB_COINKNOT,
     .                                 TAB_COINKNOT_TEST,L_TAB_COINKNOT_TEST,
     .                                 KNOT_INSERE,KNOT_INSERE2,
     .                                 TAB_MESHSURFCUT,L_TAB_MESHSURFCUT,
     .                                 TAB_MESHSURFCUT2,L_TAB_MESHSURFCUT2,FLAG)
C----------------------------------------------------------
C     ROUTINE QUI VA TESTER ET COMPTER LES MESHSURFS QUI 
C     INTERSECTENT AVEC LA MESHSURFSURF QU'ON TRAITE
C     NB : MESHSURF = SURFACE DE COUPE D'ELEMENTS DEFINISSANT
C          ONE RAFFINEMENT DANS UNE DIRECTION
C----------------------------------------------------------
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAB_MESHSURFCUT(*),TAB_MESHSURFCUT2(*)
      INTEGER DIR,DIR2,IPID,IPID2,L_TAB_MESHSURFCUT,L_TAB_MESHSURFCUT2,
     .        L_TAB_COINKNOT,L_TAB_COINKNOT_TEST,ID_MESHSURF,FLAG
      my_real TAB_COINKNOT(2,L_TAB_COINKNOT),
     .        TAB_COINKNOT_TEST(2,L_TAB_COINKNOT_TEST),
     .        KNOT_INSERE,KNOT_INSERE2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IDMESHIGE,DIRTANG1,DIRTANG2,IOUT
      my_real COINENGLOB(2,2), COINENGLOB_TEST(2,2)
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
C                                    |    
C    C1 ____________C2_ _ _ _ _ _P1  |               
C      |            |           /    |                 
C      |            |                |  C1 ___________C2
C      |            |         /      |    |            |_ _ _  _P1 _ _ _ _ P2   
C      |____________|_ _ _ _ _ _ P2  |    |            |      /           /
C     C4           C3      /         |    |            |
C                         C2    /    |    |____________|    /           /
C                        /|          |  C4             C3   
C                       / |   /      |                    /           /
C                      /  |          |                         
C                     /   | /        |                  /           /
C                  C1/    |C3        |                C2__________ C3
C                    |   /           |                /           /
C                    |  /            |               /           /
C                    | /  <= TESTEE  |              /           / <= TESTEE
C                    |/              |             /___________/
C                   C4               |           C1           C4
C                                    |
C=======================================================================
C----------------------------------------------------------------------
C   TRAITEMENT SIMPLE QUI PERMET D'ECARTER LES MESHSURF D'AUTRES PATCHS
C   OU LES MESHSURF PARALLELES
C----------------------------------------------------------------------
c
      IF(IPID2/=IPID) RETURN ! Pas les memes patchs
      IF(DIR2==DIR) RETURN ! Meshsurf paralelles
c
C----------------------------------------------------------------------
C   CONSTRUCTION DE BOITES ENGLOBANTE POUR LES MESHSURFS
C   PERMET DE SAVOIR SI IL Y A POTENTIELLEMENT INTERSECTION
C   (IL Y A ONE TEST SUR LES FONCTIONS EN AVAL, 
C   LA CERTITUDE D'INTERSECTION N'EST PAS NECESSAIRE)
C----------------------------------------------------------------------
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
c
      COINENGLOB(1,1) = MINVAL(TAB_COINKNOT(1,1:(L_TAB_COINKNOT)))
      COINENGLOB(2,1) = MINVAL(TAB_COINKNOT(2,1:(L_TAB_COINKNOT)))
      COINENGLOB(1,2) = MAXVAL(TAB_COINKNOT(1,1:(L_TAB_COINKNOT)))
      COINENGLOB(2,2) = MAXVAL(TAB_COINKNOT(2,1:(L_TAB_COINKNOT)))
c
      COINENGLOB_TEST(1,1) = MINVAL(TAB_COINKNOT_TEST(1,1:(L_TAB_COINKNOT_TEST)))
      COINENGLOB_TEST(2,1) = MINVAL(TAB_COINKNOT_TEST(2,1:(L_TAB_COINKNOT_TEST)))
      COINENGLOB_TEST(1,2) = MAXVAL(TAB_COINKNOT_TEST(1,1:(L_TAB_COINKNOT_TEST)))
      COINENGLOB_TEST(2,2) = MAXVAL(TAB_COINKNOT_TEST(2,1:(L_TAB_COINKNOT_TEST))) 
c
C----------------------------------------------------------------------
C   COMPARAISON DES INTERVALES DES BOITES ENGLOBANTES ET DES KNOT_INSERES,
C   PUIS VERIFICATION SUR LA TROISIEME DIRECTION COMPLEMENTAIRE AUX TWO DIRECTIONS
C   DES KNOT_INSERE DE LA SUPERPOSITION DES MESHSURFS
C   TEST QUI EXCLUE : PLUS SIMPLE
C----------------------------------------------------------------------
c
      IF(DIR2==DIRTANG1) THEN
        IF(KNOT_INSERE2<COINENGLOB(1,1).OR.KNOT_INSERE2>COINENGLOB(1,2)) RETURN
        IF(KNOT_INSERE<COINENGLOB_TEST(2,1).OR.KNOT_INSERE>COINENGLOB_TEST(2,2)) RETURN
        IF(COINENGLOB_TEST(1,2)<COINENGLOB(2,1)) RETURN
        IF(COINENGLOB_TEST(1,1)>COINENGLOB(2,2)) RETURN
      ELSEIF(DIR2==DIRTANG2) THEN
        IF(KNOT_INSERE2<COINENGLOB(2,1).OR.KNOT_INSERE2>COINENGLOB(2,2)) RETURN
        IF(KNOT_INSERE<COINENGLOB_TEST(1,1).OR.KNOT_INSERE>COINENGLOB_TEST(1,2)) RETURN
        IF(COINENGLOB_TEST(2,2)<COINENGLOB(1,1)) RETURN
        IF(COINENGLOB_TEST(2,1)>COINENGLOB(1,2)) RETURN
      ENDIF
c
C----------------------------------------------------------------------
C   SI ON ARRIVE LA, IL Y A ONE CROISEMENT PROBABLE
C----------------------------------------------------------------------
c
      L_TAB_MESHSURFCUT = L_TAB_MESHSURFCUT + 1
      IF(FLAG==1) THEN
        TAB_MESHSURFCUT(L_TAB_MESHSURFCUT) = ID_MESHSURF
      ENDIF
c
C----------------------------------------------------------------------
C   ON PEUT PRENDRE CETTE MESHSURF ET TOUTES LES MESHSURF QU'ELLE COUPE
C   (TESTONS SANS POUR L'INSTANT CAR ON TESTE TOUTES LES MESHSURFS 
C   DE TOUTE FACON, PAR RAPPORT A LA MESHSURF SUR LAQUELLE ON EST 
C----------------------------------------------------------------------
c
c      DO I=1,L_TAB_MESHSURFCUT2
c        IF(FLAG==0) THEN
c        L_TAB_MESHSURFCUT = L_TAB_MESHSURFCUT + 1
c        ENDIF
c        IF(FLAG==1) THEN
c          IOUT=0
c          DO J=1,L_TAB_MESHSURFCUT
c            IF(TAB_MESHSURFCUT(J)==TAB_MESHSURFCUT2(I)) IOUT=1
c          ENDDO
c          IF(IOUT==0) THEN
c            L_TAB_MESHSURFCUT = L_TAB_MESHSURFCUT + 1
c            TAB_MESHSURFCUT(L_TAB_MESHSURFCUT) = TAB_MESHSURFCUT2(I)
c          ENDIF
c        ENDIF
c      ENDDO
c
C----------------------------------------------------------------------
c
      RETURN
      END
c
