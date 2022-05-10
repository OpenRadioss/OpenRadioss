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
Chd|  MESHSURFIG3D_MOD              source/elements/ige3d/meshsurfig3d_mod.F
Chd|-- called by -----------
Chd|        LECIG3D                       source/elements/ige3d/lecig3d.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        NBADIGEMESH                   source/elements/ige3d/nbadigemesh.F
Chd|        PRERAFIG3D                    source/elements/ige3d/prerafig3d.F
Chd|        RAFIG3D                       source/elements/ige3d/rafig3d.F
Chd|        REBUILD_IG3D                  source/elements/ige3d/rebuild_ig3d.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE MESHSURFIG3D_MOD
C----------------------------------------------------------------------
C   DEFINITION DES STRUCTURES DERIVEES PERMETTANT LE RAFFINEMENT DES 
C   ELEMENTS ISOGEOMETRIQUES DANS LE STARTER
C----------------------------------------------------------------------
#include "my_real.inc"
C----------------------------------------------------------------------
C   MESHSURFIG3D_ : STRUCTURE DEFINISSANT LES SURFACE DE COUPES (MESHSURF)
C----------------------------------------------------------------------
        TYPE MESHSURFIG3D_
C--------------------------------------------------------------------------------------------------------
C VAR               | SIZE         |  TYP  |  DEFINITION
C--------------------------------------------------------------------------------------------------------
C ID_MESHSURF       |  1            | I    |  NUMERO DE LA SURFACE DE COUPE (MESHSURF) 
C ID_PID            |  1            | I    |  NUMERO DE LA PROPRIETE CORRESPONDANT (DIFFERENTIE LES PATCHS) 
C DIR               |  1            | I    |  DIRECTION DU RAFFINEMENT
C KNOT_INSERE       |  1            | R    |  NOUVEAU KNOT INSERE LOCALEMENT  
C L_TAB_ELCUT       |  1            | I    |  NOMBRE D'ELEMENTS RAFFINES 
C TAB_ELCUT         |  L_TAB_ELCUT  | I    |  LISTE DES ID DES ELEMENTS RAFFINES 
C L_TAB_NEWEL       |  1            | I    |  NOMBRE D'ELEMENTS CREES 
C TAB_NEWEL         |  L_TAB_NEWEL  | I    |  LISTE DES ID DES NOUVEAUX ELEMENTS 
C L_TAB_FCTCUT      |  1            | I    |  NOMBRE DE FONCTIONS RAFFINEES 
C TAB_FCTCUT        |  L_TAB_FCTCUT | I    |  LISTE DES ID DES FONCTIONS RAFFINEES 
C L_TAB_MESHSURFCUT |  1            | I    |  NOMBRE DE SURFACES DE COUPE CROISEES 
C TAB_MESHSURFCUT   |  L_TAB_MESHS..| I    |  LISTE DES ID DES SURFACES DE COUPE CROISEES 
C L_TAB_COINKNOT    |  1            | I    |  NOMBRE DE COINS DEFINISSANT LA SURFACE DE COUPE 
C TAB_COINKNOT      |  2,L_TAB_COI..| R    |  LISTE DES COORDONNES DANS LE PLAN TANGENT A LA DIRECTION
C                   |              |      |   DE COUPE DES COINS DEFINISSANT LA FORME DE LA SURFACE 
C--------------------------------------------------------------------------------------------------------
C
         INTEGER ID_MESHSURF
         INTEGER ID_PID
         INTEGER DIR
         my_real KNOT_INSERE
         INTEGER L_TAB_ELCUT
         INTEGER, DIMENSION(:),ALLOCATABLE :: TAB_ELCUT
         INTEGER L_TAB_NEWEL
         INTEGER, DIMENSION(:),ALLOCATABLE :: TAB_NEWEL
         INTEGER L_TAB_FCTCUT
         INTEGER, DIMENSION(:),ALLOCATABLE :: TAB_FCTCUT
         INTEGER L_TAB_MESHSURFCUT
         INTEGER, DIMENSION(:),ALLOCATABLE :: TAB_MESHSURFCUT
         INTEGER L_TAB_COINKNOT
         my_real, DIMENSION(:,:),ALLOCATABLE :: TAB_COINKNOT
C
        END TYPE MESHSURFIG3D_
C
C----------------------------------------------------------------------
C   TABCONPATCH_IG3D_ : STRUCTURE DEFINISSANT LES PATCHS ISOGOMETRIQUES
C----------------------------------------------------------------------
        TYPE TABCONPATCH_IG3D_
C--------------------------------------------------------------------------------------------------------
C VAR           | SIZE         |  TYP  |  DEFINITION
C--------------------------------------------------------------------------------------------------------
C ID_TABCONRF   |  1            | I    |  ID DU PATCH 
C PID           |  1            | I    |  ID DE LA PROPRIETE 
C L_TAB_IG3D    |  1            | I    |  NOMBRE D'ELEMENTS ISOGEOMETRIQUES DU PATCH (AVANT RAFFINEMENT)
C TAB_IG3D      |  L_TAB_IG3D   | I    |  LISTE DES ID DES ELEMENTS ISOGEOMETRIQUES DU PATCH
C INITIAL_CUT   |  3,L_TAB_IG3D | I    |  STOCKAGE DU NOMBRE INITIAL DE RAFFINEMENT DE CHACUN DES ELEMENTS
C               |              |       |  DU PATCH VENANT DE L'INPUT (PAR DIRECTION)
C--------------------------------------------------------------------------------------------------------
C
         INTEGER ID_TABCON
         INTEGER PID
         INTEGER L_TAB_IG3D
         INTEGER, DIMENSION(:),ALLOCATABLE :: TAB_IG3D
         INTEGER, DIMENSION(:,:), ALLOCATABLE :: INITIAL_CUT
C
        END TYPE TABCONPATCH_IG3D_
C
C  NB : DANS TABCONBPATCH : ON PEUT SAVOIR ET PREVOIR DANS TAB_IG3D LES ELEMENTS CREES 
C                           ON PEUT AVOIR UN TABLEAU (:) TAB_IG3D_MODIF ET L_BTA_IG3D_MODIF 
C                           POUR SAVOIR QUELS ELEMENTS ONT BESOIN D'UN TRAITEMENT DE LEUR
C                           TABLE DE CONNECTIVITE
C                           LE TABLEAU GLOBAL IDFILS DEVRAIT AUSSI ETRE PASSE EN LOCAL PAR PATCH
C   => PERMETTRAIT LE STOCKAGE PAR PATCH DONC UNE PARALLELISATION POSSIBLE 
C--------------------------------------------------------------------
C
      END MODULE MESHSURFIG3D_MOD
C
