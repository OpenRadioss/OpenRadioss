Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||====================================================================
!||    meshsurfig3d_mod   ../starter/source/elements/ige3d/meshsurfig3d_mod.F
!||--- called by ------------------------------------------------------
!||    lecig3d            ../starter/source/elements/ige3d/lecig3d.F
!||    lectur             ../starter/source/starter/lectur.F
!||    nbadigemesh        ../starter/source/elements/ige3d/nbadigemesh.F
!||    prerafig3d         ../starter/source/elements/ige3d/prerafig3d.F
!||    rafig3d            ../starter/source/elements/ige3d/rafig3d.F
!||    rebuild_ig3d       ../starter/source/elements/ige3d/rebuild_ig3d.F
!||====================================================================
      MODULE MESHSURFIG3D_MOD
C----------------------------------------------------------------------
C   DEFINITION OF DERIVED STRUCTURES ALLOWING REFINEMENT OF
C   ISOGEOMETRIC ELEMENTS IN THE STARTER
C----------------------------------------------------------------------
#include "my_real.inc"
C----------------------------------------------------------------------
C   MESHSURFIG3D_ : STRUCTURE DEFINING CUTTING SURFACES (MESHSURF)
C----------------------------------------------------------------------
        TYPE MESHSURFIG3D_
C--------------------------------------------------------------------------------------------------------
C VAR               | SIZE         |  TYP  |  DEFINITION
C--------------------------------------------------------------------------------------------------------
C ID_MESHSURF       |  1            | I    |  NUMBER OF THE CUTTING SURFACE (MESHSURF)
C ID_PID            |  1            | I    |  NUMBER OF THE CORRESPONDING PROPERTY (DIFFERENTIATES PATCHES)
C DIR               |  1            | I    |  REFINEMENT DIRECTION
C KNOT_INSERE       |  1            | R    |  NEW KNOT INSERTED LOCALLY
C L_TAB_ELCUT       |  1            | I    |  NUMBER OF REFINED ELEMENTS
C TAB_ELCUT         |  L_TAB_ELCUT  | I    |  LIST OF IDs OF REFINED ELEMENTS
C L_TAB_NEWEL       |  1            | I    |  NUMBER OF CREATED ELEMENTS
C TAB_NEWEL         |  L_TAB_NEWEL  | I    |  LIST OF IDs OF NEW ELEMENTS
C L_TAB_FCTCUT      |  1            | I    |  NUMBER OF REFINED FUNCTIONS 
C TAB_FCTCUT        |  L_TAB_FCTCUT | I    |  LIST OF IDs OF REFINED FUNCTIONS
C L_TAB_MESHSURFCUT |  1            | I    |  NUMBER OF INTERSECTING CUTTING SURFACES
C TAB_MESHSURFCUT   |  L_TAB_MESHS..| I    |  LIST OF IDs OF INTERSECTING CUTTING SURFACES
C L_TAB_COINKNOT    |  1            | I    |  NUMBER OF COINS DEFINING THE CUTTING SURFACE
C list of coordinates in the plane tangent to the direction
C                   of the cut of the corners defining the shape of the surface
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
C ID_TABCONRF   |  1            | I    |  PATCH ID 
C property id
C L_TAB_IG3D    |  1            | I    |  NUMBER OF ISOGEOMETRIC ELEMENTS OF PATCH (BEFORE REFINEMENT)
C list of the ids of the isogeometric elements of the patch
C storage of the initial number of refinements for each element
C               of the patch coming from the input (per direction)
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
C  note: in tabconbpatch: one can know and predict in tab_ig3d the created elements
C                           WE CAN HAVE AN ARRAY (:) TAB_IG3D_MODIF AND L_BTA_IG3D_MODIF
C                           TO KNOW WHICH ELEMENTS NEED TREATMENT OF THEIR
C                           connectivity table
C                           THE GLOBAL ARRAY IDFILS SHOULD ALSO BE PASSED LOCALLY BY PATCH
C   would allow storage by patch, so possible parallelization
C--------------------------------------------------------------------
C
      END MODULE MESHSURFIG3D_MOD
C
