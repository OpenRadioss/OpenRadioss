Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||    i22edge_mod          ../common_source/modules/interfaces/cut-cell-buffer_mod.F
!||--- called by ------------------------------------------------------
!||    anim_nodal_p_elems   ../engine/source/output/anim/generate/anim_nodal_p_elems.F
!||    h3d_velvecc22        ../engine/source/output/h3d/h3d_results/h3d_velvecc22.F
!||    h3d_velvecz22        ../engine/source/output/h3d/h3d_results/h3d_velvecz22.F
!||    i22buce              ../engine/source/interfaces/intsort/i22buce.F
!||    i22bufbric_mod       ../common_source/modules/interfaces/cut-cell-search_mod.F
!||    i22cor3              ../engine/source/interfaces/int22/i22cor3.F
!||    i22get_prev_data     ../engine/source/interfaces/int22/i22get_prev_data.F
!||    i22ident             ../engine/source/interfaces/int22/i22ident.F
!||    i22intersect         ../engine/source/interfaces/int22/i22intersect.F
!||    i22subvol            ../engine/source/interfaces/int22/i22subvol.F
!||    i22tri_mod           ../common_source/modules/interfaces/cut-cell-search_mod.F
!||    i22trivox            ../engine/source/interfaces/intsort/i22trivox.F
!||    int22listcombi       ../engine/source/interfaces/int22/int22ListCombi.F
!||    nodal_schlieren      ../engine/source/output/anim/generate/nodal_schlieren.F
!||    nodald               ../engine/source/output/anim/generate/nodald.F
!||    nodalt               ../engine/source/output/anim/generate/nodalt.F
!||    nodalvfrac           ../engine/source/output/anim/generate/nodalvfrac.F
!||    nodalzvol            ../engine/source/output/anim/generate/nodalzvol.F
!||    resol                ../engine/source/engine/resol.F
!||    velvecc22            ../engine/source/output/anim/generate/velvec.F
!||    velvecz22            ../engine/source/output/anim/generate/velvecz22.F
!||====================================================================
      MODULE i22EDGE_MOD
      
#include "my_real.inc"

      INTEGER,ALLOCATABLE,DIMENSION(:)       :: NBOLD     
      INTEGER, ALLOCATABLE, DIMENSION(:,:)   :: LIST_B_OLD
      INTEGER,DIMENSION(:,:),ALLOCATABLE     :: IIAD22
      INTEGER,DIMENSION(:)  ,ALLOCATABLE     :: NGB, IDB, NELB, IDLOCB      
      my_real,DIMENSION(:)  ,ALLOCATABLE     :: v22max_L, dx22min_L    !kinematic time step      

      my_real,ALLOCATABLE,DIMENSION(:,:,:)   :: UVARL           ! multi-material merging          
      my_real,ALLOCATABLE,DIMENSION(:,:)     :: EINT_L          ! mono material only
      my_real,ALLOCATABLE,DIMENSION(:,:)     :: RHO_L           ! mono material only
      my_real,ALLOCATABLE,DIMENSION(:,:,:)   :: MOM_L           ! mono material only      
      my_real,ALLOCATABLE,DIMENSION(:,:,:)   :: SIG_L           ! mono material only
      my_real,ALLOCATABLE,DIMENSION(:,:,:)   :: VOLD_L          ! mono material only      
      my_real,ALLOCATABLE,DIMENSION(:,:,:)   :: SuperCellVOL_L  ! geometrical (both multi and mono material)
      INTEGER,ALLOCATABLE,DIMENSION(:,:,:)   :: UNLINKED_CELLS_L
      INTEGER,ALLOCATABLE,DIMENSION(:)       :: N_UNLINKED_L            
      INTEGER,ALLOCATABLE,DIMENSION(:)       :: IMERGEL

      TYPE EDGE_ENTITY
        INTEGER          ,DIMENSION(2)       :: NODE        
        INTEGER                              :: NBCUT                   !number of intersection points on edge
        INTEGER                              :: CUTSHELL(2)             
        my_real,          DIMENSION(2)       :: CUTCOOR                 !local coordinates 1D on edge N1->N2
        my_real,          DIMENSION(3,2)     :: CUTPOINT                !coordinates x,y,z
        my_real,          DIMENSION(3,2)     :: CUTVEL                  !velocity of intersection point
        my_real,          DIMENSION(3)       :: VECTOR
        my_real                              :: LEN
      END TYPE EDGE_ENTITY      

      TYPE CUT_PLANE
        my_real                              :: N(3)                    !normal vector
        my_real                              :: B(3)                    !basis
        my_real                              :: SCUT(1)                 !Cut Area
        my_real                              :: SEFF(1)                 !Effective Section = Wetted Surface : Cut Aera - Holes         (computed by suming lagrangian projection)
        my_real                              :: P(3,8)                  !Cut Surface Polygon (coordinates)
        my_real                              :: Vel(3)
        INTEGER                              :: NP                      !Cut Surface Polygon (number of points) ! NP<=6 | NP<=8 for additional Scut (closed surface hypothesis)
      END TYPE 

      TYPE LIST_ADJ
        INTEGER                              :: Num                     !number of adjacent elements (level2)
        INTEGER                              :: Num_inv(48)             !to define bijection with adjacent cut cell
        INTEGER                              :: IV(48)                  !internal ID
        INTEGER                              :: IB(48)                  !cut cell id
        INTEGER                              :: ICELL(48)               !secnd cell id
        INTEGER                              :: SecndFACE(48)           !secnd face ID to pickup correct flux
      END TYPE LIST_ADJ 

      TYPE LIST_SECND
        my_real                              :: VOL_Unmerged
        INTEGER                              :: Num      
        INTEGER                              :: NumSecndNodes        
        INTEGER                              :: FM(24)                  !Main face common face
        INTEGER                              :: FV(24)                  !Secnd corresponding face
        INTEGER                              :: IV(24)                  !brick id which contains secnd cell
        INTEGER                              :: IBV(24)                 !cut cell address in cut cell buffer
        INTEGER                              :: ICELLv(24)              !secnd cell id
        my_real                              :: VOL(24)                 !secnd cell volume
        my_real                              :: SURF_v(24)              !Area of linking face
        INTEGER                              :: NumNOD_Cell(24)         !noeuds sommets
        INTEGER                              :: ListNodID(24,8)         !Global nodes ID (max8) describing the cell, for each of 9 cells
      END TYPE LIST_SECND 
 
      TYPE POLYFACE_ENTITY
        INTEGER                              :: NumPOINT                !gives number of points (node+intersec.pts) on a given cell face   (%NumPOINT_Face(6,9))  
        INTEGER                              :: NAdjCell                !Number of adjacent cell for a given cell 1:9 and a given face 1:6  (%NAdjCell(9,6) )
        INTEGER                              :: Adjacent_Cell(5)        !index3: IADJ (adjacent cell id 1:%NAdjCell)
        my_real                              :: Adjacent_FLUX(5)        !1 adjacent cell = 1 flux (plus pratique pour maillage non conforme)
        my_real                              :: Adjacent_upwFLUX(5)
        my_real                              :: Center(3)               !Centroides des facettes de polyedres
        my_real                              :: Surf                    !face0=intersection surface
        my_real                              :: Vel(3)                  !velocity on polyhedra faces. Face 0 is for further implementation for porosity. Computed in i22subvol to take into account lagrangian vel.
        my_real                              :: F_FACE(3)               !FVM force on faces (and also velocity storage)
        my_real                              :: U_N
        my_real                              :: W(3)
      END TYPE POLYFACE_ENTITY

      TYPE POLYFACE_ENTITY_2
        INTEGER                              :: NumPOINT                !gives number of points (node+intersec.pts) on a given cell face   (%NumPOINT_Face(6,9))  
        INTEGER                              :: NAdjCell                !Number of adjacent cell for a given cell 1:9 and a given face 1:6  (%NAdjCell(9,6) )
        !INTEGER                              :: Adjacent_Cell(5)       !index3: IADJ (adjacent cell id 1:%NAdjCell)
        !my_real                              :: Adjacent_FLUX(5)       !1 adjacent cell = 1 flux (plus pratique pour maillage non conforme)
        !my_real                              :: Adjacent_upwFLUX(5)
        my_real                              :: Center(3)               !polyhedra Centroid
        my_real                              :: Surf                    !face0=intersection surface
        my_real                              :: F_FACE(3)               !FVM force on faces (and also velocity storage)
        my_real                              :: U_N(9)       !%U_N(index<=4) face0 is cut surface : can be the union of max. 4 cut polygons
                                                             !possible optimization : 1 single U_N per polyhedron ipoly <9
        my_real                              :: W(3)

      END TYPE POLYFACE_ENTITY_2
 
      TYPE MAT_ENTITY
        my_real                              :: rho
        my_real                              :: rhoE
        my_real                              :: rhoU(3)  
        my_real                              :: sig(6) 
        my_real                              :: ssp
        my_real, ALLOCATABLE,DIMENSION(:)    :: UVAR  !law37:size=5   !law51:size=N0PHAS+4*NVPHAS   !0:otherwise      
      END TYPE MAT_ENTITY

      TYPE POLY_ENTITY
        INTEGER                              :: NumNOD                  !vertexes (%NumNOD_Cell(9))
        INTEGER                              :: NumPOINT                !total number of intersection point which describes the volume (%NumPOINT_Cell(9))              
        INTEGER                              :: ListNodID(8)            !Global nodes ID (max8) describing the cell, for each of 9 cells  (%ListNodID(8,9))
        INTEGER                              :: IsMain                  !=1 main =0:secnd  (index is icell)   (%IsMain(9))
        INTEGER                              :: WhereIsMain(4)          !locate main cell to link with index1 : ICELL,  index2: (1)face, (2):icellm, (3):IDm, (4):IBm !(%WhereIsMain(9,4))
        INTEGER                              :: ID_FREE_NODE            !post-treatment  (!ID_FREE_NODE(9) )
        my_real                              :: Adjacent_FLU1           ! Adjacent_FLU1(9) 
        my_real                              :: DDVOL                   !d/dT (DELTA V)
        my_real                              :: DDVOL_upw             
        my_real                              :: PHI                     !data for convection (mass or energy) 
        my_real                              :: DPHI                    !convected increment (mass or energy)       
        my_real                              :: VFRACm(4)               !main cell VFRAC
        my_real                              :: Vnew                    !subvolumes (if exists)
        my_real                              :: Vold            
        my_real                              :: OLD_Vnew       
        my_real                              :: CellCenter(3)           !centroid of control volume
        my_real                              :: DVOL(0:2)               !index1:0:previous,1=real,2=prediction   index2 :0-9=icell (0=supercell)
        my_real                              :: cutFace              
        my_real                              :: DM
        TYPE(POLYFACE_ENTITY_2)              :: FACE0
        TYPE(POLYFACE_ENTITY)                :: FACE(1:6)
        !TYPE(MAT_ENTITY)                     :: MAT
      END TYPE POLY_ENTITY

      TYPE NODE_ENTITY
        INTEGER                              :: OLD_WhichCell           ! [1,8] -> [1,9]             
        INTEGER                              :: WhichCell               !node local id in[1,8] -> [1,9] = local cut cell id.
        INTEGER                              :: NodWasMain              !=1 belonged to the main cell, 0 otherwise. (index is brick node)
        INTEGER                              :: WhereWasMain            !index=node : result=face              
      END TYPE NODE_ENTITY

      TYPE BRICK_ENTITY 
        INTEGER                              :: ID
        INTEGER                              :: NG
        INTEGER                              :: IDLOC                
        INTEGER                              :: ICODE
        INTEGER                              :: OLD_ICODE        
        INTEGER                              :: IDBLE
        INTEGER                              :: NBITS
        INTEGER                              :: NPQTS
        INTEGER                              :: NBCUT                  !number of cut planes
        INTEGER                              :: MainID
        INTEGER                              :: WasCut
        INTEGER                              :: NewInBuffer        
        INTEGER                              :: OldMainStrongNode
        INTEGER                              :: MLW
        INTEGER                              :: ITASK
        INTEGER                              :: SECID_Cell(8)           !cut local id in [1,8] -> [1,52] = sec type. Each possible cut is numbered from 1 to 52 ; 8 nodes => 8 possible cut cells
        INTEGER                              :: Seg_add_LFT             !EL FIRST : first lagrangian face in CAND_E
        INTEGER                              :: Seg_add_LLT             !EL LAST  : last lagrangian face in CAND_E
        INTEGER                              :: Adjacent_Brick(6,5)     !index2: 1:IV, 2:NGV, 3:IDLOCV, 4:IBV, 5:IFV
        INTEGER                              :: MergeTarget(3,5)        !index1 (1)=Face (2)=MCELL, (3)=IB
        INTEGER                              :: NTarget                 !number of merged location
        INTEGER                              :: ClosedSurf(6)           !tells if brick face has a double cut (only for multiple cut, in this case cell 9 has no adjacent cells, only for contact forces to get pressure differential)
        INTEGER                              :: IsMergeTarget
        my_real                              :: N(6,3)                  !unitary normal vector on each 6 faces
        my_real                              :: Vnew_SCell              !SuperCell volume (if exists)
        my_real                              :: Vold_SCell              !volume main (if exists)    : need for epsdot in sigeps51, merge/demerge, link switching , srho3   
        my_real                              :: SCellCenter(3)          !SuperCell centroid
        my_real                              :: Face_Brick(6)
        my_real                              :: UncutVol                !brick volume. the one in gbuf can up extended if secnd is merged.
        my_real                              :: FCELL(3)                !FVM force on centroid
        my_real                              :: FEXT_CELL(3)
        my_real                              :: SIG(0:6)                !0 is pressure 1:6 is tensor
        my_real                              :: RHOC
        my_real                              :: MACH
        my_real                              :: DVOL
        my_real                              :: Poly9woNodes(6,2)       !Poly9woNodes(1:6,1) : tells if polygona has no nodes  
                                                                        !Poly9woNodes(1:6,2) : tells about adjacent cell  0=closed surface  1=communication with adjacent cell
        TYPE(POLY_ENTITY)                    :: POLY(9)
        TYPE(NODE_ENTITY)                    :: NODE(8)
        TYPE(LIST_ADJ)                       :: ADJ_ELEMS               !to be cleaned ?
        TYPE(CUT_PLANE)                      :: PCUT(16)
        TYPE(LIST_SECND)                     :: SecndList               !list of secnd cells linked to the given main cell
        TYPE(EDGE_ENTITY)                    :: Edge(12)                !list of edges for velocity or coordinates
        CHARACTER*14                         :: SECTYPE(8)
        TYPE(MAT_ENTITY)                     :: bakMAT
      END TYPE BRICK_ENTITY


      TYPE(LIST_SECND),DIMENSION(:,:),ALLOCATABLE    :: OLD_SecndList

      
      END MODULE
