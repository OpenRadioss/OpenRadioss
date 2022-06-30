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
C.../SUBVOLUMES/

      COMMON/SUBVOLUMES/
     .       bGnode,
     .       Gnode,
     .       Gcorner,
     .       Gface,
     .       GNPt,
     .       GNNod,     
     .       bCODE ,
     .       S_TETRA,  S_PENTA,  S_POLY3,  S_HEXAE,  S_POLY4,  S_POLY4A,  S_POLY4B,  S_POLYH,  S_POLYC,
     .       D_TETRA,  D_PENTA,  D_POLY3,  D_HEXAE,  D_POLY4,  D_POLY4A,  D_POLY4B,  D_POLYH,  D_POLYC,
     .       M_TETRA,  M_PENTA,  M_POLY3,  M_HEXAE,  M_POLY4,  M_POLY4A,  M_POLY4B,  M_POLYH,  M_POLYC, 
     .       N_TETRA,  N_PENTA,  N_POLY3,  N_HEXAE,  N_POLY4,  N_POLY4A,  N_POLY4B,  N_POLYH,  N_POLYC,
     .       F_TETRA,  F_PENTA,  F_POLY3,  F_HEXAE,  F_POLY4,  F_POLY4A,  F_POLY4B,  F_POLYH,  F_POLYC,
     .       C_TETRA,  C_PENTA,  C_POLY3,  C_HEXAE,  C_POLY4,  C_POLY4A,  C_POLY4B,  C_POLYH,  C_POLYC,
     .       S22_MAX,  LSECTYP,
     .       StrCODE,  GetPolyhedraType, GetNumTria, Gtria

C=====================================================================
C      Nu, Nv, Nw, Nx         : GRAPH corner (Brick node)
C      Ci, Cj, Ck, Cl, Cm, Cn : GRAPH corner (Edge intersection point)
C      Fa, Fb, Fc, Fd, Fe, Ff : face number (usual ALE face numbering)
C=====================================================================
C
C       +----+       +----+
C       | Ci | - - - | Cj |
C       +----+  Fa   +----+
C            \      /
C        \ Fc +----+ Fb /
C             | Nu |                               TETRA GRAPH
C          \  +----+  /                              * number of binary codes             : S_TETRA  ( 8)
C               |                                    * Number of graph for each subvolume : M_TETRA  ( 1)
C            \  |   /                                * Number of brick nodes  (Nu)        : F_TETRA  ( 3)
C             +----+                                 * Number of corner from edge (Ci,j,k): C_TETRA  ( 3)
C             | Ck |
C             +----+
C
C=====================================================================
C
C       +----+       +----+
C       | Ci | - - - | Cj |
C       +----+  Fa   +----+
C            \      /     |
C       |     +----+     
C             | Nu |      |
C       |     +----+   
C               |         |                        PENTA GRAPH
C       | Fd    |      Fc                            * number of binary codes             : S_PENTA  (12)
C             +----+      |                          * Number of graph for each subvolume : M_PENTA  ( 1)
C       |     | Nv |                                 * Number of brick nodes  (Nu)        : F_PENTA  ( 4)
C             +----+      |                          * Number of corner from edge (Ci,j,k): C_PENTA  ( 4)
C       |    /      \
C       +----+  Fb   +----+
C       | Cl | - - - | Ck |
C       +----+       +----+
C
C=====================================================================
C
C       +----+        +----+
C       | Ci | - - - -| Cj |
C       +----+  Fe    +----+ \
C            \      /         \
C       |     +----+           \
C             | Nu |           +----+
C       |     +----+    Fa     | Cj |
C          Fd   |              +----+              POLYG3 GRAPH
C       |       |             /     |                 * number of binary codes             : S_POLY3  (18)
C             +----+    +----+                        * Number of graph for each subvolume : M_POLY3  ( 1)
C       |     | Nx | - -| Nw |  Fb  |                 * Number of brick nodes  (Nu)        : F_POLY3  ( 5)
C             +----+    +----+                        * Number of corner from edge (Ci,j,k): C_POLY3  ( 5)
C       |    /       Fc      \      |
C       +----+                 +----+
C       | Cl | - - - - - - - - | Ck |
C       +----+                 +----+
c
C=====================================================================
C
C       +----+                 +----+
C       | Ci | - - - - - - - - | Cj |
C       +----+                 +----+
C            \       Fb       /     |
C       |     +----+    +----+    
C             | Nu |- - | Nv |      |
C       |     +----+    +----+   
C          Fe   |    Fa   |   Fc    |              HEXA GRAPH
C       |       |         |                          * number of binary codes             : S_HEXAE  ( 3)
C             +----+    +----+      |                * Number of graph for each subvolume : M_HEXAE  ( 2)
C       |     | Nx | - -| Nw |                       * Number of brick nodes  (Nu)        : F_HEXAE  ( 5)
C             +----+    +----+      |                * Number of corner from edge (Ci,j,k): C_HEXAE  ( 4)
C       |    /      Fd       \
C       +----+                 +----+
C       | Cl | - - - - - - - - | Ck |
C       +----+                 +----+
C
C=====================================================================
C
C              +----+       +----+
C              | Ci | - - - | Cj |
C              +----+  Fa   +----+
C            /      \      /      \
C           /        +----+        \
C                   | Nu  |       
C         /          +----+          \
C              Fc      |       Fd                  POLYG4 GRAPH
C       /              |               \             * number of binary codes             : S_POLY4  ( 4)
C       +----+       +----+        +----+            * Number of graph for each subvolume : M_POLY4  ( 2)
C       | Cn |       | Nv |        | Ck |            * Number of brick nodes  (Nu)        : F_POLY4  ( 6)
C       +----+       +----+        +----+            * Number of corner from edge (Ci,j,k): C_POLY4  ( 6)
C             \      /     \      /      
C       |      +----+       +----+      |
C         Fe   | Nx |       | Nw |   Ff
C       |      +----+       +----+      |
C             /         Fb        \
C       +----+                     +----+
C       | Cm | - - - - - - - - - - | Cl |
C       +----+                     +----+
C
C=====================================================================
C
C   +----+--------------------+----+                                                                                        
C   | C1 |                    | C2 | \
C   +----+                    +----+  \
C   |     \+----+      +----+/         \
C   |      | Nu | ---- | Nv |           \
C   |      +----+      +----+             +----+   POLYG4A GRAPH                                                                                   
C   |     /               |               | C3 |     * number of binary codes             : S_POLY4A  ( 4)                                         
C   +----+                |               +----+     * Number of graph for each subvolume : M_POLY4A  ( 2)                                         
C   | C6 |                |              /     |     * Number of brick nodes  (Nu)        : F_POLY4A  ( 6)                                         
C   +----+             +----+      +----+      |     * Number of corner from edge (Ci,j,k): C_POLY4A  ( 6)                                         
C         \            | Nw | ---- | Nx |      |                                                                                                     
C          \           +----+      +----+      |                                                                                                     
C           \         /                  \     |                                                                            
C            \  +----+                    +----+                                                                            
C             \ | C5 |--------------------| C4 |                                                                            
C               +----+                    +----+                                                                            
C
C=====================================================================
C
C
C             +----+                    +----+   
C            /| C1 |--------------------| C2 |  
C           / +----+                    +----+  
C          /        \                      |
C         /          +----+      +----+    |
C   +----+           | Nw | ---- | Nx |    |                        
C   | C5 |           +----+      +----+    |                   
C   +----+             |               \+----+                        
C   |     \+----+    +----+             | C3 |         
C   |      | Nu | ---| Nv |             +----+                                                                                       
C   |      +----+    +----+           /            POLYG4B GRAPH                                                                  
C   |     /                \         /               * number of binary codes             : S_POLY4B  ( 4)                        
C   +----+                  +----+  /                * Number of graph for each subvolume : M_POLY4B  ( 2)                        
C   | C6 |------------------| C3 | /                 * Number of brick nodes  (Nu)        : F_POLY4B  ( 6)                        
C   +----+                  +----+                   * Number of corner from edge (Ci,j,k): C_POLY4B  ( 6)                        
C
C=====================================================================
C
C       +----+                           +----+
C       | Ci | - - - - - - - - - - - - - | Cj |
C       +----+                           +----+
C       |     \                         /
C              \           Fb          /      |
C       |       +----+           +----+    
C               | Nu |- - - - - -| Nv |       |
C       |       +----+           +----+   
C          Fe     |   \     Fa  /    |     Fc |    POLYC GRAPH (continuity)
C       |         |    \       /     |              * number of binary codes             : S_POLYC  ( 8)
C               +----+  \     /   +----+      |     * Number of graph for each subvolume : M_POLYC  ( 1)
C       |       | Nx |- -| - | - -| Nw |            * Number of brick nodes  (Nu)        : F_POLYC  ( 4)
C               +----+   |   |    +----+      |     * Number of corner from edge (Ci,j,k): C_POLYC  ( 6)
C       |      /     |   | Fd|    |     \
C             /  Ff  |   |   |    |  Ff  \    |
C       +----+      +----+   +----+      +----+
C       | Cl | - - -| Cm | - | Cn |- - - | Ck |
C       +----+      +----+   +----+      +----+
C
C=====================================================================
C=====================================================================
C  variables are initialized in /src/int22/i22datainit.F
C=====================================================================

      !----------------------------------------------------!
      !Dimensions des tableaux                             !
      !----------------------------------------------------!
      !Combinaison number (binary codes)
      INTEGER        :: S_TETRA     
      INTEGER        :: S_PENTA     
      INTEGER        :: S_POLY3     
      INTEGER        :: S_HEXAE     
      INTEGER        :: S_POLY4
      INTEGER        :: S_POLY4A      
      INTEGER        :: S_POLY4B      
      INTEGER        :: S_POLYC      
      INTEGER        :: S_POLYH(8)      
      !sum
      INTEGER        :: S22_MAX 
      !Multiplicity
      INTEGER        :: M_TETRA     
      INTEGER        :: M_PENTA     
      INTEGER        :: M_POLY3     
      INTEGER        :: M_HEXAE     
      INTEGER        :: M_POLY4
      INTEGER        :: M_POLY4A
      INTEGER        :: M_POLY4B            
      INTEGER        :: M_POLYC      
      INTEGER        :: M_POLYH(8)      
      !Number of Corner from brick node
      INTEGER        :: N_TETRA     
      INTEGER        :: N_PENTA     
      INTEGER        :: N_POLY3     
      INTEGER        :: N_HEXAE     
      INTEGER        :: N_POLY4
      INTEGER        :: N_POLY4A
      INTEGER        :: N_POLY4B           
      INTEGER        :: N_POLYC      
      INTEGER        :: N_POLYH(8)      
      !Number of faces
      INTEGER        :: F_TETRA     
      INTEGER        :: F_PENTA     
      INTEGER        :: F_POLY3     
      INTEGER        :: F_HEXAE     
      INTEGER        :: F_POLY4
      INTEGER        :: F_POLY4A
      INTEGER        :: F_POLY4B     
      INTEGER        :: F_POLYC      
      INTEGER        :: F_POLYH(8)      
      !Number of Corner from edge intersection node
      INTEGER        :: C_TETRA     
      INTEGER        :: C_PENTA     
      INTEGER        :: C_POLY3     
      INTEGER        :: C_HEXAE     
      INTEGER        :: C_POLY4
      INTEGER        :: C_POLY4A
      INTEGER        :: C_POLY4B           
      INTEGER        :: C_POLYC      
      INTEGER        :: C_POLYH(8)      
      !First index in global array of subvolumes (1:106)
      INTEGER        :: D_TETRA     
      INTEGER        :: D_PENTA     
      INTEGER        :: D_POLY3     
      INTEGER        :: D_HEXAE     
      INTEGER        :: D_POLY4
      INTEGER        :: D_POLY4A
      INTEGER        :: D_POLY4B           
      INTEGER        :: D_POLYC      
      INTEGER        :: D_POLYH(8)      
      !----------------------------------------------------!
      !Tableaux contenant l'ensemble des codes binaires    !
      !  index1:combination number                         !
      !----------------------------------------------------!
      INTEGER,TARGET :: bCODE(106)  
   
      !----------------------------------------------------!
      !Denomination pour les Identification dintersection  !
      !  index1:combination number                         !
      !----------------------------------------------------!
      INTEGER LSECTYP
      CHARACTER*14,TARGET :: strCODE(106)

      !----------------------------------------------------!
      !SUBVOLUME 'G'RAPH : 'F'ACES                         !
      !  index1:face                                       !
      !  index2:combination number                         !
      !----------------------------------------------------!
      INTEGER,TARGET :: Gface(6,106)

      !----------------------------------------------------!
      !SUBVOLUME 'G'RAPH : 'N'umbert of 'P'oin't'          !
      !  index1:local polyhedron face                      !
      !----------------------------------------------------!
      INTEGER,TARGET :: GNPt(6,106)

      !----------------------------------------------------!
      !SUBVOLUME 'G'RAPH : 'N'umbert of 'Nod'e             !
      !  index1:local polyhedron face                      !
      !----------------------------------------------------!
      INTEGER,TARGET :: GNnod(6,106)
      
      !----------------------------------------------------!
      !SUBVOLUME 'G'RAPH : 'C'ORNER                        !
      !  index1:edge (corner=intersection_node)            !
      !  index2:combination number                         !
      !----------------------------------------------------!
      INTEGER,TARGET :: Gcorner(7,106)

      !----------------------------------------------------!
      !SUBVOLUME 'G'RAPH : brick 'N'ODES                   !
      !  index1:master node(on brick)                      !
      !  index2:combination number                         !
      !----------------------------------------------------!
      INTEGER,TARGET :: Gnode(4,106)
      INTEGER,TARGET :: bGnode(106)

      !----------------------------------------------------!
      ! TAG bits(from 1 to 106)for each of 106 combinations!
      !   which is using node (1-8) sent as index          !
      !   (KIND=14 = 112bits >= 106)                       !
      !----------------------------------------------------!
c      INTEGER(KIND=14),TARGET :: b22CombiNode(8)

      !----------------------------------------------------!
      ! TAG bits(from 1 to 106)for each of 106 combinations!
      !   which is using edge (1-12) sent as index         !
      !   (KIND=14 = 112bits >= 106)                       !
      !----------------------------------------------------!
c      INTEGER(KIND=14),TARGET :: b22CombiEdge(12)

      !----------------------------------------------------!
      ! function which from a combination code associates  !
      !   a combination type :                             !
      !   tetra:1,penta:2,poly3:3,hexa:4,poly4:5,etc...    !
      !   GetPolyhedraType : [1,106] -> [1,8]              !
      !----------------------------------------------------!
       INTEGER,TARGET :: GetPolyhedraType(106)

      !----------------------------------------------------!
      ! function which from a combination type (1-5)       !
      !  provides the number of tria needed to darw the    !
      !  isosurface                                        !
      !   a combination type :                             !
      !   tetra:1,penta:2,poly3:3,hexa:4,poly4:5           !
      !   GetNumTria : [1,5] -> [1,4]                      !
      !----------------------------------------------------!
       INTEGER,TARGET :: GetNumTria(8)

      !----------------------------------------------------!
      !SUBVOLUME 'G'RAPH : 'tria' intersection surface     !
      !  index1:edge local id i,j,k,l,m,n                  !
      !  index2:triangle id (ex 2 to draw a quad)          !
      !  index3:combination type
      !----------------------------------------------------!       
       INTEGER,TARGET :: Gtria(3,4,8)
       
