Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
C.../I22EDGES/

      COMMON/I22EDGES/
     .       iEDGE,          iFACE,            nodFACE,          bNodFACE,                IsNodeOnFace, 
     .       iFacesFromNode, iGetEdge,         iGetOppositeNode, IGetTransvNodes,         iGetEdgesFromFace,
     .       i22WhichEdge  , iGetAdjFace,      iNormalEdgesList, iGetNodeFromEdgeAndFace, iLeftEdge

      INTEGER, DIMENSION(2,12)  :: iEDGE                   !12 sans les diagonales, 24 avec les diagonales
      INTEGER, DIMENSION(2,2,6) :: iFACE                   ! 2 noeuds par face definissant une diagonale
      INTEGER, DIMENSION(6,4)   :: nodFACE                 ! les 4 noeuds d un HEXA8 definissant la facette
      INTEGER, DIMENSION(6)     :: bNodFACE                ! version binaire
      INTEGER, DIMENSION(8,3)   :: i22WhichEdge            !provide 3 edge number (from 1 to 12) related to local node given as index 1.
                                                           !  + : intersection point is the maximum of both if multiple intersection point on edge
                                                           !  - : intersection point is the minimum of both if multiple intersection point on edge
      LOGICAL, DIMENSION(8,6)   :: IsNodeOnFace            !tell if brick node is on a given face
      INTEGER, DIMENSION(8,3)   :: iFacesFromNode          !from a node 1:8 gives 3 connected faces
      INTEGER, DIMENSION(6,8)   :: iGetEdge                !from a node and a face, gives the normal edge
      INTEGER, DIMENSION(6,8)   :: iGetOppositeNode        !idem with opposite note instead of edge number
      INTEGER, DIMENSION(6,8,2) :: IGetTransvNodes         !from face and node, provides 2 other transverse nodes
      INTEGER, DIMENSION(6,4)   :: iGetEdgesFromFace       !from a face_id provides all edge_ids composing the face
      INTEGER, DIMENSION(6,4)   :: iNormalEdgesList        !from a face_id provides all edge_ids normal to the face.    
      INTEGER, DIMENSION(12,2)  :: iGetAdjFace             !from a edge_id get adjfaces
      INTEGER, DIMENSION(6,12)  :: iGetNodeFromEdgeAndFace ! From face_id and edge_id provide node_id which is intersection.
      INTEGER, DIMENSION(6,4)   :: iLeftEdge               ! From node_id 1:4 and face 1:6 gives Left edge (clockwise edge path)
       
C---------Numerotation locale des Arretes------------------
C                                                                                       
C                            +------------+------------+                                         
C         8+--------+7       | iEDGE(1,*) | iEDGE(2,*) |        +---12---+            +--------+ 
C         /|       /|        +------------+------------+       /|        /|          /|  3    /|   
C        / |      / |      1 +     1      +     2      +     11 10     9 6          / |      / |   
C      5+--------+6 |      2 +     1      +     4      +     +----8---+  |       6 +--------+  | 5
C       | 4|-----|--+3     3 +     1      +     5      +     |  |---5-|--+         |  |-----|--+ 
C       | /      | /       4 +     3      +     2      +     3 2      7 4          | /  4   | /         
C       |/       |/        5 +     3      +     4      +     |/       |/           |/       |/         
C       +--------+         6 +     3      +     7      +     +----1---+            +--------+         
C      1         2         7 +     6      +     2      +                                              
C                          8 +     6      +     5      +                               1         
C                          9 +     6      +     7      +                                             
C     LIST OF NODES       10 +     8      +     4      +   LIST OF EDGES       LIST OF FACES          
C     DEFINED WITH        11 +     8      +     5      +   DEFINED WITH        DEFINED WITH         
C     LOCAL IDs           12 +     8      +     7      +   LOCAL IDs               LOCAL IDs         
C                            +------------+------------+

