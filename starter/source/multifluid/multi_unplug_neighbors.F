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
Chd|  MULTI_UNPLUG_NEIGHBORS        source/multifluid/multi_unplug_neighbors.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE MULTI_UNPLUG_NEIGHBORS(ALE_CONNECTIVITY, IXS, IXQ, IXTG)
C-----------------------------------------------
C     D e s c r i p t i o n
C-----------------------------------------------
C     In the case where two elements are connected through
C     a Lagrangian face (or edge), that is to say that all nodes of
C     the shared edge are Lagrangian, there is no exchange in terms
C     of convection between this two elements : the face, or edge, has to 
C     be considered as a (moving) sliding wall.
C     Hence, the neighbors in IVOIS are set to zero for corresponding faces, or edges.
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
C     NIXS, NIXQ, NIXTG
C     NUMELS, NUMELQ, NUMELTG
#include "com04_c.inc"
C     N2D
#include "com01_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
      INTEGER, INTENT(IN) :: IXS(NIXS, *), IXQ(NIXQ, *), IXTG(NIXTG, *)
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IELEM, JELEM, KFACE, ELEM_TYPE, KFACE2, IAD, IAD2, LGTH, LGTH2
      INTEGER :: NODELIST(8), FLAGNODE(8), INODE
      INTEGER, DIMENSION(6, 4), TARGET :: HEXA_FACE
      INTEGER, DIMENSION(4, 3), TARGET :: TETRA_FACE
      INTEGER, DIMENSION(:, :), POINTER :: ELEM_FACE
      INTEGER :: NFACE, NFACE_NODE
      !     HEXA
      HEXA_FACE(1, 1) = 1
      HEXA_FACE(1, 2) = 2
      HEXA_FACE(1, 3) = 3
      HEXA_FACE(1, 4) = 4
      HEXA_FACE(2, 1) = 3
      HEXA_FACE(2, 2) = 4
      HEXA_FACE(2, 3) = 8
      HEXA_FACE(2, 4) = 7
      HEXA_FACE(3, 1) = 5
      HEXA_FACE(3, 2) = 6
      HEXA_FACE(3, 3) = 7
      HEXA_FACE(3, 4) = 8
      HEXA_FACE(4, 1) = 1
      HEXA_FACE(4, 2) = 2
      HEXA_FACE(4, 3) = 6
      HEXA_FACE(4, 4) = 5
      HEXA_FACE(5, 1) = 2
      HEXA_FACE(5, 2) = 3
      HEXA_FACE(5, 3) = 7
      HEXA_FACE(5, 4) = 6
      HEXA_FACE(6, 1) = 1
      HEXA_FACE(6, 2) = 4
      HEXA_FACE(6, 3) = 8
      HEXA_FACE(6, 4) = 5
!     TETRA
      TETRA_FACE(1, 1) = 2
      TETRA_FACE(1, 2) = 3
      TETRA_FACE(1, 3) = 7
      TETRA_FACE(2, 1) = 2
      TETRA_FACE(2, 2) = 6
      TETRA_FACE(2, 3) = 4
      TETRA_FACE(3, 1) = 4
      TETRA_FACE(3, 2) = 6
      TETRA_FACE(3, 3) = 7
      TETRA_FACE(4, 1) = 2
      TETRA_FACE(4, 2) = 7
      TETRA_FACE(4, 3) = 6
      IF (N2D == 0) THEN
C     3D case
         DO IELEM = 1, NUMELS
            IAD = ALE_CONNECTIVITY%ee_connect%iad_connect(IELEM)
            LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(IELEM+1) - IAD
            NODELIST(1) = IXS(2, IELEM)
            NODELIST(2) = IXS(3, IELEM)
            NODELIST(3) = IXS(4, IELEM)
            NODELIST(4) = IXS(5, IELEM)
            NODELIST(5) = IXS(6, IELEM)
            NODELIST(6) = IXS(7, IELEM)
            NODELIST(7) = IXS(8, IELEM)
            NODELIST(8) = IXS(9, IELEM)
            FLAGNODE(1:8) = 1
C     DEFAULT ELEM_TYPE = 1 : HEXA
            ELEM_TYPE = 1
            NFACE = 6
            NFACE_NODE = 4
            ELEM_FACE => HEXA_FACE
C     Test if TETRA
            IF ((NODELIST(1) == NODELIST(2)) .AND. 
     .           (NODELIST(3) == NODELIST(4)) .AND.
     .           (NODELIST(6) == NODELIST(7))) THEN
               ELEM_TYPE = 2
               NFACE = 4
               NFACE_NODE = 3
               ELEM_FACE => TETRA_FACE
            ENDIF
            
            DO INODE = 1, 8
               IF (ALE_CONNECTIVITY%NALE(NODELIST(INODE)) == 0) THEN
                  FLAGNODE(INODE) = 0
               ENDIF
            ENDDO
            
            DO KFACE = 1, LGTH
              JELEM = ALE_CONNECTIVITY%ee_connect%connected(IAD + KFACE - 1)     
              IF (JELEM > 0) THEN                                                
                 KFACE2 = ALE_CONNECTIVITY%ee_connect%iface2(IAD + KFACE - 1)    
                 IAD2 = ALE_CONNECTIVITY%ee_connect%iad_connect(JELEM)           
                 ALE_CONNECTIVITY%ee_connect%connected(IAD2 + KFACE - 1) = 0     
                 ALE_CONNECTIVITY%ee_connect%iface2(IAD2 + KFACE - 1) = 0        
                 ALE_CONNECTIVITY%ee_connect%connected(IAD2 + KFACE2 - 1) = 0    
                 ALE_CONNECTIVITY%ee_connect%iface2(IAD2 + KFACE2 - 1) = 0       
              ENDIF                                                              
            ENDDO  
         ENDDO
      ELSE
C     2D case
      ENDIF
      END SUBROUTINE MULTI_UNPLUG_NEIGHBORS
