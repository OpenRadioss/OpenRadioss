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
Chd|====================================================================
Chd|  MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|-- called by -----------
Chd|        ALENO                         source/airbag/fvmbag1.F       
Chd|        ALETG                         source/airbag/fvmbag1.F       
Chd|        APPLYSORT2FVM                 source/airbag/fvmesh0.F       
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|        DEALLOCATE_IGRSURF_SPLIT      source/spmd/deallocate_igrsurf_split.F
Chd|        DOMDEC2                       source/spmd/domdec2.F         
Chd|        DOMETIS                       source/spmd/domain_decomposition/grid2mat.F
Chd|        FILLCNE                       source/spmd/domdec2.F         
Chd|        FVBAG_VERTEX                  source/spmd/domain_decomposition/grid2mat.F
Chd|        FVBRIC1                       source/airbag/fvbric1.F       
Chd|        FVDIM                         source/airbag/fvmesh.F        
Chd|        FVMESH0                       source/airbag/fvmesh0.F       
Chd|        FVNODI                        source/airbag/fvmbag1.F       
Chd|        FVTGI                         source/airbag/fvmbag1.F       
Chd|        HM_READ_MONVOL_TYPE1          source/airbag/hm_read_monvol_type1.F
Chd|        HM_READ_MONVOL_TYPE10         source/airbag/hm_read_monvol_type10.F
Chd|        HM_READ_MONVOL_TYPE11         source/airbag/hm_read_monvol_type11.F
Chd|        HM_READ_MONVOL_TYPE2          source/airbag/hm_read_monvol_type2.F
Chd|        HM_READ_MONVOL_TYPE3          source/airbag/hm_read_monvol_type3.F
Chd|        HM_READ_MONVOL_TYPE4          source/airbag/hm_read_monvol_type4.F
Chd|        HM_READ_MONVOL_TYPE5          source/airbag/hm_read_monvol_type5.F
Chd|        HM_READ_MONVOL_TYPE6          source/airbag/hm_read_monvol_type6.F
Chd|        HM_READ_MONVOL_TYPE7          source/airbag/hm_read_monvol_type7.F
Chd|        HM_READ_MONVOL_TYPE8          source/airbag/hm_read_monvol_type8.F
Chd|        HM_READ_MONVOL_TYPE9          source/airbag/hm_read_monvol_type9.F
Chd|        HM_READ_THGROU                source/output/th/hm_read_thgrou.F
Chd|        HM_THGRKI_VENT                source/output/th/hm_thgrki_vent.F
Chd|        HYPERMESH_TETRA               stub/fvmbags_stub.F           
Chd|        IGRSURF_SPLIT                 source/spmd/igrsurf_split.F   
Chd|        INIT_MONVOL                   source/airbag/init_monvol.F   
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        MESHGEMS_TETRA                stub/fvmbags_stub.F           
Chd|        MONVOL_BUILD_EDGES            share/modules1/monvol_struct_mod.F
Chd|        MONVOL_CHECK_DELETE_DUPLICATEDsource/airbag/monvol_check_delete_duplicated.F
Chd|        MONVOL_ORIENT_SURF            share/modules1/monvol_struct_mod.F
Chd|        MONVOL_REVERSE_NORMALS        share/modules1/monvol_struct_mod.F
Chd|        MONVOL_TRIANGULATE_SURFACE    source/airbag/monvol_triangulate_surface.F
Chd|        READ_MONVOL                   source/airbag/read_monvol.F   
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|        ST_QAPRINT_MONVOL             source/output/qaprint/st_qaprint_monvol.F
Chd|        W_MONVOL                      source/restart/ddsplit/w_monvol.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE MONVOL_STRUCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "my_real.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
#include "nchar_c.inc"
      TYPE MONVOL_METADATA_
      INTEGER :: NVOLU
!     Temporary
      INTEGER :: LCA
      INTEGER, DIMENSION(:, :), ALLOCATABLE :: ICBAG
      my_real, DIMENSION(:, :), ALLOCATABLE :: RCBAG
      END TYPE MONVOL_METADATA_

      TYPE MONVOL_STRUCT_
!     Monvol type
      INTEGER :: TYPE
!     Monvol ID
      INTEGER :: ID
!     
      INTEGER :: NCA
!     Monvol name
      CHARACTER(LEN = nchartitle) :: TITLE
!     External surface Id, Internal surface id (internal numbering)
      INTEGER :: EXT_SURFID, INT_SURFID
!     IVOLU -> integer attributes
      INTEGER, DIMENSION(:), ALLOCATABLE :: IVOLU
!     RVOLU -> double precision attributes
      my_real, DIMENSION(:), ALLOCATABLE :: RVOLU
!     Number of injectors
      INTEGER :: NJET
!     Integer info on injectors (NJET x NIBJET)
      INTEGER, DIMENSION(:, :), ALLOCATABLE :: IBAGJET
!     Real info on injectors (NJET x NRBJET)
      my_real, DIMENSION(:, :), ALLOCATABLE :: RBAGJET
!     Vent holes and porous surfaces
      INTEGER :: NVENT, NPORSURF
!     Integer data
      INTEGER, DIMENSION(:, :), ALLOCATABLE :: IBAGHOL
!     Real data
      my_real, DIMENSION(:, :), ALLOCATABLE :: RBAGHOL
!     BUFALEI
      INTEGER :: NNS, NNI            ! Number of external, internal surface node
      INTEGER, DIMENSION(:), ALLOCATABLE :: NODES
      INTEGER :: NTG, NTGI
      INTEGER, DIMENSION(:, :), ALLOCATABLE :: ELEM
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAGEL
      INTEGER, DIMENSION(:), ALLOCATABLE :: FVBAG_ELEMID
!     Adress of element inside IXC and IXTG
      INTEGER, DIMENSION(:), ALLOCATABLE :: ELTG
!     Store mat number of triagnel surface
      INTEGER, DIMENSION(:), ALLOCATABLE :: MATTG
!     
      INTEGER :: NBRIC
      INTEGER, DIMENSION(:, :), ALLOCATABLE :: TBRIC, TFAC
!     TAGELS
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGELS
!
      INTEGER :: NNA
      INTEGER, DIMENSION(:), ALLOCATABLE :: IBUFA
!
      INTEGER :: NTGA
      INTEGER, DIMENSION(:, :), ALLOCATABLE :: ELEMA, BRNA
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGELA
      INTEGER, DIMENSION(:, :), ALLOCATABLE :: NCONA
!
      my_real, DIMENSION(:, :), ALLOCATABLE :: VELOCITY, NODE_COORD
      my_real, DIMENSION(:), ALLOCATABLE :: POROSITY, ELAREA
      INTEGER, DIMENSION(:, :), ALLOCATABLE :: THSURF_TAG
!     Automatic meshing
      INTEGER :: IMESH_ALL, KMESH
!     Automatic surface hole filling
      INTEGER :: NB_FILL_TRI
      INTEGER, DIMENSION(:), ALLOCATABLE :: FILL_TRI
!     Edges connectivity
      LOGICAL :: EDGES_BUILT
      INTEGER :: NEDGE
      INTEGER, DIMENSION(:), ALLOCATABLE :: EDGE_NODE1, EDGE_NODE2, EDGE_ELEM, IAD_EDGE_ELEM
!     Keep old adresses until full conversion is done
      INTEGER :: IADALE, IADALE2, IADALE3, IADALE4, IADALE5, IADALE6, IADALE7, IADALE8, IADALE9, 
     .     IADALE10, IADALE11, IADALE12, IADALE13, KRA5, KRA6, KR5
      LOGICAL :: OK_REORIENT

      INTEGER, DIMENSION(:), ALLOCATABLE :: NUMBER_TRI_PER_PROC      
      END TYPE MONVOL_STRUCT_

      CONTAINS
Chd|====================================================================
Chd|  COPY_TO_MONVOL                share/modules1/monvol_struct_mod.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE COPY_TO_MONVOL(T_MONVOL, LICBAG, ICBAG, SMONVOL, MONVOL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "param_c.inc"
#include "com04_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: SMONVOL, LICBAG
      INTEGER, DIMENSION(LICBAG), INTENT(IN) :: ICBAG
      INTEGER, DIMENSION(SMONVOL), INTENT(INOUT) :: MONVOL
      TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(IN) :: T_MONVOL
C-----------------------------------------------
C     L o c a l   v a r i a b l e s
C-----------------------------------------------
      INTEGER :: II, JJ, KK, I, ICOPY, N
      INTEGER :: NVENT
      INTEGER :: SHIFT
      
      SHIFT = LICBAG
      DO N = 1, NVOLU
         SHIFT = SHIFT + NIMV
         SHIFT = SHIFT + NIBJET * T_MONVOL(N)%NJET
         SHIFT = SHIFT + NIBHOL * T_MONVOL(N)%NVENT
      ENDDO

      I = 1
      DO II = 1, NVOLU
         DO JJ = 1, NIMV
            MONVOL(I) = T_MONVOL(II)%IVOLU(JJ)
            I = I + 1
         ENDDO
      ENDDO
      MONVOl(I:I + LICBAG - 1) = ICBAG(1:LICBAG)
      I = I + LICBAG
      DO II = 1, NVOLU
         DO JJ = 1, T_MONVOL(II)%NJET
            DO KK = 1, NIBJET
               MONVOL(I) = T_MONVOL(II)%IBAGJET(KK, JJ)
               I = I + 1
            ENDDO
         ENDDO
      ENDDO
      DO II = 1, NVOLU
         NVENT = T_MONVOL(II)%NVENT
         DO JJ = 1, NVENT
            DO KK = 1, NIBHOL
               MONVOL(I) = T_MONVOL(II)%IBAGHOL(KK, JJ)
               I = I + 1
            ENDDO
         ENDDO
      ENDDO
      ICOPY = I
      DO N = 1, NVOLU
         IF (T_MONVOL(N)%TYPE == 6 .OR. T_MONVOL(N)%TYPE == 8) THEN
            ICOPY = SHIFT + T_MONVOL(N)%IADALE
         ENDIF
         DO I = 1, T_MONVOL(N)%NNS + T_MONVOL(N)%NNI
            MONVOL(ICOPY) = T_MONVOL(N)%NODES(I)
            ICOPY = ICOPY + 1
         ENDDO
         DO I = 1, T_MONVOL(N)%NTG + T_MONVOL(N)%NTGI
            MONVOL(ICOPY) = T_MONVOL(N)%ELEM(1, I)
            ICOPY = ICOPY + 1
            MONVOL(ICOPY) = T_MONVOL(N)%ELEM(2, I)
            ICOPY = ICOPY + 1
            MONVOL(ICOPY) = T_MONVOL(N)%ELEM(3, I)
            ICOPY = ICOPY + 1
         ENDDO
         DO I = 1, T_MONVOL(N)%NTG + T_MONVOL(N)%NTGI
            MONVOL(ICOPY) = T_MONVOL(N)%ITAGEL(I)
            ICOPY = ICOPY + 1
         ENDDO
         DO I = 1, T_MONVOL(N)%NTG + T_MONVOL(N)%NTGI
            MONVOL(ICOPY) = T_MONVOL(N)%ELTG(I)
            ICOPY = ICOPY + 1
         ENDDO
         DO I = 1, T_MONVOL(N)%NTG + T_MONVOL(N)%NTGI
            MONVOL(ICOPY) = T_MONVOL(N)%MATTG(I)
            ICOPY = ICOPY + 1
         ENDDO
         DO I = 1, T_MONVOL(N)%NBRIC
            DO II = 1, 2
               MONVOL(ICOPY) = T_MONVOL(N)%TBRIC(II, I)
               ICOPY = ICOPY + 1
            ENDDO
         ENDDO
         DO I = 1, T_MONVOL(N)%NBRIC
            DO II = 1, 12
               MONVOL(ICOPY) = T_MONVOL(N)%TFAC(II, I)
               ICOPY = ICOPY + 1
            ENDDO
         ENDDO
         DO I = 1, T_MONVOL(N)%NTG + 2 * T_MONVOL(N)%NTGI
            MONVOL(ICOPY) = T_MONVOL(N)%TAGELS(I)
            ICOPY = ICOPY + 1
         ENDDO
         ICOPY = T_MONVOL(N)%IADALE8 + SHIFT
         IF (T_MONVOL(N)%IADALE8 == 0) ICOPY = ICOPY + 1
         DO I = 1, T_MONVOL(N)%NNA
            MONVOL(ICOPY) = T_MONVOL(N)%IBUFA(I)
            ICOPY = ICOPY + 1
         ENDDO
         IF (T_MONVOL(N)%NBRIC == 0) THEN
            ICOPY = T_MONVOL(N)%IADALE9  + SHIFT
         IF (T_MONVOL(N)%IADALE9 == 0) ICOPY = ICOPY + 1
         ENDIF
         DO I = 1, T_MONVOL(N)%NTGA
            DO II = 1, 3
               MONVOL(ICOPY) = T_MONVOL(N)%ELEMA(II, I)
               ICOPY = ICOPY + 1
            ENDDO
         ENDDO
         DO I = 1, T_MONVOL(N)%NTGA
            MONVOL(ICOPY) = T_MONVOL(N)%TAGELA(I)
            ICOPY = ICOPY + 1
         ENDDO
         DO I = 1, T_MONVOL(N)%NBRIC
            DO II = 1, 8
               MONVOL(ICOPY) = T_MONVOL(N)%BRNA(II, I)
               ICOPY = ICOPY + 1
            ENDDO
         ENDDO
         DO I = 1, T_MONVOL(N)%NNA
            DO II = 1, 16
               MONVOL(ICOPY) = T_MONVOL(N)%NCONA(II, I)
               ICOPY = ICOPY + 1
            ENDDO
         ENDDO
         IF (T_MONVOL(N)%NTGI > 0) THEN
            DO JJ = 1, NSURF
               DO I = 1, T_MONVOL(N)%NTGI + 1
                  MONVOL(ICOPY) = T_MONVOL(N)%THSURF_TAG(JJ, I)
                  ICOPY = ICOPY + 1
               ENDDO
            ENDDO
         ENDIF
      ENDDO
      END SUBROUTINE COPY_TO_MONVOL

Chd|====================================================================
Chd|  COPY_TO_VOLMON                share/modules1/monvol_struct_mod.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE COPY_TO_VOLMON(T_MONVOL, LRCBAG, RCBAG, SVOLMON, VOLMON)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "param_c.inc"
#include "com04_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: SVOLMON, LRCBAG
      my_real, DIMENSION(LRCBAG), INTENT(IN) :: RCBAG
      my_real, DIMENSION(SVOLMON), INTENT(INOUT) :: VOLMON
      TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(IN) :: T_MONVOL
C-----------------------------------------------
C     L o c a l   v a r i a b l e s
C-----------------------------------------------
      INTEGER :: II, JJ, KK, I, ICOPY, N
      INTEGER :: NVENT, NJET
      INTEGER :: SHIFT
!     *****    !
!     RVOLU    !
!     *****    !
      ICOPY = 1
      DO II = 1, NVOLU
         DO JJ = 1, NRVOLU
            VOLMON(ICOPY) = T_MONVOL(II)%RVOLU(JJ)
            ICOPY = ICOPY + 1
         ENDDO
      ENDDO
      DO I = 1, LRCBAG
         VOLMON(ICOPY) = RCBAG(I)
         ICOPY = ICOPY + 1
      ENDDO

      DO II = 1, NVOLU
         NJET = T_MONVOL(II)%NJET
         DO JJ = 1, NJET
            DO KK = 1, NRBJET
               VOLMON(ICOPY) = T_MONVOL(II)%RBAGJET(KK, JJ)
               ICOPY = ICOPY + 1
            ENDDO
         ENDDO
      ENDDO
!     RBAGHOL
      DO II = 1, NVOLU
         NVENT = T_MONVOL(II)%NVENT
         DO JJ = 1, NVENT
            DO KK = 1, NRBHOL
               VOLMON(ICOPY) = T_MONVOL(II)%RBAGHOL(KK, JJ)
               ICOPY = ICOPY + 1
            ENDDO
         ENDDO
      ENDDO

!     *******    !
!     BUFALER    !
!     *******    !
      DO II = 1, NVOLU
!     Velocities and node coordinates
         IF (T_MONVOL(II)%KR5 > 0) THEN
            ICOPY = T_MONVOL(II)%KR5
            DO JJ = 1, T_MONVOL(II)%NTG + T_MONVOL(II)%NTGI 
               VOLMON(ICOPY) = T_MONVOL(II)%ELAREA(JJ)
               ICOPY = ICOPY + 1
            ENDDO
         ENDIF
         IF (T_MONVOL(II)%KRA5 > 0) THEN
            ICOPY = T_MONVOL(II)%KRA5
            DO JJ = 1, T_MONVOL(II)%NNA
               VOLMON(ICOPY) = T_MONVOL(II)%NODE_COORD(1, JJ)
               ICOPY = ICOPY + 1
               VOLMON(ICOPY) = T_MONVOL(II)%NODE_COORD(2, JJ)
               ICOPY = ICOPY + 1
               VOLMON(ICOPY) = T_MONVOL(II)%NODE_COORD(3, JJ)
               ICOPY = ICOPY + 1
            ENDDO
            ICOPY = ICOPY + 3 * T_MONVOL(II)%NNA
            DO JJ = 1, T_MONVOL(II)%NTGI
               VOLMON(ICOPY) = T_MONVOL(II)%POROSITY(JJ)
               ICOPY = ICOPY + 1
            ENDDO
         ENDIF
      ENDDO
      END SUBROUTINE COPY_TO_VOLMON

Chd|====================================================================
Chd|  MONVOL_CHECK_SURFCLOSE        share/modules1/monvol_struct_mod.F
Chd|-- called by -----------
Chd|        HM_READ_MONVOL_TYPE1          source/airbag/hm_read_monvol_type1.F
Chd|        HM_READ_MONVOL_TYPE10         source/airbag/hm_read_monvol_type10.F
Chd|        HM_READ_MONVOL_TYPE11         source/airbag/hm_read_monvol_type11.F
Chd|        HM_READ_MONVOL_TYPE2          source/airbag/hm_read_monvol_type2.F
Chd|        HM_READ_MONVOL_TYPE3          source/airbag/hm_read_monvol_type3.F
Chd|        HM_READ_MONVOL_TYPE4          source/airbag/hm_read_monvol_type4.F
Chd|        HM_READ_MONVOL_TYPE5          source/airbag/hm_read_monvol_type5.F
Chd|        HM_READ_MONVOL_TYPE6          source/airbag/hm_read_monvol_type6.F
Chd|        HM_READ_MONVOL_TYPE7          source/airbag/hm_read_monvol_type7.F
Chd|        HM_READ_MONVOL_TYPE8          source/airbag/hm_read_monvol_type8.F
Chd|        HM_READ_MONVOL_TYPE9          source/airbag/hm_read_monvol_type9.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MONVOL_BUILD_EDGES            share/modules1/monvol_struct_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE MONVOL_CHECK_SURFCLOSE(T_MONVOLN, ITAB, SURF, X)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"      
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C     NSURF
#include      "com04_c.inc"
C     NIMV
#include      "param_c.inc"
C     nchartitle
#include      "scr17_c.inc"
C     IOUT
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: ITAB(*)
      TYPE(SURF_), INTENT(IN) :: SURF
      TYPE(MONVOL_STRUCT_), INTENT(INOUT) :: T_MONVOLN
      my_real, INTENT(IN) :: X(3, *)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: JJ, NEDGE, NELEM, IEDGE, NODE1, NODE2, INODE
      INTEGER :: NB_FREE_EDGE
      INTEGER(8) :: graph_ptr, tri_ptr, tri_ptr_global
      INTEGER, DIMENSION(:), ALLOCATABLE :: FREE_EDGES_ID, FREE_EDGES, LOCAL_NODE_ID, 
     .     GLOBAL_NODE_ID
      INTEGER :: NB_CONNECTED_COMPS, TOTAL_SIZE, II, ITRI
      INTEGER, DIMENSION(:), ALLOCATABLE :: PATHS, SIZES, CYCLES, SHIFT
      INTEGER :: NPT, NTRI
      my_real, DIMENSION(:), ALLOCATABLE :: NODE_COORD
      INTEGER, DIMENSION(:), ALLOCATABLE :: TRI_LIST
      CHARACTER(LEN=1024) :: FILENAME
C-----------------------------------------------
C     B e g i n n i n g   o f   s o u r c e
C-----------------------------------------------

!     ********************     !
!     ** Initialization **     !
!     ********************     !
      graph_ptr = 0
      tri_ptr = 0
      tri_ptr_global = 0

!     *****************************     !
!     ** Build edge connectivity **     !
!     *****************************     !
      CALL MONVOL_BUILD_EDGES(T_MONVOLN, SURF)

!     *************************     !
!     ** Identify free edges **     !
!     *************************     !
      NEDGE = T_MONVOLN%NEDGE
      NB_FREE_EDGE = 0
      DO JJ = 1, NEDGE
         NELEM = T_MONVOLN%IAD_EDGE_ELEM(JJ + 1) - T_MONVOLN%IAD_EDGE_ELEM(JJ)
         IF (NELEM == 1) THEN
            NB_FREE_EDGE = NB_FREE_EDGE + 1
         ENDIF
      ENDDO

!     ************************     !
!     ** Recover free edges **     !
!     ************************     !
      IF (NB_FREE_EDGE > 0) THEN
         ALLOCATE(FREE_EDGES_ID(NB_FREE_EDGE))
         ALLOCATE(FREE_EDGES(2 * NB_FREE_EDGE))
         ALLOCATE(LOCAL_NODE_ID(NUMNOD))
         LOCAL_NODE_ID(1:NUMNOD) = 0
         IEDGE = 0
         INODE = 0
         DO JJ = 1, NEDGE
            NELEM = T_MONVOLN%IAD_EDGE_ELEM(JJ + 1) - T_MONVOLN%IAD_EDGE_ELEM(JJ)
            IF (NELEM == 1) THEN
               IEDGE = IEDGE + 1
               NODE1 = T_MONVOLN%EDGE_NODE1(JJ)
               NODE2 = T_MONVOLN%EDGE_NODE2(JJ)
               FREE_EDGES(2 * (IEDGE - 1) + 1) = NODE1
               FREE_EDGES(2 * (IEDGE - 1) + 2) = NODE2
               IF (LOCAL_NODE_ID(NODE1) == 0) THEN
                  INODE = INODE + 1
                  LOCAL_NODE_ID(NODE1) = INODE
               ENDIF
               IF (LOCAL_NODE_ID(NODE2) == 0) THEN
                  INODE = INODE + 1
                  LOCAL_NODE_ID(NODE2) = INODE
               ENDIF
            ENDIF
         ENDDO
         ALLOCATE(GLOBAL_NODE_ID(INODE))
         DO II = 1, NUMNOD
            IF(LOCAL_NODE_ID(II) > 0) THEN
               GLOBAL_NODE_ID(LOCAL_NODE_ID(II)) = II
            ENDIF
         ENDDO
!     change edges node id to local node id
         DO IEDGE = 1, NB_FREE_EDGE
            FREE_EDGES(2 * (IEDGE - 1) + 1) = LOCAL_NODE_ID(FREE_EDGES(2 * (IEDGE - 1) + 1)) - 1
            FREE_EDGES(2 * (IEDGE - 1) + 2) = LOCAL_NODE_ID(FREE_EDGES(2 * (IEDGE - 1) + 2)) - 1
         ENDDO
         CALL GRAPH_BUILD_PATH(INODE, NB_FREE_EDGE, FREE_EDGES, 
     .        NB_CONNECTED_COMPS, graph_ptr)

         ALLOCATE(SIZES(NB_CONNECTED_COMPS), CYCLES(NB_CONNECTED_COMPS))
         CALL GRAPH_BUILD_CYCLES(graph_ptr, CYCLES)
         CALL GRAPH_GET_SIZES(graph_ptr, SIZES)
         TOTAL_SIZE = 0
         ALLOCATE(SHIFT(NB_CONNECTED_COMPS + 1))
         SHIFT(1) = 0
         DO II = 1, NB_CONNECTED_COMPS
            SHIFT(II + 1) = SHIFT(II) + SIZES(II)
            TOTAL_SIZE = TOTAL_SIZE + SIZES(II)
         ENDDO
         ALLOCATE(PATHS(TOTAL_SIZE))
         CALL GRAPH_GET_PATH(graph_ptr, PATHS)
         CALL GRAPH_FREE_MEMORY(graph_ptr)
         
         CALL TAB1_INIT(tri_ptr_global)
#ifdef DNC
         DO II = 1, NB_CONNECTED_COMPS
            IF (CYCLES(II) == 0) THEN
!     The connected component is not a hole -> cannot be closed
               CYCLE
            ENDIF
            NPT = SIZES(II)
            ALLOCATE(NODE_COORD(3 * NPT))
            DO JJ = 1, NPT
               NODE_COORD(3 * (JJ - 1) + 1) = X(1, GLOBAL_NODE_ID(1+PATHS(JJ + SHIFT(II))))
               NODE_COORD(3 * (JJ - 1) + 2) = X(2, GLOBAL_NODE_ID(1+PATHS(JJ + SHIFT(II))))
               NODE_COORD(3 * (JJ - 1) + 3) = X(3, GLOBAL_NODE_ID(1+PATHS(JJ + SHIFT(II))))
            ENDDO
            CALL HM_FILL_LOOP(NPT, NODE_COORD, NTRI, tri_ptr)
            ALLOCATE(TRI_LIST(3 * NTRI))
            CALL HM_FILL_LOOP_GET_TRI(TRI_LIST, tri_ptr)
            DO JJ = 1, 3 * NTRI
               TRI_LIST(JJ) = GLOBAL_NODE_ID(1+PATHS(SHIFT(II) + TRI_LIST(JJ) + 1))
            ENDDO
            CALL TRI_FREE_MEMORY(tri_ptr)
            CALL TAB1_APPEND_TAB(tri_ptr_global, 3 * NTRI, TRI_LIST)
            DEALLOCATE(TRI_LIST)
            DEALLOCATE(NODE_COORD)
         ENDDO
#endif
         
         CALL TAB1_GET_SIZE(tri_ptr_global, NTRI)
         IF (NTRI > 0) THEN
            T_MONVOLN%NB_FILL_TRI = NTRI / 3
            ALLOCATE(T_MONVOLN%FILL_TRI(NTRI))
            WRITE(IOUT, 1000) NB_FREE_EDGE, NB_CONNECTED_COMPS
            WRITE(IOUT, 1001) T_MONVOLN%NB_FILL_TRI
            CALL TAB1_GET(tri_ptr_global, T_MONVOLN%FILL_TRI)
            CALL TAB1_FREE_MEMORY(tri_ptr_global)

!     ****************************************     !
!     ** Build edge connectivity once again **     !
!     ****************************************     !
            CALL MONVOL_BUILD_EDGES(T_MONVOLN, SURF)

!     *************************     !
!     ** Identify free edges **     !
!     *************************     !
            NEDGE = T_MONVOLN%NEDGE
            NB_FREE_EDGE = 0
            DO JJ = 1, NEDGE
               NELEM = T_MONVOLN%IAD_EDGE_ELEM(JJ + 1) - T_MONVOLN%IAD_EDGE_ELEM(JJ)
               IF (NELEM == 1) THEN
                  NB_FREE_EDGE = NB_FREE_EDGE + 1
               ENDIF
            ENDDO

            IF (NB_FREE_EDGE > 0) THEN
               CALL ANCMSG(MSGID = 1875, ANMODE = ANINFO, MSGTYPE = MSGWARNING,
     .              I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
               WRITE(IOUT, 1002) NB_FREE_EDGE
            ENDIF
         ELSE
            IF (NB_FREE_EDGE > 0) THEN
               CALL ANCMSG(MSGID = 1875, ANMODE = ANINFO, MSGTYPE = MSGWARNING,
     .              I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
               WRITE(IOUT, 1002) NB_FREE_EDGE
            ENDIF
         ENDIF
      ENDIF
!     *************************     !
!     ** Memory deallocation **     !
!     *************************     !
      IF (ALLOCATED(FREE_EDGES_ID)) DEALLOCATE(FREE_EDGES_ID)
      IF (ALLOCATED(FREE_EDGES)) DEALLOCATE(FREE_EDGES)
      IF (ALLOCATED(LOCAL_NODE_ID)) DEALLOCATE(LOCAL_NODE_ID)
      IF (ALLOCATED(GLOBAL_NODE_ID)) DEALLOCATE(GLOBAL_NODE_ID)
      IF (ALLOCATED(SIZES)) DEALLOCATE(SIZES)
      IF (ALLOCATED(SHIFT)) DEALLOCATE(SHIFT)
      IF (ALLOCATED(PATHS)) DEALLOCATE(PATHS)
      IF (ALLOCATED(CYCLES)) DEALLOCATE(CYCLES)
C-----------------------------------------------
C     E n d   o f   s o u r c e
C-----------------------------------------------
 1000 FORMAT(
     .     /5X,'EXTERNAL SURFACE OF THE MONITORED VOLUME IS NOT A CLOSED SURFACE', 
     .     /5X, '  NUMBER OF FREE EDGES: ',I10,
     .     /5X, '  NUMBER OF HOLES: ', I10)  
 1001 FORMAT(
     .     5X,'  ----> AUTOMATIC CLOSURE ACTIVATED'
     .     /5X,'  ----> SURFACE CLOSE WITH: ',I10,' TRIANGLES')  
 1002 FORMAT(
     .     /5X, '  NUMBER OF REMAINING FREE EDGES: ',I10) 
      END SUBROUTINE MONVOL_CHECK_SURFCLOSE

Chd|====================================================================
Chd|  MONVOL_COMPUTE_VOLUME         share/modules1/monvol_struct_mod.F
Chd|-- called by -----------
Chd|        HM_READ_MONVOL_TYPE1          source/airbag/hm_read_monvol_type1.F
Chd|        HM_READ_MONVOL_TYPE10         source/airbag/hm_read_monvol_type10.F
Chd|        HM_READ_MONVOL_TYPE11         source/airbag/hm_read_monvol_type11.F
Chd|        HM_READ_MONVOL_TYPE2          source/airbag/hm_read_monvol_type2.F
Chd|        HM_READ_MONVOL_TYPE3          source/airbag/hm_read_monvol_type3.F
Chd|        HM_READ_MONVOL_TYPE4          source/airbag/hm_read_monvol_type4.F
Chd|        HM_READ_MONVOL_TYPE5          source/airbag/hm_read_monvol_type5.F
Chd|        HM_READ_MONVOL_TYPE6          source/airbag/hm_read_monvol_type6.F
Chd|        HM_READ_MONVOL_TYPE7          source/airbag/hm_read_monvol_type7.F
Chd|        HM_READ_MONVOL_TYPE8          source/airbag/hm_read_monvol_type8.F
Chd|        HM_READ_MONVOL_TYPE9          source/airbag/hm_read_monvol_type9.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE MONVOL_COMPUTE_VOLUME(T_MONVOLN, TITLE, IVOLU, SURF, 
     .     ITAB, NODE_COORD, PM, GEO, IXC, IXTG, 
     .     SA, ROT, VOL, VMIN, VEPS, SV)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"      
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C     NSURF
#include      "com04_c.inc"
C     NIMV
#include      "param_c.inc"
C     nchartitle
#include      "scr17_c.inc"
C     IOUT
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(MONVOL_STRUCT_), INTENT(IN) :: T_MONVOLN
      CHARACTER, INTENT(IN) :: TITLE*nchartitle
      INTEGER, INTENT(IN) :: IVOLU(NIMV), ITAB(*), IXC(NIXC, *), IXTG(NIXTG, *)
      TYPE(SURF_), INTENT(IN) :: SURF
      my_real, INTENT(IN) :: NODE_COORD(3, *), GEO(NPROPG, *), PM(NPROPM, *)
      my_real, INTENT(INOUT) :: SA, ROT, VOL, VMIN, VEPS, SV
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: J, I
      INTEGER :: IJET, NN, I1, I2, I3, I4, ISH34
      my_real :: SX, SY, SZ, DIR
      my_real :: XX, YY, ZZ, X13, Y13, Z13, X24, Y24, Z24, NX, NY, NZ, DS
C-----------------------------------------------
C     B e g i n n i n g   o f   s o u r c e
C-----------------------------------------------
      NN = SURF%NSEG

      IJET= 0
      VOL = ZERO
      ROT = ZERO
      SX  = ZERO
      SY  = ZERO
      SZ  = ZERO
      SA  = ZERO

      DO J = 1, NN
         DIR = HALF
         I1 = SURF%NODES(J,1)
         I2 = SURF%NODES(J,2)
         I3 = SURF%NODES(J,3)
         I4 = SURF%NODES(J,4)
         ISH34 = SURF%ELTYP(J)
         I = SURF%ELEM(J)
         IF(ISH34==7)I4 = I3
         XX =HALF*(NODE_COORD(1,I1)+NODE_COORD(1,I2))
         YY =HALF*(NODE_COORD(2,I1)+NODE_COORD(2,I2))
         ZZ =HALF*(NODE_COORD(3,I1)+NODE_COORD(3,I2))

         X13=NODE_COORD(1,I3)-NODE_COORD(1,I1)
         Y13=NODE_COORD(2,I3)-NODE_COORD(2,I1)
         Z13=NODE_COORD(3,I3)-NODE_COORD(3,I1)
         X24=NODE_COORD(1,I4)-NODE_COORD(1,I2)
         Y24=NODE_COORD(2,I4)-NODE_COORD(2,I2)
         Z24=NODE_COORD(3,I4)-NODE_COORD(3,I2)
         NX=DIR*(Y13*Z24-Y24*Z13)
         NY=DIR*(Z13*X24-Z24*X13)
         NZ=DIR*(X13*Y24-X24*Y13)
         VOL = VOL+THIRD*( NX*XX+NY*YY+NZ*ZZ )
         SX = SX + NX
         SY = SY + NY
         SZ = SZ + NZ
         DS = SQRT(NX*NX+NY*NY+NZ*NZ)
         SA = SA + DS
         IF(ISH34==3)THEN
            ROT = ROT + PM(1,IXC(1,I))*GEO(1,IXC(6,I))*DS
         ELSEIF(ISH34==7)THEN
            ROT = ROT + PM(1,IXTG(1,I))*GEO(1,IXTG(5,I))*DS
         ENDIF
      ENDDO

      DO J = 1, T_MONVOLN%NB_FILL_TRI
         DIR = HALF
         I1 = T_MONVOLN%FILL_TRI(3 * (J - 1) + 1)
         I2 = T_MONVOLN%FILL_TRI(3 * (J - 1) + 2)
         I3 = T_MONVOLN%FILL_TRI(3 * (J - 1) + 3)
         I4 = I3

         XX =HALF*(NODE_COORD(1,I1)+NODE_COORD(1,I2))
         YY =HALF*(NODE_COORD(2,I1)+NODE_COORD(2,I2))
         ZZ =HALF*(NODE_COORD(3,I1)+NODE_COORD(3,I2))

         X13=NODE_COORD(1,I3)-NODE_COORD(1,I1)
         Y13=NODE_COORD(2,I3)-NODE_COORD(2,I1)
         Z13=NODE_COORD(3,I3)-NODE_COORD(3,I1)
         X24=NODE_COORD(1,I4)-NODE_COORD(1,I2)
         Y24=NODE_COORD(2,I4)-NODE_COORD(2,I2)
         Z24=NODE_COORD(3,I4)-NODE_COORD(3,I2)
         NX=DIR*(Y13*Z24-Y24*Z13)
         NY=DIR*(Z13*X24-Z24*X13)
         NZ=DIR*(X13*Y24-X24*Y13)
         VOL = VOL+THIRD*( NX*XX+NY*YY+NZ*ZZ )
         SX = SX + NX
         SY = SY + NY
         SZ = SZ + NZ
         DS = SQRT(NX*NX+NY*NY+NZ*NZ)
         SA = SA + DS
      ENDDO
C     
      ROT = ROT/SA
C     
      SV   = SQRT(SX*SX+SY*SY+SZ*SZ)
      VMIN = EM4*SA**THREE_HALF
      VEPS = MAX(ZERO,VMIN-ABS(VOL))
C-----------------------------------------------
C     E n d   o f   s o u r c e
C-----------------------------------------------
      RETURN 
      END SUBROUTINE MONVOL_COMPUTE_VOLUME
      
Chd|====================================================================
Chd|  MONVOL_CHECK_VENTHOLE_SURF    share/modules1/monvol_struct_mod.F
Chd|-- called by -----------
Chd|        HM_READ_MONVOL_TYPE11         source/airbag/hm_read_monvol_type11.F
Chd|        HM_READ_MONVOL_TYPE4          source/airbag/hm_read_monvol_type4.F
Chd|        HM_READ_MONVOL_TYPE5          source/airbag/hm_read_monvol_type5.F
Chd|        HM_READ_MONVOL_TYPE6          source/airbag/hm_read_monvol_type6.F
Chd|        HM_READ_MONVOL_TYPE7          source/airbag/hm_read_monvol_type7.F
Chd|        HM_READ_MONVOL_TYPE8          source/airbag/hm_read_monvol_type8.F
Chd|        HM_READ_MONVOL_TYPE9          source/airbag/hm_read_monvol_type9.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE MONVOL_CHECK_VENTHOLE_SURF(IPRI, T_MONVOLN, IGRSURF, IHOL, SHOL, X, IXC, IXTG)
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"   
#include "units_c.inc"
#include "param_c.inc"
#include "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------  
      TYPE(MONVOL_STRUCT_), INTENT(IN) :: T_MONVOLN
      INTEGER, INTENT(IN) :: IHOL, IPRI
      INTEGER, INTENT(IN) :: IXC(NIXC, *), IXTG(NIXTG, *)
      my_real, INTENT(IN) :: X(3, *)
      my_real, INTENT(OUT) :: SHOL
      TYPE (SURF_), DIMENSION(NSURF), INTENT(IN) :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: ISUR, IPVENT, NN, J
      my_real :: DIR, XX, YY, ZZ, X13, Y13, Z13, X24, Y24, Z24, 
     .     NX, NY, NZ, DS
      INTEGER :: I1, I2, I3, I4, ISH34, CHKSURF, J1, ITY
      LOGICAL :: FOUND
      INTEGER :: EXT_SURFID, INT_SURFID, JI, NN1, JI1, ITY1, IVENTYP, ITYPE, NEL
      CHARACTER (LEN = nchartitle) :: TITR1, TITR2, TITR3

      ITYPE = T_MONVOLN%TYPE
      ISUR = T_MONVOLN%IBAGHOL(2, IHOL)
      IVENTYP = T_MONVOLN%IBAGHOL(13, IHOL)
      IPVENT = IGRSURF(ISUR)%ID
      IF(IVENTYP == 0) THEN
         TITR1='VENT HOLE SURFACE'
      ELSE
         TITR1='POROUS SURFACE'
      ENDIF
      SHOL = ZERO
      NN = IGRSURF(ISUR)%NSEG
      DO J=1,NN
         DIR = HALF
         I1 = IGRSURF(ISUR)%NODES(J,1)
         I2 = IGRSURF(ISUR)%NODES(J,2)
         I3 = IGRSURF(ISUR)%NODES(J,3)
         I4 = IGRSURF(ISUR)%NODES(J,4)
         ISH34 = IGRSURF(ISUR)%ELTYP(J)
         IF(ISH34==7)I4 = I3
         IF(ISH34/=3.AND.ISH34/=7)
     .        CALL ANCMSG(MSGID=18,ANMODE=ANINFO,MSGTYPE=MSGERROR,I2=IGRSURF(ISUR)%ID,I1=T_MONVOLN%ID,C1=T_MONVOLN%TITLE)
         XX=HALF*(X(1,I1)+X(1,I2))
         YY=HALF*(X(2,I1)+X(2,I2))
         ZZ=HALF*(X(3,I1)+X(3,I2))
         X13=X(1,I3)-X(1,I1)
         Y13=X(2,I3)-X(2,I1)
         Z13=X(3,I3)-X(3,I1)
         X24=X(1,I4)-X(1,I2)
         Y24=X(2,I4)-X(2,I2)
         Z24=X(3,I4)-X(3,I2)
         NX=DIR*(Y13*Z24-Y24*Z13)
         NY=DIR*(Z13*X24-Z24*X13)
         NZ=DIR*(X13*Y24-X24*Y13)
         DS = SQRT(NX*NX+NY*NY+NZ*NZ)
         SHOL = SHOL + DS
      ENDDO
C------------------------------------------------
C     Ajout condition Svent incluse dans Surf airbag
C------------------------------------------------
      CHKSURF=0
      NN =IGRSURF(ISUR)%NSEG
      EXT_SURFID = T_MONVOLN%EXT_SURFID
      DO J=1,NN
         JI =IGRSURF(ISUR)%ELEM(J)
         ITY=IGRSURF(ISUR)%ELTYP(J)
         IF(ITY == 7) JI=JI+NUMELC
         NN1 =IGRSURF(EXT_SURFID)%NSEG
         FOUND = .FALSE.
C     Test surface externe
         DO J1=1,NN1
            JI1 =IGRSURF(EXT_SURFID)%ELEM(J1)
            ITY1=IGRSURF(EXT_SURFID)%ELTYP(J1)
            IF(ITY1 == 7) JI1=JI1+NUMELC
            IF(JI == JI1) THEN
               FOUND = .TRUE.
               EXIT
            END IF
         ENDDO
         IF (.NOT. FOUND) THEN
            INT_SURFID = T_MONVOLN%IVOLU(67)
            IF((ITYPE == 8 .OR. ITYPE == 11) .AND. INT_SURFID > 0 .AND. IVENTYP == 1) THEN
               NN1 =IGRSURF(INT_SURFID)%NSEG
C     Test surface interne
               DO J1=1,NN1
                  JI1 =IGRSURF(INT_SURFID)%ELEM(J1)
                  ITY1=IGRSURF(INT_SURFID)%ELTYP(J1)
                  IF(ITY1 == 7) JI1=JI1+NUMELC
                  IF(JI == JI1) THEN
                     FOUND = .TRUE.
                     EXIT
                  END IF
               ENDDO
            ENDIF
         ENDIF
         IF(.NOT. FOUND) CHKSURF = CHKSURF+1
         IF (IPRI >= 5.AND..NOT. FOUND) THEN
            IF(CHKSURF == 1) THEN
               TITR2 = IGRSURF(ISUR)%TITLE
               TITR3 = IGRSURF(EXT_SURFID)%TITLE
               CALL ANCMSG(MSGID=41,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              I1=T_MONVOLN%ID,
     .              C1=T_MONVOLN%TITLE,
     .              C2=TITR1,
     .              I2=IGRSURF(ISUR)%ID,
     .              C3=TITR1,
     .              C4=TITR2,
     .              I3=IGRSURF(EXT_SURFID)%ID,
     .              C5=TITR3)
               IF((ITYPE == 8 .OR. ITYPE == 11) .AND. INT_SURFID > 0 .AND. IVENTYP == 1) THEN
                  TITR3 = IGRSURF(INT_SURFID)%TITLE
                  CALL ANCMSG(MSGID=41,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                 I1=T_MONVOLN%ID,
     .                 C1=T_MONVOLN%TITLE,
     .                 C2=TITR1,
     .                 I2=IGRSURF(ISUR)%ID,
     .                 C3=TITR1,
     .                 C4=TITR2,
     .                 I3=IGRSURF(INT_SURFID)%ID,
     .                 C5=TITR3)
               ENDIF
            ENDIF
            IF(ITY == 3)THEN
               NEL=IXC(NIXC,JI)
               WRITE(IOUT,1486) NEL,TRIM(TITR1),IPVENT
            ELSEIF(ITY == 7)THEN
               NEL=IXTG(NIXTG,JI-NUMELC)
               WRITE(IOUT,1487) NEL,TRIM(TITR1),IPVENT
            ENDIF
         ENDIF
      ENDDO
C     
      IF (CHKSURF > 0) THEN
         CALL ANCMSG(MSGID=903,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .        I2=IGRSURF(ISUR)%ID,I3=IGRSURF(EXT_SURFID)%ID,
     .        I1=T_MONVOLN%ID,C1=T_MONVOLN%TITLE,C2=TITR1)
         IF((ITYPE == 8 .OR. ITYPE == 11) .AND. INT_SURFID > 0 .AND. IVENTYP == 1) THEN
            CALL ANCMSG(MSGID=903,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .           I2=IGRSURF(ISUR)%ID,I3=IGRSURF(INT_SURFID)%ID,
     .           I1=T_MONVOLN%ID,C1=T_MONVOLN%TITLE,C2=TITR1)
         ENDIF
      END IF
      RETURN
 1486 FORMAT(6X,'SHELL ELEMENT ID=',I10,' OF ',A17,1X,I10,' DOES NOT BELONG TO THE AIRBAG SURFACE')
 1487 FORMAT(6X,'SH3N  ELEMENT ID=',I10,' OF ',A17,1X,I10,' DOES NOT BELONG TO THE AIRBAG SURFACE')     
      END SUBROUTINE MONVOL_CHECK_VENTHOLE_SURF

Chd|====================================================================
Chd|  MONVOL_ALLOCATE               share/modules1/monvol_struct_mod.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MONVOL_ALLOCATE(NVOLU, T_MONVOL, T_MONVOL_METADATA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"    
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------   
      INTEGER, INTENT(IN) :: NVOLU
      TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(INOUT) :: T_MONVOL
      TYPE(MONVOL_METADATA_), INTENT(INOUT) :: T_MONVOL_METADATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: II

        ! ----------------------------------
        ! initialization of T_MONVOL
        T_MONVOL(1:NVOLU)%TYPE = 0
        T_MONVOL(1:NVOLU)%ID = 0  
        T_MONVOL(1:NVOLU)%NCA = 0
        T_MONVOL(1:NVOLU)%EXT_SURFID = 0
        T_MONVOL(1:NVOLU)%INT_SURFID = 0
        T_MONVOL(1:NVOLU)%NJET = 0
        T_MONVOL(1:NVOLU)%NVENT = 0
        T_MONVOL(1:NVOLU)%NPORSURF = 0
        T_MONVOL(1:NVOLU)%NNS = 0
        T_MONVOL(1:NVOLU)%NNI = 0
        T_MONVOL(1:NVOLU)%NTG = 0
        T_MONVOL(1:NVOLU)%NTGI = 0     
        T_MONVOL(1:NVOLU)%NBRIC = 0
        T_MONVOL(1:NVOLU)%NNA = 0
        T_MONVOL(1:NVOLU)%NTGA = 0
        T_MONVOL(1:NVOLU)%IMESH_ALL = 0
        T_MONVOL(1:NVOLU)%KMESH = 0
        T_MONVOL(1:NVOLU)%NB_FILL_TRI = 0
        T_MONVOL(1:NVOLU)%NEDGE = 0
        T_MONVOL(1:NVOLU)%IADALE = 0
        T_MONVOL(1:NVOLU)%IADALE2 = 0
        T_MONVOL(1:NVOLU)%IADALE3 = 0
        T_MONVOL(1:NVOLU)%IADALE4 = 0
        T_MONVOL(1:NVOLU)%IADALE5 = 0
        T_MONVOL(1:NVOLU)%IADALE6 = 0
        T_MONVOL(1:NVOLU)%IADALE7 = 0
        T_MONVOL(1:NVOLU)%IADALE8 = 0
        T_MONVOL(1:NVOLU)%IADALE9 = 0
        T_MONVOL(1:NVOLU)%IADALE10 = 0
        T_MONVOL(1:NVOLU)%IADALE11 = 0
        T_MONVOL(1:NVOLU)%IADALE12 = 0
        T_MONVOL(1:NVOLU)%IADALE13 = 0
        T_MONVOL(1:NVOLU)%KRA5 = 0
        T_MONVOL(1:NVOLU)%KRA6 = 0
        T_MONVOL(1:NVOLU)%KR5 = 0
        ! ----------------------------------

      T_MONVOL_METADATA%NVOLU = NVOLU
      ALLOCATE(T_MONVOL_METADATA%ICBAG(NICBAG, NVOLU * NVOLU))
      ALLOCATE(T_MONVOL_METADATA%RCBAG(NRCBAG, NVOLU * NVOLU))
      T_MONVOL_METADATA%RCBAG(:, :) = ZERO     
      T_MONVOL_METADATA%ICBAG(:, :) = 0
      DO II = 1, NVOLU
         ALLOCATE(T_MONVOL(II)%IVOLU(NIMV))
         T_MONVOL(II)%IVOLU(1:NIMV) = 0
         ALLOCATE(T_MONVOL(II)%RVOLU(NRVOLU))
         T_MONVOL(II)%RVOLU(1:NRVOLU) = ZERO
         T_MONVOL(II)%NVENT = 0
         T_MONVOL(II)%NPORSURF = 0
         T_MONVOL(II)%EXT_SURFID = 0
         T_MONVOL(II)%INT_SURFID = 0
         T_MONVOL(II)%NCA = 0
         T_MONVOL(II)%KR5 = 0
         T_MONVOL(II)%KRA5 = 0
         T_MONVOL(II)%EDGES_BUILT = .FALSE.
         T_MONVOL(II)%NB_FILL_TRI = 0
         T_MONVOL(II)%OK_REORIENT = .TRUE.
      ENDDO
      END SUBROUTINE MONVOL_ALLOCATE
Chd|====================================================================
Chd|  MONVOL_DEALLOCATE             share/modules1/monvol_struct_mod.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MONVOL_DEALLOCATE(NVOLU, T_MONVOL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"    
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------   
      INTEGER, INTENT(IN) :: NVOLU
      TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(INOUT) :: T_MONVOL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: II
      
      DO II = 1, NVOLU
         IF (ALLOCATED(T_MONVOL(II)%IVOLU)) DEALLOCATE(T_MONVOL(II)%IVOLU)
         IF (ALLOCATED(T_MONVOL(II)%RVOLU)) DEALLOCATE(T_MONVOL(II)%RVOLU)
         IF (ALLOCATED(T_MONVOL(II)%IBAGJET)) DEALLOCATE(T_MONVOL(II)%IBAGJET)
         IF (ALLOCATED(T_MONVOL(II)%RBAGJET)) DEALLOCATE(T_MONVOL(II)%RBAGJET)
         IF (ALLOCATED(T_MONVOL(II)%IBAGHOL)) DEALLOCATE(T_MONVOL(II)%IBAGHOL)
         IF (ALLOCATED(T_MONVOL(II)%RBAGHOL)) DEALLOCATE(T_MONVOL(II)%RBAGHOL)
         IF (ALLOCATED(T_MONVOL(II)%NODES)) DEALLOCATE(T_MONVOL(II)%NODES)
         IF (ALLOCATED(T_MONVOL(II)%ELEM)) DEALLOCATE(T_MONVOL(II)%ELEM)
         IF (ALLOCATED(T_MONVOL(II)%ITAGEL)) DEALLOCATE(T_MONVOL(II)%ITAGEL)
         IF (ALLOCATED(T_MONVOL(II)%ELTG)) DEALLOCATE(T_MONVOL(II)%ELTG)
         IF (ALLOCATED(T_MONVOL(II)%MATTG)) DEALLOCATE(T_MONVOL(II)%MATTG)
         IF (ALLOCATED(T_MONVOL(II)%TBRIC)) DEALLOCATE(T_MONVOL(II)%TBRIC)
         IF (ALLOCATED(T_MONVOL(II)%TFAC)) DEALLOCATE(T_MONVOL(II)%TFAC)
         IF (ALLOCATED(T_MONVOL(II)%TAGELS)) DEALLOCATE(T_MONVOL(II)%TAGELS)
         IF (ALLOCATED(T_MONVOL(II)%IBUFA)) DEALLOCATE(T_MONVOL(II)%IBUFA)
         IF (ALLOCATED(T_MONVOL(II)%ELEMA)) DEALLOCATE(T_MONVOL(II)%ELEMA)
         IF (ALLOCATED(T_MONVOL(II)%BRNA)) DEALLOCATE(T_MONVOL(II)%BRNA)
         IF (ALLOCATED(T_MONVOL(II)%TAGELA)) DEALLOCATE(T_MONVOL(II)%TAGELA)
         IF (ALLOCATED(T_MONVOL(II)%NCONA)) DEALLOCATE(T_MONVOL(II)%NCONA)  
         IF (ALLOCATED(T_MONVOL(II)%VELOCITY)) DEALLOCATE(T_MONVOL(II)%VELOCITY)
         IF (ALLOCATED(T_MONVOL(II)%NODE_COORD)) DEALLOCATE(T_MONVOL(II)%NODE_COORD)
         IF (ALLOCATED(T_MONVOL(II)%POROSITY)) DEALLOCATE(T_MONVOL(II)%POROSITY)
         IF (ALLOCATED(T_MONVOL(II)%THSURF_TAG)) DEALLOCATE(T_MONVOL(II)%THSURF_TAG)
         IF (ALLOCATED(T_MONVOL(II)%ELAREA)) DEALLOCATE(T_MONVOL(II)%ELAREA)
         IF (ALLOCATED(T_MONVOL(II)%FILL_TRI)) DEALLOCATE(T_MONVOL(II)%FILL_TRI)
         IF (ALLOCATED(T_MONVOL(II)%EDGE_NODE1)) DEALLOCATE(T_MONVOL(II)%EDGE_NODE1)
         IF (ALLOCATED(T_MONVOL(II)%EDGE_NODE2)) DEALLOCATE(T_MONVOL(II)%EDGE_NODE2)
         IF (ALLOCATED(T_MONVOL(II)%EDGE_ELEM)) DEALLOCATE(T_MONVOL(II)%EDGE_ELEM)
         IF (ALLOCATED(T_MONVOL(II)%IAD_EDGE_ELEM)) DEALLOCATE(T_MONVOL(II)%IAD_EDGE_ELEM)
      ENDDO
      END SUBROUTINE MONVOL_DEALLOCATE
      END
Chd|====================================================================
Chd|  MONVOL_ORIENT_SURF            share/modules1/monvol_struct_mod.F
Chd|-- called by -----------
Chd|        HM_READ_MONVOL_TYPE1          source/airbag/hm_read_monvol_type1.F
Chd|        HM_READ_MONVOL_TYPE10         source/airbag/hm_read_monvol_type10.F
Chd|        HM_READ_MONVOL_TYPE11         source/airbag/hm_read_monvol_type11.F
Chd|        HM_READ_MONVOL_TYPE2          source/airbag/hm_read_monvol_type2.F
Chd|        HM_READ_MONVOL_TYPE3          source/airbag/hm_read_monvol_type3.F
Chd|        HM_READ_MONVOL_TYPE4          source/airbag/hm_read_monvol_type4.F
Chd|        HM_READ_MONVOL_TYPE5          source/airbag/hm_read_monvol_type5.F
Chd|        HM_READ_MONVOL_TYPE6          source/airbag/hm_read_monvol_type6.F
Chd|        HM_READ_MONVOL_TYPE7          source/airbag/hm_read_monvol_type7.F
Chd|        HM_READ_MONVOL_TYPE8          source/airbag/hm_read_monvol_type8.F
Chd|        HM_READ_MONVOL_TYPE9          source/airbag/hm_read_monvol_type9.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MONVOL_BUILD_EDGES            share/modules1/monvol_struct_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|====================================================================
      SUBROUTINE MONVOL_ORIENT_SURF(T_MONVOLN, TITLE, IVOLU, ITAB, SURF, IXC, IXTG, X, ITYPE)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutines ensures that all normal from monvol surface are
C oriented on same side. 
C volume sign of resulting oriented surface is not ensured
C
C   FIND ADJACENT ELEMS (by pair)
C   -----------------------------
C
C  10    9    8                    RUN THGROUGH ELEM              SORTING 1st COLUMN                       SORTING 2nd COLUMN FOR EACH BLOCK (siz > 2)
C   +----+----+                  node1 node2 elem_id             node1 node2 elem_id
C   |    |    |                      1     2      17                1     10      17  } BLOCK 
C   | 17 | 11 |                      2     9      17                1      2      17  }   
C   |    |    |                      9    10      17    SORT.1      ----------------                       ----------------
C   +----+----+                      1    10      17    ----->      2      9      17  }         SORT.2     2      3      11  }     
C   1    2    3                      2     3      11                2      3      11   BLOCK    ----->     2      9      17   ONE COMMON EDGE IN BLOCK  : 2,3 
C                                    3     8      11                2      9      11  }                    2      9      11  }                          => elem 17 & 11 are adjacent
C                                    8     9      11                ----------------                       ----------------             
C                                    9    10      11                3      8      11
C                                    ^     ^       ^                ----------------
C                        EDGE_ARRAY_N1     ^       ^                8      9      11
C                              EDGE_ARRAY_N2       ^                ----------------
C                                    EDGE_ARRAY_ELEM                9     10      17
C
C
C   CHECK CONNECTIVITY
C   -----------------
C
C   10    9    8 
C    +-----+----+              EXAMPLE :
C    |     |    |                 reference elem  :  {09,10,01,02} U {09}
C    | REF | 11 |                 elem to treat   :  {08,03,02,09} U {08}
C    |     |    | 
C    +-----+----+                   1.  check pattern [09,10] in elem to treat : not found 
C    1    2    3                    2.  check pattern [10,01] in elem to treat : not found 
C                                   3.  check pattern [01,02] in elem to treat : not found
C                                   4.  check pattern [02,09] in elem to treat : found => reverse connectivity
C
C   REVERSE CONNECTIVITY
C   --------------------
C
C         1       2    1         2 
C         +-------+    +---------+
C         |       |    \  SH3N  /
C         | SHELL |     \      /         SHELL : switch 2<->4
C         |       |      \    /          SH3N  : switch 1<->2
C         +-------+       \  /
C         4       3        +3
C                           
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
      USE MONVOL_STRUCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "param_c.inc"
#include "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER(LEN = nchartitle), INTENT(IN) :: TITLE
      INTEGER, INTENT(IN) :: IVOLU(NIMV), ITAB(*),ITYPE, IXC(NIXC, NUMELC), IXTG(NIXTG, NUMELTG)
      my_real :: X(3,NUMNOD)
      TYPE(SURF_), INTENT(INOUT) :: SURF
      TYPE(MONVOL_STRUCT_), INTENT(INOUT) :: T_MONVOLN
C-----------------------------------------------
C     L o c a l   v a r i a b l e s
C-----------------------------------------------
      INTEGER NSEG,ISH34,JJ,II(4),IIK(4),KK, IELEM_ADJ,IDX,IDX_A,IDX_B,IFIRST,ILAST,IREAD,ICUR,IPAIR,INBLOCK,NPAIR,NB_ADJACENT,LL
      INTEGER IDX1,IDX2
      INTEGER NEDG, SUM_ADJ
      !temporary memory
      INTEGER, ALLOCATABLE,DIMENSION(:) ::  PATHS, SIZES, CHECK_FLAG_ELEM, NB_ADJ,IAD_ADJ, LIST_ADJ_TAB
      INTEGER,ALLOCATABLE,DIMENSION(:) :: db_reversed, db_path
      INTEGER, DIMENSION(:), ALLOCATABLE :: PAIR_LIST, NB_PAIR_BY_EDGE      
      INTEGER :: NB_NOEUD, NB_ARC, NB_COMP_CONNEXE, SUM_SIZES
      INTEGER(8) :: graph_ptr
      INTEGER :: IELEM,ICOMP, EDGES_A(5),EDGES_B(5), NB_REVERSED
      INTEGER :: NPT_A, NPT_B, IELEM1, IELEM2, ELTYP1, ELTYP2, NB_COMMON_NODE, 
     .     NODELIST1(4), NODELIST2(4), ELEM1ID, ELEM2ID, ELEMTG, ELEMC, IELEMTG, IELEMC
      LOGICAL :: lFOUND, lFOUND_ADJ
      INTEGER, POINTER, DIMENSION(:) :: ptrADJ
      INTEGER :: NB_DUPLICATED_ELTS
      INTEGER, DIMENSION(:), ALLOCATABLE :: DUPLICATED_ELTS
      CHARACTER(LEN=1024) :: FILENAME, FILENAME1, FILENAME2
      INTEGER(8) :: edge_ptr, duplicate_ptr
      INTEGER, DIMENSION(3) :: FOUND_TABLE
      INTEGER :: NOT_FOUND
      LOGICAL debug_output
      INTEGER :: NTRI, NB_CON
      INTEGER, DIMENSION(:), ALLOCATABLE :: IAD_COMP_CONNEX
C-----------------------------------------------
C     P r e   C o n d i t i o n
C-----------------------------------------------
C! only type 'PRES' (2) and type 'AIRBAG1' (7)   FVMBAG1 (8)
C! otherwise : unplug
C      IF(ITYPE /= 2 .AND. 
C     .   ITYPE /= 7 .AND.       
C     .   ITYPE /= 8       )RETURN
C-----------------------------------------------
C     S o u r c e   L i n e s
C-----------------------------------------------

      graph_ptr = 0
      NSEG = SURF%NSEG
      NTRI = T_MONVOLN%NB_FILL_TRI
      T_MONVOLN%OK_REORIENT = .TRUE.

!     *********************************     !
!     ** Edge connectivity if needed **     !
!     *********************************     !

      IF (.NOT. T_MONVOLN%EDGES_BUILT) THEN
         CALL MONVOL_BUILD_EDGES(T_MONVOLN, SURF)
      ENDIF
      NEDG = T_MONVOLN%NEDGE
            
!     *********************************     !
!     ** Find any duplicated element **     !
!     *********************************     !
!     REMOVE ONE OF EACH THEM FROM THE EDGE CONNECTIVITY
      NB_DUPLICATED_ELTS = 0
      duplicate_ptr = 0
      CALL TAB1_INIT(duplicate_ptr)
      DO JJ = 1, NEDG
         NB_CON = T_MONVOLN%IAD_EDGE_ELEM(JJ + 1) - T_MONVOLN%IAD_EDGE_ELEM(JJ)
         IF (NB_CON > 2) THEN
!     T connection or worse
            DO IELEM1 = T_MONVOLN%IAD_EDGE_ELEM(JJ), T_MONVOLN%IAD_EDGE_ELEM(JJ + 1) - 1
               IF (T_MONVOLN%EDGE_ELEM(IELEM1) /= 0) THEN
                  DO IELEM2 = T_MONVOLN%IAD_EDGE_ELEM(JJ), T_MONVOLN%IAD_EDGE_ELEM(JJ + 1) -1
                     IF (IELEM1 /= IELEM2) THEN
                        ELEM1ID = T_MONVOLN%EDGE_ELEM(IELEM1)
                        ELEM2ID = T_MONVOLN%EDGE_ELEM(IELEM2)
                        IF (ELEM1ID * ELEM2ID == 0) THEN
!     One of the element have already been suppressed as duplicated from another element
!     connected to the same edge
                           CYCLE
                        ENDIF
                        ELTYP1 = SURF%ELTYP(ELEM1ID)
                        ELTYP2 = SURF%ELTYP(ELEM2ID)
                        IF (ELTYP1 == ELTYP2) THEN
                           IF (ELTYP1 == 7) THEN
!     Two triangles
                              NB_COMMON_NODE = 0
                              NODELIST1(1:4) = (/0, IXTG(2:4,SURF%ELEM(ELEM1ID))/)
                              NODELIST2(1:4) = (/0, IXTG(2:4,SURF%ELEM(ELEM2ID))/)
                              DO KK = 2, 4
                                 DO LL = 2, 4
                                    IF (NODELIST1(KK) == NODELIST2(LL)) THEN
                                       NB_COMMON_NODE = NB_COMMON_NODE + 1
                                       EXIT
                                    ENDIF
                                 ENDDO
                              ENDDO
                              IF (NB_COMMON_NODE == 3) THEN
!     Get rid of ELEM2
                                 T_MONVOLN%EDGE_ELEM(IELEM2) = 0
                                 NB_DUPLICATED_ELTS = NB_DUPLICATED_ELTS + 1
                                 CALL TAB1_APPEND(duplicate_ptr, ELEM1ID)
                                 CALL TAB1_APPEND(duplicate_ptr, ELEM2ID)
                              ENDIF
                           ENDIF
                        ELSEIF (ELTYP1 == 3) THEN
!     Two QUADS
                           NB_COMMON_NODE = 0
                           NODELIST1(1:4) = (/IXC(2:5,SURF%ELEM(ELEM1ID))/)
                           NODELIST2(1:4) = (/IXC(2:5,SURF%ELEM(ELEM2ID))/)
                           DO KK = 1, 4
                              DO LL = 1, 4
                                 IF (NODELIST1(KK) == NODELIST2(LL)) THEN
                                    NB_COMMON_NODE = NB_COMMON_NODE + 1
                                    EXIT
                                 ENDIF
                              ENDDO
                           ENDDO
                           IF (NB_COMMON_NODE == 4) THEN
!     Get rid of ELEM2
                              T_MONVOLN%EDGE_ELEM(IELEM2) = 0
                              NB_DUPLICATED_ELTS = NB_DUPLICATED_ELTS + 1
                              CALL TAB1_APPEND(duplicate_ptr, ELEM1ID)
                              CALL TAB1_APPEND(duplicate_ptr, ELEM2ID)
                           ENDIF
                        ELSE
!     One triangle, one quad
                           IELEMTG = IELEM2
                           ELEMTG = ELEM2ID
                           IELEMC = IELEM1
                           ELEMC = ELEM1ID
                           IF (ELTYP1 == 7) THEN
                              IELEMTG = IELEM1
                              ELEMTG = ELEM1ID
                              IELEMC = IELEM2
                              ELEMC = ELEM2ID
                           ENDIF
                           NB_COMMON_NODE = 0
                           NODELIST1(1:4) = (/0, IXTG(2:4,SURF%ELEM(ELEMTG))/)
                           NODELIST2(1:4) = (/IXC(2:5,SURF%ELEM(ELEMC))/)
                           DO KK = 2, 4
                              DO LL = 1, 4
                                 IF (NODELIST1(KK) == NODELIST2(LL)) THEN
                                    NB_COMMON_NODE = NB_COMMON_NODE + 1
                                    EXIT
                                 ENDIF
                              ENDDO
                           ENDDO
                           IF (NB_COMMON_NODE == 3) THEN
!     Get rid of the triangle
                              T_MONVOLN%EDGE_ELEM(IELEMTG) = 0
                              NB_DUPLICATED_ELTS = NB_DUPLICATED_ELTS + 1
                              CALL TAB1_APPEND(duplicate_ptr, ELEMC)
                              CALL TAB1_APPEND(duplicate_ptr, IELEMTG)
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ENDIF
      ENDDO

      !--------------------------------------------!
      ! 4. BUILD PAIRS FOR GRAPH PATH CONSTRUCTION !
      !--------------------------------------------!
!     Number of pairs by edge
      ALLOCATE(NB_PAIR_BY_EDGE(NEDG))
      DO JJ = 1, NEDG
         NB_PAIR_BY_EDGE(JJ) = 0
         DO KK = T_MONVOLN%IAD_EDGE_ELEM(JJ), T_MONVOLN%IAD_EDGE_ELEM(JJ + 1) - 1
            IF (T_MONVOLN%EDGE_ELEM(KK) /= 0) THEN
               NB_PAIR_BY_EDGE(JJ) = NB_PAIR_BY_EDGE(JJ) + 1
            ENDIF
         ENDDO
         NB_PAIR_BY_EDGE(JJ) = (NB_PAIR_BY_EDGE(JJ) - 1) * NB_PAIR_BY_EDGE(JJ) / 2
         IF (NB_PAIR_BY_EDGE(JJ) > 1) THEN
            T_MONVOLN%OK_REORIENT = .FALSE.
         ENDIF
      ENDDO
      NPAIR = SUM(NB_PAIR_BY_EDGE)
      ALLOCATE(PAIR_LIST(2 * NPAIR))
      IPAIR = 0
      DO JJ = 1, NEDG
         NB_CON = T_MONVOLN%IAD_EDGE_ELEM(JJ + 1) - T_MONVOLN%IAD_EDGE_ELEM(JJ)
         DO KK = 1, NB_CON
            DO LL = KK + 1, NB_CON
               ELEM1ID = T_MONVOLN%EDGE_ELEM(T_MONVOLN%IAD_EDGE_ELEM(JJ) + KK - 1)
               ELEM2ID = T_MONVOLN%EDGE_ELEM(T_MONVOLN%IAD_EDGE_ELEM(JJ) + LL - 1)
               IF (ELEM1ID * ELEM2ID /= 0) THEN
                  PAIR_LIST(IPAIR + 1) = ELEM1ID - 1
                  PAIR_LIST(IPAIR + 2) = ELEM2ID - 1
                  IPAIR = IPAIR + 2 
               ENDIF
            ENDDO
         ENDDO
      ENDDO     
      
      !------------------------------------!
      ! 5. BUILD GRAPH                     !
      !------------------------------------!
      ! result : graph_ptr
      !------------------------------------!
      NB_NOEUD=NSEG+NTRI
      NB_ARC=NPAIR
      NB_COMP_CONNEXE = 0
      CALL GRAPH_BUILD_PATH(NB_NOEUD, NB_ARC, PAIR_LIST, NB_COMP_CONNEXE, graph_ptr)
      
      !------------------------------------!
      ! 6. GET PATH                        !
      !------------------------------------!
      ! result : PATHS(1:SIZE(1),SIZE(1)+1..SIZE(2),...)
      !------------------------------------!
      IF(.NOT.ALLOCATED(SIZES))ALLOCATE(SIZES(0:NB_COMP_CONNEXE))
      ALLOCATE(IAD_COMP_CONNEX(NB_COMP_CONNEXE+1))
      CALL GRAPH_GET_SIZES(graph_ptr, SIZES(1))
      SUM_SIZES=SUM(SIZES(1:NB_COMP_CONNEXE),1)
      SIZES(0)=0
      IAD_COMP_CONNEX(1) = 1
      DO JJ = 2, NB_COMP_CONNEXE + 1
         IAD_COMP_CONNEX(JJ) = IAD_COMP_CONNEX(JJ - 1) + SIZES(JJ - 1)
      ENDDO
      IF(.NOT.ALLOCATED(PATHS))ALLOCATE(PATHS(SUM_SIZES))
      CALL GRAPH_GET_PATH(graph_ptr, PATHS)
      
      !----------------------------------------!
      ! 7. DEBUG : HM TCL SCRIPT TO CHECK PATH !
      !----------------------------------------!
      debug_output=.FALSE.
      if(debug_output)then
        WRITE(FILENAME1, "(A,I0,A)") "surfmesh_",T_MONVOLN%ID,"_list_ids.tcl"
        OPEN(UNIT = 220582, FILE = FILENAME1, FORM ='formatted')
        write (220582,FMT='(A)')"set ids { \"
        kk=0                                                                                                                           
        do while (kk < sizes(1))                                                                                                       
         if(kk+1<sizes(1))then                                                                                                         
           ISH34 = SURF%ELTYP(1+PATHS(kk+1))                                                                                           
           IF(ISH34==3)THEN                                                                                                            
             write (220582,FMT='(I10,A,I10,A)')IXC(7,SURF%ELEM(1+PATHS(kk+1)) ) ," ",10000000+IXC(7,SURF%ELEM(1+PATHS(kk+1)) ),' \' 
           ELSE                                                                                                                        
             write (220582,FMT='(I10,A,I10,A)')IXTG(6,SURF%ELEM(1+PATHS(kk+1)) )," ",10000000+IXTG(6,SURF%ELEM(1+PATHS(kk+1)) ),' \'
           ENDIF                                                                                                                       
         endif                                                                                                                         
         kk=kk+1                                                                                                                       
        enddo    
        write (220582,FMT='(A)') " } ; "  
        CLOSE(220582)                                                                                                                   

        WRITE(FILENAME2, "(A,I0,A)") "surfmesh_",T_MONVOLN%ID,"_list_types.tcl"
        OPEN(UNIT = 220582, FILE = FILENAME2, FORM ='formatted')
        write (220582,FMT='(A)')"set types { \"       
        kk=0                                              
        do while (kk < sizes(1))                          
         if(kk+1<sizes(1))then                            
           ISH34 = SURF%ELTYP(1+PATHS(kk+1))              
           IF(ISH34==3)THEN                               
             write (*,FMT='(I10,A,I10,A)')3 ,"  ",3,'  \' 
           ELSE                                           
             write (*,FMT='(I10,A,I10,A)')7,"  ",7,'  \'  
           ENDIF                                          
         endif                                            
         kk=kk+1                                          
        enddo   
        CLOSE(220582)
                                                  
        WRITE(FILENAME, "(A,I0,A)") "surfmesh_",T_MONVOLN%ID,"_HM_TCL_MACTO.tcl"
        OPEN(UNIT = 220582, FILE = FILENAME, FORM ='formatted')
        write (220582,FMT='(A)') '#--$ids                                                                      '                
        write (220582,FMT='(A)') '::hwt::Source "'//FILENAME1//'";'               
        write (220582,FMT='(A)') '#--$types                                                                    '                
        write (220582,FMT='(A)') '::hwt::Source "'//FILENAME2//'";'               
        write (220582,FMT='(A)') '                                                                             '                
        write (220582,FMT='(A)') 'for {set i 0} {$i < [llength $ids]} {incr i 2} {                             ' 
        write (220582,FMT='(A)') '   set ityp [lindex $types $i]                                               '  
        write (220582,FMT='(A)') '   set id [lindex $ids $i]                                                   '  
        write (220582,FMT='(A)') '                                                                             '  
        write (220582,FMT='(A)') '   if {$ityp == 3} {                                                         '  
        write (220582,FMT='(A)') '     *createmark elements 1 [hm_getinternalid shell_idpool $id -bypoolname] ;'  
        write (220582,FMT='(A)') ' } elseif {$ityp == 7} {                                                     '  
        write (220582,FMT='(A)') '     *createmark elements 1 [hm_getinternalid sh3n_idpool $id -bypoolname] ; '  
        write (220582,FMT='(A)') ' }                                                                           '  
        write (220582,FMT='(A)') ' hm_redraw;                                                                  '  
        write (220582,FMT='(A)') ' *movemark elements 1 \"COLOR\";                                             '  
        write (220582,FMT='(A)') '}                                                                            '  
        CLOSE(220582)                                                                                                                   
      endif !(debug_output)
      
      !------------------------------------!
      ! 8. GET PATH                        !
      !------------------------------------!
      IF(.NOT.ALLOCATED(NB_ADJ))ALLOCATE(NB_ADJ(NSEG+NTRI))
      IF(.NOT.ALLOCATED(IAD_ADJ))ALLOCATE(IAD_ADJ(NSEG+NTRI+1))
      CALL GRAPH_GET_NB_ADJ(graph_ptr, NB_ADJ)
      SUM_ADJ=SUM(NB_ADJ)
      IAD_ADJ(1)=1
      DO KK=2,NSEG+NTRI+1
        IAD_ADJ(KK)=IAD_ADJ(KK-1)+NB_ADJ(KK-1)
      ENDDO
      IF(.NOT.ALLOCATED(LIST_ADJ_TAB))ALLOCATE(LIST_ADJ_TAB(SUM_ADJ))
      CALL GRAPH_GET_ADJ(graph_ptr, LIST_ADJ_TAB)
      DO KK=1,SUM_ADJ
        LIST_ADJ_TAB(KK)=LIST_ADJ_TAB(KK)+1
      ENDDO      
      !------------------------------------!
      ! 7. DEBUG OUTPUT : SURF IN FILE     !
      !------------------------------------!
      !--write a Radioss input file to check final surface
      debug_output=.false.
      if(debug_output)then
        NSEG=SURF%NSEG
        WRITE(FILENAME, "(A,I0,A)") "surfmesh_before_",T_MONVOLN%ID,"_0000.rad"
        OPEN(UNIT = 210486, FILE = TRIM(FILENAME), FORM ='formatted')
        WRITE(210486, '(A)') "#RADIOSS STARTER"
        WRITE(210486, '(A)') "/BEGIN"
        WRITE(210486, '(A)') "ORIENTED_SURFACE "
        WRITE(210486, '(A)') "       100         0"
        WRITE(210486, '(A)') "                   g                  mm                  ms"
        WRITE(210486, '(A)') "                   g                  mm                  ms"
        WRITE(210486, "(A5)") "/NODE"
        DO KK = 1, NUMNOD
        WRITE(210486, "(I10, 1PG20.13, 1PG20.13, 1PG20.13)") ITAB(kk),X(1, kk), X(2, kk), X(3, kk)
        ENDDO
        DO kk = 1, NSEG
         II(1:4) = SURF%NODES(kk,1:4)
         ISH34 = SURF%ELTYP(kk)
         IF (ISH34 == 3) THEN
           WRITE(210486, "(A6)") "/SHELL"
           WRITE(210486, '(I10,I10,I10,I10,I10)') IXC(7,SURF%ELEM(kk)), ITAB(II(1)), ITAB(II(2)),ITAB(II(3)), ITAB(II(4))
         ENDIF
        ENDDO
        DO kk = 1, NSEG
         II(1:4) = SURF%NODES(kk,1:4)
         ISH34 = SURF%ELTYP(kk)        
         IF (ISH34 == 7) THEN
          WRITE(210486, "(A5)") "/SH3N"         
          WRITE(210486, '(I10,I10,I10,I10)') IXTG(6,SURF%ELEM(kk)), ITAB(II(1)), ITAB(II(2)),ITAB(II(3))
         ENDIF
        ENDDO
        IF (T_MONVOLN%NB_FILL_TRI > 0) THEN
           WRITE(210486, "(A5)") "/SH3N"  
        ENDIF
        DO kk = 1, T_MONVOLN%NB_FILL_TRI
           WRITE(210486, '(I10,I10,I10,I10)') kk + NSEG, ITAB(T_MONVOLN%FILL_TRI(3 * (kk - 1) + 1)),
     .          ITAB(T_MONVOLN%FILL_TRI(3 * (kk - 1) + 2)), ITAB(T_MONVOLN%FILL_TRI(3 * (kk - 1) + 3))
        ENDDO
        CLOSE (210486)       
      endif  !debug_output      
      !------------------------------------!
      ! 9. SPREAD NORMAL                   !
      !------------------------------------!
      ! result : SIZES(1:NB_COMP_CONNEXE)
      !------------------------------------!
      IF(.NOT.ALLOCATED(CHECK_FLAG_ELEM))ALLOCATE(CHECK_FLAG_ELEM(NSEG+NTRI))  
      CHECK_FLAG_ELEM(:)=0 
         
      IF (T_MONVOLN%OK_REORIENT) THEN
         DO ICOMP=1,NB_COMP_CONNEXE
            
!--REFERENCE ELEM (FIRST ONE)
            JJ = 1 + PATHS(IAD_COMP_CONNEX(ICOMP))
            
            CHECK_FLAG_ELEM(JJ)=1 !already traveled
            NB_REVERSED = 0
            
            DO IELEM=IAD_COMP_CONNEX(ICOMP) + 1, IAD_COMP_CONNEX(ICOMP + 1) - 1
               
!--CURRENT ELEM
               JJ=1+PATHS(IELEM)
               
               IF (JJ <= NSEG) THEN
                  II(1:4) = SURF%NODES(JJ,1:4)                   
                  ISH34 = SURF%ELTYP(JJ)                  
                  IF(ISH34==3.AND.II(3)/=II(4))THEN 
                     EDGES_A(1:5)=(/ II(1:4), II(1) /)
                     NPT_A=4                                   
                  ELSE
                     EDGES_A(1:5)=(/ II(1:3), II(1), 0 /) 
                     NPT_A=3                                                      
                  ENDIF
               ELSE
                  II(1:3) = T_MONVOLN%FILL_TRI(3 * (JJ - NSEG - 1) + 1 : 3 * (JJ - NSEG - 1) + 3)
                  II(4) = II(3)
                  EDGES_A(1:5) = (/ II(1:3), II(1), 0 /) 
                  NPT_A = 3  
               ENDIF
               
!--CHECK ADJACENT ELEM ALREADY TREATED ( KK : CHECK_FLAG_ELEM(KK) = 1)  
!need to get KK
               IDX1 = IAD_ADJ(JJ)
               IDX2 = IAD_ADJ(JJ+1)-1
               lFOUND_ADJ = .FALSE.
               DO KK=IDX1,IDX2
                  IELEM_ADJ = LIST_ADJ_TAB(KK)
                  IF(CHECK_FLAG_ELEM(IELEM_ADJ) /= 0 )THEN
                     lFOUND_ADJ = .TRUE.
                     EXIT
                  ENDIF
               ENDDO
               IF(.NOT. lFOUND_ADJ)THEN
                  print *, "**error when forcing monvol surface orientation"
                  CALL ARRET(2);
                  return;
               ENDIF
               KK = IELEM_ADJ 
!print *, "found adjacent element already treated =", IXTG(6,  SURF%ELEM(KK) ) 
               
!--LIST OF EDGES FOR ADJACENT ELEM
               IF (KK <= NSEG) THEN
                  II(1:4) = SURF%NODES(KK,1:4)                   
                  ISH34 = SURF%ELTYP(KK)                  
                  IF(ISH34==3.AND.II(3)/=II(4))THEN 
                     EDGES_B(1:5)=(/ II(1:4), II(1) /)
                     NPT_B=4                                   
                  ELSE
                     EDGES_B(1:5)=(/ II(1:3), II(1), 0 /) 
                     NPT_B=3                                                      
                  ENDIF 
               ELSE
                  II(1:3) = T_MONVOLN%FILL_TRI(3 * (KK - NSEG - 1) + 1 : 3 * (KK - NSEG - 1) + 3)
                  II(4) = II(3)
                  EDGES_B(1:5) = (/ II(1:3), II(1), 0 /) 
                  NPT_B = 3  
               ENDIF
               
!--CHECK PATTERN (CURRENT vs ADJACENT)
               lFOUND = .FALSE.            
               DO IDX_A=1,NPT_A
                  DO IDX_B=1,NPT_B
                     IF(EDGES_B(IDX_B)==EDGES_A(IDX_A))THEN
                        IF(EDGES_B(IDX_B+1)==EDGES_A(IDX_A+1))THEN
                           lFOUND = .TRUE.
                           EXIT
                        ENDIF
                     ENDIF
                  ENDDO
                  IF(lFOUND)EXIT
               ENDDO
               
!--REVERSE IF NEEDED (CURRENT ELEM)                   
               IF(lFOUND)THEN
                  IF (JJ <= NSEG) THEN
                     II(1:4) = SURF%NODES(JJ,1:4) 
                     IF(NPT_A == 4)THEN
                        SURF%NODES(JJ,1:4)=(/ II(1), II(4), II(3), II(2) /)
                     ELSE
                        SURF%NODES(JJ,1:4)=(/ II(2), II(1), II(3), II(4) /)
                     ENDIF
                  ELSE
                     II(1:3) = T_MONVOLN%FILL_TRI(3 * (JJ - NSEG - 1) + 1 : 3 * (JJ - NSEG - 1) + 3)
                     II(4) = II(3)
                     T_MONVOLN%FILL_TRI(3 * (JJ - NSEG - 1) + 1 : 3 * (JJ - NSEG - 1) + 3) = (/ II(2), II(1), II(3) /)
                  ENDIF
!print *, "--> reversed normal =", IXTG(6,  SURF%ELEM(JJ) ) 
                  NB_REVERSED = NB_REVERSED + 1
                  CHECK_FLAG_ELEM(JJ)=-1 
               ENDIF
               
!MARK ELEM AS TREATED & NEXT
               CHECK_FLAG_ELEM(JJ)=1 !treated and unchanged
               IF(lFOUND)CHECK_FLAG_ELEM(JJ)=-1 !treated and reversed
               
            ENDDO               !next IELEM
         ENDDO
      ELSE
         CALL ANCMSG(MSGID = 1882, ANMODE = ANINFO, MSGTYPE = MSGWARNING,
     .              I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE)
      ENDIF

      !----------------------------------------------------!
      ! 10. CONSISTENT ORIENTATION OF DUPLICATED ELEMENTS
      !----------------------------------------------------!
      ALLOCATE(DUPLICATED_ELTS(NB_DUPLICATED_ELTS * 2))
      CALL TAB1_GET(duplicate_ptr, DUPLICATED_ELTS)
      DO JJ = 1, NB_DUPLICATED_ELTS
         ELEM1ID = SURF%ELEM(DUPLICATED_ELTS(2 * (JJ - 1) + 1))
         ELEM2ID = SURF%ELEM(DUPLICATED_ELTS(2 * (JJ - 1) + 2))
!     ELEM1D is already oriented, ELEM2ID has to be oriented reversely
         ELTYP1 = SURF%ELTYP(DUPLICATED_ELTS(2 * (JJ - 1) + 1))
         ELTYP2 = SURF%ELTYP(DUPLICATED_ELTS(2 * (JJ - 1) + 2))
         IF (ELTYP1 == ELTYP2) THEN
            II(1:4) = SURF%NODES(ELEM1ID, 1:4)
            IF (ELTYP1 == 7) THEN
!     Triangles
               SURF%NODES(ELEM2ID, 1:4) = (/ II(2), II(1), II(3), II(4) /)
            ELSE
!     Quads
               SURF%NODES(ELEM2ID, 1:4) = (/ II(1), II(4), II(3), II(2) /)
            ENDIF
         ELSE
!     Target element is necessarily the triangle
            II(1:4) = SURF%NODES(ELEM2ID,1:4)  
            EDGES_A(1:5) = (/ II(1:3), II(1), 0 /) 
            NPT_A = 3 
            II(1:4) = SURF%NODES(ELEM1ID,1:4)  
            EDGES_B(1:5) = (/ II(1:4), II(1) /)
            NPT_B = 4   
            !--CHECK PATTERN (CURRENT vs ADJACENT)
            lFOUND = .FALSE.            
            DO IDX_A=1,NPT_A
               DO IDX_B=1,NPT_B
                  IF(EDGES_B(IDX_B)==EDGES_A(IDX_A))THEN
                     IF(EDGES_B(IDX_B+1)==EDGES_A(IDX_A+1))THEN
                        lFOUND = .TRUE.
                        EXIT
                     ENDIF
                  ENDIF
               ENDDO
               IF(lFOUND)EXIT
            ENDDO
            IF(lFOUND)THEN
               II(1:4) = SURF%NODES(ELEM2ID, 1:4) 
               IF(NPT_A == 4)THEN
                  SURF%NODES(ELEM2ID,1:4)=(/ II(1), II(4), II(3), II(2) /)
               ELSE
                  SURF%NODES(ELEM2ID,1:4)=(/ II(2), II(1), II(3), II(4) /)
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      CALL TAB1_FREE_MEMORY(duplicate_ptr)
      
      !-------------------------------------!
      ! 11. DEBUG OUTPUT : RESULT ON SCREEN !
      !-------------------------------------!
      !--display on screen the element path (possible mixed SHELL,SH3N)
      debug_output=.false.
      if(debug_output)then
        ICOMP=1
        ALLOCATE(db_path(SIZES(ICOMP)))
        do ielem=1,SIZES(ICOMP)
          JJ=1+PATHS(IELEM)
          II(1:4) = SURF%NODES(JJ,1:4)
          ISH34 = SURF%ELTYP(JJ)                  
          IF(ISH34==3.AND.II(3)/=II(4))THEN 
            db_path(JJ) = IXC(7,SURF%ELEM((JJ)))
          else
            db_path(JJ) = IXTG(6,SURF%ELEM((JJ)))
          endif    
        enddo
        print *,"____________________________________________________"
        print *, "there are ",SIZES(ICOMP)," elements along the path"
        print *, db_path(1:SIZES(ICOMP))         
        print *,"____________________________________________________"        
        deallocate(db_path)
      endif !debug_output
           
      debug_output=.false.
      if(debug_output)then
        !--display on screen the reversed elems (possible mixed SHELL,SH3N)      
        idx=0
        ALLOCATE(db_reversed(SIZES(ICOMP)))  
        do ielem=1,SIZES(ICOMP)
          JJ=1+PATHS(IELEM)
          II(1:4) = SURF%NODES(JJ,1:4)
          ISH34 = SURF%ELTYP(JJ)     
          IF(CHECK_FLAG_ELEM(JJ)==-1)THEN
            idx=idx+1
            IF(ISH34==3.AND.II(3)/=II(4))THEN 
              db_reversed(IDX) = IXC(7,SURF%ELEM((JJ)))
            else
              db_reversed(IDX) = IXTG(6,SURF%ELEM((JJ)))
            endif
          ENDIF  
        enddo
        print *, "there were ",NB_REVERSED," element(s) reversed along the path"
        print *, db_reversed(1:NB_REVERSED)        
        print *,"____________________________________________________"        
        DEALLOCATE(db_reversed)
      endif !debug_output

      !------------------------------------!
      ! 8. FREE MEMORY                     !
      !------------------------------------!
      IF(ALLOCATED(NB_ADJ))DEALLOCATE(NB_ADJ)
      IF(ALLOCATED(IAD_ADJ))DEALLOCATE(IAD_ADJ)
      IF(ALLOCATED(CHECK_FLAG_ELEM))DEALLOCATE(CHECK_FLAG_ELEM)
      IF(ALLOCATED(LIST_ADJ_TAB))DEALLOCATE(LIST_ADJ_TAB)
      IF(ALLOCATED(PATHS))DEALLOCATE(PATHS)
      IF(ALLOCATED(SIZES))DEALLOCATE(SIZES)
      IF(ALLOCATED(DUPLICATED_ELTS)) DEALLOCATE(DUPLICATED_ELTS)
      IF(ALLOCATED(PAIR_LIST)) DEALLOCATE(PAIR_LIST)
      IF(ALLOCATED(NB_PAIR_BY_EDGE)) DEALLOCATE(NB_PAIR_BY_EDGE)
      IF (ALLOCATED(IAD_COMP_CONNEX)) DEALLOCATE(IAD_COMP_CONNEX)
      CALL GRAPH_FREE_MEMORY(graph_ptr)


      END SUBROUTINE
Chd|====================================================================
Chd|  MONVOL_REVERSE_NORMALS        share/modules1/monvol_struct_mod.F
Chd|-- called by -----------
Chd|        HM_READ_MONVOL_TYPE1          source/airbag/hm_read_monvol_type1.F
Chd|        HM_READ_MONVOL_TYPE10         source/airbag/hm_read_monvol_type10.F
Chd|        HM_READ_MONVOL_TYPE11         source/airbag/hm_read_monvol_type11.F
Chd|        HM_READ_MONVOL_TYPE2          source/airbag/hm_read_monvol_type2.F
Chd|        HM_READ_MONVOL_TYPE3          source/airbag/hm_read_monvol_type3.F
Chd|        HM_READ_MONVOL_TYPE4          source/airbag/hm_read_monvol_type4.F
Chd|        HM_READ_MONVOL_TYPE5          source/airbag/hm_read_monvol_type5.F
Chd|        HM_READ_MONVOL_TYPE6          source/airbag/hm_read_monvol_type6.F
Chd|        HM_READ_MONVOL_TYPE7          source/airbag/hm_read_monvol_type7.F
Chd|        HM_READ_MONVOL_TYPE8          source/airbag/hm_read_monvol_type8.F
Chd|        HM_READ_MONVOL_TYPE9          source/airbag/hm_read_monvol_type9.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|====================================================================
      SUBROUTINE MONVOL_REVERSE_NORMALS(T_MONVOLN, TITLE, IVOLU, ITAB, SURF, IXC, IXTG, VOL, X, ITYPE)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine reverse all normals composing a given surface.
C Pre-condition : volume must be negative, otherwise normal are consider
C                 to be correctly oriented.
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
      USE MONVOL_STRUCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "param_c.inc"
#include "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER(LEN = nchartitle), INTENT(IN) :: TITLE
      INTEGER, INTENT(IN) :: IVOLU(NIMV), ITAB(*), ITYPE
      TYPE(SURF_), INTENT(INOUT) :: SURF
      my_real, INTENT(INOUT) :: VOL
      my_real, INTENT(IN) :: X(3,NUMNOD)
      INTEGER,INTENT(IN) :: IXC(NIXC, NUMELC), IXTG(NIXTG, NUMELTG)
      TYPE(MONVOL_STRUCT_), INTENT(INOUT) :: T_MONVOLN
C-----------------------------------------------
C     L o c a l   v a r i a b l e s
C-----------------------------------------------
      INTEGER JJ,ISH34,II(4),KK,NSEG
      CHARACTER(LEN=1024) :: FILENAME  
      LOGICAL debug_output    
C-----------------------------------------------
C     P r e   C o n d i t i o n
C-----------------------------------------------
! nothing to do if vol>0.0, normal are already correctly oriented.
C      IF(VOL > ZERO) RETURN   !commented to get debug output (surf in file)
C-----------------------------------------------
C     S o u r c e   L i n e s
C-----------------------------------------------

      IF (.NOT. T_MONVOLN%OK_REORIENT) RETURN
      NSEG = SURF%NSEG
      IF(VOL<ZERO)THEN
!print *, "VOLUME IS NEGATIVE, SURFACE IS REVERTED" .
         VOL = -VOL
         DO JJ=1,NSEG
            ISH34 = SURF%ELTYP(JJ)
            II(1:4) = SURF%NODES(JJ,1:4) 
            IF(ISH34 == 3)THEN                                         
!SHELL                                                   
               SURF%NODES(JJ,1:4)=(/ II(1), II(4), II(3), II(2) /)      
            ELSEIF(ISH34 == 7)THEN                                        
!SH3N                                                   
               SURF%NODES(JJ,1:4)=(/ II(2), II(1), II(3), II(4) /)      
            ENDIF                                                      
         ENDDO
         DO JJ = 1, T_MONVOLN%NB_FILL_TRI
            II(1:3) = T_MONVOLN%FILL_TRI(3 * (JJ - 1) + 1 : 3 * (JJ - 1) + 3)
            II(4) = II(3)
            T_MONVOLN%FILL_TRI(3 * (JJ - 1) + 1 : 3 * (JJ - 1) + 3) = (/ II(2), II(1), II(3) /)    
         ENDDO
      ENDIF
      
      !------------------------------------!
      ! 7. DEBUG OUTPUT : SURF IN FILE     !
      !------------------------------------!
      !--write a Radioss input file to check final surface
      debug_output=.false.
      if(debug_output)then
        NSEG=SURF%NSEG
        WRITE(FILENAME, "(A,I0,A)") "surfmesh_after_",T_MONVOLN%ID,"_0000.rad"
        OPEN(UNIT = 210486, FILE = TRIM(FILENAME), FORM ='formatted')
        WRITE(210486, '(A)') "#RADIOSS STARTER"
        WRITE(210486, '(A)') "/BEGIN"
        WRITE(210486, '(A)') "ORIENTED_SURFACE "
        WRITE(210486, '(A)') "       100         0"
        WRITE(210486, '(A)') "                   g                  mm                  ms"
        WRITE(210486, '(A)') "                   g                  mm                  ms"
        WRITE(210486, "(A5)") "/NODE"
        DO KK = 1, NUMNOD
        WRITE(210486, "(I10, 1PG20.13, 1PG20.13, 1PG20.13)") ITAB(kk),X(1, kk), X(2, kk), X(3, kk)
        ENDDO
        DO kk = 1, NSEG
         II(1:4) = SURF%NODES(kk,1:4)
         ISH34 = SURF%ELTYP(kk)
         IF (ISH34 == 3) THEN
           WRITE(210486, "(A6)") "/SHELL"
           WRITE(210486, '(I10,I10,I10,I10,I10)') IXC(7,SURF%ELEM(kk)), ITAB(II(1)), ITAB(II(2)),ITAB(II(3)), ITAB(II(4))
         ENDIF
        ENDDO
        DO kk = 1, NSEG
         II(1:4) = SURF%NODES(kk,1:4)
         ISH34 = SURF%ELTYP(kk)        
         IF (ISH34 == 7) THEN
          WRITE(210486, "(A5)") "/SH3N"         
          WRITE(210486, '(I10,I10,I10,I10)') IXTG(6,SURF%ELEM(kk)), ITAB(II(1)), ITAB(II(2)),ITAB(II(3))
         ENDIF
        ENDDO
        IF (T_MONVOLN%NB_FILL_TRI > 0) THEN
           WRITE(210486, "(A5)") "/SH3N"  
        ENDIF
        DO kk = 1, T_MONVOLN%NB_FILL_TRI
           WRITE(210486, '(I10,I10,I10,I10)') kk + NSEG, ITAB(T_MONVOLN%FILL_TRI(3 * (kk - 1) + 1)),
     .          ITAB(T_MONVOLN%FILL_TRI(3 * (kk - 1) + 2)), ITAB(T_MONVOLN%FILL_TRI(3 * (kk - 1) + 3))
        ENDDO
        CLOSE (210486)       
      endif  !debug_output    

      END SUBROUTINE      
      

Chd|====================================================================
Chd|  MONVOL_BUILD_EDGES            share/modules1/monvol_struct_mod.F
Chd|-- called by -----------
Chd|        MONVOL_CHECK_SURFCLOSE        share/modules1/monvol_struct_mod.F
Chd|        MONVOL_ORIENT_SURF            share/modules1/monvol_struct_mod.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|====================================================================
      SUBROUTINE MONVOL_BUILD_EDGES(T_MONVOLN, SURF)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     Build edges connectivity of monvol external surface
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
      USE MONVOL_STRUCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "param_c.inc"
#include "com04_c.inc"      
C-----------------------------------------------
C     D u m m y   a r g u m e n t s
C-----------------------------------------------
      TYPE(SURF_), INTENT(IN) :: SURF
      TYPE(MONVOL_STRUCT_), INTENT(INOUT) :: T_MONVOLN
C-----------------------------------------------
C     L o c a l   v a r i a b l e s
C-----------------------------------------------
      INTEGER :: NSEG, NTRI
      INTEGER, DIMENSION(:), ALLOCATABLE :: EDGE_ARRAY_N1, EDGE_ARRAY_N2, EDGE_ARRAY_ELEM, 
     .     NB_CONNECT
      INTEGER(8) :: edge_ptr
      INTEGER :: JJ, II(4), IDX, ELTYP, NEDG, KK
C-----------------------------------------------
C     S o u r c e   L i n e s
C-----------------------------------------------
      IF (ALLOCATED(T_MONVOLN%EDGE_NODE1)) DEALLOCATE(T_MONVOLN%EDGE_NODE1)
      IF (ALLOCATED(T_MONVOLN%EDGE_NODE2)) DEALLOCATE(T_MONVOLN%EDGE_NODE2)
      IF (ALLOCATED(T_MONVOLN%EDGE_ELEM)) DEALLOCATE(T_MONVOLN%EDGE_ELEM)
      IF (ALLOCATED(T_MONVOLN%IAD_EDGE_ELEM)) DEALLOCATE(T_MONVOLN%IAD_EDGE_ELEM)
      T_MONVOLN%NEDGE = 0
      
      NSEG = SURF%NSEG
      NTRI = T_MONVOLN%NB_FILL_TRI

      ALLOCATE(EDGE_ARRAY_N1(4 * (NSEG + NTRI)))
      ALLOCATE(EDGE_ARRAY_N2(4 * (NSEG + NTRI)))
      ALLOCATE(EDGE_ARRAY_ELEM(4 * (NSEG + NTRI)))

!     *******************************     !
!     ** External surface elements **     !
!     *******************************     !
      IDX = 0
      DO JJ = 1, NSEG
         II(1:4) = SURF%NODES(JJ, 1:4)
         ELTYP = SURF%ELTYP(JJ)
         SELECT CASE (ELTYP)
         CASE (3)
!     Quads
            EDGE_ARRAY_N1(IDX + 1) = MIN(II(1), II(2))
            EDGE_ARRAY_N2(IDX + 1) = MAX(II(1), II(2))
            EDGE_ARRAY_N1(IDX + 2) = MIN(II(2), II(3))
            EDGE_ARRAY_N2(IDX + 2) = MAX(II(2), II(3)) 
            EDGE_ARRAY_N1(IDX + 3) = MIN(II(3), II(4))
            EDGE_ARRAY_N2(IDX + 3) = MAX(II(3), II(4)) 
            EDGE_ARRAY_N1(IDX + 4) = MIN(II(4), II(1))
            EDGE_ARRAY_N2(IDX + 4) = MAX(II(4), II(1)) 
            EDGE_ARRAY_ELEM(IDX + 1:IDX + 4) = JJ
            IDX = IDX + 4
         CASE (7)
!     Tri
            EDGE_ARRAY_N1(IDX + 1) = MIN(II(1), II(2))
            EDGE_ARRAY_N2(IDX + 1) = MAX(II(1), II(2))
            EDGE_ARRAY_N1(IDX + 2) = MIN(II(2), II(3))
            EDGE_ARRAY_N2(IDX + 2) = MAX(II(2), II(3)) 
            EDGE_ARRAY_N1(IDX + 3) = MIN(II(3), II(1))
            EDGE_ARRAY_N2(IDX + 3) = MAX(II(3), II(1)) 
            EDGE_ARRAY_ELEM(IDX + 1:IDX + 3) = JJ
            IDX = IDX + 3      
         CASE DEFAULT

         END SELECT
      ENDDO

!     ****************************     !
!     ** Filling hole triangles **     !
!     ****************************     !
      DO JJ = 1, NTRI
         II(1:3) = T_MONVOLN%FILL_TRI(3 * (JJ - 1) + 1 : 3 * (JJ - 1) + 3)
         EDGE_ARRAY_N1(IDX + 1) = MIN(II(1), II(2))
         EDGE_ARRAY_N2(IDX + 1) = MAX(II(1), II(2))
         EDGE_ARRAY_N1(IDX + 2) = MIN(II(2), II(3))
         EDGE_ARRAY_N2(IDX + 2) = MAX(II(2), II(3)) 
         EDGE_ARRAY_N1(IDX + 3) = MIN(II(3), II(1))
         EDGE_ARRAY_N2(IDX + 3) = MAX(II(3), II(1)) 
         EDGE_ARRAY_ELEM(IDX + 1:IDX + 3) = JJ + NSEG
         IDX = IDX + 3 
      ENDDO
      NEDG = IDX

!     *********************************     !
!     ** Edge sorting and compaction **     !
!     *********************************     !
      
      edge_ptr = 0
      CALL EDGE_SORT(edge_ptr, EDGE_ARRAY_N1, EDGE_ARRAY_N2, EDGE_ARRAY_ELEM, NEDG)
      ALLOCATE(NB_CONNECT(NEDG))
      CALL EDGE_GET_NB_CONNECT(edge_ptr, NB_CONNECT)
      
      ALLOCATE(T_MONVOLN%EDGE_NODE1(NEDG))
      ALLOCATE(T_MONVOLN%EDGE_NODE2(NEDG))
      ALLOCATE(T_MONVOLN%EDGE_ELEM(SUM(NB_CONNECT)))
      ALLOCATE(T_MONVOLN%IAD_EDGE_ELEM(NEDG + 1))
          
      CALL EDGE_GET_CONNECT(edge_ptr, T_MONVOLN%EDGE_ELEM)

      T_MONVOLN%IAD_EDGE_ELEM(1) = 1
      DO JJ = 2, NEDG + 1
         T_MONVOLN%IAD_EDGE_ELEM(JJ) = T_MONVOLN%IAD_EDGE_ELEM(JJ - 1) + NB_CONNECT(JJ - 1)
      ENDDO
      DO JJ = 1, NEDG
         T_MONVOLN%EDGE_NODE1(JJ) = EDGE_ARRAY_N1(JJ)
         T_MONVOLN%EDGE_NODE2(JJ) = EDGE_ARRAY_N2(JJ)
      ENDDO

      CALL EDGE_FREE_MEMORY(edge_ptr)
      T_MONVOLN%NEDGE = NEDG
      T_MONVOLN%EDGES_BUILT = .TRUE.
      
!     *************************     !
!     ** Memory deallocation **     !
!     *************************     !
      DEALLOCATE(EDGE_ARRAY_N1)
      DEALLOCATE(EDGE_ARRAY_N2)
      DEALLOCATE(EDGE_ARRAY_ELEM)
      DEALLOCATE(NB_CONNECT)
C-----------------------------------------------
C     E n d   O f   S u b r o u t i n e
C-----------------------------------------------
      END SUBROUTINE
