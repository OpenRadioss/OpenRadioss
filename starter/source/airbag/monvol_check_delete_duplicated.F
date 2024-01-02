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
Chd|  MONVOL_CHECK_DELETE_DUPLICATEDsource/airbag/monvol_check_delete_duplicated.F
Chd|-- called by -----------
Chd|        INIT_MONVOL                   source/airbag/init_monvol.F   
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|====================================================================
      SUBROUTINE MONVOL_CHECK_DELETE_DUPLICATED(T_MONVOLN, NTG, NTGI, ITAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MONVOL_STRUCT_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"    
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(MONVOL_STRUCT_), INTENT(INOUT) :: T_MONVOLN
      INTEGER, INTENT(INOUT) :: NTG, NTGI
      INTEGER, DIMENSION(NUMNOD), INTENT(IN) :: ITAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER(8) :: EDGE_PTR, PAIR_VEC_PTR, GRAPH_PTR, LIST_PTR
      INTEGER :: II, JJ, KK, IDX, NEDGE
      INTEGER, DIMENSION(:), ALLOCATABLE :: EDGE_ARRAY_N1, EDGE_ARRAY_N2, 
     .     EDGE_ARRAY_ELEM, NB_CONNECT, EDGE_ELEM, IAD_EDGE_ELEM, PAIR_LIST, 
     .     SIZES, INV_LIST, LIST
      INTEGER :: NB_DUPLICATED_ELTS, NODE_LIST1(3), NODE_LIST2(3), IAD1, IAD2, NB_CON, 
     .     IELEM1, IELEM2, NB_COMMON_NODE, ELEM_ID1, ELEM_ID2, NPAIR
      INTEGER :: NB_CONNEX_COMP, ICOMP, PATH_SIZE
      INTEGER, DIMENSION(:), ALLOCATABLE :: FLAG_ELEM, PATHS
      INTEGER :: NTG_NEW, NTGI_NEW
      INTEGER, DIMENSION(:, :), ALLOCATABLE :: ELEM
      INTEGER, DIMENSION(:), ALLOCATABLE :: ISAVE

      CALL MY_ALLOC(ISAVE, (NTG + NTGI))
!     Resize and copy ELTG
      DO II = 1, NTG + NTGI
         ISAVE(II) = T_MONVOLN%ELTG(II)
      ENDDO
      DEALLOCATE(T_MONVOLN%ELTG)
      CALL MY_ALLOC(T_MONVOLN%ELTG, (NTG + NTGI))
      DO II = 1, NTG + NTGI
         T_MONVOLN%ELTG(II) = ISAVE(II)
      ENDDO
!     Build edges of the elements
      CALL MY_ALLOC(EDGE_ARRAY_N1, (3 * (NTG + NTGI)))
      CALL MY_ALLOC(EDGE_ARRAY_N2, (3 * (NTG + NTGI)))
      CALL MY_ALLOC(EDGE_ARRAY_ELEM, (3 * (NTG + NTGI)))
      IDX = 0
      DO II = 1, NTG + NTGI
         EDGE_ARRAY_N1(IDX + 1) = MIN(T_MONVOLN%ELEM(1, II), T_MONVOLN%ELEM(2, II))
         EDGE_ARRAY_N2(IDX + 1) = MAX(T_MONVOLN%ELEM(1, II), T_MONVOLN%ELEM(2, II))
         EDGE_ARRAY_N1(IDX + 2) = MIN(T_MONVOLN%ELEM(2, II), T_MONVOLN%ELEM(3, II))
         EDGE_ARRAY_N2(IDX + 2) = MAX(T_MONVOLN%ELEM(2, II), T_MONVOLN%ELEM(3, II))
         EDGE_ARRAY_N1(IDX + 3) = MIN(T_MONVOLN%ELEM(3, II), T_MONVOLN%ELEM(1, II))
         EDGE_ARRAY_N2(IDX + 3) = MAX(T_MONVOLN%ELEM(3, II), T_MONVOLN%ELEM(1, II))
         EDGE_ARRAY_ELEM(IDX + 1 : IDX + 3) = II
         IDX = IDX + 3
      ENDDO
      NEDGE = IDX

!     Edge sorting and compaction
      EDGE_PTR = 0
      CALL EDGE_SORT(EDGE_PTR, EDGE_ARRAY_N1, EDGE_ARRAY_N2, EDGE_ARRAY_ELEM, NEDGE)
      
      CALL MY_ALLOC(NB_CONNECT, (NEDGE))
      CALL EDGE_GET_NB_CONNECT(EDGE_PTR, NB_CONNECT)
      CALL MY_ALLOC(EDGE_ELEM, (SUM(NB_CONNECT)))
      CALL MY_ALLOC(IAD_EDGE_ELEM, (NEDGE + 1))
      CALL EDGE_GET_CONNECT(EDGE_PTR, EDGE_ELEM)

      IAD_EDGE_ELEM(1) = 1
      DO II = 2, NEDGE + 1
         IAD_EDGE_ELEM(II) = IAD_EDGE_ELEM(II - 1) + NB_CONNECT(II - 1)
      ENDDO

!     Check for duplicated elements
      CALL INTVECTOR_CREATE(PAIR_VEC_PTR)
      DO II = 1, NEDGE
         IAD1 = IAD_EDGE_ELEM(II)
         IAD2 = IAD_EDGE_ELEM(II + 1) - 1
         NB_CON = IAD2 - IAD1 + 1
         IF (NB_CON > 2) THEN
!     T-connection, or worse, or simply duplicated elements
            DO IELEM1 = IAD1, IAD2
               DO IELEM2 = IAD1, IAD2
                  ELEM_ID1 = EDGE_ELEM(IELEM1)
                  ELEM_ID2 = EDGE_ELEM(IELEM2)
                  IF (ELEM_ID1 /= ELEM_ID2) THEN
                     NODE_LIST1(1:3) = T_MONVOLN%ELEM(1:3, ELEM_ID1)
                     NODE_LIST2(1:3) = T_MONVOLN%ELEM(1:3, ELEM_ID2)
                     NB_COMMON_NODE = 0
                     DO JJ = 1, 3
                        DO KK = 1, 3
                           IF (NODE_LIST1(JJ) == NODE_LIST2(KK)) THEN
                              NB_COMMON_NODE = NB_COMMON_NODE + 1
                              EXIT
                           ENDIF
                        ENDDO
                     ENDDO
                     IF (NB_COMMON_NODE == 3) THEN
!     IELEM2 is duplicated from IELEM1
                        CALL INTVECTOR_PUSH_BACK(PAIR_VEC_PTR, ELEM_ID1)
                        CALL INTVECTOR_PUSH_BACK(PAIR_VEC_PTR, ELEM_ID2)
                     ENDIF
                  ENDIF
               ENDDO
            ENDDO
         ENDIF
      ENDDO
      
!     Get pairs of duplicated elements
      CALL INTVECTOR_GET_SIZE(PAIR_VEC_PTR, NPAIR)
      CALL MY_ALLOC(PAIR_LIST, (NPAIR))
      CALL INTVECTOR_COPY_TO(PAIR_VEC_PTR, PAIR_LIST)
      NPAIR = NPAIR / 2

!     Reduce
      CALL MY_ALLOC(FLAG_ELEM, (NTG + NTGI))
      FLAG_ELEM(1:NTG + NTGI) = 0
      CALL MY_ALLOC(INV_LIST, (NTG + NTGI))
      INV_LIST(1:NTG + NTGI) = 0

      NB_DUPLICATED_ELTS = 0
      CALL INTVECTOR_CREATE(LIST_PTR)
      DO II = 1, NPAIR
         IF (FLAG_ELEM(PAIR_LIST(2 * (II - 1) + 1)) == 0) THEN
            CALL INTVECTOR_PUSH_BACK(LIST_PTR, PAIR_LIST(2 * (II - 1) + 1))
            NB_DUPLICATED_ELTS = NB_DUPLICATED_ELTS + 1
            INV_LIST(PAIR_LIST(2 * (II - 1) + 1)) = NB_DUPLICATED_ELTS
            FLAG_ELEM(PAIR_LIST(2 * (II - 1) + 1)) = 1
         ENDIF
         IF (FLAG_ELEM(PAIR_LIST(2 * (II - 1) + 2)) == 0) THEN
            CALL INTVECTOR_PUSH_BACK(LIST_PTR, PAIR_LIST(2 * (II - 1) + 2))
            NB_DUPLICATED_ELTS = NB_DUPLICATED_ELTS + 1
            INV_LIST(PAIR_LIST(2 * (II - 1) + 2)) = NB_DUPLICATED_ELTS
            FLAG_ELEM(PAIR_LIST(2 * (II - 1) + 2)) = 1
         ENDIF
      ENDDO
      
      IF (NB_DUPLICATED_ELTS == 0) THEN
         RETURN
      ENDIF

      CALL INTVECTOR_GET_SIZE(LIST_PTR, NB_DUPLICATED_ELTS)
      CALL MY_ALLOC(LIST, (NB_DUPLICATED_ELTS))
      CALL INTVECTOR_COPY_TO(LIST_PTR, LIST)
      
      DO II = 1, NPAIR
         PAIR_LIST(2 * (II - 1) + 1) = INV_LIST(PAIR_LIST(2 * (II - 1) + 1)) - 1
         PAIR_LIST(2 * (II - 1) + 2) = INV_LIST(PAIR_LIST(2 * (II - 1) + 2)) - 1
      ENDDO

      GRAPH_PTR = 0
      CALL GRAPH_BUILD_PATH(NB_DUPLICATED_ELTS, NPAIR, PAIR_LIST, NB_CONNEX_COMP, GRAPH_PTR)
      CALL MY_ALLOC(SIZES, (NB_CONNEX_COMP))
      CALL GRAPH_GET_SIZES(GRAPH_PTR, SIZES)
      PATH_SIZE = SUM(SIZES)
      CALL MY_ALLOC(PATHS, (PATH_SIZE))
      CALL GRAPH_GET_PATH(GRAPH_PTR, PATHS)

      DO II = 1, PATH_SIZE
         PATHS(II) = LIST(PATHS(II) + 1)
      ENDDO

      
!     Flag Elems to suppress
      FLAG_ELEM(1:NTG + NTGI) = 1
      IAD1 = 1
      DO ICOMP = 1, NB_CONNEX_COMP
         IAD2 = IAD1 + SIZES(ICOMP) - 1
         CALL QUICKSORT_I(PATHS(IAD1:IAD2), IAD1, IAD2)
         ELEM_ID1 = PATHS(IAD1)
         NODE_LIST1(1:3) = ITAB(T_MONVOLN%ELEM(1:3, ELEM_ID1))
         CALL ANCMSG(MSGID = 2072, ANMODE = ANINFO, MSGTYPE = MSGWARNING,
     .        I1 = T_MONVOLN%ID, C1 = T_MONVOLN%TITLE, 
     .        I2 = NODE_LIST1(1), I3 = NODE_LIST1(2), I4 = NODE_LIST1(3), I5 = SIZES(ICOMP)-1)
         DO II = IAD1 + 1, IAD2
            FLAG_ELEM(PATHS(II)) = 0
         ENDDO
         IAD1 = IAD1 + SIZES(ICOMP)
      ENDDO

      NTG_NEW = SUM(FLAG_ELEM(1:NTG))
      NTGI_NEW = SUM(FLAG_ELEM(NTG + 1:NTG + NTGI))
      
      CALL MY_ALLOC(ELEM, 3, NTG_NEW + NTGI_NEW)
      IAD1 = 1
      DO II = 1, NTG + NTGI
         IF (FLAG_ELEM(II) == 1) THEN
            ELEM(1:3, IAD1) = T_MONVOLN%ELEM(1:3, II)
            IAD1 = IAD1 + 1
         ENDIF
      ENDDO
      DEALLOCATE(T_MONVOLN%ELEM)
!     Resize and copy FVBAG_ELEMID
      DO II = 1, NTG + NTGI
         ISAVE(II) = T_MONVOLN%FVBAG_ELEMID(II)
      ENDDO
      DEALLOCATE(T_MONVOLN%FVBAG_ELEMID)
      CALL MY_ALLOC(T_MONVOLN%FVBAG_ELEMID, NTG_NEW + NTGI_NEW)
      IAD1 = 1
      DO II = 1, NTG + NTGI
         IF (FLAG_ELEM(II) == 1) THEN
            T_MONVOLN%FVBAG_ELEMID(IAD1) = ISAVE(II)
            IAD1 = IAD1 + 1
         ENDIF
      ENDDO
!     Resize and copy ELTG
      DO II = 1, NTG + NTGI
         ISAVE(II) = T_MONVOLN%ELTG(II)
      ENDDO
      DEALLOCATE(T_MONVOLN%ELTG)
      CALL MY_ALLOC(T_MONVOLN%ELTG, (NTG_NEW + NTGI_NEW))
      IAD1 = 1
      DO II = 1, NTG + NTGI
         IF (FLAG_ELEM(II) == 1) THEN
            T_MONVOLN%ELTG(IAD1) = ISAVE(II)
            IAD1 = IAD1 + 1
         ENDIF
      ENDDO

      T_MONVOLN%NTG = NTG_NEW
      T_MONVOLN%NTGI = NTGI_NEW
      NTG = NTG_NEW
      NTGI = NTGI_NEW
      CALL MY_ALLOC(T_MONVOLN%ELEM, 3, NTG + NTGI)
      DO II = 1, NTG + NTGI
         T_MONVOLN%ELEM(1:3, II) = ELEM(1:3, II)
      ENDDO

!     ***********************    !
!     * Memory deallocation *    !
!     ***********************    !
      CALL EDGE_FREE_MEMORY(EDGE_PTR)
      CALL INTVECTOR_DELETE(PAIR_VEC_PTR)
      CALL INTVECTOR_DELETE(LIST_PTR)
      DEALLOCATE(ISAVE)
      DEALLOCATE(EDGE_ARRAY_N1)
      DEALLOCATE(EDGE_ARRAY_N2)
      DEALLOCATE(EDGE_ARRAY_ELEM)
      DEALLOCATE(NB_CONNECT)
      DEALLOCATE(EDGE_ELEM)
      DEALLOCATE(IAD_EDGE_ELEM)
      DEALLOCATE(PAIR_LIST)
      DEALLOCATE(FLAG_ELEM)
      DEALLOCATE(SIZES)
      DEALLOCATE(PATHS)
      DEALLOCATE(ELEM)
      END SUBROUTINE MONVOL_CHECK_DELETE_DUPLICATED
