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
Chd|  MONVOL_TRIANGULATE_SURFACE    source/airbag/monvol_triangulate_surface.F
Chd|-- called by -----------
Chd|        INIT_MONVOL                   source/airbag/init_monvol.F   
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|====================================================================
      SUBROUTINE MONVOL_TRIANGULATE_SURFACE(T_MONVOLN, IGRSURF, LOCAL_NODEID, LOCAL_INT_NODEID, TAGE, 
     .     X, KMESH, NNS, NNI, NTG, NTGI, SIZE1, SIZE2, FVBAG_ELEMID)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE MONVOL_STRUCT_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
!     NSURF
#include      "com04_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(MONVOL_STRUCT_), INTENT(INOUT) :: T_MONVOLN
      TYPE(SURF_), DIMENSION(NSURF), INTENT(IN) :: IGRSURF
      INTEGER, DIMENSION(NUMNOD), INTENT(IN) :: LOCAL_NODEID, LOCAL_INT_NODEID
      INTEGER, DIMENSION(IGRSURF(T_MONVOLN%EXT_SURFID)%NSEG), INTENT(IN) :: TAGE
      my_real, DIMENSION(3, NUMNOD), INTENT(IN) :: X
      INTEGER, INTENT(IN) :: NNS, NNI, KMESH, SIZE1, SIZE2
      INTEGER, INTENT(INOUT) :: NTG, NTGI, FVBAG_ELEMID(2 * (SIZE1 + SIZE2))
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: II, NODE1, NODE2, NODE3, NODE4, ITYPE, KK
      INTEGER :: NTRI, NQUAD, NNODE, IQUAD, ITRI, INODE
      INTEGER, DIMENSION(:), ALLOCATABLE :: QUAD, TRI, SPLITTED_QUAD
      my_real, DIMENSION(:), ALLOCATABLE :: COORD
      INTEGER, DIMENSION(:, :), ALLOCATABLE :: ELEM_TMP
      INTEGER :: NB_FILL_TRI

!     ******************************************    !
!     * Counting number of triangles and quads *    !
!     ******************************************    !
      NTRI = 0
      NQUAD = 0
      NTG = 0
      NTGI = 0

!     External surface
!     ----------------
      DO II = 1, IGRSURF(T_MONVOLN%EXT_SURFID)%NSEG
         ITYPE = IGRSURF(T_MONVOLN%EXT_SURFID)%ELTYP(II)
         SELECT CASE (TAGE(II))
         CASE(5)
            CYCLE
         CASE(0)
            IF (ITYPE == 7) THEN
               NTRI = NTRI + 1
               NTG = NTG + 1
            ELSE
               NQUAD = NQUAD + 1
               NTG = NTG + 2
            ENDIF
         CASE(1, 2, 3, 4)
            NTRI = NTRI + 1
            NTG = NTG + 1
         END SELECT
      ENDDO

!     Internal surface
!     ----------------
      IF (T_MONVOLN%INT_SURFID > 0) THEN
         DO II = 1, IGRSURF(T_MONVOLN%INT_SURFID)%NSEG
            IF (IGRSURF(T_MONVOLN%INT_SURFID)%ELTYP(II) == 7) THEN
               NTRI = NTRI + 1
               NTGI = NTGI + 1
            ELSE IF (IGRSURF(T_MONVOLN%INT_SURFID)%ELTYP(II) == 3) THEN
               NQUAD = NQUAD + 1
               NTGI = NTGI + 2
            ENDIF
         ENDDO
      ENDIF

!     Store triangle count into monvol struct
!     ---------------------------------------
      T_MONVOLN%NTG = NTG
      T_MONVOLN%NTGI = NTGI
      NB_FILL_TRI = T_MONVOLN%NB_FILL_TRI

      IF (NTG + NB_FILL_TRI + NTGI > 0) THEN
         ALLOCATE(T_MONVOLN%ELEM(3, NTG + NB_FILL_TRI + NTGI))
      ENDIF

      IF (KMESH == 12 .OR. KMESH == 14) THEN
         IF (NQUAD == 0) THEN
!     *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*    !
!     ***********************************    !
!     * Surface is already triangulated *    !
!     ***********************************    !
!     *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*    !
            ITRI = 1
            DO II = 1, IGRSURF(T_MONVOLN%EXT_SURFID)%NSEG
               NODE1 = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 1))
               NODE2 = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 2))
               NODE3 = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 3))
               NODE4 = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 4))
               SELECT CASE (TAGE(II))
               CASE(5)
                  CYCLE
               CASE(0)
                  T_MONVOLN%ELEM(1, ITRI) = NODE1
                  T_MONVOLN%ELEM(2, ITRI) = NODE2
                  T_MONVOLN%ELEM(3, ITRI) = NODE3
                  FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%EXT_SURFID)%ELEM(II)
                  T_MONVOLN%ELTG(ITRI) = NUMELC + FVBAG_ELEMID(ITRI)
               CASE(1)
                  T_MONVOLN%ELEM(1, ITRI) = NODE1
                  T_MONVOLN%ELEM(2, ITRI) = NODE2
                  T_MONVOLN%ELEM(3, ITRI) = NODE4
                  FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%EXT_SURFID)%ELEM(II)
                  T_MONVOLN%ELTG(ITRI) = NUMELC + FVBAG_ELEMID(ITRI)
              CASE(2)
                  T_MONVOLN%ELEM(1, ITRI) = NODE2
                  T_MONVOLN%ELEM(2, ITRI) = NODE3
                  T_MONVOLN%ELEM(3, ITRI) = NODE1
                  FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%EXT_SURFID)%ELEM(II)
                  T_MONVOLN%ELTG(ITRI) = NUMELC + FVBAG_ELEMID(ITRI)
              CASE(3)
                  T_MONVOLN%ELEM(1, ITRI) = NODE3
                  T_MONVOLN%ELEM(2, ITRI) = NODE4
                  T_MONVOLN%ELEM(3, ITRI) = NODE2
                  FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%EXT_SURFID)%ELEM(II)
                  T_MONVOLN%ELTG(ITRI) = NUMELC + FVBAG_ELEMID(ITRI)
               CASE(4)
                  T_MONVOLN%ELEM(1, ITRI) = NODE4
                  T_MONVOLN%ELEM(2, ITRI) = NODE1
                  T_MONVOLN%ELEM(3, ITRI) = NODE3
                  FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%EXT_SURFID)%ELEM(II)
                  T_MONVOLN%ELTG(ITRI) = NUMELC + FVBAG_ELEMID(ITRI)
               END SELECT
               ITRI = ITRI + 1
            ENDDO
            IF (NB_FILL_TRI > 0) THEN
               DO II = 1, NB_FILL_TRI
                  NODE1 = T_MONVOLN%FILL_TRI(3 * (II - 1) + 1)
                  NODE2 = T_MONVOLN%FILL_TRI(3 * (II - 1) + 2)
                  NODE3 = T_MONVOLN%FILL_TRI(3 * (II - 1) + 3)
                  T_MONVOLN%ELEM(1, ITRI) = LOCAL_NODEID(NODE1)
                  T_MONVOLN%ELEM(2, ITRI) = LOCAL_NODEID(NODE2)
                  T_MONVOLN%ELEM(3, ITRI) = LOCAL_NODEID(NODE3)
                  FVBAG_ELEMID(ITRI) = 0
                  ITRI = ITRI + 1
               ENDDO
            ENDIF
            IF (T_MONVOLN%INT_SURFID > 0) THEN
               DO II = 1, IGRSURF(T_MONVOLN%INT_SURFID)%NSEG
                  NODE1 = LOCAL_INT_NODEID(IGRSURF(T_MONVOLN%INT_SURFID)%NODES(II, 1))
                  NODE2 = LOCAL_INT_NODEID(IGRSURF(T_MONVOLN%INT_SURFID)%NODES(II, 2))
                  NODE3 = LOCAL_INT_NODEID(IGRSURF(T_MONVOLN%INT_SURFID)%NODES(II, 3))
                  T_MONVOLN%ELEM(1, ITRI) = NODE1
                  T_MONVOLN%ELEM(2, ITRI) = NODE2
                  T_MONVOLN%ELEM(3, ITRI) = NODE3
                  FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%INT_SURFID)%ELEM(II)
                  T_MONVOLN%ELTG(ITRI) = NUMELC + FVBAG_ELEMID(ITRI)
                  ITRI = ITRI + 1
               ENDDO
            ENDIF
            NTG = NTG + NB_FILL_TRI
            T_MONVOLN%NTG = T_MONVOLN%NTG + NB_FILL_TRI
         ELSE
!     *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-    !
!     **********************************    !
!     * Hypermesh Smart Quad splitting *    !
!     **********************************    !
!     *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-    !

!     Allocate
!     --------
            IF (NTRI + NB_FILL_TRI > 0) ALLOCATE(TRI(4 * (NTRI + NB_FILL_TRI)))
            IF (NQUAD > 0) THEN 
               ALLOCATE(QUAD(5 * NQUAD))
               ALLOCATE(SPLITTED_QUAD(6 * NQUAD))
            ENDIF
            ALLOCATE(COORD(4 * (NNS + NNI)))
!     *****************************    !
!     * Fill in quad and tri tabs *    !
!     *****************************    !
            IQUAD = 0
            ITRI = 0

!     External surface
!     ----------------
            DO II = 1, IGRSURF(T_MONVOLN%EXT_SURFID)%NSEG
               ITYPE = IGRSURF(T_MONVOLN%EXT_SURFID)%ELTYP(II)
               SELECT CASE (TAGE(II))
               CASE(5)
                  CYCLE
               CASE(0)
                  IF (ITYPE == 7) THEN
                     TRI(ITRI + 1) = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 1))
                     TRI(ITRI + 2) = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 2))
                     TRI(ITRI + 3) = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 3))
                     TRI(ITRI + 4) = 0
                     ITRI = ITRI + 4
                  ELSE
                     QUAD(IQUAD + 1) = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 1))
                     QUAD(IQUAD + 2) = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 2))
                     QUAD(IQUAD + 3) = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 3))
                     QUAD(IQUAD + 4) = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 4))
                     QUAD(IQUAD + 5) = 0
                     IQUAD = IQUAD + 5
                  ENDIF
               CASE(1)
                  TRI(ITRI + 1) = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 1))
                  TRI(ITRI + 2) = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 2))
                  TRI(ITRI + 3) = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 4))
                  TRI(ITRI + 4) = 0
                  ITRI = ITRI + 4
               CASE(2)
                  TRI(ITRI + 1) = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 2))
                  TRI(ITRI + 2) = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 3))
                  TRI(ITRI + 3) = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 1))
                  TRI(ITRI + 4) = 0
                  ITRI = ITRI + 4
               CASE(3)
                  TRI(ITRI + 1) = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 3))
                  TRI(ITRI + 2) = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 4))
                  TRI(ITRI + 3) = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 2))
                  TRI(ITRI + 4) = 0
                  ITRI = ITRI + 4                  
               CASE(4)
                  TRI(ITRI + 1) = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 4))
                  TRI(ITRI + 2) = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 1))
                  TRI(ITRI + 3) = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 3))
                  TRI(ITRI + 4) = 0
                  ITRI = ITRI + 4                  
               END SELECT
            ENDDO   

!     Additional triangles
!     --------------------
            IF (NB_FILL_TRI > 0) THEN
               DO II = 1, NB_FILL_TRI
                  NODE1 = T_MONVOLN%FILL_TRI(3 * (II - 1) + 1)
                  NODE2 = T_MONVOLN%FILL_TRI(3 * (II - 1) + 2)
                  NODE3 = T_MONVOLN%FILL_TRI(3 * (II - 1) + 3)
                  TRI(ITRI + 1) = LOCAL_NODEID(NODE1)
                  TRI(ITRI + 2) = LOCAL_NODEID(NODE2)
                  TRI(ITRI + 3) = LOCAL_NODEID(NODE3)
                  TRI(ITRI + 4) = 0
                  ITRI = ITRI + 4
               ENDDO
            ENDIF

!     Internal surface
!     ----------------
            IF (T_MONVOLN%INT_SURFID > 0) THEN
               DO II = 1, IGRSURF(T_MONVOLN%INT_SURFID)%NSEG
                  IF (IGRSURF(T_MONVOLN%INT_SURFID)%ELTYP(II) == 7) THEN
                     TRI(ITRI + 1) = LOCAL_INT_NODEID(IGRSURF(T_MONVOLN%INT_SURFID)%NODES(II, 1))
                     TRI(ITRI + 2) = LOCAL_INT_NODEID(IGRSURF(T_MONVOLN%INT_SURFID)%NODES(II, 2))
                     TRI(ITRI + 3) = LOCAL_INT_NODEID(IGRSURF(T_MONVOLN%INT_SURFID)%NODES(II, 3))
                     TRI(ITRI + 4) = 0
                     ITRI = ITRI + 4
                  ELSE IF (IGRSURF(T_MONVOLN%INT_SURFID)%ELTYP(II) == 3) THEN
                     QUAD(IQUAD + 1) = LOCAL_INT_NODEID(IGRSURF(T_MONVOLN%INT_SURFID)%NODES(II, 1))
                     QUAD(IQUAD + 2) = LOCAL_INT_NODEID(IGRSURF(T_MONVOLN%INT_SURFID)%NODES(II, 2))
                     QUAD(IQUAD + 3) = LOCAL_INT_NODEID(IGRSURF(T_MONVOLN%INT_SURFID)%NODES(II, 3))
                     QUAD(IQUAD + 4) = LOCAL_INT_NODEID(IGRSURF(T_MONVOLN%INT_SURFID)%NODES(II, 4))
                     QUAD(IQUAD + 5) = 0
                     IQUAD = IQUAD + 5
                  ENDIF
               ENDDO
            ENDIF  
!     ****************************    !
!     * Fill in node coordinates *    !
!     ****************************    !
            INODE = 0
            NNODE = NNS + NNI
            DO II = 1, NNS + NNI
               COORD(INODE + 1) = X(1, T_MONVOLN%NODES(II))
               COORD(INODE + 2) = X(2, T_MONVOLN%NODES(II))
               COORD(INODE + 3) = X(3, T_MONVOLN%NODES(II))
               COORD(INODE + 4) = ZERO
               INODE = INODE + 4
            ENDDO
!     **********************    !
!     * Call HM quad_split *    !
!     **********************    !
#ifdef DNC
            CALL hm_quad_split(NNODE, COORD(1), NTRI, TRI(1), NQUAD, QUAD(1), SPLITTED_QUAD(1))
#else
       IF(NQUAD > 0) SPLITTED_QUAD = 0
#endif
!     ****************************    !
!     * Fill in monvol structure *    !
!     ****************************    !
            ITRI = 0
            IQUAD = 0
            DO II = 1, IGRSURF(T_MONVOLN%EXT_SURFID)%NSEG
               ITYPE = IGRSURF(T_MONVOLN%EXT_SURFID)%ELTYP(II)
               NODE1 = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 1))
               NODE2 = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 2))
               NODE3 = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 3))
               NODE4 = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 4))
               SELECT CASE (TAGE(II))
               CASE(5)
                  CYCLE
               CASE(0)
                  IF (ITYPE == 3) THEN
                     IQUAD = IQUAD + 1
                     ITRI = ITRI + 1
                     T_MONVOLN%ELEM(1, ITRI) = SPLITTED_QUAD(6 * (IQUAD - 1) + 1)
                     T_MONVOLN%ELEM(2, ITRI) = SPLITTED_QUAD(6 * (IQUAD - 1) + 2)
                     T_MONVOLN%ELEM(3, ITRI) = SPLITTED_QUAD(6 * (IQUAD - 1) + 3)
                     FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%EXT_SURFID)%ELEM(II)
                     T_MONVOLN%ELTG(ITRI) = FVBAG_ELEMID(ITRI)
                     ITRI = ITRI + 1
                     T_MONVOLN%ELEM(1, ITRI) = SPLITTED_QUAD(6 * (IQUAD - 1) + 4)
                     T_MONVOLN%ELEM(2, ITRI) = SPLITTED_QUAD(6 * (IQUAD - 1) + 5)
                     T_MONVOLN%ELEM(3, ITRI) = SPLITTED_QUAD(6 * (IQUAD - 1) + 6)
                     FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%EXT_SURFID)%ELEM(II)
                     T_MONVOLN%ELTG(ITRI) = FVBAG_ELEMID(ITRI)
                 ELSE
                     ITRI = ITRI + 1
                     T_MONVOLN%ELEM(1, ITRI) = NODE1
                     T_MONVOLN%ELEM(2, ITRI) = NODE2
                     T_MONVOLN%ELEM(3, ITRI) = NODE3
                     FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%EXT_SURFID)%ELEM(II)
                     T_MONVOLN%ELTG(ITRI) = NUMELC + FVBAG_ELEMID(ITRI)
                 ENDIF
               CASE(1)
                  ITRI = ITRI + 1
                  T_MONVOLN%ELEM(1, ITRI) = NODE1
                  T_MONVOLN%ELEM(2, ITRI) = NODE2
                  T_MONVOLN%ELEM(3, ITRI) = NODE4
                  FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%EXT_SURFID)%ELEM(II)
                  IF (ITYPE == 7) THEN
                     T_MONVOLN%ELTG(ITRI) = NUMELC + FVBAG_ELEMID(ITRI)
                  ELSE
                     T_MONVOLN%ELTG(ITRI) = FVBAG_ELEMID(ITRI)
                  ENDIF
               CASE(2)
                  ITRI = ITRI + 1
                  T_MONVOLN%ELEM(1, ITRI) = NODE2
                  T_MONVOLN%ELEM(2, ITRI) = NODE3
                  T_MONVOLN%ELEM(3, ITRI) = NODE1
                  FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%EXT_SURFID)%ELEM(II)
                  IF (ITYPE == 7) THEN
                     T_MONVOLN%ELTG(ITRI) = NUMELC + FVBAG_ELEMID(ITRI)
                  ELSE
                     T_MONVOLN%ELTG(ITRI) = FVBAG_ELEMID(ITRI)
                  ENDIF
               CASE(3)
                  ITRI = ITRI + 1
                  T_MONVOLN%ELEM(1, ITRI) = NODE3
                  T_MONVOLN%ELEM(2, ITRI) = NODE4
                  T_MONVOLN%ELEM(3, ITRI) = NODE2
                  FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%EXT_SURFID)%ELEM(II)
                  IF (ITYPE == 7) THEN
                     T_MONVOLN%ELTG(ITRI) = NUMELC + FVBAG_ELEMID(ITRI)
                  ELSE
                     T_MONVOLN%ELTG(ITRI) = FVBAG_ELEMID(ITRI)
                  ENDIF
               CASE(4)
                  ITRI = ITRI + 1
                  T_MONVOLN%ELEM(1, ITRI) = NODE4
                  T_MONVOLN%ELEM(2, ITRI) = NODE1
                  T_MONVOLN%ELEM(3, ITRI) = NODE3
                  FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%EXT_SURFID)%ELEM(II)
                  IF (ITYPE == 7) THEN
                     T_MONVOLN%ELTG(ITRI) = NUMELC + FVBAG_ELEMID(ITRI)
                  ELSE
                     T_MONVOLN%ELTG(ITRI) = FVBAG_ELEMID(ITRI)
                  ENDIF
               END SELECT
            ENDDO
            IF (NB_FILL_TRI > 0) THEN
               DO II = 1, NB_FILL_TRI
                  NODE1 = T_MONVOLN%FILL_TRI(3 * (II - 1) + 1)
                  NODE2 = T_MONVOLN%FILL_TRI(3 * (II - 1) + 2)
                  NODE3 = T_MONVOLN%FILL_TRI(3 * (II - 1) + 3)
                  ITRI = ITRI + 1
                  T_MONVOLN%ELEM(1, ITRI) = LOCAL_NODEID(NODE1)
                  T_MONVOLN%ELEM(2, ITRI) = LOCAL_NODEID(NODE2)
                  T_MONVOLN%ELEM(3, ITRI) = LOCAL_NODEID(NODE3)
                  FVBAG_ELEMID(ITRI) = 0
               ENDDO
            ENDIF
            IF (T_MONVOLN%INT_SURFID > 0) THEN
               DO II = 1, IGRSURF(T_MONVOLN%INT_SURFID)%NSEG
                  ITYPE = IGRSURF(T_MONVOLN%INT_SURFID)%ELTYP(II)
                  NODE1 = LOCAL_INT_NODEID(IGRSURF(T_MONVOLN%INT_SURFID)%NODES(II, 1))
                  NODE2 = LOCAL_INT_NODEID(IGRSURF(T_MONVOLN%INT_SURFID)%NODES(II, 2))
                  NODE3 = LOCAL_INT_NODEID(IGRSURF(T_MONVOLN%INT_SURFID)%NODES(II, 3))
                  NODE4 = LOCAL_INT_NODEID(IGRSURF(T_MONVOLN%INT_SURFID)%NODES(II, 4))
                  IF (ITYPE == 3) THEN
                     IQUAD = IQUAD + 1
                     ITRI = ITRI + 1
                     T_MONVOLN%ELEM(1, ITRI) = SPLITTED_QUAD(6 * (IQUAD - 1) + 1)
                     T_MONVOLN%ELEM(2, ITRI) = SPLITTED_QUAD(6 * (IQUAD - 1) + 2)
                     T_MONVOLN%ELEM(3, ITRI) = SPLITTED_QUAD(6 * (IQUAD - 1) + 3)
                     FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%INT_SURFID)%ELEM(II)
                     T_MONVOLN%ELTG(ITRI) = FVBAG_ELEMID(ITRI)
                     ITRI = ITRI + 1
                     T_MONVOLN%ELEM(1, ITRI) = SPLITTED_QUAD(6 * (IQUAD - 1) + 4)
                     T_MONVOLN%ELEM(2, ITRI) = SPLITTED_QUAD(6 * (IQUAD - 1) + 5)
                     T_MONVOLN%ELEM(3, ITRI) = SPLITTED_QUAD(6 * (IQUAD - 1) + 6)
                     FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%INT_SURFID)%ELEM(II)
                     T_MONVOLN%ELTG(ITRI) = FVBAG_ELEMID(ITRI)
                  ELSE
                     ITRI = ITRI + 1
                     T_MONVOLN%ELEM(1, ITRI) = NODE1
                     T_MONVOLN%ELEM(2, ITRI) = NODE2
                     T_MONVOLN%ELEM(3, ITRI) = NODE3
                     FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%INT_SURFID)%ELEM(II)
                     T_MONVOLN%ELTG(ITRI) = NUMELC + FVBAG_ELEMID(ITRI)
                  ENDIF
               ENDDO
            ENDIF
            NTG = NTG + NB_FILL_TRI
            T_MONVOLN%NTG = T_MONVOLN%NTG + NB_FILL_TRI

            IF (1==0) THEN
               OPEN(UNIT = 210486, FILE = "OUTPUT_0000.rad", FORM ='formatted')
               WRITE(210486, '(A)') "#RADIOSS STARTER"
               WRITE(210486, '(A)') "/BEGIN"
               WRITE(210486, '(A)') "ORIENTED_SURFACE "
               WRITE(210486, '(A)') "       100         0"
               WRITE(210486, '(A)') "                   g                  mm                  ms"
               WRITE(210486, '(A)') "                   g                  mm                  ms"
               WRITE(210486, "(A5)") "/NODE"
               DO KK = 1, NNODE
                  WRITE(210486, "(I10, 1PG20.13, 1PG20.13, 1PG20.13)") KK, COORD(4 * (KK - 1) + 1), COORD(4 * (KK - 1) + 2),
     .                 COORD(4 * (KK - 1) + 3)
               ENDDO
               WRITE(210486, "(A5)") "/SH3N"
                    
               DO KK = 1, NTG
                  WRITE(210486, '(I10,I10,I10,I10)') KK, T_MONVOLN%ELEM(1, KK), T_MONVOLN%ELEM(2, KK), T_MONVOLN%ELEM(3, KK)
               ENDDO
               CLOSE (210486) 
            ENDIF
            IF (1==0) THEN
               OPEN(UNIT = 210486, FILE = "OUTPUT_IN_0000.rad", FORM ='formatted')
               WRITE(210486, '(A)') "#RADIOSS STARTER"
               WRITE(210486, '(A)') "/BEGIN"
               WRITE(210486, '(A)') "ORIENTED_SURFACE "
               WRITE(210486, '(A)') "       100         0"
               WRITE(210486, '(A)') "                   g                  mm                  ms"
               WRITE(210486, '(A)') "                   g                  mm                  ms"
               WRITE(210486, "(A5)") "/NODE"
               DO KK = 1, NNODE
                  WRITE(210486, "(I10, 1PG20.13, 1PG20.13, 1PG20.13)") KK, COORD(4 * (KK - 1) + 1), COORD(4 * (KK - 1) + 2),
     .                 COORD(4 * (KK - 1) + 3)
               ENDDO
               WRITE(210486, "(A5)") "/SH3N"
                    
               DO KK = NTG + 1, NTG + NTGI
                  WRITE(210486, '(I10,I10,I10,I10)') KK, T_MONVOLN%ELEM(1, KK), T_MONVOLN%ELEM(2, KK), T_MONVOLN%ELEM(3, KK)
               ENDDO
               CLOSE (210486) 
            ENDIF
!     ***********************    !
!     * Memory deallocation *    !
!     ***********************    !
            IF (ALLOCATED(QUAD)) DEALLOCATE(QUAD)
            IF (ALLOCATED(TRI)) DEALLOCATE(TRI)
            IF (ALLOCATED(COORD)) DEALLOCATE(COORD)
            IF (ALLOCATED(SPLITTED_QUAD)) DEALLOCATE(SPLITTED_QUAD)
         ENDIF
      ELSE
!     *-*-*-*-*-*-*-*-*-*-*-*-*-*-    !
!     ****************************    !
!     * Radioss legacy splitting *    !
!     ****************************    !
!     *-*-*-*-*-*-*-*-*-*-*-*-*-*-    !
         ITRI = 1
         DO II = 1, IGRSURF(T_MONVOLN%EXT_SURFID)%NSEG
            ITYPE = IGRSURF(T_MONVOLN%EXT_SURFID)%ELTYP(II)
            NODE1 = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 1))
            NODE2 = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 2))
            NODE3 = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 3))
            NODE4 = LOCAL_NODEID(IGRSURF(T_MONVOLN%EXT_SURFID)%NODES(II, 4))
            SELECT CASE (TAGE(II))
            CASE(5)
               CYCLE
            CASE(0)
               IF (ITYPE == 7) THEN
                  T_MONVOLN%ELEM(1, ITRI) = NODE1
                  T_MONVOLN%ELEM(2, ITRI) = NODE2
                  T_MONVOLN%ELEM(3, ITRI) = NODE3
                  FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%EXT_SURFID)%ELEM(II)
                  T_MONVOLN%ELTG(ITRI) = NUMELC + FVBAG_ELEMID(ITRI)
               ELSE
                  T_MONVOLN%ELEM(1, ITRI) = NODE1
                  T_MONVOLN%ELEM(2, ITRI) = NODE2
                  T_MONVOLN%ELEM(3, ITRI) = NODE4
                  FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%EXT_SURFID)%ELEM(II)
                  T_MONVOLN%ELTG(ITRI) = FVBAG_ELEMID(ITRI)
                  ITRI = ITRI + 1
                  T_MONVOLN%ELEM(1, ITRI) = NODE2
                  T_MONVOLN%ELEM(2, ITRI) = NODE3
                  T_MONVOLN%ELEM(3, ITRI) = NODE4
                  FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%EXT_SURFID)%ELEM(II)
                  T_MONVOLN%ELTG(ITRI) = FVBAG_ELEMID(ITRI)
               ENDIF
            CASE(1)
               T_MONVOLN%ELEM(1, ITRI) = NODE1
               T_MONVOLN%ELEM(2, ITRI) = NODE2
               T_MONVOLN%ELEM(3, ITRI) = NODE4
               FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%EXT_SURFID)%ELEM(II)
               IF (ITYPE == 7) THEN
                  T_MONVOLN%ELTG(ITRI) = NUMELC + FVBAG_ELEMID(ITRI)
               ELSE
                  T_MONVOLN%ELTG(ITRI) = FVBAG_ELEMID(ITRI)
               ENDIF
            CASE(2)
               T_MONVOLN%ELEM(1, ITRI) = NODE2
               T_MONVOLN%ELEM(2, ITRI) = NODE3
               T_MONVOLN%ELEM(3, ITRI) = NODE1
               FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%EXT_SURFID)%ELEM(II)
               IF (ITYPE == 7) THEN
                  T_MONVOLN%ELTG(ITRI) = NUMELC + FVBAG_ELEMID(ITRI)
               ELSE
                  T_MONVOLN%ELTG(ITRI) = FVBAG_ELEMID(ITRI)
               ENDIF
            CASE(3)
               T_MONVOLN%ELEM(1, ITRI) = NODE3
               T_MONVOLN%ELEM(2, ITRI) = NODE4
               T_MONVOLN%ELEM(3, ITRI) = NODE2
               FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%EXT_SURFID)%ELEM(II)
               IF (ITYPE == 7) THEN
                  T_MONVOLN%ELTG(ITRI) = NUMELC + FVBAG_ELEMID(ITRI)
               ELSE
                  T_MONVOLN%ELTG(ITRI) = FVBAG_ELEMID(ITRI)
               ENDIF
            CASE(4)
               T_MONVOLN%ELEM(1, ITRI) = NODE4
               T_MONVOLN%ELEM(2, ITRI) = NODE1
               T_MONVOLN%ELEM(3, ITRI) = NODE3
               FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%EXT_SURFID)%ELEM(II)
               IF (ITYPE == 7) THEN
                  T_MONVOLN%ELTG(ITRI) = NUMELC + FVBAG_ELEMID(ITRI)
               ELSE
                  T_MONVOLN%ELTG(ITRI) = FVBAG_ELEMID(ITRI)
               ENDIF
            END SELECT
            ITRI = ITRI + 1
         ENDDO
         IF (ITRI - 1 /= NTG) THEN
            NTG = ITRI - 1
            T_MONVOLN%NTG = NTG
            ALLOCATE(ELEM_TMP(3, NTG))
            DO II = 1, NTG
               ELEM_TMP(1:3, II) = T_MONVOLN%ELEM(1:3, II)
            ENDDO
            DEALLOCATE(T_MONVOLN%ELEM)
            ALLOCATE(T_MONVOLN%ELEM(3, NTG + NB_FILL_TRI + NTGI))
            DO II = 1, NTG
               T_MONVOLN%ELEM(1:3, II) = ELEM_TMP(1:3, II)
            ENDDO
            DEALLOCATE(ELEM_TMP)
         ENDIF
         IF (NB_FILL_TRI > 0) THEN
            DO II = 1, NB_FILL_TRI
               NODE1 = T_MONVOLN%FILL_TRI(3 * (II - 1) + 1)
               NODE2 = T_MONVOLN%FILL_TRI(3 * (II - 1) + 2)
               NODE3 = T_MONVOLN%FILL_TRI(3 * (II - 1) + 3)
               T_MONVOLN%ELEM(1, ITRI) = LOCAL_NODEID(NODE1)
               T_MONVOLN%ELEM(2, ITRI) = LOCAL_NODEID(NODE2)
               T_MONVOLN%ELEM(3, ITRI) = LOCAL_NODEID(NODE3)
               FVBAG_ELEMID(ITRI) = 0
               ITRI = ITRI + 1
            ENDDO
         ENDIF
         NTG = NTG + NB_FILL_TRI
         T_MONVOLN%NTG = T_MONVOLN%NTG + NB_FILL_TRI

         IF (NTGI > 0) THEN
            DO II = 1, IGRSURF(T_MONVOLN%INT_SURFID)%NSEG
               ITYPE = IGRSURF(T_MONVOLN%INT_SURFID)%ELTYP(II)
               NODE1 = LOCAL_INT_NODEID(IGRSURF(T_MONVOLN%INT_SURFID)%NODES(II, 1))
               NODE2 = LOCAL_INT_NODEID(IGRSURF(T_MONVOLN%INT_SURFID)%NODES(II, 2))
               NODE3 = LOCAL_INT_NODEID(IGRSURF(T_MONVOLN%INT_SURFID)%NODES(II, 3))
               NODE4 = LOCAL_INT_NODEID(IGRSURF(T_MONVOLN%INT_SURFID)%NODES(II, 4))
               IF (ITYPE == 7) THEN
                  T_MONVOLN%ELEM(1, ITRI) = NODE1
                  T_MONVOLN%ELEM(2, ITRI) = NODE2
                  T_MONVOLN%ELEM(3, ITRI) = NODE3
                  FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%INT_SURFID)%ELEM(II)
                  T_MONVOLN%ELTG(ITRI) = NUMELC + FVBAG_ELEMID(ITRI)
                  ITRI = ITRI + 1
               ELSE
                  T_MONVOLN%ELEM(1, ITRI) = NODE1
                  T_MONVOLN%ELEM(2, ITRI) = NODE2
                  T_MONVOLN%ELEM(3, ITRI) = NODE3
                  FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%INT_SURFID)%ELEM(II)
                  T_MONVOLN%ELTG(ITRI) = FVBAG_ELEMID(ITRI)
                  ITRI = ITRI + 1
                  T_MONVOLN%ELEM(1, ITRI) = NODE1
                  T_MONVOLN%ELEM(2, ITRI) = NODE3
                  T_MONVOLN%ELEM(3, ITRI) = NODE4
                  FVBAG_ELEMID(ITRI) = IGRSURF(T_MONVOLN%INT_SURFID)%ELEM(II)
                  T_MONVOLN%ELTG(ITRI) = FVBAG_ELEMID(ITRI)
                  ITRI = ITRI + 1
               ENDIF
            ENDDO
         ENDIF
      ENDIF
      END SUBROUTINE MONVOL_TRIANGULATE_SURFACE
