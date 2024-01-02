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
Chd|  CREATE_LINE_FROM_SURFACE_EXT  source/model/sets/create_line_from_surface_ext.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|====================================================================
      SUBROUTINE CREATE_LINE_FROM_SURFACE_EXT(CLAUSE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
      USE SETDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (SET_) :: CLAUSE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,J1,J2,SIZEMAX,NSEG,IEXT,NSEG_EDGE_EXT,
     .        NSEG_EDGE_ALL,NSEG_SURF,LINE_NENTITY
      INTEGER IWORK(70000),IPERM(4)
!
      DATA IPERM /2,3,4,1/
!
      INTEGER , DIMENSION(:),ALLOCATABLE :: INDEX,IW1,IW2,IW5,IW6
      INTEGER , DIMENSION(:,:),ALLOCATABLE :: ITRI,LINE_ORD
C=======================================================================
      SIZEMAX = CLAUSE%NB_LINE_SEG +  4*CLAUSE%NB_SURF_SEG

      IF (SIZEMAX == 0) RETURN

!
      LINE_NENTITY = 4 ! NOD1, NOD2, ELTYP, ELEM
      ALLOCATE(LINE_ORD(LINE_NENTITY,SIZEMAX))
      ALLOCATE(ITRI(3,SIZEMAX))
      ALLOCATE(INDEX(2*SIZEMAX))
!
      ALLOCATE(IW1(4*SIZEMAX))
      ALLOCATE(IW2(4*SIZEMAX))
      ALLOCATE(IW5(4*SIZEMAX))
      ALLOCATE(IW6(4*SIZEMAX))
!------------------
      ! NSEG of lines  initialization
      NSEG = CLAUSE%NB_LINE_SEG  ! lines from 1D_element
!     Fill in LINE_ORD array the already stored line from 1D_element
      DO I=1,NSEG
        LINE_ORD(1,I) = CLAUSE%LINE_NODES(I,1)
        LINE_ORD(2,I) = CLAUSE%LINE_NODES(I,2)
        LINE_ORD(3,I) = CLAUSE%LINE_ELTYP(I)
        LINE_ORD(4,I) = CLAUSE%LINE_ELEM(I)
      ENDDO
!
!     extract lines from furfaces
!
      NSEG_SURF = CLAUSE%NB_SURF_SEG
      IF (NSEG_SURF > 0) THEN
!***********************************
!***********************************
      ! -- EXT -- edges form SURFACE
!***********************************
!***********************************
        NSEG_EDGE_EXT = 0
!---
        K=0
        IW1 = 0
        IW2 = 0
        IW5 = 0
        IW6 = 0
        DO I=1,NSEG_SURF
          DO J1=1,4
            J2=IPERM(J1)
            IF (CLAUSE%SURF_NODES(I,J2) /= 0 .AND.
     .          CLAUSE%SURF_NODES(I,J1) > CLAUSE%SURF_NODES(I,J2)) THEN
              K=K+1
              IW1(K)=CLAUSE%SURF_NODES(I,J2)
              IW2(K)=CLAUSE%SURF_NODES(I,J1)
              IW5(K)=CLAUSE%SURF_ELTYP(I)  
              IW6(K)=CLAUSE%SURF_ELEM(I)
            ELSEIF (CLAUSE%SURF_NODES(I,J1) /= 0 .AND.
     .              CLAUSE%SURF_NODES(I,J1) < CLAUSE%SURF_NODES(I,J2)) THEN
              K=K+1
              IW1(K)=CLAUSE%SURF_NODES(I,J1)
              IW2(K)=CLAUSE%SURF_NODES(I,J2)
              IW5(K)=CLAUSE%SURF_ELTYP(I)
              IW6(K)=CLAUSE%SURF_ELEM(I)
            ENDIF
          ENDDO
        ENDDO ! DO I=1,NSEG_SURF
!
        NSEG_EDGE_EXT = K
C-----------------------------------------------
        INDEX = 0
        IWORK(1:70000) = 0
        CALL MY_ORDERS( 0,IWORK,IW1,INDEX,K,1)
        IWORK(1:70000) = 0
        CALL MY_ORDERS(10,IWORK,IW2,INDEX,K,1)   ! my_order(10 uses previous index
!---
        IF (NSEG_EDGE_EXT > 0) THEN
C-----------------------------------------------
C       REMOVE INTERNAL SEGMENTS (EXCEPT BOUNDARY)
C-----------------------------------------------
          IF (IW1(INDEX(1)) /= IW1(INDEX(2)).OR.
     .        IW2(INDEX(1)) /= IW2(INDEX(2)))THEN
            NSEG=NSEG+1
            LINE_ORD(1,NSEG) = IW1(INDEX(1))
            LINE_ORD(2,NSEG) = IW2(INDEX(1))
            LINE_ORD(3,NSEG) = IW5(INDEX(1))
            LINE_ORD(4,NSEG) = IW6(INDEX(1))
          ENDIF
          DO I=2,K-1
            IF ((IW1(INDEX(I-1)) /= IW1(INDEX(I)).OR.
     .           IW2(INDEX(I-1)) /= IW2(INDEX(I))).AND.
     .          (IW1(INDEX(I+1)) /= IW1(INDEX(I)).OR.
     .           IW2(INDEX(I+1)) /= IW2(INDEX(I)))) THEN
              NSEG=NSEG+1
              LINE_ORD(1,NSEG) = IW1(INDEX(I))
              LINE_ORD(2,NSEG) = IW2(INDEX(I))
              LINE_ORD(3,NSEG) = IW5(INDEX(I))
              LINE_ORD(4,NSEG) = IW6(INDEX(I))
            ENDIF
          ENDDO
          IF (IW1(INDEX(K-1)) /= IW1(INDEX(K)).OR.
     .        IW2(INDEX(K-1)) /= IW2(INDEX(K))) THEN
            NSEG=NSEG+1
            LINE_ORD(1,NSEG) = IW1(INDEX(K))
            LINE_ORD(2,NSEG) = IW2(INDEX(K))
            LINE_ORD(3,NSEG) = IW5(INDEX(K))
            LINE_ORD(4,NSEG) = IW6(INDEX(K))
          ENDIF
        ENDIF ! IF (NSEG_EDGE_EXT > 0)
      ENDIF ! IF (NSEG_SURF > 0)
!------------------------------------------
!------------------------------------------
!     ORDER LINES and LINE CLAUSE FILLING
!------------------------------------------
!------------------------------------------
      INDEX = 0
      IWORK(1:70000) = 0
      DO I=1,NSEG
        INDEX(I)=I
        ITRI(1,I) = LINE_ORD(1,I)
        ITRI(2,I) = LINE_ORD(2,I)
        ITRI(3,I) = LINE_ORD(4,I) ! elem_id
      ENDDO
      CALL MY_ORDERS(0,IWORK,ITRI,INDEX,NSEG,3)
!
      ! reallocation of the line CLAUSE at the right dimension and final fill
!
      IF (ALLOCATED(CLAUSE%LINE_NODES)) DEALLOCATE(CLAUSE%LINE_NODES)
      IF (ALLOCATED(CLAUSE%LINE_ELTYP)) DEALLOCATE(CLAUSE%LINE_ELTYP)
      IF (ALLOCATED(CLAUSE%LINE_ELEM))  DEALLOCATE(CLAUSE%LINE_ELEM)
!
      CLAUSE%NB_LINE_SEG = NSEG
      CALL MY_ALLOC(CLAUSE%LINE_NODES,NSEG,2)
      CALL MY_ALLOC(CLAUSE%LINE_ELTYP,NSEG)
      CALL MY_ALLOC(CLAUSE%LINE_ELEM,NSEG)
!
      DO I=1,NSEG
        CLAUSE%LINE_NODES(I,1) = LINE_ORD(1,INDEX(I))
        CLAUSE%LINE_NODES(I,2) = LINE_ORD(2,INDEX(I))
        CLAUSE%LINE_ELTYP(I)   = LINE_ORD(3,INDEX(I))
        CLAUSE%LINE_ELEM(I)    = LINE_ORD(4,INDEX(I))
      ENDDO
!---
      IF (ALLOCATED(LINE_ORD))  DEALLOCATE(LINE_ORD)
      IF (ALLOCATED(ITRI))  DEALLOCATE(ITRI)
      IF (ALLOCATED(INDEX)) DEALLOCATE(INDEX)
      IF (ALLOCATED(IW1))   DEALLOCATE(IW1)
      IF (ALLOCATED(IW2))   DEALLOCATE(IW2)
      IF (ALLOCATED(IW5))   DEALLOCATE(IW5)
      IF (ALLOCATED(IW6))   DEALLOCATE(IW6)
C-----------
      RETURN
      END
