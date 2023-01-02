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
Chd|  CREATE_LINE_FROM_SURFACE_EXT_ALLsource/model/sets/create_line_from_ext_surface_ext_all.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SET_SCRATCH_MOD               ../common_source/modules/setdef_mod.F
Chd|====================================================================
      SUBROUTINE CREATE_LINE_FROM_SURFACE_EXT_ALL( CLAUSE ,IEXT ,OPT_E,DELBUF,GO_IN_ARRAY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SETDEF_MOD
      USE SET_SCRATCH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IEXT,OPT_E
      TYPE (SET_) :: CLAUSE
      TYPE (SET_SCRATCH) ::  DELBUF
      LOGICAL GO_IN_ARRAY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,J1,J2,SIZEMAX,NSEG,NSEG_EDGE_EXT,
     .        NSEG_EDGE_ALL,NSEG_SURF,LINE_NENTITY,
     .        NSEG_EXT,NSEG_ALL,ELTYP
      INTEGER IWORK(70000),IPERM(4)
!
      DATA IPERM /2,3,4,1/
!
      INTEGER , DIMENSION(:),ALLOCATABLE :: INDEX,IW1,IW2,IW5,IW6
      INTEGER , DIMENSION(:,:),ALLOCATABLE :: ITRI,LINE_ORD
      INTEGER , DIMENSION(:,:),ALLOCATABLE :: SURF_NODE_EXT,SURF_NODE_ALL
      INTEGER , DIMENSION(:),ALLOCATABLE :: SURF_ELTYP_EXT,SURF_ELEM_EXT,
     . SURF_ELTYP_ALL,SURF_ELEM_ALL
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
!
      ALLOCATE(SURF_NODE_EXT(LINE_NENTITY,CLAUSE%NB_SURF_SEG))
      ALLOCATE(SURF_ELTYP_EXT(CLAUSE%NB_SURF_SEG))
      ALLOCATE(SURF_ELEM_EXT(CLAUSE%NB_SURF_SEG))

      ALLOCATE(SURF_NODE_ALL(LINE_NENTITY,CLAUSE%NB_SURF_SEG))
      ALLOCATE(SURF_ELTYP_ALL(CLAUSE%NB_SURF_SEG))
      ALLOCATE(SURF_ELEM_ALL(CLAUSE%NB_SURF_SEG))
!------------------
      ! NSEG of lines  initialization
      NSEG = 0
      IF (GO_IN_ARRAY .EQV. .TRUE.) THEN
        NSEG = DELBUF%SZ_LINE
        ! Fill in LINE_ORD array the already stored line from 1D_element
        DO I=1,NSEG
          LINE_ORD(1,I) = DELBUF%LINE(I,1)
          LINE_ORD(2,I) = DELBUF%LINE(I,2)
          LINE_ORD(3,I) = DELBUF%LINE(I,3)
          LINE_ORD(4,I) = DELBUF%LINE(I,4)
        ENDDO
      ELSE
        NSEG = CLAUSE%NB_LINE_SEG  ! lines from 1D_element
        ! Fill in LINE_ORD array the already stored line from 1D_element
        DO I=1,NSEG
          LINE_ORD(1,I) = CLAUSE%LINE_NODES(I,1)
          LINE_ORD(2,I) = CLAUSE%LINE_NODES(I,2)
          LINE_ORD(3,I) = CLAUSE%LINE_ELTYP(I)
          LINE_ORD(4,I) = CLAUSE%LINE_ELEM(I)
        ENDDO
      ENDIF
!
!     extract lines from surfaces
!
      NSEG_SURF = CLAUSE%NB_SURF_SEG
      IF (NSEG_SURF > 0) THEN


        !
        !  detach solid and shell surfaces from surfaces of all parts
        !

        NSEG_EXT = 0
        NSEG_ALL = 0
        DO I=1,NSEG_SURF

          ELTYP = CLAUSE%SURF_ELTYP(I)

          IF (ELTYP == 1. OR. ELTYP == 2 .OR. ELTYP == 0) THEN
          ! solid + quad + SEG

            ! tag all edges of surf segment
            NSEG_ALL = NSEG_ALL + 1
            SURF_NODE_ALL(1,NSEG_ALL) = CLAUSE%SURF_NODES(I,1)
            SURF_NODE_ALL(2,NSEG_ALL) = CLAUSE%SURF_NODES(I,2)
            SURF_NODE_ALL(3,NSEG_ALL) = CLAUSE%SURF_NODES(I,3)
            SURF_NODE_ALL(4,NSEG_ALL) = CLAUSE%SURF_NODES(I,4)
            SURF_ELTYP_ALL(NSEG_ALL)  = CLAUSE%SURF_ELTYP(I)
            SURF_ELEM_ALL(NSEG_ALL)   = CLAUSE%SURF_ELEM(I)

          ELSEIF (ELTYP == 3 . OR. ELTYP == 7) THEN  ! shell + sh3n

            IF ( IEXT == 1 .AND. OPT_E == 1 ) THEN

              ! tag only free edges of surf segment
              NSEG_EXT = NSEG_EXT + 1
              SURF_NODE_EXT(1,NSEG_EXT)  = CLAUSE%SURF_NODES(I,1)
              SURF_NODE_EXT(2,NSEG_EXT)  = CLAUSE%SURF_NODES(I,2)
              SURF_NODE_EXT(3,NSEG_EXT)  = CLAUSE%SURF_NODES(I,3)
              SURF_NODE_EXT(4,NSEG_EXT)  = CLAUSE%SURF_NODES(I,4)
              SURF_ELTYP_EXT(NSEG_EXT)   = CLAUSE%SURF_ELTYP(I)
              SURF_ELEM_EXT(NSEG_EXT)    = CLAUSE%SURF_ELEM(I)

            ELSE

              ! tag all edges of surf segment
              NSEG_ALL = NSEG_ALL + 1
              SURF_NODE_ALL(1,NSEG_ALL) = CLAUSE%SURF_NODES(I,1)
              SURF_NODE_ALL(2,NSEG_ALL) = CLAUSE%SURF_NODES(I,2)
              SURF_NODE_ALL(3,NSEG_ALL) = CLAUSE%SURF_NODES(I,3)
              SURF_NODE_ALL(4,NSEG_ALL) = CLAUSE%SURF_NODES(I,4)
              SURF_ELTYP_ALL(NSEG_ALL)  = CLAUSE%SURF_ELTYP(I)
              SURF_ELEM_ALL(NSEG_ALL)   = CLAUSE%SURF_ELEM(I)

            ENDIF ! IF (IEXT == 1 .AND. OPT_E == 1 )

          ENDIF ! IF (ELTYP == 1. OR. ELTYP == 2 .OR. ELTYP == 0)

        ENDDO ! DO I=1,NSEG_SURF



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

        DO I=1,NSEG_EXT
          DO J1=1,4
            J2=IPERM(J1)
            IF (SURF_NODE_EXT(J2,I) /= 0 .AND.
     .          SURF_NODE_EXT(J1,I) > SURF_NODE_EXT(J2,I)) THEN
              K=K+1
              IW1(K)=SURF_NODE_EXT(J2,I)
              IW2(K)=SURF_NODE_EXT(J1,I)
              IW5(K)=SURF_ELTYP_EXT(I)  
              IW6(K)=SURF_ELEM_EXT(I)
            ELSEIF (SURF_NODE_EXT(J1,I) /= 0 .AND.
     .              SURF_NODE_EXT(J1,I) < SURF_NODE_EXT(J2,I)) THEN
              K=K+1
              IW1(K)=SURF_NODE_EXT(J1,I)
              IW2(K)=SURF_NODE_EXT(J2,I)
              IW5(K)=SURF_ELTYP_EXT(I)
              IW6(K)=SURF_ELEM_EXT(I)
            ENDIF
          ENDDO
        ENDDO ! DO I=1,NSEG_EXT
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


!***********************************
!***********************************
      ! -- ALL -- edges form SURFACE
!***********************************
!***********************************

        NSEG_EDGE_ALL = 0
!---
        K=0
        IW1 = 0
        IW2 = 0
        IW5 = 0
        IW6 = 0
        DO I=1,NSEG_ALL
          DO J1=1,4
            J2=IPERM(J1)
            IF (SURF_NODE_ALL(J2,I) /= 0 .AND.
     .          SURF_NODE_ALL(J1,I) > SURF_NODE_ALL(J2,I)) THEN
              K=K+1
              IW1(K)=SURF_NODE_ALL(J2,I)
              IW2(K)=SURF_NODE_ALL(J1,I)
              IW5(K)=SURF_ELTYP_ALL(I)
              IW6(K)=SURF_ELEM_ALL(I)
            ELSEIF (SURF_NODE_ALL(J1,I) /= 0 .AND.
     .              SURF_NODE_ALL(J1,I) < SURF_NODE_ALL(J2,I)) THEN
              K=K+1
              IW1(K)=SURF_NODE_ALL(J1,I)
              IW2(K)=SURF_NODE_ALL(J2,I)
              IW5(K)=SURF_ELTYP_ALL(I)
              IW6(K)=SURF_ELEM_ALL(I)
            ENDIF
          ENDDO
        ENDDO ! DO I=1,NSEG_ALL
!
        NSEG_EDGE_ALL = K
C-----------------------------------------------
        INDEX = 0
        IWORK(1:70000) = 0
        CALL MY_ORDERS( 0,IWORK,IW1,INDEX,K,1)
        IWORK(1:70000) = 0
        CALL MY_ORDERS(10,IWORK,IW2,INDEX,K,1)
!---
        IF (NSEG_EDGE_ALL > 0) THEN
C-----------------------------------------------
C         REMOVE DOUBLE SEGMENTS
C-----------------------------------------------
          NSEG = NSEG + 1
          LINE_ORD(1,NSEG) = IW1(INDEX(1))
          LINE_ORD(2,NSEG) = IW2(INDEX(1))
          LINE_ORD(3,NSEG) = IW5(INDEX(1))
          LINE_ORD(4,NSEG) = IW6(INDEX(1))
          DO I=2,K
            IF (IW1(INDEX(I-1)) /= IW1(INDEX(I)).OR.
     .          IW2(INDEX(I-1)) /= IW2(INDEX(I))) THEN
              NSEG = NSEG + 1
              LINE_ORD(1,NSEG) = IW1(INDEX(I))
              LINE_ORD(2,NSEG) = IW2(INDEX(I))
              LINE_ORD(3,NSEG) = IW5(INDEX(I))
              LINE_ORD(4,NSEG) = IW6(INDEX(I))
            ENDIF
          ENDDO
        ENDIF ! IF (NSEG_EDGE_ALL > 0)

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
!------------------
!
! Decide whether the result is stored in an array or in the clause.
! In certain cases it is useful to store in ARRAY.
! Example : Clause with delete clause. Lines must be recreated & merged...
! ----------------------------------------------------------------------------
      IF (GO_IN_ARRAY .EQV. .TRUE.) THEN
        DELBUF%SZ_LINE = NSEG
        IF(ALLOCATED(DELBUF%LINE)) DEALLOCATE(DELBUF%LINE)                                    
        ALLOCATE(DELBUF%LINE(NSEG,4))
        DO I=1,NSEG
          DELBUF%LINE(I,1) = LINE_ORD(1,INDEX(I))
          DELBUF%LINE(I,2) = LINE_ORD(2,INDEX(I))
          DELBUF%LINE(I,3) = LINE_ORD(3,INDEX(I))
          DELBUF%LINE(I,4) = LINE_ORD(4,INDEX(I))
        ENDDO
      ELSE
        IF (ALLOCATED(CLAUSE%LINE_NODES)) DEALLOCATE(CLAUSE%LINE_NODES)
        IF (ALLOCATED(CLAUSE%LINE_ELTYP)) DEALLOCATE(CLAUSE%LINE_ELTYP)
        IF (ALLOCATED(CLAUSE%LINE_ELEM))  DEALLOCATE(CLAUSE%LINE_ELEM)
!
        CLAUSE%NB_LINE_SEG = NSEG
        MY_ALLOCATE2D(CLAUSE%LINE_NODES,NSEG,2)
        MY_ALLOCATE(CLAUSE%LINE_ELTYP,NSEG)
        MY_ALLOCATE(CLAUSE%LINE_ELEM,NSEG)
!
        DO I=1,NSEG
          CLAUSE%LINE_NODES(I,1) = LINE_ORD(1,INDEX(I))
          CLAUSE%LINE_NODES(I,2) = LINE_ORD(2,INDEX(I))
          CLAUSE%LINE_ELTYP(I)   = LINE_ORD(3,INDEX(I))
          CLAUSE%LINE_ELEM(I)    = LINE_ORD(4,INDEX(I))
        ENDDO
      ENDIF ! IF (GO_IN_ARRAY .EQV. .TRUE.)
!---
      IF (ALLOCATED(LINE_ORD))  DEALLOCATE(LINE_ORD)
      IF (ALLOCATED(ITRI))  DEALLOCATE(ITRI)
      IF (ALLOCATED(INDEX)) DEALLOCATE(INDEX)
      IF (ALLOCATED(IW1))   DEALLOCATE(IW1)
      IF (ALLOCATED(IW2))   DEALLOCATE(IW2)
      IF (ALLOCATED(IW5))   DEALLOCATE(IW5)
      IF (ALLOCATED(IW6))   DEALLOCATE(IW6)
!
      IF (ALLOCATED(SURF_NODE_EXT))   DEALLOCATE(SURF_NODE_EXT)
      IF (ALLOCATED(SURF_ELTYP_EXT))  DEALLOCATE(SURF_ELTYP_EXT)
      IF (ALLOCATED(SURF_ELEM_EXT))   DEALLOCATE(SURF_ELEM_EXT)
      IF (ALLOCATED(SURF_NODE_ALL))   DEALLOCATE(SURF_NODE_ALL)
      IF (ALLOCATED(SURF_ELTYP_ALL))  DEALLOCATE(SURF_ELTYP_ALL)
      IF (ALLOCATED(SURF_ELEM_ALL))   DEALLOCATE(SURF_ELEM_ALL)
C-----------
      RETURN
      END
