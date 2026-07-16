Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    create_line_from_surface_ext_all   ../starter/source/model/sets/create_line_from_ext_surface_ext_all.F
!||--- called by ------------------------------------------------------
!||    create_line_from_surface           ../starter/source/model/sets/create_line_from_surface.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    message_mod                        ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE CREATE_LINE_FROM_SURFACE_EXT_ALL( CLAUSE ,IEXT ,DELBUF,GO_IN_ARRAY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
      USE SETDEF_MOD
      USE SET_SCRATCH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      use my_dealloc_mod, only : my_dealloc
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IEXT
      TYPE (SET_) :: CLAUSE
      TYPE (SET_SCRATCH) ::  DELBUF
      LOGICAL GO_IN_ARRAY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,J1,J2,SIZEMAX,NSEG,
     .        NSEG_SURF,LINE_NENTITY,
     .        ELTYP,NSEG_TEMP
      INTEGER IWORK(70000),IPERM(4)
!
      DATA IPERM /2,3,4,1/
!
      INTEGER , DIMENSION(:),ALLOCATABLE :: INDEX,IW1,IW2,IW5,IW6
      INTEGER , DIMENSION(:,:),ALLOCATABLE :: ITRI,LINE_ORD
C=======================================================================
      SIZEMAX = CLAUSE%NB_LINE_SEG + 8*CLAUSE%NB_SURF_SEG
!
      IF (SIZEMAX == 0) RETURN
!
      LINE_NENTITY = 4 ! NOD1, NOD2, ELTYP, ELEM
      call my_alloc(LINE_ORD, LINE_NENTITY, SIZEMAX, "LINE_ORD")
      call my_alloc(ITRI, 3, SIZEMAX, "ITRI")
      call my_alloc(INDEX, 2*SIZEMAX, "INDEX")
!
      call my_alloc(IW1, 4*SIZEMAX, "IW1")
      call my_alloc(IW2, 4*SIZEMAX, "IW2")
      call my_alloc(IW5, 4*SIZEMAX, "IW5")
      call my_alloc(IW6, 4*SIZEMAX, "IW6")
!------------------
      ! NSEG of lines  initialization
      NSEG = 0
!------------------
!
!     extract lines from surfaces
!
!------------------
      NSEG_SURF = CLAUSE%NB_SURF_SEG
      K=0
      IW1 = 0
      IW2 = 0
      IW5 = 0
      IW6 = 0
      IF (NSEG_SURF > 0) THEN
!---
        DO I=1,NSEG_SURF
          ELTYP = CLAUSE%SURF_ELTYP(I)
          IF (ELTYP == 0 .OR. ELTYP == 1 .OR. ELTYP == 2 .OR. 
     .        ELTYP == 3 .OR. ELTYP == 7) THEN
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
          ENDIF
        ENDDO ! DO I=1,NSEG_SURF
C-----------------------------------------------
        INDEX = 0
        IWORK(1:70000) = 0
        CALL MY_ORDERS( 0,IWORK,IW1,INDEX,K,1)
        IWORK(1:70000) = 0
        CALL MY_ORDERS(10,IWORK,IW2,INDEX,K,1)
C-----------------------------------------------
C       KEEP ALL EDGES OF ALL SELECTED SEGMENTS - SHELLS & SOLIDS
C-----------------------------------------------
        NSEG = NSEG + 1
        LINE_ORD(1,NSEG) = IW1(INDEX(1))
        LINE_ORD(2,NSEG) = IW2(INDEX(1))
        LINE_ORD(3,NSEG) = IW5(INDEX(1))
        LINE_ORD(4,NSEG) = IW6(INDEX(1))
        DO I=2,K
          IF (IW1(INDEX(I-1)) /= IW1(INDEX(I)).OR.
     .        IW2(INDEX(I-1)) /= IW2(INDEX(I))) THEN
            NSEG = NSEG + 1
            LINE_ORD(1,NSEG) = IW1(INDEX(I))
            LINE_ORD(2,NSEG) = IW2(INDEX(I))
            LINE_ORD(3,NSEG) = IW5(INDEX(I))
            LINE_ORD(4,NSEG) = IW6(INDEX(I))
          ENDIF
        ENDDO
C-----------------------------------------------
C       TAG ONLY INTERNAL EDGES FOR REMOVAL - SHELLS ONLY
C-----------------------------------------------
        IF (IEXT == 2) THEN !! only for shells
          DO I=2,K-1
            IF (( (IW1(INDEX(I-1)) == IW1(INDEX(I)).AND.IW2(INDEX(I-1)) == IW2(INDEX(I)) ) .AND.
     .            (IW1(INDEX(I+1)) /= IW1(INDEX(I)).OR. IW2(INDEX(I+1)) /= IW2(INDEX(I)))).AND.
     .            ((IW5(INDEX(I-1))==3.OR.IW5(INDEX(I-1))==7).AND.
     .             (IW5(INDEX(I))==3.OR.IW5(INDEX(I))==7).AND.
     .             (IW5(INDEX(I+1))==3.OR.IW5(INDEX(I+1))==7)) ) THEN
              NSEG=NSEG+1
              LINE_ORD(1,NSEG) = IW1(INDEX(I))
              LINE_ORD(2,NSEG) = IW2(INDEX(I))
              LINE_ORD(3,NSEG) = IW5(INDEX(I))
              LINE_ORD(4,NSEG) = IW6(INDEX(I))
            ENDIF
          ENDDO
!
          IF ((IW1(INDEX(K-1)) == IW1(INDEX(K)).AND.
     .        IW2(INDEX(K-1)) == IW2(INDEX(K))).AND.
     .        ((IW5(INDEX(K-1))==3.OR.IW5(INDEX(K-1))==7).AND.
     .         (IW5(INDEX(K))==3.OR.IW5(INDEX(K))==7)) ) THEN
            NSEG=NSEG+1
            LINE_ORD(1,NSEG) = IW1(INDEX(K))
            LINE_ORD(2,NSEG) = IW2(INDEX(K))
            LINE_ORD(3,NSEG) = IW5(INDEX(K))
            LINE_ORD(4,NSEG) = IW6(INDEX(K))
          ENDIF
C-----------------------------------------------
C       ORDER ALL EDGES FOR REMOVING INTERNAL EDGES ONLY
C-----------------------------------------------
          IW1 = 0
          IW2 = 0
          IW5 = 0
          IW6 = 0
          DO I=1,NSEG
             IW1(I)=LINE_ORD(1,I)
             IW2(I)=LINE_ORD(2,I)
             IW5(I)=LINE_ORD(3,I)
             IW6(I)=LINE_ORD(4,I)
          ENDDO
C-----------------------------------------------
          INDEX = 0
          IWORK(1:70000) = 0
          CALL MY_ORDERS( 0,IWORK,IW1,INDEX,NSEG,1)
          IWORK(1:70000) = 0
          CALL MY_ORDERS(10,IWORK,IW2,INDEX,NSEG,1)
C-----------------------------------------------
          LINE_ORD = 0
          NSEG_TEMP = NSEG
          NSEG = 0
C-----------------------------------------------
C       REMOVING INTERNAL EDGES - SHELLS ONLY
C-----------------------------------------------
          IF (IW1(INDEX(1)) /= IW1(INDEX(2)).OR.
     .        IW2(INDEX(1)) /= IW2(INDEX(2)))THEN
            NSEG=NSEG+1
            LINE_ORD(1,NSEG) = IW1(INDEX(1))
            LINE_ORD(2,NSEG) = IW2(INDEX(1))
            LINE_ORD(3,NSEG) = IW5(INDEX(1))
            LINE_ORD(4,NSEG) = IW6(INDEX(1))
          ENDIF
!
          DO I=2,NSEG_TEMP-1
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
!
          IF (IW1(INDEX(K-1)) /= IW1(INDEX(K)).OR.
     .        IW2(INDEX(K-1)) /= IW2(INDEX(K))) THEN
            NSEG=NSEG+1
            LINE_ORD(1,NSEG) = IW1(INDEX(K))
            LINE_ORD(2,NSEG) = IW2(INDEX(K))
            LINE_ORD(3,NSEG) = IW5(INDEX(K))
            LINE_ORD(4,NSEG) = IW6(INDEX(K))
          ENDIF
        ENDIF
      ENDIF ! IF (IEXT == 2)
!------------------
!     add already existing 1D - element lines
!------------------
      IF (GO_IN_ARRAY .EQV. .TRUE.) THEN
        DO I=1+NSEG,NSEG+DELBUF%SZ_LINE
          LINE_ORD(1,I) = DELBUF%LINE(I-NSEG,1)
          LINE_ORD(2,I) = DELBUF%LINE(I-NSEG,2)
          LINE_ORD(3,I) = DELBUF%LINE(I-NSEG,3)
          LINE_ORD(4,I) = DELBUF%LINE(I-NSEG,4)
        ENDDO
        NSEG = NSEG + DELBUF%SZ_LINE  ! update NSEG after adding lines from 1D_element
      ELSE
        DO I=1+NSEG,NSEG+CLAUSE%NB_LINE_SEG
          LINE_ORD(1,I) = CLAUSE%LINE_NODES(I-NSEG,1)
          LINE_ORD(2,I) = CLAUSE%LINE_NODES(I-NSEG,2)
          LINE_ORD(3,I) = CLAUSE%LINE_ELTYP(I-NSEG)
          LINE_ORD(4,I) = CLAUSE%LINE_ELEM(I-NSEG)
        ENDDO
        NSEG = NSEG + CLAUSE%NB_LINE_SEG  ! update NSEG after adding lines from 1D_element
      ENDIF
!------------------
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
!------------------------------------------
! ----------------------------------------------------------------------------
! Decide whether the result is stored in an array or in the clause.
! In certain cases it is useful to store in ARRAY.
! Example : Clause with delete clause. Lines must be recreated & merged...
! ----------------------------------------------------------------------------
      IF (GO_IN_ARRAY .EQV. .TRUE.) THEN
        DELBUF%SZ_LINE = NSEG
        IF(ALLOCATED(DELBUF%LINE)) CALL MY_DEALLOC(DELBUF%LINE)
        call my_alloc(DELBUF%LINE, NSEG, 4, "DELBUF%LINE")
        DO I=1,NSEG
          DELBUF%LINE(I,1) = LINE_ORD(1,INDEX(I))
          DELBUF%LINE(I,2) = LINE_ORD(2,INDEX(I))
          DELBUF%LINE(I,3) = LINE_ORD(3,INDEX(I))
          DELBUF%LINE(I,4) = LINE_ORD(4,INDEX(I))
        ENDDO
      ELSE
        IF (ALLOCATED(CLAUSE%LINE_NODES)) CALL MY_DEALLOC(CLAUSE%LINE_NODES)
        IF (ALLOCATED(CLAUSE%LINE_ELTYP)) CALL MY_DEALLOC(CLAUSE%LINE_ELTYP)
        IF (ALLOCATED(CLAUSE%LINE_ELEM))  CALL MY_DEALLOC(CLAUSE%LINE_ELEM)
!
        CLAUSE%NB_LINE_SEG = NSEG
        CALL MY_ALLOC(CLAUSE%LINE_NODES,NSEG,2,"CLAUSE%LINE_NODES")
        CALL MY_ALLOC(CLAUSE%LINE_ELTYP,NSEG,"CLAUSE%LINE_ELTYP")
        CALL MY_ALLOC(CLAUSE%LINE_ELEM,NSEG,"CLAUSE%LINE_ELEM")
!
        DO I=1,NSEG
          CLAUSE%LINE_NODES(I,1) = LINE_ORD(1,INDEX(I))
          CLAUSE%LINE_NODES(I,2) = LINE_ORD(2,INDEX(I))
          CLAUSE%LINE_ELTYP(I)   = LINE_ORD(3,INDEX(I))
          CLAUSE%LINE_ELEM(I)    = LINE_ORD(4,INDEX(I))
        ENDDO
      ENDIF ! IF (GO_IN_ARRAY .EQV. .TRUE.)
!---
!     DEALLOCATIONS
!---
      IF (ALLOCATED(LINE_ORD))  CALL MY_DEALLOC(LINE_ORD)
      IF (ALLOCATED(ITRI))  CALL MY_DEALLOC(ITRI)
      IF (ALLOCATED(INDEX)) CALL MY_DEALLOC(INDEX)
      IF (ALLOCATED(IW1))   CALL MY_DEALLOC(IW1)
      IF (ALLOCATED(IW2))   CALL MY_DEALLOC(IW2)
      IF (ALLOCATED(IW5))   CALL MY_DEALLOC(IW5)
      IF (ALLOCATED(IW6)) call my_dealloc(IW6)
C-----------
      RETURN
      END
