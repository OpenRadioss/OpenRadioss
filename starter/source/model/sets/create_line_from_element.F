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
Chd|====================================================================
Chd|  CREATE_LINE_FROM_ELEMENT      source/model/sets/create_line_from_element.F
Chd|-- called by -----------
Chd|        CREATE_BOX_CLAUSE             source/model/sets/create_box_clause.F
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|        INSERT_CLAUSE_IN_SET          source/model/sets/insert_clause_in_set.F
Chd|-- calls ---------------
Chd|        LINE_BUFFER                   source/model/sets/line_buffer.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SET_SCRATCH_MOD               ../common_source/modules/setdef_mod.F
Chd|====================================================================
      SUBROUTINE CREATE_LINE_FROM_ELEMENT(IXT     ,IXP  ,IXR  ,CLAUSE ,DELBUF,
     .                                    GO_IN_ARRAY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SETDEF_MOD
      USE MESSAGE_MOD
      USE SET_SCRATCH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*)
!
      TYPE (SET_) :: CLAUSE
      TYPE (SET_SCRATCH) ::  DELBUF
      LOGICAL GO_IN_ARRAY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IEXT,I,NIX,SZELMAX,IAD_LINE,NSEG,LINE_NENTITY
      INTEGER IWORK(70000)
!
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: ITRI
      INTEGER, ALLOCATABLE, DIMENSION(:) :: INDEX, BUFTMPLINE
C=======================================================================
      DELBUF%SZ_LINE = 0
!
!!      SZELMAX = MAX(CLAUSE%NB_TRUSS,CLAUSE%NB_BEAM,CLAUSE%NB_SPRING)
      SZELMAX = CLAUSE%NB_TRUSS + CLAUSE%NB_BEAM + CLAUSE%NB_SPRING
      IF (SZELMAX == 0) RETURN
!
      LINE_NENTITY = 4 ! NOD1, NOD2, ELTYP, ELEM
      ALLOCATE(BUFTMPLINE(SZELMAX*LINE_NENTITY))
      ALLOCATE(ITRI(3,SZELMAX))
      ALLOCATE(INDEX(2*SZELMAX))
!
      IAD_LINE = 1
!------------------
!----
!     ! temporary line segments got from elements
      NSEG = 0
      CALL LINE_BUFFER(
     .             IXT      ,IXP    ,IXR  ,BUFTMPLINE ,  NSEG,
     .             IAD_LINE ,CLAUSE )
!----
!
      NIX = LINE_NENTITY
!
      DO  I=1,NSEG
        INDEX(I)=I
        ITRI(1,I) = BUFTMPLINE((I-1)*NIX+1)
        ITRI(2,I) = BUFTMPLINE((I-1)*NIX+2)
        ITRI(3,I) = BUFTMPLINE((I-1)*NIX+4)
      ENDDO
      IWORK(1:70000) = 0
      CALL MY_ORDERS(0,IWORK,ITRI,INDEX,NSEG,3)
!---
!     clause surf allocation
!---
!------------------
!
! Decide whether the result is stored in an array or in the clause.
! In certain cases it is useful to store in ARRAY.
! Example : Clause with delete clause. Lines must be recreated & merged...
! ----------------------------------------------------------------------------
      IF (GO_IN_ARRAY .EQV. .TRUE.) THEN
        DELBUF%SZ_LINE = NSEG
        ALLOCATE(DELBUF%LINE(NSEG,4))
        DO I=1,NSEG
          DELBUF%LINE(I,1) = BUFTMPLINE((INDEX(I)-1)*NIX+1)
          DELBUF%LINE(I,2) = BUFTMPLINE((INDEX(I)-1)*NIX+2)
          DELBUF%LINE(I,3) = BUFTMPLINE((INDEX(I)-1)*NIX+3)
          DELBUF%LINE(I,4) = BUFTMPLINE((INDEX(I)-1)*NIX+4)
        ENDDO
      ELSE
        IF(ALLOCATED( CLAUSE%LINE_NODES )) DEALLOCATE( CLAUSE%LINE_NODES )
        IF(ALLOCATED( CLAUSE%LINE_ELTYP )) DEALLOCATE( CLAUSE%LINE_ELTYP )
        IF(ALLOCATED( CLAUSE%LINE_ELEM ))  DEALLOCATE( CLAUSE%LINE_ELEM )
!
        CLAUSE%NB_LINE_SEG = NSEG
        MY_ALLOCATE2D(CLAUSE%LINE_NODES,NSEG,2)
        MY_ALLOCATE(CLAUSE%LINE_ELTYP,NSEG)
        MY_ALLOCATE(CLAUSE%LINE_ELEM,NSEG)
!
        DO I=1,NSEG
          CLAUSE%LINE_NODES(I,1) = BUFTMPLINE((INDEX(I)-1)*NIX+1)
          CLAUSE%LINE_NODES(I,2) = BUFTMPLINE((INDEX(I)-1)*NIX+2)
          CLAUSE%LINE_ELTYP(I)   = BUFTMPLINE((INDEX(I)-1)*NIX+3)
          CLAUSE%LINE_ELEM(I)    = BUFTMPLINE((INDEX(I)-1)*NIX+4)
        ENDDO
      ENDIF ! IF (GO_IN_ARRAY .EQV. .TRUE.)
!------------------
      IF(ALLOCATED(ITRI))       DEALLOCATE(ITRI)
      IF(ALLOCATED(INDEX))      DEALLOCATE(INDEX)
      IF(ALLOCATED(BUFTMPLINE)) DEALLOCATE(BUFTMPLINE)
!------------------
!-----------
      RETURN
      END
