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
Chd|  CREATE_SURFACE_FROM_ELEMENT   source/model/sets/create_surface_from_element.F
Chd|-- called by -----------
Chd|        CREATE_BOX_CLAUSE             source/model/sets/create_box_clause.F
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|        INSERT_CLAUSE_IN_SET          source/model/sets/insert_clause_in_set.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        SURFACE_BUFFER                source/model/sets/surface_buffer.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SET_SCRATCH_MOD               ../common_source/modules/setdef_mod.F
Chd|====================================================================
      SUBROUTINE CREATE_SURFACE_FROM_ELEMENT(
     .                       IXS       ,IXS10    ,SH4TREE   ,SH3TREE   ,IXC       ,
     .                       IXTG      ,KNOD2ELS ,NOD2ELS   ,KNOD2ELC  ,NOD2ELC   ,
     .                       KNOD2ELTG ,NOD2ELTG ,IPARTC    ,IPARTG    ,IPARTS    ,
     .                       IPART     ,CLAUSE   ,OPT_A     ,OPT_O     ,IXQ       ,
     .                       KNOD2ELQ  ,NOD2ELQ  ,X         ,KEYSET    ,DELBUF    ,
     .                       GO_IN_ARRAY)
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
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  OPT_A,OPT_O
      INTEGER, INTENT(IN) :: IXS(NIXS,*),IXS10(6,*),IXC(NIXC,*),IXTG(NIXTG,*),
     .  KNOD2ELS(*),KNOD2ELC(*),KNOD2ELTG(*),NOD2ELS(*),NOD2ELC(*),
     .  NOD2ELTG(*),IPARTS(*),IPARTC(*),IPARTG(*),SH4TREE(*),SH3TREE(*),
     .  IPART(LIPART1,*),KNOD2ELQ(*),NOD2ELQ(*),IXQ(NIXQ,*)
      my_real
     .  X(3,*)
      CHARACTER KEYSET*ncharfield
!
      TYPE (SET_) :: CLAUSE
      TYPE (SET_SCRATCH) ::  DELBUF
      LOGICAL GO_IN_ARRAY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IEXT,J,L,NIX,SZELMAX,IAD_SURF,NSEG,NFACES
      INTEGER IWORK(70000)
!
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: ITRI
      INTEGER, ALLOCATABLE, DIMENSION(:) :: INDEX, BUFTMPSURF
C=======================================================================
      DELBUF%SZ_SURF = 0
      DELBUF%SZ_LINE = 0
!
!!      SZELMAX = MAX(CLAUSE%NB_SOLID,CLAUSE%NB_QUAD,CLAUSE%NB_SH4N,CLAUSE%NB_SH3N)
      SZELMAX = CLAUSE%NB_SOLID + CLAUSE%NB_QUAD + CLAUSE%NB_SH4N + CLAUSE%NB_SH3N
      IF (SZELMAX == 0) RETURN
!
      NFACES = 6  ! HEXA_8
      IF (NUMELS > NUMELS8) NFACES = 16 ! TETRA_10
!
      ALLOCATE(BUFTMPSURF(SZELMAX*NFACES*6))
      ALLOCATE(ITRI(5,SZELMAX*NFACES))
      ALLOCATE(INDEX(2*SZELMAX*NFACES))
!
      IAD_SURF = 1
!------------------
      IEXT = 1  ! par default (external surface)
      IF ( OPT_A == 1 )  IEXT = 2 ! all surfaces (internal + external)
!------------------

!
!----
!     ! temporary surface segments got from elements
      NSEG = 0
      CALL SURFACE_BUFFER(
     .             IXS       ,IXS10    ,IXC       ,SH4TREE   ,SH3TREE   ,
     .             IXTG      ,KNOD2ELS ,NOD2ELS   ,KNOD2ELC  ,NOD2ELC   ,
     .             KNOD2ELTG ,NOD2ELTG ,NSEG      ,IEXT      ,IPARTC    ,
     .             IPARTS    ,IPARTG   ,CLAUSE    ,BUFTMPSURF,IAD_SURF  ,
     .             OPT_O     ,IPART    ,KNOD2ELQ  ,NOD2ELQ   ,X         ,
     .             IXQ       ,KEYSET)
!----
!
      NIX = 6
!
      DO  L=1,NSEG
        INDEX(L)=L
        ITRI(1,L) = BUFTMPSURF((L-1)*NIX+1)
        ITRI(2,L) = BUFTMPSURF((L-1)*NIX+2)
        ITRI(3,L) = BUFTMPSURF((L-1)*NIX+3)
        ITRI(4,L) = BUFTMPSURF((L-1)*NIX+4)
        ITRI(5,L) = BUFTMPSURF((L-1)*NIX+6)
      ENDDO
      IWORK(1:70000) = 0
      CALL MY_ORDERS(0,IWORK,ITRI,INDEX,NSEG,5)
!---
!     clause surf allocation
!---
!------------------
!
! Decide wheter the result is stored in an array or in the clause.
! In certain cases it is usfull to store in ARRAY.
! Example : Clause with delete clause. Surfaces must be recreated & merged...
! ----------------------------------------------------------------------------
      IF (GO_IN_ARRAY .EQV. .TRUE.) THEN
        DELBUF%SZ_SURF = NSEG
        ALLOCATE(DELBUF%SURF(NSEG,6))
        DO L=1,NSEG
          DELBUF%SURF(L,1) = BUFTMPSURF((INDEX(L)-1)*NIX+1)
          DELBUF%SURF(L,2) = BUFTMPSURF((INDEX(L)-1)*NIX+2)
          DELBUF%SURF(L,3) = BUFTMPSURF((INDEX(L)-1)*NIX+3)
          DELBUF%SURF(L,4) = BUFTMPSURF((INDEX(L)-1)*NIX+4)
          DELBUF%SURF(L,5) = BUFTMPSURF((INDEX(L)-1)*NIX+5)
          DELBUF%SURF(L,6) = BUFTMPSURF((INDEX(L)-1)*NIX+6)
        ENDDO
      ELSE
        IF (ALLOCATED(CLAUSE%SURF_NODES)) DEALLOCATE(CLAUSE%SURF_NODES)
        IF (ALLOCATED(CLAUSE%SURF_ELTYP)) DEALLOCATE(CLAUSE%SURF_ELTYP)
        IF (ALLOCATED(CLAUSE%SURF_ELEM))  DEALLOCATE(CLAUSE%SURF_ELEM)
!
        CLAUSE%NB_SURF_SEG = NSEG
        MY_ALLOCATE2D(CLAUSE%SURF_NODES,NSEG,4)
        MY_ALLOCATE(CLAUSE%SURF_ELTYP,NSEG)
        MY_ALLOCATE(CLAUSE%SURF_ELEM,NSEG)
!
        DO L=1,NSEG
          CLAUSE%SURF_NODES(L,1) = BUFTMPSURF((INDEX(L)-1)*NIX+1)
          CLAUSE%SURF_NODES(L,2) = BUFTMPSURF((INDEX(L)-1)*NIX+2)
          CLAUSE%SURF_NODES(L,3) = BUFTMPSURF((INDEX(L)-1)*NIX+3)
          CLAUSE%SURF_NODES(L,4) = BUFTMPSURF((INDEX(L)-1)*NIX+4)
          CLAUSE%SURF_ELTYP(L)   = BUFTMPSURF((INDEX(L)-1)*NIX+5)
          CLAUSE%SURF_ELEM(L)    = BUFTMPSURF((INDEX(L)-1)*NIX+6)
        ENDDO
      ENDIF ! IF (GO_IN_ARRAY .EQV. .TRUE.)
!------------------
      IF(ALLOCATED(ITRI))       DEALLOCATE(ITRI)
      IF(ALLOCATED(INDEX))      DEALLOCATE(INDEX)
      IF(ALLOCATED(BUFTMPSURF)) DEALLOCATE(BUFTMPSURF)
C-----------
!-----------
      RETURN
      END
