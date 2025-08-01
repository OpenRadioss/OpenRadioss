Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    create_node_from_element        ../starter/source/model/sets/create_node_from_element.F
!||--- called by ------------------------------------------------------
!||    hm_set                          ../starter/source/model/sets/hm_set.F
!||--- calls      -----------------------------------------------------
!||    tag_node_from_1d_2d_elem        ../starter/source/model/sets/tag_node_from_1D_2D_elem.F
!||    tag_node_from_part_sphcel       ../starter/source/model/sets/tag_node_from_part_sphcel.F90
!||    tag_node_from_solid             ../starter/source/model/sets/tag_node_from_solid.F
!||    tag_node_from_spring            ../starter/source/model/sets/tag_node_from_spring.F
!||--- uses       -----------------------------------------------------
!||    message_mod                     ../starter/share/message_module/message_mod.F
!||    tag_node_from_part_sphcel_mod   ../starter/source/model/sets/tag_node_from_part_sphcel.F90
!||====================================================================
      SUBROUTINE CREATE_NODE_FROM_ELEMENT(
     .                            IXS   ,IXS10  ,IXS20  ,IXS16  ,IXQ    ,
     .                            IXC   ,IXTG   ,IXT    ,IXP    ,IXR    ,
     .                            IXX   ,KXX    ,KXSP   ,CLAUSE ,GEO,
     .                            ARRAY ,SZ     ,GO_IN_ARRAY    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SETDEF_MOD
      USE TAG_NODE_FROM_PART_SPHCEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IXS10(6,*),IXS16(8,*),IXS20(12,*),
     .        IXQ(NIXQ,*),IXC(NIXC,*),IXTG(NIXTG,*),IXT(NIXT,*),
     .        IXP(NIXP,*),IXR(NIXR,*),IXX(*),KXX(*),KXSP(NISP,*)
      my_real
     .        GEO(NPROPG,*)
!
      TYPE (SET_) ::  CLAUSE
      INTEGER ARRAY(*),SZ
      LOGICAL GO_IN_ARRAY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IND,LIMIT
      INTEGER, ALLOCATABLE, DIMENSION(:) :: TAGNOD,CLAUSE_NODE
      INTEGER IWORK(70000)
      INTEGER, DIMENSION(:),ALLOCATABLE:: IDX,SORT
C=======================================================================
!
      ALLOCATE(TAGNOD(NUMNOD))
      TAGNOD(:) = 0
      ALLOCATE(CLAUSE_NODE(NUMNOD))

      IND=0
!
      ! SOLID
      IF ( CLAUSE%NB_SOLID > 0 )
     . CALL TAG_NODE_FROM_SOLID(
     .                         IXS   ,IXS10  ,IXS20  ,IXS16   ,CLAUSE%NB_SOLID ,
     .                         CLAUSE%SOLID  ,TAGNOD ,CLAUSE_NODE,IND )
      ! QUAD
      IF ( CLAUSE%NB_QUAD > 0 )
     . CALL TAG_NODE_FROM_1D_2D_ELEM(
     .                         IXQ         ,NIXQ   ,2     ,5    ,CLAUSE%NB_QUAD,
     .                         CLAUSE%QUAD ,TAGNOD,CLAUSE_NODE,IND )
      ! SH4N
      IF ( CLAUSE%NB_SH4N > 0 )
     . CALL TAG_NODE_FROM_1D_2D_ELEM(
     .                         IXC         ,NIXC   ,2     ,5    ,CLAUSE%NB_SH4N,
     .                         CLAUSE%SH4N ,TAGNOD,CLAUSE_NODE,IND  )
      ! SH3N
      IF ( CLAUSE%NB_SH3N > 0 .AND. CLAUSE%NB_TRIA == 0 )
     . CALL TAG_NODE_FROM_1D_2D_ELEM(
     .                         IXTG        ,NIXTG  ,2     ,4    ,CLAUSE%NB_SH3N,
     .                         CLAUSE%SH3N ,TAGNOD,CLAUSE_NODE,IND  )
      ! TRIA
      IF ( CLAUSE%NB_TRIA > 0 )
     . CALL TAG_NODE_FROM_1D_2D_ELEM(
     .                         IXTG        ,NIXTG  ,2     ,4    ,CLAUSE%NB_TRIA,
     .                         CLAUSE%TRIA ,TAGNOD,CLAUSE_NODE,IND  )
      ! TRUSS
      IF ( CLAUSE%NB_TRUSS > 0 )
     . CALL TAG_NODE_FROM_1D_2D_ELEM(
     .                         IXT         ,NIXT   ,2     ,3   ,CLAUSE%NB_TRUSS,
     .                         CLAUSE%TRUSS,TAGNOD,CLAUSE_NODE,IND  )
      ! BEAM
      IF ( CLAUSE%NB_BEAM > 0 )
     . CALL TAG_NODE_FROM_1D_2D_ELEM(
     .                         IXP         ,NIXP   ,2     ,3   ,CLAUSE%NB_BEAM,
     .                         CLAUSE%BEAM ,TAGNOD,CLAUSE_NODE,IND  )
      ! SPRING
      IF ( CLAUSE%NB_SPRING > 0 )
     . CALL TAG_NODE_FROM_SPRING(
     .                   IXR   ,GEO  ,CLAUSE%NB_SPRING  ,CLAUSE%SPRING  ,TAGNOD,CLAUSE_NODE,IND)
!
!
!   ATTENTION --- PARTS SPH are not inverted
!
!
      ! SPH
      IF ( CLAUSE%NB_SPHCEL > 0 )
     . CALL TAG_NODE_FROM_PART_SPHCEL(
     .                   CLAUSE%NB_SPHCEL  ,CLAUSE%SPHCEL  ,TAGNOD,CLAUSE_NODE,IND,NUMNOD)
!      IF (NUMSPH > 0) 
!     .    CALL TAGNOD_PART(KXSP,NISP,3,3,NUMSPH,IPARTSP,TAGPART,TAGNOD)
C-----------
      LIMIT = NUMNOD/2
      IF (IND < LIMIT)THEN                                  ! cheaper to use Order on small node groups
        ALLOCATE(IDX(2*IND))
        ALLOCATE(SORT(IND))
        SORT(1:IND) = CLAUSE_NODE(1:IND)
        CALL MY_ORDERS(0,IWORK,SORT,IDX,IND,1)

        DO I=1,IND
          CLAUSE_NODE(I) = SORT(IDX(I))
        ENDDO
        DEALLOCATE(IDX)
        DEALLOCATE(SORT)
      ELSE
       IND = 0
       DO I=1,NUMNOD
         IF (TAGNOD(I) == 1) THEN
           IND = IND + 1
           CLAUSE_NODE(IND) = I
         ENDIF
       ENDDO
      ENDIF
!
! Decide whether the result is stored in an array or in the clause.
! In certain cases it is useful to store in ARRAY.
! Example : Clause with delete clause. Nodes must be recreated & merged...
! ----------------------------------------------------------------------------
       IF (GO_IN_ARRAY .EQV. .TRUE.) THEN
        SZ = IND
        ARRAY(1:IND) = CLAUSE_NODE(1:IND)
       ELSE
        ! clause node allocation
        SZ=0
        CLAUSE%NB_NODE = IND
        IF(ALLOCATED( CLAUSE%NODE )) DEALLOCATE( CLAUSE%NODE )
        ALLOCATE( CLAUSE%NODE(IND) )
        CLAUSE%NODE(1:IND) = CLAUSE_NODE(1:IND)
      ENDIF
C-----------
      DEALLOCATE(TAGNOD)
      DEALLOCATE(CLAUSE_NODE)
C-----------
      RETURN
      END
