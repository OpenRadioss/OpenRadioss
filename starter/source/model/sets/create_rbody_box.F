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
      !||====================================================================
      !||    create_rbody_box         ../starter/source/model/sets/create_rbody_box.F
      !||--- called by ------------------------------------------------------
      !||    create_box_clause        ../starter/source/model/sets/create_box_clause.F
      !||    create_rbody_clause      ../starter/source/model/sets/create_rbody_clause.F
      !||--- calls      -----------------------------------------------------
      !||    fill_clause_rbody_box    ../starter/source/model/sets/fill_clause_rbody_box.F
      !||    hm_get_int_array_index   ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
      !||    read_clause_box          ../starter/source/model/sets/read_clause_box.F
      !||--- uses       -----------------------------------------------------
      !||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
      !||    message_mod              ../starter/share/message_module/message_mod.F
      !||    submodel_mod             ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE CREATE_RBODY_BOX(
     .               CLAUSE  ,IRBODYM ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL,
     .               IBOX    ,X       ,SKEW    ,SET_TITLE    ,KEYSET   ,
     .               RBY_MSN )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Create NODE Clause from BOX
C------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     CLAUSE        (SET structure) Clause to be treated
C     IRBODYM       MAP Table UID -> LocalID
C     JCLAUSE       parameter with HM_READER (current clause read)
C     IS_AVAILABLE  Bool / Result of HM_interface
C     LSUBMODEL     SUBMODEL Structure.
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SETDEF_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE HM_OPTION_READ_MOD
      USE OPTIONDEF_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARFIELD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JCLAUSE
      LOGICAL :: IS_AVAILABLE
      INTEGER, INTENT(IN), DIMENSION(NRBODY,2) :: IRBODYM
      INTEGER, INTENT(IN), DIMENSION(2,NRBODY) :: RBY_MSN
      my_real X(3,*),SKEW(LSKEW,*)
      TYPE (SET_) ::  CLAUSE
      TYPE(SUBMODEL_DATA),INTENT(IN):: LSUBMODEL(*)!
      TYPE (BOX_)  , DIMENSION(NBBOX)  :: IBOX
      CHARACTER(LEN=NCHARFIELD) :: KEYSET
      CHARACTER(LEN=NCHARTITLE) :: SET_TITLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IDS,NINDX,LIST_SIZE,NODSYS,NINDX_B,NOD
      INTEGER IDS_MAX, BOXLIST_SIZE,SZ_BOXNDS
      INTEGER IWORK(70000)
!
      INTEGER, ALLOCATABLE, DIMENSION(:) :: BOXNDS,BOXLIST
     .                                      
C
!      INTEGER SET_USRTOS
!      EXTERNAL SET_USRTOS
C=======================================================================
!
      NINDX_B = 0
!
      ALLOCATE(BOXNDS(NUMNOD))
      SZ_BOXNDS = 0



      ! Read Boxes
      ! ---------------------

      CALL HM_GET_INT_ARRAY_INDEX('idsmax' ,IDS_MAX ,JCLAUSE,IS_AVAILABLE,LSUBMODEL)      ! Get the numbers of boxes to read

      ALLOCATE(BOXLIST(IDS_MAX))
      BOXLIST_SIZE=0

      CALL READ_CLAUSE_BOX(IBOX   ,IS_AVAILABLE ,LSUBMODEL ,JCLAUSE, IDS_MAX ,   
     *                     BOXLIST, BOXLIST_SIZE)

!
      ! Fill Boxes
      ! ---------------------
      CALL FILL_CLAUSE_RBODY_BOX( IBOX   ,X  ,SKEW  ,SET_TITLE ,KEYSET,
     *                            BOXLIST, BOXLIST_SIZE,
     *                            BOXNDS, SZ_BOXNDS    ,RBY_MSN)


      ALLOCATE (CLAUSE%NODE(SZ_BOXNDS))
      CLAUSE%NODE(1:SZ_BOXNDS) = BOXNDS(1:SZ_BOXNDS)
      CLAUSE%NB_NODE = SZ_BOXNDS

      RETURN
      END
