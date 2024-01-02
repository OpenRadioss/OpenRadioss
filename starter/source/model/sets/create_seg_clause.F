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
Chd|  CREATE_SEG_CLAUSE             source/model/sets/create_seg_clause.F
Chd|-- called by -----------
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_INT_ARRAY_2INDEXES     source/devtools/hm_reader/hm_get_int_array_2indexes.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        SET_USRTOS                    source/model/sets/ipartm1.F   
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE CREATE_SEG_CLAUSE(
     .               CLAUSE,  ITABM1  ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Create PART Clause from LIST
C------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     CLAUSE        (SET structure) Clause to be treated
C     IPARTM1       MAP Table UID -> LocalID
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
      INTEGER JCLAUSE
      LOGICAL :: IS_AVAILABLE
      INTEGER, INTENT(IN), DIMENSION(NUMNOD,2) :: ITABM1
!
      TYPE (SET_) ::  CLAUSE
      TYPE(SUBMODEL_DATA),INTENT(IN):: LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NINDX,SEG_MAX,NOD_1,NOD_2,NOD_3,NOD_4,
     . NODSYS_1,NODSYS_2,NODSYS_3,NODSYS_4,SEG_ID,LINE_SEG
!
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: BUFSURF
!
      INTEGER SET_USRTOS
      EXTERNAL SET_USRTOS
C=======================================================================

      LINE_SEG = 0

      CALL HM_GET_INT_ARRAY_INDEX('segmax' ,SEG_MAX ,JCLAUSE,IS_AVAILABLE,LSUBMODEL)

      ALLOCATE(BUFSURF(4,SEG_MAX))

      NINDX = 0
!
      ! Read & convert Segs
      ! ---------------------
      DO I=1,SEG_MAX  ! always = 1 for clause 'SEG'

        CALL HM_GET_INT_ARRAY_2INDEXES('segid',SEG_ID,JCLAUSE ,I,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INT_ARRAY_2INDEXES('ids1' ,NOD_1 ,JCLAUSE ,I,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INT_ARRAY_2INDEXES('ids2' ,NOD_2 ,JCLAUSE ,I,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INT_ARRAY_2INDEXES('ids3' ,NOD_3 ,JCLAUSE ,I,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INT_ARRAY_2INDEXES('ids4' ,NOD_4 ,JCLAUSE ,I,IS_AVAILABLE,LSUBMODEL)


        NODSYS_1 = SET_USRTOS(NOD_1,ITABM1,NUMNOD)
        IF (NODSYS_1 == 0) THEN
           ! Node was not found. Issue a Warning & Skip.
           CALL ANCMSG(MSGID=1903,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             I1 = CLAUSE%SET_ID,
     .                             I2=SEG_ID,
     .                             I3=NOD_1,
     .                             C1=TRIM(CLAUSE%TITLE),
     .                             C2='NODE')
        ELSE
          NODSYS_1 = ITABM1(NODSYS_1,2)
        ENDIF
        

        NODSYS_2 = SET_USRTOS(NOD_2,ITABM1,NUMNOD)
        IF (NODSYS_2 == 0) THEN
           ! Node was not found. Issue a Warning & Skip.
           CALL ANCMSG(MSGID=1903,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             I1 = CLAUSE%SET_ID,
     .                             I2=SEG_ID,
     .                             I3=NOD_2,
     .                             C1=TRIM(CLAUSE%TITLE),
     .                             C2='NODE')
        ELSE
          NODSYS_2 = ITABM1(NODSYS_2,2)
        ENDIF


        NODSYS_3 = SET_USRTOS(NOD_3,ITABM1,NUMNOD)
        NODSYS_4 = SET_USRTOS(NOD_4,ITABM1,NUMNOD)

        IF (NODSYS_3 == 0 .AND. NODSYS_4 == 0) LINE_SEG = 1 ! Line SEG



        IF (LINE_SEG == 0) THEN! Surf SEG --> continue Node check existence
          IF (NODSYS_3 == 0) THEN
            ! Node was not found. Issue a Warning & Skip.
            CALL ANCMSG(MSGID=1903,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             I1 = CLAUSE%SET_ID,
     .                             I2=SEG_ID,
     .                             I3=NOD_3,
     .                             C1=TRIM(CLAUSE%TITLE),
     .                             C2='NODE')
          ELSE
            NODSYS_3 = ITABM1(NODSYS_3,2)
          ENDIF

!         correction to allow for 3 noded surface (triangle)
          IF (NODSYS_4 == 0) NODSYS_4 = NODSYS_3
!
!          IF (NODSYS_4 == 0) THEN
!            ! Node was not found. Issue a Warning & Skip.
!            CALL ANCMSG(MSGID=1903,ANMODE=ANINFO,
!     .                             MSGTYPE=MSGERROR,
!     .                             I1 = CLAUSE%SET_ID,
!     .                             I2=SEG_ID,
!     .                             I3=NOD_4,
!     .                             C1=TRIM(CLAUSE%TITLE),
!     .                             C2='NODE')
!          ELSE
!            NODSYS_4 = ITABM1(NODSYS_4,2)
!          ENDIF
        ENDIF ! IF (LINE_SEG == 0)




        NINDX = NINDX+1    !   nb of CLAUSE SEGs
        BUFSURF(1,NINDX) = NODSYS_1
        BUFSURF(2,NINDX) = NODSYS_2
        BUFSURF(3,NINDX) = NODSYS_3
        BUFSURF(4,NINDX) = NODSYS_4

      ENDDO ! DO I=1,SEG_MAX



      ! Copy in final SET
      ! ------------------

      !------------------------------------!
      !  create SURF clause or LINE clause !
      !------------------------------------!


      IF (LINE_SEG == 0) THEN

        ! SURF seg (4-node SEG)
        CLAUSE%NB_SURF_SEG = NINDX
        ALLOCATE(CLAUSE%SURF_NODES(NINDX,4))
        ALLOCATE(CLAUSE%SURF_ELTYP(NINDX))
        ALLOCATE(CLAUSE%SURF_ELEM(NINDX))

        DO I=1,NINDX
          CLAUSE%SURF_NODES(I,1) = BUFSURF(1,I) ! N1
          CLAUSE%SURF_NODES(I,2) = BUFSURF(2,I) ! N2
          CLAUSE%SURF_NODES(I,3) = BUFSURF(3,I) ! N3
          CLAUSE%SURF_NODES(I,4) = BUFSURF(4,I) ! N4
          CLAUSE%SURF_ELTYP(I)   = 0  ! ELTYP
          CLAUSE%SURF_ELEM(I)    = 0  ! ELEM
        ENDDO

      ELSE

        ! LINE seg (2-node SEG)
        CLAUSE%NB_LINE_SEG = NINDX
        ALLOCATE(CLAUSE%LINE_NODES(NINDX,2))
        ALLOCATE(CLAUSE%LINE_ELTYP(NINDX))
        ALLOCATE(CLAUSE%LINE_ELEM(NINDX))

        DO I=1,NINDX
          CLAUSE%LINE_NODES(I,1) = BUFSURF(1,I) ! N1
          CLAUSE%LINE_NODES(I,2) = BUFSURF(2,I) ! N2
          CLAUSE%LINE_NODES(I,3) = 0 ! N3
          CLAUSE%LINE_NODES(I,4) = 0 ! N4
          CLAUSE%LINE_ELTYP(I)   = 0  ! ELTYP
          CLAUSE%LINE_ELEM(I)    = 0  ! ELEM
        ENDDO

      ENDIF ! IF (LINE_SEG == 0)

C-------------------------
      DEALLOCATE(BUFSURF)
C-------------------------
      RETURN
      END
