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
Chd|  FILL_IGR                      source/model/sets/fill_igr.F  
Chd|-- called by -----------
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|-- calls ---------------
Chd|        FILL_GR                       source/model/sets/fill_gr.F   
Chd|        FILL_LINE                     source/model/sets/fill_gr.F   
Chd|        FILL_SURF                     source/model/sets/fill_gr.F   
Chd|        HM_GROUP_IS_USED              source/devtools/hm_reader/hm_group_is_used.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SURF_MOD                      share/modules1/surf_mod.F     
Chd|====================================================================
      SUBROUTINE FILL_IGR(SET,
     .                    IGRSURF ,IGRNOD  ,IGRSLIN  ,IGRPART ,IGRBRIC, IGRQUAD ,   
     .                    IGRSH4N ,IGRSH3N ,IGRTRUSS ,IGRBEAM ,IGRSPRING)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Fill Radioss Group & Surfaces from SETs
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     SET           Set Structure - Current SET
C     IGRxxx        SURFACES & Groups
C============================================================================
C-----------------------------------------------
C   D e f i n i t i o n s 
C-----------------------------------------------
#include      "set_def.inc"
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE HM_OPTION_READ_MOD
      USE SETDEF_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SURF_MOD
      USE QA_OUT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "my_allocate.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (GROUP_)  , INTENT(INOUT):: IGRNOD(*)
      TYPE (SURF_)   , INTENT(INOUT):: IGRSURF(*)
      TYPE (SURF_)   , INTENT(INOUT):: IGRSLIN(*)
      TYPE (GROUP_)  , INTENT(INOUT):: IGRPART(*)
      TYPE (GROUP_)  , INTENT(INOUT):: IGRBRIC(*)
      TYPE (GROUP_)  , INTENT(INOUT):: IGRQUAD(*)
      TYPE (GROUP_)  , INTENT(INOUT):: IGRSH4N(*)
      TYPE (GROUP_)  , INTENT(INOUT):: IGRSH3N(*)
      TYPE (GROUP_)  , INTENT(INOUT):: IGRTRUSS(*)
      TYPE (GROUP_)  , INTENT(INOUT):: IGRBEAM(*)
      TYPE (GROUP_)  , INTENT(INOUT):: IGRSPRING(*)
      TYPE (SET_), DIMENSION(NSETS),INTENT(INOUT) :: SET
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IGS,GRTYPE
      LOGICAL IS_USED

C-----------------------------------------------

       DO IGS = 1,NSETS

         IF( SET(IGS)%SET_ACTIV == 0 ) CYCLE                  !  SET_ACTIV = 0 in case of /SET/COLLECT
                                                              !  With /SET/COLLECT with same ID, Create Radioss
                                                              !  groups only on one SET which has all included.
         ! PART
         CALL HM_GROUP_IS_USED('/GRPART',7,SET(IGS)%SET_ID,IS_USED)
         IF(IS_USED .OR. DOQA == 1)THEN
           GRTYPE = 0
           CALL FILL_GR( IGRPART ,NGRPART,GRTYPE,
     *                 SET(IGS)%SET_ID,SET(IGS)%TITLE,SET(IGS)%PART,SET(IGS)%NB_PART,SET(IGS)%SET_GRPART_ID)
         ENDIF

         IF(DOQA == 0) THEN              ! if qa_print keep SET memory
           IF(ALLOCATED (SET(IGS)%PART )) DEALLOCATE ( SET(IGS)%PART )
           SET(IGS)%NB_PART = 0
         ENDIF

         ! SOLID
         CALL HM_GROUP_IS_USED('/GRBRIC',7,SET(IGS)%SET_ID,IS_USED)
         IF(IS_USED .OR. DOQA == 1)THEN
           GRTYPE = 1
           CALL FILL_GR( IGRBRIC ,NGRBRIC,GRTYPE,
     *                 SET(IGS)%SET_ID,SET(IGS)%TITLE,SET(IGS)%SOLID,SET(IGS)%NB_SOLID,SET(IGS)%SET_GRSOLID_ID)
         ENDIF

         IF(DOQA == 0) THEN              ! if qa_print keep SET memory
           IF(ALLOCATED (SET(IGS)%SOLID )) DEALLOCATE ( SET(IGS)%SOLID )
           SET(IGS)%NB_SOLID = 0
         ENDIF

         ! QUAD
         CALL HM_GROUP_IS_USED('/GRQUAD',7,SET(IGS)%SET_ID,IS_USED)
         IF(IS_USED .OR. DOQA == 1)THEN
           GRTYPE = 2
           CALL FILL_GR( IGRQUAD ,NGRQUAD,GRTYPE,
     *                 SET(IGS)%SET_ID,SET(IGS)%TITLE,SET(IGS)%QUAD,SET(IGS)%NB_QUAD,SET(IGS)%SET_GRQUAD_ID)
         ENDIF

         IF(DOQA == 0) THEN              ! if qa_print keep SET memory
           IF(ALLOCATED (SET(IGS)%QUAD )) DEALLOCATE ( SET(IGS)%QUAD )
           SET(IGS)%NB_QUAD = 0
         ENDIF

         ! SH4N
         CALL HM_GROUP_IS_USED('/GRSHEL',7,SET(IGS)%SET_ID,IS_USED)
         IF(IS_USED .OR. DOQA == 1)THEN
           GRTYPE = 3
           CALL FILL_GR( IGRSH4N ,NGRSHEL,GRTYPE,
     *                 SET(IGS)%SET_ID,SET(IGS)%TITLE,SET(IGS)%SH4N,SET(IGS)%NB_SH4N,SET(IGS)%SET_GRSH4N_ID)
         ENDIF

         IF(DOQA == 0) THEN              ! if qa_print keep SET memory
           IF(ALLOCATED (SET(IGS)%SH4N )) DEALLOCATE ( SET(IGS)%SH4N )
           SET(IGS)%NB_SH4N = 0
         ENDIF

         ! SH3N
         CALL HM_GROUP_IS_USED('/GRSH3N',7,SET(IGS)%SET_ID,IS_USED)
         IF(IS_USED .OR. DOQA == 1)THEN
           GRTYPE = 7
           CALL FILL_GR( IGRSH3N ,NGRSH3N,GRTYPE,
     *                 SET(IGS)%SET_ID,SET(IGS)%TITLE,SET(IGS)%SH3N,SET(IGS)%NB_SH3N,SET(IGS)%SET_GRSH3N_ID)
         ENDIF

         IF(DOQA == 0) THEN              ! if qa_print keep SET memory
           IF(ALLOCATED (SET(IGS)%SH3N )) DEALLOCATE ( SET(IGS)%SH3N )
           SET(IGS)%NB_SH3N = 0
         ENDIF

         ! TRIA
         CALL HM_GROUP_IS_USED('/GRTRIA',7,SET(IGS)%SET_ID,IS_USED)
         IF(IS_USED .OR. DOQA == 1)THEN
           IF (NUMELTRIA > 0) THEN
             GRTYPE = 7
             CALL FILL_GR( IGRSH3N ,NGRSH3N,GRTYPE,
     *                 SET(IGS)%SET_ID,SET(IGS)%TITLE,SET(IGS)%TRIA,SET(IGS)%NB_TRIA,SET(IGS)%SET_GRTRIA_ID)
           ENDIF ! IF (NUMELTRIA > 0)
         ENDIF

         IF(DOQA == 0) THEN              ! if qa_print keep SET memory
           IF(ALLOCATED (SET(IGS)%TRIA )) DEALLOCATE ( SET(IGS)%TRIA )
           SET(IGS)%NB_TRIA = 0
         ENDIF

         ! TRUSS
         CALL HM_GROUP_IS_USED('/GRTRUSS',8,SET(IGS)%SET_ID,IS_USED)
         IF(IS_USED .OR. DOQA == 1)THEN
           GRTYPE = 4
           CALL FILL_GR( IGRTRUSS ,NGRTRUS,GRTYPE,
     *                 SET(IGS)%SET_ID,SET(IGS)%TITLE,SET(IGS)%TRUSS,SET(IGS)%NB_TRUSS,SET(IGS)%SET_GRTRUSS_ID)
         ENDIF

         IF(DOQA == 0) THEN              ! if qa_print keep SET memory
           IF(ALLOCATED (SET(IGS)%TRUSS )) DEALLOCATE ( SET(IGS)%TRUSS )
           SET(IGS)%NB_TRUSS = 0
         ENDIF

         ! BEAM
         CALL HM_GROUP_IS_USED('/GRBEAM',7,SET(IGS)%SET_ID,IS_USED)
         IF(IS_USED .OR. DOQA == 1)THEN
           GRTYPE = 5
           CALL FILL_GR( IGRBEAM ,NGRBEAM,GRTYPE,
     *                 SET(IGS)%SET_ID,SET(IGS)%TITLE,SET(IGS)%BEAM,SET(IGS)%NB_BEAM,SET(IGS)%SET_GRBEAM_ID)
         ENDIF

         IF(DOQA == 0) THEN              ! if qa_print keep SET memory
           IF(ALLOCATED (SET(IGS)%BEAM )) DEALLOCATE ( SET(IGS)%BEAM )
           SET(IGS)%NB_BEAM = 0
         ENDIF

         ! SPRING
         CALL HM_GROUP_IS_USED('/GRSPRING',9,SET(IGS)%SET_ID,IS_USED)
         IF(IS_USED .OR. DOQA == 1)THEN
           GRTYPE = 6
           CALL FILL_GR( IGRSPRING ,NGRSPRI,GRTYPE,
     *                 SET(IGS)%SET_ID,SET(IGS)%TITLE,SET(IGS)%SPRING,SET(IGS)%NB_SPRING,SET(IGS)%SET_GRSPRING_ID)
         ENDIF

         IF(DOQA == 0) THEN              ! if qa_print keep SET memory
           IF(ALLOCATED (SET(IGS)%SPRING )) DEALLOCATE ( SET(IGS)%SPRING )
           SET(IGS)%NB_SPRING = 0
         ENDIF

         ! Nodes
         CALL HM_GROUP_IS_USED('/GRNOD',6,SET(IGS)%SET_ID,IS_USED)
         IF(IS_USED .OR. DOQA == 1)THEN
           GRTYPE = 0
           CALL FILL_GR( IGRNOD ,NGRNOD,GRTYPE,
     *                 SET(IGS)%SET_ID,SET(IGS)%TITLE,SET(IGS)%NODE,SET(IGS)%NB_NODE,SET(IGS)%SET_GRNOD_ID)
         ENDIF

         IF(DOQA == 0) THEN              ! if qa_print keep SET memory
           IF(ALLOCATED (SET(IGS)%NODE )) DEALLOCATE ( SET(IGS)%NODE )
           SET(IGS)%NB_NODE = 0
         ENDIF

         ! Surfaces
         CALL HM_GROUP_IS_USED('/SURF',5,SET(IGS)%SET_ID,IS_USED)
         IF(IS_USED .OR. DOQA == 1)THEN
           CALL FILL_SURF(SET(IGS),IGRSURF,NSURF)
         ENDIF

         IF(DOQA == 0) THEN              ! if qa_print keep SET memory
           IF(ALLOCATED (SET(IGS)%SURF_NODES )) DEALLOCATE ( SET(IGS)%SURF_NODES )
           IF(ALLOCATED (SET(IGS)%SURF_ELTYP )) DEALLOCATE ( SET(IGS)%SURF_ELTYP )
           IF(ALLOCATED (SET(IGS)%SURF_ELEM ))  DEALLOCATE ( SET(IGS)%SURF_ELEM )
           SET(IGS)%NB_SURF_SEG = 0
         ENDIF

         ! Lines
         CALL HM_GROUP_IS_USED('/LINE',5,SET(IGS)%SET_ID,IS_USED)
         IF(IS_USED .OR. DOQA == 1)THEN
           CALL FILL_LINE(SET(IGS),IGRSLIN,NSLIN)
         ENDIF

         IF(DOQA == 0) THEN              ! if qa_print keep SET memory
           IF(ALLOCATED (SET(IGS)%LINE_NODES )) DEALLOCATE ( SET(IGS)%LINE_NODES )
           IF(ALLOCATED (SET(IGS)%LINE_ELTYP )) DEALLOCATE ( SET(IGS)%LINE_ELTYP )
           IF(ALLOCATED (SET(IGS)%LINE_ELEM ))  DEALLOCATE ( SET(IGS)%LINE_ELEM )
           SET(IGS)%NB_LINE_SEG = 0
         ENDIF

       ENDDO

      END 


