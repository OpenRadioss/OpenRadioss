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
      !||    fill_igr              ../starter/source/model/sets/fill_igr.F
      !||--- called by ------------------------------------------------------
      !||    hm_set                ../starter/source/model/sets/hm_set.F
      !||--- calls      -----------------------------------------------------
      !||    fill_gr               ../starter/source/model/sets/fill_gr.F
      !||    fill_line             ../starter/source/model/sets/fill_gr.F
      !||    fill_surf             ../starter/source/model/sets/fill_gr.F
      !||    fill_surf_ellipse     ../starter/source/model/sets/fill_gr_surf_ellipse.F
      !||    fill_surf_plane       ../starter/source/model/sets/fill_gr_surf_plane.F90
      !||    hm_group_is_used      ../starter/source/devtools/hm_reader/hm_group_is_used.F
      !||--- uses       -----------------------------------------------------
      !||    file_descriptor_mod   ../starter/source/modules/file_descriptor_mod.F90
      !||    fill_surf_plane_mod   ../starter/source/model/sets/fill_gr_surf_plane.F90
      !||    hm_option_read_mod    ../starter/share/modules1/hm_option_read_mod.F
      !||    message_mod           ../starter/share/message_module/message_mod.F
      !||    surf_mod              ../starter/share/modules1/surf_mod.F
      !||====================================================================
      SUBROUTINE FILL_IGR(SET,
     .                    IGRSURF ,IGRNOD  ,IGRSLIN  ,IGRPART ,IGRBRIC  ,IGRQUAD,   
     .                    IGRSH4N ,IGRSH3N ,IGRTRUSS ,IGRBEAM ,IGRSPRING,BUFSF  ,
     .                    LISURF1 ,ROOTNAM ,ROOTLEN  ,INFILE_NAME ,INFILE_NAME_LEN)
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
C   M o d u l e s
C-----------------------------------------------
      USE HM_OPTION_READ_MOD
      USE SETDEF_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SURF_MOD
      USE QA_OUT_MOD
      USE FILL_SURF_PLANE_MOD
      USE file_descriptor_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
      INTEGER, INTENT(IN) :: LISURF1
      MY_REAL, INTENT(INOUT) :: BUFSF(LISURF1*(NSURF+NSETS))
      INTEGER, INTENT(IN)   ::  ROOTLEN,INFILE_NAME_LEN
      CHARACTER(LEN=ROOTLEN), INTENT(IN) ::  ROOTNAM
      CHARACTER(LEN=INFILE_NAME_LEN), INTENT(IN) ::  INFILE_NAME
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IGS,GRTYPE,IO_ERR1,IO_ERR2,NB_GRNODE
      LOGICAL IS_USED
      CHARACTER FILNAM*109, KEYA*80
      CHARACTER(LEN=NCHARLINE) ::CARTE
      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=4096) :: TMP_NAME
      INTEGER , DIMENSION(:), ALLOCATABLE :: GRNODE
C-----------------------------------------------
C     READING ENGINE FILE
C-----------------------------------------------   
      IO_ERR1 = 0
      NB_GRNODE = 0
      DO I = 1, 10000
        WRITE(FILNAM, '(A, A, I4.4, A)') ROOTNAM(1:ROOTLEN), '_', I-1, '.rad'
        TMP_NAME = INFILE_NAME(1:INFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))    
        LEN_TMP_NAME = INFILE_NAME_LEN + LEN_TRIM(FILNAM)
        OPEN(UNIT=tmp_engine, FILE=TMP_NAME(1:LEN_TMP_NAME),
     .     ACCESS='SEQUENTIAL', STATUS='OLD', IOSTAT=IO_ERR1)
C
        IF (IO_ERR1 == 0) THEN
C
          IO_ERR2 = 0
          DO WHILE (IO_ERR2 == 0)
            READ(UNIT=tmp_engine, FMT='(A)', IOSTAT=IO_ERR2) KEYA
            IF (IO_ERR2 == 0) THEN
      ! Process the line stored in KEYA here if needed
              IF(KEYA(1:14) == '/DT/NODA/CST/1' .OR. KEYA(1:8) == '/DYREL/1' .OR. KEYA(1:8) == '/KEREL/1' .OR.
     .           KEYA(1:14) == '/INIV/AXIS/X/2' .OR. KEYA(1:14) == '/INIV/AXIS/X/2' .OR.
     .           KEYA(1:14) == '/INIV/AXIS/Z/2')  THEN
                NB_GRNODE = NB_GRNODE + 1
              ELSE IF(KEYA(1:6) == '/BEGIN') THEN
                IO_ERR2 = 1
              ENDIF
            END IF
          END DO

          CLOSE(tmp_engine)
        ENDIF
      END DO

      IF (NB_GRNODE > 0) ALLOCATE(GRNODE(NB_GRNODE))
      NB_GRNODE = 0

      DO I = 1, 10000
        WRITE(FILNAM, '(A, A, I4.4, A)') ROOTNAM(1:ROOTLEN), '_', I-1, '.rad'
        TMP_NAME = INFILE_NAME(1:INFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))    
        LEN_TMP_NAME = INFILE_NAME_LEN + LEN_TRIM(FILNAM)
        OPEN(UNIT=tmp_engine, FILE=TMP_NAME(1:LEN_TMP_NAME),
     .     ACCESS='SEQUENTIAL', STATUS='OLD', IOSTAT=IO_ERR1)
C
        IF (IO_ERR1 == 0) THEN
C
          IO_ERR2 = 0
          DO WHILE (IO_ERR2 == 0)
            READ(UNIT=tmp_engine, FMT='(A)', IOSTAT=IO_ERR2) KEYA
            IF (IO_ERR2 == 0) THEN
      ! Process the line stored in KEYA here if needed
              IF(KEYA(1:14) == '/DT/NODA/CST/1') THEN
                NB_GRNODE = NB_GRNODE + 1
                READ(UNIT=tmp_engine, FMT='(A)', IOSTAT=IO_ERR2) CARTE
                READ(UNIT=tmp_engine, FMT='(A)', IOSTAT=IO_ERR2) CARTE
                READ(CARTE, FMT=*) GRNODE(NB_GRNODE)
              ELSEIF(KEYA(1:8) == '/DYREL/1') THEN
                NB_GRNODE = NB_GRNODE + 1
                READ(UNIT=tmp_engine, FMT='(A)', IOSTAT=IO_ERR2) CARTE
                READ(CARTE, FMT=*) GRNODE(NB_GRNODE)
              ELSEIF(KEYA(1:8) == '/KEREL/1') THEN
                NB_GRNODE = NB_GRNODE + 1
                READ(UNIT=tmp_engine, FMT='(A)', IOSTAT=IO_ERR2) CARTE
                READ(CARTE, FMT=*) GRNODE(NB_GRNODE)
              ELSEIF(KEYA(1:14) == '/INIV/AXIS/X/2' .OR. KEYA(1:14) == '/INIV/AXIS/Y/2' .OR. 
     .               KEYA(1:14) == '/INIV/AXIS/Z/2') THEN
                NB_GRNODE = NB_GRNODE + 1
                READ(UNIT=tmp_engine, FMT='(A)', IOSTAT=IO_ERR2) CARTE
                READ(UNIT=tmp_engine, FMT='(A)', IOSTAT=IO_ERR2) CARTE
                READ(UNIT=tmp_engine, FMT='(A)', IOSTAT=IO_ERR2) CARTE
                READ(CARTE, FMT=*) GRNODE(NB_GRNODE)
              ELSE IF(KEYA(1:6) == '/BEGIN') THEN
                IO_ERR2 = 1
              ENDIF
            END IF
          END DO

          CLOSE(tmp_engine)
        ENDIF
      END DO



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

         IF(.NOT. IS_USED .AND. NB_GRNODE > 0) THEN
            DO I=1,NB_GRNODE
              IF(SET(IGS)%SET_ID == GRNODE(I)) THEN
                IS_USED = .TRUE.
              ENDIF
            ENDDO
         ENDIF

         IF((IS_USED .OR. DOQA == 1) .AND. SET(IGS)%NB_ELLIPSE == 0 .AND. SET(IGS)%NB_PLANE == 0
     *                               .AND. SET(IGS)%NB_NODENS == 0 )THEN
           GRTYPE = 0
           CALL FILL_GR( IGRNOD ,NGRNOD,GRTYPE,
     *                 SET(IGS)%SET_ID,SET(IGS)%TITLE,SET(IGS)%NODE,SET(IGS)%NB_NODE,SET(IGS)%SET_GRNOD_ID)
         ENDIF

         IF(DOQA == 0) THEN              ! if qa_print keep SET memory
           IF(ALLOCATED (SET(IGS)%NODE )) DEALLOCATE ( SET(IGS)%NODE )
           SET(IGS)%NB_NODE = 0
         ENDIF

          ! Nodens
         CALL HM_GROUP_IS_USED('/GRNOD',6,SET(IGS)%SET_ID,IS_USED)
         IF((IS_USED .OR. DOQA == 1) .AND. SET(IGS)%NB_ELLIPSE == 0 .AND. SET(IGS)%NB_PLANE == 0
     *                               .AND. SET(IGS)%NB_NODENS /= 0 )THEN
           GRTYPE = 0
           CALL FILL_GR( IGRNOD ,NGRNOD,GRTYPE,
     *                 SET(IGS)%SET_ID,SET(IGS)%TITLE,SET(IGS)%NODENS,SET(IGS)%NB_NODENS,SET(IGS)%SET_GRNOD_ID)
           IGRNOD(SET(IGS)%SET_GRNOD_ID)%SORTED = 1
         ENDIF

         IF(DOQA == 0) THEN              ! if qa_print keep SET memory
           IF(ALLOCATED (SET(IGS)%NODENS )) DEALLOCATE ( SET(IGS)%NODENS )
           SET(IGS)%NB_NODENS = 0
         ENDIF

         ! Surfaces
         CALL HM_GROUP_IS_USED('/SURF',5,SET(IGS)%SET_ID,IS_USED)
         IF(IS_USED .OR. DOQA == 1)THEN
           IF (SET(IGS)%NB_ELLIPSE == 0 .and. SET(IGS)%NB_PLANE == 0) CALL FILL_SURF(SET(IGS),IGRSURF,NSURF)
           IF (SET(IGS)%NB_ELLIPSE > 0)  CALL FILL_SURF_ELLIPSE(SET(IGS),IGRSURF,NSURF,BUFSF,LISURF1,NSURF)
           IF (SET(IGS)%NB_PLANE > 0)    CALL FILL_SURF_PLANE(SET(IGS),IGRSURF,NSURF,BUFSF,LISURF1,NSURF)
         ENDIF

         IF(DOQA == 0) THEN              ! if qa_print keep SET memory
           IF(ALLOCATED (SET(IGS)%SURF_NODES )) DEALLOCATE ( SET(IGS)%SURF_NODES )
           IF(ALLOCATED (SET(IGS)%SURF_ELTYP )) DEALLOCATE ( SET(IGS)%SURF_ELTYP )
           IF(ALLOCATED (SET(IGS)%SURF_ELEM ))  DEALLOCATE ( SET(IGS)%SURF_ELEM )
           SET(IGS)%NB_SURF_SEG = 0
           ! ellipse surface
           IF(ALLOCATED (SET(IGS)%ELLIPSE_SKEW )) DEALLOCATE ( SET(IGS)%ELLIPSE_SKEW )
           SET(IGS)%ELLIPSE_A = ZERO
           SET(IGS)%ELLIPSE_B = ZERO
           SET(IGS)%ELLIPSE_C = ZERO
           SET(IGS)%ELLIPSE_XC = ZERO
           SET(IGS)%ELLIPSE_YC = ZERO
           SET(IGS)%ELLIPSE_ZC = ZERO
           SET(IGS)%ELLIPSE_N = ZERO
           SET(IGS)%ELLIPSE_IAD_BUFR = 0
           SET(IGS)%ELLIPSE_ID_MADYMO = 0
           ! plane surface
           SET(IGS)%PLANE_XM = ZERO
           SET(IGS)%PLANE_YM = ZERO
           SET(IGS)%PLANE_ZM = ZERO
           SET(IGS)%PLANE_XM1 = ZERO
           SET(IGS)%PLANE_YM1 = ZERO
           SET(IGS)%PLANE_ZM1 = ZERO
           SET(IGS)%PLANE_IAD_BUFR = 0
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

       IF(ALLOCATED(GRNODE))  DEALLOCATE(GRNODE)

      END 


