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
!||    fill_gr                ../starter/source/model/sets/fill_gr.F
!||--- called by ------------------------------------------------------
!||    fill_igr               ../starter/source/model/sets/fill_igr.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE FILL_GR(IGRELE, NGRELEM  ,IELT,
     .                   SET_ID  ,SET_TITLE,GETELEM, NELEM,SET_GREID)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Merge one SET type in Group 
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
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SETDEF_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: SET_ID
      INTEGER,INTENT(INOUT) :: NGRELEM
      INTEGER,INTENT(IN) :: NELEM,IELT
      INTEGER,INTENT(IN) :: GETELEM(*)
      INTEGER,INTENT(OUT) :: SET_GREID
C-----------------------------------------------
      TYPE (GROUP_)  , TARGET ,INTENT(INOUT):: IGRELE(*)
      CHARACTER(LEN=NCHARTITLE)::SET_TITLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IGRE
C-----------------------------------------------
      !IF (NELEM == 0) RETURN  create even if empty
!
!  NELEM ==> nb of the elemes of the new group of element (IGRBRIC, ...)
!                   (from /SET)
!
      IGRE = NGRELEM
!
!     create new grelem (IGRBRIC, etc) from elems of /SET
!
!---
      IGRE = IGRE + 1
!---
      IGRELE(IGRE)%ID = SET_ID
      IGRELE(IGRE)%TITLE = SET_TITLE
      IGRELE(IGRE)%NENTITY = NELEM
      IGRELE(IGRE)%GRTYPE = IELT

!     not printout empty group
      IF (NELEM == 0) IGRELE(IGRE)%SET_GROUP = 1
!
      IF (NELEM > 0) THEN
        CALL MY_ALLOC(IGRELE(IGRE)%ENTITY,NELEM)
        IGRELE(IGRE)%ENTITY(1:NELEM) = GETELEM(1:NELEM)
      ENDIF

      ! increment NGRELEM
      NGRELEM = IGRE

      ! In SET save GRID
      SET_GREID = IGRE
C-----
      RETURN
      END
!||====================================================================
!||    fill_surf      ../starter/source/model/sets/fill_gr.F
!||--- called by ------------------------------------------------------
!||    fill_igr       ../starter/source/model/sets/fill_igr.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    message_mod    ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE FILL_SURF(SET,IGRSURF,IGRS)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Merge SET%SURFACE into Radioss Surface 
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     SET           Set Structure - Current SET
C     IGRSURF       SURFACES 
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SETDEF_MOD
      USE QA_OUT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGRS
      TYPE (SURF_)  , TARGET ,INTENT(INOUT):: IGRSURF(*)
      TYPE (SET_)   , INTENT(INOUT) :: SET
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL LINE_SEG, SURF_SEG
      INTEGER NSEG
      CHARACTER MESS*40
      DATA MESS/'SET SURF GROUP DEFINITION               '/
C-----------------------------------------------
!
!     create new grelem (IGRBRIC, etc) from elems of /SET
!
      ! 3D or 2D
      SURF_SEG = .FALSE.
      LINE_SEG = .FALSE.
      NSEG = 0
      IF(SET%NB_SURF_SEG > 0) THEN
        !3D case
        NSEG = SET%NB_SURF_SEG
        SURF_SEG = .TRUE.
      ELSEIF(SET%NB_LINE_SEG > 0)THEN
        !2D case
        NSEG = SET%NB_LINE_SEG
        LINE_SEG = .TRUE.
      ENDIF
      !IF (NSEG == 0) RETURN      ! create a Surface if empty
!---
      IGRS = IGRS + 1
!
      IGRSURF(IGRS)%ID = SET%SET_ID
      IGRSURF(IGRS)%TITLE = SET%TITLE
      IGRSURF(IGRS)%NSEG = NSEG
!
      IGRSURF(IGRS)%TYPE = 0
      IGRSURF(IGRS)%ID_MADYMO = 0
      IGRSURF(IGRS)%IAD_BUFR = 0
      IGRSURF(IGRS)%NB_MADYMO = 0
      IGRSURF(IGRS)%TYPE_MADYMO = 0
      IGRSURF(IGRS)%LEVEL = 1
      IGRSURF(IGRS)%TH_SURF = 0
      IGRSURF(IGRS)%ISH4N3N = 0
      IGRSURF(IGRS)%NSEG_R2R_ALL = 0
      IGRSURF(IGRS)%NSEG_R2R_SHARE = 0
!
!     not printout empty group
!
      IF (NSEG == 0) IGRSURF(IGRS)%SET_GROUP = 1
!
!
      IF (NSEG > 0) THEN
!
        IF (ALLOCATED(IGRSURF(IGRS)%NODES)) DEALLOCATE(IGRSURF(IGRS)%NODES)
        IF (ALLOCATED(IGRSURF(IGRS)%ELTYP)) DEALLOCATE(IGRSURF(IGRS)%ELTYP)
        IF (ALLOCATED(IGRSURF(IGRS)%ELEM))  DEALLOCATE(IGRSURF(IGRS)%ELEM)
!
        CALL MY_ALLOC(IGRSURF(IGRS)%NODES,NSEG,4)
        CALL MY_ALLOC(IGRSURF(IGRS)%ELTYP,NSEG)
        CALL MY_ALLOC(IGRSURF(IGRS)%ELEM,NSEG)
!
        IF(SURF_SEG)THEN
          IGRSURF(IGRS)%NODES(1:NSEG,1) = SET%SURF_NODES(1:NSEG,1)
          IGRSURF(IGRS)%NODES(1:NSEG,2) = SET%SURF_NODES(1:NSEG,2)
          IGRSURF(IGRS)%NODES(1:NSEG,3) = SET%SURF_NODES(1:NSEG,3)
          IGRSURF(IGRS)%NODES(1:NSEG,4) = SET%SURF_NODES(1:NSEG,4)
          IGRSURF(IGRS)%ELTYP(1:NSEG)   = SET%SURF_ELTYP(1:NSEG)
          IGRSURF(IGRS)%ELEM(1:NSEG)    = SET%SURF_ELEM(1:NSEG)
          IGRSURF(IGRS)%EXT_ALL    = SET%EXT_ALL
        ENDIF

        IF(LINE_SEG)THEN
          IGRSURF(IGRS)%NODES(1:NSEG,1) = SET%LINE_NODES(1:NSEG,1)
          IGRSURF(IGRS)%NODES(1:NSEG,2) = SET%LINE_NODES(1:NSEG,2)
          IGRSURF(IGRS)%NODES(1:NSEG,3) = 0
          IGRSURF(IGRS)%NODES(1:NSEG,4) = 0
          IGRSURF(IGRS)%ELTYP(1:NSEG)   = SET%LINE_ELTYP(1:NSEG)
          IGRSURF(IGRS)%ELEM(1:NSEG)    = SET%LINE_ELEM(1:NSEG)
        ENDIF

      ENDIF ! IF (NSEG > 0)

      SET%SET_NSURF_ID = IGRS
      SET%HAS_SURF_SEG = NSEG

C-----
      RETURN
      END
!||====================================================================
!||    fill_line      ../starter/source/model/sets/fill_gr.F
!||--- called by ------------------------------------------------------
!||    fill_igr       ../starter/source/model/sets/fill_igr.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    message_mod    ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE FILL_LINE(SET,IGRSLIN  ,IGRL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Merge SET%LINE into Radioss Lines 
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     SET           Set Structure - Current SET
C     IGRSURF       SURFACES 
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SETDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(INOUT) :: IGRL
C-----------------------------------------------
      TYPE (SURF_)   , INTENT(INOUT) :: IGRSLIN(*)
      TYPE (SET_)    , INTENT(INOUT) :: SET
      CHARACTER(LEN=NCHARTITLE)::SET_TITLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSEG
      CHARACTER MESS*40
      DATA MESS/'SET LINE GROUP DEFINITION               '/
C-----------------------------------------------
!
      NSEG = SET%NB_LINE_SEG
      !IF (NSEG == 0) RETURN      ! create a Surface if empty

      IGRL = IGRL + 1
!
!
      IGRSLIN(IGRL)%ID = SET%SET_ID
      IGRSLIN(IGRL)%TITLE = SET%TITLE
      IGRSLIN(IGRL)%NSEG = NSEG
!
      IGRSLIN(IGRL)%TYPE           = 0
      IGRSLIN(IGRL)%LEVEL          = 1
      IGRSLIN(IGRL)%NSEG_R2R_ALL   = 0
      IGRSLIN(IGRL)%NSEG_R2R_SHARE = 0
!
!     not printout empty group
!
      IF (NSEG == 0) IGRSLIN(IGRL)%SET_GROUP = 1
!
!
      IF (NSEG > 0) THEN
!
        IF (ALLOCATED(IGRSLIN(IGRL)%NODES)) DEALLOCATE(IGRSLIN(IGRL)%NODES)
        IF (ALLOCATED(IGRSLIN(IGRL)%ELTYP)) DEALLOCATE(IGRSLIN(IGRL)%ELTYP)
        IF (ALLOCATED(IGRSLIN(IGRL)%ELEM))  DEALLOCATE(IGRSLIN(IGRL)%ELEM)
        IF (ALLOCATED(IGRSLIN(IGRL)%PROC))  DEALLOCATE(IGRSLIN(IGRL)%PROC)
!
        CALL MY_ALLOC(IGRSLIN(IGRL)%NODES,NSEG,2)
        CALL MY_ALLOC(IGRSLIN(IGRL)%ELTYP,NSEG)
        CALL MY_ALLOC(IGRSLIN(IGRL)%ELEM,NSEG)
        CALL MY_ALLOC(IGRSLIN(IGRL)%PROC,NSEG)
!
        IGRSLIN(IGRL)%NODES(1:NSEG,1) = SET%LINE_NODES(1:NSEG,1)
        IGRSLIN(IGRL)%NODES(1:NSEG,2) = SET%LINE_NODES(1:NSEG,2)
        IGRSLIN(IGRL)%ELTYP(1:NSEG)   = SET%LINE_ELTYP(1:NSEG)
        IGRSLIN(IGRL)%ELEM(1:NSEG)    = SET%LINE_ELEM(1:NSEG)
        IGRSLIN(IGRL)%PROC(1:NSEG) = 0
      ENDIF ! IF (NSEG > 0)
C-----
      SET%SET_NSLIN_ID=IGRL
      SET%HAS_LINE_SEG = NSEG

      RETURN
      END

