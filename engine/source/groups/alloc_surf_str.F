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
!||    alloc_surf_str         ../engine/source/groups/alloc_surf_str.F
!||--- called by ------------------------------------------------------
!||    surf_ini               ../engine/source/groups/surf_ini.F
!||--- calls      -----------------------------------------------------
!||    fretitl2               ../engine/source/input/freform.F
!||    read_i_c               ../common_source/tools/input_output/write_routtines.c
!||--- uses       -----------------------------------------------------
!||    groupdef_mod           ../common_source/modules/groupdef_mod.F
!||    message_mod            ../engine/share/message_module/message_mod.F
!||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
!||====================================================================
      SUBROUTINE ALLOC_SURF_STR(IGRSURF)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(SURF_)   ,DIMENSION(NSURF) :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISU,I,J,K,L_SURF,ID,NSEG,TYPE,ID_MADYMO,IAD_BUFR,
     .        NB_MADYMO,TYPE_MADYMO,LEVEL,TH_SURF,ISH4N3N,NSEG_R2R_ALL,
     .        NSEG_R2R_SHARE,NODE,ELTYP,ELEM,ERR,ITITLE(LTITR),LEN
      CHARACTER(LEN=NCHARTITLE) :: TITR
      INTEGER, ALLOCATABLE, DIMENSION (:)  ::  ISURFI
!
!=======================================================================
      DO ISU=1,NSURF
        TITR(1:NCHARTITLE) = ' '
        CALL READ_I_C(ITITLE,LTITR)
        CALL FRETITL2(TITR,ITITLE,LTITR)
        LEN = 0
        DO J=1,LTITR
          IF (TITR(J:J) /= ' ') LEN = J
        ENDDO
        IGRSURF(ISU)%TITLE = TITR
      ENDDO ! DO ISU=1,NSURF
!!------  Allocate and read subset data from restart
!
      ALLOCATE (ISURFI(LENISURF) ,STAT=ERR)
      CALL READ_I_C(ISURFI, LENISURF)
!=======================================================================
      L_SURF = 0
!
      DO ISU=1,NSURF
        ID             = ISURFI(L_SURF+1)
          L_SURF = L_SURF+1
        NSEG           = ISURFI(L_SURF+1)
          L_SURF = L_SURF+1
        TYPE           = ISURFI(L_SURF+1)
          L_SURF = L_SURF+1
        ID_MADYMO      = ISURFI(L_SURF+1)
          L_SURF = L_SURF+1
        IAD_BUFR       = ISURFI(L_SURF+1)
          L_SURF = L_SURF+1
        NB_MADYMO      = ISURFI(L_SURF+1)
          L_SURF = L_SURF+1
        TYPE_MADYMO    = ISURFI(L_SURF+1)
          L_SURF = L_SURF+1
        LEVEL          = ISURFI(L_SURF+1)
          L_SURF = L_SURF+1
        TH_SURF        = ISURFI(L_SURF+1)
          L_SURF = L_SURF+1
        ISH4N3N        = ISURFI(L_SURF+1)
          L_SURF = L_SURF+1
        NSEG_R2R_ALL   = ISURFI(L_SURF+1)
          L_SURF = L_SURF+1
        NSEG_R2R_SHARE = ISURFI(L_SURF+1)
          L_SURF = L_SURF+1
!
!        ICHAR(TITR)    = ISURFI(L_SURF+1)
!          L_SURF = L_SURF+1
!---
       IGRSURF(ISU)%ID             = ID
       IGRSURF(ISU)%NSEG           = NSEG
       IGRSURF(ISU)%TYPE           = TYPE
       IGRSURF(ISU)%ID_MADYMO      = ID_MADYMO
       IGRSURF(ISU)%IAD_BUFR       = IAD_BUFR
       IGRSURF(ISU)%NB_MADYMO      = NB_MADYMO
       IGRSURF(ISU)%TYPE_MADYMO    = TYPE_MADYMO
       IGRSURF(ISU)%LEVEL          = LEVEL
       IGRSURF(ISU)%TH_SURF        = TH_SURF
       IGRSURF(ISU)%ISH4N3N        = ISH4N3N
       IGRSURF(ISU)%NSEG_R2R_ALL   = NSEG_R2R_ALL
       IGRSURF(ISU)%NSEG_R2R_SHARE = NSEG_R2R_SHARE
!
! SURF ENTITIES (NODES, ELTYP, ELEM)
!
       ALLOCATE(IGRSURF(ISU)%NODES(NSEG,4))
       ALLOCATE(IGRSURF(ISU)%ELTYP(NSEG))
       ALLOCATE(IGRSURF(ISU)%ELEM(NSEG))
!
        DO J=1,NSEG
          DO K=1,4
            NODE = ISURFI(L_SURF+1)
              L_SURF = L_SURF+1
            IGRSURF(ISU)%NODES(J,K) = NODE
          ENDDO
          ELTYP = ISURFI(L_SURF+1)
            L_SURF = L_SURF+1
          IGRSURF(ISU)%ELTYP(J) = ELTYP
          ELEM = ISURFI(L_SURF+1)
            L_SURF = L_SURF+1
          IGRSURF(ISU)%ELEM(J) = ELEM
        ENDDO
      ENDDO ! DO ISU=1,NSURF
!---------
      DEALLOCATE (ISURFI)
!---------
      RETURN
      END
