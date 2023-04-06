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
Chd|  ISURF_INI                     source/model/group/isurf_ini.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE ISURF_INI(IGRSURF)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(SURF_)   ,DIMENSION(NSURF) :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER L_SURF,ISU,ID,NSEG,TYPE,ID_MADYMO,IAD_BUFR,
     .        NB_MADYMO,TYPE_MADYMO,LEVEL,TH_SURF,ISH4N3N,NSEG_R2R_ALL,
     .        NSEG_R2R_SHARE
C-----------------------------------------------
!---
!  Size of ISURF structure
!---
!
      L_SURF = 0
      DO ISU=1,NSURF
! variables
        ID        = IGRSURF(ISU)%ID
            L_SURF = L_SURF + 1
        NSEG      = IGRSURF(ISU)%NSEG
            L_SURF = L_SURF + 1
        TYPE      = IGRSURF(ISU)%TYPE
            L_SURF = L_SURF + 1
        ID_MADYMO = IGRSURF(ISU)%ID_MADYMO
            L_SURF = L_SURF + 1
        IAD_BUFR  = IGRSURF(ISU)%IAD_BUFR
            L_SURF = L_SURF + 1
        NB_MADYMO = IGRSURF(ISU)%NB_MADYMO
            L_SURF = L_SURF + 1
        TYPE_MADYMO = IGRSURF(ISU)%TYPE_MADYMO
            L_SURF = L_SURF + 1
        LEVEL     = IGRSURF(ISU)%LEVEL
            L_SURF = L_SURF + 1
        TH_SURF   = IGRSURF(ISU)%TH_SURF
            L_SURF = L_SURF + 1
        ISH4N3N   = IGRSURF(ISU)%ISH4N3N
            L_SURF = L_SURF + 1
        NSEG_R2R_ALL = IGRSURF(ISU)%NSEG_R2R_ALL
            L_SURF = L_SURF + 1
        NSEG_R2R_SHARE = IGRSURF(ISU)%NSEG_R2R_SHARE
            L_SURF = L_SURF + 1
!
          L_SURF = L_SURF + 4*NSEG  ! nodes of segment
          L_SURF = L_SURF + NSEG    ! ELTYP of segment
          L_SURF = L_SURF + NSEG    ! ELEM of segment
!
!!        IF ( NSEG_IGE > 0) THEN
!!          L_SURF = L_SURF + 4*9*NSEG_IGE  ! nodes of segment
!!          L_SURF = L_SURF + 1*9           ! ELTYP
!!          L_SURF = L_SURF + 1*9           ! ELEM
!!        ENDIF ! IF ( NSEG_IGE > 0)
      ENDDO ! DO ISU=1,NSURF
!
!---------
      LENISURF = L_SURF
!---------
      RETURN
      END
