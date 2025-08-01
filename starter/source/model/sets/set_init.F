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
!||    set_init               ../starter/source/model/sets/set_init.F
!||--- called by ------------------------------------------------------
!||    hm_set                 ../starter/source/model/sets/hm_set.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE SET_INIT(
     .                    SET     ,IGS  ,SET_ID, SET_TITLE, ISET_TYPE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
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
      INTEGER IGS,SET_ID,ISET_TYPE
      CHARACTER(LEN=NCHARTITLE)::SET_TITLE
C-----------------------------------------------
      TYPE (SET_), DIMENSION(NSETS)   :: SET
C-----------------------------------------------
!
      SET(IGS)%SET_ID = SET_ID
      SET(IGS)%TITLE  = SET_TITLE
      SET(IGS)%SET_TYPE  = ISET_TYPE
      SET(IGS)%EXT_ALL = 0 ! type of surface : /EXT or /ALL
!
!      SET(IGS)%SET_ACTIV = 1         ! SET_ACTIV was already initialized in sort_sets.F, 
                                      ! this permits to treat the /SET/COLLECT, in /SET/COLLECT, only one is active,
                                      ! the others are as dependency
      SET(IGS)%SURF_SET_ID = 0
      SET(IGS)%LINE_SET_ID = 0
      SET(IGS)%SURF_SET_FLAG = 0
      SET(IGS)%LINE_SET_FLAG = 0
!
      SET(IGS)%SET_GRNOD_ID= 0        ! SET grnod identifier
      SET(IGS)%SET_GRPART_ID= 0       ! SET grpart identifier
      SET(IGS)%SET_GRSOLID_ID= 0      ! SET grsolid identifier
      SET(IGS)%SET_GRSH4N_ID= 0       ! SET grsh4n identifier
      SET(IGS)%SET_GRSH3N_ID= 0       ! SET grsh3n identifier
      SET(IGS)%SET_GRQUAD_ID= 0       ! SET grquad identifier
      SET(IGS)%SET_GRTRIA_ID= 0       ! SET grtria identifier
      SET(IGS)%SET_GRBEAM_ID= 0       ! SET grbeam identifier
      SET(IGS)%SET_GRTRUSS_ID= 0      ! SET grtruss identifier
      SET(IGS)%SET_GRSPRING_ID= 0     ! SET grspring identifier
      SET(IGS)%SET_NSURF_ID= 0        ! SET grsurf identifier
      SET(IGS)%SET_NSLIN_ID= 0        ! SET grslin identifier
!
      SET(IGS)%NB_NODE= 0             ! SET nb of nodes
      SET(IGS)%NB_PART= 0             ! SET nb of parts
      SET(IGS)%NB_SOLID= 0            ! SET nb of solids
      SET(IGS)%NB_SH4N= 0             ! SET nb of sh4n
      SET(IGS)%NB_SH3N= 0             ! SET nb of sh3n
      SET(IGS)%NB_QUAD= 0             ! SET nb of quads
      SET(IGS)%NB_TRIA= 0             ! SET nb of trias
      SET(IGS)%NB_BEAM= 0             ! SET nb of beams
      SET(IGS)%NB_TRUSS= 0            ! SET nb of truss
      SET(IGS)%NB_SPRING= 0           ! SET nb of spring
      SET(IGS)%NB_SURF_SEG= 0         ! SET nb of surfacce's segments
      SET(IGS)%NB_LINE_SEG= 0         ! SET nb of line/edge's segments
      SET(IGS)%HAS_SURF_SEG= 0        ! Flag if a surface was created from surface (QA Print)
      SET(IGS)%HAS_LINE_SEG= 0        ! Flag if a line was created from surface (QA Print)
      SET(IGS)%NB_ELLIPSE=0           ! Flag nb of ellipses (one per /SET)
      SET(IGS)%NB_NODENS= 0           ! SET nb of nodens
      SET(IGS)%NB_SPHCEL= 0           ! SET nb of sphcel

      SET(IGS)%NB_ELLIPSE= 0          ! SET nb of Ellipses (max = 1, one per /SET)
      SET(IGS)%ELLIPSE_IAD_BUFR= 0    ! surface buffer adress in SET
      SET(IGS)%ELLIPSE_ID_MADYMO=0    ! ID of Madymo surface
      SET(IGS)%ELLIPSE_N=0            ! Degree of the hyper-ellipsoid
      SET(IGS)%ELLIPSE_XC=0           ! Center coordinate X of the hyper-ellipsoid
      SET(IGS)%ELLIPSE_YC=0           ! Center coordinate Y of the hyper-ellipsoid
      SET(IGS)%ELLIPSE_ZC=0           ! Center coordinate Z of the hyper-ellipsoid
      SET(IGS)%ELLIPSE_A=0            ! Semi-axis length along X direction of the hyper-ellipsoid
      SET(IGS)%ELLIPSE_B=0            ! Semi-axis length along Y direction of the hyper-ellipsoid
      SET(IGS)%ELLIPSE_C=0            ! Semi-axis length along Z direction of the hyper-ellipsoid

      SET(IGS)%NB_PLANE= 0            ! SET nb of Planes (max = 1, one per /SET)
      SET(IGS)%PLANE_IAD_BUFR= 0      ! surface buffer adress in SET
      SET(IGS)%PLANE_XM=0             ! X coordinate of M (end of normal vector) in PLANE surface
      SET(IGS)%PLANE_YM=0             ! Y coordinate of M (end of normal vector) in PLANE surface
      SET(IGS)%PLANE_ZM=0             ! Z coordinate of M (end of normal vector) in PLANE surface
      SET(IGS)%PLANE_XM1=0            ! X coordinate of M1 (head of normal vector) in PLANE surface
      SET(IGS)%PLANE_YM1=0            ! Y coordinate of M1 (head of normal vector) in PLANE surface
      SET(IGS)%PLANE_ZM1=0            ! Z coordinate of M1 (head of normal vector) in PLANE surface
C-----
      RETURN
      END
