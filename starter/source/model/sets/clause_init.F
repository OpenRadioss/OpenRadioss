Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    clause_init      ../starter/source/model/sets/clause_init.F
!||--- called by ------------------------------------------------------
!||    hm_set           ../starter/source/model/sets/hm_set.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    message_mod      ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE CLAUSE_INIT(CLAUSE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SETDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      use my_dealloc_mod, only : my_dealloc
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (SET_) ::  CLAUSE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CLAUSE%NB_NODE= 0             ! CLAUSE nb of nodes
      CLAUSE%NB_PART= 0             ! CLAUSE nb of parts
      CLAUSE%NB_SOLID= 0            ! CLAUSE nb of solids
      CLAUSE%NB_QUAD= 0             ! CLAUSE nb of quads
      CLAUSE%NB_SH4N= 0             ! CLAUSE nb of sh4n
      CLAUSE%NB_SH3N= 0             ! CLAUSE nb of sh3n
      CLAUSE%NB_TRIA= 0             ! CLAUSE nb of trias
      CLAUSE%NB_BEAM= 0             ! CLAUSE nb of beams
      CLAUSE%NB_TRUSS= 0            ! CLAUSE nb of truss
      CLAUSE%NB_SPRING= 0           ! CLAUSE nb of spring
      CLAUSE%NB_SURF_SEG= 0         ! CLAUSE nb of surfacce's segments
      CLAUSE%NB_LINE_SEG= 0         ! CLAUSE nb of line/edge's segments
      CLAUSE%NB_RBODY= 0            ! CLAUSE nb of Rbodys
      CLAUSE%NB_SPHCEL= 0           ! CLAUSE nb of sphcel
      CLAUSE%EXT_ALL = 0

      CLAUSE%NB_ELLIPSE= 0          ! CLAUSE nb of Ellipses within a SET (max = 1, one per /SET)
      CLAUSE%ELLIPSE_IAD_BUFR= 0
      CLAUSE%ELLIPSE_ID_MADYMO=0
      CLAUSE%ELLIPSE_N=0
      CLAUSE%ELLIPSE_XC=0
      CLAUSE%ELLIPSE_YC=0
      CLAUSE%ELLIPSE_ZC=0
      CLAUSE%ELLIPSE_A=0
      CLAUSE%ELLIPSE_B=0
      CLAUSE%ELLIPSE_C=0

      CLAUSE%NB_PLANE= 0          ! CLAUSE nb of Planes within a SET (max = 1, one per /SET)
      CLAUSE%PLANE_IAD_BUFR= 0
      CLAUSE%PLANE_XM=0
      CLAUSE%PLANE_YM=0
      CLAUSE%PLANE_ZM=0
      CLAUSE%PLANE_XM1=0
      CLAUSE%PLANE_YM1=0
      CLAUSE%PLANE_ZM1=0

      CLAUSE%NB_NODENS= 0         ! CLAUSE nb of NODENS -> unsortable node list
!---
      IF(ALLOCATED( CLAUSE%NODE )) call my_dealloc(CLAUSE%NODE)
      IF(ALLOCATED( CLAUSE%PART )) call my_dealloc(CLAUSE%PART)
      IF(ALLOCATED( CLAUSE%SOLID )) call my_dealloc(CLAUSE%SOLID)
      IF(ALLOCATED( CLAUSE%QUAD )) call my_dealloc(CLAUSE%QUAD)
      IF(ALLOCATED( CLAUSE%SH4N )) call my_dealloc(CLAUSE%SH4N)
      IF(ALLOCATED( CLAUSE%SH3N )) call my_dealloc(CLAUSE%SH3N)
      IF(ALLOCATED( CLAUSE%TRIA )) call my_dealloc(CLAUSE%TRIA)
      IF(ALLOCATED( CLAUSE%BEAM )) call my_dealloc(CLAUSE%BEAM)
      IF(ALLOCATED( CLAUSE%TRUSS )) call my_dealloc(CLAUSE%TRUSS)
      IF(ALLOCATED( CLAUSE%SPRING )) call my_dealloc(CLAUSE%SPRING)
      IF(ALLOCATED( CLAUSE%SURF_NODES )) call my_dealloc(CLAUSE%SURF_NODES)
      IF(ALLOCATED( CLAUSE%SURF_ELTYP )) call my_dealloc(CLAUSE%SURF_ELTYP)
      IF(ALLOCATED( CLAUSE%SURF_ELEM )) call my_dealloc(CLAUSE%SURF_ELEM)
      IF(ALLOCATED( CLAUSE%LINE_NODES )) call my_dealloc(CLAUSE%LINE_NODES)
      IF(ALLOCATED( CLAUSE%LINE_ELTYP )) call my_dealloc(CLAUSE%LINE_ELTYP)
      IF(ALLOCATED( CLAUSE%LINE_ELEM )) call my_dealloc(CLAUSE%LINE_ELEM)
      IF(ALLOCATED( CLAUSE%RBODY )) call my_dealloc(CLAUSE%RBODY)
      IF(ALLOCATED( CLAUSE%ELLIPSE_SKEW )) call my_dealloc(CLAUSE%ELLIPSE_SKEW)
      IF(ALLOCATED( CLAUSE%NODENS )) call my_dealloc(CLAUSE%NODENS)
      IF(ALLOCATED( CLAUSE%SPHCEL )) call my_dealloc(CLAUSE%SPHCEL)
C-----
      RETURN
      END
