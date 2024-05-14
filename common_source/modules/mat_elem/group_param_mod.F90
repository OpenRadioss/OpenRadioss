!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!hd|====================================================================
!hd|  GROUP_PARAM_MOD               modules/mat_elem/group_param_mod.F
!hd|-- called by -----------
!hd|        MAT_ELEM_MOD                  common_source/modules/mat_elem/mat_elem_mod.F
!hd|        C3DERII                       starter/source/elements/sh3n/coque3n/c3derii.F
!hd|        C3INIT3                       starter/source/elements/sh3n/coque3n/c3init3.F
!hd|        CBAINIT3                      starter/source/elements/shell/coqueba/cbainit3.F
!hd|        CDKDERII                      starter/source/elements/sh3n/coquedk/cdkderii.F
!hd|        CDKINIT3                      starter/source/elements/sh3n/coquedk/cdkinit3.F
!hd|        CDLENI                        starter/source/elements/shell/coque/cdleni.F
!hd|        CINIT3                        starter/source/elements/shell/coque/cinit3.F
!hd|        CNDLENI                       starter/source/elements/shell/coqueba/cndleni.F
!hd|        INIRIG_MAT                    starter/source/elements/initia/inirig_mat.F
!hd|        INITIA                        starter/source/elements/initia/initia.F
!hd|        OUTPART5                      starter/source/elements/initia/initia.F
!hd|        SET_ELGROUP_PARAM             starter/source/elements/shell/coque/set_elgroup_param.F
!hd|        WRITE_ELGROUP_PARAM           starter/source/restart/ddsplit/write_elgroup_param.F
!hd|        CMAIN3PINCH                   engine/source/elements/shell/coqueba/cmain3pinch.F
!hd|        WRITE_ELGROUP_PARAM           engine/source/output/restart/write_elgroup_param.F
!hd|-- calls ---------------
!hd|====================================================================
      MODULE GROUP_PARAM_MOD
!-----------------------------------------------------------------------
#include "my_real.inc"
!=======================================================================
! define type GROUP_PARAM_STRUCT_ for element group parameters depending on property/material
! allocatable dimension : NGROUP
!=======================================================================
!      INTEGER ,PARAMETER  :: IPARG_X =
!      INTEGER ,PARAMETER  :: IPARG_Y =
!      INTEGER ,PARAMETER  :: IPARG_Z =

!      INTEGER ,PARAMETER  :: NPARG  = 100
!
        TYPE GROUP_PARAM_               ! (NGROUP)
          integer     :: IMAT
          integer     :: IPROP
          integer     :: NPARG
          my_real     :: VISC_DN
          my_real     :: VISC_DM
          integer     :: ISMSTR

          integer ,DIMENSION(:) ,ALLOCATABLE :: IPARG  ! size = NPARG

        END TYPE GROUP_PARAM_
!
!---------------
      END MODULE GROUP_PARAM_MOD
