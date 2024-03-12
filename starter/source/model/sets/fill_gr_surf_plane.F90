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
      module fill_surf_plane_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!\brief This subroutine creates a new igrsurf from a /SET of plane surface
!=======================================================================================================================
        subroutine fill_surf_plane(set,igrsurf,igrs,bufsf,lisurf1,nsurf)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use GROUPDEF_MOD
          use SETDEF_MOD
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(SURF_),                               intent(inout) :: igrsurf(nsurf+nsets)           !< group of surfaces structure
          type(SET_),                                intent(inout) :: set                            !< set group data structure
          integer,                                   intent(inout) :: igrs                           !< surface group increment
          integer,                                   intent(in) :: nsurf                             !< total number of model surfaces
          integer,                                   intent(in) :: lisurf1                           !< number of variable in surface buffer bufsf
          my_real,                                   intent(inout) :: bufsf(lisurf1*(nsurf+lisurf1)) !<surface real bufsf storage
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: nseg,iad
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!         
          nseg = set%nb_plane
!
          igrs = igrs + 1
!
          igrsurf(igrs)%id = set%set_id
          igrsurf(igrs)%title = set%title
!
          igrsurf(igrs)%type = 0
          igrsurf(igrs)%id_madymo = 0
          igrsurf(igrs)%iad_bufr = 0
          igrsurf(igrs)%nb_madymo = 0
          igrsurf(igrs)%type_madymo = 0
          igrsurf(igrs)%level = 1
          igrsurf(igrs)%th_surf = 0
          igrsurf(igrs)%ish4n3n = 0
          igrsurf(igrs)%nseg_r2r_all = 0
          igrsurf(igrs)%nseg_r2r_share = 0
          igrsurf(igrs)%iad_ige = 0
          igrsurf(igrs)%nseg_ige = 0
!         not printout empty group
          igrsurf(igrs)%set_group = 1
!
          igrsurf(igrs)%type = 200 
          iad=set%plane_iad_bufr
          igrsurf(igrs)%iad_bufr = iad
!
          bufsf(iad+1)=set%plane_xm
          bufsf(iad+2)=set%plane_ym
          bufsf(iad+3)=set%plane_zm
          bufsf(iad+4)=set%plane_xm1                                                
          bufsf(iad+5)=set%plane_ym1
          bufsf(iad+6)=set%plane_zm1
!
          set%set_nsurf_id = igrs
          set%has_surf_seg = nseg
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine fill_surf_plane
      end module fill_surf_plane_mod
