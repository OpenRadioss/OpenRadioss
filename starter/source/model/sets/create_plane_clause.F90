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
      module create_plane_clause_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!\brief This subroutine creates a clause from a plane surface
!=======================================================================================================================
        subroutine create_plane_clause(id,title,sub_id,clause,lsubmodel,unitab,iad,nrtrans,ntransf,rtrans)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use SETDEF_MOD
          use SUBMODEL_MOD
          use MESSAGE_MOD
          use HM_OPTION_READ_MOD
          use UNITAB_MOD
          use MY_ALLOC_MOD
          use NAMES_AND_TITLES_MOD
          use CONSTANT_MOD
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
          integer,                                   intent(inout) :: iad                            !< surface buffer address
          integer,                                   intent(in) :: ID                                !< set ID
          integer,                                   intent(in) :: sub_id                            !< sub-set ID
          integer,                                   intent(in) :: ntransf                           !< first dimension of transformation array
          integer,                                   intent(in) :: nrtrans                           !< second dimension of transformation array
          character(len=nchartitle),                 intent(in) :: title                             !< set title
          my_real,                                   intent(in) :: rtrans(ntransf,nrtrans)           !< transformation storage array
          type(SET_),                                intent(inout) :: clause                         !< clause of set
          type(UNIT_TYPE_),                          intent(in) :: unitab                            !< unit table 
          type(SUBMODEL_DATA),                       intent(in) :: lsubmodel(nsubmod)              !< submodel storage array
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          my_real :: xm,ym,zm                 !< coordinates of end of the normal vector to planar surface
          my_real :: xm1,ym1,zm1              !< coordinates of head of the normal vector to planar surface
          my_real :: vectx, vecty,vectz       !< components of normal vestor to planar surface
          my_real :: vect                     !< normal to the planar surface
          logical :: is_available             !< logical syntax to check the available option
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
          xm = ZERO
          ym = ZERO
          zm = ZERO
          xm1 = ZERO
          ym1 = ZERO
          zm1 = ZERO
!
          call hm_get_floatv('XM' ,xm,is_available,lsubmodel,unitab)                                                            
          call hm_get_floatv('YM' ,ym,is_available,lsubmodel,unitab)                                                            
          call hm_get_floatv('ZM' ,zm,is_available,lsubmodel,unitab)
          if (sub_id /= 0) call subrotpoint(xm,ym,zm,rtrans,sub_id,lsubmodel)
!
          call hm_get_floatv  ('XM1' ,xm1,is_available,lsubmodel,unitab)                                                            
          call hm_get_floatv  ('YM1' ,ym1,is_available,lsubmodel,unitab)                                                            
          call hm_get_floatv  ('ZM1' ,zm1,is_available,lsubmodel,unitab)                                                            
          if (sub_id /= 0) call subrotpoint(xm1,ym1,zm1,rtrans,sub_id,lsubmodel)
!
!         Normal Vector
          vectx = (xm1-xm)*(xm1-xm)
          vecty = (ym1-ym)*(ym1-ym)
          vectz = (zm1-zm)*(zm1-zm)
          vect  = SQRT(vectx+vecty+vectz)
!
          if(vect <= EM10)call ancmsg(msgid=891,msgtype=msgerror,anmode=aninfo,i1=id,c1=title)
!
!         Copy in final SET
!
!         Create SURF clause
!
          clause%nb_plane=1 ! only one PLANE per /SET
          clause%plane_iad_bufr = iad ! Analytical Surfaces address (reals BUFSF - temp)
          clause%plane_xm=xm
          clause%plane_ym=ym
          clause%plane_zm=zm
          clause%plane_xm1=xm1
          clause%plane_ym1=ym1
          clause%plane_zm1=zm1
!
          iad=iad+6
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine create_plane_clause
      end module create_plane_clause_mod
