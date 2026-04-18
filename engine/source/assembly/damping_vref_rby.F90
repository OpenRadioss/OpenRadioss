!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||====================================================================
!||    damping_vref_rby_mod   ../engine/source/assembly/damping_vref_rby.F90
!||--- called by ------------------------------------------------------
!||    rbyfor                 ../engine/source/constraints/general/rbody/rbyfor.F
!||====================================================================
      module damping_vref_rby_mod
      implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine computes damping forces for /DAMP/VREL with RBODY
!=======================================================================================================================
!
!||====================================================================
!||    damping_vref_rby                 ../engine/source/assembly/damping_vref_rby.F90
!||--- called by ------------------------------------------------------
!||    rbyfor                           ../engine/source/constraints/general/rbody/rbyfor.F
!||--- calls      -----------------------------------------------------
!||    damping_vref_compute_dampa       ../engine/source/assembly/damping_vref_compute_dampa.F90
!||    damping_vref_sum6_rby            ../engine/source/assembly/damping_vref_sum6_rby.F90
!||    get_u_func                       ../engine/source/user_interface/ufunc.F
!||--- uses       -----------------------------------------------------
!||    constant_mod                     ../common_source/modules/constant_mod.F
!||    damping_vref_compute_dampa_mod   ../engine/source/assembly/damping_vref_compute_dampa.F90
!||    damping_vref_sum6_rby_mod        ../engine/source/assembly/damping_vref_sum6_rby.F90
!||    groupdef_mod                     ../common_source/modules/groupdef_mod.F
!||    precision_mod                    ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine damping_vref_rby(igrnod,ngrnod,v,vr,a,                             &
          x,ms,dampr,nrdamp,ndamp,                          &
          ndamp_vrel,iparit,numnod,dt1,id_damp_vrel,        &
          tt,nnpby,nrbykin,npby,rby6,                       &
          rby6_c,tagslv_rby,weight,lskew,numskw,            &
          dim,damp,skew,wfext,size_rby6_c,nhi)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use GROUPDEF_MOD , only: GROUP_
          use constant_mod , only: pi,one,zero,two,half,em20
          use damping_vref_sum6_rby_mod
          use damping_vref_compute_dampa_mod
          use precision_mod , only: WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(GROUP_),                              intent(in) :: igrnod(ngrnod)              !< group od nodes structure
          integer,                                   intent(in) :: ngrnod                      !< number of groups of nodes
          integer,                                   intent(in) :: ndamp                       !< number of /DAMP
          integer,                                   intent(in) :: nrdamp                      !< first dimension of array DAMP
          integer,                                   intent(in) :: ndamp_vrel                  !< number of /DAMP/VREL
          integer,                                   intent(in) :: numnod                      !< number of nodes
          integer,                                   intent(in) :: id_damp_vrel(ndamp_vrel)    !< id of /DAMP/VREL in DAMP array
          integer,                                   intent(in) :: nnpby                       !< first dimension of array NPBY
          integer,                                   intent(in) :: nrbykin                     !< number of rigid bodies
          integer,                                   intent(in) :: npby(nnpby,nrbykin)         !< main structure for rigid bodies
          integer,                                   intent(in) :: tagslv_rby(numnod)          !< tag of slaves nodes of rigid bodies
          integer,                                   intent(in) :: iparit                      !< PARITH/ON flag
          integer,                                   intent(in) :: weight(numnod)              !< weight
          integer,                                   intent(in) :: lskew                       !< first dimension of array skew
          integer,                                   intent(in) :: numskw                      !< number of skews
          integer,                                   intent(in) :: dim                         !< first dimension of array damp
          integer,                                   intent(in) :: size_rby6_c                 !< dimension of array rby6c
          integer,                                   intent(in) :: nhi                         !< hierarchy level of Rbody
          real(kind=WP),                                   intent(in) :: v(3,numnod)                 !< nodal velocity
          real(kind=WP),                                   intent(in) :: vr(3,numnod)                !< nodal rotational velocity
          real(kind=WP),                                intent(inout) :: a(3,numnod)                 !< nodal force
          real(kind=WP),                                   intent(in) :: x(3,numnod)                 !< node position
          real(kind=WP),                                   intent(in) :: ms(numnod)                  !< nodal mass
          real(kind=WP),                                intent(inout) :: dampr(nrdamp,ndamp)         !< main structure for option /DAMP
          real(kind=WP),                                   intent(in) :: dt1                         !< time step
          real(kind=WP),                                   intent(in) :: tt                          !< current time
          real(kind=WP),                                intent(inout) :: damp(dim,numnod)            !< damping force at previous time step
          real(kind=WP),                                   intent(in) :: skew(lskew,numskw)          !< main structure for skews
          double precision,                       intent(inout) :: wfext                       !< external forces work
          double precision,                       intent(inout) :: rby6(8,6,nrbykin)           !< working array for rigid body assembly
          double precision,                       intent(inout) :: rby6_c(2,6,size_rby6_c)     !< working array for rigid body damping assembly
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: nd,id,igr,isk,id_rby,id_func,im,nsn
          real(kind=WP) :: damp_a(3),damp_a2(3),get_u_func,t_start,t_stop
          double precision :: dw
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
          external get_u_func
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
          rby6_c(1:2,1:6,1:size_rby6_c) = zero
          dw = zero
!
          do nd=1,ndamp_vrel
!
            id = id_damp_vrel(nd)
            id_rby = nint(dampr(25,id))
            t_start = dampr(17,id)
            t_stop  = dampr(18,id)
!
            if ((id_rby > 0).and.(tt>=t_start).and.(tt<=t_stop)) then
              if (npby(20,id_rby)/=nhi) cycle
              igr   = nint(dampr(2,id))
              isk   = nint(dampr(15,id))
              id_func = nint(dampr(26,id))
              im = npby(1,id_rby)
              nsn = igrnod(igr)%nentity
!
!             computation of damping parameters - function of time
              call damping_vref_compute_dampa(id,ndamp,nrdamp,dampr,dt1,tt,damp_a)
!
              damp_a2(1)  = dampr(22,id)
              damp_a2(2)  = dampr(23,id)
              damp_a2(3)  = dampr(24,id)
!
              call damping_vref_sum6_rby(nsn,igr,id_rby,isk,im,                  &
                igrnod,ngrnod,v,vr,a,                   &
                x,ms,dt1,numnod,tagslv_rby,           &
                nrbykin,rby6,rby6_c,weight,lskew,       &
                numskw,skew,damp_a,dim,damp,            &
                dw,damp_a2,iparit,size_rby6_c)
!
            end if
!
          end do
!
          wfext = wfext + dw
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine damping_vref_rby
      end module damping_vref_rby_mod
