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
      module nodal_arrays_mod
#include "my_real.inc"
        implicit none
        integer, parameter :: padding = 5 !< percentage of padding for reallocation

        type nodal_arrays_
            integer :: iroddl
            integer :: iparith
            integer :: nthreads
            integer :: numnod
            integer :: max_numnod
            integer, dimension(:), allocatable :: itab !< node user id , itabm1, max_id_user_globaux
            integer, dimension(:), allocatable :: IKINE !< node kinematic id
            integer, dimension(:), allocatable :: WEIGHT !< node weight : 1 = owned by current proc, 0 = ghost
            my_real, dimension(:,:), allocatable :: A !< accelerations: 3 x numnod (x nthreads if parith/off)
            my_real, dimension(:,:), allocatable :: AR !< accelerations
            my_real, dimension(:,:), allocatable :: V !< velocities
            my_real, dimension(:,:), allocatable :: X !< coordinates
            my_real, dimension(:,:), allocatable :: D !< displacements
            my_real, dimension(:,:), allocatable :: VR !<velocities
            my_real, dimension(:,:), allocatable :: DR !< displacements
            my_real, dimension(:), allocatable :: MS !< mass               
            my_real, dimension(:), allocatable :: IN !< inertia
            my_real, dimension(:), allocatable :: STIFN !< nodal stiffness
            my_real, dimension(:), allocatable :: STIFR !< numnod*iroddl(* nthreads)
            my_real, dimension(:), allocatable :: MS0 !< initial mass
            my_real, dimension(:), allocatable :: IN0 !< initial inertia
            my_real, dimension(:), allocatable :: VISCN !< nodal  3

            double precision, dimension(:), allocatable :: DDP !< double precision D 
            double precision, dimension(:), allocatable :: XDP !< double precision X  
        end type nodal_arrays_
 ! faire la rupture vers checkstfn
!       type animation_buffers
!           ! pas obligatoire si pas de ANIM/VECT :
!           integer :: current_numnod
!           integer :: max_numnod
!      ! animation SFANI, SANIN, STANI
!           my_real, dimension(3,:), allocatable :: FANI
!           my_real, dimension(:), allocatable :: ANIN
!           my_real, dimension(:), allocatable :: TANI
!       end type
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Allocate nodal arrays                                                              
        subroutine allocate_nodal_arrays(arrays, numnod, nthreads, iroddl, iparith)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use my_alloc_mod, only: my_alloc
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
            type(nodal_arrays_) :: arrays
            integer, intent(in) :: numnod !< number of nodes
            integer, intent(in) :: nthreads !< number of OpenMP threads
            integer, intent(in) :: iroddl !< number of degrees of freedom per node
            integer, intent(in) :: iparith !< numerical reproducibility (/PARITH option) (0: off, 1: on)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
            arrays%iroddl = iroddl
            arrays%iparith = iparith
            arrays%nthreads = nthreads
            arrays%max_numnod = numnod
            call my_alloc(arrays%itab,numnod)
            call my_alloc(arrays%IKINE,numnod)
            call my_alloc(arrays%V,3,numnod)
            call my_alloc(arrays%X,3,numnod)
            call my_alloc(arrays%D,3,numnod)
            call my_alloc(arrays%VR,3,numnod)
            call my_alloc(arrays%DR,3,numnod)
            call my_alloc(arrays%MS,numnod)
            call my_alloc(arrays%IN,numnod)
            call my_alloc(arrays%STIFN,numnod)
            call my_alloc(arrays%MS0,numnod)
            call my_alloc(arrays%IN0,numnod)
            call my_alloc(arrays%DDP,numnod)
            call my_alloc(arrays%XDP,numnod)
            call my_alloc(arrays%WEIGHT,numnod)

            if(iparith == 0) then
              call my_alloc(arrays%A,3,numnod*nthreads)
              call my_alloc(arrays%AR,3,numnod*nthreads)
              call my_alloc(arrays%STIFR,numnod*iroddl*nthreads)
              call my_alloc(arrays%VISCN,numnod*nthreads)
            else                      
              call my_alloc(arrays%A,3,numnod)
              call my_alloc(arrays%AR,3,numnod)
              call my_alloc(arrays%STIFR,numnod)
              call my_alloc(arrays%VISCN,numnod)
            endif               
            arrays%numnod = numnod
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine allocate_nodal_arrays
!! \brief extend nodal arrays                                                              
        subroutine extend_nodal_arrays(arrays, numnod)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
            use extend_array_mod, only: extend_array
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
            type(nodal_arrays_) :: arrays
            integer, intent(in) :: numnod !< new number of nodes
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
            arrays%numnod = numnod
            if(numnod > arrays%max_numnod) then
              arrays%max_numnod = int(numnod * (1.0D0 + padding/100.0D0)) 
              arrays%max_numnod = max(arrays%max_numnod, numnod+1)
              call extend_array(arrays%itab,size(arrays%itab), arrays%max_numnod)
              call extend_array(arrays%IKINE, size(arrays%IKINE), arrays%max_numnod)
              call extend_array(arrays%V, 3, size(arrays%V, 2), 3, arrays%max_numnod)
              call extend_array(arrays%X, 3, size(arrays%X, 2), 3, arrays%max_numnod)
              call extend_array(arrays%D, 3, size(arrays%D, 2), 3, arrays%max_numnod)
              call extend_array(arrays%VR,3, size(arrays%VR,2), 3, arrays%max_numnod)
              call extend_array(arrays%DR,3, size(arrays%DR,2), 3, arrays%max_numnod)
              call extend_array(arrays%MS, size(arrays%MS), arrays%max_numnod)
              call extend_array(arrays%IN, size(arrays%IN), arrays%max_numnod)
              call extend_array(arrays%STIFN, size(arrays%STIFN), arrays%max_numnod)
              call extend_array(arrays%MS0, size(arrays%MS0), arrays%max_numnod)
              call extend_array(arrays%IN0, size(arrays%IN0), arrays%max_numnod)
              call extend_array(arrays%DDP, size(arrays%DDP), arrays%max_numnod)
              call extend_array(arrays%XDP, size(arrays%XDP), arrays%max_numnod)
              call extend_array(arrays%WEIGHT, size(arrays%WEIGHT), arrays%max_numnod)
             
              if(arrays%iparith == 0) then
                call extend_array(arrays%A,3,size(arrays%A,2),3,arrays%max_numnod*arrays%nthreads)
                call extend_array(arrays%AR,3,size(arrays%AR,2),3,arrays%max_numnod*arrays%nthreads)
                call extend_array(arrays%STIFR,size(arrays%STIFR) ,arrays%max_numnod*arrays%iroddl*arrays%nthreads)
                call extend_array(arrays%VISCN,size(arrays%VISCN) ,arrays%max_numnod*arrays%nthreads)
              else                      
                call extend_array(arrays%A,3,size(arrays%A,2),3,arrays%max_numnod)
                call extend_array(arrays%AR,3,size(arrays%AR,2),3,arrays%max_numnod)
                call extend_array(arrays%STIFR,size(arrays%STIFR) ,arrays%max_numnod)
                call extend_array(arrays%VISCN,size(arrays%VISCN) ,arrays%max_numnod)
              endif               
            endif
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine extend_nodal_arrays


      end module nodal_arrays_mod 
