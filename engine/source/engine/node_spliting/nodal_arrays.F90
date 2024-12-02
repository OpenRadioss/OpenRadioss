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
            integer :: nrcvvois !< ALE ghost nodes in SPMD
            logical :: used_dr
            integer :: sicodt_fac !< size of ICODT


            integer :: numnod
            integer :: max_numnod
            integer, dimension(:), allocatable :: ITAB !< node user id , itabm1, max_id_user_globaux
            integer, dimension(:), allocatable :: ITABM1 !< node user id , itabm1, max_id_user_globaux
            integer, dimension(:), allocatable :: IKINE !< node kinematic id
            integer, dimension(:), allocatable :: WEIGHT !< node weight : 1 = owned by current proc, 0 = ghost
            integer, dimension(:), allocatable :: WEIGHT_MD !< r2r weight, but allways allocated
            integer, dimension(:), allocatable :: ICODT !< SICODT=NUMNOD+2*NUMNOD*MAX(IALE,IEULER,IALELAG)
            integer, dimension(:), allocatable :: ICODR !< NUMNOD * IRODDL
            integer, dimension(:), allocatable :: ISKEW
            integer, dimension(:), allocatable :: ICODE
            my_real, dimension(:,:), allocatable :: A !< accelerations: 3 x numnod (x nthreads if parith/off)
            my_real, dimension(:,:), allocatable :: AR !< accelerations
            my_real, dimension(:,:), allocatable :: V !< velocities
            my_real, dimension(:,:), allocatable :: X !< coordinates 3*(NUMNOD+NRCVVOIS)
            my_real, dimension(:,:), allocatable :: D !< displacements 3*(NUMNOD+NRCVVOIS)
            my_real, dimension(:,:), allocatable :: VR !<velocities 3*(NUMNOD*IRODDL)
            my_real, dimension(:,:), allocatable :: DR !< displacements (SRD) 
            my_real, dimension(:), allocatable :: MS !< mass     (numnod)          
            my_real, dimension(:), allocatable :: IN !< inertia * IRODDL
            my_real, dimension(:), allocatable :: STIFN !< nodal stiffness
            my_real, dimension(:), allocatable :: STIFR !< numnod*iroddl(* nthreads)
            my_real, dimension(:), allocatable :: MS0 !< initial mass
            my_real, dimension(:), allocatable :: IN0 !< initial inertia
            my_real, dimension(:), allocatable :: VISCN !< nodal 

            ! 3*NUMNOD if IRESP == 1, else 3
            double precision, dimension(:,:), allocatable :: DDP !< double precision D 
            double precision, dimension(:,:), allocatable :: XDP !< double precision X  
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
  subroutine allocate_nodal_arrays(arrays, numnod, nthreads, iroddl, iparith, &
           isecut, iisrot, impose_dr, idrot, nrcvvois, sicodt)
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
            integer, intent(in) :: isecut !< number of sections
            integer, intent(in) :: iisrot !< number of isotropic rotations
            integer, intent(in) :: impose_dr !< impose DR
            integer, intent(in) :: idrot !< number of discrete rotations
            integer, intent(in) :: nrcvvois !< ALE ghost nodes in SPMD
            integer, intent(in) :: sicodt !< SICODT=NUMNOD+2*NUMNOD*MAX(IALE,IEULER,IALELAG)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
            arrays%iroddl = iroddl
            arrays%iparith = iparith
            arrays%nthreads = nthreads
            arrays%max_numnod = numnod
            arrays%nrcvvois = nrcvvois
            arrays%sicodt_fac = 0
            if(numnod >0 ) arrays%sicodt_fac = sicodt / numnod
            IF(ISECUT > 0 .OR. IISROT > 0 .OR. IMPOSE_DR /= 0 .OR. IDROT > 0) THEN
              call my_alloc(arrays%DR,3,numnod*iroddl)
              arrays%used_dr = .true.
            else
              arrays%used_dr = .false.
              call my_alloc(arrays%DR,3,0)
            endif
            call my_alloc(arrays%itab,numnod)
            call my_alloc(arrays%IKINE,numnod)
            call my_alloc(arrays%ICODT,sicodt)
            call my_alloc(arrays%ICODR,numnod*iroddl)
            call my_alloc(arrays%V,3,numnod + nrcvvois)
            call my_alloc(arrays%X,3,numnod + nrcvvois)
            call my_alloc(arrays%D,3,numnod + nrcvvois)
            call my_alloc(arrays%VR,3,numnod*iroddl)
            call my_alloc(arrays%MS,numnod)
            call my_alloc(arrays%IN,numnod*iroddl)
            call my_alloc(arrays%MS0,numnod)
            call my_alloc(arrays%IN0,numnod*iroddl)
            call my_alloc(arrays%ISKEW,numnod)
            call my_alloc(arrays%ICODE,numnod)
#ifdef MYREAL4
            call my_alloc(arrays%DDP,3,numnod)
            call my_alloc(arrays%XDP,3,numnod)
#else
            call my_alloc(arrays%DDP,3,1)
            call my_alloc(arrays%XDP,3,1)
#endif
            call my_alloc(arrays%WEIGHT,numnod)
            call my_alloc(arrays%WEIGHT_MD,numnod)
            call my_alloc(arrays%ITABM1,2*numnod)

            if(iparith == 0) then
              call my_alloc(arrays%A,3,numnod*nthreads)
              call my_alloc(arrays%AR,3,numnod*nthreads)
              call my_alloc(arrays%STIFR,numnod*iroddl*nthreads)
              call my_alloc(arrays%VISCN,numnod*nthreads)
              call my_alloc(arrays%STIFN,numnod*nthreads)
            else                      
              call my_alloc(arrays%A,3,numnod)
              call my_alloc(arrays%AR,3,numnod)
              call my_alloc(arrays%STIFR,numnod)
              call my_alloc(arrays%VISCN,numnod)
              call my_alloc(arrays%STIFN,numnod)

            endif               
            arrays%numnod = numnod
            ! initialization to 0
            arrays%itab = 0
            arrays%IKINE = 0
            arrays%V = 0
            arrays%X = 0
            arrays%D = 0
            arrays%VR = 0
            arrays%DR = 0
            arrays%MS = 0
            arrays%IN = 0
            arrays%STIFN = 0
            arrays%MS0 = 0
            arrays%IN0 = 0
            arrays%ISKEW = 0
            arrays%ICODE = 0
            arrays%DDP = 0
            arrays%XDP = 0
            arrays%WEIGHT = 0
            arrays%WEIGHT_MD = 0
            arrays%ITABM1 = 0
            arrays%A = 0
            arrays%AR = 0
            arrays%STIFR = 0
            arrays%VISCN = 0
            arrays%ICODT = 0
            arrays%ICODR = 0

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
              call extend_array(arrays%V, 3, size(arrays%V, 2), 3, arrays%max_numnod + arrays%nrcvvois)
              call extend_array(arrays%X, 3, size(arrays%X, 2), 3, arrays%max_numnod + arrays%nrcvvois)
              call extend_array(arrays%D, 3, size(arrays%D, 2), 3, arrays%max_numnod + arrays%nrcvvois)
              call extend_array(arrays%iskew, size(arrays%iskew), arrays%max_numnod)
              arrays%iskew(arrays%numnod + 1:) = 0
              call extend_array(arrays%ICODE, size(arrays%ICODE), arrays%max_numnod)
              arrays%ICODE(arrays%numnod + 1:) = 0
              if(arrays%iroddl >0) then
                call extend_array(arrays%VR,3, size(arrays%VR,2), 3, arrays%max_numnod)
                call extend_array(arrays%IN, size(arrays%IN), arrays%max_numnod)
                call extend_array(arrays%IN0, size(arrays%IN0), arrays%max_numnod)
                call extend_array(arrays%ICODR, size(arrays%ICODR), arrays%max_numnod*arrays%iroddl)
              endif
              call extend_array(arrays%ICODT, size(arrays%ICODT), arrays%sicodt_fac * arrays%max_numnod) 
              arrays%ICODT(arrays%sicodt_fac * arrays%numnod + 1:) = 0
              if(arrays%used_dr) then
                call extend_array(arrays%DR,3, size(arrays%DR,2), 3, arrays%max_numnod)
              endif
              call extend_array(arrays%MS, size(arrays%MS), arrays%max_numnod)
              call extend_array(arrays%MS0, size(arrays%MS0), arrays%max_numnod)
#ifdef MYREAL4
              call extend_array(arrays%DDP,3, size(arrays%DDP,2), 3,arrays%max_numnod)
              call extend_array(arrays%XDP,3, size(arrays%XDP,2), 3,arrays%max_numnod)
#endif
              call extend_array(arrays%WEIGHT, size(arrays%WEIGHT), arrays%max_numnod)
              call extend_array(arrays%WEIGHT_MD, size(arrays%WEIGHT_MD), arrays%max_numnod)
              call extend_array(arrays%ITABM1, size(arrays%ITABM1), 2*arrays%max_numnod)
             
              if(arrays%iparith == 0) then
                call extend_array(arrays%A,3,size(arrays%A,2),3,arrays%max_numnod*arrays%nthreads)
                call extend_array(arrays%AR,3,size(arrays%AR,2),3,arrays%max_numnod*arrays%nthreads)
                call extend_array(arrays%STIFR,size(arrays%STIFR) ,arrays%max_numnod*arrays%iroddl*arrays%nthreads)
                call extend_array(arrays%VISCN,size(arrays%VISCN) ,arrays%max_numnod*arrays%nthreads)
                call extend_array(arrays%STIFN, size(arrays%STIFN), arrays%max_numnod*arrays%nthreads)
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
