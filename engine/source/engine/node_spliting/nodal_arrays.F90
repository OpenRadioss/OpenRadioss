!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||    nodal_arrays_mod                         ../engine/source/engine/node_spliting/nodal_arrays.F90
      !||--- called by ------------------------------------------------------
      !||    asspar4                                  ../engine/source/assembly/asspar4.F
      !||    check_nan_acc                            ../engine/source/output/outfile/check_nan_acc.F
      !||    chkload                                  ../engine/source/interfaces/chkload.F
      !||    chkstfn3n                                ../engine/source/interfaces/interf/chkstfn3.F
      !||    count_remote_nb_elem_edge                ../engine/source/interfaces/interf/count_remote_nb_elem_edge.F
      !||    detach_node                              ../engine/source/engine/node_spliting/detach_node.F90
      !||    detach_node_from_interfaces              ../engine/source/engine/node_spliting/detach_node.F90
      !||    detach_node_from_shells                  ../engine/source/engine/node_spliting/detach_node.F90
      !||    find_edge_from_remote_proc               ../engine/source/interfaces/interf/find_edge_from_remote_proc.F
      !||    find_surface_from_remote_proc            ../engine/source/interfaces/interf/find_surface_from_remote_proc.F
      !||    fixvel                                   ../engine/source/constraints/general/impvel/fixvel.F
      !||    force                                    ../engine/source/loads/general/force.F90
      !||    funct_python_update_elements             ../engine/source/tools/curve/funct_python_update_elements.F90
      !||    get_neighbour_surface                    ../engine/source/interfaces/interf/get_neighbour_surface.F90
      !||    get_neighbour_surface_from_remote_proc   ../engine/source/interfaces/interf/get_neighbour_surface_from_remote_proc.F90
      !||    init_nodal_state                         ../engine/source/interfaces/interf/init_nodal_state.F
      !||    python_call_funct_cload_dp               ../engine/source/loads/general/python_call_funct_cload.F90
      !||    python_call_funct_cload_sp               ../engine/source/loads/general/python_call_funct_cload.F90
      !||    radioss2                                 ../engine/source/engine/radioss2.F
      !||    rbe3t1                                   ../engine/source/constraints/general/rbe3/rbe3f.F
      !||    rbe3v                                    ../engine/source/constraints/general/rbe3/rbe3v.F
      !||    rdresb                                   ../engine/source/output/restart/rdresb.F
      !||    resol                                    ../engine/source/engine/resol.F
      !||    resol_head                               ../engine/source/engine/resol_head.F
      !||    restalloc                                ../engine/source/output/restart/arralloc.F
      !||    set_new_node_values                      ../engine/source/engine/node_spliting/detach_node.F90
      !||    spmd_exch_deleted_surf_edge              ../engine/source/mpi/interfaces/spmd_exch_deleted_surf_edge.F
      !||    spmd_exch_neighbour_segment              ../engine/source/mpi/interfaces/spmd_exch_neighbour_segment.F90
      !||    spmd_exchmsr_idel                        ../engine/source/mpi/interfaces/spmd_exchmsr_idel.F
      !||    spmd_exchseg_idel                        ../engine/source/mpi/kinematic_conditions/spmd_exchseg_idel.F
      !||    tagoff3n                                 ../engine/source/interfaces/interf/chkstfn3.F
      !||    test_jc_shell_detach                     ../engine/source/engine/node_spliting/detach_node.F90
      !||    user_interface_mod                       ../engine/source/modules/user_interface_mod.F90
      !||    wrrestp                                  ../engine/source/output/restart/wrrestp.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      module nodal_arrays_mod
#include "my_real.inc"
        use iso_c_binding, only: C_PTR
        implicit none
        integer, parameter :: padding = 5 !< percentage of padding for reallocation

        type nodal_arrays_
            type(C_PTR) :: loc2glob
            integer :: iroddl
            integer :: iparith
            integer :: nthreads
            integer :: nrcvvois !< ALE ghost nodes in SPMD
            logical :: used_dr
            integer :: sicodt_fac !< size of ICODT
            integer :: max_uid !< maximum user id
            integer :: itherm_fe
            integer :: numnod0


            integer :: numnod
            integer :: max_numnod
            integer, dimension(:), allocatable :: ITAB !< node user id, max_id_user_globaux
            integer, dimension(:), allocatable :: ITABM1 !< node user id , itabm1, max_id_user_globaux
            integer, dimension(:), allocatable :: IKINE !< node kinematic id
            integer, dimension(:), allocatable :: MAIN_PROC
            integer, dimension(:), allocatable :: WEIGHT !< node weight : 1 = owned by current proc, 0 = ghost
            integer, dimension(:), allocatable :: WEIGHT_MD !< r2r weight, but always allocated
            integer, dimension(:), allocatable :: ICODT !< SICODT=NUMNOD+2*NUMNOD*MAX(IALE,IEULER,IALELAG)
            integer, dimension(:), allocatable :: ICODR !< NUMNOD * IRODDL
            integer, dimension(:), allocatable :: ISKEW
            integer, dimension(:), allocatable :: ICODE
            integer, dimension(:), allocatable :: TAG_S_RBY
            integer, dimension(:), allocatable :: deleted_node ! working array to mark nodes connected to deleted element
            integer, dimension(:), allocatable :: work_array_node ! working array to mark nodes (connected to active element or deleted element)
            integer, dimension(:), allocatable :: parent_node 
            integer, dimension(:), allocatable :: nchilds

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
            my_real, dimension(:), allocatable :: MCP !< thermal
            my_real, dimension(:), allocatable :: TEMP !< temperature
          
            ! 3*NUMNOD if IRESP == 1, else 3
            double precision, dimension(:,:), allocatable :: DDP !< double precision D 
            double precision, dimension(:,:), allocatable :: XDP !< double precision X  
            double precision, dimension(:,:), allocatable :: ACC_DP !< double precision acceleration
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
       !\details Assign the pointer to the coordinates
      !||====================================================================
      !||    assign_ptrx   ../engine/source/engine/node_spliting/nodal_arrays.F90
      !||--- called by ------------------------------------------------------
      !||    resol         ../engine/source/engine/resol.F
      !||====================================================================
        subroutine assign_ptrX(ptrX, X, numnod)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          my_real, pointer, dimension(:,:), contiguous :: ptrX
          integer, intent(in) :: numnod
          my_real, target , dimension(3,numnod) :: X
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          ptrX => X
        end subroutine assign_ptrX


!! \brief Allocate nodal arrays                                                              
      !||====================================================================
      !||    allocate_nodal_arrays   ../engine/source/engine/node_spliting/nodal_arrays.F90
      !||--- called by ------------------------------------------------------
      !||    restalloc               ../engine/source/output/restart/arralloc.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    my_alloc_mod            ../common_source/tools/memory/my_alloc.F90
      !||====================================================================
        subroutine allocate_nodal_arrays(arrays, numnod, nthreads, iroddl, iparith, &
           isecut, iisrot, impose_dr, idrot, nrcvvois, sicodt, itherm_fe)
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
            integer, intent(in) :: itherm_fe !< thermal finite element
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
            integer :: i
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

            arrays%numnod0 = numnod
            arrays%iroddl = iroddl
            arrays%iparith = iparith
            arrays%nthreads = nthreads
            arrays%max_numnod = numnod
            arrays%nrcvvois = nrcvvois
            arrays%itherm_fe = itherm_fe
            arrays%sicodt_fac = 0
            if(numnod >0 ) arrays%sicodt_fac = sicodt / numnod
            IF(ISECUT > 0 .OR. IISROT > 0 .OR. IMPOSE_DR /= 0 .OR. IDROT > 0) THEN
              call my_alloc(arrays%DR,3,numnod*iroddl)
              arrays%used_dr = .true.
            else
              arrays%used_dr = .false.
              call my_alloc(arrays%DR,3,0)
            endif
            call my_alloc(arrays%parent_node,numnod)
            call my_alloc(arrays%nchilds,numnod)
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
            call my_alloc(arrays%TAG_S_RBY,numnod)
            call my_alloc(arrays%deleted_node,2*numnod)
            call my_alloc(arrays%work_array_node,nthreads*numnod)
            if(itherm_fe > 0) then
              call my_alloc(arrays%MCP,numnod)
              call my_alloc(arrays%TEMP,numnod)
            else 
              call my_alloc(arrays%MCP,0)
              call my_alloc(arrays%TEMP,0)
            endif
#ifdef MYREAL4
            call my_alloc(arrays%DDP,3,numnod)
            call my_alloc(arrays%XDP,3,numnod)
            if(iparith==0) then
              call my_alloc(arrays%ACC_DP,3,numnod) 
            endif
#else
            call my_alloc(arrays%DDP,3,1)
            call my_alloc(arrays%XDP,3,1)
#endif
            call my_alloc(arrays%WEIGHT,numnod)
            call my_alloc(arrays%MAIN_PROC,numnod)
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
            arrays%TAG_S_RBY = 0
            arrays%deleted_node = 0
            arrays%work_array_node = 0
            arrays%DDP = 0
            arrays%XDP = 0
            arrays%WEIGHT = 0
            arrays%MAIN_PROC = 0
            arrays%WEIGHT_MD = 0
            arrays%ITABM1 = 0
            arrays%A = 0
            arrays%AR = 0
            arrays%STIFR = 0
            arrays%VISCN = 0
            arrays%ICODT = 0
            arrays%ICODR = 0
            if(itherm_fe > 0) then
              arrays%MCP = 0
              arrays%TEMP = 0
            endif
            do i = 1, numnod
            arrays%parent_node(i) = i
            enddo
            arrays%nchilds = 0


! ----------------------------------------------------------------------------------------------------------------------
        end subroutine allocate_nodal_arrays
!! \brief extend nodal arrays                                                              
      !||====================================================================
      !||    extend_nodal_arrays   ../engine/source/engine/node_spliting/nodal_arrays.F90
      !||--- called by ------------------------------------------------------
      !||    detach_node           ../engine/source/engine/node_spliting/detach_node.F90
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    extend_array_mod      ../common_source/tools/memory/extend_array.F90
      !||====================================================================
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
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
            integer :: old_numnod
            integer :: i
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
           old_numnod = arrays%numnod
           !arrays%numnod = numnod
           if(numnod > arrays%max_numnod) then
              arrays%max_numnod = int(numnod * (1.0D0 + padding/100.0D0)) 
              arrays%max_numnod = max(arrays%max_numnod, numnod+1)
              call extend_array(arrays%itab,size(arrays%itab), arrays%max_numnod)
              call extend_array(arrays%IKINE, size(arrays%IKINE), arrays%max_numnod)
              call extend_array(arrays%V, 3, size(arrays%V, 2), 3, arrays%max_numnod + arrays%nrcvvois)
              call extend_array(arrays%X, 3, size(arrays%X, 2), 3, arrays%max_numnod + arrays%nrcvvois)
              call extend_array(arrays%D, 3, size(arrays%D, 2), 3, arrays%max_numnod + arrays%nrcvvois)
              call extend_array(arrays%iskew, size(arrays%iskew), arrays%max_numnod)
              call extend_array(arrays%parent_node, size(arrays%parent_node), arrays%max_numnod)
              call extend_array(arrays%nchilds, size(arrays%nchilds), arrays%max_numnod)
              arrays%iskew(arrays%numnod + 1:) = 0
              call extend_array(arrays%ICODE, size(arrays%ICODE), arrays%max_numnod)
              arrays%ICODE(arrays%numnod + 1:) = 0
              call extend_array(arrays%TAG_S_RBY, size(arrays%TAG_S_RBY), arrays%max_numnod)
              arrays%TAG_S_RBY(arrays%numnod + 1:) = 0
              call extend_array(arrays%deleted_node, size(arrays%deleted_node), 2*arrays%max_numnod)
              do i = 2*arrays%numnod, arrays%numnod+1, -1
                arrays%deleted_node(i+1) = arrays%deleted_node(i)
              end do
              arrays%deleted_node(arrays%numnod+1) = 0

              call extend_array(arrays%work_array_node, size(arrays%work_array_node), arrays%nthreads*arrays%max_numnod)
              if(arrays%iroddl >0) then
                call extend_array(arrays%VR,3, size(arrays%VR,2), 3, arrays%max_numnod)
                call extend_array(arrays%IN, size(arrays%IN), arrays%max_numnod)
                call extend_array(arrays%IN0, size(arrays%IN0), arrays%max_numnod)
                call extend_array(arrays%ICODR, size(arrays%ICODR), arrays%max_numnod*arrays%iroddl)
              endif
              if(arrays%itherm_fe > 0) then
                call extend_array(arrays%MCP, size(arrays%MCP), arrays%max_numnod)
                call extend_array(arrays%TEMP, size(arrays%TEMP), arrays%max_numnod)
              else 
                call extend_array(arrays%MCP, size(arrays%MCP), 0)
                call extend_array(arrays%TEMP, size(arrays%TEMP), 0)
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
              if(arrays%iparith==0) then
                call extend_array(arrays%ACC_DP,3, size(arrays%ACC_DP,2), 3,arrays%max_numnod)
              endif
#endif
              call extend_array(arrays%WEIGHT, size(arrays%WEIGHT), arrays%max_numnod)
              call extend_array(arrays%MAIN_PROC, size(arrays%MAIN_PROC), arrays%max_numnod)
              call extend_array(arrays%WEIGHT_MD, size(arrays%WEIGHT_MD), arrays%max_numnod)
              call extend_array(arrays%ITABM1, size(arrays%ITABM1), 2*arrays%max_numnod)
              if(arrays%iparith == 0) then
                call extend_array(arrays%A,3,size(arrays%A,2),3,arrays%max_numnod*arrays%nthreads)
                call extend_array(arrays%AR,3,size(arrays%AR,2),3,arrays%max_numnod*arrays%nthreads)
                call extend_array(arrays%STIFR,size(arrays%STIFR) ,arrays%max_numnod*arrays%iroddl*arrays%nthreads)
                call extend_array(arrays%VISCN,size(arrays%VISCN) ,arrays%max_numnod*arrays%nthreads)
                call extend_array(arrays%STIFN, size(arrays%STIFN), arrays%max_numnod*arrays%nthreads)
                arrays%stifn(arrays%numnod*arrays%nthreads + 1:) = -HUGE(arrays%stifn(1))
              else                      
                call extend_array(arrays%A,3,size(arrays%A,2),3,arrays%max_numnod)
                call extend_array(arrays%AR,3,size(arrays%AR,2),3,arrays%max_numnod)
                call extend_array(arrays%STIFR,size(arrays%STIFR) ,arrays%max_numnod)
                call extend_array(arrays%VISCN,size(arrays%VISCN) ,arrays%max_numnod)
                call extend_array(arrays%STIFN, size(arrays%STIFN), arrays%max_numnod)
              endif               
            endif
            ! ITABM1 is of size 2*arrays%max_numnod, but only the first 2*arrays%numnod elements are used
            ! When adding an element, the second half have to be shifted
            ! ITABM1 is used in SYSFUS2, that should be replaced by a hash table
            do i = numnod, 1, -1
              arrays%ITABM1(2 * numnod - i + 1) = arrays%ITABM1(2 * old_numnod - i + 1)
              ! i = 1 ; arrays%ITABM1(2 * numnod) = arrays%ITABM1(2 * old_numnod)
              ! i = numnod ; arrays%ITABM1(numnod+1) = arrays%ITABM1(old_numnod+1)
            end do
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine extend_nodal_arrays


!! \brief extend nodal arrays                                                              
      !||====================================================================
      !||    init_global_node_id   ../engine/source/engine/node_spliting/nodal_arrays.F90
      !||--- called by ------------------------------------------------------
      !||    rdresb                ../engine/source/output/restart/rdresb.F
      !||--- calls      -----------------------------------------------------
      !||    reserve_capacity      ../common_source/tools/container/umap_mod.F90
      !||--- uses       -----------------------------------------------------
      !||    umap_mod              ../common_source/tools/container/umap_mod.F90
      !||====================================================================
        subroutine init_global_node_id(arrays, numnod)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use umap_mod
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
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
            integer :: i
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
            arrays%loc2glob = create_umap()
            arrays%max_uid = 0
            call reserve_capacity(arrays%loc2glob, numnod)
            do i = 1, numnod
              arrays%max_uid = max(arrays%max_uid, arrays%itab(i))
              call add_entry_umap(arrays%loc2glob, arrays%itab(i), i)
            end do

        end subroutine init_global_node_id


      !||====================================================================
      !||    get_local_node_id                        ../engine/source/engine/node_spliting/nodal_arrays.F90
      !||--- called by ------------------------------------------------------
      !||    count_remote_nb_elem_edge                ../engine/source/interfaces/interf/count_remote_nb_elem_edge.F
      !||    find_edge_from_remote_proc               ../engine/source/interfaces/interf/find_edge_from_remote_proc.F
      !||    find_surface_from_remote_proc            ../engine/source/interfaces/interf/find_surface_from_remote_proc.F
      !||    get_neighbour_surface_from_remote_proc   ../engine/source/interfaces/interf/get_neighbour_surface_from_remote_proc.F90
      !||    spmd_exch_neighbour_segment              ../engine/source/mpi/interfaces/spmd_exch_neighbour_segment.F90
      !||    spmd_exchmsr_idel                        ../engine/source/mpi/interfaces/spmd_exchmsr_idel.F
      !||    spmd_exchseg_idel                        ../engine/source/mpi/kinematic_conditions/spmd_exchseg_idel.F
      !||    tagoff3n                                 ../engine/source/interfaces/interf/chkstfn3.F
      !||--- uses       -----------------------------------------------------
      !||    umap_mod                                 ../common_source/tools/container/umap_mod.F90
      !||====================================================================
        function get_local_node_id(arrays, global_id) result(local_id)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use umap_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
            type(nodal_arrays_) :: arrays !< nodal arrays
            integer, intent(in) :: global_id !< global id
            integer :: local_id !< local id or 0
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
            local_id = get_value_umap(arrays%loc2glob, global_id, 0)
        end function get_local_node_id

      end module nodal_arrays_mod 
