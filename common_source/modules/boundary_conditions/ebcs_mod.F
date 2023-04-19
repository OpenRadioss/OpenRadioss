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
Chd|  EBCS_MOD                      modules/boundary_conditions/ebcs_mod.F
Chd|-- called by -----------
Chd|        C_IEBCS                       starter/source/restart/ddsplit/c_iebcs.F
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        DOMDEC2                       starter/source/spmd/domdec2.F 
Chd|        EBCS_SET_TCARP                starter/source/boundary_conditions/ebcs/iniebcs_nrf_tcar.F
Chd|        FILLCNE                       starter/source/spmd/domdec2.F 
Chd|        HM_READ_EBCS_FLUXOUT          starter/source/boundary_conditions/ebcs/hm_read_ebcs_fluxout.F
Chd|        HM_READ_EBCS_GRADP0           starter/source/boundary_conditions/ebcs/hm_read_ebcs_gradp0.F
Chd|        HM_READ_EBCS_INIP             starter/source/boundary_conditions/ebcs/hm_read_ebcs_inip.F
Chd|        HM_READ_EBCS_INIV             starter/source/boundary_conditions/ebcs/hm_read_ebcs_iniv.F
Chd|        HM_READ_EBCS_INLET            starter/source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
Chd|        HM_READ_EBCS_MONVOL           starter/source/boundary_conditions/ebcs/hm_read_ebcs_monvol.F
Chd|        HM_READ_EBCS_NORMV            starter/source/boundary_conditions/ebcs/hm_read_ebcs_normv.F
Chd|        HM_READ_EBCS_NRF              starter/source/boundary_conditions/ebcs/hm_read_ebcs_nrf.F
Chd|        HM_READ_EBCS_PRES             starter/source/boundary_conditions/ebcs/hm_read_ebcs_pres.F
Chd|        HM_READ_EBCS_VALVIN           starter/source/boundary_conditions/ebcs/hm_read_ebcs_valvin.F
Chd|        HM_READ_EBCS_VALVOUT          starter/source/boundary_conditions/ebcs/hm_read_ebcs_valvout.F
Chd|        HM_READ_EBCS_VEL              starter/source/boundary_conditions/ebcs/hm_read_ebcs_vel.F
Chd|        INIEBCS                       starter/source/boundary_conditions/ebcs/iniebcs.F
Chd|        INIEBCSP0                     starter/source/boundary_conditions/ebcs/iniebcsp0.F
Chd|        INIEBCS_NRF_TCAR              starter/source/boundary_conditions/ebcs/iniebcs_nrf_tcar.F
Chd|        INIGRAV_LOAD                  starter/source/initial_conditions/inigrav/inigrav_load.F
Chd|        INIGRAV_PART_LIST             starter/source/initial_conditions/inigrav/inigrav_part_list.F
Chd|        INITIA                        starter/source/elements/initia/initia.F
Chd|        LECTUR                        starter/source/starter/lectur.F
Chd|        READ_EBCS                     starter/source/boundary_conditions/ebcs/read_ebcs.F
Chd|        SPLIT_EBCS                    starter/source/boundary_conditions/ebcs/split_ebcs.F
Chd|        STARTER0                      starter/source/starter/starter0.F
Chd|        ST_QAPRINT_DRIVER             starter/source/output/qaprint/st_qaprint_driver.F
Chd|        ST_QAPRINT_EBCS               starter/source/output/qaprint/st_qaprint_ebcs.F
Chd|        W_PON                         starter/source/restart/ddsplit/w_pon.F
Chd|        ALEMAIN                       engine/source/ale/alemain.F   
Chd|        EBCCLAP                       engine/source/boundary_conditions/ebcs/ebcclap.F
Chd|        EBCS0                         engine/source/boundary_conditions/ebcs/ebcs0.F
Chd|        EBCS1                         engine/source/boundary_conditions/ebcs/ebcs1.F
Chd|        EBCS10                        engine/source/boundary_conditions/ebcs/ebcs10.F
Chd|        EBCS4                         engine/source/boundary_conditions/ebcs/ebcs4.F
Chd|        EBCS5                         engine/source/boundary_conditions/ebcs/ebcs5.F
Chd|        EBCS6                         engine/source/boundary_conditions/ebcs/ebcs6.F
Chd|        EBCS7                         engine/source/boundary_conditions/ebcs/ebcs7.F
Chd|        EBCS_EXTRAPOL                 engine/source/boundary_conditions/ebcs/ebcs_extrapol.F
Chd|        EBCS_MAIN                     engine/source/boundary_conditions/ebcs/ebcs_main.F
Chd|        EBCVIT4                       engine/source/boundary_conditions/ebcs/ebcvit4.F
Chd|        EBCVIT5                       engine/source/boundary_conditions/ebcs/ebcvit5.F
Chd|        EBCVIT7                       engine/source/boundary_conditions/ebcs/ebcvit7.F
Chd|        MULTI_EBCS                    engine/source/multifluid/multi_ebcs.F
Chd|        MULTI_NRF_EBCS                engine/source/multifluid/multi_nrf_ebcs.F
Chd|        MULTI_TIMEEVOLUTION           engine/source/multifluid/multi_timeevolution.F
Chd|        RADIOSS2                      engine/source/engine/radioss2.F
Chd|        RDRESB                        engine/source/output/restart/rdresb.F
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|        RESOL_HEAD                    engine/source/engine/resol_head.F
Chd|        SPMD_INIT_EBCS                engine/source/mpi/fluid/spmd_cfd.F
Chd|        WRRESTP                       engine/source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        MULTI_FVM_MOD                 modules/ale/multi_fvm_mod.F   
Chd|====================================================================
      MODULE EBCS_MOD
      USE MULTI_FVM_MOD
#include "my_real.inc"

      
!     -----------------
!     parith/on structure
!     -----------------
      type ebcs_parith_on
            integer, dimension(:,:), allocatable :: elem_adress
      end type
      type(ebcs_parith_on), dimension(:), allocatable :: ebcs_parithon
!     Generic ebcs type
!     -----------------

!     Generic ebcs type
!     -----------------
      type, public :: t_ebcs
      integer :: type = -1
      integer :: ebcs_id = -1
      integer :: surf_id = -1
!     number of nodes
      integer :: nb_node = 0
!     number of elements
      integer :: nb_elem = 0
!     node list
      integer, dimension(:), allocatable :: node_list
!     element list
      integer, dimension(:, :), allocatable :: elem_list
!     Id of the attached element (local id)
      integer, dimension(:), allocatable :: ielem
!     Id of the attached element (global id)
      integer, dimension(:), allocatable :: global_ielem
!     type of the attached element
      integer, dimension(:), allocatable :: itype
!     iseg 
      integer, dimension(:), allocatable :: iseg
!     Local id of the face of ielem 
      logical :: has_iface = .false.
      integer, dimension(:), allocatable :: iface
!     debug print
      logical :: debug_print = .false.
      logical :: is_multifluid = .false.
      logical :: has_ielem = .false.

!     work tables
      logical :: has_la = .false.
      my_real, dimension(:, :), allocatable :: la
      logical :: has_p0 = .false.
      my_real, dimension(:), allocatable :: p0
      logical :: has_dp0 = .false.
      my_real, dimension(:), allocatable :: dp0
      logical :: has_ro0 = .false.
      my_real, dimension(:), allocatable :: ro0
      logical :: has_en0 = .false.
      my_real, dimension(:), allocatable :: en0
      logical :: has_vold = .false.
      my_real, dimension(:), allocatable :: vold
      logical :: has_pold = .false.
      my_real, dimension(:), allocatable :: pold
      logical :: has_v0 = .false.
      my_real, dimension(:, :), allocatable :: v0
      logical :: has_reso = .false.
      my_real, dimension(:, :), allocatable :: reso

      contains

      procedure, pass :: set_nodes_elems
      procedure, pass :: write_data, read_data
      procedure, pass :: write_common_data, read_common_data
      procedure, pass :: ebcs_destroy
      end type t_ebcs

!     type = 1 /EBCS/PRES
!     -------------------
      type, public, extends(t_ebcs) :: t_ebcs_pres
      integer :: ipres = 0, irho = 0, iener = 0, ivx = 0, ivy = 0, ivz = 0
      my_real :: c = 0, pres = 0, rho = 0, lcar = 0, r1 = 0, r2 = 0, 
     .     ener = 0, vx = 0, vy = 0, vz = 0
      
      contains
      
      procedure, pass :: write_data => write_data_pres
      procedure, pass :: read_data => read_data_pres
      end type t_ebcs_pres

!     type = 2 /EBCS/VALVIN
!     ---------------------
      type, public, extends(t_ebcs) :: t_ebcs_valvin
      integer :: ipres = 0, irho = 0, iener = 0
      my_real :: c = 0, ener = 0, rho = 0, pres = 0
      my_real :: lcar = 0, r1 = 0, r2 = 0
      
      contains
      
      procedure, pass :: write_data => write_data_valvin
      procedure, pass :: read_data => read_data_valvin
      end type t_ebcs_valvin

!     type = 3 /EBCS/VALVOUT
!     ----------------------
      type, public, extends(t_ebcs) :: t_ebcs_valvout
      integer :: ipres = 0, irho = 0, iener = 0
      my_real :: c = 0, ener = 0, rho = 0, pres = 0
      my_real :: lcar = 0, r1 = 0, r2 = 0
     
      contains
      
      procedure, pass :: write_data => write_data_valvout
      procedure, pass :: read_data => read_data_valvout
      end type t_ebcs_valvout

!     type = 0 /EBCS/GRADP0
!     ---------------------
      type, public, extends(t_ebcs) :: t_ebcs_gradp0
      integer :: ipres = 0, irho = 0, iener = 0
      my_real :: c = 0, ener = 0, rho = 0, pres = 0
      my_real :: lcar = 0, r1 = 0, r2 = 0

      contains
      
      procedure, pass :: write_data => write_data_gradp0
      procedure, pass :: read_data => read_data_gradp0
      end type t_ebcs_gradp0

!     type = 4 /EBCS/VEL
!     ------------------
      type, public, extends(t_ebcs) :: t_ebcs_vel
      integer :: ivx = 0, ivy = 0, ivz = 0, irho = 0, iener = 0
      my_real :: c = 0, ener = 0, rho = 0, vx = 0, vy = 0, vz = 0
      my_real :: lcar = 0, r1 = 0, r2 = 0
      
      contains
      
      procedure, pass :: write_data => write_data_vel
      procedure, pass :: read_data => read_data_vel
      end type t_ebcs_vel

!     type = 5 /EBCS/NORMV
!     --------------------
      type, public, extends(t_ebcs) :: t_ebcs_normv
      integer :: ivimp = 0, irho = 0, iener = 0
      my_real :: c = 0, ener = 0, rho = 0, vimp = 0
      my_real :: lcar = 0, r1 = 0, r2 = 0
      
      contains
      
      procedure, pass :: write_data => write_data_normv
      procedure, pass :: read_data => read_data_normv
      end type t_ebcs_normv

!     type = 6 /EBCS/INIP
!     -------------------
      type, public, extends(t_ebcs) :: t_ebcs_inip
      my_real :: rho = 0, c = 0, lcar = 0

      contains
      
      procedure, pass :: write_data => write_data_inip
      procedure, pass :: read_data => read_data_inip
      end type t_ebcs_inip

!     type = 7 /EBCS/INIV
!     -------------------
      type, public, extends(t_ebcs) :: t_ebcs_iniv
      my_real :: rho = 0, c = 0, lcar = 0      

      contains
      
      procedure, pass :: write_data => write_data_iniv
      procedure, pass :: read_data => read_data_iniv
      end type t_ebcs_iniv

!     type = 100 /EBCS/MONVOL
!     -----------------------
      type, public, extends(t_ebcs) :: t_ebcs_monvol
      integer :: sensor_id = 0, monvol_id = 0
      
      contains
      
      procedure, pass :: write_data => write_data_monvol
      procedure, pass :: read_data => read_data_monvol
      end type t_ebcs_monvol

!     type = 8 /EBCS/INLET
!     --------------------
      type, public, extends(t_ebcs) :: t_ebcs_inlet
      type(fvm_inlet_data_struct) :: fvm_inlet_data
      
      contains
      
      procedure, pass :: write_data => write_data_inlet
      procedure, pass :: read_data => read_data_inlet
      end type t_ebcs_inlet

!     type = 9 /EBCS/FLUXOUT
!     ----------------------
      type, public, extends(t_ebcs) :: t_ebcs_fluxout
      type(fvm_inlet_data_struct) :: fvm_inlet_data
      
      contains
      
      procedure, pass :: write_data => write_data_fluxout
      procedure, pass :: read_data => read_data_fluxout
      end type t_ebcs_fluxout
      
!     type = 10 /EBCS/NRF
!     ----------------------
      type, public, extends(t_ebcs) :: t_ebcs_nrf
      integer :: nbmat=21
      my_real :: tcar_p = 0., tcar_vf = 1E20
      !my_real, dimension(:,:), allocatable :: phase_alpha      
      type(fvm_inlet_data_struct) :: fvm_inlet_data    
      
      contains
      
      procedure, pass :: write_data => write_data_nrf
      procedure, pass :: read_data => read_data_nrf
      end type t_ebcs_nrf      

      
!     ----------------------
!     Polymorphic variable (points to a specific ebcs type)
!     ----------------------
      type, public :: t_ebcs_poly
      class(t_ebcs), allocatable :: poly
      end type t_ebcs_poly

!     Array of polymorphic variables, to handle every ebcs types (potentially different) in 
!     one same array of size nebcs
      type, public :: t_ebcs_tab
      integer :: nebcs
      class(t_ebcs_poly), dimension(:), allocatable :: tab
      integer, dimension(:), allocatable :: my_typ
      logical, dimension(:), allocatable :: need_to_compute
      logical :: is_created = .false.
      integer :: nebcs_loc = 0, nebcs_fvm = 0, nebcs_nrf = 0
      contains
      procedure, pass :: create, destroy, write_type_data, create_from_types
      procedure, nopass :: read_type_data
      end type t_ebcs_tab

      contains

!     =============     !
!     ** Methods **     !
!     =============     !

      subroutine ebcs_destroy(this)
      implicit none
      class(t_ebcs), intent(inout) :: this
      if(allocated(this%node_list)) deallocate(this%node_list)
      if(allocated(this%elem_list)) deallocate(this%elem_list)
      if(allocated(this%ielem)) deallocate(this%ielem)
      if(allocated(this%global_ielem)) deallocate(this%global_ielem)
      if(allocated(this%itype)) deallocate(this%itype)
      if(allocated(this%iseg)) deallocate(this%iseg)
      if(allocated(this%iface)) deallocate(this%iface)
      if(allocated(this%la)) deallocate(this%la)
      if(allocated(this%p0)) deallocate(this%p0)
      if(allocated(this%dp0)) deallocate(this%dp0)
      if(allocated(this%ro0)) deallocate(this%ro0)
      if(allocated(this%en0)) deallocate(this%en0)
      if(allocated(this%vold)) deallocate(this%vold)
      if(allocated(this%pold)) deallocate(this%pold)
      if(allocated(this%v0)) deallocate(this%v0)
      if(allocated(this%reso)) deallocate(this%reso)
      end subroutine ebcs_destroy
      
!     ******************     !
!     t_ebcs_tab methods     !
!     ******************     !

!     Allocation
!     ----------
      subroutine create(this, nebcs)
      implicit none
      class (t_ebcs_tab), intent(inout) :: this
      integer, intent(in) :: nebcs
      this%nebcs = 0
      if (nebcs > 0) then
         this%nebcs = nebcs
         allocate(this%tab(nebcs))
         this%is_created = .true.
      endif
      end subroutine create

!     Creation from given type numbers
!     --------------------------------
      subroutine create_from_types(this, nebcs, type_tab)
      implicit none
      class (t_ebcs_tab), intent(inout) :: this
      integer, intent(in) :: nebcs
      integer, dimension(nebcs), intent(in) :: type_tab

      integer :: ii, type
      
      class (t_ebcs), pointer :: ebcs

      if (nebcs > 0) then
         allocate(this%tab(nebcs))
         this%is_created = .true.
         do ii = 1, nebcs
            type = type_tab(ii)
            select case (type)
               case (1) ! /EBCS/PRES
                  allocate (t_ebcs_pres :: this%tab(ii)%poly)
                  this%tab(ii)%poly%type = 1
               case (2) ! /EBCS/VALVIN
                  allocate (t_ebcs_valvin :: this%tab(ii)%poly)
                  this%tab(ii)%poly%type = 2
               case (3) ! /EBCS/VALVOUT
                  allocate (t_ebcs_valvout :: this%tab(ii)%poly)
                  this%tab(ii)%poly%type = 3
               case (0) ! /EBCS/GRADP0
                  allocate (t_ebcs_gradp0 :: this%tab(ii)%poly)
                  this%tab(ii)%poly%type = 0
               case (4) ! /EBCS/VEL
                  allocate (t_ebcs_vel :: this%tab(ii)%poly)
                  this%tab(ii)%poly%type = 4
               case (5) ! /EBCS/NORMV
                  allocate (t_ebcs_normv :: this%tab(ii)%poly)
                  this%tab(ii)%poly%type = 5
               case (6) ! /EBCS/INIP
                  allocate (t_ebcs_inip :: this%tab(ii)%poly)
                  this%tab(ii)%poly%type = 6
               case (7) ! /ENCS/INIV
                  allocate (t_ebcs_iniv :: this%tab(ii)%poly)
                  this%tab(ii)%poly%type = 7
               case (100) ! /EBCS/MONVOL
                  allocate (t_ebcs_monvol :: this%tab(ii)%poly)
                  this%tab(ii)%poly%type = 100
               case (8) ! /EBCS/INLET
                  allocate (t_ebcs_inlet :: this%tab(ii)%poly)
                  this%tab(ii)%poly%type = 8
              case (9) ! /EBCS/FLUXOUT
                  allocate (t_ebcs_fluxout :: this%tab(ii)%poly)
                  this%tab(ii)%poly%type = 9
              case (10) ! /EBCS/NRF
                  allocate (t_ebcs_nrf :: this%tab(ii)%poly)
                  this%tab(ii)%poly%type = 10                  
               case default
                  print*, "EBCS type ", type, " unrecognized"
            end select
         enddo
      endif
      end subroutine create_from_types
!     Deallocation
!     ----------
      subroutine destroy(this)
      implicit none
      class (t_ebcs_tab), intent(inout) :: this
      
      integer :: ii
      if (allocated(this%tab)) then
C this is not useful and it causes a bug with Intel OneAPI 2023
c        do ii = 1, this%nebcs
c           call this%tab(ii)%poly%ebcs_destroy()
c        enddo
C All allocatable subobjects are deallocated here:  
         deallocate(this%tab)
      endif
      end subroutine destroy

!     Read and write methods
!     ----------------------
      subroutine write_type_data(this, leni, lenr)
      implicit none
      class (t_ebcs_tab), intent(in) :: this
      integer, intent(inout) :: leni, lenr

      integer, dimension(this%nebcs) :: integer_data
      integer :: ii
      
      do ii = 1, this%nebcs
         integer_data(ii) = this%tab(ii)%poly%type
      enddo
      call write_i_c(integer_data, this%nebcs)
      leni = leni + this%nebcs
      end subroutine write_type_data
      
      subroutine read_type_data(nebcs, integer_data)
      implicit none
      integer, intent(in) :: nebcs
      integer, dimension(nebcs), intent(inout) :: integer_data
      call read_i_c(integer_data, nebcs)
      end subroutine read_type_data

!     **********************     !
!     t_ebcs common routines     !
!     **********************     !

!     Nodes and elements of the surface
!     ---------------------------------
      subroutine set_nodes_elems(this, nelem, numnod, surf_node)
      implicit none
!     Dummy
      class (t_ebcs), intent(inout) :: this
      integer, intent(in) :: numnod, nelem
      integer, dimension(nelem, 4), intent(in) :: surf_node
!     Local
      integer :: ii, jj, nnode
      integer, dimension(:), allocatable :: tag_node
!     tag surface nodes
      allocate(tag_node(0:numnod))
      tag_node(0:numnod) = 0
      do ii = 1, nelem
         do jj = 1, 4
            tag_node(surf_node(ii, jj)) = 1
         enddo
      enddo
!     keep track of nodes
      nnode = 0
      do ii = 1, numnod
         if (tag_node(ii) == 1) then
            nnode = nnode + 1
         endif
      enddo

      this%nb_node = nnode
      if(allocated(this%node_list)) deallocate(this%node_list)
      allocate(this%node_list(nnode))
      
      nnode = 0
      do ii = 1, numnod
         if (tag_node(ii) == 1) then
            nnode = nnode + 1
            this%node_list(nnode) = ii
            tag_node(ii) = nnode
         endif
      enddo

!     keep track of elems
      this%nb_elem = nelem
      if(allocated(this%elem_list)) deallocate(this%elem_list)
      allocate(this%elem_list(4, nelem))
      do ii = 1, nelem
         do jj = 1, 4
            this%elem_list(jj, ii) = tag_node(surf_node(ii, jj))
         enddo
      enddo
!     Allocate members
      if(.not.allocated(this%ielem)) allocate(this%ielem(nelem))

      if(.not.allocated(this%itype)) allocate(this%itype(nelem))

      if(.not.allocated(this%iseg)) allocate(this%iseg(nelem))

!     memory deallocation
      deallocate(tag_node)
      end subroutine set_nodes_elems

!     ***********************     !
!     read and write routines     !
!     ***********************     !

!     Abstract
!     --------
      subroutine write_data(this, leni, lenr)
      implicit none
      class (t_ebcs), intent(inout) :: this
      integer, intent(inout) :: leni, lenr
      end subroutine write_data
      
      subroutine read_data(this)
      implicit none
      class (t_ebcs), intent(inout) :: this
      end subroutine read_data

!     Common write routine
!     --------------------

      subroutine write_common_data(this, leni, lenr)
      implicit none
      class (t_ebcs), intent(inout) :: this
      integer, intent(inout) :: leni, lenr

      integer, dimension(7) :: integer_data 
      integer :: siz     
      integer_data(1) = this%type
      integer_data(2) = this%ebcs_id
      integer_data(3) = this%surf_id
      integer_data(4) = this%nb_node
      integer_data(5) = this%nb_elem
      integer_data(6) = 0
      if(this%is_multifluid)integer_data(6) = 1
      integer_data(7) = 0
      if(this%has_ielem)integer_data(7) = 1
      if (this%debug_print) print*, "integer_data ", integer_data

      call write_i_c(integer_data, 7)
      leni = leni + 7

!     write node list
      if (this%debug_print) print*, "node_list ", this%node_list
      call write_i_c(this%node_list, this%nb_node)
      leni = leni + this%nb_node
      
!     write elem_list
      if (this%debug_print) print*, "elem_list ", this%elem_list
      call write_i_c(this%elem_list, 4 * this%nb_elem)
      leni = leni + 4 * this%nb_elem

!     write ielem
      if (this%debug_print) print*, "ielem ", this%ielem
      call write_i_c(this%ielem, this%nb_elem)
      leni = leni + this%nb_elem

!     write itype
      if (this%debug_print) print*, "itype ", this%itype
      call write_i_c(this%itype, this%nb_elem)
      leni = leni + this%nb_elem

!     write iseg
      if (this%debug_print) print*, "iseg ", this%iseg
      call write_i_c(this%iseg, this%nb_elem)
      leni = leni + this%nb_elem

!     write iface
      if (this%has_iface) then
         call write_i_c(1, 1)
         leni = leni + 1
         if (this%debug_print) print*, "iface ", this%iface
         call write_i_c(this%iface, this%nb_elem)
         leni = leni + this%nb_elem
      else
         call write_i_c(0, 1)
         leni = leni + 1
      endif

!     write la
      if (this%has_la) then
         call write_i_c(1, 1)
         leni = leni + 1
         if (this%debug_print) print*, "la ", this%la
         call write_db(this%la, 3 * this%nb_node)
         leni = leni + 3 * this%nb_node
      else
         call write_i_c(0, 1)
         leni = leni + 1
      endif

!     write p0
      if (this%has_p0) then
         call write_i_c(1, 1)
         leni = leni + 1
         if (this%debug_print) print*, 'p0 = ', this%p0
         call write_db(this%p0, this%nb_node)
         leni = leni + this%nb_node
      else
         call write_i_c(0, 1)
         leni = leni + 1
      endif
      
!     write dp0
      if (this%has_dp0) then
         call write_i_c(1, 1)
         leni = leni + 1
         if (this%debug_print) print*, 'dp0 = ', this%dp0
         call write_db(this%dp0, this%nb_elem)
         leni = leni + this%nb_node
      else
         call write_i_c(0, 1)
         leni = leni + 1
      endif      

!     write ro0
      if (this%has_ro0) then
         call write_i_c(1, 1)
         leni = leni + 1
         if (this%debug_print) print*, "ro0 ", this%ro0
         call write_db(this%ro0, this%nb_elem)
         leni = leni + this%nb_elem
      else
         call write_i_c(0, 1)
         leni = leni + 1
      endif

!     write en0
      if (this%has_en0) then
         call write_i_c(1, 1)
         leni = leni + 1
         if (this%debug_print) print*, "en0 ", this%en0
         call write_db(this%en0, this%nb_elem)
         leni = leni + this%nb_elem
      else
         call write_i_c(0, 1)
         leni = leni + 1
      endif

!     write pold
      if (this%has_pold) then
         if(this%is_multifluid)then
           siz=this%nb_elem
         else
           siz = this%nb_node
         endif
         call write_i_c(1, 1)
         leni = leni + 1
         if (this%debug_print) print*, "pold ", this%pold
         call write_db(this%pold, siz)
         leni = leni + siz
      else
         call write_i_c(0, 1)
         leni = leni + 1
      endif

!     write +
      if (this%has_vold) then
         if(this%is_multifluid)then
           siz=this%nb_elem
         else
           siz = this%nb_node
         endif      
         call write_i_c(1, 1)
         leni = leni + 1
         if (this%debug_print) print*, "vold ", this%vold
         call write_db(this%vold, siz)
         leni = leni + siz
      else
         call write_i_c(0, 1)
         leni = leni + 1
      endif

!     write v0
      if (this%has_v0) then
         call write_i_c(1, 1)
         leni = leni + 1
         if (this%debug_print) print*, "v0 ", this%v0
         call write_db(this%v0, 3 * this%nb_node)
         leni = leni + 3 * this%nb_node
      else
         call write_i_c(0, 1)
         leni = leni + 1
      endif

!     write reso
      if (this%has_reso) then
         call write_i_c(1, 1)
         leni = leni + 1
         if (this%debug_print) print*, "reso ", this%reso
         call write_db(this%reso, 3 * this%nb_node)
         leni = leni + 3 * this%nb_node
      else
         call write_i_c(0, 1)
         leni = leni + 1
      endif

      end subroutine write_common_data

!     Common read routine
!     -------------------

      subroutine read_common_data(this)
      implicit none
      class (t_ebcs), intent(inout) :: this

      integer, dimension(7) :: integer_data  
      integer :: ihas_la, ihas_iface, ihas_p0, ihas_dp0, ihas_ro0, ihas_en0, 
     .     ihas_pold, ihas_vold, ihas_v0, ihas_reso,siz
      
      call read_i_c(integer_data, 7)
      this%type = integer_data(1)
      this%ebcs_id = integer_data(2)
      this%surf_id = integer_data(3)
      this%nb_node = integer_data(4)
      this%nb_elem = integer_data(5)
      this%is_multifluid=.false.
      if(integer_data(6) == 1)this%is_multifluid=.true.
      this%has_ielem=.false.
      if(integer_data(7) == 1)this%has_ielem=.true.
      if (this%debug_print) print*, "integer_data ", integer_data

!     read node list
      allocate(this%node_list(this%nb_node))
      call read_i_c(this%node_list, this%nb_node)
      if (this%debug_print) print*, "node_list ", this%node_list
      
!     read elem_list
      allocate(this%elem_list(4, this%nb_elem))
      call read_i_c(this%elem_list, 4 * this%nb_elem)
      if (this%debug_print) print*, "elem_list ", this%elem_list

!     read ielem
      allocate(this%ielem(this%nb_elem))
      call read_i_c(this%ielem, this%nb_elem)
      if (this%debug_print) print*, "ielem ", this%ielem

!     read ielem
      allocate(this%itype(this%nb_elem))
      call read_i_c(this%itype, this%nb_elem)
      if (this%debug_print) print*, "itype ", this%itype

!     read iseg
      allocate(this%iseg(this%nb_elem))
      call read_i_c(this%iseg, this%nb_elem)
      if (this%debug_print) print*, "iseg ", this%iseg

!     read iface
      call read_i_c(ihas_iface, 1)
      if (ihas_iface == 1) then
         this%has_iface = .true.
         allocate(this%iface(this%nb_elem))
         call read_i_c(this%iface, this%nb_elem)
         if (this%debug_print) print*, "iface ", this%iface
      else
         this%has_iface = .false.
      endif

!     read la
      call read_i_c(ihas_la, 1)
      if (ihas_la == 1) then
         this%has_la = .true.
         allocate(this%la(3, this%nb_node))
         call read_db(this%la, 3 * this%nb_node)
        if (this%debug_print) print*, "la ", this%la
      else
         this%has_la = .false.
      endif

!     read p0
      call read_i_c(ihas_p0, 1)
      if (ihas_p0 == 1) then
         this%has_p0 = .true.
         allocate(this%p0(this%nb_node))
         call read_db(this%p0, this%nb_node)
         if (this%debug_print) print*, 'p0 = ', this%p0
      else
         this%has_p0 = .false.
      endif   
      
!     read dp0
      call read_i_c(ihas_dp0, 1)
      if (ihas_dp0 == 1) then
         this%has_dp0 = .true.
         allocate(this%dp0(this%nb_elem))
         call read_db(this%dp0, this%nb_elem)
         if (this%debug_print) print*, 'dp0 = ', this%dp0
      else
         this%has_dp0 = .false.
      endif         

!     read ro0
      call read_i_c(ihas_ro0, 1)
      if (ihas_ro0 == 1) then
         this%has_ro0 = .true.
         allocate(this%ro0(this%nb_elem))
         call read_db(this%ro0, this%nb_elem)
         if (this%debug_print) print*, "ro0 ", this%ro0
      else
         this%has_ro0 = .false.
      endif 

!     read en0
      call read_i_c(ihas_en0, 1)
      if (ihas_en0 == 1) then
         this%has_en0 = .true.
         allocate(this%en0(this%nb_elem))
         call read_db(this%en0, this%nb_elem)
         if (this%debug_print) print*, "en0 ", this%en0
      else
         this%has_en0 = .false.
      endif    

!     read pold
      call read_i_c(ihas_pold, 1)                      
      if (ihas_pold == 1) then
         this%has_pold = .true.      
         if(this%is_multifluid)then   
           siz=this%nb_elem          
         else                        
           siz = this%nb_node           
         endif       
         allocate(this%pold(siz))
         call read_db(this%pold, siz)
         if (this%debug_print) print*, "pold ", this%pold
      else
         this%has_pold = .false.
      endif  

!     read vold
      call read_i_c(ihas_vold, 1)
      if (ihas_vold == 1) then
         this%has_vold = .true.
         if(this%is_multifluid)then   
           siz=this%nb_elem          
         else                        
           siz = this%nb_node           
         endif                
         allocate(this%vold(siz))
         call read_db(this%vold, siz)
         if (this%debug_print) print*, "vold ", this%vold
      else
         this%has_vold = .false.
      endif  

!     read v0
      call read_i_c(ihas_v0, 1)
      if (ihas_v0 == 1) then
         this%has_v0 = .true.
         allocate(this%v0(3, this%nb_node))
         call read_db(this%v0, 3 * this%nb_node)
         if (this%debug_print) print*, "v0 ", this%v0
      else
         this%has_v0 = .false.
      endif

!     read reso
      call read_i_c(ihas_reso, 1)
      if (ihas_reso == 1) then
         this%has_reso = .true.
         allocate(this%reso(3, this%nb_node))
         call read_db(this%reso, 3 * this%nb_node)
         if (this%debug_print) print*, "reso ", this%reso
      else
         this%has_reso = .false.
      endif
      end subroutine read_common_data

!     /EBCS/PRES
!     ----------

      subroutine write_data_pres(this, leni, lenr)
      implicit none
      class (t_ebcs_pres), intent(inout) :: this
      integer, intent(inout) :: leni, lenr
      integer, dimension(6) :: integer_data
      my_real, dimension(10) :: real_data
      
      integer_data(1) = this%ipres
      integer_data(2) = this%irho
      integer_data(3) = this%iener
      integer_data(4) = this%ivx
      integer_data(5) = this%ivy
      integer_data(6) = this%ivz

      real_data(1) = this%c
      real_data(2) = this%pres
      real_data(3) = this%rho
      real_data(4) = this%lcar
      real_data(5) = this%r1
      real_data(6) = this%r2
      real_data(7) = this%ener
      real_data(8) = this%vx
      real_data(9) = this%vy
      real_data(10) = this%vz

      call write_i_c(integer_data, 6)
      leni = leni + 6
      call write_db(real_data, 10)
      lenr = lenr + 10
      end subroutine write_data_pres

      subroutine read_data_pres(this)
      implicit none
      class (t_ebcs_pres), intent(inout) :: this

      integer, dimension(6) :: integer_data
      my_real, dimension(10) :: real_data

      call read_i_c(integer_data, 6)
      this%ipres =integer_data(1)
      this%irho = integer_data(2)
      this%iener = integer_data(3)
      this%ivx = integer_data(4)
      this%ivy = integer_data(5)
      this%ivz = integer_data(6)

      call read_db(real_data, 10)
      this%c = real_data(1)
      this%pres = real_data(2)
      this%rho = real_data(3)
      this%lcar = real_data(4)
      this%r1 = real_data(5)
      this%r2 = real_data(6)
      this%ener = real_data(7)
      this%vx = real_data(8) 
      this%vy = real_data(9) 
      this%vz = real_data(10)
      end subroutine read_data_pres

!     /EBCS/VALVIN
!     ----------

      subroutine write_data_valvin(this, leni, lenr)
      implicit none
      class (t_ebcs_valvin), intent(inout) :: this
      integer, intent(inout) :: leni, lenr

      integer, dimension(3) :: integer_data
      my_real, dimension(7) :: real_data

      integer_data(1) = this%ipres
      integer_data(2) = this%irho
      integer_data(3) = this%iener

      call write_i_c(integer_data, 3)
      leni = leni + 3

      real_data(1) = this%c
      real_data(2) = this%ener
      real_data(3) = this%rho
      real_data(4) = this%pres
      real_data(5) = this%lcar
      real_data(6) = this%r1
      real_data(7) = this%r2

      call write_db(real_data, 7)
      lenr = lenr + 7
      end subroutine write_data_valvin

      subroutine read_data_valvin(this)
      implicit none
      class (t_ebcs_valvin), intent(inout) :: this

      integer, dimension(3) :: integer_data
      my_real, dimension(7) :: real_data

      call read_i_c(integer_data, 3)
      this%ipres =integer_data(1)
      this%irho = integer_data(2)
      this%iener = integer_data(3)

      call read_db(real_data, 7)
      this%c = real_data(1)
      this%ener = real_data(2)
      this%rho = real_data(3)
      this%pres = real_data(4)
      this%lcar = real_data(5)
      this%r1 = real_data(6)
      this%r2 = real_data(7)
      end subroutine read_data_valvin

!     /EBCS/VALVOUT
!     ----------

      subroutine write_data_valvout(this, leni, lenr)
      implicit none
      class (t_ebcs_valvout), intent(inout) :: this
      integer, intent(inout) :: leni, lenr

      integer, dimension(3) :: integer_data
      my_real, dimension(7) :: real_data

      integer_data(1) = this%ipres
      integer_data(2) = this%irho
      integer_data(3) = this%iener

      call write_i_c(integer_data, 3)
      leni = leni + 3

      real_data(1) = this%c
      real_data(2) = this%ener
      real_data(3) = this%rho
      real_data(4) = this%pres
      real_data(5) = this%lcar
      real_data(6) = this%r1
      real_data(7) = this%r2

      call write_db(real_data, 7)
      lenr = lenr + 7
      end subroutine write_data_valvout

      subroutine read_data_valvout(this)
      implicit none
      class (t_ebcs_valvout), intent(inout) :: this

      integer, dimension(3) :: integer_data
      my_real, dimension(7) :: real_data

      call read_i_c(integer_data, 3)
      this%ipres =integer_data(1)
      this%irho = integer_data(2)
      this%iener = integer_data(3)

      call read_db(real_data, 7)
      this%c = real_data(1)
      this%ener = real_data(2)
      this%rho = real_data(3)
      this%pres = real_data(4)
      this%lcar = real_data(5)
      this%r1 = real_data(6)
      this%r2 = real_data(7)
      end subroutine read_data_valvout

!     /EBCS/GRADP0
!     ----------

      subroutine write_data_gradp0(this, leni, lenr)
      implicit none
      class (t_ebcs_gradp0), intent(inout) :: this
      integer, intent(inout) :: leni, lenr

      integer, dimension(3) :: integer_data
      my_real, dimension(7) :: real_data

      integer_data(1) = this%ipres
      integer_data(2) = this%irho
      integer_data(3) = this%iener

      call write_i_c(integer_data, 3)
      leni = leni + 3

      real_data(1) = this%c
      real_data(2) = this%ener
      real_data(3) = this%rho
      real_data(4) = this%pres
      real_data(5) = this%lcar
      real_data(6) = this%r1
      real_data(7) = this%r2

      call write_db(real_data, 7)
      lenr = lenr + 7
      end subroutine write_data_gradp0

      subroutine read_data_gradp0(this)
      implicit none
      class (t_ebcs_gradp0), intent(inout) :: this

      integer, dimension(3) :: integer_data
      my_real, dimension(7) :: real_data

      call read_i_c(integer_data, 3)
      this%ipres =integer_data(1)
      this%irho = integer_data(2)
      this%iener = integer_data(3)

      call read_db(real_data, 7)
      this%c = real_data(1)
      this%ener = real_data(2)
      this%rho = real_data(3)
      this%pres = real_data(4)
      this%lcar = real_data(5)
      this%r1 = real_data(6)
      this%r2 = real_data(7)
      end subroutine read_data_gradp0

!     /EBCS/VEL
!     ----------

      subroutine write_data_vel(this, leni, lenr)
      implicit none
      class (t_ebcs_vel), intent(inout) :: this
      integer, intent(inout) :: leni, lenr

      integer, dimension(5) :: integer_data
      my_real, dimension(9) :: real_data

      integer_data(1) = this%ivx
      integer_data(2) = this%ivy
      integer_data(3) = this%ivz
      integer_data(4) = this%irho
      integer_data(5) = this%iener

      call write_i_c(integer_data, 5)
      leni = leni + 5

      real_data(1) = this%c
      real_data(2) = this%ener
      real_data(3) = this%rho
      real_data(4) = this%vx
      real_data(5) = this%vy
      real_data(6) = this%vz
      real_data(7) = this%lcar
      real_data(8) = this%r1
      real_data(9) = this%r2

      call write_db(real_data, 9)
      lenr = lenr + 9
      end subroutine write_data_vel

      subroutine read_data_vel(this)
      implicit none
      class (t_ebcs_vel), intent(inout) :: this

      integer, dimension(5) :: integer_data
      my_real, dimension(9) :: real_data

      call read_i_c(integer_data, 5)
      this%ivx = integer_data(1)
      this%ivy = integer_data(2)
      this%ivz = integer_data(3)
      this%irho = integer_data(4)
      this%iener = integer_data(5)

      call read_db(real_data, 9)
      this%c = real_data(1)
      this%ener = real_data(2)
      this%rho = real_data(3)
      this%vx = real_data(4)
      this%vy = real_data(5)
      this%vz = real_data(6)
      this%lcar = real_data(7)
      this%r1 = real_data(8)
      this%r2 = real_data(9)
      end subroutine read_data_vel

!     /EBCS/NORMV
!     ----------

      subroutine write_data_normv(this, leni, lenr)
      implicit none
      class (t_ebcs_normv), intent(inout) :: this
      integer, intent(inout) :: leni, lenr

      integer, dimension(3) :: integer_data
      my_real, dimension(7) :: real_data

      integer_data(1) = this%ivimp
      integer_data(2) = this%irho
      integer_data(3) = this%iener

      call write_i_c(integer_data, 3)
      leni = leni + 3

      real_data(1) = this%c
      real_data(2) = this%ener
      real_data(3) = this%rho
      real_data(4) = this%vimp
      real_data(5) = this%lcar
      real_data(6) = this%r1
      real_data(7) = this%r2

      call write_db(real_data, 7)
      lenr = lenr + 7
      end subroutine write_data_normv

      subroutine read_data_normv(this)
      implicit none
      class (t_ebcs_normv), intent(inout) :: this

      integer, dimension(3) :: integer_data
      my_real, dimension(7) :: real_data

      call read_i_c(integer_data, 3)
      this%ivimp = integer_data(1)
      this%irho = integer_data(2)
      this%iener = integer_data(3)

      call read_db(real_data, 7)
      this%c = real_data(1)
      this%ener = real_data(2)
      this%rho = real_data(3)
      this%vimp = real_data(4)
      this%lcar = real_data(5)
      this%r1 = real_data(6)
      this%r2 = real_data(7)
      end subroutine read_data_normv

!     /EBCS/INIP
!     ----------

      subroutine write_data_inip(this, leni, lenr)
      implicit none
      class (t_ebcs_inip), intent(inout) :: this
      integer, intent(inout) :: leni, lenr

      my_real, dimension(3) :: real_data

      real_data(1) = this%rho
      real_data(2) = this%c
      real_data(3) = this%lcar
      call write_db(real_data, 3)
      lenr = lenr + 3
      end subroutine write_data_inip

      subroutine read_data_inip(this)
      implicit none
      class (t_ebcs_inip), intent(inout) :: this

      my_real, dimension(3) :: real_data

      call read_db(real_data, 3)
      this%rho = real_data(1)
      this%c = real_data(2)
      this%lcar = real_data(3)
      end subroutine read_data_inip

!     /EBCS/INIV
!     ----------

      subroutine write_data_iniv(this, leni, lenr)
      implicit none
      class (t_ebcs_iniv), intent(inout) :: this
      integer, intent(inout) :: leni, lenr

      my_real, dimension(3) :: real_data

      real_data(1) = this%rho
      real_data(2) = this%c
      real_data(3) = this%lcar
      call write_db(real_data, 3)
      lenr = lenr + 3
      end subroutine write_data_iniv

      subroutine read_data_iniv(this)
      implicit none
      class (t_ebcs_iniv), intent(inout) :: this

      my_real, dimension(3) :: real_data

      call read_db(real_data, 3)
      this%rho = real_data(1)
      this%c = real_data(2)
      this%lcar = real_data(3)
      end subroutine read_data_iniv

!     /EBCS/MONVOL
!     ----------

      subroutine write_data_monvol(this, leni, lenr)
      implicit none
      class (t_ebcs_monvol), intent(inout) :: this
      integer, intent(inout) :: leni, lenr
      
      integer, dimension(2) :: integer_data

      integer_data(1) = this%sensor_id
      integer_data(2) = this%monvol_id
      call write_i_c(integer_data, 2)
      leni = leni + 2
      end subroutine write_data_monvol

      subroutine read_data_monvol(this)
      implicit none
      class (t_ebcs_monvol), intent(inout) :: this

      integer, dimension(2) :: integer_data

      call read_i_c(integer_data, 2)
      this%sensor_id = integer_data(1)
      this%monvol_id = integer_data(2)
      end subroutine read_data_monvol

!     /EBCS/INLET
!     ----------

      subroutine write_data_inlet(this, leni, lenr)
      implicit none
      class (t_ebcs_inlet), intent(inout) :: this
      integer, intent(inout) :: leni, lenr
      
      call write_i_c(this%fvm_inlet_data%vector_velocity, 1)
      leni = leni + 1
      call write_i_c(this%fvm_inlet_data%formulation, 1)
      leni = leni + 1
      call write_i_c(this%fvm_inlet_data%func_vel, 3)
      leni = leni + 3
      call write_i_c(this%fvm_inlet_data%func_alpha, 21)
      leni = leni + 21
      call write_i_c(this%fvm_inlet_data%func_rho, 21)
      leni = leni + 21
      call write_i_c(this%fvm_inlet_data%func_pres, 21)
      leni = leni + 21

      call write_db(this%fvm_inlet_data%val_vel, 3)
      lenr = lenr + 3
      call write_db(this%fvm_inlet_data%val_alpha, 21)
      lenr = lenr + 21
      call write_db(this%fvm_inlet_data%val_rho, 21)
      lenr = lenr + 21
      call write_db(this%fvm_inlet_data%val_pres, 21)
      lenr = lenr + 21
      end subroutine write_data_inlet

      subroutine read_data_inlet(this)
      implicit none
      class (t_ebcs_inlet), intent(inout) :: this

      call read_i_c(this%fvm_inlet_data%vector_velocity, 1)
      call read_i_c(this%fvm_inlet_data%formulation, 1)
      call read_i_c(this%fvm_inlet_data%func_vel, 3)
      call read_i_c(this%fvm_inlet_data%func_alpha, 21)
      call read_i_c(this%fvm_inlet_data%func_rho, 21)
      call read_i_c(this%fvm_inlet_data%func_pres, 21)

      call read_db(this%fvm_inlet_data%val_vel, 3)
      call read_db(this%fvm_inlet_data%val_alpha, 21)
      call read_db(this%fvm_inlet_data%val_rho, 21)
      call read_db(this%fvm_inlet_data%val_pres, 21)
      end subroutine read_data_inlet

!     /EBCS/FLUXOUT
!     ----------

      subroutine write_data_fluxout(this, leni, lenr)
      implicit none
      class (t_ebcs_fluxout), intent(inout) :: this
      integer, intent(inout) :: leni, lenr

      call write_i_c(this%fvm_inlet_data%vector_velocity, 1)
      leni = leni + 1
      call write_i_c(this%fvm_inlet_data%formulation, 1)
      leni = leni + 1
      call write_i_c(this%fvm_inlet_data%func_vel, 3)
      leni = leni + 3
      call write_i_c(this%fvm_inlet_data%func_alpha, 21)
      leni = leni + 21
      call write_i_c(this%fvm_inlet_data%func_rho, 21)
      leni = leni + 21
      call write_i_c(this%fvm_inlet_data%func_pres, 21)
      leni = leni + 21

      call write_db(this%fvm_inlet_data%val_vel, 3)
      lenr = lenr + 3
      call write_db(this%fvm_inlet_data%val_alpha, 21)
      lenr = lenr + 21
      call write_db(this%fvm_inlet_data%val_rho, 21)
      lenr = lenr + 21
      call write_db(this%fvm_inlet_data%val_pres, 21)
      lenr = lenr + 21
      end subroutine write_data_fluxout

      subroutine read_data_fluxout(this)
      implicit none
      class (t_ebcs_fluxout), intent(inout) :: this

      call read_i_c(this%fvm_inlet_data%vector_velocity, 1)
      call read_i_c(this%fvm_inlet_data%formulation, 1)
      call read_i_c(this%fvm_inlet_data%func_vel, 3)
      call read_i_c(this%fvm_inlet_data%func_alpha, 21)
      call read_i_c(this%fvm_inlet_data%func_rho, 21)
      call read_i_c(this%fvm_inlet_data%func_pres, 21)

      call read_db(this%fvm_inlet_data%val_vel, 3)
      call read_db(this%fvm_inlet_data%val_alpha, 21)
      call read_db(this%fvm_inlet_data%val_rho, 21)
      call read_db(this%fvm_inlet_data%val_pres, 21)

      end subroutine read_data_fluxout
      
!     /EBCS/NRF
!     ----------

      subroutine write_data_nrf(this, leni, lenr)
      implicit none
      class (t_ebcs_nrf), intent(inout) :: this
      integer, intent(inout) :: leni, lenr

      my_real, dimension(2) :: real_data

      call write_i_c(this%nbmat, 1)
      leni = leni + 1
      real_data(1) = this%tcar_p
      real_data(2) = this%tcar_vf
      call write_db(real_data, 2)
      lenr = lenr + 2
      !call write_db(this%phase_alpha, this%nbmat*this%nb_elem)
      !lenr = lenr + this%nbmat*this%nb_elem
       if(this%is_multifluid)then
        call write_i_c(this%fvm_inlet_data%vector_velocity, 1)
        leni = leni + 1
        call write_i_c(this%fvm_inlet_data%formulation, 1)
        leni = leni + 1
        call write_i_c(this%fvm_inlet_data%func_vel, 3)
        leni = leni + 3
        call write_i_c(this%fvm_inlet_data%func_alpha, 21)
        leni = leni + 21
        call write_i_c(this%fvm_inlet_data%func_rho, 21)
        leni = leni + 21
        call write_i_c(this%fvm_inlet_data%func_pres, 21)
        leni = leni + 21

        call write_db(this%fvm_inlet_data%val_vel, 3)
        lenr = lenr + 3
        call write_db(this%fvm_inlet_data%val_alpha, 21)
        lenr = lenr + 21
        call write_db(this%fvm_inlet_data%val_rho, 21)
        lenr = lenr + 21
        call write_db(this%fvm_inlet_data%val_pres, 21)
        lenr = lenr + 21
       endif     
      end subroutine write_data_nrf

      subroutine read_data_nrf(this)
      implicit none
      class (t_ebcs_nrf), intent(inout) :: this
      integer :: itmp

      call read_i_c(this%nbmat, 1)
      call read_db(this%tcar_p, 1)
      call read_db(this%tcar_vf, 1)      
      !if(this%nbmat > 0)then
      !  allocate(this%phase_alpha(this%nbmat,this%nb_elem))      
      !  call read_db(this%phase_alpha, this%nbmat*this%nb_elem)
      !endif
      if(this%is_multifluid)then
        call read_i_c(this%fvm_inlet_data%vector_velocity, 1)
        call read_i_c(this%fvm_inlet_data%formulation, 1)
        call read_i_c(this%fvm_inlet_data%func_vel, 3)
        call read_i_c(this%fvm_inlet_data%func_alpha, 21)
        call read_i_c(this%fvm_inlet_data%func_rho, 21)
        call read_i_c(this%fvm_inlet_data%func_pres, 21)

        call read_db(this%fvm_inlet_data%val_vel, 3)
        call read_db(this%fvm_inlet_data%val_alpha, 21)
        call read_db(this%fvm_inlet_data%val_rho, 21)
        call read_db(this%fvm_inlet_data%val_pres, 21)
      endif      
      end subroutine read_data_nrf    

      end module ebcs_mod
