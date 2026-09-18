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
      module alemain_cutcell_mod
        implicit none
        contains
        ! ======================================================================================================================
        !                                                   procedures
        ! ======================================================================================================================
        !! \brief Main Subroutine called by the resol loop to treat ALE MULTIMAT with cut-cell interface tracking
        !! \details ...
        subroutine  alemain_cutcell(nixtg, nixq, numeltg, numelq, ixtg, ixq, numnod, x, ale_connect, ncycle, &
          ityptstt, neltstt, t1s, tt, &
          ngrnod, igrnod, dt_scale, dt1, dt2t, multi_cutcell, &
          ngroup, elbuf, nparg, iparg, ebcs_tab, n2d, ngrquad, igrquad)
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   Modules
          ! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : pi,zero,one,em01               !module containing all constant insitialized with either single or double precision
          use precision_mod, only : wp                            !provides kind for eigther single or double precision (wp means working precision)
          use ale_connectivity_mod , only : t_ale_connectivity    !data structure for ale elem-elem connectivities
          use groupdef_mod , only : group_
          use ale_mod , only : ALE
          use debug_mod , only : ITAB_DEBUG
          use multi_cutcell_mod, only : multi_cutcell_struct, allocate_multi_cutcell_type
          use multicutcell_solver_mod, only : initialize_solver_multicutcell, update_fluid_multicutcell, build_full_states
          use multicutcell_solver_mod, only : multicutcell_mixed_cell
          use elbufdef_mod , only : elbuf_struct_
          use multicutcell_solver_mod , only : multicutcell_initial_state
          use ebcs_mod !, only : t_ebcs_tab, t_ebcs
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   Implicit none
          ! ----------------------------------------------------------------------------------------------------------------------
          implicit none
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   Included files
          ! ----------------------------------------------------------------------------------------------------------------------
          !
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   Arguments
          ! ----------------------------------------------------------------------------------------------------------------------
          ! input
          integer,intent(in) :: nixq                                    !< array size for ixq array
          integer,intent(in) :: nixtg                                   !< array size for ixtg array
          integer,intent(in) :: numeltg                                 !< number of tri in the input file
          integer,intent(in) :: numelq                                  !< number of quad in the input file
          integer,intent(in) :: ixq(nixq,numelq)                        !< elem connectivity (1:7 : mat_id,n1,n2,n3,n4,pid,user_id for each elem 1:numelq)
          integer,intent(in) :: ixtg(nixtg,numeltg)                     !< elem connectivity (1:6 : mat_id,n1,n2,n3,n4,pid,user_id for each elem 1:numetg)
          integer,intent(in) :: numnod                                  !< number of nodes in the input file
          integer,intent(in) :: ncycle                                  !< resol cycle number (time loop)
          integer,intent(in) :: n2d                                     !< analysis flag : 0(3d), 1(axi), 2(plane strain)
          real(kind=wp),intent(in) :: x(3,numnod)                       !< node coordinates
          real(kind=wp),intent(in) :: tt                                !< current time
          type(t_ale_connectivity), intent(inout) :: ale_connect
          integer,intent(in) :: ngrnod                                  !< number of group of nodes(array size for igrnod)
          type(group_)  ,dimension(ngrnod)  :: igrnod                   !< group of nodes (data structure)
          real(kind=WP), intent(in) :: dt_scale
          real(kind=wp),intent(in) :: dt1
          type(t_ebcs_tab), target, intent(in) :: ebcs_tab              !< data structure for user boundary conditions
          
          !output
          real(kind=wp),intent(out) :: dt2t
          type(multi_cutcell_struct), intent(inout) :: multi_cutcell        !< element buffer (storage for output files : pressure, density, velocity, ...)
          integer,intent(inout) :: ityptstt                             !< element type imposing the time step (here 2)
          integer,intent(inout) :: neltstt                              !< element user id imposing the time step
          real(kind=WP),intent(inout) :: t1s                            !< simulation time
          
          ! post-treatment
          type(elbuf_struct_),dimension(ngroup) :: elbuf
          integer,intent(in) :: nparg  !< size IPARG
          integer,intent(in) :: ngroup !< number of group
          integer,intent(in) :: iparg(nparg,ngroup)

          INTEGER,INTENT(IN) :: ngrquad
          TYPE(GROUP_),INTENT(IN),DIMENSION(NGRQUAD) :: IGRQUAD
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   Local variables
          ! ----------------------------------------------------------------------------------------------------------------------
          !integer :: part_id, polyg_id, polyg_id2, polyg_id3, polyg_id4, polyg_id5  !< user parameters
          integer :: polyg_id, polyg_id2, polyg_id3, polyg_id4, polyg_id5  !< user parameters
          integer :: nel      !< number of element in the group
          integer :: nft      !< shift (group partitionning nel<=128)
          integer :: ng       !< loop on groups
          integer :: ii       !< local id in the group
          integer :: elem_iid !< internal id
          integer :: iad2,lgth, iadj, JJ !< ale_connectivity usage (elem-elem)
          real(kind=WP) :: gamma(2)
          real(kind=WP) :: largest_speed_wave
          integer :: sign !Not used for now
          integer :: nb_phase  !< number of phases
          integer(kind=8) :: nb_polygon
          class (t_ebcs), pointer :: ebcs !< pointer to ebcs data structure (in order to retrieve list of elems annd related faces)
          integer :: ibc !< boundary condition (several may be defined in the input file)
          integer :: ebcs_ityp !< boundary condition type
          integer :: nelem
          integer(kind=8) :: i_print
          integer :: num_mixed !< number of mixed cells (identified as cells intersected by the interface, and consequently requiring a cut-cell treatment)
          integer, allocatable :: list_mixed(:) !< list of mixed cells internal ids
          ! initial velocity
          integer :: ninivel
          integer :: grquad_id !< inivel quad group id (used to retrieve cells with initial velocity)
          integer :: isubmat !< inivel quad submat id
          real(kind=wp) :: vy0, vz0 !< inivel quad initial velocity
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   Precondition
          ! ----------------------------------------------------------------------------------------------------------------------
          if(ALE%solver%multimat%is_defined_mmale3 == 0) return  ! otherwise interface tracking with cut-cell is required
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   Body
          ! ----------------------------------------------------------------------------------------------------------------------
          ! [ the code must be indented with 2 spaces]
          ! [ the code must be commented ]
          ! [ routines must be short, and should not exceed 200 lines for leaf routines, 1000 lines for main routines]
          
          ! [ separators can be used between blocks of code]
          
          ! ---------------------------DO PRE-TRETMENT HERE
          ! -----------------------------------------------
          !user input
          nb_phase = 2 !currently 2, possible further extension to nb_phase > 2
          
          ! EXAMPLE OF USAGE
          if(ncycle == 0)then
            ! elem connecvitivy
            !print *, "elem iid=1 : uid,n1,n2,n3,n4 =,", 1, ixq(7, 1), ixq(2:5, 1)   !iid : internal id, uid : user id
            !print *, "elem iid=1  uid(n1),uid(n2),uid(n3),uid(n4) =,", ITAB_DEBUG(ixq(2:5, 1))   !iid : internal id, uid : user id
            
            !! polygon
            !print *, "user polygon internal identifier : ", polyg_id
            !print *, "     number of nodes : ", igrnod(polyg_id)%nentity
            !print *, "     list of nodes   : ", igrnod(polyg_id)%entity (1:igrnod(polyg_id)%nentity)
            
            !!ale_connectivity (elem-elem)
            !ELEM_IID=1
            !IAD2 = ALE_CONNECT%ee_connect%iad_connect(ELEM_IID)
            !LGTH = ALE_CONNECT%ee_connect%iad_connect(ELEM_IID+1) - IAD2
            !DO JJ=1,LGTH
            !  IADJ = ALE_CONNECT%ee_connect%connected(IAD2 + JJ - 1)
            !  IF(IADJ > 0)THEN
            !    !there is an adjacent elem on face J, its elem_iid is IADJ
            !  END IF
            !ENDDO
            
            !! node coordinates
            !print *, "node iid=3, x,y,z=", X(1:3, 3), "uid=", ITAB_DEBUG(3)

            !print *, "volumes phase 1="
            !write(*,*) elbuf(1)%bufly(1)%lbuf(1,1,1)%vol
            !print *, "volumes phase 2="
            !write(*,*) elbuf(1)%bufly(2)%lbuf(1,1,1)%vol

            !! initial velocity
            !NINIVEL = ALE%CUTCELL%NINIVEL_CUT_CELL
            !DO II=1,NINIVEL
            !    GRQUAD_ID = ALE%CUTCELL%FVM_VEL(II)%grquadid
            !    ISUBMAT = ALE%CUTCELL%FVM_VEL(II)%isubmat
            !    VY0 = ALE%CUTCELL%FVM_VEL(II)%vy
            !    VZ0 = ALE%CUTCELL%FVM_VEL(II)%vz
            !    print *, "initial velocity : grquad_id=", GRQUAD_ID, "isubmat=", ISUBMAT, "vy0=", VY0, "vz0=", VZ0
            !    NEL = IGRQUAD(GRQUAD_ID)%nentity
            !    DO JJ=1,NEL
            !        ELEM_IID = IGRQUAD(GRQUAD_ID)%entity(JJ)
            !            print *, "internal elem id=", ELEM_IID, " user elem id=", IXQ(7,ELEM_IID),  &
            !            " init with vy0=", VY0, "vz0=", VZ0, " and SUBMAT_id=", ISUBMAT
            !    END DO
            !END DO
            !IBC = IGRQUAD(1)%NENTITY

            
          end if ! ncycle == 0
          
          
          
          
          !-------------BOUNDARY CONDITION EXEMPLE OF USAGE  (polymorphism) -------------!
          !TODO : usage example witten here but this can be added in one of the following subroutines below
          DO IBC = 1, EBCS_TAB%nebcs_fvm
            EBCS => EBCS_TAB%tab(IBC)%poly
            ebcs_ityp = EBCS%type  ! ebcs can be identified by member %type (9) or by its class 'TYPE IS(t_ebcs_fluxout)'
            NELEM = EBCS%nb_elem ! number of element to treat (elems which have a user BC)
            
            SELECT TYPE (twf => EBCS)
            TYPE IS(t_ebcs_fluxout)
            !CALL FLUX_CALCULATION(twf%ielem, twf%iface)
            !   twf%ielem : list of elem ids which have a user BC
            !   twf%iface : corresponding (local) face number ( iface \in {1,2,3,4} )
          END SELECT
        ENDDO
        
        
        ! -------------------------------CALL 2D POC HERE
        ! -----------------------------------------------
        gamma(1) =  ALE%SOLVER%MULTIMAT%gamma(1)   !defined in input file
        gamma(2) =  ALE%SOLVER%MULTIMAT%gamma(2)   !defined in input file
        sign = 1 !not used for now
        
        if (dt1>0) then
          i_print = ncycle !+ 1
          call update_fluid_multicutcell(n2d, numelq, numeltg, numnod, ixq, ixtg, x, ALE_CONNECT, &
          multi_cutcell%grid, multi_cutcell%phase_vely, multi_cutcell%phase_velz, &
          multi_cutcell%phase_rho, multi_cutcell%phase_pres, &
          gamma, dt1, dt_scale, sign, ebcs_tab, &
          multi_cutcell%rho, multi_cutcell%pres, multi_cutcell%vel, multi_cutcell%etot, &
          dt2t, multi_cutcell%sound_speed, i_print)
          !call run_simple_test()
          !call exit(0)
        else
          !Initialization
          call multicutcell_mixed_cell(ngroup, elbuf, nparg, iparg, numelq, num_mixed, list_mixed) !Identifying mixed cells
            print *, "number of mixed cells : ", num_mixed
            ! internal ids : 'list_mixed(1:num_mixed)'
            ! user ids : ixq(7, internal id)
                !debug --- tcl script for HyperMesh
                ! PRINT LIST OF MIXED CELLS RECIEVED FROM STARTER. THIS ALLOW TO VISUALIZE THE MIXED CELLS IN HYPERMESH (VERIFICATION PURPOSE)
                ! do ii=1,num_mixed
                !   print *, '*createmark elements 1 ', ixq(7, list_mixed(ii))
                !   print *, '*movemark elements 1 "component1"'
                ! end do
                !debug-end
          call allocate_multi_cutcell_type(nb_phase, numelq + numeltg, multi_cutcell)

          !part_id = ALE%solver%multimat%list(1)%part_id ! by default we can take into account all elem and ignore part_id
          nb_polygon = 0
          polyg_id  = ALE%solver%multimat%list(1)%surf_id  ; if(polyg_id  > 0) nb_polygon=nb_polygon+1
          polyg_id2 = ALE%solver%multimat%list(1)%surf_id2 ; if(polyg_id2 > 0) nb_polygon=nb_polygon+1
          polyg_id3 = ALE%solver%multimat%list(1)%surf_id3 ; if(polyg_id3 > 0) nb_polygon=nb_polygon+1
          polyg_id4 = ALE%solver%multimat%list(1)%surf_id4 ; if(polyg_id4 > 0) nb_polygon=nb_polygon+1
          polyg_id5 = ALE%solver%multimat%list(1)%surf_id5 ; if(polyg_id5 > 0) nb_polygon=nb_polygon+1
          print *, "Number of provided polygons : ", nb_polygon

          call multicutcell_initial_state(ngroup, elbuf, nparg, iparg, IGRQUAD, multi_cutcell)
          call initialize_solver_multicutcell(n2d, numelq, numeltg, numnod, ixq, ixtg, &
              ngroup, nparg, iparg, x, elbuf, & !gamma, &
              nb_polygon, [polyg_id, polyg_id2, polyg_id3, polyg_id4, polyg_id5], ngrnod, igrnod, &
              !multi_cutcell,
              multi_cutcell%grid,num_mixed,list_mixed)
          call build_full_states(multi_cutcell%grid, multi_cutcell%phase_rho, &
          multi_cutcell%phase_vely, multi_cutcell%phase_velz, multi_cutcell%phase_pres, &
          gamma, &
          multi_cutcell%rho, multi_cutcell%pres, multi_cutcell%vel, multi_cutcell%etot)
          largest_speed_wave = -1.
          do ng = 1,numelq+numeltg
            multi_cutcell%sound_speed(ng) = (multi_cutcell%grid(ng,1)%lambdanp1_per_cell*& 
                                          sqrt(gamma(1) * multi_cutcell%phase_pres(ng,1) / multi_cutcell%phase_rho(ng,1)) + &
                                          multi_cutcell%grid(ng,2)%lambdanp1_per_cell*&
                                          sqrt(gamma(2) * multi_cutcell%phase_pres(ng,2) / multi_cutcell%phase_rho(ng,2)))/&
                                    (multi_cutcell%grid(ng,1)%lambdanp1_per_cell + multi_cutcell%grid(ng,2)%lambdanp1_per_cell)
            largest_speed_wave = max(largest_speed_wave, &
                                      max(abs(multi_cutcell%vel(2, ng)), abs(multi_cutcell%vel(3, ng)))&
                                              + multi_cutcell%sound_speed(ng))
          end do
          dt2t = dt_scale * sqrt(minval(multi_cutcell%grid(:, 1)%area)) / largest_speed_wave
          i_print = 1
          call print_clipped_fortran(i_print)
        end if
        
        
        ! -------------------------DO POST-TREATMENT HERE
        ! -----------------------------------------------
        ! required for time loop
        !dt2t = em01  ! time step used computed or imposed by the poc
        neltstt = 1  ! element user id imposing the minimal time step
        ityptstt = 2 !element type imposing the minimal time step
        T1S = TT     ! required to go to next cycle
        
        
        
        ! Retrieving cell data for post-treatment (ANIM / H3D output : resol > sortie_main)
        do ng=1,ngroup
          nel = iparg(2,ng)
          nft = iparg(3,ng) ! shift
          do ii=1,nel
            elem_iid = ii+nft
            !mass density
            elbuf(ng)%gbuf%rho(ii) = multi_cutcell%rho(elem_iid)
            !stress tensor
            elbuf(ng)%gbuf%sig(0*NEL+ii) = -multi_cutcell%pres(elem_iid)
            elbuf(ng)%gbuf%sig(1*NEL+ii) = -multi_cutcell%pres(elem_iid)
            elbuf(ng)%gbuf%sig(2*NEL+ii) = -multi_cutcell%pres(elem_iid)

            !elbuf(ng)%gbuf%vel(0*NEL+ii) = multi_cutcell%vel(0, elem_iid)
            !elbuf(ng)%gbuf%vel(1*NEL+ii) = multi_cutcell%vel(1, elem_iid)
            !elbuf(ng)%gbuf%vel(2*NEL+ii) = multi_cutcell%vel(2, elem_iid)
            
            elbuf(ng)%BUFLY(1)%LBUF(1,1,1)%rho(ii) = multi_cutcell%phase_rho(elem_iid,1)
            elbuf(ng)%BUFLY(2)%LBUF(1,1,1)%rho(ii) = multi_cutcell%phase_rho(elem_iid,2)
            
            elbuf(ng)%BUFLY(1)%LBUF(1,1,1)%ssp(ii) = multi_cutcell%sound_speed(elem_iid)
            elbuf(ng)%BUFLY(2)%LBUF(1,1,1)%ssp(ii) = multi_cutcell%sound_speed(elem_iid)
            
            elbuf(ng)%BUFLY(1)%LBUF(1,1,1)%sig(0*NEL+ii) = -multi_cutcell%phase_pres(elem_iid,1)
            elbuf(ng)%BUFLY(1)%LBUF(1,1,1)%sig(1*NEL+ii) = -multi_cutcell%phase_pres(elem_iid,1)
            elbuf(ng)%BUFLY(1)%LBUF(1,1,1)%sig(2*NEL+ii) = -multi_cutcell%phase_pres(elem_iid,1)
            !
            elbuf(ng)%BUFLY(2)%LBUF(1,1,1)%sig(0*NEL+ii) = -multi_cutcell%phase_pres(elem_iid,2)
            elbuf(ng)%BUFLY(2)%LBUF(1,1,1)%sig(1*NEL+ii) = -multi_cutcell%phase_pres(elem_iid,2)
            elbuf(ng)%BUFLY(2)%LBUF(1,1,1)%sig(2*NEL+ii) = -multi_cutcell%phase_pres(elem_iid,2)
            
            ! note : MULTI_CUTCELL%EINT ! is output as internal energy per unit volume (SI:J/m3) :  rho.e = EINT/VOL
            !        consequently  MULTI_CUTCELL%EINT / MULTI_CUTCELL%RHO is internal energy density :  e
            ! + remove kinetic energy
            elbuf(ng)%gbuf%eint(ii) = multi_cutcell%rho(elem_iid) * (multi_cutcell%etot(elem_iid) - 0.5 * &
            (multi_cutcell%vel(1,elem_iid)*multi_cutcell%vel(1,elem_iid) + &
            multi_cutcell%vel(2,elem_iid)*multi_cutcell%vel(2,elem_iid)))
            
            !element time step
            elbuf(ng)%gbuf%dt(ii) = max(abs(multi_cutcell%vel(1,elem_iid)), abs(multi_cutcell%vel(2,elem_iid))) &
            + multi_cutcell%sound_speed(elem_iid)
            
            !volume fraction
            ELBUF(NG)%BUFLY(1)%LBUF(1,1,1)%VOL(II) =  multi_cutcell%grid(elem_iid, 1)%lambdanp1_per_cell
            ELBUF(NG)%BUFLY(2)%LBUF(1,1,1)%VOL(II) =  multi_cutcell%grid(elem_iid, 2)%lambdanp1_per_cell
            !if (multi_cutcell%grid(elem_iid, 1)%is_narrowband) then
            !  ELBUF(NG)%BUFLY(1)%LBUF(1,1,1)%ssp(II) =  1.0
            !  ELBUF(NG)%BUFLY(2)%LBUF(1,1,1)%ssp(II) =  1.0
            !else
            !  ELBUF(NG)%BUFLY(1)%LBUF(1,1,1)%ssp(II) =  0.0
            !  ELBUF(NG)%BUFLY(2)%LBUF(1,1,1)%ssp(II) =  0.0
            !endif
            !if (multi_cutcell%grid(elem_iid, 1)%close_cells) then
            !  ELBUF(NG)%BUFLY(1)%LBUF(1,1,1)%ssp(II) = 1.0
            !  ELBUF(NG)%BUFLY(2)%LBUF(1,1,1)%ssp(II) = 1.0
            !else
            !  ELBUF(NG)%BUFLY(1)%LBUF(1,1,1)%ssp(II) = 0.0
            !  ELBUF(NG)%BUFLY(2)%LBUF(1,1,1)%ssp(II) = 0.0
            !end if
            
            !velocity-phase1
            elbuf(ng)%BUFLY(1)%LBUF(1,1,1)%vel(ii)     = multi_cutcell%phase_vely(elem_iid,1)
            elbuf(ng)%BUFLY(1)%LBUF(1,1,1)%vel(nel+ii) = multi_cutcell%phase_velz(elem_iid,1)
            !velocity-phase2
            elbuf(ng)%BUFLY(2)%LBUF(1,1,1)%vel(ii)     = multi_cutcell%phase_vely(elem_iid,2)
            elbuf(ng)%BUFLY(2)%LBUF(1,1,1)%vel(nel+ii) = multi_cutcell%phase_velz(elem_iid,2)
            
            
          end do
        enddo
        
        
        
        ! ----------------------------------------------------------------------------------------------------------------------
      end subroutine alemain_cutcell
    end module alemain_cutcell_mod
