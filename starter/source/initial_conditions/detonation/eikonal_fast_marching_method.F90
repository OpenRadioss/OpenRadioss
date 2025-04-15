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
      !||    eikonal_fast_marching_method_mod   ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
      !||--- called by ------------------------------------------------------
      !||    eikonal_solver                     ../starter/source/initial_conditions/detonation/eikonal_solver.F90
      !||====================================================================
      module eikonal_fast_marching_method_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Solver for the Eikonal equation (arrival time depending on medium velocity)
!! \details The Fast Marching Method (FMM) is used to solve this equation on elem centroids.
!! \details Connectivity is provided with nod2el and knod2el arrays
!! \details updown(:) array is used to define narrow band 1:frozen, 0:narrow_band, -1:far
      !||====================================================================
      !||    eikonal_fast_marching_method     ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
      !||--- called by ------------------------------------------------------
      !||    eikonal_solver                   ../starter/source/initial_conditions/detonation/eikonal_solver.F90
      !||--- calls      -----------------------------------------------------
      !||    eikonal_compute_adjacent         ../starter/source/initial_conditions/detonation/eikonal_compute_adjacent.F90
      !||    eikonal_init_mixture_vel         ../starter/source/initial_conditions/detonation/eikonal_ini_mixture_vel.F90
      !||    eikonal_init_start_list_2d       ../starter/source/initial_conditions/detonation/eikonal_init_start_list_2d.F90
      !||    eikonal_remove_first             ../starter/source/initial_conditions/detonation/eikonal_remove_first.F90
      !||    eikonal_sort_narrow_band         ../starter/source/initial_conditions/detonation/eikonal_sort_narrow_band.F90
      !||--- uses       -----------------------------------------------------
      !||    detonators_mod                   ../starter/share/modules1/detonators_mod.F
      !||    eikonal_compute_adjacent_mod     ../starter/source/initial_conditions/detonation/eikonal_compute_adjacent.F90
      !||    eikonal_init_mixture_vel_mod     ../starter/source/initial_conditions/detonation/eikonal_ini_mixture_vel.F90
      !||    eikonal_init_start_list_2d_mod   ../starter/source/initial_conditions/detonation/eikonal_init_start_list_2d.F90
      !||    eikonal_remove_first_mod         ../starter/source/initial_conditions/detonation/eikonal_remove_first.F90
      !||    eikonal_sort_narrow_band_mod     ../starter/source/initial_conditions/detonation/eikonal_sort_narrow_band.F90
      !||====================================================================
        subroutine eikonal_fast_marching_method(&
                                     ix,nix,numel,x,numnod, &
                                     elbuf_tab,ngroup,nparg,iparg,ale_connectivity,npropm,nummat,pm,&
                                     detonators, idet, nvois, nod2el, knod2el, npropmi, ipm)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : zero, one, ep20,ep21, fourth, third, one_over_8
          use elbufdef_mod, only : elbuf_struct_
          use ale_connectivity_mod , only : t_ale_connectivity
          use eikonal_sort_narrow_band_mod , only : eikonal_sort_narrow_band
          use eikonal_remove_first_mod , only : eikonal_remove_first
          use eikonal_compute_adjacent_mod , only : eikonal_compute_adjacent
          use eikonal_init_start_list_2d_mod , only : eikonal_init_start_list_2d
          use eikonal_init_mixture_vel_mod , only : eikonal_init_mixture_vel
          use detonators_mod , only : detonators_struct_
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
          integer,intent(in) :: ngroup !< number of groups
          integer,intent(in) :: nix   !< array size ix
          integer,intent(in) :: numnod !< number of nodes
          integer,intent(in) :: numel !< number of quad elems (solid)
          integer,intent(in) :: ix(nix,numel) !< quad connectivities
          integer,intent(in) :: nparg !< array size
          integer,intent(in) :: iparg(nparg,ngroup)
          my_real,intent(in) :: x(3,numnod) !< node coordinates
          integer,intent(in) :: npropm, nummat
          my_real,intent(in) :: pm(npropm,nummat)
          type (elbuf_struct_), target, dimension(ngroup) :: elbuf_tab
          type (t_ale_connectivity), intent(inout) :: ale_connectivity
          type (detonators_struct_),intent(in) :: detonators
          integer,intent(in) :: idet
          integer,intent(in) :: nod2el(nvois*numel)
          integer,intent(in) :: knod2el(numnod+1)
          integer,intent(in) :: nvois !< max number of adjacent cells
          integer,intent(in) :: npropmi
          integer,intent(in) :: ipm(npropmi, nummat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ng
          integer :: ity
          integer :: mlw
          integer :: nel
          integer :: neldet
          integer :: nft
          integer :: mat_det
          integer :: iel
          integer :: i  !<local elem id
          integer :: ii,jj !< loops
          integer :: iad1,LGTH,ie, iev !< local variable for adjacent cell loop
          integer :: inod(8) !tria 3<=8,  quad 4<=8, hexa 8
          integer :: n_queue
          integer, allocatable, dimension(:) :: idx_ng  !< group identifier for a given solid elem
          integer, allocatable, dimension(:) :: idx_i   !< local id in group
          integer, allocatable, dimension(:) :: elem_list
          my_real, allocatable, dimension(:) :: vel
          my_real :: vel_adj(6)   ! tria 3<6, quad:4<6, hexa 6
          my_real, allocatable, dimension(:) :: tdet   !< detonation time
          my_real :: tdet_adj(6)  ! tria 3<6, quad:4<6, hexa 6
          my_real, allocatable, dimension(:,:) :: Xel   !< centroids coordinates
          my_real :: xel_adj(3,6) ! tria 3<6, quad:4<6, hexa 6
          integer, allocatable, dimension(:) :: priority_queue_id
          my_real, allocatable, dimension(:) :: priority_queue_tt
          integer,allocatable, dimension(:) :: updown ! -1 down, 1:up, 0:narrow_band
          integer :: nstart !< number of deotnation points (centroids)  ! can be adapt later from mesh nodes to elem centroid (spherical wave from node to centroid)
          integer,allocatable,dimension(:) :: start_elem_list
          my_real,allocatable,dimension(:) :: start_elem_tdet
          integer, allocatable, dimension(:) :: elem_list_bij
          integer num_new_activated, list_new_activated(6)
          my_real :: dx,dy,dz,dl,s, tmp
          my_real :: Dcj !< Detonation velocity (law5 and 97)
          integer :: multimat_id !< 0:default, 51:if law51 found, 151:if law151 found
          integer :: mid
          integer :: ishadow !< shadowing option for detonators (Eikonal equation solver)
          my_real :: fac
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

         mat_det = detonators%point(idet)%mat

         !numbering
         neldet = 0
         do ng=1,ngroup
           mlw = iparg(1,ng)
           nel = iparg(2,ng)
           nft = iparg(3,ng)
           ity = iparg(5,ng)
           mid = ix(1,nft+1)
           ishadow = nint(pm(96,mid))
           if(ishadow == 0)cycle
           if(ity == 1 .or. ity == 2 .or. ity == 7)then
             if(mlw == 5 .or. mlw == 97 .or. mlw == 51 .or. mlw == 151)then
               if(mat_det == 0 .or. ix(1,nft+1) == mat_det)then
                 neldet = neldet + nel
                end if
             end if
           endif
         enddo

         ! size neldet
         allocate(elem_list(neldet)) ; elem_list(:) = 0
         allocate(idx_ng(neldet))    ; idx_ng(:) = 0
         allocate(idx_i(neldet))     ; idx_i(:) = 0
         allocate(updown(neldet))    ; updown(:) = -1
         allocate(tdet(neldet))      ; tdet(:) = ep21
         allocate(vel(neldet))       ; vel(:) = ep20 !lowest chosen for stability
         allocate(Xel(3,neldet))     ; Xel(:,:) = zero
         allocate(priority_queue_id(neldet)) ; priority_queue_id(:) = 0
         allocate(priority_queue_tt(neldet)) ; priority_queue_tt(:) = ep21

         ! size numel
         allocate(elem_list_bij(numel)) ; elem_list_bij = 0

         ! list of relevant centroids
         !   and group id and local id in this group : elem_list(k) -> (idx_ng(k), idx_i(k))   ! used to set burning time ELBUF_TAB(NG)MGBUF%TB(I)
         neldet = 0
         iad1 = neldet + 1
         lgth = 0
         do ng=1,ngroup
           multimat_id = 0
           mlw = iparg(1,ng)
           nel = iparg(2,ng)
           nft = iparg(3,ng)
           ity = iparg(5,ng)
           mid = ix(1,nft+1)
           ishadow = nint(pm(96,mid))
           if(ishadow == 0)cycle
           if(ity /=1 .and. ity /= 2 .and. ity /= 7)cycle
           if(mid /= mat_det .and. mat_det /= 0)cycle
           Dcj = pm(38,mid)
           if(mlw == 51)then
             multimat_id = 51
           elseif(mlw == 151)then
             multimat_id = 151
           elseif(mlw /= 5 .and. mlw /= 97)then
             cycle
           endif
           if(ity == 2)then
             fac = fourth
           elseif(ity == 7)then
             fac = third
           else !ity == 1
             fac = one_over_8
           end if
           if(nvois < 6)then !2d
             do i=1,nel
               !building list
               neldet = neldet + 1
               lgth = lgth + 1
               elem_list(neldet) = i+nft       ! elem_list     : [1..neldet] -> [1..numel]
               idx_ng(neldet) = ng
               idx_i(neldet) = i
               elem_list_bij(i+nft) = neldet   ! elem_list_inv : [1..numel] -> [1..neldet]
               !centroid coordinates
               inod(1:nvois) = ix(2:nvois+1, i+nft)
               xel(1, neldet) = zero ! x-center
               xel(2, neldet) = fac * sum(x(2,inod(1:nvois))) ! y-center
               xel(3, neldet) = fac * sum(x(3,inod(1:nvois))) ! z-center
               !medium velocity
               vel(neldet) = Dcj ! chapman jouget velocity
             enddo
           else ! 3d
             do i=1,nel
               !building list
               neldet = neldet + 1
               lgth = lgth + 1
               elem_list(neldet) = i+nft       ! elem_list     : [1..neldet] -> [1..numel]
               idx_ng(neldet) = ng
               idx_i(neldet) = i
               elem_list_bij(i+nft) = neldet   ! elem_list_inv : [1..numel] -> [1..neldet]
               !centroid coordinates
               inod(1:8) = ix(2:9, i+nft)
               xel(1, neldet) = fac * sum(x(1,inod(1:8))) ! x-center
               xel(2, neldet) = fac * sum(x(2,inod(1:8))) ! y-center
               xel(3, neldet) = fac * sum(x(3,inod(1:8))) ! z-center
               !medium velocity
               vel(neldet) = Dcj ! chapman jouget velocity
             enddo
           end if
           if(multimat_id /= 0)then
             ! Dcj not initialized for multimaterial law
             ! it has no sens since there is a possible mixture
             call eikonal_init_mixture_vel(lgth,vel(iad1),idx_ng(iad1),idx_i(iad1),multimat_id,mid,&
                                           ngroup,nummat,npropm,pm,npropmi,ipm,elbuf_tab, &
                                           nparg, iparg)
             iad1 = neldet + 1
             lgth = 0
           end if
         enddo

         call eikonal_init_start_list_2d(nstart, start_elem_list, start_elem_tdet, detonators, numel, numnod, &
                                         nvois, nod2el, knod2el, ale_connectivity, elem_list_bij, neldet, xel, x,&
                                         nix, ix, mat_det, vel)

         if(nstart == 0)then
           !DEALLOCATE
           if(allocated(start_elem_list))deallocate(start_elem_list)
           if(allocated(start_elem_tdet))deallocate(start_elem_tdet)
           if(allocated(elem_list))deallocate(elem_list)
           if(allocated(idx_ng))deallocate(idx_ng)
           if(allocated(idx_i))deallocate(idx_i)
           if(allocated(updown))deallocate(updown)
           if(allocated(tdet))deallocate(tdet)
           if(allocated(vel))deallocate(vel)
           if(allocated(Xel))deallocate(Xel)
           if(allocated(priority_queue_id))deallocate(priority_queue_id)
           if(allocated(priority_queue_tt))deallocate(priority_queue_tt)
           if(allocated(elem_list_bij))deallocate(elem_list_bij)
           return
         end if

         ! initial detonation locations
         ! mark first upwind points (centroids)
         do jj=1,nstart
           updown(start_elem_list(jj)) = 1
           tdet(start_elem_list(jj)) = start_elem_tdet(jj)
         enddo ! next jj

         ! initial narrow band
         !    mark first points in the narrow band (close)
         !    by the way list their detonation time
         n_queue = 0
         do ii=1,neldet
           if(updown(ii) == 1)then
             !tag adjacent cells with updown -1
             ie = elem_list(ii)
             IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
             LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(IE+1) - IAD1
             DO JJ=1,LGTH
               IEV = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + JJ - 1)
               IF(IEV == 0)CYCLE
               iel = elem_list_bij(iev)
               if(updown(iel) == -1) then
                 if(mat_det /= 0 .and. ix(1,iev) /= mat_det)cycle
                 updown(iel) = 0
                 n_queue = n_queue + 1
                 s=max(vel(iel),vel(ie))
                 s=one/s
                 dx = xel(1,ie)-xel(1,iel)
                 dy = xel(2,ie)-xel(2,iel)
                 dz = xel(3,ie)-xel(3,iel)
                 dl = sqrt(dx*dx + dy*dy + dz*dz)
                 priority_queue_id(n_queue) = iel
                 priority_queue_tt(n_queue) = tdet(ie) + dl*s
                 tdet(iel) = priority_queue_tt(n_queue)
               end if
             ENDDO
           end if
         enddo ! next ii
         call eikonal_sort_narrow_band(priority_queue_id,priority_queue_tt,n_queue)

         ! main loop -------------------------------------------------------------------------
         ! FAST MARCHING ALGORIHTM
         do while (n_queue > 0)

           !freeze minimum
           ie = priority_queue_id(1) ! list of  priority_queue_tt is already sorted
           updown(ie) = 1
           call eikonal_remove_first(priority_queue_id,priority_queue_tt,n_queue)

           !compute adjacent (might be far)
           call eikonal_compute_adjacent(ie, ALE_CONNECTIVITY,neldet, &
                                           tdet,tdet_adj,vel,vel_adj,xel,xel_adj,numel,elem_list_bij, &
                                           updown, num_new_activated, list_new_activated,  mat_det, &
                                           nix,ix, nvois )

           !we may init only updated tdet from previous call above
           do ii=1,n_queue
             priority_queue_tt(ii) = tdet(elem_list_bij(priority_queue_id(ii)))
           end do

           do ii=1,num_new_activated
             n_queue = n_queue + 1
             ie = list_new_activated(ii)
             priority_queue_id(n_queue) = ie
             priority_queue_tt(n_queue) = tdet(ie)
           end do

           ! reorder priority queue
           call eikonal_sort_narrow_band(priority_queue_id,priority_queue_tt,n_queue)

         enddo !wend
         ! end of main loop -------------------------------------------------------------------------

         ! initialize element buffer (arrival times)
         do ii=1,neldet
           ng = idx_ng(ii)
           i  = idx_i(ii)
           tmp = -elbuf_tab(ng)%gbuf%tb(i)
           tmp = min (tmp, tdet(ii))
           elbuf_tab(ng)%gbuf%tb(i) = -tmp
         end do

         !DEALLOCATE
         if(allocated(start_elem_list))deallocate(start_elem_list)
         if(allocated(start_elem_tdet))deallocate(start_elem_tdet)
         if(allocated(elem_list))deallocate(elem_list)
         if(allocated(idx_ng))deallocate(idx_ng)
         if(allocated(idx_i))deallocate(idx_i)
         if(allocated(updown))deallocate(updown)
         if(allocated(tdet))deallocate(tdet)
         if(allocated(vel))deallocate(vel)
         if(allocated(Xel))deallocate(Xel)
         if(allocated(priority_queue_id))deallocate(priority_queue_id)
         if(allocated(priority_queue_tt))deallocate(priority_queue_tt)
         if(allocated(elem_list_bij))deallocate(elem_list_bij)

        end subroutine eikonal_fast_marching_method
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
      end module eikonal_fast_marching_method_mod
