Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      subroutine ns_fvm_diffusion(ale_connect, multi_fvm, time_step, ebcs_tab, 
     .     diffusion, ipm, pm, iparg, elbuf_tab, nercvois, nesdvois, lercvois, lesdvois, 
     .     ixs, func_value)
      USE ALE_CONNECTIVITY_MOD
      USE MULTI_FVM_MOD
      USE DIFFUSION_MOD
      USE EBCS_MOD
      USE ELBUFDEF_MOD
c-----------------------------------------------
c   i m p l i c i t   t y p e s
c-----------------------------------------------
#include      "implicit_f.inc"
!     nspmd, ngroup
#include "com01_c.inc"
!     numels
#include "com04_c.inc"
!     nsvois
!     npropmi, npropm, nparg
#include "param_c.inc"
c-----------------------------------------------
c   m e s s a g e   p a s s i n g
c-----------------------------------------------
#ifdef mpi
#endif
!     dummy
      type(t_ale_connectivity), intent(in) :: ale_connect
      type(multi_fvm_struct), intent(inout) :: multi_fvm
      my_real, intent(in) :: time_step
      type(t_diffusion), intent(inout) :: diffusion
      type(t_ebcs_tab), intent(inout), target :: ebcs_tab
      integer, intent(in) :: ipm(npropmi, *)
      my_real, intent(in) :: pm(npropm, *)
      integer, intent(in) :: iparg(nparg, *)
      integer, intent(in) :: nercvois(*), nesdvois(*), lercvois(*), lesdvois(*)
      integer, intent(in) :: ixs(nixs, numels)
      my_real, intent(in) :: func_value(*)
      type(elbuf_struct_), target, dimension(ngroup), intent(inout) :: elbuf_tab
c----------------------------------------------
c   l o c a l   v a r i a b l e s
c-----------------------------------------------
      integer :: i, ii, jj, kk, ng, iebcs, typ, nelem, ielem
      my_real :: mass, surf, ux, uy, uz, nu, dist, xk(3), xl(3), xf(3)
      double precision, dimension(:), pointer :: sol
      my_real :: vel(3), nx, ny, nz
      integer :: mtn, nel, nft, imat, submatlaw, submatid, lencom, ifunc
      type(g_bufel_), pointer :: gbuf  
      integer :: mat_nz, glob_dim, ierr, max_id, icount1, icount2
      class (t_ebcs), pointer :: ebcs
      integer :: iad, lgth
      integer :: kface

!     diffusion coefficient
      diffusion%nu(:) = zero
      if (multi_fvm%nbmat == 1) then
         do ng = 1, ngroup
            mtn = iparg(1, ng)
            if (mtn == 151) then
               nel = iparg(2, ng)
               nft = iparg(3, ng)
               submatid = ipm(20 + 1, ixs(1, 1 + nft))
               submatlaw = ipm(2, submatid)
               if (submatlaw == 6) then
                  do ii = 1, nel
                     i = ii + nft
                     diffusion%nu(i) = pm(24, submatid) * multi_fvm%rho(i)
                  enddo
               endif
            endif
         enddo
      else
         do ng = 1, ngroup
            mtn = iparg(1, ng)
            if (mtn == 151) then
               nel = iparg(2, ng)
               nft = iparg(3, ng)
               do imat = 1, multi_fvm%nbmat
                  submatid = ipm(20 + imat, ixs(1, 1 + nft))
                  submatlaw = ipm(2, submatid)
                  if (submatlaw == 6) then
                     do ii = 1, nel
                        i = ii + nft
                        diffusion%nu(i) = diffusion%nu(i) + multi_fvm%phase_alpha(imat, i) * 
     .                       pm(24, submatid) * multi_fvm%phase_rho(imat, i)
                     enddo
                  endif
               enddo
            endif
         enddo
      endif
!     mpi comm
      if (nspmd > 1) then
         lencom = nercvois(nspmd + 1) + nesdvois(nspmd + 1)
         call spmd_e1vois(diffusion%nu, 
     .        nercvois, nesdvois, lercvois, lesdvois, lencom)
      endif


!     ebcs / fluxout and ebcs / inlet : newman boundary conditions
      if (ebcs_tab%nebcs_fvm > 0) then
!     flag_outlet = 0 for standard dirichlet condition: u_boundary = 0 un Euler, or w_grid in ALE. Rhs needs be modified
!     flag_outlet = -1 for newman boundary condition
!     flag_outlet = iebcs for dirichlet boundary condition, value of velocity is then taken as the one given in the inlet card.
         if (.not. diffusion%outlet_flagged) then
            do iebcs = 1, ebcs_tab%nebcs_fvm
               typ = ebcs_tab%tab(iebcs)%poly%type
               nelem = ebcs_tab%tab(iebcs)%poly%nb_elem
               ebcs => ebcs_tab%tab(iebcs)%poly
               select type (ebcs)
               type is (t_ebcs_inlet)
!     inlet
               do ielem = 1, nelem
                  ii = ebcs_tab%tab(iebcs)%poly%ielem(ielem)
                  jj = ebcs_tab%tab(iebcs)%poly%iface(ielem)
                  diffusion%flag_outlet(6 * (ii - 1) + jj) = iebcs
               enddo
               type is (t_ebcs_fluxout)
!     fluxout
               do ielem = 1, nelem
                  ii = ebcs_tab%tab(iebcs)%poly%ielem(ielem)
                  jj = ebcs_tab%tab(iebcs)%poly%iface(ielem)
                  diffusion%flag_outlet(6 * (ii - 1) + jj) = -1
               enddo
               end select
            enddo
            diffusion%outlet_flagged = .true.
         endif
      endif

      diffusion%rhs%val(1:3 * numels) = zero
!     fill in matrix + rhs
      icount1 = 0
      icount2 = numels
      do ii = 1, numels
         iad = ale_connect%ee_connect%iad_connect(ii)
         lgth = ale_connect%ee_connect%iad_connect(ii+1) - iad
         icount1 = icount1 + 1
         diffusion%mat%irow(icount1) = ale_connect%idglob%id(ii)
         diffusion%mat%jcol(icount1) = ale_connect%idglob%id(ii)
         xk(1:3) = multi_fvm%elem_data%centroid(1:3, ii)
         mass = multi_fvm%vol(ii) * multi_fvm%rho(ii)
         diffusion%mat%val(icount1) = mass
!     diagonal part
         do jj = 1, lgth
            kk = ale_connect%ee_connect%connected(iad + jj - 1)
            if (kk > 0) then
               surf = multi_fvm%face_data%surf(jj, ii)
               xl(1:3) = multi_fvm%elem_data%centroid(1:3, kk)
               dist = sqrt((xk(1) - xl(1))**2 + (xk(2) - xl(2))**2 + (xk(3) - xl(3))**2)
               nu = 0.5 * (diffusion%nu(ii) + diffusion%nu(kk))
               diffusion%mat%val(icount1) = diffusion%mat%val(icount1) + time_step * surf * nu / dist
            else if (diffusion%flag_outlet(6 * (ii - 1) + jj) == -1) then
!     Neumann boundary condition
            else if (diffusion%flag_outlet(6 * (ii - 1) + jj) >= 0) then
!     Dirichlet boundary condition
               if (diffusion%flag_outlet(6 * (ii - 1) + jj) == 0) then
                  vel(1:3) = zero
               else
                  iebcs = diffusion%flag_outlet(6 * (ii - 1) + jj)
                  ebcs => ebcs_tab%tab(iebcs)%poly
                  nx = multi_fvm%face_data%normal(1, jj, ii)
                  ny = multi_fvm%face_data%normal(2, jj, ii)
                  nz = multi_fvm%face_data%normal(3, jj, ii)
                  select type (ebcs)
                  type is (t_ebcs_inlet)
                  if (ebcs%fvm_inlet_data%vector_velocity == 0) then
!     normal velocity imposed
                     ifunc = ebcs%fvm_inlet_data%func_vel(1)
                     if (ifunc > 0) then
                        vel(1) = -ebcs%fvm_inlet_data%val_vel(1) * func_value(ifunc) * nx
                        vel(2) = -ebcs%fvm_inlet_data%val_vel(2) * func_value(ifunc) * ny
                        vel(3) = -ebcs%fvm_inlet_data%val_vel(3) * func_value(ifunc) * nz
                     else
                        vel(1) = -ebcs%fvm_inlet_data%val_vel(1) * nx
                        vel(2) = -ebcs%fvm_inlet_data%val_vel(2) * ny
                        vel(3) = -ebcs%fvm_inlet_data%val_vel(3) * nz
                     endif
                  else
!     all components of velocity are imposed
                     ifunc = ebcs%fvm_inlet_data%func_vel(1)
                     if (ifunc > 0) then
                        vel(1) = ebcs%fvm_inlet_data%val_vel(1) * func_value(ifunc)
                     else
                        vel(1) = ebcs%fvm_inlet_data%val_vel(1)
                     endif
                     ifunc = ebcs%fvm_inlet_data%func_vel(2)
                     if (ifunc > 0) then
                        vel(2) = ebcs%fvm_inlet_data%val_vel(2) * func_value(ifunc)
                     else
                        vel(2) = ebcs%fvm_inlet_data%val_vel(2)
                     endif
                     ifunc = ebcs%fvm_inlet_data%func_vel(3)
                     if (ifunc > 0) then
                        vel(3) = ebcs%fvm_inlet_data%val_vel(3) * func_value(ifunc)
                     else
                        vel(3) = ebcs%fvm_inlet_data%val_vel(3)
                     endif
                  endif
               end select
               endif
               surf = multi_fvm%face_data%surf(jj, ii)
               xf(1:3) = multi_fvm%face_data%centroid(1:3, jj, ii)
               xl(1:3) = TWO * xf(1:3) - xk(1:3)
               dist = sqrt((xk(1) - xl(1))**2 + (xk(2) - xl(2))**2 + (xk(3) - xl(3))**2)
               nu = diffusion%nu(ii)
               diffusion%mat%val(icount1) = diffusion%mat%val(icount1) + time_step * surf * nu / dist
               diffusion%rhs%val(ii + (1 - 1) * numels) = diffusion%rhs%val(ii + (1 - 1) * numels) + 
     .              time_step * surf * nu * vel(1) / dist
               diffusion%rhs%val(ii + (2 - 1) * numels) = diffusion%rhs%val(ii + (2 - 1) * numels) + 
     .              time_step * surf * nu * vel(2) / dist
               diffusion%rhs%val(ii + (3 - 1) * numels) = diffusion%rhs%val(ii + (3 - 1) * numels) + 
     .              time_step * surf * nu * vel(3) / dist
            endif
         enddo
!     non diagonal part
         do jj = 1, lgth
            kk = ale_connect%ee_connect%connected(iad + jj - 1)
            if (kk > 0) then
               icount2 = icount2 + 1
               diffusion%mat%irow(icount2) = ale_connect%idglob%id(ii)
               diffusion%mat%jcol(icount2) = ale_connect%idglob%id(kk)
               surf = multi_fvm%face_data%surf(jj, ii)
               xl(1:3) = multi_fvm%elem_data%centroid(1:3, kk)
               dist = sqrt((xk(1) - xl(1))**2 + (xk(2) - xl(2))**2 + (xk(3) - xl(3))**2)
               nu = 0.5 * (diffusion%nu(ii) + diffusion%nu(kk))
               diffusion%mat%val(icount2) = - time_step * surf * nu / dist
            endif
         enddo
      enddo

!     right hand side
      icount1 = 0
      do ii = 1, numels
         ux = multi_fvm%vel(1, ii)
         uy = multi_fvm%vel(2, ii)
         uz = multi_fvm%vel(3, ii)
         mass = multi_fvm%vol(ii) * multi_fvm%rho(ii)
         icount1 = icount1 + 1
         diffusion%rhs%irow(icount1) = ale_connect%idglob%id(ii)
         diffusion%rhs%val(ii + (1 - 1) * numels) = diffusion%rhs%val(ii + (1 - 1) * numels) + mass * ux
         diffusion%rhs%val(ii + (2 - 1) * numels) = diffusion%rhs%val(ii + (2 - 1) * numels) + mass * uy
         diffusion%rhs%val(ii + (3 - 1) * numels) = diffusion%rhs%val(ii + (3 - 1) * numels) + mass * uz
      enddo
      
      nullify(sol)
      call diffusion%solve_diffusion()
      call diffusion%get_solution(sol, glob_dim)

      if (associated(sol)) then
         do ii = 1, numels
            ux = multi_fvm%vel(1, ii)
            uy = multi_fvm%vel(2, ii)
            uz = multi_fvm%vel(3, ii)
            multi_fvm%eint(ii) = multi_fvm%eint(ii) + 0.5 * multi_fvm%rho(ii) * (ux**2 + uy**2 + uz**2)
            ux = diffusion%sol(ale_connect%idglob%id(ii) + 0 * glob_dim)
            uy = diffusion%sol(ale_connect%idglob%id(ii) + 1 * glob_dim)
            uz = diffusion%sol(ale_connect%idglob%id(ii) + 2 * glob_dim)
            multi_fvm%vel(1, ii) = ux
            multi_fvm%vel(2, ii) = uy
            multi_fvm%vel(3, ii) = uz
            multi_fvm%eint(ii) = multi_fvm%eint(ii) - 0.5 * multi_fvm%rho(ii) * (ux**2 + uy**2 + uz**2)
         enddo
      endif
      end
