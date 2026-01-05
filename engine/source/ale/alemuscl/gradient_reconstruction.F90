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
!||    gradient_reconstruction_mod     ../engine/source/ale/alemuscl/gradient_reconstruction.F90
!||--- called by ------------------------------------------------------
!||    ale51_gradient_reconstruction   ../engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
!||====================================================================
      module gradient_reconstruction_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief here is a small description of the routine, [after the header]
!! \details if needed, more details can be added here
!||====================================================================
!||    gradient_reconstruction_new     ../engine/source/ale/alemuscl/gradient_reconstruction.F90
!||--- called by ------------------------------------------------------
!||    ale51_gradient_reconstruction   ../engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
!||--- calls      -----------------------------------------------------
!||    conjugate_gradient_vec          ../engine/source/ale/alemuscl/conjugate_gradient_vec.F90
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod            ../common_source/modules/ale/ale_connectivity_mod.F
!||    alemuscl_mod                    ../common_source/modules/ale/alemuscl_mod.F
!||    conjugate_gradient_vec_mod      ../engine/source/ale/alemuscl/conjugate_gradient_vec.F90
!||    constant_mod                    ../common_source/modules/constant_mod.F
!||    element_mod                     ../common_source/modules/elements/element_mod.F90
!||    precision_mod                   ../common_source/modules/precision_mod.F90
!||    segvar_mod                      ../engine/share/modules/segvar_mod.F
!||====================================================================
        subroutine gradient_reconstruction_new(numels,numnod,nel,nft,trimat,facet_nb,ixs,x,ale_connect,segvar)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use alemuscl_mod
          use segvar_mod
          use ale_connectivity_mod
          use conjugate_gradient_vec_mod , only : conjugate_gradient_vec
          use element_mod , only :nixs
          use precision_mod , only : wp
          use constant_mod , only : zero,two,half,third,em10
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: numels !< number of solid elements
          integer, intent(in) :: numnod !< number of nodes
          integer, intent(in) :: nel !< number of element of the current group
          integer, intent(in) :: nft !< first element index of the current group
          integer, intent(in) :: trimat !< number of sub-materials
          integer, intent(in) :: facet_nb !< number of facets per element (tetra --> 4, hexa --> 6)
          integer, intent(in) :: ixs(nixs,numels) !< element to nodes connectivity
          real(kind=wp), intent(in) :: x(3,numnod) !< nodal coordinates
          type(t_ale_connectivity), intent(in) :: ale_connect !< ale connectivity data          
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,ii,kk,j,imat
          integer :: iad2,lgth
          real(kind=wp) :: xf,yf,zf,vall
          real(kind=wp), dimension(nel) :: xk,yk,zk
          real(kind=wp), dimension(nel,facet_nb) :: xl,yl,zl
          real(kind=wp) :: valk(nel,trimat)
          real(kind=wp) :: mat(nel,3,3), rhs(nel,3,trimat), sol(nel,3)
          integer :: vois_id
          integer :: face_to_node_local_id(6, 4)
          real(kind=wp) :: tol,norm(3), a(3), b(3), c(3), surf, surf1, surf2
          type(t_segvar) :: segvar
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          ! once for all, associate node local id to a face number
          ! face 1
          face_to_node_local_id(1, 1) = 1
          face_to_node_local_id(1, 2) = 4
          face_to_node_local_id(1, 3) = 3
          face_to_node_local_id(1, 4) = 2
          ! face 2
          face_to_node_local_id(2, 1) = 3
          face_to_node_local_id(2, 2) = 4
          face_to_node_local_id(2, 3) = 8
          face_to_node_local_id(2, 4) = 7
          ! face 3
          face_to_node_local_id(3, 1) = 5
          face_to_node_local_id(3, 2) = 6
          face_to_node_local_id(3, 3) = 7
          face_to_node_local_id(3, 4) = 8
          ! face 4
          face_to_node_local_id(4, 1) = 1
          face_to_node_local_id(4, 2) = 2
          face_to_node_local_id(4, 3) = 6 
          face_to_node_local_id(4, 4) = 5
          ! face 5
          face_to_node_local_id(5, 1) = 2
          face_to_node_local_id(5, 2) = 3
          face_to_node_local_id(5, 3) = 7
          face_to_node_local_id(5, 4) = 6
          ! face 6
          face_to_node_local_id(6, 1) = 1
          face_to_node_local_id(6, 2) = 5
          face_to_node_local_id(6, 3) = 8
          face_to_node_local_id(6, 4) = 4

          mat(1:nel,1:3,1:3) = zero
          rhs(1:nel,1:3,1:trimat) = zero
          sol(1:nel,1:3) = zero

          ! -------------
          ! initialize the coordinates of the element centers
          do i = 1,nel
            ii = i + nft
            xk(i) = alemuscl_buffer%elcenter(ii,1)
            yk(i) = alemuscl_buffer%elcenter(ii,2)
            zk(i) = alemuscl_buffer%elcenter(ii,3)
          enddo
          ! -------------

          ! -------------      
          do j=1,trimat
            do i = 1,nel
              ii = i + nft
              ! value of the target function in the element
              valk(i,j) = alemuscl_buffer%volume_fraction(ii,j)
            enddo
          enddo
          ! -------------      

          ! -------------
          ! initilize the matrix & the nodal coordinates of the neighbors
          do kk=1,facet_nb
            do i = 1,nel
              ii = i + nft
              iad2 = ale_connect%ee_connect%iad_connect(ii)
              lgth = ale_connect%ee_connect%iad_connect(ii+1)-iad2
              if(lgth>facet_nb) then
                print*," GROSSIERE ERROR !!!! ",lgth,facet_nb,ii
                stop
              endif
              if(kk <= lgth) then
                vois_id = ale_connect%ee_connect%connected(iad2 + kk - 1)
                if (vois_id > 0) then
                  xl(i,kk) = alemuscl_buffer%elcenter(vois_id,1) 
                  yl(i,kk) = alemuscl_buffer%elcenter(vois_id,2)  
                  zl(i,kk) = alemuscl_buffer%elcenter(vois_id,3) 
                else
                  xf = zero
                  yf = zero
                  zf = zero

                  a(1:3) = x(1:3, ixs(face_to_node_local_id(kk, 1) + 1, ii))
                  b(1:3) = x(1:3, ixs(face_to_node_local_id(kk, 2) + 1, ii))
                  c(1:3) = x(1:3, ixs(face_to_node_local_id(kk, 3) + 1, ii))

                  norm(1) = (b(2) - a(2)) * (c(3) - a(3)) - (b(3) - a(3)) * (c(2) - a(2))
                  norm(2) = (b(3) - a(3)) * (c(1) - a(1)) - (b(1) - a(1)) * (c(3) - a(3))
                  norm(3) = (b(1) - a(1)) * (c(2) - a(2)) - (b(2) - a(2)) * (c(1) - a(1))

                  surf1 = half * abs(sqrt(norm(1) * norm(1) + norm(2) * norm(2) + norm(3) * norm(3)))
                  xf = surf1 * third * (a(1) + b(1) + c(1))
                  yf = surf1 * third * (a(2) + b(2) + c(2))
                  zf = surf1 * third * (a(3) + b(3) + c(3))

                  a(1:3) = x(1:3, ixs(face_to_node_local_id(kk, 1) + 1, ii))
                  b(1:3) = x(1:3, ixs(face_to_node_local_id(kk, 3) + 1, ii))
                  c(1:3) = x(1:3, ixs(face_to_node_local_id(kk, 4) + 1, ii))

                  norm(1) = (b(2) - a(2)) * (c(3) - a(3)) - (b(3) - a(3)) * (c(2) - a(2))
                  norm(2) = (b(3) - a(3)) * (c(1) - a(1)) - (b(1) - a(1)) * (c(3) - a(3))
                  norm(3) = (b(1) - a(1)) * (c(2) - a(2)) - (b(2) - a(2)) * (c(1) - a(1))

                  surf2 = half * abs(sqrt(norm(1) * norm(1) + norm(2) * norm(2) + norm(3) * norm(3)))
                  xf = xf + surf2 * third * (a(1) + b(1) + c(1))
                  yf = yf + surf2 * third * (a(2) + b(2) + c(2))
                  zf = zf + surf2 * third * (a(3) + b(3) + c(3)) 
                  
                  surf = surf1 + surf2
                  xf = xf / surf
                  yf = yf / surf
                  zf = zf / surf

                  xl(i,kk) = two * xf - xk(i)
                  yl(i,kk) = two * yf - yk(i)
                  zl(i,kk) = two * zf - zk(i)
                endif

                ! incrementing mat
                mat(i,1,1) = mat(i,1, 1) + (xl(i,kk) - xk(i)) * (xl(i,kk) - xk(i))
                mat(i,1,2) = mat(i,1, 2) + (xl(i,kk) - xk(i)) * (yl(i,kk) - yk(i))
                mat(i,1,3) = mat(i,1, 3) + (xl(i,kk) - xk(i)) * (zl(i,kk) - zk(i))
                mat(i,2,1) = mat(i,2, 1) + (yl(i,kk) - yk(i)) * (xl(i,kk) - xk(i))
                mat(i,2,2) = mat(i,2, 2) + (yl(i,kk) - yk(i)) * (yl(i,kk) - yk(i))
                mat(i,2,3) = mat(i,2, 3) + (yl(i,kk) - yk(i)) * (zl(i,kk) - zk(i))
                mat(i,3,1) = mat(i,3, 1) + (zl(i,kk) - zk(i)) * (xl(i,kk) - xk(i))
                mat(i,3,2) = mat(i,3, 2) + (zl(i,kk) - zk(i)) * (yl(i,kk) - yk(i))
                mat(i,3,3) = mat(i,3, 3) + (zl(i,kk) - zk(i)) * (zl(i,kk) - zk(i))
              endif
            enddo
          enddo
          ! -------------      

          ! -------------
          ! loop on the sub-materials to build the rhs
          do imat=1,trimat
            ! -------------
            do kk=1,facet_nb
              do i = 1,nel
                ii = i + nft
                
                !!! ixs(2:9, ii) : node global id
                iad2 = ale_connect%ee_connect%iad_connect(ii)
                lgth = ale_connect%ee_connect%iad_connect(ii+1)-iad2     
                if(kk <= lgth) then                 
                  vois_id = ale_connect%ee_connect%connected(iad2 + kk - 1)
                  if (vois_id > 0) then
                    !!! value of the target function in the current neighbor
                    vall = alemuscl_buffer%volume_fraction(vois_id,imat)
                  elseif(vois_id == 0) then
                    vall = valk(i,imat)
                  else
                    !vois_id<0 : means ebcs), -vois_id is seg_id
                    vall = segvar%phase_alpha(imat,-vois_id)
                  endif
!         if(ii==31) then              
!              write(*,*) "Input rhs 0 ",imat,ii,valk(i,imat),vall
!              write(*,*) "Input rhs 1 ",imat,ii,xl(i,kk),xk(i)
!              write(*,*) "Input rhs 2 ",imat,ii,yl(i,kk),yk(i)
 !             write(*,*) "Input rhs 3 ",imat,ii,zl(i,kk),zk(i)
 !         endif                   
                  !!! incrementing rhs
                  rhs(i,1,imat) = rhs(i,1,imat) + (valk(i,imat) - vall) * (xl(i,kk) - xk(i))
                  rhs(i,2,imat) = rhs(i,2,imat) + (valk(i,imat) - vall) * (yl(i,kk) - yk(i))
                  rhs(i,3,imat) = rhs(i,3,imat) + (valk(i,imat) - vall) * (zl(i,kk) - zk(i))              
                endif
              enddo
            enddo
            ! -------------
          enddo  
          ! -------------   

          ! -------------
          ! loop on the sub-materials to solve the linear system
          tol = em10
          do imat=1,trimat
            do i=1,nel
              ii = i + nft            
!         if(ii==31) then              
!              write(*,*) "Input rhs ",imat,ii,rhs(i,1,imat),rhs(i,2,imat),rhs(i,3,imat)
!              write(*,*) "Input mat ",imat,ii,mat(i,1,1),mat(i,1,2),mat(i,1,3), &
!                mat(i,2,1),mat(i,2,2),mat(i,2,3),mat(i,3,1),mat(i,3,2),mat(i,3,3)
!          endif           
            enddo 
            call conjugate_gradient_vec(nel,3,3,mat,rhs(1,1,imat),sol,tol)        
            alemuscl_buffer%grad(nft+1:nft+nel,1:3,imat) = -sol(1:nel,1:3) ! save the solution
!            do i=1,nel
!              ii = i + nft
!         if(ii==31) then              
!              write(*,*) "Sol ",imat,ii,alemuscl_buffer%grad(ii,1,imat), &
!                alemuscl_buffer%grad(ii,2,imat),alemuscl_buffer%grad(ii,3,imat)
!          endif
            !enddo
          enddo
          ! -------------
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine gradient_reconstruction_new
      end module gradient_reconstruction_mod
