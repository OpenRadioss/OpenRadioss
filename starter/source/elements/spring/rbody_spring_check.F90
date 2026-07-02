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
!||    rbody_spring_check_mod   ../starter/source/elements/spring/rbody_spring_check.F90
!||--- called by ------------------------------------------------------
!||    initia                   ../starter/source/elements/initia/initia.F
!||====================================================================
      module rbody_spring_check_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
! ======================================================================================================================
!! \brief Check connection between springs and rigid bodies for massless springs
! ======================================================================================================================
!
!||====================================================================
!||    rbody_spring_check   ../starter/source/elements/spring/rbody_spring_check.F90
!||--- called by ------------------------------------------------------
!||    initia               ../starter/source/elements/initia/initia.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine rbody_spring_check(nrbykin, nnpby, npby, slpby, lpby, numnod,                &
                                      knod2el1d, snod2el1d, nod2el1d, numel, numelt, numelp,    &
                                      numelr, numels, numelc, ixr, nixr, nparg,                 &
                                      iparg, ngroup, elbuf_tab, dtelem, spring_con_rbody)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod,                         only : ep30
          use precision_mod,                        only : WP                        
          use elbufdef_mod,                         only : elbuf_struct_
          use MY_ALLOC_MOD,                         only : my_alloc, my_dealloc
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                       intent(in)   :: nrbykin                !< Number of rigid bodies kinematic
          integer,                       intent(in)   :: numnod                 !< Number of nodes
          integer,                       intent(in)   :: numel                  !< Total number of elements
          integer,                       intent(in)   :: numels                 !< Number of shell elements
          integer,                       intent(in)   :: numelc                 !< Number of solid elements
          integer,                       intent(in)   :: numelt                 !< Number of truss elements
          integer,                       intent(in)   :: numelp                 !< Number of beam elements
          integer,                       intent(in)   :: numelr                 !< Number of spring elements
          integer,                       intent(in)   :: nixr                   !< Dimension of spring connectivity array
          integer,                       intent(in)   :: nnpby                  !< Number of parameters per rigid body
          integer,                       intent(in)   :: slpby                  !< Size of lpby array
          integer,                       intent(in)   :: ngroup                 !< Number of groups
          integer,                       intent(in)   :: nparg                  !< Number of parameters per group
          integer,                       intent(in)   :: snod2el1d              !< Size of nod2el1d array
          integer,                       intent(in)   :: npby(nnpby,nrbykin)    !< main structure for rigid bodies
          integer,                       intent(in)   :: lpby(slpby)            !< Rigid body node list
          integer,                       intent(in)   :: knod2el1d(numnod+1)    !< Node to 1D element pointer array
          integer,                       intent(in)   :: nod2el1d(snod2el1d)    !< Node to 1D element connectivity
          integer,                       intent(in)   :: ixr(nixr,numelr)       !< Spring element connectivity
          integer,                       intent(in)   :: iparg(nparg,ngroup)    !< Group parameters
          integer,                       intent(inout):: spring_con_rbody       !< Spring connected to rigid body flag
          real(kind=WP),                 intent(inout):: dtelem(2*numel)        !< Element time step array
          type (elbuf_struct_),          intent(inout):: elbuf_tab(ngroup)      !< Element buffer arrayr

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: iel, rb, rbf, j, nn, ff
          integer :: elem_id,mastern
          integer :: zk
          integer :: nsl
          integer :: spring_offset
          integer :: ng, nel, ity, nft
          integer, dimension(:,:), allocatable :: spring_rbody
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          
!         Initialize spring-rbody connection array
          call my_alloc(spring_rbody, 2, numelr, "spring_rbody in rbody_spring_check")
          spring_rbody(1:2,1:numelr) = 0
          spring_offset = numels + numelc + numelt + numelp
!                 
!         Loop over all rigid bodies to fill spring-rbody connection
          zk = 0  
          do rb = 1, nrbykin
            nsl = npby(2,rb)
            rbf = rb            
!           Check for merged rigid bodies (secondary rbody - switch to main)
            if (npby(12, rb) /= 0) then
              rbf = npby(13,rb)
            end if            
!           Loop over all slave nodes in this rigid body
            do j = 1, nsl
              nn = lpby(j+zk)              
!             Loop over all 1D elements connected to this node
              do ff = knod2el1d(nn) + 1, knod2el1d(nn + 1)                
!               Check if element is a spring (ID > numelt + numelp)
                if (nod2el1d(ff) > numelt + numelp) then
                  elem_id = nod2el1d(ff) - numelt - numelp                  
!                 Identify which node of the spring is connected to rbody
                  if (ixr(2, elem_id) == nn) then
                    spring_rbody(1,elem_id) = rbf
                  else
                    spring_rbody(2,elem_id) = rbf
                  end if
                end if                
              end do              
            end do           
            zk = zk + nsl            
          end do

!         store rbody mass in elment buffer for massless springs
          do ng = 1, ngroup
            nel = iparg(2,ng)
            nft = iparg(3,ng)
            ity = iparg(5,ng)
            if (ity == 6) then
              if (elbuf_tab(ng)%gbuf%g_rbody_node > 0) then              
                do iel = 1,nel
                  if (spring_rbody(1,nft+iel) > 0) then
                    rb = spring_rbody(1,nft+iel)    
                    mastern = npby(1,rb)
                    elbuf_tab(ng)%gbuf%rbody_node(2*(iel-1)+1) = mastern
!                   deactivation of time step for starter estimation
                    dtelem(spring_offset+nft+iel) = ep30
                    spring_con_rbody = 1
                  end if
                  if (spring_rbody(2,nft+iel) > 0) then
                    rb = spring_rbody(2,nft+iel)    
                    mastern = npby(1,rb)
                    elbuf_tab(ng)%gbuf%rbody_node(2*(iel-1)+2) = mastern
!                   deactivation of time step for starter estimation    
                    dtelem(spring_offset+nft+iel) = ep30
                    spring_con_rbody = 1
                  end if
                end do  
              end if
            end if
          end do

          if (allocated(spring_rbody)) call my_dealloc(spring_rbody)! ----------------------------------------------------------------------------------------------------------------------
        end subroutine rbody_spring_check
      end module rbody_spring_check_mod
