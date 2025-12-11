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
!||    init_ale_boundary_condition_mod   ../engine/source/ale/init_ale_boundary_condition.F90
!||--- called by ------------------------------------------------------
!||    init_ale                          ../engine/source/ale/init_ale.F90
!||====================================================================
      module init_ale_aconve_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Initialize ALE boundary condition data structure
!! \details This routine allocates and initializes the convection-related data structures used by the ALE solver.
!||====================================================================
!||    init_ale_boundary_condition   ../engine/source/ale/init_ale_boundary_condition.F90
!||--- called by ------------------------------------------------------
!||    init_ale                      ../engine/source/ale/init_ale.F90
!||--- calls      -----------------------------------------------------
!||    initbuf                       ../engine/share/resol/initbuf.F
!||--- uses       -----------------------------------------------------
!||    initbuf_mod                   ../engine/share/resol/initbuf.F
!||    mvsiz_mod                     ../engine/share/spe_inc/mvsiz_mod.F90
!||====================================================================
        subroutine init_ale_aconve(numels,numelq,numeltg,nsvois,nqvois,ntgvois, &
                                   trimat,nmult,ngroup,nparg,nsegflu,nspmd,iparg)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use ale_mod , only : ale
          use initbuf_mod
          use spmd_mod , only : spmd_max,spmd_allreduce
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
          integer, intent(in) :: numelq !< number of quad elements
          integer, intent(in) :: numeltg !< number of triangle elements 
          integer, intent(in) :: nsvois !< number of frontier solid elements
          integer, intent(in) :: nqvois !< number of frontier quad elements
          integer, intent(in) :: ntgvois !< number of frontier triangle elements                   
          integer, intent(in) :: trimat !< number of material (law51)
          integer, intent(in) :: nmult !< number of material
          integer, intent(in) :: ngroup !< number of group of element
          integer, intent(in) :: nparg !< first dimension of iparg array
          integer, intent(in) :: nsegflu !< number of segments of EBCS
          integer, intent(in) :: nspmd !< number of mpi processors
          integer, dimension(nparg,ngroup), intent(in) :: iparg !< group element data                     
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: mat_number,next,next_51,next_1,itrimat,my_check
          integer :: ng,nm,nvar
          integer :: isilent,codtot
          integer :: mtn,llt,nft,iad,ity,npt,jale,ismstr,jeul,jtur
          integer :: jthe,jlag,jmult,jhbe,jivf,nvaux,jpor,jcvt,jclose,jplasol
          integer :: irep,iint,igtp,israt,isrot,icsen,isorth,isorthg,ifailure,jsms
          integer, dimension(ale%global%lconv,1:max(1,nmult)+trimat) :: need_to_compute_l  ! local array to know if we need to compute the rezoning variable
          integer, dimension(ale%global%lconv,1:max(1,nmult)+trimat) :: need_to_compute ! global array to know if we need to compute the rezoning variable
          integer, dimension(ale%global%lconv) :: jcodv

! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! [ external functions must be kept to minimum ]
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          ! --------------
          mat_number = max(1,nmult)     
          need_to_compute_l(1:ale%global%lconv,1:mat_number+trimat) = 0
          need_to_compute(1:ale%global%lconv,1:mat_number+trimat) = 0
          do ng=1,ngroup
            if(iparg(76,ng)==1) cycle ! --> off
            call initbuf(iparg,ng,mtn,llt,nft,iad,ity,npt,jale,ismstr,jeul,jtur,   &
                         jthe,jlag,jmult,jhbe,jivf,nvaux,jpor,jcvt,jclose,jplasol, &
                         irep,iint,igtp,israt,isrot,icsen,isorth,isorthg,ifailure,jsms)
            if(jale+jeul==0) cycle
            if(iparg(8,ng)==1) cycle
            isilent = iparg(64,ng)
            call varcondec(jcodv,iparg(34,ng),codtot)
            do nvar=ale%global%lconv,1,-1
              if(jcodv(nvar)/=0) then
                if(isilent==1) cycle
                do nm=1,mat_number
                  if(max(1,jmult)<nm) cycle
                  need_to_compute_l(nvar,nm) = 1
                end do
              endif
            enddo
            do nvar=1,2
              if(jcodv(nvar)/=0) then
                if(isilent==1) cycle
                do itrimat=1,trimat
                  need_to_compute_l(nvar,mat_number+itrimat) = 1
                end do
              endif
            enddo
          end do
          ! --------------

          ! --------------
          if(nspmd>1) then
            call spmd_allreduce(need_to_compute_l(:,1),need_to_compute(:,1),ale%global%lconv*(mat_number+trimat),spmd_max)
          else
            need_to_compute(:,:) = need_to_compute_l(:,:)
          end if
          my_check = 0          
          do nvar = 1,ale%global%lconv
            do nm=1,mat_number
              if(need_to_compute(nvar,nm)==1) then
                my_check = 1
              end if
            end do
          end do
          do nvar = 1,2
            do itrimat=1,trimat
              if(need_to_compute(nvar,mat_number+itrimat)==1) then
                my_check = 1
              end if
            end do
          end do          
          ! --------------

          ! --------------
          next = 0
          next_51 = 0
          allocate(ale%conv%phi_data(mat_number))
          allocate(ale%conv%phi_51_data(trimat))

          ! --------------
          do nm=1,mat_number
            ale%conv%phi_data(nm)%nvar_list_nb = 0
          enddo
          do itrimat=1,trimat
            ale%conv%phi_51_data(itrimat)%nvar_list_nb = 0
          end do
          ! --------------

          if(my_check==1) then
            do nm=1,mat_number
              allocate(ale%conv%phi_data(nm)%address(ale%global%lconv))
              ale%conv%phi_data(nm)%address(1:ale%global%lconv) = 0
              allocate(ale%conv%phi_data(nm)%nvar_list(ale%global%lconv))
              ale%conv%phi_data(nm)%nvar_list(1:ale%global%lconv) = 0
              next_1 = 0
              do nvar=ale%global%lconv,1,-1
                if(need_to_compute(nvar,nm)==1) then
                  next = next + 1
                  next_1 = next_1 + 1
                  ale%conv%phi_data(nm)%nvar_list(next_1) = nvar
                  ale%conv%phi_data(nm)%address(next_1) = next
                endif
              enddo
              ale%conv%phi_data(nm)%nvar_list_nb = next_1
            end do

            if(trimat>0) then
              do itrimat=1,trimat
                allocate(ale%conv%phi_51_data(itrimat)%address(2))
                ale%conv%phi_51_data(itrimat)%address(1:2) = 0
                allocate(ale%conv%phi_51_data(itrimat)%nvar_list(2))
                ale%conv%phi_51_data(itrimat)%nvar_list(1:2) = 0
                next_1 = 0
                do nvar=1,2
                  if(need_to_compute(nvar,mat_number+itrimat)==1) then
                    next_51 = next_51 + 1
                    next_1 = next_1 + 1
                    ale%conv%phi_51_data(itrimat)%nvar_list(next_1) = nvar
                    ale%conv%phi_51_data(itrimat)%address(next_1) = next_51
                  end if
                end do
                ale%conv%phi_51_data(itrimat)%nvar_list_nb = next_1
              end do
            end if 
          endif
          ! --------------

          ! --------------
          ale%conv%phi_dim1 = (numels + nsvois) + (numelq + nqvois) + (numeltg + ntgvois) + nsegflu
          ale%conv%phi_dim2(1) = max(next,next_51)
          ale%conv%phi_dim2(2) = next
          ale%conv%phi_dim2(3) = next_51
          ! --------------
        
          return
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine init_ale_aconve
      end module init_ale_aconve_mod
