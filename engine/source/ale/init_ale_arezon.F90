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
!||    init_ale_arezon_mod   ../engine/source/ale/init_ale_arezon.F90
!||--- called by ------------------------------------------------------
!||    init_ale              ../engine/source/ale/init_ale.F90
!||====================================================================
      module init_ale_arezon_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This subroutine initializes the rezoning variable indexing arrays and allocates the phi_data arrays
!!        used in the rezoning process depending on the material models and rezoning variables needed.
!! \details
!! \brief
!! \details
!||====================================================================
!||    init_ale_arezon   ../engine/source/ale/init_ale_arezon.F90
!||--- called by ------------------------------------------------------
!||    init_ale          ../engine/source/ale/init_ale.F90
!||--- calls      -----------------------------------------------------
!||    initbuf           ../engine/share/resol/initbuf.F
!||--- uses       -----------------------------------------------------
!||    ale_mod           ../common_source/modules/ale/ale_mod.F
!||    elbufdef_mod      ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    initbuf_mod       ../engine/share/resol/initbuf.F
!||    spmd_mod          ../engine/source/mpi/spmd_mod.F90
!||====================================================================
        subroutine init_ale_arezon(n2d,numels,numelq,numeltg,nsvois,nqvois,ntgvois,trimat,nmult,ngroup,nparg, &
                                        nspmd,iparg,elbuf_tab)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use elbufdef_mod
          use spmd_mod
          use initbuf_mod
          use ale_mod , only : ale
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
          integer, intent(in) :: n2d !< 0 : 3D, 1 : 2D
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
          integer, intent(in) :: nspmd !< number of mpi tatsks
          integer, dimension(nparg,ngroup), intent(in) :: iparg !< group element data
          type(elbuf_struct_), dimension(ngroup), intent(in) :: elbuf_tab !< element buffer structure       
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------        
          integer :: itrimat,mat_number,my_check,flag_mat_eos
          integer :: nm,ijk,nvar,idx_nb,ng,idx,j
          integer :: mtn,llt,nft,iad,ity,npt,jale,ismstr,jeul,jtur
          integer :: jthe,jlag,jmult,jhbe,jivf,nvaux,jpor,jcvt,jclose,jplasol
          integer :: irep,iint,igtp,israt,isrot,icsen,isorth,isorthg,ifailure,jsms
          integer :: nuvar_mat,nuvar_eos,ire,irs
          integer :: i,n_entity
          integer :: phi_size,phi_size_2,next
          integer, dimension(5), parameter :: nvar_list = (/2,10,11,11,12/)
          integer, dimension(5,trimat+1,1:max(1,nmult)) :: need_to_compute_l  ! local array to know if we need to compute the rezoning variable
          integer, dimension(5,trimat+1,1:max(1,nmult)) :: need_to_compute ! global array to know if we need to compute the rezoning variable
          integer, dimension(5,trimat+1) :: idx_list ! list of index for each rezoning variable
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
                                                            
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          if(n2d==0) then
            n_entity = numels ! define the number of element for 3D
          else
            n_entity = numelq ! define the number of element for 2D
          end if
          ! define the index list for each rezoning variable
          ! var 1 : rezoning sigma --> 6
          ! var 2 : rezoning plasticity or burning time --> 1
          ! var 3 : rezoning uvar ( i,  1:nuvar_mat + nuvar_eos) --> ale%rezon%num_nuvar_mat + ale%rezon%num_nuvar_eos
          ! var 4 : rezoning uvar ( ii, 1:nuvar_eos) --> ale%rezon%num_nuvar_eos
          ! var 5 : rezoning temperature --> 1
          idx_list(1:5,1:trimat+1) = 0
          idx_list(1,1) = 6
          idx_list(2,1) = 1
          idx_list(3,1) = ale%rezon%num_nuvar_mat
          idx_list(4,1) = ale%rezon%num_nuvar_eos          
          idx_list(5,1) = 1         
          do itrimat=1,trimat
            if(itrimat/=4) idx_list(1,itrimat+1) = 6
            idx_list(2,itrimat+1) = 1
            idx_list(3,itrimat+1) = 0
            idx_list(4,itrimat+1) = ale%rezon%num_nuvar_eos            
            idx_list(5,itrimat+1) = 1         
          end do
          
          mat_number = max(1,nmult)
          need_to_compute_l(1:5,1:trimat+1,1:mat_number) = 0
          need_to_compute(1:5,1:trimat+1,1:mat_number) = 0
          ! ------------
          ! loop over the material number and rezoning variables to know if we need to compute it         
          do nm=1,mat_number
            do itrimat=0,trimat
              do ijk=1,5
                my_check = 0             
                nvar = nvar_list(ijk)
                idx_nb = idx_list(ijk,itrimat+1)
                do idx=1,idx_nb
                  flag_mat_eos = 0
                  if(nvar==11) then
                    if(itrimat==0.and.idx<=ale%rezon%num_nuvar_mat) flag_mat_eos = 1
                    if(itrimat==0.and.idx>ale%rezon%num_nuvar_mat) flag_mat_eos = 2
                    if(itrimat>0) flag_mat_eos = 2
                  end if
                  do ng=1,ngroup
                    call initbuf(iparg,ng,mtn,llt,nft,iad,ity,npt,jale,ismstr,jeul,jtur,   &
                                jthe,jlag,jmult,jhbe,jivf,nvaux,jpor,jcvt,jclose,jplasol, &
                                irep,iint,igtp,israt,isrot,icsen,isorth,isorthg,ifailure,jsms)
                    nuvar_mat = iparg(81,ng)
                    nuvar_eos = iparg(82,ng)
                    if(itrimat>0.and.mtn/=51) cycle
                    if(jale+jeul==0) cycle
                    if(iparg(8,ng)==1) cycle
                    if(max(1,jmult)<nm) cycle
                    if(iparg(76,ng)==1) cycle ! --> off
                    ! pressurer rezoning for outlets (continuity)
                    if(jmult/=0) mtn=iparg(24+nm,ng)
                    if(nvar==12.and.elbuf_tab(ng)%gbuf%g_temp==0) cycle
                    if(nvar==11)then
                      if(flag_mat_eos==0.or.idx==0) cycle ! %var (mat and eos)
                      if(flag_mat_eos==1)then
                        if(idx>nuvar_mat) cycle ! rezon uvar( i,  1:nuvar_mat) only
                      elseif (flag_mat_eos==2)then
                      if(idx>nuvar_eos) cycle ! rezon uvar( i,  1:nuvar_mat) only
                      endif
                      if(mtn==51 .and. (itrimat==0.or.itrimat==4))cycle
                    endif
                    irs = iparg(15,ng) !rezoning sigma enabled
                    if(nvar==2) then
                      if(irs/=1) cycle
                      if(itrimat==4) cycle
                    endif

                    ire = iparg(16,ng) !rezoning plasticity or burning time
                    if(nvar==10) then
                      if(ire/=1) cycle
                      if(mtn==41) cycle
                      if(mtn==37) cycle
                      if(mtn==51.and.itrimat>0) then
                        ! ok
                      elseif(mtn==5.or.mtn==97.or.mtn==105.or.itrimat==4) then
                        ! ok
                      elseif(mtn==6) then
                        ! ok
                      elseif(mtn>=28.and.mtn/=67.and.mtn/=49.and.mtn/=51) then
                        ! ok
                      else
                        if(mtn==51 .and. itrimat==0)cycle ! nok
                        ! ok
                      endif
                    endif
                    
                    if(nvar==12) then
                      if(itrimat/=0) then
                        ! ok
                      else
                        if(mtn==51 .and. itrimat==0)cycle ! nok
                        ! ok
                      end if
                    endif
                    my_check = 1 ! if i'm here --> we need to compute this rezoning variable
                  end do ! loop over the group of element
                end do ! loop over idx
                need_to_compute_l(ijk,itrimat+1,nm) = my_check
              end do ! loop over nvar
            end do ! loop over itrimat
          end do ! loop over material number
          ! ------------

          ! ------------
          ! mpi reduction          
          if(nspmd>1) then
            call spmd_allreduce(need_to_compute_l(:,1,1),need_to_compute(:,1,1),5*(trimat+1)*mat_number,spmd_max)
          else
            need_to_compute(:,:,:) = need_to_compute_l(:,:,:)
          end if
          ! ------------          

          ! ------------
          ! allocation of some arrays          
          allocate(ale%rezon%nvar_idx_list(5,1,1:mat_number))          
          allocate(ale%rezon%nvar_idx_list_51(5,trimat,1:mat_number))
          allocate(ale%rezon%phi_data(5,1,1:mat_number))          
          allocate(ale%rezon%phi_51_data(5,trimat,1:mat_number))          
          ! ------------
          
          ! ------------
          ! computation of the index of phi array for each rezoning variable   
          next = 0
          do nm=1,mat_number
            do j=1,5
              if(need_to_compute(j,1,nm)==0) then
                ale%rezon%nvar_idx_list(j,1,nm) = 0                  
              else
                if(.not.allocated(ale%rezon%phi_data(j,1,nm)%address)) then
                  allocate(ale%rezon%phi_data(j,1,nm)%address(idx_list(j,1)))
                end if
                ale%rezon%nvar_idx_list(j,1,nm) = idx_list(j,1)
                do i=1,idx_list(j,1)
                  next = next + 1
                  ale%rezon%phi_data(j,1,nm)%address(i) = next
                enddo
              end if
            end do            
          end do    
          ale%rezon%rezone_size = next   
          ! ------------
          
          ! ------------
          ! computation of the index of phi array for each rezoning variable for law 51 materials  
          next = 0   
          do nm=1,mat_number
            do itrimat=1,trimat
              do j=1,5
                if(need_to_compute(j,itrimat+1,nm)==0) then
                  ale%rezon%nvar_idx_list_51(j,itrimat,nm) = 0                  
                else
                  if(.not.allocated(ale%rezon%phi_51_data(j,itrimat,nm)%address)) then
                    allocate(ale%rezon%phi_51_data(j,itrimat,nm)%address(idx_list(j,itrimat+1)))
                  end if                  
                  ale%rezon%nvar_idx_list_51(j,itrimat,nm) = idx_list(j,itrimat+1)
                  do i=1,idx_list(j,itrimat+1)
                    next = next + 1
                    ale%rezon%phi_51_data(j,itrimat,nm)%address(i) = next
                  enddo
                end if
              end do
            end do
          end do
          ale%rezon%rezone_51_size = next
          ! ------------

          ! ------------
          ! computation of the phi array dimensions
          phi_size = 0
          phi_size_2 = 0
          do nm=1,mat_number
            do j=1,5
              phi_size = phi_size + idx_list(j,1) * need_to_compute(j,1,nm) 
            enddo
            do itrimat=1,trimat
              do j=1,5
                phi_size_2 = phi_size_2 + idx_list(j,itrimat+1) * need_to_compute(j,itrimat+1,nm) 
              enddo
            enddo
          enddo
          if(n2d==0) then
            ale%rezon%phi_dim1 = numels + nsvois
          else
            ale%rezon%phi_dim1 = (numelq + nqvois) + (numeltg + ntgvois)
          end if

          ale%rezon%phi_dim2(1) = max(phi_size,phi_size_2)
          ale%rezon%phi_dim2(2) = phi_size
          ale%rezon%phi_dim2(3) = phi_size_2
          ! ------------

          return
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine init_ale_arezon
      end module init_ale_arezon_mod
