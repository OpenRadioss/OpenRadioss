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
      module arezon_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This subroutine fills the rezoning variable array "phi" for all the rezoned variables and for all the materials
!!        from the element buffer structure "elbuf_tab" before calling the rezoning solver.
!! \details
        subroutine arezon(n2d,numelq, &
                          trimat,nmult,mat_number, &      
                          ngroup,nparg,flag_mat_51,itask,nspmd, &
                          s_lesdvois,s_lercvois,s_flux,s_bhole,nesdvois,nercvois,lesdvois,lercvois,iparg, &
                          nvar_idx_list,bhole,phi,flux,elbuf_tab,ale_connect,phi_data,timers)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use elbufdef_mod
          use spmd_mod
          use initbuf_mod
          use ale_mod , only : ale,phi_data_
          use ale_connectivity_mod , only : t_ale_connectivity
          use timer_mod
          use array_mod
          use precision_mod , only : WP
          use multimat_param_mod , only : m51_n0phas,m51_nvphas
          use spmd_exch_n_neighbor_mod , only : spmd_exch_n_neighbor
          use arezo2_mod
          use arezo3_mod
          use constant_mod , only : zero
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
          integer, intent(in) :: numelq !< number of quad elements                  
          integer, intent(in) :: trimat !< number of material of law51, 1 if not used
          integer, intent(in) :: nmult !< number of material
          integer, intent(in) :: mat_number !< max(1,nmult)
          integer, intent(in) :: ngroup !< number of group of element
          integer, intent(in) :: nparg !< first dimension of iparg array
          integer, intent(in) :: flag_mat_51 !< flag for material law 51 activation
          integer, intent(in) :: itask !< omp task
          integer, intent(in) :: nspmd !< number of mpi tasks
          integer, intent(in) :: s_lesdvois !< size of send frontier element ids array
          integer, intent(in) :: s_lercvois !< size of rcv frontier element ids array
          integer, intent(in) :: s_flux !< size of flux array 1rst dimension
          integer, intent(in) :: s_bhole !< size of bhole array
          integer, dimension(nspmd+1), intent(in) :: nesdvois !< number of frontier elements (send)          
          integer, dimension(nspmd+1), intent(in) :: nercvois !< number of frontier elements (rcv)
          integer, dimension(s_lesdvois), intent(in) :: lesdvois !< frontier element ids (send)
          integer, dimension(s_lercvois), intent(in) :: lercvois !< frontier element ids (rcv)
          integer, dimension(nparg,ngroup), intent(in) :: iparg !< group element data
          integer, dimension(5,trimat,mat_number), intent(in) :: nvar_idx_list !< list of index for each rezoning variable and each material (0 if no rezoning)
          integer, dimension(max(1,nmult),s_bhole), intent(in) :: bhole !< hole flag array
          real(kind=WP), dimension(ale%rezon%phi_dim1,ale%rezon%phi_dim2(1)), intent(inout) :: phi !< rezoning variable array
          real(kind=WP), dimension(s_flux,ale%global%nv46,trimat), intent(inout) :: flux !< rezoning flux array
          type(elbuf_struct_), dimension(ngroup), intent(in) :: elbuf_tab !< element buffer structure
          type(t_ale_connectivity), intent(in) :: ale_connect !< ALE data structure for connectivity
          type(phi_data_), dimension(5,trimat,mat_number), intent(in) :: phi_data !< rezoning variable data
          type(timer_), intent(inout) :: timers !< timers
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------  
          integer :: itrimat,flag_mat_eos
          integer :: nm,ijk,nvar,idx_nb,ng,idx,j
          integer :: mtn,llt,nel,nft,iad,ity,npt,jale,ismstr,jeul,jtur
          integer :: jthe,jlag,jmult,jhbe,jivf,nvaux,jpor,jcvt,jclose,jplasol
          integer :: irep,iint,igtp,israt,isrot,icsen,isorth,isorthg,ifailure,jsms
          integer :: nuvar_mat,nuvar_eos,ire,irs
          integer :: i,k,n_entity,offset
          integer :: add,add0,phi_add
          integer :: flag,s_proc_nb,r_proc_nb
          integer, dimension(nspmd) :: s_index,r_index
          integer, dimension(nspmd) :: s_req,r_req          
          integer :: nvar_index,rezone_size,local_index
          type(array_type_my_real_2d), dimension(:), allocatable :: s_elcenter !< send buffer
          type(array_type_my_real_2d), dimension(:), allocatable :: r_elcenter !< rcv buffer   
          
          real(kind=WP), dimension(:), pointer :: var,vol
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
                                                            
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          ! loop over the material number (1 if jmult=0)
          do nm=1,mat_number
!$omp do schedule(guided)            
            do ng=1,ngroup ! loop over the group of element
              ! initialize some element group variables
              call initbuf(iparg,ng,mtn,llt,nft,iad,ity,npt,jale,ismstr,jeul,jtur,   &
                           jthe,jlag,jmult,jhbe,jivf,nvaux,jpor,jcvt,jclose,jplasol, &
                           irep,iint,igtp,israt,isrot,icsen,isorth,isorthg,ifailure,jsms)
              nel = llt
              if(jale+jeul==0) cycle
              if(iparg(8,ng)==1) cycle              
              if(max(1,jmult)<nm) cycle
              if(iparg(76,ng)==1) cycle ! --> off
              ! pressurer rezoning for outlets (continuity)
              if(jmult/=0) mtn=iparg(24+nm,ng)                                           
              nuvar_mat = iparg(81,ng)
              nuvar_eos = iparg(82,ng)
              irs = iparg(15,ng) !rezoning sigma enabled
              ire = iparg(16,ng) !rezoning plasticity or burning time

              do ijk=1,trimat ! loop over the sub-materials (1 if the law is not mtn=51, trimat otherwise)
                do nvar_index=1,5 ! loop over the rezoned variables
                  nvar = ale%rezon%nvar_list(nvar_index) ! get the rezoned variable
                  idx_nb = nvar_idx_list(nvar_index,ijk,nm) ! get the number of index for the rezoning variable (0 if not needed)
                  itrimat = 0
                  local_index = 1
                  if(flag_mat_51/=0) then
                    local_index = ijk
                    itrimat = ijk ! initialize itrimat for mtn=51
                  endif
                  if(nvar==12.and.elbuf_tab(ng)%gbuf%g_temp==0) cycle
                  if(itrimat>0.and.mtn/=51) cycle                                    
                  do idx=1,idx_nb ! loop over the index for the rezoning variable
                    phi_add = phi_data(nvar_index,local_index,nm)%address(idx) ! get the address in phi array
                    do i=1,llt !other material laws or mtn=51
                      j = i + nft                    
                      phi(j,phi_add) = zero
                    end do                  
                    ! ------------
                    ! sigma rezoning
                    if(nvar==2) then                 
                      ! skip conditions
                      if(irs/=1) cycle
                      if(itrimat==4) cycle
                      if(itrimat>0.and.mtn==51) then
                        add = m51_n0phas + (itrimat-1)*m51_nvphas + idx ! add+1 => sig(1)
                        add = add *llt
                        do i=1,llt
                          j = i + nft
                          phi(j,phi_add) = elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var(add+i)
                        end do
                      else
                        do i=1,llt !other material laws or mtn=51
                          j = i + nft
                          k = (idx-1)*nel + i !idx : 1-->6                          
                          phi(j,phi_add) = elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%sig(k)
                        end do
                      end if
                    !endif
                    ! ------------


                    ! ------------
                    elseif(nvar==10) then
                      ! skip conditions
                      if(ire/=1) cycle
                      if(mtn==41) cycle
                      if(mtn==37) cycle
                      if(mtn==51.and.itrimat>0) then
                        if(itrimat<=3) then
                          add0 = m51_n0phas + (itrimat-1)*m51_nvphas
                          add = add0 + 15  ! PLAS
                          k = llt*(add-1)
                          phi(nft+1:nft+llt,phi_add) = elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var(k+1:k+llt)
                        else
                          phi(nft+1:nft+llt,phi_add) = elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%tb(1:llt) ! detonation time
                        endif
                      elseif(mtn==5.or.mtn==97.or.mtn==105.or.(mtn==51.and.itrimat==4)) then ! detonation time
                        phi(nft+1:nft+llt,phi_add) = elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%tb(1:llt)
                      elseif(mtn==6) then
                        phi(nft+1:nft+llt,phi_add) = elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%rk(1:llt)
                      elseif(mtn>=28.and.mtn/=67.and.mtn/=49.and.mtn/=51) then
                        phi(nft+1:nft+llt,phi_add) = elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%pla(1:llt)
                      else
                        if(mtn==51 .and. itrimat==0)cycle ! nok
                        phi(nft+1:nft+llt,phi_add) = elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%pla(1:llt)
                      endif
                    !endif
                    ! ------------

                    ! ------------
                    ! var mat and eos rezoning
                    elseif(nvar==11)then
                      flag_mat_eos = 0
                      if(nvar_index==3) then
                        flag_mat_eos=1 ! %var mat
                      elseif(nvar_index==4) then
                        flag_mat_eos=2 ! %var eos
                      endif
                      ! skip conditions
                      if(flag_mat_eos==0.or.idx==0) cycle ! %var (mat and eos)
                      if(mtn==51 .and. (itrimat==0.or.itrimat==4))cycle
                      
                      if(flag_mat_eos==1)then
                        if(idx>nuvar_mat) cycle ! rezon uvar(i,1:nuvar_mat) only
                        phi(nft+1:nft+llt,phi_add) = elbuf_tab(ng)%bufly(nm)%mat(1,1,1)%var(llt*(idx-1)+1:llt*(idx))
                      elseif (flag_mat_eos==2)then
                        if(idx>nuvar_eos) cycle ! rezon uvar(i,1:nuvar_mat) only
                        if(itrimat==0) then
                          phi(nft+1:nft+llt,phi_add) = elbuf_tab(ng)%bufly(nm)%eos(1,1,1)%var(llt*(idx-1)+1:llt*(idx))
                        elseif(itrimat/=4) then
                          add0 = m51_n0phas + (itrimat-1)*m51_nvphas
                          add = add0 + 23 + idx ! EOSVAR
                          k   = llt*(add-1)
                          phi(nft+1:nft+llt,phi_add) = elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var(k+1:k+llt)
                        endif
                      endif
                    !endif
                    ! ------------                    

                    ! ------------ 
                    elseif(nvar==12) then
                      if(mtn==51) then
                        if(itrimat>0) then 
                          add0 = m51_n0phas + (itrimat-1)*m51_nvphas
                          add = add0 + 16  ! TEMP
                          k   = llt*(add-1)
                          phi(nft+1:nft+llt,phi_add) = elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var(k+1:k+llt)
                        else
                          cycle ! nok
                        endif
                      else
                        phi(nft+1:nft+llt,phi_add) = elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%temp(1:llt)
                      endif
                    !endif
                    ! ------------
                    else
                      !print*," erreur new_arezon nvar invalide ",nvar,ng,mtn,itrimat,irs,trimat
                      !stop
                    end if
                  end do ! loop over the index for the rezoning variable
                end do ! loop over the rezoned variables
              end do ! loop over the sub-materials
            end do ! loop over group of element
!$omp end do
          end do ! loop over material number

          ! ------------
          ! MPI comms : iSend & iRecv phi values for frontier elements + Wait
          if(nspmd>1) then
            call my_barrier()
            if(itask==0) call startime(timers,timer_spmdcfd)
            if(itask==0) then
              flag = 0
              s_proc_nb = 0
              r_proc_nb = 0
              n_entity = ale%rezon%phi_dim1
              if(flag_mat_51==0) then
                rezone_size = ale%rezon%rezone_size
              else
                rezone_size = ale%rezon%rezone_51_size
              endif             
              allocate(s_elcenter(nspmd))
              allocate(r_elcenter(nspmd))
              ! iSend & iRecv
              call spmd_exch_n_neighbor(flag,nspmd,n_entity,rezone_size,s_lesdvois,s_lercvois, &
                                        s_proc_nb,r_proc_nb,s_index,r_index,s_req,r_req, &
                                        nesdvois,nercvois,lesdvois,lercvois, &
                                        phi,s_elcenter,r_elcenter)
              ! Wait
              flag = 1
              call spmd_exch_n_neighbor(flag,nspmd,n_entity,rezone_size,s_lesdvois,s_lercvois, &
                                        s_proc_nb,r_proc_nb,s_index,r_index,s_req,r_req, &
                                        nesdvois,nercvois,lesdvois,lercvois, &
                                        phi,s_elcenter,r_elcenter)
              deallocate(s_elcenter)
              deallocate(r_elcenter)       
            endif
            call my_barrier()
            if(itask==0) call stoptime(timers,timer_spmdcfd)
          else
            call my_barrier()
          endif
          ! ------------    
          
          
          ! loop over the material number (1 if jmult=0)
          do nm=1,mat_number
!$omp do schedule(guided)            
            do ng=1,ngroup ! loop over the group of element
              ! initialize some element group variables
              call initbuf(iparg,ng,mtn,llt,nft,iad,ity,npt,jale,ismstr,jeul,jtur,   &
                           jthe,jlag,jmult,jhbe,jivf,nvaux,jpor,jcvt,jclose,jplasol, &
                           irep,iint,igtp,israt,isrot,icsen,isorth,isorthg,ifailure,jsms)
              if(jale+jeul==0) cycle
              if(iparg(8,ng)==1) cycle
              if(max(1,jmult)<nm) cycle
              if(iparg(76,ng)==1) cycle ! --> off
              ! pressurer rezoning for outlets (continuity)
              if(jmult/=0) mtn=iparg(24+nm,ng)                                           
              nuvar_mat = iparg(81,ng)
              nuvar_eos = iparg(82,ng)
              irs = iparg(15,ng) !rezoning sigma enabled
              ire = iparg(16,ng) !rezoning plasticity or burning time

              do ijk=1,trimat ! loop over the sub-materials (1 if the law is not mtn=51, trimat otherwise)
                do nvar_index=1,5 ! loop over the rezoned variables
                  nvar = ale%rezon%nvar_list(nvar_index) ! get the rezoned variable
                  idx_nb = nvar_idx_list(nvar_index,ijk,nm) ! get the number of index for the rezoning variable (0 if not needed)
                  itrimat = 0
                  local_index = 1
                  if(flag_mat_51/=0) then
                    local_index = ijk
                    itrimat = ijk ! initialize itrimat for mtn=51
                  endif
                  if(itrimat>0.and.mtn/=51) cycle                  
                  if(nvar==12.and.elbuf_tab(ng)%gbuf%g_temp==0) cycle
                  do idx=1,idx_nb ! loop over the index for the rezoning variable
                    phi_add = phi_data(nvar_index,local_index,nm)%address(idx) ! get the address in phi array

                    ! nullify the pointers
                    var => null()
                    vol => null()
                   
                    ! ------------
                    ! sigma rezoning
                    if(nvar==2) then
                      ! skip conditions
                      if(irs/=1) cycle
                      if(itrimat==4) cycle
                      if(itrimat>0.and.mtn==51) then
                        add = m51_n0phas + (itrimat-1)*m51_nvphas + idx ! add+1 => sig(1)
                        add = add *llt
                        var => elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var(add+1:add+llt)
                        add = m51_n0phas + (itrimat-1)*m51_nvphas + 10 ! add+1 => vol(1)
                        add = add *llt
                        vol => elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var(add+1:add+llt)
                      else
                        var => elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%sig(llt*(idx-1)+1:llt*idx)
                        vol => elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%vol(1:llt)
                      end if
                    endif              
                    ! ------------


                    ! ------------
                    if(nvar==10) then
                      ! skip conditions
                      if(ire/=1) cycle
                      if(mtn==41) cycle
                      if(mtn==37) cycle
                      vol => elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%vol(1:llt)
                      if(mtn==51.and.itrimat>0) then
                        if(itrimat<=3) then
                          add0 = m51_n0phas + (itrimat-1)*m51_nvphas
                          add = add0 + 15  ! PLAS
                          k = llt*(add-1)
                          var => elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var(k+1:k+llt)
                          add0 = m51_n0phas + (itrimat-1)*m51_nvphas
                          add = add0 + 11
                          k = llt*(add-1)
                          vol => elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var(k+1:k+llt)           
                        else
                          var => elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%tb(1:llt) ! detonation time
                        endif
                      elseif(mtn==5.or.mtn==97.or.mtn==105.or.(mtn==51.and.itrimat==4)) then ! detonation time
                        var => elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%tb(1:llt)
                      elseif(mtn==6) then
                        var => elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%rk(1:llt)
                      elseif(mtn>=28.and.mtn/=67.and.mtn/=49.and.mtn/=51) then
                        var => elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%pla(1:llt)
                      else
                        if(mtn==51 .and. itrimat==0)cycle ! nok
                        var => elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%pla(1:llt)
                      endif       
                    endif
                    ! ------------

                    ! ------------
                    ! var mat and eos rezoning
                    if(nvar==11)then
                      flag_mat_eos = 0
                      if(nvar_index==3) then
                        flag_mat_eos=1 ! %var mat
                      elseif(nvar_index==4) then
                        flag_mat_eos=2 ! %var eos
                      endif
                      ! skip conditions
                      if(flag_mat_eos==0.or.idx==0) cycle ! %var (mat and eos)
                      if(flag_mat_eos==1)then
                        if(idx>nuvar_mat) cycle ! rezon uvar(i,1:nuvar_mat) only
                      elseif (flag_mat_eos==2)then
                        if(idx>nuvar_eos) cycle ! rezon uvar(i,1:nuvar_mat) only
                      endif                      
                      if(mtn==51 .and. (itrimat==0.or.itrimat==4))cycle
                      
                      if(itrimat/=0) then
                        add0 = m51_n0phas + (itrimat-1)*m51_nvphas
                        add = add0 + 11 ! volume
                        k = llt*(add-1)
                        vol => elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var(k+1:k+llt)
                        add0 = m51_n0phas + (itrimat-1)*m51_nvphas
                        add = add0 + 24 + idx ! temperature
                        k = llt*(add-1)
                        var => elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var(k+1:k+llt)
                      else
                        vol => elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%vol(1:llt)
                        if(flag_mat_eos==1)then
                          var => elbuf_tab(ng)%bufly(nm)%mat(1,1,1)%var(llt*(idx-1)+1:llt*(idx))
                        elseif (flag_mat_eos==2)then
                          var => elbuf_tab(ng)%bufly(nm)%eos(1,1,1)%var(llt*(idx-1)+1:llt*(idx))
                        endif
                      endif
                    endif
                    ! ------------            

                    ! ------------ 
                    if(nvar==12) then
                      if(mtn==51) then
                        if(itrimat>0) then 
                          add0 = m51_n0phas + (itrimat-1)*m51_nvphas
                          add = add0 + 16  ! temperature
                          k   = llt*(add-1)
                          var => elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var(k+1:k+llt)
                          add = add0 + 11  ! volume
                          k   = llt*(add-1)
                          vol => elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var(k+1:k+llt)
                        else
                          cycle ! nok                                                   
                        endif
                      else
                        var => elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%temp(1:llt)
                        vol => elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%vol(1:llt) 
                      endif
                    endif      
                    ! ------------
                    if(n2d==0) then
                      call arezo3(ale_connect,var,phi(:,phi_add),flux(:,:,max(itrimat,1)),vol, &
                                  elbuf_tab(ng)%gbuf%tag22(1:),s_flux,nft,llt)
                    else
                      offset = (nm-1)*numelq
                      if(nmult==0) then
                        call arezo2(ale_connect,var,phi(:,phi_add),flux(:,:,max(itrimat,1)),vol, &
                                    s_flux,offset,nft,llt,jmult)
                      else
                        call brezo2(ale_connect,var,phi(:,phi_add),flux(:,:,max(itrimat,1)),vol, &
                                    bhole,nm,s_flux,offset,nft,llt,jmult,nmult)
                      end if
                    end if                                   
                  end do ! loop over the index for the rezoning variable
                end do ! loop over the rezoned variables
              end do ! loop over the sub-materials
            end do ! loop over group of element
!$omp end do            
          end do ! loop over material number          

          return
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine arezon
      end module arezon_mod
