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
!||    aconve_mod   ../engine/source/ale/aconve.F90
!||--- called by ------------------------------------------------------
!||    alethe       ../engine/source/ale/alethe.F
!||====================================================================
      module aconve_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Convection of variables
!! \details This routine performs the ALE convection of all transported variables associated with elements
!||====================================================================
!||    aconve                     ../engine/source/ale/aconve.F90
!||--- called by ------------------------------------------------------
!||    alethe                     ../engine/source/ale/alethe.F
!||--- calls      -----------------------------------------------------
!||    a22conv3                   ../engine/source/ale/alefvm/cut_cells/a22conv3.F
!||    a4conv3                    ../engine/source/ale/ale3d/a4conv3.F
!||    aconv2                     ../engine/source/ale/ale2d/aconv2.F
!||    aconv3                     ../engine/source/ale/ale3d/aconv3.F
!||    bconv2                     ../engine/source/ale/ale2d/bconv2.F
!||    initbuf                    ../engine/share/resol/initbuf.F
!||    my_barrier                 ../engine/source/system/machine.F
!||    startime                   ../engine/source/system/timer_mod.F90
!||    stoptime                   ../engine/source/system/timer_mod.F90
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod       ../common_source/modules/ale/ale_connectivity_mod.F
!||    ale_mod                    ../common_source/modules/ale/ale_mod.F
!||    alefvm_mod                 ../common_source/modules/ale/alefvm_mod.F
!||    arezo2_mod                 ../engine/source/ale/ale2d/arezo2.F
!||    arezo3_mod                 ../engine/source/ale/ale3d/arezo3.F
!||    array_mod                  ../common_source/modules/array_mod.F
!||    constant_mod               ../common_source/modules/constant_mod.F
!||    debug_mod                  ../engine/share/modules/debug_mod.F
!||    elbufdef_mod               ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    element_mod                ../common_source/modules/elements/element_mod.F90
!||    i22bufbric_mod             ../common_source/modules/interfaces/cut-cell-search_mod.F
!||    initbuf_mod                ../engine/share/resol/initbuf.F
!||    multimat_param_mod         ../common_source/modules/multimat_param_mod.F90
!||    precision_mod              ../common_source/modules/precision_mod.F90
!||    segvar_mod                 ../engine/share/modules/segvar_mod.F
!||    spmd_exch_n_neighbor_mod   ../engine/source/mpi/ale/spmd_exch_n_neighbor.F90
!||    spmd_mod                   ../engine/source/mpi/spmd_mod.F90
!||    timer_mod                  ../engine/source/system/timer_mod.F90
!||====================================================================
        subroutine aconve(n2d,nv46,npropm,nummat,numels,numelq,numeltg, &
                          numnod, &
                          trimat,trimat0,nmult,mat_number, &      
                          ngroup,nparg,flag_mat_51,itask,nspmd, &
                          s_lesdvois,s_lercvois,s_flux,s_qmv,s_bhole, &
                          int22,i22len,nsvois,nqvois,ntgvois,nsegflu,phi_data_s, &
                          nesdvois,nercvois,lesdvois,lercvois, &
                          iparg,bhole,ixs,ixq, &
                          phi,flux,qmv,flu1,pm,x, &
                          elbuf_tab,ale_connect,phi_data,brick_list,segvar,timers)
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
          use i22bufbric_mod , only : brick_entity
          use segvar_mod
          use alefvm_mod , only : alefvm_param
          use element_mod , only : nixs, nixq
          use debug_mod
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
          integer, intent(in) :: nv46 !< number of element's facet (2d-->4, 3d-->6)
          integer, intent(in) :: npropm !< number of properties
          integer, intent(in) :: nummat !< number of materials     
          integer, intent(in) :: numels !< number of solid elements
          integer, intent(in) :: numelq !< number of quad elements
          integer, intent(in) :: numeltg !< number of triangle elements
          integer, intent(in) :: numnod !< number of nodes          
          integer, intent(in) :: trimat !< number of material of law51, 1 if not used
          integer, intent(in) :: trimat0 !< real number of material of law51, 0 if not used
          integer, intent(in) :: nmult !< number of material
          integer, intent(in) :: mat_number !< max(1,nmult)
          integer, intent(in) :: ngroup !< number of group of element
          integer, intent(in) :: nparg !< first dimension of iparg array
          integer, intent(in) :: flag_mat_51 !< flag for material law 51 activation
          integer, intent(in) :: itask !< omp task
          integer, intent(in) :: nspmd !< number of mpi tasks
          integer, intent(in) :: s_lesdvois !< size of send frontier element ids array
          integer, intent(in) :: s_lercvois !< size of rcv frontier element ids array
          integer, intent(in) :: s_flux !< size of flux array, 1rst dimension
          integer, intent(in) :: s_qmv !< size of qmv array, 1rst dimension
          integer, intent(in) :: s_bhole !< size of bhole array
          integer, intent(in) :: int22 !< number of /TYPE22 interfaces          
          integer, intent(in) :: i22len !< buffer size for intersected bricks
          integer, intent(in) :: nsvois !< number of frontier solid elements
          integer, intent(in) :: nqvois !< number of frontier quad elements
          integer, intent(in) :: ntgvois !< number of frontier triangle elements
          integer, intent(in) :: nsegflu !< number of segments of EBCS 
          integer, intent(in) :: phi_data_s !< size of phi_data structre --> trimat for mtn==51, else max(1,mat_number)
          integer, dimension(nspmd+1), intent(in) :: nesdvois !< number of frontier elements (send)          
          integer, dimension(nspmd+1), intent(in) :: nercvois !< number of frontier elements (rcv)
          integer, dimension(s_lesdvois), intent(in) :: lesdvois !< frontier element ids (send)
          integer, dimension(s_lercvois), intent(in) :: lercvois !< frontier element ids (rcv)
          integer, dimension(nparg,ngroup), intent(in) :: iparg !< group element data
          integer, dimension(max(1,nmult),s_bhole), intent(in) :: bhole !< hole flag array
          integer, dimension(nixs,numels), intent(in) :: ixs !< Solid element connectivity
          integer, dimension(nixq,numelq), intent(in) :: ixq !< Quad element connectivity
          real(kind=WP), dimension(s_flux,trimat), intent(inout) :: flu1 !< flux      
          real(kind=WP), dimension(ale%conv%phi_dim1,ale%conv%phi_dim2(1)), intent(inout) :: phi !< rezoning variable array
          real(kind=WP), dimension(s_flux,ale%global%nv46,trimat), intent(inout) :: flux !< rezoning flux array
          real(kind=WP), dimension(s_qmv,2*nv46,trimat), intent(inout) :: qmv !< momentum flux array
          real(kind=WP), dimension(npropm,nummat), intent(in) :: pm !< material & property array
          real(kind=WP), dimension(3,numnod), intent(inout) :: x !< coordinates array          
          type(elbuf_struct_), dimension(ngroup), intent(in) :: elbuf_tab !< element buffer structure
          type(t_ale_connectivity), intent(in) :: ale_connect !< ALE data structure for connectivity
          type(phi_data_), dimension(phi_data_s), intent(in) :: phi_data !< rezoning variable data
          type(brick_entity), dimension(int22,i22len), intent(inout) :: brick_list !< interface 22 data structure
          type(t_segvar), intent(in) :: segvar !< EBCS data structure
          type(timer_), intent(inout) :: timers !< timers
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------  
          integer :: itrimat,real_itrimat
          integer :: nm,ijk,nvar,idx_nb,ng,idx,j
          integer :: mtn,llt,nel,nft,iad,ity,npt,jale,ismstr,jeul,jtur
          integer :: jthe,jlag,jmult,jhbe,jivf,nvaux,jpor,jcvt,jclose,jplasol
          integer :: irep,iint,igtp,israt,isrot,icsen,isorth,isorthg,ifailure,jsms
          integer :: i,k,n_entity,offset
          integer :: add,add0,phi_add
          integer :: flag,s_proc_nb,r_proc_nb
          integer :: brick_add,ib,ibv,icellv,mcell,nin,num
          integer :: my_index,ndim,nf1
          integer :: imat,ioff,isilent,isolnod
          integer :: iflg ! flag tu update qmv array, 1 --> qmv update for nvar=1 & mtn=51, O --> nothing          
          integer, dimension(nspmd) :: s_index,r_index
          integer, dimension(nspmd) :: s_req,r_req          
          integer :: nvar_index,conv_size,local_index
          type(array_type_my_real_2d), dimension(:), allocatable :: s_buffer !< send buffer
          type(array_type_my_real_2d), dimension(:), allocatable :: r_buffer !< rcv buffer             
          real(kind=WP), dimension(:), pointer :: var,pvol,peint,prho,piad22
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
                                                            
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!$omp do schedule(guided)            
          do ng=1,ngroup ! loop over the group of element
            ! initialize some element group variables
            if(iparg(76,ng)==1) cycle ! --> off
            call initbuf(iparg,ng,mtn,llt,nft,iad,ity,npt,jale,ismstr,jeul,jtur,   &
                         jthe,jlag,jmult,jhbe,jivf,nvaux,jpor,jcvt,jclose,jplasol, &
                         irep,iint,igtp,israt,isrot,icsen,isorth,isorthg,ifailure,jsms)
            nel = llt
            if(jmult/=0) mtn = iparg(24+nm,ng)
            if(jale+jeul==0) cycle
            if(iparg(8,ng)==1) cycle
            isilent = iparg(64,ng)
            if(flag_mat_51==1.and.mtn/=51) cycle

            do nm=1,mat_number ! loop over the material number (1 if jmult=0)
              if(max(1,jmult)<nm) cycle
              do itrimat=1,trimat ! loop over the sub-material of law 51 (=1 if mtn/=51)
                if(flag_mat_51==0) then 
                  my_index = nm
                else
                  my_index = itrimat
                end if
                do ijk=1,phi_data(my_index)%nvar_list_nb

                  nvar = phi_data(my_index)%nvar_list(ijk) ! get the convective variable index
                  phi_add = phi_data(my_index)%address(ijk) ! get the address in phi array                
                  phi(1+nft:llt+nft,phi_add) = zero ! initialize phi values

                  ! -----------
                  if(nvar==1) then
                    if(flag_mat_51==0) then
                      prho => elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%rho(1:llt)
                      pvol => elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%vol(1:llt)
                    else                     
                      add0 = m51_n0phas + (itrimat-1)*m51_nvphas
                      add = add0 + 9 ! add+9 => rho
                      k = llt*(add-1) ! uvar(i,add) = u
                      prho => elbuf_tab(ng)%bufly(nm)%mat(1,1,1)%var(k+1:k+llt)

                      add = add0 + 11 ! add+11 => vol
                      k = llt*(add-1) ! uvar(i,add) = u
                      pvol => elbuf_tab(ng)%bufly(nm)%mat(1,1,1)%var(k+1:k+llt)                      
                    end if
                    do i=1,llt
                      j = i + nft
                      phi(j,phi_add) = prho(i)
                    end do
                    do i=1,llt
                      prho(i) = prho(i) * pvol(i)
                    end do
                  ! -----------
                  elseif(nvar==2) then
                    if(flag_mat_51==0) then
                      peint => elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%eint(1:llt)
                      pvol => elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%vol(1:llt)
                    else
                      add0 = m51_n0phas + (itrimat-1)*m51_nvphas
                      add = add0 + 8 ! add+8 => eint
                      k = llt*(add-1) ! uvar(i,add) = u
                      peint => elbuf_tab(ng)%bufly(nm)%mat(1,1,1)%var(k+1:k+llt)

                      add = add0 + 11 ! add+11 => vol
                      k = llt*(add-1) ! uvar(i,add) = u
                      pvol => elbuf_tab(ng)%bufly(nm)%mat(1,1,1)%var(k+1:k+llt)                                          
                    endif
                    do i=1,llt
                      j = i + nft
                      phi(j,phi_add) = peint(i)
                    end do
                    do i=1,llt
                      peint(i) = peint(i) * pvol(i)
                    end do
                  ! -----------
                  elseif(nvar==3) then
                    do i=1,llt
                      j = i + nft
                      phi(j,phi_add) = elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%rk(i)
                    end do
                    do i=1,llt
                      j = i + nft
                      elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%rk(i) = elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%rk(i) * &
                                                                  elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%vol(i)
                    enddo
                  ! -----------
                  elseif(nvar==4) then
                    do i=1,llt
                      j = i + nft
                      phi(j,phi_add) = elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%re(i)
                    end do
                    do i=1,llt
                      j = i + nft
                      elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%re(i) = elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%re(i) * &
                                                                  elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%vol(i)
                    enddo
                  ! -----------
                  elseif(nvar==5) then
                    var => elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var(1:llt)
                    if(mtn==41) then
                      prho => elbuf_tab(ng)%gbuf%rho(1:llt)
                      do i=1,llt
                        j = i + nft
                        phi(j,phi_add) = var(i) * prho(i)
                      end do
                      do i=1,llt
                        var(i) = var(i) * prho(i) * elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%vol(i)
                      end do
                    else
                      do i=1,llt
                        j = i + nft
                        phi(j,phi_add) = var(i)
                      end do
                      do i=1,llt
                        var(i) = var(i) * elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%vol(i)
                      end do
                    end if
                  ! -----------
                  elseif (nvar==6) then
                    idx = 1
                    ndim = 3
                    var => elbuf_tab(ng)%gbuf%mom(1:llt*ndim)
                    do i=1,llt
                      j = i + nft
                      k = llt*(idx-1) + i
                      phi(j,phi_add) = var(k)
                      var(k) = var(k) * elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%vol(i)
                    end do
                  ! -----------
                  elseif (nvar==7) then
                    idx = 2
                    ndim = 3
                    var => elbuf_tab(ng)%gbuf%mom(1:llt*ndim)
                    do i=1,llt
                      j = i + nft
                      k = llt*(idx-1) + i
                      phi(j,phi_add) = var(k)
                      var(k) = var(k) * elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%vol(i)
                    end do
                  ! -----------
                  elseif(nvar==8) then
                    idx = 3
                    ndim = 3
                    var => elbuf_tab(ng)%gbuf%mom(1:llt*ndim)
                    do i=1,llt
                      j = i + nft
                      k = llt*(idx-1) + i
                      phi(j,phi_add) = var(k)
                      var(k) = var(k) * elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%vol(i)
                    end do
                  ! -----------
                  elseif(nvar==9.and.isilent==1) then
                  ! nothing to do
                  elseif(nvar==10.and.isilent==1) then
                  ! nothing to do
                  else
                    ! -----------
                    if(n2d==0) then
                      imat = ixs(1,1+nft)                    
                    else
                      imat = ixq(1,1+nft)
                    end if

                    do i=1,llt
                      j = i + nft
                      phi(j,phi_add) = pm(180+nvar,imat)*elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%rho(i)
                    end do
                  end if

                  if(int22>0) then
                    ! Fill Secnd cells linked to current main cell with main cell value.
                    nin = 1
                    pIAD22 => elbuf_tab(ng)%gbuf%tag22(1:)

                    if(flag_mat_51==0) then 
                      brick_add = ale%global%lconv*(nm-1)+nvar
                    else
                      brick_add = ale%global%lconv*(itrimat-1)+nvar
                    endif
                    do i=1,llt
                      j = i + nft
                      ib = nint(pIAD22(i))
                      if(ib==0) cycle
                      num = brick_list(nin,ib)%secndlist%num
                      mcell = brick_list(nin,ib)%mainid
                      if(.not.allocated(brick_list(nin,ib)%poly(mcell)%phi_)) then
                        allocate(brick_list(nin,ib)%poly(mcell)%phi_(ale%global%lconv*max(1,nmult,trimat0)))
                      end if
                      if(flag_mat_51==0) then 
                        brick_add = ale%global%lconv*(nm-1)+nvar
                      else
                        brick_add = ale%global%lconv*(itrimat-1)+nvar
                      endif
                      brick_list(nin,ib)%poly(mcell)%phi_(brick_add) = phi(j,phi_add)
                      do k=1,num
                        ibv = brick_list(nin,ib)%secndlist%ibv(k)
                        icellv = brick_list(nin,ib)%secndlist%icellv(k)
                        if(.not.allocated(brick_list(nin,ibv)%poly(icellv)%phi_)) then
                          allocate(brick_list(nin,ibv)%poly(icellv)%phi_(ale%global%lconv*max(1,nmult,trimat0)))
                        end if
                        brick_list(nin,ibv)%poly(icellv)%phi_(brick_add) = phi(j,phi_add)
                      end do
                    end do ! next I                 
                  end if

                end do ! loop over the convective variable
              end do ! loop over the sub-material of law 51
            end do ! loop over the material number   
          enddo ! loop over group of element  
!$omp end do
          ioff = 0
          if(nsegflu>0) then
            call my_barrier()
            ioff = numels + numelq + numeltg
            if(nspmd>1) ioff = ioff + nsvois + nqvois + ntgvois
            if(flag_mat_51==0) then
              do nm=1,mat_number ! loop over the material number (1 if jmult=0)
                do ijk=1,phi_data(nm)%nvar_list_nb
                  nvar = phi_data(nm)%nvar_list(ijk) ! get the convective variable index
                  phi_add = phi_data(nm)%address(ijk) ! get the address in phi array   
                  select case(nvar)
                    !RHO
                    case(1)
                      do j=1,nsegflu
                        phi(ioff+j,phi_add) = segvar%rho(j)
                      end do
                    !EINT
                    case(2)                
                      do j=1,nsegflu
                        phi(ioff+j,phi_add) = segvar%eint(j)
                      end do
                    !RK    
                    case(3)
                      do j=1,nsegflu
                        phi(ioff+j,phi_add) = segvar%rk(j)
                      end do
                    !RE
                    case(4)
                      do j=1,nsegflu
                        phi(ioff+j,phi_add) = segvar%re(j)
                      end do
                    !UVAR                  
                    case(5)
                      do j=1,nsegflu
                        phi(ioff+j,phi_add) = segvar%uvar(j)
                      end do 
                  end select
                end do ! loop over the convective variable
              end do ! loop over the material number          
            else
              do itrimat=1,trimat ! loop over the material number of law 51
                do ijk=1,phi_data(itrimat)%nvar_list_nb
                  nvar = phi_data(itrimat)%nvar_list(ijk) ! get the convective variable index
                  phi_add = phi_data(itrimat)%address(ijk) ! get the address in phi array   
                  select case(nvar)
                    !RHO
                    case(1)
                      do j=1,nsegflu
                        phi(ioff+j,phi_add) = segvar%phase_rho(itrimat,j)
                      end do
                    !EINT
                    case(2)
                      do j=1,nsegflu
                        phi(ioff+j,phi_add) = segvar%phase_eint(itrimat,j)
                      end do
                  end select
                end do ! loop over the convective variable
              end do ! loop over the material number                  
            end if
          end if

          ! ------------
          ! MPI comms : iSend & iRecv phi values for frontier elements + Wait
          if(nspmd>1) then
            call my_barrier()
            if(itask==0) call startime(timers,timer_spmdcfd)
            if(itask==0) then
              flag = 0
              s_proc_nb = 0
              r_proc_nb = 0
              n_entity = ale%conv%phi_dim1
              if(flag_mat_51==0) then
                conv_size = ale%conv%phi_dim2(2)
              else
                conv_size = ale%conv%phi_dim2(3)
              endif             
              allocate(s_buffer(nspmd))
              allocate(r_buffer(nspmd))
              ! iSend & iRecv
              call spmd_exch_n_neighbor(flag,nspmd,n_entity,conv_size,s_lesdvois,s_lercvois, &
                                        s_proc_nb,r_proc_nb,s_index,r_index,s_req,r_req, &
                                        nesdvois,nercvois,lesdvois,lercvois, &
                                        phi,s_buffer,r_buffer)
              ! Wait
              flag = 1
              call spmd_exch_n_neighbor(flag,nspmd,n_entity,conv_size,s_lesdvois,s_lercvois, &
                                        s_proc_nb,r_proc_nb,s_index,r_index,s_req,r_req, &
                                        nesdvois,nercvois,lesdvois,lercvois, &
                                        phi,s_buffer,r_buffer)
              deallocate(s_buffer)
              deallocate(r_buffer)       
            endif
            call my_barrier()
            if(itask==0) call stoptime(timers,timer_spmdcfd)
          else
            call my_barrier()
          endif
          ! ------------    

          ! ------------
          if(int22>0) then
            do nm=1,mat_number ! loop over the material number (1 if jmult=0)
              nf1 = nft+1+(nm-1)*numels            
              do itrimat=1,trimat
                if(flag_mat_51==0) then 
                  my_index = nm
                  real_itrimat = 0
                else
                  my_index = itrimat
                  real_itrimat = itrimat
                end if
                do ijk=1,phi_data(my_index)%nvar_list_nb
                  nvar = phi_data(my_index)%nvar_list(ijk) ! get the convective variable index
                  phi_add = phi_data(my_index)%address(ijk) ! get the address in phi array    

                  iflg = 0
                  if(flag_mat_51==0) then 
                    brick_add = ale%global%lconv*(nm-1)+nvar
                  else
                    brick_add = ale%global%lconv*(itrimat-1)+nvar
                  endif 
                  if(nvar==1.and.flag_mat_51==1) iflg = 1        
                  call a22conv3( phi(1,phi_add),iflg,real_itrimat,nvar,itask, &
                                  elbuf_tab,ixs,iparg,brick_add)
                enddo
              enddo
            enddo
            call my_barrier()
          end if
          ! ------------

!$omp do schedule(guided)            
          do ng=1,ngroup ! loop over the group of element
            ! initialize some element group variables
            if(iparg(76,ng)==1) cycle ! --> off
            call initbuf(iparg,ng,mtn,llt,nft,iad,ity,npt,jale,ismstr,jeul,jtur,   &
                         jthe,jlag,jmult,jhbe,jivf,nvaux,jpor,jcvt,jclose,jplasol, &
                         irep,iint,igtp,israt,isrot,icsen,isorth,isorthg,ifailure,jsms)
            nel = llt
            isolnod = iparg(28,ng)
            isilent = iparg(64,ng)            
            if(jale+jeul==0) cycle
            if(iparg(8,ng)==1) cycle
            if(isilent==1) cycle
            if(flag_mat_51==1.and.mtn/=51) cycle

            do nm=1,mat_number ! loop over the material number (1 if jmult=0)
              if(max(1,jmult)<nm) cycle
              do itrimat=1,trimat ! loop over the sub-material of law 51 (=1 if mtn/=51)
                if(flag_mat_51==0) then 
                  my_index = nm
                else
                  my_index = itrimat
                end if
                do ijk=1,phi_data(my_index)%nvar_list_nb

                  nvar = phi_data(my_index)%nvar_list(ijk) ! get the convective variable index
                  phi_add = phi_data(my_index)%address(ijk) ! get the address in phi array                
                  ! -----------
                  if(nvar==1) then
                    if(flag_mat_51==0) then
                      prho => elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%rho(1:llt)
                    else
                      add0 = m51_n0phas + (itrimat-1)*m51_nvphas
                      add = add0 + 9 ! add+9 => rho
                      k = llt*(add-1) ! uvar(i,add) = u
                      prho => elbuf_tab(ng)%bufly(nm)%mat(1,1,1)%var(k+1:k+llt)
                   
                    end if
                    var => prho                  
                  ! -----------
                  elseif(nvar==2) then
                    if(flag_mat_51==0) then
                      peint => elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%eint(1:llt)
                    else
                      add0 = m51_n0phas + (itrimat-1)*m51_nvphas
                      add = add0 + 8 ! add+8 => eint
                      k = llt*(add-1) ! uvar(i,add) = u
                      peint => elbuf_tab(ng)%bufly(nm)%mat(1,1,1)%var(k+1:k+llt)                                        
                    endif
                    var => peint
                  ! -----------
                  elseif(nvar==3) then
                    var => elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%rk(1:llt) 
                  ! -----------
                  elseif(nvar==4) then
                    var => elbuf_tab(ng)%bufly(nm)%lbuf(1,1,1)%re(1:llt)
                  ! -----------
                  elseif(nvar==5) then
                    var => elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var(1:llt)
                  ! -----------
                  elseif(nvar==6) then
                    if(alefvm_param%ienabled==0) then
                      if (mtn==51.and.itrimat/=0) then                                        
                        var => elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var(5*llt+1:6*llt)            
                      else
                        var => elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var(llt+1:2*llt)
                      endif
                    else
                      var => elbuf_tab(ng)%gbuf%mom(1:llt)                                                                                   
                    endif   
                  ! ----------- 
                  elseif(nvar==7) then
                    if(alefvm_param%ienabled==0) then                                        
                      var => elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var(2*llt+1:3*llt)            
                    else
                      var => elbuf_tab(ng)%gbuf%mom(llt+1:2*llt)                                                                                  
                    endif
                  ! -----------
                  elseif(nvar==8) then
                    if(alefvm_param%ienabled==0) then                                        
                      var => elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var(3*llt+1:4*llt)            
                    else
                      var => elbuf_tab(ng)%gbuf%mom(2*llt+1:3*llt)                                                                                  
                    endif
                  ! -----------   
                  elseif(nvar==9.and.isilent==1) then
                  ! nothing to do
                  elseif(nvar==10.and.isilent==1) then
                  ! nothing to do
                  elseif(nvar==9.and.isilent==0) then
                    var => elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var(4*llt+1:5*llt) 
                  ! -----------
                  else
                    var => null()
                  end if

                  piad22 => elbuf_tab(ng)%gbuf%tag22(1:llt)
                  ! -----------
                  iflg = 0
                  if(nvar==1.and.flag_mat_51==1) iflg = 1
                  if(n2d==0) then
                    offset = (nm-1)*numels
                    nf1 = nft + 1 + (nm-1)*numels
                    if(isolnod/=4) then
                      call aconv3( var,phi(1,phi_add),flux(1,1,itrimat),flu1(nf1,itrimat),ixs,  &
                                    ale_connect,ioff,qmv(1,1,itrimat),iflg,   &
                                    piad22,nvar,itask,s_flux,s_qmv,offset,nv46, &
                                    llt,nft)
                    else
                      call a4conv3( var,phi(1,phi_add),flux(1,1,itrimat),flu1(nf1,itrimat),  &
                                    ale_connect,ioff,s_flux,offset,nv46,llt,nft)
                    endif
                  ! -----------
                  else
                    nf1 = nft + 1 + (nm-1)*numelq
                    offset = (nm-1)*numelq
                    if(nmult==0) then
                      call aconv2( var,phi(1,phi_add),flux(1,1,itrimat),flu1(nf1,itrimat),ale_connect, &
                                   qmv(1,1,itrimat),iflg,ixq,x,ioff,s_flux,s_qmv, &
                                   offset,nv46,llt,nft)
                    else
                      call bconv2( var,phi(1,phi_add),flux(1,1,itrimat),flu1(nf1,itrimat),ale_connect, &
                                   bhole,nm,s_flux,offset,nv46,llt,nft)
                    end if
                  end if
                  ! -----------
                end do ! loop over the convective variable
              end do ! loop over the sub-material of law 51
            end do ! loop over the material number   
          enddo ! loop over group of element  
!$omp end do          

          return
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine aconve
      end module aconve_mod
