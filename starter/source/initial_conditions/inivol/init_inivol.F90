!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Here is a small description of the routine, [after the header]
!! \details if needed, more details can be added here
     subroutine init_inivol(    NUM_INIVOL,   inivol,   nsurf, igrsurf, &
                                nparg     ,   ngroup,   iparg,  numnod, npart,&
                                numels    ,     nixs,     ixs,&
                                numeltg   ,    nixtg,    ixtg,&
                                numelq    ,     nixq,     ixq,&
                                x         , nbsubmat,    kvol,&
                                elbuf_tab ,  numels8,   xrefs,&
                                n2d       ,multi_fvm,  sipart, ipart  ,&
                                i15a      ,     i15b,    i15h, sbufmat,  bufmat,&
                                npropmi   ,   nummat,     ipm, sbufsf ,   bufsf,&
                                npropg    ,   numgeo,     geo, mvsiz)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use constant_mod , only : zero, ep9
      use array_mod , only : array_type, alloc_1d_array, dealloc_1d_array, dealloc_3d_array
      use inivol_def_mod , only : inivol_struct_
      use groupdef_mod , only : surf_
      use elbufdef_mod , only : elbuf_struct_, buf_mat_
      use multi_fvm_mod , only : MULTI_FVM_STRUCT
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
      integer,intent(in) :: nsurf, NUM_INIVOL, n2d, sbufmat, npropg, numgeo, mvsiz                !< array sizes
      integer, intent(in) :: nixs,nixtg,nixq,numels,numeltg,numelq, numnod, nparg, ngroup, npart  !< array sizes
      integer,intent(in) :: numels8, nbsubmat, npropmi, nummat, sipart, sbufsf                    !< array sizes
      integer, intent(in) :: ixs(nixs,numels), ixtg(nixtg,numeltg), ixq(nixq,numelq)              !< elems node-connectivity
      integer, intent(in) :: iparg(nparg,ngroup)                                                  !< buffer for elem groups
      integer,intent(in) :: i15a,i15b,i15h                                                        !< indexes for ipart array
      integer,intent(in) :: ipart(sipart)                                                         !< buffer for parts
      integer,intent(in) :: ipm(npropmi,nummat)                                                   !< material data (integers)
      my_real, intent(in) :: x(3,numnod)                                                          !< coordinates
      my_real :: xrefs(8,3,numels8)                                                               !<
      my_real, intent(inout) :: kvol(nbsubmat,*)                                                  !< inivol working array
      my_real,intent(in) :: bufsf(sbufsf)                                                         !< buffer
      my_real,intent(in) :: bufmat(sbufmat)                                                       !< material buffer
      my_real,intent(in) ::  geo(npropg,numgeo)                                                   !< propery buffer (real parameters)
      type (elbuf_struct_), target, dimension(ngroup), intent(in) :: elbuf_tab                    !< elem buffer
      type (multi_fvm_struct),intent(in) :: multi_fvm                                             !< buffer for colocated scheme (law151)
      type (inivol_struct_), dimension(NUM_INIVOL), intent(inout) :: inivol                          !< inivol data structure
      type (surf_), dimension(nsurf), intent(in) :: igrsurf                                       !< surface buffer
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer, dimension(:), allocatable :: tagn,iphase,itagnsol,inod2surf,part_fill,ivolsurf,itagsurf,segtosurf,swiftsurf
      integer :: leading_dimension ! dimension of largest size
      integer :: ale_element_number ! number of ale element with material 51 or 151
      integer, parameter :: nb_box_limit=128 ! maximum number of cell
      integer :: nb_cell_x,nb_cell_y,nb_cell_z ! number of cell in x/y/z direction
      integer, dimension(:,:), allocatable :: cell_position ! position of node/cell
      integer :: ale_node_number ! number of ale node
      integer, dimension(:), allocatable :: list_ale_node ! list of ale node
      integer, dimension(:,:), allocatable :: nsoltosf,nbip,inphase
      integer, dimension(:), allocatable :: knod2surf
      integer ntrace0,ntrace,nnod2surf,jad0,nsegsurf
      integer stat, ng, nf1, nel, mtn
      integer :: ifrac,idp,idc,idsurf,ireversed,jmid,jadconty
      integer ::nb_container,nseg_swift_surf,nseg_used,nsurf_invol,imat,NUMEL_TOT,ICUMU,I15_,nuvar,nft
      integer :: ity,isolnod,invol,ii,iad,isize_inivol,sipart_
      my_real, dimension(3) :: size_cell ! cell's size in x/y/z direction
      my_real, dimension(:,:), allocatable :: dis
      my_real :: element_size ! max element size
      my_real, dimension(6) :: min_max_position ! min/max position
      my_real, dimension(:), allocatable :: nod_norm
      my_real :: vfrac
      type(buf_mat_) ,pointer :: mbuf
      type(array_type), dimension(:), allocatable :: cell ! voxcell
      type(array_type), dimension(:), allocatable :: nodal_phase ! phase of node
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      if (NUM_INIVOL > 0) then
        ntrace0 = 3
        ntrace0 = 2*ntrace0+1
        ntrace  = ntrace0**3
        allocate( cell_position(3,numnod) )
        allocate( list_ale_node(numnod) )

        do ii=1,NUM_INIVOL
          ifrac = inivol(ii)%id
          nb_container = inivol(ii)%num_container
          idp = inivol(ii)%part_id
          numel_tot = max(numeltg,max(numels,numelq))
          isize_inivol = (nbsubmat+1)*numel_tot
          inivol(ii)%size = isize_inivol
          allocate(iphase(inivol(ii)%size),stat=stat) ; iphase(:) = 0
          allocate(nbip(nbsubmat,numel_tot),stat=stat) ; nbip(:,:) = 0
          allocate(itagnsol(numnod), stat=stat) ; itagnsol(:) = 0
          allocate (knod2surf(numnod+1),stat=stat) ; knod2surf(:) = 0
          allocate(part_fill(npart),stat=stat) ; part_fill(:) = 0
          allocate(ivolsurf(nsurf),stat=stat); ;ivolsurf(:) = 0
          allocate(itagsurf(nsurf),stat=stat) ; itagsurf(:) = 0
          allocate(swiftsurf(nsurf),stat=stat) ; swiftsurf(:) = 0
          !  fill background ale mesh with phase
          do ng=1,ngroup
            mtn     = iparg(1,ng)
            nel     = iparg(2,ng)
            nft     = iparg(3,ng)
            iad     = iparg(4,ng)
            ity     = iparg(5,ng)
            isolnod = iparg(28,ng)
            invol   = iparg(53,ng)
            if(ity == 1)then
              if (isolnod /= 4 .and. isolnod /= 8) cycle
              imat = ixs(1,1+nft)
              i15_=i15a
            elseif(n2d > 0)then
              if(ity == 7)then
                imat = ixtg(1,1+nft)
                i15_=i15h
              elseif(ity == 2)then
                imat = ixq(1,1+nft)
                i15_=i15b
              else
                cycle
              endif
            endif
            if (mtn /= 51 .and. mtn /= 151) cycle
                sipart_ = sipart - i15_ + 1
                call inifill(numels    ,numeltg,numelq      ,numnod     ,npart   , &
                             npropmi   ,nummat ,n2d         ,ngroup     , &
                             ixs       ,ipm    ,ipart(i15_) ,iphase     ,sipart_ , &
                             idp       ,kvol   ,bufmat      ,itagnsol   ,isolnod , &
                             nbip      ,ntrace ,part_fill   ,nbsubmat   ,mtn     , &
                             elbuf_tab ,ng     ,multi_fvm   ,ixq        ,ixtg    , &
                             ity       ,imat   ,isize_inivol,numel_tot  ,nel     , &
                             nft       ,sbufmat)
          enddo ! next element group ng

          ! surface containers :
          nnod2surf = 0
          knod2surf(1:numnod+1) = 0
          nseg_used = 0
          do idc=1,nb_container
            idsurf   = inivol(ii)%container(idc)%surf_id
            nsegsurf = igrsurf(idsurf)%nseg
            if (igrsurf(idsurf)%type /= 101 .and. igrsurf(idsurf)%type /= 200 .and. ivolsurf(idsurf) == 0) then
              ivolsurf(idsurf) = 1
              call connesurf(nsegsurf,igrsurf(idsurf)%nodes,knod2surf,nnod2surf)
              nseg_used = nseg_used + nsegsurf
            endif
          enddo ! do idc=1,nb_container
          knod2surf(1:numnod+1) = 0
!---
          nsurf_invol = 0
          ivolsurf(1:nsurf) = 0
          do idc=1,nb_container
            idsurf = inivol(ii)%container(idc)%surf_id
            if (ivolsurf(idsurf) == 0) then
               nsurf_invol = nsurf_invol + 1
               ivolsurf(idsurf) = nsurf_invol
            endif
          enddo
!---
          allocate(nsoltosf(nb_container,numnod),stat=stat) ; nsoltosf(:,:) = 0
          allocate(inod2surf(nnod2surf*numnod),stat=stat) ; inod2surf(:) = 0
          allocate(dis(nsurf_invol,numnod),stat=stat) ; dis(:,:) = zero
          allocate(nod_norm(3*numnod),stat=stat) ; nod_norm(1:3*numnod) = zero
          allocate(segtosurf(nseg_used),stat=stat) ; segtosurf(1:nseg_used) = 0
!---
          ! -----------------
          ! compute the min / max position of ale elements
          ale_node_number = 0
          call ale_element_size_computation( &
                     nparg, ngroup, numels,numeltg,numelq,numnod,n2d,&
                     iparg,ixs,ixq,ixtg,&
                     element_size,min_max_position,x,&
                     ale_element_number,ale_node_number,list_ale_node)
          ! -----------------
          ! compute the min/max position of surface elements
          call surface_min_max_computation( &
                                   nsurf,numnod, &
                                   nb_container,min_max_position,x,igrsurf,inivol(ii))
          ! -----------------
          ! creation of the grid
          call ale_box_creation(nb_box_limit,nb_cell_x,nb_cell_y,nb_cell_z,      &
                                ale_element_number,element_size,min_max_position,&
                                leading_dimension,size_cell)
          ! -----------------
          ! coloration of cell with nodes of surface
          allocate( cell(nb_container) )

          call ale_box_coloration(nsurf,numnod,&
                                  nb_cell_x,nb_cell_y,nb_cell_z,nb_container,               &
                                  min_max_position,cell,x,igrsurf,inivol(ii),cell_position, &
                                  ale_node_number,list_ale_node)
          ! -----------------

          allocate( nodal_phase(nb_container) )
          nodal_phase(1:nb_container)%size_int_array_1d = numnod
          itagsurf(1:nsurf) = 0
          nseg_swift_surf = 0
          do idc=1,nb_container
            call alloc_1d_array(nodal_phase(idc))
            nodal_phase(idc)%int_array_1d(1:numnod) = 0
            idsurf    = inivol(ii)%container(idc)%surf_id
            jmid      = inivol(ii)%container(idc)%submat_id
            ireversed = inivol(ii)%container(idc)%ireversed
            vfrac     = inivol(ii)%container(idc)%vfrac
            vfrac     = vfrac/ep9
            icumu     = inivol(ii)%container(idc)%icumu
            nsegsurf  = igrsurf(idsurf)%nseg
            allocate(tagn(numnod)        ,stat=stat)
            tagn(1:numnod) = 0
            if (itagsurf(idsurf) == 0) then
              itagsurf(idsurf) = 1  !distances,node to surf, are now already calculated with idsurf
              call getphase(  &
              mvsiz                   , numels             ,numeltg                      ,numelq         ,numnod, &
              nparg                   ,ngroup              ,nsurf                        ,n2d            ,&
              x                       ,igrsurf(idsurf)%type,itagnsol                     ,dis            ,nsoltosf             ,&
              igrsurf(idsurf)%eltyp   ,knod2surf           ,nnod2surf                    ,inod2surf      ,tagn                 ,&
              idsurf                  ,nsegsurf            ,bufsf                        ,nod_norm       ,igrsurf(idsurf)%nodes,&
              igrsurf(idsurf)%iad_bufr,idc                 ,nb_container                 ,nseg_swift_surf,swiftsurf            ,&
              segtosurf               ,ivolsurf            ,nsurf_invol                  ,nseg_used      ,&
              leading_dimension       ,nb_cell_x           ,nb_cell_y                    ,nb_cell_z      ,&
              iparg                   ,ixs                 ,ixq                          ,ixtg           ,&
              cell(idc)%int_array_3d  ,cell_position       ,nodal_phase(idc)%int_array_1d,nb_box_limit)
            endif
            deallocate(tagn)
            call dealloc_1d_array(nodal_phase(idc))
            call dealloc_3d_array(cell(idc))
          enddo ! do idc=1,nb_container
          deallocate( nodal_phase )
          deallocate( cell )
!-------------
          do ng=1,ngroup
            mtn     = iparg(1,ng)
            nel     = iparg(2,ng)
            nft     = iparg(3,ng)
            ity     = iparg(5,ng)
            isolnod = iparg(28,ng)
            invol   = iparg(53,ng)
            if (invol <=  0) cycle
            if (mtn /= 51 .and. mtn /= 151) cycle
            if(n2d ==0 .and. ity /= 1)then
              cycle
            elseif(n2d > 0 .and. ity /= 7 .and. ity /= 2)then
              cycle
            endif
            ! loop over containers
            allocate(inphase(ntrace,nel) ,stat=stat)
            inphase(1:ntrace,1:nel) = 1
            numel_tot= max(numeltg,max(numels,numelq))
            do idc=1,nb_container
              idsurf     = inivol(ii)%container(idc)%surf_id
              jmid      = inivol(ii)%container(idc)%submat_id
              ireversed = inivol(ii)%container(idc)%ireversed
              vfrac     = inivol(ii)%container(idc)%vfrac
              vfrac     = vfrac/ep9
              icumu     = inivol(ii)%container(idc)%icumu
              nsegsurf  = igrsurf(idsurf)%nseg
              if(ity == 1)then
                i15_=i15a
              elseif(n2d > 0)then
                if(ity == 7)then
                  i15_=i15h
                elseif(ity == 2)then
                  i15_=i15b
                else
                  i15_=0
               endif
              endif
              call inisoldist( &
                ireversed               ,ixs         ,x          ,geo                  ,ng                   ,&
                iparg                   ,idp         ,ipart(i15_),xrefs                ,                      &
                ntrace                  ,ntrace0     ,dis        ,nsoltosf             ,nbip                 ,&
                nnod2surf               ,inod2surf   ,knod2surf  ,igrsurf(idsurf)%eltyp,igrsurf(idsurf)%nodes,&
                jmid                    ,iphase      ,inphase    ,kvol                 ,igrsurf(idsurf)%type ,&
                igrsurf(idsurf)%iad_bufr,bufsf       ,nod_norm   ,isolnod              ,nbsubmat             ,&
                vfrac                   ,icumu       ,idc        ,nb_container         ,nsegsurf             ,&
                idsurf                  ,swiftsurf   ,segtosurf  ,igrsurf              ,ivolsurf             ,&
                nsurf_invol             ,ixq         ,ixtg       ,ity                  ,nel                  ,numel_tot)
                mbuf  => elbuf_tab(ng)%bufly(1)%mat(1,1,1)
                nuvar =  elbuf_tab(ng)%bufly(1)%nvar_mat
                nf1   =  nft+1
              if (idc == nb_container)then
                call inivol_set( &
                                 mbuf%var  , nuvar, nel        , kvol(1,nf1) , mtn , &
                                 elbuf_tab , ng   , nbsubmat   , multi_fvm   , &
                                 ixs       , idp  , ipart(i15_), nft  )
              end if
            enddo ! do idc=1,nb_container
            deallocate(inphase)
          enddo ! next ng=1,ngroup
!---
          if(allocated(iphase))   deallocate(iphase)
          if(allocated(nbip))     deallocate(nbip)
          if(allocated(itagnsol)) deallocate(itagnsol)
          if(allocated(knod2surf))deallocate(knod2surf)
          if(allocated(part_fill))deallocate(part_fill)
          if(allocated(ivolsurf)) deallocate(ivolsurf)
          if(allocated(swiftsurf))deallocate(swiftsurf)
          if(allocated(nsoltosf)) deallocate(nsoltosf)
          if(allocated(inod2surf))deallocate(inod2surf)
          if(allocated(dis))      deallocate(dis)
          if(allocated(nod_norm)) deallocate(nod_norm)
          if(allocated(segtosurf))deallocate(segtosurf)
!---
        enddo ! next ii=1,NUM_INIVOL
        deallocate( cell_position )
        deallocate( list_ale_node )
      else
        allocate(iphase(0),nbip(0,0),itagnsol(0),knod2surf(0),part_fill(0),ivolsurf(0))
        allocate(swiftsurf(0),nsoltosf(0,0),inod2surf(0),dis(0,0),nod_norm(0),segtosurf(0))
      endif ! if (NUM_INIVOL > 0)

    end subroutine init_inivol
