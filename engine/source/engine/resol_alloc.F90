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
!||    resol_alloc_mod   ../engine/source/engine/resol_alloc.F90
!||--- called by ------------------------------------------------------
!||    resol             ../engine/source/engine/resol.F
!||====================================================================
      module resol_alloc_mod
        implicit none
      contains

! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================

!! \brief Allocation Phase extracted from resol.F lines 1360-2049
!! \details This routine encapsulates the allocation and initialization
!!          logic originally located in resol.F (fixed-form).
!||====================================================================
!||    resol_alloc_phase1   ../engine/source/engine/resol_alloc.F90
!||--- called by ------------------------------------------------------
!||    resol                ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    prepare_debug        ../engine/share/modules/debug_mod.F
!||--- uses       -----------------------------------------------------
!||    connectivity_mod     ../common_source/modules/connectivity.F90
!||    interfaces_mod       ../common_source/modules/interfaces/interfaces_mod.F90
!||    my_alloc_mod         ../common_source/tools/memory/my_alloc.F90
!||    nodal_arrays_mod     ../common_source/modules/nodal_arrays.F90
!||    precision_mod        ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine resol_alloc_phase1(rby6,dxancg,nb25_candt,nb25_impct,nb25_dst1,nb25_dst2,igrouc,igrounc, &
          interfaces,int18add,idamp_rdof_tab,icontact_old, &
          nrbykin,numnod,parasiz,ngroup,ninter,sicontact,nodes,&
          isendto,ircvfrom,intlist,intlist25,niskyfi,niskyfie,&
          count_remslv,count_remslve,fr_nbcc,fr_nbcci2,&
          dretri,xsec,nsect,nspmd,ispmd,ninter25,ibrkin_l,fr_nbcc1)


          use precision_mod, only : wp
          use interfaces_mod, only : interfaces_
          use connectivity_mod, only : connectivity_
          use nodal_arrays_mod, only : nodal_arrays_
          use my_alloc_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nrbykin
          integer, intent(in) :: numnod
          integer, intent(in) :: parasiz
          integer, intent(in) :: ngroup
          integer, intent(in) :: ninter
          integer, intent(in) :: sicontact
          integer, intent(in) :: nsect
          integer, intent(in) :: nspmd
          integer, intent(in) :: ispmd
          integer, intent(in) :: ninter25
          double precision, allocatable, intent(inout) :: rby6(:,:,:)
          real(kind=wp), allocatable, intent(inout) :: dxancg(:,:)
          real(kind=wp), allocatable, intent(inout) :: dretri(:)
          real(kind=wp), allocatable, intent(inout) :: xsec(:)
          integer, allocatable, intent(inout) :: nb25_candt(:)
          integer, allocatable, intent(inout) :: nb25_impct(:)
          integer, allocatable, intent(inout) :: nb25_dst1(:)
          integer, allocatable, intent(inout) :: nb25_dst2(:)
          integer, allocatable, intent(inout) :: igrouc(:)
          integer, allocatable, intent(inout) :: igrounc(:)
          integer, allocatable, intent(inout) :: isendto(:,:)
          integer, allocatable, intent(inout) :: ircvfrom(:,:)
          integer, allocatable, intent(inout) :: fr_nbcc(:,:)
          integer, allocatable, intent(inout) :: fr_nbcci2(:,:)
          integer, allocatable, intent(inout) :: intlist(:)
          integer, allocatable, intent(inout) :: intlist25(:)
          integer, allocatable, intent(inout) :: niskyfi(:)
          integer, allocatable, intent(inout) :: niskyfie(:)
          integer, allocatable, intent(inout) :: count_remslv(:)
          integer, allocatable, intent(inout) :: count_remslve(:)
          type(interfaces_), intent(inout) :: interfaces
          integer, allocatable, intent(inout) :: int18add(:)
          integer, allocatable, intent(inout) :: idamp_rdof_tab(:)
          integer, allocatable, intent(inout) :: icontact_old(:)
          integer, allocatable, intent(inout) :: ibrkin_l(:)
          integer, allocatable, intent(inout) :: fr_nbcc1(:,:)
          type(nodal_arrays_), intent(inout) :: nodes

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          call prepare_debug(nodes%itab,numnod,ispmd,nspmd)
          call my_alloc(rby6,8,6,nrbykin)
          call my_alloc(dxancg,3,numnod)
          call my_alloc(nb25_candt,parasiz)
          call my_alloc(nb25_impct,parasiz)
          call my_alloc(nb25_dst1,parasiz)
          call my_alloc(nb25_dst2,parasiz)
          call my_alloc(igrouc,ngroup)
          call my_alloc(igrounc,ngroup)
          allocate(interfaces%pon%adskyi(0:numnod+1))
          call my_alloc(int18add,ninter+1)
          int18add(1) = 1
          int18add(ninter+1) = 0
          allocate(idamp_rdof_tab(sicontact))
          allocate(icontact_old(sicontact))
          allocate(isendto(ninter+1,nspmd+1))
          allocate(ircvfrom(ninter+1,nspmd+1))
          isendto(1:ninter+1, 1:nspmd+1) = 0
          ircvfrom(1:ninter+1, 1:nspmd+1) = 0
          allocate(intlist(2*ninter))
          allocate(intlist25(ninter25))
          allocate(niskyfi(ninter))
          allocate(niskyfie(ninter))
          allocate(count_remslv(ninter))
          allocate(count_remslve(ninter))
          count_remslv(1:ninter)=0
          count_remslve(1:ninter)=0
          allocate(fr_nbcc  (2,nspmd+1))
          allocate(fr_nbcci2(2,nspmd+1))
          allocate(dretri(5*ninter))
          allocate(xsec(3*4*nsect))
          allocate(ibrkin_l(nrbykin))
          allocate(fr_nbcc1(2,nspmd+1))
        end subroutine resol_alloc_phase1
!||====================================================================
!||    resol_alloc_phase2   ../engine/source/engine/resol_alloc.F90
!||--- called by ------------------------------------------------------
!||    resol                ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    assinit              ../engine/source/assembly/assadd2.F
!||    assinit_crkxfem      ../engine/source/assembly/assadd2.F
!||    assinit_pxfem        ../engine/source/assembly/assadd2.F
!||--- uses       -----------------------------------------------------
!||    connectivity_mod     ../common_source/modules/connectivity.F90
!||    nodal_arrays_mod     ../common_source/modules/nodal_arrays.F90
!||    precision_mod        ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine resol_alloc_phase2(element,nodes,iplyxfem,icrack3d,nspmd,lisendp, lirecvp, &
          lisendp_pxfem, lirecvp_pxfem, lisendp_crk, lirecvp_crk,&
          isendp_pxfem, irecvp_pxfem, isendp_crk, irecvp_crk, adsky_crk, inod_crk,&
          procne_crk, procne_pxfem, adsky_pxfem,inod_pxfem)
          use precision_mod, only : wp
          use connectivity_mod, only : connectivity_
          use nodal_arrays_mod, only : nodal_arrays_
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(connectivity_), intent(inout) :: element
          type(nodal_arrays_), intent(inout) :: nodes
          integer, intent(in) :: iplyxfem
          integer, intent(in) :: icrack3d
          integer, intent(in) :: nspmd
          integer, intent(inout) :: lisendp
          integer, intent(inout) :: lirecvp
          integer, intent(inout) :: procne_pxfem(*)
          integer, intent(inout) :: procne_crk(*)
          integer, intent(inout) :: lisendp_pxfem
          integer, intent(inout) :: lirecvp_pxfem
          integer, intent(inout) :: lisendp_crk
          integer, intent(inout) :: lirecvp_crk
          integer, allocatable, intent(inout) :: isendp_pxfem(:)
          integer, allocatable, intent(inout) :: irecvp_pxfem(:)
          integer, allocatable, intent(inout) :: isendp_crk(:)
          integer, allocatable, intent(inout) :: irecvp_crk(:)
          integer, intent(inout) :: adsky_crk(*)
          integer, intent(inout) :: inod_crk(*)
          integer, intent(inout) :: adsky_pxfem(*)
          integer, intent(inout) :: inod_pxfem(*)

! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------

          call assinit(element%pon%adsky,nodes%boundary_add,nodes%boundary,element%pon%procne,lisendp,lirecvp)
          if(iplyxfem > 0 ) then
            call assinit_pxfem(adsky_pxfem,inod_pxfem,nodes%boundary_add,nodes%boundary,&
              procne_pxfem,lisendp_pxfem,lirecvp_pxfem)
          end if
          if (icrack3d > 0 .and. nspmd > 1) then
            call assinit_crkxfem(adsky_crk,inod_crk,nodes%boundary_add,&
              nodes%boundary,procne_crk,lisendp_crk,&
              lirecvp_crk)
          endif
          allocate(element%pon%isendp(min(lisendp,1):lisendp))
          allocate(element%pon%irecvp(min(lirecvp,1):lirecvp))
          allocate(isendp_pxfem(min(lisendp_pxfem,1):lisendp_pxfem))
          allocate(irecvp_pxfem(min(lirecvp_pxfem,1):lirecvp_pxfem))
          allocate(isendp_crk(min(lisendp_crk,1):lisendp_crk))
          allocate(irecvp_crk(min(lirecvp_crk,1):lirecvp_crk))
        end subroutine resol_alloc_phase2


!||====================================================================
!||    resol_alloc_phase3   ../engine/source/engine/resol_alloc.F90
!||--- called by ------------------------------------------------------
!||    resol                ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    my_alloc_mod         ../common_source/tools/memory/my_alloc.F90
!||====================================================================
        subroutine resol_alloc_phase3(nloadp_hyd_inter,nintloadp,nloadp_hyd,iloadp,sizloadp,numnod, &
          loadp_hyd_inter,tagncont, ninter,&
          s_loadpinter,npresload,loadp_tagdel)
          use my_alloc_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nintloadp
          integer, intent(in) :: nloadp_hyd
          integer, intent(in) :: sizloadp
          integer, intent(in) :: numnod
          integer, intent(in) :: ninter
          integer, intent(in) :: iloadp(*)
          integer, intent(inout) :: nloadp_hyd_inter
          integer, allocatable, intent(inout) :: loadp_hyd_inter(:)
          integer, allocatable, intent(inout) :: tagncont(:,:)
          integer, intent(inout) :: s_loadpinter
          integer, intent(inout) :: npresload
          integer, allocatable, intent(inout) :: loadp_tagdel(:)

!----------------------------------------------------------------------------------------------------------------------
          integer :: k
          nloadp_hyd_inter = 0
          if(nintloadp > 0 ) then
            call my_alloc(loadp_hyd_inter,nloadp_hyd)
            do k=1,nloadp_hyd
              if(iloadp(sizloadp*(k-1)+5) > 0 ) then
                nloadp_hyd_inter = nloadp_hyd_inter + 1
                loadp_hyd_inter(k) = nloadp_hyd_inter
              endif
            enddo
          endif
          if(nloadp_hyd_inter > 0) then
            call my_alloc(tagncont,nloadp_hyd_inter,numnod)
            tagncont = 0
          else
            call my_alloc(tagncont,0,0)
          endif
          s_loadpinter = ninter*nloadp_hyd
          npresload = 0
          if(nloadp_hyd > 0 ) then
            do  k=1,nloadp_hyd
              npresload = npresload + iloadp(sizloadp*(k-1)+1)/4
            enddo
            call my_alloc(loadp_tagdel,npresload)
            loadp_tagdel(1:npresload) =0
          else
            call my_alloc(loadp_tagdel,0)
          endif


        end subroutine resol_alloc_phase3

!||====================================================================
!||    resol_alloc_phase4   ../engine/source/engine/resol_alloc.F90
!||--- called by ------------------------------------------------------
!||    resol                ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    alemuscl_mod         ../common_source/modules/ale/alemuscl_mod.F
!||    constant_mod         ../common_source/modules/constant_mod.F
!||    output_mod           ../common_source/modules/output/output_mod.F90
!||    pblast_mod           ../common_source/modules/loads/pblast_mod.F90
!||    precision_mod        ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine resol_alloc_phase4(nsurf,output,th_surf_num_channel,nloadp, &
          nloadp_f,pblast,iloadp,sizloadp, cnel, addcnel,size_cnel,size_addcnel, &
          idel7ng,irad2r,alemuscl_param,alemuscl_buffer,pdel, addtmpl,tagel,&
          numnod,lcnel,numels,numelq,numelc,numelt,numelp,numelr,numeltg, &
          npart,partsav2,ipartl,elem_state,s_elem_state)
          use precision_mod, only : wp
          use output_mod, only : output_
          use pblast_mod, only : pblast_
          use ALEMUSCL_MOD, only : alemuscl_param_, alemuscl_buffer_
          use constant_mod, only : zero
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(output_),intent(inout)       :: output
          integer, intent(in)               :: nsurf
          integer, intent(in)               :: th_surf_num_channel
          type(pblast_), intent(in)         :: pblast
          integer, intent(in)               :: sizloadp
          integer, intent(in)               :: numnod
          integer, intent(in)               :: lcnel
          integer, intent(in)               :: numels
          integer, intent(in)               :: numelq
          integer, intent(in)               :: numelc
          integer, intent(in)               :: numelt
          integer, intent(in)               :: numelp
          integer, intent(in)               :: numelr
          integer, intent(in)               :: numeltg
          integer, intent(in)               :: npart
          integer, intent(in)               :: idel7ng
          integer, intent(in)               :: irad2r
          integer, intent(in)               :: nloadp
          integer, intent(in)               :: nloadp_f
          type(alemuscl_param_), intent(in) :: alemuscl_param
          type(alemuscl_buffer_), intent(inout) :: alemuscl_buffer
          integer, intent(in)               :: pdel
          integer, intent(in)               :: iloadp(*)
          integer, allocatable, intent(inout) :: ipartl(:)
          real(kind=wp), allocatable, intent(inout) :: partsav2(:,:)
          logical, allocatable, intent(inout) :: elem_state(:)
          integer, intent(inout)              :: s_elem_state
          integer, allocatable, intent(inout),target :: addtmpl(:)
          integer, allocatable, intent(inout) :: tagel(:)
          integer, allocatable, intent(inout),target :: cnel(:)
          integer, allocatable, intent(inout), target :: addcnel(:)
          integer :: size_addcnel
          integer :: size_cnel

          integer :: k,neleml

          if(nsurf > 0) then
            allocate(output%th%th_surf%channels(th_surf_num_channel,nsurf))
            output%th%th_surf%channels(1:th_surf_num_channel,1:nsurf)=zero
          else
            allocate(output%th%th_surf%channels(0,0))
          endif
          if(output%th%th_surf%iok > 0 ) then
            if(output%th%th_surf%loadp_flag > 0 ) then
              output%th%th_surf%nsegloadpf = 0
              do k=1,nloadp_f
                output%th%th_surf%nsegloadpf = output%th%th_surf%nsegloadpf + iloadp(sizloadp*(k-1)+1)/4
              enddo
              output%th%th_surf%nsegloadpb = 0
              do k=nloadp_f+1,pblast%nloadp_b
                output%th%th_surf%nsegloadpb = output%th%th_surf%nsegloadpb + iloadp(sizloadp*(k-1)+1)/4
              enddo
              output%th%th_surf%nsegloadp = 0
              do k=nloadp_f+pblast%nloadp_b+1,nloadp
                output%th%th_surf%nsegloadp = output%th%th_surf%nsegloadp + iloadp(sizloadp*(k-1)+1)/4
              enddo
            endif
          endif
          if((idel7ng>0).or.(irad2r/=0).or.(alemuscl_param%ialemuscl>0).or.(pdel>0))then
            size_addcnel = numnod+1
            size_cnel = lcnel
            neleml = numels+numelq+numelc+numelt+numelp+numelr+numeltg
            s_elem_state = neleml
          else
            size_addcnel = 0
            size_cnel = 0
            s_elem_state = 0
          endif
          allocate(cnel(0:size_cnel))
          allocate(addcnel(0:size_addcnel))
          if((idel7ng>0).or.(irad2r/=0).or.(alemuscl_param%ialemuscl>0).or.(pdel>0))then
! allocation of inverse connectivity array
            allocate(addtmpl(0:numnod+1))
            neleml = numels+numelq+numelc+numelt+numelp+numelr+numeltg
            allocate(tagel(1:neleml))
            tagel(:) = 0
            alemuscl_buffer%pcnel => cnel
            alemuscl_buffer%paddcnel => addcnel
            alemuscl_buffer%paddtmpl => addtmpl
          else
            allocate(addtmpl(0),tagel(0))
          end if
          allocate(ipartl(npart))
          allocate(partsav2(2,npart))
          allocate( elem_state(s_elem_state) )
          elem_state(1:s_elem_state) = .true.
        end subroutine resol_alloc_phase4


!||====================================================================
!||    resol_alloc_phase5   ../engine/source/engine/resol_alloc.F90
!||--- called by ------------------------------------------------------
!||    resol                ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    constant_mod         ../common_source/modules/constant_mod.F
!||    glob_therm_mod       ../common_source/modules/mat_elem/glob_therm_mod.F90
!||    precision_mod        ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine resol_alloc_phase5(glob_therm,numnod,ninter,nodadt,nthread,iparit,lsky,lskyi,&
          icodt0,icodr0,mcp_off,fthe,fthesky,qfricint,condn,condnsky, &
          ftheskyi,condnskyi,icondn,ifthe)

          use precision_mod, only : wp
          use glob_therm_mod, only : glob_therm_
          use constant_mod, only : zero
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: numnod
          integer, intent(in) :: ninter
          integer, intent(in) :: nodadt
          integer, intent(in) :: nthread
          integer, intent(in) :: iparit
          integer, intent(in) :: lsky
          integer, intent(in) :: lskyi
          integer, intent(inout) :: icondn
          integer, intent(inout) :: ifthe
          type(glob_therm_), intent(inout) :: glob_therm
          real(kind=wp), allocatable, intent(inout) :: icodt0(:)
          real(kind=wp), allocatable, intent(inout) :: icodr0(:)
          real(kind=wp), allocatable, intent(inout) :: mcp_off(:)
          real(kind=wp), allocatable, intent(inout) :: fthe(:)
          real(kind=wp), allocatable, intent(inout) :: fthesky(:)
          real(kind=wp), allocatable, intent(inout) :: qfricint(:)
          real(kind=wp), allocatable, intent(inout) :: condn(:)
          real(kind=wp), allocatable, intent(inout) :: condnsky(:)
          real(kind=wp), allocatable, intent(inout) :: ftheskyi(:)
          real(kind=wp), allocatable, intent(inout) :: condnskyi(:)
!----------------------------------------------------------------------------------------------------------------------
          glob_therm%nodadt_therm = 0
          if(glob_therm%idt_therm == 1) then
            if(ninter>0.or.nodadt>0) glob_therm%nodadt_therm = 1 ! flag for nodal thermal time step
          endif
          if(glob_therm%idt_therm == 1) then
            allocate(icodt0(numnod)) ! tabs for storing initial kinimatic when thermal time step
            allocate(icodr0(numnod))
          else
            allocate(icodt0(0))
            allocate(icodr0(0))
          endif

          allocate(mcp_off(numnod))
          mcp_off(1:numnod) = 1.0

          ifthe  = 1
          icondn = 1
          if(glob_therm%itherm_fe > 0 ) then
            if(iparit == 3 ) then
              ifthe = numnod+3*numnod*nthread
              allocate(fthe(ifthe), fthesky(lsky))
            elseif(iparit /= 0 ) then
              ifthe = numnod
              allocate(fthe(ifthe), fthesky(lsky))
            else
              ifthe = numnod*nthread
              allocate(fthe(ifthe), fthesky(0))
            endif
            allocate(qfricint(ninter))
            qfricint(1:ninter) = zero
            if (glob_therm%nodadt_therm == 1) then
              if(iparit == 0 ) then
                icondn = numnod*nthread
                allocate (condn(icondn), condnsky(0))
              else
                icondn = numnod
                allocate (condn(icondn))
                allocate (condnsky(lsky))
              endif
            endif
          else
            ifthe  = 1
            icondn = 1
            allocate(fthe(ifthe), fthesky(0))
            allocate(qfricint(ninter))
            qfricint(1:ninter) = zero
            allocate(condn(icondn),condnsky(0))
          endif
          if (glob_therm%intheat > 0  ) then
            if(iparit /= 0 )then
              allocate(ftheskyi(lskyi))
              ftheskyi(1:lskyi) = 0
            else
              allocate(ftheskyi(0))
            endif
            if (glob_therm%nodadt_therm == 1) then
              if(iparit /= 0 )then
                allocate(condnskyi(lskyi))
              else
                allocate(condnskyi(0))
              endif
            else
              allocate(condnskyi(0))
            endif
          else
            allocate(ftheskyi(0))
            allocate(condnskyi(0))
          endif

        end subroutine resol_alloc_phase5

!||====================================================================
!||    resol_alloc_phase6   ../engine/source/engine/resol_alloc.F90
!||--- called by ------------------------------------------------------
!||    resol                ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    constant_mod         ../common_source/modules/constant_mod.F
!||    plyxfem_mod          ../engine/share/modules/plyxfem_mod.F
!||    precision_mod        ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine resol_alloc_phase6(intplyxfem,iplyxfem,nplymax,ply,plysky,iparit,nthread, &
          nplyxfe,anim_ply,vn_nod,irigid_mat,nrbym, &
          vrbym,vrrbym,arbym,arrbym,plyskyi,lskyi,lskypxfem)
          use plyxfem_mod, only : ply_data
          use constant_mod, only : zero
          use precision_mod, only : wp
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: intplyxfem
          integer, intent(in) :: iplyxfem
          integer, intent(in) :: nplymax
          integer, intent(in) :: lskypxfem
          integer, intent(in) :: lskyi
          integer, intent(in) :: nthread
          type(ply_data), allocatable, intent(inout) :: ply(:)
          type(ply_data), allocatable, intent(inout) :: plysky(:)
          integer, intent(in) :: iparit
          integer, intent(in) :: nplyxfe
          integer, intent(in) :: anim_ply
          real(kind=wp), allocatable, intent(inout) :: vn_nod(:,:)
          integer, intent(in) :: irigid_mat
          integer, intent(in) :: nrbym
          real(kind=wp), allocatable, intent(inout) :: vrbym(:)
          real(kind=wp), allocatable, intent(inout) :: vrrbym(:)
          real(kind=wp), allocatable, intent(inout) :: arbym(:)
          real(kind=wp), allocatable, intent(inout) :: arrbym(:)
          type(ply_data), intent(inout), allocatable :: plyskyi
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
          if(iplyxfem > 0 ) then
            if(iparit /= 0 ) then
              allocate(plysky(nplymax))
              do i=1,nplymax
                nullify(ply(i)%a,plysky(i)%fsky)
                allocate(ply(i)%a(4,nplyxfe),plysky(i)%fsky(4,lskypxfem))
                plysky(i)%fsky = zero
                ply(i)%a = zero
                nullify(ply(i)%itag)
                allocate(ply(i)%itag(nplyxfe))
                ply(i)%itag = 0
              enddo
            else
! is not available iparit = 0
              do i=1,nplymax
                nullify(ply(i)%a)
                allocate(ply(i)%a(4,nplyxfe*nthread))
                ply(i)%a  = zero
              enddo
              allocate(plysky(0))
            endif
            if(anim_ply > 0) then
              allocate(vn_nod(3,nplyxfe))
              vn_nod = zero
            else
              allocate(vn_nod(0,0))
            endif
          else
            allocate(ply(0),plysky(0))
          endif
! for interface type 24 + pxfem
          if(intplyxfem > 0 ) then
            if(iparit /= 0 ) then
              allocate(plyskyi)
              nullify(plyskyi%fskyi)
              allocate(plyskyi%fskyi(lskyi,5))
              plyskyi%fskyi = zero
            else
              allocate(plyskyi)
            endif
          else
          endif
          if(irigid_mat > 0 ) then
            allocate(vrbym(3*nrbym),vrrbym(3*nrbym), arbym(3*nrbym),arrbym(3*nrbym))
            vrbym  = zero
            vrrbym = zero
            arbym  = zero
            arrbym = zero
          else
            allocate(vrbym(0),vrrbym(0),arbym(0),arrbym(0))
          endif
        end subroutine resol_alloc_phase6

!||====================================================================
!||    resol_alloc_phase7   ../engine/source/engine/resol_alloc.F90
!||--- called by ------------------------------------------------------
!||    resol                ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    constant_mod         ../common_source/modules/constant_mod.F
!||    precision_mod        ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine resol_alloc_phase7(ialelag,numnod,nthread,iparit,lsky,msnf,msf, &
          aflow,vflow,dflow,wflow,ffsky,ifoam,ifoam_cont)
          use constant_mod, only : zero
          use precision_mod, only : wp
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: ialelag
          integer, intent(in) :: numnod
          integer, intent(in) :: nthread
          integer, intent(in) :: iparit
          integer, intent(in) :: lsky
          real(kind=wp), intent(in) :: msnf(*)
          real(kind=wp), intent(inout) :: msf(*)
          real(kind=wp), allocatable, intent(inout) :: aflow(:)
          real(kind=wp), allocatable, intent(inout) :: vflow(:)
          real(kind=wp), allocatable, intent(inout) :: dflow(:)
          real(kind=wp), allocatable, intent(inout) :: wflow(:)
          real(kind=wp), allocatable, intent(inout) :: ffsky(:)
          integer, allocatable, intent(inout) :: ifoam(:)
          integer, allocatable, intent(inout) :: ifoam_cont(:)
! ----------------------------------------------------------------------------------------------------------------------

          if( ialelag > 0) then
            if(iparit == 0) then
              allocate (aflow(3*numnod*nthread))
              allocate(ffsky(0))
              allocate(ifoam(numnod*nthread))
              allocate(ifoam_cont(numnod*nthread))
            else
              allocate (aflow(3*numnod))
              allocate(ffsky(3*lsky))
              allocate(ifoam(numnod))
              allocate(ifoam_cont(numnod))
              ffsky = zero
            endif
            aflow = zero
            ifoam = 0
            ifoam_cont =0
            msf(1:numnod) =  msnf(1:numnod)
          else
            allocate(aflow(0),vflow(0),dflow(0),wflow(0),ffsky(0),ifoam(0),ifoam_cont(0))
          endif
        end subroutine resol_alloc_phase7
!||====================================================================
!||    resol_alloc_phase8   ../engine/source/engine/resol_alloc.F90
!||--- called by ------------------------------------------------------
!||    resol                ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    h3d_mod              ../engine/share/modules/h3d_mod.F
!||    precision_mod        ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine resol_alloc_phase8(nadmesh,numelc,numeltg,numnod,levelmax,iparit,istatcnd,anim_n, &
          h3d_data,iroddl,lsh4act,lsh4kin,psh4act,psh4kin,lsh3act,lsh3kin, &
          psh3act,psh3kin,msh4sky,msh3sky,ilevnod,lsh4upl,lsh3upl,psh4upl,psh3upl,&
          acnd,arcnd,stcnd,strcnd,stifr_tmp,stifn_tmp,nthread)
          use precision_mod, only : wp
          use h3d_mod, only : h3d_database
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nadmesh
          integer, intent(in) :: numelc
          integer, intent(in) :: numeltg
          integer, intent(in) :: numnod
          integer, intent(in) :: levelmax
          integer, intent(in) :: iparit
          integer, intent(in) :: istatcnd
          integer, intent(in) :: nthread
          integer, intent(in) :: anim_n(*)
          type(h3d_database), intent(in) :: h3d_data
          integer, intent(in) :: iroddl
          integer, allocatable, intent(inout) :: lsh4act(:)
          integer, allocatable, intent(inout) :: lsh4kin(:)
          integer, allocatable, intent(inout) :: psh4act(:)
          integer, allocatable, intent(inout) :: psh4kin(:)
          integer, allocatable, intent(inout) :: lsh3act(:)
          integer, allocatable, intent(inout) :: lsh3kin(:)
          integer, allocatable, intent(inout) :: psh3act(:)
          integer, allocatable, intent(inout) :: psh3kin(:)
          integer, allocatable, intent(inout) :: msh4sky(:)
          integer, allocatable, intent(inout) :: msh3sky(:)
          integer, allocatable, intent(inout) :: ilevnod(:)
          integer, allocatable, intent(inout) :: lsh4upl(:)
          integer, allocatable, intent(inout) :: lsh3upl(:)
          integer, allocatable, intent(inout) :: psh4upl(:)
          integer, allocatable, intent(inout) :: psh3upl(:)
          real(kind=wp), allocatable, intent(inout) :: acnd(:,:)
          real(kind=wp), allocatable, intent(inout) :: arcnd(:,:)
          real(kind=wp), allocatable, intent(inout) :: stcnd(:)
          real(kind=wp), allocatable, intent(inout) :: strcnd(:)
          real(kind=wp), allocatable, intent(inout) :: stifr_tmp(:)
          real(kind=wp), allocatable, intent(inout) :: stifn_tmp(:)
! ----------------------------------------------------------------------------------------------------------------------
          if(nadmesh/=0)then
            allocate(lsh4act(numelc),lsh4kin(numelc))
            allocate(psh4act(0:levelmax+1),psh4kin(0:levelmax+1))
            allocate(lsh3act(numeltg),lsh3kin(numeltg))
            allocate(psh3act(0:levelmax+1),psh3kin(0:levelmax+1))
            if (iparit/=0) then
              allocate(msh4sky(numelc),msh3sky(numeltg))
            else
              allocate(msh4sky(0),msh3sky(0))
            end if
            allocate(ilevnod(0:numnod))
          end if
          if(istatcnd /= 0)then
            allocate(lsh4upl(numelc),lsh3upl(numeltg),psh4upl(0:levelmax),psh3upl(0:levelmax))
            allocate(acnd(3,numnod),arcnd(3,numnod),stcnd(nthread*numnod) ,strcnd(numnod))
          else
            allocate(lsh4upl(0),lsh3upl(0),psh4upl(0),psh3upl(0))
            allocate(acnd(0,0),arcnd(0,0),stcnd(0) ,strcnd(0) )
          end if
          if( (anim_n(18) /= 0 .or. h3d_data%n_scal_stifr /= 0) .and. iroddl /= 0) then
            allocate(stifr_tmp(numnod))
          else
            allocate(stifr_tmp(0))
          endif
          if( anim_n(19) /= 0 .or. h3d_data%n_scal_stifn /= 0) then
            allocate(stifn_tmp(numnod))
          else
            allocate(stifn_tmp(0))
          endif

        end subroutine resol_alloc_phase8

!||====================================================================
!||    resol_alloc_phase9   ../engine/source/engine/resol_alloc.F90
!||--- called by ------------------------------------------------------
!||    resol                ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    constant_mod         ../common_source/modules/constant_mod.F
!||    h3d_mod              ../engine/share/modules/h3d_mod.F
!||    my_alloc_mod         ../common_source/tools/memory/my_alloc.F90
!||    precision_mod        ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine resol_alloc_phase9(ANIM_CE,NUMELC,NUMELTG,NUMNOD,IADMERRT,H3D_DATA, &
          INTER_ITHKNOD,ERR_THK_SH4,ERR_THK_SH3,THKSH4,THKSH3,THKNOD, &
          AREA_SH4,AREA_SH3,AREA_NOD,THICK_SH4,THICK_SH3,THICK_NOD, &
          admerr_AREA_SH4,admerr_AREA_SH3,admerr_AREA_NOD, &
          admerr_THICK_SH4,admerr_THICK_SH3,admerr_THICK_NOD)
          use precision_mod, only : wp
          use constant_mod, only : ZERO
          use H3D_MOD, only : H3D_DATABASE
          use my_alloc_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: NUMELC
          integer, intent(in) :: NUMELTG
          integer, intent(in) :: NUMNOD
          integer, intent(in) :: IADMERRT
          integer, intent(in) :: INTER_ITHKNOD
          integer, intent(in) :: ANIM_CE(*)
          type(H3D_DATABASE), intent(in) :: H3D_DATA
          real(kind=wp), allocatable, intent(inout) :: ERR_THK_SH4(:)
          real(kind=wp), allocatable, intent(inout) :: ERR_THK_SH3(:)
          real(kind=wp), allocatable, intent(inout) :: THKSH4(:)
          real(kind=wp), allocatable, intent(inout) :: THKSH3(:)
          real(kind=wp), allocatable, intent(inout) :: THKNOD(:)
          real(kind=wp), allocatable, intent(inout) :: AREA_SH4(:)
          real(kind=wp), allocatable, intent(inout) :: AREA_SH3(:)
          real(kind=wp), allocatable, intent(inout) :: AREA_NOD(:)
          real(kind=wp), allocatable, intent(inout) :: THICK_SH4(:)
          real(kind=wp), allocatable, intent(inout) :: THICK_SH3(:)
          real(kind=wp), allocatable, intent(inout) :: THICK_NOD(:)
          real(kind=wp), allocatable, intent(inout) :: admerr_AREA_SH4(:)
          real(kind=wp), allocatable, intent(inout) :: admerr_AREA_SH3(:)
          real(kind=wp), allocatable, intent(inout) :: admerr_AREA_NOD(:)
          real(kind=wp), allocatable, intent(inout) :: admerr_THICK_SH4(:)
          real(kind=wp), allocatable, intent(inout) :: admerr_THICK_SH3(:)
          real(kind=wp), allocatable, intent(inout) :: admerr_THICK_NOD(:)
! ----------------------------------------------------------------------------------------------------------------------
          IF(ANIM_CE(2156)/=0 .OR. IADMERRT/=0 .OR. H3D_DATA%SH_SCAL_ERR_THK /= 0)THEN
            ALLOCATE(ERR_THK_SH4(NUMELC))
            ALLOCATE(ERR_THK_SH3(NUMELTG))
            ERR_THK_SH4(1:NUMELC) = ZERO
            ERR_THK_SH3(1:NUMELTG) = ZERO
          ELSE
            ALLOCATE(ERR_THK_SH4(0))
            ALLOCATE(ERR_THK_SH3(0))
          END IF
          IF(INTER_ITHKNOD/=0)THEN
            ALLOCATE(THKSH4(NUMELC),THKSH3(NUMELTG),THKNOD(NUMNOD))
          ELSE
            ALLOCATE(THKSH4(0))
            ALLOCATE(THKSH3(0))
            ALLOCATE(THKNOD(0))
          END IF
          IF( ANIM_CE(2156)/=0 .OR. H3D_DATA%SH_SCAL_ERR_THK /=0) THEN
            CALL MY_ALLOC(AREA_SH4,NUMELC)
            CALL MY_ALLOC(AREA_SH3,NUMELTG)
            CALL MY_ALLOC(AREA_NOD,NUMNOD)
            CALL MY_ALLOC(THICK_SH4,NUMELC)
            CALL MY_ALLOC(THICK_SH3,NUMELTG)
            CALL MY_ALLOC(THICK_NOD,NUMNOD)
          ENDIF
          IF (IADMERRT/=0) THEN
            CALL MY_ALLOC(admerr_AREA_SH4,NUMELC)
            CALL MY_ALLOC(admerr_AREA_SH3,NUMELTG)
            CALL MY_ALLOC(admerr_AREA_NOD,NUMNOD)
            CALL MY_ALLOC(admerr_THICK_SH4,NUMELC)
            CALL MY_ALLOC(admerr_THICK_SH3,NUMELTG)
            CALL MY_ALLOC(admerr_THICK_NOD,NUMNOD)
          ENDIF
        end subroutine resol_alloc_phase9

!||====================================================================
!||    resol_alloc_phase10   ../engine/source/engine/resol_alloc.F90
!||--- called by ------------------------------------------------------
!||    resol                 ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    constant_mod          ../common_source/modules/constant_mod.F
!||    precision_mod         ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine resol_alloc_phase10(ngroup,idtmins, idtmins_int,tagnod_sms,nativ_sms,tagprt_sms,tagrel_sms,indx1_sms,indx2_sms, &
          tagslv_rby_sms,tagmsr_rby_sms,kad_sms,jad_sms,iad_sms,lad_sms, &
          jadc_sms,jads_sms,jads10_sms,jadt_sms,jadp_sms,jadr_sms,jadtg_sms, &
          x_sms,p_sms,y_sms,z_sms,prec_sms,xmom_sms,prec_sms3,diag_sms3, &
          t2main_sms,t2fac_sms,fr_rms,fr_sms,ptr_sms,nspmd,nintstamp, &
          jadi21_sms,tagslv_i21_sms,tagmsr_i21_sms,nodxi_sms,nodii_sms, &
          mskyi_sms,iskyi_sms,jadi_sms,jdii_sms,lti_sms,lskyi_sms,&
          npart,numelc,numels,numels10,numelt,numelp,numelr,numeltg,numnod)
          use precision_mod, only : wp
          use constant_mod, only : one
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: ngroup
          integer, intent(in) :: idtmins
          integer, intent(in) :: idtmins_int
          integer, intent(in) :: nspmd
          integer, intent(in) :: nintstamp
          integer, intent(in) :: lskyi_sms
          integer, intent(in) :: npart
          integer, intent(in) :: numelc
          integer, intent(in) :: numels
          integer, intent(in) :: numels10
          integer, intent(in) :: numelt
          integer, intent(in) :: numelp
          integer, intent(in) :: numelr
          integer, intent(in) :: numeltg
          integer, intent(in) :: numnod
          integer, allocatable, intent(inout) :: tagnod_sms(:)
          integer, allocatable, intent(inout),target :: nativ_sms(:)
          integer, allocatable, intent(inout) :: tagprt_sms(:)
          integer, allocatable, intent(inout) :: tagrel_sms(:)
          integer, allocatable, intent(inout) :: indx1_sms(:)
          integer, allocatable, intent(inout) :: indx2_sms(:)
          integer, allocatable, intent(inout) :: tagslv_rby_sms(:)
          integer, allocatable, intent(inout) :: tagmsr_rby_sms(:)
          integer, allocatable, intent(inout) :: kad_sms(:)
          integer, allocatable, intent(inout) :: jad_sms(:)
          integer, allocatable, intent(inout) :: iad_sms(:)
          integer, allocatable, intent(inout) :: lad_sms(:)
          integer, allocatable, intent(inout) :: jadc_sms(:)
          integer, allocatable, intent(inout) :: jads_sms(:)
          integer, allocatable, intent(inout) :: jads10_sms(:)
          integer, allocatable, intent(inout) :: jadt_sms(:)
          integer, allocatable, intent(inout) :: jadp_sms(:)
          integer, allocatable, intent(inout) :: jadr_sms(:)
          integer, allocatable, intent(inout) :: jadtg_sms(:)
          real(kind=wp), allocatable, intent(inout) :: x_sms(:,:)
          real(kind=wp), allocatable, intent(inout) :: p_sms(:,:)
          real(kind=wp), allocatable, intent(inout) :: y_sms(:,:)
          real(kind=wp), allocatable, intent(inout) :: z_sms(:,:)
          real(kind=wp), allocatable, intent(inout) :: prec_sms(:)
          real(kind=wp), allocatable, intent(inout) :: xmom_sms(:,:)
          real(kind=wp), allocatable, intent(inout) :: prec_sms3(:,:)
          real(kind=wp), allocatable, intent(inout) :: diag_sms3(:,:)
          integer, allocatable, intent(inout) :: t2main_sms(:,:)
          real(kind=wp), allocatable, intent(inout) :: t2fac_sms(:)
          integer, intent(inout) :: fr_rms(*)
          integer, intent(inout) :: fr_sms(*)
          integer, pointer, intent(inout) :: ptr_sms(:)
          integer, allocatable, intent(inout) :: jadi21_sms(:)
          integer, allocatable, intent(inout) :: tagslv_i21_sms(:)
          integer, allocatable, intent(inout) :: tagmsr_i21_sms(:)
          integer, allocatable, intent(inout) :: nodxi_sms(:)
          integer, allocatable, intent(inout) :: nodii_sms(:)
          real(kind=wp), allocatable, intent(inout) :: mskyi_sms(:)
          integer, allocatable, intent(inout) :: iskyi_sms(:,:)
          integer, allocatable, intent(inout) :: jadi_sms(:)
          integer, allocatable, intent(inout) :: jdii_sms(:)
          real(kind=wp), allocatable, intent(inout) :: lti_sms(:)
! ----------------------------------------------------------------------------------------------------------------------
          if(idtmins /= 0)then
            allocate(tagnod_sms(numnod),nativ_sms(numnod),&
              tagprt_sms(npart),tagrel_sms(ngroup),&
              indx1_sms(numnod),indx2_sms(numnod),&
              tagslv_rby_sms(numnod),tagmsr_rby_sms(numnod),&
              kad_sms(numnod+1), jad_sms(numnod+1), iad_sms(numnod+1), lad_sms(numnod+1),&
              jadc_sms(4*numelc),&
              jads_sms(8*numels), jads10_sms(6*numels10),&
              jadt_sms(2*numelt),&
              jadp_sms(2*numelp),&
              jadr_sms(3*numelr),&
              jadtg_sms(3*numeltg),&
              x_sms(3,numnod), p_sms(3,numnod),&
              y_sms(3,numnod), z_sms(3,numnod),&
              prec_sms(numnod), xmom_sms(3,numnod),&
              prec_sms3(3,numnod),diag_sms3(3,numnod),&
              t2main_sms(6,numnod),&
              t2fac_sms(numnod))
            fr_rms(1:nspmd+1)=0
            fr_sms(1:nspmd+1)=0
            t2main_sms = 0
            t2fac_sms = one
          elseif(idtmins_int/=0)then
            allocate(tagnod_sms(numnod),nativ_sms(0),&
              tagprt_sms(0),tagrel_sms(0),&
              indx1_sms(numnod),indx2_sms(numnod),&
              tagslv_rby_sms(numnod),tagmsr_rby_sms(numnod),&
              kad_sms(0), jad_sms(0), lad_sms(0),&
              jadc_sms(0),&
              jads_sms(0), jads10_sms(0),&
              jadt_sms(0),&
              jadp_sms(0),&
              jadr_sms(0),&
              jadtg_sms(0),&
              x_sms(3,numnod), p_sms(3,numnod),&
              y_sms(3,numnod), z_sms(3,numnod),&
              prec_sms(numnod), xmom_sms(3,numnod),&
              prec_sms3(3,numnod),diag_sms3(3,numnod),&
              t2main_sms(6,numnod),&
              t2fac_sms(numnod))
            tagnod_sms(1:numnod)=0
            fr_rms(1:nspmd+1)=0
            fr_sms(1:nspmd+1)=0
            t2main_sms = 0
            t2fac_sms = one
          else
            allocate(tagnod_sms(0),nativ_sms(0),&
              tagprt_sms(0),tagrel_sms(0),&
              indx1_sms(0),indx2_sms(0),&
              tagslv_rby_sms(0),tagmsr_rby_sms(0),&
              kad_sms(0), jad_sms(0), lad_sms(0),&
              jadc_sms(0),&
              jads_sms(0), jads10_sms(0),&
              jadt_sms(0),&
              jadp_sms(0),&
              jadr_sms(0),&
              jadtg_sms(0),&
              x_sms(0,0), p_sms(0,0),&
              y_sms(0,0), z_sms(0,0), prec_sms(0),&
              xmom_sms(0,0), prec_sms3(0,0),diag_sms3(0,0),&
              t2main_sms(0,0),&
              t2fac_sms(0))
            ptr_sms => nativ_sms
          end if
          if(idtmins == 2 .and. nintstamp /=0)then
            allocate(jadi21_sms(nintstamp),tagslv_i21_sms(numnod),tagmsr_i21_sms(numnod))
          else
            allocate(jadi21_sms(0),tagslv_i21_sms(0),tagmsr_i21_sms(0))
          end if
          if(idtmins /= 0 .or. idtmins_int /= 0)then
            allocate(nodxi_sms(numnod),nodii_sms(numnod))
          else
            allocate(nodxi_sms(0),nodii_sms(0))
          end if
          if(idtmins == 2 .or. idtmins_int /= 0)then
            allocate(mskyi_sms(lskyi_sms),iskyi_sms(lskyi_sms,3),&
              jadi_sms(numnod+1),jdii_sms(2*lskyi_sms),lti_sms(2*lskyi_sms))
          else
            allocate(mskyi_sms(0),iskyi_sms(0,0),jadi_sms(0),jdii_sms(0),lti_sms(0))
          end if

        end subroutine resol_alloc_phase10

!||====================================================================
!||    resol_alloc_phase11   ../engine/source/engine/resol_alloc.F90
!||--- called by ------------------------------------------------------
!||    resol                 ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    constant_mod          ../common_source/modules/constant_mod.F
!||    precision_mod         ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine resol_alloc_phase11(IREAC,IGRELEM,NUMNOD,NUMELSG,NUMELS16G,NUMSPHG, &
          NUMELCG,NUMELTGG,NUMELQG,NUMELTG,NUMELPG,NUMELRG,NTHPART, &
          NGPE,NGRTH,NELEM,NODREAC,GRTH,IGRTH,DXANCG, &
          IGROUPFLG,IGROUPC,IGROUPTG,IGROUPS,NUMMAT,IPM,NPROPMI, &
          NVOLU,NUMELC,NUMELS)
          use precision_mod, only : wp
          use constant_mod, only : ZERO
          implicit none
  ! ----------------------------------------------------------------------------------------------------------------------
  !                                                     arguments
  ! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: IREAC
          integer, intent(in) :: IGRELEM
          integer, intent(in) :: NUMNOD
          integer, intent(in) :: NUMELSG
          integer, intent(in) :: NUMELS16G
          integer, intent(in) :: NUMSPHG
          integer, intent(in) :: NUMELCG
          integer, intent(in) :: NUMELTGG
          integer, intent(in) :: NUMELQG
          integer, intent(in) :: NUMELPG
          integer, intent(in) :: NUMELRG
          integer, intent(in) :: NTHPART
          integer, intent(in) :: NUMMAT
          integer, intent(in) :: NVOLU
          integer, intent(in) :: NUMELC
          integer, intent(in) :: NUMELS
          integer, intent(in) :: NUMELTG
          integer, intent(in) :: NPROPMI
          integer, intent(inout) :: NGPE
          integer, intent(inout) :: NGRTH
          integer, intent(inout) :: NELEM
          integer, allocatable, intent(inout) :: NODREAC(:)
          integer, allocatable, intent(inout) :: GRTH(:)
          integer, allocatable, intent(inout) :: IGRTH(:)
          real(kind=wp), intent(inout) :: DXANCG(:,:)
          integer, intent(inout) :: IGROUPFLG(2)
          integer, allocatable, intent(inout) :: IGROUPC(:)
          integer, allocatable, intent(inout) :: IGROUPTG(:)
          integer, allocatable, intent(inout) :: IGROUPS(:)
          integer, intent(in) :: IPM(NPROPMI,*)
  ! ----------------------------------------------------------------------------------------------------------------------
          integer :: I
            IF (IREAC == 1 ) THEN
                ALLOCATE(NODREAC(NUMNOD))
              ELSE
                ALLOCATE(NODREAC(0))
              ENDIF
              IF (IGRELEM == 1) THEN
                NGPE = NTHPART
                NGRTH = NTHPART
                NELEM=NUMELSG+3*NUMELS16G+NUMSPHG+NUMELCG+NUMELTGG+NUMELQG+NUMELTG+NUMELPG+2*NUMELRG
              ELSE
                NGPE = 0
                NGRTH = 0
                NELEM = 0
              ENDIF
              IF (IGRELEM == 1 ) THEN
                ALLOCATE(GRTH(NELEM+NGRTH+1))
                ALLOCATE(IGRTH(NELEM+1))
              ELSE
                ALLOCATE(GRTH(1))
                ALLOCATE(IGRTH(1))
              ENDIF
              IGRTH = 0
              GRTH = 0
              DXANCG = ZERO
!------------------------------------------
!     ALLOCATION IGROUPC AND IGROUPTG
!     TABLE GIVING GROUP NUMBER FOR SHELLS
!     ALLOCATION IGROUPS FOR BRICKS
!------------------------------------------
              IGROUPFLG(1:2)=0
              IF(NVOLU > 0) IGROUPFLG(1) = 1
              DO I=1,NUMMAT
                IF(IPM(2,I)/=19.AND.IPM(2,I)/=58) CYCLE
                IF(IPM(4,I) >= 4) IGROUPFLG(1)=1
              ENDDO
              IF(IGROUPFLG(1) == 1) THEN
                ALLOCATE(IGROUPC(NUMELC))
                ALLOCATE(IGROUPTG(NUMELTG))
              ELSE
                ALLOCATE(IGROUPC(0))
                ALLOCATE(IGROUPTG(0))
              ENDIF
              IGROUPFLG(2)=1
              ALLOCATE(IGROUPS(NUMELS))
        end subroutine resol_alloc_phase11

!||====================================================================
!||    resol_alloc_nitsche   ../engine/source/engine/resol_alloc.F90
!||--- called by ------------------------------------------------------
!||    resol                 ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    constant_mod          ../common_source/modules/constant_mod.F
!||    precision_mod         ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine resol_alloc_nitsche(NFACNIT, NITSCHE, IPARIT, NUMELS, NUMELS10G, LSKY, &
          STRESSMEAN, FORNEQSKY)
          use constant_mod, only : ZERO
          use precision_mod, only : wp
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: NITSCHE
          integer, intent(in) :: IPARIT
          integer, intent(in) :: NUMELS
          integer, intent(in) :: NUMELS10G
          integer, intent(in) :: LSKY
          integer, intent(inout) :: NFACNIT
          real(kind=wp), allocatable, intent(inout) :: STRESSMEAN(:,:)
          real(kind=wp), allocatable, intent(inout) :: FORNEQSKY(:)
! ----------------------------------------------------------------------------------------------------------------------
! NITSCHE METHOD
              NFACNIT =0
              IF (NITSCHE > 0 ) THEN
!  Element mean stress
                ALLOCATE(STRESSMEAN(6,NUMELS))
!  Equivalent nodal force
                IF(IPARIT /= 0 ) THEN
                  IF(NUMELS10G ==0) THEN
                    NFACNIT = 6
                    ALLOCATE(FORNEQSKY(18*LSKY))
                    FORNEQSKY(1:18*LSKY) = ZERO
                  ELSE
                    NFACNIT = 16
                    ALLOCATE(FORNEQSKY(48*LSKY))
                    FORNEQSKY(1:48*LSKY) = ZERO
                  ENDIF
                ELSE
                  ALLOCATE(FORNEQSKY(0))
                ENDIF
                STRESSMEAN(1:6,1:NUMELS)=ZERO
              ELSE
                ALLOCATE(STRESSMEAN(0,0))
                ALLOCATE( FORNEQSKY(0))
              ENDIF
        end subroutine resol_alloc_nitsche

!||====================================================================
!||    resol_alloc_python                 ../engine/source/engine/resol_alloc.F90
!||--- called by ------------------------------------------------------
!||    resol                              ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    funct_python_update_elements       ../engine/source/tools/curve/funct_python_update_elements.F90
!||    python_dummy_active_node           ../engine/source/loads/general/python_call_funct_cload.F90
!||    python_register                    ../engine/source/tools/curve/python_register.F90
!||    python_share_memory                ../engine/source/coupling/python/python_share_memory.F90
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod               ../common_source/modules/ale/ale_connectivity_mod.F
!||    connectivity_mod                   ../common_source/modules/connectivity.F90
!||    elbufdef_mod                       ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    funct_python_update_elements_mod   ../engine/source/tools/curve/funct_python_update_elements.F90
!||    glob_therm_mod                     ../common_source/modules/mat_elem/glob_therm_mod.F90
!||    mat_elem_mod                       ../common_source/modules/mat_elem/mat_elem_mod.F90
!||    multi_fvm_mod                      ../common_source/modules/ale/multi_fvm_mod.F90
!||    nodal_arrays_mod                   ../common_source/modules/nodal_arrays.F90
!||    output_mod                         ../common_source/modules/output/output_mod.F90
!||    precision_mod                      ../common_source/modules/precision_mod.F90
!||    python_call_funct_cload_mod        ../engine/source/loads/general/python_call_funct_cload.F90
!||    python_funct_mod                   ../common_source/modules/python_mod.F90
!||    python_register_mod                ../engine/source/tools/curve/python_register.F90
!||    python_share_memory_mod            ../engine/source/coupling/python/python_share_memory.F90
!||    stack_mod                          ../engine/share/modules/stack_mod.F
!||====================================================================
        subroutine resol_alloc_python(PYTHON, NODES, NUMNOD, IXS, NIXS, NUMELS, &
          NIXC, NUMELC, IXP, NIXP, NUMELP, IXT, NIXT, NUMELT, &
          IXQ, NIXQ, NUMELQ, IXTG, NIXTG, NUMELTG, &
          IXR, NIXR, NUMELR, IPARG, NGROUP, NPARG, MVSIZ, &
          IPART, LIPART1, NPART, NTHPART, ISPMD, N2D, NUMGEO, NUMMAT, &
          ELBUF_TAB, GEO, PM, BUFMAT, EANI, &
          IPM, IGEO, THKE, STHKE, ERR_THK_SH4, ERR_THK_SH3, &
          W, ALE_CONNECTIVITY, NPROPG, NPROPGI, NPROPM, NPROPMI, SEANI, &
          snercvois, snesdvois, slercvois, slesdvois, &
          NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, &
          M51_N0PHAS, M51_NVPHAS, STACK, &
          MULTI_FVM, MAT_ELEM, OUTPUT, GLOB_THERM, ELEMENT)
          use precision_mod, only : wp
          use python_funct_mod
          use python_register_mod
          use PYTHON_SHARE_MEMORY_mod
          use nodal_arrays_mod, only : nodal_arrays_
          use funct_python_update_elements_mod
          USE python_call_funct_cload_mod
          use mat_elem_mod, only : MAT_ELEM_
          use elbufdef_mod
          use stack_mod, only : STACK_PLY
          USE MULTI_FVM_MOD
          USE ALE_CONNECTIVITY_MOD, only : T_ALE_CONNECTIVITY
          USE CONNECTIVITY_MOD
          use glob_therm_mod, only : glob_therm_
          use output_mod, only : output_

          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_),intent(inout) :: python      !< the Fortran structure that holds the python function
          TYPE(connectivity_), INTENT(INOUT) :: ELEMENT
          integer,      intent(inout) :: numnod         !< the global number of nodes
          type(nodal_arrays_), intent(inout) :: nodes   !< the nodal arrays
          TYPE(MAT_ELEM_), INTENT(INOUT) :: MAT_ELEM
          type (glob_therm_) ,intent(inout)                  :: glob_therm
          type (output_) ,intent(inout)                  :: output
          integer, intent(in) :: nixs                !< number of integers in the solid data structure
          integer, intent(in) :: numels              !< number of solids
          integer, intent(inout) :: ixs(nixs,numels)    !< solid data structure
          integer, intent(in) :: nixc                !< number of integers in the shell data structure
          integer, intent(in) :: numelc              !< number of shells
          integer, intent(in) :: nixp                !< number of integers in the beam data structure
          integer, intent(in) :: numelp              !< number of beams
          integer, intent(inout) :: ixp(nixp,numelp)    !< beam data structure
          integer, intent(in) :: nixt                !< number of integers in the truss data structure
          integer, intent(in) :: numelt              !< number of trusses
          integer, intent(in) :: ixt(nixt,numelt)    !< truss data structure
          integer, intent(in) :: nixtg               !< number of integers in the triangle data structure
          integer, intent(in) :: numeltg             !< number of triangles
          integer, intent(inout) :: ixtg(nixtg,numeltg) !< triangle data structure
          integer, intent(in) :: nixr                !< number of integers in the spring data structure
          integer, intent(in) :: numelr              !< number of springs
          integer, intent(in) :: ixr(nixr,numelr)    !< spring data structure
          integer, intent(in) :: nixq                !< number of integers in the quad data structure
          integer, intent(in) :: numelq              !< number of quads
          integer, intent(inout) :: ixq(nixq,numelq)    !< quad data structure
          integer, intent(in) :: ngroup              !< number of groups
          integer, intent(in) :: nparg               !< number of integers in the group data structure
          integer, intent(inout) :: iparg(nparg,ngroup) !< group data structure
          integer, intent(in) :: mvsiz               !< maximum size of a group
          integer, intent(in) :: lipart1             !< index in ipart array
          integer, intent(in) :: npart               !< number of particles
          integer, intent(in) :: nthpart             !< number of thermal particles
          integer, intent(in) :: ispmd               !< flag for smoothed particle method
          integer, intent(in) :: n2d                 !< number of 2D elements
          integer, intent(in) :: numgeo              !< number of geometrical entities
          integer, intent(in) :: nummat              !< number of materials
          integer, intent(in) :: npropg              !< number of properties in geo array
          integer, intent(in) :: npropgi             !< number of integers in geo array
          integer, intent(in) :: npropm              !< number of properties in pm array
          integer, intent(in) :: npropmi             !< number of integers in pm array
          integer, intent(in) :: seani                                   
          integer, intent(in) :: M51_N0PHAS !< law 51 phases
          integer, intent(in) :: M51_nvphas !< law 51 phases
          integer, intent(in) :: sthke               !< size of thke array
          real(kind=WP), intent(inout) :: w(3,numnod) !< for ALE?
          real(kind=WP), intent(inout) :: thke(sthke) !< thickness of shell elements ?
          real(kind=WP), intent(inout) :: geo(npropg,numgeo) !< property array
          real(kind=WP), intent(inout) :: pm(npropm,nummat) !< property array
          real(kind=WP), intent(inout) :: err_thk_sh4(numelc) !< ?
          real(kind=WP), intent(inout) :: err_thk_sh3(numeltg) !< ?
          real(kind=WP), intent(inout) :: eani(seani) !< working array ?
          integer, intent(inout) :: ipart(*) !< part array
          integer, intent(inout) :: ipm(npropmi,nummat) !< property array
          integer, intent(inout) :: igeo(npropgi,numgeo) !< property array
          integer, intent(inout) :: snercvois 
          integer, intent(inout) :: snesdvois
          integer, intent(inout) :: slercvois
          integer, intent(inout) :: slesdvois
          integer, intent(inout) :: nercvois(snercvois) !< for Schlieren option
          integer, intent(inout) :: nesdvois(snesdvois) !< for Schlieren option
          integer, intent(inout) :: lercvois(slercvois) !< for Schlieren option
          integer, intent(inout) :: lesdvois(slesdvois) !< for Schlieren option
          type(ELBUF_STRUCT_), dimension(ngroup), intent(inout), target :: elbuf_tab !< element buffer structure
          type(STACK_PLY), intent(inout) :: stack !< tack structure
          type(MULTI_FVM_STRUCT), intent(in) :: multi_fvm !< finite volume structure
          real(kind=WP), intent(in) :: bufmat(*) !< buffer material ?
          type(T_ALE_CONNECTIVITY), intent(in) :: ALE_CONNECTIVITY

!
!

! ------------------------------------------------------------------------------------------------------
          integer :: K1,K2,K3,K4,K5,K6,K7,K8,K9
              CALL PYTHON_REGISTER(PYTHON,NODES,NUMNOD, &
                                   IXS, NIXS, NUMELS, &
                                   ELEMENT%SHELL%IXC, NIXC, NUMELC, &
                                   IXP, NIXP, NUMELP, &
                                   IXT, NIXT, NUMELT, &
                                   IXQ, NIXQ, NUMELQ, &
                                   IXTG, NIXTG, NUMELTG, &
                                   IXR, NIXR, NUMELR, &
                                   IPARG, NGROUP, NPARG, MVSIZ)

             IF(PYTHON%NB_FUNCTS > 0) CALL PYTHON_SHARE_MEMORY(PYTHON,NODES,NUMNOD, &
                                   IXS, NIXS, NUMELS, &
                                   ELEMENT%SHELL%IXC, NIXC, NUMELC, &
                                   IXP, NIXP, NUMELP, &
                                   IXT, NIXT, NUMELT, &
                                   IXQ, NIXQ, NUMELQ, &
                                   IXTG, NIXTG, NUMELTG, &
                                   IXR, NIXR, NUMELR, &
                                   IPARG, NGROUP, NPARG)

              K1=1+LIPART1*(NPART+NTHPART)+2*9*(NPART+NTHPART)
              K2=K1+NUMELS
              K3=K2+NUMELQ
              K4=K3+NUMELC
              K5=K4+NUMELT
              K6=K5+NUMELP
              K7=K6+NUMELR
              K8=K7
              K9=K8+NUMELTG
              CALL python_dummy_active_node(PYTHON)
              CALL FUNCT_PYTHON_UPDATE_ELEMENTS(PYTHON, ISPMD, &
                       N2D,  NGROUP,  NIXC, NIXTG, NIXS,NIXQ, &
                       NUMGEO, NUMELC, NUMELTG, NUMELS, NUMELQ, NUMMAT, NUMNOD, &
                       NPARG, NPROPG, NPROPM, NPROPMI, NPROPGI, &
                       SNERCVOIS, SNESDVOIS, SLERCVOIS, SLESDVOIS, &
                       STHKE, SEANI, NPART, &
                       ELBUF_TAB   ,IPARG       ,GEO        , &
                       ELEMENT%SHELL%IXC ,IXTG, IXS, IXQ, PM,BUFMAT, &
                       EANI, &
                       IPM         ,IGEO      ,THKE      ,ERR_THK_SH4 ,ERR_THK_SH3, &
                       NODES                  ,W           ,ALE_CONNECTIVITY,  &
                       NERCVOIS    ,NESDVOIS  ,LERCVOIS    ,LESDVOIS, &
                       M51_N0PHAS, M51_NVPHAS, STACK,& 
                       IPART(K3:K4-1),IPART(K1:K2-1),IPART(K8:K9-1), IPART(K2:K3-1), &
                       MULTI_FVM , &
                       MAT_ELEM%MAT_PARAM , OUTPUT%DATA%FANI_CELL,GLOB_THERM%ITHERM)

        end subroutine resol_alloc_python
      end module resol_alloc_mod

