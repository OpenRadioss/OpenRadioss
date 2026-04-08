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
      module rbody_part_modif_mod
      contains
! ======================================================================================================================
! \brief for rbody by part(rbody_id=irb) merging rbody (rbody_id<irb) by adding hierarchy 
! \detail check if slave nodes of rbody_part are also defined as slave nodes of previous rbodies
!         if yes : remove all these nodes and add their main_id inplace
! ======================================================================================================================
        subroutine rbody_part_modif(nrbykin ,nnpby ,npby  ,slpby ,lpby  ,            &
                                    numnod  ,irb   ,nsn   ,isl   ,nrbykin0,          &
                                    parent_of)
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use my_alloc_mod
          use precision_mod, only : WP
          use message_mod
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                    Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)                                      :: numnod          !< number of nodes
          integer, intent(in)                                      :: nrbykin         !< number of rbody
          integer, intent(in)                                      :: irb             !< rbody_id
          integer, intent(in)                                      :: nnpby           !< first dimension of npby
          integer, intent(in)                                      :: slpby           !< dimesion of lpby
          integer, dimension(nnpby,nrbykin),    intent(inout)      :: npby            !< rbody data
          integer, dimension(slpby),            intent(inout)      :: lpby            !< rbody secondary node data
          integer, intent(inout)                                   :: nsn             !< number of secondary nodes
          integer, dimension(nsn),              intent(inout)      :: isl             !< secondary node list data
          integer, intent(in)                                      :: nrbykin0        !< initial number of rbody
          integer, dimension(nrbykin),          intent(inout)      :: parent_of       !< parent of rbody
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,k,m,iad,iadj,ns,nsnj,nsn_i,id,id_j,j_end
          logical :: changed
          integer, dimension(nrbykin) :: itagm
          integer, dimension(nsn)     :: isl_copy
          integer, dimension(:), allocatable :: itag
! ======================================================================================================================
          call my_alloc(itag,numnod)
          itag = 0
          do i=1,nsn
              ns = isl(i)
              itag(ns) = 1
          enddo
          itagm(1:nrbykin) = 0
          isl_copy = isl
          changed = .false.
          j_end = irb-1
          do j=1,j_end      
            iadj = npby(11,j)
            nsnj = npby(2,j)
            do k=1,nsnj
              ns = lpby(iadj+k)
              if (itag(ns)>0) then
                itag(ns)=-itag(ns)
                itagm(j) = 1
                changed = .true.
              end if
            end do
          end do
          if (changed) then 
            id = npby(6,irb)
            nsn_i = 0
            do i=1,nsn
              ns = isl(i)
              if (itag(ns)>0) then  ! remove common nodes
                nsn_i = nsn_i + 1
                isl_copy(nsn_i) = ns
              end if
            end do 
! first to check between rbody_parts to avoid adding double main_id nodes          
            do j=1+nrbykin0,j_end
              if (itagm(j)==1) then
                nsn_i = nsn_i + 1
                isl_copy(nsn_i) = npby(1,j)
                id_j = npby(6,j)
                if (itagm(parent_of(j))==1) itagm(parent_of(j))=0
                parent_of(irb) = j
                call ancmsg(msgid=3142,                      &
                            msgtype=msgwarning,              &
                            anmode=aninfo_blind_1,           &
                            i1=id_j,                         &
                            i2=id ) 
              end if
            end do
            do j=1,nrbykin0      ! add main_id of rbodies not by part
              if (itagm(j)==1) then
                nsn_i = nsn_i + 1
                isl_copy(nsn_i) = npby(1,j)
                id_j = npby(6,j)
                if (parent_of(irb) == 0) parent_of(irb) = j
                  call ancmsg(msgid=3141,                    &
                            msgtype=msgwarning,              &
                            anmode=aninfo_blind_1,           &
                            i1=id_j,                         &
                            i2=id ) 
              end if
            end do
            nsn = nsn_i
            isl(1:nsn) = isl_copy(1:nsn)
          end if !(changed) then
        deallocate(itag)
        end subroutine rbody_part_modif 
! ======================================================================================================================
! \brief check rbody by part with bcs(boundary conditions),impvel,gravity,inivel
! ======================================================================================================================
        subroutine rbody_part_check(nrbykin ,nnpby ,npby  ,slpby ,lpby   ,       &
                                    numnod  ,itab  ,npart ,ipart ,lipart1,       &
                                    icode   ,iskew ,nfxvel,nifv  ,ibfv   ,       &
                                    ngrav   ,nigrav,igrav ,slgrav,lgrav  )

! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use my_alloc_mod
          use precision_mod, only : WP
          use message_mod
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                    Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)                                      :: numnod          !< number of nodes
          integer, intent(in)                                      :: nrbykin         !< number of rbody
          integer, intent(in)                                      :: nnpby           !< first dimension of npby
          integer, intent(in)                                      :: slpby           !< dimesion of lpby
          integer, dimension(nnpby,nrbykin),    intent(inout)      :: npby            !< rbody data
          integer, dimension(slpby),            intent(inout)      :: lpby            !< rbody secondary node data
          integer, intent(in)                                      :: npart           !< number of parts
          integer, intent(in)                                      :: lipart1         !< first dimension of ipart
          integer,dimension(lipart1,npart),     intent(in)         :: ipart
          integer, dimension(numnod),           intent(inout)      :: icode           !< coded nodes data (boundary conditions)
          integer, dimension(numnod),           intent(inout)      :: iskew           !< local skew data
          integer, dimension(numnod),           intent(in   )      :: itab            !< noed user id
          integer, intent(in)                                      :: nfxvel          !< number of imposed velocities
          integer, intent(in)                                      :: nifv            !< first dimesion of ibfv
          integer, dimension(nifv,nfxvel),      intent(inout)      :: ibfv            !< imposed velocities/dips/acc
          integer, intent(in)                                      :: nigrav          !< first dimension of grav
          integer, intent(in)                                      :: ngrav           !< number of gravities
          integer, intent(in)                                      :: slgrav          !< first dimension of lgrav
          integer, dimension(nigrav,ngrav),     intent(inout)      :: igrav           !< gravities data
          integer, dimension(slgrav),           intent(inout)      :: lgrav           !< gravities node list
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,k,m,iad,ns,jpart,nsl,part_id
          integer, dimension(:), allocatable :: itag
! ======================================================================================================================
!  for BSC remove 2nd nodes & add main_id (tra->rot)
!  for impvel, do like itetra10=2 using ibfv(3,) to replace & remove
!  for gravity add main_id, change num inside HM_PREREAD_GRAV
!  for inivel,add main_id if his 2nd nodes inside 
! ======================================================================================================================
          call my_alloc(itag,numnod)
          itag = 0
          do i=1,nrbykin
            jpart = npby(21,i)
            if (jpart==0) cycle
            m = npby(1,i)
            iad = npby(11,i)
            nsl = npby(2,i)
            part_id = ipart(4,jpart)
            itag(lpby(iad+1:iad+nsl)) = 1
            call rpart_bcs_check(m,nsl,lpby(iad+1:iad+nsl),icode,iskew,numnod,itab,part_id)
            call rpart_fv_check(m,itag,nfxvel,nifv,ibfv,numnod,itab,part_id)
            call rpart_grav_check(m,itag,ngrav,nigrav,igrav,slgrav,lgrav,numnod,itab,part_id)
            itag(lpby(iad+1:iad+nsl)) = 0
          end do
          deallocate(itag)
        end subroutine rbody_part_check 
! ======================================================================================================================
! \brief check rbody by part with bcs(boundary conditions),replace slave nodes by main_id if needed
! ======================================================================================================================
        subroutine rpart_bcs_check(m ,nsl ,isl  ,icode ,iskew ,numnod,itab,part_id)
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use message_mod
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                    Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! -------------------------i---------------------------------------------------------------------------------------------
          integer, intent(in)                                      :: numnod          !< number of nodes
          integer, intent(in)                                      :: m               !< main node id
          integer, intent(in)                                      :: nsl             !< number of secondary nodes
          integer, dimension(nsl),              intent(inout)      :: isl             !< secondary node list data
          integer, dimension(numnod),           intent(inout)      :: icode           !< coded nodes data (boundary conditions)
          integer, dimension(numnod),           intent(inout)      :: iskew           !< local skew data
          integer, dimension(numnod),           intent(in)         :: itab            !< node user id 
          integer, intent(in)                                      :: part_id         !< part id
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,k,ns,jt(3),jr(3),ic,ict,icr,ict_m,icr_m,isk,isk_m,ic_m,jt_m(3),jr_m(3)
! ======================================================================================================================
          jt_m(1:3)=0
          jr_m(1:3)=0
          isk_m = iskew(isl(1))
          do i=1,nsl
            ns = isl(i)
            ic = icode(ns)
            if (ic==0) cycle
            ict = ic/512  !tra_x*4 +tra_y*2 +tra_z
            icr = (ic-512*(ict))/64
            isk = iskew(ns)
            if (isk_m/=isk) then
              call ancmsg(msgid=3143,                      &
                          msgtype=msgerror,                &
                          anmode=aninfo_blind_1,           &
                          i1=part_id,                      &
                          i2=itab(ns),                     &
                          i3=itab(isl(1)) ) 
            end if
            if (jt_m(1)+jt_m(2)+jt_m(3)<3) then
              jt(1) = ict/4
              jt(2) = (ict-4*jt(1))/2
              jt(3) = ict-4*jt(1)-2*jt(2)
              if ((jt(1)+jt_m(1))>2) jr_m(2:3) = 1
              if ((jt(2)+jt_m(2))>2) then 
                jr_m(1) = 1
                jr_m(3) = 1
              end if
              if ((jt(3)+jt_m(3))>2) jr_m(1:2) = 1
              jt_m(1:3) = max(jt_m(1:3),jt(1:3))
            end if
            if (jr_m(1)+jr_m(2)+jr_m(3)<3) then
              jr(1) = icr/4
              jr(2) = (icr-4*jr(1))/2
              jr(3) = icr-4*jr(1)-2*jr(2)
              jr_m(1:3) = max(jr_m(1:3),jr(1:3))
            end if
            icode(ns) = 0
          end do
          ict_m = jt_m(1)*4 + jt_m(2)*2 + jt_m(3)
          icr_m = jr_m(1)*4 + jr_m(2)*2 + jr_m(3)
          ic_m = ict_m*512 + icr_m*64
          if (ic_m>0) then 
             icode(m) = ic_m
             iskew(m) = isk_m
             call ancmsg(msgid=3137,                       &
                          msgtype=msgwarning,              &
                          anmode=aninfo_blind_1,           &
                          i1=part_id,                      &
                          i2=itab(m)) 
          end if

        end subroutine rpart_bcs_check 
! ======================================================================================================================
! \brief check rbody by part with impvel(/IMPVEL,IMP/DISP...),replace slave nodes by main_id if needed
! ======================================================================================================================
        subroutine rpart_fv_check(m ,itag ,nfxvel,nifv,ibfv,numnod,itab,part_id)
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use message_mod
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                    Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! -------------------------i---------------------------------------------------------------------------------------------
          integer, intent(in)                                      :: numnod          !< number of nodes
          integer, intent(in)                                      :: m               !< main node id
          integer, intent(in)                                      :: nfxvel          !< number of imposed velocities
          integer, intent(in)                                      :: nifv            !< first dimesion of ibfv
          integer, dimension(numnod),           intent(in)         :: itag            !< taged secondary node list
          integer, dimension(numnod),           intent(in)         :: itab            !< node user id 
          integer, intent(in)                                      :: part_id         !< part id
          integer, dimension(nifv,nfxvel),      intent(inout)      :: ibfv            !< imposed velocities
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,k,ns,jd(6),ifm,isk,ipr
! ======================================================================================================================
         jd(1:6)=0 ! tag for 6 dof   
         ipr = 0 
        do i=1,nfxvel
          ns = iabs(ibfv(1,i))
          if (itag(ns)==0) cycle
          j = ibfv(2,i)
          ifm   = ibfv(9,i)
          isk   = ibfv(2,i)/10
          if (ifm<=1) j=j-10*isk
          if (jd(j)==0) then
              ibfv(1,i) = m  ! replace by main_id
              if (j>3) ibfv(6,i) = 1 ! imposed rot on main_id
              jd(j) = 1
              ipr = 1
          else
              ibfv(3,i) = -iabs(ibfv(3,i)) ! remove this impvel
          end if
        end do 
          
        if (ipr>0) then 
             call ancmsg(msgid=3138,                       &
                          msgtype=msgwarning,              &
                          anmode=aninfo_blind_1,           &
                          i1=part_id,                      &
                          i2=itab(m)) 
        end if

        end subroutine rpart_fv_check 
! ======================================================================================================================
! \brief check rbody by part with /GRAV,add main_id if his 2nd nodes inside
! ======================================================================================================================
        subroutine rpart_grav_check(m ,itag ,ngrav,nigrav,igrav,slgrav,lgrav,numnod,itab,part_id)
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use message_mod
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                    Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! -------------------------i---------------------------------------------------------------------------------------------
          integer, intent(in)                                      :: numnod          !< number of nodes
          integer, intent(in)                                      :: m               !< main node id
          integer, intent(in)                                      :: ngrav           !< number of gravities
          integer, intent(in)                                      :: nigrav          !< first dimension of grav
          integer, intent(in)                                      :: slgrav          !< dimension of lgrav
          integer, dimension(numnod),           intent(in)         :: itag            !< taged secondary node list
          integer, dimension(numnod),           intent(in)         :: itab            !< node user id 
          integer, intent(in)                                      :: part_id         !< part id
          integer, dimension(nigrav,ngrav),     intent(inout)      :: igrav           !< gravities data
          integer, dimension(slgrav),           intent(inout)      :: lgrav           !< gravities node list
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,k,nsl,i_add,iad,iad_n,ns,ipr
! ======================================================================================================================
         ipr = 0 
        do i=1,ngrav
          nsl = igrav(1,i)
          iad = igrav(4,i)
          i_add = 0
          do j = 1,nsl
            ns = iabs(lgrav(iad+j-1))
            if (itag(ns)==1) then 
              i_add = 1
              exit
            end if
          end do
          if (i_add==1) then ! add main_id m in grav 
             ipr = 1
             igrav(1,i) = nsl + 1
             if (i/=ngrav) then ! shift next grav
                do k=i+1,ngrav
                   igrav(4,k) = igrav(4,k) + 1
                end do
                do k=iad + nsl+1,slgrav
                   lgrav(k) = lgrav(k-1)
                end do
             end if
             lgrav(iad + nsl) = m
          end if
        end do 
          
        if (ipr>0) then 
             call ancmsg(msgid=3139,                       &
                          msgtype=msgwarning,              &
                          anmode=aninfo_blind_1,           &
                          i1=part_id,                      &
                          i2=itab(m)) 
        end if

        end subroutine rpart_grav_check
! ======================================================================================================================
! \brief check rbody by part with /inivel, add main_id if his 2nd nodes inside
! ======================================================================================================================
        subroutine rpart_inivel_check(nrbykin ,nnpby ,npby  ,slpby    ,lpby   ,        &
                                    hm_ninvel ,numnod,itabm1,lsubmodel,unitab ,        &
                                    ntransf   ,nrtrans,rtrans,rby_iniaxis,iskwn,       &
                                    liskn    ,numskw ,lskew  ,skew   ,iframe  ,        &
                                    numfram  ,nxframe,xframe ,ngrnod ,igrnod  ,        &
                                    n2d      ,x      ,v     ,vr  )
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use unitab_mod
          use message_mod
          use submodel_mod
          use message_mod
          use groupdef_mod
          use my_alloc_mod
          use hm_option_read_mod 
          use names_and_titles_mod 
          use precision_mod,          only: WP
          use submodel_mod , only : submodel_data, nsubmod
          use constant_mod , only : zero, one
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                    Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! -------------------------i---------------------------------------------------------------------------------------------
!-----------------------------------------------
          integer, intent(in)                                      :: hm_ninvel       !< number of inivel
          integer, intent(in)                                      :: numnod          !< number of nodes
          integer, intent(in)                                      :: n2d             !< 2D analyse flag
          integer,                                   intent(in)    :: ntransf         !< first dimension of transformation array
          integer,                                   intent(in)    :: nrtrans         !< second dimension of transformation array
          integer , intent(in)                                     :: numskw          !< number of skew
          integer , intent(in)                                     :: numfram         !< number of frame
          integer , intent(in)                                     :: nxframe         !< 1er dimension of frame
          integer , intent(in)                                     :: lskew           !< 1er dimension of skew
          integer , intent(in)                                     :: liskn           !< 1er dimension of iskwn
          integer , intent(in)                                     :: ngrnod          !< number node group
          integer, intent(in)                                      :: nrbykin         !< number of rbody
          integer, intent(in)                                      :: nnpby           !< 1er dimension of npby
          integer, intent(in)                                      :: slpby           !< dimesion of lpby
          integer, dimension(nnpby,nrbykin),    intent(inout)      :: npby            !< rbody data
          integer, dimension(slpby),            intent(inout)      :: lpby            !< rbody secondary node data
          integer, dimension(2*numnod),         intent(in)         :: itabm1          !< node user id 
          integer , intent(in  ),dimension(liskn,numskw+1)         :: iskwn           !< iskew skew id data
          integer , intent(in  ),dimension(liskn,numfram+1)        :: iframe          !< iframe frame id data
          real(kind=WP), intent(in) ,dimension(3,numnod)           :: x               !< coordinate array
          real(kind=WP),dimension(3,numnod),     intent (inout)    :: v               !< velocity of nodes
          real(kind=WP),dimension(3,numnod),     intent (inout)    :: vr              !< rotational velocity of nodes
          real(kind=WP), intent(in) ,dimension(lskew,numskw+1)     :: skew            !< local skew data
          real(kind=WP), intent(in) ,dimension(nxframe,numfram+1)  :: xframe          !< frame data
          real(kind=WP), intent(in) ,dimension(ntransf,nrtrans)    :: rtrans          !< transformation storage array
          real(kind=WP), intent(inout) ,dimension(7,nrbykin)       :: rby_iniaxis     !< storage array for initial axis data modification
          type(submodel_data), dimension(nsubmod),intent(in)       :: lsubmodel
          type (unit_type_),                    intent(in)         :: unitab
          type (group_)  , dimension(ngrnod),   intent(in)         :: igrnod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer :: i,j,k,n,irb,kpri,nnod,nosys,itype,id,isk,igr,igrs,iad,nsl,part_id,m_id
      integer :: user_unit_id,sub_index,idir,sens_id,ninit,sensid
      integer :: nod_count,nodinivel,cpt,sub_id,itagm(nrbykin)
      integer :: ifra,ifm,iun,k1,k2,k3,inod,nb_nodes, id_node,iok
      real(kind=WP) :: vi(3),vri(3),vl(3),vrl(3),vra, ox, oy, oz, nixj(6)
      character mess*40
      character(len=nchartitle) :: titr
      character(len=ncharkey) :: key
      character(len=ncharfield) ::xyz
      character*16 :: label
      logical :: is_available, is_found
      real(kind=WP) :: tstart
      integer, dimension(:), allocatable :: itagns2rb
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External Functions
! ----------------------------------------------------------------------------------------------------------------------
          integer,external :: usr2sys
! ======================================================================================================================
! itagns2rb: map ns2rb_id of RBODY by part supoosing no ns in different activated rbodies
! for inivel w/ restart, to be done :do differently w/ module (adding n_add, i_add in each inivel_t)
! ======================================================================================================================
          call my_alloc(itagns2rb,numnod)
          itagns2rb = 0
          do i=1,nrbykin
            part_id = npby(21,i)
            if (part_id==0) cycle
            iad = npby(11,i)
            nsl = npby(2,i)
            itagns2rb(lpby(iad+1:iad+nsl)) = i 
          end do
      is_available = .false.
!--------------------------------------------------
      call hm_option_start('/inivel')
      i = 0 

      do cpt=1,hm_ninvel
        i = i + 1
        !---set cursor on next inivel option
        call hm_option_read_key(lsubmodel,option_id = id,unit_id = user_unit_id,submodel_index = sub_index,  &
                               submodel_id = sub_id,option_titr = titr,keyword2 = key)
        !---set itype depending on user keyword
        tstart = zero
        sens_id = 0
        label='/inivel'
        if(key(1:3)=='tra')then
          itype=0
          label='/inivel/tra'
        elseif(key(1:3)=='rot')then
          itype=1
          label='/inivel/rot'
        elseif(key(1:3)=='t+g')then
          itype=2
          label='/inivel/t+g'
          cycle
        elseif(key(1:3)=='gri')then
          itype=3
          label='/inivel/grid'
          cycle
        elseif(key(1:4)=='axis')then
          itype=4
          label='/inivel/axis'
        elseif(key(1:3) == 'fvm') then
          itype=5
          label='/inivel/fvm'
          cycle
        elseif(key(1:4)=='node')then
          itype=6
          label='/inivel/node'
        endif
        if (itype > 6) cycle
        itagm(1:nrbykin) = 0
        is_found =.false.
        if (itype <= 4) then
          call hm_get_floatv('tstart',tstart,is_available,lsubmodel,unitab)
          call hm_get_intv('sensor_id',sens_id,is_available,lsubmodel)
          if(tstart>zero .or. sens_id>0) cycle
          call hm_get_intv('entityid',igr,is_available,lsubmodel)
           igrs=0
           do j=1,ngrnod
              if(igr == igrnod(j)%id) igrs=j
           enddo
           if(igrs /= 0 )then
              do j=1,igrnod(igrs)%nentity
                 nosys=igrnod(igrs)%entity(j)
                 irb = itagns2rb(nosys)
                 if (irb>0) then 
                   itagm(irb) = 1
                   is_found = .true. 
                 end if
              end do
           endif
        elseif (itype == 6) then 
          call hm_get_intv('nb_nodes', nb_nodes, is_available, lsubmodel)
          do n=1,nb_nodes
            call hm_get_int_array_index('node', id_node, n, is_available, lsubmodel)
            nosys = usr2sys(id_node,itabm1,mess,id)
            irb = itagns2rb(nosys)
            if (irb>0) is_found = .true. 
          end do
        end if
        if (.not.is_found) cycle
       !---reader /inivel/node (6)
        if (itype == 6) then
          call hm_get_intv('nb_nodes', nb_nodes, is_available, lsubmodel)
          do n=1,nb_nodes
            call hm_get_int_array_index('node', id_node, n, is_available, lsubmodel)
            call hm_get_int_array_index('skewa', isk, n, is_available, lsubmodel)
            call hm_get_float_array_index('vxta', vi(1), n, is_available, lsubmodel, unitab)
            call hm_get_float_array_index('vyta', vi(2), n, is_available, lsubmodel, unitab)
            call hm_get_float_array_index('vzta', vi(3), n, is_available, lsubmodel, unitab)
            call hm_get_float_array_index('vxra', vri(1), n, is_available, lsubmodel, unitab)
            call hm_get_float_array_index('vyra', vri(2), n, is_available, lsubmodel, unitab)
            call hm_get_float_array_index('vzra', vri(3), n, is_available, lsubmodel, unitab)
            if(n2d /= 0 .and. isk == 0)then
                vi(1) = zero
                vri(2:3) = zero
            end if
            if (id_node == 0) cycle
            nosys = usr2sys(id_node,itabm1,mess,id)
            irb = itagns2rb(nosys)
            if (irb == 0) cycle
            m_id = npby(1,irb)
            if (m_id > 0) then
              if (isk > 0) then
                vl(1:3) = zero
                vrl(1:3) = zero
                do j=0,numskw
                  if (isk == iskwn(4,j+1)) then
                    isk=j+1
                    vl(1) = skew(1,isk)*vi(1)+skew(4,isk)*vi(2)+skew(7,isk)*vi(3)
                    vl(2) = skew(2,isk)*vi(1)+skew(5,isk)*vi(2)+skew(8,isk)*vi(3)
                    vl(3) = skew(3,isk)*vi(1)+skew(6,isk)*vi(2)+skew(9,isk)*vi(3)
                    vrl(1) = skew(1,isk)*vri(1)+skew(4,isk)*vri(2)+skew(7,isk)*vri(3)
                    vrl(2) = skew(2,isk)*vri(1)+skew(5,isk)*vri(2)+skew(8,isk)*vri(3)
                    vrl(3) = skew(3,isk)*vri(1)+skew(6,isk)*vri(2)+skew(9,isk)*vri(3)
                  end if
                end do
                 v(1:3,m_id)  = vl(1:3)
                 vr(1:3,m_id) = vrl(1:3)
              else 
                 v(1:3,m_id)  = vi(1:3)
                 vr(1:3,m_id) = vri(1:3)
              end if
              call ancmsg(msgid=3140,                      &
                          msgtype=msgwarning,              &
                          anmode=aninfo_blind_1,           &
                          i1=m_id,                         &
                          i2=id) 
            end if
          end do !n=1,nb_nodes
        elseif (itype == 4) then
            call hm_get_string('rad_dir',xyz,ncharfield,is_available)
            call hm_get_intv('inputsystem',ifra,is_available,lsubmodel)
            call hm_get_intv('entityid',igr,is_available,lsubmodel)

            call hm_get_floatv('vector_x',vi(1),is_available,lsubmodel,unitab)
            call hm_get_floatv('vector_y',vi(2),is_available,lsubmodel,unitab)
            call hm_get_floatv('vector_z',vi(3),is_available,lsubmodel,unitab)
            call hm_get_floatv('rad_rotational_velocity',vra,is_available,lsubmodel,unitab)
            if(n2d /= 0 .and. ifra == 0) vi(2:3) = zero
            if(ifra == 0 .and. sub_index  /=  0) call subrotvect(vi(1),vi(2),vi(3),rtrans,sub_id,lsubmodel)
            idir = 0
            if(xyz(1:1)=='x') then
              idir=1
            elseif(xyz(1:1)=='y') then
              idir=2
            elseif(xyz(1:1)=='z') then
              idir=3
            endif
            nixj = zero
            if (ifra > 0) then
              do k=1,numfram
                j=k+1
                if(ifra==iframe(4,j)) then
                  vl(1) = xframe(1,j)*vi(1)+xframe(4,j)*vi(2)+xframe(7,j)*vi(3)
                  vl(2) = xframe(2,j)*vi(1)+xframe(5,j)*vi(2)+xframe(8,j)*vi(3)
                  vl(3) = xframe(3,j)*vi(1)+xframe(6,j)*vi(2)+xframe(9,j)*vi(3)
                  exit
                endif
              enddo
              ifm = j
               k1=3*idir-2
               k2=3*idir-1
               k3=3*idir
               ox  = xframe(10,ifm)
               oy  = xframe(11,ifm)
               oz  = xframe(12,ifm)
               nixj(1)=xframe(k1,ifm)*(x(2,m_id)-oy)
               nixj(2)=xframe(k2,ifm)*(x(1,m_id)-ox)
               nixj(3)=xframe(k2,ifm)*(x(3,m_id)-oz)
               nixj(4)=xframe(k3,ifm)*(x(2,m_id)-oy)
               nixj(5)=xframe(k3,ifm)*(x(1,m_id)-ox)
               nixj(6)=xframe(k1,ifm)*(x(3,m_id)-oz)
               vrl(1)= vra*xframe(k1,ifm)
               vrl(2)= vra*xframe(k2,ifm)
               vrl(3)= vra*xframe(k3,ifm)
               vl(1)= vl(1)+vra*(nixj(3)-nixj(4))
               vl(2)= vl(2)+vra*(nixj(5)-nixj(6))
               vl(3)= vl(3)+vra*(nixj(1)-nixj(2))
            else
               if(idir==1) then
                  nixj(1)=x(2,m_id)
                  nixj(6)=x(3,m_id)
               elseif(idir==2) then
                  nixj(2)=x(1,m_id)
                  nixj(3)=x(3,m_id)
               elseif(idir==3) then
                  nixj(4)=x(2,m_id)
                  nixj(5)=x(1,m_id)
               endif
                  vrl(1:3)= zero !vra*xframe(k1,ifm)
                  vrl(idir)= vra
            endif
          do j=1,nrbykin
             if (itagm(j) == 1) then
               m_id = npby(1,j)
               v(1:3,m_id)= vl(1:3)
               vr(1:3,m_id)= vrl(1:3)
!-- /inivel/axis to be updated later in inirby.f due to position change of rbody main node
               rby_iniaxis(1,j) = one
               rby_iniaxis(2:4,j) = v(1:3,m_id)
               rby_iniaxis(5:7,j) = vr(1:3,m_id)
               call ancmsg(msgid=3140,                     &
                          msgtype=msgwarning,              &
                          anmode=aninfo_blind_1,           &
                          i1=id,                           &
                          i2=m_id,                         &
                          i3=npby(6,j)) 
             end if 
          end do
        else ! 0,1,2
          isk = 0
          call hm_get_intv('entityid',igr,is_available,lsubmodel)
          call hm_get_intv('inputsystem',isk,is_available,lsubmodel)
          if(isk == 0 .and. sub_index /= 0 ) isk = lsubmodel(sub_index)%skew
          call hm_get_floatv('vector_x',vi(1),is_available,lsubmodel,unitab)
          call hm_get_floatv('vector_y',vi(2),is_available,lsubmodel,unitab)
          call hm_get_floatv('vector_z',vi(3),is_available,lsubmodel,unitab)
          if(n2d /= 0 .and. isk == 0) vi(1:3) = zero
          if (isk > 0) then
              do j=0,numskw
                if (isk == iskwn(4,j+1)) then
                  isk=j+1
                  vl(1) = skew(1,isk)*vi(1)+skew(4,isk)*vi(2)+skew(7,isk)*vi(3)
                  vl(2) = skew(2,isk)*vi(1)+skew(5,isk)*vi(2)+skew(8,isk)*vi(3)
                  vl(3) = skew(3,isk)*vi(1)+skew(6,isk)*vi(2)+skew(9,isk)*vi(3)
                  exit
                endif
              enddo
          else
            vl(1:3) = vi(1:3)
          endif 
          do j=1,nrbykin
             if (itagm(j) == 1) then
               m_id = npby(1,j)
               if(itype == 0) then
                  v(1:3,m_id)=vl(1:3)
               elseif(itype == 1) then
                  vr(1:3,m_id)=vl(1:3)
               end if 
               call ancmsg(msgid=3140,                     &
                          msgtype=msgwarning,              &
                          anmode=aninfo_blind_1,           &
                          i1=id,                           &
                          i2=m_id,                         &
                          i3=npby(6,j)) 
             end if 
          end do
        end if !(itype == 6) then

      end do
      deallocate(itagns2rb)


        end subroutine rpart_inivel_check
      end module rbody_part_modif_mod
