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
!||    hm_read_adinertia_mod   ../starter/source/tools/adinertia/hm_read_adinertia.F90
!||--- called by ------------------------------------------------------
!||    lectur                  ../starter/source/starter/lectur.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      module hm_read_adinertia_mod
        use precision_mod, only : WP
        implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \details Read the added inertia data defined by /ADINERTIA/
!||====================================================================
!||    hm_read_adinertia          ../starter/source/tools/adinertia/hm_read_adinertia.F90
!||--- called by ------------------------------------------------------
!||    lectur                     ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                     ../starter/source/output/message/message.F
!||    hm_get_float_array_index   ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||    hm_get_floatv              ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_int_array_index     ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||    hm_get_intv                ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_read_key         ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start            ../starter/source/devtools/hm_reader/hm_option_start.F
!||    usr2sys                    ../starter/source/system/sysfus.F
!||--- uses       -----------------------------------------------------
!||    adinertia_mod              ../starter/share/modules1/adinertia_mod.F90
!||    hm_option_read_mod         ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod                ../starter/share/message_module/message_mod.F
!||    r2r_mod                    ../starter/share/modules1/r2r_mod.F
!||    submodel_mod               ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_adinertia(ms        ,in        ,itabm1    ,igrnod   ,unitab  ,   &
                                  &  nbaddiner ,adinertia ,totaddmas ,lsubmodel,iddlevel,   &
                                  &  ipid      ,nsubdom   ,ngrnod    ,npart    ,totaddiner, &
                                  &  numnod    )
! ----------------------------------------------------------------------------------------------------------------------
!                                                      modules
! ----------------------------------------------------------------------------------------------------------------------
          use unitab_mod
          use r2r_mod
          use message_mod
          use groupdef_mod
          use submodel_mod
          use hm_option_read_mod
          use adinertia_mod, only : adiner_,node_
          use precision_mod, only : WP
          use names_and_titles_mod, only : nchartitle
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                                   intent(in) :: numnod                       !< Total number of nodes
          integer,                                   intent(in) :: itabm1(numnod)               !< Mapping table from user node ID to system node ID
          integer,                                   intent(in) :: nbaddiner                    !< Number of added inertia definitions
          integer,                                   intent(in) :: iddlevel                     !< Level of passing (0: first pass, 1: second pass)
          integer,                                   intent(in) :: ipid
          integer,                                   intent(in) :: nsubdom                      !< Number of subdomains
          integer,                                   intent(in) :: ngrnod                       !< Number of node groups
          integer,                                   intent(in) :: npart                        !< Total number of parts
          real(kind=WP),                             intent(inout) :: ms(numnod)                !< Nodal mass array to be updated with added mass
          real(kind=WP),                             intent(inout) :: in(numnod)                !< Nodal inertia array to be updated with added inertia
          real(kind=WP),                             intent(inout) :: totaddmas                 !< Total added mass (to be updated)
          real(kind=WP),                             intent(inout) :: totaddiner                !< Total added inertia (to be updated)

          type (group_)        ,dimension(ngrnod)   ,intent(in) :: igrnod                       !< Node groups data
          type (unit_type_)                         ,intent(in) :: unitab                       !< Unit table for unit conversion
          type(submodel_data)  ,dimension(nsubdom)  ,intent(in) :: lsubmodel                    !< Submodel data array for submodel-specific options reading
          type (adiner_)       ,dimension(nbaddiner),intent(inout) :: adinertia                 !< Added inertia definitions data (to be updated with node IDs and inertia/mass values)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i, j, igr, igrs, nosys, nnod, jcurr , cpt_last                !< local entities id's and counters
          integer :: id, uid, itype, entitymax                                     !< local entities id's and counters
          real(kind=WP) :: amas, adiner                                            !< local inertia/mass values
          real(kind=WP) :: compIxx, compIxy, compIxz, compIyy, compIyz, compIzz    !< local inertia components for type 0
          logical :: is_available                                                  !< flag for option availability
          logical :: loop_2                                                        !< flag for second loop in optimization of group search
          character(len=nchartitle) :: title, mess                                 !< local strings
          integer, dimension(:), allocatable :: entity_multi, iflag_multi          !< local arrays for type 5
          real(kind=WP), dimension(:), allocatable :: amas_multi,      &           !< local inertia/mass arrays for type 5
                                  &   ixx_multi, ixy_multi, ixz_multi, &
                                  &   iyy_multi, iyz_multi, izz_multi
! ----------------------------------------------------------------------------------------------------------------------
!                                                    E x t e r n a l   F u n c t i o n s
! ----------------------------------------------------------------------------------------------------------------------
          integer :: usr2sys
          data mess/'ADDED INERTIA DEFINITION                '/   
!=======================================================================
!         adinertia(ig)%id    : INERTIA IDENTIFIER
!         adinertia(ig)%title : INERTIA title
!         adinertia(ig)%nbnod : NUMBER of nodes to add INERTIA
!         adinertia(ig)%type  : INERTIA type
!                                   = 0 -> Inertia is added to each node of node group
!                                   = 5 -> Inertia is added to each single node
!         adinertia(ig)%nodeid : NODE_ID to add INERTIA
!         adinertia(ig)%node(inod)%inertia : INERTIA added to NODES
!         adinertia(ig)%node(inod)%mass : MASS added to NODES
!=======================================================================
!
          is_available = .false.
!
!--------------------------------------------------
          call hm_option_start('/ADINERTIA')
!--------------------------------------------------
!         browsing model ADINERTIA 1-> NBADDINER
!--------------------------------------------------
!         for optimisation 1 (last group is memorised)
          jcurr = 1
!
          do i=1,nbaddiner
            title = ''
!--------------i-----------------------------------
!         extractCT data of /ADINERTIA/...
!--------------------------------------------------
            call hm_option_read_key(lsubmodel,          &
                                &   option_id   = id,   &
                                &   unit_id     = uid,  &
                                &   option_titr = title)
!
            call hm_get_intv('type'   ,itype  ,is_available,lsubmodel)
!------
            adinertia(i)%title = title
            adinertia(i)%id = id
            adinertia(i)%type = itype
!------
            if (itype == 0) then
!------
!------
!           added mass to nodes of grnod
!------
              call hm_get_floatv('IXX' ,compIxx ,is_available ,lsubmodel ,unitab)
              call hm_get_floatv('IXY' ,compIxy ,is_available ,lsubmodel ,unitab)
              call hm_get_floatv('IXZ' ,compIxz ,is_available ,lsubmodel ,unitab)
              call hm_get_floatv('IYY' ,compIyy ,is_available ,lsubmodel ,unitab)
              call hm_get_floatv('IYZ' ,compIyz ,is_available ,lsubmodel ,unitab)
              call hm_get_floatv('IZZ' ,compIzz ,is_available ,lsubmodel ,unitab)

              call hm_get_floatv('Mass'  ,amas ,is_available ,lsubmodel ,unitab)
              call hm_get_intv('grnd_ID' ,igr  ,is_available ,lsubmodel)
!
              ! For deformable bodies, the only one value of Inertia tensor is needed, IXX. 
              ! For rigid bodies (not yet implemented), the 6 components of inertia tensor are added to each node of the group.
              adiner = compIxx
!
                if (amas < 0.0_wp) then
                  call ancmsg(msgid=3147,            &
                          &   msgtype=msgwarning,    &
                          &   anmode=aninfo_blind_1, &
                          &   i1=id,                 &
                          &   c1=title,              &
                          &   r1=amas)
                endif
!
                if (adiner < 0.0_wp) then
                  call ancmsg(msgid=3148,            &
                          &   msgtype=msgwarning,    &
                          &   anmode=aninfo_blind_1, &
                          &   i1=id,                 &
                          &   c1=title,              &
                          &   r1=adiner)
                endif
!
              if (igr == 0) then
                call ancmsg(msgid=668,        &
                        &   msgtype=msgerror, &
                        &   anmode=aninfo,    &
                        &   c1='/ADINERTIA',  &
                        &   c2='/ADINERTIA',  &
                        &   c3=title,         &
                        &   i1=id)
              endif
!
              igrs=0

!             original
!             do j=1,ngrnod
!               if (igr == igrou(1,j)) then
!                 igrs=j
!                 goto 100
!               endif
!             enddo
!             end original
!-----------------------
!             OPTIMISATION1
!-----------------------
!             optimisation to avoid quadratic loop
!             if group is found, next search start from this group
!             optimal in case of sorted list of GRNOD in ADMAS file
!             in case on non sorted file, a dichotomic search is more appropriate

              cpt_last = ngrnod
              loop_2 = .false.
110       continue
              do j = jcurr,cpt_last
                if (igr == igrnod(j)%id) then
                  igrs = j
                  jcurr = j
!           group found
                   goto 100
                endif
                if (j == ngrnod) then 
                  if(loop_2)then
!           second passage IGRS has not been found we output in error 
                    goto 100
                  else
!           first passage in loop, we will start a 2nd passage from 1 to jcurr
                    loop_2 = .true.
                  endif
                  cpt_last = jcurr
                  jcurr = 1
!           begin again loop 1
                  goto 110
                endif
              enddo ! do j = jcurr,cpt_last
!-----------------------
!           end OPTIMISATION1
!-----------------------
100       continue
!
              if (igrs /= 0) then
                nnod = igrnod(igrs)%nentity
                if (.not.allocated(adinertia(i)%nodeid) .and. iddlevel==0) ALLOCATE(adinertia(i)%nodeid(nnod))
                if (.not.allocated(adinertia(i)%node) .and. iddlevel==0)   ALLOCATE(adinertia(i)%node(nnod))
                do j=1,nnod
                  nosys=igrnod(igrs)%entity(j)
!-----------Multidomains: common nodes are only processed on 1 domain--------------
                  if ((nsubdom > 0).and.(ipid == 0)) then
                    if (tagno(npart+nosys) > 1) goto 150
                  endif
                  ms(nosys) = ms(nosys) + amas
                  totaddmas = totaddmas + amas
                  !! compute of inertia for each node of the group (only IXX is kept for the moment)
                  in(nosys) = in(nosys) + adiner
                  totaddiner = totaddiner + adiner
                  ! inertia/mass storage
                  adinertia(i)%nbnod = nnod
                  adinertia(i)%nodeid(j) = nosys
                  adinertia(i)%node(j)%inertia = adiner
                  adinertia(i)%node(j)%mass = amas
 150      continue
                enddo ! j=1,nnod
              else
                call ancmsg(msgid=53,                  &
                        &   msgtype=msgerror,          &
                        &   anmode=aninfo,             &
                        &   c1='IN /ADINERTIA OPTION', &
                        &   i1=igr)
              endif ! if (igr /= 0)
!------
            elseif (itype == 5) then
!------
!         added mass to nodes
!------
              call hm_get_intv('entityidsmax'   ,entitymax ,is_available ,lsubmodel)
!
              allocate(ixx_multi(entitymax))
              ixx_multi(1:entitymax) = 0.0_WP
              allocate(ixy_multi(entitymax))
              ixy_multi(1:entitymax) = 0.0_WP
              allocate(ixz_multi(entitymax))
              ixz_multi(1:entitymax) = 0.0_WP
              allocate(iyy_multi(entitymax))
              iyy_multi(1:entitymax) = 0.0_WP
              allocate(iyz_multi(entitymax))
              iyz_multi(1:entitymax) = 0.0_WP
              allocate(izz_multi(entitymax))
              izz_multi(1:entitymax) = 0.0_WP
              allocate(amas_multi(entitymax))
              amas_multi(1:entitymax) = 0.0_WP
              allocate(entity_multi(entitymax))
              entity_multi(1:entitymax) = 0
!
              if (.not.allocated(adinertia(i)%nodeid) .and. iddlevel==0)  allocate(adinertia(i)%nodeid(entitymax))
              if (.not.allocated(adinertia(i)%node) .and. iddlevel==0)    allocate(adinertia(i)%node(entitymax))
!
              do j=1,entitymax
                call hm_get_float_array_index('IXX'  ,ixx_multi(j)   ,j ,is_available, lsubmodel, unitab)
                call hm_get_float_array_index('IXY'  ,ixy_multi(j)   ,j ,is_available, lsubmodel, unitab)
                call hm_get_float_array_index('IXZ'  ,ixz_multi(j)   ,j ,is_available, lsubmodel, unitab)
                call hm_get_float_array_index('IYY'  ,iyy_multi(j)   ,j ,is_available, lsubmodel, unitab)
                call hm_get_float_array_index('IYZ'  ,iyz_multi(j)   ,j ,is_available, lsubmodel, unitab)
                call hm_get_float_array_index('IZZ'  ,izz_multi(j)   ,j ,is_available, lsubmodel, unitab)
                call hm_get_float_array_index('Mass'  ,amas_multi(j)   ,j ,is_available, lsubmodel, unitab)
                call hm_get_int_array_index('node_ID' ,entity_multi(j) ,j ,is_available, lsubmodel)
!
                if (amas_multi(j) < 0.0_wp) then
                  call ancmsg(msgid=3147,            &
                          &   msgtype=msgwarning,    &
                          &   anmode=aninfo_blind_1, &
                          &   i1=id,                 &
                          &   c1=title,              &
                          &   r1=amas_multi(j))
                endif
!
                if (ixx_multi(j) < 0.0_wp) then
                  call ancmsg(msgid=3148,            &
                          &   msgtype=msgwarning,    &
                          &   anmode=aninfo_blind_1, &
                          &   i1=id,                 &
                          &   c1=title,              &
                          &   r1=ixx_multi(j))
                endif
!
                if (entity_multi(j) <=  0) then
                call ancmsg(msgid=3149,        &
                        &   msgtype=msgerror,  &
                        &   anmode=aninfo,     &
                        &   i1=id,             &
                        &   c1=title,          &
                        &   i2=entity_multi(j))
                endif
                nosys = usr2sys(entity_multi(j),itabm1,mess,id)
!-----------multidomains: common nodes are only processed in one domain--------------
                if ((nsubdom > 0) .and. (ipid == 0)) then
                  if (tagno(npart+nosys) > 1) goto 170
                endif
                ms(nosys) = ms(nosys) + amas_multi(j)
                totaddmas = totaddmas + amas_multi(j)
                !! compute of inertia for each node of the group (only ixx is kept for the moment)
                in(nosys) = in(nosys) + ixx_multi(j)
                totaddiner = totaddiner + ixx_multi(j)
                ! inertia/mass storage
                adinertia(i)%nbnod = entitymax
                adinertia(i)%nodeid(j) = entity_multi(j)
                adinertia(i)%node(j)%inertia = ixx_multi(j)
                adinertia(i)%node(j)%mass = amas_multi(j)
 170      continue
              enddo ! j=1,entitymax
!
              if (allocated(amas_multi)) deallocate(amas_multi)
              if (allocated(ixx_multi)) deallocate(ixx_multi)
              if (allocated(ixy_multi)) deallocate(ixy_multi)
              if (allocated(ixz_multi)) deallocate(ixz_multi)
              if (allocated(iyy_multi)) deallocate(iyy_multi)
              if (allocated(iyz_multi)) deallocate(iyz_multi)
              if (allocated(izz_multi)) deallocate(izz_multi)
              if (allocated(entity_multi)) deallocate(entity_multi)
!------
            endif ! if (itype == 0 .or. itype == 1)
!------
          enddo ! do i=1,nbaddiner
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine hm_read_adinertia
      end module hm_read_adinertia_mod
