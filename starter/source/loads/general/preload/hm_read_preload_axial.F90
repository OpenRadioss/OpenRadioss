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
      module hm_read_preload_axial_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine do the dimensioning of hm-reader of /PRELOAD/AXIAL
!=======================================================================================================================
        subroutine hm_pre_read_preload_axial(ngrspri,igrspring,npreload_a,lsubmodel)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
        use hm_option_read_mod    
        use submodel_mod
        use groupdef_mod
        use names_and_titles_mod
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
        integer, intent (in   )             :: ngrspri          !< number of spring elem group
        type (group_)  , dimension(ngrspri) :: igrspring        !< array of spring group  
        integer, intent (inout)             :: npreload_a       !< number /PRELOAD/AXIAL
        type(submodel_data) lsubmodel(*)                        !< submodel structure
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
        integer i,nld,iset,nn,id,uid,ifun,is,np
        character (len=nchartitle) :: titr
        character (len=ncharline)  :: key
!
        logical is_available 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------                   
!     
        is_available = .false.
!
        call hm_option_start('/PRELOAD')       
!
        nld  = npreload_a 
        np = 0                                  
        do i=1,nld
          ! read title, id and unit id
          titr = ''   
          call hm_option_read_key(lsubmodel,                                   &
                                 option_id      = id,                          & 
                                 unit_id        = uid,                         &
                                 option_titr    = titr,                        &
                                 keyword2       = key)
!
          if (key(1:len_trim(key))/='AXIAL') cycle

          call hm_get_intv('curveid' ,ifun  ,is_available,lsubmodel) 
          if (ifun ==0) cycle
          call hm_get_intv('set_id'  ,iset  ,is_available,lsubmodel)                                             
          
          nn = 0
          if (iset > 0) then
            do is=1,ngrspri
              if (iset==igrspring(is)%ID) then
                nn = 1
                exit
              endif
            enddo 
          endif
          if (nn>0) np = np + 1   
        enddo 
        npreload_a = np
!

        end subroutine hm_pre_read_preload_axial
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine do hm-reader of /PRELOAD/AXIAL
!=======================================================================================================================
        subroutine hm_read_preload_axial(                                     &
                   npreload_a,    ngrspri,    igrspring, itagprld_spring,     &
                   unitab    ,  lsubmodel,    preload_a, numelr         ,     &
                   snpc      ,  npc      ,    nfunct   , sensors        ,     &
                    ngrbeam  , igrbeam   ,    numelp   , itagprld_beam  ,     &
                   ngrtrus   , igrtruss  ,   numelt    , itagprld_truss ,     &
                   iout      )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
        use hm_option_read_mod    
        use submodel_mod
        use groupdef_mod
        use unitab_mod
        use message_mod
        use names_and_titles_mod
        use bpreload_mod
        use constant_mod, only : zero,one
        use sensor_mod
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"       
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
        integer, intent (in   )                         :: ngrspri          !< number of spring elem group
        type (group_)  , dimension(ngrspri)             :: igrspring        !< array of spring group
        integer, intent (in   )                         :: ngrbeam          !< number of beam elem group
        type (group_)  , dimension(ngrbeam)             :: igrbeam          !< array of beam group
        integer, intent (in   )                         :: ngrtrus          !< number of truss elem group
        type (group_)  , dimension(ngrtrus)             :: igrtruss         !< array of truss group
        integer, intent (in   )                         :: numelr           !< number spring element
        integer, intent (in   )                         :: numelp           !< number beam element
        integer, intent (in   )                         :: numelt           !< number truss element
        integer, intent (in   )                         :: nfunct           !< number of function
        integer, intent (in   )                         :: snpc             !< dimension of npc
        integer, intent (in   ) ,dimension(snpc)        :: npc              !< index pointer of function
        integer, intent (inout) ,dimension(numelr)      :: itagprld_spring  !< tag spring element using /PRELOAD
        integer, intent (inout) ,dimension(numelp)      :: itagprld_beam    !< tag beam element using /PRELOAD
        integer, intent (inout) ,dimension(numelt)      :: itagprld_truss   !< tag truss element using /PRELOAD
        integer, intent (inout)                         :: npreload_a       !< number of /PRELOAD/AXIAL
        integer, intent (in   )                         :: iout             !< id of out file
        type (prel1d_), target ,dimension(npreload_a)   :: preload_a        !< structrue data of /PRELOAD/AXIAL
        type (unit_type_),intent(in)                    :: unitab           !< structrue data of unity
        type (sensors_) ,intent(in)                     :: sensors          !< structrue data of sensor
        type(submodel_data) lsubmodel(*)                                    !< structrue data of submodel
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
        integer i,j,nld,iset,nn,id,uid,ifun,is,np,iflagunit,isens,itype
        character (len=nchartitle) :: titr
        character (len=ncharline)  :: key
        my_real loadval,damp
!
        logical is_available
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------                   
!
        is_available = .false.
        write(iout,2000)
!
        call hm_option_start('/PRELOAD/AXIAL')       
!
        do i=1,npreload_a
! read title, id and unit id 
          titr = ''   
          call hm_option_read_key(lsubmodel,                                   & 
                                  option_id      = id,                         & 
                                  unit_id        = uid,                        &
                                  option_titr    = titr)
!       
          iflagunit = 0 
!       
          do j=1,unitab%nunits                           
            if (unitab%unit_id(j) == uid) then
              iflagunit = 1
              exit
            endif
          enddo                                  
          if (uid/=0.and.iflagunit==0) then
            call ancmsg(msgid=659,anmode=aninfo,msgtype=msgerror,              &
                        i2=uid,i1=id,c1='bolt preload',                        &
                         c2='bolt preload_axial',                              &
                         c3=titr)                              
          endif                                    
        
          call hm_get_intv('set_id'  ,iset  ,is_available,lsubmodel)                                             
          call hm_get_intv('sens_id' ,isens ,is_available,lsubmodel)
          is = 0
          do j=1,sensors%nsensor
            if(sensors%sensor_tab(j)%sens_id==isens) is=j
          enddo
          if(is==0.and.isens/=0)then
            call ancmsg(msgid=521,anmode=aninfo,msgtype=msgerror,              &
                        i2=isens,i1=id,c1=titr) 
          endif
          preload_a(i)%sens_id = is
!          
          call hm_get_intv('curveid' ,ifun  ,is_available,lsubmodel)
          is = 0
          do j=1,nfunct
            if(npc(nfunct+j+1)==ifun) is=j
          enddo
          if(is==0)then
            call ancmsg(msgid=154,anmode=aninfo,msgtype=msgerror,              &
                        i2=ifun,i1=id,c1=titr) 
          endif
          preload_a(i)%fun_id  = is
!          
          call hm_get_floatv('Preload',loadval,is_available, lsubmodel, unitab)
          call hm_get_floatv('Damp',damp,is_available, lsubmodel, unitab)
!       
          if (loadval==zero) loadval = one
          preload_a(i)%preload = loadval
          preload_a(i)%damp    = Damp
!          
          nn = 0
          if (iset > 0) then
            do is=1,ngrspri
                if (iset==igrspring(is)%ID) then
                  nn = is
                  itype = 6
                  exit
                endif
            enddo 
          endif
          if (iset > 0 .and. nn==0) then
            do is=1,ngrbeam
                if (iset==igrbeam(is)%ID) then
                  nn = is
                  itype = 5
                  exit
                endif
            enddo 
          endif
          if (iset > 0 .and. nn==0) then
            do is=1,ngrtrus
                if (iset==igrtruss(is)%ID) then
                  nn = is
                  itype = 4
                  exit
                endif
            enddo 
          endif
          if (nn==0) then
            call ancmsg(msgid=3052,anmode=aninfo,msgtype=msgerror,              &
                        i2=iset,i1=id,c1=titr) 
          else
            select case (itype)
              case(4)
                 np=igrtruss(nn)%nentity 
                 do j=1,np
                   is = igrtruss(nn)%entity(j)      ! sys_id alread
                   if(is>0) itagprld_truss(is)=i
                 enddo
                 key ='TRUSS'
              case(5)
                 np=igrbeam(nn)%nentity 
                 do j=1,np
                   is = igrbeam(nn)%entity(j)      
                   if(is>0) itagprld_beam(is)=i
                 enddo
                 key ='BEAM'
              case(6)
                 np=igrspring(nn)%nentity 
                 do j=1,np
                   is = igrspring(nn)%entity(j)      
                   if(is>0) itagprld_spring(is)=i
                 enddo
                 key ='SPRING'
            end select 
          end if
!          
          write(iout,'(I10,1X,I10,4X,A6,1X,I10,1X,I10,2(1X,1PE10.3))')         &
                       id,iset,key,isens,ifun,loadval,Damp
        enddo 
!
 2000 FORMAT(//                                                                &
      '     BOLT 1D-ELEMENT PRELOADINGS  '/                                    &
      '     ----------------  '/                                               &
      '        ID     SET_ID   1D-ELEM    SENS_ID    FUNC_ID    PRELOAD       DAMP ')
!-----------
        end subroutine hm_read_preload_axial
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine initialize itag array for 1D-element which use /PRELOAD/AXIAL
!=======================================================================================================================
        subroutine initag_preload_a(nlist,ilist,ix,nix,nx,pl_id,itag)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
        use message_mod
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"       
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
        integer, intent (in   )                     :: nlist           !< number of elem in ilist
        integer, intent (in   ) , dimension(nlist)  :: ilist           !< element list
        integer, intent (in   )                     :: nix,nx          !< element connectivity dimensions
        integer, intent (in   ) , dimension(nix,nx) :: ix              !< element connectivity
        integer, intent (in   )                     :: pl_id           !< id of preload
        integer, intent (inout) , dimension(nx)     :: itag            !< tag element using preload
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
        integer ie, stat, ipl, ne, j 
        integer works(70000)
        integer, dimension(:), allocatable ::itris
        integer, dimension(:), allocatable ::indexs
        integer, dimension(:), allocatable ::ksysusrs
! ----------------------------------------------------------------------------------------------------------------------
!   e x t e r n a l   f u n c t i o n s
! ----------------------------------------------------------------------------------------------------------------------
        integer uel2sys
        external uel2sys
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------                   
        allocate (itris(nx) ,stat=stat)
        if (stat /= 0) then
          call ancmsg(msgid=268,anmode=aninfo,                                 &
                                   msgtype=msgerror,                           &
                              c1='preload_a_itris')                            
          return
        end if
        allocate (indexs(2*nx) ,stat=stat)
        if (stat /= 0) then
          call ancmsg(msgid=268,anmode=aninfo,                                 &
                                   msgtype=msgerror,                           &
                              c1='preload_a_indexs')                        
          return
        end if
        allocate (ksysusrs(2*nx),stat=stat)
        if (stat /= 0) then
          call ancmsg(msgid=268,anmode=aninfo,                                 &
                                   msgtype=msgerror,                           &
                              c1='preload_a_ksysusrs')
          return
        end if
        itris = 0
        indexs = 0
        ksysusrs = 0
!       
        do ie = 1, nx
          itris(ie) = ix(nix,ie)
        end do
        call my_orders(0,works,itris,indexs,nx,1)
        do j = 1, nx
          ie=indexs(j)
          ksysusrs(j) =ix(nix,ie)
          ksysusrs(nx+j)=ie
        end do
!       
        do ipl = 1,nlist
          ne = ilist(ipl)  
!         get sys_id from users
          ie=uel2sys(ne,ksysusrs,nx)
          if(ie/=0) then
              if (itag(ie) ==0) then  
                itag(ie) =pl_id
              else
! error out                
            end if
          endif
        enddo 
!-----------
        deallocate(ksysusrs,indexs,itris)
!---

        end subroutine initag_preload_a
!-------------------------------------------------        
      end module hm_read_preload_axial_mod
