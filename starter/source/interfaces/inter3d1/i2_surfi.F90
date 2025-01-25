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
      !||    i2_surfi_mod   ../starter/source/interfaces/inter3d1/i2_surfi.F90
      !||--- called by ------------------------------------------------------
      !||    lecins         ../starter/source/interfaces/interf1/lecins.F
      !||====================================================================
      module i2_surfi_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief this subroutine doing the initialization of the interface type2 w/ input surf/surf
      !||====================================================================
      !||    i2_surfi                ../starter/source/interfaces/inter3d1/i2_surfi.F90
      !||--- called by ------------------------------------------------------
      !||    lecins                  ../starter/source/interfaces/interf1/lecins.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                  ../starter/source/output/message/message.F
      !||    ineltc                  ../starter/source/interfaces/inter3d1/inelt.F
      !||    inelts                  ../starter/source/interfaces/inter3d1/inelt.F
      !||    select_s2s              ../starter/source/interfaces/inter3d1/select_s2s.F90
      !||--- uses       -----------------------------------------------------
      !||    message_mod             ../starter/share/message_module/message_mod.F
      !||    select_s2s_mod          ../starter/source/interfaces/inter3d1/select_s2s.F90
      !||====================================================================
        subroutine i2_surfi(                                                 &
                         npari   ,ipari   ,nsurf   ,igrsurf ,                &
                         nsn     ,nsv     ,nrtm    ,irect   ,                &
                         nmn     ,msr     ,msegtyp ,dsearch ,                &
                         x       ,numnod  ,itab    ,ipri    ,                &
                         iout    ,ixs     ,numels  ,noint   ,                &
                         irtl    ,st      ,dmin    )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use groupdef_mod
          use select_s2s_mod,   only : select_s2s
          use message_mod
          use constant_mod,     only : nine,ep20
          use connectivity_size_mod, only : nixs
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
          integer,                                   intent(in) :: npari               !< the size of ipari
          integer,   dimension(npari),               intent(in) :: ipari               !< interface general data
          integer,                                   intent(in) :: nsurf               !< the size of igrsurf
          type (surf_)   , dimension(nsurf),         intent(in) :: igrsurf             !< surf data
          integer,                                   intent(in) :: nsn                 !< number of secondary nodes
          integer,                                   intent(in) :: nrtm                !< number of main segs
          integer,                                   intent(in) :: nmn                 !< number of main nodes
          integer,                                   intent(in) :: numnod              !< the size of ipari
          integer,   dimension(nsn),              intent(inout) :: nsv                 !< secondary node array
          integer,   dimension(nmn),              intent(inout) :: msr                 !< main node array
          integer,   dimension(4,nrtm),           intent(inout) :: irect               !< main seg array
          integer,   dimension(nrtm),             intent(inout) :: msegtyp             !< main seg type array
          integer,                                   intent(in) :: ipri                !< message out flag 
          integer,                                   intent(in) :: iout                !< outfile unit  
          integer,                                   intent(in) :: numels              !< number of solid elements
          integer,    dimension(numnod),             intent(in) :: itab                !< number user_id
          my_real,                                   intent(in) :: dsearch             !< search distance
          my_real,    dimension(3,numnod),           intent(in) :: x                   !< coordinates of the nodes
          integer,    dimension(nixs,numels),        intent(in) :: ixs                 !< solid connectivity
          integer,                                   intent(in) :: noint               !< user_id of interfaces
          integer,    dimension(nsn),             intent(inout) :: irtl                !< interface node array irtlm
          my_real,    dimension(2,nsn),           intent(inout) :: st                  !< interface node work array csts
          my_real,    dimension(nsn),             intent(inout) :: dmin                !< interface node work array dpara
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,l,k,m,n,ns,isu1,isu2,nsu1,nsu2,l1,l2,id,inrt, nels, nelc, neltg,nint
          integer,  dimension(:), allocatable :: itags1,itags2,itagn,igrelem
          my_real :: rem,area
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! [ external functions must be kept to minimum ]
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      isu1  = ipari(45)
      isu2  = ipari(46)
      nsu1  = igrsurf(isu1)%nseg
      nsu2  = igrsurf(isu2)%nseg
      allocate(itags1(nsu1))
      allocate(itags2(nsu2))
      allocate(igrelem(nrtm))
      itags1 = 1
      itags2 = 1
! 
    call select_s2s(nsu1,nsu2,igrsurf(isu1)%nodes,igrsurf(isu2)%nodes,itags1,itags2,x,numnod,dsearch)
    l = 0
    do i=1,igrsurf(isu1)%nseg
      if (itags1(i)>0) then 
        l = l + 1
        irect(1:4,l) = igrsurf(isu1)%nodes(i,1:4)
        msegtyp(l) = igrsurf(isu1)%eltyp(i)
        if (msegtyp(l)==0) msegtyp(l) = 10
        igrelem(l) = igrsurf(isu1)%elem(i)
      end if
    end do
    l1 = l
    if (l1>0.and.ipri>=1) then 
      rem = 100*l/nsu1
      write(iout,1000) l,rem
      if (ipri>=5) then
        write(iout,'(/,A/)') 'Remain surface 1 list'
        do i=1,l
          write(iout,*) 'id,irect(1:4)=',i,itab(irect(1:4,i))
        end do
      end if
    end if 
!
    do i=1,igrsurf(isu2)%nseg
      if (itags2(i)>0) then 
        l = l + 1
        irect(1:4,l) = igrsurf(isu2)%nodes(i,1:4)
        msegtyp(l) = igrsurf(isu2)%eltyp(i)
        if (msegtyp(l)==0) msegtyp(l) = 10
        igrelem(l) = igrsurf(isu2)%elem(i)
      end if
    end do
    l2 = l-l1
    if (l2>0.and.ipri>=1) then 
      rem = 100*(l2)/nsu2
      write(iout,2000) l2,rem
      if (ipri>=5) then
        write(iout,'(/,A/)') 'Remain surface 2 list'
        do i=l1+1,l
          id = i-l1
          write(iout,*) 'id,irect(1:4)=',id,itab(irect(1:4,i))
        end do
      end if
    end if
!    if (l/=nrtm) print *,'error dimensionning: l,nrtm',l,nrtm
! nsn
    allocate(itagn(numnod))
    itagn = 0
    ns = 0
    do i=1,igrsurf(isu1)%nseg
      if (itags1(i)==0) cycle
      do k=1,4
        n=igrsurf(isu1)%nodes(i,k)
        if (itagn(n) == 0) then
          ns = ns + 1
          itagn(n) = 1
          nsv(ns) = n
        endif
      enddo
    enddo
!    
    do i=1,igrsurf(isu2)%nseg
      if (itags2(i)==0) cycle
      do k=1,4
        n=igrsurf(isu2)%nodes(i,k)
        if(itagn(n) == 0)then
          ns = ns + 1
          itagn(n) = 2
          nsv(ns) = n 
        endif
      enddo
    enddo
!---i2chk3 done here
    nint = 1  ! not used
    do i=1,nrtm
        inrt=i
        nels=0
        nelc=0
        neltg=0
        if (msegtyp(i)==1) then
          call inelts(x           ,irect,ixs  ,nint,nels         ,           &
                      inrt        ,area ,noint,0   ,msegtyp      ,           &
                      igrelem     )
        else
          call ineltc(nelc ,neltg ,inrt ,msegtyp, igrelem)
        end if
        if(nels+nelc+neltg==0)then
           call ancmsg(msgid=93,msgtype=msgwarning,                          &
                       anmode=aninfo_blind_2,i2=noint,i1=i)
        endif
    end do
! ns
    do i=1,nsn
        irtl(i)=0
        st(1,i)=nine
        st(2,i)=nine
        dmin(i)=ep20
    enddo
!    if (nsn/=ns) print *,'***error dimensionning: nsn,ns',nsn,ns
!    if (nsn/=nmn) print *,'***error dimensionning: nsn,nmn',nsn,nmn
    msr(1:nsn) = nsv(1:nsn)
    deallocate(itags1)
    deallocate(itags2)
    deallocate(itagn)
    deallocate(igrelem)
 1000  FORMAT(/1X,'SURFACE 1: Number of remain seg and % = ',I10,F10.1)
 2000  FORMAT(/1X,'SURFACE 2: Number of remain seg and % = ',I10,F10.1)

! ----------------------------------------------------------------------------------------------------------------------
        end subroutine i2_surfi        
      end module i2_surfi_mod







