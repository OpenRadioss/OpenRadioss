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
      !||    i2_surfi         ../starter/source/interfaces/inter3d1/i2_surfi.F90
      !||--- called by ------------------------------------------------------
      !||    lecins           ../starter/source/interfaces/interf1/lecins.F
      !||--- calls      -----------------------------------------------------
      !||    select_s2s       ../starter/source/interfaces/inter3d1/select_s2s.F90
      !||--- uses       -----------------------------------------------------
      !||    select_s2s_mod   ../starter/source/interfaces/inter3d1/select_s2s.F90
      !||====================================================================
        subroutine i2_surfi(                                                 &
                         npari   ,ipari   ,nsurf   ,igrsurf ,                &
                         nsn     ,nsv     ,nrtm    ,irect   ,                &
                         nmn     ,msr     ,msegtyp ,dsearch ,                &
                         x       ,numnod  ,itab    ,ipri    ,                &
                         iout    )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use groupdef_mod
          use select_s2s_mod, only : select_s2s
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
          integer,    dimension(numnod),             intent(in) :: itab                !< number user_id
          my_real,                                   intent(in) :: dsearch             !< search distance
          my_real,    dimension(3,numnod),           intent(in) :: x                   !< coordinates of the nodes
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,l,k,m,n,ns,isu1,isu2,nsu1,nsu2,l1,l2,id
          integer,  dimension(:), allocatable :: itags1,itags2,itagn
          my_real :: rem
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! [ external functions must be kept to mimimum ]
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      isu1  = ipari(45)
      isu2  = ipari(46)
      nsu1  = igrsurf(isu1)%nseg
      nsu2  = igrsurf(isu2)%nseg
      allocate(itags1(nsu1))
      allocate(itags2(nsu2))
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
!    if (nsn/=ns) print *,'***error dimensionning: nsn,ns',nsn,ns
!    if (nsn/=nmn) print *,'***error dimensionning: nsn,nmn',nsn,nmn
    msr(1:nsn) = nsv(1:nsn)
    deallocate(itags1)
    deallocate(itags2)
    deallocate(itagn)
 1000  FORMAT(/1X,'SURFACE 1: Number of remain seg and % = ',I10,F10.1)
 2000  FORMAT(/1X,'SURFACE 2: Number of remain seg and % = ',I10,F10.1)

! ----------------------------------------------------------------------------------------------------------------------
        end subroutine i2_surfi        
      end module i2_surfi_mod







