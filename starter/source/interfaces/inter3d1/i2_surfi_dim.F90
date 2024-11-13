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
      !||    i2_surfi_dim_mod   ../starter/source/interfaces/inter3d1/i2_surfi_dim.F90
      !||--- called by ------------------------------------------------------
      !||    lecint             ../starter/source/interfaces/interf1/lecint.F
      !||====================================================================
      module i2_surfi_dim_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief this subroutine doing the dimensioning of the interface type2 w/ input surf/surf
      !||====================================================================
      !||    i2_surfi_dim     ../starter/source/interfaces/inter3d1/i2_surfi_dim.F90
      !||--- called by ------------------------------------------------------
      !||    lecint           ../starter/source/interfaces/interf1/lecint.F
      !||--- calls      -----------------------------------------------------
      !||    select_s2s       ../starter/source/interfaces/inter3d1/select_s2s.F90
      !||--- uses       -----------------------------------------------------
      !||    select_s2s_mod   ../starter/source/interfaces/inter3d1/select_s2s.F90
      !||====================================================================
        subroutine i2_surfi_dim(                                             &
                         npari   ,ipari   ,nsurf   ,igrsurf ,                &
                         nsn     ,nrtm    ,nmn     ,dsearch ,                &
                         x       ,numnod  )
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
          integer,                                   intent(in) :: numnod              !< the size of ipari
          integer,                                  intent(out) :: nsn                 !< number of secondary nodes
          integer,                                  intent(out) :: nrtm                !< number of main segs
          integer,                                  intent(out) :: nmn                 !< number of main nodes
          my_real,                                   intent(in) :: dsearch             !< search distance
          my_real,    dimension(3,numnod),           intent(in) :: x                   !< coordinates of the nodes
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,l,k,m,n,ns,isu1,isu2,nsu1,nsu2,id
          integer,  dimension(:), allocatable :: itags1,itags2,itagn
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
      id  = ipari(15)
      allocate(itags1(nsu1))
      allocate(itags2(nsu2))
      itags1 = 1
      itags2 = 1
! 
    call select_s2s(nsu1,nsu2,igrsurf(isu1)%nodes,igrsurf(isu2)%nodes,itags1,itags2,x,numnod,dsearch)
    allocate(itagn(numnod))
    itagn = 0
    l = 0
    do i=1,igrsurf(isu1)%nseg
      if (itags1(i)>0) l = l + 1
    end do
    nrtm = l
    l = 0
    do i=1,igrsurf(isu2)%nseg
      if (itags2(i)>0) l = l + 1
    end do
    nrtm = nrtm + l
! nsn
    ns = 0
    do i=1,igrsurf(isu1)%nseg
      if (itags1(i)==0) cycle
        do k=1,4
          n=igrsurf(isu1)%nodes(i,k)
          if(itagn(n) == 0)then
            ns = ns + 1
            itagn(n) = 1
          endif
        enddo
    enddo
    nsn = ns
    ns = 0
    do i=1,igrsurf(isu2)%nseg
      if (itags2(i)==0) cycle
      do k=1,4
        n=igrsurf(isu2)%nodes(i,k)
        if(itagn(n) == 0)then
          ns = ns + 1
          itagn(n) = 2
        endif
      enddo
    enddo
    nsn = nsn + ns
    nmn = nsn
    deallocate(itags1)
    deallocate(itags2)
    deallocate(itagn)

! ----------------------------------------------------------------------------------------------------------------------
        end subroutine i2_surfi_dim        
      end module i2_surfi_dim_mod







