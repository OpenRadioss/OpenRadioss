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
      module inter_offset_itag_mod

        contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine tag the shells used for contact : Option DEF_SHELL / IOFFSET=1
!=======================================================================================================================
        subroutine inter_offset_itag(                                          &                                    
                       ninter,    ipari,      npari,       igrsurf,            &
                        nsurf,   numelc,    numeltg,        itagsh)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use intbufdef_mod
      use groupdef_mod
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"       
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
        integer, intent (in   )                          :: ninter           !< number of interface
        integer, intent (in   )                          :: npari            !< 1er dim of ipari
        integer, intent (in   )                          :: nsurf            !< number of surface
        integer, intent (in   )                          :: numelc           !< number shell 4n element
        integer, intent (in   )                          :: numeltg          !< number shell 3n element
        integer, intent (in   ) ,dimension(npari,ninter) :: ipari            !< interface array
        integer, intent (inout),dimension(numelc+numeltg):: itagsh           !< < shell w/ offset
        type (surf_)   ,       dimension(nsurf) ,target  :: igrsurf          !< surf array
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
        integer i,j,n,ni,ntyp,isu1,isu2,ilev,ie,etyp
        integer, dimension(:), allocatable   :: intage
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------  
      allocate(intage(numelc+numeltg))
      intage(1:numelc+numeltg)=-itagsh(1:numelc+numeltg)
! for the moment sencondary node isn't taken into account      
      do ni = 1,ninter        
        ntyp  = ipari(7,ni)
        isu1  = ipari(45,ni)
        isu2  = ipari(46,ni)
        ilev  = ipari(20,ni)
        select case (ntyp)
        case(24,25)
          if (ilev==1.or.ilev==2) then
            do j=1,igrsurf(isu1)%nseg
              etyp = igrsurf(isu1)%eltyp(j)
              if (etyp/=3 .and. etyp/=7) cycle
              ie = igrsurf(isu1)%elem(j)
              if (etyp==7) ie = ie + numelc
              if (ie>0.and.itagsh(ie)>0) intage(ie)=1
            end do
          end if
          if (ilev==2.or.ilev==3) then
            do j=1,igrsurf(isu2)%nseg
              etyp = igrsurf(isu2)%eltyp(j)
              if (etyp/=3 .and. etyp/=7) cycle
              ie = igrsurf(isu2)%elem(j)
              if (ie>0.and.itagsh(ie)>0) intage(ie)=1
            end do
          end if
        end select
      end do
      deallocate(intage)
!-----------
      end subroutine inter_offset_itag
    end module  inter_offset_itag_mod