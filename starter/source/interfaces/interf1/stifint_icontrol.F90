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
      !||    stifint_icontrol_mod   ../starter/source/interfaces/interf1/stifint_icontrol.F90
      !||--- called by ------------------------------------------------------
      !||    lectur                 ../starter/source/starter/lectur.F
      !||====================================================================
      module stifint_icontrol_mod
      
      contains
!=======================================================================================================================
!!\brief This subroutine do the initialization of stiffness contact interface for solid distortion control 
!=======================================================================================================================
      !||====================================================================
      !||    stifint_icontrol   ../starter/source/interfaces/interf1/stifint_icontrol.F90
      !||--- called by ------------------------------------------------------
      !||    lectur             ../starter/source/starter/lectur.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
        subroutine stifint_icontrol(                                           &                                    
                       numnod,    stifint,      npari,        ninter,          &
                        ipari,    npropgi,     numgeo,          igeo,          &
                       numels,       nixs,        ixs,       numels8,          &
                     numels10,      ixs10,   numels16,         ixs16,          &
                     numels20,      ixs20,     npropm,        nummat,          &
                           pm, intbuf_tab)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod,             only: zero,em20,one
          use intbufdef_mod   
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent (in   )                          :: numnod           !< number node
          integer, intent (in   )                          :: npari            !< 1er dim of ipari
          integer, intent (in   )                          :: ninter           !< number interface
          integer, intent (in   )                          :: npropgi          !< 1er dim of igeo
          integer, intent (in   )                          :: numgeo           !< number of prop
          integer, intent (in   )                          :: npropm           !< 1er dim of pm
          integer, intent (in   )                          :: nummat           !< number of materials
          integer, intent (in   )                          :: numels           !< number solid element
          integer, intent (in   )                          :: nixs             !< 1er dim of ixs
          integer, intent (in   )                          :: numels8          !< number solid (n<=8) element
          integer, intent (in   )                          :: numels10         !< number tet10 element
          integer, intent (in   )                          :: numels16         !< number s16 element
          integer, intent (in   )                          :: numels20         !< number s20 element
          integer, intent (in   ) ,dimension(npari,ninter) :: ipari            !< interface array
          integer, intent (in   ),dimension(npropgi,numgeo):: igeo             !< property array
          integer, intent (in   ) ,dimension(nixs,numels)  :: ixs              !< solid connectivity
          integer, intent (in   ) ,dimension(6,numels10)   :: ixs10            !< tet10 connectivity supp
          integer, intent (in   ) ,dimension(8,numels16)   :: ixs16            !< s16 connectivity supp
          integer, intent (in   ) ,dimension(12,numels20)  :: ixs20            !< s20 connectivity supp
          my_real, intent (in   ) ,dimension(npropm,nummat):: pm               !< material data array 
          my_real, intent (inout) ,dimension(numnod)       :: stifint          !< nodal stiffness for interface
          type(intbuf_struct_),  dimension(ninter)         :: intbuf_tab       !< interface structure
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
        integer i,j,k,n,ii,mid,pid,icontr,nty,igsti,nsn,ns
        integer, dimension(:)  ,  allocatable :: itag    
        my_real sfac,sfac_max
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
         sfac_max = one
         allocate(itag(numnod))
         sfac = -HUGE(sfac)
         itag = 0
         do i = 1, numels8 
           mid = ixs(1,i)
           pid = ixs(nixs-1,i)
           icontr = igeo(97,pid)
           if (icontr/=1) cycle
           sfac = pm(107,mid)/max(em20,pm(32,mid))
           sfac_max=max(sfac_max,sfac)
           do j = 1, 8
             n = ixs(1+j,i)
             if (itag(n)==0) then 
                stifint(n) =  sfac*stifint(n)
                itag(n) = 1
             end if
           end do
         end do
! tet10       
         do ii = 1, numels10 
           i = numels8 + ii
           mid = ixs(1,i)
           pid = ixs(nixs-1,i)
           icontr = igeo(97,pid)
           if (icontr/=1) cycle
           sfac = pm(107,mid)/max(em20,pm(32,mid))
           sfac_max=max(sfac_max,sfac)
           do j = 1, 8
             n = ixs(1+j,i)
             if (itag(n)==0) then 
                stifint(n) =  sfac*stifint(n)
                itag(n) = 1
             end if
           end do
           do j = 1, 6
             n = ixs10(j,ii)
             if (itag(n)==0) then 
                stifint(n) =  sfac*stifint(n)
                itag(n) = 1
             end if
           end do
         end do
! s20         
         do ii = 1, numels20 
           i = numels8 +numels10 + ii
           mid = ixs(1,i)
           pid = ixs(nixs-1,i)
           icontr = igeo(97,pid)
           if (icontr/=1) cycle
           sfac = pm(107,mid)/max(em20,pm(32,mid))
           sfac_max=max(sfac_max,sfac)
           do j = 1, 8
             n = ixs(1+j,i)
             if (itag(n)==0) then 
                stifint(n) =  sfac*stifint(n)
                itag(n) = 1
             end if
           end do
           do j = 1, 12
             n = ixs20(j,ii)
             if (itag(n)==0) then 
                stifint(n) =  sfac*stifint(n)
                itag(n) = 1
             end if
           end do
         end do
! s16         
         do ii = 1, numels16 
           i = numels8 +numels10 + ii
           mid = ixs(1,i)
           pid = ixs(nixs-1,i)
           icontr = igeo(97,pid)
           if (icontr/=1) cycle
           sfac = pm(107,mid)/max(em20,pm(32,mid))
           sfac_max=max(sfac_max,sfac)
           do j = 1, 8
             n = ixs(1+j,i)
             if (itag(n)==0) then 
                stifint(n) =  sfac*stifint(n)
                itag(n) = 1
             end if
           end do
           do j = 1, 8
             n = ixs16(j,ii)
             if (itag(n)==0) then 
                stifint(n) =  sfac*stifint(n)
                itag(n) = 1
             end if
           end do
         end do
!         
         do n=1,ninter
          nty=ipari(7,n)
          if (nty==24)then
            igsti=ipari(34,n)
            if(igsti==-1)then
              nsn=ipari(5,n)
              do j = 1, nsn
                ns = intbuf_tab(n)%nsv(j)
                if (itag(ns)==0) stifint(ns) =  sfac_max*stifint(ns)
              end do
            endif
          end if
         end do
!         
         deallocate(itag)
!         
        end subroutine stifint_icontrol
!
      end module stifint_icontrol_mod
