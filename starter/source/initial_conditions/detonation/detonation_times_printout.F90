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
      !||    detonation_times_printout_mod   ../starter/source/initial_conditions/detonation/detonation_times_printout.F90
      !||--- called by ------------------------------------------------------
      !||    initia                          ../starter/source/elements/initia/initia.F
      !||====================================================================
      module detonation_times_printout_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Output Detonation Times for relevant elements
!! \details default detonation time set to 0.0 if no detonator is related
      !||====================================================================
      !||    detonation_times_printout   ../starter/source/initial_conditions/detonation/detonation_times_printout.F90
      !||--- called by ------------------------------------------------------
      !||    initia                      ../starter/source/elements/initia/initia.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
        subroutine detonation_times_printout(NPARG,NGROUP,IPARG,N2D,IPRI,ELBUF_TAB, &
                                              NIXS,NIXQ,NIXTG,NUMELS,NUMELQ,NUMELTG,IXS,IXQ,IXTG)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only : zero, ep21
          use elbufdef_mod , only : elbuf_struct_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
#include "units_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,intent(in) :: ngroup  !< number of groups
          integer,intent(in) :: nparg   !< array size
          integer,intent(in) :: iparg(nparg,ngroup) !<group parameters
          integer,intent(in) :: n2d     !< 2d/3d flag (/ANALY)
          integer,intent(in) :: ipri    !< printout flag (/IOFLAG)          
          integer,intent(in) :: nixs    !< array size ixs
          integer,intent(in) :: nixq    !< array size ixq
          integer,intent(in) :: nixtg   !< array size ixtg
          integer,intent(in) :: numels  !< number of hexa elems (solid)
          integer,intent(in) :: numelq  !< number of quad elems (solid)
          integer,intent(in) :: numeltg !< number of tria elems (solid)
          integer,intent(in),target :: ixs(nixs,numels)    !< hexa connectivities
          integer,intent(in),target :: ixq(nixq,numelq)    !< quad connectivities
          integer,intent(in),target :: ixtg(nixtg,numeltg) !< tria connectivities
          type (elbuf_struct_), target, dimension(ngroup) :: elbuf_tab
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ng, i
          integer :: nel,nft
          integer :: mpr
          integer :: iel
          integer :: mlw
          my_real :: tdet
          integer,dimension(:,:),pointer :: ix
          integer :: nix
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

       !---------------------------------!
       !   UNINT. ELEMS (NO DETONATOR)   !
       !---------------------------------!
       do ng = 1,ngroup
         nel = iparg(2,ng)
         mlw = iparg(1,ng)
         if(elbuf_tab(ng)%gbuf%g_tb > 0)then
           do i=1,nel
             tdet = elbuf_tab(ng)%gbuf%tb(i)
             if(tdet == ep21 .or. tdet == -ep21)then
               elbuf_tab(ng)%gbuf%tb(i) = zero
             end if
           enddo
         endif
       end do

       !---------------------------------!
       !            PRINTOUT             !
       !---------------------------------!
       if(n2d == 0)then
          ix => ixs(1:nixs,1:numels)
          nix = nixs
        elseif(numelq > 0)then
          ix => ixq(1:nixq,1:numelq)
          nix = nixq
        else
          ix => ixtg(1:nixtg,1:numeltg)
          nix = nixq
        end if

       if(ipri >= 3)then
         mpr =0
         write(iout,500)         
         do ng = 1,ngroup
           nel = iparg(2,ng)
           nft = iparg(3,ng)
           if(elbuf_tab(ng)%gbuf%g_tb > 0)then           
             do i=1,nel
               mpr = mpr+1
               iel = ix(nix,i+nft)
               tdet=-elbuf_tab(ng)%gbuf%tb(i)
               write(iout,510) nel,tdet
               if(mpr == 50) mpr=0
             end do
           endif
         end do
       endif
       
       return

! ----------------------------------------------------------------------------------------------------------------------
  500 FORMAT(//, &
       5X, 'DETONATION TIMES FOR JWL ELEMENTS' /, &
       5X, '---------------------------------' //, &
       5X, 'ELEMENT DETONATION TIME' /)
  510 FORMAT(5X,I10,E15.5)
  
! ----------------------------------------------------------------------------------------------------------------------
      end subroutine detonation_times_printout
! ----------------------------------------------------------------------------------------------------------------------

      end module detonation_times_printout_mod
