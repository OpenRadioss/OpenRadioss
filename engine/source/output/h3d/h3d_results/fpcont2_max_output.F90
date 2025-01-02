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
      !||    fpcont2_max_output_mod   ../engine/source/output/h3d/h3d_results/fpcont2_max_output.F90
      !||--- called by ------------------------------------------------------
      !||    sortie_main              ../engine/source/output/sortie_main.F
      !||====================================================================
      module fpcont2_max_output_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine computes maximum of pressure vector for PCONT2 - MAX
!=======================================================================================================================
!
      !||====================================================================
      !||    fpcont2_max_output   ../engine/source/output/h3d/h3d_results/fpcont2_max_output.F90
      !||--- called by ------------------------------------------------------
      !||    sortie_main          ../engine/source/output/sortie_main.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod         ../common_source/modules/constant_mod.F
      !||====================================================================
        subroutine fpcont2_max_output(fcont,fcont_max,sz_npcont2,npcont2,weight,      &
                                      npcont2_max,numnod)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only: zero,em20
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
          integer,                                   intent(in) :: numnod                      !< number of nodes
          integer,                                   intent(in) :: sz_npcont2                  !< dimension of array npcont2
          integer,                                   intent(in) :: weight(numnod)              !< weight
          my_real,                                intent(inout) :: fcont(3,numnod)             !< output vector for PCONT2
          my_real,                                intent(inout) :: fcont_max(3,numnod)         !< max of output vector
          my_real,                                   intent(in) :: npcont2(3,sz_npcont2)       !< average normal on node for tied contact
          my_real,                                intent(inout) :: npcont2_max(3,numnod)       !< average normal on node for max force
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: n
          my_real :: fold,fnew,normal(1:3),nnn
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
          do n=1,numnod
            if(weight(n) == 1) then
!
              normal(1:3) = npcont2_max(1:3,n)
              nnn = sqrt(max(em20,normal(1)**2+normal(2)**2+normal(3)**2))
              normal(1:3) = normal(1:3)/nnn    
              fold = fcont_max(1,n)*normal(1)+fcont_max(2,n)*normal(2)+fcont_max(3,n)*normal(3)
!
              normal(1:3) = npcont2(1:3,n)
              nnn = sqrt(max(em20,normal(1)**2+normal(2)**2+normal(3)**2))
              normal(1:3) = normal(1:3)/nnn                        
              fnew = fcont(1,n)*normal(1)+fcont(2,n)*normal(2)+fcont(3,n)*normal(3)
!
              if(fnew > fold) then
                fcont_max(1:3,n) = fnew*normal(1:3)   
                npcont2_max(1:3,n) = normal(1:3)
             endif
           else
             fcont(1:3,n) =  zero
           endif
         enddo
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine fpcont2_max_output
! ----------------------------------------------------------------------------------------------------------------------
      end module fpcont2_max_output_mod
