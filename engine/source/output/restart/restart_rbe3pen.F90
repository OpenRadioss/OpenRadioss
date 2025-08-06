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
!||    restart_rbe3pen_mod   ../engine/source/output/restart/restart_rbe3pen.F90
!||--- called by ------------------------------------------------------
!||    rdresb                ../engine/source/output/restart/rdresb.F
!||    wrrestp               ../engine/source/output/restart/wrrestp.F
!||====================================================================
      module restart_rbe3pen_mod        
       contains
! ----------------------------------------------------------------------------------------------------------------------
  !! \brief get the number of penalty formulation of RBE3
!||====================================================================
!||    get_nrbe3pen_l   ../engine/source/output/restart/restart_rbe3pen.F90
!||--- called by ------------------------------------------------------
!||    rdresb           ../engine/source/output/restart/rdresb.F
!||--- uses       -----------------------------------------------------
!||    rbe3_mod         ../common_source/modules/constraints/rbe3_mod.F90
!||====================================================================
        subroutine get_nrbe3pen_l(nrbe3,nrbe3l,irbe3,nrbe3pen_l)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use rbe3_mod 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer , intent(in  )                            :: nrbe3       !< number of rbe3
      integer , intent(in  )                            :: nrbe3l      !< 1er dimension irbe3
      integer , dimension(nrbe3l,nrbe3),   intent(in  ) :: irbe3       !< rbe3 data array
      integer , intent(out)                             :: nrbe3pen_l  !< number of rbe3 using penalty
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer  :: i,ipen
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
       nrbe3pen_l = 0
       do i =1,nrbe3 
         ipen = irbe3(9,i)
         if (ipen >0) nrbe3pen_l = nrbe3pen_l + 1
       end do 
       end subroutine get_nrbe3pen_l
! ----------------------------------------------------------------------------------------------------------------------
  !! \brief read internal arrays used for rbe3 penalty
!||====================================================================
!||    read_rrbe3pen      ../engine/source/output/restart/restart_rbe3pen.F90
!||--- called by ------------------------------------------------------
!||    rdresb             ../engine/source/output/restart/rdresb.F
!||--- calls      -----------------------------------------------------
!||    allocate_rbe3pen   ../common_source/modules/constraints/rbe3_mod.F90
!||    read_db            ../common_source/tools/input_output/read_db.F
!||--- uses       -----------------------------------------------------
!||    rbe3_mod           ../common_source/modules/constraints/rbe3_mod.F90
!||====================================================================
        subroutine read_rrbe3pen(rbe3pen,nrbe3pen_l)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use rbe3_mod 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer , intent(in)           :: nrbe3pen_l  !< number of rbe3 using penalty
      TYPE (RBE3_pen), INTENT(INOUT) :: rbe3pen     !< rbe3pen data array
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
       rbe3pen%nrbe3_lp = nrbe3pen_l
       call allocate_rbe3pen(rbe3pen)
       call read_db ( rbe3pen%rrbe3pen_vi,nrbe3pen_l)
       call read_db ( rbe3pen%rrbe3pen_f,3*nrbe3pen_l)
       call read_db ( rbe3pen%rrbe3pen_m,3*nrbe3pen_l)
       call read_db ( rbe3pen%rrbe3pen_stf,2*nrbe3pen_l)
       call read_db ( rbe3pen%rrbe3pen_fac,nrbe3pen_l)
!
       end subroutine read_rrbe3pen
! ----------------------------------------------------------------------------------------------------------------------
  !! \brief write internal arrays used for rbe3 penalty
!||====================================================================
!||    write_rrbe3pen   ../engine/source/output/restart/restart_rbe3pen.F90
!||--- called by ------------------------------------------------------
!||    wrrestp          ../engine/source/output/restart/wrrestp.F
!||--- calls      -----------------------------------------------------
!||    write_db         ../common_source/tools/input_output/write_db.F
!||--- uses       -----------------------------------------------------
!||    rbe3_mod         ../common_source/modules/constraints/rbe3_mod.F90
!||====================================================================
        subroutine write_rrbe3pen(rbe3pen)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use rbe3_mod 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      TYPE (RBE3_pen),  INTENT(IN) :: rbe3pen     !< rbe3%pen data array
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
      integer  :: nrbe3pen_l
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
       nrbe3pen_l = rbe3pen%nrbe3_lp 
       call write_db ( rbe3pen%rrbe3pen_vi,nrbe3pen_l)
       call write_db ( rbe3pen%rrbe3pen_f,3*nrbe3pen_l)
       call write_db ( rbe3pen%rrbe3pen_m,3*nrbe3pen_l)
       call write_db ( rbe3pen%rrbe3pen_stf,2*nrbe3pen_l)
       call write_db ( rbe3pen%rrbe3pen_fac,nrbe3pen_l)
!
       end subroutine write_rrbe3pen
! ----------------------------------------------------------------------------------------------------------------------
      end module restart_rbe3pen_mod
