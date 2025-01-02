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
      !||    write_inivel_mod   ../engine/source/output/restart/write_inivel.F90
      !||--- called by ------------------------------------------------------
      !||    wrrestp            ../engine/source/output/restart/wrrestp.F
      !||====================================================================
      module write_inivel_mod        
       contains
  !! \brief write inivel rst data (engine)
      !||====================================================================
      !||    write_inivel   ../engine/source/output/restart/write_inivel.F90
      !||--- called by ------------------------------------------------------
      !||    wrrestp        ../engine/source/output/restart/wrrestp.F
      !||--- calls      -----------------------------------------------------
      !||    write_db       ../common_source/tools/input_output/write_db.F
      !||    write_i_c      ../common_source/tools/input_output/write_routtines.c
      !||--- uses       -----------------------------------------------------
      !||    inivel_mod     ../common_source/modules/inivel_mod.F90
      !||====================================================================
        subroutine write_inivel(ninivelt,inivel_t)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use inivel_mod 
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
      integer , intent(in  )                            :: ninivelt  !< dimension of inivel_t
      type(inivel_), dimension(ninivelt), intent(in   ) :: inivel_t  !< inivel_struc 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer  :: i,j,id,ninivel_l,n,itype,igr,node,nel,itmp(10),nl,nr 
      integer  :: igrs,igbric,igqd,igtria,igbric_loc,igqd_loc,igtria_loc,is,isens 
      my_real  :: rtmp(6)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
       do n =1,ninivelt 
         itmp(1) = inivel_t(n)%id  
         itype = inivel_t(n)%itype
         itmp(2) = itype  
         call write_i_c(itmp,2) 
           select case (itype)
             case(0,1,2,3)
               itmp(1) = inivel_t(n)%general%type    
               itmp(2) = inivel_t(n)%general%skew_id 
               itmp(3) = inivel_t(n)%general%grnd_id 
               itmp(4) = inivel_t(n)%general%sensor_id 
               rtmp(1) = inivel_t(n)%general%vx 
               rtmp(2) = inivel_t(n)%general%vy 
               rtmp(3) = inivel_t(n)%general%vz 
               rtmp(4) = inivel_t(n)%general%tstart 
               nl = 4
               nr = 4
             case(4) ! axis
               itmp(1) = inivel_t(n)%axis%dir 
               itmp(2) = inivel_t(n)%axis%frame_id 
               itmp(3) = inivel_t(n)%axis%grnd_id 
               itmp(4) = inivel_t(n)%axis%sensor_id 
               rtmp(1) = inivel_t(n)%axis%vx 
               rtmp(2) = inivel_t(n)%axis%vy 
               rtmp(3) = inivel_t(n)%axis%vz 
               rtmp(4) = inivel_t(n)%axis%vr 
               rtmp(5) = inivel_t(n)%axis%tstart
               nl = 4
               nr = 5
             case(5) ! fvm
               itmp(1) = inivel_t(n)%fvm%skew_id 
               itmp(2) = inivel_t(n)%fvm%grbric_id 
               itmp(3) = inivel_t(n)%fvm%grqd_id   
               itmp(4) = inivel_t(n)%fvm%grtria_id 
               itmp(5) = inivel_t(n)%fvm%sensor_id 
               rtmp(1) = inivel_t(n)%fvm%vx 
               rtmp(2) = inivel_t(n)%fvm%vy 
               rtmp(3) = inivel_t(n)%fvm%vz 
               rtmp(4) = inivel_t(n)%fvm%tstart
               nl = 5
               nr = 4
           end select
           call write_i_c(itmp,nl)
           call write_db(rtmp,nr)
       end do 
       end subroutine write_inivel
      end module write_inivel_mod
