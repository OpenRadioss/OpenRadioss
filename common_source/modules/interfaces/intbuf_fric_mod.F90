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
!hd|====================================================================
!hd|  intbuf_fric_mod               share/modules/intbuf_fric_mod.f
!hd|-- called by -----------
!hd|        i11mainf                      source/interfaces/int11/i11mainf.f
!hd|        i24mainf                      source/interfaces/int24/i24main.f
!hd|        i25mainf                      source/interfaces/int25/i25mainf.f
!hd|        i7mainf                       source/interfaces/int07/i7mainf.f
!hd|        intfop2                       source/interfaces/interf/intfop2.f
!hd|        intfric_rresti                share/modules/intbuf_fric_mod.f
!hd|        intfric_rrestr                share/modules/intbuf_fric_mod.f
!hd|        intfric_wresti                share/modules/intbuf_fric_mod.f
!hd|        intfric_wrestr                share/modules/intbuf_fric_mod.f
!hd|        resol                         source/engine/resol.f
!hd|        resol_head                    source/engine/resol_head.f
!hd|        wrrestp                       source/output/restart/wrrestp.f
!hd|        intbuffric_mod                share/modules/restart_mod.f
!hd|-- calls ---------------
!hd|====================================================================
      module intbuf_fric_mod
!-----------------------------------------------
!   m o d u l e s
!-----------------------------------------------

!-----------------------------------------------
!   m y _ r e a l
!-----------------------------------------------
#include      "my_real.inc"
!-----------------------------------------------
        type intbuf_fric_struct_

          integer ::    nsetprts       ! number of couple of parts
          integer ::    fricmod        ! the friction model
          integer ::    fricform       ! the friction formulation ( viscous or incremental)
          integer ::    iffilter       !friction filtering flag
          integer ::    s_tabparts_fric  ! number of parts
          integer ::    iorthfric          ! flag for orthotropic friction
          my_real :: xfiltr_fric        !filtering coefficient

          integer, dimension(:), allocatable :: tabcoupleparts_fric    ! table of couple of parts
          integer, dimension(:), allocatable :: tabparts_fric          ! table of parts
          integer, dimension(:), allocatable :: adparts_fric           ! table of adress of couple of parts
          integer, dimension(:), allocatable :: ifricorth              ! table of orthotropic type of couple of parts
          my_real,dimension(:), allocatable :: tabcoef_fric            ! table of friction coefficients

        end type intbuf_fric_struct_

      contains
!hd|====================================================================
!hd|  intfric_wresti                share/modules/intbuf_fric_mod.f
!hd|-- called by -----------
!hd|        wrrestp                       source/output/restart/wrrestp.f
!hd|-- calls ---------------
!hd|        write_i_c                     ../common_source/tools/input_output/write_routtines.c
!hd|        intbuf_fric_mod               share/modules/intbuf_fric_mod.f
!hd|====================================================================
        subroutine intfric_wresti(intbuf_fric_tab,ninterfric)
!-----------------------------------------------
!   i m p l i c i t   t y p e s
!-----------------------------------------------
          implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
          integer, intent(in) :: ninterfric
          type(intbuf_fric_struct_),intent(in):: intbuf_fric_tab(ninterfric)
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
          integer len, n, j, nset
!--------------------------------------
!     ecriture des dimensions
!--------------------------------------
          do n=1,ninterfric
            len    =1
            call write_i_c(intbuf_fric_tab(n)%nsetprts,len)

            len    =1
            call write_i_c(intbuf_fric_tab(n)%fricmod,len)

            len    =1
            call write_i_c(intbuf_fric_tab(n)%fricform,len)

            len    =1
            call write_i_c(intbuf_fric_tab(n)%iffilter,len)

            len    =1
            call write_i_c(intbuf_fric_tab(n)%iorthfric,len)

            len    =1
            call write_i_c(intbuf_fric_tab(n)%s_tabparts_fric ,len)

            nset = intbuf_fric_tab(n)%nsetprts
            len    = nset
            call write_i_array_c(intbuf_fric_tab(n)%tabcoupleparts_fric,len)

            len    = intbuf_fric_tab(n)%s_tabparts_fric
            call write_i_array_c(intbuf_fric_tab(n)%tabparts_fric,len)

            len    = intbuf_fric_tab(n)%s_tabparts_fric +1
            call write_i_array_c(intbuf_fric_tab(n)%adparts_fric,len)

            len  = intbuf_fric_tab(n)%nsetprts
            call write_i_array_c(intbuf_fric_tab(n)%ifricorth,len)
          end do
          return
        end subroutine intfric_wresti
!hd|====================================================================
!hd|  intfric_wrestr                share/modules/intbuf_fric_mod.f
!hd|-- called by -----------
!hd|        wrrestp                       source/output/restart/wrrestp.f
!hd|-- calls ---------------
!hd|        write_db                      source/output/tools/write_db.f
!hd|        intbuf_fric_mod               share/modules/intbuf_fric_mod.f
!hd|====================================================================
        subroutine intfric_wrestr(intbuf_fric_tab,ninterfric)
!-----------------------------------------------
!   i m p l i c i t   t y p e s
!-----------------------------------------------
          implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
          integer, intent(in) :: ninterfric
          type(intbuf_fric_struct_),intent(in) :: intbuf_fric_tab(ninterfric)
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
          integer len, n, j, iorth ,mfrot ,lenc
          integer nset
!--------------------------------------
          do n=1,ninterfric
            len    =1
            call write_db(intbuf_fric_tab(n)%xfiltr_fric,len)

            nset = intbuf_fric_tab(n)%nsetprts
            iorth = intbuf_fric_tab(n)%iorthfric
            mfrot = intbuf_fric_tab(n)%fricmod
            if(mfrot ==0 ) then
              lenc =2
            else
              lenc = 8
            endif
            if(iorth == 0) then
              len    =lenc*(nset+1)
              call write_db_array(intbuf_fric_tab(n)%tabcoef_fric,len)
            else
              len    =lenc+2*lenc*nset
              call write_db_array(intbuf_fric_tab(n)%tabcoef_fric,len)
            endif
          end do
          return
        end subroutine intfric_wrestr

!hd|====================================================================
!hd|  intfric_rresti                share/modules/intbuf_fric_mod.f
!hd|-- called by -----------
!hd|        rdresb                        source/output/restart/rdresb.f
!hd|-- calls ---------------
!hd|        read_i_c                      ../common_source/tools/input_output/write_routtines.c
!hd|        intbuf_fric_mod               share/modules/intbuf_fric_mod.f
!hd|====================================================================
        subroutine intfric_rresti(intbuf_fric_tab,ninterfric)
!-----------------------------------------------
!   i m p l i c i t   t y p e s
!-----------------------------------------------
          implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
          integer, intent(in) :: ninterfric
          type(intbuf_fric_struct_),intent(inout) :: intbuf_fric_tab(ninterfric)
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
          integer len, n, j, nset,leni
!--------------------------------------
!     ecriture des dimensions
!--------------------------------------
          leni=0
          do n=1,ninterfric
            len    =1
            call read_i_c(intbuf_fric_tab(n)%nsetprts,len)
            leni = leni + len

            len    =1
            call read_i_c(intbuf_fric_tab(n)%fricmod,len)
            leni = leni + len

            len    =1
            call read_i_c(intbuf_fric_tab(n)%fricform,len)
            leni = leni + len

            len    =1
            call read_i_c(intbuf_fric_tab(n)%iffilter,len)
            leni = leni + len

            len    =1
            call read_i_c(intbuf_fric_tab(n)%iorthfric,len)
            leni = leni + len

            len    =1
            call read_i_c(intbuf_fric_tab(n)%s_tabparts_fric,len)
            leni = leni + len

            len = intbuf_fric_tab(n)%nsetprts
            allocate(intbuf_fric_tab(n)%tabcoupleparts_fric(len))
            call read_i_array_c(intbuf_fric_tab(n)%tabcoupleparts_fric,len)
            leni = leni + len

            len    = intbuf_fric_tab(n)%s_tabparts_fric
            allocate(intbuf_fric_tab(n)%tabparts_fric(len))
            call read_i_array_c(intbuf_fric_tab(n)%tabparts_fric,len)
            leni = leni + len

            len = intbuf_fric_tab(n)%s_tabparts_fric +1
            allocate(intbuf_fric_tab(n)%adparts_fric(len))
            call read_i_array_c(intbuf_fric_tab(n)%adparts_fric,len)
            leni = leni + len

            len = intbuf_fric_tab(n)%nsetprts
            allocate(intbuf_fric_tab(n)%ifricorth(len))
            call read_i_array_c(intbuf_fric_tab(n)%ifricorth,len)
            leni = leni + len

          end do
          return
        end subroutine intfric_rresti

!hd|====================================================================
!hd|  intfric_rrestr                share/modules/intbuf_fric_mod.f
!hd|-- called by -----------
!hd|        rdresb                        source/output/restart/rdresb.f
!hd|-- calls ---------------
!hd|        read_db                       source/output/tools/read_db.f
!hd|        intbuf_fric_mod               share/modules/intbuf_fric_mod.f
!hd|====================================================================
        subroutine intfric_rrestr(intbuf_fric_tab,ninterfric)
!-----------------------------------------------
!   i m p l i c i t   t y p e s
!-----------------------------------------------
          implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
          integer,intent(in) :: ninterfric
          type(intbuf_fric_struct_),intent(inout) :: intbuf_fric_tab(ninterfric)
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
          integer len, n, j ,nset ,iorth ,mfrot ,lenc
!--------------------------------------
          do n=1,ninterfric
            len    =1
            call read_db(intbuf_fric_tab(n)%xfiltr_fric,len)
            nset = intbuf_fric_tab(n)%nsetprts
            iorth = intbuf_fric_tab(n)%iorthfric
            mfrot = intbuf_fric_tab(n)%fricmod
            if(mfrot ==0 ) then
              lenc =2
            else
              lenc = 8
            endif

            if(iorth == 0) then
              len    =lenc*(nset+1)
            else
              len    =lenc+2*lenc*nset
            endif

            if(len>0)then
              allocate(intbuf_fric_tab(n)%tabcoef_fric(len))
              call read_db_array(intbuf_fric_tab(n)%tabcoef_fric,len)
            endif

          end do
          return
        end subroutine intfric_rrestr

      end module intbuf_fric_mod
