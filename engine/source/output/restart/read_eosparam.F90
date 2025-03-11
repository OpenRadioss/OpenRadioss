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
      !||    read_eosparam_mod   ../engine/source/output/restart/read_eosparam.F90
      !||--- called by ------------------------------------------------------
      !||    read_matparam       ../engine/source/output/restart/read_matparam.F
      !||====================================================================
      module read_eosparam_mod
      contains
      !||====================================================================
      !||    read_eosparam          ../engine/source/output/restart/read_eosparam.F90
      !||--- called by ------------------------------------------------------
      !||    read_matparam          ../engine/source/output/restart/read_matparam.F
      !||--- calls      -----------------------------------------------------
      !||    read_c_c               ../common_source/tools/input_output/write_routtines.c
      !||    read_db                ../common_source/tools/input_output/read_db.F
      !||    read_i_c               ../common_source/tools/input_output/write_routtines.c
      !||    read_mat_table         ../engine/source/materials/tools/read_mat_table.F
      !||--- uses       -----------------------------------------------------
      !||    eos_param_mod          ../common_source/modules/mat_elem/eos_param_mod.F90
      !||    message_mod            ../engine/share/message_module/message_mod.F
      !||    my_alloc_mod           ../common_source/tools/memory/my_alloc.F90
      !||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
      !||====================================================================
      SUBROUTINE READ_EOSPARAM(EOS)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      USE MESSAGE_MOD
      USE EOS_PARAM_MOD , ONLY : EOS_PARAM_
      USE NAMES_AND_TITLES_MOD
      USE MY_ALLOC_MOD
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      TYPE(EOS_PARAM_) ,INTENT(INOUT) :: EOS
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
      INTEGER :: I,NUPARAM,NIPARAM,NUMTABL,IAD,LEN,NUMFUNC
      INTEGER ,DIMENSION(NCHARTITLE) :: NAME
      INTEGER ,DIMENSION(1) :: ILEN
      INTEGER ,DIMENSION(:), ALLOCATABLE :: IBUF
      my_real ,DIMENSION(:), ALLOCATABLE :: RBUF
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      ! read eos model parameters (integer)
      call read_i_c(ilen, 1)
      len = ilen(1)
      allocate (ibuf(len) )
      call read_i_c(ibuf, len)
      iad = 0
      iad = iad+1 ; eos%nuparam = ibuf(iad)
      iad = iad+1 ; eos%niparam = ibuf(iad)
      iad = iad+1 ; eos%nuvar   = ibuf(iad)
      iad = iad+1 ; eos%nfunc   = ibuf(iad)
      iad = iad+1 ; eos%ntable  = ibuf(iad)
      iad = iad+1 ; eos%isfluid = ibuf(iad)
      deallocate( ibuf )

      ! read eos model parameters (real)
      call read_i_c(ilen, 1)
      len = ilen(1)
      allocate (rbuf(len))
      call read_db(rbuf, len)
      iad = 0
      iad = iad+1 ; eos%cv = rbuf(iad)
      iad = iad+1 ; eos%cp = rbuf(iad)
      deallocate( rbuf )
      
      ! read material title                      
      call read_c_c(name,nchartitle)             
      do i=1,nchartitle                          
        eos%title(i:i) = char(name(i))           
      end do                                     
      
      ! read eos parameter arrays          
      nuparam = eos%nuparam                      
      niparam = eos%niparam                      
      call my_alloc(eos%uparam ,nuparam)         
      call my_alloc(eos%iparam ,niparam)         

      if (nuparam > 0) then                      
        call read_db(eos%uparam  ,nuparam)       
      end if                                     
      if (niparam > 0) then                      
        call read_i_c(eos%iparam ,niparam)       
      end if                                     

      ! read eos law function                    
      numfunc  = eos%nfunc                       
      if (numfunc > 0) then                      
        allocate (eos%func(numfunc))             
        call read_i_c(eos%func, numfunc)         
      end if                                     

      ! read eos law tables                      
      numtabl  = eos%ntable                      
      if (numtabl > 0) then                      
        allocate (eos%table(numtabl))            
        call read_mat_table(eos%table, numtabl)  
      end if

!-----------
      return
      end
      end module read_eosparam_mod
