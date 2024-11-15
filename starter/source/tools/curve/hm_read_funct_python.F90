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
      !||    read_funct_python_mod   ../starter/source/tools/curve/hm_read_funct_python.F90
      !||--- called by ------------------------------------------------------
      !||    lectur                  ../starter/source/starter/lectur.F
      !||====================================================================
      module read_funct_python_mod
      contains
!! \details Read the python function defined by /FUNCT_PYTHON/
      !||====================================================================
      !||    hm_read_funct_python      ../starter/source/tools/curve/hm_read_funct_python.F90
      !||--- called by ------------------------------------------------------
      !||    lectur                    ../starter/source/starter/lectur.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                    ../starter/source/output/message/message.F
      !||    hm_get_intv               ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_get_string_index       ../starter/source/devtools/hm_reader/hm_get_string_index.F
      !||    hm_option_count           ../starter/source/devtools/hm_reader/hm_option_count.F
      !||    hm_option_read_key        ../starter/source/devtools/hm_reader/hm_option_read_key.F
      !||    hm_option_start           ../starter/source/devtools/hm_reader/hm_option_start.F
      !||--- uses       -----------------------------------------------------
      !||    hm_option_read_mod        ../starter/share/modules1/hm_option_read_mod.F
      !||    message_mod               ../starter/share/message_module/message_mod.F
      !||    submodel_mod              ../starter/share/modules1/submodel_mod.F
      !||    table_mod                 ../starter/share/modules1/table_mod.F
      !||====================================================================
        subroutine hm_read_funct_python(python,npc,snpc,total_nb_funct,&
        &lsubmodel,nbsubmod, pld, npts, table, ntable)
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                      modules
! ----------------------------------------------------------------------------------------------------------------------
          USE TABLE_MOD
          USE MESSAGE_MOD
          USE SUBMODEL_MOD
          USE HM_OPTION_READ_MOD
          USE PYTHON_FUNCT_MOD
          USE NAMES_AND_TITLES_MOD, only : ncharline, nchartitle
          use iso_c_binding , only : c_char, c_null_char
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nbsubmod !< number of submodels
          type(submodel_data), intent(in)  :: lsubmodel(nbsubmod) !< submodel data
          type(python_), intent(inout) :: python !< python functions
          integer, intent(in) :: snpc !< size of npc
          integer, intent(inout) :: npc(snpc) !< array containing the id the /FUNCT , /TABLE and so on...
          integer, intent(inout) :: total_nb_funct !< number of /FUNCT already read
          integer, intent(in)   ::  npts !< number of points in pld
          my_real, dimension(npts), intent(inout) :: pld !< data points
          integer, intent(in) :: ntable !< number of tables
          type(Ttable), dimension(ntable), intent(inout) :: table
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          character(len=max_line_length) :: rline
          logical :: is_available
          integer :: nlines
          integer :: nb_funct
          integer :: i,j,l,ipt
          integer :: func_id
          integer :: position_in_code
          character(kind=c_char, len=:), allocatable :: code
          integer :: line_len
          integer :: error,error_old
          double precision :: argin(1), argout(1)

!         character(len=:), allocatable :: titr !function name
          character(len=nchartitle) :: titr !function name
          double precision :: XX(funct_python_nsamples)
          double precision :: YY(funct_python_nsamples)

! ----------------------------------------------------------------------------------------------------------------------
!                                                      body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(character(kind=c_char, len=max_code_length) :: code)
          call hm_option_count('/FUNCT_PYTHON', nb_funct)
          allocate(python%functs(nb_funct))
          python%nb_functs = nb_funct
          position_in_code = 1
          ! the total number of functions minus the number of python function is the offset into npc
          python%funct_offset = total_nb_funct - nb_funct
          if(nb_funct > 0) then
            error_old = python_error
            if(python_error > 0) then
              call ancmsg(MSGID=3039,&
              &MSGTYPE=MSGERROR,&
              &ANMODE=ANINFO_BLIND_2)
            endif

            call python_initialize(python_error)
            call python_load_environment()
            call hm_option_start('/FUNCT_PYTHON')
            do i = 1, nb_funct
              !fill code with spaces:
              code(1:max_code_length) = repeat(' ',max_code_length)
              call hm_option_read_key(lsubmodel, option_id = func_id)
              call hm_get_intv('Number_of_datalines' ,nlines ,is_available, lsubmodel)
              python%functs(i)%num_lines = nlines
              python%functs(i)%user_id = func_id
!             write(6,*) "Python test: funct_id",func_id,"nlines",nlines
              position_in_code = 1
              if(nlines > 0) then
                ! create tempo file
                do j=1,nlines
                  call hm_get_string_index('arraydatalines', rline, j, max_line_length, is_available)
!              write(6,fmt='(a)') trim(rline)
                  !append trim(rline) to "code"
                  line_len = len_trim(rline)
                  code(position_in_code:position_in_code+line_len-1) = trim(rline)
                  ! add c_null_char
                  position_in_code = position_in_code + line_len
                  code(position_in_code:position_in_code) = c_null_char
                  position_in_code = position_in_code + 1
                enddo
                l = i + python%funct_offset
                npc(l + 1) = npc(l)
                npc(total_nb_funct + l + 1) = func_id
                npc(2 * total_nb_funct + l + 1) = -i
!               write(6,*) "Python test: code",code(1:position_in_code-1)
                call python_funct_init(python%functs(i), code, position_in_code, nlines)
!               write(6,*) "Check python function"
                call python_check_function(python%functs(i)%name,error)
                if(error > 0 .and. error_old == 0) then
                  ! converts python%functs(i)%name of type  character(kind=c_char), dimension(:), allocatable :: name
                  ! initialize titr with "/FUNCT_PYTHON"
                  titr = repeat(' ',nchartitle)
                  call ancmsg(MSGID=3038,&
                  &MSGTYPE=MSGERROR,&
                  &ANMODE=ANINFO_BLIND_2,&
                  &I1=func_id)
                endif
                table(l)%notable= func_id
                table(l)%ndim = -1
                allocate(table(l)%X(1))
                allocate(table(l)%Y)
                allocate(table(l)%X(1)%values(funct_python_nsamples))
                allocate(table(l)%Y%values(funct_python_nsamples))
                call python_sample_function(python%functs(i)%name,XX,YY,funct_python_nsamples)

                do ipt =1, funct_python_nsamples
                  !write(6,*) ipt,"X",table(l)%X(1)%values(ipt),"Y",table(l)%Y%values(ipt)
                  table(l)%X(1)%values(ipt) = XX(ipt)                         
                  PLD(NPC(L+1)) = table(l)%X(1)%values(ipt)
                  NPC(L + 1) = NPC(L + 1) + 1 
                  table(l)%Y%values(ipt) = YY(ipt)
                  PLD(NPC(l+1)) = table(l)%Y%values(ipt)
                  NPC(L + 1) = NPC(L + 1) + 1 
                enddo
              else
              endif
            enddo
          endif
          deallocate(code)
          return

        end
! end the module
      end module read_funct_python_mod
