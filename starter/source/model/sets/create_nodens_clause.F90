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
      module create_nodens_clause_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!\brief This subroutine creates a clause from nodens
!=======================================================================================================================
      subroutine create_nodens_clause(clause ,itabm1 ,jclause ,is_available ,lsubmodel ,numnod)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use SETDEF_MOD
          use SUBMODEL_MOD
          use MESSAGE_MOD
          use HM_OPTION_READ_MOD
          use OPTIONDEF_MOD
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
          integer,                                   intent(in) :: jclause                        !< parameter with HM_READER (current clause read)                      !< treat the NODENS Clause, read NODENS from HM_READER & fill clause
          logical,                                   intent(inout) :: is_available                   !< Bool / Result of HM_interface
          integer,                                   intent(in) :: numnod                         !< total nb of model nodes
          integer,                                   intent(in)    :: itabm1(NUMNOD,2)               !< MAP Table UID -> LocalID
          type(SET_),                                intent(inout) :: clause                         !< (SET structure) Clause to be treated
          type(SUBMODEL_DATA),                       intent(in) :: lsubmodel(nsubmod)                !< SUBMODEL Structure
          integer set_usrtos
          external set_usrtos
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,ids,nodsys                 !< local entities id's
          integer :: nindx                        !< counting entities (NODENS)
          integer :: ids_max                      !< nb of input clause NODENS
          integer :: list_size                    !< size of the final clause of NODENS
          integer, allocatable, dimension(:) :: nodens_read_tmp !< temporary NODENS storage array
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
          call hm_get_int_array_index('idsmax' ,ids_max ,jclause,is_available,lsubmodel)

          allocate(nodens_read_tmp(ids_max))
          nodens_read_tmp(1:ids_max) = 0

          nindx = 0
          list_size = 0

!         Read & convert Nodens
!         ---------------------
          do i=1,ids_max
            call hm_get_int_array_2indexes('ids',ids,jclause,i,is_available,lsubmodel)
            nodsys = set_usrtos(ids,itabm1,numnod)
            if(nodsys == 0)then      
!             Nodens was not found. Issue a Warning & Skip.
              call ancmsg(msgid=1902,anmode=aninfo,msgtype=msgwarning,i1= clause%set_id,i2=ids,c1=trim(clause%title),c2='NODENS')
            else
              nodsys = itabm1(nodsys,2)
              nindx = nindx+1    !   nb of CLAUSE nodens
              nodens_read_tmp(nindx) = nodsys
            endif
          enddo

          list_size = nindx

!         Copy in final SET
!         ------------------
          clause%nb_nodens = list_size  
          allocate(clause%nodens(list_size))

          do i=1,list_size
            clause%nodens(i) = nodens_read_tmp(i)
          enddo
!
          deallocate(nodens_read_tmp)
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine create_nodens_clause
      end module create_nodens_clause_mod