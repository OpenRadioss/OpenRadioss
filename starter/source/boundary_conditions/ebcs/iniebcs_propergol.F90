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
      !||    iniebcs_propergol_   ../starter/source/boundary_conditions/ebcs/iniebcs_propergol.F90
      !||--- called by ------------------------------------------------------
      !||    lectur               ../starter/source/starter/lectur.F
      !||====================================================================
      module iniebcs_propergol_
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief For option /EBCS/PROPERGOL, need to get from adjacent EoS Cv parameter
!! \details this Cv parameter allows to determine q_combustion=Cv.T_combustion
      !||====================================================================
      !||    iniebcs_propergol          ../starter/source/boundary_conditions/ebcs/iniebcs_propergol.F90
      !||--- called by ------------------------------------------------------
      !||    lectur                     ../starter/source/starter/lectur.F
      !||--- calls      -----------------------------------------------------
      !||    iniebcs_propergol_get_cv   ../starter/source/boundary_conditions/ebcs/iniebcs_propergol.F90
      !||--- uses       -----------------------------------------------------
      !||    message_mod                ../starter/share/message_module/message_mod.F
      !||====================================================================
      subroutine iniebcs_propergol(ixs,ixq,ixtg,multi_fvm_is_used,ebcs_tab,mat_param,sixs,sixq,sixtg,nummat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
       ! use groupdef_mod
      use message_mod
      use ale_ebcs_mod
      use ebcs_mod
      use matparam_def_mod, only : matparam_struct_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include      "my_real.inc"
#include      "elements.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer,intent(in) :: nummat                                                          !< number of material law (size for mat_param data structure)
      integer,intent(in) :: sixs,sixq,sixtg                                                 !<   array size
      integer,intent(in) :: ixs(nixs,sixs/nixs),ixq(nixq,sixq/nixq),ixtg(nixtg,sixtg/nixtg) !< element connectivity (brick,quad,triangles)
      logical, intent(in) :: multi_fvm_is_used                                              !< law151 buffer (collocated scheme)
      type(t_ebcs_tab), target, intent(inout) :: ebcs_tab                                   !< data structure for /EBCS options
      type(matparam_struct_) ,dimension(nummat) ,intent(in) :: mat_param                    !< data structure for material parameters
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
      INTEGER II !< loop
      INTEGER TYP,ISU !< ebcs data
      CLASS (T_EBCS), POINTER :: EBCS
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      do ii = 1, nebcs
        ebcs => ebcs_tab%tab(ii)%poly
        typ = ebcs%type
        isu = ebcs%surf_id
        if(multi_fvm_is_used )then; ;end if
        if(isu>0)then
          !/EBCS/PROPERGOL (TYP=11) : retrieve Cv parameter in adjacent (sub)material
          !  warn user if adjacent elem is not matching an Ideal Gas EoS
          select type (twf => ebcs_tab%tab(ii)%poly)
            type is (t_ebcs_propergol)
              call iniebcs_propergol_get_cv(twf,mat_param,nummat,twf%title,ixs,ixq,ixtg,sixs,sixq,sixtg)
          end select
         end if
      end do
      end subroutine iniebcs_propergol

! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief For option /EBCS/PROPERGOL,loop over adjacent elems and get cv parameter
!! \details in case of different values are detected, use median one (ignition may be starter with a high density gas : few elems only)
      !||====================================================================
      !||    iniebcs_propergol_get_cv   ../starter/source/boundary_conditions/ebcs/iniebcs_propergol.F90
      !||--- called by ------------------------------------------------------
      !||    iniebcs_propergol          ../starter/source/boundary_conditions/ebcs/iniebcs_propergol.F90
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                     ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod                ../starter/share/message_module/message_mod.F
      !||====================================================================
      subroutine iniebcs_propergol_get_cv(ebcs,mat_param,nummat,title,ixs,ixq,ixtg,sixs,sixq,sixtg)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use ebcs_mod
      use matparam_def_mod, only : matparam_struct_
      use message_mod
      use names_and_titles_mod , only : nchartitle
      use constant_mod , only : em06, zero
      use array_reindex_mod, only : real_array_reindex
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include      "my_real.inc"
#include      "elements.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      type(t_ebcs_propergol), target, intent(inout) :: ebcs                !< ebcs propergol data structure
      integer,intent(in) :: nummat                                         !< number of material law (size for mat_param data structure)
      type(matparam_struct_) ,dimension(nummat) ,intent(in) :: mat_param   !< data strucutre for material parameters
      character(len=nchartitle) :: title
      integer,intent(in) :: sixs,sixq,sixtg
      integer,intent(in) :: ixs(nixs,sixs/nixs),ixq(nixq,sixq/nixq),ixtg(nixtg,sixtg/nixtg)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
      integer :: EOSid, imat, mlw    !< material & eos data
      integer kk, icell              !< loop
      my_real :: T_combust           !< parameter for combustion model
      my_real :: Cv0,Cv              !< Specific Heat parameter (Cv0 : first segment)
      my_real,allocatable,dimension(:) :: tmp  !< Cv parameters for each segment
      integer,allocatable,dimension(:) :: indx !< array for sorting algorithm
      logical :: MULTIPLE_CV_DETECTED
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
              allocate (tmp(ebcs%nb_elem))
              allocate (indx(ebcs%nb_elem))
              do kk = 1, ebcs%nb_elem
               indx(kk) = kk
              end do
              tmp(1:) = zero
              MULTIPLE_CV_DETECTED = .false.

              do kk=1,ebcs%nb_elem

                icell = ebcs%ielem(kk)
                if(ebcs%itype(kk) == 8)then
                  imat = ixs(1,icell)
                elseif(ebcs%itype(kk) == 4)then
                  imat = ixq(1,icell)
                elseif(ebcs%itype(kk) == 3)then
                  imat = ixtg(1,icell)
                endif

                !multimaterial case
                mlw = mat_param(imat)%ilaw
                if(mlw == 51 .or. mlw == 151) then
                   imat = mat_param(imat)%multimat%mid( ebcs%submat_id )
                endif

                !eos parameters
                Cv0=Cv
                eosid = mat_param(imat)%ieos
                cv = mat_param(imat)%eos%cv
                tmp(kk) = Cv
                if(kk==1)Cv0=Cv

                if(abs(Cv0-Cv)/Cv0 > em06 )then
                   MULTIPLE_CV_DETECTED = .TRUE.
                end if

                T_combust = ebcs%q

                if(eosid /= 7)then
                  CALL ANCMSG(MSGID = 1602, MSGTYPE = MSGERROR, ANMODE = ANINFO, &
                         I1 = ebcs%ebcs_id, C1 = title(1:len_trim(title)), &
                         C2 = "EBCS PROPERGOL ONLY COMPATIBLE WITH IDEAL-GAS EOS")
                end if

              enddo

              if (ebcs%nb_elem >=2 .and. multiple_cv_detected)then
                call real_array_reindex(tmp, indx, ebcs%nb_elem)
                Cv = tmp( int(ebcs%nb_elem/2) )     !median value
                  call ancmsg(msgid = 3083, msgtype = msgwarning, anmode = aninfo, &
                         i1 = ebcs%ebcs_id, c1 = title(1:len_trim(title)), &
                         C2 = "EBCS PROPERGOL IS FACING DIFFERENT GAS EOS : CHECK Cv PARAMETER (Cv=E0/RHO0/T0)", &
                         C3 = "RETAINED Cv VALUE FROM EOS ID :", &
                         I2 = MAT_param(imat)%mat_id )
              end if

              !Setting Heat of combustion (q)
              ebcs%q = Cv * ebcs%q    !q = Cv * Tcomb

              if (allocated(tmp)) deallocate(tmp)
              if (allocated(indx)) deallocate(indx)

      end subroutine iniebcs_propergol_get_cv


! ----------------------------------------------------------------------------------------------------------------------
      end module iniebcs_propergol_

