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
!||    iniebcs_propellant_   ../starter/source/boundary_conditions/ebcs/iniebcs_propellant.F90
!||--- called by ------------------------------------------------------
!||    lectur                ../starter/source/starter/lectur.F
!||====================================================================
      module iniebcs_propellant_
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!||====================================================================
!||    iniebcs_propellant          ../starter/source/boundary_conditions/ebcs/iniebcs_propellant.F90
!||--- called by ------------------------------------------------------
!||    lectur                      ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    iniebcs_propellant_get_cp   ../starter/source/boundary_conditions/ebcs/iniebcs_propellant.F90
!||--- uses       -----------------------------------------------------
!||    message_mod                 ../starter/share/message_module/message_mod.F
!||====================================================================
        subroutine iniebcs_propellant(ixs,ixq,ixtg,multi_fvm_is_used,ebcs_tab,mat_param,sixs,sixq,sixtg,nummat)
!! \brief For option /EBCS/PROPERGOL, need to get from adjacent EoS Cp parameter
!! \details this Cp parameter allows to determine q_combustion=Cp.T_combustion
!! \details U=Cv.T_combustion.dm if an advection step is used (dvol is then incremented from EBCS ghost cell and treated later in EOS updates dE=-PdV)
!! \details When using a source term, H=U+W=Cv.T_combustion+PdV=...=Cp.T_combustion.dm  (the work is then automatically included)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          ! use groupdef_mod
          use message_mod
          use ale_ebcs_mod
          use ebcs_mod
          use matparam_def_mod, only : matparam_struct_
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
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
          INTEGER :: II !< loop
          INTEGER :: TYP,ISU !< ebcs data
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
              !/EBCS/PROPERGOL (TYP=11) : retrieve Cp parameter in adjacent (sub)material
              !  warn user if adjacent elem is not matching an Ideal Gas EoS
              select type (twf => ebcs_tab%tab(ii)%poly)
               type is (t_ebcs_propellant)
                call iniebcs_propellant_get_cp(twf,mat_param,nummat,twf%title,ixs,ixq,ixtg,sixs,sixq,sixtg)
              end select
            end if
          end do
        end subroutine iniebcs_propellant

! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief For option /EBCS/PROPERGOL,loop over adjacent elems and get cp parameter
!! \details in case of different values are detected, use median one (ignition may be starter with a high density gas : few elems only)
!||====================================================================
!||    iniebcs_propellant_get_cp   ../starter/source/boundary_conditions/ebcs/iniebcs_propellant.F90
!||--- called by ------------------------------------------------------
!||    iniebcs_propellant          ../starter/source/boundary_conditions/ebcs/iniebcs_propellant.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                      ../starter/source/output/message/message.F
!||--- uses       -----------------------------------------------------
!||    message_mod                 ../starter/share/message_module/message_mod.F
!||====================================================================
        subroutine iniebcs_propellant_get_cp(ebcs,mat_param,nummat,title,ixs,ixq,ixtg,sixs,sixq,sixtg)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use ebcs_mod
          use matparam_def_mod, only : matparam_struct_
          use message_mod
          use names_and_titles_mod , only : nchartitle
          use constant_mod , only : em06, zero, one
          use array_reindex_mod, only : real_array_reindex
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include      "elements.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(t_ebcs_propellant), target, intent(inout) :: ebcs                !< ebcs propellant data structure
          integer,intent(in) :: nummat                                         !< number of material law (size for mat_param data structure)
          type(matparam_struct_) ,dimension(nummat) ,intent(in) :: mat_param   !< data structure for material parameters
          character(len=nchartitle) :: title
          integer,intent(in) :: sixs,sixq,sixtg
          integer,intent(in) :: ixs(nixs,sixs/nixs),ixq(nixq,sixq/nixq),ixtg(nixtg,sixtg/nixtg)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: EOSid, imat, mlw                    !< material & eos data
          integer :: kk, icell                           !< loop
          real(kind=WP) :: Cp0,Cp,Cv                     !< Specific Heat parameter (Cp0 : first segment)
          real(kind=WP) :: gamma
          real(kind=WP),allocatable,dimension(:) :: tmp,tmp2  !< Cp parameters for each segment
          integer,allocatable,dimension(:) :: indx       !< array for sorting algorithm
          logical :: MULTIPLE_CP_DETECTED
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate (tmp(ebcs%nb_elem))
          allocate (tmp2(ebcs%nb_elem))
          allocate (indx(ebcs%nb_elem))
          do kk = 1, ebcs%nb_elem
            indx(kk) = kk
          end do
          tmp(1:) = zero
          tmp2(1:) = zero
          MULTIPLE_CP_DETECTED = .false.
          imat = 0
          cp = zero
          cv = zero
          gamma = one

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
            Cp0=Cp
            eosid = mat_param(imat)%ieos
            cp = mat_param(imat)%eos%cp
            cv = mat_param(imat)%eos%cv
            tmp(kk) = Cp
            if(Cv > zero)then
              tmp2(kk) = Cp/Cv
              gamma = Cp/Cv
            end if
            if(kk==1)Cp0=Cp

            if(abs(Cp0-Cp)/Cp0 > em06 )then
              MULTIPLE_CP_DETECTED = .TRUE.
            end if

            if(eosid /= 7)then
              CALL ANCMSG(MSGID = 1602, MSGTYPE = MSGERROR, ANMODE = ANINFO, &
                I1 = ebcs%ebcs_id, C1 = title(1:len_trim(title)), &
                C2 = "EBCS PROPERGOL ONLY COMPATIBLE WITH IDEAL-GAS EOS")
            end if

          enddo

          if (ebcs%nb_elem >=2 .and. multiple_cp_detected)then
            call real_array_reindex(tmp, indx, ebcs%nb_elem)
            Cp = tmp( int(ebcs%nb_elem/2) )     !median value
            gamma = tmp2( int(ebcs%nb_elem/2) )     !median value
            call ancmsg(msgid = 3083, msgtype = msgwarning, anmode = aninfo, &
              i1 = ebcs%ebcs_id, c1 = title(1:len_trim(title)), &
              C2 = "EBCS PROPERGOL IS FACING DIFFERENT GAS EOS : CHECK Cp PARAMETER (Cp=GAMMA.E0/RHO0/T0)", &
              C3 = "RETAINED Cp VALUE FROM EOS ID :", &
              I2 = MAT_param(imat)%mat_id )
          end if

          !Setting Heat of combustion (q)
          if(ebcs%ienthalpy == -1) then
            ebcs%q = Cv * ebcs%t
          else
            ebcs%q = Cp * ebcs%t
          end if
          ebcs%gamma = gamma



          if (allocated(tmp)) deallocate(tmp)
          if (allocated(tmp2)) deallocate(tmp2)
          if (allocated(indx)) deallocate(indx)

        end subroutine iniebcs_propellant_get_cp


! ----------------------------------------------------------------------------------------------------------------------
      end module iniebcs_propellant_

