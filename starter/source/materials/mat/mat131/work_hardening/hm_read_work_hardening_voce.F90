!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    hm_read_work_hardening_voce_mod   ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening_voce.F90
!||--- called by ------------------------------------------------------
!||    hm_read_work_hardening            ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening.F90
!||====================================================================
      module hm_read_work_hardening_voce_mod
! \brief Read Voce work hardening input data for /MAT/LAW131
! \details Read the Voce work hardening model parameters
!          for /MAT/LAW131.
      contains
!||====================================================================
!||    hm_read_work_hardening_voce   ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening_voce.F90
!||--- called by ------------------------------------------------------
!||    hm_read_work_hardening        ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening.F90
!||--- calls      -----------------------------------------------------
!||    hm_get_float_array_index      ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod            ../starter/share/modules1/hm_option_read_mod.F
!||    submodel_mod                  ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_work_hardening_voce(                                &
          ikey     ,ihard    ,nupar_hard,upar_hard,is_available,               &
          unitab   ,lsubmodel,iout     ,is_encrypted)
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
          use unitab_mod
          use submodel_mod
          use hm_option_read_mod
          use constant_mod
          use matparam_def_mod
          use precision_mod, only : WP
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
          implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
          integer,                 intent(in)    :: ikey                  !< material key
          integer,                 intent(inout) :: ihard                 !< work hardening type
          integer,                 intent(inout) :: nupar_hard            !< number of work hardening parameters
          real(kind=WP),dimension(100),intent(inout) :: upar_hard         !< work hardening parameters
          logical,                 intent(in)    :: is_available          !< availability flag
          type(unit_type_),        intent(in)    :: unitab                !< units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel  !< submodel data structure
          integer,                 intent(in)    :: iout                  !< output unit
          logical,                 intent(in)    :: is_encrypted          !< encryption flag
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
          real(kind=WP) :: r0,q1,b1,q2,b2,q3,b3
!===============================================================================
!         
          !===================================================================
          !< Voce hardening parameters
          !===================================================================
          call hm_get_float_array_index("HARD_VOCE_R0",r0,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("HARD_VOCE_Q1",q1,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("HARD_VOCE_B1",b1,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("HARD_VOCE_Q2",q2,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("HARD_VOCE_B2",b2,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("HARD_VOCE_Q3",q3,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("HARD_VOCE_B3",b3,ikey,is_available,lsubmodel,unitab)
          !< Work hardening type
          ihard = 2
          !< Number of parameters
          nupar_hard = 7
          !< Save hardening parameters
          upar_hard(1) = r0
          upar_hard(2) = q1
          upar_hard(3) = b1
          upar_hard(4) = q2
          upar_hard(5) = b2
          upar_hard(6) = q3
          upar_hard(7) = b3
          !< Printing work hardening parameters
          if (is_encrypted)then
            write(iout,"(5X,A,//)") "CONFIDENTIAL DATA"
          else
            write(iout,1000) r0,q1,b1,q2,b2,q3,b3
          endif
! ------------------------------------------------------------------------------
1000 format(/                                                                  &
          5X,"-------------------------------------------------------",/       &
          5X,"VOCE WORK HARDENING                                    ",/,      &
          5X,"-------------------------------------------------------",/,      &
          5X,"INITIAL YIELD STRESS (R0). . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"VOCE 1 SATURATION STRESS (Q1). . . . . . . . . . . . .=",1PG20.13/&
          5X,"VOCE 1 SATURATION RATE (B1). . . . . . . . . . . . . .=",1PG20.13/&
          5X,"VOCE 2 SATURATION STRESS (Q2). . . . . . . . . . . . .=",1PG20.13/&
          5X,"VOCE 2 SATURATION RATE (B2). . . . . . . . . . . . . .=",1PG20.13/&
          5X,"VOCE 3 SATURATION STRESS (Q3). . . . . . . . . . . . .=",1PG20.13/&
          5X,"VOCE 3 SATURATION RATE (B3). . . . . . . . . . . . . .=",1PG20.13/)
! -------------------------------------------------------------------------------
        end subroutine hm_read_work_hardening_voce
      end module hm_read_work_hardening_voce_mod