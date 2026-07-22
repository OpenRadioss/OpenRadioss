!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
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
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    hm_read_work_hardening_zerilli_armstrong_mod   ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening_zerilli_armstrong.F90
!||--- called by ------------------------------------------------------
!||    hm_read_work_hardening                         ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening.F90
!||====================================================================
      module hm_read_work_hardening_zerilli_armstrong_mod
! \brief Read Zerilli-Armstrong work hardening input data for /MAT/LAW131
! \details Read the Zerilli-Armstrong (FCC/BCC) work hardening model
!          parameters for /MAT/LAW131.
      contains
!||====================================================================
!||    hm_read_work_hardening_zerilli_armstrong   ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening_zerilli_armstrong.F90
!||--- called by ------------------------------------------------------
!||    hm_read_work_hardening                     ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening.F90
!||--- calls      -----------------------------------------------------
!||    hm_get_float_array_index                   ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||    hm_get_floatv_dim                          ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod                               ../starter/share/modules1/elbuftag_mod.F
!||    hm_option_read_mod                         ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod                                ../starter/share/message_module/message_mod.F
!||    submodel_mod                               ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_work_hardening_zerilli_armstrong(                   &
          ikey     ,ihard    ,nupar_hard,upar_hard,is_available,               &
          unitab   ,lsubmodel,iout      ,is_encrypted,matparam ,               &
          iflag    ,mtag     )
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
          use unitab_mod
          use submodel_mod
          use hm_option_read_mod
          use constant_mod
          use matparam_def_mod
          use precision_mod, only : WP
          use message_mod
          use elbuftag_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
          implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
          integer,                 intent(in)    :: ikey                  !< Material key
          integer,                 intent(inout) :: ihard                 !< Work hardening type
          integer,                 intent(inout) :: nupar_hard            !< Number of work hardening parameters
          real(kind=WP),dimension(100),intent(inout) :: upar_hard         !< Work hardening parameters
          logical,                 intent(in)    :: is_available          !< Availability flag
          type(unit_type_),        intent(in)    :: unitab                !< Units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel  !< Submodel data structure
          integer,                 intent(in)    :: iout                  !< Output unit
          logical,                 intent(in)    :: is_encrypted          !< Encryption flag
          type(matparam_struct_),  intent(inout) :: matparam              !< Matparam data structure
          integer,                 intent(in)    :: iflag                 !< Flag for input format (1: FCC, 2: BCC)
          type(mlaw_tag_),         intent(inout) :: mtag                  !< Material tag for internal variables in element buffer
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
          real(kind=WP) :: c0,c1,c2,c3,c4,c5,n,eref
!===============================================================================
!
          !=====================================================================
          !< Zerilli-Armstrong hardening parameters
          !=====================================================================
          c0 = -HUGE(c0)
          c1 = -HUGE(c1)
          c2 = -HUGE(c2)
          c3 = -HUGE(c3)
          c4 = -HUGE(c4)
          c5 = -HUGE(c5)
          !< Work hardening type
          ihard = 5
          !< Flag for material type (1: FCC, 2: BCC)
          upar_hard(1) = iflag
          !< FCC materials hardening parameters
          if (iflag == 1) then
            call hm_get_float_array_index("HARD_ZERILLI_C0",c0,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("HARD_ZERILLI_C2",c2,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("HARD_ZERILLI_C3",c3,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("HARD_ZERILLI_C4",c4,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("HARD_ZERILLI_N" ,n ,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("HARD_ZERILLI_EREF",eref,ikey,is_available,lsubmodel,unitab)
            !< Check default values
            if (eref == zero) then 
              call hm_get_floatv_dim('HARD_ZERILLI_EREF',eref,is_available,lsubmodel,unitab)
            endif
            !< Number of parameters
            nupar_hard   = 7
            upar_hard(2) = c0
            upar_hard(3) = c2
            upar_hard(4) = c3
            upar_hard(5) = c4
            upar_hard(6) = n
            upar_hard(7) = eref
          !< BCC materials hardening parameters
          elseif (iflag == 2) then
            call hm_get_float_array_index("HARD_ZERILLI_C0",c0,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("HARD_ZERILLI_C1",c1,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("HARD_ZERILLI_C3",c3,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("HARD_ZERILLI_C4",c4,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("HARD_ZERILLI_C5",c5,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("HARD_ZERILLI_N" ,n ,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("HARD_ZERILLI_EREF",eref,ikey,is_available,lsubmodel,unitab)
            !< Check default values
            if (eref == zero) then 
              call hm_get_floatv_dim('HARD_ZERILLI_EREF',eref,is_available,lsubmodel,unitab)
            endif
            !< Number of parameters
            nupar_hard   = 8
            upar_hard(2) = c0
            upar_hard(3) = c1
            upar_hard(4) = c3
            upar_hard(5) = c4
            upar_hard(6) = c5
            upar_hard(7) = n
            upar_hard(8) = eref
          endif
          !< Set temperature variable tag
          if (mtag%g_temp == 0) mtag%g_temp = 1
          if (mtag%l_temp == 0) mtag%l_temp = 1
          !< Activate heat source calculation in material
          if (matparam%heat_flag == 0) matparam%heat_flag = 1          
          !< Printing work hardening parameters
          if (is_encrypted) then
            write(iout,"(5X,A,//)") "CONFIDENTIAL DATA"
          else
            if (iflag == 1) then
              write(iout,1000) c0,c2,c3,c4,n,eref
            elseif (iflag == 2) then
              write(iout,2000) c0,c1,c3,c4,c5,n,eref
            endif
          endif
! ------------------------------------------------------------------------------
1000      format(/                                                               &
            5X,"-------------------------------------------------------",/       &
            5X,"ZERILLI-ARMSTRONG FCC WORK HARDENING                   ",/,      &
            5X,"-------------------------------------------------------",/,      &
            5X,"INITIAL YIELD STRESS (C0). . . . . . . . . . . . . . .=",1PG20.13/&
            5X,"HARDENING MODULUS (C2) . . . . . . . . . . . . . . . .=",1PG20.13/&
            5X,"THERMAL SOFTENING PARAMETER (C3) . . . . . . . . . . .=",1PG20.13/&
            5X,"THERMAL-STRAIN RATE PARAMETER (C4) . . . . . . . . . .=",1PG20.13/&
            5X,"HARDENING EXPONENT (N) . . . . . . . . . . . . . . . .=",1PG20.13/&
            5X,"REFERENCE STRAIN RATE (EREF) . . . . . . . . . . . . .=",1PG20.13/)
2000      format(/                                                               &
            5X,"-------------------------------------------------------",/       &
            5X,"ZERILLI-ARMSTRONG BCC WORK HARDENING                   ",/,      &
            5X,"-------------------------------------------------------",/,      &
            5X,"INITIAL YIELD STRESS (C0). . . . . . . . . . . . . . .=",1PG20.13/&
            5X,"THERMAL-RATE DEPENDENCY MODULUS (C1) . . . . . . . . .=",1PG20.13/&
            5X,"THERMAL SOFTENING PARAMETER (C3) . . . . . . . . . . .=",1PG20.13/&
            5X,"THERMAL-STRAIN RATE PARAMETER (C4) . . . . . . . . . .=",1PG20.13/&
            5X,"HARDENING MODULUS (C5) . . . . . . . . . . . . . . . .=",1PG20.13/&
            5X,"HARDENING EXPONENT (N) . . . . . . . . . . . . . . . .=",1PG20.13/&
            5X,"REFERENCE STRAIN RATE (EREF) . . . . . . . . . . . . .=",1PG20.13/)
! -------------------------------------------------------------------------------
        end subroutine hm_read_work_hardening_zerilli_armstrong
      end module hm_read_work_hardening_zerilli_armstrong_mod
