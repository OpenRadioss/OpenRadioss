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
!||    hm_read_work_hardening_tabulated_mod   ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening_tabulated.F90
!||--- called by ------------------------------------------------------
!||    hm_read_work_hardening                 ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening.F90
!||====================================================================
      module hm_read_work_hardening_tabulated_mod
! \brief Read tabulated work hardening input data for /MAT/LAW131
! \details Read the tabulated work hardening model parameters
!          for /MAT/LAW131.
      contains
!||====================================================================
!||    hm_read_work_hardening_tabulated   ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening_tabulated.F90
!||--- called by ------------------------------------------------------
!||    hm_read_work_hardening             ../starter/source/materials/mat/mat131/work_hardening/hm_read_work_hardening.F90
!||--- calls      -----------------------------------------------------
!||    hm_get_float_array_index           ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||    hm_get_floatv_dim                  ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
!||    hm_get_int_array_index             ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod                 ../starter/share/modules1/hm_option_read_mod.F
!||    submodel_mod                       ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_work_hardening_tabulated(                           &
          ikey     ,ihard    ,ntab_hard ,itab_hard ,x2vect   ,x3vect   ,       &
          x4vect   ,fscale   ,nvartmp   ,is_available,unitab ,lsubmodel,       &
          iout     ,is_encrypted,vpflag ,israte    ,parmat   ,npar_hard,       & 
          upar_hard)
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
          integer,                       intent(in)    :: ikey           !< Material key
          integer,                       intent(inout) :: ihard          !< Work hardening type
          integer,                       intent(inout) :: ntab_hard      !< Number of tabulated hardening functions/tables
          integer,       dimension(100), intent(inout) :: itab_hard      !< Identifiers of tabulated hardening functions/tables
          real(kind=WP), dimension(100), intent(inout) :: x2vect         !< x2 scale factor for tabulated hardening
          real(kind=WP), dimension(100), intent(inout) :: x3vect         !< x3 scale factor for tabulated hardening
          real(kind=WP), dimension(100), intent(inout) :: x4vect         !< x4 scale factor for tabulated hardening
          real(kind=WP), dimension(100), intent(inout) :: fscale         !< y  scale factor for tabulated hardening
          integer,                       intent(inout) :: nvartmp        !< number of variables used in tabulated hardening
          logical,                       intent(in)    :: is_available   !< Availability flag
          type(unit_type_),              intent(in)    :: unitab         !< Units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel !< Submodel data structure
          integer,                       intent(in)    :: iout           !< Output unit
          logical,                       intent(in)    :: is_encrypted   !< Encryption flag
          integer,                       intent(inout) :: vpflag         !< Strain rate dependency flag
          integer,                       intent(inout) :: israte         !< Strain rate filtering flag
          real(kind=WP), dimension(100), intent(inout) :: parmat         !< Material parameters
          integer,                       intent(inout) :: npar_hard      !< Number of work hardening parameters
          real(kind=WP), dimension(100), intent(inout) :: upar_hard      !< Work hardening parameters
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
          integer :: tab_id,flat
          real(kind=WP) :: xscale,yscale,fcut
!===============================================================================
!       
          !===================================================================
          !< Tabulated hardening parameters
          !===================================================================
          call hm_get_int_array_index  ("HARD_TAB_ID"    ,tab_id,ikey,is_available,lsubmodel)
          call hm_get_int_array_index  ("HARD_TAB_EXTRAP",flat  ,ikey,is_available,lsubmodel)
          call hm_get_float_array_index("HARD_TAB_XSCALE",xscale,ikey,is_available,lsubmodel,unitab)
          call hm_get_float_array_index("HARD_TAB_YSCALE",yscale,ikey,is_available,lsubmodel,unitab)
          call hm_get_int_array_index  ("SRATE_VFLAG"    ,vpflag,ikey,is_available,lsubmodel)
          call hm_get_float_array_index("SRATE_FCUT"     ,fcut  ,ikey,is_available,lsubmodel,unitab)
          !< Work hardening type
          ihard = 3
          !< Check default values
          if (xscale == zero) then 
            call hm_get_floatv_dim('HARD_TAB_XSCALE',xscale,is_available,lsubmodel,unitab)
          endif
          if (yscale == zero) then
            call hm_get_floatv_dim('HARD_TAB_YSCALE',yscale,is_available,lsubmodel,unitab)
          endif
          !< Number of tabulated hardening functions/tables
          ntab_hard = 1
          !< Number of variables used in tabulated hardening
          nvartmp = 2
          !< Flat extrapolation flag
          flat = min(max(flat,0),1)
          npar_hard = 1
          upar_hard(1) = flat
          !< Save table id
          itab_hard(1) = tab_id
          !< Save scale factors
          x2vect(1) = xscale
          x3vect(1) = one
          x4vect(1) = one
          fscale(1) = yscale
          !< Viscous formulation
          vpflag = min(max(vpflag,0),3)
          if (vpflag == 0) vpflag = 1
          israte = 1
          if (fcut   == zero) fcut = 10000.0d0*unitab%fac_t_work
          !< Parmat global table
          parmat(4) = israte
          parmat(5) = fcut
          !< Printing work hardening parameters
          if (is_encrypted)then
            write(iout,"(5X,A,//)") "CONFIDENTIAL DATA"
          else
            write(iout,1000) tab_id,xscale,yscale,vpflag,fcut,flat
          endif
! ------------------------------------------------------------------------------
1000 format(/                                                                  &
          5X,"-------------------------------------------------------",/       &
          5X,"TABULATED WORK HARDENING                               ",/,      &
          5X,"-------------------------------------------------------",/,      &
          5X,"WORK HARDENING TABULATED IDENTIFIER (TAB_ID) . . . . .=",I10/&
          5X,"STRAIN RATE SCALE FACTOR (SRATE_XSCALE). . . . . . . .=",1PG20.13/&
          5X,"YIELD STRESS SCALE FACTOR (YSCALE) . . . . . . . . . .=",1PG20.13/&
          5X,'VISCOPLASTIC FLAG (VP) . . . . . . . . . . . . . . . .=',I10/,   &
          5X,'    = 1: SCALED YIELD STRESS WITH PLASTIC STRAIN RATE   ',/,     &
          5X,'    = 2: SCALED YIELD STRESS WITH TOTAL STRAIN RATE     ',/,     &
          5X,'    = 3: SCALED YIELD STRESS WITH DEVIATORIC STRAIN RATE',/,     &
          5X,'STRAIN RATE FILTERING FREQUENCY (FCUT) . . . . . . . .=',1PG20.13/&
          5X,'CURVE EXTRAPOLATION FLAG (FLAT_EXTRAP) . . . . . . . .=',I10/,   &
          5X,'    = 0: LINEAR EXTRAPOLATION                          ',/,      &     
          5X,'    = 1: FLAT   EXTRAPOLATION                          ',/)
! -------------------------------------------------------------------------------
        end subroutine hm_read_work_hardening_tabulated
      end module hm_read_work_hardening_tabulated_mod