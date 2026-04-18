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
!||    hm_read_yield_criterion_hill_mod   ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion_hill.F90
!||--- called by ------------------------------------------------------
!||    hm_read_yield_criterion            ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion.F90
!||====================================================================
      module hm_read_yield_criterion_hill_mod
        implicit none
! \brief Read Hill yield criterion input data for /MAT/LAW131
! \details Read the Hill 1948 anisotropic yield criterion parameters
!          for /MAT/LAW131.
      contains
!||====================================================================
!||    hm_read_yield_criterion_hill   ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion_hill.F90
!||--- called by ------------------------------------------------------
!||    hm_read_yield_criterion        ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion.F90
!||--- calls      -----------------------------------------------------
!||    hm_get_float_array_index       ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||    hm_get_int_array_index         ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod             ../starter/share/modules1/hm_option_read_mod.F
!||    submodel_mod                   ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_yield_criterion_hill(                               &
          ikey     ,icrit    ,nupar_crit,upar_crit,is_available,unitab   ,     &
          lsubmodel,iout     ,is_encrypted,iflag  )
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
          integer,                 intent(in)    :: ikey                  !< Material key
          integer,                 intent(inout) :: icrit                 !< Yield criterion type
          integer,                 intent(inout) :: nupar_crit            !< Number of yield criterion parameters
          real(kind=WP),dimension(100),intent(inout) :: upar_crit         !< Yield criterion parameters
          logical,                 intent(in)    :: is_available          !< Availability flag
          type(unit_type_),        intent(in)    :: unitab                !< Units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel  !< Submodel data structure
          integer,                 intent(in)    :: iout                  !< Output unit
          logical,                 intent(in)    :: is_encrypted          !< Encryption flag
          integer,                 intent(in)    :: iflag                 !< Input flag for Hill parameters
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
          integer :: idir
          real(kind=WP) :: R11,R22,R33,R12,R23,R31
          real(kind=WP) :: F,G,H,L,M,N
          real(kind=WP) :: r00,r45,r90
          real(kind=WP) :: rlank,hlank
          real(kind=WP) :: a1,a2,a3,a12
!===============================================================================
!  
          !===================================================================
          !< Hill (1948) yield criterion
          !===================================================================
          !< Classic Hill coefficient input
          if (iflag == 1) then 
            call hm_get_float_array_index("CRIT_HILL_R11",R11,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_HILL_R22",R22,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_HILL_R33",R33,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_HILL_R12",R12,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_HILL_R31",R31,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_HILL_R23",R23,ikey,is_available,lsubmodel,unitab)
            !< Check default values
            if (R11 == zero) R11 = one
            if (R22 == zero) R22 = one
            if (R33 == zero) R33 = one
            if (R12 == zero) R12 = one
            if (R23 == zero) R23 = one
            if (R31 == zero) R31 = one
            !< Compute Hill parameters
            F = half*((one/(R22**2)) + (one/(R33**2)) - (one/(R11**2)))
            G = half*((one/(R33**2)) + (one/(R11**2)) - (one/(R22**2)))
            H = half*((one/(R11**2)) + (one/(R22**2)) - (one/(R33**2)))
            L = (three/(two*(R23**2)))
            M = (three/(two*(R31**2)))
            N = (three/(two*(R12**2)))
          elseif (iflag == 2) then 
            call hm_get_float_array_index("CRIT_HILL_F",F,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_HILL_G",G,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_HILL_H",H,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_HILL_L",L,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_HILL_M",M,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_HILL_N",N,ikey,is_available,lsubmodel,unitab)
          elseif (iflag == 3) then 
            call hm_get_float_array_index("CRIT_HILL_R00",r00,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_HILL_R45",r45,ikey,is_available,lsubmodel,unitab)
            call hm_get_float_array_index("CRIT_HILL_R90",r90,ikey,is_available,lsubmodel,unitab)
            call hm_get_int_array_index  ("CRIT_HILL_DIR",idir,ikey,is_available,lsubmodel)
            rlank = (r00 + two*r45 + r90)/four
            hlank = rlank/(one + rlank)
            a1  = hlank*(one + (one/r00))
            a2  = hlank*(one + (one/r90))
            a3  = two*hlank
            a12 = two*hlank*(r45+half)*((one/r00)+(one/r90))
            !< Yield stress measured in direction 1
            if (idir == 1) then 
              a2  = a2/a1
              a3  = a3/a1
              a12 = a12/a1
              a1  = one
            !< Yield stress measured in direction 2  
            elseif (idir == 2) then 
              a1  = a1/a2
              a3  = a3/a2
              a12 = a12/a2
              a2  = one
            endif
            !< Compute Hill parameters
            F = a2 - a3*half
            G = a1 - a3*half
            H = a3*half   
            L = three*half
            M = three*half
            N = a12*half
          endif  
          !< Yield criterion type
          icrit = 3
          !< Number of parameters
          nupar_crit = 6
          !< Save yield criterion parameters
          upar_crit(1) = F
          upar_crit(2) = G
          upar_crit(3) = H
          upar_crit(4) = L
          upar_crit(5) = M
          upar_crit(6) = N
          !< Printing yield criterion parameters
          if (is_encrypted) then
            write(iout,"(5X,A,//)") "CONFIDENTIAL DATA"
          else
            write(iout,1000) 
            if (iflag == 1) then
              write(iout,1002) R11,R22,R33,R12,R23,R31
            elseif (iflag == 3) then
              write(iout,1003) r00,r45,r90,idir
            endif
            write(iout,1001) F,G,H,L,M,N
          endif
! ------------------------------------------------------------------------------
1000 format(/                                                                  &
          5X,"-------------------------------------------------------",/       &
          5X,"HILL (1948) YIELD CRITERION                            ",/,      &
          5X,"-------------------------------------------------------")
1001 format(                                                                   &
          5X,"HILL COEFFICIENT F . . . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"HILL COEFFICIENT G . . . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"HILL COEFFICIENT H . . . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"HILL COEFFICIENT L . . . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"HILL COEFFICIENT M . . . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"HILL COEFFICIENT N . . . . . . . . . . . . . . . . . .=",1PG20.13) 
1002 format(                                                                   &
          5X,"HILL STRESS RATIO R11. . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"HILL STRESS RATIO R22. . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"HILL STRESS RATIO R33. . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"HILL STRESS RATIO R12. . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"HILL STRESS RATIO R23. . . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"HILL STRESS RATIO R31. . . . . . . . . . . . . . . . .=",1PG20.13/)
1003 format(                                                                   &
          5X,"LANKFORD COEFFICIENT R00 . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"LANKFORD COEFFICIENT R45 . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"LANKFORD COEFFICIENT R90 . . . . . . . . . . . . . . .=",1PG20.13/&
          5X,"DIRECTION OF MEASUREMENT DIR . . . . . . . . . . . . .=",I10/,   &
          5X,"   = 0: AVERAGE OF 0, 45 AND 90 DEGREES                ",/,      &
          5X,"   = 1: MEASUREMENT IN 0 DEGREE DIRECTION              ",/,      &
          5X,"   = 2: MEASUREMENT IN 90 DEGREES DIRECTION            ",/)
! -------------------------------------------------------------------------------
        end subroutine hm_read_yield_criterion_hill
      end module hm_read_yield_criterion_hill_mod