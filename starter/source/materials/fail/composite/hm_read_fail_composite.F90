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
      !||    hm_read_fail_composite_mod   ../starter/source/materials/fail/composite/hm_read_fail_composite.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_fail                 ../starter/source/materials/fail/hm_read_fail.F
      !||====================================================================
      module hm_read_fail_composite_mod
      contains
      !||====================================================================
      !||    hm_read_fail_composite   ../starter/source/materials/fail/composite/hm_read_fail_composite.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_fail             ../starter/source/materials/fail/hm_read_fail.F
      !||--- calls      -----------------------------------------------------
      !||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||--- uses       -----------------------------------------------------
      !||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
      !||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
      !||    message_mod              ../starter/share/message_module/message_mod.F
      !||    submodel_mod             ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      subroutine hm_read_fail_composite(                                      &
                  fail     ,fail_id  ,irupt    ,lsubmodel,unitab  ,fail_tag , &
                  iout     ) 
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use fail_param_mod
      use unitab_mod
      use message_mod 
      use submodel_mod
      use hm_option_read_mod 
      use elbuftag_mod
      use constant_mod
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
      implicit none 
#include  "my_real.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      type (fail_param_)  ,intent(inout) :: fail         !< failure model data structure
      integer             ,intent(in)    :: fail_id      !< failure model ID
      integer             ,intent(in)    :: irupt        !< failure model number
      type (submodel_data),intent(in)    :: lsubmodel(*) !< submodel table
      type (unit_type_)   ,intent(in)    :: unitab       !< table of input units
      type (fail_tag_)    ,intent(inout) :: fail_tag     !< failure model tag for buffer allocation
      integer             ,intent(in)    :: iout         !< output unit number
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: ifail_sh,ifail_so
      my_real :: sigt1,sigc1,sigt2,sigc2,sig12
      my_real :: sigt3,sigc3,sig23,sig31
      my_real :: beta,tmax,expn,pthkf
      logical :: is_available,is_encrypted
!-----------------------------------------------
!   S o u r c e   L i n e s
!-----------------------------------------------
      is_encrypted = .false.
      is_available = .false.
!----------------------------------------------------------------
      call hm_option_is_encrypted(is_encrypted)
!----------------------------------------------------------------
      !< First line model parameters
      call hm_get_floatv('SIGMA_1T',sigt1   ,is_available,lsubmodel,unitab)
      call hm_get_floatv('SIGMA_1C',sigc1   ,is_available,lsubmodel,unitab)
      call hm_get_floatv('SIGMA_2T',sigt2   ,is_available,lsubmodel,unitab)
      call hm_get_floatv('SIGMA_2C',sigc2   ,is_available,lsubmodel,unitab)
      call hm_get_floatv('SIGMA_12',sig12   ,is_available,lsubmodel,unitab)
      !< Second line model parameters
      call hm_get_floatv('SIGMA_3T',sigt3   ,is_available,lsubmodel,unitab)
      call hm_get_floatv('SIGMA_3C',sigc3   ,is_available,lsubmodel,unitab)
      call hm_get_floatv('SIGMA_23',sig23   ,is_available,lsubmodel,unitab)
      call hm_get_floatv('SIGMA_31',sig31   ,is_available,lsubmodel,unitab)
      !< Third line model parameters
      call hm_get_floatv('BETA'    ,beta    ,is_available,lsubmodel,unitab)
      call hm_get_floatv('TAU_MAX' ,tmax    ,is_available,lsubmodel,unitab)
      call hm_get_floatv('EXPN'    ,expn    ,is_available,lsubmodel,unitab)
      call hm_get_intv  ('IFAIL_SH',ifail_sh,is_available,lsubmodel)
      call hm_get_intv  ('IFAIL_SO',ifail_so,is_available,lsubmodel)
!
      !< Check parameters and set defaults value
      if (sigt1 == zero) sigt1 = infinity
      if (sigc1 == zero) sigc1 = infinity
      if (sigt2 == zero) sigt2 = infinity
      if (sigc2 == zero) sigc2 = infinity
      if (sig12 == zero) sig12 = infinity
      if (sigt3 == zero) sigt3 = infinity
      if (sigc3 == zero) sigc3 = infinity
      if (sig23 == zero) sig23 = infinity
      if (sig31 == zero) sig31 = infinity
      if (tmax  == zero) tmax  = infinity
      if (expn  == zero) expn  = one
      ifail_sh = min(ifail_sh,2)
      ifail_sh = max(0,ifail_sh)
      ifail_so = min(ifail_so,2)
      ifail_so = max(0,ifail_so)
!
      !< Shell element deletion flag
      if (ifail_sh == 1) then
        pthkf = em06
      elseif (ifail_sh == 2) then 
        pthkf = one
      endif
!
!--------------------------
!     Filling buffer tables
!--------------------------   
      !< Failure tables size and allocation
      fail%keyword = 'COMPOSITE' 
      fail%irupt   = irupt 
      fail%fail_id = fail_id 
      fail%nuparam = 12
      fail%niparam = 2
      fail%nuvar   = 1
      fail%nfunc   = 0
      fail%ntable  = 0
      fail%nmod    = 9
      fail%pthk    = pthkf
!          
      allocate (fail%uparam(fail%nuparam))
      allocate (fail%iparam(fail%niparam))
      allocate (fail%ifunc (fail%nfunc))
      allocate (fail%table (fail%ntable))
!
      ! Modes of failure
      fail_tag%lf_dammx = fail_tag%lf_dammx + fail%nmod
      allocate (fail%mode(fail%nmod))
      fail%mode(1) = "Tensile failure index in dir. 1"
      fail%mode(2) = "Compression failure index in dir. 1"
      fail%mode(3) = "Tensile failure index in dir. 2"
      fail%mode(4) = "Compression failure index in dir. 2"
      fail%mode(5) = "Shear failure index in plane 1-2"
      fail%mode(6) = "Tensile failure index in dir. 3"
      fail%mode(7) = "Compression failure index in dir. 3"
      fail%mode(8) = "Shear failure index in plane 2-3"
      fail%mode(9) = "Shear failure index in plane 3-1"
!
      !< Integer material parameters
      fail%iparam(1) = ifail_sh
      fail%iparam(2) = ifail_so
!
      !< Real material parameters
      fail%uparam(1)  = sigt1
      fail%uparam(2)  = sigc1
      fail%uparam(3)  = sigt2
      fail%uparam(4)  = sigc2
      fail%uparam(5)  = sig12
      fail%uparam(6)  = sigt3
      fail%uparam(7)  = sigc3
      fail%uparam(8)  = sig23
      fail%uparam(9)  = sig31
      fail%uparam(10) = beta
      fail%uparam(11) = expn
      fail%uparam(12) = tmax
!
      ! Print parameter in 0.out file
      if (is_encrypted) then
        write (iout,'(5X,A,//)') 'CONFIDENTIAL DATA'
      else
        write(iout,1000)
        write(iout,1100) sigt1,sigc1,sigt2,sigc2,sig12
        write(iout,1200) sigt3,sigc3,sig23,sig31
        write(iout,1300) beta,expn
        if (ifail_sh > 0 .or. ifail_so > 0) then 
          write(iout,1400) tmax
        endif
        write(iout,1500) ifail_sh
        write(iout,1600) ifail_so   
      endif
!----------- 
 1000 format(                                                                  &
       5X,'  ----------------------------------------------------   ',/        &
       5X,'            FAILURE CRITERION : COMPOSITE                ',/,       &
       5X,'  ----------------------------------------------------   ',/)          
 1100 format(                                                                  &
       5X,'TENSILE STRENGTH IN DIRECTION 1 (SIG_11_T). . . . . . . =',1PG20.13/&
       5X,'COMPRESSIVE STRENGTH IN DIRECTION 1 (SIG_11_C). . . . . =',1PG20.13/&
       5X,'TENSILE STRENGTH IN DIRECTION 2 (SIG_22_T). . . . . . . =',1PG20.13/&
       5X,'COMPRESSIVE STRENGTH IN DIRECTION 2 (SIG_22_C). . . . . =',1PG20.13/&
       5X,'SHEAR STRENGTH IN PLANE 1-2 (SIG_12). . . . . . . . . . =',1PG20.13/)
 1200 format(                                                                  &
       5X,'SOLID ELEMENTS ONLY PARAMETERS:                          ',/,       &
       5X,'TENSILE STRENGTH IN DIRECTION 1 (SIG_33_T). . . . . . . =',1PG20.13/& 
       5X,'COMPRESSIVE STRENGTH IN DIRECTION 1 (SIG_33_C). . . . . =',1PG20.13/&
       5X,'SHEAR STRENGTH IN PLANE 2-3 (SIG_23). . . . . . . . . . =',1PG20.13/&
       5X,'SHEAR STRENGTH IN PLANE 3-1 (SIG_31). . . . . . . . . . =',1PG20.13/)
 1300 format(                                                                  &
       5X,'SHEAR SCALING FACTOR (BETA) . . . . . . . . . . . . . . =',1PG20.13/&
       5X,'EXPONENT (N). . . . . . . . . . . . . . . . . . . . . . =',1PG20.13/)
 1400 format(                                                                  &
       5X,'STRESS SOFTENING ACTIVATED:                              ',/,       &
       5X,'RELAXATION TIME TAU_MAX . . . . . . . . . . . . . . . . =',1PG20.13/)
 1500 format(                                                                  &
       5X,'SHELL DELETION FLAG IFAIL_SH  . . . . . . . . . . . . . =',I10,/    &
       5X,'    = 0: SHELL NEVER DELETED AND NO STRESS SOFTENING      ',/,      &
       5X,'    = 1: SHELL DELETED IF DAMAGE IS REACHED FOR ONE LAYER ',/,      &
       5X,'    = 2: SHELL DELETED IF DAMAGE IS REACHED FOR ALL LAYERS',/) 
 1600 format(                                                                  &
       5X,'SOLID DELETION FLAG IFAIL_SO  . . . . . . . . . . . . . =',I10,/    &
       5X,'    = 0: SOLID NEVER DELETED AND NO STRESS SOFTENING                  ',/,&
       5X,'    = 1: SOLID DELETED IF DAMAGE IS REACHED FOR ONE INTEGRATION POINT ',/,&
       5X,'    = 2: SOLID DELETED IF DAMAGE IS REACHED FOR ALL INTEGRATION POINTS',/)   
!----------- 
      end subroutine hm_read_fail_composite
      end module hm_read_fail_composite_mod
