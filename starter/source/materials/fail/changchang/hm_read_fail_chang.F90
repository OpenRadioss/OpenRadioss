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
!||    hm_read_fail_chang_mod   ../starter/source/materials/fail/changchang/hm_read_fail_chang.F90
!||--- called by ------------------------------------------------------
!||    hm_read_fail             ../starter/source/materials/fail/hm_read_fail.F
!||====================================================================
      module hm_read_fail_chang_mod
      contains
!||====================================================================
!||    hm_read_fail_chang       ../starter/source/materials/fail/changchang/hm_read_fail_chang.F90
!||--- called by ------------------------------------------------------
!||    hm_read_fail             ../starter/source/materials/fail/hm_read_fail.F
!||--- calls      -----------------------------------------------------
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      subroutine hm_read_fail_chang(                                           &
        fail ,fail_id  ,irupt    ,lsubmodel,unitab   ,fail_tag ,iout     ,     &
        mtag )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use fail_param_mod
      use unitab_mod
      use submodel_mod
      use hm_option_read_mod 
      use elbuftag_mod  
      use precision_mod, only : WP     
      use constant_mod   
      use elbuftag_mod  
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      type (fail_param_),                       intent(inout) :: fail      !< Failure model data structure
      integer,                                  intent(in)    :: fail_id   !< Failure model id
      integer,                                  intent(in)    :: irupt     !< Failure model number
      type (submodel_data), dimension(nsubmod), intent(in)    :: lsubmodel !< Submodel data structure
      type (unit_type_),                        intent(in)    :: unitab    !< Table of input units
      type (fail_tag_),                         intent(inout) :: fail_tag  !< Failure model tag for buffer allocation
      integer,                                  intent(in)    :: iout      !< Output unit number
      type (mlaw_tag_),                         intent(inout) :: mtag      !< Material law tag for buffer allocation
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: ifail_sh,failip
      real(kind=WP) :: sigt1,sigt2,sigt12,sigc1,sigc2,beta,tmax,pthkf,fcut
      logical :: is_available,is_encrypted
!--------------------------------------------------
!   S o u r c e   L i n e s
!--------------------------------------------------
      is_encrypted = .false.
      is_available = .false.
! ----------------------------------------------------------------------------------------------------------------------
      call hm_option_is_encrypted(is_encrypted)
! ----------------------------------------------------------------------------------------------------------------------
      call hm_get_floatv('Sigma_1t',sigt1   ,is_available,lsubmodel,unitab)
      call hm_get_floatv('Sigma_2t',sigt2   ,is_available,lsubmodel,unitab)
      call hm_get_floatv('Sigma_12',sigt12  ,is_available,lsubmodel,unitab)
      call hm_get_floatv('Sigma_1c',sigc1   ,is_available,lsubmodel,unitab)
      call hm_get_floatv('Sigma_2c',sigc2   ,is_available,lsubmodel,unitab)
!
      call hm_get_floatv('Beta'    ,beta    ,is_available,lsubmodel,unitab)
      call hm_get_floatv('Tau_max' ,tmax    ,is_available,lsubmodel,unitab)
      call hm_get_intv  ('Ifail_sh',ifail_sh,is_available,lsubmodel)
      call hm_get_intv  ('Failip'  ,failip  ,is_available,lsubmodel)
      call hm_get_floatv('Fcut'    ,fcut    ,is_available,lsubmodel,unitab)
! ----------------------------------------------------------------------------------------------------------------------
      !< Check values and set default values  
      if (sigt1  == zero) sigt1  = infinity
      if (sigt2  == zero) sigt2  = infinity
      if (sigt12 == zero) sigt12 = infinity
      if (sigc1  == zero) sigc1  = infinity
      if (sigc2  == zero) sigc2  = infinity
      if (tmax   == zero) tmax   = infinity
!
      !< Percentage of failed layer
      if (ifail_sh == 1) then        ! matrix or fiber failure
        pthkf = em06
      elseif (ifail_sh == 2) then    ! matrix or fiber failure
        pthkf = one
      elseif (ifail_sh == 3) then    ! fiber only failure
        pthkf = em06
      elseif (ifail_sh == 4) then    ! fiber only failure
        pthkf = one
      endif                  
! ----------------------------------------------------------------------------------------------------------------------
!     Filling buffer tables
! ----------------------------------------------------------------------------------------------------------------------
      !< Failure tables size
      fail%keyword = 'CHANG' 
      fail%irupt   = irupt 
      fail%fail_id = fail_id 
      fail%nuparam = 8
      fail%niparam = 2
      fail%nuvar   = 6
      fail%nfunc   = 0
      fail%ntable  = 0
      fail%nmod    = 4
      fail%pthk    = pthkf
!
      !< Allocation of failure parameters tables              
      allocate (fail%uparam(fail%nuparam))
      allocate (fail%iparam(fail%niparam))
      allocate (fail%ifunc (fail%nfunc))
      allocate (fail%table (fail%ntable))
!
      ! Modes of failure
      fail_tag%lf_dammx = fail_tag%lf_dammx + fail%nmod
      allocate (fail%mode(fail%nmod))
      fail%mode(1) = "Failure index tension dir. 1"
      fail%mode(2) = "Failure index compression dir. 1"
      fail%mode(3) = "Failure index tension dir. 2"
      fail%mode(4) = "Failure index compression dir. 2"
!
      !< Integer material parameter
      fail%iparam(1)  = failip
      fail%iparam(2)  = ifail_sh
!      
      !< Real material parameters
      fail%uparam(1) = sigt1
      fail%uparam(2) = sigt2
      fail%uparam(3) = sigt12
      fail%uparam(4) = sigc1
      fail%uparam(5) = sigc2
      fail%uparam(6) = beta
      fail%uparam(7) = tmax
      fail%uparam(8) = fcut
!
      !< Damage softening scale factor
      mtag%l_dmgscl = 1
      mtag%g_dmgscl = 1
!
! ------------------------------------------------------------------------------
!     Parameters printout
! ------------------------------------------------------------------------------
      if (is_encrypted) then
        write(iout,"(5X,A,//)")"CONFIDENTIAL DATA"        
      else
        write(iout,1000)
        write(iout,1100) sigt1,sigt2,sigt12,sigc1,sigc2,beta,tmax    
        write(iout,1200) ifail_sh,failip    
      endif
! ------------------------------------------------------------------------------
 1000 format(/                                                                 &
     & 5X,'---------------------------------------------------------',/,       &
     & 5X,'              CHANG CHANG FAILURE MODEL                  ',/,       &
     & 5X,'---------------------------------------------------------',/)
 1100 format(/                                                                 &
     & 5X,"FAILURE CRITERION PARAMETERS:                            ",/,       &
     & 5X,"-----------------------------                            ",/,       &
     & 5X,'LONGITUDINAL TENSILE STRENGTH (SIGMA1_T) . . . . . . . .=',1pg20.13/&
     & 5X,'TRANSVERSAL  TENSILE STRENGTH (SIGMA2_T) . . . . . . . .=',1pg20.13/&
     & 5X,'SHEAR STRENGTH (SIGMA_12). . . . . . . . . . . . . . . .=',1pg20.13/&
     & 5X,'LONGITUDINAL COMPRESSIVE STRENGTH (SIGMA1_C) . . . . . .=',1pg20.13/&
     & 5X,'TRANSVERSAL  COMPRESSIVE STRENGTH (SIGMA2_C) . . . . . .=',1pg20.13/&
     & 5X,'SHEAR SCALING FACTOR (BETA). . . . . . . . . . . . . . .=',1pg20.13/&
     & 5X,'RELAXATION TIME (TAU_MAX). . . . . . . . . . . . . . . .=',1pg20.13/)  
 1200 format(/                                                                 & 
     & 5X,"ELEMENT DELETION:                                        ",/,       &
     & 5X,"-----------------                                        ",/,       &
     & 5X,'SHELL FAILURE FLAG (IFAIL_SH). . . . . . . . . . . . . .=',i10/     &
     & 5X,'   = 0: NO ELEMENT DELETION                              ',/        &
     & 5X,'   = 1: PLY/SHELL IS DELETED IF DAMAGE IS REACHED ON 1 THICKNESS INTG. POINT',/&
     & 5X,'   = 2: PLY/SHELL IS DELETED IF DAMAGE IS REACHED ON ALL THICKNESS INTG. POINTS',/&
     & 5X,'NBR OF FAILED INTG. PT. PRIOR TO SOLID DELETION (FAILIP)=',i10/)
! ------------------------------------------------------------------------------
      end subroutine hm_read_fail_chang
      end module hm_read_fail_chang_mod
