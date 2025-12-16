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
      module hm_read_fail_puck_mod
      implicit none
      contains
      subroutine hm_read_fail_puck(                                            &
        fail     ,fail_id  ,irupt    ,lsubmodel,unitab   ,fail_tag ,iout     , &
        mtag     )
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
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer            ,intent(in)    :: fail_id      !< Failure model id
      integer            ,intent(in)    :: irupt        !< Failure model type number
      type(unit_type_)   ,intent(in)    :: unitab       !< Table of input units
      type(submodel_data),intent(in)    :: lsubmodel(*) !< Submodel table
      type(fail_param_)  ,intent(inout) :: fail         !< Failure model data structure
      type (fail_tag_)   ,intent(inout) :: fail_tag     !< Failure model tag for buffer allocation
      integer            ,intent(in)    :: iout         !< Output unit number
      type (mlaw_tag_)   ,intent(inout) :: mtag         !< Material law tag for buffer allocation
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: ifail_sh,ifail_so
      real(kind=WP) ::                                                         &
        sigt1,sigt2,sigt12,sigc1,sigc2,pn12,pp12,pn22,tmax,fcut,pthkf
      logical :: is_available,is_encrypted
!--------------------------------------------------
!   S o u r c e   L i n e s
!--------------------------------------------------
      is_encrypted = .false.
      is_available = .false.
! ----------------------------------------------------------------------------------------------------------------------
      call hm_option_is_encrypted(is_encrypted)
! ----------------------------------------------------------------------------------------------------------------------
      call hm_get_floatv    ('Sigma_1t'     ,sigt1    ,is_available,lsubmodel,unitab)
      call hm_get_floatv    ('Sigma_2t'     ,sigt2    ,is_available,lsubmodel,unitab)
      call hm_get_floatv    ('Sigma_12'     ,sigt12   ,is_available,lsubmodel,unitab)
      call hm_get_floatv    ('Sigma_1c'     ,sigc1    ,is_available,lsubmodel,unitab)
      call hm_get_floatv    ('Sigma_2c'     ,sigc2    ,is_available,lsubmodel,unitab)
!
      call hm_get_floatv    ('p12_Positive' ,pp12     ,is_available,lsubmodel,unitab)
      call hm_get_floatv    ('p12_Negative' ,pn12     ,is_available,lsubmodel,unitab)
      call hm_get_floatv    ('p22_Negative' ,pn22     ,is_available,lsubmodel,unitab)
      call hm_get_floatv    ('Tau_max'      ,tmax     ,is_available,lsubmodel,unitab)
      call hm_get_intv      ('Ifail_sh'     ,ifail_sh ,is_available,lsubmodel)
      call hm_get_intv      ('Ifail_so'     ,ifail_so ,is_available,lsubmodel)
!
      call hm_get_floatv    ('Fcut'         ,fcut     ,is_available,lsubmodel,unitab)
! ----------------------------------------------------------------------------------------------------------------------
      !< Check values and set default values
      if (sigt1  == zero) sigt1  = ep20
      if (sigt2  == zero) sigt2  = ep20
      if (sigt12 == zero) sigt12 = ep20
      if (sigc1  == zero) sigc1  = ep20
      if (sigc2  == zero) sigc2  = ep20
      if (tmax   == zero) tmax   = ep20
      !< Solid failure flag
      ifail_so = max(0,min(ifail_so,2))
      !< Shell failure flag
      ifail_sh = max(0,min(ifail_sh,2))
      if (ifail_sh == 1) then
        pthkf = em06
      elseif (ifail_sh == 2) tHEN
        pthkf =  one
      endif
      !< Stress tensor filtering frequency
      fcut = max(zero,fcut)        
! ----------------------------------------------------------------------------------------------------------------------
!     Filling buffer tables
! ----------------------------------------------------------------------------------------------------------------------
      !< Failure tables size
      fail%keyword = 'PUCK' 
      fail%irupt   = irupt 
      fail%fail_id = fail_id 
      fail%nuparam = 10
      fail%niparam = 2
      fail%nuvar   = 6
      fail%nfunc   = 0
      fail%ntable  = 0
      fail%nmod    = 5
      fail%pthk    = pthkf
!       
      !< Allocation of failure parameters tables    
      allocate (fail%uparam(fail%nuparam))
      allocate (fail%iparam(fail%niparam))
      allocate (fail%ifunc (fail%nfunc))
      allocate (fail%table (fail%ntable))
!
      !< Modes of failure
      fail_tag%lf_dammx = fail_tag%lf_dammx + fail%nmod
      allocate (fail%mode(fail%nmod))
      fail%mode(1) = "Tensile fiber failure"
      fail%mode(2) = "Compression fiber failure"
      fail%mode(3) = "Inter-fiber failure A"
      fail%mode(4) = "Inter-fiber failure B"
      fail%mode(5) = "Inter-fiber failure C"
!
      !< Integer material parameter
      fail%iparam(1)  = ifail_so
      fail%iparam(2)  = ifail_sh
!
      !< Real material parameters
      fail%uparam(1)  = sigt1
      fail%uparam(2)  = sigt2
      fail%uparam(3)  = sigc1
      fail%uparam(4)  = sigc2
      fail%uparam(5)  = sigt12
      fail%uparam(6)  = pp12
      fail%uparam(7)  = pn12
      fail%uparam(8)  = pn22
      fail%uparam(9)  = tmax
      fail%uparam(10) = fcut   
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
        write(iout,1001) sigt1,sigt2,sigt12,sigc1,sigc2
        write(iout,1002) pp12,pn12,pn22,tmax,fcut  
        write(iout,1003) ifail_sh,ifail_so
      endif
! ------------------------------------------------------------------------------
1000 format(/                                                                  &
     & 5X,"---------------------------------------------------------",/,       &
     & 5X,"                PUCK FAILURE CRITERION                   ",/,       &
     & 5X,"---------------------------------------------------------",/)
1001 format(/                                                                  &
     & 5X,"FAILURE CRITERION PARAMETERS:                            ",/,       &
     & 5X,"-----------------------------                            ",/,       &
     & 5X,'LONGITUDINAL TENSILE STRENGTH (SIGMA_1T) . . . . . . . .=',1pg20.13/&
     & 5X,'TRANSVERSAL  TENSILE STRENGTH (SIGMA_2T) . . . . . . . .=',1pg20.13/&
     & 5X,'SHEAR STRENGTH (SIGMA_12). . . . . . . . . . . . . . . .=',1pg20.13/&
     & 5X,'LONGITUDINAL COMPRESSIVE STRENGTH (SIGMA_1C) . . . . . .=',1pg20.13/&
     & 5X,'TRANSVERSAL  COMPRESSIVE STRENGTH (SIGMA_2C) . . . . . .=',1pg20.13/)
1002 format(/                                                                  &
     & 5X,'FAILURE ENVELOP PARAMETER IN DIRECTION 12 (P12+) . . . .=',1pg20.13/&
     & 5X,'FAILURE ENVELOP PARAMETER IN DIRECTION 12 (P12-) . . . .=',1pg20.13/&
     & 5X,'FAILURE ENVELOP PARAMETER IN DIRECTION 22 (P22-) . . . .=',1pg20.13/& 
     & 5X,'RELAXATION TIME (TAU_MAX). . . . . . . . . . . . . . . .=',1pg20.13/& 
     & 5X,'STRESS TENSOR FILTERING FREQUENCY (FCUT) . . . . . . . .=',1pg20.13/)  
1003 format(/                                                                  &
     & 5X,"ELEMENT DELETION:                                        ",/,       &
     & 5X,"-----------------                                        ",/,       &
     & 5X,'SHELL FAILURE FLAG (IFAIL_SH). . . . . . . . . . . . . .=',i10/     &
     & 5X,'   = 0: NO ELEMENT DELETION                              ',/        &
     & 5X,'   = 1: PLY/SHELL IS DELETED IF DAMAGE IS REACHED ON 1 THICKNESS INTG. POINT',/&
     & 5X,'   = 2: PLY/SHELL IS DELETED IF DAMAGE IS REACHED ON ALL THICKNESS INTG. POINTS',/&
     & 5X,'SOLID FAILURE FLAG (IFAIL_SO). . . . . . . . . . . . . .=',i10/     &
     & 5X,'   = 0: NO ELEMENT DELETION                              ',/        &
     & 5X,'   = 1: ELEMENT IS DELETED IF DAMAGE IS REACHED ON 1 INTEGRATION POINT',/&
     & 5X,'   = 2: ELEMENT IS DELETED IF DAMAGE IS REACHED ON ALL INTEGRATION POINTS',/)
!
      end subroutine hm_read_fail_puck
      end module hm_read_fail_puck_mod
