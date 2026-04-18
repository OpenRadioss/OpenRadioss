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
!||    hm_read_fail_hashin_mod   ../starter/source/materials/fail/hashin/hm_read_fail_hashin.F90
!||--- called by ------------------------------------------------------
!||    hm_read_fail              ../starter/source/materials/fail/hm_read_fail.F
!||====================================================================
      module hm_read_fail_hashin_mod
      contains
!||====================================================================
!||    hm_read_fail_hashin      ../starter/source/materials/fail/hashin/hm_read_fail_hashin.F90
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
      subroutine hm_read_fail_hashin(                                          &
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
      type(fail_param_),                       intent(inout) :: fail         !< Failure model data structure
      integer,                                 intent(in)    :: fail_id      !< Failure model ID
      integer,                                 intent(in)    :: irupt        !< Failure model type number
      type(submodel_data), dimension(nsubmod), intent(in)    :: lsubmodel    !< Submodel table
      type(unit_type_),                        intent(in)    :: unitab       !< Table of input units
      type (fail_tag_),                        intent(inout) :: fail_tag     !< Failure model tag for buffer allocation
      integer,                                 intent(in)    :: iout         !< Output unit number
      type (mlaw_tag_),                        intent(inout) :: mtag         !< Material law tag for buffer allocation
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: ifail_sh,imodel,ifail_so
      real(kind=WP) :: sigt1,sigt2,sigt3,sigc1,sigc2,fsig12,msig12,      &
        msig23,msig13,angle,sdelam,csig,pthkf,tmax,ratio,fcut
      logical  :: is_available,is_encrypted
!--------------------------------------------------
!   S o u r c e   L i n e s
!--------------------------------------------------
      is_encrypted = .false.
      is_available = .false.
!-------------------------------------------------------------------------------
      call hm_option_is_encrypted(is_encrypted)
!-------------------------------------------------------------------------------
      call hm_get_intv   ('Iform'     ,imodel   ,is_available,lsubmodel)
      call hm_get_intv   ('Ifail_sh'  ,ifail_sh ,is_available,lsubmodel)
      call hm_get_intv   ('Ifail_so'  ,ifail_so ,is_available,lsubmodel)
      call hm_get_floatv ('ratio'     ,ratio    ,is_available,lsubmodel,unitab)
!
      call hm_get_floatv ('Sigma_1t'  ,sigt1    ,is_available,lsubmodel,unitab)
      call hm_get_floatv ('Sigma_2t'  ,sigt2    ,is_available,lsubmodel,unitab)
      call hm_get_floatv ('Sigma_3t'  ,sigt3    ,is_available,lsubmodel,unitab)
      call hm_get_floatv ('Sigma_1c'  ,sigc1    ,is_available,lsubmodel,unitab)
      call hm_get_floatv ('Sigma_2c'  ,sigc2    ,is_available,lsubmodel,unitab)
!
      call hm_get_floatv ('Sigma_c'   ,csig     ,is_available,lsubmodel,unitab)
      call hm_get_floatv ('Sigma_12f' ,fsig12   ,is_available,lsubmodel,unitab)
      call hm_get_floatv ('Sigma_12m' ,msig12   ,is_available,lsubmodel,unitab)
      call hm_get_floatv ('Sigma_23m' ,msig23   ,is_available,lsubmodel,unitab)
      call hm_get_floatv ('Sigma_13m' ,msig13   ,is_available,lsubmodel,unitab)
!
      call hm_get_floatv ('Phi'       ,angle    ,is_available,lsubmodel,unitab)
      call hm_get_floatv ('Sdel'      ,sdelam   ,is_available,lsubmodel,unitab)
      call hm_get_floatv ('Tau_max'   ,tmax     ,is_available,lsubmodel,unitab)
      call hm_get_floatv ('Fcut'      ,fcut     ,is_available,lsubmodel,unitab)
!-------------------------------------------------------------------------------
!     Default values
!-------------------------------------------------------------------------------
      !< Check values
      if (sigt1  == zero) sigt1  = ep20
      if (sigt2  == zero) sigt2  = ep20
      if (sigt3  == zero) sigt3  = ep20
      if (sigc1  == zero) sigc1  = ep20
      if (sigc2  == zero) sigc2  = ep20
      if (csig   == zero) csig   = ep20
      if (fsig12 == zero) fsig12 = ep20
      if (msig12 == zero) msig12 = ep20
      if (msig13 == zero) msig13 = ep20
      if (msig23 == zero) msig23 = ep20
      if (tmax   == zero) tmax   = ep20
      if (sdelam == zero) sdelam = one
      if (imodel == 0)    imodel = 1
      if (ratio  == zero) ratio  = one - em06
      ratio = min(ratio, one)
      ratio = max(ratio,-one)
!
      !< Shell failure flag
      if (ifail_sh == 1) then
        pthkf = em06
      elseif (ifail_sh == 2) then
        pthkf = ratio
      elseif (ifail_sh == 3) then
        pthkf = one - em06
      else
        pthkf    = zero
      endif
! ----------------------------------------------------------------------------------------------------------------------
!     Filling buffer tables
! ----------------------------------------------------------------------------------------------------------------------
      !< Failure tables size
      fail%keyword = 'HASHIN' 
      fail%irupt   = irupt 
      fail%fail_id = fail_id 
      fail%nuparam = 14
      fail%niparam = 3
      fail%nuvar   = 7
      fail%nfunc   = 0
      fail%ntable  = 0
      fail%pthk    = pthkf
!       
      !< Allocation of failure parameters tables             
      allocate (fail%uparam(fail%nuparam))
      allocate (fail%iparam(fail%niparam))
      allocate (fail%ifunc (fail%nfunc))
      allocate (fail%table (fail%ntable))
!
      ! Modes of failure
      ! -> Uni-directional lamina model 
      if (imodel == 1) then 
        fail%nmod = 5
        allocate (fail%mode(fail%nmod))
        fail%mode(1) = "Tensile/Shear fiber"
        fail%mode(2) = "Compression fiber"
        fail%mode(3) = "Crush"
        fail%mode(4) = "Matrix failure"
        fail%mode(5) = "Delamination"
      ! -> Fabric lamina model 
      else 
        fail%nmod = 7
        allocate (fail%mode(fail%nmod))
        fail%mode(1) = "Tensile/Shear fiber 1"
        fail%mode(2) = "Tensile/Shear fiber 2"
        fail%mode(3) = "Compression fiber 1"
        fail%mode(4) = "Compression fiber 2"
        fail%mode(5) = "Crush"
        fail%mode(6) = "Shear failure matrix"
        fail%mode(7) = "Matrix failure"
      endif
      fail_tag%lf_dammx = fail_tag%lf_dammx + fail%nmod
!
      !< Integer material parameter
      fail%iparam(1)  = ifail_so
      fail%iparam(2)  = ifail_sh
      fail%iparam(3)  = imodel
!      
      !< Real material parameters
      fail%uparam(1)  = sigt1
      fail%uparam(2)  = sigt2
      fail%uparam(3)  = sigt3
      fail%uparam(4)  = sigc1
      fail%uparam(5)  = sigc2
      fail%uparam(6)  = csig
      fail%uparam(7)  = fsig12
      fail%uparam(8)  = msig12
      fail%uparam(9)  = msig13
      fail%uparam(10) = msig23
      fail%uparam(11) = angle*pi/hundred80
      fail%uparam(12) = sdelam
      fail%uparam(13) = tmax
      fail%uparam(14) = fcut
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
        write(iout, 1000)
        write(iout, 1100) imodel
        write(iout, 1200) sigt1,sigt2,sigt3,sigc1,sigc2,csig,fsig12,msig12,    &
          msig23,msig13,angle,sdelam,tmax
        write(iout, 1300) ifail_sh,ratio,ifail_so
      endif
! ------------------------------------------------------------------------------
1000 format(/                                                                  &
     & 5X,"---------------------------------------------------------",/,       &
     & 5X,"               HASHIN FAILURE CRITERION                  ",/,       &
     & 5X,"---------------------------------------------------------",/)
1100 format(                                                                   &
     & 5X,'FAILURE CRITERION FORMULATION FLAG (IFORM) . . . . . . .=',I10/     &
     & 5X,'      = 1 : UNI-DIRECTIONAL LAMINA MODEL                 ',/        &
     & 5X,'      = 2 : FABRIC LAMINA MODEL                          ',/)
1200 format(/                                                                  &
     & 5X,"FAILURE CRITERION PARAMETERS:                            ",/,       &
     & 5X,"-----------------------------                            ",/,       &
     & 5X,'LONGITUDINAL TENSILE STRENGTH (SIGMA_1T) . . . . . . . .=',1pg20.13/&
     & 5X,'TRANSVERSE   TENSILE STRENGTH (SIGMA_2T) . . . . . . . .=',1pg20.13/&
     & 5X,'THROUGH-THICKNESS TENSILE STRENGTH (SIGMA_3T). . . . . .=',1pg20.13/&
     & 5X,'LONGITUDINAL COMPRESSIVE STRENGTH (SIGMA_1C) . . . . . .=',1pg20.13/&
     & 5X,'TRANSVERSE   COMPRESSIVE STRENGTH (SIGMA_2C) . . . . . .=',1pg20.13/&
     & 5X,'CRUSH STRENGTH (SIGMA_C) . . . . . . . . . . . . . . . .=',1pg20.13/&
     & 5X,'SHEAR STRENGTH (SIGMA_12). . . . . . . . . . . . . . . .=',1pg20.13/&
     & 5X,'FIBER MODE SHEAR STRENGTH (SIGMA_12F). . . . . . . . . .=',1pg20.13/&
     & 5X,'MATRIX MODE SHEAR STRENGTH (SIGMA_12M) . . . . . . . . .=',1pg20.13/&
     & 5X,'MATRIX MODE SHEAR STRENGTH (SIGMA_23M) . . . . . . . . .=',1pg20.13/&
     & 5X,'MATRIX MODE SHEAR STRENGTH (SIGMA_13M) . . . . . . . . .=',1pg20.13/&
     & 5X,'COULOMB FRICTION ANGLE (PHI) . . . . . . . . . . . . . .=',1pg20.13/&
     & 5X,'DELAMINATION STRENGTH SCALE FACTOR (SDEL). . . . . . . .=',1pg20.13/&
     & 5X,'RELAXATION TIME (TAU_MAX). . . . . . . . . . . . . . . .=',1pg20.13/)
1300 format(/                                                                  &
     & 5X,"ELEMENT DELETION:                                        ",/,       &
     & 5X,"-----------------                                        ",/,       &
     & 5X,'SHELL FAILURE FLAG (IFAIL_SH). . . . . . . . . . . . . .=',i10/,    &
     & 5X,'   = 0: NO ELEMENT DELETION                              ',/,       &
     & 5X,'   = 1: PLY/SHELL IS DELETED IF DAMAGE IS REACHED ON 1 THICKNESS INTG. POINT',/,&
     & 5X,'   = 2: PLY/SHELL IS DELETED IF DAMAGE IS REACHED ON THE THICKNESS FRACTION PTHICKFAIL',/,&
     & 5X,'   = 3: PLY/SHELL IS DELETED IF DAMAGE IS REACHED ON ALL THICKNESS INTG. POINT',/,&
     & 5X,'FAILED THICKNESS FRACTION (PTHICKFAIL) . . . . . . . . .=',1pg20.13/,&
     & 5X,'SOLID FAILURE FLAG (IFAIL_SO). . . . . . . . . . . . . .=',i10/,    &
     & 5X,'   = 0: NO ELEMENT DELETION                              ',/,       &
     & 5X,'   = 1: ELEMENT IS DELETED IF DAMAGE IS REACHED ON 1 INTEGRATION POINT',/,&
     & 5X,'   = 2: ELEMENT IS DELETED IF DAMAGE IS REACHED ON ALL INTEGRATION POINTS',/)
! ------------------------------------------------------------------------------
     end subroutine hm_read_fail_hashin
     end module hm_read_fail_hashin_mod
