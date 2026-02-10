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
!||    hm_read_mat105_mod   ../starter/source/materials/mat/mat105/hm_read_mat105.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat          ../starter/source/materials/mat/hm_read_mat.F90
!||====================================================================
      module hm_read_mat105_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Reader for material law 105 (powder-burn EoS)
!! \details
!||====================================================================
!||    hm_read_mat105           ../starter/source/materials/mat/mat105/hm_read_mat105.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_floatv_dim        ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    init_mat_keyword         ../starter/source/materials/mat/init_mat_keyword.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_mat105(uparam   ,maxuparam,nuparam  ,nuvar    ,nfunc    , &
                                  maxfunc  ,ifunc    ,parmat   ,unitab   ,mat_id   , &
                                  pm       ,titr     ,mtag     ,lsubmodel,matparam , &
          npropm   )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use unitab_mod
          use message_mod
          use elbuftag_mod
          use submodel_mod , only : nsubmod, submodel_data
          use matparam_def_mod
          use names_and_titles_mod , only : nchartitle
          use constant_mod , only : zero, one
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Include Files
! ----------------------------------------------------------------------------------------------------------------------
#include "units_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          character(len=nchartitle) ,intent(in)              :: titr

          integer, intent(in)                                :: mat_id,maxuparam,maxfunc,npropm
          integer, intent(inout)                             :: nuparam,nuvar,ifunc(maxfunc),nfunc
          type (unit_type_),intent(in)                       :: unitab

          type(submodel_data), dimension(nsubmod),intent(in) :: lsubmodel
          type(mlaw_tag_), intent(inout)                     :: mtag
          type(matparam_struct_) ,intent(inout)              :: matparam

          real(kind=WP), dimension(maxuparam) ,intent(inout) :: uparam
          real(kind=WP), dimension(npropm) ,intent(inout)    :: pm
          real(kind=WP), dimension(100),intent(inout)        :: parmat
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=WP) :: P0
          real(kind=WP) :: C1,BULK,Gr,DD,rhoe_g, e_g, c,alpha,MU0
          real(kind=WP) :: fscale_b_unit, fscale_p_unit, fscale_rho_unit
          real(kind=WP) :: rho0
          real(kind=WP) :: nu, pmin
          real(kind=WP) :: fscale_shear, fscale_yield, fscale_burnrate !< ordinate scale factors
          real(kind=WP) :: Xscale_shear, Xscale_yield, Xscale_burnrate !< abscissa scale factors
          integer :: fctID_shear, fctID_yield, fctID_burnrate !< function IDs
          character(len=64) :: mtl_msg
          logical :: is_available,is_encrypted
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          is_encrypted = .false.
          is_available = .false.

          call hm_option_is_encrypted(is_encrypted)

          !======== MATERIAL BUFFER ALLOCATION SIZES
          nuvar   = 6
          nuparam = 14
          nfunc   = 3
          uparam(1:nuparam) = zero
          ifunc(1:nfunc)    = 0
          matparam%compatibility_eos = 1

          !======== ELEMENTARY BUFFER ALLOCATION SIZES
          mtag%g_tb     = 1
          mtag%g_temp   = 1
          mtag%g_bfrac  = 1
          mtag%l_tb     = 1
          mtag%l_temp   = 1
          mtag%l_bfrac  = 1

          !======== ELEMENTARY BUFFER ALLOCATION SIZES
!----------------------------------------------------------------
!         #---POWDER MATERIAL MODEL
!         #Line-1
          call hm_get_floatv("MAT_RHO"      , rho0            ,is_available, lsubmodel, unitab)
!         #Line-2
          call hm_get_floatv("MAT_NU"       , nu              ,is_available, lsubmodel, unitab)
          call hm_get_floatv("MAT_PMIN"     , pmin            ,is_available, lsubmodel, unitab)
          call hm_get_floatv("MAT_C1"       , c1              ,is_available, lsubmodel, unitab)
!         #Line-3
          call hm_get_floatv("GAS_D"        , dd              ,is_available, lsubmodel, unitab)
          call hm_get_floatv("GAS_RHOE"     , rhoe_g          ,is_available, lsubmodel, unitab)
          call hm_get_floatv("POWDER_Gr"    , gr              ,is_available, lsubmodel, unitab)
          call hm_get_floatv("POWDER_C"     , c               ,is_available, lsubmodel, unitab)
          call hm_get_floatv("POWDER_ALPHA" , alpha           ,is_available, lsubmodel, unitab)
!         #Line-4
          call hm_get_intv("FCT_ID_G"       , fctID_shear     ,is_available, lsubmodel)
          call hm_get_floatv("YSCALE_G"     , fscale_shear    ,is_available, lsubmodel, unitab)
          call hm_get_floatv("XSCALE_G"     , Xscale_shear    ,is_available, lsubmodel, unitab)
!         #Line-5
          call hm_get_intv("FCT_ID_Y"       , fctID_yield     ,is_available, lsubmodel)
          call hm_get_floatv("YSCALE_Y"     , fscale_yield    ,is_available, lsubmodel, unitab)
          call hm_get_floatv("XSCALE_Y"     , Xscale_yield    ,is_available, lsubmodel, unitab)
!         #Line-6
          call hm_get_intv("FCT_ID_B"       , fctID_burnrate  ,is_available, lsubmodel)
          call hm_get_floatv("YSCALE_B"     , fscale_burnrate ,is_available, lsubmodel, unitab)
          call hm_get_floatv("XSCALE_B"     , Xscale_burnrate ,is_available, lsubmodel, unitab)

!----------------------------------------------------------------
!         DEFAULT VALUES
          call hm_get_floatv_dim("YSCALE_B" ,fscale_b_unit   ,is_available, lsubmodel, unitab)
          call hm_get_floatv_dim("YSCALE_G" ,fscale_p_unit   ,is_available, lsubmodel, unitab)
          call hm_get_floatv_dim("MAT_RHO"  ,fscale_rho_unit ,is_available, lsubmodel, unitab)
          if(Xscale_burnrate == zero) Xscale_burnrate = one*fscale_p_unit
          if(Xscale_shear    == zero) Xscale_shear = one*fscale_rho_unit
          if(Xscale_yield    == zero) Xscale_yield = one*fscale_p_unit
          if(fscale_burnrate == zero) fscale_burnrate = one*fscale_b_unit
          if(fscale_shear == zero)    fscale_shear = one*fscale_p_unit
          if(fscale_yield == zero)    fscale_yield = one*fscale_p_unit
!----------------------------------------------------------------
         mtl_msg = ''

          if(fscale_shear < zero)then
            ! 0.0 means no elastic increment (pressure only)
            mtl_msg = "POWDER PARAMETER G-SCALE MUST BE POSITIVE"
            CALL ANCMSG(MSGID=856, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=133, I2=MAT_ID, C1="ERROR", C2=TITR, C3=mtl_msg)
          end if

          if(fscale_yield < zero)then
           ! 0.0 means elastic without plasticity
            mtl_msg = "POWDER PARAMETER Y-SCALE MUST BE POSITIVE"
            CALL ANCMSG(MSGID=856, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=133, I2=MAT_ID, C1="ERROR", C2=TITR, C3=mtl_msg)
          end if

          if(fscale_burnrate < zero)then
           ! 0.0 means elastic without plasticity
            mtl_msg = "POWDER PARAMETER B-SCALE MUST BE POSITIVE"
            CALL ANCMSG(MSGID=856, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=133, I2=MAT_ID, C1="ERROR", C2=TITR, C3=mtl_msg)
          end if

          if(dd <= zero)then
            mtl_msg = "GAS EOS PARAMETER D MUST BE DEFINED"
            CALL ANCMSG(MSGID=856, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=133, I2=MAT_ID, C1="ERROR", C2=TITR, C3=mtl_msg)
          end if

          if(rhoe_g <= zero)then
            mtl_msg = "GAS EOS PARAMETER RHO.E MUST BE DEFINED"
            CALL ANCMSG(MSGID=856, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=133, I2=MAT_ID, C1="ERROR", C2=TITR, C3=mtl_msg)
          end if

          if(gr <= zero)then
            mtl_msg = "GROWTH PARAMETER Gr MUST BE DEFINED"
            CALL ANCMSG(MSGID=856, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=133, I2=MAT_ID, C1="ERROR", C2=TITR, C3=mtl_msg)
          end if

          if(c1 <= zero)then
            mtl_msg = "BURNING VELOCITY C1 MUST BE DEFINED"
            CALL ANCMSG(MSGID=856, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=133, I2=MAT_ID, C1="ERROR", C2=TITR, C3=mtl_msg)
          end if

          e_g = rhoe_g/rho0

          uparam(01) = nu
          uparam(02) = pmin
          uparam(03) = fscale_shear
          uparam(04) = fscale_yield
          uparam(05) = dd
          uparam(06) = e_g
          uparam(07) = gr
          uparam(08) = c
          uparam(09) = alpha
          uparam(10) = c1
          uparam(11) = Xscale_shear
          uparam(12) = fscale_burnrate
          uparam(13) = Xscale_yield
          uparam(14) = Xscale_burnrate
          ifunc(1)   = fctID_burnrate
          ifunc(2)   = fctID_shear
          ifunc(3)   = fctID_yield

          !NUVAR = 7
          !UVAR(,1) :  !M0
          !UVAR(,2) :  !F
          !UVAR(,3) :  !RHO_S
          !UVAR(,4) :  !RHO_G
          !UVAR(,5) :  !V_S
          !UVAR(,6) :  !V_g

          !======== MATPARAM KEYWORDS
          ! EOS/Thermo keyword
          CALL INIT_MAT_KEYWORD(MATPARAM,"HYDRO_EOS")
          ! Properties compatibility
          CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC")
          CALL INIT_MAT_KEYWORD(MATPARAM,"SPH")

          !updated in law105_upd() subroutine
          parmat(1) = zero !bulk  (from EoS)
          parmat(2) = zero !young (need function internal ids)
          parmat(3) = nu

          pm(32) = zero ! default EoS
          pm(89) = rho0
          pm(1) = rho0
          pm(38) = c1
          pm(88) = zero ! PSH : will be erased by EoS if parameter is defined.
          pm(23) = zero !e0

          write(iout,1000)

          if(is_encrypted)then
            write(iout,"(5X,A,//)")"CONFIDENTIAL DATA"
          else
            write(iout,1500) rho0,nu,pmin,&
            fctID_shear, fscale_shear, Xscale_shear,&
            fctID_Yield, fscale_yield, Xscale_yield,&
            dd,rhoe_g,gr,c,alpha, &
            fctID_burnrate,fscale_burnrate,xscale_burnrate, &
            c1
          end if

          return
! ----------------------------------------------------------------------------------------------------------------------
1000      format(&
            5X,"  POWDER BURN MATERIAL ",/,&
            5X,"  -------------------- ",/)
! ----------------------------------------------------------------------------------------------------------------------
1500      format( &
            5X,"-- POWDER PARAMETERS --",/,&
            5X,"REFERENCE DENSITY . . . . . . . . . . . .=",1PG20.13/,&
            5X,"POISSON'S RATIO . . . . . . . . . . . . .=",1PG20.13/,&
            5X,"MINIMUM PRESSURE  . . . . . . . . . . . .=",1PG20.13/,&
            5X,"SHEAR MODULUS FUNCTION ID   . . . . . . .=",I20/,&
            5X,"- SHEAR MODULUS ORDINATE SCALE FACTOR . .=",1PG20.13/,&
            5X,"- SHEAR MODULUS ABSCISSA SCALE FACTOR . .=",1PG20.13/,&
            5X,"YIELD STRENGTH FUNCTION ID  . . . . . . .=",I20/,&
            5X,"- YIELD STRENGTH ORDINATE SCALE FACTOR . =",1PG20.13/,&
            5X,"- YIELD STRENGTH ABSCISSA SCALE FACTOR . =",1PG20.13//,&
            5X,"-- GAS PARAMETERS --",/,&
            5X,"D EXPONENTIAL EOS PARAMETER . . . . . . .=",1PG20.13/,&
            5X,"RHO.E VOLUMETRIC SPECIFIC ENERGY  . . . .=",1PG20.13//,&
            5X,"-- GROWTH MODEL --",/,&
            5X,"Gr GROWTH PARAMETER . . . . . . . . . . .=",1PG20.13/,&
            5X,"C GROWTH REACTION RATIO . . . . . . . . .=",1PG20.13/,&
            5X,"ALPHA REACTION RATIO FACTOR . . . . . . .=",1PG20.13/,&
            5X,"BURN RATE FUNCTION ID  . .  . . . . . . .=",I20/,&
            5X,"- BURN RATE ORDINATE SCALE FACTOR. . . . =",1PG20.13/,&
            5X,"- BURN RATE ABSCISSA SCALE FACTOR. . . . =",1PG20.13//,&
            5X,"-- SPEED OF IGNITION FRONT --",/,&
            5X,"C1 BURN FRONT VELOCITY PARAMETER. . . . .=",1PG20.13//)
! ----------------------------------------------------------------------------------------------------------------------
          return
        end subroutine hm_read_mat105
      end module hm_read_mat105_mod
