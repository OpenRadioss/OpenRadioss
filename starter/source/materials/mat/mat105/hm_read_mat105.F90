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

          real(kind=WP), dimension(maxuparam) ,intent(inout)       :: uparam
          real(kind=WP), dimension(npropm) ,intent(inout)          :: pm
          real(kind=WP), dimension(100),intent(inout)              :: parmat
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=WP) :: P0, E0, PSH
          real(kind=WP) :: C1,C2,BULK,Gr,DD,EG,c,alpha,MU0,FSCALE_g, FSCALE_rho, FSCALE_b, FSCALE_P
          real(kind=WP) :: fscale_b_unit, fscale_g_unit, fscale_p_unit, fscale_rho_unit
          real(kind=WP) :: rho0, rhor
          integer :: funcb, funcg
          character(len=36) :: mtl_msg
          logical :: is_available,is_encrypted
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          is_encrypted = .false.
          is_available = .false.

          call hm_option_is_encrypted(is_encrypted)

          !======== MATERIAL BUFFER ALLOCATION SIZES
          nuvar   = 7
          nuparam = 15
          nfunc   = 2
          uparam(1:nuparam) = zero
          ifunc(1:nfunc)    = 0

          !======== ELEMENTARY BUFFER ALLOCATION SIZES
          mtag%g_tb     = 1
          mtag%g_temp   = 1
          mtag%g_bfrac  = 1
          mtag%l_tb     = 1
          mtag%l_temp   = 1
          mtag%l_bfrac  = 1

          !======== ELEMENTARY BUFFER ALLOCATION SIZES
          call hm_get_floatv("MAT_RHO"          ,rho0            ,is_available, lsubmodel, unitab)

          call hm_get_floatv("POWDER_BULK"      ,bulk            ,is_available, lsubmodel, unitab)
          call hm_get_floatv("POWDER_P0"        ,p0              ,is_available, lsubmodel, unitab)
          call hm_get_floatv("MAT_PSH"          ,psh             ,is_available, lsubmodel, unitab)

          call hm_get_floatv("GAS_D"            ,dd               ,is_available, lsubmodel, unitab)
          call hm_get_floatv("GAS_EG"           ,eg              ,is_available, lsubmodel, unitab)

          call hm_get_floatv("POWDER_Gr"        ,gr              ,is_available, lsubmodel, unitab)
          call hm_get_floatv("POWDER_C"         ,c               ,is_available, lsubmodel, unitab)
          call hm_get_floatv("Alpha"            ,alpha           ,is_available, lsubmodel, unitab)

          call hm_get_intv  ("POWDER_B_FUNC"    ,funcb           ,is_available, lsubmodel)
          call hm_get_floatv("POWDER_SCALE_B"   ,fscale_b        ,is_available, lsubmodel, unitab)
          call hm_get_floatv("POWDER_SCALE_P"   ,fscale_p        ,is_available, lsubmodel, unitab)

          call hm_get_intv  ("POWDER_GAM_FUNC"  ,funcg           ,is_available, lsubmodel)
          call hm_get_floatv("POWDER_SCALE_GAM" ,fscale_g        ,is_available, lsubmodel, unitab)
          call hm_get_floatv("POWDER_SCALE_RHO" ,fscale_rho      ,is_available, lsubmodel, unitab)

          call hm_get_floatv("MAT_C1"           ,c1              ,is_available, lsubmodel, unitab)
          call hm_get_floatv("MAT_C2"           ,c2              ,is_available, lsubmodel, unitab)

          call hm_get_floatv_dim("FSCALE_b"     ,fscale_b_unit   ,is_available, lsubmodel, unitab)
          call hm_get_floatv_dim("FSCALE_P"     ,fscale_p_unit   ,is_available, lsubmodel, unitab)
          call hm_get_floatv_dim("FSCALE_g"     ,fscale_g_unit   ,is_available, lsubmodel, unitab)
          call hm_get_floatv_dim("FSCALE_rho"   ,fscale_rho_unit ,is_available, lsubmodel, unitab)

          if(fscale_g   == zero) fscale_g   = one*fscale_g_unit
          if(fscale_rho == zero) fscale_rho = one*fscale_rho_unit
          if(fscale_p   == zero) fscale_p   = one*fscale_p_unit
          if(fscale_b   == zero) fscale_b   = one*fscale_b_unit

!      IF(GAMMA <= ZERO)THEN
!         CALL ANCMSG(MSGID=67,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=IMIDEOS,C1='/EOS/POWDER_BURN',C2='GAMMA MUST BE GREATER THAN 1.0')
!      ENDIF

          if(bulk <= zero)then
            mtl_msg = "BULK MODULUS MUST BE DEFINED        "
            CALL ANCMSG(MSGID=856, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=133, I2=MAT_ID, C1="ERROR", C2=TITR, C3=mtl_msg)
          end if

          if(dd <= zero)then
            mtl_msg = "GAS EOS PARAMETER D MUST BE DEFINED"
            CALL ANCMSG(MSGID=856, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=133, I2=MAT_ID, C1="ERROR", C2=TITR, C3=mtl_msg)
          end if

          if(eg <= zero)then
            mtl_msg = "GAS EOS PARAMETER EG MUST BE DEFINED"
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

          rhor = rho0
          mu0  = rho0/rhor-one
          e0   = p0/(one+mu0)/exp((one+mu0)*rho0/dd)

          uparam(01) = bulk
          uparam(02) = p0
          uparam(03) = psh
          uparam(04) = e0
          uparam(05) = dd
          uparam(06) = eg
          uparam(07) = gr
          uparam(08) = c
          uparam(09) = alpha
          uparam(10) = c1
          uparam(11) = c2
          uparam(12) = fscale_g
          uparam(13) = fscale_b
          uparam(14) = fscale_p
          uparam(15) = fscale_rho
          ifunc(1)   = funcb
          ifunc(2)   = funcg

          !NUVAR = 7
          !UVAR(,1) :  !PP
          !UVAR(,2) :  !PG
          !UVAR(,3) :  !RHO_P
          !UVAR(,4) :  !RHO_G
          !UVAR(,5) :  !POLD
          !UVAR(,6) :  !F(t)
          !UVAR(,7) :  !Mass0

          !======== MATPARAM KEYWORDS
          ! EOS/Thermo keyword
          CALL INIT_MAT_KEYWORD(MATPARAM,"HYDRO_EOS")
          ! Properties compatibility
          CALL INIT_MAT_KEYWORD(MATPARAM,"SOLID_ISOTROPIC")
          CALL INIT_MAT_KEYWORD(MATPARAM,"SPH")

          parmat(1) = bulk !max
          pm(32) = bulk ! default EoS
          pm(89) = rho0
          pm(1) = rho0
          pm(38) = c1
          pm(88) = psh

          write(iout,1000)

          if(is_encrypted)then
            write(iout,"(5X,A,//)")"CONFIDENTIAL DATA"
          else
            write(iout,1500)rho0,bulk,p0,dd,eg,gr,c,alpha,funcb, fscale_b, fscale_p, c1,c2,funcg, fscale_g, fscale_rho, psh
          end if

          return
! ----------------------------------------------------------------------------------------------------------------------
1000      format(&
            5X,"  POWDER BURN EOS ",/,&
            5X,"  --------------- ",/)
! ----------------------------------------------------------------------------------------------------------------------
1500      format( &
            5X,"-- POWDER EOS--",&
            5X,"REFERENCE DENSITY . . . . . . . . . . . .=",1PG20.13/,&
            5X,"BULK MODULUS. . . . . . . . . . . . . . .=",1PG20.13/,&
            5X,"INITIAL PRESSURE. . . . . . . . . . . . .=",1PG20.13/,&
            5X,"-- GAS EOS --",&
            5X,"D EXPONENTIAL EOS PARAMETER . . . . . . .=",1PG20.13/,&
            5X,"eg SPECIFIC ENERGY BY MASS. . . . . . . .=",1PG20.13//,&
            5X,"-- GROWTH MODEL --",&
            5X,"Gr      GROWTH PARAMETER. . . . . . . . .=",1PG20.13/,&
            5X,"C       GROWTH REACTION RATIO . . . . . .=",1PG20.13/,&
            5X,"ALPHA . REACTION RATIO FACTOR . . . . . .=",1PG20.13/,&
            5X,"FUNC_B BURN RATE FUNCTION.  . . . . . . .=",I20/,&
            5X,"BURN RATE ORDINATE SCALE FACTOR. . . . . =",1PG20.13/,&
            5X,"BURN RATE ABSCISSA SCALE FACTOR. . . . . =",1PG20.13//,&
            5X,"-- SPEED OF IGNITION FRONT --",&
            5X,"C1 BURN FRONT VELOCITY PARAMETER. . . . .=",1PG20.13/,&
            5X,"C2 BURN FRONT VELOCITY PARAMETER. . . . .=",1PG20.13/,&
            5X,"FUNC_GAMMA. . . . . . . . . . . . . . . .=",I20/,&
            5X,"GAMMA ORDINATE SCALE FACTOR . . . . . . .=",1PG20.13/,&
            5X,"GAMMA ABSCISSA SCALE FACTOR . . . . . . .=",1PG20.13//,&
            5X,"-- GLOBAL PARAMETER --",&
            5X,"PSH PRESSURE SHIFT. . . . . . . . . . . .=",1PG20.13/)
! ----------------------------------------------------------------------------------------------------------------------
          return
        end subroutine hm_read_mat105
      end module hm_read_mat105_mod
