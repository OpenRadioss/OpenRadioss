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
! ======================================================================================================================


!||====================================================================
!||    hm_read_mat02_predef_mod   ../starter/source/materials/mat/mat002/hm_read_mat02_predef.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat                ../starter/source/materials/mat/hm_read_mat.F90
!||====================================================================
      module hm_read_mat02_predef_mod
      contains

! ======================================================================================================================
! \brief Reading material parameters of /MAT/LAW02
! ======================================================================================================================

!||====================================================================
!||    hm_read_mat02_predef     ../starter/source/materials/mat/mat002/hm_read_mat02_predef.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_string            ../starter/source/devtools/hm_reader/hm_get_string.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    init_mat_keyword         ../starter/source/materials/mat/init_mat_keyword.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    file_descriptor_mod      ../starter/source/modules/file_descriptor_mod.F90
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_mat02_predef(mat_param ,mtag     ,parmat   ,    &
          nuvar    ,unitab   ,mat_id   ,titr     ,    &
          npropm   ,pm       ,npropmi  ,ipm      )
!-----------------------------------------------
!   m o d u l e s
!-----------------------------------------------
          use unitab_mod
          use elbuftag_mod
          use message_mod
          use submodel_mod
          use matparam_def_mod
          use names_and_titles_mod , only : nchartitle
          use file_descriptor_mod
          use constant_mod
          use precision_mod, only : wp
!-----------------------------------------------
          implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
          integer, intent(in)    :: mat_id
          integer, intent(inout) :: nuvar
          integer, intent(in)    :: npropm
          integer, intent(in)    :: npropmi
          integer, intent(inout) :: ipm(npropmi)
          real(kind=wp)  ,intent(inout) :: pm(npropm)
          real(kind=wp)  ,intent(inout) :: parmat(128)
          type(mlaw_tag_),intent(inout) :: mtag
          character(len=nchartitle) ,intent(in) :: titr
          type (unit_type_)         ,intent(in) :: unitab
          type (matparam_struct_)   ,intent(inout) :: mat_param
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
          logical :: is_available,is_encrypted
          integer :: nfunc,ntable,niparam,nuparam
          integer :: israte,icc,vp
          integer :: iform,iformdt,mflag
          real(kind=wp) :: rhor,rho0
          real(kind=wp) :: young,anu,ca,cb,cn,epsm,sigm,cc,eps0,g,bulk,gfac
          real(kind=wp) :: a11,a12,sdsp
          real(kind=wp) :: fcut,fisokin
          real(kind=wp) :: rhocp,tref,tmelt,tmax,pmin,m_exp
          real(kind=wp) :: fac_dens,fac_pres,fac_m,fac_l,fac_t
          character :: predef*16
!===============================================================================
          is_encrypted = .false.
          is_available = .false.

          call hm_option_is_encrypted(is_encrypted)
!-----------------------------------------------
          !/MAT/PLAS_PREDEF

          call hm_get_string('Material_Name_Str'  ,Predef     ,16, is_available)

          if     (predef(1:5)  == 'STEEL')    then
            mflag = 1
          elseif (predef(1:3)  == 'HSS')      then
            mflag = 2
          elseif (predef(1:4)  == 'UHSS')     then
            mflag = 3
          elseif (predef(1:6)  == 'AA5182')   then
            mflag = 4
          elseif (predef(1:9)  == 'AA6082-T6') then
            mflag = 5
          elseif (predef(1:7)  == 'PA6GF30')  then
            mflag = 6
          elseif (predef(1:5)  == 'PPT40')    then
            mflag = 7
          else
            mflag =999 ! to unplug parameter check with plas_johns and plas_zeril
            call ancmsg(MSGID=769, MSGTYPE=MSGERROR, ANMODE=ANINFO,            &
              i1=mat_id, c1=titr, c2=predef)
          endif
!-----------------------------------------------------------------------
          fac_m = unitab%fac_m_work
          fac_l = unitab%fac_l_work
          fac_t = unitab%fac_t_work
          fac_pres = fac_m/ (fac_l*fac_t*fac_t)
          fac_dens = fac_m/ (fac_l*fac_l*fac_l)
          select case (mflag)
           case(1)                                ! mild steel
            rho0  = 7850d0           / fac_dens
            young = 210000000000.0d0 / fac_pres
            anu   = 0.3d0
            ca    = 160000000.0d0    / fac_pres
            cb    = 513330169.33870d0/ fac_pres
            cn    = 0.3257084899598d0
           case(2)                                ! hss steel
            rho0  = 7850d0           / fac_dens
            young = 210000000000.0d0 / fac_pres
            anu   = 0.3d0
            ca    = 300000000.0d0    / fac_pres
            cb    = 611407465.14830d0/ fac_pres
            cn    = 0.3967613457219d0
           case(3)
            rho0  = 7850d0           / fac_dens
            young = 210000000000.0d0 / fac_pres
            anu   = 0.3d0
            ca    = 500000000.0d0    / fac_pres
            cb    = 1306278496.3090d0/ fac_pres
            cn    = 6.4633693574514d-02
           case(4)                                   ! aluminium aa5182
            rho0  = 2700d0           / fac_dens
            young = 70000000000.0d0  / fac_pres
            anu   = 0.33d0
            ca    = 150000000.0d0    / fac_pres
            cb    = 393050051.47810d0/ fac_pres
            cn    = 0.3719059188570d0
           case(5)                                   ! aluminium aa6082-T6
            rho0  = 2700d0           / fac_dens
            young = 70000000000.0d0  / fac_pres
            anu   = 0.33d0
            ca    = 300000000.0d0    / fac_pres
            cb    = 210717297.9723d0 / fac_pres
            cn    = 0.3369645584879d0
           case(6)                                    ! plastic pa6gf30
            rho0  = 1300d0           / fac_dens
            young = 7000000000.0d0   / fac_pres
            anu   = 0.35d0
            ca    = 50000000.0d0     / fac_pres
            cb    = 60557060.655832d0/ fac_pres
            cn    = 3.8843615080968d-02
           case(7)                                    ! generic pp t40
            rho0  = 1200d0           / fac_dens
            young = 4000000000d0     / fac_pres
            anu   = 0.3d0
            ca    = 20000000.0d0     / fac_pres
            cb    = 18439331.380790d0/ fac_pres
            cn    = 0.1570297693511d0
           case default                               ! else --> mild seel
            rho0  = 7850d0           / fac_dens
            young = 210000000000d0   / fac_pres
            anu   = 0.3d0
            ca    = 160000000.0d0    / fac_pres
            cb    = 513330169.33870d0/ fac_pres
            cn    = 0.3257084899598d0
          end select
!-----------------------------------------------------------------------
          ! Default values
!-----------------------------------------------------------------------
          iform   = 2    ! Predef J-C formulation flag
          iformdt = 2    ! for solid elements time step computation
!
          rhor = rho0
          if (anu  == half) anu  = zep499
!
          vp = 2    !  total strain rate (default)
          fisokin = zero
          fcut    = zero
          israte = 1
          icc    = 1
          cc     = zero
          eps0   = one
          israte = 0
!
          tref  = three100
          m_exp = one
          rhocp = ep20
          tmelt = ep20
          tmax  = ep20
          pmin  =-ep20
          epsm  = ep20
          sigm  = ep20
!-----------------------------------------------------------------------
          g    = young / (two*(one + anu))
          bulk = young / (three*(one - two*anu))
          a11  = young/(one - anu**2)
          a12  = anu*a11
          gfac = two*g/(bulk+four_over_3*g)
          sdsp = sqrt(young/max(rhor,em20))
!-----------------------------------------------------------------------
          ! used in code outside of material laws
!-----
          pm(24) = a11
          pm(25) = a12
          pm(26) = five*one_over_6
          pm(27) = sdsp
          pm(37) = pmin      ! default pressure cut-off for eos
          pm(69) = rhocp
          pm(79) = tref      ! for j-c
          pm(80) = tmelt
          pm(105)= gfac
!-----
          pm(50) = iform     ! flag predef-jc
          pm(51) = m_exp
          pm(80) = tmelt
!
          pm(1)  = rhor
          pm(89) = rho0
          pm(20) = young
          pm(21) = anu
          pm(22) = g
          pm(28) = one/young
          pm(29) =-anu*pm(28)
          pm(30) = one/g
          pm(32) = bulk
          pm(38) = ca
          pm(39) = cb
          pm(40) = cn
          pm(41) = epsm
          pm(42) = sigm
          pm(43) = cc
          pm(44) = eps0
          pm(47) = tmax    ! like in law4
          pm(49) = icc
          if (rhocp <= zero) then
            pm(53) = zero
          else
            pm(53) = one/rhocp
          endif
          pm(55)=fisokin
!------------------
          ipm(255) = vp
!-----------------------
          parmat(4)  = israte
          parmat(5)  = fcut
          parmat(16) = iformdt  ! ipm(252) - solid elements time step formulation
          parmat(17) = gfac
!-----------------------------------------------------------------------
          ! Output
!-----------------------------------------------------------------------
          write(iout,1010) trim(titr),mat_id
          write(iout,1000)
          if (.not.is_encrypted) then
            write(iout,1100) rho0
!
            if (mflag == 1) then
              write (iout,1407) 'GENERIC MILD STEEL',rho0,young,anu,g,                &
                160000000.0d0/FAC_PRES,380000000.0d0/fac_pres,0.24d0
            endif
            if (mflag == 2) then
              write (iout,1407) 'GENERIC HSS STEEL',rho0,young,anu,g,                  &
                300000000.0d0/FAC_PRES,510000000.0d0/fac_pres,0.23d0
            endif
            if (mflag == 3) then
              write (iout,1407) 'GENERIC UHSS STEEL',rho0,young,anu,g,                 &
                500000000.0d0/FAC_PRES,1500000000.0d0/fac_pres,0.045d0
            endif
            if (mflag == 4) then
              write (iout,1407) 'GENERIC ALUMINIUM: AA5182',rho0,young,anu,g,          &
                150000000.0d0/fac_pres,300000000.0d0/fac_pres,0.25d0
            endif
            if (mflag == 5) then
              write (iout,1407) 'GENERIC ALUMINIUM: AA6082-T6',rho0,young,anu,g,       &
                300000000.0d0/fac_pres,360000000.0d0/fac_pres,0.08d0
            endif
            if (mflag == 6) then
              write (iout,1407) 'GENERIC PA6GF30',rho0,young,anu,g,                    &
                50000000.0d0/fac_pres,100000000.0d0/fac_pres,0.02d0
            endif
            if (mflag == 7) then
              write (iout,1407) 'GENERIC PP T40',rho0,young,anu,g,                     &
                20000000.0d0/fac_pres,30000000.0d0/fac_pres,0.06d0
            endif
            if (mflag > 7)  then
              write (iout,1407) 'GENERIC MILD STEEL',rho0,young,anu,g,                 &
                160000000.0d0/fac_pres,380000000.0d0/fac_pres,0.24d0
            endif
!
            write(iout,1400) ca,cb,cn

          else
            write(iout,1700)
          end if   ! is encrypted
!-------------------------------------------------------------------------------
!     new mat_param storage
!-----------------------------------------------------------------------
          nuvar   = 1
          niparam = 4
          nuparam = 12
          nfunc   = 0
          ntable  = 0
!
          mat_param%niparam = niparam
          mat_param%nuparam = nuparam
          mat_param%nfunc   = nfunc
          mat_param%ntable  = ntable
          allocate (mat_param%iparam(niparam))
          allocate (mat_param%uparam(nuparam))
          allocate (mat_param%table(ntable))
!
          mat_param%iparam(1)  = iform          ! pm(50) formulation flag of law2
          mat_param%iparam(2)  = icc            ! pm(49)
          mat_param%iparam(3)  = vp             ! ipm(252)
          mat_param%iparam(4)  = israte         ! ipm(3)
!
          mat_param%uparam(1)  = ca             ! pm(38)
          mat_param%uparam(2)  = cb             ! pm(39)
          mat_param%uparam(3)  = cn             ! pm(40)
          mat_param%uparam(4)  = epsm           ! pm(41)
          mat_param%uparam(5)  = sigm           ! pm(42)
          mat_param%uparam(6)  = cc             ! pm(43)
          mat_param%uparam(7)  = eps0           ! pm(44)
          mat_param%uparam(8)  = fisokin        ! pm(55)
          mat_param%uparam(9)  = fcut*two*pi    ! pm(9)
          mat_param%uparam(10) = m_exp          ! pm(51)
          mat_param%uparam(11) = tmax
          mat_param%uparam(12) = pmin           ! pm(37) default pressure cut-off for eos

          ! thermal parameters - common with heat mat

          mat_param%therm%rhocp = rhocp         ! pm(69)
          mat_param%therm%tref  = tref          !
          mat_param%therm%tini  = tref          ! pm(79)
          mat_param%therm%tmelt = tmelt         ! pm(80)
!----------------------------------
          ! mat_param common parameters
!----------------------------------
          mat_param%rho   = rhor
          mat_param%rho0  = rho0
          mat_param%young = young
          mat_param%bulk  = bulk
          mat_param%shear = g
          mat_param%nu    = anu
!
!---- Definition of internal variables for element buffer allocation
!
          mtag%g_epsd  = 1
          mtag%g_pla   = 1
          mtag%g_dmg   = 1
          mtag%g_temp  = 1
!
          mtag%l_epsd  = 1
          mtag%l_epsq  = 1
          mtag%l_pla   = 1
          mtag%l_sigb  = 6
          mtag%l_dmg   = 1
          mtag%l_temp  = 1
!
          ! activate heat source calculation in material
          mat_param%heat_flag = 1
!-------------------------
          call init_mat_keyword(mat_param,"ELASTO_PLASTIC")
          call init_mat_keyword(mat_param,"INCREMENTAL")
          call init_mat_keyword(mat_param,"LARGE_STRAIN")
          ! properties compatibility
          call init_mat_keyword(mat_param,"SOLID_ISOTROPIC")
          call init_mat_keyword(mat_param,"SHELL_ISOTROPIC")
          call init_mat_keyword(mat_param,"BEAM_ALL")
          call init_mat_keyword(mat_param,"TRUSS")
          call init_mat_keyword(mat_param,"SPH")
!
          ! Material compatibility with /EOS option
          call init_mat_keyword(mat_param,"EOS")
!-----------
          return
!-----------------------------------------------------------------------
1010      format(//                                                                      &
            5x,A,/,                                                                        &
            5x,40hMATERIAL NUMBER . . . . . . . . . . . .=,i10/,                           &
            5x,'MATERIAL LAW  . . . . . . . . . . . . . .= PLAS_PREDEF',/)
1000      format(                                                                         &
            5x,'  ELASTIC PLASTIC LAW                   ',/,                               &
            5x,'  -------------------                   ',//)
1100      format(                                                                         &
            5x,'INITIAL DENSITY . . . . . . . . . . . .=',1pg20.13/)
1400      format(                                                                         &
            5x,'JOHNSON COOK MODEL :',/,                                                   &
            5x,'YIELD COEFFICIENT A . . . . . . . . . .=',1pg20.13/,                       &
            5x,'YIELD COEFFICIENT B . . . . . . . . . .=',1pg20.13/,                       &
            5x,'YIELD COEFFICIENT N . . . . . . . . . .=',1pg20.13/)
1407      format(                                                                         &
            5x,'PREDEFINED VALUES USED FOR. . . . . . .: ',A/,                             &
            5x,'DENSITY . . . . . . . . . . . . . . . .=',1pg20.13/,                       &
            5x,'YOUNG''S MODULUS. . . . . . . . . . . .=',1pg20.13/,                       &
            5x,'POISSON''S RATIO. . . . . . . . . . . .=',1pg20.13/,                       &
            5x,'SHEAR MODULUS . . . . . . . . . . . . .=',1pg20.13/,                       &
            5x,'YIELD STRESS. . . . . . . . . . . . . .=',1pg20.13/,                       &
            5x,'ULTIMATE STRESS (UTS) . . . . . . . . .=',1pg20.13/,                       &
            5x,'STRAIN AT UTS (Ag). . . . . . . . . . .=',1pg20.13//)
1700      format(5x,'CONFIDENTIAL DATA'//)
!-----------------------------------------------------------------------
        end subroutine hm_read_mat02_predef
!-------------------
      end module hm_read_mat02_predef_mod
