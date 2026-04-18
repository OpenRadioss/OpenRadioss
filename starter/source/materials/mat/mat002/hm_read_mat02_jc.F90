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
!||    hm_read_mat02_jc_mod   ../starter/source/materials/mat/mat002/hm_read_mat02_jc.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat            ../starter/source/materials/mat/hm_read_mat.F90
!||====================================================================
      module hm_read_mat02_jc_mod
      contains

! ======================================================================================================================
! \brief Reading material parameters of /MAT/LAW02
! ======================================================================================================================

!||====================================================================
!||    hm_read_mat02_jc         ../starter/source/materials/mat/mat002/hm_read_mat02_jc.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    init_mat_keyword         ../starter/source/materials/mat/init_mat_keyword.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    file_descriptor_mod      ../starter/source/modules/file_descriptor_mod.F90
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_mat02_jc(mat_param ,mtag     ,parmat   ,              &
          nuvar    ,unitab   ,mat_id   ,titr     ,lsubmodel,    &
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
          type (submodel_data) ,dimension(*) ,intent(in) :: lsubmodeL
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
          logical :: is_available,is_encrypted
          integer :: nfunc,ntable,niparam,nuparam
          integer :: israte,icc,vp
          integer :: iflag,iform,iformdt
          real(kind=wp) :: rhor,rho0
          real(kind=wp) :: young,anu,ca,cb,cn,epsm,sigm,cc,eps0,g,bulk,gfac
          real(kind=wp) :: a11,a12,sdsp
          real(kind=wp) :: fcut,asrate,fisokin,cb0,rm,ag,cn0
          real(kind=wp) :: rhocp,tref,tmelt,tmax,pmin,m_exp
!===============================================================================
          is_encrypted = .false.
          is_available = .false.

          call hm_option_is_encrypted(is_encrypted)
!-----------------------------------------------
          !line-1
          call hm_get_floatv('RHO_I'  ,rho0     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('RHO_O'  ,rhor     ,is_available, lsubmodel, unitab)
          !line-2
          call hm_get_floatv('E'      ,young    ,is_available, lsubmodel, unitab)
          call hm_get_floatv('Nu'     ,anu      ,is_available, lsubmodel, unitab)
          call hm_get_intv  ('MAT_VP' ,vp       ,is_available, lsubmodel)
          call hm_get_intv  ('Iflag'  ,iflag    ,is_available, lsubmodel)
          call hm_get_floatv('Pmin'   ,pmin     ,is_available, lsubmodel, unitab)
          !line-3
          if (iflag == 1) then
            call hm_get_floatv('SIG_Y'        ,ca     ,is_available, lsubmodel, unitab)
            call hm_get_floatv('UTS'          ,cb     ,is_available, lsubmodel, unitab)
            call hm_get_floatv('EUTS'         ,cn     ,is_available, lsubmodel, unitab)
            call hm_get_floatv('EPS_p_max'    ,epsm   ,is_available, lsubmodel, unitab)
            call hm_get_floatv('SIG_max0'     ,sigm   ,is_available, lsubmodel, unitab)
          else
            call hm_get_floatv('a'            ,ca     ,is_available, lsubmodel, unitab)
            call hm_get_floatv('b'            ,cb     ,is_available, lsubmodel, unitab)
            call hm_get_floatv('n'            ,cn     ,is_available, lsubmodel, unitab)
            call hm_get_floatv('EPS_p_max'    ,epsm   ,is_available, lsubmodel, unitab)
            call hm_get_floatv('SIG_max0'     ,sigm   ,is_available, lsubmodel, unitab)
          endif
          !line-4
          call hm_get_floatv('c'              ,cc        ,is_available, lsubmodel, unitab)
          call hm_get_floatv('EPS_DOT_0'      ,eps0      ,is_available, lsubmodel, unitab)
          call hm_get_intv  ('ICC'            ,icc       ,is_available, lsubmodel)
          call hm_get_intv  ('Fsmooth'        ,israte    ,is_available, lsubmodel)
          call hm_get_floatv('F_cut'          ,fcut      ,is_available, lsubmodel, unitab)
          call hm_get_floatv('Chard'          ,fisokin   ,is_available, lsubmodel, unitab)
          !line-5
          call hm_get_floatv('m'              ,m_exp     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('T_melt'         ,tmelt     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('rhoC_p'         ,rhocp     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('T_r'            ,tref      ,is_available, lsubmodel, unitab)
          call hm_get_floatv('T_max'          ,tmax      ,is_available, lsubmodel, unitab)
!-----------------------------------------------------------------------
          ! Default values
!-----------------------------------------------------------------------
          iform   = 0    ! J-C formulation flag
          iformdt = 2    ! for solid elements time step computation
!
          if (cn == zero) cn = one
          if (iflag == 1) then   ! parameters b and n must be recomputed
            cb0 = cb
            rm  = cb *(one+cn)
            ag  = log(one+cn)
            cn0 = cn
            cn  = rm*ag / (rm-ca)
            cb  = rm/max((cn*ag**(cn-one)),em20)
            if (cn > one) then
              cn = one
              cb = (cb0*(one+cn0)-ca)/(log(1+cn0)-cb0*(1+cn0)/young-ca/young)
              call ancmsg(MSGID=277, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,     &
                i1=mat_id,  c1=titr)
            endif
            if (cn < zero .and. cb < zero) then
              cn = zero
              cb = zero
              call ancmsg(MSGID=278, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,     &
                i1=mat_id, c1=titr)
            endif
          endif
!
          if (rhor == zero) rhor = rho0
          if (anu  == half) anu  = zep499
          if (pmin == zero) pmin =-ep20
          if (epsm == zero) epsm = ep20
          if (sigm == zero) sigm = ep20
!
          if (icc == 0) icc = 1
!
          if (vp == 0) vp = 2
          ! vp = 1   => plastic strain rate
          ! vp = 2   => total strain rate (default)
          ! vp = 3   => total deviatoric strain rate  (shells)
!
          israte = 1
          if (cc == zero) then
            eps0   = one
            israte = 0
          endif
          if (vp == 1) then     ! plastic strain rate is filtered by default
            fcut = 10000.0d0*unitab%fac_t_work
          endif
          if (fcut == zero) then
            asrate = ep20
          else
            asrate = two*pi*fcut
          endif
!
          if (tref <= zero) tref = three100
          if (iform == 0) then  ! Johnson-Cook
            if (m_exp == zero) m_exp = one
            if (tmelt == zero) tmelt = ep20
            if (tmax  == zero) tmax  = ep20
          end if
!-----------------------------------------------------------------------
          ! Invalid values check
!-----------------------------------------------------------------------
          if (cc > zero .and. eps0 > zero  .and. fcut == zero .and. vp /= 1) then
            call ancmsg(msgid=1220, msgtype=msgwarning, anmode=aninfo_blind_1,   &
              i1=mat_id, c1=titr)
          endif
!
          if (anu <= -one)  then
            call ancmsg(MSGID=300,MSGTYPE=MSGERROR,ANMODE=ANINFO,i1=2,i2=mat_id,c1=titr)
          endif
          if (young <= zero) then
            call ancmsg(MSGID=276,MSGTYPE=MSGERROR,ANMODE=ANINFO,i1=2,i2=mat_id,c1=titr)
          endif
          if (fisokin > one .or. fisokin < zero) then
            call ancmsg(MSGID=912, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1, &
              i1=mat_id,c1='2', c2=titr)
          end if
!
          if (ca <= zero) then
            call ancmsg(MSGID=301, MSGTYPE=MSGERROR, ANMODE=ANINFO, &
              i1=2, i2=mat_id, c1=titr)
          endif
          if (cn > one) then
            call ancmsg(MSGID=213, MSGTYPE=MSGERROR, ANMODE=ANINFO,  &
              i1=2, i2=mat_id, c1=titr)
          endif
          if (eps0 == zero) then
            call ancmsg(MSGID=298, MSGTYPE=MSGERROR, ANMODE=ANINFO,  &
              i1=2, i2=mat_id, c1=titr)
          endif
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
          pm(50) = zero      ! flag jc
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
            write(iout,1300)young,anu,g
!
            if (iflag == 0) then
              write(iout,1400)ca,cb,cn,epsm,sigm,fisokin
            else
              write(iout,1405)ca,cb0,cn0,ca,cb,cn,epsm,sigm,fisokin
            endif
            write(iout,1600) vp,cc,eps0,icc,israte,fcut,m_exp,tmelt,rhocp,tref,tmax,pmin
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
          mat_param%uparam(9)  = asrate         ! pm(9)     ! uparam not used

          mat_param%uparam(10) = m_exp          ! pm(51)
          mat_param%uparam(11) = tmax
          mat_param%uparam(12) = pmin           ! pm(37) default pressure cut-off for eos

          ! thermal parameters - common with heat mat

          mat_param%therm%rhocp = rhocp
          ! pm(69)
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
1010      format(//                                                                     &
            5x,A,/,                                                                        &
            5x,40hMATERIAL NUMBER . . . . . . . . . . . .=,i10/,                           &
            5x,'MATERIAL LAW  . . . . . . . . . . . . . .= PLAS_JOHNS',/)
1000      format(                                                                         &
            5x,'  ELASTIC PLASTIC LAW                   ',/,                               &
            5x,'  -------------------                   ',//)
1100      format(                                                                         &
            5x,'INITIAL DENSITY . . . . . . . . . . . .=',1pg20.13/)
1300      format(                                                                         &
            5x,'YOUNG MODULUS . . . . . . . . . . . . .=',1pg20.13/,                       &
            5x,'POISSON  RATIO. . . . . . . . . . . . .=',1pg20.13/,                       &
            5x,'SHEAR MODULUS . . . . . . . . . . . . .=',1pg20.13//)
1400      format(                                                                         &
            5x,'JOHNSON COOK MODEL :',/,                                                   &
            5x,'YIELD COEFFICIENT A . . . . . . . . . .=',1pg20.13/,                       &
            5x,'YIELD COEFFICIENT B . . . . . . . . . .=',1pg20.13/,                       &
            5x,'YIELD COEFFICIENT N . . . . . . . . . .=',1pg20.13/,                       &
            5x,'EPS-MAX . . . . . . . . . . . . . . . .=',1pg20.13/,                       &
            5x,'SIG-MAX . . . . . . . . . . . . . . . .=',1pg20.13/,                       &
            5x,'ISO-KINEMATIC HARDENING FACTOR. . . . .=',1pg20.13//)
1405      format(                                                                        &
            5x,'JOHNSON COOK MODEL :',/,                                                   &
            5x,'YIELD STRESS  . . . . . . . . . . . . .=',1pg20.13/,                       &
            5x,'ULTIMATE STRESS (UTS) . . . . . . . . .=',1pg20.13/,                       &
            5x,'STRAIN AT UTS (Ag). . . . . . . . . . .=',1pg20.13/,                       &
            5x,'YIELD COEFFICIENT A . . . . . . . . . .=',1pg20.13/,                       &
            5x,'YIELD COEFFICIENT B . . . . . . . . . .=',1pg20.13/,                       &
            5x,'YIELD COEFFICIENT N . . . . . . . . . .=',1pg20.13/,                       &
            5x,'EPS-MAX . . . . . . . . . . . . . . . .=',1pg20.13/,                       &
            5x,'SIG-MAX . . . . . . . . . . . . . . . .=',1pg20.13/,                       &
            5x,'ISO-KINEMATIC HARDENING FACTOR. . . . .=',1pg20.13//)
1600      format(                                                                        &
            5x,'FLAG FOR STRAIN RATE DEPENDENCY TYPE. .=',i10/,                            &
            5x,'   VP=1  EQUIVALENT PLASTIC STRAIN RATE'/                                  &
            5x,'   VP=2  TOTAL STRAIN RATE (DEFAULT)'/                                     &
            5x,'   VP=3  DEVIATORIC STRAIN RATE'/                                          &
            5x,'STRAIN RATE COEFFICIENT CC. . . . . . .=',1pg20.13/,                       &
            5x,'REFERENCE STRAIN RATE . . . . . . . . .=',1pg20.13/,                       &
            5x,'FLAG FOR STRAIN RATE ON SIG-MAX . . . .=',i10/,                            &
            5x,'SMOOTH STRAIN RATE OPTION . . . . . . .=',i10/,                            &
            5x,'STRAIN RATE CUTTING FREQUENCY . . . . .=',1pg20.13/,                       &
            5x,'TEMPERATURE EXPONENT. . . . . . . . . .=',1pg20.13/,                       &
            5x,'MELTING TEMPERATURE K . . . . . . . . .=',1pg20.13/,                       &
            5x,'SPECIFIC HEAT Rho*Cp. . . . . . . . . .=',1pg20.13/,                       &
            5x,'INITIAL TEMPERATURE K . . . . . . . . .=',1pg20.13/,                       &
            5x,'MAXIMAL TEMPERATURE K . . . . . . . . .=',1pg20.13/,                       &
            5x,'PRESSURE CUTOFF IN TENSION. . . . . . .=',1pg20.13//)
1700      format(5x,'CONFIDENTIAL DATA'//)
!-----------------------------------------------------------------------
        end subroutine hm_read_mat02_jc
!-------------------
      end module hm_read_mat02_jc_mod
