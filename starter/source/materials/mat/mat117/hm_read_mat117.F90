!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
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
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    hm_read_mat117_mod   ../starter/source/materials/mat/mat117/hm_read_mat117.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat          ../starter/source/materials/mat/hm_read_mat.F90
!||====================================================================
      module hm_read_mat117_mod
      contains
!||====================================================================
!||    hm_read_mat117           ../starter/source/materials/mat/mat117/hm_read_mat117.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_floatv_dim        ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    init_mat_keyword         ../starter/source/materials/mat/init_mat_keyword.F
!||    mat_table_copy           ../starter/source/materials/tools/mat_table_copy.F90
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    mat_table_copy_mod       ../starter/source/materials/tools/mat_table_copy.F90
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||    table_mod                ../starter/share/modules1/table_mod.F
!||====================================================================
      subroutine hm_read_mat117(                                               &
        mtag     ,matparam  ,parmat   ,nuvar     ,unitab   ,mat_id  ,titr     ,&
        lsubmodel,iout      ,ntable   ,table     ,nvartmp  )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
        use unitab_mod
        use elbuftag_mod
        use message_mod
        use submodel_mod
        use matparam_def_mod
        use precision_mod
        use constant_mod
        use table_mod
        use mat_table_copy_mod
        use names_and_titles_mod , only : nchartitle
        use MY_ALLOC_MOD, only : my_alloc
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
        implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
        type(mlaw_tag_), intent(inout)              :: mtag
        type(matparam_struct_) ,intent(inout)       :: matparam
        real(kind=WP), intent(inout)                :: parmat(100)
        integer, intent(inout)                      :: nuvar
        type (unit_type_),intent(in)                :: unitab
        integer, intent(in)                         :: mat_id
        character(len=nchartitle) ,intent(in)       :: titr
        type(submodel_data),intent(in)              :: lsubmodel(*)
        integer, intent(in)                         :: iout
        integer, intent(in)                         :: ntable
        type(ttable) ,dimension(ntable) ,intent(in) :: table
        integer, intent(inout)                      :: nvartmp
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
        integer :: ilaw,imass,idel,irupt,icrittyp,ievoshap,isym,ifunc(2)
        real(kind=wp) ::                                                       &
          rho0,fscalex,fscalex_unit,udn,uds,alpha,exp_alpha,denom,             &
          e_elas_n,e_elas_s,tmax_n,tmax_s,coef_n,coef_t,coef0,func_alpha,      &
          gic,giic,exp_g,exp_bk,delta0n,delta0s,und,utd,gama
          
        real(kind=WP) ::                                                       &
          x1scale,x2scale,x3scale,x4scale,x2vect(2),x3vect(2),x4vect(2),       &
          fscale(2)
        logical :: is_available,is_encrypted
!=======================================================================
        ilaw = 117
        is_encrypted   = .false.
        is_available = .false.
!
        call hm_option_is_encrypted(is_encrypted)
!
        !< Card 1 
        call hm_get_floatv('MAT_RHO'        ,rho0         ,is_available, lsubmodel, unitab)
        call hm_get_floatv('MAT_E_ELAS_N'   ,e_elas_n     ,is_available, lsubmodel, unitab)
        call hm_get_floatv('MAT_E_ELAS_S'   ,e_elas_s     ,is_available, lsubmodel, unitab)
        call hm_get_intv  ('MAT_IMASS'      ,imass        ,is_available, lsubmodel)
        call hm_get_intv  ('MAT_IDEL'       ,idel         ,is_available, lsubmodel)
        call hm_get_intv  ('MAT_IRUPT'      ,irupt        ,is_available, lsubmodel)
        call hm_get_intv  ('MAT_ISYM'       ,isym         ,is_available, lsubmodel)
        !< Card 2
        call hm_get_intv  ('MAT_Fct_TN'     ,ifunc(1)     ,is_available, lsubmodel)
        call hm_get_intv  ('MAT_Fct_TT'     ,ifunc(2)     ,is_available, lsubmodel)
        call hm_get_floatv('MAT_TMAX_N'     ,tmax_n       ,is_available, lsubmodel, unitab)
        call hm_get_floatv('MAT_TMAX_S'     ,tmax_s       ,is_available, lsubmodel, unitab)
        call hm_get_floatv('MAT_Fscale_x'   ,fscalex      ,is_available, lsubmodel, unitab)
        if (fscalex == zero) then
          call hm_get_floatv_dim('MAT_Fscale_x'  ,fscalex_unit    ,is_available, lsubmodel, unitab)
          fscalex = fscalex_unit
        endif
        !< Card 3
        call hm_get_floatv('MAT_GIC'        ,gic          ,is_available, lsubmodel, unitab)
        call hm_get_floatv('MAT_GIIC'       ,giic         ,is_available, lsubmodel, unitab)
        call hm_get_floatv('MAT_EXP_G'      ,exp_g        ,is_available, lsubmodel, unitab)
        call hm_get_floatv('MAT_EXP_BK'     ,exp_bk       ,is_available, lsubmodel, unitab)
        call hm_get_floatv('MAT_GAMMA'      ,gama         ,is_available, lsubmodel, unitab)
        !< Card 4
        call hm_get_intv  ('MAT_CRIT_TYPE'  ,icrittyp     ,is_available, lsubmodel)
        call hm_get_intv  ('MAT_EVO_SHAPE'  ,ievoshap     ,is_available, lsubmodel)
        call hm_get_floatv('MAT_DELTA_FN'   ,udn          ,is_available, lsubmodel, unitab)
        call hm_get_floatv('MAT_DELTA_FS'   ,uds          ,is_available, lsubmodel, unitab)
        call hm_get_floatv('MAT_ALPHA'      ,alpha        ,is_available, lsubmodel, unitab)
!
        !-----------------------------------------------------------------------
        !< Check and set default values
        !-----------------------------------------------------------------------
        isym = min(max(0,isym),1)
        if (idel     == 0)    idel     = 1
        if (imass    == 0)    imass    = 1
        if (irupt    == 0)    irupt    = 1
        if (icrittyp == 0)    icrittyp = 1
        if (ievoshap == 0)    ievoshap = 1
        if (gama     == zero) gama     = one
        if (alpha    == zero) alpha    = three
        if (exp_g    == zero) exp_g    = two
        if (e_elas_s == zero) e_elas_s = e_elas_n
!
        !-----------------------------------------------------------------------
        ! Xu-Needleman exponential potential (IEVOSHAP = 3)
        ! Mandatory inputs: GIC > 0, GIIC > 0, TMAX_N > 0, TMAX_S > 0
        ! Derived: K0 = e^2 * Tn^2 / Gc,  delta_p = Gc / (e * Tn)
        ! User EN, ES, UDN, UDS, ALPHA are ignored
        !-----------------------------------------------------------------------
        if (ievoshap == 3) then
          if (gic <= zero) then
            call ancmsg(msgid = 3016,                                          &
                        msgtype = msgerror,                                    &
                        anmode = aninfo_blind_1,                               &
                        i1 = mat_id,                                           &
                        c1 = titr)
            gic = one
          endif  
          if (giic <= zero) then
            call ancmsg(msgid = 3017,                                          &
                        msgtype = msgerror,                                    &
                        anmode = aninfo_blind_1,                               &
                        i1 = mat_id,                                           &
                        c1 = titr)
            giic = one
          endif
          if (tmax_n <= zero) then
            call ancmsg(msgid = 3017,                                          &
                        msgtype = msgerror,                                    &
                        anmode = aninfo_blind_1,                               &
                        i1 = mat_id,                                           &
                        c1 = titr) 
            tmax_n = one
          endif
          if (tmax_s <= zero) then
            call ancmsg(msgid = 3017,                                          &
                        msgtype = msgerror,                                    &
                        anmode = aninfo_blind_1,                               &
                        i1 = mat_id,                                           &
                        c1 = titr) 
            tmax_s = one
          endif
          ! Derive Xu-Needlman params
          e_elas_n = exp(one)**2 * tmax_n**2 / gic
          e_elas_s = exp(one)**2 * tmax_s**2 / giic      
          delta0n  = gic  / (exp(one) * tmax_n)
          delta0s  = giic / (exp(one) * tmax_s)
          udn = -delta0n * log(em04)
          uds = -delta0s * log(em04)
          icrittyp = 1
        else ! ievoshap = 1 or 2
          exp_alpha  = exp(-alpha)
          denom      = one - exp_alpha
          func_alpha = (one - (one + alpha) * exp_alpha)/(alpha * denom)
          ! dft values for delta0
          delta0n  = tmax_n/e_elas_n !single mode damage initiation normal
          delta0s  = tmax_s/e_elas_s !single mode damage initiation tangential
          ! INIT CHECK FOR GIC and UDN
          if (gic == zero) then
            if (udn == zero) then
              gic = tmax_n**2 / two / e_elas_n
              udn = delta0n
              call ancmsg(msgid=3016,                                          &
                          msgtype=msgwarning,                                  &
                          anmode=aninfo_blind_1,                               &
                          i1 = mat_id,                                         &
                          c1 = titr)
            else
              ! LINEAR Criterion
              if (ievoshap == 1) then
                gic = half * udn * tmax_n
              ! EXPO
              else
                gic = tmax_n*delta0n*half + tmax_n*(udn - delta0n)*func_alpha
              endif
            endif
          else
            if ( gic < (tmax_n**2 / two/e_elas_n )  ) then
              gic = tmax_n**2 / two/ e_elas_n
              call ancmsg(msgid=3016,                                          &
                          msgtype=msgwarning,                                  &
                          anmode=aninfo_blind_1,                               &
                          i1 = mat_id,                                         &
                          c1 = titr )
            endif
            if (udn == zero) then
              if (ievoshap == 1) then
                udn = two*gic / (delta0n * e_elas_n)
              else
                coef0 = gic - tmax_n * delta0n * half
                udn = delta0n + coef0 / (func_alpha * tmax_n)
              endif
            endif
          endif
          ! init check for giic and uds
          if (giic == zero) then
            if (uds == zero) then
              giic = tmax_s**2 / two / e_elas_s
              uds  = delta0s
              call ancmsg(msgid=3017,                                          &
                          msgtype=msgwarning,                                  &
                          anmode=aninfo_blind_1,                               &
                          i1 = mat_id,                                         &
                          c1 = titr)
            else
              ! linear criterion
              if (ievoshap == 1) then
                giic = half * uds * tmax_s
              ! expo
              else
                giic = tmax_s*delta0s*half + tmax_s*(uds - delta0s)*func_alpha
              endif
            endif
          else
            if ( giic < (tmax_s**2 / two / e_elas_s )  ) then
              giic =  tmax_s**2 / two / e_elas_s
              call ancmsg(msgid=3017,                                          &
                          msgtype=msgwarning,                                  &
                          anmode=aninfo_blind_1,                               &
                          i1 = mat_id,                                         &
                          c1 = titr )
            endif
            if (uds == zero) then
              if (ievoshap == 1) then
                uds = two*giic / (delta0s * e_elas_s)
              else
                coef0 = giic - tmax_s * delta0s * half
                uds = delta0s + coef0 / (func_alpha * tmax_s)
              endif
            endif
          endif
        endif
!
        !-----------------------------------------------------------------------
        !< Number of User Element Variables and Curves
        !-----------------------------------------------------------------------
        !< Number of user defined curves
        matparam%ntable = 2
        !< Number of temporary variables
        nvartmp = 2
        !< Number of integer material parameters
        matparam%niparam = 6
        !< Number of real material parameters
        matparam%nuparam = 14
        !< Number of user variables
        nuvar = 11
!
        !< Allocate arrays
        call my_alloc(matparam%iparam, matparam%niparam, "matparam%iparam")
        call my_alloc(matparam%uparam, matparam%nuparam, "matparam%uparam")
        allocate(matparam%table(matparam%ntable))
!
        !-----------------------------------------------------------------------
        ! Integer material parameters
        !-----------------------------------------------------------------------
        matparam%iparam(1) = irupt         ! model (1 = power law, 2 = bk method) (irupt)
        matparam%iparam(2) = idel          ! idel (dft = 1 int)
        matparam%iparam(3) = icrittyp      ! 1=displacement-based, 2=energy-based
        matparam%iparam(4) = ievoshap      ! 1=linear, 2=exponential
        matparam%iparam(5) = isym          ! 0=asymmetric (tension only), 1=symmetric tension/compression     
        matparam%iparam(6) = imass         ! 1=density and area, 2=density and volume 
!
        !-----------------------------------------------------------------------
        ! Real material parameters
        !-----------------------------------------------------------------------
        matparam%uparam(1)  = e_elas_n    ! normal elastic stiffness (en)
        matparam%uparam(2)  = e_elas_s    ! tangential elastic stiffness (et)
        matparam%uparam(3)  = gama        ! mode coupling parameter (gamma)
        matparam%uparam(4)  = tmax_n         ! normal stress (tn)
        matparam%uparam(5)  = tmax_s         ! tangential shear stress (tt)
        matparam%uparam(6)  = gic         ! critical fracture energy - mode i (gic)
        matparam%uparam(7)  = giic        ! critical fracture energy - mode ii (giic)
        matparam%uparam(8)  = exp_g       !
        matparam%uparam(9)  = exp_bk      ! benzeggage-kenane exponent for the mixed mode (exp_bk)
        matparam%uparam(10) = delta0n     !         disp puremode n - initial normal displacement
        matparam%uparam(11) = delta0s     !         disp puremode s - initial tangential displacement
        matparam%uparam(12) = udn         ! ultimate displacement in normal direction (und)
        matparam%uparam(13) = uds         ! ultimate displacement in tangential direction (utd)
        matparam%uparam(14) = alpha       ! exponential law parameter (alpha)
!
        !< Material tables scaling factors and vectors
        x1scale = one
        x2scale = one
        x3scale = one
        x4scale = one
        x2vect(1:2) = fscalex
        x3vect(1:2) = zero
        x4vect(1:2) = zero
        fscale(1)   = tmax_n
        fscale(2)   = tmax_s
!
        !< Assign table IDs
        ! -> Classic clipping yield surface
        matparam%table(1)%notable = ifunc(1)
        matparam%table(2)%notable = ifunc(2)
        !< Copy tables into matparam structure
        call mat_table_copy(matparam   ,x2vect   ,x3vect   ,x4vect   ,           &
          x1scale  ,x2scale  ,x3scale  ,x4scale  ,fscale   ,ntable   ,           &
          table    ,ilaw     )
!      
        !< Initial and reference density
        matparam%rho0 = rho0
        matparam%rho  = rho0
!      
        !< PARMAT table
        parmat(1)  = max(e_elas_n,e_elas_s) / three
        parmat(2)  = max(e_elas_n,e_elas_s)
        parmat(17) = one
!
        !< MTAG variable activation
        mtag%l_epe  = 3
        mtag%l_dmg  = 1
        mtag%g_dmg  = 1
!
        !< Material model keywords
        call init_mat_keyword(matparam,"HOOK")
!
        !< Properties compatibility
        call init_mat_keyword(matparam,"SOLID_COHESIVE")
!
        !-----------------------------------------------------------------------
        !< Listing output
        !-----------------------------------------------------------------------
        write(iout,1100) trim(titr),mat_id,ilaw
        write(iout,1000)
        if (is_encrypted) then
          write(iout,'(5X,A,//)')'CONFIDENTIAL DATA'
        else
          write(iout,1200) rho0,e_elas_n,e_elas_s
          write(iout,1300) imass, idel, irupt, isym
          if (ifunc(1) > 0) then 
            write(iout,1400) ifunc(1),tmax_n, fscalex
          else
            write(iout,1401) tmax_n
          endif
          if (ifunc(2) > 0) then
            write(iout,1402) ifunc(2),tmax_s, fscalex
          else
            write(iout,1403) tmax_s
          endif
          write(iout, 1500) gic,giic,udn,uds
          if (irupt == 1) then 
            write(iout, 1501) exp_g
          else
            write(iout, 1502) exp_bk,gama
          endif
          write(iout, 1600) icrittyp,ievoshap
          if (ievoshap == 2) then
            write(iout, 1700) alpha
          endif
        endif
!
      !-------------------------------------------------------------------------
      !< Output formats
      !-------------------------------------------------------------------------
 1000 format(                                                                  &
     & 5X,"========================================================",/         &
     & 5X,"       MATERIAL MODEL: MIXED MODE COHESIVE LAW          ",/,        &
     & 5X,"========================================================",/)
 1100 format(                                                                  &
     & 5X,A,/,                                                                 &
     & 5X,"MATERIAL NUMBER . . . . . . . . . . . . . . . . . . . .=",I10/,     &
     & 5X,"MATERIAL LAW. . . . . . . . . . . . . . . . . . . . . .=",I10/)
 1200 format(                                                                  &
     & 5X,'INITIAL DENSITY (RHO) . . . . . . . . . . . . . . . . .=',1PG20.13/,&
     & 5X,'STIFFNESS NORMAL TO THE PLANE (EN). . . . . . . . . . .=',1PG20.13/,&
     & 5X,'STIFFNESS INTO THE PLANE (ES) . . . . . . . . . . . . .=',1PG20.13/)
 1300 format(                                                                  &
     & 5X,'MASS CALCULATION FLAG (IMASS) . . . . . . . . . . . . .=',I10/,     &
     & 5X,'     = 1: USING DENSITY AND AREA (DEFAULT)              ',/,        &
     & 5X,'     = 2: USING DENSITY AND VOLUME                      ',/,        &
     & 5X,'                                                        ',/,        &
     & 5X,'NUMBER OF INTG. POINTS TO FAIL (IDEL) . . . . . . . . .=',I10/,     &
     & 5X,'                                                        ',/,        &
     & 5X,'CHOICE OF PROPAGATION CRITERION (IRUPT) . . . . . . . .=',I10/,     &
     & 5X,'     = 1: POWER LAW (DEFAULT)                           ',/,        &
     & 5X,'     = 2: BENZEGGAGH-KENANE CRITERION                   ',/,        &
     & 5X,'                                                        ',/,        &
     & 5X,'SYMMETRIC TENSION/COMPRESSION FLAG (ISYM). . . . . . . =',I10/,     &
     & 5X,'     = 0: ASYMMETRIC (ONLY TENSION DAMAGES)             ',/,        &  
     & 5X,'     = 1: SYMMETRIC (TENSION = COMPRESSION)             ',/)
 1400 format(                                                                  &
     & 5X,'FCT. ID OF PEAK TRACTION IN NORMAL DIRECTION . . . . . =',I10/,     &
     & 5X,'PEAK TRACTION IN NORMAL DIRECTION . . . . . . . . . . .=',1PG20.13/,&
     & 5X,'ABSCISSA SCALE FACTOR FOR THE PEAK TRACTION FUNCTIONS .=',1PG20.13/)
 1401 format(                                                                  &
     & 5X,'PEAK TRACTION IN NORMAL DIRECTION . . . . . . . . . . .=',1PG20.13/)
 1402 format(                                                                  &
     & 5X,'FCT. ID OF PEAK TRACTION IN TANGENTIAL DIRECTION . . . =',I10/,     &
     & 5X,'PEAK TRACTION IN TANGENTIAL DIRECTION . . . . . . . . .=',1PG20.13/,&
     & 5X,'ABSCISSA SCALE FACTOR FOR THE PEAK TRACTION FUNCTIONS .=',1PG20.13/)
 1403 format(                                                                  &
     & 5X,'PEAK TRACTION IN TANGENTIAL DIRECTION . . . . . . . . .=',1PG20.13/)
 1500 format(                                                                  &
     & 5X,'ENERGY RELEASE RATE FOR MODE I. . . . . . . . . . . . .=',1PG20.13/,&
     & 5X,'ENERGY RELEASE RATE FOR MODE II . . . . . . . . . . . .=',1PG20.13/,&
     & 5X,'MAXIMUM DISPLACEMENT (FAILURE) FOR MODE I . . . . . . .=',1PG20.13/,&
     & 5X,'MAXIMUM DISPLACEMENT (FAILURE) FOR MODE II. . . . . . .=',1PG20.13/)
 1501 format(                                                                  &
     & 5X,'POWER LAW EXPONENT MU . . . . . . . . . . . . . . . . .=',1PG20.13/)
 1502 format(                                                                  &
     & 5X,'BENZEGGAGH-KENANE EXPONENT MU . . . . . . . . . . . . .=',1PG20.13/,&
     & 5X,'BENZEGGAGH-KENANE EXPONENT GAMMA. . . . . . . . . . . .=',1PG20.13/)
 1600 format(                                                                  &
     & 5X,'CHOICE OF THE FAILURE CRITERION . . . . . . . . . . . .=',I10/,     &
     & 5X,'             = 1 => DISP-BASED (COHDISP)                ',/,        &
     & 5X,'             = 2 => ENERGY-BASED (COHENRG)              ',/,        &
     & 5X,'                                                        ',/,        &
     & 5X,'CHOICE OF THE DAMAGE EVOLUTION. . . . . . . . . . . . .=',I10/,     &
     & 5X,'             = 1 => LINEAR (LINEAR-LINEAR)              ',/,        &
     & 5X,'             = 2 => EXPONENTIAL (LINEAR-EXPONENTIAL)    ',/,        &
     & 5X,'             = 3 => XU-NEEDLEMAN EXPONENTIAL (C-INFTY)  ',/)
 1700 format(                                                                  &
     & 5X,'EXPONENTIAL DAMAGE EXPONENT ALPHA . . . . . . . . . . .=',1PG20.13/)
!
      end subroutine hm_read_mat117
      end module hm_read_mat117_mod
