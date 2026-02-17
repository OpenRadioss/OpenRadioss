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
!||    hm_read_mat130_mod   ../starter/source/materials/mat/mat130/hm_read_mat130.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat          ../starter/source/materials/mat/hm_read_mat.F90
!||====================================================================
      module hm_read_mat130_mod
        implicit none
      contains
! ======================================================================================================================
! \brief Reading material parameters of /MAT/LAW130
! \details Reading material parameters of /MAT/LAW130 - Modified Honeycomb Material Model
! ======================================================================================================================
!||====================================================================
!||    hm_read_mat130           ../starter/source/materials/mat/mat130/hm_read_mat130.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F90
!||--- calls      -----------------------------------------------------
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    init_mat_keyword         ../starter/source/materials/mat/init_mat_keyword.F
!||    mat_table_copy           ../starter/source/materials/tools/mat_table_copy.F90
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    mat_table_copy_mod       ../starter/source/materials/tools/mat_table_copy.F90
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_mat130(                                             &
          matparam ,nuvar    ,mtag     ,iout     ,parmat   ,unitab   ,         &
          lsubmodel,israte   ,mat_id   ,titr     ,table    ,ntable   ,         &
          nvartmp  ,imatvis  ,iunit    )
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use unitab_mod
          use message_mod
          use submodel_mod
          use matparam_def_mod
          use elbuftag_mod
          use constant_mod
          use mat_table_copy_mod
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                 implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(matparam_struct_) ,intent(inout), target :: matparam  !< matparam data structure
          integer, intent(inout)                :: nuvar             !< number of material law user variables
          type(mlaw_tag_), intent(inout)        :: mtag              !< material tag for internal variables in element buffer
          integer, intent(in)                   :: iout              !< output file number
          real(kind=WP), intent(inout)          :: parmat(100)       !< material parameter global table 1
          type(unit_type_),intent(in)           :: unitab            !< units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel !< submodel data structure
          integer, intent(inout)                :: israte            !< strain rate filtering flag
          integer, intent(in)                   :: mat_id            !< material law user ID
          character(len=nchartitle),intent(in)  :: titr              !< material law user title
          integer, intent(in)                   :: ntable
          type(ttable)  ,dimension(ntable) ,intent(in) :: table
          integer,                intent(inout) :: nvartmp           !< temporary number of user variables
          integer,                intent(inout) :: imatvis           !< material viscosity flag
          integer,                intent(in)    :: iunit             !< material table unit number
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ipru,lca,lcb,lcc,lcs,lcab,lcbc,lcca,lcsr,itype,shdflg,    &
            lcsra,lcsrb,lcsrc,lcsrab,lcsrbc,lcsrca,ilaw,lcsrtmp,i
          real(kind=WP) ::                                                     &
            rho0,young,nu,sigy,vf,mu,eaau,ebbu,eccu,gabu,gbcu,gcau,shear,bulk, &
            x1scale,x2scale,x3scale,x4scale,x2vect(14),x3vect(14),x4vect(14),  &
            fscale(14),rfac,pruab,pruac,prubc,pruba,pruca,prucb,     &
            sigyd0,sigyp0,fcut,dtime_step,tsef,ssef,unit_pressure,unit_time
          logical :: is_encrypted, is_available
! ----------------------------------------------------------------------------------------------------------------------
!                                                      body
! ----------------------------------------------------------------------------------------------------------------------
          is_encrypted = .false.
          is_available = .false.
          ilaw = 130
          imatvis = 2
          sigyd0 = zero
          sigyp0 = zero
          pruab  = zero
          pruac  = zero
          prubc  = zero
          pruba  = zero
          pruca  = zero
          prucb  = zero
! ----------------------------------------------------------------------------------------------------------------------
          call hm_option_is_encrypted(is_encrypted)
! ----------------------------------------------------------------------------------------------------------------------
          !#Density
          call hm_get_floatv("Rho"           ,rho0   ,is_available,lsubmodel, unitab)
! ----------------------------------------------------------------------------------------------------------------------
          !#Elastic paramaters
          call hm_get_floatv("E"             ,young  ,is_available,lsubmodel, unitab)
          call hm_get_floatv("Nu"            ,nu     ,is_available,lsubmodel, unitab)
          call hm_get_floatv("LSDYNA_SIGY"   ,sigy   ,is_available,lsubmodel, unitab)
          call hm_get_floatv("LSDYNA_VF"     ,vf     ,is_available,lsubmodel, unitab)
          call hm_get_floatv("LSDYNA_MU"     ,mu     ,is_available,lsubmodel, unitab)
! ----------------------------------------------------------------------------------------------------------------------
!         #Orthotropic behavior parameters
          call hm_get_intv("LCSR_TEMP"       ,lcsrtmp,is_available,lsubmodel)
          call hm_get_intv("IFORM"           ,itype  ,is_available,lsubmodel)
          call hm_get_intv("MATL126_SHDFLG"  ,shdflg ,is_available,lsubmodel)
          call hm_get_intv("LSD_LCID"        ,lca    ,is_available,lsubmodel)
          call hm_get_intv("LSD_LCID2"       ,lcb    ,is_available,lsubmodel)
          call hm_get_intv("LSD_LCID3"       ,lcc    ,is_available,lsubmodel)
          call hm_get_intv("LSD_LCID4"       ,lcs    ,is_available,lsubmodel)
          call hm_get_intv("LSD_LCID5"       ,lcab   ,is_available,lsubmodel)
          call hm_get_intv("LSD_LCID6"       ,lcbc   ,is_available,lsubmodel)
          call hm_get_intv("LSD_LCID7"       ,lcca   ,is_available,lsubmodel)
          !< Shear stress curves
          if ((lcs > 0).and.(lcab == 0).and.(lcbc == 0).and.(lcca == 0)) then
            lcab = lcs
            lcbc = lcs
            lcca = lcs
          endif
          if (lcsrtmp >= 0) then
            call hm_get_intv("LSD_LCID8"     ,lcsr   ,is_available,lsubmodel)
          endif
! ----------------------------------------------------------------------------------------------------------------------
!         #Elastic parameters for the uncompressed configuration
          call hm_get_floatv("LSDYNA_EAAU"   ,eaau   ,is_available, lsubmodel, unitab)
          call hm_get_floatv("LSDYNA_EBBU"   ,ebbu   ,is_available, lsubmodel, unitab)
          call hm_get_floatv("LSDYNA_ECCU"   ,eccu   ,is_available, lsubmodel, unitab)
          call hm_get_floatv("LSDYNA_GABU"   ,gabu   ,is_available, lsubmodel, unitab)
          call hm_get_floatv("LSDYNA_GBCU"   ,gbcu   ,is_available, lsubmodel, unitab)
          call hm_get_floatv("LSDYNA_GCAU"   ,gcau   ,is_available, lsubmodel, unitab)
          call hm_get_floatv("LSDYNA_RFAC"   ,rfac   ,is_available, lsubmodel, unitab)
          call hm_get_floatv("LSDYNA_SIGF"   ,tsef   ,is_available, lsubmodel, unitab)
          call hm_get_floatv("LSDYNA_EPSF"   ,ssef   ,is_available, lsubmodel, unitab)
          call hm_get_intv("LSDYNA_PRU"      ,ipru   ,is_available, lsubmodel)
! ----------------------------------------------------------------------------------------------------------------------
          if (lcsrtmp < 0) then
            call hm_get_intv("MATL126_LCSRA" ,lcsra  ,is_available,lsubmodel)
            call hm_get_intv("MATL126_LCSRB" ,lcsrb  ,is_available,lsubmodel)
            call hm_get_intv("MATL126_LCSRC" ,lcsrc  ,is_available,lsubmodel)
            call hm_get_intv("MATL126_LCSRAB",lcsrab ,is_available,lsubmodel)
            call hm_get_intv("MATL126_LCSRBC",lcsrbc ,is_available,lsubmodel)
            call hm_get_intv("MATL126_LCSRCA",lcsrca ,is_available,lsubmodel)
          endif
! ----------------------------------------------------------------------------------------------------------------------
          if (ipru == 2) then
            call hm_get_floatv("LSDYNA_PRUAB",pruab  ,is_available, lsubmodel, unitab)
            call hm_get_floatv("LSDYNA_PRUAC",pruac  ,is_available, lsubmodel, unitab)
            call hm_get_floatv("LSDYNA_PRUBC",prubc  ,is_available, lsubmodel, unitab)
            call hm_get_floatv("LSDYNA_PRUBA",pruba  ,is_available, lsubmodel, unitab)
            call hm_get_floatv("LSDYNA_PRUCA",pruca  ,is_available, lsubmodel, unitab)
            call hm_get_floatv("LSDYNA_PRUCB",prucb  ,is_available, lsubmodel, unitab)
          endif
! ----------------------------------------------------------------------------------------------------------------------
          !< Elastic constants
          bulk  = young/(three * (one - two * nu))
          shear = young/(two * (one + nu))
          !< Yield stress
          if (sigy == zero) sigy = infinity
          !< Check formulation for the yield surface
          if (itype == 0) itype = 1
          if ((eccu < zero).or.(itype == 3)) then
            itype = 3
            sigyd0 = abs(eccu)
            sigyp0 = abs(gcau)
          endif
          if ((itype == 2).or.(itype == 3)) then
            eccu = ebbu
            gcau = gabu
          endif
          if (itype == 3) lcc = lcb
          !< Strain rate filtering equivalent frequency
          israte = 1
          rfac = max(min(rfac,one),zero)
          dtime_step = (ten*em3/unitab%fac_l_work)*sqrt(rho0/(bulk + four_over_3*shear))
          fcut = (one - rfac)/(two*pi*dtime_step)
          fcut = max(fcut,em20)
          if (tsef == zero) tsef = ep20
          if (ssef == zero) ssef = ep20
          !< Recover pressure unit for loading curves unit conversion
          call hm_get_floatv_dim('E' ,unit_pressure   ,is_available, lsubmodel, unitab)
          unit_time = unitab%fac_t(iunit)
!
! ----------------------------------------------------------------------------------------------------------------------
!     Filling buffer tables
! ----------------------------------------------------------------------------------------------------------------------
          !< Number of integer material parameters
          matparam%niparam = 4
          !< Number of real material parameters
          matparam%nuparam = 19
          !< Number of material array parameters
          matparam%ntable = 6
          if (lcsrtmp > 0) then
            matparam%ntable = matparam%ntable + 1
          elseif ((lcsrtmp < 0).and.(itype == 1)) then
            matparam%ntable = matparam%ntable + 6
          endif
!
          !< Number of user variables
          nuvar = 11
          !< Number of temporary variables
          if (itype == 1) then
            nvartmp = 18
          else
            nvartmp = 9
          endif
!
          !< Allocation of material parameters tables
          allocate (matparam%iparam(matparam%niparam))
          allocate (matparam%uparam(matparam%nuparam))
          allocate (matparam%table (matparam%ntable ))
!
          !< Integer material parameter
          matparam%iparam(1)  = itype
          matparam%iparam(2)  = lcsrtmp
          matparam%iparam(3)  = shdflg
          matparam%iparam(4)  = ipru
!
          !< Real material parameters
          matparam%young      = young
          matparam%nu         = nu
          matparam%shear      = shear
          matparam%bulk       = bulk
          matparam%uparam(1)  = sigy
          matparam%uparam(2)  = vf
          matparam%uparam(3)  = mu
          matparam%uparam(4)  = eaau
          matparam%uparam(5)  = ebbu
          matparam%uparam(6)  = eccu
          matparam%uparam(7)  = gabu
          matparam%uparam(8)  = gbcu
          matparam%uparam(9)  = gcau
          matparam%uparam(10) = sigyd0
          matparam%uparam(11) = sigyp0
          matparam%uparam(12) = pruab
          matparam%uparam(13) = pruac
          matparam%uparam(14) = prubc
          matparam%uparam(15) = pruba
          matparam%uparam(16) = pruca
          matparam%uparam(17) = prucb
          matparam%uparam(18) = tsef
          matparam%uparam(19) = ssef
!
          !< Material tables scaling factors and vectors
          x1scale = one
          x2scale = one
          x3scale = one
          x4scale = one
          x2vect(1:14) = one
          x3vect(1:14) = zero
          x4vect(1:14) = zero
          fscale(1:3)  = unit_pressure
          if (itype == 1) then 
            fscale(4:6) = unit_pressure
          else
            fscale(4:6) = one
          endif
          fscale(7:14) = one
!
          !< Assign table IDs
          ! -> Classic clipping yield surface
          matparam%table(1)%notable = lca
          matparam%table(2)%notable = lcb
          matparam%table(3)%notable = lcc
          matparam%table(4)%notable = lcab
          matparam%table(5)%notable = lcbc
          matparam%table(6)%notable = lcca
          if (lcsrtmp > 0) then
            matparam%table(7)%notable  = lcsr
          elseif ((lcsrtmp < 0).and.(itype == 1)) then
            matparam%table(7)%notable  = lcsra
            matparam%table(8)%notable  = lcsrb
            matparam%table(9)%notable  = lcsrc
            matparam%table(10)%notable = lcsrab
            matparam%table(11)%notable = lcsrbc
            matparam%table(12)%notable = lcsrca
          endif
          !< Copy tables into matparam structure
          call mat_table_copy(matparam   ,x2vect   ,x3vect   ,x4vect   ,       &
            x1scale  ,x2scale  ,x3scale  ,x4scale  ,fscale   ,ntable   ,       &
            table    ,ilaw     )
!
          !< Include unit conversion for strain rate
          if (lcsrtmp > 0) then
            do i = 1, size(matparam%table(7)%x(1)%values) 
              matparam%table(7)%x(1)%values(i) = matparam%table(7)%x(1)%values(i)/unit_time
            enddo
          endif
!
          !< PARMAT table
          parmat(1) = bulk
          parmat(2) = young
          parmat(3) = nu
          parmat(4) = israte
          parmat(5) = fcut
!
          !< Initial and reference density
          matparam%rho0 = rho0
          matparam%rho  = rho0
!
          !< MTAG variable activation
          mtag%g_pla  = 1
          mtag%l_pla  = 1
          mtag%g_epsd = 1
          mtag%l_epsd = 1
!
          ! Tag for damage output
          if (itype > 1) then
            ! -> Number of output modes (stored in DMG(NEL,I), I>1)
            matparam%nmod = 3
            ! Total number of damage outputs
            ! -> DMG(NEL,1) = Global damage output
            ! -> DMG(NEL,2:NMOD+1) = Damage modes output
            mtag%g_dmg = 1 + matparam%nmod
            mtag%l_dmg = 1 + matparam%nmod
            ! -> Modes allocation and definition
            allocate(matparam%mode(matparam%nmod))
            matparam%mode(1) = "Damage in shear plane ab"
            matparam%mode(2) = "Damage in shear plane bc"
            matparam%mode(3) = "Damage in shear plane ca"
          endif
!
          !< Properties compatibility
          call init_mat_keyword(matparam,"SOLID_ORTHOTROPIC")
!
          !< Properties compatibility
          call init_mat_keyword(matparam,"COMPRESSIBLE")
          call init_mat_keyword(matparam,"INCREMENTAL" )
          call init_mat_keyword(matparam,"LARGE_STRAIN")
          call init_mat_keyword(matparam,"ORTHOTROPIC" )
!
! ----------------------------------------------------------------------------------------------------------------------
!     Parameters printout
! ----------------------------------------------------------------------------------------------------------------------
          write(iout, 900) trim(titr),mat_id,130
          write(iout,1000)
          if (is_encrypted) then
            write(iout,"(5X,A,//)") "CONFIDENTIAL DATA"
          else
            write(iout,1100) rho0
            write(iout,1200) eaau,ebbu,eccu,gabu,gbcu,gcau
            write(iout,1250) ipru
            if (ipru == 2) then
              write(iout,1260) pruab,pruac,prubc,pruba,pruca,prucb
            endif
            write(iout,1300) itype
            if (itype == 1) then
              write(iout,1400) lca,lcb,lcc,lcab,lcbc,lcca,lcs
            elseif (itype == 2) then
              write(iout,1500) lca,lcb,lcc,lcab,lcbc,lcca,shdflg
            elseif (itype == 3) then
              write(iout,1600) lca,lcb,sigyd0,sigyp0,lcab,lcbc,lcca,shdflg
            endif
            if (lcsrtmp > 0) then
              write(iout,1410) lcsr,rfac
            elseif ((itype == 1).and.(lcsrtmp < 0)) then
              write(iout,1420) lcsra,lcsrb,lcsrc,lcsrab,lcsrbc,lcsrca,rfac
            endif
            write(iout,1700) young,nu,sigy,vf,mu
            write(iout,1800) tsef,ssef
            write(iout,2000)
          end if
! ----------------------------------------------------------------------------------------------------------------------
900       format(/                                                                   &
            5X,A,/,                                                             &
            5X,"MATERIAL NUMBER . . . . . . . . . . . . . . . . . .=",I10/,     &
            5X,"MATERIAL LAW. . . . . . . . . . . . . . . . . . . .=",I10/)
1000      format(/                                                                   &
            5X,"----------------------------------------------------",/         &
            5X,"        MATERIAL MODEL:  MODIFIED HONEYCOMB         ",/,        &
            5X,"----------------------------------------------------",/)
1100      format(/                                                                   &
            5X,"INITIAL DENSITY . . . . . . . . . . . . . . . . . .=",1PG20.13/)
1200      format(/                                                                   &
            5X,"UNCOMPACTED MATERIAL PROPERTIES:                    ",/,        &
            5X,"----------------------------------------------------",/,        &
            5X,"YOUNG MODULUS IN DIRECTION A (EAAU) . . . . . . . .=",1PG20.13/,&
            5X,"YOUNG MODULUS IN DIRECTION B (EBBU) . . . . . . . .=",1PG20.13/,&
            5X,"YOUNG MODULUS IN DIRECTION C (ECCU) . . . . . . . .=",1PG20.13/,&
            5X,"SHEAR MODULUS IN PLANE AB (GABU). . . . . . . . . .=",1PG20.13/,&
            5X,"SHEAR MODULUS IN PLANE BC (GBCU). . . . . . . . . .=",1PG20.13/,&
            5X,"SHEAR MODULUS IN PLANE CA (GCAU). . . . . . . . . .=",1PG20.13/)
1250      format(/                                                                   &
            5X,"POISSON'S EFFECT FLAG (IPRU). . . . . . . . . . . .=",I10/,     &
            5X,"    IPRU = 0: NO POISSON'S EFFECTS                  ",/,        &
            5X,"    IPRU = 1: RAMP ON POISSON'S RATIO               ",/,        &
            5X,"    IPRU = 2: INPUT ANISOTROPIC POISSON'S RATIO     ",/)
1260      format(/                                                                   &
            5X,"POISSON'S RATIO IN PLANE AB (PRUAB) . . . . . . . .=",1PG20.13/ &
            5X,"POISSON'S RATIO IN PLANE AC (PRUAC) . . . . . . . .=",1PG20.13/ &
            5X,"POISSON'S RATIO IN PLANE BC (PRUBC) . . . . . . . .=",1PG20.13/ &
            5X,"POISSON'S RATIO IN PLANE BA (PRUBA) . . . . . . . .=",1PG20.13/ &
            5X,"POISSON'S RATIO IN PLANE CA (PRUCA) . . . . . . . .=",1PG20.13/ &
            5X,"POISSON'S RATIO IN PLANE CB (PRUCB) . . . . . . . .=",1PG20.13/)
1300      format(/                                                                   &
            5X,"YIELD SURFACE TYPE FLAG (IFORM) . . . . . . . . . .=",I10/,     &
            5X,"    IFORM = 1: CLASSIC CLIPPING YIELD SURFACE       ",/,        &
            5X,"    IFORM = 2: TRANSVERSE ISOTROPIC YIELD SURFACE   ",/,        &
            5X,"    IFORM = 3: HYDROSTATIC/SHEAR YIELD SURFACE      ",/)
1400      format(/                                                                   &
            5X,"CLASSIC YIELD SURFACE PARAMETERS:                   ",/,        &
            5X,"----------------------------------------------------",/,        &
            5X,"LOAD EVOLUTION IN DIRECTION A FUNCTION ID (LCA) . .=",I10/,     &
            5X,"LOAD EVOLUTION IN DIRECTION B FUNCTION ID (LCB) . .=",I10/,     &
            5X,"LOAD EVOLUTION IN DIRECTION C FUNCTION ID (LCC) . .=",I10/,     &
            5X,"LOAD EVOLUTION IN PLANE AB FUNCTION ID (LCAB) . . .=",I10/,     &
            5X,"LOAD EVOLUTION IN PLANE BC FUNCTION ID (LCBC) . . .=",I10/,     &
            5X,"LOAD EVOLUTION IN PLANE CA FUNCTION ID (LCCA) . . .=",I10/,     &
            5X,"DAMAGE EVOLUTION IN SHEARING FUNCTION ID (LCS). . .=",I10/)
1410      format(/                                                                   &
            5X,"STRAIN RATE DEPENDENCY FUNCTION ID (LCSR) . . . . .=",I10/,     &
            5X,"STRAIN RATE FILTERING FACTOR (RFAC) . . . . . . . .=",1PG20.13/)
1420      format(/                                                                   &
            5X,"S-R DEPENDENCY IN DIRECTION A FUNCTION ID (LCSRA) .=",I10/,     &
            5X,"S-R DEPENDENCY IN DIRECTION B FUNCTION ID (LCSRB) .=",I10/,     &
            5X,"S-R DEPENDENCY IN DIRECTION C FUNCTION ID (LCSRC) .=",I10/,     &
            5X,"S-R DEPENDENCY IN PLANE AB FUNCTION ID (LCSRAB) . .=",I10/,     &
            5X,"S-R DEPENDENCY IN PLANE BC FUNCTION ID (LCSRBC) . .=",I10/,     &
            5X,"S-R DEPENDENCY IN PLANE CA FUNCTION ID (LCSRCA) . .=",I10/,     &
            5X,"STRAIN RATE FILTERING FACTOR (RFAC) . . . . . . . .=",1PG20.13/)
1500      format(/                                                                   &
            5X,"TRANSVERSE ISOTROPIC YIELD SURFACE PARAMETERS:      ",/,        &
            5X,"----------------------------------------------------",/,        &
            5X,"OFF-ANGLE LIMIT STRESS FUNCTION ID (LCA)  . . . . .=",I10/,     &
            5X,"STRONG AXIS LIMIT STRESS FUNCTION ID (LCB). . . . .=",I10/,     &
            5X,"WEAK AXIS LIMIT STRESS FUNCTION ID (LCC)  . . . . .=",I10/,     &
            5X,"SHEAR DAMAGE IN PLANE AB FUNCTION ID (LCAB) . . . .=",I10/,     &
            5X,"SHEAR DAMAGE IN PLANE BC FUNCTION ID (LCBC) . . . .=",I10/,     &
            5X,"SHEAR DAMAGE IN PLANE CA FUNCTION ID (LCCA) . . . .=",I10/,     &
            5X,"SHEAR DAMAGE FLAG (SHDFLG). . . . . . . . . . . . .=",I10/,     &
            5X,"   SHDFLG = 0: DAMAGE REDUCES STRESS EVERY TIME STEP",/,        &
            5X,"   SHDFLG = 1: DAMAGE = SHEAR STRESS/UNDAMAGED SHEAR STRESS",/)
1600      format(/                                                                   &
            5X,"HYDROSTATIC/SHEAR YIELD SURFACE PARAMETERS:         ",/,        &
            5X,"----------------------------------------------------",/,        &
            5X,"UNIAXIAL LIMIT STRESS FUNCTION ID (LCA) . . . . . .=",I10/     ,&
            5X,"STRONG AXIS LIMIT STRESS FUNCTION ID (LCB). . . . .=",I10/     ,&
            5X,"DEVIATORIC CONSTANT STRESS LIMIT (SIGYD0) . . . . .=",1PG20.13/,&
            5X,"HYDROSTATIC CONSTANT STRESS LIMIT (SIGYP0). . . . .=",1PG20.13/,&
            5X,"SHEAR DAMAGE IN PLANE AB FUNCTION ID (LCAB) . . . .=",I10/,     &
            5X,"SHEAR DAMAGE IN PLANE BC FUNCTION ID (LCBC) . . . .=",I10/,     &
            5X,"SHEAR DAMAGE IN PLANE CA FUNCTION ID (LCCA) . . . .=",I10/,     &
            5X,"SHEAR DAMAGE FLAG (SHDFLG). . . . . . . . . . . . .=",I10/,     &
            5X,"   SHDFLG = 0: DAMAGE REDUCES STRESS EVERY TIME STEP",/,        &
            5X,"   SHDFLG = 1: DAMAGE = SHEAR STRESS/UNDAMAGED SHEAR STRESS",/)
1700      format(/                                                                   &
            5X,"FULLY COMPACTED MATERIAL PROPERTIES:                ",/,        &
            5X,"----------------------------------------------------",/,        &
            5X,"YOUNG MODULUS E . . . . . . . . . . . . . . . . . .=",1PG20.13/,&
            5X,"POISSON'S RATIO . . . . . . . . . . . . . . . . . .=",1PG20.13/,&
            5X,"FULLY COMPACTED YIELD STRESS (SIGY) . . . . . . . .=",1PG20.13/,&
            5X,"VOLUME FRACTION AT FULL COMPACTION (VF) . . . . . .=",1PG20.13/,&
            5X,"VISCOSITY COEFFICIENT (MU). . . . . . . . . . . . .=",1PG20.13/)
1800      format(/                                                                   &
            5X,"ELEMENT DELETION PROPERTIES:                        ",/,        &
            5X,"----------------------------------------------------",/,        &
            5X,"TENSILE STRAIN AT FAILURE (TSEF). . . . . . . . . .=",1PG20.13/,&
            5X,"SHEAR STRAIN AT FAILURE (SSEF). . . . . . . . . . .=",1PG20.13/)
2000      format(/                                                                   &
            5X,"----------------------------------------------------",/)
!
        end subroutine hm_read_mat130
! ----------------------------------------------------------------------------------------------------------------------
      end module hm_read_mat130_mod

