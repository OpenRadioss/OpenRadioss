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
!||    hm_read_mat106_mod   ../starter/source/materials/mat/mat106/hm_read_mat106.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat          ../starter/source/materials/mat/hm_read_mat.F90
!||====================================================================
      module hm_read_mat106_mod
      contains
!||====================================================================
!||    hm_read_mat106           ../starter/source/materials/mat/mat106/hm_read_mat106.F90
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
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    mat_table_copy_mod       ../starter/source/materials/tools/mat_table_copy.F90
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||    table_mod                ../starter/share/modules1/table_mod.F
!||====================================================================
        subroutine hm_read_mat106(                                               &
          matparam ,nuvar    ,nfunc    ,parmat   ,unitab   ,mat_id   ,titr     , &
          mtag     ,nvartmp  ,lsubmodel,ntable   ,table    ,iout     ,israte   )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
          use unitab_mod
          use submodel_mod
          use matparam_def_mod
          use elbuftag_mod
          use constant_mod
          use mat_table_copy_mod
          use hm_option_read_mod
          use table_mod
          use message_mod
          use precision_mod, only: WP
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
          implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          type(matparam_struct_) ,intent(inout) :: matparam  !< Material parameters structure
          integer,                intent(inout) :: nuvar     !< Number of user variables
          integer,                intent(inout) :: nfunc     !< Number of functions
          real(kind=WP), dimension(100),intent(inout) :: parmat !< Material parameter local array
          type (unit_type_),      intent(in)    :: unitab    !< Units table
          integer,                intent(in)    :: mat_id    !< Material identification number
          character(len=nchartitle),intent(in)  :: titr      !< Material title
          type(mlaw_tag_),        intent(inout) :: mtag      !< Material tags structure
          integer,                intent(inout) :: nvartmp   !< Number of temporary variables
          type(submodel_data), dimension(nsubmod), intent(in) :: lsubmodel !< Submodel data structure
          integer, intent(in)                   :: ntable    !< Number of tables
          type(ttable),dimension(ntable),intent(in) :: table !< Tables data structure
          integer, intent(in)                   :: iout      !< Output file number
          integer, intent(inout)                :: israte    !< Strain rate filtering flag
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
          real(kind=WP) :: e, nu, ca, cb, cm, cn, epsm, sigm, tref, tmelt,fcut,   &
            rho0, rhor, cs, bulk, shear, t0
          real(kind=WP) :: x1scale,x2scale,x3scale,x4scale,x2vect(3),x3vect(3),   &
            x4vect(3),fscale(3),tol,deps0,cjc,eta
          integer :: ilaw,ifunc1,ifunc2,ifunc3,nmax,vp
          logical :: is_available,is_encrypted
!-----------------------------------------------
!   S o u r c e   L i n e s
!-----------------------------------------------
          is_encrypted = .false.
          is_available = .false.
          ilaw= 106
          !----------------------------------------------------------------------------------
          call hm_option_is_encrypted(is_encrypted)
          !----------------------------------------------------------------------------------
          !< Density
          call hm_get_floatv('MAT_RHO'          ,rho0   ,is_available, lsubmodel, unitab)
          call hm_get_floatv('REFER_RHO'        ,rhor   ,is_available, lsubmodel, unitab)
          !----------------------------------------------------------------------------------
          !< 1st line of material card (elastic parameters and temperature dependency)
          call hm_get_floatv('MAT_E'            ,e      ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_NU'           ,nu     ,is_available, lsubmodel, unitab)
          call hm_get_intv  ('MLAW106_FCT_ID1'  ,ifunc1 ,is_available, lsubmodel)
          call hm_get_intv  ('MLAW106_FCT_ID2'  ,ifunc2 ,is_available, lsubmodel)
          call hm_get_intv  ('MLAW106_FCT_ID3'  ,ifunc3 ,is_available, lsubmodel)
          !----------------------------------------------------------------------------------
          !< 2nd line of material card (hardening and failure parameters)
          call hm_get_floatv('MAT_SIGY'         ,ca     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_BETA'         ,cb     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_HARD'         ,cn     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MLAW106_EP_MAX '  ,epsm   ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MLAW106_SIGMA_MAX',sigm   ,is_available, lsubmodel, unitab)
          !----------------------------------------------------------------------------------
          !< 3rd line of material card (numerical parameters and strain rate dependency)
          call hm_get_floatv('MAT_FCUT'         ,fcut   ,is_available, lsubmodel, unitab)
          call hm_get_intv  ('MLAW106_VP'       ,vp     ,is_available, lsubmodel)
          call hm_get_intv  ('MLAW106_NMAX'     ,nmax   ,is_available, lsubmodel)
          call hm_get_floatv('MLAW106_TOL'      ,tol    ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MLAW106_CJC'      ,cjc    ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MLAW106_DEPS0'    ,deps0  ,is_available, lsubmodel, unitab)
          !----------------------------------------------------------------------------------
          !< 4th line of material card (plastic temperature dependency)
          call hm_get_floatv('MAT_M'            ,cm     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_TMELT'        ,tmelt  ,is_available, lsubmodel, unitab)
          !----------------------------------------------------------------------------------
          !< 5th line of material card (specific heat and reference temperature)
          call hm_get_floatv('MAT_SPHEAT'       ,cs     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MLAW106_ETA'      ,eta    ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MLAW106_T0'       ,t0     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MLAW106_TR'       ,tref   ,is_available, lsubmodel, unitab)
!
          !----------------------------------------------------------------------------------
          !< Parameters default values
          !----------------------------------------------------------------------------------
          !< Maximum plastic strain
          if (epsm  == zero) epsm  = infinity
          !< Maximum stress
          if (sigm  == zero) sigm  = infinity
          !< Temperature dependency exponent
          if (cm    == zero) cm    = one
          !< Melting temperature
          if (tmelt <= zero) tmelt = infinity
          !< Reference temperature
          if (tref  <= zero) tref  = three100
          !< Initial temperature
          if (t0    <= zero) t0    = tref
          !< Reference density
          if (rhor  == zero) rhor  = rho0
          !< Viscous formulation
          vp = min(max(vp,0),3)
          if (vp == 0) vp = 2
          !< Number of return mapping iterations
          if (nmax == 0) then
            if (vp >  1) nmax = 3
            if (vp == 1) nmax = 6
          endif
          !< Tolerance on return mapping iterations
          if (tol  == 0.0_wp) tol = em20
          !< Strain rate dependency coefficient
          if (deps0 <= zero) then
            call hm_get_floatv_dim("MLAW106_DEPS0",deps0,is_available,lsubmodel,unitab)
          endif
          !< Strain rate filtering
          if (vp > 1) israte = 1
          !< Strain rate cut-off frequency
          if (fcut == zero) fcut = 10000.0d0*unitab%fac_t_work
          if (vp == 1) fcut = zero
          !< Taylor-Quinney coefficient
          eta = min(max(eta,zero),one)
          if (eta == zero) eta = one
!
          !----------------------------------------------------------------------------------
          !< Checks on parameters
          !----------------------------------------------------------------------------------
          if (e <= zero) then
            call ancmsg(msgid=276,msgtype=msgerror,anmode=aninfo,i1=106,i2=mat_id,c1=titr)
          endif
          if (nu <= -one)  then
            call ancmsg(msgid=300,msgtype=msgerror,anmode=aninfo,i1=106,i2=mat_id,c1=titr)
          endif
!
          !----------------------------------------------------------------------------------
          !< Elastic constants
          !----------------------------------------------------------------------------------
          !< Shear modulus
          shear = e/(two*(one+nu))
          bulk  = e/(three*(one - two*nu))
!
          !----------------------------------------------------------------------------------
          !< Filling buffer tables
          !----------------------------------------------------------------------------------
          !< Number of integer material parameters
          matparam%niparam = 2
          !< Number of real material parameters
          matparam%nuparam = 14
          !< Number of user variables
          nuvar = 6
          !< Number of functions
          nfunc = 0
          !< Number of tables and temporary variables
          matparam%ntable = 3
          nvartmp = 3
!
          !< Allocation of material parameters tables
          allocate(matparam%iparam(matparam%niparam))
          allocate(matparam%uparam(matparam%nuparam))
          allocate(matparam%table (matparam%ntable ))
!
          !< Integer material parameters
          matparam%iparam(1)  = nmax
          matparam%iparam(2)  = vp
!
          !< Real material parameters
          matparam%young      = e
          matparam%nu         = nu
          matparam%shear      = shear
          matparam%bulk       = bulk
          matparam%uparam(1)  = ca
          matparam%uparam(2)  = cb
          matparam%uparam(3)  = cn
          matparam%uparam(4)  = sigm
          matparam%uparam(5)  = cm
          matparam%uparam(6)  = eta
          matparam%uparam(7)  = cjc
          matparam%uparam(8)  = deps0
          matparam%uparam(9)  = epsm
          matparam%uparam(10) = tol
!
          !< Thermal properties
          matparam%therm%rhocp = cs
          matparam%therm%tref  = tref
          matparam%therm%tini  = t0
          matparam%therm%tmelt = tmelt
!
          !< Activate heat source computation in material
          matparam%heat_flag = 1
!
          !< Transform global table into material table
          ! -> Temperature dependency of Young's modulus (heating)
          matparam%table(1)%notable = ifunc1
          ! -> Temperature dependency of Young's modulus (cooling)
          matparam%table(2)%notable = ifunc2
          ! -> Temperature dependency of Poisson's ratio
          matparam%table(3)%notable = ifunc3
          ! -> Scale factors and vectors
          x1scale     = one
          x2scale     = one
          x3scale     = one
          x4scale     = one
          x2vect(1:3) = zero
          x3vect(1:3) = zero
          x4vect(1:3) = zero
          fscale(1:2) = e
          fscale(3)   = nu
          ! -> Copy tables
          call mat_table_copy(                                                   &
            matparam ,x2vect   ,x3vect   ,x4vect   ,x1scale  ,x2scale  ,         &
            x3scale  ,x4scale  ,fscale   ,ntable   ,table    ,ilaw     )
!
          !< PARMAT table
          parmat(1)  = bulk
          parmat(2)  = e
          parmat(3)  = nu
          parmat(4)  = israte
          parmat(5)  = fcut
          parmat(16) = 2
          parmat(17) = (one - two*nu)/(one - nu)
!
          !< Initial and reference density
          matparam%rho0 = rho0
          matparam%rho  = rho0
!
          !< MTAG variable activation
          mtag%g_epsd = 1
          mtag%l_epsd = 1
          mtag%g_pla  = 1
          mtag%l_pla  = 1
          mtag%g_temp = 1
          mtag%l_temp = 1
          mtag%g_seq  = 1
          mtag%l_seq  = 1
!
          !< Properties compatibility
          call init_mat_keyword(matparam,"SOLID_ISOTROPIC")
          call init_mat_keyword(matparam,"SHELL_ISOTROPIC")
          call init_mat_keyword(matparam,"SPH"            )
!
          !< Material model keywords
          call init_mat_keyword(matparam ,"ELASTO_PLASTIC")
          call init_mat_keyword(matparam ,"INCREMENTAL"   )
          call init_mat_keyword(matparam ,"LARGE_STRAIN"  )
          call init_mat_keyword(matparam ,"HOOK"          )
          call init_mat_keyword(matparam ,"ISOTROPIC"     )
!
          !----------------------------------------------------------------------------------
          !< Listing output
          !----------------------------------------------------------------------------------
          write(iout,1001) trim(titr),mat_id,106
          write(iout,1000)
          if (is_encrypted) then
            write(iout,'(5X,A,//)') 'CONFIDENTIAL DATA'
          else
            write(iout,1002) rho0
            write(iout,1003) e,nu
            write(iout,1004) ifunc1,ifunc2,ifunc3,tref,tmelt,t0,cm,cs,eta
            write(iout,1005) ca,cb,cn,sigm
            if (cjc > zero) then
              if (vp > 1) then
                write(iout,1006) cjc,deps0,fcut,vp
              else
                write(iout,1007) cjc,deps0,vp
              endif
            endif
            write(iout,1008) epsm
            write(iout,1009) nmax,tol
          endif
!
          !----------------------------------------------------------------------------------
          !< Output formats
          !----------------------------------------------------------------------------------
1000      format(/                                                                 &
            5X,"-------------------------------------------------------",/           &
            5X,"  MATERIAL MODEL: JOHNSON-COOK TEMPERATURE DEPENDENT   ",/,          &
            5X,"-------------------------------------------------------",/)
1001      format(/                                                                 &
            5X,A,/,                                                                  &
            5X,"MATERIAL NUMBER . . . . . . . . . . . . . . . . . . . .=",I10/,      &
            5X,"MATERIAL LAW. . . . . . . . . . . . . . . . . . . . . .=",I10/)
1002      format(/                                                                 &
            5X,"INITIAL DENSITY . . . . . . . . . . . . . . . . . . . .=",1PG20.13/)
1003      format(/                                                                 &
            5X,"ELASTIC PARAMETERS:                                     ",/,         &
            5X,"-------------------                                     ",/,         &
            5X,"YOUNG MODULUS (E) . . . . . . . . . . . . . . . . . . .=",1PG20.13/  &
            5X,"POISSON RATIO (NU). . . . . . . . . . . . . . . . . . .=",1PG20.13/)
1004      format(/                                                                 &
            5X,"TEMPERATURE DEPENDENCY PARAMETERS:                     ",/,          &
            5X,"----------------------------------                     ",/,          &
            5X,'FUNCTION E(T) (HEATING) ID. . . . . . . . . . . . . . .=',I10/       &
            5X,'FUNCTION E(T) (COOLING) ID. . . . . . . . . . . . . . .=',I10/       &
            5X,'FUNCTION NU(T) ID . . . . . . . . . . . . . . . . . . .=',I10/       &
            5X,'REFERENCE TEMPERATURE (TREF). . . . . . . . . . . . . .=',1PG20.13/  &
            5X,'MELTING TEMPERATURE (TMELT) . . . . . . . . . . . . . .=',1PG20.13/  &
            5X,'INITIAL TEMPERATURE (T0). . . . . . . . . . . . . . . .=',1PG20.13/  &
            5X,'TEMPERATURE DEPENDENCY EXPONENT (M) . . . . . . . . . .=',1PG20.13/  &
            5X,'SPECIFIC HEAT (RHO*CP). . . . . . . . . . . . . . . . .=',1PG20.13/  &
            5X,'TAYLOR-QUINNEY COEFFICIENT (ETA). . . . . . . . . . . .=',1PG20.13/)
1005      format(/                                                                 &
            5X,"PLASTICITY PARAMETERS:                                  ",/,         &
            5X,"----------------------                                  ",/,         &
            5X,"INITIAL YIELD STRESS (A). . . . . . . . . . . . . . . .=",1PG20.13/  &
            5X,"HARDENING MODULUS (B) . . . . . . . . . . . . . . . . .=",1PG20.13/  &
            5X,"HARDENING EXPONENT (N). . . . . . . . . . . . . . . . .=",1PG20.13/  &
            5X,'MAXIMUM STRESS (SIG_MAX). . . . . . . . . . . . . . . .=',1PG20.13/)
1006      format(/                                                                 &
            5X,"STRAIN RATE DEPENDENCY PARAMETERS:                     ",/,          &
            5X,"----------------------------------                     ",/,          &
            5X,'JOHNSON-COOK STRAIN RATE DEPENDENCY COEFFICIENT (C) . .=',1PG20.13/  &
            5X,'REFERENCE STRAIN RATE (DEPS0) . . . . . . . . . . . . .=',1PG20.13/  &
            5X,'STRAIN RATE FILTERING FREQUENCY (FCUT). . . . . . . . .=',1PG20.13/  &
            5X,'VISCOUS FORMULATION FLAG (VP) . . . . . . . . . . . . .=',I10/       &
            5X,'  VP = 1: FULL VISCOPLASTIC FORMULATION                 ',/,         &
            5X,'  VP = 2: YIELD STRESS SCALING W.R.T TOTAL STRAIN RATE  ',/,         &
            5X,'  VP = 3: YIELD STRESS SCALING W.R.T DEV.  STRAIN RATE  ',/)
1007      format(/                                                                 &
            5X,"STRAIN RATE DEPENDENCY PARAMETERS:                     ",/,          &
            5X,"----------------------------------                     ",/,          &
            5X,'JOHNSON-COOK STRAIN RATE DEPENDENCY COEFFICIENT (C) . .=',1PG20.13/  &
            5X,'REFERENCE STRAIN RATE (DEPS0) . . . . . . . . . . . . .=',1PG20.13/  &
            5X,'VISCOUS FORMULATION FLAG (VP) . . . . . . . . . . . . .=',I10/       &
            5X,'  VP = 1: FULL VISCOPLASTIC FORMULATION                 ',/,         &
            5X,'  VP = 2: YIELD STRESS SCALING W.R.T TOTAL STRAIN RATE  ',/,         &
            5X,'  VP = 3: YIELD STRESS SCALING W.R.T DEV.  STRAIN RATE  ',/)
1008      format(/                                                                 &
            5X,"FAILURE PARAMETERS:                                    ",/,          &
            5X,"-------------------                                    ",/,          &
            5X,'MAXIMUM PLASTIC STRAIN AT FAILURE (EPSM). . . . . . . .=',1PG20.13/)
1009      format(/                                                                 &
            5X,"RETURN MAPPING PARAMETERS:                             ",/,          &
            5X,"--------------------------                             ",/,          &
            5X,'MAXIMUM NUMBER OF ITERATIONS (NMAX) . . . . . . . . . .=',I10/       &
            5X,'TOLERANCE ON YIELD FUNCTION (TOL) . . . . . . . . . . .=',1PG20.13/)
!
        end subroutine hm_read_mat106
      end module hm_read_mat106_mod
