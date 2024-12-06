!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||    hm_read_mat126_mod   ../starter/source/materials/mat/mat126/hm_read_mat126.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat          ../starter/source/materials/mat/hm_read_mat.F90
      !||====================================================================
      module hm_read_mat126_mod
      contains
! ======================================================================================================================
! \brief Reading material parameters of /MAT/LAW126
! \details Reading material parameters of /MAT/LAW126
! ======================================================================================================================
      !||====================================================================
      !||    hm_read_mat126           ../starter/source/materials/mat/mat126/hm_read_mat126.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F90
      !||--- calls      -----------------------------------------------------
      !||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||    init_mat_keyword         ../starter/source/materials/mat/init_mat_keyword.F
      !||--- uses       -----------------------------------------------------
      !||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
      !||    message_mod              ../starter/share/message_module/message_mod.F
      !||    submodel_mod             ../starter/share/modules1/submodel_mod.F
      !||====================================================================
        subroutine hm_read_mat126(                                 &
           nuvar    ,mtag     ,matparam ,npropm   ,iout     ,      &
           parmat   ,unitab   ,pm       ,lsubmodel,israte   ,      &
           mat_id   ,titr     )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
          use unitab_mod
          use message_mod
          use submodel_mod
          use matparam_def_mod    
          use elbuftag_mod      
          use constant_mod      
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
          implicit none 
#include  "my_real.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          integer, intent(inout)                :: nuvar             !< number of material law user variables
          type(mlaw_tag_), intent(inout)        :: mtag              !< material tag for internal variables in element buffer
          my_real, intent(inout)                :: parmat(100)       !< material parameter global table 1
          type(unit_type_),intent(in)           :: unitab            !< units table
          my_real, intent(inout)                :: pm(npropm)        !< material parameter global table 2
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel !< submodel data structure
          integer, intent(inout)                :: israte            !< strain rate filtering flag
          integer, intent(in)                   :: mat_id            !< material law user ID 
          character(len=nchartitle),intent(in)  :: titr              !< material law user title
          type(matparam_struct_) ,intent(inout) :: matparam          !< matparam data structure
          integer, intent(in)                   :: npropm            !< number of material property
          integer, intent(in)                   :: iout              !< output file number
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
          integer idel
          my_real                                                  &
            rho0,shear,aa,bb,nn,fc,t0,cc,eps0,asrate,sfmax,        &
            efmin,pc,muc,pl,mul,k0,k1,k2,k3,d1,d2,young,nu,        &
            eps_max,h
          logical :: is_encrypted, is_available
!-----------------------------------------------
!     S o u r c e 
!-----------------------------------------------
          is_encrypted = .false.
          is_available = .false.
!----------------------------------------------------------------
          call hm_option_is_encrypted(is_encrypted)
!----------------------------------------------------------------
!         #rhoo
          call hm_get_floatv('MAT_RHO'   ,rho0   ,is_available,lsubmodel, unitab)
!----------------------------------------------------------------
!         #g
          call hm_get_floatv('MAT_G'     ,shear  ,is_available,lsubmodel, unitab)
!----------------------------------------------------------------
!         #a b m n fc t
          call hm_get_floatv('MAT_A'     ,aa     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_B'     ,bb     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_N'     ,nn     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_FC'    ,fc     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_T0'    ,t0     ,is_available, lsubmodel, unitab)
!----------------------------------------------------------------
!         #c epsp_0 sigma_f_max
          call hm_get_floatv('MAT_C'     ,cc     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_EPS0'  ,eps0   ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_FCUT'  ,asrate ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_SFMAX' ,sfmax  ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_EFMIN' ,efmin  ,is_available, lsubmodel, unitab)
!----------------------------------------------------------------
!         #pc muc p1 mu1
          call hm_get_floatv('MAT_PC'    ,pc     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_MUC'   ,muc    ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_PL'    ,pl     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_MUL'   ,mul    ,is_available, lsubmodel, unitab)
!----------------------------------------------------------------
!         #k1 k2 k3
          call hm_get_floatv('MAT_K1'    ,k1     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_K2'    ,k2     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_K3'    ,k3     ,is_available, lsubmodel, unitab)
!----------------------------------------------------------------
!         #d1 d2 idel
          call hm_get_floatv('MAT_D1'    ,d1     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_D2'    ,d2     ,is_available, lsubmodel, unitab)
          call hm_get_intv  ('IDEL'      ,idel   ,is_available, lsubmodel)
          call hm_get_floatv('MAT_EPSMAX',eps_max,is_available, lsubmodel, unitab)
!    
          ! Bulk modulus in region 1 
          k0 = pc/muc 
          ! Tangent bulk modulus
          h  = (pl - pc)/mul
          ! Activation of strain-rate filtering
          if (asrate /= zero) then
            israte = 1
          else
            israte = 0
          endif
          ! check flag for element deletion
          idel = min(idel,4)
          idel = max(0,idel)
          ! default values
          if (efmin == zero) efmin = em20
          if (cc == zero)    eps0  = one
          if (sfmax == zero) sfmax = infinity
          if (eps_max == zero) eps_max = infinity
!
!--------------------------
!     Filling buffer tables
!-------------------------- 
          ! Number of integer material parameters
          matparam%niparam = 1
          ! Number of real material parameters
          matparam%nuparam = 23
          ! Number of user variables 
          nuvar = 4
!          
          ! Allocation of material parameters tables
          allocate (matparam%iparam(matparam%niparam))
          allocate (matparam%uparam(matparam%nuparam))
!     
          ! Integer material parameter
          matparam%iparam(1)  = idel
!    
          ! Real material parameters
          matparam%uparam(1)  = shear
          matparam%uparam(2)  = two*shear
          matparam%uparam(3)  = aa
          matparam%uparam(4)  = bb
          matparam%uparam(5)  = nn
          matparam%uparam(6)  = fc
          matparam%uparam(7)  = t0
          matparam%uparam(8)  = cc
          matparam%uparam(9)  = eps0
          matparam%uparam(10) = sfmax
          matparam%uparam(11) = efmin
          matparam%uparam(12) = pc
          matparam%uparam(13) = muc
          matparam%uparam(14) = pl
          matparam%uparam(15) = mul
          matparam%uparam(16) = k0
          matparam%uparam(17) = k1
          matparam%uparam(18) = k2
          matparam%uparam(19) = k3
          matparam%uparam(20) = d1
          matparam%uparam(21) = d2
          matparam%uparam(22) = eps_max
          matparam%uparam(23) = h
!
          ! PARMAT table
          nu = (three*k0-two*shear)/(six*k0+two*shear)
          young = nine*k0*shear/(three*k0+shear)
          parmat(1) = k1
          parmat(2) = young
          parmat(3) = nu
          parmat(4) = israte
          parmat(5) = asrate
!
          ! PM table
          pm(1)  = rho0
          pm(89) = rho0
!
          ! MTAG variable activation
          mtag%g_epsd = 1
          mtag%l_epsd = 1
          mtag%g_pla  = 1
          mtag%l_pla  = 1
          mtag%g_dmg  = 1
          mtag%l_dmg  = 1
!
          ! Properties compatibility  
          call init_mat_keyword(matparam,"SOLID_ISOTROPIC") 
          call init_mat_keyword(matparam,"SPH")      
! 
          ! Properties compatibility  
          call init_mat_keyword(matparam ,"COMPRESSIBLE")
          call init_mat_keyword(matparam ,"INCREMENTAL" )
          call init_mat_keyword(matparam ,"LARGE_STRAIN")
          call init_mat_keyword(matparam ,"HYDRO_EOS") 
          call init_mat_keyword(matparam ,"ISOTROPIC") 
!
!--------------------------
!     Parameters printout
!--------------------------
          write(iout, 900) trim(titr),mat_id,126
          write(iout,1000)
          if (is_encrypted) then
            write(iout,'(5x,a,//)') 'confidential data'
          else
            write(iout,1100) rho0
            write(iout,1200) shear,young,nu 
            write(iout,1300) aa,bb,nn,fc,t0
            if (israte > 0) then 
              write(iout,1400) cc,eps0,asrate,sfmax,efmin
            else 
              write(iout,1500) cc,eps0,sfmax,efmin
            endif
            write(iout,1600) pc,muc,pl,mul,k0,k1,k2,k3
            write(iout,1700) d1,d2,idel,eps_max
          endif
!-----------------------------------------------------------------------
  900 format(/                                                        &
        5X,A,/,                                                       &
        5X,'MATERIAL NUMBER. . . . . . . . . . . . . . .=',I10/,      &
        5X,'MATERIAL LAW . . . . . . . . . . . . . . . .=',I10/)
 1000 format(                                                         &
        5X,'-------------------------------------------',/            &
        5X,'MATERIAL MODEL:  JOHNSON-HOLMQUIST CONCRETE',/,           &
        5X,'-------------------------------------------',/)
 1100 format(                                                         &
        5X,'INITIAL DENSITY. . . . . . . . . . . . . . .=',1PG20.13/)  
 1200 format(                                                         &
        5X,'SHEAR MODULUS (G). . . . . . . . . . . . . .=',1PG20.13/, &
        5X,'YOUNG MODULUS E (COMPUTED) . . . . . . . . .=',1PG20.13/, &
        5X,"POISSON'S RATIO (COMPUTED) . . . . . . . . .=",1PG20.13/)
 1300 format(                                                         &
        5X,'NORMALIZED COHESIVE STRENGTH (A) . . . . . .=',1PG20.13/, &
        5X,'NORMALIZED PRESSURE HARDENING (B). . . . . .=',1PG20.13/, &
        5X,'PRESSURE HARDENING EXPONENT (N). . . . . . .=',1PG20.13/, &
        5X,'COMPRESSIVE STRENGTH (FC). . . . . . . . . .=',1PG20.13/, &
        5X,'TENSILE STRENGTH (T) . . . . . . . . . . . .=',1PG20.13/)
 1400 format(                                                         &
        5X,'STRAIN RATE DEPENDENCY PARAMETER (C) . . . .=',1PG20.13/, & 
        5X,'REFERENCE STRAIN RATE (EPS0) . . . . . . . .=',1PG20.13/, &
        5X,'STRAIN RATE FILTERING FREQUENCY (FCUT) . . .=',1PG20.13/, &
        5X,'NORMALIZED MAXIMUM STRENGTH (SFMAX)  . . . .=',1PG20.13/, &
        5X,'MINIMUM FRACTURE STRAIN (EFMIN). . . . . . .=',1PG20.13/)
 1500 format(                                                         &
        5X,'STRAIN RATE DEPENDENCY PARAMETER (C) . . . .=',1PG20.13/, &
        5X,'REFERENCE STRAIN RATE (EPS0) . . . . . . . .=',1PG20.13/, &
        5X,'NORMALIZED MAXIMUM STRENGTH (SFMAX)  . . . .=',1PG20.13/, &
        5X,'MINIMUM FRACTURE STRAIN (EFMIN). . . . . . .=',1PG20.13/)
 1600 format(                                                         &
        5X,'CRUSHING PRESSURE (PC) . . . . . . . . . . .=',1PG20.13/, &
        5X,'CRUSHING VOLUMETRIC STRAIN (MUC) . . . . . .=',1PG20.13/, &
        5X,'LOCKING PRESSURE (PL). . . . . . . . . . . .=',1PG20.13/, &
        5X,'LOCKING VOLUMETRIC STRAIN (MUL). . . . . . .=',1PG20.13/, &
        5X,'BULK MODULUS IN REGION 1 (K0). . . . . . . .=',1PG20.13/, &
        5X,'LINEAR BULK MODULUS (K1) . . . . . . . . . .=',1PG20.13/, &
        5X,'QUADRATIC BULK MODULUS (K2). . . . . . . . .=',1PG20.13/, &
        5X,'CUBIC BULK MODULUS (K3). . . . . . . . . . .=',1PG20.13/)
 1700 format(                                                         &
        5X,'DAMAGE PARAMETER (D1). . . . . . . . . . . .=',1PG20.13/, &
        5X,'DAMAGE EXPONENT (D2) . . . . . . . . . . . .=',1PG20.13/, &
        5X,'ELEMENT DELETION FLAG (IDEL) . . . . . . . .=',I10/,      &
        5X,'  IDEL = 0:  NO ELEMENT DELETION             ',/,         &
        5X,'  IDEL = 1:  TENSILE FAILURE P* + T* < 0     ',/,         &
        5X,'  IDEL = 2:  MAXIMUM PLASTIC STRAIN FAILURE  ',/,         &
        5X,'  IDEL = 3:  FAILURE IF SIGY <= 0            ',/,         &
        5X,'  IDEL = 4:  FAILURE IF DAMAGE = 1           ',/,         &
        5X,'MAXIMUM PLASTIC STRAIN (EPS_MAX) . . . . . .=',1PG20.13/)
!
        end subroutine hm_read_mat126
!-------------------
      end module hm_read_mat126_mod

