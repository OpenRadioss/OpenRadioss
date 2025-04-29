!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||    hm_read_mat133_mod   ../starter/source/materials/mat/mat133/hm_read_mat133.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat          ../starter/source/materials/mat/hm_read_mat.F90
      !||====================================================================
      module hm_read_mat133_mod
      contains
! ======================================================================================================================
! \brief Reading material parameters of /MAT/LAW133
! \details
! ======================================================================================================================
      !||====================================================================
      !||    hm_read_mat133           ../starter/source/materials/mat/mat133/hm_read_mat133.F90
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
      !||    table_mat_vinterp        ../starter/source/materials/tools/table_mat_vinterp.F
      !||--- uses       -----------------------------------------------------
      !||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
      !||    mat_table_copy_mod       ../starter/source/materials/tools/mat_table_copy.F90
      !||    message_mod              ../starter/share/message_module/message_mod.F
      !||    submodel_mod             ../starter/share/modules1/submodel_mod.F
      !||    table_mat_vinterp_mod    ../starter/source/materials/tools/table_mat_vinterp.F
      !||    table_mod                ../starter/share/modules1/table_mod.F
      !||====================================================================
        subroutine hm_read_mat133(                             &
           nuvar    ,mtag     , matparam ,iout     ,parmat   , &
           unitab   ,lsubmodel, mat_uid  ,titr     ,nvartmp  , &
           ntable   ,table    , npropm   ,npropmi  , &
           pm       , ipm)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use message_mod
          use elbuftag_mod , only : mlaw_tag_
          use unitab_mod , only : unit_type_
          use submodel_mod , only : submodel_data, nsubmod
          use matparam_def_mod , only : matparam_struct_
          use constant_mod , only : zero, em20, half, one, two, three
          use names_and_titles_mod , only : nchartitle
          use table_mod , only : ttable
          use mat_table_copy_mod , only : mat_table_copy
          use table_mat_vinterp_mod , only : table_mat_vinterp
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none 
#include  "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(inout)                            :: nuvar        !< number of material law user variables
          type(mlaw_tag_),intent(inout)                     :: mtag         !< material tag for internal variables in element buffer
          my_real,intent(inout)                             :: parmat(100)  !< material parameter global table 1
          type(unit_type_),intent(in)                       :: unitab       !< units table
          type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel    !< submodel data structure
          integer,intent(in)                                :: mat_uid      !< material law user ID
          character(len=nchartitle),intent(in)              :: titr         !< material law user title
          type(matparam_struct_) ,intent(inout)             :: matparam     !< matparam data structure
          integer,intent(in)                                :: iout         !< output file number
          integer,intent(in)                                :: ntable       !< array size for table
          type(ttable),dimension(ntable),intent(in)         :: table        !< tables data structure
          integer,intent(inout)                             :: nvartmp      !< number of temporary variables
          integer,intent(in)                                :: npropm,npropmi ! array size
          integer,intent(inout)                             :: ipm(npropmi) !< material parameter (integer)
          my_real,intent(inout)                             :: pm(npropm)   !< material parameter (real)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: fctID_g, fctID_y
          integer :: ierror
          integer :: ipos1(1,1)
          my_real :: xvec1(1,1)
          my_real :: g(1)
          my_real :: fscale_g, fscale_y
          my_real :: rho0, nu, pmin, shear, bulk, young
          my_real :: x1scale, x2scale, x3scale, x4scale
          my_real :: x2vect(2),x3vect(2),x4vect(2)
          my_real :: fscale(2)
          my_real :: slope(1)
          my_real :: density_unit, pressure_unit
          logical :: is_encrypted, is_available
          character*32 :: mtl_msg
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          is_encrypted = .false.
          is_available = .false.
          mtl_msg = ''

          matparam%ieos = 18  ! linear eos is used by default
          ipm(4)        = 18
!----------------------------------------------------------------
          call hm_option_is_encrypted(is_encrypted)
!----------------------------------------------------------------
!         #initial density
          call hm_get_floatv('MAT_RHO', rho0, is_available, lsubmodel, unitab)
!----------------------------------------------------------------
!         #Poisson's ratio
          call hm_get_floatv('MAT_NU', nu, is_available, lsubmodel, unitab)
!----------------------------------------------------------------
!         #Minimum/fracture Pressure
          call hm_get_floatv('MAT_PMIN', pmin, is_available, lsubmodel, unitab)
!----------------------------------------------------------------
!         #scale factors
          call hm_get_floatv('FSCALE_G', fscale_g, is_available, lsubmodel, unitab)
          call hm_get_floatv('FSCALE_Y', fscale_y, is_available, lsubmodel, unitab)
!----------------------------------------------------------------
!         #function identifiers
          call hm_get_intv('FCT_ID_G' ,fctID_g, is_available, lsubmodel)
          call hm_get_intv('FCT_ID_Y' ,fctID_y, is_available, lsubmodel)
!----------------------------------------------------------------

          !Default Values
          if (fscale_g == zero) then
            fscale_g = one
          endif
          if (fscale_y == zero) then
            fscale_y = one
          endif

          !Input checks
          if (nu <= zero .OR.nu >= half ) then
            mtl_msg = "LAW133 (GRANULAR)"
            call ancmsg(msgid=1514,msgtype=msgerror,anmode=aninfo,i1 = mat_uid,c1=mtl_msg,c2=titr)
          endif

          if(pmin == zero)then
            pmin = em20
          end if
          pm(37) = pmin

!-------------------------------------
          matparam%niparam = 0          !< Number of integer material parameters
          matparam%nuparam = 1          !< Number of real material parameters
          matparam%ntable = 2           !< Number of user functions
          nuvar = 0                     !< Number of user variables
          nvartmp = 2                   !< Number of temporary variables

          !< Allocation of material parameters tables
          !allocate (matparam%iparam(matparam%niparam))
          allocate (matparam%uparam(matparam%nuparam))
          allocate (matparam%table(matparam%ntable))

          !< user function storage
          call hm_get_floatv_dim('MAT_RHO',density_unit,is_available,lsubmodel,unitab)
          call hm_get_floatv_dim('FSCALE_G',pressure_unit,is_available,lsubmodel,unitab)
          matparam%table(1)%notable = fctID_g
          matparam%table(2)%notable = fctID_y
          x1scale   = one * density_unit
          x2scale   = one * pressure_unit
          x3scale   = one
          x4scale   = one
          x3vect(1) = zero
          x4vect(1) = zero
          fscale(1) = Fscale_g
          fscale(2) = Fscale_y
          call mat_table_copy(matparam ,x2vect   ,x3vect   ,x4vect   ,         &
                    x1scale  ,x2scale  ,x3scale  ,x4scale  ,fscale   ,         &
                    ntable   ,table    ,ierror   )

          xvec1(1,1) = rho0
          ipos1 = 1
          CALL TABLE_MAT_VINTERP(matparam%table(1),1,1,ipos1,xvec1,g(1),slope(1))

          shear = g(1)
          young = two * shear * (one + nu) !< Young's modulus
          bulk = two*shear*(one + nu) / (three*(one-two*nu)) !< Bulk modulus

          !< Real material parameters
          matparam%young = young !initial
          matparam%nu    = nu
          matparam%shear = shear !initial
          matparam%bulk  = bulk  !intial

          matparam%uparam(1) = Pmin

          !< PARMAT table
          parmat(1) = bulk !max
          pm(32) = bulk ! default EoS

          !< Initial and reference density
          matparam%rho0 = rho0
          matparam%rho  = rho0

          !< MTAG variable activation
          mtag%g_pla = 1
          mtag%l_pla = 1

          !< Properties compatibility  
          call init_mat_keyword(matparam,"SOLID_ISOTROPIC")
          call init_mat_keyword(matparam ,"COMPRESSIBLE")
          call init_mat_keyword(matparam ,"INCREMENTAL")
          call init_mat_keyword(matparam ,"LARGE_STRAIN")
          call init_mat_keyword(matparam ,"HYDRO_EOS") 
          call init_mat_keyword(matparam ,"ISOTROPIC")
          call init_mat_keyword(matparam,"SPH")
          call init_mat_keyword(matparam ,"EOS")
          call init_mat_keyword(matparam ,"VISC")

!--------------------------
!     Parameters printout
!--------------------------
          write(iout, 100) trim(titr),mat_uid,133
          write(iout,200)
          if (is_encrypted) then
            write(iout,'(5X,A,//)') 'CONFIDENTIAL DATA'
          else
            write(iout,300) rho0
            write(iout,400) nu,young,pmin
            write(iout,500) fctID_g,fctID_y
            write(iout,600) fscale_g,fscale_y
          endif

!-----------------------------------------------------------------------
  100 format(/                                                        &
        5X,A,/,                                                       &
        5X,'MATERIAL NUMBER. . . . . . . . . . . . . . .=',I10/,      &
        5X,'MATERIAL LAW . . . . . . . . . . . . . . . .=',I10/)
  200 format(/                                                        &
        5X,'-------------------------',/         &
        5X,'MATERIAL MODEL:  GRANULAR',/,        &
        5X,'-------------------------',/)
  300 format(/                                                        &
        5X,'INITIAL DENSITY. . . . . . . . . . . . . .  =',1PG20.13/)
  400 format(/                                                        &
        5X,'POISSON''S RATIO  . . . . . . . . . . . . .  =',1PG20.13/, &
        5X,'YOUNG MODULUS E (COMPUTED) . . . . . . . .  =',1PG20.13/, &
        5X,"MINIMUM PRESSURE (FRACTURE). . . . . . . .  =",1PG20.13/)
  500 format(/                                                        &
        5X,'SHEAR MODULUS FUNCTION ID - G(RHO) . . . .  =',I10/,      &
        5X,'YIELD FUNCTION ID - Y(P) . . . . . . . . .  =',I10/ )
  600 format(/                                                        &
        5X,'SHEAR SCALE FACTOR (FSCALE_G)  . . . . . .  =',1PG20.13/, &
        5X,'YIELD SCALE FACTOR (FSCALE_Y)  . . . . . .  =',1PG20.13/ )
!
        end subroutine hm_read_mat133
!-------------------
      end module hm_read_mat133_mod

