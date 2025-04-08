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
      !||    hm_read_mat163_mod   ../starter/source/materials/mat/mat163/hm_read_mat163.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat          ../starter/source/materials/mat/hm_read_mat.F90
      !||====================================================================
      module hm_read_mat163_mod
      contains
! ======================================================================================================================
! \brief Reading material parameters of /MAT/LAW163 (CRUSHABLE_FOAM)
! \details Reading material parameters of /MAT/LAW163 (CRUSHABLE_FOAM)
! ======================================================================================================================
      !||====================================================================
      !||    hm_read_mat163           ../starter/source/materials/mat/mat163/hm_read_mat163.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F90
      !||--- calls      -----------------------------------------------------
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
      !||    submodel_mod             ../starter/share/modules1/submodel_mod.F
      !||    table_mod                ../starter/share/modules1/table_mod.F
      !||====================================================================
      subroutine hm_read_mat163(                                               &
                   matparam ,nvartmp  ,parmat   ,unitab   ,mat_id   ,titr     ,&
                   mtag     ,lsubmodel,iout     ,nuvar    ,ilaw     ,ntable   ,&
                   table    )
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
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
        implicit none 
#include  "my_real.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
        type(matparam_struct_) ,intent(inout) :: matparam          !< matparam data structure

        integer, intent(inout)                :: nvartmp           !< number of temporary variables
        my_real, intent(inout)                :: parmat(100)       !< material parameter global table 1
        type(unit_type_),intent(in)           :: unitab            !< units table
        integer, intent(in)                   :: mat_id            !< material law user ID 
        character(len=nchartitle),intent(in)  :: titr              !< material law user title
        type(mlaw_tag_), intent(inout)        :: mtag              !< material tag for internal variables in element buffer
        type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel !< submodel data structure
        integer, intent(in)                   :: iout              !< output file number
        integer, intent(inout)                :: nuvar             !< number of user variables
        integer, intent(in)                   :: ilaw              !< material law number
        integer, intent(in)                   :: ntable            !< number of tables
        type(ttable),dimension(ntable),intent(in) :: table         !< tables data structure
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
        integer :: tab_id,ncycle,nrs,ierr
        my_real :: rho0,shear,young,nu,bulk,tsc,damp,srclmt,lam,cii,cij,       &
          x1scale,x2scale,x3scale,x4scale,x2vect(1),x3vect(1),x4vect(1),       &
          fscale(1)
        logical :: is_encrypted,is_available
!-----------------------------------------------
!     S o u r c e 
!-----------------------------------------------
        is_encrypted = .false.
        is_available = .false.
!-------------------------------------------------------------------------------
        call hm_option_is_encrypted(is_encrypted)
!-------------------------------------------------------------------------------
        !< Density
        call hm_get_floatv('Rho'         ,rho0   ,is_available, lsubmodel, unitab)
!-------------------------------------------------------------------------------
        !< 1st line of material card
        call hm_get_floatv('E'           ,young  ,is_available, lsubmodel, unitab)
        call hm_get_floatv('Nu'          ,nu     ,is_available, lsubmodel, unitab)
        call hm_get_floatv('LSDYNA_TSC'  ,tsc    ,is_available, lsubmodel, unitab)
        call hm_get_floatv('LSD_MAT_DAMP',damp   ,is_available, lsubmodel, unitab)
        call hm_get_intv  ('LSD_NCYCLE'  ,ncycle ,is_available, lsubmodel)
!-------------------------------------------------------------------------------
        !< 2nd line of material card
        call hm_get_intv  ('LSD_TID'   ,tab_id   ,is_available, lsubmodel)
        call hm_get_floatv('EPSD_REF'  ,x2vect(1),is_available, lsubmodel, unitab)
        call hm_get_floatv('FSCALE'    ,fscale(1),is_available, lsubmodel, unitab)
        call hm_get_floatv('LSD_SRCLMT',srclmt   ,is_available, lsubmodel, unitab)
        call hm_get_intv  ('NRSFlag'   ,nrs      ,is_available, lsubmodel)
!-------------------------------------------------------------------------------
!
        !< Reference strain rate
        if (x2vect(1) == zero) then
          call hm_get_floatv_dim('EPSD_REF',x2vect(1),is_available,lsubmodel,unitab)
        endif
        !< Scale factor
        if (fscale(1) == zero) then
          call hm_get_floatv_dim('FSCALE',fscale(1),is_available,lsubmodel,unitab)
        endif 
        !< Shear modulus
        shear = young/(two*(one+nu))
        bulk  = young/(three*(one - two*nu))
        !< Stifness matrix components
        lam = young*nu / (one+nu) / (one - two*nu)
        cii = lam + shear*two
        cij = lam   
        !< Damping default value
        if (damp == zero) damp = em01
        !< Default number of cycles
        if (ncycle == zero) ncycle = 12
        !< Default strain rate change limit
        if (srclmt == zero) srclmt = infinity
!
!-------------------------------------------------------------------------------
        !< Filling buffer tables
!------------------------------------------------------------------------------- 
        !< Number of integer material parameters
        matparam%niparam = 2
        !< Number of real material parameters
        matparam%nuparam = 5
        !< Number of user variables 
        nuvar = 2
        !< Number of tables and temporary variables
        matparam%ntable = 1
        nvartmp = 2
!          
        !< Allocation of material parameters tables
        allocate(matparam%iparam(matparam%niparam))
        allocate(matparam%uparam(matparam%nuparam))
        allocate(matparam%table (matparam%ntable ))
!     
        !< Integer material parameter
        matparam%iparam(1)  = ncycle
        matparam%iparam(2)  = nrs
!    
        !< Real material parameters
        matparam%young      = young
        matparam%nu         = nu
        matparam%shear      = shear
        matparam%bulk       = bulk
        matparam%uparam(1)  = cii
        matparam%uparam(2)  = cij
        matparam%uparam(3)  = tsc
        matparam%uparam(4)  = damp
        matparam%uparam(5)  = srclmt
!
        !< Transform global table into material table
        matparam%table(1)%notable = tab_id
        x1scale   = one
        x2scale   = one
        x3scale   = one
        x4scale   = one
        x3vect(1) = zero
        x4vect(1) = zero
        call mat_table_copy(matparam ,x2vect   ,x3vect   ,x4vect   ,         &
                  x1scale  ,x2scale  ,x3scale  ,x4scale  ,fscale   ,         & 
                  ntable   ,table    ,ierr     )
!
        !< PARMAT table
        parmat(1)  = bulk
        parmat(2)  = young
        parmat(3)  = nu
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
!
        !< Properties compatibility  
        call init_mat_keyword(matparam,"SOLID_ISOTROPIC") 
        call init_mat_keyword(matparam,"SPH")      
! 
        !< Material model keywords
        call init_mat_keyword(matparam ,"COMPRESSIBLE")
        call init_mat_keyword(matparam ,"INCREMENTAL" )
        call init_mat_keyword(matparam ,"LARGE_STRAIN")
        call init_mat_keyword(matparam ,"HOOK")
        call init_mat_keyword(matparam ,"ISOTROPIC") 
!
!-------------------------------------------------------------------------------
        !< Printing out the material data
!-------------------------------------------------------------------------------
        write(iout,1001) trim(titr),mat_id,ilaw
        write(iout,1000)
        if (is_encrypted)then                                     
          write(iout,'(5X,A,//)') 'CONFIDENTIAL DATA'
        else     
          write(iout,1002) rho0
          write(iout,1003) young,nu
          write(iout,1004) tsc,damp,ncycle,tab_id,x2vect(1),fscale(1),srclmt,nrs
        endif
!
 1000 format(/                                                                 &                                                                                            
       5X,'-------------------------------------------------------',/          &     
       5X,'           MATERIAL MODEL: CRUSHABLE FOAM              ',/,         & 
       5X,'-------------------------------------------------------',/)
 1001 format(/                                                                 &
       5X,A,/,                                                                 &
       5X,'MATERIAL NUMBER. . . . . . . . . . . . . . . . . . . .=',I10/,      &
       5X,'MATERIAL LAW . . . . . . . . . . . . . . . . . . . . .=',I10/)
 1002 format(/                                                                 &
       5X,'INITIAL DENSITY. . . . . . . . . . . . . . . . . . . .=',1PG20.13/)  
 1003 format(/                                                                 &
       5X,'ELASTIC PARAMETERS:                                    ',/,         &
       5X,'-------------------                                    ',/,         &
       5X,'YOUNG MODULUS (E). . . . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'POISSON RATIO (NU) . . . . . . . . . . . . . . . . . .=',1PG20.13/)      
 1004 format(/                                                                 &
       5X,'PLASTIC PARAMETERS:                                    ',/,         &
       5X,'-------------------                                    ',/,         &
       5X,'TENSILE STRESS CUTOFF (TSC). . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'DAMPING PARAMETER (DAMP) . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'NUMBER OF CYCLES (NCYCLE). . . . . . . . . . . . . . .=',I10/       &
       5X,'YIELD STRESS TABLE ID. . . . . . . . . . . . . . . . .=',I10/       &
       5X,'REFERENCE STRAIN RATE (EPSD_REF) . . . . . . . . . . .=',1PG20.13/  &
       5X,'SCALE FACTOR (FSCALE). . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'STRAIN RATE CHANGE LIMIT (SRCLMT). . . . . . . . . . .=',1PG20.13/  &
       5X,'STRAIN RATE FLAG (NRS) . . . . . . . . . . . . . . . .=',I10/       &
       5X,'  = 0: TRUE STRAIN RATE                                ',/          &
       5X,'  = 1: ENGINEERING STRAIN RATE                         ',/)
!
      end subroutine hm_read_mat163
      end module hm_read_mat163_mod