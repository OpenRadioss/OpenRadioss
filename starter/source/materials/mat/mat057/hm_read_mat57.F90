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
      !||    hm_read_mat57_mod   ../starter/source/materials/mat/mat057/hm_read_mat57.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat         ../starter/source/materials/mat/hm_read_mat.F90
      !||====================================================================
     module hm_read_mat57_mod
      contains
      !||====================================================================
      !||    hm_read_mat57                  ../starter/source/materials/mat/mat057/hm_read_mat57.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat                    ../starter/source/materials/mat/hm_read_mat.F90
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                         ../starter/source/output/message/message.F
      !||    calculp2                       ../starter/source/materials/mat/mat057/calculp2.F90
      !||    func_table_copy                ../starter/source/materials/tools/func_table_copy.F90
      !||    hm_get_float_array_index       ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
      !||    hm_get_float_array_index_dim   ../starter/source/devtools/hm_reader/hm_get_float_array_index_dim.F
      !||    hm_get_floatv                  ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_int_array_index         ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
      !||    hm_get_intv                    ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted         ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||    init_mat_keyword               ../starter/source/materials/mat/init_mat_keyword.F
      !||--- uses       -----------------------------------------------------
      !||    calculp2_mod                   ../starter/source/materials/mat/mat057/calculp2.F90
      !||    elbuftag_mod                   ../starter/share/modules1/elbuftag_mod.F
      !||    func_table_copy_mod            ../starter/source/materials/tools/func_table_copy.F90
      !||    message_mod                    ../starter/share/message_module/message_mod.F
      !||    submodel_mod                   ../starter/share/modules1/submodel_mod.F
      !||    table_mod                      ../starter/share/modules1/table_mod.F
      !||====================================================================
      subroutine hm_read_mat57(matparam ,nuvar    ,parmat   ,mat_id   ,        & 
                               titr     ,unitab   ,lsubmodel,mtag     ,        &
                               iout     ,nvartmp  ,israte   ,ntable   ,        &
                               table    )                     
!-------------------------------------------------------------------------------
!   M o d u l e s
!-------------------------------------------------------------------------------
      use unitab_mod
      use message_mod
      use submodel_mod
      use matparam_def_mod    
      use elbuftag_mod      
      use constant_mod   
      use calculp2_mod 
      use func_table_copy_mod
      use table_mod
!-------------------------------------------------------------------------------
!   I m p l i c i t   T y p e s
!-------------------------------------------------------------------------------
      implicit none 
#include  "my_real.inc"
!-------------------------------------------------------------------------------
!   D u m m y   A r g u m e n t s
!-------------------------------------------------------------------------------
      type(matparam_struct_) ,intent(inout)       :: matparam !< matparam data structure
      integer, intent(inout)                      :: nuvar    !< number of material law user variables
      my_real, dimension(100),intent(inout)       :: parmat   !< material parameter global table 1
      integer, intent(in)                         :: mat_id   !< material law user ID 
      character(len=nchartitle),intent(in)        :: titr     !< material law user title
      type(unit_type_),intent(in)                 :: unitab   !< units table
      type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel !< submodel data structure
      type(mlaw_tag_), intent(inout)              :: mtag     !< material tag for internal variables in element buffer
      integer, intent(in)                         :: iout     !< output file number
      integer, intent(inout)                      :: nvartmp  !< number of temporary variables
      integer, intent(inout)                      :: israte   !< strain rate filtering flag
      integer, intent(in)                         :: ntable   !< number of tables
      type(ttable), dimension(ntable), intent(in) :: table    !< tables
!-------------------------------------------------------------------------------
!   L o c a l   V a r i a b l e s
!-------------------------------------------------------------------------------
      integer :: i,j,nrate,opte,ifunce(1),ilaw,vp,ierr2,ifunc(11)
      my_real :: rho0,rhor,e,nu,c1,epsmax,epsr1,epsr2,rate(11),yfac(11),    &
        yfac_unit(11),r0,r45,r90,r,h,fisokin,m,einf,ce,asrate,      &
        x1scale,x2scale,x2vect(11),fscale(11) 
      logical :: is_available,is_encrypted
!-------------------------------------------------------------------------------
!     S o u r c e 
!------------------------------------------------------------------------------- 
      is_encrypted = .false.
      is_available = .false.
      ilaw = 57
!-------------------------------------------------------------------------------
      call hm_option_is_encrypted(is_encrypted)
!-------------------------------------------------------------------------------
!< Card1
      call hm_get_floatv('MAT_RHO'    ,rho0    ,is_available, lsubmodel, unitab)
      call hm_get_floatv('Refer_Rho'  ,rhor    ,is_available, lsubmodel, unitab)
!< Card2
      call hm_get_floatv('MAT_E'      ,e       ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_NU'     ,nu      ,is_available, lsubmodel, unitab)
!< Card3
      call hm_get_intv  ('MAT_fct_IDE',ifunce(1),is_available, lsubmodel)
      call hm_get_floatv('MAT_EA'     ,einf    ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_CE'     ,ce      ,is_available, lsubmodel, unitab)
!< Card4
      call hm_get_floatv('MAT_R00'    ,r0      ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_R45'    ,r45     ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_R90'    ,r90     ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_CHARD'  ,fisokin ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_M'      ,m       ,is_available, lsubmodel, unitab)
!< Card5
      call hm_get_floatv('MAT_EPS'    ,epsmax  ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_EPST1'  ,epsr1   ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_EPST2'  ,epsr2   ,is_available, lsubmodel, unitab)
      call hm_get_floatv('Fcut'       ,asrate  ,is_available, lsubmodel, unitab)
      call hm_get_intv  ('Fsmooth'    ,israte  ,is_available, lsubmodel)
      call hm_get_intv  ('MAT_VP'     ,vp      ,is_available, lsubmodel)
!< Card6
      nrate = 0
      do j = 1,10
        call hm_get_int_array_index  ('FunctionIds',ifunc(j),j,is_available,lsubmodel)
        call hm_get_float_array_index('ABG_cpa',yfac(j),j,is_available,lsubmodel,unitab)
        if (yfac(j) == zero) then
          call hm_get_float_array_index_dim('ABG_cpa',yfac_unit(j),j,is_available,lsubmodel,unitab) 
          yfac(j) = one*yfac_unit(j)
        endif
        call hm_get_float_array_index('ABG_cpb',rate(j),j,is_available,lsubmodel,unitab)
        if (ifunc(j) /= 0) nrate = j                          
      enddo
!
      !-------------------------------------------------------------------------
      !< Data checking
      !-------------------------------------------------------------------------
      if (nrate == 0) then
         call ancmsg(msgid = 366,                                              &
                     msgtype = msgerror,                                       &  
                     anmode = aninfo,                                          &
                     i1 = mat_id,                                              &
                     c1 = titr)
      endif
!   
      !-------------------------------------------------------------------------
      !< Default values
      !-------------------------------------------------------------------------
      !< Default Lankford coefficients (= isotropic material)
      if (r0    == zero) r0  = one
      if (r45   == zero) r45 = one
      if (r90   == zero) r90 = one
      !< Default yield criterion exponent
      if (m     == zero) m   = six
      !< Default failure strains
      if (epsmax == zero) epsmax = infinity
      if (epsr1 == zero)  epsr1  = infinity
      if (epsr2 == zero)  epsr2  = two*infinity
      !< R and H Barlat parameters
      r =  r0/(one +  r0)
      h = r90/(one + r90)
      !< Strain rate formulation flag
      vp = min(vp,1)
      vp = max(vp,0)
      !< Tabulated Young modulus evolution flag
      if (ifunce(1) > 0) then
        opte = 1
      else
        opte = 0
      endif
      !< Kinematic hardening factor
      fisokin = max(fisokin,zero)
      fisokin = min(fisokin,one)
!
      !< Total strain rate filtering (VP = 0)
      if (vp == 0) then 
        !< Strain rate filtering frequency parameter
        if (asrate /= zero) then
          ! if a filtering frequency is given by the user
          israte = 1
        else
          ! if no filtering frequency is given but the flag is activated
          if (israte /= 0) then
            asrate  = 10000.0d0*unitab%fac_t_work  
          ! if no filtering frequency and no flag is activated => no filtering
          else
            asrate  = zero
          endif
        endif  
      !< By default, filtering activated with VP = 1
      else 
        israte = 1
        if (asrate == zero) asrate = 10000.0d0*unitab%fac_t_work
      endif
!
      !-------------------------------------------------------------------------
      !< Filling buffer tables
      !-------------------------------------------------------------------------
!
      !< Number of integer material parameters
      matparam%niparam = 2
      !< Number of real material parameters
      matparam%nuparam = 13
      !< Number of user variables 
      nuvar = 0
      !< Number of temporary variables and size of the material table
      if (opte > 0) then 
        nvartmp = 3
        matparam%ntable = 2
      else
        nvartmp = 2
        matparam%ntable = 1
      endif
!
      !< Allocation of material parameters tables
      allocate(matparam%iparam(matparam%niparam))
      allocate(matparam%uparam(matparam%nuparam))
      allocate(matparam%table(matparam%ntable))
!
      !< Elastic parameters
      matparam%bulk  = third*e/(one - nu*two)
      matparam%shear = half*e/(one+nu)
      matparam%young = e
      matparam%nu    = nu
!     
      !< Integer material parameter
      matparam%iparam(1) = opte
      matparam%iparam(2) = vp
!
      !< Real material parameters
      matparam%uparam(1)  = e/(one-nu*nu)
      matparam%uparam(2)  = nu*matparam%uparam(1)
      matparam%uparam(3)  = two*sqrt(r*h)
      matparam%uparam(4)  = two-matparam%uparam(3) 
      matparam%uparam(5)  = sqrt(r/h)
      matparam%uparam(6)  = one
      call calculp2(matparam%uparam(4),                                        &
                    matparam%uparam(3),                                        &
                    matparam%uparam(5),                                        &
                    matparam%uparam(6),m,r45)
      matparam%uparam(7)  = m  
      matparam%uparam(8)  = epsmax
      matparam%uparam(9)  = epsr1
      matparam%uparam(10) = epsr2  
      matparam%uparam(11) = fisokin
      matparam%uparam(12) = einf
      matparam%uparam(13) = ce
!
      !< Transform series of functions into material table
      x1scale = one 
      x2scale = one
      x2vect(1:nrate) = rate(1:nrate)
      fscale(1:nrate) = yfac(1:nrate)
      call func_table_copy(matparam%table(1) ,titr     ,mat_id   ,             &
               nrate    ,ifunc     ,x2vect   ,x1scale  ,x2scale  ,             &
               fscale   ,ntable    ,table    ,ierr2    )
!
      !< Tabulated Young modulus evolution
      if (opte > 0) then
        x1scale   = one 
        x2scale   = one
        x2vect(1) = zero
        fscale(1) = matparam%young
        call func_table_copy(matparam%table(2) ,titr     ,mat_id   ,           &
                  1        ,ifunce(1),x2vect(1),x1scale  ,x2scale  ,           &
                  fscale(1),ntable   ,table    ,ierr2    )
      endif 
!
      !< PARMAT table
      parmat(1)  = matparam%bulk
      parmat(2)  = matparam%young
      parmat(3)  = matparam%nu
      parmat(4)  = israte
      parmat(5)  = asrate
      parmat(16) = 2
      parmat(17) = two*matparam%shear/                                         &
                   (matparam%bulk+four_over_3*matparam%shear)
!
      !< Reference and initial density
      if (rhor == zero) rhor = rho0
      matparam%rho  = rhor
      matparam%rho0 = rho0
!
      !< Standard variables table size
      ! -> Equivalent stress
      mtag%g_seq = 1
      mtag%l_seq = 1
      ! -> Equivalent plastic strain 
      mtag%g_pla = 2
      mtag%l_pla = 2      
      ! -> Strain rate
      mtag%l_epsd = 1
      mtag%g_epsd = 1
      ! -> Back stresses for kinematic hardening
      mtag%l_sigb = 3
      ! -> Damage variable
      !   -> Number of output modes (stored in DMG(NEL,I), I>1)
      matparam%nmod  = 2
      ! Total number of damage outputs
      ! -> dmg(nel,1) = Global damage output 
      ! -> dmg(nel,2:nmod+1) = Damage modes output
      mtag%g_dmg = 1 + matparam%nmod
      mtag%l_dmg = 1 + matparam%nmod
      ! -> Modes allocation and definition
      allocate(matparam%mode(matparam%nmod))
      matparam%mode(1) = "Maximum plastic strain failure"
      matparam%mode(2) = "Tensile failure damage"
!
      !< Properties compatibility  
      call init_mat_keyword(matparam,"SHELL_ORTHOTROPIC")  
!
      !-------------------------------------------------------------------------
      !< - Parameters printing in output listing
      !-------------------------------------------------------------------------
      write(iout,1000) trim(titr),mat_id,ilaw
      write(iout,1001)
      if (is_encrypted) then                                     
        write(iout,'(5x,a,//)') 'CONFIDENTIAL DATA'
      else     
        write(iout,1200) rho0
        write(iout,1300) e,nu,matparam%shear
        write(iout,1400) r0,r45,r90,fisokin
        write(iout,1500) m,matparam%uparam(4),matparam%uparam(3),              &
                           matparam%uparam(5),matparam%uparam(6)
        write(iout,1700) 
        write(iout,1800) (ifunc(j),yfac(j),rate(j),j=1,nrate)
        write(iout,1900) epsmax,epsr1,epsr2,ifunce(1),einf,ce,israte,asrate,vp
      endif
!
 1000 format(/                                                                 &
       5X,A,/,                                                                 &
       5X,'MATERIAL NUMBER. . . . . . . . . . . . . . .=',I10/,                &
       5X,'MATERIAL LAW . . . . . . . . . . . . . . . .=',I10/)
 1001 format(/                                                                 &
       5X,'-----------------------------------------------------',/,           &
       5X,' MATERIAL MODEL: 3 PARAMETERS BARLAT 1989 TABULATED  ',/,           &
       5X,'-----------------------------------------------------',/)
 1200 format(/                                                                 &
       5X,'INITIAL DENSITY  . . . . . . . . . . . . . .=',1PG20.13/)  
 1300 format(/                                                                 &
       5X,'ELASTIC PARAMETERS:                          ',/,                   &
       5X,'-------------------                          ',/,                   &
       5X,'YOUNG MODULUS . . . . . . . . . . . . . . . =',1PG20.13/            &
       5X,'POISSON RATIO . . . . . . . . . . . . . . . =',1PG20.13/            &
       5X,'SHEAR MODULUS . . . . . . . . . . . . . . . =',1PG20.13/)
 1400 format(/                                                                 &
       5X,'PLASTIC PARAMETERS:                          ',/,                   &
       5X,'-------------------                          ',/,                   &
       5X,'LANKFORD COEFFICIENT R00 . . . . . . . . . .=',1PG20.13/            &
       5X,'LANKFORD COEFFICIENT R45 . . . . . . . . . .=',1PG20.13/            &
       5X,'LANKFORD COEFFICIENT R90 . . . . . . . . . .=',1PG20.13/            &
       5X,'ISO-KINEMATIC HARDENNING FACTOR. . . . . . .=',1PG20.13/)
 1500 format(/                                                                 &
       5X,'BARLAT 1989 YIELD CRITERION PARAMETERS:      ',/,                   &
       5X,'---------------------------------------      ',/,                   &
       5X,'BARLAT YIELD EXPONENT M . . . . . . . . . . =',1PG20.13/            & 
       5X,'BARLAT COEFFICIENT A . . . . . . . . . . . .=',1PG20.13/            &
       5X,'BARLAT COEFFICIENT C . . . . . . . . . . . .=',1PG20.13/            &
       5X,'BARLAT COEFFICIENT H . . . . . . . . . . . .=',1PG20.13/            &
       5X,'BARLAT COEFFICIENT P . . . . . . . . . . . .=',1PG20.13/)
 1700 format(/                                                                 &
       5X,'TABULATED YIELD STRESS PARAMETERS:           ',/,                   &
       5X,'----------------------------------           ',/)
 1800 format(                                                                  &
       5X,'YIELD STRESS FUNCTION NUMBER . . . . . . . .=',I10/                 & 
       5X,'YIELD SCALE FACTOR . . . . . . . . . . . . .=',1PG20.13/            &
       5X,'STRAIN RATE. . . . . . . . . . . . . . . . .=',1PG20.13)
 1900 format(/                                                                 &
       5X,'OTHER PARAMETERS:                           ',/,                    &
       5X,'-----------------                           ',/,                    &
       5X,'MAXIMUM PLASTIC STRAIN. . . . . . . . . . .=',1PG20.13/             &
       5X,'TENSILE FAILURE STRAIN 1. . . . . . . . . .=',1PG20.13/             &
       5X,'TENSILE FAILURE STRAIN 2. . . . . . . . . .=',1PG20.13/             & 
       5X,'YOUNG MODULUS SCALE FACTOR FUNCTION . . . .=',I10/                  &
       5X,'YOUNG MODULUS SATURATION VALUE EINF . . . .=',1PG20.13/             &
       5X,'YOUNG MODULUS EVOLUTION RATE CE . . . . . .=',1PG20.13/             &
       5X,'STRAIN RATE FILTERING FLAG. . . . . . . . .=',I10/                  &
       5X,'STRAIN RATE CUTTING FREQUENCY . . . . . . .=',1PG20.13/             &
       5X,'STRAIN RATE FORMULATION FLAG. . . . . . . .=',I10/                  &
       5X,'     = 0: TOTAL EQUIVALENT STRAIN RATE      ',/                     &
       5X,'     = 1: PLASTIC STRAIN RATE               '/)            
!
     end subroutine hm_read_mat57
   end module hm_read_mat57_mod







