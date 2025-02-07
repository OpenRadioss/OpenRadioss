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
      !||    hm_read_mat87_mod   ../starter/source/materials/mat/mat087/hm_read_mat87.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat         ../starter/source/materials/mat/hm_read_mat.F90
      !||====================================================================
      module hm_read_mat87_mod
      contains
! ======================================================================================================================
! \brief Reading material parameters of /MAT/LAW87
! \details Reading material parameters of /MAT/LAW87
! ======================================================================================================================
      !||====================================================================
      !||    hm_read_mat87                  ../starter/source/materials/mat/mat087/hm_read_mat87.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat                    ../starter/source/materials/mat/hm_read_mat.F90
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                         ../starter/source/output/message/message.F
      !||    func_table_copy                ../starter/source/materials/tools/func_table_copy.F90
      !||    hm_get_float_array_index       ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
      !||    hm_get_float_array_index_dim   ../starter/source/devtools/hm_reader/hm_get_float_array_index_dim.F
      !||    hm_get_floatv                  ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_floatv_dim              ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
      !||    hm_get_int_array_index         ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
      !||    hm_get_intv                    ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted         ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||    init_mat_keyword               ../starter/source/materials/mat/init_mat_keyword.F
      !||    mat_table_copy                 ../starter/source/materials/tools/mat_table_copy.F90
      !||--- uses       -----------------------------------------------------
      !||    elbuftag_mod                   ../starter/share/modules1/elbuftag_mod.F
      !||    func_table_copy_mod            ../starter/source/materials/tools/func_table_copy.F90
      !||    hm_option_read_mod             ../starter/share/modules1/hm_option_read_mod.F
      !||    mat_table_copy_mod             ../starter/source/materials/tools/mat_table_copy.F90
      !||    message_mod                    ../starter/share/message_module/message_mod.F
      !||    submodel_mod                   ../starter/share/modules1/submodel_mod.F
      !||    table_mod                      ../starter/share/modules1/table_mod.F
      !||====================================================================
      subroutine hm_read_mat87(                                                &
        &matparam ,nuvar    ,parmat   ,mat_id   ,titr     ,                    &
        &unitab   ,lsubmodel,mtag     ,iout     ,nvartmp  ,                    &
        &israte   ,ntable   ,table    ,maxfunc  )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
        use unitab_mod
        use elbuftag_mod            
        use message_mod      
        use submodel_mod
        use matparam_def_mod          
        use hm_option_read_mod
        use constant_mod     
        use table_mod 
        use func_table_copy_mod
        use mat_table_copy_mod
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
        implicit none 
#include  "my_real.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
        type(matparam_struct_),intent(inout)              :: matparam  !< Material parameters structure
        integer, intent(inout)                            :: nuvar     !< Number of user variables
        my_real, dimension(100),intent(inout)             :: parmat    !< Material parameter local reading table
        integer, intent(in)                               :: mat_id    !< Material identifier
        character(len=nchartitle) ,intent(in)             :: titr      !< Material title
        type(unit_type_),intent(in)                       :: unitab    !< Units table
        type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel !< Submodel data structure
        type(mlaw_tag_),intent(inout)                     :: mtag      !< Material law tag
        integer, intent(in)                               :: iout      !< Output file number
        integer, intent(inout)                            :: nvartmp   !< Number of temporary variables
        integer, intent(inout)                            :: israte    !< Strain rate filtering flag
        integer, intent(in)                               :: ntable    !< Number of tables
        type(ttable), dimension(ntable), intent(in)       :: table     !< Tables
        integer, intent(in)                               :: maxfunc   !< Maximum number of material functions
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
        integer :: i,j,nbmat,iflagsr,iflag,flag_fit,ilaw,nrate,offset,         &
          tab_id0,ismooth,ierr2,ifunc(maxfunc),itable(3),ikin
        my_real :: e,nu,g,bulk,fcut,al1,al2,al3,al4,al5,al6,al7,al8,           &
          fisokin,expn,invp,invc,unspt,unsct,aswift,epso,qvoce,beta,           &
          ko,alpha,nexp,unsp,unsc,rho0,rhor,rate(maxfunc),yfac(maxfunc),       &
          yfac_unit(maxfunc),k1,k2,ahs,bhs,mhs,eps0hs,nhs,hmart,temp0,         &
          tref,eta,cp,am,bm,cm,dm,ppm,qm,e0mart,vm0,ckh(4),akh(4),fscale0,     &
          fscale45,fscale90,epsd0,epsd45,epsd90,expa,x2vect(maxfunc),          &
          x3vect(maxfunc),x4vect(maxfunc),fscale(maxfunc),x1scale,x2scale,     &
          x3scale,x4scale,yld0
        logical :: is_available,is_encrypted
!-----------------------------------------------
!   S o u r c e   L i n e s
!-----------------------------------------------
        is_encrypted = .false.
        is_available = .false.
        ilaw = 87
!--------------------------------------------------
! Parameters initialisation
!--------------------------------------------------
        rate(1:maxfunc) = zero
        itable(1:3) = 0
        iflag   = 0
        iflagsr = 0
        invp    = zero
        invc    = zero
        unsp    = zero
        unsc    = zero
        unspt   = zero
        unsct   = zero
        nrate   = 0
        israte  = 0
        fcut    = zero
        k1      = zero
        k2      = zero
!--------------------------------------------------
! Extract data (is option crypted)
!--------------------------------------------------
        call hm_option_is_encrypted(is_encrypted)
!-----------------------------------------------
        !< card1 - Density
        call hm_get_floatv('MAT_RHO'  ,rho0    ,is_available,lsubmodel,unitab)
        call hm_get_floatv('Refer_Rho',rhor    ,is_available,lsubmodel,unitab)
!
        !< card2 - Elastic properties and flags
        call hm_get_floatv('MAT_E'    ,e       ,is_available,lsubmodel,unitab)
        call hm_get_floatv('MAT_NU'   ,nu      ,is_available,lsubmodel,unitab)
        call hm_get_intv  ('MAT_Iflag',iflag   ,is_available,lsubmodel)
        call hm_get_intv  ('Vflag'    ,iflagsr ,is_available,lsubmodel)
        call hm_get_floatv('STRAIN1'  ,invc    ,is_available,lsubmodel,unitab)
        call hm_get_floatv('MAT_EXP1' ,invp    ,is_available,lsubmodel,unitab)
        call hm_get_intv  ('Ifit'     ,flag_fit,is_available,lsubmodel)
!
        !< card3/4 - Fitting Parameters
        if (flag_fit == 1) then
          !< Yield stresses
          call hm_get_floatv('Sigma_00',al1    ,is_available,lsubmodel,unitab)
          call hm_get_floatv('Sigma_45',al2    ,is_available,lsubmodel,unitab)
          call hm_get_floatv('Sigma_90',al3    ,is_available,lsubmodel,unitab)
          call hm_get_floatv('Sigma_b' ,al4    ,is_available,lsubmodel,unitab)
          !< Lankford coefficients
          call hm_get_floatv('r_00'    ,al5    ,is_available,lsubmodel,unitab)
          call hm_get_floatv('r_45'    ,al6    ,is_available,lsubmodel,unitab)
          call hm_get_floatv('r_90'    ,al7    ,is_available,lsubmodel,unitab)
          call hm_get_floatv('r_b'     ,al8    ,is_available,lsubmodel,unitab)
        !< card3/4 - Barlat parameters
        else
          call hm_get_floatv('MAT_ALPHA2',al2  ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_ALPHA1',al1  ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_ALPHA3',al3  ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_ALPHA4',al4  ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_ALPHA5',al5  ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_ALPHA6',al6  ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_ALPHA7',al7  ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_ALPHA8',al8  ,is_available,lsubmodel,unitab)
        endif
!
        !< card5 - Kinematic hardening and yield criterion exponent
        call hm_get_floatv('MAT_kin' ,fisokin  ,is_available,lsubmodel,unitab)  
        call hm_get_intv  ('MAT_IKIN',ikin     ,is_available,lsubmodel)
        ikin = max(ikin,1)
        ikin = min(ikin,2)
!    
        !< card6 - Yield stress type parameters
        !< - Tabulated
        if (iflag == 0) then
          !< Yield criterion exponent
          call hm_get_floatv('A'    ,expa      ,is_available,lsubmodel,unitab)   
          !< Strain rate filtering parameters
          call hm_get_floatv('Fcut'       ,fcut  ,is_available,lsubmodel,unitab)
          call hm_get_intv  ('MAT_FSMOOTH',israte,is_available,lsubmodel)
          !< Number of rate dependent functions
          call hm_get_intv('MAT_NRATE',nrate   ,is_available,lsubmodel)
          if(nrate == 0) then
            call ancmsg(msgid=1116,                                            &
                        msgtype=msgerror,                                      &
                        anmode=aninfo_blind_1,                                 &
                        i1=mat_id,                                             &
                        c1=titr)         
          endif
          !< Read the rate dependent functions identifiers, scale factors and strain rates
          call hm_get_int_array_index  ('FUN_LOAD'  ,ifunc(1),1,is_available,  &
            lsubmodel)
          if (ifunc(1) == 0) then
            call ancmsg(msgid=126,                                             &
                        msgtype=msgerror,                                      &
                        anmode=aninfo_blind_1,                                 &
                        i1=mat_id,                                             &
                        c1=titr,                                               &
                        i2=ifunc(1)) 
          endif
          call hm_get_float_array_index('SCALE_LOAD',yfac(1) ,1,is_available,  &
            lsubmodel,unitab)
          if (yfac(1) == zero) then
            call hm_get_float_array_index_dim('SCALE_LOAD',yfac_unit(1),1,     &
              is_available,lsubmodel,unitab) 
            yfac(1) = one*yfac_unit(1)
          endif
          call hm_get_float_array_index('STRAINRATE_LOAD',rate(1),1,           &
            is_available,lsubmodel,unitab)
          !< Strain rate dependency is active
          if (nrate > 1) then 
            do i = 2,nrate
              call hm_get_int_array_index  ('FUN_LOAD'  ,ifunc(i),i,           &
                is_available,lsubmodel)
              if (ifunc(i) == 0) then
                call ancmsg(msgid=126,                                         &
                            msgtype=msgerror,                                  &
                            anmode=aninfo_blind_1,                             &
                            i1=mat_id,                                         &
                            c1=titr,                                           &
                            i2=ifunc(i)) 
                exit
              endif
              call hm_get_float_array_index('SCALE_LOAD',yfac(i),i,            &
                is_available,lsubmodel,unitab)
              if (yfac(i) == zero) then
                call hm_get_float_array_index_dim('SCALE_LOAD',yfac_unit(i),i, &
                  is_available,lsubmodel,unitab) 
                yfac(i) = one*yfac_unit(i)
              endif
              call hm_get_float_array_index('STRAINRATE_LOAD',rate(i),i,       &
                is_available,lsubmodel,unitab)
              if (rate(i) < rate(i-1)) then
                call ancmsg(msgid=478,                                         &                  
                            msgtype=msgerror,                                  &
                            anmode=aninfo_blind_1,                             &
                            i1=mat_id,                                         &
                            c1=titr)  
                exit
              endif
            enddo
          endif
        !< - Swift-Voce
        elseif (iflag == 1) then
          !< Yield criterion exponent
          call hm_get_floatv('A'          ,expa  ,is_available,lsubmodel,unitab) 
          !< Strain rate filtering parameters     
          call hm_get_floatv('Fcut'       ,fcut  ,is_available,lsubmodel,unitab)
          call hm_get_intv  ('MAT_FSMOOTH',israte,is_available,lsubmodel)
          !< Yield stress parameters
          call hm_get_floatv('MAT_VOL'    ,alpha ,is_available,lsubmodel,unitab)
          call hm_get_floatv('FScale33'   ,nexp  ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_A'      ,aswift,is_available,lsubmodel,unitab)
          call hm_get_floatv('FScale22'   ,epso  ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_PR'     ,qvoce ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_T0'     ,beta  ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_NUt'    ,ko    ,is_available,lsubmodel,unitab)
        !< - Hansel parameters
        elseif (iflag == 2) then
          !< Yield criterion exponent
          call hm_get_floatv('A'         ,expa  ,is_available,lsubmodel,unitab) 
          !< Yield stress parameters
          call hm_get_floatv('MAT_AM'    ,am    ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_BM'    ,bm    ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_CM'    ,cm    ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_DM'    ,dm    ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_PM'    ,ppm   ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_QM'    ,qm    ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_E0MART',e0mart,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_VM0'   ,vm0   ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_AHS'   ,ahs   ,is_available,lsubmodel,unitab) 
          call hm_get_floatv('MAT_BHS'   ,bhs   ,is_available,lsubmodel,unitab) 
          call hm_get_floatv('MAT_MHS'   ,mhs   ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_EPS0HS',eps0hs,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_NHS'   ,nhs   ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_HMART' ,hmart ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_K1'    ,k1    ,is_available,lsubmodel,unitab)  
          call hm_get_floatv('MAT_K2'    ,k2    ,is_available,lsubmodel,unitab) 
          call hm_get_floatv('MAT_TEMP0' ,temp0 ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_TREF'  ,tref  ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_ETA'   ,eta   ,is_available,lsubmodel,unitab)
          call hm_get_floatv('MAT_CP'    ,cp    ,is_available,lsubmodel,unitab)
        !< - Orthotropic tabulated yield stress (may be strain rate dependent)
        elseif (iflag == 3) then
          !< Yield criterion exponent
          call hm_get_floatv('A'       ,expa  ,is_available,lsubmodel,unitab) 
          !< Strain rate filtering parameters     
          call hm_get_floatv('Fcut'    ,fcut  ,is_available,lsubmodel,unitab)
          call hm_get_intv  ('MAT_FSMOOTH',israte,is_available,lsubmodel)
          !< Tabulated yield stress in direction 0
          call hm_get_intv  ('TAB_ID0' ,itable(1),is_available,lsubmodel)
          call hm_get_floatv('FSCALE0' ,fscale0,is_available,lsubmodel,unitab)
          call hm_get_floatv('EPSD0'   ,epsd0  ,is_available,lsubmodel,unitab)
          !< Tabulated yield stress in direction 45
          call hm_get_intv  ('TAB_ID45',itable(2),is_available,lsubmodel)
          call hm_get_floatv('FSCALE45',fscale45,is_available,lsubmodel,unitab)
          call hm_get_floatv('EPSD45'  ,epsd45  ,is_available,lsubmodel,unitab)
          !< Tabulated yield stress in direction 90
          call hm_get_intv  ('TAB_ID90',itable(3),is_available,lsubmodel)
          call hm_get_floatv('FSCALE90',fscale90,is_available,lsubmodel,unitab)
          call hm_get_floatv('EPSD90'  ,epsd90  ,is_available,lsubmodel,unitab)
        endif
!
        !< card7 - Kinematic hardening parameters
        if ((ikin == 1).and.(fisokin >zero)) then 
          call hm_get_floatv('MAT_CRC1',ckh(1),is_available,lsubmodel,unitab)  
          call hm_get_floatv('MAT_CRA1',akh(1),is_available,lsubmodel,unitab)  
          call hm_get_floatv('MAT_CRC2',ckh(2),is_available,lsubmodel,unitab)  
          call hm_get_floatv('MAT_CRA2',akh(2),is_available,lsubmodel,unitab)  
          call hm_get_floatv('MAT_CRC3',ckh(3),is_available,lsubmodel,unitab)  
          call hm_get_floatv('MAT_CRA3',akh(3),is_available,lsubmodel,unitab)  
          call hm_get_floatv('MAT_CRC4',ckh(4),is_available,lsubmodel,unitab)  
          call hm_get_floatv('MAT_CRA4',akh(4),is_available,lsubmodel,unitab)  
        endif
!
!-------------------------------------------------------------
!     Check and default values
!-------------------------------------------------------------  
        !< Cowper-Symonds parameters
        if (invp == zero .or. invc == zero) then
          unsp  = zero
          unsc  = zero
        else
          unsp = one/invp
          unsc = one/invc  
        endif
        if (expa == zero) expa = two
        !< Bulk modulus
        bulk = e/three/(one-two*nu)
        !< Shear modulus
        g = half*e/(one+nu)
        !< Strain rate filtering
        if (iflagsr == 1) then
          israte = 1 
          if (fcut == zero) fcut = 10000.0d0*unitab%fac_t_work
        else 
          if (israte == 1 .and. fcut == zero) fcut = 10000.0d0*unitab%fac_t_work
        endif
        !< Swift-Voce initial yield stress
        if (iflag == 1) then 
          yld0 = (one-alpha)*ko 
          if (epso > zero) then 
            yld0 = yld0 + alpha*aswift*exp(nexp*log(epso))
          endif
        endif
        !< Hensel parameters
        if (iflag == 2)then
          if (k1==zero .or. k2==zero) then
            k1 = one
            k2 = zero
          endif
        endif
        !< Orthotropic yield stress parameters
        if (iflag == 3) then
          if (itable(1) == 0) then 
            call ancmsg(msgid=3084,                                            &                  
                        msgtype=msgerror,                                      &
                        anmode=aninfo_blind_1,                                 &
                        i1=mat_id,                                             &
                        c1=titr)  
          endif
          if (itable(2) == 0) itable(2) = itable(1)
          if (itable(3) == 0) itable(3) = itable(1)
          if (fscale0 == zero) then 
            call hm_get_floatv_dim('FSCALE0',fscale0,is_available,lsubmodel,   &
              unitab)
          endif
          if (fscale45 == zero) then 
            call hm_get_floatv_dim('FSCALE45',fscale45,is_available,lsubmodel, &
              unitab)
          endif
          if (fscale90 == zero) then
            call hm_get_floatv_dim('FSCALE90',fscale90,is_available,lsubmodel, &
              unitab)
          endif
          if (epsd0 == zero) then
            call hm_get_floatv_dim('EPSD0',epsd0,is_available,lsubmodel,unitab)
          endif
          if (epsd45 == zero) then
            call hm_get_floatv_dim('EPSD45',epsd45,is_available,lsubmodel,     &
              unitab)
          endif
          if (epsd90 == zero) then
            call hm_get_floatv_dim('EPSD90',epsd90,is_available,lsubmodel,     &
              unitab)
          endif
        endif
!
!-------------------------------------------------------------
!     Filling buffer tables
!-------------------------------------------------------------
        !< Number of functions/tables/temporary variables
        if (iflag == 0) then
          matparam%ntable = 1
          nvartmp = 2
        elseif (iflag == 3) then 
          matparam%ntable = 3
          nvartmp = 6
        else
          matparam%ntable = 0
          nvartmp = 0
        endif         
!
        !< Number of integer material parameters
        matparam%niparam = 4
        !< Number of real material parameters
        offset = 12
        if (iflag == 1) then
          offset = offset + 10
        elseif (iflag == 2) then
          offset = offset + 20
        endif
        matparam%nuparam = offset
        if (fisokin > zero) then
          matparam%nuparam = matparam%nuparam + 8
        endif
!
        !< Allocation of material parameters tables
        allocate (matparam%iparam(matparam%niparam))
        allocate (matparam%uparam(matparam%nuparam))
        allocate (matparam%table (matparam%ntable ))
!
        !< Number of user variables
        if (iflag == 2) then 
          nuvar = 1 !< Martensite volume fraction for Hansel
        else
          nuvar = 0
        endif 
!
        !< Material integer parameters
        matparam%iparam(1)  = iflag
        matparam%iparam(2)  = iflagsr
        matparam%iparam(3)  = flag_fit
        matparam%iparam(4)  = ikin
!
        !< Material real parameters
        if (rhor == zero) rhor = rho0
        matparam%rho        = rhor
        matparam%rho0       = rho0
        matparam%young      = e
        matparam%nu         = nu
        matparam%shear      = g
        matparam%bulk       = bulk
        matparam%uparam(1)  = e/(one - nu*nu)
        matparam%uparam(2)  = nu*matparam%uparam(1)
        matparam%uparam(3)  = al1
        matparam%uparam(4)  = al2     
        matparam%uparam(5)  = al3
        matparam%uparam(6)  = al4
        matparam%uparam(7)  = al5
        matparam%uparam(8)  = al6
        matparam%uparam(9)  = al7
        matparam%uparam(10) = al8
        matparam%uparam(11) = fisokin
        matparam%uparam(12) = expa
        !< Tabulated yield stress
        if (iflag == 0) then 
          !< Transform series of functions into material table
          x1scale = one 
          x2scale = one
          x2vect(1:nrate) = rate(1:nrate)
          fscale(1:nrate) = yfac(1:nrate)
          call func_table_copy(matparam%table(1) ,titr     ,mat_id   ,         &
                   nrate    ,ifunc     ,x2vect   ,x1scale  ,x2scale  ,         &
                   fscale   ,ntable    ,table    ,ierr2    )
        !< Swift-Voce yield stress
        elseif (iflag == 1) then 
          matparam%uparam(13) = aswift
          matparam%uparam(14) = nexp
          matparam%uparam(15) = alpha
          matparam%uparam(16) = epso
          matparam%uparam(17) = qvoce
          matparam%uparam(18) = beta
          matparam%uparam(19) = ko
          matparam%uparam(20) = unsp
          matparam%uparam(21) = unsc
          matparam%uparam(22) = yld0
        !< Hansel yield stress
        elseif (iflag == 2) then
          matparam%uparam(13) = k1
          matparam%uparam(14) = k2
          matparam%uparam(15) = ahs
          matparam%uparam(16) = bhs
          matparam%uparam(17) = mhs
          matparam%uparam(18) = eps0hs
          matparam%uparam(19) = nhs
          matparam%uparam(20) = hmart
          matparam%uparam(21) = temp0
          matparam%uparam(22) = tref
          matparam%uparam(23) = eta
          matparam%uparam(24) = cp
          matparam%uparam(25) = am
          matparam%uparam(26) = bm
          matparam%uparam(27) = cm
          matparam%uparam(28) = dm
          matparam%uparam(29) = ppm
          matparam%uparam(30) = qm
          matparam%uparam(31) = e0mart
          matparam%uparam(32) = vm0
        !< Orthotropic 3 directions yield stress
        elseif (iflag == 3) then
          !< Transform global table into material table
          matparam%table(1)%notable = itable(1)
          matparam%table(2)%notable = itable(2)
          matparam%table(3)%notable = itable(3)
          x1scale   = one
          x2scale   = one
          x3scale   = one
          x4scale   = one
          x2vect(1) = epsd0
          x2vect(2) = epsd45
          x2vect(3) = epsd90
          x3vect(1:3) = zero
          x4vect(1:3) = zero
          fscale(1) = fscale0
          fscale(2) = fscale45
          fscale(3) = fscale90
          call mat_table_copy(matparam ,x2vect   ,x3vect   ,x4vect   ,         &
                    x1scale  ,x2scale  ,x3scale  ,x4scale  ,fscale   ,         & 
                    ntable   ,table    ,ierr     )
        endif
        !< Kinematic hardening parameters
        if ((ikin == 1).and.(fisokin > zero)) then       
          matparam%uparam(offset + 1) = ckh(1)
          matparam%uparam(offset + 2) = akh(1)
          matparam%uparam(offset + 3) = ckh(2)
          matparam%uparam(offset + 4) = akh(2)
          matparam%uparam(offset + 5) = ckh(3)
          matparam%uparam(offset + 6) = akh(3)
          matparam%uparam(offset + 7) = ckh(4)
          matparam%uparam(offset + 8) = akh(4)
        endif
!
!-------------------------------------------------------------
!     Filling PARMAT tables
!-------------------------------------------------------------
        parmat(1)  = bulk
        parmat(2)  = e
        parmat(3)  = nu
        parmat(4)  = israte
        parmat(5)  = fcut
        parmat(16) = 2
        parmat(17) = (one - two*nu)/(one - nu)
!
!-------------------------------------------------------------
!     Size of standardized variables
!-------------------------------------------------------------
        mtag%g_pla  = 1
        mtag%l_pla  = 1
        mtag%g_seq  = 1
        mtag%l_seq  = 1
        mtag%g_epsd = 1
        mtag%l_epsd = 1
        !< Temperature needed in case of Hansel model
        if (iflag == 2) then 
          mtag%l_temp = 1
          mtag%g_temp = 1
        endif
        !< Backstresses needed in case of kinematic hardening
        if ((ikin == 1).and.(fisokin > zero)) then 
          mtag%l_sigb = 12
        elseif ((ikin == 2).and.(fisokin > zero)) then
          mtag%l_sigb = 3
        endif  
!-------------------------------------------------------------
        ! MATPARAM keywords
        call init_mat_keyword(matparam,"ORTHOTROPIC")
        ! Properties compatibility
        call init_mat_keyword(matparam,"SHELL_ORTHOTROPIC")
!-------------------------------------------------------------
!
!-------------------------------------------------------------
!     Printing out the material data
!-------------------------------------------------------------
        write(iout,1001) trim(titr),mat_id,ilaw
        write(iout,1000)
        if (is_encrypted)then                                     
          write(iout,'(5X,A,//)') 'CONFIDENTIAL DATA'
        else     
          write(iout,1002) rho0
          write(iout,1003) e,nu
          write(iout,1004) iflag,iflagsr,israte,fcut,fisokin,ikin
          if (flag_fit == 0) then
            write(iout,1005) expa,al1,al2,al3,al4,al5,al6,al7,al8
          else
            write(iout,1006) expa,al1,al2,al3,al4,al5,al6,al7,al8
          endif
          if (iflag == 0) then
            write(iout,1007) nrate
            if (nrate>0) write(iout,1008)(ifunc(i),yfac(i),rate(i),i=1,nrate)
          elseif (iflag == 1) then
            write(iout,1009) invp,invc,qvoce,beta,ko,alpha,aswift,nexp,epso
          elseif (iflag == 2) then
            write(iout,1010) k1,k2,ahs,bhs,mhs,eps0hs,nhs,hmart,temp0,         &
              tref,eta,cp,am,bm,cm,dm,ppm,qm,e0mart,vm0 
          elseif (iflag == 3) then
            write(iout,1011) itable(1),fscale0,epsd0,itable(2),fscale45,       &
              epsd45,itable(3),fscale90,epsd90
          endif
          if ((ikin==1).and.(fisokin > zero)) then
            write(iout,1012)(ckh(i), akh(i),i=1,4)
          endif
        endif
!
 1000 format(/                                                                 &                                                                                            
       5X,'-------------------------------------------------------',/          &     
       5X,'           MATERIAL MODEL: BARLAT YLD2000              ',/,         & 
       5X,'-------------------------------------------------------',/)
 1001 format(/                                                                 &
       5X,A,/,                                                                 &
       5X,'MATERIAL NUMBER. . . . . . . . . . . . . . . . . . . .=',I10/,      &
       5X,'MATERIAL LAW . . . . . . . . . . . . . . . . . . . . .=',I10/)
 1002 format(/                                                                 &
       5X,'INITIAL DENSITY. . . . . . . . . . . . . . . . . . . .=',1PG20.13/)  
 1003 format(/                                                                 &
       5X,'YOUNG''S MODULUS. . . . . . . . . . . . . . . . . . . .=',1PG20.13/ &
       5X,'POISSON''S RATIO. . . . . . . . . . . . . . . . . . . .=',1PG20.13/)
 1004 format(/                                                                 &
       5X,'YIELD STRESS FORMULATION FLAG: . . . . . . . . . . . .=',I10/       &
       5X,'    = 0: TABULATED YIELD STRESS                        ',/          &
       5X,'    = 1: SWIFT-VOCE                                    ',/          &
       5X,'    = 2: HANSEL                                        ',/          &
       5X,'    = 3: ORTHOTROPIC 3-DIR TABULATED                   ',/          &
       5X,'STRAIN RATE FLAG:. . . . . . . . . . . . . . . . . . .=',I10/       & 
       5X,'    = 0: TOTAL STRAIN RATE                             ',/          &
       5X,'    = 1: PLASTIC STRAIN RATE                           ',/          &
       5X,'STRAIN RATE FILTERING FLAG . . . . . . . . . . . . . .=',I10/       &
       5X,'STRAIN RATE CUTTING FREQUENCY. . . . . . . . . . . . .=',1PG20.13/  &
       5X,'ISOTROPIC/KINEMATIC HARDENING PARAMETER (CHARD). . . .=',1PG20.13/  &
       5X,'KINEMATIC HARDENING FORMULATION FLAG:. . . . . . . . .=',I10/       & 
       5X,'    = 1: CHABOCHE-ROUSSELIER (DEFAULT)                 ',/          &
       5X,'    = 2: PRAGER-ZIEGLER                                ',/)
 1005 format(/                                                                 &
       5X,'BARLAT 2000 YIELD CRITERION PARAMETERS:                ',/,         &
       5X,'---------------------------------------                ',/,         &
       5X,'EXPONENT OF YIELD CRITERION A. . . . . . . . . . . . .=',1PG20.13/  &
       5X,'ANISOTROPY COEFFICIENT ALPHA1. . . . . . . . . . . . .=',1PG20.13/  &
       5X,'ANISOTROPY COEFFICIENT ALPHA2. . . . . . . . . . . . .=',1PG20.13/  &
       5X,'ANISOTROPY COEFFICIENT ALPHA3. . . . . . . . . . . . .=',1PG20.13/  &
       5X,'ANISOTROPY COEFFICIENT ALPHA4. . . . . . . . . . . . .=',1PG20.13/  &
       5X,'ANISOTROPY COEFFICIENT ALPHA5. . . . . . . . . . . . .=',1PG20.13/  &
       5X,'ANISOTROPY COEFFICIENT ALPHA6. . . . . . . . . . . . .=',1PG20.13/  &
       5X,'ANISOTROPY COEFFICIENT ALPHA7. . . . . . . . . . . . .=',1PG20.13/  &
       5X,'ANISOTROPY COEFFICIENT ALPHA8. . . . . . . . . . . . .=',1PG20.13/)
 1006 format(/                                                                 &
       5X,'EXPERIMENTAL DATA USED TO FIT BARLAT 2000:             ',/,         &
       5X,'------------------------------------------             ',/,         &
       5X,'EXPONENT OF YIELD CRITERION A. . . . . . . . . . . . .=',1PG20.13/  & 
       5X,'YIELD STRESS IN DIRECTION 00 . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'YIELD STRESS IN DIRECTION 45 . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'YIELD STRESS IN DIRECTION 90 . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'YIELD STRESS FOR BIAXIAL . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'R-VALUE IN DIRECTION 00. . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'R-VALUE IN DIRECTION 45. . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'R-VALUE IN DIRECTION 90. . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'R-VALUE FOR BIAXIAL. . . . . . . . . . . . . . . . . .=',1PG20.13/)
 1007 format(/                                                                 &
       5X,'TABULATED YIELD STRESS PARAMETERS:                     ',/,         &
       5X,'----------------------------------                     ',/,         &
       5X,'NUMBER OF YIELD STRESS FUNCTIONS . . . . . . . . . . .=',I10/)
 1008 format(/                                                                 &
       5X,'YIELD STRESS FUNCTION NUMBER . . . . . . . . . . . . .=',I10/       &
       5X,'YIELD STRESS SCALE FACTOR. . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'REFERENCE STRAIN RATE. . . . . . . . . . . . . . . . .=',1PG20.13/)     
 1009 format(/                                                                 &
       5X,'SWIFT-VOCE YIELD STRESS PARAMETERS:                    ',/,         &
       5X,'-----------------------------------                    ',/,         &
       5X,'COWPER SEYMONDS EXPONENT P . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'COWPER SEYMONDS COEFFICIENT C. . . . . . . . . . . . .=',1PG20.13/  &
       5X,'YIELD VOCE PARAMETER Q . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'YIELD VOCE PARAMETER B . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'YIELD VOCE PARAMETER K0. . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'YIELD GLOBAL PARAMETER ALPHA . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'YIELD SWIFT PARAMETER A. . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'YIELD SWIFT EXPONENT N . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'YIELD SWIFT REFERENCE STRAIN . . . . . . . . . . . . .=',1PG20.13/)
 1010 format(/                                                                 &
       5X,'HANSEL YIELD STRESS PARAMETERS:                        ',/,         &
       5X,'-------------------------------                        ',/,         &
       5X,'TEMPERATURE DEPENDENCY PARAMETER K1. . . . . . . . . .=',1PG20.13/  &
       5X,'TEMPERATURE DEPENDENCY PARAMETER K2. . . . . . . . . .=',1PG20.13/  &
       5X,'PARAMETER AHS . . . . . . . .. . . . . . . . . . . . .=',1PG20.13/  &
       5X,'PARAMETER BHS. . . . . . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'FACTOR M . . . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'PARAMETER EPSH0. . . . . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'EXPONENT N LAW NHS . . . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'FACTOR FOR MARTENSITE DH . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'INITIAL TEMPERATURE. . . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'REFERENCE TEMPERATURE. . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'TAYLOR QUINEY COEFFICIENT ETA. . . . . . . . . . . . .=',1PG20.13/  &
       5X,'SPECIFIC HEAT FOR TEMPERATURE CALCULATION. . . . . . .=',1PG20.13/  & 
       5X,'PARAMETER AM FOR MARTENSITE RATE EQUATION. . . . . . .=',1PG20.13/  &
       5X,'PARAMETER BM FOR MARTENSITE RATE EQUATION. . . . . . .=',1PG20.13/  &
       5X,'PARAMETER CM FOR MARTENSITE RATE EQUATION. . . . . . .=',1PG20.13/  &
       5X,'PARAMETER DM FOR MARTENSITE RATE EQUATION. . . . . . .=',1PG20.13/  &
       5X,'PARAMETER PM FOR MARTENSITE RATE EQUATION. . . . . . .=',1PG20.13/  &
       5X,'PARAMETER QM FOR MARTENSITE RATE EQUATION. . . . . . .=',1PG20.13/  &
       5X,'MARTENSITE STARTING VALUE E0MART . . . . . . . . . . .=',1PG20.13/  &   
       5X,'INITIAL FRACTION OF MARTENSITE VM0 . . . . . . . . . .=',1PG20.13/)
 1011 format(/                                                                 &
       5X,'3 DIR. ORHOTROPIC TABULATED YIELD STRESS PARAMETERS:   ',/,         &
       5X,'----------------------------------------------------   ',/,         &
       5X,'DIRECTION  0 YIELD STRESS TABLE NUMBER . . . . . . . .=',I10/       &
       5X,'DIRECTION  0 YIELD STRESS SCALE FACTOR . . . . . . . .=',1PG20.13/  &
       5X,'DIRECTION  0 YIELD STRESS REFERENCE STRAIN RATE. . . .=',1PG20.13/  &
       5X,'                                                       ',/,         &
       5X,'DIRECTION 45 YIELD STRESS TABLE NUMBER . . . . . . . .=',I10/       &
       5X,'DIRECTION 45 YIELD STRESS SCALE FACTOR . . . . . . . .=',1PG20.13/  &
       5X,'DIRECTION 45 YIELD STRESS REFERENCE STRAIN RATE. . . .=',1PG20.13/  &
       5X,'                                                       ',/,         &
       5X,'DIRECTION 90 YIELD STRESS TABLE NUMBER . . . . . . . .=',I10/       &
       5X,'DIRECTION 90 YIELD STRESS SCALE FACTOR . . . . . . . .=',1PG20.13/  &
       5X,'DIRECTION 90 YIELD STRESS REFERENCE STRAIN RATE. . . .=',1PG20.13/)
 1012 format(/                                                                 &
       5X,'CHABOCHE-ROUSSELIER KINEMATIC HARDENING PARAMETERS:    ',/,         &
       5X,'---------------------------------------------------    ',/,         &
       5X,'PARAMETER C1 . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'PARAMETER A1 . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'PARAMETER C2 . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'PARAMETER A2 . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'PARAMETER C3 . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'PARAMETER A3 . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'PARAMETER C4 . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'PARAMETER A4 . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/)
      
      end subroutine hm_read_mat87
!=======================================================================
      end module hm_read_mat87_mod

