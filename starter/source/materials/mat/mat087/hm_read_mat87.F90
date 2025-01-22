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
      !||    hm_get_float_array_index       ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
      !||    hm_get_float_array_index_dim   ../starter/source/devtools/hm_reader/hm_get_float_array_index_dim.F
      !||    hm_get_floatv                  ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_floatv_dim              ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
      !||    hm_get_int_array_index         ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
      !||    hm_get_intv                    ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted         ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||    init_mat_keyword               ../starter/source/materials/mat/init_mat_keyword.F
      !||--- uses       -----------------------------------------------------
      !||    elbuftag_mod                   ../starter/share/modules1/elbuftag_mod.F
      !||    hm_option_read_mod             ../starter/share/modules1/hm_option_read_mod.F
      !||    message_mod                    ../starter/share/message_module/message_mod.F
      !||    submodel_mod                   ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      subroutine hm_read_mat87(                                                &                            
        israte   ,nuvar    ,nfunc    ,maxfunc  ,ifunc    ,mtag     ,           &
        parmat   ,unitab   ,npropm   ,pm       ,lsubmodel,id       ,           &
        titr     ,matparam ,numtabl  ,maxtabl  ,itable   ,nvartmp  ,           &
        iout     )                    
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
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
        implicit none 
#include  "my_real.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
        integer, intent(inout)                            :: israte    !< strain rate filtering flag
        integer, intent(inout)                            :: nuvar     !< number of user variables
        integer, intent(inout)                            :: nfunc     !< number of material functions
        integer, intent(in)                               :: maxfunc   !< maximum number of material functions
        integer, dimension(maxfunc) ,intent(inout)        :: ifunc     !< material functions identifiers
        type(mlaw_tag_),intent(inout)                     :: mtag      !< material law tag
        my_real, dimension(100)    ,intent(inout)         :: parmat    !< material parameter global table 1
        type(unit_type_),intent(in)                       :: unitab    !< units table
        integer, intent(in)                               :: npropm    !< number of material parameters
        my_real, dimension(npropm) ,intent(inout)         :: pm        !< material parameter global table 2
        type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel !< submodel data structure
        integer, intent(in)                               :: id        !< material identifier
        character(len=nchartitle) ,intent(in)             :: titr      !< material title
        type(matparam_struct_),intent(inout)              :: matparam  !< material parameters structure
        integer, intent(inout)                            :: numtabl   !< number of tables
        integer, intent(in)                               :: maxtabl   !< maximum number of tables
        integer, dimension(maxtabl), intent(inout)        :: itable    !< table identifiers
        integer, intent(inout)                            :: nvartmp   !< number of temporary variables
        integer, intent(in)                               :: iout      !< output file number
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
        integer :: i,j,nbmat, mat_id,iflagsr,iflag,flag_fit,                   &
          ilaw,nrate,iyield,offset,tab_id0,ismooth
        my_real :: e,nu,g,bulk,fcut,al1,al2,al3,al4,al5,al6,al7,al8,           &
          fisokin,expn,invp,invc,unspt,unsct,aswift,epso,qvoce,beta,           &
          ko,alpha,nexp,unsp,unsc,rho0,rhor,rate(maxfunc),yfac(maxfunc),       &
          yfac_unit(maxfunc),k1,k2,ahs,bhs,mhs,eps0hs,nhs,hmart,temp0,         &
          tref,eta,cp,am,bm,cm,dm,ppm,qm,e0mart,vm0,ckh(4),akh(4),             &
          fscale0,fscale45,fscale90,epsd0,epsd45,epsd90,expa 
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
        nfunc   = 0
        numtabl = 0
        nvartmp = 0
        ismooth = 0
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
          !< yield stresses
          call hm_get_floatv('Sigma_00',al1    ,is_available,lsubmodel,unitab)
          call hm_get_floatv('Sigma_45',al2    ,is_available,lsubmodel,unitab)
          call hm_get_floatv('Sigma_90',al3    ,is_available,lsubmodel,unitab)
          call hm_get_floatv('Sigma_b' ,al4    ,is_available,lsubmodel,unitab)
          !< lankford coefficients
          call hm_get_floatv('r_00'    ,al5    ,is_available,lsubmodel,unitab)
          call hm_get_floatv('r_45'    ,al6    ,is_available,lsubmodel,unitab)
          call hm_get_floatv('r_90'    ,al7    ,is_available,lsubmodel,unitab)
          call hm_get_floatv('r_b'     ,al8    ,is_available,lsubmodel,unitab)
        !< card3/4 - barlat parameters
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
        call hm_get_floatv('MAT_kin',fisokin   ,is_available,lsubmodel,unitab)  
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
                        i1=id,                                                 &
                        c1=titr)         
          endif
          !< Read the rate dependent functions identifiers, scale factors and strain rates
          call hm_get_int_array_index  ('FUN_LOAD'  ,ifunc(1),1,is_available,  &
            lsubmodel)
          if (ifunc(1) == 0) then
            call ancmsg(msgid=126,                                             &
                        msgtype=msgerror,                                      &
                        anmode=aninfo_blind_1,                                 &
                        i1=id,                                                 &
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
                            i1=id,                                             &
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
                            i1=id,                                             &
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
          call hm_get_intv  ('MAT_ISMOOTH',ismooth,is_available,lsubmodel)
          !< Strain rate interpolation flag
          if (ismooth == 0) ismooth = 1
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
        if (fisokin >zero) then
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
          unspt = zero
          unsct = zero
        elseif (iflagsr == 0) then
          unspt = one/invp
          unsct = one/invc  
        elseif (iflagsr == 1) then
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
            write(*,*) "error: mat_orthotropic: tabid0 is not defined"
            stop
          endif
          if (itable(2) == 0) then 
            write(*,*) "error: mat_orthotropic: tabid45 is not defined"
            stop
          endif
          if (itable(3) == 0) then
            write(*,*) "error: mat_orthotropic: tabid90 is not defined"
            stop
          endif
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
        !< Yield stress type
        if (iflag == 1) then
          iyield = 1 ! Swift-Voce
        elseif (iflag == 2) THEN
          iyield = 2 ! Hansel
        elseif (iflag == 3) THEN
          iyield = 5 ! Tabulated orthotropic
        else
          if(iflagsr == 1) then
            iyield = 3 ! Tabulated + Plas Strain rate 
          else
            iyield = 4 ! Tabulated + Strain rate 
          endif
        endif
!
!-------------------------------------------------------------
!     Filling buffer tables
!-------------------------------------------------------------
        !< Number of functions/tables/temporary variables
        if (iflag == 0) then
          if (nrate == 1) then
            nrate    = 2
            ifunc(2) = ifunc(1)
            rate(1)  = zero
            rate(2)  = one
            yfac(2)  = yfac(1)
          elseif (rate(1) == zero) then
          else
            nrate = nrate+1
            do j = nrate,1,-1
              ifunc(j+1) = ifunc(j)
              rate(j+1)  = rate(j)
              yfac(j+1)  = yfac(j)
            enddo
            rate(1) = zero
          endif  
          nfunc = nrate
        elseif (iflag == 3) then 
          numtabl = 3
          nvartmp = 6
        endif      
!
        !< Number of integer material parameters
        matparam%niparam = 7
        !< Number of real material parameters
        offset = 17
        if (iflag == 0) then 
          offset = offset + 2*nfunc
        elseif (iflag == 1) then
          offset = offset + 7
        elseif (iflag == 2) then
          offset = offset + 20
        elseif (iflag == 3) then
          offset = offset + 6
        endif
        matparam%nuparam = offset
        if (fisokin > zero) then
          matparam%nuparam = matparam%nuparam + 8
        endif
!
        !< Allocation of material parameters tables
        allocate (matparam%iparam(matparam%niparam))
        allocate (matparam%uparam(matparam%nuparam))
!
        !< Number of user variables
        nuvar = 5 + nfunc + 3 + 2 + 10
!
        !< Material integer parameters
        matparam%iparam(1)  = israte
        matparam%iparam(2)  = iyield
        matparam%iparam(3)  = iflagsr
        matparam%iparam(4)  = flag_fit
        matparam%iparam(5)  = nrate
        matparam%iparam(6)  = iflag
        matparam%iparam(7)  = ismooth
!
        !< Material real parameters
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
        matparam%uparam(13) = fcut
        matparam%uparam(14) = unsp
        matparam%uparam(15) = unsc
        matparam%uparam(16) = unspt
        matparam%uparam(17) = unsct
        if (iflag == 0) then 
          do i = 1,nfunc
            matparam%uparam(17 + i) = rate(i)
            matparam%uparam(17 + i + nfunc) = yfac(i)
          enddo
        elseif (iflag == 1) then 
          matparam%uparam(18) = aswift
          matparam%uparam(19) = nexp
          matparam%uparam(20) = alpha
          matparam%uparam(21) = epso
          matparam%uparam(22) = qvoce
          matparam%uparam(23) = beta
          matparam%uparam(24) = ko
        elseif (iflag == 2) then
          matparam%uparam(18) = k1
          matparam%uparam(19) = k2
          matparam%uparam(20) = ahs
          matparam%uparam(21) = bhs
          matparam%uparam(22) = mhs
          matparam%uparam(23) = eps0hs
          matparam%uparam(24) = nhs
          matparam%uparam(25) = hmart
          matparam%uparam(26) = temp0
          matparam%uparam(27) = tref
          matparam%uparam(28) = eta
          matparam%uparam(29) = cp
          matparam%uparam(30) = am
          matparam%uparam(31) = bm
          matparam%uparam(32) = cm
          matparam%uparam(33) = dm
          matparam%uparam(34) = ppm
          matparam%uparam(35) = qm
          matparam%uparam(36) = e0mart
          matparam%uparam(37) = vm0
        elseif (iflag == 3) then
          matparam%uparam(18) = fscale0
          matparam%uparam(19) = fscale45
          matparam%uparam(20) = fscale90
          matparam%uparam(21) = epsd0
          matparam%uparam(22) = epsd45
          matparam%uparam(23) = epsd90
        endif
        if (fisokin > zero) then       
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
!     Filling PM tables
!-------------------------------------------------------------
        if (rhor == zero) rhor = rho0
        pm(1)  = rhor
        pm(89) = rho0 
        pm(9)  = two*pi*fcut   
        matparam%rho  = rhor
        matparam%rho0 = rho0
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
        if (fisokin > zero) mtag%l_sigb = 12
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
        write(iout,1001) trim(titr),id,ilaw
        write(iout,1000)
        if (is_encrypted)then                                     
          write(iout,'(5X,A,//)') 'CONFIDENTIAL DATA'
        else     
          write(iout,1002) rho0
          write(iout,1003) e,nu
          write(iout,1004) iflag,iflagsr,israte,fcut,fisokin,ismooth
          if (flag_fit == 0) then
            write(iout,1005) expa,al1,al2,al3,al4,al5,al6,al7,al8
          else
            write(iout,1006) expa,al1,al2,al3,al4,al5,al6,al7,al8
          endif
          if (iflag == 0) then
            write(iout,1007) nrate
            if (nrate>0) write(iout,1008)(ifunc(i),yfac(i),rate(i),i=1,nfunc)
          elseif (iflag == 1) then
            write(iout,1009) invp,invc,qvoce,beta,ko,alpha,aswift,nexp,epso
          elseif (iflag == 2) then
            write(iout,1010) k1,k2,ahs,bhs,mhs,eps0hs,nhs,hmart,temp0,         &
              tref,eta,cp,am,bm,cm,dm,ppm,qm,e0mart,vm0 
          elseif (iflag == 3) then
            write(iout,1011) itable(1),fscale0,epsd0,itable(2),fscale45,       &
              epsd45,itable(3),fscale90,epsd90
          endif
          if (fisokin > zero) write(iout,1012)(ckh(i), akh(i),i=1,4)
        endif
!
 1000 format(                                                                  &                                                                                            
       5X,'-------------------------------------------------------',/          &     
       5X,'           MATERIAL MODEL: BARLAT YLD2000              ',/,         & 
       5X,'-------------------------------------------------------',/)
 1001 format(/                                                                 &
       5X,A,/,                                                                 &
       5X,'MATERIAL NUMBER. . . . . . . . . . . . . . . . . . . .=',I10/,      &
       5X,'MATERIAL LAW . . . . . . . . . . . . . . . . . . . . .=',I10/)
 1002 format(                                                                  &
       5X,'INITIAL DENSITY. . . . . . . . . . . . . . . . . . . .=',1PG20.13/)  
 1003 format(                                                                  &
       5X,'YOUNG''S MODULUS. . . . . . . . . . . . . . . . . . . .=',1PG20.13/ &
       5X,'POISSON''S RATIO. . . . . . . . . . . . . . . . . . . .=',1PG20.13/)
 1004 format(                                                                  &
       5X,'YIELD STRESS FORMULATION FLAG: . . . . . . . . . . . .=',I10/       &
       5X,'    = 0: TABULATED YIELD STRESS                        ',/          &
       5X,'    = 1: SWIFT-VOCE                                    ',/          &
       5X,'    = 2: HANSEL                                        ',/          &
       5X,'    = 3: ORTHOTROPIC 3-DIR TABULATED                   ',/          &
       5X,'STRAIN RATE FLAG:. . . . . . . . . . . . . . . . . . .=',I10/       & 
       5X,'    = 0: TOTAL STRAIN RATE                             ',/          &
       5X,'    = 1: PLASTIC STRAIN RATE                           ',/          &
       5X,'SMOOTH STRAIN RATE OPTION. . . . . . . . . . . . . . .=',I10/       &
       5X,'STRAIN RATE CUTTING FREQUENCY. . . . . . . . . . . . .=',1PG20.13/  &
       5X,'ISOTROPIC/KINEMATIC HARDENING PARAMETER (CHARD). . . .=',1PG20.13/  &
       5X,'STRAIN RATE INTERPOLATION FLAG (ISMOOTH) . . . . . . .=',I10/       &
       5X,'    = 0: NOT USED                                     ',/,          &
       5X,'    = 1: LINEAR INTERPOLATION                         ',/,          &
       5X,'    = 2: LOGARITHMIC BASE 10 INTERPOLATION            ',/,          &
       5X,'    = 3: LOGARITHMIC BASE N INTERPOLATION             ',/)
 1005 format(                                                                  &
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
 1006 format(                                                                  &
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
 1007 format(                                                                  &
       5X,'TABULATED YIELD STRESS PARAMETERS:                     ',/,         &
       5X,'----------------------------------                     ',/,         &
       5X,'NUMBER OF YIELD STRESS FUNCTIONS . . . . . . . . . . .=',I10/)
 1008 format(                                                                  &
       5X,'YIELD STRESS FUNCTION NUMBER . . . . . . . . . . . . .=',I10/       &
       5X,'YIELD STRESS SCALE FACTOR. . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'REFERENCE STRAIN RATE. . . . . . . . . . . . . . . . .=',1PG20.13/)     
 1009 format(                                                                  &
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
       5X,'YIELD SWIFT REFERENCE STRAIN . . . . . . . . . . . . .=',1PG20.13)
 1010 format(                                                                  &
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
       5X,'INITIAL FRACTION OF MARTENSITE VM0 . . . . . . . . . .=',1PG20.13)
 1011 format(                                                                  &
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
 1012 format(                                                                  &
       5X,'CHABOCHE-ROUSSELIER KINEMATIC HARDENING PARAMETERS:    ',/,         &
       5X,'---------------------------------------------------    ',/,         &
       5X,'PARAMETER C1 . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'PARAMETER A1 . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'PARAMETER C2 . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'PARAMETER A2 . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'PARAMETER C3 . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'PARAMETER A3 . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'PARAMETER C4 . . . . . . . . . . . . . . . . . . . . .=',1PG20.13/  &
       5X,'PARAMETER A4 . . . . . . . . . . . . . . . . . . . . .=',1PG20.13)
      
      end subroutine hm_read_mat87
!=======================================================================
      end module hm_read_mat87_mod

