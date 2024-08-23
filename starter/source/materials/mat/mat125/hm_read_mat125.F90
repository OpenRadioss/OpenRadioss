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
      !||    hm_read_mat125_mod   ../starter/source/materials/mat/mat125/hm_read_mat125.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat          ../starter/source/materials/mat/hm_read_mat.F
      !||====================================================================
      module hm_read_mat125_mod
      contains
! ======================================================================================================================
! \brief Reading material parameters of /MAT/LAW125
! \details Reading material parameters of /MAT/LAW125
! ======================================================================================================================
      !||====================================================================
      !||    hm_read_mat125           ../starter/source/materials/mat/mat125/hm_read_mat125.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                   ../starter/source/output/message/message.F
      !||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||    init_mat_keyword         ../starter/source/materials/mat/init_mat_keyword.F
      !||--- uses       -----------------------------------------------------
      !||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
      !||    message_mod              ../starter/share/message_module/message_mod.F
      !||    submodel_mod             ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      subroutine hm_read_mat125(                                      &       
               nuvar    ,maxfunc  ,npropm   ,iout,                    &
               nfunc    ,ifunc    ,mtag     ,parmat   ,unitab   ,     &
               pm       ,lsubmodel,israte   ,mat_id   ,titr     ,     &
               matparam ,nvartmp  )
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
 !   I m p l i c i t   T y p e sXM
 !-----------------------------------------------
         implicit none 
#include  "my_real.inc"
 !-----------------------------------------------
 !   D u m m y   A r g u m e n t s
 !-----------------------------------------------
      integer, intent(in)                          :: mat_id,maxfunc,npropm,iout
      integer, intent(inout)                       :: nuvar,nfunc,nvartmp
      integer, intent(inout)                       :: israte
      integer, dimension(maxfunc), intent(inout)   :: ifunc
      type(mlaw_tag_), intent(inout)               :: mtag
      my_real, dimension(100),intent(inout)        :: parmat
      type (unit_type_),intent(in)                 :: unitab 
      my_real, dimension(npropm) ,intent(inout)    :: pm   
      type(submodel_data), dimension(*),intent(in) :: lsubmodel  
      character(len=nchartitle),intent(in)         :: titr 
      type(matparam_struct_) ,intent(inout)        :: matparam    
 !-----------------------------------------------
 !   L o c a l   V a r i a b l e s
 !-----------------------------------------------
      integer fs, ifem11t,ifxc,ifem11c,ifxt,ifem22t,ifyc,ifyt,      &
       ifzt,ifzc,ifem33t,ifem33c, ifems, ifems13,ifsc13,            &
       ifem23, ifsc23,ifsc,ifgamma,iferods,ifgamma2,ifgamma3,       &
       iftau, iftau2,iftau3,ifem22c,ifems23,ilaw,damage
      my_real                                                         &
         rho0,e1,e2,e3,g12,g23,g13,nu12,nu21,nu23,nu31,nu13,soft,     &
         em11t,em22t,em33t,em11c,em22c,em33c,ems,ems13,ems23,         &
         xc,xt,yc,yt,zc,zt,sc, sc23,sc13,gamma,tau,gamma2,tau2,       &
         tau3,gamma3, erods,tsdm, gammar,gammaf,nu32,                 &
         slimt1,slimc1,slimt2,slimc2,slimt3,slimc3,slims,             &
         slims13,slims23, a11,a22,a12,c11,c22,c33,c12,c13,c23,        &
         detc, d11,d22,d33,d12,d13,d23,dmn,dmx,al1c,al1t,al2c,        &
         al2t,al3c,al3t,m1t,m2t,m1c,m2c,m3c,m3t,ef11t,ef11c,          &
         ef22t,ef22c,ef33t,ef33c,fac,tsmd,g31,                  &
          fcut,efs,ms,als,                                            &
         c1,gmax,ssp,nu,young,asrate,ms13,efs13,als13,ms23,           &
         efs23,als23      
      logical :: is_available,is_encrypted
 !=======================================================================
      is_encrypted = .false.
      is_available = .false.
      ilaw         = 125
      g31         = zero  !is not initialized elsewhere
 !------------------------------------------
      call hm_option_is_encrypted(is_encrypted)
 !------------------------------------------
!card1 - Density
      call hm_get_floatv('MAT_RHO'   ,rho0     ,is_available, lsubmodel, unitab)
!card2 - young's moduli + shear moduli
      call hm_get_floatv('LSD_MAT_EA'    ,e1       ,is_available, lsubmodel, unitab)
      call hm_get_floatv('LSD_MAT_EB'    ,e2       ,is_available, lsubmodel, unitab)
      call hm_get_floatv('LSD_MAT_EC'    ,e3       ,is_available, lsubmodel, unitab)
      call hm_get_floatv('LSD_SOFT'      ,soft      ,is_available, lsubmodel, unitab)
      call hm_get_intv('LSD_FS' ,fs    ,is_available, lsubmodel)
!card3 - shear modulus +
      call hm_get_floatv('LSD_MAT_GAB'   ,g12      ,is_available, lsubmodel, unitab)
      call hm_get_floatv('LSD_MAT_GBC'   ,g23      ,is_available, lsubmodel, unitab)
      call hm_get_floatv('LSD_MAT_GCA'   ,g13      ,is_available, lsubmodel, unitab)
!card4 -  poisson's ratio
      call hm_get_floatv('LSD_MAT_PRBA'  ,nu12     ,is_available, lsubmodel, unitab)
      call hm_get_floatv('LSD_MAT_PRBC'  ,nu23     ,is_available, lsubmodel, unitab)
      call hm_get_floatv('LSD_MAT_PRAC'  ,nu13     ,is_available, lsubmodel, unitab) 
!card5 - dir 11 tention 
      call hm_get_floatv  ('LSD_M11T'        ,em11t      ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_XT'      ,xt         ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('MATL58_SLIMT1'   ,slimt1     ,is_available, lsubmodel, unitab)
      call hm_get_intv ('LSD_LCID12'  ,ifem11t     ,is_available, lsubmodel)
      call hm_get_intv ('LSD_LCID2'   ,ifxt       ,is_available, lsubmodel)
! card6 - dir 11 compression 
      call hm_get_floatv  ('LSD_M11C'         ,em11c      ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_XC'       ,xc         ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('MATL58_SLIMC1'    ,slimc1     ,is_available, lsubmodel, unitab)
      call hm_get_intv  ('LSD_LCID11'  ,ifem11c   ,is_available, lsubmodel)
      call hm_get_intv  ('LSD_LCID'    ,ifxc     ,is_available, lsubmodel)  
!card7 - dir 22 tention 
      call hm_get_floatv  ('LSD_M22T'         ,em22t      ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_YT'       ,yt         ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('MATL58_SLIMT2'    ,slimt2     ,is_available, lsubmodel, unitab)
      call hm_get_intv ('LSD_LCID14'  ,ifem22t     ,is_available, lsubmodel)
      call hm_get_intv ('LSD_LCID4'   ,ifyt       ,is_available, lsubmodel)
! card8 - dir 22 compression 
      call hm_get_floatv  ('LSD_M22C'         ,em22c      ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_YC'       ,yc         ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('MATL58_SLIMC2'    ,slimc2     ,is_available, lsubmodel, unitab)
      call hm_get_intv  ('LSD_LCID13'  ,ifem22c   ,is_available, lsubmodel)
      call hm_get_intv  ('LSD_LCID3'   ,ifyc     ,is_available, lsubmodel) 
!card9 - dir 33 tention  only for solid
      call hm_get_floatv  ('LSD_M33T'    ,em33t      ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_ZT'       ,zt         ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('MATL58_SLIMT3'   ,slimt3     ,is_available, lsubmodel, unitab)
      call hm_get_intv ('LSD_LCID22'  ,ifem33t     ,is_available, lsubmodel)
      call hm_get_intv ('LSD_LCID18'   ,ifzt       ,is_available, lsubmodel)
! card10- dir 33 compression only for solid
      call hm_get_floatv  ('LSD_M33C'    ,em33c      ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_ZC'       ,zc         ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('MATL58_SLIMC3'   ,slimc3     ,is_available, lsubmodel, unitab)
      call hm_get_intv  ('LSD_LCID21'  ,ifem33c   ,is_available, lsubmodel)
      call hm_get_intv  ('LSD_LCID17'   ,ifzc     ,is_available, lsubmodel)   
!card11 - shear 12 
      call hm_get_floatv  ('MATL58_GAMMA1'   ,gamma     ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('MATL58_TAU1'     ,tau       ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MS'          ,ems       ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_SC'      ,sc        ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('MATL58_SLIMS'    ,slims     ,is_available, lsubmodel, unitab)
!card12 - strain rate dependency (optional)
      call hm_get_intv  ('LSD_LCID7'    ,ifgamma   ,is_available, lsubmodel)
      call hm_get_intv  ('LSD_LCID6'    ,iftau     ,is_available, lsubmodel)
      call hm_get_intv  ('LSD_LCID15'   ,ifems     ,is_available, lsubmodel)
      call hm_get_intv  ('LSD_LCID5'    ,ifsc     ,is_available, lsubmodel)
!card13 - shear 13 for solid 
      call hm_get_floatv  ('MATL58_GAMMA2'   ,gamma2     ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('MATL58_TAU2'     ,tau2       ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MS13'        ,ems13      ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_SC13'    ,sc13       ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('MATL58_SLIMS13'  ,slims13    ,is_available, lsubmodel, unitab)
!card14 - strain rate dependency (optional)
      call hm_get_intv  ('LSD_LCID26'  ,ifgamma2   ,is_available, lsubmodel)
      call hm_get_intv  ('LSD_LCID25'    ,iftau2     ,is_available, lsubmodel)
      call hm_get_intv  ('LSD_LCID23'     ,ifems13     ,is_available, lsubmodel)
      call hm_get_intv  ('LSD_LCID20'      ,ifsc13      ,is_available, lsubmodel)
!card15- shear 13 for solid 
      call hm_get_floatv  ('MATL58_GAMMA3'   ,gamma3     ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('MATL58_TAU3'     ,tau3       ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MS23'        ,ems23      ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_SC23'    ,sc23       ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('MATL58_SLIMS23'  ,slims23    ,is_available, lsubmodel, unitab)
!card16 - strain rate dependency (optional)
      call hm_get_intv  ('LSD_LCID28'     ,ifgamma3   ,is_available, lsubmodel)
      call hm_get_intv  ('LSD_LCID27'      ,iftau3     ,is_available, lsubmodel)
      call hm_get_intv  ('LSD_LCID24'     ,ifems23     ,is_available, lsubmodel)
      call hm_get_intv  ('LSD_LCID19'      ,ifsc23      ,is_available, lsubmodel)   
!card17    
      call hm_get_floatv  ('LSD_MAT_EPSF'   ,gammaf     ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_EPSF'   ,gammar       ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_EPSF'   ,tsmd      ,is_available, lsubmodel, unitab)       
!card18
      call hm_get_intv  ('LSD_LCID16'        ,iferods  ,is_available, lsubmodel)  
      call hm_get_floatv  ('MATL58_ERODS'   ,erods       ,is_available, lsubmodel, unitab)       
!card? - equivalent strain rate cutoff frequency 
      call hm_get_floatv('fcut'      ,fcut     ,is_available, lsubmodel, unitab)

      ! young modulus initialization
      if (e2 == zero)  e2  = e1
      if (e3 == zero)  e3  = e2
      ! shear modulus
      if (g13 == zero) g13 = g12
      if (g23 == zero) g23 = g13
!-----------------------------
 !     check and default values
 !-----------------------------
      ! poisson's ratio
      if (nu12 < zero .or. nu12 >= half) then
        call ancmsg(msgid=3032,                        &
                  msgtype=msgerror,                    &
                  anmode=aninfo_blind_2,               &
                  r1=nu12,                             &
                  i1=mat_id,                           &
                  c1=titr)
      endif    
      nu21 = nu12*e2/e1
      if (nu21 < zero .or. nu21 >= half) then
        call ancmsg(msgid=3033,                      &                              
                  msgtype=msgerror,                  &
                  anmode=aninfo_blind_2,             &
                  r1=nu21,                           &
                  i1=mat_id,                         &
                  c1=titr)   
      endif
      if (nu23 < zero .or. nu23 >= half) then
        call ancmsg(msgid=3034,                        &
                    msgtype=msgerror,                  &
                    anmode=aninfo_blind_2,             &
                    r1=nu23,                           &
                    i1=mat_id,                         &
                    c1=titr)
      endif
      nu32 = nu23*e3/e2
      if (nu32 < zero .or. nu32 >= half) then
        call ancmsg(msgid=3035,                     &
                 msgtype=msgerror,                  &
                 anmode=aninfo_blind_2,             &
                 r1=nu32,                           &
                 i1=mat_id,                         &
                 c1=titr)
      endif
      if (nu13 < zero .or. nu13 >= half) then
        call ancmsg(msgid=3036,                     &
                 msgtype=msgerror,                  &
                 anmode=aninfo_blind_2,             &
                 r1=nu31,                           &
                 i1=mat_id,                         &
                 c1=titr)
      endif 
      nu31 = nu13*e3/e1
      if (nu31 < zero .or. nu31 >= half) then
        call ancmsg(msgid=3037,                     &
                  msgtype=msgerror,                 &
                  anmode=aninfo_blind_2,            &
                  r1=nu13,                          &
                  i1=mat_id,                        &
                  c1=titr)
      endif 
      ! checking poisson's ratio 
      detc = one - nu12*nu21
      if (detc <= zero) then
        call ancmsg(msgid=307,                        &
                    msgtype=msgerror,                 &
                    anmode=aninfo,                    &
                    i1=mat_id,                        &
                    c1=titr)
      endif 
      ! elasticity matrix for 2d plane stress
      fac = one/(one - nu12*nu21)
      a11 = e1*fac
      a12 = nu21*a11
      a22 = e2*fac
      ! compliance matrix for 3d
      c11 = one/e1
      c22 = one/e2
      c33 = one/e3
      c12 =-nu12/e1
      c13 =-nu31/e3
      c23 =-nu23/e2      
      ! checking input
      detc= c11*c22*c33-c11*c23*c23-c12*c12*c33+c12*c13*c23      &
            +c13*c12*c23-c13*c22*c13
      if(detc <= zero) then
         call ancmsg(msgid=307,                                  &
                    msgtype=msgerror,                            &     
                    anmode=aninfo,                               &
                    i1=mat_id,                                   &
                    c1=titr) 
      endif
      ! 3d elastic matrix
      d11  = (c22*c33-c23*c23)/detc
      d12  =-(c12*c33-c13*c23)/detc
      d13  = (c12*c23-c13*c22)/detc
      d22  = (c11*c33-c13*c13)/detc
      d23  =-(c11*c23-c13*c12)/detc
      d33  = (c11*c22-c12*c12)/detc  
      dmn  = min(d11*d22 -d12**2, d11*d33 - d13**2, d22*d33 - d23**2 )      
      dmx  = max(d11,d22,d33)
!
      ! matrix damage
      tsmd  = min(tsmd,one)
!    
      ! default strain rate cutoff frequency
      if (fcut == zero) fcut = 5000.0d0*unitab%fac_t_work
 !--------------------------
 !     filling buffer tables
 !-------------------------- 
      ! number of material parameters
      matparam%nuparam = 80

      allocate (matparam%uparam(matparam%nuparam))

      ! number of functions
      nfunc   = 24
      ! number of user variables 
      nuvar   = 10
      ! number of temporary variable for interpolation
      nvartmp = 0
!     
      ! computing alpha and m for each direction
         ! dir 11 (tension - compression)
      damage = 0 
      al1t = ep20
      m1t = one 
      al1c = ep20
      m1c = one 
      if(e1 > zero) then
        if(em11t /= zero )then
          ef11t  = xt/e1
          m1t = -one/log(ef11t/em11t)     
          al1t = m1t*(em11t/ef11t)**m1t
          damage = 1
        endif 
      
        if(em11c /= 0  )then
          ef11c  = xc/e1
          m1c = -one/log(ef11c/em11c)     
          al1c = m1c*(em11c/ef11c)**m1c
        endif
      endif 
      al2c = ep20
      m2t = one 
      al2c = ep20
      m2c = one 
      if(e2 > zero) then
        if(em22t /= zero )then
          ef22t  = yt/e2
          m2t = -one/log(ef22t/em22t)     
          al2t = m2t*(em22t/ef22t)**m2t
        endif 
        !
        if(em22c /= 0  )then
          ef22c  = yc/e1
          m2c = -one/log(ef22c/em22c)     
          al2c = m2c*(em22c/ef22c)**m2c
        endif
      endif  
      al3c = ep20
      m3t = one 
      al3c = ep20
      m3c = one     
      if(e3 > zero) then
        if(em33t /= zero )then
          ef33t  = zt/e3
          m3t = -one/log(ef33t/em33t)     
          al3t = m3t*(em33t/ef33t)**m3t
          !!if(ef11t < em11t ) 'error message'
        endif 
        !
        if(em33c /= 0  )then
          ef33c  = zc/e3
          m3c = -one/log(ef33c/em33c)     
          al3c = m3c*(em33c/ef33c)**m3c
        endif
      endif 
       ms = one
       als = ep20
       efs = zero
      if(fs == -1) then
         ! plane shear 
        if(g12 > zero .and. tau > zero .and. gamma  > zero )then
            efs  = tau /g12
            ms = -one/log(efs/gamma) ! one/ln(epsm/epsf)    
            als = ms*(gamma/efs)**ms
        else 
           ! adding error messsage   
        endif
         ! transverse shear 13 (only for solid)
        ms13 = one
        als13 = ep20
        efs13 = zero
        if(g13 > zero .and. tau2 > zero .and. gamma2 > 0 )then
          efs13  = tau2 /g13
          ms13 = -one/log(efs13/gamma2) ! one/ln(epsm/epsf)    
          als13 = ms*(gamma2/efs13)**ms13
        else 
           ! adding error messsage   
        endif 
        ! transverse shear 23 (only for solid)
        ms23 = one
        als23 = ep20
        efs23 = zero
        if(g23 > zero .and. tau3 > zero .and. gamma3 > 0 )then
          efs23  = tau3 /g23
          ms23 = -one/log(efs23/gamma3) ! one/ln(epsm/epsf)    
          als23 = ms23*(gamma3/efs23)**ms23
        else 
           ! adding error messsage   
        endif 

      endif ! fs = -1
      ! material parameters
      matparam%uparam(1)  = e1
      matparam%uparam(2)  = e2
      matparam%uparam(3)  = e3
      matparam%uparam(4)  = g12
      matparam%uparam(5)  = g13
      matparam%uparam(6)  = g23
      !
      matparam%uparam(8)  = nu12
      matparam%uparam(9)  = nu13
      matparam%uparam(10) = nu23
      !
      matparam%uparam(11)  = em11t
      matparam%uparam(12)  = xt
      matparam%uparam(13)  = slimt1
      matparam%uparam(14)  = em11c
      matparam%uparam(15)  = yt
      matparam%uparam(16) = slimc1
     !
      matparam%uparam(17)  = em22t
      matparam%uparam(18)  = yt
      matparam%uparam(19) = slimt2
      matparam%uparam(20)  = em22c
      matparam%uparam(21)  = yc
      matparam%uparam(22) = slimc2
    !
      matparam%uparam(23)  = gamma
      matparam%uparam(24)  = tau
      matparam%uparam(25)  = ems
      matparam%uparam(26)  = sc
      matparam%uparam(27)  = slims
     !
      matparam%uparam(28)  = em33t
      matparam%uparam(29)  = zt
      matparam%uparam(30)  = slimt3
      matparam%uparam(31)  = em33c
      matparam%uparam(32)  = zc
      matparam%uparam(33)  = slimc3
   !
      matparam%uparam(34)  = gamma2
      matparam%uparam(35)  = tau2
      matparam%uparam(36)  = ems13
      matparam%uparam(37)  = sc13
      matparam%uparam(38)  = slims13
   !
      matparam%uparam(39)  = gamma3
      matparam%uparam(40)  = tau3
      matparam%uparam(41)  = ems23
      matparam%uparam(42)  = sc23
      matparam%uparam(43)  = slims23

      matparam%uparam(44)  = gammaf
      matparam%uparam(45)  = gammar
      matparam%uparam(46)  = tsdm
      !
      matparam%uparam(47)  = erods
      !
      matparam%uparam(49) = ef11t
      matparam%uparam(50) = m1t
      matparam%uparam(51) = al1t
      matparam%uparam(52) = ef11c
      matparam%uparam(53) = m1c
      matparam%uparam(54) = al1c

      matparam%uparam(55) = ef22t
      matparam%uparam(56) = m2t
      matparam%uparam(57) = al2t
      matparam%uparam(58) = ef22c
      matparam%uparam(59) = m2c
      matparam%uparam(60) = al2c
     
      matparam%uparam(61) = ef33t
      matparam%uparam(62) = m3t
      matparam%uparam(63) = al3t
      matparam%uparam(64) = ef33c
      matparam%uparam(65) = m3c
      matparam%uparam(66) = al3c

      matparam%uparam(67) = efs
      matparam%uparam(68) = ms
      matparam%uparam(69) = als


      matparam%uparam(70) = efs13
      matparam%uparam(71) = ms13
      matparam%uparam(72) = als13

      matparam%uparam(73) = efs23
      matparam%uparam(74) = ms23
      matparam%uparam(75) = als23
      
      matparam%uparam(76) = fs
      matparam%uparam(77) = damage
      !
      matparam%uparam(78)  = nu21
      matparam%uparam(70)  = nu31
      matparam%uparam(80) = nu32   
      ! function ids
      ifunc(1)  = ifem11t 
      ifunc(2)  = ifxt
      ifunc(3)  = ifem11c
      ifunc(4)  = ifxc
      !
      ifunc(5)  = ifem22t 
      ifunc(6)  = ifyt
      ifunc(7)  = ifem22c
      ifunc(8)  = ifyc
      !
      ifunc(9)   = ifem33t 
      ifunc(10)  = ifzt
      ifunc(11)  = ifem33c
      ifunc(12)  = ifzc
      !
      ifunc(13)  = ifgamma
      ifunc(14)  = iftau
      ifunc(15)  = ifsc
      ifunc(15)  = ifems
      !
      ifunc(16)  = ifgamma2
      ifunc(17)  = iftau2
      ifunc(18)  = ifsc13
      ifunc(19)  = ifems13
      !
      ifunc(20)  = ifgamma3
      ifunc(21)  = iftau3
      ifunc(22)  = ifsc23
      ifunc(23)  = ifems23
      !
      ifunc(24)  = iferods
!
       nu21   = nu12*e2/e1
       nu    = sqrt(nu12*nu21)
       detc  = one - nu12*nu21
       young = max(e1,e2,e3)
       c1    = third*young/(one - two*nu)
       a11    = max(e1,e2,e3)/detc
       gmax  = max(g12,g23,g31)
       ssp   = sqrt(max(a11,gmax)/ rho0)
       asrate = two*pi*fcut

       ! mat_param%asrate = asrate 
       ! mat_param%young = c1
       ! mat_param%nu = nu
       ! mat_param%shear = gmax
       ! mat_param%ssp  = sqrt(c1/rho0
       ! mat_param%bulk = c1
!-----------------------------------------
          ! parameters used outside the law
!-----------------------------------------
          pm(9)  = asrate            !  mat_param%asrate
          pm(20) = a11               !  mat_param%young
          pm(21) = nu                !  mat_param%nu
          pm(22) = max(g12,g12,g23)  !  mat_param%shear
          pm(24) = a11
          pm(26) = five_over_6
          pm(27) = ssp     !  mat_param%ssp
          pm(32) = c1                !  mat_param%bulk

      ! still used in elements
        pm(33) = e1                !  mat_param%e11
        pm(34) = e2                !  mat_param%e22
        pm(186)= e2                !  mat_param%e33
        pm(35) = nu12              !  mat_param%n12
        pm(36) = nu21              !  mat_param%n21
        pm(37) = g12               !  mat_param%g12
        pm(38) = g23               !  mat_param%g23
        pm(39) = g31               !  mat_param%g31   

          ! parmat table
        israte     = 1
        parmat(1)  = c1
        parmat(2)  = c1
        parmat(3)  = nu
        parmat(4)  = israte
        parmat(5)  = fcut
        parmat(16) = 1
!
      ! pm table
        pm(1)  = rho0
        pm(89) = rho0
      
!     
      ! mtag variable activation
       mtag%g_pla  = 1
       mtag%l_pla  = 1
       mtag%g_epsd = 1
       mtag%l_epsd = 1
       mtag%g_seq  = 1
       mtag%l_seq  = 1
       mtag%g_dmg  = 6
       mtag%l_dmg  = 6
     ! number of output mod
     ! matparam%nmod = 3
     ! allocate(matparam%mode(matparam%nmod))
     ! matparam%mode(1) = "fiber damage"
     ! matparam%mode(2) = "shear matrix damage"
     ! matparam%mode(3) = "transverse matrix damage"
!
      call init_mat_keyword(matparam ,"ELASTO_PLASTIC")
      !call init_mat_keyword(matparam ,"incremental"   )
      call init_mat_keyword(matparam ,"TOTAL"   )
      call init_mat_keyword(matparam ,"LARGE_STRAIN"  )
      call init_mat_keyword(matparam ,"HOOK")
      call init_mat_keyword(matparam ,"ORTHOTROPIC")
!
      ! properties compatibility
      call init_mat_keyword(matparam,"SOLID_ORTHOTROPIC")
      call init_mat_keyword(matparam,"SHELL_ORTHOTROPIC")
!------------------------- 
!     parameters printout
!-------------------------- 
      write(iout,1000) trim(titr),mat_id,ilaw
      write(iout,1050)
      if (is_encrypted) then
        write(iout,'(5x,a,//)')'confidential data'
      else
        write(iout,1200) rho0
        write(iout,1300) e1,e2,e3,g12,g23,g13,nu12,nu23,nu13
        write(iout,1400)  em11t, xt,slimt1,em11c,xc,slimc1
        write(iout,1500)  em22t, yt,slimt2,em22c,yt,slimc2
        write(iout,1600)  em33t, zt,slimt3,em33c,zt,slimc3
        write(iout,1700) gamma,tau, ems,sc, slims
        write(iout,1800) gamma2,tau2, ems13,sc13, slims13
        write(iout,1900) gamma3,tau3, ems23,sc23, slims23

        write(iout,2000) ifem11t, ifxt,ifem11c, ifxc
        write(iout,2100) ifem22t, ifyt,ifem22c, ifyc
        write(iout,2200) ifem33t, ifzt,ifem33c, ifzc

        write(iout,2300) ifgamma, iftau,ifems, ifsc
        write(iout,2400) ifgamma2, iftau2,ifems13, ifsc13
        write(iout,2500) ifgamma3, iftau3,ifems23, ifsc23
        write(iout,2600) gammaf, gammar, tsdm, erods, iferods

        write(iout,2800) fcut
      endif     
!-----------------------------------------------------------------------
 1000 format(/                                                               &
       5x,a,/,                                                               &
       5x,'material number. . . . . . . . . . . . =',i10/,                   &
       5x,'material law . . . . . . . . . . . . . =',i10/)
 1050 format                                                                 &
      (5x,'material model : laminated composite ',/,                         &
       5x,'----------------------------------',/)
 1200 format(                                                                &
       5x,'initial density . . . . . . . . . . . . . . . . .=',1pg20.13/) 
 1300 format(                                                                &
       5x,'elasticity parameters:                            ',/             &
       5x,'----------------------                            ',/             &
       5x,'young modulus in dir. 1 (fiber)  e1 . . . . . . .=',1pg20.13/     &
       5x,'young modulus in dir. 2 (matrix) e2 . . . . . . .=',1pg20.13/     &
       5x,'young modulus in dir. 3 (matrix) e3 . . . . . . .=',1pg20.13/     &
       5x,'shear modulus in plane 12 g12 . . . . . . . . . .=',1pg20.13/     &
       5x,'shear modulus in plane 23 g23 . . . . . . . . . .=',1pg20.13/     &
       5x,'shear modulus in plane 31 g13 . . . . . . . . . .=',1pg20.13/     &
       5x,'poisson ratio in plane 12 nu12. . . . . . . . . .=',1pg20.13/     &
       5x,'poisson ratio in plane 23 nu23. . . . . . . . . .=',1pg20.13/     &
       5x,'poisson ratio in plane 31 nu13. . . . . . . . . .=',1pg20.13)
 1400 format(                                                                     &
       5x,' fiber (dir. 1) parameters   :                            ',/          &
       5x,'---------------------------                               ',/          &
       7x,'strain at longitudinal tensile strength . . . . . . . . =',1pg20.13/   &    
       7x,'longitudinal tensile strength  . . . .  . . . . . . . . =',1pg20.13/   &
       7x,'scale for minimum longitudinal tensile stress limit. . .=',1pg20.13/   &
       7x,'strain at longitudinal compressive strength . . . . . . =',1pg20.13/   &     
       7x,'longitudinal compressive strength  . . . .  . . . . . . =',1pg20.13/   &
       7x,'scale for minimum compressive tensile stress limit. . . =',1pg20.13/ )

 1500 format(                                                                     &
       5x,'matrix (dir. 2) parameters   :                            ',/          &
       5x,'---------------------------                               ',/          &
       7x,'strain at transverse  tensile strength . . . . . . . . =',1pg20.13/    &     
       7x,'transverse tensile strength  . . . .  . . . . . . . . =',1pg20.13/     &
       7x,'scale for minimum transverse tensile stress limit. . .=',1pg20.13/     &
       7x,'strain at transverse compressive strength . . . . . . =',1pg20.13/     &     
       7x,'transverse compressive strength  . . . .  . . . . . . =',1pg20.13/     &
       7x,'scale for minimum compressive tensile stress limit. . . =',1pg20.13/ )

 1600 format(                                                                     &
       5x,'matrix (dir. 3) parameters   :                            ',/          &
       5x,'---------------------------                               ',/          &
       5x,'strain at transverse  tensile strength . . . . . . . . =',1pg20.13/    &    
       5x,'transverse tensile strength  . . . .  . . . . . . . . =',1pg20.13/     &
       5x,'scale for minimum transverse tensile stress limit. . .=',1pg20.13/     &
       5x,'strain at transverse compressive strength . . . . . . =',1pg20.13/     &    
       5x,'transverse compressive strength  . . . .  . . . . . . =',1pg20.13/     &
       5x,'scale for minimum compressive tensile stress limit. . . =',1pg20.13/ )
 1700 format(                                                                    &
       5x,' shear (12) parameters   :                            ',/               &
       5x,'---------------------------                               ',/           &
       7x,'shear strain limit in direction 12 . . . . . . . . . . =',1pg20.13/     &
       7x,'shear stress limit in direction 12  . . . .  . . . . . =',1pg20.13/     &
       7x,'engineering shear strain at shear strength 12 . . . . .=',1pg20.13/     &
       7x,'shear strength 12 . . . . . . . . . . . . . . . . . . . =',1pg20.13/    &
       7x,'scale for minimum shear stress limit 12 . . . . .. . . =',1pg20.13/ )
 1800 format(                                                                     &
       5x,' shear (13) parameters   :                            ',/              &
       5x,'---------------------------                               ',/          &
       7x,'shear strain limit in direction 13 . . . . . . . . . . =',1pg20.13/    &
       7x,'shear stress limit in direction 13  . . . .  . . . . . =',1pg20.13/    &
       7x,'engineering shear strain at shear strength 13 . . . . .=',1pg20.13/    &
       7x,'shear strength 13 . . . . . . . . . . . . . . . . . . . =',1pg20.13/   & 
       7x,'scale for minimum shear stress limit 13 . . . . .. . . =',1pg20.13/ )
 1900 format(                                                                     &
       5x,' shear (23) parameters   :                            ',/                &
       5x,'---------------------------                               ',/            &
       7x,'shear strain limit in direction 23 . . . . . . . . . . =',1pg20.13/      &
       7x,'shear stress limit in direction 23  . . . .  . . . . . =',1pg20.13/      &
       7x,'engineering shear strain at shear strength 23 . . . . .=',1pg20.13/      &
       7x,'shear strength 23 . . . . . . . . . . . . . . . . . . . =',1pg20.13/     &
       7x,'scale for minimum shear stress limit 23 . . . . .. . . =',1pg20.13/ )
 2000 format(                                                                   &
       5x,' fiber (dir. 1) strain rate dependency  :                 ',/        &
       5x,'---------------------------                               ',/        &
       7x,'curve id defining em11t  . . . .  . . . . . . . . . . . . =',i10     &
       7x,'curve id defining longitudinal tensile strength xt . . .  =',i10/    &
       7x,'curve id defining em11c  . . . .  . . . . . . . . . . . . =',i10/    &
       7x,'curve id defining longitudinal compresssive strength xc . =',i10/  )
 2100 format(                                                                   &                                                              
       5x,' matrix (dir. 2) strain rate dependency  :                ',/        &
       5x,'---------------------------                               ',/        &
       7x,'curve id defining em22t  . . . .  . . . . . . . . . . . . =',i10/    &
       7x,'curve id defining longitudinal tensile strength yt . . .  =',i10/    &
       7x,'curve id defining em22c  . . . .  . . . . . . . . . . . . =',i10/    &
       7x,'curve id defining longitudinal compresssive strength xc . =',i10/  )
 2200 format(                                                                   &
       5x,' matrix (dir. 3) strain rate dependency  :                ',/        &
       5x,'---------------------------                               ',/        &
       7x,'curve id defining em33t  . . . .  . . . . . . . . . . . . =',i10/    &
       7x,'curve id defining longitudinal tensile strength yt . . .  =',i10/    &
       7x,'curve id defining em33c  . . . .  . . . . . . . . . . . . =',i10/    &
       7x,'curve id defining longitudinal compresssive strength yt . =',i10/ )
 2300 format(                                                                   &
       5x,' dir 12 - strain rate dependency  :                       ',/        &
       5x,'---------------------------                               ',/        &
       7x,'curve id defining shear strain gamma dir12 . . . . . . . .=',i10/    &
       7x,'curve id defining shear stress tau dir12 . . . . .  . . . =',i10/    &
       7x,'curve id defining shear strain ems dir12. . . . . . . . . =',i10/    &
       7x,'curve id defining shear strength sc12 at ems strain . . . =',i10/  )    
 2400 format(                                                                   &
       5x,' dir 13 - strain rate dependency  :                       ',/        &
       5x,'---------------------------                               ',/        &
       7x,'curve id defining shear strain gamma dir13 . . . . . . .. =',i10/    &
       7x,'curve id defining shear stress tau dir13 . . . . .  . . . =',i10/    &
       7x,'curve id defining shear strain ems dir13. . . . . . . . . =',i10/    &
       7x,'curve id defining shear strength sc13 at ems strain . . . =',i10/  )    

 2500 format(                                                                   &
       5x,' dir 23 - strain rate dependency  :                       ',/        &
       5x,'---------------------------                               ',/        &
       7x,'curve id defining shear strain gamma dir23 . . . . . . . .=',i10/    &
       7x,'curve id defining shear stress tau dir23 . . . . .  . . . =',i10/    &
       7x,'curve id defining shear strain ems dir23. . . . . . . . . =',i10/    &
       7x,'curve id defining shear strength sc23 at ems strain . . . =',i10/  )    

 2600 format(                                                                    &
       5x,' damage parameters:                               ',/                 &
       5x,'---------------------------------                 ',/                 &
       5x,'damage initiation transverse shear strain. . . . . . . =',1pg20.13/   &  
       5x,'final rupture transverse shear strain . .  . . . . . . =',1pg20.13/   &
       5x,'transverse shear maximum damage . . . . . . . . . . . .=',1pg20.13/   &
       5x,'maximum effective strain for ply failure erods  . . . .=',1pg20.13/   &
       5x,'curve id defining erods for strain rate dependency. . .=',i10/ )

 2800 format(                                                                    &
       5x,'strain rate filtering cutoff frequency fcut . . .=',1pg20.13/)
!-----------------------------------------------------------------------
        end subroutine hm_read_mat125
!-------------------
      end module hm_read_mat125_mod
