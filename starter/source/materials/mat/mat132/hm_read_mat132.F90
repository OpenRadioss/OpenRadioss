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
      !||    hm_read_mat132_mod   ../starter/source/materials/mat/mat132/hm_read_mat132.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat          ../starter/source/materials/mat/hm_read_mat.F
      !||====================================================================
      module hm_read_mat132_mod
      contains
! ======================================================================================================================
! \brief Reading material parameters of /MAT/LAW132
! \details Reading material parameters of /MAT/LAW132
! ======================================================================================================================
      !||====================================================================
      !||    hm_read_mat132           ../starter/source/materials/mat/mat132/hm_read_mat132.F90
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
      subroutine hm_read_mat132(                                      &       
               nuvar    ,maxfunc  ,npropm   ,iout,                    &
               mtag     ,parmat   ,unitab   , ntable   ,table     ,    &
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
          use table_mod 
          use func_table_copy_mod
          use mat_table_copy_mod  
          use precision_mod, only : WP 
 !-----------------------------------------------
 !   I m p l i c i t   T y p e s
 !-----------------------------------------------
         implicit none 
 !-----------------------------------------------
 !   D u m m y   A r g u m e n t s
 !-----------------------------------------------
      integer, intent(in)                                 :: mat_id,maxfunc,npropm,iout
      integer, intent(inout)                              :: nuvar,nvartmp
      integer, intent(inout)                              :: israte
      type(mlaw_tag_), intent(inout)                      :: mtag
      real(kind=WP) , dimension(100),intent(inout)        :: parmat
      type (unit_type_),intent(in)                        :: unitab 
      real(kind=WP) , dimension(npropm) ,intent(inout)    :: pm   
      type(submodel_data), dimension(*),intent(in) :: lsubmodel  
      character(len=nchartitle),intent(in)         :: titr 
      type(matparam_struct_) ,intent(inout)        :: matparam    
      integer, intent(in)                               :: ntable    !< Number of tables
      type(ttable), dimension(ntable), intent(in)       :: table     !< Tables
 !-----------------------------------------------
 !   L o c a l   V a r i a b l e s
 !-----------------------------------------------
      integer i,ifxc,ifxt,ifyc,ifyt,ifsc,ilaw,func_sc,         &
      func_gxc,func_gxt,func_gyt,func_gyc,func_sl,func_xt,func_xc, &
      func_yt,func_yc,nfunc,func_gsl,func_xt0,func_xc0,          &
      func_gxt0,func_gxc0
      integer , dimension(maxfunc) :: ifunc,func
      real(kind=WP)                                                         &
         rho0,e1,e2,e3,g12,g23,g13,nu12,nu21,nu23,nu31,nu13,soft,     &
         xc,xt,yc,yt,sl,nu32,                                         &
         gxt,gxc,gyt,gyc,gsl,etan,                                    &
         a11,a22,a12,c11,c22,c33,c12,c13,c23, detc,scale ,            &
         d11,d22,d33,d12,d13,d23,dmn,dmx,g31, fcut ,c1,gmax,ssp,nu,   &
         young,asrate,fac,ang0,sigy,beta,gxc0,gxt0,xt0,xc0,           &
         gammal,yscale(1),x1scale,x2scale,y1scale,y2scale,            &
         x2vect(maxfunc),ratio,g_ratio,st,eta_l,eta_t,tmp,tmp1,  &
         cf12,cf31,cf23,ef11t,ef11c,ef22t,ef22c,ef12,ef31,ef23 ,      &
         epsf23,epsr23,tsmd23,epsf31,epsr31,tsmd31,theta_c
      !   
      logical :: is_available,is_encrypted
 !=======================================================================
      is_encrypted = .false.
      is_available = .false.
      ilaw         = 132
      g31         = zero  !is not initialized elsewhere
 !------------------------------------------
      call hm_option_is_encrypted(is_encrypted)
 !------------------------------------------
!card1 - Density
      call hm_get_floatv('Rho'   ,rho0     ,is_available, lsubmodel, unitab)
!card2 - young's moduli + shear moduli
      call hm_get_floatv('LSDYNA_EA'    ,e1       ,is_available, lsubmodel, unitab)
      call hm_get_floatv('LSDYNA_EB'    ,e2       ,is_available, lsubmodel, unitab)
      call hm_get_floatv('LSDYNA_EC'    ,e3       ,is_available, lsubmodel, unitab)
! card3 - shear modulus +
      call hm_get_floatv('LSDYNA_GAB'   ,g12      ,is_available, lsubmodel, unitab)
      call hm_get_floatv('LSDYNA_GBC'   ,g23      ,is_available, lsubmodel, unitab)
      call hm_get_floatv('LSDYNA_GCA'   ,g13      ,is_available, lsubmodel, unitab)     
!card4 -  poisson's ratio ! tocheck
      call hm_get_floatv('LSDYNA_PRBA'  ,nu21     ,is_available, lsubmodel, unitab)
      call hm_get_floatv('LSDYNA_PRCB'  ,nu32     ,is_available, lsubmodel, unitab)
      call hm_get_floatv('LSDYNA_PRCA'  ,nu31     ,is_available, lsubmodel, unitab) 
!card5 strenght energy 
      call hm_get_floatv  ('LSD_GXC'       ,gxc         ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_GXT'       ,gxt         ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_GYC'       ,gyc         ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_GYT'       ,gyt         ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_GSL'       ,gsl         ,is_available, lsubmodel, unitab)      
!card6 strenght 
      call hm_get_floatv  ('LSD_MAT_XT'       ,xt         ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_XC'       ,xc         ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_YT'       ,yt         ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_YC'       ,yc         ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_SL'       ,sl         ,is_available, lsubmodel, unitab)
!card7 - for bilinear damage
      call hm_get_floatv  ('LSD_GXC0'          ,gxc0       ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_GXT0'          ,gxt0       ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_XT0'       ,xt0        ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_XC0'       ,xc0         ,is_available, lsubmodel, unitab)
!card8 - for in plane plastic behavior
      call hm_get_floatv  ('LSD_FIO'          ,ang0         ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSDYNA_SIGY'       ,sigy        ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_BETA'     ,beta        ,is_available, lsubmodel, unitab) 
      call hm_get_floatv  ('LSDYNA_ETAN'      ,etan        ,is_available, lsubmodel, unitab) 
      call hm_get_intv    ('LSD_LCSS'         ,func_sc     ,is_available, lsubmodel)
!card9 
      call hm_get_floatv  ('LSD_MAT_EPSF23'       ,epsf23      ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_EPSR23'       ,epsr23      ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_TSMD23'       ,tsmd23      ,is_available, lsubmodel, unitab) 
! card 10
      call hm_get_floatv  ('LSD_MAT_EPSF31'       ,epsf31      ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_EPSR31'       ,epsr31      ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_TSMD31'       ,tsmd31      ,is_available, lsubmodel, unitab)
! card11
      call hm_get_floatv  ('LSD_MAT_EF11T'       ,ef11t      ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_EF11C'       ,ef11c      ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_EF22T'       ,ef22t      ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_EF22c'       ,ef22c      ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_EF12'        ,ef12        ,is_available, lsubmodel, unitab)
! card12    
       call hm_get_floatv  ('LSD_MAT_EF31'        ,ef31      ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_EF23'         ,ef23      ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_CF12'       ,cf12      ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_CF31'       ,cf31      ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_CF23'       ,cf23      ,is_available, lsubmodel, unitab)
!card 13 - equivalent strain rate cutoff frequency and ratio 
     call hm_get_floatv('LRD_RATIO'      ,ratio     ,is_available, lsubmodel, unitab)
     call hm_get_floatv('FCUT'      ,fcut     ,is_available, lsubmodel, unitab)
!-----------------------------
      ! young modulus initialization
      if (e2 == zero)  e2  = e1
      if (e3 == zero)  e3  = e2
      ! shear modulus
      if (g13 == zero) g13 = g12
      if (g23 == zero) g23 = g13
      if(nu31 == zero ) nu31 = nu21
      if(nu32 == zero ) nu32 = nu21
      if(tsmd23 == zero) tsmd23 = zep9
      if(tsmd31 == zero) tsmd31 = zep9
      if(ef11t == zero) ef11t = ep20
      if(ef22t == zero) ef22t = ep20
      if(ef11c == zero) ef11c = ep20
      if(ef22c == zero) ef22c = ep20
      if(ef12  == zero) ef12  = ep20
      if(ef31  == zero) ef31  = ep20
      if(ef23  == zero) ef23  = ep20
      if(epsf31 == zero) epsf31 = ep20
      if(epsr31 == zero) epsr31 = two*epsf31
      if(epsf23 == zero) epsf23 = ep20
      if(epsr23 == zero) epsr23 =two*epsf23
      if(sigy == zero) sigy = ep20
      if(ratio <= zero) ratio = one
!-----------------------------
 !     check and default values
 !-----------------------------
      ! poisson's ratio
      if (nu21 < zero .or. nu21 >= half) then
        call ancmsg(msgid=3032,                        &
                  msgtype=msgerror,                    &
                  anmode=aninfo_blind_2,               &
                  r1=nu21,                             &
                  i1=mat_id,                           &
                  c1=titr)
      endif    
      nu12 = nu21*e1/e2
      if (nu12 < zero .or. nu12 >= half) then
        call ancmsg(msgid=3033,                      &                              
                  msgtype=msgerror,                  &
                  anmode=aninfo_blind_2,             &
                  r1=nu12,                           &
                  i1=mat_id,                         &
                  c1=titr)   
      endif
      if (nu32 < zero .or. nu32 >= half) then
        call ancmsg(msgid=3034,                        &
                    msgtype=msgerror,                  &
                    anmode=aninfo_blind_2,             &
                    r1=nu32,                           &
                    i1=mat_id,                         &
                    c1=titr)
      endif
      nu23 = nu32*e2/e3
      if (nu23 < zero .or. nu23 >= half) then
        call ancmsg(msgid=3035,                     &
                 msgtype=msgerror,                  &
                 anmode=aninfo_blind_2,             &
                 r1=nu23,                           &
                 i1=mat_id,                         &
                 c1=titr)
      endif
      if (nu31 < zero .or. nu31 >= half) then
        call ancmsg(msgid=3036,                     &
                 msgtype=msgerror,                  &
                 anmode=aninfo_blind_2,             &
                 r1=nu31,                           &
                 i1=mat_id,                         &
                 c1=titr)
      endif 
      nu13 = nu31*e1/e3
      if (nu13 < zero .or. nu13 >= half) then
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
      ! default strain rate cutoff frequency
      if (fcut == zero) fcut = 5000.0d0*unitab%fac_t_work
!    
      matparam%ntable = 0
      if(func_sc > 0) then
         matparam%ntable = 1
          func(1) = func_sc
      endif    

      if(xt == zero ) then
        xt = ep20
      elseif(xt < zero) then
        func_xt = nint(abs(xt))
        matparam%ntable  = matparam%ntable + 1
        func(matparam%ntable) = func_xt
      endif   
      if(xc == zero )then
         xc = ep20
      elseif(xc < zero ) then
        func_xc = nint(abs(xc))
        matparam%ntable  = matparam%ntable + 1
        func(matparam%ntable) = func_xc
      endif 
      if(yt == zero ) then
         yt = ep20
      elseif(yt < zero ) then
        func_yt = nint(abs(yt))
        matparam%ntable  = matparam%ntable + 1
         func(matparam%ntable) = func_yt
      endif   
      if(yc == zero ) then
        yc = ep20
      elseif(yc < zero ) then
          func_yc = nint(abs(yc))
          matparam%ntable  = matparam%ntable + 1  
           func(matparam%ntable) = func_yc
      endif   
       if(sl == zero ) then
        sl = ep20
      elseif(sl < zero) then
         func_sl = nint(abs(sl))
         matparam%ntable  = matparam%ntable + 1
         func(matparam%ntable) = func_sl
      endif    
      if(gxt == zero) then
        gxt = ep20
      elseif(gxt < zero) then
        func_gxt = nint(abs(gxt))
        matparam%ntable  = matparam%ntable + 1
         func(matparam%ntable) = func_gxt
      endif
      if(gxc == zero) then
        gxc = ep20
      elseif(gxc < zero) then
        func_gxc = nint(abs(gxc))
        matparam%ntable  = matparam%ntable + 1
         func(matparam%ntable) = func_gxc
      endif   
      if(gyt == zero) then
        gyt = ep20
      elseif(gyt < zero) then
        func_gyt = nint(abs(gyt))
        matparam%ntable  = matparam%ntable + 1
         func(matparam%ntable) = func_gyt
      endif
      if(gyc == zero) then
        gyc = ep20
      elseif(gyc < zero) then
            func_gyc = nint(abs(gyc))
            matparam%ntable  = matparam%ntable + 1
            func(matparam%ntable) = func_gyc
      endif
      if(gsl == zero) then
        gsl = ep20
      elseif(gsl < zero) then
            func_gsl = nint(abs(gsl))
            matparam%ntable  = matparam%ntable + 1
            func(matparam%ntable) = func_gsl
      endif
    
      if(xt0 < zero) then
        func_xt0 = nint(abs(xt0))
        matparam%ntable  = matparam%ntable + 1
        func(matparam%ntable) = func_xt0
      endif   
     
      if(xc0 < zero ) then
        func_xc0 = nint(abs(xc0))
        matparam%ntable  = matparam%ntable + 1
        func(matparam%ntable) = func_xc0
      endif 
      
      if(gxt0 < zero) then
        func_gxt0 = nint(abs(gxt0))
        matparam%ntable  = matparam%ntable + 1
         func(matparam%ntable) = func_gxt0
      endif
      if(gxc0 < zero) then
        func_gxc0 = nint(abs(gxc0))
        matparam%ntable  = matparam%ntable + 1
         func(matparam%ntable) = func_gxc0
      endif   
      !
      if(ang0 == zero ) ang0 = 53  ! 53°
      ang0 = ang0*pi/HUNDRED80
 !---------------------------------------------------------------------------------------------
 !                                filling buffer tables
 !---------------------------------------------------------------------------------------------
      ! number of material parameters
      matparam%nuparam = 60
      allocate (matparam%uparam(matparam%nuparam))
      matparam%uparam(1:matparam%nuparam) = zero
      ! number of user variables 
      nuvar   = 20
      ! number of temporary variable for interpolation
      nvartmp =  matparam%ntable
      ! material parameters
      matparam%uparam(1)  = e1
      matparam%uparam(2)  = e2
      matparam%uparam(3)  = e3
      matparam%uparam(4)  = g12
      matparam%uparam(5)  = g13
      matparam%uparam(6)  = g23
      !
      matparam%uparam(7)  = nu12
      matparam%uparam(8)  = nu21
      matparam%uparam(9)  = nu13
      matparam%uparam(10)  = nu31
      matparam%uparam(11)  = nu23
      matparam%uparam(12)  = nu32        
      !
      !!g_ratio = min(one,gxc/gyc)
      g_ratio = gyc/gxc ! to check 
      !
      matparam%uparam(13)  = xt
      matparam%uparam(14)  = xc
      matparam%uparam(15)  = yt
      matparam%uparam(16)  = yc
      matparam%uparam(17)  = sl
      matparam%uparam(18)  = xt0
      matparam%uparam(19)  = xc0
     !
      matparam%uparam(20)  = gxt
      matparam%uparam(21)  = gxc
      matparam%uparam(22)  = gyt
      matparam%uparam(23)  = gyc
      matparam%uparam(24)  = gsl
      matparam%uparam(25)  = gxt0
      matparam%uparam(26)  = gxc0 !
      !
      matparam%uparam(27)  = ang0  
      !
      matparam%uparam(28)  = sigy
      matparam%uparam(29)  = beta
      matparam%uparam(30)  = etan
      !
      matparam%uparam(31)  = d11
      matparam%uparam(32)  = d22
      matparam%uparam(33)  = d33
      matparam%uparam(34)  = d12
      matparam%uparam(35)  = d13
      matparam%uparam(36)  = d23
      !
      matparam%uparam(37)  = epsf23
      matparam%uparam(38)  = epsr23
      matparam%uparam(39)  = tsmd23
      matparam%uparam(40)  = epsf31
      matparam%uparam(41)  = epsr31
      matparam%uparam(42)  = tsmd31
      !
      matparam%uparam(43)  = ef11t
      matparam%uparam(44)  = ef11c
      matparam%uparam(45)  = ef22t
      matparam%uparam(46)  = ef22c
      matparam%uparam(47)  = ef12
      matparam%uparam(48)  = ef31
      matparam%uparam(49)  = ef23
      !
      matparam%uparam(50)  = cf12
      matparam%uparam(51)  = cf31
      matparam%uparam(52)  = cf23   
      ! 
      eta_l = -sl*cos(two*ang0)/yc/cos(ang0)**2
      eta_t = -one/tan(two*ang0)
      st    = yc*cos(ang0)*(sin(ang0) + cos(ang0)/tan(two*ang0))
      tmp = sl/xc
      theta_c = atan((one - sqrt(one - four*(tmp + eta_l)*tmp))  &
                                     / (two*(tmp + eta_l)))
      matparam%uparam(53) = eta_l
      matparam%uparam(54) = eta_t
      matparam%uparam(55) = st
      matparam%uparam(56) = theta_c
      matparam%uparam(57) = g_ratio
      matparam%uparam(58)  = ratio  ! 
      ! copy fonction in table
      matparam%nfunc  = 0
      allocate (matparam%table(matparam%ntable))           ! allocate material table array
!
      nfunc = 1
      x1scale   = one
      x2scale   = one
      x2vect(:) = zero
      do i=1,matparam%ntable  
         matparam%table(i)%notable  = func(i)
         ifunc(1)  = matparam%table(i)%notable
         yscale(1) = one   ! we should take care of the scale 
        call func_table_copy(matparam%table(i),matparam%title ,matparam%mat_id  ,     &
                             nfunc   ,ifunc   ,x2vect  ,x1scale ,x2scale  ,yscale  ,     &
                             ntable  ,table   ,ierr    )
      enddo    
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
!-----------------------------------------
          ! parameters used outside the law
!-----------------------------------------
    ! pm table
       pm(1)  = rho0
       pm(89) = rho0
       pm(9)  = asrate            !  mat_param%asrate
       pm(20) = young             !  mat_param%young
       pm(21) = nu                !  mat_param%nu
       pm(22) = max(g12,g12,g23)  !  mat_param%shear
       pm(24) = a11
       pm(26) = five_over_6
       pm(27) = ssp     !  mat_param%ssp
       pm(32) = c1                !  mat_param%bulk

      ! still used in elements
        pm(33) = e1                !  mat_param%e11
        pm(34) = e2                !  mat_param%e22
        pm(186)= e3                !  mat_param%e33
        pm(35) = nu12              !  mat_param%n12
        pm(36) = nu21              !  mat_param%n21
        pm(37) = g12               !  mat_param%g12
        pm(38) = g23               !  mat_param%g23
        pm(39) = g31               !  mat_param%g31   

          ! parmat table
        israte     = 1
        parmat(1)  = c1
        parmat(2)  = young
        parmat(3)  = nu
        parmat(4)  = israte
        parmat(5)  = fcut
        parmat(16) = 1
        ! matparam structure
       matparam%rho0     = rho0
       matparam%young    = young
       matparam%nu       = nu
       matparam%shear    = gmax
       matparam%bulk     = c1        
!     
      ! mtag variable activation
       mtag%g_pla  = 1
       mtag%l_pla  = 1
       mtag%g_epsd = 1
       mtag%l_epsd = 1
       mtag%g_seq  = 1
       mtag%l_seq  = 1
       mtag%g_dmg  = 1 
       mtag%l_dmg  = 8
     ! number of output mod
      ! -> Stored in DMG(:,2:4)
       matparam%nmod = 4
       allocate(matparam%mode(matparam%nmod))
       matparam%mode(1) = "Tension Fiber Damage "
       matparam%mode(2) = "Fiber Kinking Damage"
       matparam%mode(3) = "Transverse Matrix Damage in Tension "
       matparam%mode(4) = "Transverse Matrix Damage in Compression"
!
      call init_mat_keyword(matparam ,"ELASTO_PLASTIC")
      call init_mat_keyword(matparam ,"TOTAL"   )
      call init_mat_keyword(matparam ,"HOOK")
      call init_mat_keyword(matparam ,"ORTHOTROPIC")
!
      ! properties compatibility
      call init_mat_keyword(matparam,"SHELL_ORTHOTROPIC")
      call init_mat_keyword(matparam,"SOLID_ORTHOTROPIC")
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
        write(iout,1400)  xt,xc,yt,yt,sl, ang0
        write(iout,1500)  gxt,gxc,gyt,gyc,gsl
        write(iout,1600) xt0,xc0,gxt0,gxc0
        write(iout,1700) sigy, beta, etan, func_sc
        write(iout,1750) epsf23,epsr23,tsmd23,epsf31,epsr31,tsmd31
        write(iout,1760) ef11t,ef11c,ef22t,ef22c,ef12,ef31,epsf23
        write(iout,1770) cf12,cf31,cf23
        write(iout,1800) fcut
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
           7x,'young modulus in dir. 1 (fiber)  e1 . . . . . . .=',1pg20.13/     &
           7x,'young modulus in dir. 2 (matrix) e2 . . . . . . .=',1pg20.13/     &
           7x,'young modulus in dir. 3 (matrix) e3 . . . . . . .=',1pg20.13/     &
           7x,'shear modulus in plane 12 g12 . . . . . . . . . .=',1pg20.13/     &
           7x,'shear modulus in plane 23 g23 . . . . . . . . . .=',1pg20.13/     &
           7x,'shear modulus in plane 31 g13 . . . . . . . . . .=',1pg20.13/     &
           7x,'poisson ratio in plane 12 nu12. . . . . . . . . .=',1pg20.13/     &
           7x,'poisson ratio in plane 23 nu23. . . . . . . . . .=',1pg20.13/     &
           7x,'poisson ratio in plane 31 nu13. . . . . . . . . .=',1pg20.13)
 1400 format(                                                                     &
           5x,' damage parameters   :                            ',/          &
           5x,'---------------------------                               ',/          &
           7x,'longitudinal tensile strength  . . . .  . . . . . . . =',1pg20.13/     &
           7x,'longitudinal compressive strength  . . . .  . . . . . =',1pg20.13/     &  
           7x,'transverse tensile strength  . . . .  . . . . . . . . =',1pg20.13/     &
           7x,'transverse compressive strength  . . . .  . . . . . . =',1pg20.13/     &
           7x,'shear strength    . . . . . . . . . . . . . . . . . . =',1pg20.13/     &
           7x,'Fracture angle in pure transverse compression (default = 53.0°) = ',1pg20.13/)    
 1500 format(                                                                     &
           5x,' Fracture toughnesse  parameters   :                      ',/          &
           5x,'-----------------------------------                       ',/          &
           7x,'Fracture toughness for longitudinal (fiber) compressive failure  mode. =',1pg20.13/     &
           7x,'Fracture toughness for longitudinal (fiber) tensile failure mode       =',1pg20.13/     &  
           7x,'Fracture toughness for intralaminar matrix tensile failure.            =',1pg20.13/     &
           7x,'Fracture toughness for intralaminar matrix transverse shear failure    =',1pg20.13/     &
           7x,'Fracture toughness for intralaminar matrix longitudinal shear failure. =',1pg20.13/     )
 1600 format(                                                                     &
           5x,' bilinear damage parameters  :                            ',/          &
           5x,'---------------------------                               ',/          &
           7x,'longitudinal tensile strength  . . . .  . . . . . . . =',1pg20.13/     &
           7x,'longitudinal compressive strength  . . . .  . . . . . =',1pg20.13/     &  
           7x,'Fracture toughness for longitudinal (fiber) tensile failure  mode. =',1pg20.13/     &
           7x,'Fracture toughness for longitudinal (fiber) transverse failure mode       =',1pg20.13/     )      
 1700 format(                                                                   &                                                              
           5x,' plasticity parameters  :                ',/        &
           5x,'---------------------------                               ',/        &
           7x,'In-plane shear yield stress (only used when BETA < 1.0) . . .       =',1pg20.13/    &
           7x,'Hardening parameter for in-plane shear plasticity (0.0 ≤ BETA ≤ 1.0 =',1pg20.13/    &
           7x,'Tangent modulus for in-plane shear plasticity . . . . . . . . . . . =',1pg20.13/    &
           7x,'Load curve ID stress vs strain   . .  . . . . . . . . . . . . . . . ='i10/           )
 1750 format(                                                                   &                                                              
           5x,' out-of-plane matrix failure parameters  :                ',/        &
            5x,'-------------------------------------                       ',/        &
            7x,'Damage initiation transverse shear strain in 2-3 plane . . . . . .  =',1pg20.13/    &
            7x,'Final rupture transverse shear strain in 2-3 plane . . . . . . . . =',1pg20.13/    &
            7x,'Transverse shear maximum damage in 2-3 plane   . . . . . . . . . . =',1pg20.13/    &
            7x,'Damage initiation transverse shear strain in 3-1 plane . . . . . .  =',1pg20.13/    &
            7x,'Final rupture transverse shear strain in 3-1 plane . . . . . . . . =',1pg20.13/    &
            7x,'Transverse shear maximum damage in 3-1 plane   . . . . . . . . . . =',1pg20.13/    )
1760 format(                                                                   &     
            5x,' failure strain parameters  :                ',/        &
            5x,'-------------------------------------                       ',/        &
            7x,'Tensile failure strain  in 1-1 direction . . . . .  . . . . . .=',1pg20.13/    &
            7x,'compressive failure strain  in 1-1 direction  . . . . . . . . .=',1pg20.13/    &
            7x,'Tensile failure strain  in 2-2 direction . . . . .  . . . . . .=',1pg20.13/    &
            7x,'compressive failure strain  in 2-2 direction  . . . . . . . . .=',1pg20.13/    &
            7x,'In-plane shear failure strain in 1-2 direction  . . . . . . . .=',1pg20.13/    &
            7x,'Out-of-plane shear failure strain in 3-1 direction  . . . . . .=',1pg20.13/    &
            7x,'Out-of-plane shear failure strain in 2-3 direction  . . . . . .=',1pg20.13/)
1770 format(                                                                   &
            5x,' Coupling factor  parameters  :                ',/        &
            5x,'-------------------------------------                       ',/        &
            7x,'Coupling factor for in-plane shear in 1-2 direction .. . . . . .=',1pg20.13/    &
            7x,'Coupling factor for out-of-plane shear in 3-1 direction .. . . .=',1pg20.13/    &
            7x,'Coupling factor for out-of-plane shear in 2-3 direction .. . . .=',1pg20.13/)

 1800 format(                                                                    &
            5x,'strain rate filtering cutoff frequency fcut . . .=',1pg20.13/)
!-----------------------------------------------------------------------
        end subroutine hm_read_mat132
!-------------------
      end module hm_read_mat132_mod
