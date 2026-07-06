!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
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
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    hm_read_mat123_mod   ../starter/source/materials/mat/mat123/hm_read_mat123.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat          ../starter/source/materials/mat/hm_read_mat.F90
!||====================================================================
      module hm_read_mat123_mod
      contains
! ======================================================================================================================
! \brief Reading material parameters of /MAT/LAW123
! \details Reading material parameters of /MAT/LAW123
! ======================================================================================================================
!||====================================================================
!||    hm_read_mat123           ../starter/source/materials/mat/mat123/hm_read_mat123.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F90
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    func_table_copy          ../starter/source/materials/tools/func_table_copy.F90
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    init_mat_keyword         ../starter/source/materials/mat/init_mat_keyword.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    func_table_copy_mod      ../starter/source/materials/tools/func_table_copy.F90
!||    mat_table_copy_mod       ../starter/source/materials/tools/mat_table_copy.F90
!||    mat_table_table_copy_mod ../starter/source/materials/tools/mat_table_table_copy.F90
!||    extract_table_plas_mod    ../starter/source/materials/tools/extract_table_plas.F90
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||    table_mod                ../starter/share/modules1/table_mod.F
!||====================================================================
        subroutine hm_read_mat123(                                      &
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
          use mat_table_table_copy_mod
          use MY_ALLOC_MOD
          use precision_mod, only : WP
          use extract_table_plas_mod
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
          integer ilaw,tab_sc,tab_ena,         &
            tab_enkink,tab_enb,tab_ent,tab_enl,func_sl,func_xt,func_xc, &
            func_yt,func_yc,nfunc,i,j,ierror,ndim,npt, npt_new

          integer , dimension(maxfunc) :: ifunc,func,itab
          real(kind=WP)                                                 &
            rho0,e1,e2,e3,g12,g23,g13,nu12,nu21,nu23,nu31,nu13,         & 
            xc,xt,yc,yt,sl,nu32, enkink,ena,enb,ent,enl,st,mut,mul,     &
            a11,a22,a12,c11,c22,c33,c12,c13,c23, detc, scale(maxfunc) ,       &
            d11,d22,d33,d12,d13,d23,dmn,dmx,g31, fcut ,c1,gmax,ssp,nu,   &
            young,asrate,fac,ang0,chard,yscale(1),x1scale,x2scale,    &
            x2vect(maxfunc),thetac,efs,ratio,aa,bb,scale_sc,unit_stress,  &
            unit_ener,scale_tab(maxfunc),scale_sr,x3scale,x4scale,xscale_xr, &
            scale_lgth, unit_lgth,unit_sr,x0,y0,x1,y1,epsp,yld0

          real(kind=WP) , allocatable, dimension(:) :: xp, yld
          !
          logical :: is_available,is_encrypted
          !=======================================================================
          is_encrypted = .false.
          is_available = .false.
          ilaw         = 123
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
!card5  strenght function
          call hm_get_intv  ('ITAB_ENK'    ,tab_enkink    ,is_available, lsubmodel) 
          call hm_get_intv  ('ITAB_ENA'    ,tab_ena     ,is_available, lsubmodel)
          call hm_get_intv  ('ITAB_ENB'    ,tab_enb     ,is_available, lsubmodel)  
          call hm_get_intv  ('ITAB_ENT'    ,tab_ent     ,is_available, lsubmodel)
          call hm_get_intv  ('ITAB_ENL'    ,tab_enl     ,is_available, lsubmodel)  
           call hm_get_floatv ('SCALE_XTAB'       , scale_lgth      ,is_available, lsubmodel, unitab)   
!card6- strenght energy
          call hm_get_floatv  ('LSD_ENKINK'    ,enkink        ,is_available, lsubmodel, unitab)
          call hm_get_floatv  ('LSD_ENA'       ,ena         ,is_available, lsubmodel, unitab)
          call hm_get_floatv  ('LSD_ENB'       ,enb         ,is_available, lsubmodel, unitab)
          call hm_get_floatv  ('LSD_ENT'       ,ent         ,is_available, lsubmodel, unitab)
          call hm_get_floatv  ('LSD_ENL'       ,enl        ,is_available, lsubmodel, unitab)
!card7  strenght function
          call hm_get_intv  ('IFUN_XT'    ,func_xt     ,is_available, lsubmodel) 
          call hm_get_intv  ('IFUN_XC'    ,func_xc     ,is_available, lsubmodel)
          call hm_get_intv  ('IFUN_YT'    ,func_yt     ,is_available, lsubmodel)  
          call hm_get_intv  ('IFUN_YC'    ,func_yc     ,is_available, lsubmodel)
          call hm_get_intv  ('IFUN_SL'    ,func_sl     ,is_available, lsubmodel)  

          call hm_get_floatv ('SCALE_RS'       , scale_sr      ,is_available, lsubmodel, unitab)       
!card8 strenght
          call hm_get_floatv  ('LSD_MAT_XT'       ,xt         ,is_available, lsubmodel, unitab)
          call hm_get_floatv  ('LSD_XC'           ,xc         ,is_available, lsubmodel, unitab)
          call hm_get_floatv  ('LSD_MAT_YT'       ,yt         ,is_available, lsubmodel, unitab)
          call hm_get_floatv  ('LSD_MAT_YC'       ,yc         ,is_available, lsubmodel, unitab)
          call hm_get_floatv  ('LSD_SL'           ,sl        ,is_available, lsubmodel, unitab)
!card13 - shear 13 for solid
          call hm_get_floatv  ('LSD_FIO'          ,ang0         ,is_available, lsubmodel, unitab)
          call hm_get_floatv  ('SCALE_SC'         ,scale_sc       ,is_available, lsubmodel, unitab)
          call hm_get_floatv  ('LSD_MAT_CHARD'    ,chard      ,is_available, lsubmodel, unitab)
          call hm_get_floatv  ('EFS'             ,efs      ,is_available, lsubmodel, unitab)
!card
          call hm_get_intv  ('LSD_ITABS'    ,tab_sc     ,is_available, lsubmodel)           
!card? - equivalent strain rate cutoff frequency
          call hm_get_floatv('fcut'      ,fcut     ,is_available, lsubmodel, unitab)
!-----------------------------
! unit 
          CALL HM_GET_FLOATV_DIM('LSD_MAT_XT' ,unit_stress    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV_DIM('LSD_ENA' ,unit_ener    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV_DIM('SCALE_XTAB' ,unit_lgth    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV_DIM('SCALE_RS' ,unit_sr    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
! ----------------------------------
          ! young modulus initialization
          if (e2 == zero)  e2  = e1
          if (e3 == zero)  e3  = e2
          ! shear modulus
          if (g13 == zero) g13 = g12
          if (g23 == zero) g23 = g13
          if(nu31 == zero ) nu31 = nu21
          if(nu32 == zero ) nu32 = nu21
          if(efs <= zero) efs = ep20
          if(scale_sr == zero) scale_sr = unit_sr
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
            call ancmsg(msgid=307,              &
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
            call ancmsg(msgid=307,                         &
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
          if(scale_sc == zero) scale_sc = unit_stress
          if(scale_lgth == zero) scale_lgth = unit_lgth
!
          matparam%ntable = 12
          func(1:5) = 0
          itab(1:6) = 0
          !!stop
          if(tab_sc > 0) then
             itab(1) = tab_sc
             scale_tab(1) = scale_sc 
          else
            itab(1) = 0
            scale_tab(1) = one 
          endif
          if(tab_enkink == 0) then
           if(enkink == zero ) enkink = ep20
          else
            itab(2) = tab_enkink
            if(enkink == zero) enkink = unit_ener
            scale_tab(2) = unit_ener
          endif
          if(tab_ena == 0) then
           if(ena == zero)  ena = ep20
          else
            itab(3) = tab_ena
            if(ena == zero) ena = unit_ener
            scale_tab(3) = ena
          endif
          if(tab_enb == 0) then
           if(enb == zero)  enb = ep20
          else
            itab(4) = tab_enb
            if(enb == zero) enb = unit_ener
            scale_tab(4) = enb
          endif
          if(tab_ent == 0) then
            if(ent == zero)  ent = ep20
          else
            itab(5) = tab_ent
            if(ent == zero) ent = unit_ener
            scale_tab(5) = ent
          endif
          if(tab_enl == 0) then
            if(enl == zero ) enl = ep20
          else
            itab(6) = tab_enl
            if(enl == zero ) enl = unit_ener
            scale_tab(6) = enl
          endif
          ! functions for strenght
          if(func_xt == 0 ) then
            if(xt == zero) xt = ep20
          else
            func(1) = func_xt
            if(xt == zero) xt = unit_stress
            scale(1) = xt 
          endif
          if(func_xc == 0 )then
            if(xc == zero) xc = ep20
          else
            func(2) = func_xc
            if(xc == zero) xc= unit_stress
            scale(2) = xc 
          endif
          if( func_yt == 0 ) then
            if(yt == zero) yt = ep20
          else
            func(3) = func_yt
            if(yt == zero) yt=unit_stress
            scale(3) = yt
          endif
          if( func_yc == zero ) then
           if(yc == zero)  yc = ep20
          else
            func(4) = func_yc
            if(yc == zero) yc=unit_stress
            scale(4) = yc
          endif
          if(func_sl == zero ) then
            if(sl== zero) sl = ep20
          else
            func(5) = func_sl
            if(sl == zero) sl=unit_stress
            scale(5) = sl
          endif
          if(ang0 == zero ) ang0 = 53  ! 53°
          ang0 = ang0*pi/HUNDRED80
          fac = one/tan(ang0)
          st = half*fac*yc
          mut = -one/tan(two*ang0)
          mul = sl*mut/st
          aa = two*(sl/xc + mul)
          bb = one - two*aa*sl/xc
          if( bb  < zero) then
            thetac = sl/g12
          else
            thetac = (one - sqrt(max(em20,bb)) ) / aa
            thetac = atan(thetac)   ! used for misalignment angle
          endif
          !---------------------------------------------------------------------------------------------
          !                                filling buffer tables
          !---------------------------------------------------------------------------------------------
          ! number of material parameters
          matparam%nuparam = 38
          call my_alloc(matparam%uparam, matparam%nuparam, "matparam%uparam")
          ! number of user variables
          nuvar   = 17
          ! number of temporary variable for interpolation
          nvartmp =  17 ! (6 tables * 2dim + 5)
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
          matparam%uparam(13)  = xt
          matparam%uparam(14)  = xc
          matparam%uparam(15)  = yt
          matparam%uparam(16)  = yc
          matparam%uparam(17)  = sl
          !
          matparam%uparam(18)  = enkink
          matparam%uparam(19)  = ena
          matparam%uparam(20)  = enb
          matparam%uparam(21)  = ent
          matparam%uparam(22)  = enl
          !
          matparam%uparam(23)  = st
          matparam%uparam(24)  = mut
          matparam%uparam(25)  = mul
          !
          matparam%uparam(26)  = ang0
          matparam%uparam(27)  = thetac
          matparam%uparam(28)  = zero  ! misalignment angle : updated in law123_upd
          matparam%uparam(30)  = chard

          matparam%uparam(31)  = d11
          matparam%uparam(32)  = d22
          matparam%uparam(33)  = d33
          matparam%uparam(34)  = d12
          matparam%uparam(35)  = d13
          matparam%uparam(36)  = d23
          matparam%uparam(37)  = zero ! not used 
          matparam%uparam(38)  = efs
          !
          ! copy fonction in table
          matparam%nfunc  = 0
          allocate (matparam%table(matparam%ntable))           ! allocate material table array
!
          nfunc = 1
          do i=1,matparam%ntable  - 1 !
            if(i <= 6) then  ! tables 
               matparam%table(i)%notable  = itab(i)
               if(itab(i) > 0 ) then
                  ifunc(1)  =  matparam%table(i)%notable !
                  yscale(1) = scale_tab(i)  
                  x1scale = scale_lgth
                  x2scale = scale_sr ! check the unit from the table 
                  x3scale = one
                  x4scale = one
                  call mat_table_table_copy(                                       &                                    
                  matparam%table(i)  ,ifunc(1),matparam%title,matparam%mat_id , &
                  x1scale  ,x2scale   ,x3scale  ,x4scale  ,                        &             
                  yscale(1)  ,ntable    ,table    ,ierr    )
               endif
            else  ! functions
               j= i-6
               matparam%table(i)%notable  = func(j)
               if(func(j) > 0 ) then
                  ifunc(1)  = matparam%table(i)%notable
                  yscale(1) = scale(j)  
                  x1scale   = scale_sr 
                  x1scale   = one
                  x2scale   = one
                  x2vect(:) = zero
                  call func_table_copy(matparam%table(i),matparam%title ,matparam%mat_id  ,     &
                  nfunc   ,ifunc   ,x2vect  ,x1scale ,x2scale  ,yscale  ,     &
                  ntable  ,table   ,ierr    )
               endif
            endif
          enddo
          ! checking shear stress strain table
          if (matparam%table(1)%notable > 0) then
              ndim = matparam%table(1)%ndim
              ierror = 0
              if (ndim == 1) then
                y0 = minval(matparam%table(1)%y1d )
                x0 = matparam%table(1)%x(1)%values(1)
                if (y0/= zero .or. x0 /= zero) ierror = 1
              else if (ndim == 2) then
                x0 = matparam%table(1)%x(1)%values(1)
                if (maxval(matparam%table(1)%y2d(1,:)) /= zero .or. x0/= zero) ierror = 1
              end if
              if (ierror == 1) call ancmsg(msgid=3167,        &
                                       msgtype=msgerror,      &
                                       anmode=aninfo_blind_1, &
                                        i1=matparam%mat_id,   &
                                        c1=matparam%title, i2=itab(1))  
          end if
          ! extract taux(gama_plas) from taux(gama_total)
          ndim =matparam%table(1)%ndim
          npt  = size(matparam%table(1)%x(1)%values)
          npt_new = 0
          yld0 = zero
          matparam%table(12)%notable = 0
          if(ndim > 2) then
              call ancmsg(msgid=3168,          &
                      msgtype=msgerror,       &
                      anmode=aninfo_blind_1,   &
                      i1=mat_id,               &
                      c1=titr,                 &
                      i2=itab(1))
          endif
          if(matparam%table(1)%notable > 0) then
            matparam%table(12)%notable = matparam%table(1)%notable
            call extract_table_plas(matparam%table(1), matparam%table(12), g12, yld0, npt_new )
          endif 
          matparam%uparam(29)  = yld0
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
          pm(20) = young               !  mat_param%young
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
          matparam%nmod = 3
          allocate(matparam%mode(matparam%nmod))
          matparam%mode(1) = "Tension Fiber Damage "
          matparam%mode(2) = "Fiber Kinking Damage"
          matparam%mode(3) = "Transverse Matrix Damage"
!
          call init_mat_keyword(matparam ,"ELASTO_PLASTIC")
          call init_mat_keyword(matparam ,"incremental"   )
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
            write(iout,1400) 
            if(func_xt > 0 ) then
              write(iout,1401) func_xt, xt
            else
              write(iout,1402) xt
            endif
            if(func_xc > 0) then
              write(iout,1403) func_xc,xc
            else
              write(iout,1404) xc
            endif
             if(func_yt > 0 ) then
              write(iout,1405) func_yt, yt
            else
              write(iout,1406) yt
            endif
            if(func_yc > 0) then
              write(iout,1407) func_yc,yc
            else
              write(iout,1408) yc
            endif
            if(func_sl > 0) then
              write(iout,1409) func_sl,sl
            else
              write(iout,1410) sl
            endif
            write(iout,1420)  ang0
            write(iout,1500) 
            !
            if(tab_enkink > 0) then
              write(iout,1501) tab_enkink, enkink
            else
              write(iout,1502) enkink
            endif
            if(tab_ena > 0 ) then
               write(iout, 1503) tab_ena, ena
            else
               write(iout,1504) ena
            endif
            if(tab_enb > 0 ) then
               write(iout, 1505) tab_enb, enb
            else
                write(iout, 1506) enb
            endif
            if(tab_ent > 0 ) then
                write(iout, 1507) tab_ent, ent
            else
                write(iout,1508) ent 
            endif
            if(tab_enl > 0 ) then
                write(iout,1509) tab_enl, enl
            else
                write(iout,1510) enl
            endif
            if(tab_sc > 0) then
              write(iout,1600) chard, tab_sc,scale_sc
            endif 
            write(iout,1620) efs
            write(iout,1700) fcut
          endif
!-----------------------------------------------------------------------
1000      FORMAT(/                                                               &
            5X,A,/,                                                               &
            5X,'MATERIAL NUMBER. . . . . . . . . . . . =',I10/,                   &
            5X,'MATERIAL LAW . . . . . . . . . . . . . =',I10/)
1050      FORMAT                                                                 &
            (5X,'MATERIAL MODEL : LAMINATED COMPOSITE ',/,                         &
            5X,'----------------------------------',/)
1200      FORMAT(                                                                &
            5X,'INITIAL DENSITY . . . . . . . . . . . . . . . . .=',1PG20.13/)
1300      FORMAT(                                                                &
            5X,'ELASTICITY PARAMETERS:                            ',/             &
            5X,'----------------------                            ',/             &
            5X,'YOUNG MODULUS IN DIR. 1 (FIBER)  E1 . . . . . . .=',1PG20.13/     &
            5X,'YOUNG MODULUS IN DIR. 2 (MATRIX) E2 . . . . . . .=',1PG20.13/     &
            5X,'YOUNG MODULUS IN DIR. 3 (MATRIX) E3 . . . . . . .=',1PG20.13/     &
            5X,'SHEAR MODULUS IN PLANE 12 G12 . . . . . . . . . .=',1PG20.13/     &
            5X,'SHEAR MODULUS IN PLANE 23 G23 . . . . . . . . . .=',1PG20.13/     &
            5X,'SHEAR MODULUS IN PLANE 31 G13 . . . . . . . . . .=',1PG20.13/     &
            5X,'POISSON RATIO IN PLANE 12 NU12. . . . . . . . . .=',1PG20.13/     &
            5X,'POISSON RATIO IN PLANE 23 NU23. . . . . . . . . .=',1PG20.13/     &
            5X,'POISSON RATIO IN PLANE 31 NU13. . . . . . . . . .=',1PG20.13)
1400      FORMAT(  /                                                                  &
            5X,' DAMAGE PARAMETERS   :                            ',/                 &
            5X,'---------------------                               ',/         )
1401      FORMAT(                                                                     &
            7X,' CURVE ID OF THE LONGITUDINAL TENSILE STRENGTH . . .  =',I10/         &
            7X,' FUNCTION SCALE FACTOR . . . . . . . . . . . . . .. =',1PG20.13 /  )            
1402      FORMAT(                                                                  &
            7X,'LONGITUDINAL TENSILE STRENGTH  . . . .  . . . . . . . =',1PG20.13   )
1403      FORMAT(                                                                   &
            7X,' CURVE ID OF THE LONGITUDINAL COMPRESSIVE STRENGTH. .  =',I10/         &
            7X,' FUNCTION SCALE FACTOR . . . . . . . . . . . . . . . =',1PG20.13 /) 
1404      FORMAT(                                                                     & 
            7X,'LONGITUDINAL COMPRESSIVE STRENGTH  . . . .  . . . . . =',1PG20.13    )
1405      FORMAT(                                                                   &
            7X,' CURVE ID OF THE TRANSVERSE TENSILE STRENGTH . . . .  =',I10/         &
            7X,' FUNCTION SCALE FACTOR . . . . . . . . . . . . . .. =',1PG20.13  / )   
1406       FORMAT(& 
            7X,'TRANSVERSE TENSILE STRENGTH  . . . .  . . . . . . . . =',1PG20.13    )
1407      FORMAT(                                                                   &
            7X,' CURVE ID OF THE TRANSVERSE COMPRESSIVE STRENGTH. .  =',I10/         &
            7X,' FUNCTION SCALE FACTOR . . . . . . . . . . . . . . . =',1PG20.13  / )
1408       FORMAT(& 
            7X,'TRANSVERSE COMPRESSIVE STRENGTH  . . . .  . . . . . . =',1PG20.13   )
1409      FORMAT(                                                                   &
            7X,' CURVE ID OF THE SHER  STRENGTH. . . . . . . . . . .  =',I10/         &
            7X,' FUNCTION SCALE FACTOR . . . . . . . . . . . . . . . =',1PG20.13 / )           
1410       FORMAT(& 
            7X,'SHEAR STRENGTH    . . . . . . . . . . . . . . . . . . =',1PG20.13    )
1420       FORMAT(& 
            7X,'FRACTURE ANGLE IN PURE TRANSVERSE COMPRESSION (DEFAULT = 53.0°) = ',1PG20.13/)

1500      FORMAT(                                                                     &
            5X,' FRACTURE TOUGHNESSE  PARAMETERS   :                      ',/          &
            5X,'-----------------------------------                       ',/          )
1501      FORMAT(                                                                     &
            7X,' TABLE ID OF THE FRACTURE TOUGHNESS FOR LONGITUDINAL (FIBER) COMPRESSIVE FAILURE  MODE  =',I10/         &
            7X,' FUNCTION SCALE FACTOR . . . . . . . . . . . . . .. =',1PG20.13 /  )   
1502      FORMAT( &
            7X,'FRACTURE TOUGHNESS FOR LONGITUDINAL (FIBER) COMPRESSIVE FAILURE  MODE. =',1PG20.13     )
1503      FORMAT(                                                                     &
            7X,' TABLE ID OF FRACTURE TOUGHNESS FOR LONGITUDINAL (FIBER) TENSILE FAILURE MODE . . . ..  =',I10/         &
            7X,' FUNCTION SCALE FACTOR . . . . . . . . . . . . . .. =',1PG20.13 /  )   
1504      FORMAT( & 
            7X,'FRACTURE TOUGHNESS FOR LONGITUDINAL (FIBER) TENSILE FAILURE MODE       =',1PG20.13     )
1505      FORMAT(                                                                     &
            7X,' TABLE ID OF THE FRACTURE TOUGHNESS FOR INTRALAMINAR MATRIX TENSILE FAILURE . . . . .  =',I10/         &
            7X,' FUNCTION SCALE FACTOR . . . . . . . . . . . . . .. =',1PG20.13 /  ) 
1506      FORMAT( &
            7X,'FRACTURE TOUGHNESS FOR INTRALAMINAR MATRIX TENSILE FAILURE.            =',1PG20.13 )
1507       FORMAT(                                                                     &
            7X,' TABLE ID OF THE FRACTURE TOUGHNESS FOR INTRALAMINAR MATRIX TRANSVERSE SHEAR FAILURE .  =',I10/         &
            7X,' FUNCTION SCALE FACTOR . . . . . . . . . . . . . .. =',1PG20.13 /  )    
1508      FORMAT( & 
            7X,'FRACTURE TOUGHNESS FOR INTRALAMINAR MATRIX TRANSVERSE SHEAR FAILURE    =',1PG20.13 )
1509      FORMAT(                                                                     &
            7X,' TABLE ID OF THE FRACTURE TOUGHNESS FOR INTRALAMINAR MATRIX LONGITUDINAL SHEAR FAILURE  =',I10/         &
            7X,' FUNCTION SCALE FACTOR . . . . . . . . . . . . . .. =',1PG20.13 /   )           
1510      FORMAT(& 
            7X,'FRACTURE TOUGHNESS FOR INTRALAMINAR MATRIX LONGITUDINAL SHEAR FAILURE. =',1PG20.13 /    )
1600      FORMAT(                                                                   &
            5X,' PLASTICITY PARAMETERS  :                ',/        &
            5X,'---------------------------                               ',/        &
            7X,'HARDENING PARAMETER FOR IN-PLANE SHEAR PLASTICITY (0.0 ≤ CHARD ≤ 1.0 =',1PG20.13/    &
            7X,'LOAD TABLE ID STRESS VS STRAIN   . .  . . . . . . . . . . . . . . . =',I10/         &
            7X,'FUNCTION SCALE FACTOR . . . . . . . . . . . . . . .  . . . . . . . .=',1PG20.13 /   )
1610      FORMAT(                                                                   &
            5X,' PLASTICITY PARAMETERS  :                ',/        &
            5X,'---------------------------                               ',/        &
            7X,'IN-PLANE SHEAR YIELD STRESS (ONLY USED WHEN CHARD < 1.0) . . .       =',1PG20.13/    &
            7X,'HARDENING PARAMETER FOR IN-PLANE SHEAR PLASTICITY (0.0 ≤ CHARD ≤ 1.0 =',1PG20.13/    )
1620      FORMAT(                                                                   &
            7X,"EFFECTIVE FAILURE STRAIN . . . . . . . . . . . . . . . . . . . . . .=",1PG20.13 /      )
1700      FORMAT(                                                                    &
            5X,'STRAIN RATE FILTERING CUTOFF FREQUENCY FCUT . . .=',1PG20.13/)
!-----------------------------------------------------------------------
        end subroutine hm_read_mat123
!-------------------
      end module hm_read_mat123_mod
