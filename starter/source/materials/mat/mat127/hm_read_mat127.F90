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
      !||    hm_read_mat127_mod   ../starter/source/materials/mat/mat127/hm_read_mat127.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat          ../starter/source/materials/mat/hm_read_mat.F
      !||====================================================================
      module hm_read_mat127_mod
      contains
! ======================================================================================================================
! \brief Reading material parameters of /MAT/LAW127
! \details Reading material parameters of /MAT/LAW127
! ======================================================================================================================
      !||====================================================================
      !||    hm_read_mat127           ../starter/source/materials/mat/mat127/hm_read_mat127.F90
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
      subroutine hm_read_mat127(                                      &       
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
      integer ifxc,ifxt,ifyc,ifyt,ifsc,ilaw
      my_real                                                         &
         rho0,e1,e2,e3,g12,g23,g13,nu12,nu21,nu23,nu31,nu13,soft,     &
         xc,xt,yc,yt,sc,nu32,  slimt1,slimc1,slimt2,slimc2,slims,     &
         a11,a22,a12,c11,c22,c33,c12,c13,c23, detc,                   &
         d11,d22,d33,d12,d13,d23,dmn,dmx,g31, fcut ,c1,gmax,ssp,nu,   &
         young,asrate,ms13,fac,fbrt,ycfac
      logical :: is_available,is_encrypted
 !=======================================================================
      is_encrypted = .false.
      is_available = .false.
      ilaw         = 127
      g31         = zero  !is not initialized elsewhere
 !------------------------------------------
      call hm_option_is_encrypted(is_encrypted)
 !------------------------------------------
!card1 - Density
      call hm_get_floatv('MAT_RHO'   ,rho0     ,is_available, lsubmodel, unitab)
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
      call hm_get_floatv('LSDYNA_PRCA'  ,nu31    ,is_available, lsubmodel, unitab) 
!card5 - dir 11 tention 
      call hm_get_floatv  ('LSD_MAT_XT'       ,xt         ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_SLIMT1'   ,slimt1     ,is_available, lsubmodel, unitab)
      call hm_get_intv  ('LSD_LCXT'    ,ifxt     ,is_available, lsubmodel)  
! card6 - dir 11 compression 
      call hm_get_floatv  ('LSD_MAT_XC'       ,xc         ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_SLIMC1'   ,slimc1     ,is_available, lsubmodel, unitab)
      call hm_get_intv  ('LSD_LCXC'    ,ifxc  ,is_available, lsubmodel)  
! ccard5 - dir 22 tention 
      call hm_get_floatv  ('LSD_MAT_YT'       ,yt         ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_SLIMT2'   ,slimt2     ,is_available, lsubmodel, unitab)
      call hm_get_intv  ('LSD_LCYT'    ,ifyt     ,is_available, lsubmodel)
! card6 - dir 22 compression 
      call hm_get_floatv  ('LSD_MAT_YC'       ,yc         ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_SLIMC2'   ,slimc2     ,is_available, lsubmodel, unitab)
      call hm_get_intv  ('LSD_LCYC'    ,ifyc  ,is_available, lsubmodel)   
!card11 - shear 12 
      call hm_get_floatv  ('LSD_MAT_SC'      ,sc        ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_SLIMS'    ,slims     ,is_available, lsubmodel, unitab)
      call hm_get_intv  ('LSD_LCSC'    ,ifsc  ,is_available, lsubmodel)   
!card13 - shear 13 for solid 
      call hm_get_floatv  ('LSD_SOFT'     ,soft    ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_FBRT'     ,fbrt       ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_YCFAC'     ,ycfac      ,is_available, lsubmodel, unitab)
!card? - equivalent strain rate cutoff frequency 
      call hm_get_floatv('fcut'      ,fcut     ,is_available, lsubmodel, unitab)

      ! young modulus initialization
      if (e2 == zero)  e2  = e1
      if (e3 == zero)  e3  = e2
      ! shear modulus
      if (g13 == zero) g13 = g12
      if (g23 == zero) g23 = g13
        ! ratio poisson's
      if(nu31 == zero) nu31 = nu21
      if(nu32 == zero) nu32 = nu21
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
 !--------------------------
 !     filling buffer tables
 !-------------------------- 
      ! number of material parameters
      matparam%nuparam = 21

      allocate (matparam%uparam(matparam%nuparam))

      ! number of functions
      nfunc   = 5
      ! number of user variables 
      nuvar   = 2
      ! number of temporary variable for interpolation
      nvartmp = 0
!     
      ! computing alpha and m for each direction
         ! dir 11 (tension - compression)
      if(sc == zero ) sc = ep20
      if(xt == zero ) xt = ep20
      if(xc == zero ) xc = ep20
      if(yt == zero ) yt = ep20
      if(yc == zero ) yc = ep20
      if(soft == zero ) soft= one
      if(fbrt == zero ) fbrt= one
      if(ycfac == zero) ycfac= two
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
       ! not used 
      !matparam%uparam(8)  = nu13
      !matparam%uparam(9)  = nu23
      !!matparam%uparam(9)  = nu31
      !!matparam%uparam(10)  = nu32  
      !
      matparam%uparam(9)   = xt
      matparam%uparam(10)  = slimt1
      matparam%uparam(11)  = xc
      matparam%uparam(12)  = slimc1
     !
      matparam%uparam(13)  = yt
      matparam%uparam(14)  = slimt2
      matparam%uparam(15)  = yc
      matparam%uparam(16)  = slimc2
    !
      matparam%uparam(17)  = sc
      matparam%uparam(18)  = slims
     !
      matparam%uparam(19)  = soft
      matparam%uparam(20)  = fbrt
      matparam%uparam(21)  = ycfac
      !
      ! function ids 
      ifunc(1)  = ifxt
      ifunc(2)  = ifxc
      ifunc(3)  = ifyt
      ifunc(4)  = ifyc
      ifunc(5)  = ifsc
      !
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
        pm(186)= e2                !  mat_param%e33
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
      call init_mat_keyword(matparam ,"incremental"   )
      call init_mat_keyword(matparam ,"TOTAL"   )
      call init_mat_keyword(matparam ,"HOOK")
      call init_mat_keyword(matparam ,"ORTHOTROPIC")
!
      ! properties compatibility
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
        write(iout,1400)  xt,slimt1,xc,slimc1
        write(iout,1500)  yt,slimt2,yc,slimc2
        write(iout,1700) sc, slims

        write(iout,2000) ifxt,ifxc
        write(iout,2100) ifyt, ifyc
        write(iout,2300) ifsc
       !! write(iout,2400) soft, fbrt, ycfac
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
       7x,'longitudinal tensile strength  . . . .  . . . . . . . . =',1pg20.13/   &
       7x,'scale for minimum longitudinal tensile stress limit. . .=',1pg20.13/   &
       7x,'longitudinal compressive strength  . . . .  . . . . . . =',1pg20.13/   &
       7x,'scale for minimum compressive tensile stress limit. . . =',1pg20.13/ )

 1500 format(                                                                     &
       5x,'matrix (dir. 2) parameters   :                            ',/          &
       5x,'---------------------------                               ',/          &   
       7x,'transverse tensile strength  . . . .  . . . . . . . . =',1pg20.13/     &
       7x,'scale for minimum transverse tensile stress limit. . .=',1pg20.13/     &   
       7x,'transverse compressive strength  . . . .  . . . . . . =',1pg20.13/     &
       7x,'scale for minimum compressive tensile stress limit. . . =',1pg20.13/ )

 1700 format(                                                                    &
       5x,' shear (12) parameters   :                            ',/               &
       5x,'---------------------------                               ',/           &
       7x,'shear strength 12 . . . . . . . . . . . . . . . . . . . =',1pg20.13/    &
       7x,'scale for minimum shear stress limit 12 . . . . .. . . =',1pg20.13/ )
 2000 format(                                                                   &
       5x,' fiber (dir. 1) strain rate dependency  :                 ',/        &
       5x,'---------------------------                               ',/        &
       7x,'curve id defining longitudinal tensile strength xt . . .  =',i10/    &
       7x,'curve id defining longitudinal compresssive strength xc . =',i10/  )
 2100 format(                                                                   &                                                              
       5x,' matrix (dir. 2) strain rate dependency  :                ',/        &
       5x,'---------------------------                               ',/        &
       7x,'curve id defining longitudinal tensile strength yt . . .  =',i10/    &
       7x,'curve id defining longitudinal compresssive strength xc . =',i10/  )
 2300 format(                                                                   &
       5x,' dir 12 - strain rate dependency  :                       ',/        &
       5x,'---------------------------                               ',/        &
       7x,'curve id defining shear strength sc at ems strain . . . =',i10/  )    
 2800 format(                                                                    &
       5x,'strain rate filtering cutoff frequency fcut . . .=',1pg20.13/)
!-----------------------------------------------------------------------
        end subroutine hm_read_mat127
!-------------------
      end module hm_read_mat127_mod
