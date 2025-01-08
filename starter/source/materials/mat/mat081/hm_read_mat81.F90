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
      !||    hm_read_mat81_mod   ../starter/source/materials/mat/mat081/hm_read_mat81.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat         ../starter/source/materials/mat/hm_read_mat.F90
      !||====================================================================
      module hm_read_mat81_mod
      contains
! ======================================================================================================================
! \brief Reading material parameters of /MAT/LAW81
! \details Reading material parameters of /MAT/LAW81
! ======================================================================================================================
      !||====================================================================
      !||    hm_read_mat81            ../starter/source/materials/mat/mat081/hm_read_mat81.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F90
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                   ../starter/source/output/message/message.F
      !||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_floatv_dim        ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
      !||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||    init_mat_keyword         ../starter/source/materials/mat/init_mat_keyword.F
      !||--- uses       -----------------------------------------------------
      !||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
      !||    message_mod              ../starter/share/message_module/message_mod.F
      !||    submodel_mod             ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      subroutine hm_read_mat81(matparam ,nuvar    ,ifunc    ,maxfunc  ,        &
                               nfunc    ,parmat   ,mat_id   ,titr     ,        &
                               unitab   ,lsubmodel,mtag     ,iout     ,        &
                               nvartmp  )
!-------------------------------------------------------------------------------
!   M o d u l e s
!-------------------------------------------------------------------------------
      use unitab_mod
      use message_mod
      use submodel_mod
      use matparam_def_mod    
      use elbuftag_mod      
      use constant_mod   
!-------------------------------------------------------------------------------
!   I m p l i c i t   T y p e s
!-------------------------------------------------------------------------------
      implicit none 
#include  "my_real.inc"
!-------------------------------------------------------------------------------
!   D u m m y   A r g u m e n t s
!-------------------------------------------------------------------------------
      type(matparam_struct_) ,intent(inout)      :: matparam  !< matparam data structure
      integer, intent(inout)                     :: nuvar     !< number of material law user variables
      integer, dimension(maxfunc), intent(inout) :: ifunc     !< ids. of functions associated to the material
      integer, intent(in)                        :: maxfunc   !< maximal size of ifunc
      integer, intent(inout)                     :: nfunc     !< number of functions associated to the material
      my_real, dimension(100),intent(inout)      :: parmat    !< material parameter global table 1
      integer, intent(in)                        :: mat_id    !< material law user ID 
      character(len=nchartitle),intent(in)       :: titr      !< material law user title
      type(unit_type_),intent(in)                :: unitab    !< units table
      type(submodel_data),dimension(nsubmod),intent(in) :: lsubmodel !< submodel data structure
      type(mlaw_tag_), intent(inout)             :: mtag      !< material tag for internal variables in element buffer
      integer, intent(in)                        :: iout      !< output file number
      integer, intent(inout)                     :: nvartmp   !< number of temporary variables
!------------------------------------------------------------------------------
!   L o c a l   V a r i a b l e s
!------------------------------------------------------------------------------
      my_real :: kini,gini,cini,capini,phi,psi,alpha,max_dilat,epsvini,     &
        kwater,por0,sat0,u0,muw0,viscfac,tol,rho0,rhor,fac_unit
      integer :: soft_flag,ilaw
      logical :: is_available,is_encrypted
!-------------------------------------------------------------------------------
!     S o u r c e 
!-------------------------------------------------------------------------------    
      is_encrypted = .false.
      is_available = .false.
!
      ilaw = 81
!-------------------------------------------------------------------------------
      call hm_option_is_encrypted(is_encrypted)
!-------------------------------------------------------------------------------
!< Card1
      call hm_get_floatv('MAT_RHO'  ,rho0     ,is_available, lsubmodel, unitab)
      call hm_get_floatv('REFER_RHO',rhor     ,is_available, lsubmodel, unitab)
!< Card2
      call hm_get_floatv('K0'       ,kini     ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_G0'   ,gini     ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_COH0' ,cini     ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_PB0'  ,capini   ,is_available, lsubmodel, unitab)
!< Card3
      call hm_get_floatv('MAT_Beta' ,phi      ,is_available, lsubmodel, unitab)
      call hm_get_floatv('PSI'      ,psi      ,is_available, lsubmodel, unitab)
!< Card4
      call hm_get_floatv('MAT_ALPHA',alpha    ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_EPS'  ,max_dilat,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_SRP'  ,epsvini  ,is_available, lsubmodel, unitab)
!< Card5
      call hm_get_intv  ('FUN_A1'   ,ifunc(1) ,is_available, lsubmodel)
      call hm_get_intv  ('FUN_A2'   ,ifunc(2) ,is_available, lsubmodel)
      call hm_get_intv  ('FUN_A3'   ,ifunc(3) ,is_available, lsubmodel)
      call hm_get_intv  ('FUN_A4'   ,ifunc(4) ,is_available, lsubmodel)
      call hm_get_intv  ('Iflag'    ,soft_flag,is_available, lsubmodel)
!< Card6
      call hm_get_floatv('MAT_KW'   ,kwater   ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_POR0' ,por0     ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_SAT0' ,sat0     ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_MUE0' ,u0       ,is_available, lsubmodel, unitab)
!< Card7
      call hm_get_floatv('MAT_TOL'  ,tol      ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_VIS'  ,viscfac  ,is_available, lsubmodel, unitab)
!      
      !-------------------------------------------------------------------------
      !< Default values
      !-------------------------------------------------------------------------
      !< Alpha ratio Pa/Pb
      if (alpha == zero) alpha = half
      alpha = min(alpha,one)
      alpha = max(alpha,zero)
      !< Friction angle
      phi = max(phi,zero)
      phi = min(phi,89.0d0)
      !< Flow angle
      psi = max(psi,zero)
      psi = min(psi,89.0d0)
      !< Maximum dilatancy
      if (max_dilat == zero) max_dilat = -infinity
      max_dilat = -abs(max_dilat)
      !< Porosity related parameters
      sat0 = min(sat0,one)
      sat0 = max(sat0,zero)
      if (tol == zero)     tol     = em04
      if (viscfac == zero) viscfac = half
!
      !-------------------------------------------------------------------------
      !< Data checking
      !-------------------------------------------------------------------------
      if (kini<=zero) then
        call ancmsg(msgid=1012,                                                &
                    msgtype=msgerror,                                          &
                    anmode=aninfo_blind_1,                                     &
                    i1=mat_id,                                                 &
                    c1=titr)
      endif
      if (gini<=zero) then
        call ancmsg(msgid=1013,                                                &
                    msgtype=msgerror,                                          &
                    anmode=aninfo_blind_1,                                     &
                    i1=mat_id,                                                 &
                    c1=titr)
      endif
      !< Default value for scale factors of c and Pb
      if (cini == zero) then 
        call hm_get_floatv_dim('mat_coh0',fac_unit,is_available,lsubmodel,     &
                               unitab    )
        cini = one*fac_unit
      endif
      if (capini == zero) then 
        call hm_get_floatv_dim('mat_pb0',fac_unit,is_available,lsubmodel,      &
                              unitab    )
        capini = one*fac_unit
      endif
      if (sat0 /= zero) then
        if (kwater <= zero) then
          call ancmsg(msgid=1085,                                              &
                      msgtype=msgerror,                                        &
                      anmode=aninfo_blind_1,                                   &
                      i1=mat_id,                                               &
                      c1=titr)
        endif
        muw0 = 0
        if (por0 == zero) then
          call ancmsg(msgid=1086,                                              &
                      msgtype=msgerror,                                        &
                      anmode=aninfo_blind_1,                                   &
                      i1=mat_id,                                               &
                      c1=titr)
        elseif (u0 > zero) then
          muw0 = u0/kwater
          sat0 = one + muw0
        else
          muw0 = sat0 - one
          if (muw0 > zero) then
            u0 = kwater*muw0
          else 
            u0 = zero
          endif
        endif
      else
        muw0 = -one
      endif
!
      !-------------------------------------------------------------------------
      !< Filling buffer tables
      !-------------------------------------------------------------------------
      !< Number of integer material parameters
      matparam%niparam = 1
      !< Number of real material parameters
      matparam%nuparam = 15
      !< Number of user variables 
      nuvar = 10
      !< Number of functions
      nfunc = 4
      !< Number of temporary variables
      nvartmp = 4
!          
      !< Allocation of material parameters tables
      allocate (matparam%iparam(matparam%niparam))
      allocate (matparam%uparam(matparam%nuparam))
!     
      !< Integer material parameter
      matparam%iparam(1)  = soft_flag
!
      !< Real material parameters
      matparam%uparam(1)  = kini
      matparam%uparam(2)  = gini
      matparam%uparam(3)  = tand(phi)
      matparam%uparam(4)  = tand(psi)
      matparam%uparam(5)  = cini
      matparam%uparam(6)  = capini
      matparam%uparam(7)  = alpha
      matparam%uparam(8)  = max_dilat
      matparam%uparam(9)  = epsvini
      matparam%uparam(10) = kwater
      matparam%uparam(11) = por0
      matparam%uparam(12) = muw0 + one
      matparam%uparam(13) = u0
      matparam%uparam(14) = tol
      matparam%uparam(15) = viscfac
!
      !< Elastic parameters
      matparam%bulk  = kini
      matparam%shear = gini
      matparam%young = nine*kini*gini/(three*kini+gini)
      matparam%nu    = (three*kini-two*gini)/(six*kini+two*gini)
!
      !< PARMAT table
      parmat(1)  = matparam%bulk
      parmat(2)  = matparam%young
      parmat(3)  = matparam%nu
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
      ! -> Equivalent plastic strain (1 - deviatoric, 2 - volumetric)
      mtag%g_pla = 2
      mtag%l_pla = 2
!
      !< Properties compatibility  
      call init_mat_keyword(matparam,"SOLID_ISOTROPIC") 
      call init_mat_keyword(matparam,"SPH")      
! 
      !< MATPARAM keywords   
      call init_mat_keyword(matparam,"HOOK")
      call init_mat_keyword(matparam,"COMPRESSIBLE")
      call init_mat_keyword(matparam,"INCREMENTAL" )
      call init_mat_keyword(matparam,"LARGE_STRAIN")
      call init_mat_keyword(matparam,"HYDRO_EOS") 
      call init_mat_keyword(matparam,"ISOTROPIC") 
!
      !-------------------------------------------------------------------------
      !< - Parameters printing in output listing
      !-------------------------------------------------------------------------
      write(iout,1000) trim(titr),mat_id,ilaw
      write(iout,1100)
      if (is_encrypted) then
        write(iout,'(5x,a,//)')'CONFIDENTIAL DATA'
      else
        write(iout,1200) rho0
        write(iout,1300)
        if (ifunc(1) > 0) then 
          write(iout,1310) ifunc(1),kini
        else
          write(iout,1320) kini
        endif
        if (ifunc(2) > 0) then 
          write(iout,1330) ifunc(2),gini
        else
          write(iout,1340) gini
        endif
        write(iout,1400)
        if (ifunc(3) > 0) then 
          write(iout,1410) ifunc(3),cini
        else
          write(iout,1420) cini
        endif
        if (ifunc(4) > 0) then 
          write(iout,1430) ifunc(4),capini
        else
          write(iout,1440) capini
        endif
        write(iout,1450) phi,psi,alpha,max_dilat,soft_flag
        write(iout,1500) epsvini,kwater,por0,sat0,u0,tol,viscfac 
      endif     
!-------------------------------------------------------------------------------
 1000 format(/                                                                 &
       5X,A,/,                                                                 &
       5X,'MATERIAL NUMBER. . . . . . . . . . . . . . .=',I10/,                &
       5X,'MATERIAL LAW . . . . . . . . . . . . . . . .=',I10/)
 1100 format(/                                                                 &
       5X,'-----------------------------------------------------',/,           &
       5X,'  MATERIAL MODEL: DRUCKER-PRAGER WITH CAP HARDENING  ',/,           &
       5X,'-----------------------------------------------------',/)
 1200 format(/                                                                 &
       5X,'INITIAL DENSITY  . . . . . . . . . . . . . .=',1PG20.13/)  
 1300 format(/                                                                 &
       5X,'ELASTIC PARAMETERS:                          ',/,                   &
       5X,'---------------------------------------------',/)
 1310 format(/                                                                 &
       5X,'BULK MODULUS K SCALE FUNCTION ID. . . . . . =',I10/,                &
       5X,'BULK MODULUS SCALE FACTOR (K0). . . . . . . =',1PG20.13/)
 1320 format(/                                                                 &
       5X,'CONSTANT BULK MODULUS (K0). . . . . . . . . =',1PG20.13/)
 1330 format(/                                                                 &
       5X,'SHEAR MODULUS G SCALE FUNCTION ID . . . . . =',I10/,                &
       5X,'SHEAR MODULUS SCALE FACTOR (G0) . . . . . . =',1PG20.13/)
 1340 format(/                                                                 &
       5X,'CONSTANT SHEAR MODULUS (G0) . . . . . . . . =',1PG20.13/)
 1400 format(/                                                                 &
       5X,'YIELD CRITERION & PLAST.POTENTIAL PARAMETERS:',/,                   &
       5X,'---------------------------------------------',/)
 1410 format(/                                                                 &
       5X,'MATERIAL COHESION C SCALE FUNCTION ID . . . =',I10/,                &
       5X,'MATERIAL COHESION SCALE FACTOR (C0) . . . . =',1PG20.13/)
 1420 format(/                                                                 &
       5X,'CONSTANT MATERIAL COHESION (C0) . . . . . . =',1PG20.13/)
 1430 format(/                                                                 &
       5X,'CAP LIMIT PRESSURE PB SCALE FUNCTION ID . . =',I10/,                &
       5X,'CAP LIMIT PRESSURE PB SCALE FACTOR (PB0). . =',1PG20.13/)
 1440 format(/                                                                 &   
       5X,'CONSTANT CAP LIMIT PRESSURE (PB0) . . . . . =',1PG20.13/)  
 1450 format(/                                                                 &
       5X,'YIELD CRITERION FRICTION ANGLE (PHI). . . . =',1PG20.13/,           &
       5X,'PLASTIC POTENTIAL FLOW ANGLE (PSI). . . . . =',1PG20.13/,           &
       5X,'ALPHA RATIO (PA/PB) . . . . . . . . . . . . =',1PG20.13/,           &
       5X,'MAXIMUM DILATANCY (EPS_MAX) . . . . . . . . =',1PG20.13/,           &
       5X,'CAP SOFTENING FLAG (ISOFT). . . . . . . . . =',I10/                 &
       5X,'  ISOFT = 0: CAP HARDENING SOFTENING ALLOWED ',/,                   &
       5X,'  ISOFT = 1: NO CAP HARDENING SOFTENING      ',/)
 1500 format(/                                                                 &
       5X,'POROSITY PARAMETERS:                         ',/,                   &
       5X,'---------------------------------------------',/,                   &
       5X,'INITIAL PLASTIC VOLUMETRIC STRAIN (EPSPV0). =',1PG20.13/,           &
       5X,'BULK MODULUS OF WATER . . . . . . . . . . . =',1PG20.13/            &
       5X,'INITIAL POROSITY POR0 . . . . . . . . . . . =',1PG20.13/            &
       5X,'INITIAL WATER SATURATION SAT0 . . . . . . . =',1PG20.13/            &
       5X,'INITIAL PORE PRESSURE U0  . . . . . . . . . =',1PG20.13/            &
       5X,'TOLERANCE FOR THE CRITERION SHIFT . . . . . =',1PG20.13/            &
       5X,'VISCOSITY FACTOR  . . . . . . . . . . . . . =',1PG20.13/)
!-------------------------------------------------------------------------------
      end subroutine hm_read_mat81
      end module hm_read_mat81_mod

