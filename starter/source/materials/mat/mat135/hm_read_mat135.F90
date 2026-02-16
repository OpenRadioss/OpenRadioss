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
!||    hm_read_mat135_mod   ../starter/source/materials/mat/mat135/hm_read_mat135.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat          ../starter/source/materials/mat/hm_read_mat.F90
!||====================================================================
      module hm_read_mat135_mod
      contains
! ======================================================================================================================
! \brief Reading material parameters of /MAT/LAW135
! \details Reading material parameters of /MAT/LAW135
! ======================================================================================================================
!||====================================================================
!||    hm_read_mat135           ../starter/source/materials/mat/mat135/hm_read_mat135.F90
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
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||    table_mod                ../starter/share/modules1/table_mod.F
!||====================================================================
      subroutine hm_read_mat135(            &
          matparam ,mtag     ,nuvar    ,    &
          iout     ,unitab   ,lsubmodel, mat_id  )

!C-----------------------------------------------
!C   D e s c r i p t i o n
!C-----------------------------------------------
!C   ROUTINE DESCRIPTION :
!C   ===================
!C   READ MAT LAW135 WITH HM READER
!C-----------------------------------------------
!C   DUMMY ARGUMENTS DESCRIPTION:
!C   ===================
!C
!C     NAME            DESCRIPTION                         
!C
!C     IPM             MATERIAL ARRAY(INTEGER)
!C     PM              MATERIAL ARRAY(REAL)
!C     UNITAB          UNITS ARRAY
!C     ID              MATERIAL ID(INTEGER)
!C     TITR            MATERIAL TITLE
!C     LSUBMODEL       SUBMODEL STRUCTURE    
!C-----------------------------------------------
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
      type(matparam_struct_) ,intent(inout)        :: matparam    
      type(mlaw_tag_), intent(inout)               :: mtag
      integer, intent(inout)                       :: nuvar
      integer, intent(in)                          :: iout
      type (unit_type_),intent(in)                 :: unitab 
      type(submodel_data), dimension(*),intent(in) :: lsubmodel
      integer, intent(in)                          :: mat_id
!-----------------------------------------------
 ! L o c a l   V a r i a b l e s
 !-----------------------------------------------
      integer :: ilaw
      ! Variables used for reading constraints
      integer :: lsd_tr, lsd_ts, lsd_tt, lsd_rr, lsd_rs, lsd_rt
      ! Variables used for reading/writing density and stiffness
      real(kind=WP) :: rho, lsd_tkr, lsd_rkr
      real(kind=WP) :: tkr, rkr
      logical :: is_available, is_encrypted

 !=======================================================================

      is_encrypted = .false.
      is_available = .false.
      ilaw         = 135
     
 !------------------------------------------ OKK
      call hm_option_is_encrypted(is_encrypted)
 !------------------------------------------
!card1 - Density
      call hm_get_floatv('RHO'   ,Rho     ,is_available, lsubmodel, unitab)
!card2 - Material parameters
      call hm_get_intv  ('LSD_TR'       ,LSD_TR       ,is_available, lsubmodel)
      call hm_get_intv  ('LSD_TS'       ,LSD_TS       ,is_available, lsubmodel)
      call hm_get_intv  ('LSD_TT'       ,LSD_TT       ,is_available, lsubmodel)
      call hm_get_intv  ('LSD_RR'       ,LSD_RR       ,is_available, lsubmodel)
      call hm_get_intv  ('LSD_RS'       ,LSD_RS       ,is_available, lsubmodel)
      call hm_get_intv  ('LSD_RT'       ,LSD_RT       ,is_available, lsubmodel)
      call hm_get_floatv('LSDYNA_TKR'   ,LSD_TKR   ,is_available, lsubmodel, unitab)
      call hm_get_floatv('LSDYNA_RKR'   ,LSD_RKR   ,is_available, lsubmodel, unitab)      
!card1 -OKK
      if (LSD_TKR == zero) then
        LSD_TKR = ONE
      end if
     if (LSD_RKR == zero) then
        LSD_RKR = ONE
      end if
 !---------------------------------------------------------------------------------------------
 !                                filling buffer tables
 !---------------------------------------------------------------------------------------------
      nuvar   = 39
      ! number of material parameters
      matparam%niparam = 7
      matparam%nuparam = 2
      matparam%nfunc   = 0
      matparam%ntable  = 0
      allocate (matparam%uparam(matparam%nuparam))
      allocate (matparam%iparam(matparam%niparam))
      allocate (matparam%table(matparam%ntable))
      ! number of user variables 


      ! material parameters
      matparam%iparam(1) = LSD_TR
      matparam%iparam(2) = LSD_TS
      matparam%iparam(3) = LSD_TT 
      matparam%iparam(4) = LSD_RR
      matparam%iparam(5) = LSD_RS
      matparam%iparam(6) = LSD_RT
      matparam%uparam(1)  = LSD_TKR
      matparam%uparam(2)  = LSD_RKR
      matparam%iparam(7)  = 6
      !< Real material parameters
      matparam%rho       = rho
      matparam%rho0      = rho

!C------------------------
!C------------------------ 
      MTAG%G_TOTDEPL = 3  ! DX (DY,DZ) - total deformation (translation)
      MTAG%G_TOTROT = 3   ! RX (RY,RZ) - total deformation (rotation)
      MTAG%G_DEP_IN_TENS = 3   ! DPX  (DPY,DPZ) - max displacement in tension
      MTAG%G_DEP_IN_COMP = 3   ! DPX2 (DPY2, DPZ2) - Max Displacement in Compression
      MTAG%G_ROT_IN_TENS = 3   ! RPX (RPY,RPZ) - max rotation in tension
      MTAG%G_ROT_IN_COMP = 3   ! RPX2 (RPY2,RPY2) - max rotation in compression
      MTAG%G_POSX = 5
      MTAG%G_POSY = 5
      MTAG%G_POSZ = 5
      MTAG%G_POSXX = 5
      MTAG%G_POSYY = 5
      MTAG%G_POSZZ = 5
      MTAG%G_YIELD = 6               
      MTAG%G_RUPTCRIT = 1 


      MTAG%G_NUVAR = 39  
      
      MTAG%G_MASS = 1
      MTAG%G_SKEW_ID = 1  

!C 
!C------------------------

!C------------------------
      ! Properties compatibility
      CALL INIT_MAT_KEYWORD(MATPARAM,"SPRING_MATERIAL")

!------------------------- 
!     parameters printout  OKKKKK
!-------------------------- 
      if (is_encrypted) then
        write(iout,'(5x,a,//)')'confidential data'
      else
! Write Header and ID
        write(iout, 100)
        write(iout, 200) mat_id

        ! Write Density
        write(iout, 300) rho

          ! Write Constraints
        write(iout, 400)
        write(iout, 500) lsd_tr, lsd_ts, lsd_tt, lsd_rr, lsd_rs, lsd_rt

        ! Write Stiffness Factors
        write(iout, 600) LSD_tkr, LSD_rkr
      endif     
! ----------------------------------------------------------------------
!   Format Definitions
! ----------------------------------------------------------------------

! Header Format
100       format(/,                                                     &
     &      5X, "---------------------------------", /,                 &
     &      5X, "MATERIAL LAW 135 (GENERAL JOINT) ", /,                 &
     &      5X, "---------------------------------", /)

! Material ID Format
200       format(                                                       &
     &      5X, "MATERIAL ID . . . . . . . . . . . . . . . . . =", I10)

! Density Format
300       format(                                                       &
     &      5X, "INITIAL DENSITY . . . . . . . . . . . . . . . =", 1PG20.13, /)

! Constraints Format Header
400       format(                                                       &
     &      5X, "CONSTRAINTS (0=Free, 1=Constrained):           ", /,   &
     &      5X, "------------------------------------           ")

! Detailed Constraints Format
500       format(                                                       &
     &      5X, "   TRANSLATIONAL (R, S, T) . . . . . . . . . . =", 3I5, /, &
     &      5X, "   ROTATIONAL    (R, S, T) . . . . . . . . . . =", 3I5, /)

! Stiffness Scales Format
600       format(                                                       &
     &      5X, "STIFFNESS SCALING FACTORS:                     ", /,   &
     &      5X, "--------------------------                     ", /,   &
     &      5X, "   TRANSLATIONAL STIFFNESS SCALE (TKR) . . . . =", 1PG20.13, /, &
     &      5X, "   ROTATIONAL STIFFNESS SCALE    (RKR) . . . . =", 1PG20.13, /)
!-----------------------------------------------------------------------
        end subroutine hm_read_mat135
!-------------------
      end module hm_read_mat135_mod
