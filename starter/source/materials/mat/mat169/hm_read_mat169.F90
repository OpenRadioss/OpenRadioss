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
! ======================================================================================================================

!||====================================================================
!||    hm_read_mat169_arup_mod   ../starter/source/materials/mat/mat169/hm_read_mat169.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat               ../starter/source/materials/mat/hm_read_mat.F90
!||====================================================================
      module hm_read_mat169_arup_mod
        implicit none
      contains

!||====================================================================
!||    hm_read_mat169_arup      ../starter/source/materials/mat/mat169/hm_read_mat169.F90
!||--- called by ------------------------------------------------------
!||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F90
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
        subroutine hm_read_mat169_arup( mtag, matparam ,                         &
          parmat   ,nuvar, unitab   ,lsubmodel,                       &
          mat_id   ,titr     ,pm         ,                            &
          iout     ,npropm   )
          ! ----------------------------------------------------------------------------------------------------------------------
          !                                                   modules
          ! ----------------------------------------------------------------------------------------------------------------------
          use elbuftag_mod
          use matparam_def_mod
          use unitab_mod
          use message_mod
          use submodel_mod
          use constant_mod , only : one ,two, zero
          use precision_mod, only : WP
! --------------------------------------------------------------------------------------------------------------------
! --------------------------------------------------------------------------------------------------------------------
!                                                 implicit none
! --------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   included files
! ----------------------------------------------------------------------------------------------------------------------
!c-----------------------------------------------
!c   d u m m y   a r g u m e n t s
!c-----------------------------------------------
          integer, intent(in)                          :: mat_id
          integer, intent(in)                          :: iout
          integer, intent(in)                          :: npropm
          integer, intent(out)                         :: nuvar
          type (unit_type_),intent(in) ::unitab
          type(submodel_data), dimension(nsubmod),intent(in) :: lsubmodel
          character(len=nchartitle) ,intent(in)             :: titr
          real(kind=WP), dimension(100)       ,intent(inout)   :: parmat
          real(kind=WP), dimension(npropm)    ,intent(inout)   :: pm
          type(matparam_struct_) ,intent(inout) :: matparam
          type(mlaw_tag_), intent(inout)  :: mtag
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
          logical :: is_available,is_encrypted
          integer :: ilaw,pwrt, pwrs
          real(kind=WP) :: rho0, young,shear, nu, tenmax, gcten ,shrmax,gcshr,shrp
          real(kind=WP) :: sht_sl,dp,dfs,dfn,limit_sh,eps_n0,eps_sh0
!=======================================================================
          is_encrypted = .false.
          is_available = .false.
          ilaw  = 169
!--------------------------------------------------------
!
          call hm_option_is_encrypted(is_encrypted)
!
!--------------------------------------------------------
!     read input fields
!--------------------------------------------------------

          call hm_get_floatv('Rho'            ,rho0    ,is_available, lsubmodel, unitab)
          !line2
          call hm_get_floatv('E'              ,young   ,is_available, lsubmodel, unitab)
          call hm_get_floatv('Nu'             ,nu      ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT169_SHT_SL'  ,sht_sl  ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT169_TENMAX'  ,tenmax  ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT169_GCTEN'   ,gcten   ,is_available, lsubmodel, unitab)
          !line 3
          call hm_get_floatv('MAT169_SHRMAX'  ,shrmax  ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT169_GCSHR'   ,gcshr   ,is_available, lsubmodel, unitab)
          call hm_get_intv  ('MAT169_PWRT'    ,pwrt    ,is_available, lsubmodel)
          call hm_get_intv  ('MAT169_PWRS'    ,pwrs    ,is_available, lsubmodel)
          call hm_get_floatv('MAT169_SHRP'    ,shrp    ,is_available, lsubmodel, unitab)
!-------------------------------------
          shear   = young/(two * (one + nu))

          dfn     =  two*gcten/tenmax !displacement failure in tension
          dfs     = (two*gcshr/(one+shrp)/shrmax)
          dp      = shrp*dfs

          eps_n0  = tenmax / young
          eps_sh0 = shrmax / shear + dp

          !condition on GC
          if(gcten < (tenmax**2/two/young))then
            gcten = tenmax**2/two/young
            CALL ANCMSG(MSGID=3074,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_1,     &
              I1 = MAT_ID,                                             &
              C1 = TITR,                                               &
              C2 = 'GCTEN',                                            &
              R1 =  gcten  )


          endif
          limit_sh = shrmax*dp + shrmax**2/two/shear
          if (gcshr < limit_sh ) then
            ! dfs must be modified
            gcshr = limit_sh
            dfs     = (two*gcshr/(one+shrp)/shrmax)
            dp      = shrp*dfs
            CALL ANCMSG(MSGID=3075,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_1,     &
              I1 = MAT_ID,                                             &
              C1 = TITR,                                               &
              C2 = 'GCSHR',                                            &
              R1 =  gcshr  )
          endif
!-------------------------------------
          nuvar = 15
!-------------------------------------

          matparam%niparam = 2
          matparam%nuparam = 14
          matparam%nfunc   = 0
          matparam%ntable  = 0
!
          allocate (matparam%uparam(matparam%nuparam))
          allocate (matparam%iparam(matparam%niparam))
          allocate (matparam%table(matparam%ntable))
!

          matparam%iparam(1)  = pwrt
          matparam%iparam(2)  = pwrs

          matparam%uparam(1)  = young
          matparam%uparam(2)  = shear
          matparam%uparam(3)  = nu
          matparam%uparam(4)  = tenmax
          matparam%uparam(5)  = gcten
          matparam%uparam(6)  = shrmax
          matparam%uparam(7)  = gcshr
          matparam%uparam(8)  = shrp
          matparam%uparam(9)  = sht_sl
          matparam%uparam(10) = dfn
          matparam%uparam(11) = dfs
          matparam%uparam(12) = dp
          matparam%uparam(13) = eps_n0
          matparam%uparam(14) = eps_sh0
!-------------------------------------------------
          pm(1)  = rho0
          pm(89) = rho0
!-------------------------------------------------
          mtag%g_thk  = 1
          mtag%g_pla  = 1
          mtag%l_pla  = 1
          mtag%l_dmg  = 1
!-------------------------------------------------
          ! properties compatibility
          call init_mat_keyword(matparam,"SOLID_COHESIVE")
!-------------------------------------------------
          write(iout,1050) trim(titr),mat_id,169
          write(iout,1000)
          if (is_encrypted) then
            write(iout,'(5x,a,//)')'CONFIDENTIAL DATA'
          else
            write(iout,1060) rho0
            write(iout,1100) young,nu, tenmax,shrmax, pwrt,pwrs,gcten , gcshr,   &
              shrp, sht_sl
          endif
!      parmat(1)  = c1
          parmat(2)  = young
          parmat(3)  = nu

!-----------
          return
!-----------
1000      format(                                                                &
            5x,a,/,                                                                 &
            5x,40h  ELASTOPLASTIC MATERIAL FOR COHESIVE   ,/,                       &
            5x,40h  -----------------------------------   ,//)
1050      format(/                                                               &
            5x,a,/,                                                                &
            5x,'MATERIAL NUMBER . . . . . . . . . . . . .=',i10/,                  &
            5x,'MATERIAL LAW. . . . . . . . . . . . . . .=',i10/)
1060      format(                                                                &
            5x,'INITIAL DENSITY . . . . . . . . . . . . .=',1pg20.13/)
1100      format(                                                                &
            5x,'YOUNG MODULUS PER THICKNESS UNIT IN TENSION. . . . .=',1PG20.13/,  &
            5x,'POISSON RATION . . . . . . . . . . . . . . . . . . .=',1PG20.13/,  &
            5x,'MAXIMAL TENSILE STRESS . . . . . . . . . . . . . . .=',1PG20.13/,  &
            5x,'MAXIMAL SHEAR STRESS . . . . . . . . . . . . . . . .=',1PG20.13/,  &
            5x,'POWER TERM FOR TENSION . . . . . . . . . . . . . . .=',1PG20.13/,  &
            5x,'POWER TERM FOR SHEAR . . . . . . . . . . . . . . . .=',1PG20.13/,  &
            5x,'ENERGY PER UNIT AREA TO FAIL IN TENSION. . . . . . .=',1PG20.13/,  &
            5x,'ENERGY PER UNIT AREA TO FAIL IN SHEAR. . . . . . . .=',1PG20.13/,  &
            5x,'SHEAR PLATEAU RATIO. . . . . . . . . . . . . . . . .=',1PG20.13/,  &
            5x,'SLOPE OF YIELD SURFACE AT ZERO TENSION . . . . . . .=',1PG20.13/)
!-----------------
        end subroutine hm_read_mat169_arup
      end module hm_read_mat169_arup_mod
