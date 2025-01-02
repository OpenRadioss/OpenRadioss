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
      !||    defaults_mod                  ../starter/source/modules/defaults_mod.F90
      !||--- called by ------------------------------------------------------
      !||    contrl                        ../starter/source/starter/contrl.F
      !||    elbuf_ini                     ../starter/source/elements/elbuf_init/elbuf_ini.F
      !||    hm_preread_properties         ../starter/source/properties/hm_preread_properties.F
      !||    hm_prop_read21                ../starter/source/properties/thickshell/hm_read_prop21.F
      !||    hm_read_defshell              ../starter/source/general_controls/default_values/hm_read_defshell.F
      !||    hm_read_defsolid              ../starter/source/general_controls/default_values/hm_read_defsolid.F
      !||    hm_read_prop01                ../starter/source/properties/shell/hm_read_prop01.F
      !||    hm_read_prop06                ../starter/source/properties/solid/hm_read_prop06.F
      !||    hm_read_prop09                ../starter/source/properties/shell/hm_read_prop09.F
      !||    hm_read_prop10                ../starter/source/properties/shell/hm_read_prop10.F
      !||    hm_read_prop11                ../starter/source/properties/shell/hm_read_prop11.F
      !||    hm_read_prop14                ../starter/source/properties/solid/hm_read_prop14.F
      !||    hm_read_prop14f               ../starter/source/properties/solid/hm_read_prop14.F
      !||    hm_read_prop15                ../starter/source/properties/solid/hm_read_prop15.F
      !||    hm_read_prop16                ../starter/source/properties/shell/hm_read_prop16.F
      !||    hm_read_prop17                ../starter/source/properties/shell/hm_read_prop17.F
      !||    hm_read_prop20                ../starter/source/properties/thickshell/hm_read_prop20.F
      !||    hm_read_prop22                ../starter/source/properties/thickshell/hm_read_prop22.F
      !||    hm_read_prop43                ../starter/source/properties/solid/hm_read_prop43.F
      !||    hm_read_prop51                ../starter/source/properties/shell/hm_read_prop51.F
      !||    hm_read_prop_generic          ../starter/source/properties/hm_read_prop_generic.F
      !||    hm_read_properties            ../starter/source/properties/hm_read_properties.F
      !||    hm_read_stack                 ../starter/source/stack/hm_read_stack.F
      !||    inirig_mat                    ../starter/source/elements/initia/inirig_mat.F
      !||    initia                        ../starter/source/elements/initia/initia.F
      !||    inivoid                       ../starter/source/elements/initia/inivoid.F
      !||    lecstack_ply                  ../starter/source/properties/composite_options/stack/lecstack_ply.F
      !||    lectur                        ../starter/source/starter/lectur.F
      !||    multifluid_init3t             ../starter/source/multifluid/multifluid_init3t.F
      !||    s10init3                      ../starter/source/elements/solid/solide10/s10init3.F
      !||    s4init3                       ../starter/source/elements/solid/solide4/s4init3.F
      !||    s6cinit3                      ../starter/source/elements/thickshell/solide6c/s6cinit3.F
      !||    shell_offset_ini              ../starter/source/elements/shell/shell_offset/shell_offset_ini.F90
      !||    shell_offsetp                 ../starter/source/elements/shell/shell_offset/shell_offsetp.F90
      !||    st_qaprint_driver             ../starter/source/output/qaprint/st_qaprint_driver.F
      !||    st_qaprint_general_controls   ../starter/source/output/qaprint/st_qaprint_general_controls.F
      !||    starter0                      ../starter/source/starter/starter0.F
      !||====================================================================
      module defaults_mod
!=======================================================================================================================
!!\brief default type : Hosts the variables for /DEF/xxx Starter Deck option
!=======================================================================================================================

        ! Variables from /DEF/SHELL Option.
        type shell_defaults_
          integer ::  ioffset    !< offset support contact 
          integer ::  ishell     !< 4n shell formulations     old :IHBE_D
          integer ::  ish3n      !< 3n shell formulations
          integer ::  ismstr     !< small/large strain flag   old :ISST_D
          integer ::  iplas      !< plastic strain flag       old :IPLA_D
          integer ::  ithick     !< thickness flag
          integer ::  idrill     !< drilling dof flag
! old obsolet flags : ISTRA_D: fixed to 1,NPTS_D ,ISHEA_D : fixed to 0      
        end type  shell_defaults_

        ! --------------------------------
        ! Variables from /DEF/SOLID Option.
        type solid_defaults_
          integer ::  isolid    !< solid elem formulations
          integer ::  ismstr    !< small/large strain flag
          integer ::  icpre     !< constant pressure flag
          integer ::  itetra4   !< tet4 elem formulations
          integer ::  itetra10  !< tet10 elem formulations
          integer ::  imas      !< nodal mass distribution method
          integer ::  iframe    !< elementary coordinate system
          integer ::  icontrol  !< distortion control
! old obsolet flags : IPLA_DS: fixed to 2     
        end type  solid_defaults_

        ! --------------------------------
        ! Variables from /DEF/INTER Option.
        type interface_defaults_
          integer ::  DEF_INTER(100)    !< /DEFAULT/INTER options
        end type  interface_defaults_

        ! --------------------------------
        ! Variables from /DEF/xxx Option.
        type defaults_
          type (shell_defaults_)     :: shell       !< /DEF_SHELL option
          type (solid_defaults_)     :: solid       !< /DEF_SOLID option
          type (interface_defaults_) :: interface   !< /DEFAULT/INTER option
        end type  defaults_

        contains        
!=======================================================================================================================
!!\brief initialize first to zero all flag value of /DEF_*
!=======================================================================================================================
      !||====================================================================
      !||    init_def_zero   ../starter/source/modules/defaults_mod.F90
      !||--- called by ------------------------------------------------------
      !||    contrl          ../starter/source/starter/contrl.F
      !||====================================================================
          subroutine init_def_zero(this)
            implicit none
           
            type (defaults_), intent(inout) :: this

            this%shell%ioffset = 0
            this%shell%ishell  = 0
            this%shell%ish3n   = 0
            this%shell%ismstr  = 0
            this%shell%iplas   = 0
            this%shell%ithick  = 0
            this%shell%idrill  = 0
! solid                                                            
            this%solid%isolid   = 0
            this%solid%ismstr   = 0
            this%solid%icpre    = 0
            this%solid%itetra4  = 0
            this%solid%itetra10 = 0
            this%solid%imas     = 0
            this%solid%iframe   = 0
            this%solid%icontrol = 0
! interf                                                           
            this%interface%DEF_INTER(1:100)   = 0
          end subroutine init_def_zero                  
!=======================================================================================================================
!!\brief initialize to default values of /DEF_SHELL,
!=======================================================================================================================
      !||====================================================================
      !||    init_def_elem   ../starter/source/modules/defaults_mod.F90
      !||--- called by ------------------------------------------------------
      !||    contrl          ../starter/source/starter/contrl.F
      !||====================================================================
          subroutine init_def_elem(n2d,iimplicit,this)
            implicit none
           
            integer,  intent(in)            :: n2d          !< 2d analy option
            integer,  intent(in)            :: iimplicit    !< /IMPLICIT option
            type (defaults_), intent(inout) :: this

            if (iimplicit == 1) then
              if (this%shell%iplas==0) this%shell%iplas   = 1
              if (this%shell%ishell==0) this%shell%ishell  = 24
              if (this%shell%idrill==0) this%shell%idrill  = 1
            end if
            if (this%shell%ishell==0) this%shell%ishell  = 1
            if (this%shell%ish3n==0) this%shell%ish3n  = 2
! solid                                                            
            if (iimplicit == 1) then
              if (this%solid%isolid==0) this%solid%isolid   = 14
            end if
            if (this%solid%isolid==0) this%solid%isolid   = 1
            if (this%solid%itetra4==0) this%solid%itetra4   = 1000
            if (this%solid%itetra10==0) this%solid%itetra10   = 1000
            if (this%solid%iframe==0) this%solid%iframe   = 1
            if (n2d/=0.and.this%solid%isolid/=17) this%solid%isolid = 2
          end subroutine init_def_elem
!          
       end module defaults_mod
