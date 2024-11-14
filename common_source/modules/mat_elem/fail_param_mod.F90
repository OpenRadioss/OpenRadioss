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
!Chd|====================================================================
!Chd|  fail_param_mod                modules/mat_elem/fail_param_mod.f
!Chd|-- called by -----------
!Chd|        matparam_def_mod              common_source/modules/mat_elem/matparam_def_mod.f
!Chd|        fail_fun2sys                  starter/source/materials/tools/fail_fun2sys.f
!Chd|        fail_tab2sys                  starter/source/materials/tools/fail_tab2sys.f
!Chd|        hm_read_fail_alter            starter/source/materials/fail/windshield_alter/hm_read_fail_alter.F
!Chd|        hm_read_fail_biquad           starter/source/materials/fail/biquad/hm_read_fail_biquad.F
!Chd|        hm_read_fail_chang            starter/source/materials/fail/changchang/hm_read_fail_chang.F
!Chd|        hm_read_fail_cockcroft        starter/source/materials/fail/cockroft_latham/hm_read_fail_cockcroft.F
!Chd|        hm_read_fail_connect          starter/source/materials/fail/connect/hm_read_fail_connect.F
!Chd|        hm_read_fail_emc              starter/source/materials/fail/emc/hm_read_fail_emc.f
!Chd|        hm_read_fail_energy           starter/source/materials/fail/energy/hm_read_fail_energy.F
!Chd|        hm_read_fail_fabric           starter/source/materials/fail/fabric/hm_read_fail_fabric.F
!Chd|        hm_read_fail_fld              starter/source/materials/fail/fld/hm_read_fail_fld.f
!Chd|        hm_read_fail_gene1            starter/source/materials/fail/gene1/hm_read_fail_gene1.f
!Chd|        hm_read_fail_gurson           starter/source/materials/fail/gurson/hm_read_fail_gurson.F
!Chd|        hm_read_fail_hashin           starter/source/materials/fail/hashin/hm_read_fail_hashin.F
!Chd|        hm_read_fail_hc_dsse          starter/source/materials/fail/hc_dsse/hm_read_fail_hc_dsse.F
!Chd|        hm_read_fail_hoffman          starter/source/materials/fail/hoffman/hm_read_fail_hoffman.F
!Chd|        hm_read_fail_inievo           starter/source/materials/fail/inievo/hm_read_fail_inievo.F
!Chd|        hm_read_fail_johnson          starter/source/materials/fail/johnson_cook/hm_read_fail_johnson.F
!Chd|        hm_read_fail_ladeveze         starter/source/materials/fail/ladeveze/hm_read_fail_ladeveze.F
!Chd|        hm_read_fail_maxstrain        starter/source/materials/fail/max_strain/hm_read_fail_maxstrain.F
!Chd|        hm_read_fail_nxt              starter/source/materials/fail/nxt/hm_read_fail_nxt.f
!Chd|        hm_read_fail_orthbiquad       starter/source/materials/fail/orthbiquad/hm_read_fail_orthbiquad.F
!Chd|        hm_read_fail_orthenerg        starter/source/materials/fail/orthenerg/hm_read_fail_orthenerg.F
!Chd|        hm_read_fail_orthstrain       starter/source/materials/fail/orthstrain/hm_read_fail_orthstrain.F
!Chd|        hm_read_fail_puck             starter/source/materials/fail/puck/hm_read_fail_puck.f
!Chd|        hm_read_fail_rtcl             starter/source/materials/fail/rtcl/hm_read_fail_rtcl.f
!Chd|        hm_read_fail_sahraei          starter/source/materials/fail/sahraei/hm_read_fail_sahraei.F
!Chd|        hm_read_fail_snconnect        starter/source/materials/fail/snconnect/hm_read_fail_snconnect.F
!Chd|        hm_read_fail_spalling         starter/source/materials/fail/spalling/hm_read_fail_spalling.F
!Chd|        hm_read_fail_syazwan          starter/source/materials/fail/syazwan/hm_read_fail_syazwan.F
!Chd|        hm_read_fail_tab1             starter/source/materials/fail/tabulated/hm_read_fail_tab1.F
!Chd|        hm_read_fail_tab2             starter/source/materials/fail/tabulated/hm_read_fail_tab2.F
!Chd|        hm_read_fail_tab_old          starter/source/materials/fail/tabulated/hm_read_fail_tab_old.F
!Chd|        hm_read_fail_tbutcher         starter/source/materials/fail/tuler_butcher/hm_read_fail_tbutcher.F
!Chd|        hm_read_fail_tensstrain       starter/source/materials/fail/tensstrain/hm_read_fail_tensstrain.F
!Chd|        hm_read_fail_tsaihill         starter/source/materials/fail/tsaihill/hm_read_fail_tsaihill.F
!Chd|        hm_read_fail_tsaiwu           starter/source/materials/fail/tsaiwu/hm_read_fail_tsaiwu.F
!Chd|        hm_read_fail_user             starter/source/materials/fail/failuser/hm_read_fail_user.F
!Chd|        hm_read_fail_visual           starter/source/materials/fail/visual/hm_read_fail_visual.F
!Chd|        hm_read_fail_wierzbicki       starter/source/materials/fail/wierzbicki/hm_read_fail_wierzbicki.F
!Chd|        hm_read_fail_wilkins          starter/source/materials/fail/wilkins/hm_read_fail_wilkins.F
!Chd|        hm_read_mullins_or            starter/source/materials/fail/mullins_or/hm_read_fail_mullins_or.F
!Chd|        write_failparam               starter/source/materials/fail/write_failparam.f
!Chd|        delm01law                     engine/source/properties/composite_options/stack/delm01law.F
!Chd|        delm02law                     engine/source/properties/composite_options/stack/delm02law.F
!Chd|        delm24law                     engine/source/properties/composite_options/stack/delm24law.F
!Chd|        h3d_fld_tsh                   engine/source/output/h3d/h3d_results/h3d_fld_tsh.f
!Chd|        read_failparam                engine/source/output/restart/read_failparam.f
!Chd|        write_failparam               engine/source/output/restart/write_failparam.f
!Chd|-- calls ---------------
!Chd|        names_and_titles_mod          modules/names_and_titles_mod.f
!Chd|        table4d_mod                   modules/table4d_mod.f         
!Chd|====================================================================
!
      !||====================================================================
      !||    fail_param_mod            ../common_source/modules/mat_elem/fail_param_mod.F90
      !||--- called by ------------------------------------------------------
      !||    brokmann_random           ../starter/source/materials/fail/windshield_alter/brokmann_random.F90
      !||    delm01law                 ../engine/source/properties/composite_options/stack/delm01law.F
      !||    delm02law                 ../engine/source/properties/composite_options/stack/delm02law.F
      !||    delm24law                 ../engine/source/properties/composite_options/stack/delm24law.F
      !||    fail_fun2sys              ../starter/source/materials/tools/fail_fun2sys.F
      !||    fail_tab2sys              ../starter/source/materials/tools/fail_tab2sys.F
      !||    h3d_fld_tsh               ../engine/source/output/h3d/h3d_results/h3d_fld_tsh.F
      !||    hm_read_fail_alter        ../starter/source/materials/fail/windshield_alter/hm_read_fail_alter.F
      !||    hm_read_fail_biquad       ../starter/source/materials/fail/biquad/hm_read_fail_biquad.F
      !||    hm_read_fail_chang        ../starter/source/materials/fail/changchang/hm_read_fail_chang.F
      !||    hm_read_fail_cockcroft    ../starter/source/materials/fail/cockroft_latham/hm_read_fail_cockcroft.F
      !||    hm_read_fail_connect      ../starter/source/materials/fail/connect/hm_read_fail_connect.F
      !||    hm_read_fail_emc          ../starter/source/materials/fail/emc/hm_read_fail_emc.F
      !||    hm_read_fail_energy       ../starter/source/materials/fail/energy/hm_read_fail_energy.F
      !||    hm_read_fail_fabric       ../starter/source/materials/fail/fabric/hm_read_fail_fabric.F
      !||    hm_read_fail_fld          ../starter/source/materials/fail/fld/hm_read_fail_fld.F
      !||    hm_read_fail_gene1        ../starter/source/materials/fail/gene1/hm_read_fail_gene1.F
      !||    hm_read_fail_gurson       ../starter/source/materials/fail/gurson/hm_read_fail_gurson.F
      !||    hm_read_fail_hashin       ../starter/source/materials/fail/hashin/hm_read_fail_hashin.F
      !||    hm_read_fail_hc_dsse      ../starter/source/materials/fail/hc_dsse/hm_read_fail_hc_dsse.F
      !||    hm_read_fail_hoffman      ../starter/source/materials/fail/hoffman/hm_read_fail_hoffman.F
      !||    hm_read_fail_inievo       ../starter/source/materials/fail/inievo/hm_read_fail_inievo.F
      !||    hm_read_fail_johnson      ../starter/source/materials/fail/johnson_cook/hm_read_fail_johnson.F
      !||    hm_read_fail_ladeveze     ../starter/source/materials/fail/ladeveze/hm_read_fail_ladeveze.F
      !||    hm_read_fail_maxstrain    ../starter/source/materials/fail/max_strain/hm_read_fail_maxstrain.F
      !||    hm_read_fail_nxt          ../starter/source/materials/fail/nxt/hm_read_fail_nxt.F
      !||    hm_read_fail_orthbiquad   ../starter/source/materials/fail/orthbiquad/hm_read_fail_orthbiquad.F
      !||    hm_read_fail_orthenerg    ../starter/source/materials/fail/orthenerg/hm_read_fail_orthenerg.F
      !||    hm_read_fail_orthstrain   ../starter/source/materials/fail/orthstrain/hm_read_fail_orthstrain.F
      !||    hm_read_fail_puck         ../starter/source/materials/fail/puck/hm_read_fail_puck.F
      !||    hm_read_fail_rtcl         ../starter/source/materials/fail/rtcl/hm_read_fail_rtcl.F
      !||    hm_read_fail_sahraei      ../starter/source/materials/fail/sahraei/hm_read_fail_sahraei.F
      !||    hm_read_fail_snconnect    ../starter/source/materials/fail/snconnect/hm_read_fail_snconnect.F
      !||    hm_read_fail_spalling     ../starter/source/materials/fail/spalling/hm_read_fail_spalling.F
      !||    hm_read_fail_syazwan      ../starter/source/materials/fail/syazwan/hm_read_fail_syazwan.F
      !||    hm_read_fail_tab1         ../starter/source/materials/fail/tabulated/hm_read_fail_tab1.F
      !||    hm_read_fail_tab2         ../starter/source/materials/fail/tabulated/hm_read_fail_tab2.F
      !||    hm_read_fail_tab_old      ../starter/source/materials/fail/tabulated/hm_read_fail_tab_old.F
      !||    hm_read_fail_tbutcher     ../starter/source/materials/fail/tuler_butcher/hm_read_fail_tbutcher.F
      !||    hm_read_fail_tensstrain   ../starter/source/materials/fail/tensstrain/hm_read_fail_tensstrain.F
      !||    hm_read_fail_tsaihill     ../starter/source/materials/fail/tsaihill/hm_read_fail_tsaihill.F
      !||    hm_read_fail_tsaiwu       ../starter/source/materials/fail/tsaiwu/hm_read_fail_tsaiwu.F
      !||    hm_read_fail_user         ../starter/source/materials/fail/failuser/hm_read_fail_user.F
      !||    hm_read_fail_visual       ../starter/source/materials/fail/visual/hm_read_fail_visual.F
      !||    hm_read_fail_wierzbicki   ../starter/source/materials/fail/wierzbicki/hm_read_fail_wierzbicki.F
      !||    hm_read_fail_wilkins      ../starter/source/materials/fail/wilkins/hm_read_fail_wilkins.F
      !||    hm_read_fractal_dmg       ../starter/source/materials/fail/fractal/hm_read_fractal_dmg.F90
      !||    hm_read_mullins_or        ../starter/source/materials/fail/mullins_or/hm_read_fail_mullins_or.F
      !||    matparam_def_mod          ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    random_walk_dmg           ../starter/source/materials/fail/fractal/random_walk_dmg.F90
      !||    read_failparam            ../engine/source/output/restart/read_failparam.F
      !||    write_failparam           ../engine/source/output/restart/write_failparam.F
      !||--- uses       -----------------------------------------------------
      !||    names_and_titles_mod      ../common_source/modules/names_and_titles_mod.F
      !||    table4d_mod               ../common_source/modules/table4d_mod.F
      !||====================================================================
      module fail_param_mod
!
! ==================================================================================
!! \brief  to define data structure for failure and damage material model parameters
!! \details 


      use table4d_mod
      use names_and_titles_mod

!----------------------------------------------------------------------- 
!     included files
!----------------------------------------------------------------------- 

      implicit none
!
#include "my_real.inc"
!=======================================================================      
      
      type fail_param_
        character(len=nchartitle) :: keyword  !< failure model name
        integer     :: irupt                  !< failure model type (number)
        integer     :: fail_id                !< failure model Id
        integer     :: nuparam                !< number of real value paraameters
        integer     :: niparam                !< number of int value parameters
        integer     :: nuvar                  !< number of internal state variables
        integer     :: nfunc                  !< number of local functions
        integer     :: ntable                 !< number of local function tables
        integer     :: nmod                   !< number of rupture/damage modes
        integer     :: fail_ip                !< ruputure criterion (integration point based)              
        my_real     :: pthk                   !< ruputure criterion (layer thickness based)  
        
        character(len=nchartitle) ,dimension(:) ,allocatable :: mode   !< damage mode table
        my_real ,dimension(:) ,allocatable :: uparam  !< real value failure parameter table
        integer ,dimension(:) ,allocatable :: iparam  !< int  value failure parameter table
        integer ,dimension(:) ,allocatable :: ifunc   !< function table in failure models
        integer ,dimension(:) ,allocatable :: table   !< local function tables

        contains
          procedure :: destruct => destruct_fail_param

      end type fail_param_

      contains

      !||====================================================================
      !||    destruct_fail_param   ../common_source/modules/mat_elem/fail_param_mod.F90
      !||====================================================================
        subroutine destruct_fail_param(this)
          implicit none
          class(fail_param_) ,intent(inout) :: this
          if(allocated(this%mode))   deallocate(this%mode)
          if(allocated(this%uparam)) deallocate(this%uparam)
          if(allocated(this%iparam)) deallocate(this%iparam)
          if(allocated(this%ifunc))  deallocate(this%ifunc)
          if(allocated(this%table))  deallocate(this%table)
        end subroutine destruct_fail_param
!
!---------------
      end module fail_param_mod
