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
!Chd|  matparam_def_mod              modules/mat_elem/matparam_def_mod.f
!Chd|-- called by -----------
!Chd|        mat_elem_mod                  common_source/modules/mat_elem/mat_elem_mod.f
!Chd|        c3grhead                      starter/source/elements/sh3n/coque3n/c3grhead.f
!Chd|        c3grtails                     starter/source/elements/sh3n/coque3n/c3grtails.f
!Chd|        cbainit3                      starter/source/elements/shell/coqueba/cbainit3.f
!Chd|        cdkinit3                      starter/source/elements/sh3n/coquedk/cdkinit3.f
!Chd|        cfailini                      starter/source/elements/shell/coque/cfailini.f
!Chd|        cfailini4                     starter/source/elements/shell/coque/cfailini.f
!Chd|        cgrhead                       starter/source/elements/shell/coque/cgrhead.f
!Chd|        cgrtails                      starter/source/elements/shell/coque/cgrtails.f
!Chd|        cinit3                        starter/source/elements/shell/coque/cinit3.f
!Chd|        dometis                       starter/source/spmd/domain_decomposition/grid2mat.f
!Chd|        dometis2                      starter/source/spmd/domain_decomposition/domdecs.f
!Chd|        failini                       starter/source/elements/solid/solide/failini.f
!Chd|        fail_init                     starter/source/materials/fail/fail_init.f
!Chd|        fill_buffer_51_0              starter/source/materials/mat/mat051/fill_buffer_51_0.f
!Chd|        func_comp                     starter/source/materials/mat/mat076/law76_upd.f
!Chd|        hm_read_eos                   starter/source/materials/eos/hm_read_eos.f
!Chd|        hm_read_fail                  starter/source/materials/fail/hm_read_fail.f
!Chd|        hm_read_inistate_d00          starter/source/elements/initia/hm_read_inistate_d00.f
!Chd|        hm_read_mat                   starter/source/materials/mat/hm_read_mat.f
!Chd|        hm_read_mat00                 starter/source/materials/mat/mat000/hm_read_mat00.f
!Chd|        hm_read_mat01                 starter/source/materials/mat/mat001/hm_read_mat01.f
!Chd|        hm_read_mat02                 starter/source/materials/mat/mat002/hm_read_mat02.f
!Chd|        hm_read_mat03                 starter/source/materials/mat/mat003/hm_read_mat03.f
!Chd|        hm_read_mat04                 starter/source/materials/mat/mat004/hm_read_mat04.f
!Chd|        hm_read_mat05                 starter/source/materials/mat/mat005/hm_read_mat05.f
!Chd|        hm_read_mat06                 starter/source/materials/mat/mat006/hm_read_mat06.f
!Chd|        hm_read_mat06_keps            starter/source/materials/mat/mat006/hm_read_mat06_keps.f
!Chd|        hm_read_mat10                 starter/source/materials/mat/mat010/hm_read_mat10.f
!Chd|        hm_read_mat100                starter/source/materials/mat/mat100/hm_read_mat100.f
!Chd|        hm_read_mat101                starter/source/materials/mat/mat101/hm_read_mat101.f
!Chd|        hm_read_mat102                starter/source/materials/mat/mat102/hm_read_mat102.f
!Chd|        hm_read_mat103                starter/source/materials/mat/mat103/hm_read_mat103.f
!Chd|        hm_read_mat104                starter/source/materials/mat/mat104/hm_read_mat104.f
!Chd|        hm_read_mat106                starter/source/materials/mat/mat106/hm_read_mat106.f
!Chd|        hm_read_mat107                starter/source/materials/mat/mat107/hm_read_mat107.f
!Chd|        hm_read_mat108                starter/source/materials/mat/mat108/hm_read_mat108.f
!Chd|        hm_read_mat109                starter/source/materials/mat/mat109/hm_read_mat109.f
!Chd|        hm_read_mat11                 starter/source/materials/mat/mat011/hm_read_mat11.f
!Chd|        hm_read_mat110                starter/source/materials/mat/mat110/hm_read_mat110.f
!Chd|        hm_read_mat111                starter/source/materials/mat/mat111/hm_read_mat111.f
!Chd|        hm_read_mat112                starter/source/materials/mat/mat112/hm_read_mat112.f
!Chd|        hm_read_mat113                starter/source/materials/mat/mat113/hm_read_mat113.f
!Chd|        hm_read_mat114                starter/source/materials/mat/mat114/hm_read_mat114.f
!Chd|        hm_read_mat115                starter/source/materials/mat/mat115/hm_read_mat115.f
!Chd|        hm_read_mat116                starter/source/materials/mat/mat116/hm_read_mat116.f
!Chd|        hm_read_mat117                starter/source/materials/mat/mat117/hm_read_mat117.f
!Chd|        hm_read_mat119                starter/source/materials/mat/mat119/hm_read_mat119.f
!Chd|        hm_read_mat11_k_eps           starter/source/materials/mat/mat011/hm_read_mat11_k_eps.f
!Chd|        hm_read_mat12                 starter/source/materials/mat/mat012/hm_read_mat12.f
!Chd|        hm_read_mat120                starter/source/materials/mat/mat120/hm_read_mat120.f
!Chd|        hm_read_mat121                starter/source/materials/mat/mat121/hm_read_mat121.f
!Chd|        hm_read_mat122                starter/source/materials/mat/mat122/hm_read_mat122.f
!Chd|        hm_read_mat124                starter/source/materials/mat/mat124/hm_read_mat124.f
!Chd|        hm_read_mat13                 starter/source/materials/mat/mat013/hm_read_mat13.f
!Chd|        hm_read_mat14                 starter/source/materials/mat/mat014/hm_read_mat14.f
!Chd|        hm_read_mat15                 starter/source/materials/mat/mat015/hm_read_mat15.f
!Chd|        hm_read_mat151                starter/source/materials/mat/mat151/hm_read_mat151.f
!Chd|        hm_read_mat158                starter/source/materials/mat/mat158/hm_read_mat158.f
!Chd|        hm_read_mat16                 starter/source/materials/mat/mat016/hm_read_mat16.f
!Chd|        hm_read_mat18                 starter/source/materials/mat/mat018/hm_read_mat18.f
!Chd|        hm_read_mat19                 starter/source/materials/mat/mat019/hm_read_mat19.f
!Chd|        hm_read_mat190                starter/source/materials/mat/mat190/hm_read_mat190.f
!Chd|        hm_read_mat20                 starter/source/materials/mat/mat020/hm_read_mat20.f
!Chd|        hm_read_mat21                 starter/source/materials/mat/mat021/hm_read_mat21.f
!Chd|        hm_read_mat22                 starter/source/materials/mat/mat022/hm_read_mat22.f
!Chd|        hm_read_mat23                 starter/source/materials/mat/mat023/hm_read_mat23.f
!Chd|        hm_read_mat24                 starter/source/materials/mat/mat024/hm_read_mat24.f
!Chd|        hm_read_mat25                 starter/source/materials/mat/mat025/hm_read_mat25.f
!Chd|        hm_read_mat26                 starter/source/materials/mat/mat026/hm_read_mat26.f
!Chd|        hm_read_mat27                 starter/source/materials/mat/mat027/hm_read_mat27.f
!Chd|        hm_read_mat28                 starter/source/materials/mat/mat028/hm_read_mat28.f
!Chd|        hm_read_mat29_31              starter/source/materials/mat/matuser/hm_read_mat_user29_31.f
!Chd|        hm_read_mat32                 starter/source/materials/mat/mat032/hm_read_mat32.f
!Chd|        hm_read_mat33                 starter/source/materials/mat/mat033/hm_read_mat33.f
!Chd|        hm_read_mat34                 starter/source/materials/mat/mat034/hm_read_mat34.f
!Chd|        hm_read_mat35                 starter/source/materials/mat/mat035/hm_read_mat35.f
!Chd|        hm_read_mat36                 starter/source/materials/mat/mat036/hm_read_mat36.f
!Chd|        hm_read_mat37                 starter/source/materials/mat/mat037/hm_read_mat37.f
!Chd|        hm_read_mat38                 starter/source/materials/mat/mat038/hm_read_mat38.f
!Chd|        hm_read_mat40                 starter/source/materials/mat/mat040/hm_read_mat40.f
!Chd|        hm_read_mat41                 starter/source/materials/mat/mat041/hm_read_mat41.f
!Chd|        hm_read_mat42                 starter/source/materials/mat/mat042/hm_read_mat42.f
!Chd|        hm_read_mat43                 starter/source/materials/mat/mat043/hm_read_mat43.f
!Chd|        hm_read_mat44                 starter/source/materials/mat/mat044/hm_read_mat44.f
!Chd|        hm_read_mat46                 starter/source/materials/mat/mat046/hm_read_mat46.f
!Chd|        hm_read_mat48                 starter/source/materials/mat/mat048/hm_read_mat48.f
!Chd|        hm_read_mat49                 starter/source/materials/mat/mat049/hm_read_mat49.f
!Chd|        hm_read_mat50                 starter/source/materials/mat/mat050/hm_read_mat50.f
!Chd|        hm_read_mat51                 starter/source/materials/mat/mat051/hm_read_mat51.f
!Chd|        hm_read_mat52                 starter/source/materials/mat/mat052/hm_read_mat52.f
!Chd|        hm_read_mat53                 starter/source/materials/mat/mat053/hm_read_mat53.f
!Chd|        hm_read_mat54                 starter/source/materials/mat/mat054/hm_read_mat54.f
!Chd|        hm_read_mat57                 starter/source/materials/mat/mat057/hm_read_mat57.f
!Chd|        hm_read_mat58                 starter/source/materials/mat/mat058/hm_read_mat58.f
!Chd|        hm_read_mat59                 starter/source/materials/mat/mat059/hm_read_mat59.f
!Chd|        hm_read_mat60                 starter/source/materials/mat/mat060/hm_read_mat60.f
!Chd|        hm_read_mat62                 starter/source/materials/mat/mat062/hm_read_mat62.f
!Chd|        hm_read_mat63                 starter/source/materials/mat/mat063/hm_read_mat63.f
!Chd|        hm_read_mat64                 starter/source/materials/mat/mat064/hm_read_mat64.f
!Chd|        hm_read_mat65                 starter/source/materials/mat/mat065/hm_read_mat65.f
!Chd|        hm_read_mat66                 starter/source/materials/mat/mat066/hm_read_mat66.f
!Chd|        hm_read_mat68                 starter/source/materials/mat/mat068/hm_read_mat68.f
!Chd|        hm_read_mat69                 starter/source/materials/mat/mat069/hm_read_mat69.f
!Chd|        hm_read_mat70                 starter/source/materials/mat/mat070/hm_read_mat70.f
!Chd|        hm_read_mat71                 starter/source/materials/mat/mat071/hm_read_mat71.f
!Chd|        hm_read_mat72                 starter/source/materials/mat/mat072/hm_read_mat72.f
!Chd|        hm_read_mat73                 starter/source/materials/mat/mat073/hm_read_mat73.f
!Chd|        hm_read_mat74                 starter/source/materials/mat/mat074/hm_read_mat74.f
!Chd|        hm_read_mat75                 starter/source/materials/mat/mat075/hm_read_mat75.f
!Chd|        hm_read_mat76                 starter/source/materials/mat/mat076/hm_read_mat76.f
!Chd|        hm_read_mat77                 starter/source/materials/mat/mat077/hm_read_mat77.f
!Chd|        hm_read_mat78                 starter/source/materials/mat/mat078/hm_read_mat78.f
!Chd|        hm_read_mat79                 starter/source/materials/mat/mat079/hm_read_mat79.f
!Chd|        hm_read_mat80                 starter/source/materials/mat/mat080/hm_read_mat80.f
!Chd|        hm_read_mat81                 starter/source/materials/mat/mat081/hm_read_mat81.f
!Chd|        hm_read_mat82                 starter/source/materials/mat/mat082/hm_read_mat82.f
!Chd|        hm_read_mat83                 starter/source/materials/mat/mat083/hm_read_mat83.f
!Chd|        hm_read_mat84                 starter/source/materials/mat/mat084/hm_read_mat84.f
!Chd|        hm_read_mat87                 starter/source/materials/mat/mat087/hm_read_mat87.f
!Chd|        hm_read_mat88                 starter/source/materials/mat/mat088/hm_read_mat88.f
!Chd|        hm_read_mat90                 starter/source/materials/mat/mat090/hm_read_mat90.f
!Chd|        hm_read_mat92                 starter/source/materials/mat/mat092/hm_read_mat92.f
!Chd|        hm_read_mat93                 starter/source/materials/mat/mat093/hm_read_mat93.f
!Chd|        hm_read_mat94                 starter/source/materials/mat/mat094/hm_read_mat94.f
!Chd|        hm_read_mat95                 starter/source/materials/mat/mat095/hm_read_mat95.f
!Chd|        hm_read_mat97                 starter/source/materials/mat/mat097/hm_read_mat97.f
!Chd|        hm_read_mat_99                starter/source/materials/mat/matuser/hm_read_mat_user_99.f
!Chd|        hm_read_nonlocal              starter/source/materials/nonlocal/hm_read_nonlocal.f
!Chd|        hm_read_visc                  starter/source/materials/visc/hm_read_visc.f
!Chd|        inigrav_eos                   starter/source/initial_conditions/inigrav/inigrav_eos.f
!Chd|        inigrav_load                  starter/source/initial_conditions/inigrav/inigrav_load.f
!Chd|        initia                        starter/source/elements/initia/initia.f
!Chd|        initwg                        starter/source/spmd/domain_decomposition/initwg.f
!Chd|        initwg_shell                  starter/source/spmd/domain_decomposition/initwg_shell.f
!Chd|        initwg_solid                  starter/source/spmd/domain_decomposition/initwg_solid.f
!Chd|        initwg_tri                    starter/source/spmd/domain_decomposition/initwg_tri.f
!Chd|        init_mat_keyword              starter/source/materials/mat/init_mat_keyword.f
!Chd|        ini_inimap1d                  starter/source/initial_conditions/inimap/ini_inimap1d.f
!Chd|        law104_upd                    starter/source/materials/mat/mat104/law104_upd.f
!Chd|        law190_upd                    starter/source/materials/mat/mat190/law190_upd.f
!Chd|        law70_upd                     starter/source/materials/mat/mat070/law70_upd.f
!Chd|        law76_upd                     starter/source/materials/mat/mat076/law76_upd.f
!Chd|        law77_upd                     starter/source/materials/mat/mat077/law77_upd.f
!Chd|        lec_inistate                  starter/source/elements/initia/lec_inistate.f
!Chd|        mulaw                         starter/source/materials/mat_share/mulaw.f
!Chd|        pgrtails                      starter/source/elements/beam/pgrtails.f
!Chd|        qgrhead                       starter/source/elements/solid_2d/quad/qgrhead.f
!Chd|        qgrtails                      starter/source/elements/solid_2d/quad/qgrtails.f
!Chd|        r2r_group                     starter/source/coupling/rad2rad/r2r_group.f
!Chd|        r2r_matparam_copy             starter/source/elements/elbuf_init/r2r_matparam_copy.f
!Chd|        s10init3                      starter/source/elements/solid/solide10/s10init3.f
!Chd|        s16init3                      starter/source/elements/thickshell/solide16/s16init3.f
!Chd|        s20init3                      starter/source/elements/solid/solide20/s20init3.f
!Chd|        s4init3                       starter/source/elements/solid/solide4/s4init3.f
!Chd|        s6cinit3                      starter/source/elements/thickshell/solide6c/s6cinit3.f
!Chd|        s8cinit3                      starter/source/elements/thickshell/solide8c/s8cinit3.f
!Chd|        s8zinit3                      starter/source/elements/solid/solide8z/s8zinit3.f
!Chd|        sgrhead                       starter/source/elements/solid/solide/sgrhead.f
!Chd|        sgrtails                      starter/source/elements/solid/solide/sgrtails.f
!Chd|        sigeps70                      starter/source/materials/mat/mat070/sigeps70.f
!Chd|        sinit3                        starter/source/elements/solid/solide/sinit3.f
!Chd|        solve_eint                    starter/source/initial_conditions/inimap/ini_inimap1d.f
!Chd|        spgrhead                      starter/source/elements/sph/spgrhead.f
!Chd|        spgrtails                     starter/source/elements/sph/spgrtails.f
!Chd|        suinit3                       starter/source/elements/elbuf_init/suinit3.f
!Chd|        t3grhead                      starter/source/elements/solid_2d/tria/t3grhead.f
!Chd|        t3grtails                     starter/source/elements/solid_2d/tria/t3grtails.f
!Chd|        tagnod_r2r_nl                 starter/source/coupling/rad2rad/tagnod_r2r_nl.f
!Chd|        updmat                        starter/source/materials/updmat.f
!Chd|        eig                           engine/stub/eig.f             
!Chd|        eig1                          engine/stub/eig1.f            
!Chd|        eigp                          engine/stub/eigp.f            
!Chd|        genani                        engine/source/output/anim/generate/genani.f
!Chd|        genh3d                        engine/source/output/h3d/h3d_results/genh3d.f
!Chd|        h3d_shell_scalar              engine/source/output/h3d/h3d_results/h3d_shell_scalar.f
!Chd|        h3d_shell_scalar_1            engine/source/output/h3d/h3d_results/h3d_shell_scalar_1.f
!Chd|        h3d_shell_tensor              engine/source/output/h3d/h3d_results/h3d_shell_tensor.f
!Chd|        h3d_solid_scalar              engine/source/output/h3d/h3d_results/h3d_solid_scalar.f
!Chd|        hist2                         engine/source/output/th/hist2.f
!Chd|        imp_buck                      engine/source/implicit/imp_buck.f
!Chd|        lech3d                        engine/source/output/h3d/h3d_build_fortran/lech3d.f
!Chd|        s4voln_m                      engine/source/elements/solid/solide4_sfem/s4voln_m.f
!Chd|        sigeps190                     engine/source/materials/mat/mat190/sigeps190.f
!Chd|        sigeps25c                     engine/source/materials/mat/mat025/sigeps25c.f
!Chd|        sigeps25cp                    engine/source/materials/mat/mat025/sigeps25cp.f
!Chd|        sigeps50                      engine/source/materials/mat/mat050/sigeps50.f
!Chd|        sigeps66c                     engine/source/materials/mat/mat066/sigeps66c.f
!Chd|        sigeps70                      engine/source/materials/mat/mat070/sigeps70.f
!Chd|        sigeps75                      engine/source/materials/mat/mat075/sigeps75.f
!Chd|        sigeps76                      engine/source/materials/mat/mat076/sigeps76.f
!Chd|        sigeps76c                     engine/source/materials/mat/mat076/sigeps76c.f
!Chd|        tensorc                       engine/source/output/anim/generate/tensorc.f
!Chd|        thcoq                         engine/source/output/th/thcoq.f
!Chd|        check_mat_elem_prop_compatibilitystarter/source/materials/mat/check_mat_elem_prop_compatibility.f
!Chd|-- calls ---------------
!Chd|        fail_param_mod                modules/mat_elem/fail_param_mod.f
!Chd|        names_and_titles_mod          modules/names_and_titles_mod.f
!Chd|        table4d_mod                   modules/table4d_mod.f         
!Chd|        visc_param_mod                modules/mat_elem/visc_param_mod.f
!Chd|====================================================================
      module matparam_def_mod
!
      use table4d_mod
      use visc_param_mod
      use fail_param_mod
      use names_and_titles_mod
! ----------------------------------------------------------------------------------------------------------------------
!     included files
!----------------------------------------------------------------------- 
      implicit none

#include "my_real.inc"

!=======================================================================      
  !! \brief module to define data structure for all material model parameters 
  !! \details  allocatable dimension : nummat
!=======================================================================      
!
      type matparam_struct_
      
        character(len=nchartitle) :: title  !< material law title
        integer     :: ilaw                 !< material law number (type)    
        integer     :: mat_id               !< material law id   
        integer     :: nuparam              !< number of real value material paraameters
        integer     :: niparam              !< number of int value material parameters
        integer     :: nfunc                !< number of local functions in material
        integer     :: ntable               !< number of local function tables
        integer     :: nsubmat              !< number of submaterials (multi-mat law51)
        integer     :: nfail                !< number of failure models
        integer     :: ivisc                !< viscosity model number
        integer     :: ieos                 !< eos model number
        integer     :: itherm               !< therm model number                       
        ! -------  material characteristics flags
        integer     :: compressibility      !< "compressible","incompressible","elasto_plastic"
        integer     :: smstr                !< "small_strain", "large_strain"
        integer     :: strain_formulation   !< "total", "incremental"
        integer     :: ipres                !< "hydrostatic",hydro_eos","hook"
        integer     :: orthotropy           !< "isotropic", "orthotropic", "anisotropic"
        ! ------- compatibility flags
        integer     :: prop_solid           !< "solid_isotropic","solid_orthotropic","solid_composite","solid_cohesive"   ,"solid_porous","solid_all"
        integer     :: prop_shell           !< "shell_isotropic","shell_orthotropic","shell_composite","shell_anisotropic","shell_all"
        integer     :: prop_beam            !< "beam_classic"   ,"beam_integrated"  ,"beam_all"
        integer     :: prop_spring          !< "spring_predit"  ,"spring_material"  ,"spring_all"
        integer     :: prop_truss           !< "truss"
        integer     :: prop_sph             !< "sph"
        integer     :: compatibility_eos    !< "eos"
        integer     :: compatibility_visc   !< "visc"
!        integer     :: compatibility_nloc   !< "nloc"
        ! --------------------------------- !<  
        integer     :: nloc                 !< non-local variable regularization flag
        integer     :: ifailwave            !< failwave propagation flag
        integer     :: ixfem                !< xfem flag
        ! --------------------------------- !<
        integer     :: nmod                 !< number of rupture/damage modes
        ! --------------------------------- !
        my_real     ::  rho
        my_real     ::  rho0        
        my_real     ::  young        
!                        
        my_real                   ,dimension(:) ,allocatable :: uparam !< real value material parameter table (nuparam)
        integer                   ,dimension(:) ,allocatable :: iparam !< integer value material parameter table (niparam)
        type (table_4d_)          ,dimension(:) ,allocatable :: table  !< local function tables
        character(len=nchartitle) ,dimension(:) ,allocatable :: mode   !< damage mode keywords
!                
        type (fail_param_),dimension(:) ,allocatable :: fail     !< failure models data structure (nfail)
        type (visc_param_)                           :: visc     !< viscosity model data structure        

!        type (eos_param_)    :: eos                             !< eos model data structure (to be defined)     
!        type (therm_param_)  :: therm                           !< thermal model data structure (to be defined)           
!        type (submat_)  ,dimension(:) ,allocatable :: submat    !< multi material data structure (to be defined) 

      end type matparam_struct_
!
!---------------
      end module matparam_def_mod
