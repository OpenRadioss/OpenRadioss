Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
Copyright>    
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>    
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>    
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>    
Copyright>    
Copyright>        Commercial Alternative: Altair Radioss Software 
Copyright>    
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss 
Copyright>        software under a commercial license.  Contact Altair to discuss further if the 
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.    
Chd|====================================================================
Chd|  ALE_CONNECTIVITY_MOD          modules/ale_connectivity_mod.F
Chd|-- called by -----------
Chd|        AGRAD2                        starter/source/ale/ale2d/agrad2.F
Chd|        AGRAD3                        starter/source/ale/ale3d/agrad3.F
Chd|        ALELEC                        starter/source/ale/alelec.F   
Chd|        ALESO2                        starter/source/ale/ale2d/aleso2.F
Chd|        ALESO2T                       starter/source/ale/ale2d/aleso2t.F
Chd|        ALESO3                        starter/source/ale/ale3d/aleso3.F
Chd|        ALESOP                        starter/source/ale/alesop.F   
Chd|        BINIT2                        starter/source/ale/bimat/binit2.F
Chd|        C_IDGLOB                      starter/source/restart/ddsplit/c_idglob.F
Chd|        C_IXFLOC                      starter/source/restart/ddsplit/c_ixfloc.F
Chd|        C_SPMD_NE_CONNECT             starter/source/ale/spmd_ne_connect.F
Chd|        C_VOIS                        starter/source/restart/ddsplit/c_vois.F
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        EPORIN3                       starter/source/ale/ale3d/eporin3.F
Chd|        FINDELE                       starter/source/boundary_conditions/ebcs/findele.F
Chd|        I12CHK3                       starter/source/interfaces/inter3d1/i12chk3.F
Chd|        IG3DINIT3                     starter/source/elements/ige3d/ig3dinit3.F
Chd|        INIEBCS                       starter/source/boundary_conditions/ebcs/iniebcs.F
Chd|        INIGRAV_LOAD                  starter/source/initial_conditions/inigrav/inigrav_load.F
Chd|        INIGRAV_M51                   starter/source/initial_conditions/inigrav/inigrav_m51.F
Chd|        ININT3                        starter/source/interfaces/inter3d1/inint3.F
Chd|        ININTR                        starter/source/interfaces/interf1/inintr.F
Chd|        INITIA                        starter/source/elements/initia/initia.F
Chd|        LECTUR                        starter/source/starter/lectur.F
Chd|        M51INIT                       starter/source/materials/mat/mat051/m51init.F
Chd|        MATINI                        starter/source/materials/mat_share/matini.F
Chd|        MULTIFLUID_INIT2              starter/source/multifluid/multifluid_init2.F
Chd|        MULTIFLUID_INIT2T             starter/source/multifluid/multifluid_init2t.F
Chd|        MULTIFLUID_INIT3              starter/source/multifluid/multifluid_init3.F
Chd|        MULTIFLUID_INIT3T             starter/source/multifluid/multifluid_init3t.F
Chd|        MULTI_CONNECTIVITY            starter/source/multifluid/multi_connectivity.F
Chd|        MULTI_UNPLUG_NEIGHBORS        starter/source/multifluid/multi_unplug_neighbors.F
Chd|        NRF51INI                      starter/source/materials/mat/mat051/nrf51ini.F
Chd|        Q4INIT2                       starter/source/elements/solid_2d/quad4/q4init2.F
Chd|        QINIT2                        starter/source/elements/solid_2d/quad/qinit2.F
Chd|        S10INIT3                      starter/source/elements/solid/solide10/s10init3.F
Chd|        S16INIT3                      starter/source/elements/thickshell/solide16/s16init3.F
Chd|        S20INIT3                      starter/source/elements/solid/solide20/s20init3.F
Chd|        S4INIT3                       starter/source/elements/solid/solide4/s4init3.F
Chd|        S6CINIT3                      starter/source/elements/thickshell/solide6c/s6cinit3.F
Chd|        S8CINIT3                      starter/source/elements/thickshell/solide8c/s8cinit3.F
Chd|        S8ZINIT3                      starter/source/elements/solid/solide8z/s8zinit3.F
Chd|        SCINIT3                       starter/source/elements/thickshell/solidec/scinit3.F
Chd|        SINIT3                        starter/source/elements/solid/solide/sinit3.F
Chd|        SPINIT3                       starter/source/elements/sph/spinit3.F
Chd|        SPLIT_CFD_SOLIDE              starter/source/spmd/split_cfd_solide.F
Chd|        SPMD_NE_CONNECT               starter/source/ale/spmd_ne_connect.F
Chd|        SUINIT3                       starter/source/elements/elbuf_init/suinit3.F
Chd|        WRCOMIP                       starter/source/restart/ddsplit/wrcommp.F
Chd|        W_FRONT                       starter/source/restart/ddsplit/w_front.F
Chd|        W_ING2LOC                     starter/source/restart/ddsplit/w_ing2loc.F
Chd|        A4CONV3                       engine/source/ale/ale3d/a4conv3.F
Chd|        A4FLUX3                       engine/source/ale/ale3d/a4flux3.F
Chd|        ACONV2                        engine/source/ale/ale2d/aconv2.F
Chd|        ACONV3                        engine/source/ale/ale3d/aconv3.F
Chd|        ACONVE                        engine/source/ale/aconve.F    
Chd|        ADIFF2                        engine/source/ale/ale2d/adiff2.F
Chd|        ADIFF3                        engine/source/ale/ale3d/adiff3.F
Chd|        AETURB                        engine/source/ale/turbulence/aeturb.F
Chd|        AFIMP2                        engine/source/ale/ale2d/afimp2.F
Chd|        AFIMP3                        engine/source/ale/ale3d/afimp3.F
Chd|        AFLUX0                        engine/source/ale/aflux0.F    
Chd|        AFLUX2                        engine/source/ale/ale2d/aflux2.F
Chd|        AFLUX3                        engine/source/ale/ale3d/aflux3.F
Chd|        AFLUXT                        engine/source/ale/ale51/afluxt.F
Chd|        AGAUG3                        engine/source/ale/agauge.F    
Chd|        AGAUGE                        engine/source/ale/agauge.F    
Chd|        AGRAD0                        engine/source/ale/agrad0.F    
Chd|        AGRAD2                        engine/source/ale/ale2d/agrad2.F
Chd|        AGRAD3                        engine/source/ale/ale3d/agrad3.F
Chd|        AKTURB                        engine/source/ale/turbulence/akturb.F
Chd|        ALE51_ANTIDIFF2               engine/source/ale/ale51/ale51_antidiff2.F
Chd|        ALE51_ANTIDIFF3               engine/source/ale/ale51/ale51_antidiff3.F
Chd|        ALE51_FINISH                  engine/source/ale/ale51/ale51_finish.F
Chd|        ALE51_GRADIENT_RECONSTRUCTION engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
Chd|        ALE51_INIT                    engine/source/ale/ale51/ale51_init.F
Chd|        ALE51_SPMD2                   engine/source/ale/ale51/ale51_spmd2.F
Chd|        ALE51_SPMD3                   engine/source/ale/ale51/ale51_spmd3.F
Chd|        ALE51_UPWIND2                 engine/source/ale/ale51/ale51_upwind2.F
Chd|        ALE51_UPWIND3                 engine/source/ale/ale51/ale51_upwind3.F
Chd|        ALECONV3                      engine/source/ale/porous/aleconv.F
Chd|        ALECONVE                      engine/source/ale/porous/aleconv.F
Chd|        ALEFLOW                       engine/source/ale/porous/aleflow.F
Chd|        ALEFLUX                       engine/source/ale/porous/aleflux.F
Chd|        ALEFVM_AFLUX3                 engine/source/ale/alefvm/alefvm_aflux3.F
Chd|        ALEFVM_EFLUX3                 engine/source/ale/alefvm/alefvm_eflux3.F
Chd|        ALEFVM_MAIN                   engine/source/ale/alefvm/alefvm_main.F
Chd|        ALEFVM_SFINT3                 engine/source/ale/alefvm/alefvm_sfint3.F
Chd|        ALEFVM_TFEXT                  engine/source/ale/alefvm/alefvm_tfext.F
Chd|        ALEMAIN                       engine/source/ale/alemain.F   
Chd|        ALEMUSCL_UPWIND               engine/source/ale/alemuscl/alemuscl_upwind.F
Chd|        ALEMUSCL_UPWIND2              engine/source/ale/alemuscl/alemuscl_upwind2.F
Chd|        ALETHE                        engine/source/ale/alethe.F    
Chd|        ALEW                          engine/source/ale/grid/alew.F 
Chd|        ALEW1                         engine/source/ale/grid/alew1.F
Chd|        ALEW5                         engine/source/ale/grid/alew5.F
Chd|        ALEW6                         engine/source/ale/grid/alew6.F
Chd|        ALEWDX                        engine/source/ale/grid/alewdx.F
Chd|        AMULF2                        engine/source/ale/bimat/amulf2.F
Chd|        AREZO2                        engine/source/ale/ale2d/arezo2.F
Chd|        AREZO2_SIG                    engine/source/ale/ale2d/arezo2.F
Chd|        AREZO3                        engine/source/ale/ale3d/arezo3.F
Chd|        AREZO3_SIG                    engine/source/ale/ale3d/arezo3.F
Chd|        AREZON                        engine/source/ale/arezon.F    
Chd|        ATHERM                        engine/source/ale/atherm.F    
Chd|        BCONV2                        engine/source/ale/ale2d/bconv2.F
Chd|        BFORC2                        engine/source/ale/bimat/bforc2.F
Chd|        BREZO2                        engine/source/ale/ale2d/brezo2.F
Chd|        BREZO2_SIG                    engine/source/ale/ale2d/brezo2.F
Chd|        BUILD_CONNECTIVITY            engine/source/multifluid/connectivity.F
Chd|        DFUNC0                        engine/source/output/anim/generate/dfunc0.F
Chd|        DFUNCC                        engine/source/output/anim/generate/dfuncc.F
Chd|        DFUNCS                        engine/source/output/anim/generate/dfunc6.F
Chd|        EDE112                        engine/source/ale/euler2d/ede112.F
Chd|        EFLUX2                        engine/source/ale/euler2d/eflux2.F
Chd|        EFLUX3                        engine/source/ale/euler3d/eflux3.F
Chd|        EGRAD2                        engine/source/ale/euler2d/egrad2.F
Chd|        EGRAD3                        engine/source/ale/euler3d/egrad3.F
Chd|        EIG                           engine/stub/eig.F             
Chd|        EIG1                          engine/stub/eig1.F            
Chd|        EIGCOND                       engine/stub/eigcond.F         
Chd|        EIGP                          engine/stub/eigp.F            
Chd|        FORINT                        engine/source/elements/forint.F
Chd|        GENANI                        engine/source/output/anim/generate/genani.F
Chd|        GENH3D                        engine/source/output/h3d/h3d_results/genh3d.F
Chd|        GRADIENT_RECONSTRUCTION       engine/source/ale/alemuscl/gradient_reconstruction.F
Chd|        GRADIENT_RECONSTRUCTION2      engine/source/ale/alemuscl/gradient_reconstruction2.F
Chd|        H3D_NODAL_SCALAR              engine/source/output/h3d/h3d_results/h3d_nodal_scalar.F
Chd|        H3D_QUAD_SCALAR               engine/source/output/h3d/h3d_results/h3d_quad_scalar.F
Chd|        H3D_SHELL_SCALAR              engine/source/output/h3d/h3d_results/h3d_shell_scalar.F
Chd|        H3D_SHELL_SCALAR_1            engine/source/output/h3d/h3d_results/h3d_shell_scalar_1.F
Chd|        H3D_SOLID_SCALAR              engine/source/output/h3d/h3d_results/h3d_solid_scalar.F
Chd|        H3D_SOLID_SCALAR_1            engine/source/output/h3d/h3d_results/h3d_solid_scalar_1.F
Chd|        I18DST3                       engine/source/interfaces/int18/i18dst3.F
Chd|        I18MAIN_KINE_1                engine/source/interfaces/int18/i18main_kine.F
Chd|        I18MAIN_KINE_I                engine/source/interfaces/int18/i18main_kine.F
Chd|        I22BUCE                       engine/source/interfaces/intsort/i22buce.F
Chd|        I22MAIN_TRI                   engine/source/interfaces/intsort/i22main_tri.F
Chd|        I7MAINF                       engine/source/interfaces/int07/i7mainf.F
Chd|        IG3DUFORC3                    engine/source/elements/ige3d/ig3duforc3.F
Chd|        IMP_BUCK                      engine/source/implicit/imp_buck.F
Chd|        INTFOP2                       engine/source/interfaces/interf/intfop2.F
Chd|        INTTRI                        engine/source/interfaces/intsort/inttri.F
Chd|        M11LAW                        engine/source/materials/mat/mat011/m11law.F
Chd|        M11VS2                        engine/source/materials/mat/mat011/m11vs2.F
Chd|        M11VS3                        engine/source/materials/mat/mat011/m11vs3.F
Chd|        M51VOIS2                      engine/source/materials/mat/mat051/m51vois2.F
Chd|        M51VOIS3                      engine/source/materials/mat/mat051/m51vois3.F
Chd|        MMAIN                         engine/source/materials/mat_share/mmain.F
Chd|        MULAW                         engine/source/materials/mat_share/mulaw.F
Chd|        MULTI_FLUXES_COMPUTATION      engine/source/multifluid/multi_fluxes_computation.F
Chd|        MULTI_MUSCL_GRADIENTS         engine/source/multifluid/multi_muscl_gradients.F
Chd|        MULTI_TIMEEVOLUTION           engine/source/multifluid/multi_timeevolution.F
Chd|        NODAL_SCHLIEREN               engine/source/output/anim/generate/nodal_schlieren.F
Chd|        Q4FORC2                       engine/source/elements/solid_2d/quad4/q4forc2.F
Chd|        QFORC2                        engine/source/elements/solid_2d/quad/qforc2.F
Chd|        RADIOSS2                      engine/source/engine/radioss2.F
Chd|        RDRESB                        engine/source/output/restart/rdresb.F
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|        RESOL_HEAD                    engine/source/engine/resol_head.F
Chd|        RESTALLOC                     engine/source/output/restart/arralloc.F
Chd|        S10FORC3                      engine/source/elements/solid/solide10/s10forc3.F
Chd|        S11DEFO3                      engine/source/elements/solid/solide/s11defo3.F
Chd|        S11FX3                        engine/source/elements/solid/solide/s11fx3.F
Chd|        S16FORC3                      engine/source/elements/thickshell/solide16/s16forc3.F
Chd|        S20FORC3                      engine/source/elements/solid/solide20/s20forc3.F
Chd|        S4FORC3                       engine/source/elements/solid/solide4/s4forc3.F
Chd|        S6CFORC3                      engine/source/elements/thickshell/solide6c/s6cforc3.F
Chd|        S8CFORC3                      engine/source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      engine/source/elements/solid/solide8e/s8eforc3.F
Chd|        S8FORC3                       engine/source/elements/solid/solide8/s8forc3.F
Chd|        S8SFORC3                      engine/source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      engine/source/elements/solid/solide8z/s8zforc3.F
Chd|        SCFORC3                       engine/source/elements/thickshell/solidec/scforc3.F
Chd|        SCHLIEREN                     engine/source/output/anim/generate/schlieren.F
Chd|        SEGGETV                       engine/source/interfaces/interf/seggetv.F
Chd|        SFORC3                        engine/source/elements/solid/solide/sforc3.F
Chd|        SIGEPS51                      engine/source/materials/mat/mat051/sigeps51.F
Chd|        SINIT22_FVM                   engine/source/interfaces/int22/sinit22_fvm.F
Chd|        SORTIE_MAIN                   engine/source/output/sortie_main.F
Chd|        SPSTRES                       engine/source/elements/sph/spstres.F
Chd|        SZFORC3                       engine/source/elements/solid/solidez/szforc3.F
Chd|        WRRESTP                       engine/source/output/restart/wrrestp.F
Chd|        ALE51_GRADIENT_RECONSTRUCTION2engine/source/ale/alemuscl/ale51_gradient_reconstruction2.F
Chd|        MULTI_MUSCL_FLUXES_COMPUTATIONengine/source/multifluid/multi_muscl_fluxes_computation.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE ALE_CONNECTIVITY_MOD

!     ************    !
!     Connectivity
!     ************    !
      type, public :: t_connectivity
      integer, dimension(:), allocatable :: iad_connect
      integer, dimension(:), allocatable :: connected
      integer, dimension(:), allocatable :: type
      end type t_connectivity

!     Extended type : + iface
      type, public, extends(t_connectivity) :: t_connectivity_ext1
      integer, dimension(:), allocatable :: iface2
      end typet_connectivity_ext1


!     ******************    !
!     Extended local ids    !
!     ******************    !
      type :: t_idglob
      integer, dimension(:), allocatable :: id
      integer, dimension(:), allocatable :: uid
      end type t_idglob

!     ****************     !
!     ALE connectivity     !
!     ****************     !
      type, public :: t_ale_connectivity
!     node-node, node-element, element-element
      type(t_connectivity) :: nn_connect, ne_connect
      type(t_connectivity_ext1) :: ee_connect
      logical :: has_nn_connect = .false.
      logical :: has_ne_connect = .false.
      logical :: has_ee_connect = .false.
      logical :: has_idglob = .false.
      logical :: nale_already_computed = .false.
      integer, dimension(:), allocatable :: nale
      type(t_idglob) :: idglob
      contains
      procedure, pass :: ale_connectivity_init
      procedure, pass :: ale_compute_connectivity
      procedure, pass :: ale_compute_ee_connectivity
      procedure, pass :: ale_deallocate_connectivity
      end type t_ale_connectivity

!     ***********    !
!     Subroutines    !
!     ***********    !
      CONTAINS
      
      subroutine ale_connectivity_init(this)
      implicit none
      class(t_ale_connectivity), intent(inout) :: this
#include "com01_c.inc"
#include "inter18.inc"
#include "ale_param.inc"   
      logical result  
      result = .false.
      if(iale > 0 .and. nwale == 6)result=.true.                 !/ale/grid/volume defined
      if(inter18_autoparam == 1)result=.true. !/inter/type18 with flag iauto set to 1
      if(inter18_is_variable_gap_defined)result=.true. !/inter/type18 with flag iauto set to 1      
      this%has_ne_connect = result
      result = .false.
      if(iale > 0 .and. nwale < 2)result=.true.     !/ale/grid/donea, disp, spring
      if(iale > 0 .and. nwale == 5)result=.true. !/ale/grid/laplacian
      this%has_nn_connect = result
      end subroutine ale_connectivity_init
      subroutine ale_compute_connectivity(this, numnod, numelq, numeltg, numels, 
     .     nixq, nixtg, nixs, itab,
     .     ixq, ixtg, ixs)
      implicit none
C-----------------------------------------------
C     D e s c r i p t i o n
C-----------------------------------------------
!     Comptes Node to Element connectivities,    !
!     and Node to Node connectivities for ALE    !
!     grid velocity formulations                 !
!     ---------------------------------------    !   
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      class(t_ale_connectivity), intent(inout) :: this
      integer, intent(in) :: numnod, numelq, numeltg, numels, 
     .     nixq, nixtg, nixs
      integer, dimension(numnod), intent(in) :: itab
      integer, dimension(nixq, numelq), intent(in) :: ixq
      integer, dimension(nixtg, numeltg), intent(in) :: ixtg
      integer, dimension(nixs, numels), intent(in) :: ixs
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      integer :: ii, jj, node_id, node1, node2
      integer, dimension(:), allocatable :: adsky
      integer :: iad1, iad2, itmp
      integer :: max_edge, nb_edge, nb_edge_new, iedge, cur_pos
      integer, dimension(:, :), allocatable :: edges, edges_tmp
      integer, dimension(:), allocatable :: idx
      integer, dimension(2, 3) :: tri_edge
      integer, dimension(2, 4) :: quad_edge
      integer, dimension(2, 12) :: hexa_edge
      integer, dimension(2, 6) :: tetra_edge
      integer, dimension(:), allocatable :: nn_nb_connect, ne_nb_connect
C-----------------------------------------------
C     B e g i n n i n g   o f   S u b r o u t i n e
C-----------------------------------------------
!     In case we go through this routine a second time
!     node-node connectivity
      if (allocated(this%nn_connect%iad_connect)) deallocate(this%nn_connect%iad_connect)
      if (allocated(this%nn_connect%connected)) deallocate(this%nn_connect%connected)
!     node-element connectivity
      if (allocated(this%ne_connect%iad_connect)) deallocate(this%ne_connect%iad_connect)
      if (allocated(this%ne_connect%connected)) deallocate(this%ne_connect%connected)
      if (allocated(this%ne_connect%type)) deallocate(this%ne_connect%type)
      
      allocate(nn_nb_connect(numnod))
      nn_nb_connect(1:numnod) = 0
      allocate(ne_nb_connect(numnod))
      ne_nb_connect(1:numnod) = 0
      max_edge = 12 * numels + 3 * numeltg + 4 * numelq
      allocate(edges(2, max_edge))
      nb_edge = 0
      node_id = 0
      
      tri_edge(1, 1) = 1
      tri_edge(2, 1) = 2
      tri_edge(1, 2) = 2
      tri_edge(2, 2) = 3
      tri_edge(1, 3) = 3
      tri_edge(2, 3) = 1

      quad_edge(1, 1) = 1
      quad_edge(2, 1) = 2
      quad_edge(1, 2) = 2
      quad_edge(2, 2) = 3
      quad_edge(1, 3) = 3
      quad_edge(2, 3) = 4
      quad_edge(1, 4) = 4
      quad_edge(2, 4) = 1

      hexa_edge(1, 1) = 1
      hexa_edge(2, 1) = 2
      hexa_edge(1, 2) = 2
      hexa_edge(2, 2) = 3
      hexa_edge(1, 3) = 3
      hexa_edge(2, 3) = 4
      hexa_edge(1, 4) = 4
      hexa_edge(2, 4) = 1
      hexa_edge(1, 5) = 5
      hexa_edge(2, 5) = 6
      hexa_edge(1, 6) = 6
      hexa_edge(2, 6) = 7
      hexa_edge(1, 7) = 7
      hexa_edge(2, 7) = 8
      hexa_edge(1, 8) = 8
      hexa_edge(2, 8) = 5
      hexa_edge(1, 9) = 1
      hexa_edge(2, 9) = 5
      hexa_edge(1, 10) = 2
      hexa_edge(2, 10) = 6
      hexa_edge(1, 11) = 3
      hexa_edge(2, 11) = 7
      hexa_edge(1, 12) = 4
      hexa_edge(2, 12) = 8

      tetra_edge(1, 1) = 1
      tetra_edge(2, 1) = 3
      tetra_edge(1, 2) = 3
      tetra_edge(2, 2) = 6
      tetra_edge(1, 3) = 6
      tetra_edge(2, 3) = 1
      tetra_edge(1, 4) = 1
      tetra_edge(2, 4) = 5
      tetra_edge(1, 5) = 3
      tetra_edge(2, 5) = 5
      tetra_edge(1, 6) = 6
      tetra_edge(2, 6) = 5
       
!     2D elements
!     /TRIA
      do ii = 1, numeltg
         do jj = 1, 3
            node_id = ixtg(1 + jj, ii)
            ne_nb_connect(node_id) = ne_nb_connect(node_id) + 1
         enddo
!     edges
         do iedge = 1, 3
            node1 = ixtg(1 + tri_edge(1, iedge), ii)
            node2 = ixtg(1 + tri_edge(2, iedge), ii)
            nb_edge = nb_edge + 1
            edges(1, nb_edge) = node1
            edges(2, nb_edge) = node2
         enddo
      enddo
!     /QUAD
      do ii = 1, numelq
         do jj = 1, 4
            node_id = ixq(1 + jj, ii)
            ne_nb_connect(node_id) = ne_nb_connect(node_id) + 1
         enddo
!     edges
         do iedge = 1, 4
            node1 = ixq(1 + quad_edge(1, iedge), ii)
            node2 = ixq(1 + quad_edge(2, iedge), ii)
            nb_edge = nb_edge + 1
            edges(1, nb_edge) = node1
            edges(2, nb_edge) = node2
         enddo
      enddo
!     3D elements
      do ii = 1, numels
         if (ixs(2, ii) == ixs(3, ii) .and. ixs(4, ii) == ixs(5, ii) .and. 
     .        ixs(6, ii) == ixs(9, ii) .and. ixs(7, ii) == ixs(8, ii)) then
!     tetra 
            node_id = ixs(2, ii)
            ne_nb_connect(node_id) = ne_nb_connect(node_id) + 1
            node_id = ixs(4, ii)
            ne_nb_connect(node_id) = ne_nb_connect(node_id) + 1
            node_id = ixs(7, ii)
            ne_nb_connect(node_id) = ne_nb_connect(node_id) + 1
            node_id = ixs(6, ii)
            ne_nb_connect(node_id) = ne_nb_connect(node_id) + 1
!     edges
            do iedge = 1, 6
               node1 = ixs(1 + tetra_edge(1, iedge), ii)
               node2 = ixs(1 + tetra_edge(2, iedge), ii)
               nb_edge = nb_edge + 1
               edges(1, nb_edge) = node1
               edges(2, nb_edge) = node2
            enddo
          else
!     bricks
            do jj = 1, 8
               node_id = ixs(1 + jj, ii)
               ne_nb_connect(node_id) = ne_nb_connect(node_id) + 1
            enddo
!     edges
            do iedge = 1, 12
               node1 = ixs(1 + hexa_edge(1, iedge), ii)
               node2 = ixs(1 + hexa_edge(2, iedge), ii)
               nb_edge = nb_edge + 1
               edges(1, nb_edge) = node1
               edges(2, nb_edge) = node2
            enddo
         endif
      enddo

      do ii = 1, nb_edge
         if (edges(1, ii) > edges(2, ii)) then
            itmp = edges(1, ii)
            edges(1, ii) = edges(2, ii)
            edges(2, ii) = itmp
         endif
      enddo
      

!     Indirection tab
      allocate(this%ne_connect%iad_connect(numnod + 1))
      this%ne_connect%iad_connect(1) = 1
      do ii = 2, numnod + 1
         this%ne_connect%iad_connect(ii) = this%ne_connect%iad_connect(ii - 1) + 
     .        ne_nb_connect(ii - 1) 
      enddo

      allocate(adsky(numnod))
      do ii = 1, numnod
         adsky(ii) = this%ne_connect%iad_connect(ii)
      enddo
!     Connectivities
      allocate(this%ne_connect%connected(this%ne_connect%iad_connect(numnod + 1)))
      this%ne_connect%connected(:) = 0
      allocate(this%ne_connect%type(this%ne_connect%iad_connect(numnod + 1)))
      this%ne_connect%type(:) = 0
!     2D elements
!     /TRIA
      do ii = 1, numeltg
         do jj = 1, 3
            node_id = ixtg(1 + jj, ii)
            this%ne_connect%connected(adsky(node_id)) = ii
            this%ne_connect%type(adsky(node_id)) = 3
            adsky(node_id) = adsky(node_id) + 1
         enddo
      enddo
!     /QUAD
      do ii = 1, numelq
         do jj = 1, 4
            node_id = ixq(1 + jj, ii)
            this%ne_connect%connected(adsky(node_id)) = ii
            this%ne_connect%type(adsky(node_id)) = 2
            adsky(node_id) = adsky(node_id) + 1
         enddo
      enddo
!     3D elements
      do ii = 1, numels
         if (ixs(2, ii) == ixs(3, ii) .and. ixs(4, ii) == ixs(5, ii) .and. 
     .        ixs(6, ii) == ixs(9, ii) .and. ixs(7, ii) == ixs(8, ii)) then
!     tetra 
            node_id = ixs(2, ii)
            this%ne_connect%connected(adsky(node_id)) = ii
            this%ne_connect%type(adsky(node_id)) = 1
            adsky(node_id) = adsky(node_id) + 1
            node_id = ixs(4, ii)
            this%ne_connect%connected(adsky(node_id)) = ii
            this%ne_connect%type(adsky(node_id)) = 1
            adsky(node_id) = adsky(node_id) + 1
            node_id = ixs(7, ii)
            this%ne_connect%connected(adsky(node_id)) = ii
            this%ne_connect%type(adsky(node_id)) = 1
            adsky(node_id) = adsky(node_id) + 1
            node_id = ixs(6, ii)
            this%ne_connect%connected(adsky(node_id)) = ii
            this%ne_connect%type(adsky(node_id)) = 1
            adsky(node_id) = adsky(node_id) + 1
         else
!     bricks
            do jj = 1, 8
               node_id = ixs(1 + jj, ii)
               this%ne_connect%connected(adsky(node_id)) = ii
               this%ne_connect%type(adsky(node_id)) = 1
               adsky(node_id) = adsky(node_id) + 1
            enddo
         endif
      enddo

      allocate(idx(nb_edge), edges_tmp(2, nb_edge))
      do ii = 1, nb_edge
         idx(ii) = ii
         edges_tmp(1, ii) = edges(1, ii)
         edges_tmp(2, ii) = edges(2, ii)
      enddo
      call quicksort_i2(edges_tmp(1, :), idx, 1, nb_edge)
      do ii = 1, nb_edge
         edges_tmp(1, ii) = edges(1, idx(ii))
         edges_tmp(2, ii) = edges(2, idx(ii))
      enddo
      
      nb_edge_new = 0
      ii = 1
      do while (ii < nb_edge)
         cur_pos = ii
         iad1 = 1
         do while (edges_tmp(1, ii + iad1) == edges_tmp(1, ii))
            if (ii + iad1 == nb_edge) then
               exit
            endif
            iad1 = iad1 + 1
         enddo
         if (iad1 == 1) then
            nb_edge_new = nb_edge_new + 1
            edges(1, nb_edge_new) = edges_tmp(1, ii)
            edges(2, nb_edge_new) = edges_tmp(2, ii)
            ii = ii + 1
         else
            call quicksort_i(edges_tmp(2, ii : ii + iad1 - 1), 1, iad1)
            node1 = edges_tmp(1, ii)
            node2 = edges_tmp(2, ii)
            nb_edge_new = nb_edge_new + 1
            edges(1, nb_edge_new) = node1
            edges(2, nb_edge_new) = node2
            do iad2 = 0, iad1 - 1
               if (edges_tmp(2, ii + iad2) /= node2) then
                  nb_edge_new = nb_edge_new + 1
                  node2 = edges_tmp(2, ii + iad2)
                  edges(1, nb_edge_new) = node1
                  edges(2, nb_edge_new) = node2
               endif
            enddo
            ii = ii + iad1
         endif
      enddo
!     node node connectivity
      do ii = 1, nb_edge_new
         nn_nb_connect(edges(1, ii)) = nn_nb_connect(edges(1, ii)) + 1
         nn_nb_connect(edges(2, ii)) = nn_nb_connect(edges(2, ii)) + 1
      enddo
      ne_nb_connect(node_id) = ne_nb_connect(node_id) + 1
!     indirection tab
      allocate(this%nn_connect%iad_connect(numnod + 1))
      this%nn_connect%iad_connect(1) = 1
      do ii = 2, numnod + 1
         this%nn_connect%iad_connect(ii) = this%nn_connect%iad_connect(ii - 1) + 
     .        nn_nb_connect(ii - 1) 
      enddo

      do ii = 1, numnod
         adsky(ii) = this%nn_connect%iad_connect(ii)
      enddo
      allocate(this%nn_connect%connected(this%nn_connect%iad_connect(numnod + 1)))
      this%nn_connect%connected(:) = 0
      do ii = 1, nb_edge_new
         node1 = edges(1, ii)
         node2 = edges(2, ii)
         this%nn_connect%connected(adsky(node1)) = node2
         this%nn_connect%connected(adsky(node2)) = node1
         adsky(node1) = adsky(node1) + 1
         adsky(node2) = adsky(node2) + 1
      enddo

      deallocate(adsky, edges, idx, edges_tmp, nn_nb_connect, ne_nb_connect)
      end subroutine ale_compute_connectivity

      subroutine ale_deallocate_connectivity(this)
      implicit none
      class(t_ale_connectivity), intent(inout) :: this
      
      if (allocated(this%ne_connect%connected)) deallocate(this%ne_connect%connected)
      if (allocated(this%ne_connect%iad_connect)) deallocate(this%ne_connect%iad_connect)

      if (allocated(this%nn_connect%connected)) deallocate(this%nn_connect%connected)
      if (allocated(this%nn_connect%iad_connect)) deallocate(this%nn_connect%iad_connect)

      if (allocated(this%nale)) deallocate(this%nale)
      end subroutine ale_deallocate_connectivity

      subroutine ale_compute_ee_connectivity(this, pm, 
     .     npropm, nummat, numnod, numelq, numeltg, numels, n2d, 
     .     iale, ieuler, itherm, ialelag,
     .     nixq, nixtg, nixs, itab,
     .     ixq, ixtg, ixs)
      implicit none
C-----------------------------------------------
C     D e s c r i p t i o n
C-----------------------------------------------
!     Computes Element to Element connectivities  !
!     for ALE, and EULER computations             !
!     ---------------------------------------     !   
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      class(t_ale_connectivity), intent(inout) :: this
      integer, intent(in) :: numnod, numelq, numeltg, numels, 
     .     nixq, nixtg, nixs, n2d, iale, ieuler, itherm, ialelag, npropm, nummat
      my_real, dimension(npropm, nummat), intent(in) :: pm
      integer, dimension(numnod), intent(in) :: itab
      integer, dimension(nixq, numelq), intent(in) :: ixq
      integer, dimension(nixtg, numeltg), intent(in) :: ixtg
      integer, dimension(nixs, numels), intent(in) :: ixs
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      integer :: ii, jj, node_id, node1, node2, inode
      integer, dimension(:), allocatable :: adsky
      integer :: iad1, iad2, itmp, iad
      integer :: max_edge, nb_edge, nb_edge_new, iedge, cur_pos
      integer, dimension(:, :), allocatable :: edges, edges_tmp
      integer, dimension(:), allocatable :: idx
      integer, dimension(2, 3) :: tri_edge
      integer, dimension(2, 4) :: quad_edge
      integer, dimension(2, 12) :: hexa_edge
      integer, dimension(2, 6) :: tetra_edge
      integer, dimension(:), allocatable :: iad_connect, ne_nb_connect, connected, type, ee_nb_connect, 
     .     itag
      integer(8) :: vec_ptr1
      integer :: jal, jalt, mlw, imid, tmp, count
      integer, dimension(4), target :: tetra_nodes
      integer, dimension(6, 4), target :: hexa_face
      integer, dimension(6, 3), target :: tetra_face
      integer, dimension(4, 2), target :: quad_face
      integer, dimension(3, 2), target :: tri_face
      integer, dimension(:, :), pointer :: elem_face, elem_face2
      integer :: kface, kface2, nface, nface_node, nface2, nface_node2
C-----------------------------------------------
C     B e g i n n i n g   o f   S u b r o u t i n e
C-----------------------------------------------  
      if (iale + ieuler + ialelag +itherm == 0) then
         return
      endif
!     List of nodes of interest for tetra
      tetra_nodes(1) = 2
      tetra_nodes(2) = 4
      tetra_nodes(3) = 7
      tetra_nodes(4) = 6
!     Faces for each element
!     hexa
      hexa_face(1, 1) = 1
      hexa_face(1, 2) = 2
      hexa_face(1, 3) = 3
      hexa_face(1, 4) = 4
      hexa_face(2, 1) = 3
      hexa_face(2, 2) = 4
      hexa_face(2, 3) = 8
      hexa_face(2, 4) = 7
      hexa_face(3, 1) = 5
      hexa_face(3, 2) = 6
      hexa_face(3, 3) = 7
      hexa_face(3, 4) = 8
      hexa_face(4, 1) = 1
      hexa_face(4, 2) = 2
      hexa_face(4, 3) = 6
      hexa_face(4, 4) = 5
      hexa_face(5, 1) = 2
      hexa_face(5, 2) = 3
      hexa_face(5, 3) = 7
      hexa_face(5, 4) = 6
      hexa_face(6, 1) = 1
      hexa_face(6, 2) = 4
      hexa_face(6, 3) = 8
      hexa_face(6, 4) = 5
!     tetra
      tetra_face(1, 1) = -1
      tetra_face(1, 2) = -1
      tetra_face(1, 3) = -1
      tetra_face(2, 1) = 5
      tetra_face(2, 2) = 6
      tetra_face(2, 3) = 3
      tetra_face(3, 1) = -1
      tetra_face(3, 2) = -1
      tetra_face(3, 3) = -1
      tetra_face(4, 1) = 5
      tetra_face(4, 2) = 1
      tetra_face(4, 3) = 6
      tetra_face(5, 1) = 1
      tetra_face(5, 2) = 3
      tetra_face(5, 3) = 6
      tetra_face(6, 1) = 5
      tetra_face(6, 2) = 3
      tetra_face(6, 3) = 1
!     quad
      quad_face(1, 1) = 1
      quad_face(1, 2) = 2
      quad_face(2, 1) = 2
      quad_face(2, 2) = 3
      quad_face(3, 1) = 3
      quad_face(3, 2) = 4
      quad_face(4, 1) = 4
      quad_face(4, 2) = 1
!     tria
      tri_face(1, 1) = 1
      tri_face(1, 2) = 2
      tri_face(2, 1) = 2
      tri_face(2, 2) = 3
      tri_face(3, 1) = 3
      tri_face(3, 2) = 1
      if (.not. this%nale_already_computed) then
!     Marking ALE nodes
         if (allocated(this%nale)) deallocate(this%nale)
         allocate(this%nale(numnod))
         this%nale(1:numnod) = 0
      endif
!     element-element connectivity
      if (allocated(this%ee_connect%iad_connect)) deallocate(this%ee_connect%iad_connect)
      if (allocated(this%ee_connect%connected)) deallocate(this%ee_connect%connected)
      if (allocated(this%ee_connect%type)) deallocate(this%ee_connect%type)
      if (allocated(this%ee_connect%iface2)) deallocate(this%ee_connect%iface2)

!     Node element connectivity
      allocate(ne_nb_connect(numnod))
      ne_nb_connect(1:numnod) = 0

!     2D elements
!     /TRIA
      do ii = 1, numeltg
!     ale : jal = 1, euler : jal = 2
         jal = nint(pm(72, iabs(ixtg(1, ii)))) 
         jalt = jal + nint(pm(71, iabs(ixtg(1, ii))) + pm(96, iabs(ixtg(1, ii))))
         imid = iabs(ixtg(1, ii))
         if (jalt == 0) cycle
         mlw  = nint(pm(19,imid))
         do jj = 1, 3
            node_id = ixtg(1 + jj, ii)
            ne_nb_connect(node_id) = ne_nb_connect(node_id) + 1
            if (.not. this%nale_already_computed) then
               this%nale(node_id) = max(this%nale(node_id), jal)
               if (mlw == 151) then
                  if (this%nale(node_id) == 1 .or. this%nale(node_id) == 2) this%nale(node_id) = 150 + this%nale(node_id)
               endif
            endif
         enddo
      enddo
!     /QUAD
      do ii = 1, numelq
!     ale : jal = 1, euler : jal = 2
         jal = nint(pm(72, iabs(ixq(1, ii)))) 
         jalt = jal + nint(pm(71, iabs(ixq(1, ii))) + pm(96, iabs(ixq(1, ii))))
         imid = iabs(ixq(1, ii))
         if (jalt == 0) cycle
         mlw  = nint(pm(19,imid))
         do jj = 1, 4
            node_id = ixq(1 + jj, ii)
            ne_nb_connect(node_id) = ne_nb_connect(node_id) + 1
            if (.not. this%nale_already_computed) then
               this%nale(node_id) = max(this%nale(node_id), jal)
               if (mlw == 151) then
                  if (this%nale(node_id) == 1 .or. this%nale(node_id) == 2) this%nale(node_id) = 150 + this%nale(node_id)
               endif
            endif
         enddo
      enddo
!     3D elements
      do ii = 1, numels
         jal = nint(pm(72, iabs(ixs(1, ii)))) 
         jalt = jal + nint(pm(71, iabs(ixs(1, ii))) + pm(96, iabs(ixs(1, ii))))
         imid = iabs(ixs(1, ii))
         if (jalt == 0) cycle
         mlw  = nint(pm(19,imid))
         if (ixs(2, ii) == ixs(3, ii) .and. ixs(4, ii) == ixs(5, ii) .and. 
     .        ixs(6, ii) == ixs(9, ii) .and. ixs(7, ii) == ixs(8, ii)) then
!     tetra 
            do jj = 1, 4
               node_id = ixs(tetra_nodes(jj), ii)
               ne_nb_connect(node_id) = ne_nb_connect(node_id) + 1
               if (.not. this%nale_already_computed) then
                  this%nale(node_id) = max(this%nale(node_id), jal)
                  if (mlw == 151) then
                     if (this%nale(node_id) == 1 .or. this%nale(node_id) == 2) this%nale(node_id) = 150 + this%nale(node_id)
                  endif
               endif
            enddo
          else
!     bricks
            do jj = 1, 8
               node_id = ixs(1 + jj, ii)
               ne_nb_connect(node_id) = ne_nb_connect(node_id) + 1
               if (.not. this%nale_already_computed) then
                  this%nale(node_id) = max(this%nale(node_id), jal)
                  if (mlw == 151) then
                     if (this%nale(node_id) == 1 .or. this%nale(node_id) == 2) this%nale(node_id) = 150 + this%nale(node_id)
                  endif
               endif
            enddo
         endif
      enddo    

      this%nale_already_computed = .true.

!     Indirection tab
      allocate(iad_connect(numnod + 1))
      iad_connect(1) = 1
      do ii = 2, numnod + 1
         iad_connect(ii) = iad_connect(ii - 1) + ne_nb_connect(ii - 1) 
      enddo
      allocate(adsky(numnod))
      do ii = 1, numnod
         adsky(ii) = iad_connect(ii)
      enddo

      allocate(connected(iad_connect(numnod + 1)))
      connected(:) = 0
      allocate(type(iad_connect(numnod + 1)))
      type(:) = 0
!     2D elements
!     /TRIA
      do ii = 1, numeltg
         jal = nint(pm(72, iabs(ixtg(1, ii)))) 
         jalt = jal + nint(pm(71, iabs(ixtg(1, ii))) + pm(96, iabs(ixtg(1, ii))))
         imid = iabs(ixtg(1, ii))
         if (jalt == 0) cycle
         do jj = 1, 3
            node_id = ixtg(1 + jj, ii)
            connected(adsky(node_id)) = ii
            type(adsky(node_id)) = 3
            adsky(node_id) = adsky(node_id) + 1
         enddo
      enddo
!     /QUAD
      do ii = 1, numelq
         jal = nint(pm(72, iabs(ixq(1, ii)))) 
         jalt = jal + nint(pm(71, iabs(ixq(1, ii))) + pm(96, iabs(ixq(1, ii))))
         imid = iabs(ixq(1, ii))
         if (jalt == 0) cycle
         do jj = 1, 4
            node_id = ixq(1 + jj, ii)
            connected(adsky(node_id)) = ii
            type(adsky(node_id)) = 2
            adsky(node_id) = adsky(node_id) + 1
         enddo
      enddo
!     3D elements
      do ii = 1, numels
         jal = nint(pm(72, iabs(ixs(1, ii)))) 
         jalt = jal + nint(pm(71, iabs(ixs(1, ii))) + pm(96, iabs(ixs(1, ii))))
         imid = iabs(ixs(1, ii))
         if (jalt == 0) cycle
         if (ixs(2, ii) == ixs(3, ii) .and. ixs(4, ii) == ixs(5, ii) .and. 
     .        ixs(6, ii) == ixs(9, ii) .and. ixs(7, ii) == ixs(8, ii)) then
!     tetra 
            node_id = ixs(2, ii)
            connected(adsky(node_id)) = ii
            type(adsky(node_id)) = 1
            adsky(node_id) = adsky(node_id) + 1
            node_id = ixs(4, ii)
            connected(adsky(node_id)) = ii
            type(adsky(node_id)) = 1
            adsky(node_id) = adsky(node_id) + 1
            node_id = ixs(7, ii)
            connected(adsky(node_id)) = ii
            type(adsky(node_id)) = 1
            adsky(node_id) = adsky(node_id) + 1
            node_id = ixs(6, ii)
            connected(adsky(node_id)) = ii
            type(adsky(node_id)) = 1
            adsky(node_id) = adsky(node_id) + 1
         else
!     bricks
            do jj = 1, 8
               node_id = ixs(1 + jj, ii)
               connected(adsky(node_id)) = ii
               type(adsky(node_id)) = 1
               adsky(node_id) = adsky(node_id) + 1
            enddo
         endif
      enddo

      
!     Counting connected elements
      if (n2d == 0) then
         allocate(this%ee_connect%iad_connect(numels+1))
         allocate(ee_nb_connect(numels))
      else
         allocate(this%ee_connect%iad_connect(numeltg + numelq + 1))
         allocate(ee_nb_connect(numeltg + numelq))
      endif
      ee_nb_connect(:) = 0

      tmp = 0
      if (n2d == 0) then
!     3D elements
         do ii = 1, numels
            jal = nint(pm(72, iabs(ixs(1, ii)))) 
            jalt = jal + nint(pm(71, iabs(ixs(1, ii))) + pm(96, iabs(ixs(1, ii))))
            if (jalt == 0) cycle
            if (ixs(2, ii) == ixs(3, ii) .and. ixs(4, ii) == ixs(5, ii) .and. 
     .           ixs(6, ii) == ixs(9, ii) .and. ixs(7, ii) == ixs(8, ii)) then
!     Tetras
               ee_nb_connect(ii) = 6
            else
!     Hexa
               ee_nb_connect(ii) = 6
            endif
         enddo
         this%ee_connect%iad_connect(1) = 1
         do ii = 2, numels + 1
            this%ee_connect%iad_connect(ii) = this%ee_connect%iad_connect(ii - 1) + ee_nb_connect(ii - 1)
         enddo
         tmp = this%ee_connect%iad_connect(numels + 1)
      else
!     2D elements
!     /QUAD
         do ii = 1, numelq
            jal = nint(pm(72, iabs(ixq(1, ii)))) 
            jalt = jal + nint(pm(71, iabs(ixq(1, ii))) + pm(96, iabs(ixq(1, ii))))
            if (jalt == 0) cycle
            ee_nb_connect(ii) = 4
         enddo   
!     /TRIA
         do ii = 1, numeltg
            jal = nint(pm(72, iabs(ixtg(1, ii)))) 
            jalt = jal + nint(pm(71, iabs(ixtg(1, ii))) + pm(96, iabs(ixtg(1, ii))))
            if (jalt == 0) cycle
            ee_nb_connect(ii) = 3
         enddo
         this%ee_connect%iad_connect(1) = 1
         do ii = 2, numelq + numeltg + 1
            this%ee_connect%iad_connect(ii) = this%ee_connect%iad_connect(ii - 1) + ee_nb_connect(ii - 1)
         enddo
         tmp = this%ee_connect%iad_connect(numelq + numeltg + 1) - 1
      endif

      allocate(this%ee_connect%connected(tmp))
      allocate(this%ee_connect%type(tmp))
      allocate(this%ee_connect%iface2(tmp))
      this%ee_connect%type(1:tmp) = 0
      this%ee_connect%connected(1:tmp) = 0
      this%ee_connect%iface2(1:tmp) = 0
      call intvector_create(vec_ptr1)
      allocate(itag(numnod))
      itag(1:numnod) = 0
      if (n2d == 0) then
         do ii = 1, numels
            jal = nint(pm(72, iabs(ixs(1, ii)))) 
            jalt = jal + nint(pm(71, iabs(ixs(1, ii))) + pm(96, iabs(ixs(1, ii))))
            if (jalt == 0) cycle
            iad1 = this%ee_connect%iad_connect(ii)
            if (ixs(2, ii) == ixs(3, ii) .and. ixs(4, ii) == ixs(5, ii) .and. 
     .           ixs(6, ii) == ixs(9, ii) .and. ixs(7, ii) == ixs(8, ii)) then
!     Tetras
               nface = 6
               nface_node = 3
               count = 3
               elem_face => tetra_face
            else
!     Hexa
               nface = 6
               nface_node = 4
               count = 4
               elem_face => hexa_face
            endif
            do kface = 1, nface
               call intvector_clear(vec_ptr1)
               do inode = 1, nface_node
                  if (elem_face(kface, inode) < 0) cycle
                  node_id = ixs(1 + elem_face(kface, inode), ii)
                  itag(node_id) = 1
                  do iad = iad_connect(node_id), iad_connect(node_id + 1) - 1
                     if (connected(iad) /= ii) then
                        call intvector_push_back(vec_ptr1, connected(iad))
                     endif
                  enddo
               enddo
!     get the redundant element number
               call intvector_get_redundant(vec_ptr1, jj, itmp, count)
               iad1 = this%ee_connect%iad_connect(ii)
               this%ee_connect%connected(iad1 + kface - 1) = jj
               if (jj > 0) then
                  if (ixs(2, jj) == ixs(3, jj) .and. ixs(4, jj) == ixs(5, jj) .and. 
     .                 ixs(6, jj) == ixs(9, jj) .and. ixs(7, jj) == ixs(8, jj)) then
!     Tetras
                     nface2 = 6
                     nface_node2 = 3
                     elem_face2 => tetra_face
                     this%ee_connect%type(iad1 + kface - 1) = 1
                  else
!     Hexa
                     nface2 = 6
                     nface_node2 = 4
                     elem_face2 => hexa_face
                     this%ee_connect%type(iad1 + kface - 1) = 0
                  endif
                  do kface2 = 1, nface2
                     itmp = 1
                     do inode = 1, nface_node2
                        if (elem_face(kface2, inode) < 0) cycle
                        itmp = itmp * itag(ixs(1 + elem_face(kface2, inode), jj))
                     enddo
                     if (itmp == 1) then
                        this%ee_connect%iface2(iad1 + kface - 1) = kface2
                        exit
                     endif
                  enddo
               endif
               do inode = 1, nface_node
                  if (elem_face(kface, inode) < 0) cycle
                  node_id = ixs(1 + elem_face(kface, inode), ii)
                  itag(node_id) = 0
               enddo
            enddo
         enddo
      else
!     Quad
         do ii = 1, numelq
            jal = nint(pm(72, iabs(ixq(1, ii)))) 
            jalt = jal + nint(pm(71, iabs(ixq(1, ii))) + pm(96, iabs(ixq(1, ii))))
            if (jalt == 0) cycle
            iad1 = this%ee_connect%iad_connect(ii)
            nface = 4
            nface_node = 2
            elem_face => quad_face
            count = 2
            do kface = 1, nface
               call intvector_clear(vec_ptr1)
               do inode = 1, nface_node
                  node_id = ixq(1 + elem_face(kface, inode), ii)
                  itag(node_id) = 1
                  do iad = iad_connect(node_id), iad_connect(node_id + 1) - 1
                     if (connected(iad) /= ii) then
                        call intvector_push_back(vec_ptr1, connected(iad))
                     endif
                  enddo
               enddo
!     get the redundant element number
               call intvector_get_redundant(vec_ptr1, jj, itmp, count)
               this%ee_connect%connected(iad1 + kface - 1) = jj
               if (jj > 0) then
                  if (jj > numelq) then
                     nface2 = 3
                     nface_node2 = 2
                     elem_face2 => tri_face
                     this%ee_connect%type(iad1 + kface - 1) = 3
                  else
                     nface2 = 4
                     nface_node2 = 2
                     elem_face2 => quad_face
                     this%ee_connect%type(iad1 + kface - 1) = 2
                  endif
                  do kface2 = 1, nface2
                     itmp = 1
                     do inode = 1, nface_node2
                        itmp = itmp * itag(ixq(1 + elem_face(kface2, inode), jj))
                     enddo
                     if (itmp == 1) then
                        this%ee_connect%iface2(iad1 + kface - 1) = kface2
                        exit
                     endif
                  enddo
               endif
               do inode = 1, nface_node
                  node_id = ixq(1 + elem_face(kface, inode), ii)
                  itag(node_id) = 0
               enddo
            enddo
         enddo
!     Tria
         do ii = 1, numeltg
            jal = nint(pm(72, iabs(ixtg(1, ii)))) 
            jalt = jal + nint(pm(71, iabs(ixtg(1, ii))) + pm(96, iabs(ixtg(1, ii))))
            if (jalt == 0) cycle
            iad1 = this%ee_connect%iad_connect(ii)
            nface = 3
            nface_node = 2
            elem_face => tri_face
            count = 2
            do kface = 1, nface
               call intvector_clear(vec_ptr1)
               do inode = 1, nface_node
                  node_id = ixtg(1 + elem_face(kface, inode), ii)
                  itag(node_id) = 1
                  do iad = iad_connect(node_id), iad_connect(node_id + 1) - 1
                     if (connected(iad) /= ii) then
                        call intvector_push_back(vec_ptr1, connected(iad))
                     endif
                  enddo
               enddo
!     get the redundant element number
               call intvector_get_redundant(vec_ptr1, jj, itmp, count)
               this%ee_connect%connected(iad1 + kface - 1) = jj
               if (jj > 0) then
                  if (jj > numelq) then
                     nface2 = 2
                     nface_node2 = 2
                     elem_face2 => tri_face
                     this%ee_connect%type(iad1 + kface - 1) = 3
                  else
                     nface2 = 4
                     nface_node2 = 3
                     elem_face2 => quad_face
                     this%ee_connect%type(iad1 + kface - 1) = 2
                  endif
                  do kface2 = 1, nface2
                     itmp = 1
                     do inode = 1, nface_node2
                        itmp = itmp * itag(ixtg(1 + elem_face(kface2, inode), jj))
                     enddo
                     if (itmp == 1) then
                        this%ee_connect%iface2(iad1 + kface - 1) = kface2
                        exit
                     endif
                  enddo
               endif
               do inode = 1, nface_node
                  node_id = ixtg(1 + elem_face(kface, inode), ii)
                  itag(node_id) = 0
               enddo
            enddo
         enddo
      endif

      call intvector_delete(vec_ptr1)
      if (allocated(ee_nb_connect)) deallocate(ee_nb_connect)
      if (allocated(itag)) deallocate(itag)
      if (allocated(ne_nb_connect)) deallocate(ne_nb_connect)
      if (allocated(iad_connect)) deallocate(iad_connect)
      if (allocated(adsky)) deallocate(adsky)
      if (allocated(connected)) deallocate(connected)
      
      end subroutine ale_compute_ee_connectivity
      END MODULE ALE_CONNECTIVITY_MOD
