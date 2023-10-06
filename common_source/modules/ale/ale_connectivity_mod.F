Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  ALE_CONNECTIVITY_MOD          modules/ale/ale_connectivity_mod.F
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
Chd|        AGAUG3Q                       engine/source/ale/agaug3q.F   
Chd|        AGAUG3T                       engine/source/ale/agaug3t.F   
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
Chd|        INIT_DIFFUSION                engine/share/modules/diffusion_mod.F
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
Chd|        OUTPUT_SCHLIEREN              engine/source/output/anim/generate/output_schlieren.F
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
      
        IMPLICIT NONE
#include      "my_real.inc"

!     ************    !
!     Connectivity
!     ************    !
      TYPE, PUBLIC :: T_CONNECTIVITY
        INTEGER, DIMENSION(:), ALLOCATABLE :: IAD_CONNECT
        INTEGER, DIMENSION(:), ALLOCATABLE :: CONNECTED
        INTEGER, DIMENSION(:), ALLOCATABLE :: TYPE
      END TYPE T_CONNECTIVITY

!     Extended type : + iface
      TYPE, PUBLIC, EXTENDS(T_CONNECTIVITY) :: T_CONNECTIVITY_EXT1
        INTEGER, DIMENSION(:), ALLOCATABLE :: IFACE2
      END TYPET_CONNECTIVITY_EXT1


!     ******************    !
!     Extended local ids    !
!     ******************    !
      TYPE :: T_IDGLOB
        INTEGER, DIMENSION(:), ALLOCATABLE :: ID
        INTEGER, DIMENSION(:), ALLOCATABLE :: UID
      END TYPE T_IDGLOB

!     ****************     !
!     ALE connectivity     !
!     ****************     !
      TYPE, PUBLIC :: T_ALE_CONNECTIVITY
!     node-node, node-element, element-element
        TYPE(T_CONNECTIVITY) :: NN_CONNECT, NE_CONNECT
        TYPE(T_CONNECTIVITY_EXT1) :: EE_CONNECT
        LOGICAL :: HAS_NN_CONNECT = .FALSE.
        LOGICAL :: HAS_NE_CONNECT = .FALSE.
        LOGICAL :: HAS_EE_CONNECT = .FALSE.
        LOGICAL :: HAS_IDGLOB = .FALSE.
        LOGICAL :: NALE_ALREADY_COMPUTED = .FALSE.
        INTEGER, DIMENSION(:), ALLOCATABLE :: NALE
        TYPE(T_IDGLOB) :: IDGLOB
        CONTAINS
          PROCEDURE, PASS :: ALE_CONNECTIVITY_INIT
          PROCEDURE, PASS :: ALE_COMPUTE_CONNECTIVITY
          PROCEDURE, PASS :: ALE_COMPUTE_EE_CONNECTIVITY
          PROCEDURE, PASS :: ALE_DEALLOCATE_CONNECTIVITY
      END TYPE T_ALE_CONNECTIVITY

!     ***********    !
!     Subroutines    !
!     ***********    !
      CONTAINS
      
Chd|====================================================================
Chd|  ALE_CONNECTIVITY_INIT         modules/ale/ale_connectivity_mod.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ALE_MOD                       modules/ale/ale_mod.F         
Chd|====================================================================
      SUBROUTINE ALE_CONNECTIVITY_INIT(THIS)
        USE ALE_MOD      
        IMPLICIT NONE
        CLASS(T_ALE_CONNECTIVITY), INTENT(INOUT) :: THIS
#include "com01_c.inc"
#include "inter18.inc"
        LOGICAL RESULT  
        RESULT = .FALSE.
        IF(IALE > 0 .AND. ALE%GRID%NWALE == 6)RESULT=.TRUE. !/ALE/GRID/VOLUME DEFINED
        IF(INTER18_AUTOPARAM == 1)RESULT=.TRUE. !/INTER/TYPE18 WITH FLAG IAUTO SET TO 1
        IF(INTER18_IS_VARIABLE_GAP_DEFINED)RESULT=.TRUE. !/INTER/TYPE18 WITH FLAG IAUTO SET TO 1    
        THIS%HAS_NE_CONNECT = RESULT
        RESULT = .FALSE.
        IF(IALE > 0 .AND. ALE%GRID%NWALE < 2)RESULT=.TRUE. !/ALE/GRID/DONEA, DISP, SPRING
        IF(IALE > 0 .AND. ALE%GRID%NWALE == 7)RESULT=.TRUE. !/ALE/GRID/MASSFLOW
        IF(IALE > 0 .AND. ALE%GRID%NWALE == 5)RESULT=.TRUE. !/ALE/GRID/LAPLACIAN
        THIS%HAS_NN_CONNECT = RESULT
      END SUBROUTINE ALE_CONNECTIVITY_INIT
      
Chd|====================================================================
Chd|  ALE_COMPUTE_CONNECTIVITY      modules/ale/ale_connectivity_mod.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ALE_COMPUTE_CONNECTIVITY(THIS, NUMNOD, NUMELQ, NUMELTG, NUMELS, 
     .     NIXQ, NIXTG, NIXS, ITAB,
     .     IXQ, IXTG, IXS)
        IMPLICIT NONE
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
        CLASS(T_ALE_CONNECTIVITY), INTENT(INOUT) :: THIS
        INTEGER, INTENT(IN) :: NUMNOD, NUMELQ, NUMELTG, NUMELS, NIXQ, NIXTG, NIXS
        INTEGER, DIMENSION(NUMNOD), INTENT(IN) :: ITAB
        INTEGER, DIMENSION(NIXQ, NUMELQ), INTENT(IN) :: IXQ
        INTEGER, DIMENSION(NIXTG, NUMELTG), INTENT(IN) :: IXTG
        INTEGER, DIMENSION(NIXS, NUMELS), INTENT(IN) :: IXS
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: II, JJ, NODE_ID, NODE1, NODE2
        INTEGER, DIMENSION(:), ALLOCATABLE :: ADSKY
        INTEGER :: IAD1, IAD2, ITMP
        INTEGER :: MAX_EDGE, NB_EDGE, NB_EDGE_NEW, IEDGE, CUR_POS
        INTEGER, DIMENSION(:, :), ALLOCATABLE :: EDGES, EDGES_TMP
        INTEGER, DIMENSION(:), ALLOCATABLE :: IDX
        INTEGER, DIMENSION(2, 3) :: TRI_EDGE
        INTEGER, DIMENSION(2, 4) :: QUAD_EDGE
        INTEGER, DIMENSION(2, 12) :: HEXA_EDGE
        INTEGER, DIMENSION(2, 6) :: TETRA_EDGE
        INTEGER, DIMENSION(:), ALLOCATABLE :: NN_NB_CONNECT, NE_NB_CONNECT
C-----------------------------------------------
C     S o u r c e   L i n e s
C-----------------------------------------------
!       In case we go through this routine a second time
!       node-node connectivity
        IF (ALLOCATED(THIS%NN_CONNECT%IAD_CONNECT)) DEALLOCATE(THIS%NN_CONNECT%IAD_CONNECT)
        IF (ALLOCATED(THIS%NN_CONNECT%CONNECTED)) DEALLOCATE(THIS%NN_CONNECT%CONNECTED)
!     node-element connectivity
        IF (ALLOCATED(THIS%NE_CONNECT%IAD_CONNECT)) DEALLOCATE(THIS%NE_CONNECT%IAD_CONNECT)
        IF (ALLOCATED(THIS%NE_CONNECT%CONNECTED)) DEALLOCATE(THIS%NE_CONNECT%CONNECTED)
        IF (ALLOCATED(THIS%NE_CONNECT%TYPE)) DEALLOCATE(THIS%NE_CONNECT%TYPE)
      
        ALLOCATE(NN_NB_CONNECT(NUMNOD))
        NN_NB_CONNECT(1:NUMNOD) = 0
        ALLOCATE(NE_NB_CONNECT(NUMNOD))
        NE_NB_CONNECT(1:NUMNOD) = 0
        MAX_EDGE = 12 * NUMELS + 3 * NUMELTG + 4 * NUMELQ
        ALLOCATE(EDGES(2, MAX_EDGE))
        NB_EDGE = 0
        NODE_ID = 0
      
        TRI_EDGE(1, 1) = 1
        TRI_EDGE(2, 1) = 2
        TRI_EDGE(1, 2) = 2
        TRI_EDGE(2, 2) = 3
        TRI_EDGE(1, 3) = 3
        TRI_EDGE(2, 3) = 1

        QUAD_EDGE(1, 1) = 1
        QUAD_EDGE(2, 1) = 2
        QUAD_EDGE(1, 2) = 2
        QUAD_EDGE(2, 2) = 3
        QUAD_EDGE(1, 3) = 3
        QUAD_EDGE(2, 3) = 4
        QUAD_EDGE(1, 4) = 4
        QUAD_EDGE(2, 4) = 1

        HEXA_EDGE(1, 1) = 1
        HEXA_EDGE(2, 1) = 2
        HEXA_EDGE(1, 2) = 2
        HEXA_EDGE(2, 2) = 3
        HEXA_EDGE(1, 3) = 3
        HEXA_EDGE(2, 3) = 4
        HEXA_EDGE(1, 4) = 4
        HEXA_EDGE(2, 4) = 1
        HEXA_EDGE(1, 5) = 5
        HEXA_EDGE(2, 5) = 6
        HEXA_EDGE(1, 6) = 6
        HEXA_EDGE(2, 6) = 7
        HEXA_EDGE(1, 7) = 7
        HEXA_EDGE(2, 7) = 8
        HEXA_EDGE(1, 8) = 8
        HEXA_EDGE(2, 8) = 5
        HEXA_EDGE(1, 9) = 1
        HEXA_EDGE(2, 9) = 5
        HEXA_EDGE(1, 10) = 2
        HEXA_EDGE(2, 10) = 6
        HEXA_EDGE(1, 11) = 3
        HEXA_EDGE(2, 11) = 7
        HEXA_EDGE(1, 12) = 4
        HEXA_EDGE(2, 12) = 8

        TETRA_EDGE(1, 1) = 1
        TETRA_EDGE(2, 1) = 3
        TETRA_EDGE(1, 2) = 3
        TETRA_EDGE(2, 2) = 6
        TETRA_EDGE(1, 3) = 6
        TETRA_EDGE(2, 3) = 1
        TETRA_EDGE(1, 4) = 1
        TETRA_EDGE(2, 4) = 5
        TETRA_EDGE(1, 5) = 3
        TETRA_EDGE(2, 5) = 5
        TETRA_EDGE(1, 6) = 6
        TETRA_EDGE(2, 6) = 5
       
!       2D elements
!       /TRIA
        DO II = 1, NUMELTG
          DO JJ = 1, 3
            NODE_ID = IXTG(1 + JJ, II)
            NE_NB_CONNECT(NODE_ID) = NE_NB_CONNECT(NODE_ID) + 1
          ENDDO
          !edges
          DO IEDGE = 1, 3
            NODE1 = IXTG(1 + TRI_EDGE(1, IEDGE), II)
            NODE2 = IXTG(1 + TRI_EDGE(2, IEDGE), II)
            NB_EDGE = NB_EDGE + 1
            EDGES(1, NB_EDGE) = NODE1
            EDGES(2, NB_EDGE) = NODE2
          ENDDO
        ENDDO
!       /QUAD
        DO II = 1, NUMELQ
          DO JJ = 1, 4
            NODE_ID = IXQ(1 + JJ, II)
            NE_NB_CONNECT(NODE_ID) = NE_NB_CONNECT(NODE_ID) + 1
          ENDDO
          !edges
          DO IEDGE = 1, 4
            NODE1 = IXQ(1 + QUAD_EDGE(1, IEDGE), II)
            NODE2 = IXQ(1 + QUAD_EDGE(2, IEDGE), II)
            NB_EDGE = NB_EDGE + 1
            EDGES(1, NB_EDGE) = NODE1
            EDGES(2, NB_EDGE) = NODE2
          ENDDO
        ENDDO
!     3D elements
        DO II = 1, NUMELS
          IF (IXS(2, II) == IXS(3, II) .AND. IXS(4, II) == IXS(5, II) .AND. 
     .         IXS(6, II) == IXS(9, II) .AND. IXS(7, II) == IXS(8, II)) THEN
!            TETRA
             NODE_ID = IXS(2, II)
             NE_NB_CONNECT(NODE_ID) = NE_NB_CONNECT(NODE_ID) + 1
             NODE_ID = IXS(4, II)
             NE_NB_CONNECT(NODE_ID) = NE_NB_CONNECT(NODE_ID) + 1
             NODE_ID = IXS(7, II)
             NE_NB_CONNECT(NODE_ID) = NE_NB_CONNECT(NODE_ID) + 1
             NODE_ID = IXS(6, II)
             NE_NB_CONNECT(NODE_ID) = NE_NB_CONNECT(NODE_ID) + 1
             !edges
             DO IEDGE = 1, 6
                NODE1 = IXS(1 + TETRA_EDGE(1, IEDGE), II)
                NODE2 = IXS(1 + TETRA_EDGE(2, IEDGE), II)
                NB_EDGE = NB_EDGE + 1
                EDGES(1, NB_EDGE) = NODE1
                EDGES(2, NB_EDGE) = NODE2
             ENDDO
           ELSE
!             BRICKS
             DO JJ = 1, 8
                NODE_ID = IXS(1 + JJ, II)
                NE_NB_CONNECT(NODE_ID) = NE_NB_CONNECT(NODE_ID) + 1
             ENDDO
!             edges
             DO IEDGE = 1, 12
                NODE1 = IXS(1 + HEXA_EDGE(1, IEDGE), II)
                NODE2 = IXS(1 + HEXA_EDGE(2, IEDGE), II)
                IF(NODE1 /= NODE2)THEN
                  NB_EDGE = NB_EDGE + 1
                  EDGES(1, NB_EDGE) = NODE1
                  EDGES(2, NB_EDGE) = NODE2
                ENDIF
             ENDDO
          ENDIF
        ENDDO

        DO II = 1, NB_EDGE
          IF (EDGES(1, II) > EDGES(2, II)) THEN
            ITMP = EDGES(1, II)
            EDGES(1, II) = EDGES(2, II)
            EDGES(2, II) = ITMP
          ENDIF
        ENDDO
      

!       Indirection tab
        ALLOCATE(THIS%NE_CONNECT%IAD_CONNECT(NUMNOD + 1))
        THIS%NE_CONNECT%IAD_CONNECT(1) = 1
        DO II = 2, NUMNOD + 1
          THIS%NE_CONNECT%IAD_CONNECT(II) = THIS%NE_CONNECT%IAD_CONNECT(II - 1) + NE_NB_CONNECT(II - 1) 
        ENDDO

        ALLOCATE(ADSKY(NUMNOD))
        DO II = 1, NUMNOD
          ADSKY(II) = THIS%NE_CONNECT%IAD_CONNECT(II)
        ENDDO
!       Connectivities
        ALLOCATE(THIS%NE_CONNECT%CONNECTED(THIS%NE_CONNECT%IAD_CONNECT(NUMNOD + 1)))
        THIS%NE_CONNECT%CONNECTED(:) = 0
        ALLOCATE(THIS%NE_CONNECT%TYPE(THIS%NE_CONNECT%IAD_CONNECT(NUMNOD + 1)))
        THIS%NE_CONNECT%TYPE(:) = 0
        
!       2D elements
!       /TRIA
        DO II = 1, NUMELTG
          DO JJ = 1, 3
            NODE_ID = IXTG(1 + JJ, II)
            THIS%NE_CONNECT%CONNECTED(ADSKY(NODE_ID)) = II
            THIS%NE_CONNECT%TYPE(ADSKY(NODE_ID)) = 3
            ADSKY(NODE_ID) = ADSKY(NODE_ID) + 1
          ENDDO
        ENDDO
!       /QUAD
        DO II = 1, NUMELQ
          DO JJ = 1, 4
            NODE_ID = IXQ(1 + JJ, II)
            THIS%NE_CONNECT%CONNECTED(ADSKY(NODE_ID)) = II
            THIS%NE_CONNECT%TYPE(ADSKY(NODE_ID)) = 2
            ADSKY(NODE_ID) = ADSKY(NODE_ID) + 1
          ENDDO
        ENDDO
        
!       3D elements
        DO II = 1, NUMELS
         IF (IXS(2, II) == IXS(3, II) .AND. IXS(4, II) == IXS(5, II) .AND. 
     .        IXS(6, II) == IXS(9, II) .AND. IXS(7, II) == IXS(8, II)) THEN
!          TETRA                                          
           NODE_ID = IXS(2, II)                           
           THIS%NE_CONNECT%CONNECTED(ADSKY(NODE_ID)) = II 
           THIS%NE_CONNECT%TYPE(ADSKY(NODE_ID)) = 1       
           ADSKY(NODE_ID) = ADSKY(NODE_ID) + 1            
           NODE_ID = IXS(4, II)                           
           THIS%NE_CONNECT%CONNECTED(ADSKY(NODE_ID)) = II 
           THIS%NE_CONNECT%TYPE(ADSKY(NODE_ID)) = 1       
           ADSKY(NODE_ID) = ADSKY(NODE_ID) + 1            
           NODE_ID = IXS(7, II)                           
           THIS%NE_CONNECT%CONNECTED(ADSKY(NODE_ID)) = II 
           THIS%NE_CONNECT%TYPE(ADSKY(NODE_ID)) = 1       
           ADSKY(NODE_ID) = ADSKY(NODE_ID) + 1            
           NODE_ID = IXS(6, II)                           
           THIS%NE_CONNECT%CONNECTED(ADSKY(NODE_ID)) = II 
           THIS%NE_CONNECT%TYPE(ADSKY(NODE_ID)) = 1       
           ADSKY(NODE_ID) = ADSKY(NODE_ID) + 1            
         ELSE
!          BRICKS                                            
           DO JJ = 1, 8                                      
              NODE_ID = IXS(1 + JJ, II)                      
              THIS%NE_CONNECT%CONNECTED(ADSKY(NODE_ID)) = II 
              THIS%NE_CONNECT%TYPE(ADSKY(NODE_ID)) = 1       
              ADSKY(NODE_ID) = ADSKY(NODE_ID) + 1            
           ENDDO                                             
         ENDIF
        ENDDO

        ALLOCATE(IDX(NB_EDGE), EDGES_TMP(2, NB_EDGE))
        DO II = 1, NB_EDGE
          IDX(II) = II
          EDGES_TMP(1, II) = EDGES(1, II)
          EDGES_TMP(2, II) = EDGES(2, II)
        ENDDO
        CALL QUICKSORT_I2(EDGES_TMP(1, :), IDX, 1, NB_EDGE)
        DO II = 1, NB_EDGE
          EDGES_TMP(1, II) = EDGES(1, IDX(II))
          EDGES_TMP(2, II) = EDGES(2, IDX(II))
        ENDDO
      
        NB_EDGE_NEW = 0
        II = 1
        DO WHILE (II < NB_EDGE)
           CUR_POS = II
           IAD1 = 1
           DO WHILE (EDGES_TMP(1, II + IAD1) == EDGES_TMP(1, II))
              IF (II + IAD1 == NB_EDGE) THEN
                 EXIT
              ENDIF
              IAD1 = IAD1 + 1
           ENDDO
           IF (IAD1 == 1) THEN
              NB_EDGE_NEW = NB_EDGE_NEW + 1
              EDGES(1, NB_EDGE_NEW) = EDGES_TMP(1, II)
              EDGES(2, NB_EDGE_NEW) = EDGES_TMP(2, II)
              II = II + 1
           ELSE
              CALL QUICKSORT_I(EDGES_TMP(2, II : II + IAD1 - 1), 1, IAD1)
              NODE1 = EDGES_TMP(1, II)
              NODE2 = EDGES_TMP(2, II)
              NB_EDGE_NEW = NB_EDGE_NEW + 1
              EDGES(1, NB_EDGE_NEW) = NODE1
              EDGES(2, NB_EDGE_NEW) = NODE2
              DO IAD2 = 0, IAD1 - 1
                 IF (EDGES_TMP(2, II + IAD2) /= NODE2) THEN
                    NB_EDGE_NEW = NB_EDGE_NEW + 1
                    NODE2 = EDGES_TMP(2, II + IAD2)
                    EDGES(1, NB_EDGE_NEW) = NODE1
                    EDGES(2, NB_EDGE_NEW) = NODE2
                 ENDIF
              ENDDO
              II = II + IAD1
           ENDIF
        ENDDO
        
!       node node connectivity
        DO II = 1, NB_EDGE_NEW
          NN_NB_CONNECT(EDGES(1, II)) = NN_NB_CONNECT(EDGES(1, II)) + 1
          NN_NB_CONNECT(EDGES(2, II)) = NN_NB_CONNECT(EDGES(2, II)) + 1
        ENDDO
        NE_NB_CONNECT(NODE_ID) = NE_NB_CONNECT(NODE_ID) + 1
!       indirection tab
        ALLOCATE(THIS%NN_CONNECT%IAD_CONNECT(NUMNOD + 1))
        THIS%NN_CONNECT%IAD_CONNECT(1) = 1
        DO II = 2, NUMNOD + 1
          THIS%NN_CONNECT%IAD_CONNECT(II) = THIS%NN_CONNECT%IAD_CONNECT(II - 1) + NN_NB_CONNECT(II - 1) 
        ENDDO

        DO II = 1, NUMNOD
          ADSKY(II) = THIS%NN_CONNECT%IAD_CONNECT(II)
        ENDDO
        ALLOCATE(THIS%NN_CONNECT%CONNECTED(THIS%NN_CONNECT%IAD_CONNECT(NUMNOD + 1)))
        THIS%NN_CONNECT%CONNECTED(:) = 0
        DO II = 1, NB_EDGE_NEW
          NODE1 = EDGES(1, II)
          NODE2 = EDGES(2, II)
          THIS%NN_CONNECT%CONNECTED(ADSKY(NODE1)) = NODE2
          THIS%NN_CONNECT%CONNECTED(ADSKY(NODE2)) = NODE1
          ADSKY(NODE1) = ADSKY(NODE1) + 1
          ADSKY(NODE2) = ADSKY(NODE2) + 1
        ENDDO

        DEALLOCATE(ADSKY, EDGES, IDX, EDGES_TMP, NN_NB_CONNECT, NE_NB_CONNECT)
      END SUBROUTINE ALE_COMPUTE_CONNECTIVITY

Chd|====================================================================
Chd|  ALE_DEALLOCATE_CONNECTIVITY   modules/ale/ale_connectivity_mod.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ALE_DEALLOCATE_CONNECTIVITY(THIS)
        IMPLICIT NONE
        CLASS(T_ALE_CONNECTIVITY), INTENT(INOUT) :: THIS
        
        IF (ALLOCATED(THIS%NE_CONNECT%CONNECTED)) DEALLOCATE(THIS%NE_CONNECT%CONNECTED)
        IF (ALLOCATED(THIS%NE_CONNECT%IAD_CONNECT)) DEALLOCATE(THIS%NE_CONNECT%IAD_CONNECT)
        IF (ALLOCATED(THIS%NN_CONNECT%CONNECTED)) DEALLOCATE(THIS%NN_CONNECT%CONNECTED)
        IF (ALLOCATED(THIS%NN_CONNECT%IAD_CONNECT)) DEALLOCATE(THIS%NN_CONNECT%IAD_CONNECT)
        IF (ALLOCATED(THIS%NALE)) DEALLOCATE(THIS%NALE)
        
      END SUBROUTINE ALE_DEALLOCATE_CONNECTIVITY

Chd|====================================================================
Chd|  ALE_COMPUTE_EE_CONNECTIVITY   modules/ale/ale_connectivity_mod.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ALE_COMPUTE_EE_CONNECTIVITY(THIS, PM, IGEO,
     .     NPROPGI,NUMGEO, NPROPM, NUMMAT, NUMNOD, NUMELQ, NUMELTG, NUMELS, N2D, 
     .     IALE, IEULER, ITHERM, IALELAG,
     .     NIXQ, NIXTG, NIXS, ITAB,
     .     IXQ, IXTG, IXS)
        IMPLICIT NONE
C-----------------------------------------------
C     D e s c r i p t i o n
C-----------------------------------------------
!     Computes Element to Element connectivities  !
!     for ALE, and EULER computations             !
!     ---------------------------------------     !   
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
        CLASS(T_ALE_CONNECTIVITY), INTENT(INOUT) :: THIS
        INTEGER, INTENT(IN) :: NUMNOD, NUMELQ, NUMELTG, NUMELS, NPROPGI
        INTEGER, INTENT(IN) :: NIXQ, NIXTG, NIXS, N2D, IALE, IEULER, ITHERM, IALELAG, NPROPM, NUMMAT,NUMGEO
        MY_REAL, DIMENSION(NPROPM, NUMMAT), INTENT(IN) :: PM
        INTEGER, DIMENSION(NUMNOD), INTENT(IN) :: ITAB
        INTEGER, DIMENSION(NIXQ, NUMELQ), INTENT(IN) :: IXQ
        INTEGER, DIMENSION(NIXTG, NUMELTG), INTENT(IN) :: IXTG
        INTEGER, DIMENSION(NIXS, NUMELS), INTENT(IN) :: IXS
        INTEGER, DIMENSION(NPROPGI, NUMGEO), INTENT(IN) :: IGEO
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: II, JJ, NODE_ID, NODE1, NODE2, INODE
        INTEGER, DIMENSION(:), ALLOCATABLE :: ADSKY
        INTEGER :: IAD1, IAD2, ITMP, IAD
        INTEGER, DIMENSION(:), ALLOCATABLE :: IDX
        INTEGER, DIMENSION(:), ALLOCATABLE :: IAD_CONNECT, NE_NB_CONNECT, CONNECTED, TYPE, EE_NB_CONNECT,ITAG
        INTEGER(8) :: VEC_PTR1
        INTEGER :: JAL_FROM_MAT, JAL_FROM_PROP, JAL, JALT, MLW, IMID, TMP, COUNT
        INTEGER, DIMENSION(4), TARGET :: TETRA_NODES
        INTEGER, DIMENSION(6, 4), TARGET :: HEXA_FACE
        INTEGER, DIMENSION(6, 3), TARGET :: TETRA_FACE
        INTEGER, DIMENSION(4, 2), TARGET :: QUAD_FACE
        INTEGER, DIMENSION(3, 2), TARGET :: TRI_FACE
        INTEGER, DIMENSION(:, :), POINTER :: ELEM_FACE, ELEM_FACE2
        INTEGER :: KFACE, KFACE2, NFACE, NFACE_NODE, NFACE2, NFACE_NODE2
C-----------------------------------------------
C     B e g i n n i n g   o f   S u b r o u t i n e
C-----------------------------------------------  
        IF (IALE + IEULER + IALELAG +ITHERM == 0) THEN
           RETURN
        ENDIF
!     List of nodes of interest for tetra
        TETRA_NODES(1) = 2
        TETRA_NODES(2) = 4
        TETRA_NODES(3) = 7
        TETRA_NODES(4) = 6
!     Faces for each element
!     hexa
        HEXA_FACE(1, 1) = 1
        HEXA_FACE(1, 2) = 2
        HEXA_FACE(1, 3) = 3
        HEXA_FACE(1, 4) = 4
        HEXA_FACE(2, 1) = 3
        HEXA_FACE(2, 2) = 4
        HEXA_FACE(2, 3) = 8
        HEXA_FACE(2, 4) = 7
        HEXA_FACE(3, 1) = 5
        HEXA_FACE(3, 2) = 6
        HEXA_FACE(3, 3) = 7
        HEXA_FACE(3, 4) = 8
        HEXA_FACE(4, 1) = 1
        HEXA_FACE(4, 2) = 2
        HEXA_FACE(4, 3) = 6
        HEXA_FACE(4, 4) = 5
        HEXA_FACE(5, 1) = 2
        HEXA_FACE(5, 2) = 3
        HEXA_FACE(5, 3) = 7
        HEXA_FACE(5, 4) = 6
        HEXA_FACE(6, 1) = 1
        HEXA_FACE(6, 2) = 4
        HEXA_FACE(6, 3) = 8
        HEXA_FACE(6, 4) = 5
!     tetra
        TETRA_FACE(1, 1) = -1
        TETRA_FACE(1, 2) = -1
        TETRA_FACE(1, 3) = -1
        TETRA_FACE(2, 1) = 5
        TETRA_FACE(2, 2) = 6
        TETRA_FACE(2, 3) = 3
        TETRA_FACE(3, 1) = -1
        TETRA_FACE(3, 2) = -1
        TETRA_FACE(3, 3) = -1
        TETRA_FACE(4, 1) = 5
        TETRA_FACE(4, 2) = 1
        TETRA_FACE(4, 3) = 6
        TETRA_FACE(5, 1) = 1
        TETRA_FACE(5, 2) = 3
        TETRA_FACE(5, 3) = 6
        TETRA_FACE(6, 1) = 5
        TETRA_FACE(6, 2) = 3
        TETRA_FACE(6, 3) = 1
!     quad
        QUAD_FACE(1, 1) = 1
        QUAD_FACE(1, 2) = 2
        QUAD_FACE(2, 1) = 2
        QUAD_FACE(2, 2) = 3
        QUAD_FACE(3, 1) = 3
        QUAD_FACE(3, 2) = 4
        QUAD_FACE(4, 1) = 4
        QUAD_FACE(4, 2) = 1
!     tria
        TRI_FACE(1, 1) = 1
        TRI_FACE(1, 2) = 2
        TRI_FACE(2, 1) = 2
        TRI_FACE(2, 2) = 3
        TRI_FACE(3, 1) = 3
        TRI_FACE(3, 2) = 1
        IF (.NOT. THIS%NALE_ALREADY_COMPUTED) THEN
!     Marking ALE nodes
          IF (ALLOCATED(THIS%NALE)) DEALLOCATE(THIS%NALE)
          ALLOCATE(THIS%NALE(NUMNOD))
          THIS%NALE(1:NUMNOD) = 0
        ENDIF
!     element-element connectivity
        IF (ALLOCATED(THIS%EE_CONNECT%IAD_CONNECT)) DEALLOCATE(THIS%EE_CONNECT%IAD_CONNECT)
        IF (ALLOCATED(THIS%EE_CONNECT%CONNECTED)) DEALLOCATE(THIS%EE_CONNECT%CONNECTED)
        IF (ALLOCATED(THIS%EE_CONNECT%TYPE)) DEALLOCATE(THIS%EE_CONNECT%TYPE)
        IF (ALLOCATED(THIS%EE_CONNECT%IFACE2)) DEALLOCATE(THIS%EE_CONNECT%IFACE2)

!     Node element connectivity
        ALLOCATE(NE_NB_CONNECT(NUMNOD))
        NE_NB_CONNECT(1:NUMNOD) = 0

!       2D elements
!       /TRIA
        DO II = 1, NUMELTG
!       ale : jal = 1, euler : jal = 2
         JAL_FROM_MAT = NINT(PM(72, IABS(IXTG(1, II)))) 
         JAL_FROM_PROP = IGEO(62, IABS(IXTG(5, II)))
         JAL = MAX(JAL_FROM_MAT, JAL_FROM_PROP)
         JALT = JAL + NINT(PM(71, IABS(IXTG(1, II))) + PM(96, IABS(IXTG(1, II))))
         IMID = IABS(IXTG(1, II))
         IF (JALT == 0) CYCLE
         MLW  = NINT(PM(19,IMID))
         DO JJ = 1, 3
            NODE_ID = IXTG(1 + JJ, II)
            NE_NB_CONNECT(NODE_ID) = NE_NB_CONNECT(NODE_ID) + 1
            IF (.NOT. THIS%NALE_ALREADY_COMPUTED) THEN
               THIS%NALE(NODE_ID) = MAX(THIS%NALE(NODE_ID), JAL)
               IF (MLW == 151) THEN
                  IF (THIS%NALE(NODE_ID) == 1 .OR. THIS%NALE(NODE_ID) == 2) THIS%NALE(NODE_ID) = 150 + THIS%NALE(NODE_ID)
               ENDIF
            ENDIF
         ENDDO
        ENDDO
!       /QUAD
        DO II = 1, NUMELQ
!       ale : jal = 1, euler : jal = 2
         JAL_FROM_MAT = NINT(PM(72, IABS(IXQ(1, II))))
         JAL_FROM_PROP = IGEO(62,IABS(IXQ(6, II))) 
         JAL = MAX(JAL_FROM_MAT, JAL_FROM_PROP)
         JALT = JAL + NINT(PM(71, IABS(IXQ(1, II))) + PM(96, IABS(IXQ(1, II))))
         IMID = IABS(IXQ(1, II))
         IF (JALT == 0) CYCLE
         MLW  = NINT(PM(19,IMID))
         DO JJ = 1, 4
            NODE_ID = IXQ(1 + JJ, II)
            NE_NB_CONNECT(NODE_ID) = NE_NB_CONNECT(NODE_ID) + 1
            IF (.NOT. THIS%NALE_ALREADY_COMPUTED) THEN
               THIS%NALE(NODE_ID) = MAX(THIS%NALE(NODE_ID), JAL)
               IF (MLW == 151) THEN
                  IF (THIS%NALE(NODE_ID) == 1 .OR. THIS%NALE(NODE_ID) == 2) THIS%NALE(NODE_ID) = 150 + THIS%NALE(NODE_ID)
               ENDIF
            ENDIF
         ENDDO
        ENDDO
        
!       3D elements
        DO II = 1, NUMELS
         JAL_FROM_MAT = NINT(PM(72, IABS(IXS(1, II))))
         JAL_FROM_PROP = IGEO(62, IABS(IXS(10, II)))
         JAL = MAX(JAL_FROM_MAT, JAL_FROM_PROP) 
         JALT = JAL + NINT(PM(71, IABS(IXS(1, II))) + PM(96, IABS(IXS(1, II))))
         IMID = IABS(IXS(1, II))
         IF (JALT == 0) CYCLE
         MLW  = NINT(PM(19,IMID))
         IF (IXS(2, II) == IXS(3, II) .AND. IXS(4, II) == IXS(5, II) .AND. 
     .        IXS(6, II) == IXS(9, II) .AND. IXS(7, II) == IXS(8, II)) THEN
!         tetra 
           DO JJ = 1, 4
             NODE_ID = IXS(TETRA_NODES(JJ), II)
             NE_NB_CONNECT(NODE_ID) = NE_NB_CONNECT(NODE_ID) + 1
             IF (.NOT. THIS%NALE_ALREADY_COMPUTED) THEN
               THIS%NALE(NODE_ID) = MAX(THIS%NALE(NODE_ID), JAL)
               IF (MLW == 151) THEN
                  IF (THIS%NALE(NODE_ID) == 1 .OR. THIS%NALE(NODE_ID) == 2) THIS%NALE(NODE_ID) = 150 + THIS%NALE(NODE_ID)
               ENDIF
             ENDIF
            ENDDO
         ELSE
!          bricks
           DO JJ = 1, 8
             NODE_ID = IXS(1 + JJ, II)
             NE_NB_CONNECT(NODE_ID) = NE_NB_CONNECT(NODE_ID) + 1
             IF (.NOT. THIS%NALE_ALREADY_COMPUTED) THEN
               THIS%NALE(NODE_ID) = MAX(THIS%NALE(NODE_ID), JAL)
               IF (MLW == 151) THEN
                 IF (THIS%NALE(NODE_ID) == 1 .OR. THIS%NALE(NODE_ID) == 2) THIS%NALE(NODE_ID) = 150 + THIS%NALE(NODE_ID)
               ENDIF
             ENDIF
           ENDDO
         ENDIF
        ENDDO    

        THIS%NALE_ALREADY_COMPUTED = .TRUE.

!       Indirection tab
        ALLOCATE(IAD_CONNECT(NUMNOD + 1))
        IAD_CONNECT(1) = 1
        DO II = 2, NUMNOD + 1
          IAD_CONNECT(II) = IAD_CONNECT(II - 1) + NE_NB_CONNECT(II - 1) 
        ENDDO
        ALLOCATE(ADSKY(NUMNOD))
        DO II = 1, NUMNOD
          ADSKY(II) = IAD_CONNECT(II)
        ENDDO

        ALLOCATE(CONNECTED(IAD_CONNECT(NUMNOD + 1)))
        CONNECTED(:) = 0
        ALLOCATE(TYPE(IAD_CONNECT(NUMNOD + 1)))
        TYPE(:) = 0
        
!       2D elements
!       /TRIA
        DO II = 1, NUMELTG
         JAL_FROM_MAT = NINT(PM(72, IABS(IXTG(1, II)))) 
         JAL_FROM_PROP = IGEO(62,IABS(IXTG(5, II)))
         JAL = MAX(JAL_FROM_MAT, JAL_FROM_PROP)
         JALT = JAL + NINT(PM(71, IABS(IXTG(1, II))) + PM(96, IABS(IXTG(1, II))))
         IMID = IABS(IXTG(1, II))
         IF (JALT == 0) CYCLE
         DO JJ = 1, 3
            NODE_ID = IXTG(1 + JJ, II)
            CONNECTED(ADSKY(NODE_ID)) = II
            TYPE(ADSKY(NODE_ID)) = 3
            ADSKY(NODE_ID) = ADSKY(NODE_ID) + 1
         ENDDO
        ENDDO
!       /QUAD
        DO II = 1, NUMELQ
         JAL_FROM_MAT = NINT(PM(72, IABS(IXQ(1, II))))
         JAL_FROM_PROP = IGEO(62, IABS(IXQ(6, II)) ) 
         JAL = MAX(JAL_FROM_MAT, JAL_FROM_PROP)
         JALT = JAL + NINT(PM(71, IABS(IXQ(1, II))) + PM(96, IABS(IXQ(1, II))))
         IMID = IABS(IXQ(1, II))
         IF (JALT == 0) CYCLE
         DO JJ = 1, 4
            NODE_ID = IXQ(1 + JJ, II)
            CONNECTED(ADSKY(NODE_ID)) = II
            TYPE(ADSKY(NODE_ID)) = 2
            ADSKY(NODE_ID) = ADSKY(NODE_ID) + 1
         ENDDO
       ENDDO
!      3D elements
        DO II = 1, NUMELS
         JAL_FROM_MAT = NINT(PM(72, IABS(IXS(1, II))))
         JAL_FROM_PROP = IGEO(62, IABS(IXS(10, II)) ) 
         JAL = MAX(JAL_FROM_MAT, JAL_FROM_PROP)         
         JALT = JAL + NINT(PM(71, IABS(IXS(1, II))) + PM(96, IABS(IXS(1, II))))
         IMID = IABS(IXS(1, II))
         IF (JALT == 0) CYCLE
         IF (IXS(2, II) == IXS(3, II) .AND. IXS(4, II) == IXS(5, II) .AND. 
     .        IXS(6, II) == IXS(9, II) .AND. IXS(7, II) == IXS(8, II)) THEN
!          tetra 
           NODE_ID = IXS(2, II)                
           CONNECTED(ADSKY(NODE_ID)) = II      
           TYPE(ADSKY(NODE_ID)) = 1            
           ADSKY(NODE_ID) = ADSKY(NODE_ID) + 1 
           NODE_ID = IXS(4, II)                
           CONNECTED(ADSKY(NODE_ID)) = II      
           TYPE(ADSKY(NODE_ID)) = 1            
           ADSKY(NODE_ID) = ADSKY(NODE_ID) + 1 
           NODE_ID = IXS(7, II)                
           CONNECTED(ADSKY(NODE_ID)) = II      
           TYPE(ADSKY(NODE_ID)) = 1            
           ADSKY(NODE_ID) = ADSKY(NODE_ID) + 1 
           NODE_ID = IXS(6, II)                
           CONNECTED(ADSKY(NODE_ID)) = II      
           TYPE(ADSKY(NODE_ID)) = 1            
           ADSKY(NODE_ID) = ADSKY(NODE_ID) + 1 
         ELSE
!           bricks
            DO JJ = 1, 8
               NODE_ID = IXS(1 + JJ, II)
               CONNECTED(ADSKY(NODE_ID)) = II
               TYPE(ADSKY(NODE_ID)) = 1
               ADSKY(NODE_ID) = ADSKY(NODE_ID) + 1
            ENDDO
         ENDIF
        ENDDO

      
!       Counting connected elements
        IF (N2D == 0) THEN
         ALLOCATE(THIS%EE_CONNECT%IAD_CONNECT(NUMELS+1))
         ALLOCATE(EE_NB_CONNECT(NUMELS))
        ELSE
         ALLOCATE(THIS%EE_CONNECT%IAD_CONNECT(NUMELTG + NUMELQ + 1))
         ALLOCATE(EE_NB_CONNECT(NUMELTG + NUMELQ))
        ENDIF
        EE_NB_CONNECT(:) = 0

        TMP = 0
        IF (N2D == 0) THEN
!        3D elements
         DO II = 1, NUMELS
            JAL_FROM_MAT = NINT(PM(72, IABS(IXS(1, II))))
            JAL_FROM_PROP = IGEO(62, IABS(IXS(10, II)) ) 
            JAL = MAX(JAL_FROM_MAT, JAL_FROM_PROP)            
            JALT = JAL + NINT(PM(71, IABS(IXS(1, II))) + PM(96, IABS(IXS(1, II))))
            IF (JALT == 0) CYCLE
            IF (IXS(2, II) == IXS(3, II) .AND. IXS(4, II) == IXS(5, II) .AND. 
     .           IXS(6, II) == IXS(9, II) .AND. IXS(7, II) == IXS(8, II)) THEN
!             Tetras
              EE_NB_CONNECT(II) = 6
            ELSE
!             Hexa
              EE_NB_CONNECT(II) = 6
            ENDIF
         ENDDO
         THIS%EE_CONNECT%IAD_CONNECT(1) = 1
         DO II = 2, NUMELS + 1
            THIS%EE_CONNECT%IAD_CONNECT(II) = THIS%EE_CONNECT%IAD_CONNECT(II - 1) + EE_NB_CONNECT(II - 1)
         ENDDO
         TMP = THIS%EE_CONNECT%IAD_CONNECT(NUMELS + 1)
        ELSE
!          2D elements                                                                                    
!         /QUAD                                                                                           
          DO II = 1, NUMELQ                                                                               
             JAL_FROM_MAT = NINT(PM(72, IABS(IXQ(1, II))))                                                
             JAL_FROM_PROP = IGEO(62, IABS(IXQ(6, II)) )                                                  
             JAL = MAX(JAL_FROM_MAT, JAL_FROM_PROP)                                                       
             JALT = JAL + NINT(PM(71, IABS(IXQ(1, II))) + PM(96, IABS(IXQ(1, II))))                       
             IF (JALT == 0) CYCLE                                                                         
             EE_NB_CONNECT(II) = 4                                                                        
          ENDDO                                                                                           
!         /TRIA                                                                                           
          DO II = 1, NUMELTG                                                                              
             JAL_FROM_MAT = NINT(PM(72, IABS(IXTG(1, II))))                                               
             JAL_FROM_PROP = IGEO(62, IABS(IXTG(5, II)) )                                                 
             JAL = MAX(JAL_FROM_MAT, JAL_FROM_PROP)                                                       
             JALT = JAL + NINT(PM(71, IABS(IXTG(1, II))) + PM(96, IABS(IXTG(1, II))))                     
             IF (JALT == 0) CYCLE                                                                         
             EE_NB_CONNECT(II) = 3                                                                        
          ENDDO                                                                                           
          THIS%EE_CONNECT%IAD_CONNECT(1) = 1                                                              
          DO II = 2, NUMELQ + NUMELTG + 1                                                                 
             THIS%EE_CONNECT%IAD_CONNECT(II) = THIS%EE_CONNECT%IAD_CONNECT(II - 1) + EE_NB_CONNECT(II - 1)
          ENDDO                                                                                           
          TMP = THIS%EE_CONNECT%IAD_CONNECT(NUMELQ + NUMELTG + 1) - 1                                     
        ENDIF

        ALLOCATE(THIS%EE_CONNECT%CONNECTED(TMP))
        ALLOCATE(THIS%EE_CONNECT%TYPE(TMP))
        ALLOCATE(THIS%EE_CONNECT%IFACE2(TMP))
        THIS%EE_CONNECT%TYPE(1:TMP) = 0
        THIS%EE_CONNECT%CONNECTED(1:TMP) = 0
        THIS%EE_CONNECT%IFACE2(1:TMP) = 0
        CALL INTVECTOR_CREATE(VEC_PTR1)
        ALLOCATE(ITAG(NUMNOD))
        ITAG(1:NUMNOD) = 0
        IF (N2D == 0) THEN
           DO II = 1, NUMELS 
              JAL_FROM_MAT = NINT(PM(72, IABS(IXS(1, II))))
              JAL_FROM_PROP = IGEO(62, IABS(IXS(10, II)) ) 
              JAL = MAX(JAL_FROM_MAT, JAL_FROM_PROP)            
              JALT = JAL + NINT(PM(71, IABS(IXS(1, II))) + PM(96, IABS(IXS(1, II))))
              IF (JALT == 0) CYCLE
              IAD1 = THIS%EE_CONNECT%IAD_CONNECT(II)
              IF (IXS(2, II) == IXS(3, II) .AND. IXS(4, II) == IXS(5, II) .AND. 
     .             IXS(6, II) == IXS(9, II) .AND. IXS(7, II) == IXS(8, II)) THEN
!              Tetras
               NFACE = 6
               NFACE_NODE = 3
               COUNT = 3
               ELEM_FACE => TETRA_FACE
              ELSE
!              Hexa
               NFACE = 6
               NFACE_NODE = 4
               COUNT = 4
               ELEM_FACE => HEXA_FACE
              ENDIF
              DO KFACE = 1, NFACE
               CALL INTVECTOR_CLEAR(VEC_PTR1)
               DO INODE = 1, NFACE_NODE
                  IF (ELEM_FACE(KFACE, INODE) < 0) CYCLE
                  NODE_ID = IXS(1 + ELEM_FACE(KFACE, INODE), II)
                  ITAG(NODE_ID) = 1
                  DO IAD = IAD_CONNECT(NODE_ID), IAD_CONNECT(NODE_ID + 1) - 1
                     IF (CONNECTED(IAD) /= II) THEN
                        CALL INTVECTOR_PUSH_BACK(VEC_PTR1, CONNECTED(IAD))
                     ENDIF
                  ENDDO
               ENDDO
!              get the redundant element number
               CALL INTVECTOR_GET_REDUNDANT(VEC_PTR1, JJ, ITMP, COUNT)
               IAD1 = THIS%EE_CONNECT%IAD_CONNECT(II)
               THIS%EE_CONNECT%CONNECTED(IAD1 + KFACE - 1) = JJ
               IF (JJ > 0) THEN
                  IF (IXS(2, JJ) == IXS(3, JJ) .AND. IXS(4, JJ) == IXS(5, JJ) .AND. 
     .                 IXS(6, JJ) == IXS(9, JJ) .AND. IXS(7, JJ) == IXS(8, JJ)) THEN
!                    Tetras
                     NFACE2 = 6
                     NFACE_NODE2 = 3
                     ELEM_FACE2 => TETRA_FACE
                     THIS%EE_CONNECT%TYPE(IAD1 + KFACE - 1) = 1
                  ELSE
!                    Hexa
                     NFACE2 = 6
                     NFACE_NODE2 = 4
                     ELEM_FACE2 => HEXA_FACE
                     THIS%EE_CONNECT%TYPE(IAD1 + KFACE - 1) = 0
                  ENDIF
                  DO KFACE2 = 1, NFACE2
                     ITMP = 1
                     DO INODE = 1, NFACE_NODE2
                        IF (ELEM_FACE(KFACE2, INODE) < 0) CYCLE
                        ITMP = ITMP * ITAG(IXS(1 + ELEM_FACE(KFACE2, INODE), JJ))
                     ENDDO
                     IF (ITMP == 1) THEN
                        THIS%EE_CONNECT%IFACE2(IAD1 + KFACE - 1) = KFACE2
                        EXIT
                     ENDIF
                  ENDDO
               ENDIF
               DO INODE = 1, NFACE_NODE
                  IF (ELEM_FACE(KFACE, INODE) < 0) CYCLE
                  NODE_ID = IXS(1 + ELEM_FACE(KFACE, INODE), II)
                  ITAG(NODE_ID) = 0
               ENDDO
              ENDDO
           ENDDO
        ELSE
!         Quad
          DO II = 1, NUMELQ
            JAL_FROM_MAT = NINT(PM(72, IABS(IXQ(1, II))))
            JAL_FROM_PROP = IGEO(62, IABS(IXQ(6, II)) ) 
            JAL = MAX(JAL_FROM_MAT, JAL_FROM_PROP)
            JALT = JAL + NINT(PM(71, IABS(IXQ(1, II))) + PM(96, IABS(IXQ(1, II))))
            IF (JALT == 0) CYCLE
            IAD1 = THIS%EE_CONNECT%IAD_CONNECT(II)
            NFACE = 4
            NFACE_NODE = 2
            ELEM_FACE => QUAD_FACE
            COUNT = 2
            DO KFACE = 1, NFACE
              CALL INTVECTOR_CLEAR(VEC_PTR1)                                 
              DO INODE = 1, NFACE_NODE                                       
                 NODE_ID = IXQ(1 + ELEM_FACE(KFACE, INODE), II)              
                 ITAG(NODE_ID) = 1                                           
                 DO IAD = IAD_CONNECT(NODE_ID), IAD_CONNECT(NODE_ID + 1) - 1 
                    IF (CONNECTED(IAD) /= II) THEN                           
                       CALL INTVECTOR_PUSH_BACK(VEC_PTR1, CONNECTED(IAD))    
                    ENDIF                                                    
                 ENDDO                                                       
              ENDDO                                                          
!     get the redundant element number
              CALL INTVECTOR_GET_REDUNDANT(VEC_PTR1, JJ, ITMP, COUNT)            
              THIS%EE_CONNECT%CONNECTED(IAD1 + KFACE - 1) = JJ                   
              IF (JJ > 0) THEN                                                   
                 IF (JJ > NUMELQ) THEN                                           
                    NFACE2 = 3                                                   
                    NFACE_NODE2 = 2                                              
                    ELEM_FACE2 => TRI_FACE                                       
                    THIS%EE_CONNECT%TYPE(IAD1 + KFACE - 1) = 3                   
                 ELSE                                                            
                    NFACE2 = 4                                                   
                    NFACE_NODE2 = 2                                              
                    ELEM_FACE2 => QUAD_FACE                                      
                    THIS%EE_CONNECT%TYPE(IAD1 + KFACE - 1) = 2                   
                 ENDIF                                                           
                 DO KFACE2 = 1, NFACE2                                           
                    ITMP = 1                                                     
                    DO INODE = 1, NFACE_NODE2                                    
                       ITMP = ITMP * ITAG(IXQ(1 + ELEM_FACE(KFACE2, INODE), JJ)) 
                    ENDDO                                                        
                    IF (ITMP == 1) THEN                                          
                       THIS%EE_CONNECT%IFACE2(IAD1 + KFACE - 1) = KFACE2         
                       EXIT                                                      
                    ENDIF                                                        
                 ENDDO                                                           
              ENDIF                                                              
              DO INODE = 1, NFACE_NODE                                           
                 NODE_ID = IXQ(1 + ELEM_FACE(KFACE, INODE), II)                  
                 ITAG(NODE_ID) = 0                                               
              ENDDO                                                              
            ENDDO
          ENDDO
!         Tria
          DO II = 1, NUMELTG
            JAL_FROM_MAT = NINT(PM(72, IABS(IXTG(1, II))))
            JAL_FROM_PROP = IGEO(62, IABS(IXTG(5, II)) ) 
            JAL = MAX(JAL_FROM_MAT, JAL_FROM_PROP)            
            JALT = JAL + NINT(PM(71, IABS(IXTG(1, II))) + PM(96, IABS(IXTG(1, II))))
            IF (JALT == 0) CYCLE
            IAD1 = THIS%EE_CONNECT%IAD_CONNECT(II)
            NFACE = 3
            NFACE_NODE = 2
            ELEM_FACE => TRI_FACE
            COUNT = 2
            DO KFACE = 1, NFACE
               CALL INTVECTOR_CLEAR(VEC_PTR1)
               DO INODE = 1, NFACE_NODE
                  NODE_ID = IXTG(1 + ELEM_FACE(KFACE, INODE), II)
                  ITAG(NODE_ID) = 1
                  DO IAD = IAD_CONNECT(NODE_ID), IAD_CONNECT(NODE_ID + 1) - 1
                     IF (CONNECTED(IAD) /= II) THEN
                        CALL INTVECTOR_PUSH_BACK(VEC_PTR1, CONNECTED(IAD))
                     ENDIF
                  ENDDO
               ENDDO
!               get the redundant element number
               CALL INTVECTOR_GET_REDUNDANT(VEC_PTR1, JJ, ITMP, COUNT)
               THIS%EE_CONNECT%CONNECTED(IAD1 + KFACE - 1) = JJ
               IF (JJ > 0) THEN
                  IF (JJ > NUMELQ) THEN
                     NFACE2 = 2
                     NFACE_NODE2 = 2
                     ELEM_FACE2 => TRI_FACE
                     THIS%EE_CONNECT%TYPE(IAD1 + KFACE - 1) = 3
                  ELSE
                     NFACE2 = 4
                     NFACE_NODE2 = 3
                     ELEM_FACE2 => QUAD_FACE
                     THIS%EE_CONNECT%TYPE(IAD1 + KFACE - 1) = 2
                  ENDIF
                  DO KFACE2 = 1, NFACE2
                     ITMP = 1
                     DO INODE = 1, NFACE_NODE2
                        ITMP = ITMP * ITAG(IXTG(1 + ELEM_FACE(KFACE2, INODE), JJ))
                     ENDDO
                     IF (ITMP == 1) THEN
                        THIS%EE_CONNECT%IFACE2(IAD1 + KFACE - 1) = KFACE2
                        EXIT
                     ENDIF
                  ENDDO
               ENDIF
               DO INODE = 1, NFACE_NODE
                  NODE_ID = IXTG(1 + ELEM_FACE(KFACE, INODE), II)
                  ITAG(NODE_ID) = 0
               ENDDO
            ENDDO
          ENDDO
        ENDIF

        CALL INTVECTOR_DELETE(VEC_PTR1)
        IF (ALLOCATED(EE_NB_CONNECT)) DEALLOCATE(EE_NB_CONNECT)
        IF (ALLOCATED(ITAG)) DEALLOCATE(ITAG)
        IF (ALLOCATED(NE_NB_CONNECT)) DEALLOCATE(NE_NB_CONNECT)
        IF (ALLOCATED(IAD_CONNECT)) DEALLOCATE(IAD_CONNECT)
        IF (ALLOCATED(ADSKY)) DEALLOCATE(ADSKY)
        IF (ALLOCATED(CONNECTED)) DEALLOCATE(CONNECTED)
      
      END SUBROUTINE ALE_COMPUTE_EE_CONNECTIVITY
C-----------------------------------------------
      
      END MODULE ALE_CONNECTIVITY_MOD
