Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||====================================================================
      !||    nlocal_reg_mod         ../common_source/modules/nlocal_reg_mod.F
      !||--- called by ------------------------------------------------------
      !||    alemain                ../engine/source/ale/alemain.F
      !||    bforc2                 ../engine/source/ale/bimat/bforc2.F
      !||    c3fint_reg             ../engine/source/elements/sh3n/coque3n/c3fint_reg.F
      !||    c3fint_reg_ini         ../starter/source/elements/sh3n/coque3n/c3fint_reg_ini.F
      !||    c3forc3                ../engine/source/elements/sh3n/coque3n/c3forc3.F
      !||    c3forc3_crk            ../engine/source/elements/xfem/c3forc3_crk.F
      !||    c3init3                ../starter/source/elements/sh3n/coque3n/c3init3.F
      !||    c_front                ../starter/source/restart/ddsplit/c_front.F
      !||    cbafint_reg            ../engine/source/elements/shell/coqueba/cbafint_reg.F
      !||    cbafint_reg_ini        ../starter/source/elements/shell/coqueba/cbafint_reg_ini.F
      !||    cbaforc3               ../engine/source/elements/shell/coqueba/cbaforc3.F
      !||    cbainit3               ../starter/source/elements/shell/coqueba/cbainit3.F
      !||    cbavarnl               ../engine/source/elements/shell/coqueba/cbavarnl.F
      !||    cdk6fint_reg           ../engine/source/elements/sh3n/coquedk6/cdk6fint_reg.F
      !||    cdk6forc3              ../engine/source/elements/sh3n/coquedk6/cdk6forc3.F
      !||    cdkfint_reg            ../engine/source/elements/sh3n/coquedk/cdkfint_reg.F
      !||    cdkfint_reg_ini        ../starter/source/elements/sh3n/coquedk/cdkfint_reg_ini.F
      !||    cdkforc3               ../engine/source/elements/sh3n/coquedk/cdkforc3.F
      !||    cdkinit3               ../starter/source/elements/sh3n/coquedk/cdkinit3.F
      !||    cfint_reg              ../engine/source/elements/shell/coque/cfint_reg.F
      !||    cfint_reg_ini          ../starter/source/elements/shell/coque/cfint_reg_ini.F
      !||    cforc3                 ../engine/source/elements/shell/coque/cforc3.F
      !||    cforc3_crk             ../engine/source/elements/xfem/cforc3_crk.F
      !||    cinit3                 ../starter/source/elements/shell/coque/cinit3.F
      !||    cmain3                 ../engine/source/materials/mat_share/cmain3.F
      !||    czforc3                ../engine/source/elements/shell/coquez/czforc3.F
      !||    czforc3_crk            ../engine/source/elements/xfem/czforc3_crk.F
      !||    ddsplit                ../starter/source/restart/ddsplit/ddsplit.F
      !||    dtcba_reg              ../engine/source/elements/shell/coqueba/dtcba_reg.F
      !||    dtcdk_reg              ../engine/source/elements/sh3n/coquedk/dtcdk_reg.F
      !||    forint                 ../engine/source/elements/forint.F
      !||    forintc                ../engine/source/elements/forintc.F
      !||    forintp                ../engine/source/elements/forintp.F
      !||    hm_read_fail           ../starter/source/materials/fail/hm_read_fail.F
      !||    hm_read_fail_gurson    ../starter/source/materials/fail/gurson/hm_read_fail_gurson.F
      !||    hm_read_nonlocal       ../starter/source/materials/nonlocal/hm_read_nonlocal.F
      !||    ig3duforc3             ../engine/source/elements/ige3d/ig3duforc3.F
      !||    initia                 ../starter/source/elements/initia/initia.F
      !||    law104_upd             ../starter/source/materials/mat/mat104/law104_upd.F
      !||    lectur                 ../engine/source/input/lectur.F
      !||    mmain                  ../engine/source/materials/mat_share/mmain.F90
      !||    mulaw                  ../engine/source/materials/mat_share/mulaw.F90
      !||    mulawc                 ../engine/source/materials/mat_share/mulawc.F
      !||    nloc_dmg_init          ../starter/source/materials/fail/nloc_dmg_init.F
      !||    nlocal_acc             ../engine/source/assembly/nlocal_acc.F
      !||    nlocal_dtnoda          ../engine/source/time_step/nlocal_dtnoda.F
      !||    nlocal_incr            ../engine/source/assembly/nlocal_incr.F
      !||    nlocal_init_sta        ../starter/source/materials/fail/nlocal_init_sta.F
      !||    nlocal_vel             ../engine/source/assembly/nlocal_vel.F
      !||    q4forc2                ../engine/source/elements/solid_2d/quad4/q4forc2.F
      !||    qforc2                 ../engine/source/elements/solid_2d/quad/qforc2.F
      !||    r2r_exchange           ../engine/source/coupling/rad2rad/r2r_exchange.F
      !||    r2r_getdata            ../engine/source/coupling/rad2rad/r2r_getdata.F
      !||    r2r_group              ../starter/source/coupling/rad2rad/r2r_group.F
      !||    r2r_init               ../engine/source/coupling/rad2rad/r2r_init.F
      !||    rdresb                 ../engine/source/output/restart/rdresb.F
      !||    read_material_models   ../starter/source/materials/read_material_models.F
      !||    read_nloc_struct       ../engine/source/output/restart/read_nloc_struct.F
      !||    resol                  ../engine/source/engine/resol.F
      !||    resol_init             ../engine/source/engine/resol_init.F
      !||    restmod                ../engine/share/modules/restart_mod.F
      !||    s10forc3               ../engine/source/elements/solid/solide10/s10forc3.F
      !||    s16forc3               ../engine/source/elements/thickshell/solide16/s16forc3.F
      !||    s20forc3               ../engine/source/elements/solid/solide20/s20forc3.F
      !||    s4fint_reg             ../engine/source/elements/solid/solide4/s4fint_reg.F
      !||    s4forc3                ../engine/source/elements/solid/solide4/s4forc3.F
      !||    s6cfint_reg            ../engine/source/elements/thickshell/solide6c/s6cfint_reg.F
      !||    s6cforc3               ../engine/source/elements/thickshell/solide6c/s6cforc3.F
      !||    s8cfint_reg            ../engine/source/elements/thickshell/solide8c/s8cfint_reg.F
      !||    s8cforc3               ../engine/source/elements/thickshell/solide8c/s8cforc3.F
      !||    s8eforc3               ../engine/source/elements/solid/solide8e/s8eforc3.F
      !||    s8sforc3               ../engine/source/elements/solid/solide8s/s8sforc3.F
      !||    s8zfint_reg            ../engine/source/elements/solid/solide8z/s8zfint_reg.F
      !||    s8zforc3               ../engine/source/elements/solid/solide8z/s8zforc3.F
      !||    scfint_reg             ../engine/source/elements/thickshell/solidec/scfint_reg.F
      !||    scforc3                ../engine/source/elements/thickshell/solidec/scforc3.F
      !||    sfint_reg              ../engine/source/elements/solid/solide/sfint_reg.F
      !||    sforc3                 ../engine/source/elements/solid/solide/sforc3.F
      !||    spmd_collect_nlocal    ../engine/source/mpi/output/spmd_collect_nlocal.F
      !||    spmd_exch_r2r_nl       ../engine/source/mpi/r2r/spmd_exch_r2r_nl.F
      !||    spmd_exch_sub_poff     ../engine/source/mpi/spmd_exch_sub.F
      !||    spmd_exch_sub_pon      ../engine/source/mpi/spmd_exch_sub.F
      !||    spmd_sub_boundaries    ../engine/source/mpi/spmd_exch_sub.F
      !||    spstres                ../engine/source/elements/sph/spstres.F
      !||    szforc3                ../engine/source/elements/solid/solidez/szforc3.F
      !||    updmat                 ../starter/source/materials/updmat.F
      !||    usermat_shell          ../engine/source/materials/mat_share/usermat_shell.F
      !||    usermat_solid          ../engine/source/materials/mat_share/usermat_solid.F
      !||    w_front                ../starter/source/restart/ddsplit/w_front.F
      !||    write_nloc_struct      ../engine/source/output/restart/write_nloc_struct.F
      !||====================================================================
      MODULE NLOCAL_REG_MOD
C----------------------------------------------------------------------- 
#include "my_real.inc"
c=======================================================================      
c define type NLOCAL_STRUCT_ for nodal variable regularization
c=======================================================================      
c 
      TYPE NLOCAL_STR_
        integer  :: IMOD                                  ! Non-local regularization flag  
        integer  :: NNOD                                  ! Number of nodes with additional d.o.fs 
        integer  :: L_NLOC                                ! Length of non-local vectors
        integer  :: NUMELS_NL                             ! Number of non-local solids elements
        integer  :: NUMELC_NL                             ! Number of non-local shell elements
        integer  :: NUMELTG_NL                            ! Number of non-local triangle shell elements
        integer  :: NDDMAX                                ! Maximum number of additional d.o.fs
        integer  :: LCNE_NL                               ! Length of the PROCNE table
        my_real, DIMENSION(:)   , ALLOCATABLE :: DENS     ! (NUMMAT) Non-local 'density' parameter  
        my_real, DIMENSION(:)   , ALLOCATABLE :: DAMP     ! (NUMMAT) Non-local 'damping' coefficient      
        my_real, DIMENSION(:)   , ALLOCATABLE :: LEN      ! (NUMMAT) Non-local internal length
        my_real, DIMENSION(:)   , ALLOCATABLE :: LE_MAX   ! (NUMMAT) Maximal element length target
        my_real, DIMENSION(:)   , ALLOCATABLE :: SSPNL    ! (NUMMAT) Non-local "sound speed"
        integer, DIMENSION(:)   , ALLOCATABLE :: INDX     ! (NNOD)   Nodal index table
        integer, DIMENSION(:)   , ALLOCATABLE :: POSI     ! (NNOD+1) Node first d.o.f position in non-local vectors
        integer, DIMENSION(:)   , ALLOCATABLE :: IDXI     ! (NUMNOD) Inversed nodal index table
        integer, DIMENSION(:)   , ALLOCATABLE :: ADDCNE   ! (0:NNOD+1) Addresses of non-local nodes in the FSKY vector
        integer, DIMENSION(:)   , ALLOCATABLE :: CNE      ! ADDCNE(NNOD+1)-1 Element attached to nodes in the FSKY vector
        integer, DIMENSION(:)   , ALLOCATABLE :: PROCNE   ! LCNE_NL Number of the processor on which the element is located
        integer, DIMENSION(:,:) , ALLOCATABLE :: IADS     ! Addresses sky for solids non-local elements
        integer, DIMENSION(:,:) , ALLOCATABLE :: IADC     ! Addresses sky for shells non-local elements
        integer, DIMENSION(:,:) , ALLOCATABLE :: IADTG    ! Addresses sky for triangle shells non-local elements
        my_real, DIMENSION(:)   , ALLOCATABLE :: MASS     ! (L_NLOC)  Non-local mass
        my_real, DIMENSION(:)   , ALLOCATABLE :: MASS0    ! (L_NLOC)  Non-local mass initial
        my_real, DIMENSION(:,:) , ALLOCATABLE :: FNL      ! (L_NLOC,NTHREAD)  Non-local force/accelerations
        my_real, DIMENSION(:)   , ALLOCATABLE :: VNL      ! (L_NLOC)  Non-local variable velocities
        my_real, DIMENSION(:)   , ALLOCATABLE :: VNL_OLD  ! (L_NLOC)  Old Non-local variable velocities
        my_real, DIMENSION(:)   , ALLOCATABLE :: DNL      ! (L_NLOC)  Non-local variable increment
        my_real, DIMENSION(:)   , ALLOCATABLE :: UNL      ! (L_NLOC)  Non-local cumulated variable
        my_real, DIMENSION(:,:) , ALLOCATABLE :: STIFNL   ! (L_NLOC,NTHREAD)  Non-local nodal stiffness
        my_real, DIMENSION(:,:) , ALLOCATABLE :: FSKY     ! (ADDCNE(NNOD+1),NDDMAX)  Non-local skyline vector for PARITH/ON
        my_real, DIMENSION(:,:) , ALLOCATABLE :: STSKY    ! (ADDCNE(NNOD+1),NDDMAX)  Non-local skyline vector for stiffness in PARITH/ON
        integer, DIMENSION(:)   , ALLOCATABLE :: IAD_ELEM ! (NSPMD+1) Non-local IAD_ELEM vector for SPMD boundaries
        integer, DIMENSION(:)   , ALLOCATABLE :: IAD_SIZE ! (NSPMD+1) Non-local size of the buffer at the domains boundaries
        integer, DIMENSION(:)   , ALLOCATABLE :: FR_ELEM  ! (TOTAL_NODES) Non-local FR_ELEM vector for SPMD boundaries
C       SPMD PON BOUNDARIES
        integer, DIMENSION(:)   , ALLOCATABLE :: ISENDSP  ! Skyline PON Send index
        integer, DIMENSION(:)   , ALLOCATABLE :: IRECSP   ! Skyline PON Receive index

! --------------------------------   
!       communication for parith/on 
        integer, DIMENSION(:)   , ALLOCATABLE :: IADSDP  ! dim = NSPMD+1
        integer, DIMENSION(:)   , ALLOCATABLE :: IADRCP  ! dim = NSPMD+1
        integer, dimension(:,:) , allocatable :: FR_NBCC ! dim = 2,NSPMD
        integer, DIMENSION(:)   , ALLOCATABLE :: FR_ELEM_S
        integer, DIMENSION(:)   , ALLOCATABLE :: FR_ELEM_R
! --------------------------------  
      END TYPE NLOCAL_STR_
      CONTAINS 
c=======================================================================      
c define routine GET_LEMAX to automatically determine the maximal element length
c======================================================================= 
      !||====================================================================
      !||    get_lemax             ../common_source/modules/nlocal_reg_mod.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_fail_gurson   ../starter/source/materials/fail/gurson/hm_read_fail_gurson.F
      !||    hm_read_nonlocal      ../starter/source/materials/nonlocal/hm_read_nonlocal.F
      !||    law104_upd            ../starter/source/materials/mat/mat104/law104_upd.F
      !||====================================================================
        SUBROUTINE GET_LEMAX(LE_MAX,NLOC_LENGTH)
#include      "implicit_f.inc"
          my_real, INTENT(IN)  :: NLOC_LENGTH
          my_real, INTENT(OUT) :: LE_MAX
          LE_MAX = SQRT(PI)*NLOC_LENGTH/THREE
        END SUBROUTINE GET_LEMAX
c=======================================================================      
c define routine GET_LENGTH to automatically determine the maximal element length
c======================================================================= 
      !||====================================================================
      !||    get_length            ../common_source/modules/nlocal_reg_mod.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_fail_gurson   ../starter/source/materials/fail/gurson/hm_read_fail_gurson.F
      !||    hm_read_nonlocal      ../starter/source/materials/nonlocal/hm_read_nonlocal.F
      !||====================================================================
        SUBROUTINE GET_LENGTH(NLOC_LENGTH,LE_MAX)
#include      "implicit_f.inc"
          my_real, INTENT(IN)  :: LE_MAX
          my_real, INTENT(OUT) :: NLOC_LENGTH
          NLOC_LENGTH = THREE*LE_MAX/SQRT(PI)
      END SUBROUTINE GET_LENGTH
c=======================================================================       
      END MODULE NLOCAL_REG_MOD
