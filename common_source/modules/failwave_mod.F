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
      !||    failwave_mod               ../common_source/modules/failwave_mod.F
      !||--- called by ------------------------------------------------------
      !||    c3forc3                    ../engine/source/elements/sh3n/coque3n/c3forc3.F
      !||    c3forc3_crk                ../engine/source/elements/xfem/c3forc3_crk.F
      !||    cbaforc3                   ../engine/source/elements/shell/coqueba/cbaforc3.F
      !||    cdk6forc3                  ../engine/source/elements/sh3n/coquedk6/cdk6forc3.F
      !||    cdkforc3                   ../engine/source/elements/sh3n/coquedk/cdkforc3.F
      !||    cforc3                     ../engine/source/elements/shell/coque/cforc3.F
      !||    cforc3_crk                 ../engine/source/elements/xfem/cforc3_crk.F
      !||    cmain3                     ../engine/source/materials/mat_share/cmain3.F
      !||    cmain3pinch                ../engine/source/elements/shell/coqueba/cmain3pinch.F
      !||    czforc3                    ../engine/source/elements/shell/coquez/czforc3.F
      !||    czforc3_crk                ../engine/source/elements/xfem/czforc3_crk.F
      !||    ddsplit                    ../starter/source/restart/ddsplit/ddsplit.F
      !||    fail_setoff_c              ../engine/source/materials/fail/fail_setoff_c.F
      !||    fail_setoff_npg_c          ../engine/source/materials/fail/fail_setoff_npg_c.F
      !||    fail_setoff_wind_frwave    ../engine/source/materials/fail/fail_setoff_wind_frwave.F
      !||    failwave_init              ../starter/source/materials/fail/failwave_init.F
      !||    forintc                    ../engine/source/elements/forintc.F
      !||    hm_read_fail               ../starter/source/materials/fail/hm_read_fail.F
      !||    lectur                     ../engine/source/input/lectur.F
      !||    read_failwave              ../engine/source/output/restart/read_failwave.F
      !||    read_material_models       ../starter/source/materials/read_material_models.F
      !||    resol                      ../engine/source/engine/resol.F
      !||    resol_init                 ../engine/source/engine/resol_init.F
      !||    restmod                    ../engine/share/modules/restart_mod.F
      !||    set_failwave_nod3          ../engine/source/materials/fail/failwave/set_failwave_nod3.F
      !||    set_failwave_nod4          ../engine/source/materials/fail/failwave/set_failwave_nod4.F
      !||    set_failwave_sh3n          ../engine/source/materials/fail/failwave/upd_failwave_sh3n.F
      !||    set_failwave_sh4n          ../engine/source/materials/fail/failwave/upd_failwave_sh4n.F
      !||    spmd_exch_failwave         ../engine/source/mpi/output/spmd_exch_failwave.F
      !||    spmd_failwave_boundaries   ../engine/source/mpi/output/spmd_exch_failwave.F
      !||    update_failwave            ../engine/source/materials/fail/failwave/update_failwave.F
      !||    usermat_shell              ../engine/source/materials/mat_share/usermat_shell.F
      !||    w_failwave                 ../engine/source/output/restart/w_failwave.F
      !||====================================================================
      MODULE FAILWAVE_MOD
C-----------------------------------------------
#include      "my_real.inc"
C=======================================================================
c Structure for failure propagation info between elements
c---------------------------------------------
c
      TYPE FAILWAVE_STR_   
           INTEGER :: WAVE_MOD                 ! activation / mode switch flag   
           INTEGER :: NNOD                     ! number of nodes using frontwave
           INTEGER :: NDDL                     ! number of degrees of freedom per node
           INTEGER :: SIZE                     ! number of stack levels per node
           INTEGER, DIMENSION(:)     ,ALLOCATABLE :: INDX    ! (NNOD)   nodal index table
           INTEGER, DIMENSION(:)     ,ALLOCATABLE :: IDXI    ! (NUMNOD) inversed nodal index table
           INTEGER, DIMENSION(:)     ,ALLOCATABLE :: MAXLEV  ! max number of used levels
           INTEGER, DIMENSION(:)     ,ALLOCATABLE :: MAXLEV_STACK
           INTEGER, DIMENSION(:,:,:) ,ALLOCATABLE :: FWAVE_NOD        ! nodal table
           INTEGER, DIMENSION(:,:,:) ,ALLOCATABLE :: FWAVE_NOD_STACK  ! nodal table
C
           INTEGER, DIMENSION(:)     ,ALLOCATABLE :: FWAVE_IAD
           INTEGER, DIMENSION(:)     ,ALLOCATABLE :: FWAVE_FR          
      END TYPE FAILWAVE_STR_
c
c----------------------------
c     starter allocation : FWAVE_NOD(NDDL,NUMNOD,SIZE)
c                          MAXLEV(NUMNOD)
c     engine allocation  : FWAVE_NOD(NDDL,NNOD,SIZE)
c                          FWAVE_NOD_STACK(NDDL,NNOD,SIZE)   
c                          MAXLEV(NNOD)
c                          MAXLEV_STACK(NNOD)
c     index tables :
c            global node numbers    = INDX(NNOD)
c            frontwave node numbers = IDXI(NUMNNOD)
c     SPMD Neighbour structures
c            FWAVE_IAD : #nodes / SPMD domain to exchange
c            FWAVE_FR  : boundary nodes index
c----------------------------
      END MODULE FAILWAVE_MOD
