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
!hd|====================================================================
!hd|  FAILWAVE_MOD                  modules/failwave_mod.F
!hd|-- called by -----------
!hd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
!hd|        FAILWAVE_INIT                 starter/source/materials/fail/failwave_init.F
!hd|        HM_READ_FAIL                  starter/source/materials/fail/hm_read_fail.F
!hd|        LECTUR                        starter/source/starter/lectur.F
!hd|        READ_MATERIAL_MODELS          starter/source/materials/read_material_models.F
!hd|        W_FAILWAVE                    starter/source/restart/ddsplit/w_failwave.F
!hd|        C3FORC3                       engine/source/elements/sh3n/coque3n/c3forc3.F
!hd|        C3FORC3_CRK                   engine/source/elements/xfem/c3forc3_crk.F
!hd|        CBAFORC3                      engine/source/elements/shell/coqueba/cbaforc3.F
!hd|        CDK6FORC3                     engine/source/elements/sh3n/coquedk6/cdk6forc3.F
!hd|        CDKFORC3                      engine/source/elements/sh3n/coquedk/cdkforc3.F
!hd|        CFORC3                        engine/source/elements/shell/coque/cforc3.F
!hd|        CFORC3_CRK                    engine/source/elements/xfem/cforc3_crk.F
!hd|        CMAIN3                        engine/source/materials/mat_share/cmain3.F
!hd|        CMAIN3PINCH                   engine/source/elements/shell/coqueba/cmain3pinch.F
!hd|        CZFORC3                       engine/source/elements/shell/coquez/czforc3.F
!hd|        CZFORC3_CRK                   engine/source/elements/xfem/czforc3_crk.F
!hd|        FAIL_SETOFF_C                 engine/source/materials/fail/fail_setoff_c.F
!hd|        FAIL_SETOFF_NPG_C             engine/source/materials/fail/fail_setoff_npg_c.F
!hd|        FAIL_SETOFF_WIND_FRWAVE       engine/source/materials/fail/fail_setoff_wind_frwave.F
!hd|        FORINTC                       engine/source/elements/forintc.F
!hd|        MULAWC                        engine/source/materials/mat_share/mulawc.F
!hd|        READ_FAILWAVE                 engine/source/output/restart/read_failwave.F
!hd|        RESOL                         engine/source/engine/resol.F
!hd|        RESOL_INIT                    engine/source/engine/resol_init.F
!hd|        SET_FAILWAVE_NOD3             engine/source/materials/fail/failwave/set_failwave_nod3.F
!hd|        SET_FAILWAVE_NOD4             engine/source/materials/fail/failwave/set_failwave_nod4.F
!hd|        SET_FAILWAVE_SH3N             engine/source/materials/fail/failwave/upd_failwave_sh3n.F
!hd|        SET_FAILWAVE_SH4N             engine/source/materials/fail/failwave/upd_failwave_sh4n.F
!hd|        SPMD_EXCH_FAILWAVE            engine/source/mpi/output/spmd_exch_failwave.F
!hd|        SPMD_FAILWAVE_BOUNDARIES      engine/source/mpi/output/spmd_exch_failwave.F
!hd|        UPDATE_FAILWAVE               engine/source/materials/fail/failwave/update_failwave.F
!hd|        USERMAT_SHELL                 engine/source/materials/mat_share/usermat_shell.F
!hd|        W_FAILWAVE                    engine/source/output/restart/w_failwave.F
!hd|        RESTMOD                       engine/share/modules/restart_mod.F
!hd|-- calls ---------------
!hd|====================================================================
      MODULE FAILWAVE_MOD
!-----------------------------------------------
#include      "my_real.inc"
!=======================================================================
! Structure for failure propagation info between elements
!---------------------------------------------
!
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
!
          INTEGER, DIMENSION(:)     ,ALLOCATABLE :: FWAVE_IAD
          INTEGER, DIMENSION(:)     ,ALLOCATABLE :: FWAVE_FR
        END TYPE FAILWAVE_STR_
!
!----------------------------
!     starter allocation : FWAVE_NOD(NDDL,NUMNOD,SIZE)
!                          MAXLEV(NUMNOD)
!     engine allocation  : FWAVE_NOD(NDDL,NNOD,SIZE)
!                          FWAVE_NOD_STACK(NDDL,NNOD,SIZE)
!                          MAXLEV(NNOD)
!                          MAXLEV_STACK(NNOD)
!     index tables :
!            global node numbers    = INDX(NNOD)
!            frontwave node numbers = IDXI(NUMNNOD)
!     SPMD Neighbour structures
!            FWAVE_IAD : #nodes / SPMD domain to exchange
!            FWAVE_FR  : boundary nodes index
!----------------------------
      END MODULE FAILWAVE_MOD
