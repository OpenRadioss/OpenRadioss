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
!hd|  SEATBELT_MOD                  modules/seatbelt_mod.F
!hd|-- called by -----------
!hd|        CREATE_SEATBELT               starter/source/tools/seatbelts/create_seatbelt.F
!hd|        C_SEATBELTS                   starter/source/restart/ddsplit/c_seatbelts.F
!hd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
!hd|        HM_READ_RETRACTOR             starter/source/tools/seatbelts/hm_read_retractor.F
!hd|        HM_READ_SLIPRING              starter/source/tools/seatbelts/hm_read_slipring.F
!hd|        INI_SEATBELT                  starter/source/tools/seatbelts/ini_seatbelt.F
!hd|        LECTUR                        starter/source/starter/lectur.F
!hd|        NEW_SEATBELT                  starter/source/tools/seatbelts/new_seatbelt.F
!hd|        RGRHEAD                       starter/source/elements/spring/rgrhead.F
!hd|        RGRTAILS                      starter/source/elements/spring/rgrtails.F
!hd|        RINIT3                        starter/source/elements/spring/rinit3.F
!hd|        ST_QAPRINT_SEATBELTS          starter/source/output/qaprint/st_qaprint_seatbelts.F
!hd|        W_SEATBELTS                   starter/source/restart/ddsplit/w_seatbelts.F
!hd|        HIST2                         engine/source/output/th/hist2.F
!hd|        KINE_SEATBELT_FORCE           engine/source/tools/seatbelts/kine_seatbelt_force.F
!hd|        KINE_SEATBELT_VEL             engine/source/tools/seatbelts/kine_seatbelt_vel.F
!hd|        MATERIAL_FLOW                 engine/source/tools/seatbelts/material_flow.F
!hd|        R23L114DEF3                   engine/source/elements/spring/r23l114def3.F
!hd|        RDRESB                        engine/source/output/restart/rdresb.F
!hd|        RESOL                         engine/source/engine/resol.F
!hd|        RESTALLOC                     engine/source/output/restart/arralloc.F
!hd|        SPMD_COLLECT_SEATBELT         engine/source/mpi/output/spmd_collect_seatbelt.F
!hd|        SPMD_EXCH_A_SEATBELT          engine/source/mpi/seatbelts/spmd_exch_a_seatbelt.F
!hd|        UPDATE_SLIPRING               engine/source/tools/seatbelts/update_slipring.F
!hd|        WRRESTP                       engine/source/output/restart/wrrestp.F
!hd|-- calls ---------------
!hd|====================================================================
      MODULE SEATBELT_MOD
!-----------------------------------------------------------------------
#include "my_real.inc"

!=======================================================================
! define structure for slipring
!=======================================================================
!
        TYPE SLIPRING_FRAM_STRUCT
          INTEGER  UPDATE
          INTEGER  ANCHOR_NODE
          INTEGER  NODE(3)
          INTEGER  ORIENTATION_NODE
          INTEGER  NODE_NEXT(3)
          INTEGER  NODE2_PREV
          INTEGER  STRAND_DIRECTION(2)
          INTEGER  LOCKED
          INTEGER  N_REMOTE_PROC
          my_real  VECTOR(6)
          my_real  ORIENTATION_ANGLE
          my_real  MATERIAL_FLOW
          my_real  DFS
          my_real  RESIDUAL_LENGTH(2)
          my_real  CURRENT_LENGTH(2)
          my_real  RINGSLIP
          my_real  BETA
          my_real  GAMMA
          my_real  SLIP_FORCE(3)
          my_real  PREV_REF_LENGTH
          my_real  INTVAR_STR1(8)
          my_real  INTVAR_STR2(8)
        END TYPE SLIPRING_FRAM_STRUCT
!
        TYPE SLIPRING_STRUCT
          INTEGER  ID
          INTEGER  IDG
          INTEGER  NFRAM
          INTEGER  IFUNC(4)
          INTEGER  SENSID
          INTEGER  FL_FLAG
          INTEGER  RBODY
          my_real  DC
          my_real  A
          my_real  FRIC
          my_real  FAC_D(3)
          my_real  FRICS
          my_real  FAC_S(3)
          TYPE(SLIPRING_FRAM_STRUCT),ALLOCATABLE,DIMENSION(:) :: FRAM
        END TYPE SLIPRING_STRUCT
!
        TYPE(SLIPRING_STRUCT),ALLOCATABLE,DIMENSION(:) :: SLIPRING

!=======================================================================
! define structure for retractor
!=======================================================================
!
        TYPE RETRACTOR_STRUCT
          INTEGER  ID
          INTEGER  IDG
          INTEGER  UPDATE
          INTEGER  ANCHOR_NODE
          INTEGER  NODE(2)
          INTEGER  NODE_NEXT(2)
          INTEGER  STRAND_DIRECTION
          INTEGER  IFUNC(3)
          INTEGER  ISENS(2)
          INTEGER  TENS_TYP
          INTEGER  LOCKED
          INTEGER  PRETENS_ACTIV
          INTEGER  INACTI_NNOD
          INTEGER  INACTI_NNOD_MAX
          INTEGER, DIMENSION(:)   , ALLOCATABLE :: INACTI_NODE
          INTEGER  N_REMOTE_PROC
          my_real  VECTOR(3)
          my_real  ELEMENT_SIZE
          my_real  FORCE
          my_real  MATERIAL_FLOW
          my_real  RESIDUAL_LENGTH
          my_real  FAC(4)
          my_real  PULLOUT
          my_real  UNLOCK_FORCE
          my_real  LOCK_PULL
          my_real  LOCK_OFFSET
          my_real  LOCK_YIELD_FORCE
          my_real  RINGSLIP
          my_real  PRETENS_TIME
          my_real  PRETENS_PULL
          my_real  PRETENS_PULLMAX
          my_real  RET_FORCE
        END TYPE RETRACTOR_STRUCT

        TYPE(RETRACTOR_STRUCT),ALLOCATABLE,DIMENSION(:) :: RETRACTOR

!=======================================================================
! define structure for seatbelts (starter only)
!=======================================================================
!
        TYPE SEATBELT_STRUCT
          INTEGER  NFRAM
          INTEGER  NSPRING
          INTEGER  NSHELL
          INTEGER  NNOD
          INTEGER, DIMENSION(:)   , ALLOCATABLE :: SPRING
          my_real  SECTION
          my_real  ELEM_SIZE
        END TYPE SEATBELT_STRUCT

        TYPE(SEATBELT_STRUCT),ALLOCATABLE,DIMENSION(:) :: SEATBELT_TAB

!=======================================================================
! define structure for remote anchor node
!=======================================================================
        TYPE SEATBELT_REMOTE_NODES_STRUCT
          INTEGER, DIMENSION(:)   , ALLOCATABLE :: ADD_PROC
          INTEGER, DIMENSION(:)   , ALLOCATABLE :: NODE
        END TYPE SEATBELT_REMOTE_NODES_STRUCT

        TYPE(SEATBELT_REMOTE_NODES_STRUCT) ANCHOR_REMOTE,ANCHOR_REMOTE_SEND
!
        INTEGER NSEATBELT_TH_PROC
!
        TYPE SEATBELT_TH_EXCH_STRUCT
          INTEGER ID_PROC
          INTEGER ADD_PROC
          INTEGER NSLIPRING
          INTEGER NRETRACTOR
        END TYPE SEATBELT_TH_EXCH_STRUCT
!
        TYPE(SEATBELT_TH_EXCH_STRUCT),ALLOCATABLE,DIMENSION(:) :: SEATBELT_TH_EXCH
!
        my_real  , DIMENSION(:,:), ALLOCATABLE:: TH_SLIPRING,TH_RETRACTOR
!
!---------------
      END MODULE  SEATBELT_MOD
