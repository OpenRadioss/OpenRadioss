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
      !||    seatbelt_mod            ../common_source/modules/seatbelt_mod.F
      !||--- called by ------------------------------------------------------
      !||    c_seatbelts             ../starter/source/restart/ddsplit/c_seatbelts.F
      !||    create_seatbelt         ../starter/source/tools/seatbelts/create_seatbelt.F
      !||    ddsplit                 ../starter/source/restart/ddsplit/ddsplit.F
      !||    hist2                   ../engine/source/output/th/hist2.F
      !||    hm_read_retractor       ../starter/source/tools/seatbelts/hm_read_retractor.F
      !||    hm_read_slipring        ../starter/source/tools/seatbelts/hm_read_slipring.F
      !||    ini_seatbelt            ../starter/source/tools/seatbelts/ini_seatbelt.F
      !||    init_seatbelt_rbodies   ../starter/source/tools/seatbelts/init_seatbelt_rbodies.F90
      !||    kine_seatbelt_force     ../engine/source/tools/seatbelts/kine_seatbelt_force.F
      !||    kine_seatbelt_vel       ../engine/source/tools/seatbelts/kine_seatbelt_vel.F
      !||    lectur                  ../engine/source/input/lectur.F
      !||    material_flow           ../engine/source/tools/seatbelts/material_flow.F
      !||    new_seatbelt            ../starter/source/tools/seatbelts/new_seatbelt.F
      !||    r23l114def3             ../engine/source/elements/spring/r23l114def3.F
      !||    rdresb                  ../engine/source/output/restart/rdresb.F
      !||    resol                   ../engine/source/engine/resol.F
      !||    restalloc               ../engine/source/output/restart/arralloc.F
      !||    rgrhead                 ../starter/source/elements/spring/rgrhead.F
      !||    rgrtails                ../starter/source/elements/spring/rgrtails.F
      !||    rinit3                  ../starter/source/elements/spring/rinit3.F
      !||    spmd_collect_seatbelt   ../engine/source/mpi/output/spmd_collect_seatbelt.F
      !||    spmd_exch_a_seatbelt    ../engine/source/mpi/seatbelts/spmd_exch_a_seatbelt.F
      !||    st_qaprint_seatbelts    ../starter/source/output/qaprint/st_qaprint_seatbelts.F
      !||    update_slipring         ../engine/source/tools/seatbelts/update_slipring.F
      !||    w_seatbelts             ../starter/source/restart/ddsplit/w_seatbelts.F
      !||    wrrestp                 ../engine/source/output/restart/wrrestp.F
      !||====================================================================
      MODULE SEATBELT_MOD
C----------------------------------------------------------------------- 
#include "my_real.inc"

c=======================================================================      
c define structure for slipring
c=======================================================================      
c 
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
C
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
C
      TYPE(SLIPRING_STRUCT),ALLOCATABLE,DIMENSION(:) :: SLIPRING

c=======================================================================      
c define structure for retractor
c=======================================================================      
c 
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
        INTEGER  LOCKED_FREEZE
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
        my_real  LOCK_PULL_SAV
        my_real  RINGSLIP
        my_real  PRETENS_TIME
        my_real  PRETENS_PULL
        my_real  PRETENS_PULLMAX
        my_real  RET_FORCE
      END TYPE RETRACTOR_STRUCT

      TYPE(RETRACTOR_STRUCT),ALLOCATABLE,DIMENSION(:) :: RETRACTOR

c=======================================================================      
c define structure for seatbelts (starter only)
c=======================================================================      
c 
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

c======================================================================= 
C define structure for remote anchor node
c======================================================================= 
      TYPE SEATBELT_REMOTE_NODES_STRUCT
        INTEGER, DIMENSION(:)   , ALLOCATABLE :: ADD_PROC
        INTEGER, DIMENSION(:)   , ALLOCATABLE :: NODE
      END TYPE SEATBELT_REMOTE_NODES_STRUCT

      TYPE(SEATBELT_REMOTE_NODES_STRUCT) ANCHOR_REMOTE,ANCHOR_REMOTE_SEND
C
      INTEGER NSEATBELT_TH_PROC
C
      TYPE SEATBELT_TH_EXCH_STRUCT
        INTEGER ID_PROC
        INTEGER ADD_PROC
        INTEGER NSLIPRING
        INTEGER NRETRACTOR
      END TYPE SEATBELT_TH_EXCH_STRUCT
C
      TYPE(SEATBELT_TH_EXCH_STRUCT),ALLOCATABLE,DIMENSION(:) :: SEATBELT_TH_EXCH
C
      my_real  , DIMENSION(:,:), ALLOCATABLE:: TH_SLIPRING,TH_RETRACTOR
C
c---------------
      END MODULE  SEATBELT_MOD
