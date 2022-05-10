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
Chd|  FAILWAVE_MOD                  modules/failwave_mod.F        
Chd|-- called by -----------
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        FAILWAVE_INIT                 starter/source/materials/fail/failwave_init.F
Chd|        HM_READ_FAIL                  starter/source/materials/fail/hm_read_fail.F
Chd|        LECTUR                        starter/source/starter/lectur.F
Chd|        READ_MATERIAL_MODELS          starter/source/materials/read_material_models.F
Chd|        W_FAILWAVE                    starter/source/restart/ddsplit/w_failwave.F
Chd|        C3FORC3                       engine/source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   engine/source/elements/xfem/c3forc3_crk.F
Chd|        CBAFORC3                      engine/source/elements/shell/coqueba/cbaforc3.F
Chd|        CDK6FORC3                     engine/source/elements/sh3n/coquedk6/cdk6forc3.F
Chd|        CDKFORC3                      engine/source/elements/sh3n/coquedk/cdkforc3.F
Chd|        CFORC3                        engine/source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    engine/source/elements/xfem/cforc3_crk.F
Chd|        CMAIN3                        engine/source/materials/mat_share/cmain3.F
Chd|        CMAIN3PINCH                   engine/source/elements/shell/coqueba/cmain3pinch.F
Chd|        CZFORC3                       engine/source/elements/shell/coquez/czforc3.F
Chd|        CZFORC3_CRK                   engine/source/elements/xfem/czforc3_crk.F
Chd|        FAIL_SETOFF_C                 engine/source/materials/fail/fail_setoff_c.F
Chd|        FAIL_SETOFF_NPG_C             engine/source/materials/fail/fail_setoff_npg_c.F
Chd|        FAIL_SETOFF_WIND_FRWAVE       engine/source/materials/fail/fail_setoff_wind_frwave.F
Chd|        FORINTC                       engine/source/elements/forintc.F
Chd|        MULAWC                        engine/source/materials/mat_share/mulawc.F
Chd|        READ_FAILWAVE                 engine/source/output/restart/read_failwave.F
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|        RESOL_INIT                    engine/source/engine/resol_init.F
Chd|        SET_FAILWAVE_NOD3             engine/source/materials/fail/failwave/set_failwave_nod3.F
Chd|        SET_FAILWAVE_NOD4             engine/source/materials/fail/failwave/set_failwave_nod4.F
Chd|        SET_FAILWAVE_SH3N             engine/source/materials/fail/failwave/upd_failwave_sh3n.F
Chd|        SET_FAILWAVE_SH4N             engine/source/materials/fail/failwave/upd_failwave_sh4n.F
Chd|        SPMD_EXCH_FAILWAVE            engine/source/mpi/output/spmd_exch_failwave.F
Chd|        SPMD_FAILWAVE_BOUNDARIES      engine/source/mpi/output/spmd_exch_failwave.F
Chd|        UPDATE_FAILWAVE               engine/source/materials/fail/failwave/update_failwave.F
Chd|        USERMAT_SHELL                 engine/source/materials/mat_share/usermat_shell.F
Chd|        W_FAILWAVE                    engine/source/output/restart/w_failwave.F
Chd|        RESTMOD                       engine/share/modules/restart_mod.F
Chd|-- calls ---------------
Chd|====================================================================
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
