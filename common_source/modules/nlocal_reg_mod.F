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
Chd|  NLOCAL_REG_MOD                modules/nlocal_reg_mod.F      
Chd|-- called by -----------
Chd|        C3FINT_REG_INI                starter/source/elements/sh3n/coque3n/c3fint_reg_ini.F
Chd|        C3INIT3                       starter/source/elements/sh3n/coque3n/c3init3.F
Chd|        CBAFINT_REG_INI               starter/source/elements/shell/coqueba/cbafint_reg_ini.F
Chd|        CBAINIT3                      starter/source/elements/shell/coqueba/cbainit3.F
Chd|        CDKFINT_REG_INI               starter/source/elements/sh3n/coquedk/cdkfint_reg_ini.F
Chd|        CDKINIT3                      starter/source/elements/sh3n/coquedk/cdkinit3.F
Chd|        CFINT_REG_INI                 starter/source/elements/shell/coque/cfint_reg_ini.F
Chd|        CINIT3                        starter/source/elements/shell/coque/cinit3.F
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        HM_READ_FAIL                  starter/source/materials/fail/hm_read_fail.F
Chd|        HM_READ_FAIL_GURSON           starter/source/materials/fail/gurson/hm_read_fail_gurson.F
Chd|        HM_READ_NONLOCAL              starter/source/materials/nonlocal/hm_read_nonlocal.F
Chd|        INITIA                        starter/source/elements/initia/initia.F
Chd|        LAW104_UPD                    starter/source/materials/mat/mat104/law104_upd.F
Chd|        LECTUR                        starter/source/starter/lectur.F
Chd|        NLOCAL_INIT_STA               starter/source/materials/fail/nlocal_init_sta.F
Chd|        NLOC_DMG_INIT                 starter/source/materials/fail/nloc_dmg_init.F
Chd|        READ_MATERIAL_MODELS          starter/source/materials/read_material_models.F
Chd|        UPDMAT                        starter/source/materials/updmat.F
Chd|        WRITE_NLOC_STRUCT             starter/source/restart/ddsplit/write_nloc_struct.F
Chd|        ALEMAIN                       engine/source/ale/alemain.F   
Chd|        BFORC2                        engine/source/ale/bimat/bforc2.F
Chd|        C3FINT_REG                    engine/source/elements/sh3n/coque3n/c3fint_reg.F
Chd|        C3FORC3                       engine/source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   engine/source/elements/xfem/c3forc3_crk.F
Chd|        CBAFINT_REG                   engine/source/elements/shell/coqueba/cbafint_reg.F
Chd|        CBAFORC3                      engine/source/elements/shell/coqueba/cbaforc3.F
Chd|        CBAVARNL                      engine/source/elements/shell/coqueba/cbavarnl.F
Chd|        CDK6FINT_REG                  engine/source/elements/sh3n/coquedk6/cdk6fint_reg.F
Chd|        CDK6FORC3                     engine/source/elements/sh3n/coquedk6/cdk6forc3.F
Chd|        CDKFINT_REG                   engine/source/elements/sh3n/coquedk/cdkfint_reg.F
Chd|        CDKFORC3                      engine/source/elements/sh3n/coquedk/cdkforc3.F
Chd|        CFINT_REG                     engine/source/elements/shell/coque/cfint_reg.F
Chd|        CFORC3                        engine/source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    engine/source/elements/xfem/cforc3_crk.F
Chd|        CMAIN3                        engine/source/materials/mat_share/cmain3.F
Chd|        CZFORC3                       engine/source/elements/shell/coquez/czforc3.F
Chd|        CZFORC3_CRK                   engine/source/elements/xfem/czforc3_crk.F
Chd|        DTCBA_REG                     engine/source/elements/shell/coqueba/dtcba_reg.F
Chd|        DTCDK_REG                     engine/source/elements/sh3n/coquedk/dtcdk_reg.F
Chd|        FORINT                        engine/source/elements/forint.F
Chd|        FORINTC                       engine/source/elements/forintc.F
Chd|        FORINTP                       engine/source/elements/forintp.F
Chd|        IG3DUFORC3                    engine/source/elements/ige3d/ig3duforc3.F
Chd|        MMAIN                         engine/source/materials/mat_share/mmain.F
Chd|        MULAW                         engine/source/materials/mat_share/mulaw.F
Chd|        MULAWC                        engine/source/materials/mat_share/mulawc.F
Chd|        NLOCAL_ACC                    engine/source/assembly/nlocal_acc.F
Chd|        NLOCAL_DTNODA                 engine/source/time_step/nlocal_dtnoda.F
Chd|        NLOCAL_INCR                   engine/source/assembly/nlocal_incr.F
Chd|        NLOCAL_VEL                    engine/source/assembly/nlocal_vel.F
Chd|        Q4FORC2                       engine/source/elements/solid_2d/quad4/q4forc2.F
Chd|        QFORC2                        engine/source/elements/solid_2d/quad/qforc2.F
Chd|        RDRESB                        engine/source/output/restart/rdresb.F
Chd|        READ_NLOC_STRUCT              engine/source/output/restart/read_nloc_struct.F
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|        RESOL_INIT                    engine/source/engine/resol_init.F
Chd|        S10FORC3                      engine/source/elements/solid/solide10/s10forc3.F
Chd|        S16FORC3                      engine/source/elements/thickshell/solide16/s16forc3.F
Chd|        S20FORC3                      engine/source/elements/solid/solide20/s20forc3.F
Chd|        S4FINT_REG                    engine/source/elements/solid/solide4/s4fint_reg.F
Chd|        S4FORC3                       engine/source/elements/solid/solide4/s4forc3.F
Chd|        S6CFORC3                      engine/source/elements/thickshell/solide6c/s6cforc3.F
Chd|        S8CFORC3                      engine/source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      engine/source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      engine/source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFINT_REG                   engine/source/elements/solid/solide8z/s8zfint_reg.F
Chd|        S8ZFORC3                      engine/source/elements/solid/solide8z/s8zforc3.F
Chd|        SCFORC3                       engine/source/elements/thickshell/solidec/scforc3.F
Chd|        SFINT_REG                     engine/source/elements/solid/solide/sfint_reg.F
Chd|        SFORC3                        engine/source/elements/solid/solide/sforc3.F
Chd|        SPMD_COLLECT_NLOCAL           engine/source/mpi/output/spmd_collect_nlocal.F
Chd|        SPMD_EXCH_SUB_POFF            engine/source/mpi/spmd_exch_sub.F
Chd|        SPMD_EXCH_SUB_PON             engine/source/mpi/spmd_exch_sub.F
Chd|        SPMD_SUB_BOUNDARIES           engine/source/mpi/spmd_exch_sub.F
Chd|        SPSTRES                       engine/source/elements/sph/spstres.F
Chd|        SZFORC3                       engine/source/elements/solid/solidez/szforc3.F
Chd|        USERMAT_SHELL                 engine/source/materials/mat_share/usermat_shell.F
Chd|        USERMAT_SOLID                 engine/source/materials/mat_share/usermat_solid.F
Chd|        WRITE_NLOC_STRUCT             engine/source/output/restart/write_nloc_struct.F
Chd|        RESTMOD                       engine/share/modules/restart_mod.F
Chd|-- calls ---------------
Chd|====================================================================
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
Chd|====================================================================
Chd|  GET_LEMAX                     modules/nlocal_reg_mod.F      
Chd|-- called by -----------
Chd|        HM_READ_FAIL_GURSON           starter/source/materials/fail/gurson/hm_read_fail_gurson.F
Chd|        HM_READ_NONLOCAL              starter/source/materials/nonlocal/hm_read_nonlocal.F
Chd|        LAW104_UPD                    starter/source/materials/mat/mat104/law104_upd.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE GET_LEMAX(LE_MAX,NLOC_LENGTH)
#include      "implicit_f.inc"
          my_real, INTENT(IN)  :: NLOC_LENGTH
          my_real, INTENT(OUT) :: LE_MAX
          LE_MAX = SQRT(PI)*NLOC_LENGTH/THREE
        END SUBROUTINE GET_LEMAX
c=======================================================================      
c define routine GET_LENGTH to automatically determine the maximal element length
c======================================================================= 
Chd|====================================================================
Chd|  GET_LENGTH                    modules/nlocal_reg_mod.F      
Chd|-- called by -----------
Chd|        HM_READ_FAIL_GURSON           starter/source/materials/fail/gurson/hm_read_fail_gurson.F
Chd|        HM_READ_NONLOCAL              starter/source/materials/nonlocal/hm_read_nonlocal.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE GET_LENGTH(NLOC_LENGTH,LE_MAX)
#include      "implicit_f.inc"
          my_real, INTENT(IN)  :: LE_MAX
          my_real, INTENT(OUT) :: NLOC_LENGTH
          NLOC_LENGTH = THREE*LE_MAX/SQRT(PI)
      END SUBROUTINE GET_LENGTH
c=======================================================================       
      END MODULE NLOCAL_REG_MOD
