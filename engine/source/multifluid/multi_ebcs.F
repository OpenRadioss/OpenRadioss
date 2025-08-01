Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||    multi_ebcs             ../engine/source/multifluid/multi_ebcs.F
!||--- called by ------------------------------------------------------
!||    multi_timeevolution    ../engine/source/multifluid/multi_timeevolution.F
!||--- calls      -----------------------------------------------------
!||    multi_inlet_ebcs       ../engine/source/multifluid/multi_inlet_ebcs.F
!||    multi_nrf_ebcs         ../engine/source/multifluid/multi_nrf_ebcs.F
!||    multi_propergol_ebcs   ../engine/source/multifluid/multi_propergol_ebcs.F90
!||--- uses       -----------------------------------------------------
!||    ebcs_mod               ../common_source/modules/boundary_conditions/ebcs_mod.F90
!||    matparam_def_mod       ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    multi_fvm_mod          ../common_source/modules/ale/multi_fvm_mod.F90
!||    multi_inlet_ebcs_mod   ../engine/source/multifluid/multi_inlet_ebcs.F
!||    th_surf_mod            ../common_source/modules/interfaces/th_surf_mod.F
!||====================================================================
      SUBROUTINE MULTI_EBCS(ITASK, MULTI_FVM, IXS, IXQ, IXTG, XGRID, WGRID, IPM, PM,
     .     FUNC_VALUE, EBCS_TAB,NPF,TF,FSAVSURF,NSURF,TIMESTEP,
     .     NUMELS, NUMELQ, NUMELTG, NUMNOD, NCYCLE, NUMMAT, MATPARAM)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE MULTI_FVM_MOD
      USE EBCS_MOD
      USE TH_SURF_MOD , only : TH_SURF_NUM_CHANNEL
      USE MATPARAM_DEF_MOD , ONLY : MATPARAM_STRUCT_
      USE MULTI_INLET_EBCS_MOD , ONLY : MULTI_INLET_EBCS
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
! NEBCS
! NPEBC, NIXS
#include      "param_c.inc"
! LISURF1
! SNPC,STF
#include      "tabsiz_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NUMMAT !< array size
      TYPE(MATPARAM_STRUCT_), DIMENSION(NUMMAT), INTENT(IN) :: MATPARAM !material data structure
      INTEGER,INTENT(IN) :: NCYCLE
      INTEGER,INTENT(IN) :: NSURF
      INTEGER,INTENT(IN) :: NUMELS, NUMELQ, NUMELTG, NUMNOD
      my_real,INTENT(INOUT) :: FSAVSURF(TH_SURF_NUM_CHANNEL,NSURF)
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      INTEGER, INTENT(IN) :: ITASK
      INTEGER, INTENT(IN) :: IXS(NIXS, *), IXQ(NIXQ, *), IXTG(NIXTG, *)
      my_real, INTENT(IN) :: XGRID(3, *), WGRID(3, *)
      INTEGER, INTENT(IN) :: IPM(NPROPMI, *)
      my_real, INTENT(IN) :: PM(NPROPM, *), FUNC_VALUE(*)
      TYPE(t_ebcs_tab), TARGET, INTENT(IN) :: EBCS_TAB
      INTEGER, INTENT(IN) :: NPF(SNPC)
      my_real, INTENT(IN) :: TF(STF), TIMESTEP
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I
      INTEGER :: TYPE, NELEM
      CLASS (T_EBCS), POINTER :: EBCS
C-----------------------------------------------
C     B e g i n n i n g   o f   s u b r o u t i n e
C-----------------------------------------------
      DO I = 1, EBCS_TAB%nebcs_fvm
         EBCS => EBCS_TAB%tab(i)%poly
         TYPE = EBCS%type
         NELEM = EBCS%nb_elem
         SELECT TYPE (twf => EBCS)
            TYPE IS (t_ebcs_inlet)
C     Inlet, outlet boundary condition
              CALL MULTI_INLET_EBCS(ITASK, I, MULTI_FVM, NELEM,
     .           twf%ielem, twf%iface, twf%fvm_inlet_data,
     .           IXS, IXQ, IXTG, XGRID, WGRID, IPM, PM, FUNC_VALUE,
     .           twf%surf_id,NPF,TF,FSAVSURF, TIMESTEP, MATPARAM)
            TYPE IS(t_ebcs_fluxout)
              CALL MULTI_INLET_EBCS(ITASK, I, MULTI_FVM, NELEM,
     .           twf%ielem, twf%iface, twf%fvm_inlet_data,
     .           IXS, IXQ, IXTG, XGRID, WGRID, IPM, PM, FUNC_VALUE,
     .           twf%surf_id,NPF,TF,FSAVSURF,TIMESTEP, MATPARAM)
            TYPE IS(t_ebcs_nrf)
              CALL MULTI_NRF_EBCS(ITASK, I, MULTI_FVM, NELEM,
     .           twf%ielem, twf%iface, twf%fvm_inlet_data,
     .           IXS, IXQ, IXTG, XGRID, WGRID, IPM, PM, FUNC_VALUE,
     .           twf,NPF,TF,FSAVSURF,TIMESTEP, MATPARAM)
            TYPE IS(t_ebcs_propergol)
              CALL MULTI_PROPERGOL_EBCS(ITASK, I, MULTI_FVM, NELEM,
     .           twf%ielem, twf%iface, twf%fvm_inlet_data,
     .           IXS, IXQ, IXTG, XGRID, WGRID, IPM, PM, FUNC_VALUE,
     .           twf,NPF,TF,FSAVSURF,TIMESTEP,NIXS, NIXQ, NIXTG, NPROPMI, NPROPM, NSURF, STF, SNPC,
     .           NUMELS, NUMELQ, NUMELTG, NUMNOD, NCYCLE, NUMMAT ,MATPARAM)
            CLASS DEFAULT
         END SELECT
      ENDDO
C-----------------------------------------------
C     E n d   o f   s u b r o u t i n e 
C-----------------------------------------------
      END SUBROUTINE MULTI_EBCS
