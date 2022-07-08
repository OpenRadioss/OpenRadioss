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
Chd|  MULTI_EBCS                    source/multifluid/multi_ebcs.F
Chd|-- called by -----------
Chd|        MULTI_TIMEEVOLUTION           source/multifluid/multi_timeevolution.F
Chd|-- calls ---------------
Chd|        MULTI_INLET_EBCS              source/multifluid/multi_inlet_ebcs.F
Chd|        MULTI_NRF_EBCS                source/multifluid/multi_nrf_ebcs.F
Chd|        EBCS_MOD                      ../common_source/modules/ebcs_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE MULTI_EBCS(ITASK, MULTI_FVM, IXS, IXQ, IXTG, XGRID, WGRID, IPM, PM, 
     .     FUNC_VALUE, EBCS_TAB,NPF,TF)
C-----------------------------------------------
C     D e s c r i p t i o n
C-----------------------------------------------
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE MULTI_FVM_MOD
      USE EBCS_MOD
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
! NEBCS
#include      "com04_c.inc"
! NPEBC, NIXS
#include      "param_c.inc"
! LISURF1
#include      "scr17_c.inc"
! SNPC,STF
#include      "tabsiz_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      INTEGER, INTENT(IN) :: ITASK
      INTEGER, INTENT(IN) :: IXS(NIXS, *), IXQ(NIXQ, *), IXTG(NIXTG, *)
      my_real, INTENT(IN) :: XGRID(3, *), WGRID(3, *)
      INTEGER, INTENT(IN) :: IPM(NPROPMI, *)
      my_real, INTENT(IN) :: PM(NPROPM, *), FUNC_VALUE(*)
      TYPE(t_ebcs_tab), TARGET, INTENT(IN) :: EBCS_TAB
      INTEGER, INTENT(IN) :: NPF(SNPC)
      my_real, INTENT(IN) :: TF(STF)
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
     .           IXS, IXQ, IXTG, XGRID, WGRID, IPM, PM, FUNC_VALUE,NPF,TF)
            TYPE IS(t_ebcs_fluxout)
              CALL MULTI_INLET_EBCS(ITASK, I, MULTI_FVM, NELEM,
     .           twf%ielem, twf%iface, twf%fvm_inlet_data,
     .           IXS, IXQ, IXTG, XGRID, WGRID, IPM, PM, FUNC_VALUE,NPF,TF)
            TYPE IS(t_ebcs_nrf)
              CALL MULTI_NRF_EBCS(ITASK, I, MULTI_FVM, NELEM,
     .           twf%ielem, twf%iface, twf%fvm_inlet_data,
     .           IXS, IXQ, IXTG, XGRID, WGRID, IPM, PM, FUNC_VALUE,twf,NPF,TF)
            CLASS DEFAULT
         END SELECT
      ENDDO
C-----------------------------------------------
C     E n d   o f   s u b r o u t i n e 
C-----------------------------------------------
      END SUBROUTINE MULTI_EBCS
