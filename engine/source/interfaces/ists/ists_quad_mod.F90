!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
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
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!
!   Single Engine source for STS quadrature orders.
!   Change STS_QUAD_IP_GAUSS / STS_QUAD_IP_LOBATTO independently.
!   Valid range is 2-5 (fully tabulated in sts_gausspt / sts_lobattopt).
!   Later: replace PARAMETER with runtime values from IPARI (Starter).
!
!||====================================================================
!||    ists_quad_mod   ../engine/source/interfaces/ists/ists_quad_mod.F90
!||--- called by ------------------------------------------------------
!||    ists_mainf              ../engine/source/interfaces/ists/ists_mainf.F90
!||    sts_contact_eval_pair   ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||====================================================================
      MODULE ISTS_QUAD_MOD
        IMPLICIT NONE
        PRIVATE

!       Points per direction (NxN GPs). Gauss and Lobatto may differ.
        INTEGER, PARAMETER, PUBLIC :: STS_QUAD_IP_GAUSS   = 2
        INTEGER, PARAMETER, PUBLIC :: STS_QUAD_IP_LOBATTO = 3
!       Max over both modes: stack arrays and GP-history capacity.
        INTEGER, PARAMETER, PUBLIC :: STS_QUAD_IP_MAX = MAX(STS_QUAD_IP_GAUSS, STS_QUAD_IP_LOBATTO)
        INTEGER, PARAMETER, PUBLIC :: STS_QUAD_IP_TAB_MAX = 5 ! Max over both modes, dont increase Gauss and Lobatto beyond 5.

      END MODULE ISTS_QUAD_MOD
