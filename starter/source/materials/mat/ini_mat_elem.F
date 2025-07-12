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
      !||    ini_mat_elem           ../starter/source/materials/mat/ini_mat_elem.F
      !||--- called by ------------------------------------------------------
      !||    read_material_models   ../starter/source/materials/read_material_models.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      SUBROUTINE INI_MAT_ELEM(MAT_ELEM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MAT_ELEM_MOD
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(MAT_ELEM_) ,INTENT(INOUT) :: MAT_ELEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IMAT
C=======================================================================
      DO IMAT = 1,MAT_ELEM%NUMMAT
        MAT_ELEM%MAT_PARAM(IMAT)%ILAW      = 0           ! material law number(type)
        MAT_ELEM%MAT_PARAM(IMAT)%MAT_ID    = 0           ! material law ID
        MAT_ELEM%MAT_PARAM(IMAT)%NUPARAM   = 0           ! number of real value material paraameters
        MAT_ELEM%MAT_PARAM(IMAT)%NIPARAM   = 0           ! number of int value material parameters
        MAT_ELEM%MAT_PARAM(IMAT)%NFUNC     = 0           ! number of local functions in material
        MAT_ELEM%MAT_PARAM(IMAT)%NTABLE    = 0           ! number of local function tables
        MAT_ELEM%MAT_PARAM(IMAT)%NSUBMAT   = 0           ! number of submaterials (multi-mat law51)
        MAT_ELEM%MAT_PARAM(IMAT)%NFAIL     = 0           ! number of failure models
        MAT_ELEM%MAT_PARAM(IMAT)%IVISC     = 0           ! viscosity model number
        MAT_ELEM%MAT_PARAM(IMAT)%IEOS      = 0           ! eos model number
        MAT_ELEM%MAT_PARAM(IMAT)%ITHERM    = 0           ! thermal option activation flag (/heat/mat)
        MAT_ELEM%MAT_PARAM(IMAT)%IEXPAN    = 0           ! thermal volumic expansion flag (/therm_stress)
        MAT_ELEM%MAT_PARAM(IMAT)%IALE      = 0           ! ale formulation flag
        MAT_ELEM%MAT_PARAM(IMAT)%ITURB     = 0           ! turbulent flow flag
        MAT_ELEM%MAT_PARAM(IMAT)%HEAT_FLAG = 0           ! dissipated energy (heat source) is output by material law
c
        MAT_ELEM%MAT_PARAM(IMAT)%COMPRESSIBILITY    = 3  ! elsto_plastic
        MAT_ELEM%MAT_PARAM(IMAT)%SMSTR              = 2  ! large strain
        MAT_ELEM%MAT_PARAM(IMAT)%STRAIN_FORMULATION = 1  ! incremental
        MAT_ELEM%MAT_PARAM(IMAT)%IPRES              = 1  ! hydrostatic using AMU
        MAT_ELEM%MAT_PARAM(IMAT)%ORTHOTROPY         = 1  ! isotropic
c
        MAT_ELEM%MAT_PARAM(IMAT)%NLOC         = 0
        MAT_ELEM%MAT_PARAM(IMAT)%IFAILWAVE    = 0
        MAT_ELEM%MAT_PARAM(IMAT)%IXFEM        = 0
c
        MAT_ELEM%MAT_PARAM(IMAT)%NMOD         = 0        ! Number of damage modes
!
!       ! initialize viscosity model parameters
!
        MAT_ELEM%MAT_PARAM(IMAT)%VISC%ILAW    = 0
        MAT_ELEM%MAT_PARAM(IMAT)%VISC%NUPARAM = 0
        MAT_ELEM%MAT_PARAM(IMAT)%VISC%NIPARAM = 0
        MAT_ELEM%MAT_PARAM(IMAT)%VISC%NUVAR   = 0
        MAT_ELEM%MAT_PARAM(IMAT)%VISC%NFUNC   = 0
        MAT_ELEM%MAT_PARAM(IMAT)%VISC%NTABLE  = 0
c
        ! Properties compatibility flags
        MAT_ELEM%MAT_PARAM(IMAT)%PROP_SOLID   = 0
        MAT_ELEM%MAT_PARAM(IMAT)%PROP_SHELL   = 0
        MAT_ELEM%MAT_PARAM(IMAT)%PROP_BEAM    = 0
        MAT_ELEM%MAT_PARAM(IMAT)%PROP_SPRING  = 0
        MAT_ELEM%MAT_PARAM(IMAT)%PROP_TRUSS   = 0
        MAT_ELEM%MAT_PARAM(IMAT)%PROP_SPH     = 0      
c
        ! Options compatibility flags
        MAT_ELEM%MAT_PARAM(IMAT)%COMPATIBILITY_EOS = 0
c
        ! global material parameters
        MAT_ELEM%MAT_PARAM(IMAT)%RHO           = ZERO 
        MAT_ELEM%MAT_PARAM(IMAT)%RHO0          = ZERO 
        MAT_ELEM%MAT_PARAM(IMAT)%YOUNG         = ZERO 
        MAT_ELEM%MAT_PARAM(IMAT)%SHEAR         = ZERO 
        MAT_ELEM%MAT_PARAM(IMAT)%BULK          = ZERO 
        MAT_ELEM%MAT_PARAM(IMAT)%NU            = ZERO 
        MAT_ELEM%MAT_PARAM(IMAT)%STIFF_CONTACT = ZERO
        MAT_ELEM%MAT_PARAM(IMAT)%STIFF_HGLASS  = ZERO
        MAT_ELEM%MAT_PARAM(IMAT)%STIFF_TSTEP   = ZERO

        ! EOS model parameters
        MAT_ELEM%MAT_PARAM(IMAT)%EOS%TITLE = ''
        MAT_ELEM%MAT_PARAM(IMAT)%EOS%NUPARAM = 0
        MAT_ELEM%MAT_PARAM(IMAT)%EOS%NIPARAM = 0
        MAT_ELEM%MAT_PARAM(IMAT)%EOS%NFUNC = 0
        MAT_ELEM%MAT_PARAM(IMAT)%EOS%NTABLE = 0
        MAT_ELEM%MAT_PARAM(IMAT)%EOS%ISFLUID = 0
        MAT_ELEM%MAT_PARAM(IMAT)%EOS%CV = ZERO
        MAT_ELEM%MAT_PARAM(IMAT)%EOS%CP = ZERO
c        
        ! Thermal parameters
        MAT_ELEM%MAT_PARAM(IMAT)%THERM%iform      = 0
        MAT_ELEM%MAT_PARAM(IMAT)%THERM%func_thexp = 0
        MAT_ELEM%MAT_PARAM(IMAT)%THERM%TINI       = ZERO
        MAT_ELEM%MAT_PARAM(IMAT)%THERM%TREF       = ZERO
        MAT_ELEM%MAT_PARAM(IMAT)%THERM%TMELT      = ZERO
        MAT_ELEM%MAT_PARAM(IMAT)%THERM%RHOCP      = ZERO
        MAT_ELEM%MAT_PARAM(IMAT)%THERM%AS         = ZERO
        MAT_ELEM%MAT_PARAM(IMAT)%THERM%BS         = ZERO
        MAT_ELEM%MAT_PARAM(IMAT)%THERM%AL         = ZERO
        MAT_ELEM%MAT_PARAM(IMAT)%THERM%BL         = ZERO
        MAT_ELEM%MAT_PARAM(IMAT)%THERM%EFRAC      = ZERO
        MAT_ELEM%MAT_PARAM(IMAT)%THERM%scale_thexp= ZERO
c        
      END DO
c-----------
      RETURN
      END 
