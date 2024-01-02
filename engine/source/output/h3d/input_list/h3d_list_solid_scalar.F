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
Chd|====================================================================
Chd|  H3D_LIST_SOLID_SCALAR         source/output/h3d/input_list/h3d_list_solid_scalar.F
Chd|-- called by -----------
Chd|        LECH3D                        source/output/h3d/h3d_build_fortran/lech3d.F
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE H3D_LIST_SOLID_SCALAR(H3D_KEYWORD_SOLID_SCALAR,GEO,IGEO,MULTI_FVM,NKEY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE H3D_MOD
      USE MULTI_FVM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (H3D_KEYWORD) H3D_KEYWORD_SOLID_SCALAR(*)
      my_real GEO(NPROPG,NUMGEO)
      INTEGER IGEO(NPROPGI,NUMGEO)
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
      INTEGER, INTENT(INOUT) :: NKEY
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,NIPMAX
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      I = 0
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'MASS'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Mass'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'EPSP'
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_LAYER  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_LAYER_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Plastic Strain'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'NL_EPSP'
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_LAYER  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_LAYER_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Non-local Plastic Strain'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'DENS'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Density'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'DT'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Time Step'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'ENER'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Internal Energy per unit mass'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'TEMP'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Temperature'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'P'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Pressure'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'VONM'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Von Mises'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'K'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Turbulent Energy'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Specific for turbulent energy in CFD'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'TVIS'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Turbulent Viscosity'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Specific for turbulent Viscosity in CFD'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'VORTX'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Vorticity-X'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'VORTY'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Vorticity-Y'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'VORTZ'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Vorticity-Z'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'VORT'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Vorticity'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'DAM1'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Damage 1'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'DAM2'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Damage 2'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'DAM3'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Damage 3'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'SIGX'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Stress X'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'SIGY'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Stress Y'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'SIGZ'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Stress Z'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'SIGXY'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Stress XY'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'SIGYZ'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Stress YZ'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'SIGZX'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Stress ZX'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'USER'
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_UVAR  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_UVAR_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'User Var'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'HOURGLASS'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Hourglass Energy per unit mass'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'EPSD'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Strain Rate'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Equivalent strain rate (only available in case of strain rate filtering)'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3   = 'NL_EPSD'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Non-local Plastic Strain Rate'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'FLAY'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Failed layers'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Number of failed layers'
C      
      IF (MULTI_FVM%IS_USED) THEN
c
c     Number of present material is dynamic for law151, hence create full list
c     of output keywords at runtime by looping over all present materials
c     
         DO II = 1, MULTI_FVM%NBMAT
c-----------------------------------------------
            I = I + 1
            WRITE(H3D_KEYWORD_SOLID_SCALAR(I)%KEY3, '(A9,I0)') 'M151VFRAC', II
            WRITE(H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1, '(A17,I0)') 'Volume fraction ', II
            H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Volume fractions (for ALE multi-material Law151)'
c-----------------------------------------------
            I = I + 1
            WRITE(H3D_KEYWORD_SOLID_SCALAR(I)%KEY3, '(A8,I0)') 'M151DENS', II
            WRITE(H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1, '(A8,I0)') 'Density ', II
            H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Density (for ALE multi-material Law151)'
c-----------------------------------------------
            I = I + 1
            WRITE(H3D_KEYWORD_SOLID_SCALAR(I)%KEY3, '(A8,I0)') 'M151ENER', II
            WRITE(H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1, '(A7,I0)') 'Energy ', II
            H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Energy (for ALE multi-material Law151)'
c-----------------------------------------------
            I = I + 1
            WRITE(H3D_KEYWORD_SOLID_SCALAR(I)%KEY3, '(A8,I0)') 'M151PRES', II
            WRITE(H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1, '(A10,I0)') 'Pressure ', II
            H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Pressure (for ALE multi-material Law151)'
c-----------------------------------------------
         ENDDO
      ELSE
c-----------------------------------------------
         I = I + 1
         H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'VFRAC1'
         H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Volumetric Fraction 1'
         H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Volumetric fractions (for ALE multi-material laws: LAW20, LAW37 and LAW51)'
c-----------------------------------------------
         I = I + 1
         H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'VFRAC2'
         H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Volumetric Fraction 2'
         H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Volumetric fractions (for ALE multi-material laws: LAW20, LAW37 and LAW51)'
c-----------------------------------------------
         I = I + 1
         H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'VFRAC3'
         H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Volumetric Fraction 3'
         H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Volumetric fractions (for ALE multi-material laws: LAW20, LAW37 and LAW51)'
c-----------------------------------------------
         I = I + 1
         H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'VFRAC4'
         H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Volumetric Fraction 4'
         H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Volumetric fractions (for ALE multi-material laws: LAW20, LAW37 and LAW51)'
c-----------------------------------------------
      ENDIF  ! MULTI_FVM%IS_USED
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'ORTHD/PSI'
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_LAYER  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_LAYER_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IR  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IR_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IS  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IS_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IT  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IT_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Psi'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Euler angles psi, global reference system -> orthotropic reference system'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'ORTHD/THETA'
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_LAYER  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_LAYER_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IR  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IR_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IS  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IS_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IT  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IT_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Theta'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Euler angles theta, global reference system -> orthotropic reference system'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'ORTHD/PHI'
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_LAYER  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_LAYER_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IR  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IR_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IS  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IS_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IT  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IT_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Phi'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Euler angles phi, global reference system -> orthotropic reference system'
c-----------------------------------------------



c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'BFRAC'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Burn Fraction'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Burn fraction (for JWL EOS: LAW5 and LAW51)'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'VDAM1'
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IR  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IR_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IS  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IS_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IT  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IT_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Damage variable1'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Value of damage factor d'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'VDAM2'
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IR  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IR_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IS  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IS_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IT  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IT_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Damage variable2'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Value of damage surface sigmay0'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'VDAM3'
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IR  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IR_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IS  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IS_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IT  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IT_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Damage variable3'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Value of rupture surface sigmayf'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'DAMA'
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IR  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IR_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IS  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IS_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IT  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IT_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'MAX DAMAGE'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'maximum of damage over time and of all failure criteria acting in one material'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'DAMG'
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IR  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IR_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IS  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IS_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IT  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IT_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_LAYER  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_LAYER_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Damage'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Damage variable for coupled damage models'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'DAMINI'
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IR  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IR_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IS  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IS_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IT  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IT_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Damage initiation'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Damage initiation'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'FAILURE'
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IR  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IR_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IS  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IS_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IT  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_IT_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_ID  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Failure'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Damage of current failure criterion'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3   = 'TSAIWU'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Tsai-Wu Criterion'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'TDEL'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'TIME DELETION ELEMENT'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'output only for elements which are deleted, due to a /FAIL criterion.'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'SSP'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Sound Speed'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Sound speed. Only available with ALE material laws'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'VOLU'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Volume'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Element Volume'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'SCHLIEREN'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Schlieren'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Schlieren image (optical method widely used in CFD field). ALE material laws'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'DOMAIN'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Domain'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'FILL'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Filling percentage'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'SIGEQ'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Equiv stress'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Equivalent stress, based on yield criteria used for the corresponding material'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'BULK'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Artificial Viscosity'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'TDET'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Detonation Time'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'MOMX'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Momentum Density X'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'MOMY'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Momentum Density Y'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'MOMZ'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Momentum Density Z'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'MOMXY'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Momentum Density XY'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'MOMYZ'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Momentum Density YZ'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'MOMXZ'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Momentum Density XZ'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = '|MOM|'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Momentum Density ABS'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'VELX'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Velocity X'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'VELY'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Velocity Y'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'VELZ'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Velocity Z'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'VELXY'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Velocity XY'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'VELYZ'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Velocity YZ'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'VELXZ'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Velocity XZ'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = '|VEL|'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Velocity ABS'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'ENTH'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Enthalpy'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'ENTHM'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Enthalpy per unit mass'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'ENTHV'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Enthalpy per unit volume'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'EINT'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Internal Energy'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'EINTM'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Internal Energy per unit mass'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'EINTV'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Internal Energy per unit volume'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'AMS'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'AMS selection'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'OFF'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Element status'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = '0.0:deleted  >0.&<1.:under failure process 1.0:activated  <0:Standby'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'MACH'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Mach Number'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Mach Number. Only available with ALE material law 151'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'COLOR'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Color Function'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Color function for multimaterial and multifluid ALE laws'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'WPLA'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Plastic Work'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'FLDF/OUTER'
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_SKIN  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'FLD Failure Factor OUTER'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'FLD Failure Factor on the external surface of solid'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'FLDZ/OUTER'
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_SKIN  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'FLD Zone Index OUTER'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = '1:HighWrinkle 2:Compression 3:LooseMetal 4:SafeZone 5:MarginToFail 6:FailureZone'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'MDS'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'MDS'
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_MDSVAR = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_MDSVAR_ALL  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_MDSVAR_DEF  = 1
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'PEXT'
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_SKIN  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Pressure OUTER'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'External pressure on the Skin of solid'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'GROUP'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Group identifier'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'INTERNAL.ID'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Internal identifier'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'LOCAL.ID'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Local identifier within the group'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'THICK'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Thickness : direction s'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'THIN'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Thinning(percentage) : direction s'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'ENER/TMAX'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Max of Internal Energy per unit mass'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Max of Internal Energy per unit mass over time'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'VONM/TMAX'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Max of Von Mises'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Max of Von Mises stress over time'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'SIGEQ/TMAX'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Max of Equiv stress'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Max of Equivalent stress over time&ipt'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'DAMA/TMAX'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'MAX of DAMAGE'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'maximum of damage over time&ipt'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'TILLOTSON'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'Region identifier in p,v diagram'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = '1:compressed, 2:expanded cold, 3:transition, 4:hot expansion'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'FLDF/OUTER_AVERAGE'
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_SKIN  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'FLD Failure Factor OUTER_AVERAGE'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'FLD Failure Factor average of solid skin'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'FLDZ/OUTER_AVERAGE'
      H3D_KEYWORD_SOLID_SCALAR(I)%IS_SKIN  = 1
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'FLD Zone Index OUTER_AVERAGE'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = '1:HighWrinkle 2:Compression 3:LooseMetal 4:SafeZone 5:MarginToFail 6:FailureZone'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SOLID_SCALAR(I)%KEY3  = 'DIV(U)'
      H3D_KEYWORD_SOLID_SCALAR(I)%TEXT1  = 'div(u)'
      H3D_KEYWORD_SOLID_SCALAR(I)%COMMENT  = 'Volumetric dilatation rate'
c-----------------------------------------------


c-----------------------------------------------
      NKEY=I
      END
