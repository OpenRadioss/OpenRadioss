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
Chd|  H3D_LIST_SHELL_SCALAR         source/output/h3d/input_list/h3d_list_shell_scalar.F
Chd|-- called by -----------
Chd|        LECH3D                        source/output/h3d/h3d_build_fortran/lech3d.F
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE H3D_LIST_SHELL_SCALAR(H3D_KEYWORD_SHELL_SCALAR,GEO,IGEO,MULTI_FVM,NKEY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE H3D_MOD
      USE STACK_MOD
      USE MULTI_FVM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (H3D_KEYWORD) H3D_KEYWORD_SHELL_SCALAR(*)
      my_real GEO(NPROPG,NUMGEO)
      INTEGER IGEO(NPROPGI,NUMGEO)
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
      INTEGER, INTENT(INOUT) :: NKEY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,NIPMAX,IPLYMAX,CPTPLY,IPTMAX
C=========================================================================
      I = 0
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'MASS'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Mass'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'DENS'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Density'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'ENER'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Internal Energy per unit mass'
c-----------------------------------------------
            I = I + 1
            H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'EINTM'
            H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Internal Energy per unit mass'
c-----------------------------------------------
            I = I + 1
            H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'EINTV'
            H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Internal Energy per unit volume'
c-----------------------------------------------
            I = I + 1
            H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'EINT'
            H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Internal Energy'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'TEMP'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Temperature'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'THICK'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Thickness'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'P'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Pressure'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'VONM'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Von Mises'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'K'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Turbulent Energy'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = 
     .'Specific for turbulent energy in CFD'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'TVIS'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Turbulent Viscosity'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = 'Specific for turbulent Viscosity in CFD'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'VORTX'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Vorticity'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'DAM1'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Damage 1'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'DAM2'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Damage 2'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'DAM3'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Damage 3'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'SIGX'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Stress X'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'SIGY'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Stress Y'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'SIGZ'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Stress Z'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'SIGXY'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Stress XY'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'SIGYZ'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Stress YZ'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'SIGZX'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Stress ZX'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'USER'
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_UVAR  = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_UVAR_ALL  = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER  = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER_ALL  = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT  = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT_ALL  = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY  = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY_ALL  = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'User Var'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'TSAIWU'
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER  = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER_ALL  = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT  = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT_ALL  = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY  = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY_ALL  = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Tsai-Wu Criterion'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'HOURGLASS'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Hourglass Energy per unit mass'
c-----------------------------------------------
      IF (MULTI_FVM%IS_USED) THEN
c
c     Number of present material is dynamic for law151, hence create full list
c     of output keywords at runtime by looping over all present materials
c     
         DO II = 1, MULTI_FVM%NBMAT
c-----------------------------------------------
            I = I + 1
            WRITE(H3D_KEYWORD_SHELL_SCALAR(I)%KEY3, '(A9,I0)') 'M151VFRAC', II
            WRITE(H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1, '(A17,I0)') 'Volume fraction ', II
            H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = 'Volume fractions (for ALE multi-material Law151)'
c-----------------------------------------------
            I = I + 1
            WRITE(H3D_KEYWORD_SHELL_SCALAR(I)%KEY3, '(A8,I0)') 'M151DENS', II
            WRITE(H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1, '(A8,I0)') 'Density ', II
            H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = 'Density (for ALE multi-material Law151)'
c-----------------------------------------------
            I = I + 1
            WRITE(H3D_KEYWORD_SHELL_SCALAR(I)%KEY3, '(A8,I0)') 'M151ENER', II
            WRITE(H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1, '(A7,I0)') 'Energy ', II
            H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = 'Energy (for ALE multi-material Law151)'
c-----------------------------------------------
            I = I + 1
            WRITE(H3D_KEYWORD_SHELL_SCALAR(I)%KEY3, '(A8,I0)') 'M151PRES', II
            WRITE(H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1, '(A10,I0)') 'Pressure ', II
            H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = 'Pressure (for ALE multi-material Law151)'
c-----------------------------------------------
         ENDDO
c-----------------------------------------------
            I = I + 1
            H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'ENTH'
            H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Enthalpy'
c-----------------------------------------------
            I = I + 1
            H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'ENTHM'
            H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Enthalpy per unit mass'
c-----------------------------------------------
            I = I + 1
            H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'ENTHV'
            H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Enthalpy per unit volume'
c-----------------------------------------------
      ENDIF  ! MULTI_FVM%IS_USED
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'EPSD'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Strain Rate'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = 'Equivalent strain rate (only available in case of strain rate filtering)'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'EPSP'
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Plastic Strain '
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3   = 'NL_EPSD'
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Non-local Plastic Strain Rate'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'NL_EPSP'
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Non-local Plastic Strain '
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'FAIL'
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Nb of Failed layers'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = 
     .'/PROP/TYPE10, TYPE11, TYPE17, TYPE51, PCOMPP, /MAT/LAW15 & LAW25 (COMPSH)'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'CRMAS'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Airbag crossing mass'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'CRVEL'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Airbag crossing velocity'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'ALEBA/MASS'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'FVMBAG - Mass'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'ALEBA/PRES'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'FVMBAG - Pressure'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'ALEBA/MOMX'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'FVMBAG - Fluid velocity X'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'ALEBA/MOMY'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'FVMBAG - Fluid velocity Y'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'ALEBA/MOMZ'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'FVMBAG - Fluid velocity Z'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'ALEBA/DENS'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'FVMBAG - Density'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'ALEBA/ENER'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'FVMBAG - Energy'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'ALEBA/SSP'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'FVMBAG - Sound Speed'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'ALEBA/GAMA'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'FVMBAG - Gama'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'ALEBA/VISU'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'FVMBAG - Visu Polyhedra'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'THIN'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = '%Thinning'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'ERROR/THICK'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Estimated Error on Thickness'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'PHI'
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'PHI'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = 
     .'angle between element skew and direction 1 orthotropy'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'DAMA'
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'MAX DAMAGE'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = 'maximum of damage over time and of all failure criteria acting in one material'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'FAILURE'
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_ID = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Failure'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = 'Damage of current failure criterion'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'DAMINI'
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1 = 'Damage initiation'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT = 'Damage initiation variable'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'DAMA/MEMB'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'MAX DAMAGE MEMBRANE'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'DAMG'
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1 = 'Damage'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT = 'Damage variable for coupled damage models'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3    = 'DAMG/MEMB'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1   = 'Damage (Membrane value)'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'NXTF'
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'NXT FAILURE FACTOR'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'NXTF/MEMB'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'NXT FAILURE FACTOR MEMBRANE'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'TDEL'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'TIME DELETION ELEMENT'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = 'output only for elements which are deleted, due to a /FAIL criterion.'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'SSP'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Sound Speed'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = 'Sound speed. Only available with ALE material laws'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'SCHLIEREN'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Schlieren'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = 'Schlieren image (optical method widely used in CFD field). ALE material laws'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'PHI/MEMB'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'phi MEMBRANE'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'DOMAIN'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Domain'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'SIGEQ'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Equiv stress'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = 'Equivalent stress, based on yield criteria used for the corresponding material'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'BULK'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Artificial Viscosity'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'TDET'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Detonation Time'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'DT'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Time Step'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'AMS'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'AMS selection'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'ALPHA'
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'ALPHA'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = 'shear angle - change of angle between two anisotropy directions'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'OFF'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Element status'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = '0.0:deleted  >0.&<1.:under failure process 1.0:activated  <0:Standby'
c-----------------------------------------------
!to be moved in h3d_tria_scalar when implemented.
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'MACH'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Mach Number'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = 'Mach Number. Only available with ALE material law 151'
c-----------------------------------------------
!to be moved in h3d_tria_scalar when implemented.
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'COLOR'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Color Function'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = 'Color function for multimaterial and multifluid ALE laws'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'FLDF/MEMB'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'FLD Failure Factor Membrane'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'FLDF'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'FLD Failure Factor'
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER_ALL = 1
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'FLDZ/MEMB'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'FLD Zone Index Membrane'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = '1:HighWrinkle 2:Compression 3:LooseMetal 4:SafeZone 5:MarginToFail 6:FailureZone'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'FLDZ'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'FLD Zone Index'
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = '1:HighWrinkle 2:Compression 3:LooseMetal 4:SafeZone 5:MarginToFail 6:FailureZone'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'HC_DSSE_F/MEMB'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'HC_DSSE Failure Factor Membrane'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'HC_DSSE_F'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'HC_DSSE Failure Factor'
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY_ALL = 1
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'HC_DSSE_Z/MEMB'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'HC_DSSE Zone Index Membrane'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = '1:Below HC and DSSE 2: Above DSSE and below HC 3: Above HC and DSSE'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'HC_DSSE_Z'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'HC_DSSE Zone Index'
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = '1:Below HC and DSSE 2: Above DSSE and below HC 3: Above HC and DSSE'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'WPLA'
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Plastic Work'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'MDS'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'MDS'
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_MDSVAR = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_MDSVAR_ALL  = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_MDSVAR_DEF  = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER  = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER_ALL  = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT  = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT_ALL  = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY  = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY_ALL  = 1
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'PEXT'
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_SKIN  = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Pressure OUTER'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = 'External pressure on the Skin of shell'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'GROUP'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Group identifier'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'INTERNAL.ID'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Internal identifier'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'LOCAL.ID'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Local identifier within the group'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'ENER/TMAX'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Max of Internal Energy per unit mass'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = 'Max of Internal Energy per unit mass over time'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'VONM/TMAX'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Max of Von Mises'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = 'Max of Von Mises stress over time&ipt'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'SIGEQ/TMAX'
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'Max of Equiv stress'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = 'Max of Equivalent stress over time&ipt'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'DAMA/TMAX'
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_LAYER_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_IPT_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%IS_PLY_ALL = 1
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'MAX of DAMAGE'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = 'maximum of damage over time&ipt'
c-----------------------------------------------
      !2d solids (triangles)
      I = I + 1
      H3D_KEYWORD_SHELL_SCALAR(I)%KEY3  = 'DIV(U)'
      H3D_KEYWORD_SHELL_SCALAR(I)%TEXT1  = 'div(u)'
      H3D_KEYWORD_SHELL_SCALAR(I)%COMMENT  = 'Volumetric dilatation rate'
c-----------------------------------------------

      NKEY=I
      END
