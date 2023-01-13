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
Chd|  H3D_LIST_QUAD_SCALAR          source/output/h3d/input_list/h3d_list_quad_scalar.F
Chd|-- called by -----------
Chd|        LECH3D                        source/output/h3d/h3d_build_fortran/lech3d.F
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE H3D_LIST_QUAD_SCALAR(H3D_KEYWORD_QUAD_SCALAR,GEO,IGEO,MULTI_FVM,NKEY)
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (H3D_KEYWORD) H3D_KEYWORD_QUAD_SCALAR(*)
      my_real
     .      GEO(NPROPG,*)
      INTEGER IGEO(NPROPGI,*)
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
      INTEGER, INTENT(INOUT) :: NKEY
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "chara_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,NIPMAX,IPLYMAX,CPTPLY,IPTMAX
C=========================================================================
      I = 0
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'MASS'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Mass'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'DENS'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Density'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'ENER'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Internal Energy per unit mass'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'TEMP'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Temperature'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'THICK'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Thickness'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'P'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Pressure'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'VONM'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Von Mises'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'K'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Turbulent Energy'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'TVIS'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Turbulent Viscosity'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'VORTX'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Vorticity'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'DAM1'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Damage 1'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'DAM2'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Damage 2'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'DAM3'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Damage 3'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'SIGX'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Stress X'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'SIGY'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Stress Y'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'SIGZ'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Stress Z'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'SIGXY'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Stress XY'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'SIGYZ'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Stress YZ'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'SIGZX'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Stress ZX'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'USER'
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_UVAR  = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'User Var'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'HOURGLASS'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Hourglass Energy'
c-----------------------------------------------
      IF (MULTI_FVM%IS_USED) THEN
c
c     Number of present material is dynamic for law151, hence create full list
c     of output keywords at runtime by looping over all present materials
c     
         DO II = 1, MULTI_FVM%NBMAT
c-----------------------------------------------
            I = I + 1
            WRITE(H3D_KEYWORD_QUAD_SCALAR(I)%KEY3, '(A9,I0)') 'M151VFRAC', II
            WRITE(H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1, '(A17,I0)') 'Volume fraction ', II
            H3D_KEYWORD_QUAD_SCALAR(I)%COMMENT  = 
     .        'Volume fractions (for ALE multi-material Law151)'
c-----------------------------------------------
            I = I + 1
            WRITE(H3D_KEYWORD_QUAD_SCALAR(I)%KEY3, '(A8,I0)') 'M151DENS', II
            WRITE(H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1, '(A8,I0)') 'Density ', II
            H3D_KEYWORD_QUAD_SCALAR(I)%COMMENT  = 
     .        'Density (for ALE multi-material Law151)'
c-----------------------------------------------
            I = I + 1
            WRITE(H3D_KEYWORD_QUAD_SCALAR(I)%KEY3, '(A8,I0)') 'M151ENER', II
            WRITE(H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1, '(A7,I0)') 'Energy ', II
            H3D_KEYWORD_QUAD_SCALAR(I)%COMMENT  = 
     .        'Energy (for ALE multi-material Law151)'
c-----------------------------------------------
            I = I + 1
            WRITE(H3D_KEYWORD_QUAD_SCALAR(I)%KEY3, '(A8,I0)') 'M151PRES', II
            WRITE(H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1, '(A10,I0)') 'Pressure ', II
            H3D_KEYWORD_QUAD_SCALAR(I)%COMMENT  = 
     .        'Pressure (for ALE multi-material Law151)'
c-----------------------------------------------
         ENDDO
      ELSE
c-----------------------------------------------
         I = I + 1
         H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'VFRAC1'
         H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Volumetric Fraction 1'
         H3D_KEYWORD_QUAD_SCALAR(I)%COMMENT  = 
     .        'Volumetric fractions (for ALE multi-material laws: LAW20, LAW37 and LAW51)'
c-----------------------------------------------
         I = I + 1
         H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'VFRAC2'
         H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Volumetric Fraction 2'
         H3D_KEYWORD_QUAD_SCALAR(I)%COMMENT  = 
     .        'Volumetric fractions (for ALE multi-material laws: LAW20, LAW37 and LAW51)'
c-----------------------------------------------
         I = I + 1
         H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'VFRAC3'
         H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Volumetric Fraction 3'
         H3D_KEYWORD_QUAD_SCALAR(I)%COMMENT  = 
     .        'Volumetric fractions (for ALE multi-material laws: LAW20, LAW37 and LAW51)'
c-----------------------------------------------
         I = I + 1
         H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'VFRAC4'
         H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Volumetric Fraction 4'
         H3D_KEYWORD_QUAD_SCALAR(I)%COMMENT  = 
     .        'Volumetric fractions (for ALE multi-material laws: LAW20, LAW37 and LAW51)'
c-----------------------------------------------
      ENDIF  ! MULTI_FVM%IS_USED
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'BFRAC'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Burn Fraction'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'SSP'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Sound Speed'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'EPSD'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Strain Rate'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'EPSP'
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_LAYER = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_LAYER_ALL = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_IPT_ALL = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_PLY = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_PLY_ALL = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Plastic Strain '
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3   = 'TSAIWU'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Tsai-Wu Criterion'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'FAIL'
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_PLY = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Nb of Failed layers'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'CRMAS'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Airbag crossing mass'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'CRVEL'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Airbag crossing velocity'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'THIN'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = '%Thinning'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'ERROR/THICK'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Estimated Error on Thickness'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'PHI'
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_PLY = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_PLY_ALL = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_LAYER = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_LAYER_ALL = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_IPT_ALL = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'PHI'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'DAMA'
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_PLY = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_PLY_ALL = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_LAYER = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_LAYER_ALL = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_IPT_ALL = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'DAMAGE'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'DAMA/MEMB'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'MAX DAMAGE MEMBRANE'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'NXTF'
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_LAYER = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_LAYER_ALL = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_IPT = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%IS_IPT_ALL = 1
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'NXT FAILURE FACTOR'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'NXTF/MEMB'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'NXT FAILURE FACTOR MEMBRANE'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'TDEL'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'TIME DELETION ELEMENT'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'SCHLIEREN'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Schlieren'
      H3D_KEYWORD_QUAD_SCALAR(I)%COMMENT  = 
     .'Schlieren image (optical method widely used in CFD field). ALE material laws'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'PHI/MEMB'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'phi MEMBRANE'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'DOMAIN'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Domain'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'SIGEQ'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Equiv stress'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'BULK'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Artificial Viscosity'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'LAW20/DENS1'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Density-1'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'LAW20/DENS2'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Density-2'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'LAW20/ENER1'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Specific Energy-1'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'LAW20/ENER2'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Specific Energy-2'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'LAW20/TEMP1'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Temperature-1'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'LAW20/TEMP2'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Temperature-2'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'LAW20/P1'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Pressure-1'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'LAW20/P2'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Pressure-2'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'LAW20/EPSP1'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Plastic Strain-1'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'LAW20/EPSP1'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Plastic Strain-2'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'LAW20/SSP1'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Sound Speed-1'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'LAW20/SSP2'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Sound Speed-2'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'LAW20/VOLUM1'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Volume-1'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'LAW20/VOLUM2'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Volume-2'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'LAW20/QVIS1'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Artificial Viscosity-1'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'LAW20/QVIS2'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Artificial Viscosity-2'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'TDET'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Detonation Time'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'DT'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Time Step'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'AMS'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'AMS selection'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'ENTH'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Enthalpy'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'ENTHM'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Enthalpy per unit mass'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'ENTHV'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Enthalpy per unit volume'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'EINTM'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Internal Energy per unit mass'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'EINTV'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Internal Energy per unit volume'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'EINT'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Internal Energy'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'OFF'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Element status'
      H3D_KEYWORD_QUAD_SCALAR(I)%COMMENT  = '0.0:deleted  >0.&<1.:under failure process 1.0:activated  <0:Standby'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'MACH'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Mach Number'
      H3D_KEYWORD_QUAD_SCALAR(I)%COMMENT  = 'Mach Number. Only available with ALE material law 151'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'COLOR'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Color Function'
      H3D_KEYWORD_QUAD_SCALAR(I)%COMMENT  = 'Color function for multimaterial and multifluid ALE laws'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'VOLU'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Volume'
      H3D_KEYWORD_QUAD_SCALAR(I)%COMMENT  = 'Element Volume'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'GROUP'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Group identifier'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'INTERNAL.ID'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Internal identifier'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_QUAD_SCALAR(I)%KEY3  = 'LOCAL.ID'
      H3D_KEYWORD_QUAD_SCALAR(I)%TEXT1  = 'Local identifier within the group'
c-----------------------------------------------
      NKEY=I
c-----------------------------------------------
      END
