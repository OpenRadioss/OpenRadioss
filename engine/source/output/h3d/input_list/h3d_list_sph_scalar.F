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
Chd|  H3D_LIST_SPH_SCALAR           source/output/h3d/input_list/h3d_list_sph_scalar.F
Chd|-- called by -----------
Chd|        LECH3D                        source/output/h3d/h3d_build_fortran/lech3d.F
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE H3D_LIST_SPH_SCALAR(H3D_KEYWORD_SPH_SCALAR,GEO,IGEO,NKEY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (H3D_KEYWORD) H3D_KEYWORD_SPH_SCALAR(*)
      my_real
     .      GEO(NPROPG,*)
      INTEGER IGEO(NPROPGI,*)
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
      INTEGER I,J,NIPMAX
C=========================================================================
      I = 0
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'MASS'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Mass'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'DIAMETER'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Diameter'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'EPSP'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Plastic Strain'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'DENS'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Density'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'ENER'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Internal Energy per unit mass'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'TEMP'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Temperature'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'P'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Pressure'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'VONM'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Von Mises'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'K'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Turbulent Energy'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'TVIS'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Turbulent Viscosity'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'VORTX'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Vorticity'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'DAM1'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Damage 1'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'DAM2'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Damage 2'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'DAM3'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Damage 3'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'SIGX'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Stress X'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'SIGY'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Stress Y'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'SIGZ'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Stress Z'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'SIGXY'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Stress XY'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'SIGYZ'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Stress YZ'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'SIGZX'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Stress ZX'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'USER'
      H3D_KEYWORD_SPH_SCALAR(I)%IS_UVAR  = 1
      H3D_KEYWORD_SPH_SCALAR(I)%IS_UVAR_ALL  = 1
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'User Var'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'HOURGLASS'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Hourglass Energy'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'BFRAC'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Burn Fraction'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'DAMA'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'MAX DAMAGE ELEMENT'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'DOMAIN'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Domain'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'FILL'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Filling percentage'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'SIGEQ'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Equiv stress'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'TDET'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Detonation Time'
      H3D_KEYWORD_SPH_SCALAR(I)%COMMENT  = 'only available for JWL EOS or time controlled detonation'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'OFF'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Element status'
      H3D_KEYWORD_SPH_SCALAR(I)%COMMENT  =
     .'0.0:deleted  >0.&<1.:under failure process 1.0:activated  <0:Standby'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'ENTH'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Enthalpy'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'ENTHM'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Enthalpy per unit mass'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'ENTHV'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Enthalpy per unit volume'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'EINT'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Internal Energy'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'EINTM'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Internal Energy per unit mass'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'EINTV'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'Internal Energy per unit volume'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'GROUP'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'debug - Group identifier'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'INTERNAL.ID'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'debug - Internal identifier'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_SPH_SCALAR(I)%KEY3  = 'LOCAL.ID'
      H3D_KEYWORD_SPH_SCALAR(I)%TEXT1  = 'debug - Local identifier'
c-----------------------------------------------



      NKEY=I
      END
