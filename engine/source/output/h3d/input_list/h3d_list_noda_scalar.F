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
Chd|  H3D_LIST_NODAL_SCALAR         source/output/h3d/input_list/h3d_list_noda_scalar.F
Chd|-- called by -----------
Chd|        LECH3D                        source/output/h3d/h3d_build_fortran/lech3d.F
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE H3D_LIST_NODAL_SCALAR(H3D_KEYWORD_NODAL_SCALAR,GEO,NKEY)
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
      TYPE (H3D_KEYWORD) H3D_KEYWORD_NODAL_SCALAR(*)
      my_real
     .      GEO(NPROPG,*)
      INTEGER, INTENT(INOUT) :: NKEY
c-------------------------
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
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'MASS'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Node Mass'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'DT'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Nodal Time Step'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'DMASS'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Mass Change'
      H3D_KEYWORD_NODAL_SCALAR(I)%COMMENT  = 'Nodal mass variation ( see time step control /DT/Eltyp/Iflag )'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'P'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Nodal Pressure'
      H3D_KEYWORD_NODAL_SCALAR(I)%COMMENT  = 'Element pressure reported to node'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'DENS'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Nodal Density'
      H3D_KEYWORD_NODAL_SCALAR(I)%COMMENT  = 'Element density reported to node'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'ENER'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Nodal Specific Energy'
      H3D_KEYWORD_NODAL_SCALAR(I)%COMMENT  = 'Element specific energy reported to node'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'TEMP'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Nodal Temperature'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'NVAR1'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Nodal Variable 1'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'NVAR2'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Nodal Variable 2'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'NVAR3'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Nodal Variable 3'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'NVAR4'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Nodal Variable 4'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'NVAR5'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Nodal Variable 5'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'DINER'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Inertia Change'
      H3D_KEYWORD_NODAL_SCALAR(I)%COMMENT  = 'Output of added inertia per nodes'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'FPOT'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Nodal Potential'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'NDMASS'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Non Diagonal Mass Change'
      H3D_KEYWORD_NODAL_SCALAR(I)%COMMENT  = 'Non-dagonal mass variation (see time step control /DT/AMS)'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'DAMA2/NORMAL'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = '%damage(type2 interface) / Normal'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'DAMA2/TANGENT'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = '%damage(type2 interface) / Tangent'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'SCHLI'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Nodal Schlieren'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'STIFR'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Nodal Rotational Stiffness'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'STIF'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Nodal Stiffness'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'VFRAC'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Nodal Volumetric Fraction - 1'
      H3D_KEYWORD_NODAL_SCALAR(I)%COMMENT  = 'Element volumetric fraction reported to node, used with LAW37 and LAW51'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'VFRAC'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Nodal Volumetric Fraction - 2'
      H3D_KEYWORD_NODAL_SCALAR(I)%COMMENT  = 'Element volumetric fraction reported to node, used with LAW37 and LAW51'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'VFRAC'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Nodal Volumetric Fraction - 3'
      H3D_KEYWORD_NODAL_SCALAR(I)%COMMENT  = 'Element volumetric fraction reported to node, used with LAW37 and LAW51'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'VFRAC'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Nodal Volumetric Fraction - 4'
      H3D_KEYWORD_NODAL_SCALAR(I)%COMMENT  = 'Element volumetric fraction reported to node, used with LAW37 and LAW51'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'ZVFRAC'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Nodal Volumetric Fraction - 1'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'ZVFRAC'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Nodal Volumetric Fraction - 2'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'ZVFRAC'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Nodal Volumetric Fraction - 3'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'ZVFRAC'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Nodal Volumetric Fraction - 4'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'ZVOL'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Centroid New Volume'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'ZVOL'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Centroid Old Volume'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'GPS1/P'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'GPS1 Pressure'
      H3D_KEYWORD_NODAL_SCALAR(I)%COMMENT  = 'mean pressure of elements connected to node'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'GPS1/VONM'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'GPS1 Von Mises'
      H3D_KEYWORD_NODAL_SCALAR(I)%COMMENT  = 'mean Von Mises of elements connected to node'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'SKID_LINE'
      H3D_KEYWORD_NODAL_SCALAR(I)%IS_INTER = 1
      H3D_KEYWORD_NODAL_SCALAR(I)%IS_INTER_ALL = 1
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Skid Line For Interface '
      H3D_KEYWORD_NODAL_SCALAR(I)%COMMENT  = 'the ratio between Maximum tangential Force and S*Pmax/sqrt3'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'SSP'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'Nodal Sound Speed'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_SCALAR(I)%KEY3  = 'INTERNAL.ID'
      H3D_KEYWORD_NODAL_SCALAR(I)%TEXT1  = 'debug - Internal identifier'
c-----------------------------------------------

      NKEY=I
      END
