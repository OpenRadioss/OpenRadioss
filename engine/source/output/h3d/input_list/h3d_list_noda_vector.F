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
Chd|  H3D_LIST_NODAL_VECTOR         source/output/h3d/input_list/h3d_list_noda_vector.F
Chd|-- called by -----------
Chd|        LECH3D                        source/output/h3d/h3d_build_fortran/lech3d.F
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE H3D_LIST_NODAL_VECTOR(H3D_KEYWORD_NODAL_VECTOR,GEO,NKEY)
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
      TYPE (H3D_KEYWORD) H3D_KEYWORD_NODAL_VECTOR(*)
      my_real
     .      GEO(NPROPG,*)
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
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'VEL'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'Velocity'
      H3D_KEYWORD_NODAL_VECTOR(I)%COMMENT  = 
     .'Node translational velocity'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'DIS'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'Displacement'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'ACC'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'Acceleration'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'CONT'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'Contact Forces'
      H3D_KEYWORD_NODAL_VECTOR(I)%COMMENT  = 'Node contact force (sum)'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'FINT'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'Internal Forces'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'FEXT'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'External Forces'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'FOPT/FORCE'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'Sect.RBY,Wall F.'
      H3D_KEYWORD_NODAL_VECTOR(I)%COMMENT  = 'Forces for rigid bodies, rigid walls and sections'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'FOPT/MOMENT'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'Sect.RBY Moments'
      H3D_KEYWORD_NODAL_VECTOR(I)%COMMENT  = 'Moments for rigid bodies, rigid walls and sections'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'VROT'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'Rotational Velocity'
      H3D_KEYWORD_NODAL_VECTOR(I)%COMMENT  = 'drot=1 must also be set in /IOFLAG otherwise, rotational dof are not computed'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'FVEL'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'Fluid velocity'
      H3D_KEYWORD_NODAL_VECTOR(I)%COMMENT  ='Gas velocity vectors for fluid flow in Finite Volume Method monitored volume'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'FRES'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'Residual force (FEXT - FINT)'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'PCONT/NORMAL'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'Contact Pressure / Normal'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'PCONT/TANGENT'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'Contact Pressure / Tangent'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'CONT2'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'Tied Contact Forces'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'DROT'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'Rotational DOF'
      H3D_KEYWORD_NODAL_VECTOR(I)%COMMENT  ='drot=1 must also be set in /IOFLAG otherwise, rotational dof are not computed'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'DXANC'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'DXANC'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'GVEL'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'Gaz Velocity'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'FREAC'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'Reaction Forces'
      H3D_KEYWORD_NODAL_VECTOR(I)%COMMENT  = 'Reaction forces for imposed velocities, displacements, accelerations and bcs'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'MREAC'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'Reaction Moments'
      H3D_KEYWORD_NODAL_VECTOR(I)%COMMENT  ='Reaction moments for imposed velocities, displacements, accelerations and bcs'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'CLUSTER/FORCE'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'Cluster Forces'
      H3D_KEYWORD_NODAL_VECTOR(I)%COMMENT  ='Spot weld /CLUSTER force vector in global coordinates'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'CLUSTER/MOMENT'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'Cluster Moments'
      H3D_KEYWORD_NODAL_VECTOR(I)%COMMENT  ='Spot weld /CLUSTER moment vector in global coordinates'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'ZVEL'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'inter22 - Centroid Velocity'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'ZFVEL'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'inter22 - Faces Velocity'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'ZMOM'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'inter22 - Centroid Momentum Density'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'ZFP'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'inter22 - Faces Pressure Forces'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'ZFINT'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'inter22 - Centroid Internal Force'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'CONT/MAX'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'Maximum Contact Forces Over time'
      H3D_KEYWORD_NODAL_VECTOR(I)%COMMENT  = 'Node contact force (Maximum over time)'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'PCONT2/NORMAL'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'Tied Contact Pressure / Normal'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'PCONT2/TANGENT'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'Tied Contact Pressure / Tangent'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'VEL/TMAX'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'Max_Velocity'
      H3D_KEYWORD_NODAL_VECTOR(I)%COMMENT  = 
     .'Maximum node translational velocity over time'
c-----------------------------------------------
      I = I + 1
      H3D_KEYWORD_NODAL_VECTOR(I)%KEY3  = 'DIS/TMAX'
      H3D_KEYWORD_NODAL_VECTOR(I)%TEXT1  = 'Max_Displacement'
      H3D_KEYWORD_NODAL_VECTOR(I)%COMMENT  = 
     .'Maximum node displacement over time'
c-----------------------------------------------


      NKEY=I
      END
