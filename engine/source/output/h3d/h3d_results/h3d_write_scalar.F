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
Chd|  H3D_WRITE_SCALAR              source/output/h3d/h3d_results/h3d_write_scalar.F
Chd|-- called by -----------
Chd|        H3D_NODAL_SCALAR              source/output/h3d/h3d_results/h3d_nodal_scalar.F
Chd|        H3D_ONED_SCALAR               source/output/h3d/h3d_results/h3d_oned_scalar.F
Chd|        H3D_QUAD_SCALAR               source/output/h3d/h3d_results/h3d_quad_scalar.F
Chd|        H3D_SHELL_SCALAR_1            source/output/h3d/h3d_results/h3d_shell_scalar_1.F
Chd|        H3D_SOLID_SCALAR_1            source/output/h3d/h3d_results/h3d_solid_scalar_1.F
Chd|        H3D_SPH_SCALAR                source/output/h3d/h3d_results/h3d_sph_scalar.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE H3D_WRITE_SCALAR(
     .                  IOK_PART  ,IS_WRITTEN ,SCALAR ,NEL ,OFFSET ,NFT,
     .                  VALUE,IS_WRITTEN_VALUE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   SCALAR(*),VALUE(*)
      INTEGER IOK_PART(*),IS_WRITTEN(*),IS_WRITTEN_VALUE(*)
      INTEGER NEL,OFFSET,NFT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
C
      DO I=1,NEL
        SCALAR(OFFSET+NFT+I) = VALUE(I)
        IF(IOK_PART(I) == 1 .AND. IS_WRITTEN_VALUE(I) == 1) IS_WRITTEN(OFFSET+NFT+I)= 1
        VALUE(I) = ZERO
      ENDDO
 
      RETURN
      END
Chd|====================================================================
Chd|  H3D_WRITE_SCALAR_REAL         source/output/h3d/h3d_results/h3d_write_scalar.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE H3D_WRITE_SCALAR_REAL(
     .                  IOK_PART  ,IS_WRITTEN ,SCALAR ,I ,OFFSET ,NFT,
     .                  VALUE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   SCALAR(*)
      REAL VALUE
      INTEGER IOK_PART(*),IS_WRITTEN(*)
      INTEGER I,OFFSET,NFT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------

      SCALAR(OFFSET+NFT+I) = DBLE(VALUE)
      IF(IOK_PART(I) == 1 ) IS_WRITTEN(OFFSET+NFT+I)= 1
      VALUE = ZERO
 
      RETURN
      END
