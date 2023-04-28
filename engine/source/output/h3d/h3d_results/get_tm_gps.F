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
Chd|  GET_TM_GPS                    source/output/h3d/h3d_results/h3d_nodal_tensor.F
Chd|-- called by -----------
Chd|        H3D_NODAL_TENSOR              source/output/h3d/h3d_results/h3d_nodal_tensor.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GET_TM_GPS(VALUE,TM_NSIG,NNOD,IOK_PART,IGPSTAG)
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NNOD
      my_real, DIMENSION(6,NNOD) ,INTENT(OUT):: VALUE
      my_real, DIMENSION(NNOD,6) ,INTENT(INOUT):: TM_NSIG
      INTEGER, DIMENSION(NNOD) ,INTENT(IN):: IGPSTAG
      INTEGER, DIMENSION(NNOD) ,INTENT(INOUT):: IOK_PART
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J
      my_real
     .   S(3),NORM2
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
      DO  I=1,NNOD 
        IOK_PART(I) = MIN(IOK_PART(I),IGPSTAG(I))
        IF (IGPSTAG(I)==0) CYCLE
        VALUE(1:6,I) = TM_NSIG(I,1:6)
      ENDDO
C      
      RETURN
      END
