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
Chd|  H3D_WRITE_TORSOR              source/output/h3d/h3d_results/h3d_write_torsor.F
Chd|-- called by -----------
Chd|        H3D_ONED_TORSOR               source/output/h3d/h3d_results/h3d_oned_torsor.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE H3D_WRITE_TORSOR(
     .                  IOK_PART  ,IS_WRITTEN ,TORSOR ,NEL ,OFFSET ,NFT,
     .                  VALUE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   TORSOR(9,*),VALUE(9,*)
      INTEGER IOK_PART(*),IS_WRITTEN(*)
      INTEGER OFFSET,NFT,NEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
C
      DO I=1,NEL
        TORSOR(1,OFFSET+NFT+I) = VALUE(1,I)
c        TORSOR(2,OFFSET+NFT+I) = VALUE(2,I)
c        TORSOR(3,OFFSET+NFT+I) = VALUE(3,I)
c        TORSOR(4,OFFSET+NFT+I) = VALUE(4,I)
c        TORSOR(5,OFFSET+NFT+I) = VALUE(5,I)
c        TORSOR(6,OFFSET+NFT+I) = VALUE(6,I)
c        TORSOR(7,OFFSET+NFT+I) = VALUE(7,I)
c        TORSOR(8,OFFSET+NFT+I) = VALUE(8,I)
c        TORSOR(9,OFFSET+NFT+I) = VALUE(9,I)
        IF(IOK_PART(I) == 1 ) IS_WRITTEN(OFFSET+NFT+I)= 1
      ENDDO

      RETURN
      END
