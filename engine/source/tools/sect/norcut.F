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
Chd|  NORCUT                        source/tools/sect/norcut.F    
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        WRITE_S_C                     ../common_source/tools/input_output/write_routtines.c
Chd|====================================================================
      SUBROUTINE NORCUT(VN,LASTN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "scrcut_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   VN(7,*)
      my_real
     .   S3000
      INTEGER IC,I,INORM(3),IFT,ILT,LASTN(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
      S3000 = THREE1000
C
      DO 100 IC=1,NCUTS
        ILT=LASTN(IC)
        IF(IC==1)THEN
          IFT=1
        ELSE
          IFT=LASTN(IC-1)+1
        ENDIF      
       DO 100 I=IFT,ILT
         INORM(1) = VN(4,IC) * S3000
         INORM(2) = VN(5,IC) * S3000
         INORM(3) = VN(6,IC) * S3000
         CALL WRITE_S_C(INORM,3)
 100  CONTINUE
C-----------------------------------------------
      RETURN
      END
