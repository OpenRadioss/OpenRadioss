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
Chd|  R2COORK3                      source/elements/spring/r2coork3.F
Chd|-- called by -----------
Chd|        R8KE3                         source/elements/spring/r8ke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE R2COORK3(JFT    ,JLT    ,X      ,NCC   ,PID    ,
     2                    NGL    ,R11    ,R12   ,R13    ,R21    ,
     3                    R22    ,R23    ,R31   ,R32    ,R33    ,
     4                    SKEW   ,GEO    ,OFFG  ,OFF    ,AL    ,
     5                    IGEO )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
      INTEGER NCC(NIXR,*),PID(*),NGL(*), IGEO(NPROPGI,*)
C     REAL ou REAL*8
      my_real
     .   X(3,*),OFFG(*)  ,OFF(*),AL(3,*),
     .   R11(*),R12(*),R13(*),
     .   R21(*),R22(*),R23(*),
     .   R31(*),R32(*),R33(*), GEO(NPROPG,*),SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,NC1(MVSIZ),NC2(MVSIZ),ISK
C     REAL ou REAL*8
      my_real
     .   X1(MVSIZ), X2(MVSIZ),Y1(MVSIZ), Y2(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ),X21(MVSIZ),Y21(MVSIZ),Z21(MVSIZ)
C-----------------------------------------------
C
      DO I=JFT, JLT
       PID(I)=NCC(1,I)
       NGL(I)=NCC(5,I)
       NC1(I)=NCC(2,I)
       NC2(I)=NCC(3,I)
      ENDDO
C----------------------------
C     LOCAL SYSTEM
C----------------------------
      DO  I=JFT, JLT
       ISK=IGEO(2,PID(I))
       R11(I)=SKEW(1,ISK)
       R21(I)=SKEW(2,ISK)
       R31(I)=SKEW(3,ISK)
       R12(I)=SKEW(4,ISK)
       R22(I)=SKEW(5,ISK)
       R32(I)=SKEW(6,ISK)
       R13(I)=SKEW(7,ISK)
       R23(I)=SKEW(8,ISK)
       R33(I)=SKEW(9,ISK)
      ENDDO
      DO I=JFT, JLT
       X1(I)=X(1,NC1(I))
       Y1(I)=X(2,NC1(I))
       Z1(I)=X(3,NC1(I))
       X2(I)=X(1,NC2(I))
       Y2(I)=X(2,NC2(I))
       Z2(I)=X(3,NC2(I))
      ENDDO
      DO  I=JFT, JLT
       X21(I)=X2(I)-X1(I)
       Y21(I)=Y2(I)-Y1(I)
       Z21(I)=Z2(I)-Z1(I)
       AL(1,I)=X21(I)*R11(I)+Y21(I)*R21(I)+Z21(I)*R31(I)
       AL(2,I)=X21(I)*R12(I)+Y21(I)*R22(I)+Z21(I)*R32(I)
       AL(3,I)=X21(I)*R13(I)+Y21(I)*R23(I)+Z21(I)*R33(I)
      ENDDO
      DO I=JFT,JLT
       OFF(I)=OFFG(I)
      ENDDO
C
      RETURN
      END
