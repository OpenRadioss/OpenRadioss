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
Chd|  TCOORK3                       source/elements/truss/tcoork3.F
Chd|-- called by -----------
Chd|        TKE3                          source/elements/truss/tke3.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TCOORK3(JFT    ,JLT    ,X      ,NCC   ,MAT    ,
     1                   NGL    ,AL     ,R11    ,R21   ,R31    ,
     2                   OFFG   ,OFF    )
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
      INTEGER NCC(NIXT,*),MAT(*),NGL(*)
      my_real
     .   X(3,*), AL(*),R11(*),R21(*),R31(*) ,OFFG(*),OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,NC1(MVSIZ),NC2(MVSIZ)
      my_real
     .   X1(MVSIZ), X2(MVSIZ),Y1(MVSIZ), Y2(MVSIZ),  
     .   Z1(MVSIZ), Z2(MVSIZ), NORM
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO I=JFT,JLT
       NGL(I)=NCC(5,I)
       MAT(I)=NCC(1,I)
       NC1(I)=NCC(2,I)
       NC2(I)=NCC(3,I)
      ENDDO 
C----------------------------
C     COORDONNEES
C----------------------------
      DO I=JFT,JLT
       X1(I)=X(1,NC1(I))
       Y1(I)=X(2,NC1(I))
       Z1(I)=X(3,NC1(I))
       X2(I)=X(1,NC2(I))
       Y2(I)=X(2,NC2(I))
       Z2(I)=X(3,NC2(I))
      ENDDO 
C----------------------------
C     LOCAL SYSTEM
C----------------------------
      DO  I=JFT,JLT
       R11(I)=X2(I)-X1(I)
       R21(I)=Y2(I)-Y1(I)
       R31(I)=Z2(I)-Z1(I)
       AL(I)=SQRT(R11(I)*R11(I)+R21(I)*R21(I)+R31(I)*R31(I))
       NORM=ONE/AL(I)
       R11(I)=R11(I)*NORM
       R21(I)=R21(I)*NORM
       R31(I)=R31(I)*NORM
      ENDDO 
C----------------------------
C     OFF
C----------------------------
      DO I=JFT,JLT
       OFF(I)  = OFFG(I)
      ENDDO 
C
      RETURN
      END
