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
Chd|  S6CKEG3                       source/elements/thickshell/solide6c/s6ckeg3.F
Chd|-- called by -----------
Chd|        S6CLKE3                       source/elements/thickshell/solide6c/s6clke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S6CKEG3(LFT,LLT,
     .   PXYI, PYXI ,P21I, P32I ,PXZI,PZXI ,PYZI,PZYI,
     .   PXYJ, PYXJ ,P21J, P32J ,PXZJ,PZXJ ,PYZJ,PZYJ,
     .   G, KIJ,IS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT,IS
C     REAL
      my_real
     .   PXYI(*), PYXI(*) ,PXZI(*),PZXI(*) ,PYZI(*),PZYI(*),
     .   PXYJ(*), PYXJ(*) ,PXZJ(*),PZXJ(*) ,PYZJ(*),PZYJ(*), 
     .   P21I(*), P32I(*) ,P21J(*), P32J(*) ,G(*), KIJ(3,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
C-----------------------------------------------
C | PXY PYX  0 |
C | P21 PYZ PZY|
C | PXZ P32 PZX|
       DO I=LFT,LLT
        KIJ(1,1,I)=KIJ(1,1,I)+
     .        G(I)*(PXYI(I)*PXYJ(I)+PXZI(I)*PXZJ(I)+P21I(I)*P21J(I))
        KIJ(2,2,I)=KIJ(2,2,I)+
     .        G(I)*(PYXI(I)*PYXJ(I)+PYZI(I)*PYZJ(I)+P32I(I)*P32J(I))
        KIJ(3,3,I)=KIJ(3,3,I)+G(I)*(PZXI(I)*PZXJ(I)+PZYI(I)*PZYJ(I))
        KIJ(1,2,I)=KIJ(1,2,I)+
     .        G(I)*(PXYI(I)*PYXJ(I)+P21I(I)*PYZJ(I)+PXZI(I)*P32J(I))
        KIJ(1,3,I)=KIJ(1,3,I)+
     .        G(I)*(P21I(I)*PZYJ(I)+PXZI(I)*PZXJ(I))
        KIJ(2,3,I)=KIJ(2,3,I)+
     .        G(I)*(PYZI(I)*PZYJ(I)+P32I(I)*PZXJ(I))
       ENDDO
      IF (IS==0) THEN
       DO I=LFT,LLT
        KIJ(2,1,I)=KIJ(2,1,I)+
     .        G(I)*(PXYJ(I)*PYXI(I)+P21J(I)*PYZI(I)+PXZJ(I)*P32I(I))
        KIJ(3,1,I)=KIJ(3,1,I)+
     .        G(I)*(P21J(I)*PZYI(I)+PXZJ(I)*PZXI(I))
        KIJ(3,2,I)=KIJ(3,2,I)+
     .        G(I)*(PYZJ(I)*PZYI(I)+P32J(I)*PZXI(I))
       ENDDO
      ENDIF 
C
      RETURN
      END
