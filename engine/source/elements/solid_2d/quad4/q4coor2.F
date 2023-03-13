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
Chd|  Q4COOR2                       source/elements/solid_2d/quad4/q4coor2.F
Chd|-- called by -----------
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE Q4COOR2(X,NCP,
     .                  Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .                  NC1,NC2,NC3,NC4,NGL,MAT,NGEO,
     .                  VD2,VIS,V       ,VY1     ,VY2     ,VY3     ,
     .                 VY4     ,VZ1     ,VZ2     ,VZ3     ,VZ4     ,
     .                 YAVG    ,AY      ,EXX     )                   
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*), MAT(*), NGL(*), NGEO(*),
     .        NCP(7,*)
C     REAL
      my_real
     .   X(3,*),Y1(*),Y2(*),Y3(*),Y4(*),Z1(*),Z2(*),Z3(*),Z4(*),VD2(*),
     .    VY1(*),VY2(*),VY3(*),VY4(*),
     .    VZ1(*),VZ2(*),VZ3(*),VZ4(*),
     .    V(3,*),YAVG(*),EXX(*),AY(*),VIS(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,IV
      my_real
     .   VYG(MVSIZ)
C-----------------------------------------------
      DO I=LFT,LLT
      NGEO(I)=NCP(6,I)
      NGL(I)=NCP(7,I)
      MAT(I)=NCP(1,I)
      NC1(I)=NCP(2,I)
      NC2(I)=NCP(3,I)
      NC3(I)=NCP(4,I)
      NC4(I)=NCP(5,I)
      END DO
C
C----------------------------
C     COORDONNEES NODALES
C----------------------------
      DO I=LFT,LLT
      Y1(I)=X(2,NC1(I))
      Z1(I)=X(3,NC1(I))
      Y2(I)=X(2,NC2(I))
      Z2(I)=X(3,NC2(I))
      Y3(I)=X(2,NC3(I))
      Z3(I)=X(3,NC3(I))
      Y4(I)=X(2,NC4(I))
      Z4(I)=X(3,NC4(I))
      END DO
C
      DO I=LFT,LLT
        VY1(I)=V(2,NC1(I))
        VZ1(I)=V(3,NC1(I))
        VY2(I)=V(2,NC2(I))
        VZ2(I)=V(3,NC2(I))
        VY3(I)=V(2,NC3(I))
        VZ3(I)=V(3,NC3(I))
        VY4(I)=V(2,NC4(I))
        VZ4(I)=V(3,NC4(I))
      END DO
C-----Exx ia calculated anyway in global system
      IF(N2D==1) THEN
        DO I=LFT,LLT
          YAVG(I) = Y1(I)+Y2(I)+Y3(I)+Y4(I)
          VYG(I) = VY1(I)+VY2(I)+VY3(I)+VY4(I)
        ENDDO
C-----JHBE=17 -> petrov G, constant Exx, JHBE=22-> Bubnov
         DO I=LFT,LLT
          AY(I) =ONE/MAX(EM20,YAVG(I))
          EXX(I) = VYG(I)*AY(I)
         ENDDO
      ENDIF
      DO I=LFT,LLT
        VD2(I)=ZERO
        VIS(I)=ZERO
      ENDDO
C
      RETURN
      END
