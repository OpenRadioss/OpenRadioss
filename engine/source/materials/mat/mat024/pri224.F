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
Chd|  PRI224                        source/materials/mat/mat024/pri224.F
Chd|-- called by -----------
Chd|        DAMA24                        source/materials/mat/mat024/dama24.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRI224(SIG3D,EPSTOT,EPS,DIR3D,ANG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   SIG3D(*), EPSTOT(*), EPS(*), DIR3D(*), ANG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   SIG(4), DIR(2), VEC(3), CC, BB, CR, SS1, SS2, ORM, S11, S21,
     .   S31, S12, S22, S32, S13, S23, S33, A11, A12, A13, A21, A22,
     .   A23, A31, A32, A33
C-----------------------------------------------
      SIG(1)=SIG3D(2)
      SIG(2)=SIG3D(3)
      SIG(4)=SIG3D(5)
      CC=(SIG(1)+SIG(2))*HALF
      BB=(SIG(1)-SIG(2))*HALF
      CR=SQRT(BB**2+SIG(4)**2)
      SS1=CC+CR
      SS2=CC-CR
C
      DIR(1)=SIG(4)
      DIR(2)=SS1-SIG(1)
      ORM=DIR(1)*DIR(1)+DIR(2)*DIR(2)
      ORM=SQRT(ORM)
      IF(ORM<EM8)THEN
      DIR(1)=ONE
      DIR(2)=ZERO
      ELSE
      DIR(1)=DIR(1)/ORM
      DIR(2)=DIR(2)/ORM
      ENDIF
C . . . . . . . . . . . .
C     ROTATION EPS EPSTOT
C . . . . . . . . . . . .
      S11=ONE
      S21=ZERO
      S31=ZERO
      S12=ZERO    
      S22=DIR(1)
      S32=DIR(2)
      S13=ZERO
      S23=-DIR(2)
      S33=DIR(1)
C
      A11=SIG3D(1)*S11+SIG3D(4)*S21+SIG3D(6)*S31
      A12=SIG3D(1)*S12+SIG3D(4)*S22+SIG3D(6)*S32
      A13=SIG3D(1)*S13+SIG3D(4)*S23+SIG3D(6)*S33
      A21=SIG3D(4)*S11+SIG3D(2)*S21+SIG3D(5)*S31
      A22=SIG3D(4)*S12+SIG3D(2)*S22+SIG3D(5)*S32
      A23=SIG3D(4)*S13+SIG3D(2)*S23+SIG3D(5)*S33
      A31=SIG3D(6)*S11+SIG3D(5)*S21+SIG3D(3)*S31
      A32=SIG3D(6)*S12+SIG3D(5)*S22+SIG3D(3)*S32
      A33=SIG3D(6)*S13+SIG3D(5)*S23+SIG3D(3)*S33
C      SIG3D(1)=S11*A11+S21*A21+S31*A31
C      SIG3D(2)=S12*A12+S22*A22+S32*A32
C      SIG3D(3)=S13*A13+S23*A23+S33*A33
      SIG3D(4)=S11*A12+S21*A22+S31*A32
      SIG3D(5)=S12*A13+S22*A23+S32*A33
      SIG3D(6)=S11*A13+S21*A23+S31*A33
C
      EPS(4)=HALF*EPS(4)
      EPS(5)=HALF*EPS(5)
      EPS(6)=HALF*EPS(6)      
      A11=EPS(1)*S11+EPS(4)*S21+EPS(6)*S31
      A12=EPS(1)*S12+EPS(4)*S22+EPS(6)*S32
      A13=EPS(1)*S13+EPS(4)*S23+EPS(6)*S33
      A21=EPS(4)*S11+EPS(2)*S21+EPS(5)*S31
      A22=EPS(4)*S12+EPS(2)*S22+EPS(5)*S32
      A23=EPS(4)*S13+EPS(2)*S23+EPS(5)*S33
      A31=EPS(6)*S11+EPS(5)*S21+EPS(3)*S31
      A32=EPS(6)*S12+EPS(5)*S22+EPS(3)*S32
      A33=EPS(6)*S13+EPS(5)*S23+EPS(3)*S33
      EPS(1)=S11*A11+S21*A21+S31*A31
      EPS(2)=S12*A12+S22*A22+S32*A32
      EPS(3)=S13*A13+S23*A23+S33*A33
C      EPS(4)=S11*A12+S21*A22+S31*A32
C      EPS(5)=S12*A13+S22*A23+S32*A33
C      EPS(6)=S11*A13+S21*A23+S31*A33
C      EPS(4)=2.*EPS(4)
C      EPS(5)=2.*EPS(5)
C      EPS(6)=2.*EPS(6)
C
      EPSTOT(4)=HALF*EPSTOT(4)
      EPSTOT(5)=HALF*EPSTOT(5)
      EPSTOT(6)=HALF*EPSTOT(6)      
      A11=EPSTOT(1)*S11+EPSTOT(4)*S21+EPSTOT(6)*S31
      A12=EPSTOT(1)*S12+EPSTOT(4)*S22+EPSTOT(6)*S32
      A13=EPSTOT(1)*S13+EPSTOT(4)*S23+EPSTOT(6)*S33
      A21=EPSTOT(4)*S11+EPSTOT(2)*S21+EPSTOT(5)*S31
      A22=EPSTOT(4)*S12+EPSTOT(2)*S22+EPSTOT(5)*S32
      A23=EPSTOT(4)*S13+EPSTOT(2)*S23+EPSTOT(5)*S33
      A31=EPSTOT(6)*S11+EPSTOT(5)*S21+EPSTOT(3)*S31
      A32=EPSTOT(6)*S12+EPSTOT(5)*S22+EPSTOT(3)*S32
      A33=EPSTOT(6)*S13+EPSTOT(5)*S23+EPSTOT(3)*S33
      EPSTOT(1)=S11*A11+S21*A21+S31*A31
      EPSTOT(2)=S12*A12+S22*A22+S32*A32
      EPSTOT(3)=S13*A13+S23*A23+S33*A33
C      EPSTOT(4)=S11*A12+S21*A22+S31*A32
C      EPSTOT(5)=S12*A13+S22*A23+S32*A33
C      EPSTOT(6)=S11*A13+S21*A23+S31*A33
C      EPSTOT(4)=2.*EPSTOT(4)
C      EPSTOT(5)=2.*EPSTOT(5)
C      EPSTOT(6)=2.*EPSTOT(6)
C
      VEC(1)=ANG(2)*ANG(6)-ANG(5)*ANG(3)
      VEC(2)=ANG(3)*ANG(4)-ANG(6)*ANG(1)
      VEC(3)=ANG(1)*ANG(5)-ANG(4)*ANG(2)
C
      DIR3D(1)=DIR(1)*ANG(4)+DIR(2)*VEC(1)
      DIR3D(2)=DIR(1)*ANG(5)+DIR(2)*VEC(2)
      DIR3D(3)=DIR(1)*ANG(6)+DIR(2)*VEC(3)
C
      RETURN
      END
