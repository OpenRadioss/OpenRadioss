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
Chd|  CDERII                        source/elements/shell/coque/cderii.F
Chd|-- called by -----------
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|        INIRIG_MAT                    source/elements/initia/inirig_mat.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CDERII(PX1 ,PX2 ,PY1 ,PY2 ,
     .                  X1G ,X2G ,X3G ,X4G ,Y1G ,Y2G  , 
     .                  Y3G ,Y4G ,Z1G ,Z2G ,Z3G ,Z4G  ,
     .                  E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .                  X2L ,X3L ,X4L ,Y2L ,Y3L ,Y4L)
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
      my_real, DIMENSION(MVSIZ), INTENT(IN)  :: 
     .         E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .         X1G,X2G,X3G,X4G,Y1G,Y2G,Y3G,Y4G,Z1G,Z2G,Z3G,Z4G
c
      my_real, DIMENSION(MVSIZ), INTENT(OUT) :: PX1,PX2,PY1,PY2,
     .         X2L,X3L,X4L,Y2L,Y3L,Y4L
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real, DIMENSION(MVSIZ) :: X21G,Y21G,Z21G,X31G,Y31G,Z31G,X41G,
     .                             Y41G,Z41G,Z2L,Z3L,Z4L
C=======================================================================
      DO I=LFT,LLT
        X21G(I) = X2G(I)-X1G(I)
        Y21G(I) = Y2G(I)-Y1G(I)
        Z21G(I) = Z2G(I)-Z1G(I)
        X31G(I) = X3G(I)-X1G(I)
        Y31G(I) = Y3G(I)-Y1G(I)
        Z31G(I) = Z3G(I)-Z1G(I)
        X41G(I) = X4G(I)-X1G(I)
        Y41G(I) = Y4G(I)-Y1G(I)
        Z41G(I) = Z4G(I)-Z1G(I)
      ENDDO
C
      DO I=LFT,LLT
        X2L(I)=E1X(I)*X21G(I)+E1Y(I)*Y21G(I)+E1Z(I)*Z21G(I)
        Y2L(I)=E2X(I)*X21G(I)+E2Y(I)*Y21G(I)+E2Z(I)*Z21G(I)
c        Z2L(I)=E3X(I)*X21G(I)+E3Y(I)*Y21G(I)+E3Z(I)*Z21G(I)
        X3L(I)=E1X(I)*X31G(I)+E1Y(I)*Y31G(I)+E1Z(I)*Z31G(I)
        Y3L(I)=E2X(I)*X31G(I)+E2Y(I)*Y31G(I)+E2Z(I)*Z31G(I)
c        Z3L(I)=E3X(I)*X31G(I)+E3Y(I)*Y31G(I)+E3Z(I)*Z31G(I)
        X4L(I)=E1X(I)*X41G(I)+E1Y(I)*Y41G(I)+E1Z(I)*Z41G(I)
        Y4L(I)=E2X(I)*X41G(I)+E2Y(I)*Y41G(I)+E2Z(I)*Z41G(I)
c        Z4L(I)=E3X(I)*X41G(I)+E3Y(I)*Y41G(I)+E3Z(I)*Z41G(I)
      ENDDO
c
      DO I=LFT,LLT
        PX1(I)= HALF *(Y2L(I)-Y4L(I))
        PX2(I)= HALF * Y3L(I)
        PY1(I)=-HALF *(X2L(I)-X4L(I))
        PY2(I)=-HALF * X3L(I)
      ENDDO
c-----------
      RETURN
      END
