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
Chd|  QROTA_VECT                    source/output/anim/generate/qrota_vect.F
Chd|-- called by -----------
Chd|        H3D_QUAD_VECTOR               source/output/h3d/h3d_results/h3d_quad_vector.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE QROTA_VECT(X, IXQ, KCVT, VECT, GAMA, NEL)
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
      my_real
     .   X(3,*),VECT(3,*),GAMA(6,*)
      INTEGER IXQ(NIXQ,*), KCVT, NEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   R22(MVSIZ),R23(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .   SY(MVSIZ),SZ(MVSIZ),TY(MVSIZ),TZ(MVSIZ),
     .   CT,CS,SUMA,S1,S2,S3
      INTEGER NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ)
      INTEGER I
C-----------------------------------------------
      IF (KCVT == 0) THEN
        DO I=1,NEL
          NC1(I)=IXQ(2,I)
          NC2(I)=IXQ(3,I)
          NC3(I)=IXQ(4,I)
          NC4(I)=IXQ(5,I)
C----------------------------
C     COORDONNEES NODALES
C----------------------------
          Y1(I)=X(2,NC1(I))
          Z1(I)=X(3,NC1(I))
          Y2(I)=X(2,NC2(I))
          Z2(I)=X(3,NC2(I))
          Y3(I)=X(2,NC3(I))
          Z3(I)=X(3,NC3(I))
          Y4(I)=X(2,NC4(I))
          Z4(I)=X(3,NC4(I))
C---------------------------------------
C     REPERE LOCAL (ISOPARAMETRIQUE)
C---------------------------------------
          SY(I)=HALF*(Y2(I)+Y3(I)-Y1(I)-Y4(I))
          SZ(I)=HALF*(Z2(I)+Z3(I)-Z1(I)-Z4(I))
          TY(I)=HALF*(Y3(I)+Y4(I)-Y1(I)-Y2(I))
          TZ(I)=HALF*(Z3(I)+Z4(I)-Z1(I)-Z2(I))
        ENDDO
C-----------
C       REPERE CONVECTE
C-----------
        DO I=1,NEL
          CT = TY(I)*TY(I)+TZ(I)*TZ(I)
          CS = SY(I)*SY(I)+SZ(I)*SZ(I)
          IF(CS /= ZERO) THEN
            SUMA = SQRT(CT/MAX(EM20,CS))
            SY(I) = SY(I)*SUMA + TZ(I)
            SZ(I) = SZ(I)*SUMA - TY(I)
          ELSEIF(CT /= ZERO)THEN
            SUMA = SQRT(CS/MAX(EM20,CT))
            SY(I) = SY(I) + TZ(I)*SUMA
            SZ(I) = SZ(I) - TY(I)*SUMA
          END IF 
          SUMA=ONE/MAX(SQRT(SY(I)**2+SZ(I)**2),EM20)
          SY(I)=SY(I)*SUMA
          SZ(I)=SZ(I)*SUMA
C-----------
C       MATRICE DE PASSAGE GLOBAL -> CONVECTE
C-----------
          R22(I)= SY(I)
          R32(I)=-SZ(I)
          R23(I)= SZ(I)
          R33(I)= SY(I)
        ENDDO
      END IF
c
      DO I=1,NEL
C-----------
C       CHANGEMENT DE REPERE
C-----------
        S1=VECT(1,I)
        S2=VECT(2,I)
        S3=VECT(3,I)
        VECT(1,I)=S2*R22(I)+S3*R23(I)
        VECT(2,I)=S2*R32(I)+S3*R33(I)
        VECT(3,I)=ZERO
      ENDDO
C-----------
      RETURN
      END
