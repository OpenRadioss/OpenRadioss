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
Chd|  Q4RCOOR2                      source/elements/solid_2d/quad4/q4rcoor2.F
Chd|-- called by -----------
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|-- calls ---------------
Chd|        Q4RROTA2                      source/elements/solid_2d/quad4/q4rrota2.F
Chd|====================================================================
      SUBROUTINE Q4RCOOR2(
     1   X,       NCP,     Y1,      Y2,
     2   Y3,      Y4,      Z1,      Z2,
     3   Z3,      Z4,      NC1,     NC2,
     4   NC3,     NC4,     NGL,     MAT,
     5   NGEO,    VD2,     R11,     R12,
     6   R13,     R21,     R22,     R23,
     7   R31,     R32,     R33,     GAMA,
     8   Y234,    Y124,    VIS,     V,
     9   VY1,     VY2,     VY3,     VY4,
     A   VZ1,     VZ2,     VZ3,     VZ4,
     B   YAVG,    AY,      EXX,     NEL,
     C   ISORTH)
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
      INTEGER, INTENT(IN) :: ISORTH
      INTEGER, INTENT(IN) :: NEL
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*), MAT(*), NGL(*), NGEO(*),
     .        NCP(7,*)
C     REAL
      my_real
     .   X(3,*),Y1(*),Y2(*),Y3(*),Y4(*),Z1(*),Z2(*),Z3(*),Z4(*),VD2(*),
     .    VY1(*),VY2(*),VY3(*),VY4(*),
     .    VZ1(*),VZ2(*),VZ3(*),VZ4(*),
     .   R11(*),R12(*),R13(*),
     .   R21(*),R22(*),R23(*),
     .   R31(*),R32(*),R33(*),GAMA(MVSIZ,6),
     .   Y234(*),Y124(*),VIS(*),V(3,*),YAVG(*),EXX(*),AY(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,IV
      my_real
     .   SY(MVSIZ) , SZ(MVSIZ) ,
     .   TY(MVSIZ) , TZ(MVSIZ) , 
     .   YL , ZL , SUMA,CS,CT,
     .   G22,G23,G32,G33,
     .   T22,T23,T32,T33,VYG
C-----------------------------------------------
      DO I=1,NEL
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
      DO I=1,NEL
        Y1(I)=X(2,NC1(I))
        Z1(I)=X(3,NC1(I))
        Y2(I)=X(2,NC2(I))
        Z2(I)=X(3,NC2(I))
        Y3(I)=X(2,NC3(I))
        Z3(I)=X(3,NC3(I))
        Y4(I)=X(2,NC4(I))
        Z4(I)=X(3,NC4(I))
      END DO
      DO I=1,NEL
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
        DO I=1,NEL
          YAVG(I) = Y1(I)+Y2(I)+Y3(I)+Y4(I)
          VYG = VY1(I)+VY2(I)+VY3(I)+VY4(I)
          Y234(I)=Y2(I)+Y3(I)+Y4(I)
          Y124(I)=Y1(I)+Y2(I)+Y4(I)
C-----constant Exx,
          AY(I) =ONE/MAX(EM20,YAVG(I))
          EXX(I) = VYG*AY(I)
         ENDDO
      ENDIF
C---------------------------------------
C     REPERE LOCAL
C---------------------------------------
      DO I=1,NEL
        SY(I)=HALF*(Y2(I)+Y3(I)-Y1(I)-Y4(I))
        SZ(I)=HALF*(Z2(I)+Z3(I)-Z1(I)-Z4(I))
        TY(I)=HALF*(Y3(I)+Y4(I)-Y1(I)-Y2(I))
        TZ(I)=HALF*(Z3(I)+Z4(I)-Z1(I)-Z2(I))
      END DO
C-----------
C     REPERE CONVECTE : Same than the shell
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
      ENDDO
      DO I=1,NEL
        SUMA=ONE/MAX(SQRT(SY(I)*SY(I)+SZ(I)*SZ(I)),EM20)
        SY(I)=SY(I)*SUMA
        SZ(I)=SZ(I)*SUMA
      ENDDO
      DO I=1,NEL
        R11(I)= ONE
        R21(I)= ZERO
        R31(I)= ZERO
        R12(I)= ZERO
        R22(I)= SY(I)
        R32(I)= SZ(I)
        R13(I)= ZERO
        R23(I)=-SZ(I)
        R33(I)= SY(I)
      END DO
C-----------
      DO I=1,NEL
        YL=R22(I)*Y1(I)+R32(I)*Z1(I)
        ZL=R23(I)*Y1(I)+R33(I)*Z1(I)
        Y1(I)=YL
        Z1(I)=ZL
        YL=R22(I)*Y2(I)+R32(I)*Z2(I)
        ZL=R23(I)*Y2(I)+R33(I)*Z2(I)
        Y2(I)=YL
        Z2(I)=ZL
        YL=R22(I)*Y3(I)+R32(I)*Z3(I)
        ZL=R23(I)*Y3(I)+R33(I)*Z3(I)
        Y3(I)=YL
        Z3(I)=ZL
        YL=R22(I)*Y4(I)+R32(I)*Z4(I)
        ZL=R23(I)*Y4(I)+R33(I)*Z4(I)
        Y4(I)=YL
        Z4(I)=ZL
      END DO
C-----------
C     PASSAGE AU REPERE ORTHOTROPE.
C-----------
      IF (ISORTH /= 0)THEN
        DO I=1,NEL
C         Extration de G tq Xortho=Transpose(G) Xcvt
C                                 =Transpose(G) Transpose(R) Xglobal.
          G22=GAMA(I,2)
          G32=GAMA(I,3)
          G23=GAMA(I,5)
          G33=GAMA(I,6)
C
          YL=G22*Y1(I)+G32*Z1(I)
          ZL=G23*Y1(I)+G33*Z1(I)
          Y1(I)=YL
          Z1(I)=ZL
          YL=G22*Y2(I)+G32*Z2(I)
          ZL=G23*Y2(I)+G33*Z2(I)
          Y2(I)=YL
          Z2(I)=ZL
          YL=G22*Y3(I)+G32*Z3(I)
          ZL=G23*Y3(I)+G33*Z3(I)
          Y3(I)=YL
          Z3(I)=ZL
          YL=G22*Y4(I)+G32*Z4(I)
          ZL=G23*Y4(I)+G33*Z4(I)
          Y4(I)=YL
          Z4(I)=ZL
C         MATRICE DE PASSAGE GLOBAL -> ORTHOTROPE.
          T22=R22(I)*G22+R23(I)*G32
          T23=R22(I)*G23+R23(I)*G33
          T32=R32(I)*G22+R33(I)*G32
          T33=R32(I)*G23+R33(I)*G33
          R22(I)=T22
          R23(I)=T23
          R32(I)=T32
          R33(I)=T33
        ENDDO
      ENDIF
        CALL Q4RROTA2(
     1   R22,     R23,     R32,     R33,
     2   VY1,     VY2,     VY3,     VY4,
     3   VZ1,     VZ2,     VZ3,     VZ4,
     4   NEL)
C-----------
      DO I=1,NEL
        VD2(I)=ZERO
        VIS(I)=ZERO
      END DO
C
      RETURN
      END
