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
Chd|  QROTA3                        source/output/anim/generate/qrota3.F
Chd|-- called by -----------
Chd|        THQUAD                        source/output/th/thquad.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE QROTA3(
     1   X,       IXQ,     KCVT,    TENS,
     2   GAMA,    ISORTH)
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
      INTEGER, INTENT(IN) :: ISORTH
      my_real
     .   X(3,*),TENS(6),GAMA(6)
      INTEGER IXQ(NIXQ), KCVT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   X1, X2, X3, X4,
     .   Y1, Y2, Y3, Y4,
     .   Z1, Z2, Z3, Z4,
     .   L11,L12,L13,L22,L23,L33,
     .   R22,R23,R32,R33,
     .   G22,G33,G23,G32,
     .   T22,T33,T23,T32,
     .   S11,S12,S21,S13,S31,S22,S23,S32,S33,
     .   SY,SZ,TY,TZ,CT,CS,SUMA,
     .   T1,T2,T3,T4,S1,S2,S3,S4
      INTEGER NC1, NC2, NC3, NC4, N, I
C-----------------------------------------------
        NC1=IXQ(2)
        NC2=IXQ(3)
        NC3=IXQ(4)
        NC4=IXQ(5)
C----------------------------
C     COORDONNEES NODALES
C----------------------------
        Y1=X(2,NC1)
        Z1=X(3,NC1)
        Y2=X(2,NC2)
        Z2=X(3,NC2)
        Y3=X(2,NC3)
        Z3=X(3,NC3)
        Y4=X(2,NC4)
        Z4=X(3,NC4)
C-----------
C       REPERE CONVECTE
C-----------
        SY=HALF*(Y2+Y3-Y1-Y4)
        SZ=HALF*(Z2+Z3-Z1-Z4)
        TY=HALF*(Y3+Y4-Y1-Y2)
        TZ=HALF*(Z3+Z4-Z1-Z2)
        CT = TY*TY+TZ*TZ
        CS = SY*SY+SZ*SZ
        IF(CS /= ZERO) THEN
          SUMA = SQRT(CT/MAX(EM20,CS))
          SY = SY*SUMA + TZ
          SZ = SZ*SUMA - TY
        ELSEIF(CT /= ZERO)THEN
          SUMA = SQRT(CS/MAX(EM20,CT))
          SY = SY + TZ*SUMA
          SZ = SZ - TY*SUMA
        END IF 
        SUMA=ONE/MAX(SQRT(SY*SY+SZ*SZ),EM20)
        SY=SY*SUMA
        SZ=SZ*SUMA
C-----------
C       MATRICE DE PASSAGE GLOBAL -> CONVECTE
C-----------
        R22= SY
        R32=-SZ
        R23= SZ
        R33= SY
c
        IF (ISORTH /= 0) THEN
          IF (KCVT == 0) THEN
            G22=GAMA(1)
            G32=GAMA(2)
            G23=GAMA(4)
            G33=GAMA(5)
C-----------
c       MATRICE DE PASSAGE GLOBAL -> ORTHOTROPE.
C-----------
            T22=R22*G22+R23*G32
            T23=R22*G23+R23*G33
            T32=R32*G22+R33*G32
            T33=R32*G23+R33*G33
            R22=T22
            R23=T23
            R32=T32
            R33=T33
          ELSEIF (KCVT /=0) THEN
            G22=GAMA(2)
            G32=GAMA(3)
            G23=GAMA(5)
            G33=GAMA(6)
            T22=R22*G22+R23*G32
            T23=R22*G23+R23*G33
            T32=R32*G22+R33*G32
            T33=R32*G23+R33*G33
C-----------
c       MATRICE DE PASSAGE ORTHOTROPE -> GLOBAL
C-----------
            R22=T22
            R23=T23
            R32=T32
            R33=T33
          ENDIF             
        END IF
C-----------
C       SIZE(TENS)=6 ON STOCKE COMME DES SOLIDES MAIS ON N'UTILISE QUE 1, 2 et 4
C-----------
        S1=TENS(1)
        S2=TENS(2)
        S4=TENS(4)
C-----------
        IF (KCVT == 0) THEN
C-----------
C      Rotation from GLOBAL FRAME TO CONVECTED OR ORTHO 
C-----------
          T1=S1*R22+S4*R23
          T2=S4*R32+S2*R33
          T3=S1*R32+S4*R33
          T4=S4*R22+S2*R23
          TENS(1)=R22*T1+R23*T4
          TENS(2)=R32*T3+R33*T2
          TENS(4)=R22*T3+R23*T2
        ELSE
C-----------
C      Rotation from CONVECTE FRAME OR ORTHO TO GLOBAL FRAME
C-----------
          T1=S1*R22-S4*R23
          T2=-S4*R32+S2*R33
          T3=-S1*R32+S4*R33
          T4=S4*R22-S2*R23
          TENS(1)=R22*T1-R23*T4
          TENS(2)=-R32*T3+R33*T2
          TENS(4)=R22*T3-R23*T2
        ENDIF
C-----------
      RETURN
      END
