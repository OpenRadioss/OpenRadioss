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
Chd|  I21ROT                        source/interfaces/inter3d1/i21rot.F
Chd|-- called by -----------
Chd|        ININT3_THKVAR                 source/interfaces/inter3d1/inint3_thkvar.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I21ROT(IROT   ,XG, ROT, NMN, NOD_NORMAL,
     .                  XM0    ,BRACKET,NRTM,IRECTM,LXM ,
     .                  LYM    ,LZM    ,NSN ,XSAV  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IROT, NMN, NRTM, NSN, IRECTM(4,*)
C     REAL ou REAL*8
      my_real
     .   ROT(9), NOD_NORMAL(3,*), XM0(3,*), BRACKET(3),
     .   LXM, LYM, LZM, XSAV(3,*)
      DOUBLE PRECISION XG(3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IX
C     REAL ou REAL*8
      my_real
     .     XI(3), NXI(3), LMAX, DD, 
     .     X0, Y0, Z0, X1, Y1, Z1, XXX, YYY, ZZZ
C------------------------------------
C     Coordonnees initiales cote main : passage en coord locales.
C------------------------------------
      IF(IROT==0)THEN
C--------------------------------------
        DO I=1,NMN
          DO J=1,3
            XM0(J,I)=XM0(J,I)-XG(J)
          END DO
        END DO
      ELSE
C--------------------------------------
        LMAX=ZERO
        DO I=1,NMN
          DO J=1,3
            XI(J)=XM0(J,I)-XG(J)
          END DO
          XM0(1,I)= ROT(1)*XI(1)+ROT(2)*XI(2)+ROT(3)*XI(3)
          XM0(2,I)= ROT(4)*XI(1)+ROT(5)*XI(2)+ROT(6)*XI(3)
          XM0(3,I)= ROT(7)*XI(1)+ROT(8)*XI(2)+ROT(9)*XI(3)
          DD=XM0(1,I)**2+XM0(2,I)**2+XM0(3,I)**2
          IF(DD > LMAX)THEN
            LMAX=DD
            BRACKET(1)=XM0(1,I)
            BRACKET(2)=XM0(2,I)
            BRACKET(3)=XM0(3,I)
          END IF
          XSAV(1,NSN+2)=
     .       ROT(1)*BRACKET(1)+ROT(4)*BRACKET(2)+ROT(7)*BRACKET(3)
          XSAV(2,NSN+2)=
     .       ROT(2)*BRACKET(1)+ROT(5)*BRACKET(2)+ROT(8)*BRACKET(3)
          XSAV(3,NSN+2)=
     .       ROT(3)*BRACKET(1)+ROT(6)*BRACKET(2)+ROT(9)*BRACKET(3)
          DO J=1,3
            NXI(J)=NOD_NORMAL(J,I)
          END DO
          NOD_NORMAL(1,I)= ROT(1)*NXI(1)+ROT(2)*NXI(2)+ROT(3)*NXI(3)
          NOD_NORMAL(2,I)= ROT(4)*NXI(1)+ROT(5)*NXI(2)+ROT(6)*NXI(3)
          NOD_NORMAL(3,I)= ROT(7)*NXI(1)+ROT(8)*NXI(2)+ROT(9)*NXI(3)
        END DO
C--------------------------------------
        LXM=ZERO
        LYM=ZERO
        LZM=ZERO
        DO I=1,NRTM
          X0=EP30
          X1=-EP30
          Y0=EP30
          Y1=-EP30
          Z0=EP30
          Z1=-EP30
          DO J=1,4
            IX=IRECTM(J,I)
            XXX=XM0(1,IX)
            YYY=XM0(2,IX)
            ZZZ=XM0(3,IX)
            X0=MIN(X0,XXX)
            Y0=MIN(Y0,YYY)
            Z0=MIN(Z0,ZZZ)
            X1=MAX(X1,XXX)
            Y1=MAX(Y1,YYY)
            Z1=MAX(Z1,ZZZ)
          END DO
          LXM=MAX(LXM,X1-X0)
          LYM=MAX(LYM,Y1-Y0)
          LZM=MAX(LZM,Z1-Z0)
        ENDDO
C------------------------------------
      END IF
      RETURN
      END
