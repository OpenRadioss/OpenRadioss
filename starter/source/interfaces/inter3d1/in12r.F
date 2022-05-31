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
C
Chd|====================================================================
Chd|  IN12R                         source/interfaces/inter3d1/in12r.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IN12R(X,FRIGAP,NSV,NSN,FLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,FLAG
      INTEGER NSV(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*), FRIGAP(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II, I
C     REAL ou REAL*8
      my_real
     .  XX,YY,ZZ,XT,YT,ZT,XC,YC,ZC,XR,YR,ZR,TETA,CS,SN,XCT,YCT,ZCT,
     .  AX,AY,AZ,BX,BY,BZ,BN,
     .  S11,S21,S31,S12,S22,S32,S13,S23,S33
      SAVE S11,S21,S31,S12,S22,S32,S13,S23,S33
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
       TETA=FRIGAP(4)
       CS=COS(TETA)
       SN=SIN(TETA)
       XT=FRIGAP(5)
       YT=FRIGAP(6)
       ZT=FRIGAP(7)
       XC=FRIGAP(8)
       YC=FRIGAP(9)
       ZC=FRIGAP(10)
       XR=FRIGAP(12)
       YR=FRIGAP(13)
       ZR=FRIGAP(14)
       XCT=XC+XT
       YCT=YC+YT
       ZCT=ZC+ZT
C
       IF(FLAG.EQ.1)THEN
       BN=SQRT(YR**2+ZR**2)
        IF(BN.LT.EM3)THEN
         S11=ONE
         S21=ZERO
         S31=ZERO
         S12=ZERO
         S22=CS
         S32=SN
        ELSE
         AX=ONE-XR*XR
         AY=  -XR*YR
         AZ=  -XR*ZR
         BX=YR*AZ-ZR*AY
         BY=ZR*AX-XR*AZ
         BZ=XR*AY-YR*AX
         S11=(CS-ONE)*AX+SN*BX+ONE
         S21=(CS-ONE)*AY+SN*BY
         S31=(CS-ONE)*AZ+SN*BZ
         BN=SQRT(S11**2+S21**2+S31**2)
         S11=S11/BN
         S21=S21/BN
         S31=S31/BN
         AX=  -XR*XR
         AY=ONE-XR*YR
         AZ=  -XR*ZR
         BX=YR*AZ-ZR*AY
         BY=ZR*AX-XR*AZ
         BZ=XR*AY-YR*AX
         S12=(CS-ONE)*AX+SN*BX
         S22=(CS-ONE)*AY+SN*BY+ONE
         S32=(CS-ONE)*AZ+SN*BZ
         BN=SQRT(S12**2+S22**2+S32**2)
         S12=S12/BN
         S22=S22/BN
         S32=S32/BN
        ENDIF
        S13= S21*S32-S31*S22
        S23= S31*S12-S11*S32
        S33= S11*S22-S21*S12
         BN=SQRT(S13**2+S23**2+S33**2)
         S13=S13/BN
         S23=S23/BN
         S33=S33/BN
C
C TRANSFORMATION TEMPORAIRE POUR TROUVER LES RELATIONS main SECND
        DO II=1,NSN
         I=NSV(II)
         XX=X(1,I)-XC
         YY=X(2,I)-YC
         ZZ=X(3,I)-ZC
         X(1,I)=S11*XX+S12*YY+S13*ZZ+XCT
         X(2,I)=S21*XX+S22*YY+S23*ZZ+YCT
         X(3,I)=S31*XX+S32*YY+S33*ZZ+ZCT
        ENDDO
       ELSE
C RETOUR COORDONNEES ORIGINALES
        DO II=1,NSN
         I=NSV(II)
         XX=X(1,I)-XCT
         YY=X(2,I)-YCT
         ZZ=X(3,I)-ZCT
         X(1,I)=S11*XX+S21*YY+S31*ZZ+XC
         X(2,I)=S12*XX+S22*YY+S32*ZZ+YC
         X(3,I)=S13*XX+S23*YY+S33*ZZ+ZC
        ENDDO
        FRIGAP(5)=S11
        FRIGAP(6)=S21
        FRIGAP(7)=S31
        FRIGAP(8)=S12
        FRIGAP(9)=S22
        FRIGAP(10)=S32
        FRIGAP(12)=S13
        FRIGAP(13)=S23
        FRIGAP(14)=S33
       ENDIF
C
      RETURN
      END
