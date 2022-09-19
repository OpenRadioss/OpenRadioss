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
Chd|  I15CAN                        source/interfaces/int15/i15can.F
Chd|-- called by -----------
Chd|        I15CMP                        source/interfaces/int15/i15cmp.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE I15CAN(NSI    ,KSI   ,X    ,KSURF  ,IGRSURF ,
     2                  BUFSF  ,NSC  ,KSC    ,NTC   , KTC  ,
     3                  IACTIV )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
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
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSI, NSC, NTC, KSURF, KSI(4,*),
     .        IACTIV(*),KSC(*), KTC(*)
C     REAL
      my_real
     .       X(3,*), BUFSF(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ADRBUF, I, IN1, IN2, IN3, IN4
      my_real
     .   DGR,
     .   XM, YM, ZM, XG, YG, ZG, A, B, C, ROT(9),
     .   AN, BN, CN,
     .   XLMIN, XLMAX, YLMIN, YLMAX, ZLMIN, ZLMAX,
     .   XGMIN, XGMAX, YGMIN, YGMAX, ZGMIN, ZGMAX,
     .   X1, Y1, Z1,
     .   X2, Y2, Z2,
     .   X3, Y3, Z3,
     .   X4, Y4, Z4,
     .   XRMIN, XRMAX, YRMIN, YRMAX, ZRMIN, ZRMAX
C-----------------------------------------------
      ADRBUF=IGRSURF(KSURF)%IAD_BUFR
      DGR=BUFSF(ADRBUF+36)
      XM=BUFSF(ADRBUF+4)
      YM=BUFSF(ADRBUF+5)
      ZM=BUFSF(ADRBUF+6)
      A =BUFSF(ADRBUF+1)
      B =BUFSF(ADRBUF+2)
      C =BUFSF(ADRBUF+3)
C---------------------------------
C       AN=A**DGR
C       BN=B**DGR
C       CN=C**DGR
C       AN=1./AN
C       BN=1./BN
C       CN=1./CN
       DO I=1,9
        ROT(I)=BUFSF(ADRBUF+7+I-1)
       END DO
C----------------------------------
C      Boite englobante de l'ellipsoide dans le repere global.
       XLMIN=-A
       XLMAX= A
       YLMIN=-B
       YLMAX= B
       ZLMIN=-C
       ZLMAX= C
       XGMIN=ROT(1)*XLMIN+ROT(4)*YLMIN+ROT(7)*ZLMIN
       YGMIN=ROT(2)*XLMIN+ROT(5)*YLMIN+ROT(8)*ZLMIN
       ZGMIN=ROT(3)*XLMIN+ROT(6)*YLMIN+ROT(9)*ZLMIN
       XGMAX=XGMIN
       YGMAX=YGMIN
       ZGMAX=ZGMIN
       XG   =ROT(1)*XLMAX+ROT(4)*YLMIN+ROT(7)*ZLMIN
       YG   =ROT(2)*XLMAX+ROT(5)*YLMIN+ROT(8)*ZLMIN
       ZG   =ROT(3)*XLMAX+ROT(6)*YLMIN+ROT(9)*ZLMIN
       IF (XG<XGMIN) XGMIN=XG
       IF (XG>XGMAX) XGMAX=XG
       IF (YG<YGMIN) YGMIN=YG
       IF (YG>YGMAX) YGMAX=YG
       IF (ZG<ZGMIN) ZGMIN=ZG
       IF (ZG>ZGMAX) ZGMAX=ZG
       XG   =ROT(1)*XLMIN+ROT(4)*YLMAX+ROT(7)*ZLMIN
       YG   =ROT(2)*XLMIN+ROT(5)*YLMAX+ROT(8)*ZLMIN
       ZG   =ROT(3)*XLMIN+ROT(6)*YLMAX+ROT(9)*ZLMIN
       IF (XG<XGMIN) XGMIN=XG
       IF (XG>XGMAX) XGMAX=XG
       IF (YG<YGMIN) YGMIN=YG
       IF (YG>YGMAX) YGMAX=YG
       IF (ZG<ZGMIN) ZGMIN=ZG
       IF (ZG>ZGMAX) ZGMAX=ZG
       XG   =ROT(1)*XLMIN+ROT(4)*YLMIN+ROT(7)*ZLMAX
       YG   =ROT(2)*XLMIN+ROT(5)*YLMIN+ROT(8)*ZLMAX
       ZG   =ROT(3)*XLMIN+ROT(6)*YLMIN+ROT(9)*ZLMAX
       IF (XG<XGMIN) XGMIN=XG
       IF (XG>XGMAX) XGMAX=XG
       IF (YG<YGMIN) YGMIN=YG
       IF (YG>YGMAX) YGMAX=YG
       IF (ZG<ZGMIN) ZGMIN=ZG
       IF (ZG>ZGMAX) ZGMAX=ZG
       XG   =ROT(1)*XLMAX+ROT(4)*YLMAX+ROT(7)*ZLMIN
       YG   =ROT(2)*XLMAX+ROT(5)*YLMAX+ROT(8)*ZLMIN
       ZG   =ROT(3)*XLMAX+ROT(6)*YLMAX+ROT(9)*ZLMIN
       IF (XG<XGMIN) XGMIN=XG
       IF (XG>XGMAX) XGMAX=XG
       IF (YG<YGMIN) YGMIN=YG
       IF (YG>YGMAX) YGMAX=YG
       IF (ZG<ZGMIN) ZGMIN=ZG
       IF (ZG>ZGMAX) ZGMAX=ZG
       XG   =ROT(1)*XLMAX+ROT(4)*YLMIN+ROT(7)*ZLMAX
       YG   =ROT(2)*XLMAX+ROT(5)*YLMIN+ROT(8)*ZLMAX
       ZG   =ROT(3)*XLMAX+ROT(6)*YLMIN+ROT(9)*ZLMAX
       IF (XG<XGMIN) XGMIN=XG
       IF (XG>XGMAX) XGMAX=XG
       IF (YG<YGMIN) YGMIN=YG
       IF (YG>YGMAX) YGMAX=YG
       IF (ZG<ZGMIN) ZGMIN=ZG
       IF (ZG>ZGMAX) ZGMAX=ZG
       XG   =ROT(1)*XLMIN+ROT(4)*YLMAX+ROT(7)*ZLMAX
       YG   =ROT(2)*XLMIN+ROT(5)*YLMAX+ROT(8)*ZLMAX
       ZG   =ROT(3)*XLMIN+ROT(6)*YLMAX+ROT(9)*ZLMAX
       IF (XG<XGMIN) XGMIN=XG
       IF (XG>XGMAX) XGMAX=XG
       IF (YG<YGMIN) YGMIN=YG
       IF (YG>YGMAX) YGMAX=YG
       IF (ZG<ZGMIN) ZGMIN=ZG
       IF (ZG>ZGMAX) ZGMAX=ZG
       XG   =ROT(1)*XLMAX+ROT(4)*YLMAX+ROT(7)*ZLMAX
       YG   =ROT(2)*XLMAX+ROT(5)*YLMAX+ROT(8)*ZLMAX
       ZG   =ROT(3)*XLMAX+ROT(6)*YLMAX+ROT(9)*ZLMAX
       IF (XG<XGMIN) XGMIN=XG
       IF (XG>XGMAX) XGMAX=XG
       IF (YG<YGMIN) YGMIN=YG
       IF (YG>YGMAX) YGMAX=YG
       IF (ZG<ZGMIN) ZGMIN=ZG
       IF (ZG>ZGMAX) ZGMAX=ZG
C----------------------------------
       NSC=0
       NTC=0
C------------------------
       DO 110 I=1,NSI
        IF (IACTIV(I)==-1) GOTO 110
        IN1=KSI(1,I)
        X1=X(1,IN1)-XM
        Y1=X(2,IN1)-YM
        Z1=X(3,IN1)-ZM
        IN2=KSI(2,I)
        X2=X(1,IN2)-XM
        Y2=X(2,IN2)-YM
        Z2=X(3,IN2)-ZM
        IN3=KSI(3,I)
        X3=X(1,IN3)-XM
        Y3=X(2,IN3)-YM
        Z3=X(3,IN3)-ZM
        IN4=KSI(4,I)
        IF (IN4/=IN3) THEN
         X4=X(1,IN4)-XM
         Y4=X(2,IN4)-YM
         Z4=X(3,IN4)-ZM
         XRMIN=X1
         XRMAX=X1
         YRMIN=Y1
         YRMAX=Y1
         ZRMIN=Z1
         ZRMAX=Z1
         IF (X2<XRMIN) XRMIN=X2
         IF (X2>XRMAX) XRMAX=X2
         IF (Y2<YRMIN) YRMIN=Y2
         IF (Y2>YRMAX) YRMAX=Y2
         IF (Z2<ZRMIN) ZRMIN=Z2
         IF (Z2>ZRMAX) ZRMAX=Z2
         IF (X3<XRMIN) XRMIN=X3
         IF (X3>XRMAX) XRMAX=X3
         IF (Y3<YRMIN) YRMIN=Y3
         IF (Y3>YRMAX) YRMAX=Y3
         IF (Z3<ZRMIN) ZRMIN=Z3
         IF (Z3>ZRMAX) ZRMAX=Z3
         IF (X4<XRMIN) XRMIN=X4
         IF (X4>XRMAX) XRMAX=X4
         IF (Y4<YRMIN) YRMIN=Y4
         IF (Y4>YRMAX) YRMAX=Y4
         IF (Z4<ZRMIN) ZRMIN=Z4
         IF (Z4>ZRMAX) ZRMAX=Z4
         IF ( .NOT.(    XRMAX<XGMIN.OR.XRMIN>XGMAX
     .                 .OR.YRMAX<YGMIN.OR.YRMIN>YGMAX
     .                 .OR.ZRMAX<ZGMIN.OR.ZRMIN>ZGMAX) ) THEN
              NSC=NSC+1
              KSC(NSC)=I
         ENDIF
        ELSE
C---------------------------------
C        TRIANGLES.
C---------------------------------
         XRMIN=X1
         XRMAX=X1
         YRMIN=Y1
         YRMAX=Y1
         ZRMIN=Z1
         ZRMAX=Z1
         IF (X2<XRMIN) XRMIN=X2
         IF (X2>XRMAX) XRMAX=X2
         IF (Y2<YRMIN) YRMIN=Y2
         IF (Y2>YRMAX) YRMAX=Y2
         IF (Z2<ZRMIN) ZRMIN=Z2
         IF (Z2>ZRMAX) ZRMAX=Z2
         IF (X3<XRMIN) XRMIN=X3
         IF (X3>XRMAX) XRMAX=X3
         IF (Y3<YRMIN) YRMIN=Y3
         IF (Y3>YRMAX) YRMAX=Y3
         IF (Z3<ZRMIN) ZRMIN=Z3
         IF (Z3>ZRMAX) ZRMAX=Z3
         IF ( .NOT.( XRMAX<XGMIN.OR.XRMIN>XGMAX
     .                .OR.YRMAX<YGMIN.OR.YRMIN>YGMAX
     .                .OR.ZRMAX<ZGMIN.OR.ZRMIN>ZGMAX) ) THEN
              NTC=NTC+1
              KTC(NTC)=I
         ENDIF
        ENDIF
110    CONTINUE
C---------------------------------
      RETURN
      END
