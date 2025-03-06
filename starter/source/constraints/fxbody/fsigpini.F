Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    fsigpini   ../starter/source/constraints/fxbody/fsigpini.F
      !||--- called by ------------------------------------------------------
      !||    fxbsini    ../starter/source/constraints/fxbody/fxbsini.F
      !||--- calls      -----------------------------------------------------
      !||    pcurvi     ../starter/source/constraints/fxbody/fsigpini.F
      !||    pdefoi     ../starter/source/constraints/fxbody/fsigpini.F
      !||    pevecii    ../starter/source/constraints/fxbody/fsigpini.F
      !||    pm1inif    ../starter/source/constraints/fxbody/fsigpini.F
      !||====================================================================
      SUBROUTINE FSIGPINI(FXBELM, IPARG , X     , PM, IXP ,
     .                    GEO   , FXBMOD, FXBSIG, R , NELP,
     .                    IBEAM_VECTOR,RBEAM_VECTOR)
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FXBELM(*), IPARG(NPARG,*), IXP(NIXP,*), NELP
      INTEGER, INTENT (IN   ) :: IBEAM_VECTOR(NELP)
      my_real
     .        X(3,*), PM(NPROPM,*), GEO(NPROPG,*), FXBMOD(*),
     .        FXBSIG(*), R(3,*)
      my_real , INTENT (IN   )  :: RBEAM_VECTOR(3,NELP)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IG, OFFSET, LAST, NFT, NFS, I, NG, IEL,
     .        N1, N2
      INTEGER MAT(MVSIZ), PROP(MVSIZ)
      my_real
     .     EE1X(MVSIZ), EE1Y(MVSIZ), EE1Z(MVSIZ),
     .     EE2X(MVSIZ), EE2Y(MVSIZ), EE2Z(MVSIZ),  
     .     EE3X(MVSIZ), EE3Y(MVSIZ), EE3Z(MVSIZ)
      my_real
     .        VL(3,2,MVSIZ), VRL(3,2,MVSIZ)
      my_real
     .        X1(MVSIZ), Y1(MVSIZ), Z1(MVSIZ),
     .        X2(MVSIZ), Y2(MVSIZ), Z2(MVSIZ),
     .        X3(MVSIZ), Y3(MVSIZ), Z3(MVSIZ)
      my_real
     .        E2X, E2Y, E2Z, EE2, RLOC(3,MVSIZ),
     .        D11, D12, D13, D21, D22, D23,
     .        DR11, DR12, DR13, DR21, DR22, DR23,
     .        AL(MVSIZ)
      my_real
     .        FOR(3,MVSIZ), MOM(3,MVSIZ), EINT(2,MVSIZ),
     .        EXX(MVSIZ), EXY(MVSIZ), EXZ(MVSIZ),
     .        KXX(MVSIZ), KYY(MVSIZ), KZZ(MVSIZ)
C-----------------------------------------------
C
      DO IG=1,NELP,MVSIZ
         OFFSET=IG-1
         LAST=MIN(MVSIZ,NELP-OFFSET)
         NFT=OFFSET*9
         NFS=OFFSET*8
         DO I=1,LAST
            NG=FXBELM(NFT+9*(I-1)+1)
            IEL=IPARG(3,NG)+FXBELM(NFT+9*(I-1)+2)
            MAT(I)=IXP(1,IEL)
            PROP(I)=IXP(5,IEL)
            X1(I)=X(1,IXP(2,IEL))
            Y1(I)=X(2,IXP(2,IEL))
            Z1(I)=X(3,IXP(2,IEL))
            X2(I)=X(1,IXP(3,IEL))
            Y2(I)=X(2,IXP(3,IEL))
            Z2(I)=X(3,IXP(3,IEL))
            X3(I)=X(1,IXP(4,IEL))
            Y3(I)=X(2,IXP(4,IEL))
            Z3(I)=X(3,IXP(4,IEL))
            IF (IBEAM_VECTOR(IEL) > 1) THEN
              E2X=RBEAM_VECTOR(1,IEL)
              E2Y=RBEAM_VECTOR(2,IEL)
              E2Z=RBEAM_VECTOR(3,IEL)
            ELSE      
              E2X=X3(I)-X1(I)
              E2Y=Y3(I)-Y1(I)
              E2Z=Z3(I)-Z1(I)
            ENDIF  
            EE2=SQRT(E2X**2+E2Y**2+E2Z**2)
            RLOC(1,I)=E2X/EE2
            RLOC(2,I)=E2Y/EE2
            RLOC(3,I)=E2Z/EE2
            N1=FXBELM(NFT+9*(I-1)+3)
            N2=FXBELM(NFT+9*(I-1)+4)
            D11=FXBMOD(6*(N1-1)+1)
            D12=FXBMOD(6*(N1-1)+2)
            D13=FXBMOD(6*(N1-1)+3)
            D21=FXBMOD(6*(N2-1)+1)
            D22=FXBMOD(6*(N2-1)+2)
            D23=FXBMOD(6*(N2-1)+3)
            VL(1,1,I)=R(1,1)*D11+R(1,2)*D12+R(1,3)*D13
            VL(2,1,I)=R(2,1)*D11+R(2,2)*D12+R(2,3)*D13
            VL(3,1,I)=R(3,1)*D11+R(3,2)*D12+R(3,3)*D13
            VL(1,2,I)=R(1,1)*D21+R(1,2)*D22+R(1,3)*D23
            VL(2,2,I)=R(2,1)*D21+R(2,2)*D22+R(2,3)*D23
            VL(3,2,I)=R(3,1)*D21+R(3,2)*D22+R(3,3)*D23
            DR11=FXBMOD(6*(N1-1)+4)
            DR12=FXBMOD(6*(N1-1)+5)
            DR13=FXBMOD(6*(N1-1)+6)
            DR21=FXBMOD(6*(N2-1)+4)
            DR22=FXBMOD(6*(N2-1)+5)
            DR23=FXBMOD(6*(N2-1)+6)
            VRL(1,1,I)=R(1,1)*DR11+R(1,2)*DR12+R(1,3)*DR13
            VRL(2,1,I)=R(2,1)*DR11+R(2,2)*DR12+R(2,3)*DR13
            VRL(3,1,I)=R(3,1)*DR11+R(3,2)*DR12+R(3,3)*DR13
            VRL(1,2,I)=R(1,1)*DR21+R(1,2)*DR22+R(1,3)*DR23
            VRL(2,2,I)=R(2,1)*DR21+R(2,2)*DR22+R(2,3)*DR23
            VRL(3,2,I)=R(3,1)*DR21+R(3,2)*DR22+R(3,3)*DR23
            FOR(1,I)=ZERO
            FOR(2,I)=ZERO
            FOR(3,I)=ZERO
            MOM(1,I)=ZERO
            MOM(2,I)=ZERO
            MOM(3,I)=ZERO
         ENDDO
C
         CALL PEVECII(X1,  Y1,  Z1,  X2, Y2, 
     .        Z2,  VRL, RLOC, AL, LAST, 
     .        EE1X, EE1Y, EE1Z, 
     .        EE2X, EE2Y, EE2Z, 
     .        EE3X, EE3Y, EE3Z)
C     
         CALL PDEFOI(VL , EXX , EXY, EXZ, AL, LAST, 
     .        EE1X, EE1Y, EE1Z, 
     .        EE2X, EE2Y, EE2Z, 
     .        EE3X, EE3Y, EE3Z)
         CALL PCURVI(VRL, GEO , KXX , KYY , KZZ ,
     .        EXY , EXZ , AL  , LAST, PROP, 
     .        EE1X, EE1Y, EE1Z, 
     .        EE2X, EE2Y, EE2Z, 
     .        EE3X, EE3Y, EE3Z)
C
         CALL PM1INIF(PM,   FOR, MOM , EINT, GEO,
     .                EXX,  EXY, EXZ , KXX , KYY,
     .                KZZ,  AL , LAST, MAT , PROP) 
C
         DO I=1,LAST
            FXBSIG(NFS+8*(I-1)+1)=FOR(1,I)
            FXBSIG(NFS+8*(I-1)+2)=FOR(2,I)
            FXBSIG(NFS+8*(I-1)+3)=FOR(3,I)
            FXBSIG(NFS+8*(I-1)+4)=MOM(1,I)
            FXBSIG(NFS+8*(I-1)+5)=MOM(2,I)
            FXBSIG(NFS+8*(I-1)+6)=MOM(3,I)
            FXBSIG(NFS+8*(I-1)+7)=EINT(1,I)
            FXBSIG(NFS+8*(I-1)+8)=EINT(2,I)
         ENDDO
      ENDDO
C
      RETURN
      END SUBROUTINE FSIGPINI
      !||====================================================================
      !||    pevecii    ../starter/source/constraints/fxbody/fsigpini.F
      !||--- called by ------------------------------------------------------
      !||    fsigpini   ../starter/source/constraints/fxbody/fsigpini.F
      !||====================================================================
      SUBROUTINE PEVECII(X1 , Y1, Z1  , X2, Y2 ,
     .     Z2 , R , RLOC, AL, NEL, 
     .     E1X, E1Y, E1Z, 
     .     E2X, E2Y, E2Z, 
     .     E3X, E3Y, E3Z)
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
      INTEGER :: NEL
      my_real
     .     X1(*), Y1(*), Z1(*), X2(*), Y2(*), Z2(*),
     .     R(3,2,*), RLOC(3,*), AL(*),
     .     E1X(*), E1Y(*), E1Z(*),
     .     E2X(*), E2Y(*), E2Z(*),  
     .     E3X(*), E3Y(*), E3Z(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .     RX1G(MVSIZ), RY1G(MVSIZ), RZ1G(MVSIZ),
     .     RX2G(MVSIZ), RY2G(MVSIZ), RZ2G(MVSIZ),
     .     RX1(MVSIZ),
     .     RX2(MVSIZ),
     .     THETA, SUM2(MVSIZ), SUM3(MVSIZ), SUM(MVSIZ),
     .     COST(MVSIZ), SINT(MVSIZ)

C
      DO I=1,NEL
         RX1G(I)=R(1,1,I)
         RY1G(I)=R(2,1,I)
         RZ1G(I)=R(3,1,I)
         RX2G(I)=R(1,2,I)
         RY2G(I)=R(2,2,I)
         RZ2G(I)=R(3,2,I)
      ENDDO
C
      DO I=1,NEL
         E2X(I)=RLOC(1,I)
         E2Y(I)=RLOC(2,I)
         E2Z(I)=RLOC(3,I)
      ENDDO
C
      DO I=1,NEL
         E1X(I)=X2(I)-X1(I)
         E1Y(I)=Y2(I)-Y1(I)
         E1Z(I)=Z2(I)-Z1(I)
      ENDDO
C
      DO I=1,NEL
         AL(I)=SQRT(E1X(I)**2+E1Y(I)**2+E1Z(I)**2)
      ENDDO
C
      DO I=1,NEL
         E1X(I)=E1X(I)/AL(I)
         E1Y(I)=E1Y(I)/AL(I)
         E1Z(I)=E1Z(I)/AL(I)
      ENDDO
C
      DO I=1,NEL
         E3X(I)=E1Y(I)*E2Z(I)-E1Z(I)*E2Y(I)
         E3Y(I)=E1Z(I)*E2X(I)-E1X(I)*E2Z(I)
         E3Z(I)=E1X(I)*E2Y(I)-E1Y(I)*E2X(I)
      ENDDO
C
      DO I=1,NEL
         E2X(I)=E3Y(I)*E1Z(I)-E3Z(I)*E1Y(I)
         E2Y(I)=E3Z(I)*E1X(I)-E3X(I)*E1Z(I)
         E2Z(I)=E3X(I)*E1Y(I)-E3Y(I)*E1X(I)
      ENDDO
C--------------------------------------------
C Average torsion in global coordinates
C--------------------------------------------
      DO I=1,NEL
         RX1(I)=E1X(I)*RX1G(I)+E1Y(I)*RY1G(I)+E1Z(I)*RZ1G(I)
         RX2(I)=E1X(I)*RX2G(I)+E1Y(I)*RY2G(I)+E1Z(I)*RZ2G(I)
         THETA=HALF*(RX1(I)+RX2(I))
         SUM2(I)=SQRT(E2X(I)**2+E2Y(I)**2+E2Z(I)**2)
         SUM3(I)=SQRT(E3X(I)**2+E3Y(I)**2+E3Z(I)**2)
         COST(I)=COS(THETA)/SUM2(I)
         SINT(I)=SIN(THETA)/SUM3(I)
      ENDDO
C
      DO I=1,NEL
         E2X(I)=E2X(I)*COST(I)+E3X(I)*SINT(I)
         E2Y(I)=E2Y(I)*COST(I)+E3Y(I)*SINT(I)
         E2Z(I)=E2Z(I)*COST(I)+E3Z(I)*SINT(I)
      ENDDO
C
      DO I=1,NEL
         SUM(I)=SQRT(E2X(I)**2+E2Y(I)**2+E2Z(I)**2)
      ENDDO
C
      DO I=1,NEL
         E2X(I)=E2X(I)/SUM(I)
         E2Y(I)=E2Y(I)/SUM(I)
         E2Z(I)=E2Z(I)/SUM(I)
      ENDDO
C
      DO I=1,NEL
         E3X(I)=E1Y(I)*E2Z(I)-E1Z(I)*E2Y(I)
         E3Y(I)=E1Z(I)*E2X(I)-E1X(I)*E2Z(I)
         E3Z(I)=E1X(I)*E2Y(I)-E1Y(I)*E2X(I)
      ENDDO
C
      DO I=1,NEL
         SUM(I)=SQRT(E3X(I)**2+E3Y(I)**2+E3Z(I)**2)
         E3X(I)=E3X(I)/SUM(I)
         E3Y(I)=E3Y(I)/SUM(I)
         E3Z(I)=E3Z(I)/SUM(I)
      ENDDO
C
      RETURN
      END SUBROUTINE PEVECII
      !||====================================================================
      !||    pdefoi     ../starter/source/constraints/fxbody/fsigpini.F
      !||--- called by ------------------------------------------------------
      !||    fsigpini   ../starter/source/constraints/fxbody/fsigpini.F
      !||====================================================================
      SUBROUTINE PDEFOI(V  , EXX, EXY, EXZ, AL, NEL, 
     .     E1X, E1Y, E1Z, 
     .     E2X, E2Y, E2Z, 
     .     E3X, E3Y, E3Z)            
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
      INTEGER :: NEL
      my_real
     .     V(3,2,*), EXX(*), EXY(*), EXZ(*), AL(*),
     .     E1X(*), E1Y(*), E1Z(*),
     .     E2X(*), E2Y(*), E2Z(*),  
     .     E3X(*), E3Y(*), E3Z(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .        VX1G(MVSIZ), VY1G(MVSIZ), VZ1G(MVSIZ),
     .        VX2G(MVSIZ), VY2G(MVSIZ), VZ2G(MVSIZ),
     .        VX1(MVSIZ), VY1(MVSIZ), VZ1(MVSIZ),
     .        VX2(MVSIZ), VY2(MVSIZ), VZ2(MVSIZ)
C
      DO I=1,NEL
         VX1G(I)=V(1,1,I)
         VY1G(I)=V(2,1,I)
         VZ1G(I)=V(3,1,I)
         VX2G(I)=V(1,2,I)
         VY2G(I)=V(2,2,I)
         VZ2G(I)=V(3,2,I)
      ENDDO
C
      DO I=1,NEL
         VX1(I)=E1X(I)*VX1G(I)+E1Y(I)*VY1G(I)+E1Z(I)*VZ1G(I)
         VY1(I)=E2X(I)*VX1G(I)+E2Y(I)*VY1G(I)+E2Z(I)*VZ1G(I)
         VZ1(I)=E3X(I)*VX1G(I)+E3Y(I)*VY1G(I)+E3Z(I)*VZ1G(I)
         VX2(I)=E1X(I)*VX2G(I)+E1Y(I)*VY2G(I)+E1Z(I)*VZ2G(I)
         VY2(I)=E2X(I)*VX2G(I)+E2Y(I)*VY2G(I)+E2Z(I)*VZ2G(I)
         VZ2(I)=E3X(I)*VX2G(I)+E3Y(I)*VY2G(I)+E3Z(I)*VZ2G(I)
      ENDDO
C
      DO I=1,NEL
         EXX(I)=(VX2(I)-VX1(I))/AL(I)
         EXY(I)=(VY2(I)-VY1(I))/AL(I)
         EXZ(I)=(VZ2(I)-VZ1(I))/AL(I)
      ENDDO
C
      RETURN
      END SUBROUTINE PDEFOI
      !||====================================================================
      !||    pcurvi     ../starter/source/constraints/fxbody/fsigpini.F
      !||--- called by ------------------------------------------------------
      !||    fsigpini   ../starter/source/constraints/fxbody/fsigpini.F
      !||====================================================================
      SUBROUTINE PCURVI(V  , GEO, KXX, KYY, KZZ,
     .     EXY, EXZ, AL , NEL, MGM, 
     .     E1X, E1Y, E1Z, 
     .     E2X, E2Y, E2Z, 
     .     E3X, E3Y, E3Z)      
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NEL, MGM(*)
      my_real
     .     V(3,2,*), GEO(NPROPG,*), KXX(*), KYY(*), KZZ(*),
     .     EXY(*), EXZ(*), AL(*),
     .     E1X(*), E1Y(*), E1Z(*),
     .     E2X(*), E2Y(*), E2Z(*),  
     .     E3X(*), E3Y(*), E3Z(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IG, IRX, IR1Y, IR1Z, IR2Y, IR2Z, IRY, IRZ
      my_real
     .        RX1G(MVSIZ), RY1G(MVSIZ), RZ1G(MVSIZ),
     .        RX2G(MVSIZ), RY2G(MVSIZ), RZ2G(MVSIZ),
     .        RX1(MVSIZ), RY1(MVSIZ), RZ1(MVSIZ),
     .        RX2(MVSIZ), RY2(MVSIZ), RZ2(MVSIZ),
     .        RXAV(MVSIZ), RYAV(MVSIZ), RZAV(MVSIZ)
C
      DO I=1,NEL
         RX1G(I)=V(1,1,I)
         RY1G(I)=V(2,1,I)
         RZ1G(I)=V(3,1,I)
         RX2G(I)=V(1,2,I)
         RY2G(I)=V(2,2,I)
         RZ2G(I)=V(3,2,I)
      ENDDO
C
      DO I=1,NEL
         RX1(I)=E1X(I)*RX1G(I)+E1Y(I)*RY1G(I)+E1Z(I)*RZ1G(I)
         RY1(I)=E2X(I)*RX1G(I)+E2Y(I)*RY1G(I)+E2Z(I)*RZ1G(I)
         RZ1(I)=E3X(I)*RX1G(I)+E3Y(I)*RY1G(I)+E3Z(I)*RZ1G(I)
         RX2(I)=E1X(I)*RX2G(I)+E1Y(I)*RY2G(I)+E1Z(I)*RZ2G(I)
         RY2(I)=E2X(I)*RX2G(I)+E2Y(I)*RY2G(I)+E2Z(I)*RZ2G(I)
         RZ2(I)=E3X(I)*RX2G(I)+E3Y(I)*RY2G(I)+E3Z(I)*RZ2G(I)
      ENDDO
C---------------------------------------------------
C Free rotations
C---------------------------------------------------
      DO I=1,NEL
         IG=MGM(I)
         IRX =NINT(GEO(7 ,IG))
         IR1Y=NINT(GEO(8 ,IG))
         IR1Z=NINT(GEO(9 ,IG))
         IR2Y=NINT(GEO(10,IG))
         IR2Z=NINT(GEO(11,IG))
         IRY =MIN(1,IR1Y+IR2Y)
         IRZ =MIN(1,IR1Z+IR2Z)
         RX1(I)=RX1(I)*IRX
         RY1(I)=RY1(I)*IRY
         RZ1(I)=RZ1(I)*IRZ
         RX2(I)=RX2(I)*IRX
         RY2(I)=RY2(I)*IRY
         RZ2(I)=RZ2(I)*IRZ
         EXZ(I)=EXZ(I)*IRY
         EXY(I)=EXY(I)*IRZ
         RY1(I)=IR1Y*RY1(I)
     +          -(ONE -IR1Y)*(THREE_HALF*EXZ(I)+HALF*RY2(I))
         RY2(I)=IR2Y*RY2(I)
     +          -(ONE -IR2Y)*(THREE_HALF*EXZ(I)+HALF*RY1(I))
         RZ1(I)=IR1Z*RZ1(I)
     +          +(ONE-IR1Z)*(THREE_HALF*EXY(I)-HALF*RZ2(I))
         RZ2(I)=IR2Z*RZ2(I)
     +          +(ONE -IR2Z)*(THREE_HALF*EXY(I)-HALF*RZ1(I))
      ENDDO
C
      DO I=1,NEL
         KXX(I)=(RX2(I)-RX1(I))/AL(I)
         KYY(I)=(RY2(I)-RY1(I))/AL(I)
         KZZ(I)=(RZ2(I)-RZ1(I))/AL(I)
      ENDDO
C
      DO I=1,NEL
         RXAV(I)=RX1(I)+RX2(I)
         RZAV(I)=RZ1(I)+RZ2(I)
         RYAV(I)=RY1(I)+RY2(I)
      ENDDO
C
      DO I=1,NEL
         EXZ(I)=EXZ(I) + HALF*RYAV(I)
         EXY(I)=EXY(I) - HALF*RZAV(I)
      ENDDO
C
      RETURN
      END SUBROUTINE PCURVI
      !||====================================================================
      !||    pm1inif    ../starter/source/constraints/fxbody/fsigpini.F
      !||--- called by ------------------------------------------------------
      !||    fsigpini   ../starter/source/constraints/fxbody/fsigpini.F
      !||====================================================================
      SUBROUTINE PM1INIF(PM , FOR, MOM, EINT, GEO,
     .                   EXX, EXY, EXZ, KXX , KYY,
     .                   KZZ, AL , NEL, MAT , MGM)     
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NEL, MAT(*), MGM(*)
      my_real
     .        PM(NPROPM,*), FOR(3,*), MOM(3,*), EINT(2,*),
     .        GEO(NPROPG,*), EXX(*), EXY(*), EXZ(*), KXX(*),
     .        KYY(*), KZZ(*), AL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .        RHO(MVSIZ), G(MVSIZ), YM(MVSIZ), A1(MVSIZ), B1(MVSIZ),
     .        B2(MVSIZ), B3(MVSIZ), SHF(MVSIZ), SH(MVSIZ),
     .        YMA2(MVSIZ), SH10(MVSIZ), SH20(MVSIZ), SH0(MVSIZ),
     .        SH1(MVSIZ), SH2(MVSIZ), DEGMB(MVSIZ), DEGFX(MVSIZ)
C
      DO I=1,NEL
         RHO(I) =PM( 1,MAT(I))
         G(I)   =PM(22,MAT(I))
         YM(I)  =PM(20,MAT(I))
         A1(I)  =GEO(1,MGM(I))
         B1(I)  =GEO(2,MGM(I))
         B2(I)  =GEO(18,MGM(I))
         B3(I)  =GEO(4,MGM(I))
         SHF(I) =GEO(37,MGM(I))
      ENDDO
C
C Transverse shear computed with K1=12EI/L**2 K2=5/6GA
      DO I=1,NEL
         SH(I)=FIVE_OVER_6*G(I)*A1(I)
         YMA2(I)=TWELVE*YM(I)/AL(I)**2
         SH10(I)=YMA2(I)*B1(I)
         SH20(I)=YMA2(I)*B2(I)
         SH0(I)=(ONE-SHF(I))*SH(I)
         SH1(I)=SH0(I)*SH10(I)/(SH(I)+SH10(I)) + SHF(I)*SH10(I)
         SH2(I)=SH0(I)*SH20(I)/(SH(I)+SH20(I)) + SHF(I)*SH20(I)
C
         FOR(1,I)=FOR(1,I)+ EXX(I)*A1(I)*YM(I)
         FOR(2,I)=FOR(2,I)+ EXY(I)*SH2(I)
         FOR(3,I)=FOR(3,I)+ EXZ(I)*SH1(I)
         MOM(1,I)=MOM(1,I)+ KXX(I)*G(I)*B3(I)
         MOM(2,I)=MOM(2,I)+ KYY(I)*YM(I)*B1(I)
         MOM(3,I)=MOM(3,I)+ KZZ(I)*YM(I)*B2(I)
      ENDDO
C
      DO I=1,NEL
         DEGMB(I) = FOR(1,I)*EXX(I)+FOR(2,I)*EXY(I)+FOR(3,I)*EXZ(I)
         DEGFX(I) = MOM(1,I)*KXX(I)+MOM(2,I)*KYY(I)+MOM(3,I)*KZZ(I)
      ENDDO
C
      DO I=1,NEL
         EINT(1,I) = DEGMB(I)*AL(I)*HALF
         EINT(2,I) = DEGFX(I)*AL(I)*HALF
      ENDDO
C
      RETURN
      END SUBROUTINE PM1INIF
            
