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
!||    i25dst3_e2s   ../starter/source/interfaces/inter3d1/i25dst3_e2s.F
!||--- called by ------------------------------------------------------
!||    inint3        ../starter/source/interfaces/inter3d1/inint3.F
!||====================================================================
      SUBROUTINE I25DST3_E2S(
     1   JLT   ,IEDGE  ,CAND_S,CAND_M,
     2   N1    ,N2     ,M1    ,M2    ,
     3   XXS1  ,XXS2   ,XYS1  ,XYS2  ,
     4   XZS1   ,XZS2  ,XXM1  ,XXM2  ,XYM1  ,
     5   XYM2   ,XZM1  ,XZM2  ,GAPVE ,PENE  ,
     6   EX    ,EY     ,EZ    ,FX    ,FY     ,
     7   FZ    ,LEDGE  ,IRECT ,X     ,ITAB  ,
     8   E2S_NOD_NORMAL,ADMSR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, IEDGE
      INTEGER CAND_S(*), CAND_M(*), IRECT(4,*), LEDGE(NLEDGE,*), ITAB(*),
     .        N1(MVSIZ),N2(MVSIZ),M1(4,MVSIZ),M2(4,MVSIZ), ADMSR(4,*)
      my_real
     .     XXS1(*), XXS2(*), XYS1(*), XYS2(*), XZS1(*) , XZS2(*), 
     .     XXM1(4,*), XXM2(4,*) , XYM1(4,*), XYM2(4,*), XZM1(4,*), XZM2(4,*),
     .     GAPVE(*), PENE(4,*), X(3,*),
     .     EX(4,MVSIZ), EY(4,MVSIZ), EZ(4,MVSIZ),
     .     FX(MVSIZ), FY(MVSIZ) , FZ(MVSIZ)
      REAL*4 E2S_NOD_NORMAL(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IA, JA, IB, JB, SOL_EDGE, SH_EDGE, K, NJNDX, N4A, 
     .        EJ, I1, I2, I3, I4, I0, IT,IAM,IAS,JAS,ISENS,
     .        IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        JNDX(MVSIZ), I4A(MVSIZ)
      my_real
     .     H1S(4,MVSIZ),H2S(4,MVSIZ),H1M(4,MVSIZ),H2M(4,MVSIZ),
     .     NX(4,MVSIZ),NY(4,MVSIZ),NZ(4,MVSIZ),NN(4,MVSIZ),
     .     XS12(4,MVSIZ),YS12(4,MVSIZ),ZS12(4,MVSIZ),XM12(4,MVSIZ),
     .     YM12(4,MVSIZ),ZM12(4,MVSIZ),XAA,XBB,XS2(4,MVSIZ),XM2(4,MVSIZ),
     .     XSM,XS2M2,YS2,YM2,YSM,YS2M2,ZS2,ZM2,ZSM,ZS2M2,
     .     XX,YY,ZZ,ALS,ALM,DET,AAA,BBB,P1,P2
      my_real
     .     XA0(MVSIZ),XA1(MVSIZ),XA2(MVSIZ),XA3(MVSIZ),XA4(MVSIZ),
     .     YA0(MVSIZ),YA1(MVSIZ),YA2(MVSIZ),YA3(MVSIZ),YA4(MVSIZ),
     .     ZA0(MVSIZ),ZA1(MVSIZ),ZA2(MVSIZ),ZA3(MVSIZ),ZA4(MVSIZ),
     .     XA(5,MVSIZ),YA(5,MVSIZ),ZA(5,MVSIZ)
      my_real
     .     X0A(MVSIZ,4),Y0A(MVSIZ,4),Z0A(MVSIZ,4),
     .     XNA(MVSIZ,4), YNA(MVSIZ,4), ZNA(MVSIZ,4), XNB(MVSIZ,4), YNB(MVSIZ,4), ZNB(MVSIZ,4), PS,
     .     XS, YS, ZS, XM, YM, ZM, DA, DB, CNVX, DA1, DB1, DA2, DB2,
     .     RZERO, RUN, RDIX, REM30, REP30,
     .     ALP,XXS,XYS,XZS,
     .     XI0,YI0,ZI0,XI1,YI1,ZI1,XI2,YI2,ZI2,
     .     SX1,SY1,SZ1,SX2,SY2,SZ2,
     .     LBA(MVSIZ,4),LCA(MVSIZ,4),
     .     NNCX,NNCY,NNCZ,NNCP,DIST,NNNN,PEDG,NEDG,
     .     NM(3),NS(3)
      REAL*4 NNM11(3),NNM22(3),NNS11(3),NNS22(3)
      INTEGER NTRIA(3,4)
      DATA NTRIA/1,2,4,2,4,1,0,0,0,4,1,2/
C-----------------------------------------------
C       F = [A*X1+(1-A)*X2-B*X3-(1-B)*X4]^2 + [..Y..]^2 + [..Z..]^2
C       DF/DA = 0 = (X1-X2)(A(X1-X2)+X2-X4 +B(X4-X3))+...
C       DF/DA = 0 = A(X1-X2)^2 +X2-X4 + B(X1-X2)(X4-X3))+...
C       DF/DA = 0 = A[(X1-X2)^2 + (Y1-Y2)^2 + (Z1-Z2)^2] 
C                 + B[(X1-X2)(X4-X3) + (Y1-Y2)(Y4-Y3) + (Z1-Z2)(Z4-Z3)]
C                 +   (X1-X2)(X2-X4) + (Y1-Y2)(Y2-Y4) + (Z1-Z2)(Z2-Z4) 
C       DF/DB = 0 = (X4-X3)(A(X1-X2)+X2-X4 +B(X4-X3))+...
C       DF/DB = 0 = B[(X4-X3)^2 + (Y4-Y3)^2 + (Z4-Z3)^2] 
C                 + A[(X1-X2)(X4-X3) + (Y1-Y2)(Y4-Y3) + (Z1-Z2)(Z4-Z3)]
C                 +   (X4-X3)(X2-X4) + (Y4-Y3)(Y2-Y4) + (Z4-Z3)(Z2-Z4) 
C       XS2 = [(X1-X2)^2 + (Y1-Y2)^2 + (Z1-Z2)^2]
C       XM2 = [(X4-X3)^2 + (Y4-Y3)^2 + (Z4-Z3)^2]
C       XSM = [(X1-X2)(X4-X3) + (Y1-Y2)(Y4-Y3) + (Z1-Z2)(Z4-Z3)]
C       XA = (X1-X2)(X2-X4) + (Y1-Y2)(Y2-Y4) + (Z1-Z2)(Z2-Z4)
C       XB = (X4-X3)(X2-X4) + (Y4-Y3)(Y2-Y4) + (Z4-Z3)(Z2-Z4)
C       A XS2 + B XSM +   XA = 0
C       A XSM + B XM2 +   XB = 0
C
C       A = -(XA + B XSM)/XS2
C       -(XA + B XSM)*XSM + B XM2*XS2 +   XB*XS2 = 0
C       -B XSM*XSM + B XM2*XS2 +   XB*XS2-XA*XSM  = 0
C       B*(XM2*XS2 - XSM*XSM) = -XB*XS2+XA*XSM  
C       B = (XA*XSM-XB*XS2) / (XM2*XS2 - XSM*XSM)
C       A = (XB*XSM-XA*XM2) / (XM2*XS2 - XSM*XSM)
C
C IF B<0 => B=0
C
C       XS2 = [(X1-X2)^2 + (Y1-Y2)^2 + (Z1-Z2)^2]
C       XA = (X1-X2)(X2-X4) + (Y1-Y2)(Y2-Y4) + (Z1-Z2)(Z2-Z4)
C       A = - XA /XS2
C       B = 0
C
C ELSEIF B>1 => B=1
C
C       B = 1
C       XS2 = [(X1-X2)^2 + (Y1-Y2)^2 + (Z1-Z2)^2]
C       XSM = [(X1-X2)(X4-X3) + (Y1-Y2)(Y4-Y3) + (Z1-Z2)(Z4-Z3)]
C       XA = (X1-X2)(X2-X4) + (Y1-Y2)(Y2-Y4) + (Z1-Z2)(Z2-Z4)
C       A = -(XA + XSM)/XS2
C
C IF A<0 => A=0
C
C
C ELSEIF A>1 => A=1
C
C
      PENE(1:4,1:JLT)=EP20
C
      DO I=1,JLT
        DO EJ=1,4

          IF(EJ==3.AND.M1(EJ,I)==M2(EJ,I)) THEN
            PENE(EJ,I)=ZERO
            CYCLE
          END IF

          XM12(EJ,I) = XXM2(EJ,I)-XXM1(EJ,I)
          YM12(EJ,I) = XYM2(EJ,I)-XYM1(EJ,I)
          ZM12(EJ,I) = XZM2(EJ,I)-XZM1(EJ,I)
          XM2(EJ,I) = XM12(EJ,I)*XM12(EJ,I) + YM12(EJ,I)*YM12(EJ,I) + ZM12(EJ,I)*ZM12(EJ,I)

          XS12(EJ,I) = XXS2(I)-XXS1(I)
          YS12(EJ,I) = XYS2(I)-XYS1(I)
          ZS12(EJ,I) = XZS2(I)-XZS1(I)
          XS2(EJ,I)  = XS12(EJ,I)*XS12(EJ,I) + YS12(EJ,I)*YS12(EJ,I) + ZS12(EJ,I)*ZS12(EJ,I)
          XSM = - (XS12(EJ,I)*XM12(EJ,I) + YS12(EJ,I)*YM12(EJ,I) + ZS12(EJ,I)*ZM12(EJ,I))
          XS2M2 = XXM2(EJ,I)-XXS2(I)
          YS2M2 = XYM2(EJ,I)-XYS2(I)
          ZS2M2 = XZM2(EJ,I)-XZS2(I)

          XAA =  XS12(EJ,I)*XS2M2 + YS12(EJ,I)*YS2M2 + ZS12(EJ,I)*ZS2M2
          XBB = -XM12(EJ,I)*XS2M2 - YM12(EJ,I)*YS2M2 - ZM12(EJ,I)*ZS2M2 
          DET = XM2(EJ,I)*XS2(EJ,I) - XSM*XSM
          DET = MAX(EM20,DET)
C
          H1M(EJ,I) = (XAA*XSM-XBB*XS2(EJ,I)) / DET
          IF(H1M(EJ,I) < -EM03 .OR. H1M(EJ,I) > ONEP03) THEN
            PENE(EJ,I)=ZERO
            CYCLE  ! no contact
          END IF
C
          XS2(EJ,I) = MAX(XS2(EJ,I),EM20)
          XM2(EJ,I) = MAX(XM2(EJ,I),EM20)
          H1M(EJ,I)=MIN(ONE,MAX(ZERO,H1M(EJ,I)))
          H1S(EJ,I) = -(XAA + H1M(EJ,I)*XSM) / XS2(EJ,I)
          IF(H1S(EJ,I) < -EM03 .OR. H1S(EJ,I) > ONEP03) THEN
            PENE(EJ,I)=ZERO
            CYCLE  ! no contact
          END IF

          H1S(EJ,I)=MIN(ONE,MAX(ZERO,H1S(EJ,I)))
          H1M(EJ,I) = -(XBB + H1S(EJ,I)*XSM) / XM2(EJ,I)
          H1M(EJ,I)=MIN(ONE,MAX(ZERO,H1M(EJ,I)))

          H2S(EJ,I) = ONE -H1S(EJ,I)
          H2M(EJ,I) = ONE -H1M(EJ,I)
C !!!!!!!!!!!!!!!!!!!!!!!!!!
C  PENE = GAP^2 - DIST^2 UTILISE POUR TESTER SI NON NUL
C!!!!!!!!!!!!!!!!!!!!!!!!!!!
          NX(EJ,I) = H1S(EJ,I)*XXS1(I)    + H2S(EJ,I)*XXS2(I)
     .     - H1M(EJ,I)*XXM1(EJ,I) - H2M(EJ,I)*XXM2(EJ,I)
          NY(EJ,I) = H1S(EJ,I)*XYS1(I)    + H2S(EJ,I)*XYS2(I)
     .     - H1M(EJ,I)*XYM1(EJ,I) - H2M(EJ,I)*XYM2(EJ,I)
          NZ(EJ,I) = H1S(EJ,I)*XZS1(I)    + H2S(EJ,I)*XZS2(I)
     .     - H1M(EJ,I)*XZM1(EJ,I) - H2M(EJ,I)*XZM2(EJ,I)

          NN(EJ,I) = SQRT(NX(EJ,I)**2 + NY(EJ,I)**2 + NZ(EJ,I)**2)

          NX(EJ,I) = -NX(EJ,I)
          NY(EJ,I) = -NY(EJ,I)
          NZ(EJ,I) = -NZ(EJ,I)
          PENE(EJ,I) = NN(EJ,I)

        ENDDO
      ENDDO
C
      SOL_EDGE =IEDGE/10 ! solids
      SH_EDGE  =IEDGE-10*SOL_EDGE ! shells
C
C     IF(SH_EDGE/=0)THEN
C       DO I=1,JLT
C
C
C         Free edges, looking vs positive normal only
C
C                                  /     S
C                                /     x 
C                            M /           
C                      <------x        Sector with Zero force
C                      n(M)    \     
C                                \
C                                  \      
C         P2=ZERO
C         IF(LEDGE(3,CAND_S(I))==0)
C    .      P2=  NX(I)*FX(I)+NY(I)*FY(I)+NZ(I)*FZ(I)  ! (n(S),MS) > 45 degrees
C
C         IF(P2 > ZERO)PENE(I)=ZERO
C       ENDDO
C     END IF
C

      IF(SOL_EDGE/=0)THEN
       DO I=1,JLT

          IF(PENE(1,I)+PENE(2,I)+PENE(3,I)+PENE(4,I)==ZERO) CYCLE

          IF(IABS(LEDGE(7,CAND_S(I)))/=1)CYCLE

C---------------------------------------
C         Solid on secnd side !
C---------------------------------------
C
C
          IA=LEDGE(1,CAND_S(I))
          JA=LEDGE(2,CAND_S(I))
          IB=LEDGE(3,CAND_S(I))
          JB=LEDGE(4,CAND_S(I))
          IF(IA==0 .OR. IB==0) THEN
            print *,' internal error - i25dst3e'
          END IF

          DO EJ=1,4


            IF(PENE(EJ,I)==ZERO) CYCLE

C
C         Only contact solid/solid 
C            Common normal : contact normal computing using cross product
C            contact normal can't be normal to secondary and main surfaces :
C            using node normal 
C             +- 90 with tolerance ( 10deg)

C Computing cross product :

            PEDG = XM12(EJ,I) *XS12(EJ,I) + YM12(EJ,I) *YS12(EJ,I) + ZM12(EJ,I) *ZS12(EJ,I)
            NEDG = SQRT(XM2(EJ,I)) * SQRT(XS2(EJ,I))
            IF(ABS(PEDG)>ZEP999*NEDG) THEN
              PENE(EJ,I)=ZERO
              CYCLE  ! no contact
            END IF

            NNCX = YS12(EJ,I)*ZM12(EJ,I)- ZS12(EJ,I)*YM12(EJ,I) 
            NNCY = ZS12(EJ,I)*XM12(EJ,I)- ZM12(EJ,I)*XS12(EJ,I)
            NNCZ = XS12(EJ,I)*YM12(EJ,I)- YS12(EJ,I)*XM12(EJ,I)   

            NNCP = SQRT(NNCX*NNCX+NNCY*NNCY+NNCZ*NNCZ)

          
            NNCX = NNCX / MAX(EM30,NNCP)
            NNCY = NNCY / MAX(EM30,NNCP)
            NNCZ = NNCZ / MAX(EM30,NNCP) 

            DIST = NNCX*NX(EJ,I)+NNCY*NY(EJ,I)+NNCZ*NZ(EJ,I)

            NX(EJ,I)    = NNCX * DIST
            NY(EJ,I)    = NNCY * DIST
            NZ(EJ,I)    = NNCZ * DIST 

            NN(EJ,I)=SQRT(NX(EJ,I)**2 + NY(EJ,I)**2 + NZ(EJ,I)**2)
            PENE(EJ,I) = NN(EJ,I)

C Exclusions :

            IAM=CAND_M(I)
            IAS=LEDGE(1,CAND_S(I))
            JAS=LEDGE(2,CAND_S(I))


            NNM11(1:3) = E2S_NOD_NORMAL(1:3,ADMSR(EJ,IAM))
            NNM22(1:3) = E2S_NOD_NORMAL(1:3,ADMSR(MOD(EJ,4)+1,IAM))


            ISENS = 0
            IF(IRECT(JAS,IAS)==N1(I).AND.IRECT(MOD(JAS,4)+1,IAS)==N2(I))THEN
               ISENS= 1
            ELSEIF(IRECT(JAS,IAS)==N2(I).AND.IRECT(MOD(JAS,4)+1,IAS)==N1(I))THEN
               ISENS=-1
            END IF

             IF(ISENS == 1 ) THEN
                NNS11(1:3) = E2S_NOD_NORMAL(1:3,ADMSR(JAS,IAS))
                NNS22(1:3) = E2S_NOD_NORMAL(1:3,ADMSR(MOD(JAS,4)+1,IAS))
             ELSE
                NNS22(1:3) = E2S_NOD_NORMAL(1:3,ADMSR(JAS,IAS))
                NNS11(1:3) = E2S_NOD_NORMAL(1:3,ADMSR(MOD(JAS,4)+1,IAS))
             ENDIF
 
            NM(1:3)=H1M(EJ,I)*NNM11(1:3)+H2M(EJ,I)*NNM22(1:3)
            NS(1:3)=H1S(EJ,I)*NNS11(1:3)+H2S(EJ,I)*NNS22(1:3)


            P1=-(NX(EJ,I)*NM(1)+NY(EJ,I)*NM(2)+NZ(EJ,I)*NM(3))
            P2=  NX(EJ,I)*NS(1)+NY(EJ,I)*NS(2)+NZ(EJ,I)*NS(3)  

            NNNN=NN(EJ,I)

            IF(P2 > EM04*NNNN.OR.P1 > EM04*NNNN)THEN ! Tolerance EM04
              PENE(EJ,I)=ZERO 
            ENDIF

            IF(ABS(P1) <= ZEP2*NNNN .OR. ABS(P2) <= ZEP2*NNNN) THEN ! Tolerance 80deg
              PENE(EJ,I)=ZERO 
              CYCLE  ! no contact
            ENDIF

        ENDDO
       ENDDO
      ENDIF


      IF(SOL_EDGE/=0)THEN !  provisoire pour solides
C-----------Keep this condition for the moment (need?) : increase tolerance ( missing contacts )
C---------------------------------------
        LBA(1:JLT,1:4) = -ONE ! cf test LBA, LCA
        LCA(1:JLT,1:4) = ZERO
C---------------------------------------
C       Extraction des coordonnees et pre-filtrage des candidats vs diedre main
C---------------------------------------
        DO I=1,JLT

C         IF(PENE(I)==ZERO) CYCLE

C---------------------------------------
C         Solid on main side !
C---------------------------------------
C
          IA=CAND_M(I)
C
          IX1(I)=IRECT(1,IA)
          IX2(I)=IRECT(2,IA)
          IX3(I)=IRECT(3,IA)
          IX4(I)=IRECT(4,IA)
C
          XA(1,I)=X(1,IX1(I))
          YA(1,I)=X(2,IX1(I))
          ZA(1,I)=X(3,IX1(I))
          XA(2,I)=X(1,IX2(I))
          YA(2,I)=X(2,IX2(I))
          ZA(2,I)=X(3,IX2(I))
          XA(3,I)=X(1,IX3(I))
          YA(3,I)=X(2,IX3(I))
          ZA(3,I)=X(3,IX3(I))
          XA(4,I)=X(1,IX4(I))
          YA(4,I)=X(2,IX4(I))
          ZA(4,I)=X(3,IX4(I))

          IF(IX3(I)/=IX4(I))THEN
            XA(5,I) = FOURTH*(XA(1,I)+XA(2,I)+XA(3,I)+XA(4,I))
            YA(5,I) = FOURTH*(YA(1,I)+YA(2,I)+YA(3,I)+YA(4,I))
            ZA(5,I) = FOURTH*(ZA(1,I)+ZA(2,I)+ZA(3,I)+ZA(4,I)) 
          ELSE
            XA(5,I) = XA(3,I)
            YA(5,I) = YA(3,I)
            ZA(5,I) = ZA(3,I)
          ENDIF
C
        END DO

        DO I=1,JLT

C         IF(PENE(I)==ZERO) CYCLE
C
          X0A(I,1) = XA(1,I) - XA(5,I)
          Y0A(I,1) = YA(1,I) - YA(5,I)
          Z0A(I,1) = ZA(1,I) - ZA(5,I)
C
          X0A(I,2) = XA(2,I) - XA(5,I)
          Y0A(I,2) = YA(2,I) - YA(5,I)
          Z0A(I,2) = ZA(2,I) - ZA(5,I)
C
          X0A(I,3) = XA(3,I) - XA(5,I)
          Y0A(I,3) = YA(3,I) - YA(5,I)
          Z0A(I,3) = ZA(3,I) - ZA(5,I)
C
          X0A(I,4) = XA(4,I) - XA(5,I)
          Y0A(I,4) = YA(4,I) - YA(5,I)
          Z0A(I,4) = ZA(4,I) - ZA(5,I)
C
          XNA(I,1) = -(Y0A(I,1)*Z0A(I,2) - Z0A(I,1)*Y0A(I,2))
          YNA(I,1) = -(Z0A(I,1)*X0A(I,2) - X0A(I,1)*Z0A(I,2))
          ZNA(I,1) = -(X0A(I,1)*Y0A(I,2) - Y0A(I,1)*X0A(I,2))
C
          IF(IX3(I)/=IX4(I))THEN
C
            XNA(I,2) = -(Y0A(I,2)*Z0A(I,3) - Z0A(I,2)*Y0A(I,3))
            YNA(I,2) = -(Z0A(I,2)*X0A(I,3) - X0A(I,2)*Z0A(I,3))
            ZNA(I,2) = -(X0A(I,2)*Y0A(I,3) - Y0A(I,2)*X0A(I,3))
C
            XNA(I,3) = -(Y0A(I,3)*Z0A(I,4) - Z0A(I,3)*Y0A(I,4))
            YNA(I,3) = -(Z0A(I,3)*X0A(I,4) - X0A(I,3)*Z0A(I,4))
            ZNA(I,3) = -(X0A(I,3)*Y0A(I,4) - Y0A(I,3)*X0A(I,4))
C
            XNA(I,4) = -(Y0A(I,4)*Z0A(I,1) - Z0A(I,4)*Y0A(I,1))
            YNA(I,4) = -(Z0A(I,4)*X0A(I,1) - X0A(I,4)*Z0A(I,1))
            ZNA(I,4) = -(X0A(I,4)*Y0A(I,1) - Y0A(I,4)*X0A(I,1))
C
          ELSE
C
            XNA(I,2) = XNA(I,1)
            YNA(I,2) = YNA(I,1)
            ZNA(I,2) = ZNA(I,1)
C
C           XNA(I,3) = XNA(I,1)
C           YNA(I,3) = YNA(I,1)
C           ZNA(I,3) = ZNA(I,1)
C
            XNA(I,4) = XNA(I,1)
            YNA(I,4) = YNA(I,1)
            ZNA(I,4) = ZNA(I,1)
C
          END IF 
C
        END DO

C---------------------------------------
C         Solid on main side !
C---------------------------------------
        DO I=1,JLT

          IF(IABS(LEDGE(7,CAND_S(I)))==1)CYCLE ! keep condition only Shell/solid 

          DO EJ=1,4

            IF(PENE(EJ,I)==ZERO) CYCLE
            
            IF(IX3(I)==IX4(I))THEN
              IF(EJ==3) CYCLE
              I1=NTRIA(1,EJ)
              I2=NTRIA(2,EJ)
              I3=NTRIA(3,EJ)
              I4=I3
              I0=I3
            ELSE
              I1=EJ
              I2=MOD(EJ  ,4)+1
              I3=MOD(EJ+1,4)+1
              I4=MOD(EJ+2,4)+1
              I0=5
            END IF
C-----------------------------------------
C           Solid on main side !
C-----------------------------------------
C
C
C           Normal to neighboring segment (cf E == Bisector)
            PS=XNA(I,EJ)*EX(EJ,I)+YNA(I,EJ)*EY(EJ,I)+ZNA(I,EJ)*EZ(EJ,I)
            XNB(I,EJ) = -XNA(I,EJ)+TWO*PS*EX(EJ,I)
            YNB(I,EJ) = -YNA(I,EJ)+TWO*PS*EY(EJ,I)
            ZNB(I,EJ) = -ZNA(I,EJ)+TWO*PS*EZ(EJ,I)
C
            XS = H1S(EJ,I)*XXS1(I)    + H2S(EJ,I)*XXS2(I)
            YS = H1S(EJ,I)*XYS1(I)    + H2S(EJ,I)*XYS2(I)
            ZS = H1S(EJ,I)*XZS1(I)    + H2S(EJ,I)*XZS2(I)
            XM = H1M(EJ,I)*XXM1(EJ,I) + H2M(EJ,I)*XXM2(EJ,I)
            YM = H1M(EJ,I)*XYM1(EJ,I) + H2M(EJ,I)*XYM2(EJ,I)
            ZM = H1M(EJ,I)*XZM1(EJ,I) + H2M(EJ,I)*XZM2(EJ,I)
            DA = (XS-XM)*XNA(I,EJ)+(YS-YM)*YNA(I,EJ)+(ZS-ZM)*ZNA(I,EJ)
            DB = (XS-XM)*XNB(I,EJ)+(YS-YM)*YNB(I,EJ)+(ZS-ZM)*ZNB(I,EJ)
C
              CNVX= (XA(I0,I)-XA(I1,I))*XNB(I,EJ)
     .       +(YA(I0,I)-YA(I1,I))*YNB(I,EJ)
     .       +(ZA(I0,I)-ZA(I1,I))*ZNB(I,EJ) 
              IF(CNVX >= ZERO)THEN
              IF(DA <= ZERO .OR.  DB <= ZERO) THEN ! Don't compute a force in the wrong direction.
                PENE(EJ,I)=ZERO
              ENDIF
              ELSE
                IF(DA <= ZERO .AND. DB <= ZERO) PENE(EJ,I)=ZERO
              END IF

          ENDDO
        ENDDO
C---------------------------------------
C       Calculer uniquement candidats filtres par la suite...
C---------------------------------------
        NJNDX = 0
        N4A   = 0
        DO I=1,JLT
C         IF(PENE(I)==ZERO) CYCLE
C---------------------------------------
C         Solid on main side !
C---------------------------------------
          NJNDX=NJNDX+1
          JNDX(NJNDX)=I

          IF(IX3(I)/=IX4(I))THEN
            N4A=N4A+1
            I4A(N4A)=I
          END IF
        END DO
C---------------------------------------
C#include "vectorize.inc"
C       DO K=1,NJNDX
C         I=JNDX(K)
C--------------------------------------
C        Solid on main side !
C--------------------------------------
C
C          vu cas ou S1 (resp S2) est egal au 3eme noeud du triangle main
C                       --------
C          alors qu'il n'y a pas d'auto intersection de la part sur elle meme ::
C                 
C                    
C       M2  ---          M2
C         \    ---          |  
C           \     ---x M3=S1, S2       M3=S1 x----x S2
C             \    |         |
C               \    |         |
C                 \  |         |
C            M1        M1
C
C              Left view           Front view 
C
C         IF((XXS1(I)==XA3(I).AND.XYS1(I)==YA3(I).AND.XZS1(I)==ZA3(I)).OR.
C     .      (XXS1(I)==XA4(I).AND.XYS1(I)==YA4(I).AND.XZS1(I)==ZA4(I)).OR.
C     .      (XXS2(I)==XA3(I).AND.XYS2(I)==YA3(I).AND.XZS2(I)==ZA3(I)).OR.
C     .      (XXS2(I)==XA4(I).AND.XYS2(I)==YA4(I).AND.XZS2(I)==ZA4(I)))PENE(I)=ZERO
C
C       ENDDO
C---------------------------------------
C
#include "vectorize.inc"
        DO K=1,NJNDX
C
C
C---------------------------------------
C         Solid on main side !
C---------------------------------------
          I=JNDX(K)
C
          DA1 = (XXS1(I)-XA(5,I))*XNA(I,1)+(XYS1(I)-YA(5,I))*YNA(I,1)+(XZS1(I)-ZA(5,I))*ZNA(I,1)
          DA2 = (XXS2(I)-XA(5,I))*XNA(I,1)+(XYS2(I)-YA(5,I))*YNA(I,1)+(XZS2(I)-ZA(5,I))*ZNA(I,1)
C
C         On traite y-compris le cas ou S1 ou S2 sont exactement sur la surface 
C         (en dehors du 3eme point du triangle, que l'on peut rater)
          IF(DA1*DA2 <= ZERO)THEN
            ALP=-DA2/SIGN(MAX(EM20,ABS(DA1-DA2)),DA1-DA2)
            XXS=ALP*XXS1(I)+(ONE-ALP)*XXS2(I)
            XYS=ALP*XYS1(I)+(ONE-ALP)*XYS2(I)
            XZS=ALP*XZS1(I)+(ONE-ALP)*XZS2(I)

            XI0 = XA(5,I) - XXS
            YI0 = YA(5,I) - XYS
            ZI0 = ZA(5,I) - XZS
C
            XI1 = XA(1,I) - XXS
            YI1 = YA(1,I) - XYS
            ZI1 = ZA(1,I) - XZS
C
            XI2 = XA(2,I) - XXS
            YI2 = YA(2,I) - XYS
            ZI2 = ZA(2,I) - XZS
C
            SX1 = YI0*ZI1 - ZI0*YI1
            SY1 = ZI0*XI1 - XI0*ZI1
            SZ1 = XI0*YI1 - YI0*XI1
C
            SX2 = YI0*ZI2 - ZI0*YI2
            SY2 = ZI0*XI2 - XI0*ZI2
            SZ2 = XI0*YI2 - YI0*XI2
C
            AAA=ONE/MAX(EM30,XNA(I,1)*XNA(I,1)+YNA(I,1)*YNA(I,1)+ZNA(I,1)*ZNA(I,1))
            LBA(I,1) = -(-(XNA(I,1)*SX2 + YNA(I,1)*SY2 + ZNA(I,1)*SZ2))*AAA
            LCA(I,1) = -( (XNA(I,1)*SX1 + YNA(I,1)*SY1 + ZNA(I,1)*SZ1))*AAA
C
C         ELSE
C           LBA(I,1) = -ONE
C           LCA(I,1) = ZERO
          END IF
C
        ENDDO

#include "vectorize.inc"
        DO K=1,N4A
          I=I4A(K)
C
          DA1 = (XXS1(I)-XA(5,I))*XNA(I,2)+(XYS1(I)-YA(5,I))*YNA(I,2)+(XZS1(I)-ZA(5,I))*ZNA(I,2)
          DA2 = (XXS2(I)-XA(5,I))*XNA(I,2)+(XYS2(I)-YA(5,I))*YNA(I,2)+(XZS2(I)-ZA(5,I))*ZNA(I,2)
C
C         On traite y-compris le cas ou S1 ou S2 sont exactement sur la surface 
C         (en dehors du 3eme point du triangle, que l'on peut rater)
          IF(DA1*DA2 <= ZERO)THEN
            ALP=-DA2/SIGN(MAX(EM20,ABS(DA1-DA2)),DA1-DA2)
            XXS=ALP*XXS1(I)+(ONE-ALP)*XXS2(I)
            XYS=ALP*XYS1(I)+(ONE-ALP)*XYS2(I)
            XZS=ALP*XZS1(I)+(ONE-ALP)*XZS2(I)

            XI0 = XA(5,I) - XXS
            YI0 = YA(5,I) - XYS
            ZI0 = ZA(5,I) - XZS
C
            XI1 = XA(2,I) - XXS
            YI1 = YA(2,I) - XYS
            ZI1 = ZA(2,I) - XZS
C
            XI2 = XA(3,I) - XXS
            YI2 = YA(3,I) - XYS
            ZI2 = ZA(3,I) - XZS
C
            SX1 = YI0*ZI1 - ZI0*YI1
            SY1 = ZI0*XI1 - XI0*ZI1
            SZ1 = XI0*YI1 - YI0*XI1
C
            SX2 = YI0*ZI2 - ZI0*YI2
            SY2 = ZI0*XI2 - XI0*ZI2
            SZ2 = XI0*YI2 - YI0*XI2
C
            AAA=ONE/MAX(EM30,XNA(I,2)*XNA(I,2)+YNA(I,2)*YNA(I,2)+ZNA(I,2)*ZNA(I,2))
            LBA(I,2) = -(-(XNA(I,2)*SX2 + YNA(I,2)*SY2 + ZNA(I,2)*SZ2))*AAA
            LCA(I,2) = -( (XNA(I,2)*SX1 + YNA(I,2)*SY1 + ZNA(I,2)*SZ1))*AAA
C
C    ELSE
C      LBA(I,2) = -ONE
C      LCA(I,2) = ZERO
          END IF
C
        ENDDO

#include "vectorize.inc"
        DO K=1,N4A
          I=I4A(K)
C
          DA1 = (XXS1(I)-XA(5,I))*XNA(I,3)+(XYS1(I)-YA(5,I))*YNA(I,3)+(XZS1(I)-ZA(5,I))*ZNA(I,3)
          DA2 = (XXS2(I)-XA(5,I))*XNA(I,3)+(XYS2(I)-YA(5,I))*YNA(I,3)+(XZS2(I)-ZA(5,I))*ZNA(I,3)
C
C         On traite y-compris le cas ou S1 ou S2 sont exactement sur la surface 
C         (en dehors du 3eme point du triangle, que l'on peut rater)
          IF(DA1*DA2 <= ZERO)THEN
            ALP=-DA2/SIGN(MAX(EM20,ABS(DA1-DA2)),DA1-DA2)
            XXS=ALP*XXS1(I)+(ONE-ALP)*XXS2(I)
            XYS=ALP*XYS1(I)+(ONE-ALP)*XYS2(I)
            XZS=ALP*XZS1(I)+(ONE-ALP)*XZS2(I)

            XI0 = XA(5,I) - XXS
            YI0 = YA(5,I) - XYS
            ZI0 = ZA(5,I) - XZS
C
            XI1 = XA(3,I) - XXS
            YI1 = YA(3,I) - XYS
            ZI1 = ZA(3,I) - XZS
C
            XI2 = XA(4,I) - XXS
            YI2 = YA(4,I) - XYS
            ZI2 = ZA(4,I) - XZS
C
            SX1 = YI0*ZI1 - ZI0*YI1
            SY1 = ZI0*XI1 - XI0*ZI1
            SZ1 = XI0*YI1 - YI0*XI1
C
            SX2 = YI0*ZI2 - ZI0*YI2
            SY2 = ZI0*XI2 - XI0*ZI2
            SZ2 = XI0*YI2 - YI0*XI2
C
            AAA=ONE/MAX(EM30,XNA(I,3)*XNA(I,3)+YNA(I,3)*YNA(I,3)+ZNA(I,3)*ZNA(I,3))
            LBA(I,3) = -(-(XNA(I,3)*SX2 + YNA(I,3)*SY2 + ZNA(I,3)*SZ2))*AAA
            LCA(I,3) = -( (XNA(I,3)*SX1 + YNA(I,3)*SY1 + ZNA(I,3)*SZ1))*AAA
C
C    ELSE
C      LBA(I,3) = -ONE
C      LCA(I,3) = ZERO
          END IF
C
        ENDDO

#include "vectorize.inc"
        DO K=1,N4A
          I=I4A(K)
C
          DA1 = (XXS1(I)-XA(5,I))*XNA(I,4)+(XYS1(I)-YA(5,I))*YNA(I,4)+(XZS1(I)-ZA(5,I))*ZNA(I,4)
          DA2 = (XXS2(I)-XA(5,I))*XNA(I,4)+(XYS2(I)-YA(5,I))*YNA(I,4)+(XZS2(I)-ZA(5,I))*ZNA(I,4)
C
C         On traite y-compris le cas ou S1 ou S2 sont exactement sur la surface 
C         (en dehors du 3eme point du triangle, que l'on peut rater)
          IF(DA1*DA2 <= ZERO)THEN
            ALP=-DA2/SIGN(MAX(EM20,ABS(DA1-DA2)),DA1-DA2)
            XXS=ALP*XXS1(I)+(ONE-ALP)*XXS2(I)
            XYS=ALP*XYS1(I)+(ONE-ALP)*XYS2(I)
            XZS=ALP*XZS1(I)+(ONE-ALP)*XZS2(I)

            XI0 = XA(5,I) - XXS
            YI0 = YA(5,I) - XYS
            ZI0 = ZA(5,I) - XZS
C
            XI1 = XA(4,I) - XXS
            YI1 = YA(4,I) - XYS
            ZI1 = ZA(4,I) - XZS
C
            XI2 = XA(1,I) - XXS
            YI2 = YA(1,I) - XYS
            ZI2 = ZA(1,I) - XZS
C
            SX1 = YI0*ZI1 - ZI0*YI1
            SY1 = ZI0*XI1 - XI0*ZI1
            SZ1 = XI0*YI1 - YI0*XI1
C
            SX2 = YI0*ZI2 - ZI0*YI2
            SY2 = ZI0*XI2 - XI0*ZI2
            SZ2 = XI0*YI2 - YI0*XI2
C
            AAA=ONE/MAX(EM30,XNA(I,4)*XNA(I,4)+YNA(I,4)*YNA(I,4)+ZNA(I,4)*ZNA(I,4))
            LBA(I,4) = -(-(XNA(I,4)*SX2 + YNA(I,4)*SY2 + ZNA(I,4)*SZ2))*AAA
            LCA(I,4) = -( (XNA(I,4)*SX1 + YNA(I,4)*SY1 + ZNA(I,4)*SZ1))*AAA
C
C    ELSE
C      LBA(I,4) = -ONE
C      LCA(I,4) = ZERO
          END IF
C
        ENDDO

C
#include "vectorize.inc"
        DO K=1,NJNDX
          I=JNDX(K)
C
C
C---------------------------------------
C         Solid on main side !
C---------------------------------------
          IF(LBA(I,1) < -EM01 .OR. LCA(I,1) < -EM01 .OR. LBA(I,1)+LCA(I,1) > ONEP01)
     .      PENE(1,I)=ZERO
          IF(LBA(I,2) < -EM01 .OR. LCA(I,2) < -EM01 .OR. LBA(I,2)+LCA(I,2) > ONEP01)
     .      PENE(2,I)=ZERO
          IF(LBA(I,3) < -EM01 .OR. LCA(I,3) < -EM01 .OR. LBA(I,3)+LCA(I,3) > ONEP01)
     .      PENE(3,I)=ZERO
          IF(LBA(I,4) < -EM01 .OR. LCA(I,4) < -EM01 .OR. LBA(I,4)+LCA(I,4) > ONEP01)
     .      PENE(4,I)=ZERO
        ENDDO

      END IF
C
      RETURN
      END
