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
Chd|  I24DST3E                      source/interfaces/int24/i24dst3e.F
Chd|-- called by -----------
Chd|        I24MAINF                      source/interfaces/int24/i24main.F
Chd|-- calls ---------------
Chd|        BITGET                        source/interfaces/intsort/i20sto.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24DST3E(
     1                  JLT    ,A      ,X      ,CAND_N ,CAND_E ,
     2                  MBINFLG,ISEADD ,ISEDGE ,NSVG   ,NIN    ,
     3                  IXX    ,STIF   ,
     4                  JLT_NEW,INACTI ,XI     ,YI     ,ZI     ,
     5                  XX     ,YY     ,ZZ     ,PMAX_GAP,
     6                  FSKYI  ,ISKY   ,CAND_T ,FCONT   ,H3D_DATA) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, JLT_NEW,NIN,INACTI, CAND_N(*),NSVG(MVSIZ),
     .        CAND_E(*),MBINFLG(*),ISEDGE(*),ISEADD(*),CAND_T(*)
      INTEGER ISKY(*),IXX(MVSIZ,13)
      my_real
     .     N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PMAX_GAP,
     .     XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .     FSKYI(LSKYI,NFSKYI),X(3,*),A(3,*),FCONT(3,*),
     .     XX(MVSIZ,17),YY(MVSIZ,17),ZZ(MVSIZ,17)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr02_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr11_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, IFSEG,IK1(4),IK2(4),IK14(4),IK5(4),ICONT,
     .        JM,JP,JG,JMG,JPG,NES,IAD,K1,K2,K5,K14,KJ,IFE,IX(4),
     .        NISKYL,M,IG
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ)
      my_real
     .  XJM,YJM,ZJM,XJP,YJP,ZJP,FM(4,4),FSI(4),X1,Y1,Z1,X2,Y2,Z2,F,
     .  XIJM,YIJM,ZIJM,XIJP,YIJP,ZIJP,FX,FY,FZ,
     .  X12,Y12,Z12,X14,Y14,Z14,X15,Y15,Z15,XIJ,YIJ,ZIJ,AAA,BBB,
     .  SXM1,SYM1,SZM1,SXM2,SYM2,SZM2,SXM,SYM,SZM,XM2,XA,XB,DET,
     .  SXSM,SYSM,SZSM,NXIJ,NYIJ,NZIJ,XSM,XS2M2,YS2M2,ZS2M2,
     .  TX12,TY12,TZ12,XJ,YJ,ZJ,DIST,NXSM,NYSM,NZSM,H1M,H2M,
     .  SXS1,SYS1,SZS1,SXS2,SYS2,SZS2,SXS,SYS,SZS,XS2,H1S,H2S,
     .  NX12,NY12,NZ12,TXIJ,TYIJ,TZIJ,F1,F2,UNP,ZEROM,EPS,FI,FJ,FSJ
      INTEGER BITGET
      EXTERNAL BITGET

      DATA IK1 /1,2,3,4/
      DATA IK2 /2,3,4,1/
      DATA IK5 /3,1,0,2/ ! triangle only
      DATA IK14 /14,15,16,17/

      UNP   = ONE + EM4
      ZEROM = ZERO - EM4
      EPS = EM3
c      write(iout,*)'t=',tt,'i24dst3e 1'

      DO I=1,JLT
        IF(CAND_T(I)==0)CYCLE ! no edge candidate
        M=CAND_E(I)
        IF(BITGET(MBINFLG(M),8)==0)CYCLE !flag activ edge MAIN

        DO J = 1,4 
         FSI(J) = ZERO
         DO K = 1,4 
          FM(J,K) = ZERO
         ENDDO
        ENDDO

        ICONT=0

        IF(CAND_N(I)<0)THEN
c afaire !!!!!!!!!!!!!!!!!!!!!!!!!
          stop 987
        ENDIF
        IAD = ISEADD(CAND_N(I))
        NES = ISEDGE(IAD)
        
        IX1(I) = IXX(I,1)
        IX2(I) = IXX(I,2)
        IX3(I) = IXX(I,3)
        IX4(I) = IXX(I,4)

        K5=5

        DO K = 1,4 ! edge loop on segment
          IF(BITGET(MBINFLG(M),K+1)==0)CYCLE !flag edge MAIN

          K1=IK1(K)
          K2=IK2(K)
          K14=IK14(K)
          IF(IX3(I) == IX4(I)) K5=IK5(K)
           
          X1 = XX(I,K1)
          Y1 = YY(I,K1)
          Z1 = ZZ(I,K1)

          X2 = XX(I,K2)
          Y2 = YY(I,K2)
          Z2 = ZZ(I,K2)

          X15 = XX(I,K5) - X1
          Y15 = YY(I,K5) - Y1
          Z15 = ZZ(I,K5) - Z1
 
          X12 = X2 - X1
          Y12 = Y2 - Y1
          Z12 = Z2 - Z1
 
          X14 = XX(I,K14) - X1
          Y14 = YY(I,K14) - Y1
          Z14 = ZZ(I,K14) - Z1
 
c         deux vecteurs surfaces du diedre MAIN
          SXM1 = Y12*Z15 - Z12*Y15
          SYM1 = Z12*X15 - X12*Z15
          SZM1 = X12*Y15 - Y12*X15

          SXM2 = Y14*Z12 - Z14*Y12
          SYM2 = Z14*X12 - X14*Z12
          SZM2 = X14*Y12 - Y14*X12

c check if convex

          AAA = (SYM1*SZM2 - SZM1*SYM2)*X12
     .        + (SZM1*SXM2 - SXM1*SZM2)*Y12             
     .        + (SXM1*SYM2 - SYM1*SXM2)*Z12 

          IF(AAA<=ZERO)CYCLE            

          SXM = SXM1 + SXM2
          SYM = SYM1 + SYM2
          SZM = SZM1 + SZM2

          XM2 = X12*X12+Y12*Y12+Z12*Z12
          AAA = ONE/SQRT(XM2)
          TX12 = X12*AAA             
          TY12 = Y12*AAA             
          TZ12 = Z12*AAA             


          JM=NES
          DO KJ=1,NES ! loop on SECONDARY edge

            JG   = ISEDGE(IAD+KJ)
            JMG  = ISEDGE(IAD+KJ+NES)
            JPG  = ISEDGE(IAD+KJ+NES+NES)

            XJ=X(1,JG)
            YJ=X(2,JG)
            ZJ=X(3,JG)
            
            XJM=X(1,JMG)
            YJM=X(2,JMG)
            ZJM=X(3,JMG)
            
            XJP=X(1,JPG)
            YJP=X(2,JPG)
            ZJP=X(3,JPG)
            
            XIJ = XJ - XI(I)
            YIJ = YJ - YI(I)
            ZIJ = ZJ - ZI(I)

            XIJM = XJM - XI(I)
            YIJM = YJM - YI(I)
            ZIJM = ZJM - ZI(I)

            XIJP = XJP - XI(I)
            YIJP = YJP - YI(I)
            ZIJP = ZJP - ZI(I)

c           two area vectors on SECONDARY dihedral
            SXS1 = YIJ*ZIJM - ZIJ*YIJM
            SYS1 = ZIJ*XIJM - XIJ*ZIJM
            SZS1 = XIJ*YIJM - YIJ*XIJM

            SXS2 = YIJP*ZIJ - ZIJP*YIJ
            SYS2 = ZIJP*XIJ - XIJP*ZIJ
            SZS2 = XIJP*YIJ - YIJP*XIJ

c check if convex

            AAA = (SYS1*SZS2 - SZS1*SYS2)*XIJ
     .          + (SZS1*SXS2 - SXS1*SZS2)*YIJ             
     .          + (SXS1*SYS2 - SYS1*SXS2)*ZIJ 

            IF(AAA<=ZERO)CYCLE 
           
            SXS = SXS1 + SXS2
            SYS = SYS1 + SYS2
            SZS = SZS1 + SZS2

            AAA = SXS*SXM+SYS*SYM+SZS*SZM
            IF(AAA > ZERO) CYCLE ! MAIN and SECONDARY dihedral are not locking each other

c           vecteur SECONDARY MAIN 
            SXSM = Y12*ZIJ - Z12*YIJ
            SYSM = Z12*XIJ - X12*ZIJ
            SZSM = X12*YIJ - Y12*XIJ

            AAA = SXS*SXSM+SYS*SYSM+SZS*SZSM
            IF(AAA < ZERO) THEN
              SXSM = - SXSM
              SYSM = - SYSM
              SZSM = - SZSM              
            ENDIF
            AAA = SXM*SXSM+SYM*SYSM+SZM*SZSM
            IF(AAA > ZERO) CYCLE ! inconsistent

            AAA = SXSM*(XI(I)-X1)
     .          + SYSM*(YI(I)-Y1)
     .          + SZSM*(ZI(I)-Z1)
            AAA = -AAA
            BBB = SXSM*SXSM + SYSM*SYSM + SZSM*SZSM

            IF(AAA >= ZERO)CYCLE ! no intersection
            IF(BBB == ZERO)CYCLE ! // edges

            BBB = ONE/SQRT(BBB)
            DIST = AAA*BBB

            NXSM = SXSM*BBB
            NYSM = SYSM*BBB
            NZSM = SZSM*BBB 

            XS2 = XIJ*XIJ+YIJ*YIJ+ZIJ*ZIJ

            AAA = ONE/SQRT(NXSM*NXSM+NYSM*NYSM+NZSM*NZSM)
            NXSM = NXSM*AAA
            NYSM = NYSM*AAA
            NZSM = NZSM*AAA

            goto 123

C=======================================================================
c check if normal is inside MAIN dihedra

            AAA = TX12*NXSM + TY12*NYSM + TZ12*NZSM
            NX12 = NXSM*AAA
            NY12 = NYSM*AAA
            NZ12 = NZSM*AAA

            NXSM = NXSM - NX12 ! 12 component is removed
            NYSM = NYSM - NY12 ! and resored
            NZSM = NZSM - NZ12 ! later
            
            
            AAA = (SYM1*NZSM - SZM1*NYSM)*TX12
     .          + (SZM1*NXSM - SXM1*NZSM)*TY12             
     .          + (SXM1*NYSM - SYM1*NXSM)*TZ12 

            IF(AAA<=ZERO)THEN
c project on SM1
              AAA = SXM1*SXM1 + SYM1*SYM1 + SZM1*SZM1
              AAA = (SXM1*NXSM + SYM1*NYSM + SZM1*NZSM)/SQRT(AAA)
              NXSM = SXM1*AAA
              NYSM = SYM1*AAA
              NZSM = SZM1*AAA
            ENDIF
            
            AAA = (NYSM*SZM2 - NZSM*SYM2)*TX12
     .          + (NZSM*SXM2 - NXSM*SZM2)*TY12             
     .          + (NXSM*SYM2 - NYSM*SXM2)*TZ12 

            IF(AAA<=ZERO)THEN
c project on SM2
              AAA = SXM2*SXM2 + SYM2*SYM2 + SZM2*SZM2
              AAA = (SXM2*NXSM + SYM2*NYSM + SZM2*NZSM)/SQRT(AAA)
              NXSM = SXM2*AAA
              NYSM = SYM2*AAA
              NZSM = SZM2*AAA
            ENDIF            

            NXSM = -(NXSM + NX12) ! restore
            NYSM = -(NYSM + NY12) ! and
            NZSM = -(NZSM + NZ12) ! invert sign

c check if normal is inside SECONDARY dihedra

            AAA = ONE/SQRT(XS2)
            TXIJ = XIJ*AAA             
            TYIJ = YIJ*AAA             
            TZIJ = ZIJ*AAA             

            AAA = TXIJ*NXSM + TYIJ*NYSM + TZIJ*NZSM
            NXIJ = NXSM*AAA
            NYIJ = NYSM*AAA
            NZIJ = NZSM*AAA

            NXSM = NXSM - NXIJ ! IJ component is removed
            NYSM = NYSM - NYIJ ! and resored
            NZSM = NZSM - NZIJ ! later
            
            
            AAA = (SYS1*NZSM - SZS1*NYSM)*TXIJ
     .          + (SZS1*NXSM - SXS1*NZSM)*TYIJ             
     .          + (SXS1*NYSM - SYS1*NXSM)*TZIJ 

            IF(AAA<=ZERO)THEN
c project on SM1
              AAA = SXS1*SXS1 + SYS1*SYS1 + SZS1*SZS1
              AAA = (SXS1*NXSM + SYS1*NYSM + SZS1*NZSM)/SQRT(AAA)
              NXSM = SXS1*AAA
              NYSM = SYS1*AAA
              NZSM = SZS1*AAA
            ENDIF
            
            AAA = (NYSM*SZS2 - NZSM*SYS2)*TXIJ
     .          + (NZSM*SXS2 - NXSM*SZS2)*TYIJ             
     .          + (NXSM*SYS2 - NYSM*SXS2)*TZIJ 

            IF(AAA<=ZERO)THEN
c project on SM2
              AAA = SXS2*SXS2 + SYS2*SYS2 + SZS2*SZS2
              AAA = (SXS2*NXSM + SYS2*NYSM + SZS2*NZSM)/SQRT(AAA)
              NXSM = SXS2*AAA
              NYSM = SYS2*AAA
              NZSM = SZS2*AAA
            ENDIF            

            NXSM = -(NXSM + NXIJ) ! restore
            NYSM = -(NYSM + NYIJ) ! and
            NZSM = -(NZSM + NZIJ) ! invert sign

C=======================================================================

 123        continue


c compute relative location on MAIN and SECONDARY edge
c same as int 11 (i11dst3.F)
            XSM  = XIJ*X12 + YIJ*Y12 + ZIJ*Z12

            XS2M2 = X2-XJ
            YS2M2 = Y2-YJ 
            ZS2M2 = Z2-ZJ

            XA =  XIJ*XS2M2 + YIJ*YS2M2 + ZIJ*ZS2M2
            XB = -X12*XS2M2 - Y12*YS2M2 - Z12*ZS2M2 
            DET = XM2*XS2 - XSM*XSM
            DET = MAX(EM20,DET)
C
            H1M = (XA*XSM-XB*XS2) / DET

            IF(H1M < -EM3)CYCLE   ! no contact
            IF(H1M > ONE+EM3)CYCLE ! no contact
C
            XS2 = MAX(XS2,EM20)
            XM2 = MAX(XM2,EM20)
            H1M = MIN(ONE,MAX(ZERO,H1M ))
            H1S = -(XA + H1M *XSM) / XS2

            IF(H1S > ONE+EM3)CYCLE        ! no contact
            IF(H1S < HALF-EM3)CYCLE ! edge IJ will be solved as JI

            H1S = MIN(ONE,MAX(ZERO,H1S))

            H1M = -(XB + H1S *XSM) / XM2
            H1M = MIN(ONE,MAX(ZERO,H1M ))
C
            H2S  = ONE -H1S 
            H2M  = ONE -H1M 

            ICONT=1
c compute forces
            F = DIST*STIF(I)
            IF(H1S < HALF+EM3)THEN
              AAA=(H1S-HALF+EM3)/(TWO*EM3)
              F = F*AAA
            ELSEIF(H1S > ONE-EM3)THEN
              AAA=-(H1S-ONE-EM3)/(TWO*EM3)
              F = F*AAA
            ENDIF
            IF(H1M < EM3)THEN
              AAA=(H1M+EM3)/(TWO*EM3)
              F = F*AAA
            ELSEIF(H1M > ONE-EM3)THEN
              AAA=-(H1M-ONE-EM3)/(TWO*EM3)
              F = F*AAA
            ENDIF
            F1=-H1M*F
            F2=-H2M*F
            FI=H1S*F
            FJ=H2S*F

            FM(1,K1) = FM(1,K1) + F1*NXSM
            FM(2,K1) = FM(2,K1) + F1*NYSM
            FM(3,K1) = FM(3,K1) + F1*NZSM
            FM(4,K1) = FM(4,K1) + STIF(I)*ABS(H1M)
            FM(1,K2) = FM(1,K2) + F2*NXSM
            FM(2,K2) = FM(2,K2) + F2*NYSM
            FM(3,K2) = FM(3,K2) + F2*NZSM
            FM(4,K2) = FM(4,K2) + STIF(I)*ABS(H2M)
            FSI(1)   = FSI(1)   + FI*NXSM
            FSI(2)   = FSI(2)   + FI*NYSM
            FSI(3)   = FSI(3)   + FI*NZSM
            FSI(4)   = FSI(4)   + STIF(I)*ABS(H1S)
c save FJ      
            IF(FJ/=ZERO)THEN
#include "lockon.inc"
              NISKY = NISKY + 1
              NISKYL = NISKY
#include "lockoff.inc"
              FX = FJ*NXSM
              FY = FJ*NYSM
              FZ = FJ*NZSM
              FSKYI(NISKYL,1) = FX
              FSKYI(NISKYL,2) = FY
              FSKYI(NISKYL,3) = FZ
              FSKYI(NISKYL,4)=STIF(I)*ABS(H2S)
              ISKY(NISKYL) = JG
              IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT > 0)THEN
                FCONT(1,JG) =FCONT(1,JG) + FX
                FCONT(2,JG) =FCONT(2,JG) + FY
                FCONT(3,JG) =FCONT(3,JG) + FZ
              ENDIF
            ENDIF

          ENDDO ! J=1,NES ! loop on SECONDARY edge     
        ENDDO ! K = 1,4 ! edge loop on segment
c save F(Ki) and FSI
        IF(ICONT==0)CYCLE

        IX(1) = IX1(I)
        IX(2) = IX2(I)
        IX(3) = IX3(I)
        IX(4) = IX4(I)
        DO K=1,4
          IF(FM(4,K)/=ZERO)THEN
#include "lockon.inc"
            NISKY = NISKY + 1
            NISKYL = NISKY
#include "lockoff.inc"
            FSKYI(NISKYL,1)=FM(1,K)
            FSKYI(NISKYL,2)=FM(2,K)
            FSKYI(NISKYL,3)=FM(3,K)
            FSKYI(NISKYL,4)=FM(4,K)
            ISKY(NISKYL) = IX(K)
          ENDIF
        ENDDO
        IF(FSI(4)/=ZERO)THEN
#include "lockon.inc"
            NISKY = NISKY + 1
            NISKYL = NISKY
#include "lockoff.inc"
            FSKYI(NISKYL,1)=FSI(1)
            FSKYI(NISKYL,2)=FSI(2)
            FSKYI(NISKYL,3)=FSI(3)
            FSKYI(NISKYL,4)=FSI(4)
            ISKY(NISKYL) = NSVG(I)
        ENDIF

        IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT > 0)THEN
          DO K=1,4
            IF(FM(4,K)/=ZERO)THEN
              IG=IX(K)
              FCONT(1,IG) = FCONT(1,IG) + FM(1,K)
              FCONT(2,IG) = FCONT(2,IG) + FM(2,K)
              FCONT(3,IG) = FCONT(3,IG) + FM(3,K)
            ENDIF
          ENDDO
          IF(FSI(4)/=ZERO)THEN
              JG=NSVG(I)
              FCONT(1,JG) = FCONT(1,JG) + FSI(1)
              FCONT(2,JG) = FCONT(2,JG) + FSI(2)
              FCONT(3,JG) = FCONT(3,JG) + FSI(3)
          ENDIF
        ENDIF



      ENDDO ! I=1,JLT

c      write(iout,*)'i24dst3e 10'


      RETURN
      END
