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
Chd|  MJWL                          source/materials/mat/mat005/mjwl.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MJWL(PM    ,MAT  ,OFF   ,SIG   ,EINT ,
     1                PSH   ,P0   ,QOLD  ,VOL   ,BFRAC,
     2                VOLN  ,QNEW ,SOLD1 ,SOLD2 ,SOLD3,
     3                DVOL  ,NEL  ,DF    ,ABURN ,
     4                ER1V  ,ER2V ,WDR1V ,WDR2V , W1, AMU)
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
      INTEGER MAT(MVSIZ),NEL
      my_real
     .   PM(NPROPM,*),
     .   OFF(*),SIG(NEL,6),EINT(*),QOLD(*),VOL(*),BFRAC(*),VOLN(MVSIZ),QNEW(*),
     .   PSH(*),P0(*),
     .   DVOL(*),SOLD1(*),SOLD2(*),SOLD3(*), DF(*), ER1V(*), ER2V(*), WDR1V(*), WDR2V(*), W1(*),
     .   ABURN(MVSIZ),ALPHA_UNIT,AMU(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,QOPT
      my_real
     .   EINC(MVSIZ) , ESPE(MVSIZ), W1DF(MVSIZ)
      my_real
     .   TBEGIN, TEND, TT0,
     .   VOLO(MVSIZ),FACM(MVSIZ),POLD(MVSIZ),PNEW(MVSIZ),
     .   EADD, LAMBDA,RR,RR2,A,M,N,XSI, BULK
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO I=LFT,LLT
        DVOL(I)=HALF*DVOL(I)
        POLD(I)=THIRD*(SOLD1(I)+SOLD2(I)+SOLD3(I))
        EINC(I)=DVOL(I)*(POLD(I)-PSH(I)-QOLD(I)-QNEW(I))
        EINT(I)=EINT(I)+EINC(I)
        QOLD(I)=QNEW(I)
        VOLO(I)=VOLN(I)/DF(I)
      ENDDO

      !================      AFTERBURNING     ============================!
      ! computes E <- E + lambda(t)*E_add
      QOPT       = PM(042,MAT(1))
      EADD       = PM(160,MAT(1))
      TBEGIN     = PM(161,MAT(1))
      TEND       = PM(162,MAT(1))
      RR         = PM(163,MAT(1))
      A          = PM(164,MAT(1))
      M          = PM(165,MAT(1))
      N          = PM(166,MAT(1))
      RR2        = PM(167,MAT(1))
      ALPHA_UNIT = PM(168,MAT(1))
      BULK       = PM(044,MAT(1))
      IF(EADD==ZERO)THEN
      !=== no afterburning
      ! EINT(I) = EINT(I)
      ELSEIF(QOPT==0)THEN
      !=== instantaneous release
        DO I=LFT,LLT
          LAMBDA = ZERO
          IF(TT > TEND .AND. ABURN(I)==ONE)THEN
            LAMBDA   = ONE
            ABURN(I) = ONE
          ELSEIF (TT <= TBEGIN)THEN
            LAMBDA   = ZERO
            ABURN(I) = ZERO
          ELSE
            LAMBDA   = ONE
            EINT(I)  = EINT(I)+(LAMBDA-ABURN(I))*EADD*MAX(EM20,VOLO(I))
            ABURN(I) = ONE
          ENDIF
        ENDDO
      ELSEIF(QOPT==1)THEN
      !=== afterburning with constant rate from Tbegin to Tend
        DO I=LFT,LLT
          LAMBDA = ZERO
          IF(TT > TEND .AND. ABURN(I)==ONE)THEN
            LAMBDA   = ONE
            ABURN(I) = ONE
          ELSEIF (TT <= TBEGIN)THEN
            LAMBDA   = ZERO
            ABURN(I) = ZERO
          ELSE
            LAMBDA   = (TT-TBEGIN)*RR
            LAMBDA   = MIN(ONE,LAMBDA)
            EINT(I)  = EINT(I)+(LAMBDA-ABURN(I))*EADD*MAX(EM20,VOLO(I))
            ABURN(I) = LAMBDA
          ENDIF
        ENDDO
      ELSEIF(QOPT==2)THEN
      !=== afterburning with linear rate from Tbegin to Tend
        DO I=LFT,LLT
          LAMBDA = ZERO
          IF(TT > TEND .AND. ABURN(I)==ONE)THEN    ! .AND. ABURN(I)==ONE  needed to add last increment
            LAMBDA   = ONE
            ABURN(I) = ONE
          ELSEIF (TT <= TBEGIN)THEN
            LAMBDA   = ZERO
            ABURN(I) = ZERO
          ELSE
            LAMBDA   = HALF*RR*TT**2 - RR*TBEGIN*TT + RR2
            LAMBDA   = MAX(ZERO,MIN(ONE,LAMBDA))
            EINT(I)  = EINT(I)+(LAMBDA-ABURN(I))*EADD*MAX(EM20,VOLO(I))
            ABURN(I) = LAMBDA
          ENDIF
        ENDDO
      ELSEIF(QOPT==3)THEN
      !=== Miller s extension, rate is depedent on Pressure
        DO I=LFT,LLT
          LAMBDA = ZERO
          IF(-POLD(I)-PSH(I) > ZERO )THEN
            LAMBDA=ABURN(I)+ DT1*A*EXP( M*LOG(ONE+ABURN(I)) )*EXP(N*LOG(ALPHA_UNIT*(-POLD(I)-PSH(I))))
            LAMBDA  = MAX(LAMBDA,ZERO)
            LAMBDA  = MIN(LAMBDA,ONE)
            EINT(I) = EINT(I)+(LAMBDA-ABURN(I))*EADD*MAX(EM20,VOLO(I))
            ABURN(I)= LAMBDA
          ENDIF
        ENDDO
      ENDIF
      !================  END AFTERBURNING     ============================!

      DO I=LFT,LLT
        ESPE(I)=EINT(I)/MAX(EM20,VOLO(I))
        W1DF(I)=BFRAC(I)*W1(I)/DF(I)
        FACM(I)=BFRAC(I)*(WDR1V(I)*ER1V(I)+WDR2V(I)*ER2V(I))
      ENDDO

      DO I=LFT,LLT
        PNEW(I)= - PSH(I) + (ONE - BFRAC(I)) * (P0(I) + BULK * AMU(I)) +
     .        (FACM(I)+(ESPE(I))*W1DF(I))/(ONE +W1DF(I)*DVOL(I)/MAX(EM20,VOLO(I)))
      ENDDO

      DO I=LFT,LLT
        !!! Taking -PSH as the minimum value
        PNEW(I)= MAX(ZERO - PSH(I), PNEW(I))*OFF(I)
      ENDDO

      DO I=LFT,LLT
        EINC(I)= EINC(I)-(PNEW(I) + PSH(I))*DVOL(I)
        EINT(I)=(EINT(I)-(PNEW(I) + PSH(I))*DVOL(I))/MAX(EM20,VOL(I))
      ENDDO

      DO I=LFT,LLT
        SIG(I,1)=SIG(I,1)*OFF(I)-PNEW(I)
        SIG(I,2)=SIG(I,2)*OFF(I)-PNEW(I)
        SIG(I,3)=SIG(I,3)*OFF(I)-PNEW(I)
      ENDDO

C-----------------------------------------------
      RETURN
      END
