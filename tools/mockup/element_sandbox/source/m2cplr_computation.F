Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!	------------------------------------
	subroutine m2cplr_computation(
     1                  JFT    ,JLT    ,OFF      ,EPSEQ   ,
     2                  IPLA   ,T      ,Z3       ,Z4       ,EPIF    ,
     3                  NPIF   ,IRTY   ,ETSE     ,GS       ,EPSP    ,
     4                  ISRATE ,YLD    ,G        ,A1       ,A2      ,
     5                  NU     ,CA0    ,CB0      ,CN       ,YMAX0   ,
     6                  EPCHK  ,YOUNG  ,CC       ,EPDR     ,ICC     ,
     7                  DPLA   ,TSTAR  ,FISOKIN  ,
     8                  HARDM  ,NEL    ,DEPSXX   ,DEPSYY   ,DEPSXY  ,
     9                  DEPSYZ ,DEPSZX ,SIGNXX   ,SIGNYY   ,SIGNXY  ,
     A                  SIGNYZ ,SIGNZX ,
     B                  SIGOXX ,SIGOYY ,SIGOXY   ,SIGOYZ   ,SIGOZX  )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER JFT, JLT, IPLA,NPIF,IRTY,ISRATE,NEL
C     REAL ou REAL*8
        my_real
     .   OFF(*), EPSEQ(*),T(*),Z3,Z4,
     .   EPIF, ETSE(*),GS(*),EPSP(*)
        my_real
     .   A1, A2, G, YMAX0, 
     .   CA0, CB0, CN, YOUNG, YLD(MVSIZ),
     .   CC, EPDR(MVSIZ), NU, EPCHK(MVSIZ),TSTAR(MVSIZ),
     .   DPLA(MVSIZ),FISOKIN,
     .   HARDM(*),DEPSXX(MVSIZ),DEPSYY(MVSIZ),DEPSXY(MVSIZ),DEPSYZ(MVSIZ),
     .   DEPSZX(MVSIZ),SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),
     .   SIGNYZ(NEL),SIGNZX(NEL),
     .   SIGOXX(NEL),SIGOYY(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER ICC
        INTEGER I, J, N, NINDX, INDEX(MVSIZ), NMAX,IKFLG
C     REAL ou REAL*8
        my_real
     .   A(MVSIZ), B(MVSIZ), DPLA_I(MVSIZ), DPLA_J(MVSIZ), DR(MVSIZ),
     .   H(MVSIZ), NU1(MVSIZ), NU2(MVSIZ), P(MVSIZ), Q(MVSIZ), 
     .   SVM(MVSIZ),CA(MVSIZ), CB(MVSIZ), YMAX(MVSIZ),
     .   SVM2(MVSIZ),YLD2(MVSIZ),
     .   ERR,F,DF,PLA_I,P2,Q2,R,S1,S2,S3,YLD_I,NNU1,NNU2,
     .   NU3,NU4,NU5,NU6,SIGZ,PP,AA,BB,C,S11,S22,S12,S1S2,S122,UMR,
     .   VM2,QQ,SMALL,MT,TM,BETA,HI(MVSIZ),HK(MVSIZ),AAA ,
     .   NU11(MVSIZ),NU21(MVSIZ),ANU1(MVSIZ),BNU2(MVSIZ),H2(MVSIZ),
     .   HKIN,ALPHA
        DATA NMAX/3/
C------------------------------------------------------------------
        SMALL = EM07
        DO I=JFT,JLT
                ETSE(I) = UN
c remise aux val. initiales
                CA(I)   = CA0
                CB(I)   = CB0 
                YMAX(I) = YMAX0 
                H(I)    = ZERO
!
                SIGNXX(I)=SIGOXX(I)
                SIGNYY(I)=SIGOYY(I)
                SIGNXY(I)=SIGOXY(I)
                SIGNYZ(I)=SIGOYZ(I)
                SIGNZX(I)=SIGOZX(I)
        ENDDO
C---------------------------
C     CONTRAINTES ELASTIQUES
C---------------------------
        DO I=JFT,JLT
                SIGNXX(I)=SIGNXX(I)+A1*DEPSXX(I)+A2*DEPSYY(I)
                SIGNYY(I)=SIGNYY(I)+A2*DEPSXX(I)+A1*DEPSYY(I)
                SIGNXY(I)=SIGNXY(I)+G*DEPSXY(I)
                SIGNYZ(I)=SIGNYZ(I)+GS(I)*DEPSYZ(I)
                SIGNZX(I)=SIGNZX(I)+GS(I)*DEPSZX(I)
        ENDDO
C-------------
C     STRAIN RATE (JOHNSON-COOK, ZERILLI-ARMSTRONG)
C-------------
        IF (EPIF /= ZERO) THEN
                IF (NPIF == ZERO) THEN
                        DO I=JFT,JLT
                                IF (ISRATE == 0) EPSP(I) = MAX( ABS(DEPSXX(I)), ABS(DEPSYY(I)), UNDEMI*ABS(DEPSXY(I)))
                                EPSP(I) = MAX(EPSP(I),EPDR(I))
                                MT=MAX(EM20,Z3)
                                IF (TSTAR(I) == ZERO) THEN
                                        Q(I) = (UN + CC * LOG(EPSP(I)/EPDR(I)))
                                ELSE
                                        Q(I) = (UN + CC * LOG(EPSP(I)/EPDR(I)))*(UN-EXP(MT*LOG(TSTAR(I))))             
                                ENDIF
                                Q(I) = MAX(Q(I),EM20)
                                CA(I) = CA(I) * Q(I)
                                CB(I) = CB(I) * Q(I)
                                IF (ICC == 1) YMAX(I) = YMAX(I) * Q(I)
                        ENDDO
                ELSEIF (NPIF == JLT) THEN
                        DO I=JFT,JLT
                                IF (ISRATE == 0) EPSP(I) = MAX( ABS(DEPSXX(I)),ABS(DEPSYY(I)), UNDEMI*ABS(DEPSXY(I)))
                                EPSP(I) = MAX(EPSP(I),EM20)
                                Q(I) = LOG(EPSP(I)/EPDR(I))
                                Q(I)= CC*EXP((-Z3+Z4 * Q(I))*T(I))
                                IF (ICC == 1) YMAX(I)= YMAX(I) + Q(I)
                                CA(I) = CA(I) + Q(I)
                        ENDDO
                ELSE
                        DO I=JFT,JLT
                                IF (IRTY == 0) THEN
                                        IF (ISRATE == 0) EPSP(I) = MAX( ABS(DEPSXX(I)), ABS(DEPSYY(I)), UNDEMI*ABS(DEPSXY(I)))
                                        EPSP(I) = MAX(EPSP(I),EPDR(I))
                                        MT=MAX(EM20,Z3)
                                        IF (TSTAR(I) == ZERO) THEN
                                                Q(I) = (UN + CC * LOG(EPSP(I)/EPDR(I)))
                                        ELSE
                                                Q(I) = (UN + CC * LOG(EPSP(I)/EPDR(I)))*(UN-EXP(MT*LOG(TSTAR(I))))                
                                        ENDIF
                                        Q(I) = MAX(Q(I),EM20)
                                        CA(I) = CA(I) * Q(I)
                                        CB(I) = CB(I) * Q(I)
                                        IF (ICC == 1) YMAX(I) = YMAX(I) * Q(I)
                                ELSE
                                        IF (ISRATE == 0) EPSP(I) = MAX( ABS(DEPSXX(I)),ABS(DEPSYY(I)), UNDEMI*ABS(DEPSXY(I)))
                                        EPSP(I) = MAX(EPSP(I),EM20)
                                        Q(I) = LOG(EPSP(I)/EPDR(I))
                                        Q(I)= CC*EXP((-Z3+Z4 * Q(I))*T(I))
                                        IF (ICC == 1) YMAX(I)= YMAX(I) + Q(I)
                                        CA(I) = CA(I) + Q(I)
                                ENDIF ! IF (IRTY == 0)
                        ENDDO
                ENDIF ! IF (NPIF == ZERO)
        ENDIF ! IF (EPIF /= ZERO)
C-----------------------------------
C     YIELD
C-----------------------------------
        DO I=JFT,JLT
                DPLA(I) = ZERO
                IF (EPSEQ(I) == ZERO) THEN
                        YLD(I)= CA(I)
                ELSE
                        BETA = CB(I)*(UN-FISOKIN)
                        YLD(I)= CA(I)+BETA*EXP(CN*LOG(EPSEQ(I)))
                ENDIF
                YLD(I)= MIN(YLD(I),YMAX(I))
        ENDDO
C------------------------------------------
C     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C------------------------------------------
        IF (IPLA == 0) THEN
C-------------------
C     CONTRAINTE VM
C-------------------
                DO I=JFT,JLT
                        SVM(I)=SQRT(SIGNXX(I)*SIGNXX(I)+SIGNYY(I)*SIGNYY(I)-SIGNXX(I)*SIGNYY(I)+TROIS*SIGNXY(I)*SIGNXY(I))
                ENDDO
C projection radiale
C need a vectorize here because the compiler does not vectorize the loop
#include "vectorize.inc"
                DO I=JFT,JLT
                        R = MIN(UN,YLD(I)/(SVM(I)+EM15))
                        IF (R < UN) THEN 
                                SIGNXX(I)=SIGNXX(I)*R
                                SIGNYY(I)=SIGNYY(I)*R
                                SIGNXY(I)=SIGNXY(I)*R
                                DPLA(I) =  OFF(I) * MAX(ZERO,(SVM(I)-YLD(I))/YOUNG)
                                S1=UNDEMI*(SIGNXX(I)+SIGNYY(I))
                                EPSEQ(I) = EPSEQ(I) + DPLA(I)
                                EPCHK(I) = MAX(EPSEQ(I),EPCHK(I))     
                                IF (YLD(I) >=YMAX(I)) THEN
                                        H(I)=ZERO
                                ELSE
                                        H(I)=CN*CB(I)*EXP((CN-UN)*LOG(EPSEQ(I)+SMALL))
                                ENDIF
                                ETSE(I)= H(I)/(H(I)+YOUNG)
                        ENDIF
                ENDDO
C------------------------------------------------------------------------
        ELSEIF (IPLA == 1) THEN
C-------------------------
C     CRITERE DE VON MISES
C-------------------------
                DO  I=JFT,JLT
                        S1=SIGNXX(I)+SIGNYY(I)
                        S2=SIGNXX(I)-SIGNYY(I)
                        S3=SIGNXY(I)
                        A(I)=QUART*S1*S1
                        B(I)=TQUART*S2*S2+TROIS*S3*S3
                        SVM(I)=SQRT(A(I)+B(I))  
                ENDDO
C-------------------------
C     GATHER PLASTIC FLOW
C-------------------------
                NINDX=0
                DO I=JFT,JLT
                        IF (SVM(I) > YLD(I) .AND. OFF(I) == UN) THEN
                                NINDX=NINDX+1
                                INDEX(NINDX)=I
                        ENDIF
                ENDDO
C---------------------------
C    DEP EN CONTRAINTE PLANE
C---------------------------
C need a vectorize here because the compiler does not vectorize the loop
#include "vectorize.inc"
                DO  J=1,NINDX
                        I=INDEX(J)
                        NU1(I)=UN/(UN-NU)
                        NU2(I)=UN/(UN+NU)
                        IF (YLD(I) >= YMAX(I)) THEN
                                H(I)=ZERO
                        ELSE
                                H(I)=CN*CB(I)*EXP((CN-UN)*LOG(EPSEQ(I)+SMALL))
                        ENDIF
                        DPLA_J(I)=(SVM(I)-YLD(I))/(TROIS*G+H(I))
                        ETSE(I)= H(I)/(H(I)+YOUNG)
                        ANU1(I)  = A(I)*NU1(I)
                        BNU2(I) = TROIS*B(I)*NU2(I)
                        H2(I)= DEUX*H(I)
                ENDDO
C-----just keep at least old performance--to update H(I) inside iteration for implicit+ETSE---------
                DO N=1,NMAX
C need a vectorize here because the compiler does not vectorize the loop
#include "vectorize.inc"
                        DO J=1,NINDX
                                I=INDEX(J)
                                DPLA_I(I)=DPLA_J(I)
                                PLA_I =EPSEQ(I)+DPLA_I(I)
                                DPLA(I) = DPLA_J(I)
                                IF (PLA_I == ZERO) THEN
                                        YLD_I =MIN(YMAX(I),CA(I))
                                ELSE
                                        YLD_I =MIN(YMAX(I),CA(I)+CB(I)*EXP(CN*LOG(PLA_I)))
                                ENDIF
                                DR(I) =UNDEMI*YOUNG*DPLA_I(I)/YLD_I
                                P(I)  =UN/(UN+DR(I)*NU1(I))
                                Q(I)  =UN/(UN+TROIS*DR(I)*NU2(I))
                                P2    =P(I)*P(I)
                                Q2    =Q(I)*Q(I)
                                F     =A(I)*P2+B(I)*Q2-YLD_I*YLD_I
                                DF    =-(ANU1(I)*P2*P(I)+BNU2(I)*Q2*Q(I))
     .                                  *(YOUNG-DR(I)*H2(I))/YLD_I
     .                                  -H2(I)*YLD_I
                                IF (DPLA_I(I) > ZERO) THEN
                                        DPLA_J(I)=MAX(ZERO,DPLA_I(I)-F/DF)
                                ELSE
                                        DPLA_J(I)=ZERO
                                ENDIF        
                        ENDDO ! DO J=1,NINDX
                ENDDO ! DO N=1,NMAX
        ENDIF
C
C--------------------------------
C     HARDENING MODULUS
C--------------------------------
        DO I=JFT,JLT
                HARDM(I) = H(I)
        ENDDO

        return
!	------------------------------------
	end subroutine m2cplr_computation
!	------------------------------------
        

