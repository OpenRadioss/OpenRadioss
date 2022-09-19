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
Chd|  SIGEPS73C                     source/materials/mat/mat073/sigeps73c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        TABLE_VINTERP                 source/tools/curve/table_tools.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE SIGEPS73C(
     1     NEL    ,NUPARAM,NUVAR   ,TIME   ,TIMESTEP,
     2     UPARAM  ,RHO0  ,AREA    ,EINT   ,THKLY   ,
     4     EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX,THK     ,PLA     ,UVAR   ,
     B     OFF    ,NGL    ,ITABLE  ,ETSE   ,
     C     GS     ,SIGY   ,DPLA1   ,EPSP    ,TABLE  ,
     D     VOL    ,TEMPEL ,DIE     ,COEF    ,NPF    ,
     E     NFUNC  ,IFUNC  ,TF     ,SHF      ,HARDM  ,
     F     SEQ_OUTPUT,INLOC,DPLANL   )
C-----------------------------------------------
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   I N P U T   A R G U M E N T S
C-----------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR, NGL(NEL),ITABLE(*),
     .   INLOC
      my_real
     .   TIME,TIMESTEP,UPARAM(NUPARAM),
     .   AREA(NEL),RHO0(NEL),EINT(NEL,2),
     .   THKLY(NEL),PLA(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIGOXX(NEL),SIGOYY(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),SHF(NEL),
     .   GS(NEL),VOL(NEL),TEMPEL(NEL),DIE(NEL),COEF(NEL),HARDM(*),
     .   SEQ_OUTPUT(NEL),DPLANL(NEL)
      TYPE(TTABLE) TABLE(*)
C-----------------------------------------------
C   O U T P U T   A R G U M E N T S
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),SIGY(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL),ETSE(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A R G U M E N T S 
C-----------------------------------------------
      my_real UVAR(NEL,NUVAR), OFF(NEL),THK(NEL),
     .        DPLA1(NEL),EPSP(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION
C-----------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real
     .     FINTER ,TF(*)
      EXTERNAL FINTER
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J,NXK,J1,J2,N,NINDX,NMAX,
     .        INDEX(MVSIZ),OPTE,IFUNCE,FUNC_TAB
      my_real
     .        A,B,S12,FAC,DEZZ,SIGZ,S1,S2,S3,DPLA,VM2,EPST,
     .        A_1,A_2,A_3,P_1,P_2,P_3,PP1,PP2,PP3,DR,DR0,
     .        E,A1,A2,G,G3,
     .        A01,A02,A03,A12,
     .        AXX(MVSIZ),AYY(MVSIZ),AXY(MVSIZ),A_XY(MVSIZ),
     .        DYDX(MVSIZ),SVM(MVSIZ),
     .        SVM0(MVSIZ),YLD(MVSIZ),YK(MVSIZ),
     .        NNU1,NU,NU2,NU3,NU4,NU5,
     .        JQ(MVSIZ),JQ2(MVSIZ),
     .        EPSMAX,DPLA_I(MVSIZ),DPLA_J(MVSIZ),FAIL(MVSIZ),
     .        EPSR1,EPSR2,S11(MVSIZ),S22(MVSIZ),
     .        B_1(MVSIZ),B_2(MVSIZ),B_3(MVSIZ),Q12(MVSIZ),Q21(MVSIZ),
     .        ERR,F,DF,PLA_I,YLD_I,H(MVSIZ),
     .        FISOKIN,HK(MVSIZ),FHK,FA01,FA02,FA03,NU1,XFAC,YFAC,CE,
     .        E1(MVSIZ), A11(MVSIZ),A21(MVSIZ),G1(MVSIZ),G31(MVSIZ),
     .        DYDXE(MVSIZ),ESCALE(MVSIZ),EINF,NORMXX,NORMYY
      my_real
     .        TEMP(MVSIZ), VOL0, RHOCP
      my_real,
     .        DIMENSION(NEL,3) :: XX
      INTEGER,DIMENSION(NEL,3) :: IPOS
C
      DATA NMAX/2/
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
      FUNC_TAB = ITABLE(1)
      FISOKIN = UPARAM(1)
      OPTE    = UPARAM(23)
      CE    = UPARAM(25)
c--------Two options with or without young evolution with
c--------plastic strain
      IF (OPTE==1.OR. CE > ZERO)THEN
      DO I=1,NEL
        E1(I)   = UPARAM(2)
        A11(I)  = UPARAM(3)
        A21(I)  = UPARAM(4)
        G1(I)   = UPARAM(5)
        G31(I)  = UPARAM(17)
      ENDDO
      NU      = UPARAM(6)
      A01     = UPARAM(7)
      A02     = UPARAM(8)
      A03     = UPARAM(9)
      A12     = UPARAM(10)
      EPSMAX  = UPARAM(13)
      EINF    = UPARAM(24)
      IF(EPSMAX==ZERO)THEN
c      NXK=SIZE(TABLE(ITABLE)%X(1)%VALUES)
c      IF(TABLE(ITABLE)%Y%VALUES(NXK)==ZERO)THEN
c       EPSMAX = TABLE(ITABLE)%X(1)%VALUES(NXK)
c      ELSE
        EPSMAX = EP30
c      ENDIF
      ENDIF
C
      IF (OPTE == 1)THEN
       DO I=1,NEL
         IF(PLA(I) > ZERO)THEN
           IFUNCE  = UPARAM(22)
           ESCALE(I) = FINTER(IFUNC(IFUNCE),PLA(I),NPF,TF,DYDXE(I))
           E1(I) =  ESCALE(I)* E1(I)
           A11(I) = E1(I)/(ONE - NU*NU)
           A21(I) = NU*A11(I)
           G1(I) =  HALF*E1(I)/(ONE+NU)
           G31(I) = THREE*G1(I)
           GS(I) = G1(I) * SHF(I)
         ENDIF
       ENDDO
      ELSEIF ( CE /= ZERO) THEN
       DO I=1,NEL
         IF(PLA(I) > ZERO)THEN
           E1(I) = E1(I)-(E1(I)-EINF)*(ONE-EXP(-CE*PLA(I)))
           A11(I) = E1(I)/(ONE - NU*NU)
           A21(I) = NU*A11(I)
           G1(I) =  HALF*E1(I)/(ONE+NU)
           G31(I) = THREE*G1(I)
           GS(I) = G1(I) * SHF(I)
         ENDIF
       ENDDO
      ENDIF
      XFAC  =UPARAM(11)
      YFAC  =UPARAM(12)
C
      NNU1  = NU / (ONE - NU)
      EPSR1 =UPARAM(14)
      EPSR2 =UPARAM(15)
C
      DO I=1,NEL
!        SIGOXX(I) = SIGOXX(I) - UVAR(I,2)
!        SIGOYY(I) = SIGOYY(I) - UVAR(I,3)
!        SIGOXY(I) = SIGOXY(I) - UVAR(I,4)
        DPLA1(I) = ZERO
      ENDDO
C
C-----------------------------------------------
C  Calcul de la tempurature. (conduction ou adiabatique)
C--------------------    
        DO I=1,NEL
          COEF(I) = ONE
        END DO
C
        IF(JTHE > 0 ) THEN
         DO I=1,NEL
           TEMP(I) = TEMPEL(I)
         ENDDO
        ELSE
         RHOCP=UPARAM(21)
         DO I=1,NEL
           TEMP(I) = UPARAM(20)
           VOL0    = VOL(I) * RHO0(I)
           TEMP(I) = TEMP(I)
     .             + COEF(I)*RHOCP*(EINT(I,1)+ EINT(I,2))/VOL0
         ENDDO
        ENDIF
C
      DO I=1,NEL
        SIGNXX(I)=SIGOXX(I) - UVAR(I,2) +A11(I)*DEPSXX(I)+A21(I)*DEPSYY(I) !
        SIGNYY(I)=SIGOYY(I) - UVAR(I,3) +A21(I)*DEPSXX(I)+A11(I)*DEPSYY(I)!
        SIGNXY(I)=SIGOXY(I) - UVAR(I,4) +G1(I) *DEPSXY(I)!
        SIGNYZ(I)=SIGOYZ(I)+GS(I)*DEPSYZ(I)
        SIGNZX(I)=SIGOZX(I)+GS(I)*DEPSZX(I)
C
        SOUNDSP(I) = SQRT(A11(I)/RHO0(I))!
        VISCMAX(I) = ZERO
        ETSE(I) = ONE
C-------------------
C     STRAIN RATE
C-------------------
        EPSP(I) = HALF*( ABS(EPSPXX(I)+EPSPYY(I))
     .   + SQRT( (EPSPXX(I)-EPSPYY(I))*(EPSPXX(I)-EPSPYY(I))
     .                 + EPSPXY(I)*EPSPXY(I) ) )
C-------------------
C     STRAIN 
C-------------------
        EPST = HALF*( EPSXX(I)+EPSYY(I)
     .   + SQRT( (EPSXX(I)-EPSYY(I))*(EPSXX(I)-EPSYY(I))
     .                 + EPSXY(I)*EPSXY(I) ) )
        FAIL(I) = MAX(ZERO,MIN(ONE,(EPSR2-EPST)/(EPSR2-EPSR1)))
C
      ENDDO
C-------------------
C     HARDENING LAW
C-------------------
      DO I=1,NEL
        IPOS(I,1) = NINT(UVAR(I,5))
        IPOS(I,2) = NINT(UVAR(I,6))
        IPOS(I,3) = NINT(UVAR(I,7))
C
        XX(I,1)=PLA (I)
        XX(I,2)=EPSP(I)*XFAC
        XX(I,3)=TEMP(I)
      END DO
C
      CALL TABLE_VINTERP(TABLE(FUNC_TAB),NEL,IPOS,XX,YLD,DYDX)
C

      DO I=1,NEL
        YLD(I) = YFAC*YLD(I)
        YLD(I) = FAIL(I)*YLD(I)
        YLD(I) = MAX(YLD(I),EM20)
        H(I)   = FAIL(I)*DYDX(I)
        UVAR(I,5) = IPOS(I,1)
        UVAR(I,6) = IPOS(I,2)
        UVAR(I,7) = IPOS(I,3)
      END DO
C-----
      IF(FISOKIN/=ZERO)THEN
       DO I=1,NEL
         IPOS(I,1) = 1
C
         XX(I,1)=ZERO
         XX(I,2)=EPSP(I)*XFAC
         XX(I,3)=TEMP(I)
       END DO
       CALL TABLE_VINTERP(TABLE(FUNC_TAB),NEL,IPOS,XX,YK,DYDX)

       DO I=1,NEL
C        ECROUISSAGE CINEMATIQUE
         YLD(I) = (ONE-FISOKIN) * YLD(I) +
     .        FISOKIN * FAIL(I) * YFAC * YK(I)
         YLD(I) = MAX(YLD(I),EM20)
       ENDDO
      END IF
C-------------------------
C     HILL CRITERION 
C-------------------------
      DO  I=1,NEL
       SIGY(I) = YLD(I)
       H(I) = MAX(ZERO,H(I))
       S1=A01*SIGNXX(I)*SIGNXX(I)
       S2=A02*SIGNYY(I)*SIGNYY(I)
       S3=A03*SIGNXX(I)*SIGNYY(I)
       AXY(I)=A12*SIGNXY(I)*SIGNXY(I)
       SVM(I)=SQRT(S1+S2-S3+AXY(I))
       IF (INLOC == 0) THEN
         DEZZ = -(DEPSXX(I)+DEPSYY(I))*NNU1
         THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
       ENDIF
      ENDDO
C-------------------------
C     GATHER PLASTIC FLOW
C-------------------------
      NINDX=0
      DO I=1,NEL
      IF(SVM(I)>YLD(I).AND.OFF(I)==ONE) THEN
        NINDX=NINDX+1
        INDEX(NINDX)=I
      ENDIF
      ENDDO
      IF(NINDX>0) THEN
C---------------------------
C    DEP EN CONTRAINTE PLANE
C---------------------------
      DO  J=1,NINDX
       I=INDEX(J)
       DPLA_J(I)=(SVM(I)-YLD(I))/(G31(I)+H(I)) !!!
       ETSE(I)= H(I)/(H(I)+E1(I))  !!!
C   +++cette partie peut etre neglige quand FHK est tres petit
       HK(I) = H(I)*FISOKIN
       FHK = FOUR_OVER_3*HK(I)/A11(I)  !!
       FA01 =A01*FHK
       FA02 =A02*FHK
       FA03 =A03*FHK
       NU1  =NU+HALF*FHK
C   ---
       NU2 = 1.-NU1*NU1+FHK*FHK
       NU3 = NU1*HALF
       NU4 = HALF*(ONE-NU)
       NU5 = ONE-NNU1
       S1=A01*NU1*TWO-A03-FA03
       S2=A02*NU1*TWO-A03-FA03
       S12=A03-NU1*(A01+A02)+FA03
       S3=SQRT(NU2*(A01-A02)*(A01-A02)+S12*S12)
       IF (ABS(S1)<EM20) THEN
        Q12(I)=ZERO
       ELSE
        Q12(I)=-(A01-A02+S3+FA01-FA02)/S1
       ENDIF
       IF (ABS(S2)<EM20) THEN
        Q21(I)=ZERO
       ELSE
        Q21(I)=(A01-A02+S3+FA01-FA02)/S2
       ENDIF
       JQ(I)=ONE/(1-Q12(I)*Q21(I))
       JQ2(I)=JQ(I)*JQ(I)
       A=A01*Q12(I)
       B=A02*Q21(I)
       A_1=(A01+A03*Q21(I)+B*Q21(I))*JQ2(I)
       A_2=(A02+A03*Q12(I)+A*Q12(I))*JQ2(I)
       A_3=(A+B)*JQ2(I)*TWO+A03*(JQ2(I)*TWO-JQ(I))
       S11(I)=SIGNXX(I)+SIGNYY(I)*Q12(I)
       S22(I)=Q21(I)*SIGNXX(I)+SIGNYY(I)
       AXX(I)=A_1*S11(I)*S11(I)
       AYY(I)=A_2*S22(I)*S22(I)
       A_XY(I)=A_3*S11(I)*S22(I)
       A=A03*NU3
       B=S3*JQ(I)
       B_1(I)=A02-A-B+FA02
       B_2(I)=A01-A+B+FA01
       B_3(I)=A12*(NU4+HALF*FHK)
       H(I) = H(I)-HK(I)
       H(I) = MAX(ZERO,H(I))
      ENDDO
C
      DO N=1,NMAX
       DO J=1,NINDX
        I=INDEX(J)
        DPLA_I(I)=DPLA_J(I)
        IF(DPLA_I(I)>ZERO) THEN
        YLD_I =YLD(I)+H(I)*DPLA_I(I)
        DR  =A11(I)*DPLA_I(I)/YLD_I !
        P_1=ONE/(ONE + B_1(I)*DR)
        PP1=P_1*P_1
        P_2=ONE/(ONE+B_2(I)*DR)
        PP2=P_2*P_2
        P_3=ONE/(ONE+B_3(I)*DR)
        PP3=P_3*P_3
        F     =AXX(I)*PP1+AYY(I)*PP2-A_XY(I)*P_1*P_2+AXY(I)*PP3
     .         -YLD_I*YLD_I
        DF    =-((AXX(I)*P_1-A_XY(I)*P_2*HALF)*PP1*B_1(I)+
     .           (AYY(I)*P_2-A_XY(I)*P_1*HALF)*PP2*B_2(I)+
     .           AXY(I)*PP3*P_3*B_3(I))*(A11(I)-DR*H(I))/YLD_I
     .          -H(I)*YLD_I
         DPLA_J(I)=MAX(ZERO,DPLA_I(I)-F*HALF/DF)
        ELSE
         DPLA_J(I)=ZERO
        ENDIF
       ENDDO
      ENDDO

C------------------------------------------
C     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C------------------------------------------
      DO J=1,NINDX
       I=INDEX(J)
        PLA(I) = PLA(I) + DPLA_J(I)
        YLD_I =YLD(I)+H(I)*DPLA_J(I)
        DR0  =DPLA_J(I)/YLD_I
        DR  =A11(I)*DR0  !!!!
        P_1=ONE/(ONE + B_1(I)*DR)
        P_2=ONE/(ONE + B_2(I)*DR)
        P_3=ONE/(ONE + B_3(I)*DR)	
        S1=S11(I)*P_1
        S2=S22(I)*P_2
        SIGNXX(I)=JQ(I)*(S1-S2*Q12(I))
        SIGNYY(I)=JQ(I)*(S2-S1*Q21(I))
        SIGNXY(I)=SIGNXY(I)*P_3
        S1=A01*SIGNXX(I)+A02*SIGNYY(I)
     .         -A03*(SIGNXX(I)+SIGNYY(I))*HALF
        IF (INLOC == 0) THEN
          DEZZ = - NU5*DPLA_J(I)*S1/YLD_I
          THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
        ENDIF
        S1 =A03*HALF
        P_1=A01*SIGNXX(I)-S1*SIGNYY(I)
        P_2=A02*SIGNYY(I)-S1*SIGNXX(I)
        P_3=A12*SIGNXY(I)
        DR0  = TWO_THIRD*DR0*HK(I)
        UVAR(I,2) = UVAR(I,2) + (TWO*P_1+P_2)*DR0
        UVAR(I,3) = UVAR(I,3) + (TWO*P_2+P_1)*DR0
        UVAR(I,4) = UVAR(I,4) + HALF*P_3*DR0
        DPLA1(I) =  DPLA_J(I)
      ENDDO
C
      DO I=1,NEL
        IF(PLA(I)>EPSMAX.AND.OFF(I)==ONE)OFF(I)=FOUR_OVER_5
      ENDDO
      ENDIF
      DO I=1,NEL
        SIGNXX(I) = SIGNXX(I) + UVAR(I,2)
        SIGNYY(I) = SIGNYY(I) + UVAR(I,3)
        SIGNXY(I) = SIGNXY(I) + UVAR(I,4)
      ENDDO

      ELSE
      E       = UPARAM(2)
      A1      = UPARAM(3)
      A2      = UPARAM(4)
      G       = UPARAM(5)
      NU      = UPARAM(6)
      A01     = UPARAM(7)
      A02     = UPARAM(8)
      A03     = UPARAM(9)
      A12     = UPARAM(10)
      G3      = UPARAM(17)
      EPSMAX  = UPARAM(13)
      IF(EPSMAX==ZERO)THEN
c      NXK=SIZE(TABLE(ITABLE)%X(1)%VALUES)
c      IF(TABLE(ITABLE)%Y%VALUES(NXK)==ZERO)THEN
c       EPSMAX = TABLE(ITABLE)%X(1)%VALUES(NXK)
c      ELSE
        EPSMAX = EP30
c      ENDIF
      ENDIF
C
      XFAC  =UPARAM(11)
      YFAC  =UPARAM(12)
C
      NNU1  = NU / (ONE - NU)
      EPSR1 =UPARAM(14)
      EPSR2 =UPARAM(15)
C
      DO I=1,NEL
!        SIGOXX(I) = SIGOXX(I) - UVAR(I,2)
!        SIGOYY(I) = SIGOYY(I) - UVAR(I,3)
!        SIGOXY(I) = SIGOXY(I) - UVAR(I,4)
        DPLA1(I) = ZERO
      ENDDO
C
C-----------------------------------------------
C  Calcul de la tempurature. (conduction ou adiabatique)
C--------------------    
        DO I=1,NEL
          COEF(I) = ONE
        END DO
C
        IF(JTHE > 0 ) THEN
         DO I=1,NEL
           TEMP(I) = TEMPEL(I)
         ENDDO
        ELSE
         RHOCP=UPARAM(21)
         DO I=1,NEL
           TEMP(I) = UPARAM(20)
           VOL0    = VOL(I) * RHO0(I)
           TEMP(I) = TEMP(I)
     .             + COEF(I)*RHOCP*(EINT(I,1)+ EINT(I,2))/VOL0
         ENDDO
        ENDIF
C
      DO I=1,NEL
        SIGNXX(I)=SIGOXX(I) - UVAR(I,2) +A1*DEPSXX(I)+A2*DEPSYY(I)
        SIGNYY(I)=SIGOYY(I) - UVAR(I,3) +A2*DEPSXX(I)+A1*DEPSYY(I)
        SIGNXY(I)=SIGOXY(I) - UVAR(I,4) +G    *DEPSXY(I)
        SIGNYZ(I)=SIGOYZ(I)+GS(I)*DEPSYZ(I)
        SIGNZX(I)=SIGOZX(I)+GS(I)*DEPSZX(I)
C
        SOUNDSP(I) = SQRT(A1/RHO0(I))
        VISCMAX(I) = ZERO
        ETSE(I) = ONE
C-------------------
C     STRAIN RATE
C-------------------
        EPSP(I) = HALF*( ABS(EPSPXX(I)+EPSPYY(I))
     .   + SQRT( (EPSPXX(I)-EPSPYY(I))*(EPSPXX(I)-EPSPYY(I))
     .                 + EPSPXY(I)*EPSPXY(I) ) )
C-------------------
C     STRAIN 
C-------------------
        EPST = HALF*( EPSXX(I)+EPSYY(I)
     .   + SQRT( (EPSXX(I)-EPSYY(I))*(EPSXX(I)-EPSYY(I))
     .                 + EPSXY(I)*EPSXY(I) ) )
        FAIL(I) = MAX(ZERO,MIN(ONE,(EPSR2-EPST)/(EPSR2-EPSR1)))
C
      ENDDO
C-------------------
C     HARDENING LAW
C-------------------
      DO I=1,NEL
        IPOS(I,1) = NINT(UVAR(I,5))
        IPOS(I,2) = NINT(UVAR(I,6))
        IPOS(I,3) = NINT(UVAR(I,7))
C
        XX(I,1)=PLA (I)
        XX(I,2)=EPSP(I)*XFAC
c        print *,xx(i,2)
        XX(I,3)=TEMP(I)
      END DO
C
      CALL TABLE_VINTERP(TABLE(FUNC_TAB),NEL,IPOS,XX,YLD,DYDX)
C
      DO I=1,NEL
        YLD(I) = YFAC*YLD(I)
        YLD(I) = FAIL(I)*YLD(I)
        YLD(I) = MAX(YLD(I),EM20)
        H(I)   = FAIL(I)*DYDX(I)
        UVAR(I,5) = IPOS(I,1)
        UVAR(I,6) = IPOS(I,2)
        UVAR(I,7) = IPOS(I,3)
      END DO
C-----
      IF(FISOKIN/=ZERO)THEN
       DO I=1,NEL
         IPOS(I,1) = 1
C
         XX(I,1)=ZERO
         XX(I,2)=EPSP(I)*XFAC
         XX(I,3)=TEMP(I)
       END DO
       CALL TABLE_VINTERP(TABLE(FUNC_TAB),NEL,IPOS,XX,YK,DYDX)

       DO I=1,NEL
C        ECROUISSAGE CINEMATIQUE
         YLD(I) = (ONE-FISOKIN) * YLD(I) +
     .        FISOKIN * FAIL(I) * YFAC * YK(I)
         YLD(I) = MAX(YLD(I),EM20)
       ENDDO
      END IF
C-------------------------
C     HILL CRITERION 
C-------------------------
      DO  I=1,NEL
       SIGY(I) = YLD(I)
       H(I) = MAX(ZERO,H(I))
       S1=A01*SIGNXX(I)*SIGNXX(I)
       S2=A02*SIGNYY(I)*SIGNYY(I)
       S3=A03*SIGNXX(I)*SIGNYY(I)
       AXY(I)=A12*SIGNXY(I)*SIGNXY(I)
       SVM(I)=SQRT(S1+S2-S3+AXY(I))
       IF (INLOC == 0) THEN
         DEZZ = -(DEPSXX(I)+DEPSYY(I))*NNU1
         THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
       ENDIF
      ENDDO
C-------------------------
C     GATHER PLASTIC FLOW
C-------------------------
      NINDX=0
      DO I=1,NEL
      IF(SVM(I)>YLD(I).AND.OFF(I)==ONE) THEN
        NINDX=NINDX+1
        INDEX(NINDX)=I
      ENDIF
      ENDDO
      IF(NINDX>0) THEN
C---------------------------
C    DEP EN CONTRAINTE PLANE
C---------------------------
      DO  J=1,NINDX
       I=INDEX(J)
       DPLA_J(I)=(SVM(I)-YLD(I))/(G3+H(I))
       ETSE(I)= H(I)/(H(I)+E)
C   +++cette partie peut etre neglige quand FHK est tres petit
       HK(I) = H(I)*FISOKIN
       FHK = FOUR_OVER_3*HK(I)/A1
       FA01 =A01*FHK
       FA02 =A02*FHK
       FA03 =A03*FHK
       NU1  =NU+HALF*FHK
C   ---
       NU2 = 1.-NU1*NU1+FHK*FHK
       NU3 = NU1*HALF
       NU4 = HALF*(ONE-NU)
       NU5 = ONE-NNU1
       S1=A01*NU1*TWO-A03-FA03
       S2=A02*NU1*TWO-A03-FA03
       S12=A03-NU1*(A01+A02)+FA03
       S3=SQRT(NU2*(A01-A02)*(A01-A02)+S12*S12)
       IF (ABS(S1)<EM20) THEN
        Q12(I)=ZERO
       ELSE
        Q12(I)=-(A01-A02+S3+FA01-FA02)/S1
       ENDIF
       IF (ABS(S2)<EM20) THEN
        Q21(I)=ZERO
       ELSE
        Q21(I)=(A01-A02+S3+FA01-FA02)/S2
       ENDIF
       JQ(I)=ONE/(1-Q12(I)*Q21(I))
       JQ2(I)=JQ(I)*JQ(I)
       A=A01*Q12(I)
       B=A02*Q21(I)
       A_1=(A01+A03*Q21(I)+B*Q21(I))*JQ2(I)
       A_2=(A02+A03*Q12(I)+A*Q12(I))*JQ2(I)
       A_3=(A+B)*JQ2(I)*TWO+A03*(JQ2(I)*TWO-JQ(I))
       S11(I)=SIGNXX(I)+SIGNYY(I)*Q12(I)
       S22(I)=Q21(I)*SIGNXX(I)+SIGNYY(I)
       AXX(I)=A_1*S11(I)*S11(I)
       AYY(I)=A_2*S22(I)*S22(I)
       A_XY(I)=A_3*S11(I)*S22(I)
       A=A03*NU3
       B=S3*JQ(I)
       B_1(I)=A02-A-B+FA02
       B_2(I)=A01-A+B+FA01
       B_3(I)=A12*(NU4+HALF*FHK)
       H(I) = H(I)-HK(I)
       H(I) = MAX(ZERO,H(I))
      ENDDO
C
      DO N=1,NMAX
       DO J=1,NINDX
        I=INDEX(J)
        DPLA_I(I)=DPLA_J(I)
        IF(DPLA_I(I)>ZERO) THEN
        YLD_I =YLD(I)+H(I)*DPLA_I(I)
        DR  =A1*DPLA_I(I)/YLD_I
        P_1=ONE/(ONE + B_1(I)*DR)
        PP1=P_1*P_1
        P_2=ONE/(ONE+B_2(I)*DR)
        PP2=P_2*P_2
        P_3=ONE/(ONE+B_3(I)*DR)
        PP3=P_3*P_3
        F     =AXX(I)*PP1+AYY(I)*PP2-A_XY(I)*P_1*P_2+AXY(I)*PP3
     .         -YLD_I*YLD_I
        DF    =-((AXX(I)*P_1-A_XY(I)*P_2*HALF)*PP1*B_1(I)+
     .           (AYY(I)*P_2-A_XY(I)*P_1*HALF)*PP2*B_2(I)+
     .           AXY(I)*PP3*P_3*B_3(I))*(A1-DR*H(I))/YLD_I
     .          -H(I)*YLD_I
         DPLA_J(I)=MAX(ZERO,DPLA_I(I)-F*HALF/DF)
        ELSE
         DPLA_J(I)=ZERO
        ENDIF
       ENDDO
      ENDDO
C------------------------------------------
C     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C------------------------------------------
      DO J=1,NINDX
       I=INDEX(J)
        PLA(I) = PLA(I) + DPLA_J(I)
        YLD_I =YLD(I)+H(I)*DPLA_J(I)
        DR0  =DPLA_J(I)/YLD_I
        DR  =A1*DR0
        P_1=ONE/(ONE + B_1(I)*DR)
        P_2=ONE/(ONE + B_2(I)*DR)
        P_3=ONE/(ONE + B_3(I)*DR)	
        S1=S11(I)*P_1
        S2=S22(I)*P_2
        SIGNXX(I)=JQ(I)*(S1-S2*Q12(I))
        SIGNYY(I)=JQ(I)*(S2-S1*Q21(I))
        SIGNXY(I)=SIGNXY(I)*P_3
        S1=A01*SIGNXX(I)+A02*SIGNYY(I)
     .         -A03*(SIGNXX(I)+SIGNYY(I))*HALF
        IF (INLOC == 0) THEN
          DEZZ = - NU5*DPLA_J(I)*S1/YLD_I
          THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
        ENDIF
        S1 =A03*HALF
        P_1=A01*SIGNXX(I)-S1*SIGNYY(I)
        P_2=A02*SIGNYY(I)-S1*SIGNXX(I)
        P_3=A12*SIGNXY(I)
        DR0  = TWO_THIRD*DR0*HK(I)
        UVAR(I,2) = UVAR(I,2) + (TWO*P_1+P_2)*DR0
        UVAR(I,3) = UVAR(I,3) + (TWO*P_2+P_1)*DR0
        UVAR(I,4) = UVAR(I,4) + HALF*P_3*DR0
        DPLA1(I) =  DPLA_J(I)
      ENDDO
C
      DO I=1,NEL
        IF(PLA(I)>EPSMAX.AND.OFF(I)==ONE)OFF(I)=FOUR_OVER_5
      ENDDO
      ENDIF
      DO I=1,NEL
        SIGNXX(I) = SIGNXX(I) + UVAR(I,2)
        SIGNYY(I) = SIGNYY(I) + UVAR(I,3)
        SIGNXY(I) = SIGNXY(I) + UVAR(I,4)
      ENDDO
      ENDIF !  (opte=1 or ce>0)
C
C--------------------------------
C     HARDENING MODULUS
C--------------------------------
       DO I=1,NEL
           HARDM(I) = H(I)
       ENDDO
C------------------------------------------------------------------
C     EQUIVALENT STRESS FOR OUTPUT - NON-LOCAL THICKNESS VARIATION
C------------------------------------------------------------------
      DO I = 1,NEL
        S1      = A01*SIGNXX(I)*SIGNXX(I)
        S2      = A02*SIGNYY(I)*SIGNYY(I)
        S3      = A03*SIGNXX(I)*SIGNYY(I)
        AXY(I)  = A12*SIGNXY(I)*SIGNXY(I)
        SVM(I)  = SQRT(S1+S2-S3+AXY(I))
        SEQ_OUTPUT(I) = SVM(I)
        IF (INLOC > 0) THEN
          NORMXX  = (TWO*A01*SIGNXX(I) - A03*SIGNYY(I))/(MAX(TWO*SVM(I),EM20))
          NORMYY  = (TWO*A02*SIGNYY(I) - A03*SIGNXX(I))/(MAX(TWO*SVM(I),EM20))
          DEZZ    = MAX(DPLANL(I),ZERO)*(NORMXX + NORMYY)
          DEZZ    = -NU*((SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/E) - DEZZ
          THK(I)  = THK(I) + DEZZ*THKLY(I)*OFF(I)
        ENDIF
      ENDDO
!-------------------------
      RETURN
      END
