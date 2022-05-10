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
Chd|  SIGEPS43C                     source/materials/mat/mat043/sigeps43c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS43C(
     1     NEL      ,NUPARAM  ,NUVAR    ,NPF      ,TF       ,
     2     NFUNC    ,IFUNC    ,ISRATE   ,TIME     ,TIMESTEP ,
     3     UPARAM   ,RHO0     ,AREA     ,EINT     ,THKLY    ,
     4     EPSPXX   ,EPSPYY   ,EPSPXY   ,EPSPYZ   ,EPSPZX   ,
     5     DEPSXX   ,DEPSYY   ,DEPSXY   ,DEPSYZ   ,DEPSZX   ,
     6     EPSXX    ,EPSYY    ,EPSXY    ,EPSYZ    ,EPSZX    ,
     7     SIGOXX   ,SIGOYY   ,SIGOXY   ,SIGOYZ   ,SIGOZX   ,
     8     SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     9     SOUNDSP  ,THK      ,PLA      ,UVAR     ,OFF      ,
     A     NGL      ,ETSE     ,HARDM    ,SIGY     ,GS       , 
     B     DPLA1    ,EPSP     ,SHF      ,INLOC    ,DPLANL   )
C=======================================================================
C   Tabulated Hill material model
C=======================================================================
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   I N P U T   A R G U M E N T S
C-----------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR, NFUNC ,ISRATE,INLOC
      INTEGER NGL(NEL),IFUNC(NFUNC)
      my_real 
     .   TIME,TIMESTEP,UPARAM(NUPARAM),AREA(NEL),RHO0(NEL),EINT(NEL,2),
     .   THKLY(NEL),PLA(NEL),EPSPXX(NEL),EPSPYY(NEL),GS(NEL),SHF(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIGOXX(NEL),SIGOYY(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),SIGY(NEL),HARDM(NEL),
     .   DPLANL(NEL)
C-----------------------------------------------
C   O U T P U T   A R G U M E N T S
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SOUNDSP(NEL),ETSE(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A R G U M E N T S 
C-----------------------------------------------
      my_real UVAR(NEL,NUVAR), OFF(NEL),THK(NEL),
     .        DPLA1(NEL),EPSP(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*)
      my_real FINTER ,TF(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J,NRATE,J1,J2,N,NINDX,NMAX,OPTE,NS,IFUNCE,
     .   IAD1(MVSIZ),IPOS1(MVSIZ),ILEN1(MVSIZ),
     .   IAD2(MVSIZ),IPOS2(MVSIZ),ILEN2(MVSIZ),
     .   JJ(MVSIZ),INDEX(MVSIZ)
      my_real 
     .   A,B,S12,FAC,DEZZ,SIGZ,S1,S2,S3,DPLA,VM2,EPST,
     .   A_1,A_2,A_3,P_1,P_2,P_3,PP1,PP2,PP3,DR,DR0,
     .   E(MVSIZ),A1(MVSIZ),A2(MVSIZ),G(MVSIZ),G3(MVSIZ),
     .   A01,A02,A03,A12,
     .   AXX(MVSIZ),AYY(MVSIZ),AXY(MVSIZ),A_XY(MVSIZ),
     .   DYDX1(MVSIZ),DYDX2(MVSIZ),RATE(MVSIZ,2),SVM(MVSIZ),
     .   SVM0(MVSIZ),YLD(MVSIZ),Y1(MVSIZ),Y2(MVSIZ),
     .   YFAC(MVSIZ,2),NNU1,NU,NU2(MVSIZ),
     .   NU3(MVSIZ),NU4(MVSIZ),NU5(MVSIZ),JQ(MVSIZ),JQ2(MVSIZ),
     .   DPLA_I(MVSIZ),DPLA_J(MVSIZ),FAIL(MVSIZ),EPSMAX,
     .   EPSR1,EPSR2,S11(MVSIZ),S22(MVSIZ),
     .   B_1(MVSIZ),B_2(MVSIZ),B_3(MVSIZ),Q12(MVSIZ),Q21(MVSIZ),
     .   ERR,F,DF,PLA_I,YLD_I,H(MVSIZ),
     .   FISOKIN,HK(MVSIZ),FHK,FA01,FA02,FA03,NU1,
     .   CE,EINF,ESCALE(MVSIZ),C1(MVSIZ),NORMXX,NORMYY,
     .   DYDXE(MVSIZ)
      DATA NMAX/2/,NS/10/
C=======================================================================
      NRATE = NINT(UPARAM(1)) ! nfunc sig(eps)
      NU   = UPARAM(6)
      A01  = UPARAM(7)
      A02  = UPARAM(8)
      A03  = UPARAM(9)
      A12  = UPARAM(10)
      EPSMAX=UPARAM(NS+2*NRATE+1)
      IF(EPSMAX == ZERO)THEN
        IF(TF(NPF(IFUNC(1)+1)-1) == ZERO)THEN
          EPSMAX=TF(NPF(IFUNC(1)+1)-2)
        ELSE
          EPSMAX= EP30
        ENDIF
      ENDIF
      NNU1 = NU / (UN - NU)
      EPSR1 =UPARAM(NS+2*NRATE+2)
      EPSR2 =UPARAM(NS+2*NRATE+3)
      FISOKIN =UPARAM(NS+2*NRATE+8)
c------------------------------------------------------
      OPTE =UPARAM(NS+2*NRATE+10)
      EINF =UPARAM(NS+2*NRATE+11)
      CE =UPARAM(NS+2*NRATE+12)
      DO I=1,NEL
        E(I)   = UPARAM(2)
        A1(I)  = UPARAM(3)
        A2(I)  = UPARAM(4)
        G(I)   = UPARAM(5)
        G3(I)  = UPARAM(NS+2*NRATE+5)
      ENDDO
      IF (OPTE == 1)THEN      
        DO I=1,NEL      
          IF (PLA(I) > ZERO)THEN                
             IFUNCE = UPARAM(NS+2*NRATE+9)                                        
             ESCALE(I) = FINTER(IFUNC(IFUNCE),PLA(I),NPF,TF,DYDXE(I))     
             E(I) =  ESCALE(I)* E(I)   
             A1(I) = E(I)/(UN - NU*NU)
             A2(I) = NU*A1(I) 
             G(I) =  UNDEMI*E(I)/(UN+NU)                                   
             G3(I) = TROIS*G(I)    
             GS(I) = G(I) * SHF(I)                 
           ENDIF
         ENDDO           
      ELSEIF ( CE /= ZERO) THEN
        DO I=1,NEL      
           IF(PLA(I) > ZERO)THEN                                                        
             E(I) = E(I)-(E(I)-EINF)*(UN-EXP(-CE*PLA(I)))  
             A1(I) = E(I)/(UN - NU*NU)
             A2(I) = NU*A1(I)                                             
             G(I) =  UNDEMI*E(I)/(UN+NU)                                    
             G3(I) = TROIS*G(I)   
             GS(I) = G(I) * SHF(I)                                              
           ENDIF                                                                    
        ENDDO
      ENDIF

c--------------------------------------------------------------
c      IF (TIME == ZERO) THEN  
c        DO I=1,NEL            
c          DO J=1,NRATE(I)     
c            UVAR(I,J+4) = UN
c          ENDDO               
c        ENDDO                 
c      ENDIF                   
C
      DO I=1,NEL
!        SIGOXX(I) = SIGOXX(I) - UVAR(I,2)
!        SIGOYY(I) = SIGOYY(I) - UVAR(I,3)
!        SIGOXY(I) = SIGOXY(I) - UVAR(I,4)
        DPLA1(I) = ZERO           
      ENDDO
      DO I=1,NEL
        SIGNXX(I)=SIGOXX(I) - UVAR(I,2) +A1(I)*DEPSXX(I)+A2(I)*DEPSYY(I)
        SIGNYY(I)=SIGOYY(I) - UVAR(I,3) +A2(I)*DEPSXX(I)+A1(I)*DEPSYY(I)
        SIGNXY(I)=SIGOXY(I) - UVAR(I,4) +G(I) *DEPSXY(I)
        SIGNYZ(I)=SIGOYZ(I)+GS(I)*DEPSYZ(I)
        SIGNZX(I)=SIGOZX(I)+GS(I)*DEPSZX(I)
C
        SOUNDSP(I) = SQRT(A1(I)/RHO0(I))
        ETSE(I) = UN
C-------------------
C     STRAIN RATE
C-------------------
        IF (ISRATE == 0) THEN
          EPSP(I) = UNDEMI*( ABS(EPSPXX(I)+EPSPYY(I))
     .            + SQRT( (EPSPXX(I)-EPSPYY(I))*(EPSPXX(I)-EPSPYY(I))
     .            + EPSPXY(I)*EPSPXY(I) ) )
        ENDIF
C-------------------
C     STRAIN 
C-------------------
        EPST = UNDEMI*( EPSXX(I)+EPSYY(I)
     .       + SQRT( (EPSXX(I)-EPSYY(I))*(EPSXX(I)-EPSYY(I))
     .       + EPSXY(I)*EPSXY(I) ) )
        FAIL(I) = MAX(ZERO,MIN(UN,(EPSR2-EPST)/(EPSR2-EPSR1)))
C
      ENDDO
C-------------------
C     HARDENING LAW
C-------------------
      DO I=1,NEL
        JJ(I) = 1
      ENDDO
      DO I=1,NEL
        DO J=2,NRATE-1
          IF(EPSP(I).GE.UPARAM(NS+J)) JJ(I) = J
        ENDDO
        RATE(I,1)=UPARAM(NS+JJ(I))
        RATE(I,2)=UPARAM(NS+JJ(I)+1)
        YFAC(I,1)=UPARAM(NS+NRATE+JJ(I))
        YFAC(I,2)=UPARAM(NS+NRATE+JJ(I)+1)
      ENDDO
      DO I=1,NEL
        J1 = JJ(I)
        J2 = J1+1
        IPOS1(I) = NINT(UVAR(I,J1+4))
        IAD1(I)  = NPF(IFUNC(J1)) / 2 + 1
        ILEN1(I) = NPF(IFUNC(J1)+1) / 2 - IAD1(I) - IPOS1(I)
        IPOS2(I) = NINT(UVAR(I,J2+4))
        IAD2(I)  = NPF(IFUNC(J2)) / 2 + 1
        ILEN2(I) = NPF(IFUNC(J2)+1) / 2 - IAD2(I) - IPOS2(I)
      ENDDO
C
      CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,PLA,DYDX1,Y1)
      CALL VINTER(TF,IAD2,IPOS2,ILEN2,NEL,PLA,DYDX2,Y2)
C
      DO I=1,NEL
        J1 = JJ(I)
        J2 = J1+1
        Y1(I)=Y1(I)*YFAC(I,1)
        Y2(I)=Y2(I)*YFAC(I,2)
        FAC   = (EPSP(I) - RATE(I,1))/(RATE(I,2) - RATE(I,1))
        YLD(I) = FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I)))
        H(I)   = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
        UVAR(I,J1+4) = IPOS1(I)
        UVAR(I,J2+4) = IPOS2(I)
C       ECROUISSAGE CINEMATIQUE
        Y1(I)=TF(NPF(IFUNC(J1))+1)
        Y2(I)=TF(NPF(IFUNC(J2))+1)
        YLD(I) = (UN-FISOKIN) * YLD(I) + 
     .       FISOKIN * (FAIL(I)*(Y1(I) + FAC*(Y2(I)-Y1(I))))
        YLD(I) = MAX(YLD(I),EM20)
        SIGY(I) = YLD(I)
      ENDDO
C-------------------------
C     HILL CRITERION 
C-------------------------
      DO  I=1,NEL
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
        IF(SVM(I).GT.YLD(I).AND.OFF(I) == UN) THEN
          NINDX=NINDX+1
          INDEX(NINDX)=I
        ENDIF
      ENDDO
C---------------------------
      IF (NINDX > 0) THEN
C---------------------------
C       DEP EN CONTRAINTE PLANE
C---------------------------
        DO  J=1,NINDX
         I=INDEX(J)
         DPLA_J(I)=(SVM(I)-YLD(I))/(G3(I)+H(I))
         ETSE(I)= H(I)/(H(I)+E(I))
C        +++cette partie peut etre neglige quand FHK est tres petit
         HK(I) = H(I)*FISOKIN
         FHK = QTHIRD*HK(I)/A1(I)
         FA01 =A01*FHK
         FA02 =A02*FHK
         FA03 =A03*FHK
         NU1 =NU+UNDEMI*FHK
C        ---
         NU2(I) = 1.-NU1*NU1+FHK*FHK
         NU3(I) = NU1*UNDEMI
         NU4(I) = UNDEMI*(UN-NU)
         NU5(I) = UN-NNU1
         S1=A01*NU1*DEUX-A03-FA03
         S2=A02*NU1*DEUX-A03-FA03
         S12=A03-NU1*(A01+A02)+FA03
         S3=SQRT(NU2(I)*(A01-A02)*(A01-A02)+S12*S12)
         IF (ABS(S1).LT.EM20) THEN 
           Q12(I)=ZERO
         ELSE
           Q12(I)=-(A01-A02+S3+FA01-FA02)/S1
         ENDIF
         IF (ABS(S2).LT.EM20) THEN 
           Q21(I)=ZERO
         ELSE
           Q21(I)=(A01-A02+S3+FA01-FA02)/S2
         ENDIF
         JQ(I)=UN/(1-Q12(I)*Q21(I))
         JQ2(I)=JQ(I)*JQ(I)
         A=A01*Q12(I)
         B=A02*Q21(I)
         A_1=(A01+A03*Q21(I)+B*Q21(I))*JQ2(I)
         A_2=(A02+A03*Q12(I)+A*Q12(I))*JQ2(I)
         A_3=(A+B)*JQ2(I)*DEUX+A03*(JQ2(I)*DEUX-JQ(I))
         S11(I)=SIGNXX(I)+SIGNYY(I)*Q12(I)
         S22(I)=Q21(I)*SIGNXX(I)+SIGNYY(I)
         AXX(I)=A_1*S11(I)*S11(I)
         AYY(I)=A_2*S22(I)*S22(I)
         A_XY(I)=A_3*S11(I)*S22(I)
         A=A03*NU3(I)
         B=S3*JQ(I)
         B_1(I)=A02-A-B+FA02
         B_2(I)=A01-A+B+FA01
         B_3(I)=A12*(NU4(I)+UNDEMI*FHK)
         H(I) = H(I)-HK(I)
         H(I) = MAX(ZERO,H(I))
        ENDDO
C
        DO N=1,NMAX
          DO J=1,NINDX
           I=INDEX(J)
           DPLA_I(I)=DPLA_J(I)
           IF(DPLA_I(I).GT.ZERO) THEN
           YLD_I =YLD(I)+H(I)*DPLA_I(I)
           DR  =A1(I)*DPLA_I(I)/YLD_I
           P_1=UN/(UN + B_1(I)*DR)
           PP1=P_1*P_1
           P_2=UN/(UN+B_2(I)*DR)
           PP2=P_2*P_2
           P_3=UN/(UN+B_3(I)*DR)
           PP3=P_3*P_3
           F     =AXX(I)*PP1+AYY(I)*PP2-A_XY(I)*P_1*P_2+AXY(I)*PP3
     .            -YLD_I*YLD_I
           DF    =-((AXX(I)*P_1-A_XY(I)*P_2*UNDEMI)*PP1*B_1(I)+
     .              (AYY(I)*P_2-A_XY(I)*P_1*UNDEMI)*PP2*B_2(I)+
     .              AXY(I)*PP3*P_3*B_3(I))*(A1(I)-DR*H(I))/YLD_I
     .             -H(I)*YLD_I
            DPLA_J(I)=MAX(ZERO,DPLA_I(I)-F*UNDEMI/DF)
           ELSE
            DPLA_J(I)=ZERO
           ENDIF        
          ENDDO
        ENDDO
C------------------------------------------
C       CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C------------------------------------------
        DO J=1,NINDX
         I=INDEX(J)
          PLA(I) = PLA(I) + DPLA_J(I)
          YLD_I =YLD(I)+H(I)*DPLA_J(I)
          DR0  =DPLA_J(I)/YLD_I
          DR  =A1(I)*DR0
          P_1=UN/(UN + B_1(I)*DR)
          P_2=UN/(UN + B_2(I)*DR)
          P_3=UN/(UN + B_3(I)*DR)	
         S1=S11(I)*P_1
         S2=S22(I)*P_2
         SIGNXX(I)=JQ(I)*(S1-S2*Q12(I))
         SIGNYY(I)=JQ(I)*(S2-S1*Q21(I))
         SIGNXY(I)=SIGNXY(I)*P_3
         S1=A01*SIGNXX(I)+A02*SIGNYY(I)
     .          -A03*(SIGNXX(I)+SIGNYY(I))*UNDEMI
         IF (INLOC == 0) THEN 
           DEZZ = - NU5(I)*DPLA_J(I)*S1/YLD_I
           THK(I) = THK(I) + DEZZ*THKLY(I)*OFF(I)
         ENDIF
         S1 =A03*UNDEMI
         P_1=A01*SIGNXX(I)-S1*SIGNYY(I)
         P_2=A02*SIGNYY(I)-S1*SIGNXX(I)
         P_3=A12*SIGNXY(I)
         DR0  = DTHIRD*DR0*HK(I)
         UVAR(I,2) = UVAR(I,2) + (DEUX*P_1+P_2)*DR0
         UVAR(I,3) = UVAR(I,3) + (DEUX*P_2+P_1)*DR0
         UVAR(I,4) = UVAR(I,4) + UNDEMI*P_3*DR0
         DPLA1(I) =  DPLA_J(I)
        ENDDO
C
        DO I=1,NEL
          IF(PLA(I).GT.EPSMAX.AND.OFF(I) == UN)OFF(I)=QCINQ
        ENDDO
      ENDIF
      DO I=1,NEL
        SIGNXX(I) = SIGNXX(I) + UVAR(I,2)
        SIGNYY(I) = SIGNYY(I) + UVAR(I,3)
        SIGNXY(I) = SIGNXY(I) + UVAR(I,4)
      ENDDO
C--------------------------------
C     HARDENING MODULUS
C--------------------------------
      DO I=1,NEL
        HARDM(I) = H(I)
      ENDDO
C--------------------------------
C     NON-LOCAL THICKNESS VARIATION
C--------------------------------
      IF (INLOC > 0) THEN 
        DO I = 1,NEL
          S1      = A01*SIGNXX(I)*SIGNXX(I)
          S2      = A02*SIGNYY(I)*SIGNYY(I)
          S3      = A03*SIGNXX(I)*SIGNYY(I)
          AXY(I)  = A12*SIGNXY(I)*SIGNXY(I)
          SVM(I)  = SQRT(S1+S2-S3+AXY(I))  
          NORMXX  = (DEUX*A01*SIGNXX(I) - A03*SIGNYY(I))/(MAX(DEUX*SVM(I),EM20))
          NORMYY  = (DEUX*A02*SIGNYY(I) - A03*SIGNXX(I))/(MAX(DEUX*SVM(I),EM20))
          DEZZ    = MAX(DPLANL(I),ZERO)*(NORMXX + NORMYY)
          DEZZ    = -NU*((SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/E(I)) - DEZZ
          THK(I)  = THK(I) + DEZZ*THKLY(I)*OFF(I)          
        ENDDO
      ENDIF
!!-------------------------
!!     EQUIVALENT STRESS FOR OUTPUT - HILL CRITERION -
!!-------------------------
!!      IF (ISEQ > 0) THEN
!!        DO I=1,NEL
!!          S1 = A01*SIGNXX(I)*SIGNXX(I)
!!          S2 = A02*SIGNYY(I)*SIGNYY(I)
!!          S3 = A03*SIGNXX(I)*SIGNYY(I)
!!          AXY(I) = A12*SIGNXY(I)*SIGNXY(I)
!!          SEQ_OUTPUT(I) = SQRT(S1+S2-S3+AXY(I))
!!        ENDDO
!!      ENDIF 
!-----------
      RETURN
      END
