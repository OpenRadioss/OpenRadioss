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
Chd|  SIGEPS52                      source/materials/mat/mat052/sigeps52.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|-- calls ---------------
Chd|        TABLE_INTERP                  source/tools/curve/table_tools.F
Chd|        TABLE_INTERP_LAW76            source/tools/curve/table_tools.F
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE SIGEPS52(
     1     NEL    ,NUPARAM,NUVAR   ,MFUNC   ,KFUNC   ,NPF    ,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0    ,RHO    ,
     3     VOLUME ,EINT   ,
     4     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVZZ  ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX,UVAR    ,OFF     ,NGL     ,IPT    ,
     B     IPM    ,MAT    ,EPSP    ,IPLA    ,SIGY    ,DEFP  ,
     C     TABLE )       
C-----------------------------------------------
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C MFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW not used
C KFUNC   | NFUNC   | I | R | FUNCTION INDEX not used
C NPF     |  *      | I | R | FUNCTION ARRAY   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL     | F | R | INITIAL DENSITY
C RHO     | NEL     | F | R | DENSITY
C VOLUME  | NEL     | F | R | VOLUME
C EINT    | NEL     | F | R | TOTAL INTERNAL ENERGY
C EPSPXX  | NEL     | F | R | STRAIN RATE XX
C EPSPYY  | NEL     | F | R | STRAIN RATE YY
C ...     |         |   |   |
C DEPSXX  | NEL     | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL     | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C EPSXX   | NEL     | F | R | STRAIN XX
C EPSYY   | NEL     | F | R | STRAIN YY
C ...     |         |   |   |
C SIGOXX  | NEL     | F | R | OLD ELASTO PLASTIC STRESS XX 
C SIGOYY  | NEL     | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |    
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SIGVXX  | NEL     | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL     | F | W | VISCOUS STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL     | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr17_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
#include      "com08_c.inc"
C
      INTEGER NEL, NUPARAM, NUVAR,IPT,
     .   NGL(NEL),MAT(NEL),IPM(NPROPMI,*),IPLA, ITER, IFLAG
      my_real
     .   TIME,TIMESTEP,UPARAM(*),
     .   RHO(NEL),RHO0(NEL),VOLUME(NEL),EINT(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),EPSPZZ(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,EPSZZ(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOZZ(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   EPSP(NEL)
C     
       TYPE(TTABLE) TABLE(*)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),SIGVZZ(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL),SIGY(NEL),DEFP(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real 
     .        UVAR(NEL,NUVAR), OFF(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
      my_real
     .   FINTER2, TF(*)
      EXTERNAL FINTER2

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IADBUFV,J1,J2,JJ(MVSIZ),NFUNC,
     .        NRATE(MVSIZ),IPOS1(MVSIZ),IPOS2(MVSIZ),IAD1(MVSIZ),
     .        ILEN1(MVSIZ),IAD2(MVSIZ),ILEN2(MVSIZ),
     .         NFUNCV(MVSIZ),PFUN(MVSIZ),
     .        IPOSP(MVSIZ),IADP(MVSIZ),ILENP(MVSIZ),NFUNCM,NRATEM,
     .        IPFLAG,IPARAM,NPAR,IYEILD_TAB,IADBUF,
     ,        ITAB(100),NTABLE,NXH,NDIM,IPOS,NXK
      my_real
     .        F(MVSIZ), SIGM(MVSIZ), EPSM(MVSIZ), EPSP1(MVSIZ),
     .        LAMDA(MVSIZ), FSTAR(MVSIZ),P0(MVSIZ),PN(MVSIZ),
     .        ET,YEILD0(MVSIZ),CSD, 
     .        VISP(MVSIZ),  Q1, Q2, Q3, 
     .        SN, EPSN, FI,FN,
     .        FC,FF,FU, N,
     .        SIGXX(MVSIZ),SIGYY(MVSIZ),SIGXY(MVSIZ),
     .        SIGZX(MVSIZ),SIGYZ(MVSIZ), SIGZZ(MVSIZ),
     .        SGANXX(MVSIZ), SGANYY(MVSIZ), SGANZZ(MVSIZ),
     .        SGANXY(MVSIZ), SGANXZ(MVSIZ), SGANYZ(MVSIZ)
      my_real
     .        E,NU,P,DAV,VM,R,FAC,EPST,EP1,EP2,EP3,EP4,EP5,EP6,
     .        E1,E2,E3,E4,E5,E6,C,CC,D,Y,YP,E42,E52,E62,EPST2,
     .        DP11,DP22,DP33,DP12,DP13,DP23,A1,A2,
     .        D11,D22,D33,D12,D13,D23,DCRF,DCRM, DSEPP,DCD,LAM1,
     .       C1(MVSIZ),C2(MVSIZ) ,G(MVSIZ),DF,COH,SIH,VA,CRIT,CONV,
     .        FG,FN1,YLD,C11,P1, VAR, DTINV, SQR22,
     .       SXX,SYY,SZZ, SIGM1, SIGM2, VA1, VM1, VA11,A21,YP1,YP2,
     .       DF1, DF2, XFAC,YFAC,DX2,XX(2),DFT,YY
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
      NTABLE  = IPM(226,MAT(1))
      IYEILD_TAB = 0
      IF(NTABLE > 0) THEN
         IYEILD_TAB = 1
         ITAB(1)=IPM(226+1,MAT(1))
         IADBUF = IPM(7,MAT(1))-1
         XFAC  = UPARAM(IADBUF + 22 )
         YFAC  = UPARAM(IADBUF + 23 )
      ENDIF        
C
      IADBUFV = IPM(7,MAT(1))-1
      E        = UPARAM(IADBUFV+1)
      NU       = UPARAM(IADBUFV+2)
      IFLAG    = UPARAM(IADBUFV+18)
      ET    = UPARAM(IADBUFV+4)
      N     = UPARAM(IADBUFV+5)
      CSD   = ONE/UPARAM(IADBUFV+6)
      Q1    = UPARAM(IADBUFV+8)
      Q2    = UPARAM(IADBUFV+9)
      Q3    = UPARAM(IADBUFV+10)
      SN    = UPARAM(IADBUFV+11)
      IF(SN==ZERO)SN = EP20 
      EPSN  = UPARAM(IADBUFV+12)
      FI    = UPARAM(IADBUFV+13)
      FN    = UPARAM(IADBUFV+14)
      FC    = UPARAM(IADBUFV+15)
      FF    = UPARAM(IADBUFV+16)
      FU    = UPARAM(IADBUFV+17)
      DO I=1,NEL

        YEILD0(I)= UPARAM(IADBUFV+3)
        VISP(I)  = UPARAM(IADBUFV+7)
 
         G(I) = HALF*E/(ONE + NU)
         C11  = E/THREE/(ONE- TWO*NU)
        SOUNDSP(I) = SQRT((C11 + FOUR_OVER_3*G(I))/RHO0(I))
        VISCMAX(I) = ZERO
        C1(I)  = E*(ONE-NU) /((ONE + NU)*(ONE - TWO*NU))
        C2(I)  = C1(I)*NU/(ONE - NU)
      ENDDO   
C
      IF(TIME==0.0)THEN
        DO I=1,NEL
          EPSP1(I)=ZERO
          EPSM(I)=ZERO
          IF(N<ONE)EPSM(I)=EM20        
          UVAR(I,1)=EPSM(I)
          UVAR(I,2)=FI
          UVAR(I,3)=YEILD0(I)
          UVAR(I,4)=FI
          UVAR(I,5)= ZERO
          SIGY(I) = YEILD0(I)  
        ENDDO
C  
        IF(IYEILD_TAB > 0) THEN
            DO I=1,NEL
              XX(1)=ZERO
              XX(2)=ZERO 
              CALL TABLE_INTERP (TABLE(ITAB(1)),XX,YY) 
              YEILD0(I)  = YY *YFAC
              UVAR(I,3) = YEILD0(I)
              SIGY(I)   = YEILD0(I)
            ENDDO
        ENDIF        
      ENDIF
C
      SQR22= ONE/SQRT(TWO*PI)      
C
C-----------------------------------------------
C
      DO I=1,NEL
         EPSM(I) = UVAR(I,1)
         FSTAR(I)= UVAR(I,2)
         SIGM(I) = UVAR(I,3)
         F(I)    = UVAR(I,4)
         EPSP1(I)= ZERO
C
         SIGNXX(I)=SIGOXX(I) +  C1(I)*DEPSXX(I)
     .                       +  C2(I)*(DEPSYY(I) + DEPSZZ(I))
         SIGNYY(I)=SIGOYY(I) +  C1(I)*DEPSYY(I)
     .                       +  C2(I)*(DEPSXX(I) + DEPSZZ(I))
         SIGNZZ(I)=SIGOZZ(I) +  C1(I)*DEPSZZ(I)
     .                       +  C2(I)*(DEPSXX(I) + DEPSYY(I))
         SIGNXY(I)=SIGOXY(I) +   G(I)*DEPSXY(I)
         SIGNYZ(I)=SIGOYZ(I) +   G(I)*DEPSYZ(I)
         SIGNZX(I)=SIGOZX(I) +   G(I)*DEPSZX(I)
        IF(OFF(I)==ZERO) THEN
           SIGNXX(I)=ZERO
           SIGNYY(I)=ZERO
           SIGNZZ(I)=ZERO
           SIGNXY(I)=ZERO
           SIGNZX(I)=ZERO
           SIGNYZ(I)=ZERO
        ENDIF
      ENDDO
C
      DTINV=TIMESTEP/MAX(TIMESTEP**2,EM20)      
C-------------------
C     STRAIN RATE
C-------------------
C VISCOPLASTIC
      IF(IFLAG==0)THEN
C-------------
C  IFLAG = 0, 
C-----------
       DO I=1,NEL
        IF(OFF(I)==ONE)THEN
         IF(F(I)<=FC)THEN
          FSTAR(I) = F(I)
          DF       = ONE 
         ELSE
          DF      = (FU- FC)/(FF-FC)
          FSTAR(I)= FC  + DF*(F(I)-FC)                 
         ENDIF 
C          
         PN(I)= (SIGNXX(I)+SIGNYY(I)+SIGNZZ(I))*THIRD
         SXX  = SIGNXX(I)-PN(I)
         SYY  = SIGNYY(I)-PN(I)
         SZZ  = SIGNZZ(I)-PN(I)
         VM = HALF*(SXX**2 + SYY**2 + SZZ**2)
     .       +   (SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2)
         VM = SQRT(THREE*VM)
         SIGM1 = ONE/MAX(SIGM(I),EM20)
         VAR  = THREE_HALF * Q2 * PN(I)* SIGM1
         VAR = EXP(VAR)
         COH  = HALF * (VAR + ONE/MAX(EM20,VAR))
         SIH  = HALF * (VAR - ONE/MAX(EM20,VAR)) 
         VA   = ONE + Q3 * FSTAR(I)**2 - TWO * Q1 * FSTAR(I) * COH
         VA= SQRT(MAX(ZERO,VA))
         YLD  = SIGM(I) * VA
         SIGY(I) = YLD           
         CRIT = VM - YLD
         IF(VM < YLD .OR. SIGY(I) == ZERO )THEN
           LAMDA(I)=ZERO
         ELSE 
          LAMDA(I) = ZERO
          SIGXX(I) = SIGNXX(I)
          SIGYY(I) = SIGNYY(I)
          SIGZZ(I) = SIGNZZ(I)
          SIGXY(I) = SIGNXY(I)
          SIGZX(I) = SIGNZX(I)
          SIGYZ(I) = SIGNYZ(I)
          DP11 = ZERO
          DP22 = ZERO
          DP33 = ZERO
          DP12 = ZERO
          DP13 = ZERO
          DP23 = ZERO   
          IF(F(I)==ONE)THEN
           A21 = EP20
          ELSE             
           A21 =  SIGM1 /(ONE - F(I))     
          ENDIF
          A1 = FN*EXP(-HALF*((EPSM(I)-EPSN)/SN)**2)/SN
          A1 = A1*SQR22 
          DO ITER=1,5                           
           VM1 = ONE/MAX(VM,EM20)
           VA1 = ONE/MAX(VA,EM20)
           VA11 =HALF*Q1*Q2*FSTAR(I)*SIH*VA1
           D11 = HALF*(TWO*SIGNXX(I) - SIGNYY(I)-SIGNZZ(I))*VM1 
     .                   + VA11                    
           D22 = HALF*(TWO*SIGNYY(I) - SIGNXX(I)-SIGNZZ(I))*VM1
     .                   + VA11       
           D33 = HALF*(TWO*SIGNZZ(I) - SIGNXX(I)-SIGNYY(I))*VM1
     .                   + VA11                
           D12 = THREE*SIGNXY(I)*VM1       
           D13 = THREE*SIGNZX(I)*VM1
           D23 = THREE*SIGNYZ(I)*VM1
C
           A2  = D11*SIGNXX(I) + D22*SIGNYY(I) + D33*SIGNZZ(I) +
     .           TWO*(D12*SIGNXY(I) + D13*SIGNZX(I) + D23*SIGNYZ(I)) 
           A2  = A2*A21
C
           DCRF=-SIGM(I)*(Q3*FSTAR(I)*DF-Q1*COH*DF)*VA1
           DCRM= - VA - THREE*VA11*PN(I)*SIGM1
           IF(N==1)THEN
            DSEPP = ET*(ONE+ ( EPSP(I)*CSD)**VISP(I))
           ELSE
            DSEPP = ET * N * EPSM(I) ** (N - 1)*
     .      (ONE + ( EPSP(I)*CSD)**VISP(I))    
           ENDIF                   
C
           DCD = C1(I)*(D11**2 + D22**2 + D33**2) + 
     .           TWO*C2(I)*(D11*D22 + D11*D33 + D22*D33) +
     .           TWO*G(I)*(D12**2 + D13**2 + D23**2)
C
           LAM1= DCD - DCRM* DSEPP*A2 - 
     .           DCRF*((ONE - F(I))*(D11+D22+D33) + A1*A2)              
           IF(LAM1/=ZERO) LAMDA(I) =  MAX(ZERO,(VM - YLD))/LAM1
C  PLASTIC DEFORMATION          
           DP11 = DP11 + LAMDA(I) * D11
           DP22 = DP22 + LAMDA(I) * D22
           DP33 = DP33 + LAMDA(I) * D33
           DP12 = DP12 + LAMDA(I) * D12
           DP13 = DP13 + LAMDA(I) * D13
           DP23 = DP23 + LAMDA(I) * D23
C   NEW STRESS
           SIGNXX(I)= SIGXX(I) - C1(I)*DP11 - C2(I)*(DP22 + DP33) 
           SIGNYY(I)= SIGYY(I) - C1(I)*DP22 - C2(I)*(DP11 + DP33) 
           SIGNZZ(I)= SIGZZ(I) - C1(I)*DP33 - C2(I)*(DP22 + DP11)
           SIGNXY(I)= SIGXY(I) - TWO*G(I)*DP12
           SIGNZX(I)= SIGZX(I) - TWO*G(I)*DP13
           SIGNYZ(I)= SIGYZ(I) - TWO*G(I)*DP23
C....... AMISSIBLE PLASTIC DEFORMATION.

           EPSP1(I)= SIGNXX(I)*DP11 + SIGNYY(I)*DP22 + SIGNZZ(I)*DP33
     .        + TWO*(SIGNXY(I)*DP12 + SIGNZX(I)*DP13 + SIGNYZ(I)*DP23)
           EPSP1(I)=EPSP1(I)*A21
           EPSP1(I) = MAX(ZERO, EPSP1(I))
           PN(I)= (SIGNXX(I)+SIGNYY(I)+SIGNZZ(I))*THIRD
           SXX  = SIGNXX(I)-PN(I)
           SYY  = SIGNYY(I)-PN(I)
           SZZ  = SIGNZZ(I)-PN(I)
           VM = HALF*(SXX**2 + SYY**2 + SZZ**2)
     .        +   (SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2)
           VM = SQRT(THREE*VM)
           VAR  = THREE_HALF * Q2*PN(I) *SIGM1
           VAR = EXP( VAR)
           COH  = HALF * ( VAR + ONE/MAX(EM20,VAR))
           SIH  = HALF * ( VAR - ONE/MAX(EM20,VAR))
           VA   = ONE + Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)*COH
           VA = SQRT(MAX(ZERO,VA))
           YLD  = SIGM(I) * VA
C           IF(CONV<1E-3)GO TO 100
          ENDDO ! ITER
C 
          FG      = (ONE -F(I))*(DP11+DP22+DP33) 
          SIGY(I) = YLD 
          FN1     = A1*EPSP1(I) 
          EPSM(I) = EPSM(I) + EPSP1(I)         
          F(I)    = F(I) + FG + FN1                    
          IF(Q1==ZERO.and.Q2==ZERO.and.Q3==ZERO)F(I)=ZERO
          IF(F(I)<ZERO) F(I)=ZERO
          SIGM(I) = (YEILD0(I) + ET * EPSM(I) ** N)*
     .             (ONE+ (EPSP(I)*CSD) ** VISP(I))           
C....COMPUTE F*.....
          IF(F(I)<=FC)THEN
           FSTAR(I) = F(I)
          ELSE
           IF(FF==FC)THEN
            FSTAR(I) = EP20
           ELSE
            FSTAR(I) = FC+ (FU-FC)/(FF-FC)*(F(I)-FC)
           ENDIF
          ENDIF
          UVAR(I,1) = EPSM(I)
          UVAR(I,2) = FSTAR(I)
          UVAR(I,3) = SIGM(I)
          UVAR(I,4) = F(I)
          UVAR(I,5) = EPSP(I)          
         ENDIF 
         IF(OFF(I)==ONE.AND.FSTAR(I)>=FF)THEN
          OFF(I)=FOUR_OVER_5
          IDEL7NOK = 1
          WRITE(IOUT, 1000) NGL(I)
          WRITE(ISTDO,1100) NGL(I),TT
         ENDIF
        ELSE         
         IF(OFF(I)<EM01)OFF(I)=ZERO
         OFF(I)=OFF(I)*FOUR_OVER_5
        ENDIF
       ENDDO
      ELSEIF(IFLAG==1)THEN
C----------
C IFLAG set to 1
C..........
       DO I=1,NEL
        IF(OFF(I)==ONE)THEN
C
         IF(F(I)<=FC)THEN
          FSTAR(I) = F(I)
          DF       = ONE
         ELSE
          FSTAR(I) = FC +  (FU - FC)/(FF-FC)*(F(I)-FC)
          DF       = (FU - FC)/(FF - FC)
          FSTAR(I) = FC +  DF*(F(I)-FC)         
         ENDIF
         PN(I)= (SIGNXX(I)+SIGNYY(I)+SIGNZZ(I))*THIRD
         SXX  = SIGNXX(I)-PN(I)
         SYY  = SIGNYY(I)-PN(I)
         SZZ  = SIGNZZ(I)-PN(I)
         VM = HALF*(SXX**2 + SYY**2 + SZZ**2)
     .       +   (SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2)
         VM = THREE*VM      
         SIGM1 = ONE/MAX(EM20,SIGM(I))
         SIGM2 = SIGM1**2
         VAR   = THREE_HALF  *  Q2 * PN(I) * SIGM1 
         VAR = EXP(VAR)
         COH   = HALF*(VAR + ONE/MAX(EM20,VAR))
         SIH   = HALF*(VAR - ONE/MAX(EM20,VAR))
         IF(PN(I)<=ZERO) THEN
          VA = ONE +Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)
         ELSE
          VA= ONE +Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)*COH
         ENDIF
C
         YLD  = SIGM(I)
         CRIT = VM * SIGM2 - VA 
C
         SIGY(I) = YLD*SQRT(MAX(ZERO, VA))
         IF(CRIT < ZERO .OR. SIGY(I) == ZERO)THEN
          LAMDA(I) = ZERO
         ELSE
C
          LAMDA(I) = ZERO
          SIGXX(I) = SIGNXX(I)
          SIGYY(I) = SIGNYY(I)
          SIGZZ(I) = SIGNZZ(I)
          SIGXY(I) = SIGNXY(I)
          SIGZX(I) = SIGNZX(I)
          SIGYZ(I) = SIGNYZ(I)
C 
          IF(F(I)==ONE)THEN
           A21 = EP20
          ELSE             
           A21 =  SIGM1 /(ONE - F(I))     
          ENDIF
          A1 = FN*EXP(-HALF*((EPSM(I)-EPSN)/SN)**2)/SN
          A1 = A1*SQR22 
          DP11 = ZERO
          DP22 = ZERo
          DP33 = ZERO
          DP12 = ZERO
          DP13 = ZERO
          DP23 = ZERO         
          DO ITER=1,5
           IF(PN(I)<=ZERO)THEN
            D11 = (TWO*SIGNXX(I) - SIGNYY(I) - SIGNZZ(I))*SIGM2
            D22 = (TWO*SIGNYY(I) - SIGNXX(I) - SIGNZZ(I))*SIGM2
            D33 = (TWO*SIGNZZ(I) - SIGNXX(I) - SIGNYY(I))*SIGM2
            D12 = SIX*SIGNXY(I)*SIGM2
            D13 = SIX*SIGNZX(I)*SIGM2
            D23 = SIX*SIGNYZ(I)*SIGM2
            DCRM = -TWO * VM * SIGM2 * SIGM1
            DCRF =  TWO * Q1 * DF - TWO * Q3 * DF * FSTAR(I)  
           ELSE 
            VA1 =   Q1 * Q2 * FSTAR(I) * SIH*SIGM1 
            D11 = (TWO*SIGNXX(I) - SIGNYY(I) - SIGNZZ(I))*SIGM2 + VA1 
            D22 = (TWO*SIGNYY(I) - SIGNXX(I) - SIGNZZ(I))*SIGM2 + VA1
            D33 = (TWO*SIGNZZ(I) - SIGNXX(I) - SIGNYY(I))*SIGM2 + VA1       
            D12  = SIX * SIGNXY(I)*SIGM2
            D13  = SIX * SIGNZX(I)*SIGM2
            D23  = SIX * SIGNYZ(I)*SIGM2
            DCRM =-TWO* VM*SIGM1*SIGM2 -  THREE * VA1 * PN(I)*SIGM1
            DCRF = TWO * Q1*DF*COH - TWO*Q3*DF*FSTAR(I)
           ENDIF
C       
           A2 =   D11 * SIGNXX(I) + D22 * SIGNYY(I) + D33 * SIGNZZ(I) +
     .      TWO*(D12 * SIGNXY(I) + D13 * SIGNZX(I) + D23 * SIGNYZ(I) ) 
           A2 = A2 * A21
C
           IF(N==1)THEN
           DSEPP = ET*(ONE+ ( EPSP(I)*CSD)**VISP(I))
           ELSE
            DSEPP = ET * N * EPSM(I) ** (N - 1)*
     .      (ONE+ ( EPSP(I)*CSD)**VISP(I))
           ENDIF
           DCD   = C1(I) * (D11**2 + D22**2 + D33**2) + 
     .           TWO*C2(I)*(D11*D22 + D11*D33 + D22*D33) +
     .           TWO*G(I) *(D12**2 + D13**2 + D23**2)
           LAM1  = DCD - DCRM  *  DSEPP * A2 - 
     .           DCRF * ((ONE - F(I)) * (D11 + D22 + D33) + A1*A2)
           IF(LAM1/=ZERO) LAMDA(I) =  MAX(ZERO,(VM * SIGM2 - VA))/LAM1
C
C  PLASTIC DEFORMATION          
C
           DP11 = DP11 + LAMDA(I) * D11
           DP22 = DP22 + LAMDA(I) * D22
           DP33 = DP33 + LAMDA(I) * D33
           DP12 = DP12 + LAMDA(I) * D12
           DP13 = DP13 + LAMDA(I) * D13
           DP23 = DP23 + LAMDA(I) * D23
C   NEW STRESS
           SIGNXX(I)= SIGXX(I) - C1(I) * DP11 - C2(I) * (DP22 + DP33) 
           SIGNYY(I)= SIGYY(I) - C1(I) * DP22 - C2(I) * (DP11 + DP33) 
           SIGNZZ(I)= SIGZZ(I) - C1(I) * DP33 - C2(I) * (DP22 + DP11) 
           SIGNXY(I)= SIGXY(I) - TWO*G(I)  * DP12
           SIGNZX(I)= SIGZX(I) - TWO*G(I)  * DP13
           SIGNYZ(I)= SIGYZ(I) - TWO*G(I)  * DP23           
C
C....... EFFECTIVE PLASTIC DEFORMATION.
C
           EPSP1(I) = SIGNXX(I)*DP11 + SIGNYY(I)*DP22 + SIGNZZ(I)*DP33+
     .           TWO*(SIGNXY(I)*DP12 + SIGNZX(I)*DP13 + SIGNYZ(I)*DP23) 
           EPSP1(I) = EPSP1(I)*A21                       
           EPSP1(I) = MAX(ZERO, EPSP1(I))                
           PN(I)= (SIGNXX(I)+SIGNYY(I)+SIGNZZ(I))*THIRD
           SXX  = SIGNXX(I)-PN(I)
           SYY  = SIGNYY(I)-PN(I)
           SZZ  = SIGNZZ(I)-PN(I)
           VM = HALF*(SXX**2 + SYY**2 + SZZ**2)
     .        +   (SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2)
           VM = THREE*VM   
           VAR  = THREE_HALF * Q2 * PN(I)*SIGM1
           VAR = EXP(VAR)
           COH  = HALF * (VAR + ONE/MAX(EM20,VAR))
           SIH  = HALF * (VAR - ONE/MAX(EM20,VAR))
           IF(PN(I)<=ZERO) THEN
            VA = ONE + Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)
           ELSE
            VA = ONE + Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)*COH
           ENDIF
C
           YLD  = VA
          ENDDO ! ITER
          FG      = (ONE-F(I))*(DP11 + DP22 + DP33)
          SIGY(I) = SIGM(I)*SQRT(MAX(ZERO,VA))
          FN1     = A1 * EPSP1(I) 
          EPSM(I) = EPSM(I) + EPSP1(I)         
          F(I)    = F(I) + FG + FN1                    
          IF(F(I)<ZERO) F(I)=ZERO
          SIGM(I)= (YEILD0(I) + ET * EPSM(I)**N)*(ONE + 
     .            (EPSP(I)*CSD)**VISP(I))     
C....COMPUTE F*.....
          IF(F(I)<=FC)THEN
           FSTAR(I) = F(I)
          ELSE
           IF(FF==FC)THEN 
            FSTAR(I) = EP20
           ELSE
            FSTAR(I) = FC+ (FU-FC)*(F(I)-FC)/(FF-FC)
           ENDIF
          ENDIF
C
          UVAR(I , 1) = EPSM(I)
          UVAR(I , 2) = FSTAR(I)
          UVAR(I , 3) = SIGM(I)
          UVAR(I , 4) = F(I)
          UVAR(I , 5) = EPSP(I)          
C
         ENDIF 
         IF(OFF(I)==ONE.AND.FSTAR(I)>=FF)THEN
          OFF(I)=FOUR_OVER_5
          IDEL7NOK = 1
          WRITE(IOUT, 1000) NGL(I)
          WRITE(ISTDO,1100) NGL(I),TT
         ENDIF        
        ELSE         
         IF(OFF(I)<EM01)OFF(I)=ZERO
         OFF(I)=OFF(I)*FOUR_OVER_5      
        ENDIF
       ENDDO
      ELSEIF(IFLAG==2) THEN
C----------
C IFLAG set to 2
C..........
       DO I=1,NEL
        IF(OFF(I)==ONE)THEN
C
         IF(F(I)<=FC)THEN
          FSTAR(I) = F(I)
          DF       = ONE
         ELSE
          DF       = (FU - FC)/(FF - FC)
          FSTAR(I) = FC +  DF*(F(I)-FC)
         ENDIF
C
         PN(I)= (SIGNXX(I)+SIGNYY(I)+SIGNZZ(I))*THIRD
         SXX  = SIGNXX(I)-PN(I)
         SYY  = SIGNYY(I)-PN(I)
         SZZ  = SIGNZZ(I)-PN(I)
         VM = HALF*(SXX**2 + SYY**2 + SZZ**2)
     .       +   (SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2)
         VM = THREE*VM      
C
         SIGM1 = ONE/MAX(EM20,SIGM(I))
         SIGM2 = SIGM1**2
         VAR   = HALF *  Q2 * PN(I) * SIGM1 
         VAR = EXP(VAR)
         COH   = HALF*(VAR + ONE/MAX(EM20,VAR))
         SIH   = HALF*(VAR - ONE/MAX(EM20,VAR))
         IF(PN(I)<=ZERO) THEN
          VA = ONE +Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)
         ELSE
          VA= ONE+Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)*COH
         ENDIF
         YLD  = SIGM(I)
         CRIT = VM * SIGM2 - VA 
         SIGY(I) = YLD*SQRT(MAX(ZERO, VA))
         IF(CRIT < ZERO  .OR. SIGY(I) == ZERO)THEN
          LAMDA(I) = ZERO
         ELSE
          LAMDA(I) = ZERO
          SIGXX(I) = SIGNXX(I)
          SIGYY(I) = SIGNYY(I)
          SIGZZ(I) = SIGNZZ(I)
          SIGXY(I) = SIGNXY(I)
          SIGZX(I) = SIGNZX(I)
          SIGYZ(I) = SIGNYZ(I) 
C
          IF(F(I)==ONE)THEN
           A21 = EP20
          ELSE             
           A21 =  SIGM1 /(ONE - F(I))     
          ENDIF
          A1 = FN*EXP(-HALF*((EPSM(I)-EPSN)/SN)**2)/SN
          A1 = A1*SQR22 
C 
          DP11 = ZERO
          DP22 = ZERO
          DP33 = ZERO
          DP12 = ZERO
          DP13 = ZERO
          DP23 = ZERO         
C
          DO ITER=1,5
           IF(PN(I)<=ZERO)THEN
            D11 = (TWO*SIGNXX(I) - SIGNYY(I) - SIGNZZ(I))*SIGM2
            D22 = (TWO*SIGNYY(I) - SIGNXX(I) - SIGNZZ(I))*SIGM2
            D33 = (TWO*SIGNZZ(I) - SIGNXX(I) - SIGNYY(I))*SIGM2
            D12 = SIX*SIGNXY(I)*SIGM2
            D13 = SIX*SIGNZX(I)*SIGM2
            D23 = SIX*SIGNYZ(I)*SIGM2
            DCRM = -TWO * VM * SIGM2 * SIGM1
            DCRF =  TWO * Q1 * DF - TWO * Q3 * DF * FSTAR(I)
           ELSE 
            VA1 =   Q1 * Q2 * FSTAR(I) * SIH*SIGM1 
            D11 = (TWO*SIGNXX(I) - SIGNYY(I) - SIGNZZ(I))*SIGM2 + VA1 
            D22 = (TWO*SIGNYY(I) - SIGNXX(I) - SIGNZZ(I))*SIGM2 + VA1
            D33 = (TWO*SIGNZZ(I) - SIGNXX(I) - SIGNYY(I))*SIGM2 + VA1       
            D12  = SIX * SIGNXY(I)*SIGM2
            D13  = SIX * SIGNZX(I)*SIGM2
            D23  = SIX * SIGNYZ(I)*SIGM2
            DCRM =-TWO * VM*SIGM1*SIGM2 -  THREE * VA1 * PN(I)*SIGM1
            DCRF = TWO * Q1*DF*COH - TWO*Q3*DF*FSTAR(I)
           ENDIF
C       
           A2 =   D11 * SIGNXX(I) + D22 * SIGNYY(I) + D33 * SIGNZZ(I) +
     .      TWO*(D12 * SIGNXY(I) + D13 * SIGNZX(I) + D23 * SIGNYZ(I) ) 
           A2 = A2 * A21
C
           IF(N==1)THEN
            DSEPP = ET*(ONE + ( EPSP(I)*CSD)**VISP(I))
           ELSE
            DSEPP = ET * N * EPSM(I) ** (N - ONE)*
     .       (ONE + ( EPSP(I)*CSD)**VISP(I))
           ENDIF
           DCD   = C1(I) * (D11**2 + D22**2 + D33**2) + 
     .           TWO*C2(I)*(D11*D22 + D11*D33 + D22*D33) +
     .           TWO*G(I) *(D12**2 + D13**2 + D23**2)
           LAM1  = DCD - DCRM  *  DSEPP * A2 - 
     .           DCRF * ((ONE - F(I)) * (D11 + D22 + D33) + A1*A2)
           IF(LAM1/=ZERO) LAMDA(I) =  MAX(ZERO,(VM * SIGM2 - VA))/LAM1
C
C  PLASTIC DEFORMATION          
C
           DP11 = DP11 + LAMDA(I) * D11
           DP22 = DP22 + LAMDA(I) * D22
           DP33 = DP33 + LAMDA(I) * D33
           DP12 = DP12 + LAMDA(I) * D12
           DP13 = DP13 + LAMDA(I) * D13
           DP23 = DP23 + LAMDA(I) * D23
C   NEW STRESS
           SIGNXX(I)= SIGXX(I) - C1(I) * DP11 - C2(I) * (DP22 + DP33) 
           SIGNYY(I)= SIGYY(I) - C1(I) * DP22 - C2(I) * (DP11 + DP33) 
           SIGNZZ(I)= SIGZZ(I) - C1(I) * DP33 - C2(I) * (DP22 + DP11) 
           SIGNXY(I)= SIGXY(I) - TWO*G(I)  * DP12
           SIGNZX(I)= SIGZX(I) - TWO*G(I)  * DP13
           SIGNYZ(I)= SIGYZ(I) - TWO*G(I)  * DP23
C
C....... EFFECTIVE PLASTIC DEFORMATION.
C
           EPSP1(I) = SIGNXX(I)*DP11 + SIGNYY(I)*DP22 + SIGNZZ(I)*DP33+
     .           TWO*(SIGNXY(I)*DP12 + SIGNZX(I)*DP13 + SIGNYZ(I)*DP23)
           EPSP1(I) = EPSP1(I)*A21                         
           EPSP1(I) = MAX(ZERO, EPSP1(I))                

           PN(I)= (SIGNXX(I)+SIGNYY(I)+SIGNZZ(I))*THIRD
           SXX  = SIGNXX(I)-PN(I)
           SYY  = SIGNYY(I)-PN(I)
           SZZ  = SIGNZZ(I)-PN(I)
           VM = HALF*(SXX**2 + SYY**2 + SZZ**2)
     .        +   (SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2)
           VM = THREE*VM   
           VAR  = THREE_HALF * Q2 * PN(I)*SIGM1
           VAR = EXP(VAR)
           COH  = HALF * (VAR + ONE/MAX(EM20,VAR))
           SIH  = HALF * (VAR - ONE/MAX(EM20,VAR))
           IF(PN(I)<=ZERO) THEN
            VA = ONE + Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)
           ELSE
            VA = ONE + Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)*COH
           ENDIF
C
           YLD  = VA
          ENDDO ! ITER  
          FG      = (ONE -F(I))*(DP11 + DP22 + DP33)
          SIGY(I) = SIGM(I)*SQRT(MAX(ZERO,VA))
          FN1 = ZERO
          PN(I)= (SIGNXX(I)+SIGNYY(I)+SIGNZZ(I))*THIRD
          IF(PN(I)>=ZERO)FN1= A1 * EPSP1(I)          
          EPSM(I) = EPSM(I) + EPSP1(I)         
          F(I)    = F(I) + FG + FN1                    
          IF(F(I)<ZERO) F(I)=ZERO
          SIGM(I)= (YEILD0(I) + ET * EPSM(I)**N)*(ONE+ 
     .            (EPSP(I)*CSD)**VISP(I))
C....COMPUTE F*.....
          IF(F(I)<=FC)THEN
           FSTAR(I) = F(I)
          ELSE
           IF(FF==FC)THEN 
            FSTAR(I) = EP20
           ELSE
            FSTAR(I) = FC+ (FU-FC)*(F(I)-FC)/(FF-FC)
           ENDIF
          ENDIF
C
          UVAR(I , 1) = EPSM(I)
          UVAR(I , 2) = FSTAR(I)
          UVAR(I , 3) = SIGM(I)
          UVAR(I , 4) = F(I)
          UVAR(I , 5) = EPSP(I)
C
         ENDIF 
CCsm41w11 +++
         IF(OFF(I)==ONE.AND.FSTAR(I)>=FF)THEN
          OFF(I)=FOUR_OVER_5
          IDEL7NOK = 1
          WRITE(IOUT, 1000) NGL(I)
          WRITE(ISTDO,1100) NGL(I),TT
         ENDIF
        ELSE 

         IF(OFF(I)<EM01)OFF(I)=ZERO
         OFF(I)=OFF(I)*FOUR_OVER_5
        ENDIF
       ENDDO 
      ELSE
C-----
C  IFLAG = 3
C------------
C-------------
C 
C-----------
       DO I=1,NEL
        IF(OFF(I)==ONE)THEN
         IF(F(I)<=FC)THEN
          FSTAR(I) = F(I)
          DF       = ONE
         ELSE
          DF      = (FU- FC)/(FF-FC)
          FSTAR(I)= FC  + DF*(F(I)-FC)                 
         ENDIF 
C          
         PN(I)= (SIGNXX(I)+SIGNYY(I)+SIGNZZ(I))*THIRD
         SXX  = SIGNXX(I)-PN(I)
         SYY  = SIGNYY(I)-PN(I)
         SZZ  = SIGNZZ(I)-PN(I)
         VM = HALF*(SXX**2 + SYY**2 + SZZ**2)
     .        +   (SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2)
         VM = SQRT(THREE*VM)
         SIGM1 = ONE/MAX(SIGM(I),EM20)
         VAR  = THREE_HALF * Q2 * PN(I)* SIGM1
         VAR = EXP(VAR)
         COH  = HALF * (VAR + ONE/MAX(EM20,VAR))
         SIH  = HALF * (VAR - ONE/MAX(EM20,VAR)) 
         VA   = ONE + Q3 * FSTAR(I)**2 - TWO *Q1*FSTAR(I)*COH          
         VA= SQRT(MAX(ZERO,VA))
         YLD  = SIGM(I) * VA
C
         SIGY(I) = YLD           
         CRIT = VM - YLD
         IF(VM < YLD .OR. SIGY(I) == ZERO)THEN
          LAMDA(I)=ZERO        
         ELSE 
          LAMDA(I) = ZERO
          SIGXX(I) = SIGNXX(I)
          SIGYY(I) = SIGNYY(I)
          SIGZZ(I) = SIGNZZ(I)
          SIGXY(I) = SIGNXY(I)
          SIGZX(I) = SIGNZX(I)
          SIGYZ(I) = SIGNYZ(I)
C
          DP11 = ZERO
          DP22 = ZERO
          DP33 = ZERO
          DP12 = ZERO
          DP13 = ZERO
          DP23 = ZERO   
C
          IF(F(I)==ONE)THEN
           A21 = EP20
          ELSE             
           A21 =  SIGM1 /(ONE - F(I))     
          ENDIF
          A1 = FN*EXP(-HALF*((EPSM(I)-EPSN)/SN)**2)/SN
          A1 = A1*SQR22 
c                                   
          DO ITER=1,5                           
           VM1 = ONE/MAX(VM,EM20)
           VA1 = ONE/MAX(VA,EM20)
           VA11 =HALF*Q1*Q2*FSTAR(I)*SIH*VA1 
           D11 = HALF*(TWO*SIGNXX(I) 
     .                          - SIGNYY(I)-SIGNZZ(I))*VM1 + VA11   
           D22 = HALF*(TWO*SIGNYY(I) 
     .                          - SIGNXX(I)-SIGNZZ(I))*VM1 + VA11       
           D33 = HALF*(TWO*SIGNZZ(I) 
     .                          - SIGNXX(I)-SIGNYY(I))*VM1 + VA11
           D12 = THREE*SIGNXY(I)*VM1       
           D13 = THREE*SIGNZX(I)*VM1
           D23 = THREE*SIGNYZ(I)*VM1
C
           A2  = D11*SIGNXX(I) + D22*SIGNYY(I) + D33*SIGNZZ(I) +
     .            TWO*(D12*SIGNXY(I) + D13*SIGNZX(I) + D23*SIGNYZ(I)) 
           A2  = A2*A21
C
           DCRF=-SIGM(I)*(Q3*FSTAR(I)*DF-Q1*COH*DF)*VA1
           DCRM= - VA - THREE*VA11*PN(I)*SIGM1
           IF(N==1)THEN
            DSEPP = ET*(ONE + ( EPSP(I)*CSD)**VISP(I))
           ELSE
            DSEPP = ET * N * EPSM(I) ** (N - ONE)*
     .      (ONE + ( EPSP(I)*CSD)**VISP(I)) 
           ENDIF                   
           DCD = C1(I)*(D11**2 + D22**2 + D33**2) + 
     .            TWO*C2(I)*(D11*D22 + D11*D33 + D22*D33) +
     .            TWO*G(I)*(D12**2 + D13**2 + D23**2)

           LAM1= DCD - DCRM* DSEPP*A2 - 
     .             DCRF*((ONE-F(I))*(D11+D22+D33) + A1*A2)
           IF(LAM1/=ZERO) LAMDA(I) =  MAX(ZERO,(VM - YLD))/LAM1
C  PLASTIC DEFORMATION          
           DP11 = DP11 + LAMDA(I) * D11
           DP22 = DP22 + LAMDA(I) * D22
           DP33 = DP33 + LAMDA(I) * D33
           DP12 = DP12 + LAMDA(I) * D12
           DP13 = DP13 + LAMDA(I) * D13
           DP23 = DP23 + LAMDA(I) * D23
C   NEW STRESS
           SIGNXX(I)= SIGXX(I) - C1(I)*DP11 - C2(I)*(DP22 + DP33) 
           SIGNYY(I)= SIGYY(I) - C1(I)*DP22 - C2(I)*(DP11 + DP33) 
           SIGNZZ(I)= SIGZZ(I) - C1(I)*DP33 - C2(I)*(DP22 + DP11) 
           SIGNXY(I)= SIGXY(I) - TWO*G(I)*DP12
           SIGNZX(I)= SIGZX(I) - TWO*G(I)*DP13
           SIGNYZ(I)= SIGYZ(I) - TWO*G(I)*DP23
C....... AMISSIBLE PLASTIC DEFORMATION.
           EPSP1(I)= SIGNXX(I)*DP11 + SIGNYY(I)*DP22 + SIGNZZ(I)*DP33
     .        + TWO*(SIGNXY(I)*DP12 + SIGNZX(I)*DP13 + SIGNYZ(I)*DP23)
           EPSP1(I)=EPSP1(I)*A21
           EPSP1(I) = MAX(ZERO, EPSP1(I))
           PN(I)= (SIGNXX(I)+SIGNYY(I)+SIGNZZ(I))*THIRD
           SXX  = SIGNXX(I)-PN(I)
           SYY  = SIGNYY(I)-PN(I)
           SZZ  = SIGNZZ(I)-PN(I)
           VM = HALF*(SXX**2 + SYY**2 + SZZ**2)
     .        +   (SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2)
           VM = SQRT(THREE*VM)
           VAR  = THREE_HALF * Q2*PN(I) *SIGM1
           VAR = EXP( VAR)
           COH  = HALF * ( VAR + ONE/MAX(EM20,VAR))
           SIH  = HALF * ( VAR - ONE/MAX(EM20,VAR))
           VA   = ONE + Q3*FSTAR(I)**2 - TWO*Q1*FSTAR(I)*COH
           VA = SQRT(MAX(ZERO,VA))
           YLD  = SIGM(I) * VA
          ENDDO ! ITER
C
          FG      = (ONE-F(I))*(DP11+DP22+DP33)
          PN(I) = (SIGXX(I) + SIGYY(I) + SIGZZ(I))*THIRD
          SIGY(I) = YLD 
          FN1     = ZERO
          IF(PN(I)>=ZERO) FN1 = A1*EPSP1(I) 
           EPSM(I) = EPSM(I) + EPSP1(I)         
           F(I)    = F(I) + FG + FN1                    
          IF(Q1==ZERO.and.
     .     Q2==ZERO.and.Q3==ZERO) F(I)= ZERO
          IF(F(I)<ZERO) F(I)=ZERO
          SIGM(I) = (YEILD0(I) + ET * EPSM(I) ** N)*
     .             (ONE + (EPSP(I)*CSD) ** VISP(I))          
C....COMPUTE F*.....
          IF(F(I)<=FC)THEN
           FSTAR(I) = F(I)
          ELSE
           IF(FF==FC)THEN
            FSTAR(I) = EP20
           ELSE
            FSTAR(I) = FC+ (FU-FC)/(FF-FC)*(F(I)-FC)
           ENDIF
          ENDIF
          UVAR(I,1) = EPSM(I)
          UVAR(I,2) = FSTAR(I)
          UVAR(I,3) = SIGM(I)
          UVAR(I,4) = F(I)
          UVAR(I,5) = EPSP(I)          
         ENDIF
         IF(OFF(I)==ONE.AND.FSTAR(I)>=FF)THEN
          OFF(I)=FOUR_OVER_5
          IDEL7NOK = 1
          WRITE(IOUT, 1000) NGL(I)
          WRITE(ISTDO,1100) NGL(I),TT
         ENDIF
        ELSE 
         IF(OFF(I)<EM01)OFF(I)=ZERO
         OFF(I)=OFF(I)*FOUR_OVER_5
        ENDIF
       ENDDO
      ENDIF 
      DO I=1,NEL
        DEFP(I)=UVAR(I,1)
      ENDDO
C
C tabulated law
       IF(IYEILD_TAB > 0) THEN
         IADBUF = IPM(7,MAT(1))-1
         XFAC  = UPARAM(IADBUF + 22 )
         YFAC  = UPARAM(IADBUF + 23 )
         DO I=1,NEL
           NDIM= TABLE(ITAB(1))%NDIM
            IF(NDIM==2)THEN
              NXK=SIZE(TABLE(ITAB(1))%X(2)%VALUES)         
              DO J=2,NXK
                 DX2 = TABLE(ITAB(1))%X(2)%VALUES(J)*XFAC- EPSP(I) 
                 IF(DX2 >= ZERO .OR. J == NXK)THEN
                  IPOS=J-1
                  R=(TABLE(ITAB(1))%X(2)%VALUES(J)*XFAC-EPSP(I))/
     .                 (TABLE(ITAB(1))%X(2)%VALUES(J)*XFAC
     .                 -TABLE(ITAB(1))%X(2)%VALUES(J-1)*XFAC)
                  EXIT
                 ENDIF
              END DO ! NXK
            ELSE
                   R = ONE
            ENDIF ! NDIM
C   interpolation on  the curve ---
           XX(1) = EPSM(I)  
           XX(2) = EPSP(I)*XFAC
           CALL TABLE_INTERP_LAW76(TABLE(ITAB(1)),IPOS,XX,
     .                     R,DFT,YY) 
            UVAR(I,3)= YFAC*YY
         ENDDO ! NEL0
       ENDIF ! IYEILD_TAB
C ---------------------------------------------------------------------
 1000 FORMAT(1X,'RUPTURE OF SOLID ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'RUPTURE OF SOLID ELEMENT NUMBER ',I10,
     .          ' AT TIME :',G20.12)         
        RETURN
      END
C
