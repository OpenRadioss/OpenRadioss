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
Chd|  SIGEPS70                      source/materials/mat/mat070/sigeps70.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS70(
     1     NEL    ,NUPARAM,NUVAR   ,MFUNC   ,KFUNC   ,NPF    ,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0    ,RHO    ,
     3     VOLUME ,EINT   ,
     4     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIG0XX ,SIG0YY ,SIG0ZZ  ,SIG0XY  ,SIG0YZ  ,SIG0ZX ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVZZ  ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX,UVAR    ,OFF     ,NGL     ,
     B     PM     ,IPM    , MAT    ,EPSP    )
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
C SIG0XX  | NEL     | F | R | OLD ELASTO PLASTIC STRESS XX 
C SIG0YY  | NEL     | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |    
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SIGVXX  | NEL     | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL     | F | W | VISCOUS STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL     | F | W | MAXIMUN DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
C
      INTEGER NEL, NUPARAM, NUVAR,IPT,
     .   NGL(NEL),MAT(NEL),IPLA,IPM(NPROPMI,*)
      my_real
     .   TIME,TIMESTEP,UPARAM(*),
     .   RHO(NEL),RHO0(NEL),VOLUME(NEL),EINT(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),EPSPZZ(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,EPSZZ(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIG0XX(NEL),SIG0YY(NEL),SIG0ZZ(NEL),
     .   SIG0XY(NEL),SIG0YZ(NEL),SIG0ZX(NEL),
     .   PM(NPROPM,*),EPSP(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),SIGVZZ(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real 
     .        UVAR(NEL,NUVAR), OFF(NEL),  PLA(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
      my_real
     .   TF(*),FINTER
      EXTERNAL FINTER
C   EXTERNAL FINTER
C        Y = FINTER(IFUNC(J),X,NPF,TF,DYDX)
C        Y       : y = f(x)
C        X       : x
C        DYDX    : f'(x) = dy/dx
C        IFUNC(J): FUNCTION INDEX
C              J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
C        NPF,TF  : FUNCTION PARAMETER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,J1,J2,I1,I2,IADBUF,NC,NFUNC,ILOAD(MVSIZ),
     .        IFUNC(MVSIZ,100),NLOAD(MVSIZ),NUNLOAD(MVSIZ),
     .         IE_CST(MVSIZ),ILOAD0,IDAMAGE,IFLAG,IETANG,IPFUNC
      my_real
     .        R,FAC,YP1,YP2,YN1,YN2,COEFF,RMIN,RMAX,
     .        E,E1,E2,E3,E4,E5,E6,DF,BB,CC,DELTA,ALPHA,X1,X2,SVM2,
     .        SVM,
     .        E0(MVSIZ),G(MVSIZ),NU(MVSIZ),AA1(MVSIZ),AA2(MVSIZ),
     .        FAIL(MVSIZ),
     .        EPST(MVSIZ),AA(MVSIZ),YLDMIN(MVSIZ),YLDMAX(MVSIZ),
     .        YLD(MVSIZ),RATE(MVSIZ,2),
     .        YFAC(MVSIZ,2),SIG0(MVSIZ),EPS0(MVSIZ),EPSS(MVSIZ),
     .        EMAX,EPSSMAX(MVSIZ),DF1,DF2,DAV,
     .        EPS_MAX,DSIG ,YLDELAS(MVSIZ),P,SVM1(MVSIZ),EXPO,HYS,
     .        ETAN(MVSIZ), DFMIN(MVSIZ), DFMAX(MVSIZ),DFELAS(MVSIZ),
     .        EOLD,DAM,DD,A0,A1,DE,YFACC,MU
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
C
      DO I= 1, NEL
        NFUNC  = IPM(10,MAT(I))
        DO J=1,NFUNC
         IFUNC(I,J)=IPM(10+J,MAT(I))
        ENDDO
      ENDDO
C     
      DO I=1,NEL
        IADBUF    = IPM(7,MAT(I))-1
        E0(I)     = UPARAM(IADBUF+2)
        AA(I)     = UPARAM(IADBUF+3)
        EPSSMAX(I)= UPARAM(IADBUF+4)
        G(I)      = UPARAM(IADBUF+5)
        NU(I)     = UPARAM(IADBUF+6)
        NLOAD(I)  = UPARAM(IADBUF+7)
        NUNLOAD(I)= UPARAM(IADBUF+8)
        IDAMAGE   = UPARAM(IADBUF+ 2*NFUNC + 9)
        EXPO = UPARAM(IADBUF+ 2*NFUNC + 10)
        HYS  = UPARAM(IADBUF+ 2*NFUNC + 11)
        EMAX  = UPARAM(IADBUF+ 2*NFUNC + 12)
        IFLAG  = UPARAM(IADBUF+ 2*NFUNC + 13)
        YFACC =  UPARAM(IADBUF+ 8 + 2*NFUNC )
      ENDDO
C-----------------------------------------------
      DO I=1,NEL
C-------------------
C     epst_spherique
C-------------------       
        EPST(I) = EPSXX(I)**2+EPSYY(I)**2 + EPSZZ(I)**2 +
     .            UNDEMI*(EPSXY(I)**2+EPSYZ(I)**2+EPSZX(I)**2)
        EPST(I) = SQRT(EPST(I)) 
        EPST(I) = MIN(UN, EPST(I))
C-------------------
C     estimation elastique
C-------------------
        EPS0(I) = UVAR(I,1)
        SIG0(I) = UVAR(I,2)
      ENDDO            
C-------------------
C CRITERE
C-------------------
      DO I=1,NEL
C YLD_elast
          IADBUF = IPM(7,MAT(I))-1
          RATE(I,1)=UPARAM(IADBUF + 9)
          YFAC(I,1)=UPARAM(IADBUF + 9 + NFUNC )
          IF(EPST(I) >= EPSSMAX(I)) THEN
            YLDELAS(I)=YFAC(I,1)*FINTER(IFUNC(I,1),EPSSMAX(I),NPF,TF,DF)
            YLDELAS(I)=EMAX*(EPST(I) - EPSSMAX(I)) +  YLDELAS(I)  
          ELSE
            YLDELAS(I) = YFAC(I,1)*FINTER(IFUNC(I,1),EPST(I),NPF,TF,DF)
          ENDIF  
C  yld_max
          NC = NLOAD(I)
          J1 = 1
          DO J=2,NC-1
           IADBUF = IPM(7,MAT(I)) - 1        
           IF(ABS(EPSP(I)) >= ABS(UPARAM(IADBUF+ 8 + J )))THEN
            J1 = J
           ENDIF
          ENDDO    
          IADBUF = IPM(7,MAT(I))-1
          RATE(I,1)=UPARAM(IADBUF + 8 + J1)
          YFAC(I,1)=UPARAM(IADBUF + 8 + NFUNC + J1)
C          
          IF(EPST(I) >= EPSSMAX(I)) THEN
           IF(NC > 1)THEN   
            J2 = J1+1      
            RATE(I,2)=UPARAM(IADBUF + 8 + J2 )   
            YFAC(I,2)=UPARAM(IADBUF + 8 + NFUNC + J2 )
C
            YP1 = YFAC(I,1)*FINTER(IFUNC(I,J1),EPSSMAX(I),NPF,TF,DF1)
            YP2 = YFAC(I,2)*FINTER(IFUNC(I,J2),EPSSMAX(I),NPF,TF,DF2)     
C          
            FAC    = (EPSP(I) - RATE(I,1))/(RATE(I,2) - RATE(I,1))
            YLDMAX(I) = MAX(YP1 + FAC*(YP2 - YP1), EM20)   
  
               
            YLDMAX(I) = EMAX*(EPST(I) - EPSSMAX(I)) +  YLDMAX(I)  
C           
           ELSE
            YLDMAX(I) = 
     .        YFAC(I,1)*FINTER(IFUNC(I,J1),EPSSMAX(I),NPF,TF,DF)    
            YLDMAX(I) = EMAX*(EPST(I) - EPSSMAX(I)) +  YLDMAX(I) 
           ENDIF          
          
          ELSE
           IF(NC > 1)THEN   
            J2 = J1+1      
            RATE(I,2)=UPARAM(IADBUF + 8 + J2 )   
            YFAC(I,2)=UPARAM(IADBUF + 8 + NFUNC + J2 )
C
            YP1 = YFAC(I,1)*FINTER(IFUNC(I,J1),EPST(I),NPF,TF,DF1)
            YP2 = YFAC(I,2)*FINTER(IFUNC(I,J2),EPST(I),NPF,TF,DF2)
        
C           
            FAC    = (EPSP(I) - RATE(I,1))/(RATE(I,2) - RATE(I,1))
            YLDMAX(I) = MAX(YP1 + FAC*(YP2 - YP1), EM20)
           ELSE
            YLDMAX(I) = YFAC(I,1)*FINTER(IFUNC(I,J1),EPST(I),NPF,TF,DF)
           ENDIF
          ENDIF  
C  yld_min  
          NC = NUNLOAD(I)
          J1 = 1 + NLOAD(I)
          YLDMIN(I) = ZERO
          IF(NC > 0 ) THEN
           DO J=2,NC-1
             IADBUF = IPM(7,MAT(I)) - 1        
             IF(ABS(EPSP(I)) >= 
     .                     ABS(UPARAM(IADBUF+ NLOAD(I) + 8 + J )))THEN 
               J1 = NLOAD(I) + J
             ENDIF
           ENDDO
C         
           RATE(I,1)=UPARAM(IADBUF + 8 + J1)
           YFAC(I,1)=UPARAM(IADBUF + 8 + NFUNC + J1)

           IF(EPST(I) >= EPSSMAX(I)) THEN
             IF(NC > 1)THEN   
               J2 = J1+1      
                RATE(I,2)=UPARAM(IADBUF + 8 + J2 )   
                YFAC(I,2)=UPARAM(IADBUF + 8 + NFUNC + J2 )
C
                YP1 =YFAC(I,1)*FINTER(IFUNC(I,J1),EPSSMAX(I),NPF,TF,DF1)
                YP2 =YFAC(I,2)*FINTER(IFUNC(I,J2),EPSSMAX(I),NPF,TF,DF2)  
C            
                IF(YP2 < YP1 ) THEN          
                  FAC    = (RATE(I,2) - EPSP(I))/(RATE(I,2) - RATE(I,1))
                  YLDMIN(I) = MAX(YP2 + FAC*(YP1-YP2), EM20)
               ELSE
                  FAC    = (EPSP(I) - RATE(I,1))/(RATE(I,2) - RATE(I,1))
                  YLDMIN(I) = MAX(YP1 + FAC*(YP2 - YP1), EM20)
               ENDIF
                YLDMIN(I) =  EMAX*(EPST(I) - EPSSMAX(I)) + YLDMIN(I)
             ELSE
              YLDMIN(I)= 
     .          YFAC(I,1)*FINTER(IFUNC(I,J1),EPSSMAX(I),NPF,TF,DF) 
                YLDMIN(I) =  EMAX*(EPST(I) - EPSSMAX(I)) + YLDMIN(I)
             ENDIF           
            
           ELSE
             IF(NC > 1)THEN   
              J2 = J1+1      
              RATE(I,2)=UPARAM(IADBUF + 8 + J2 )   
              YFAC(I,2)=UPARAM(IADBUF + 8 + NFUNC + J2 )
C
               YP1 = YFAC(I,1)*FINTER(IFUNC(I,J1),EPST(I),NPF,TF,DF1)
               YP2 = YFAC(I,2)*FINTER(IFUNC(I,J2),EPST(I),NPF,TF,DF2)       
C                           
               IF(YP2 < YP1 ) THEN          
                  FAC    = (RATE(I,2) - EPSP(I))/(RATE(I,2) - RATE(I,1))
                  YLDMIN(I) = MAX(YP2 + FAC*(YP1-YP2), EM20)
                
               ELSE
                  FAC    = (EPSP(I) - RATE(I,1))/(RATE(I,2) - RATE(I,1))
                  YLDMIN(I) = MAX(YP1 + FAC*(YP2 - YP1), EM20)
               ENDIF
             ELSE
               YLDMIN(I)=YFAC(I,1)*FINTER(IFUNC(I,J1),EPST(I),NPF,TF,DF)
             ENDIF
            ENDIF  
           ENDIF        
         ENDDO 
C
         IF(IFLAG > 0) THEN
           DO I=1,NEL
             ALPHA = UN/MAX(EM20,UVAR(I,10))
             SIG0XX(I) = ALPHA*SIG0XX(I)
             SIG0YY(I) = ALPHA*SIG0YY(I)
             SIG0ZZ(I) = ALPHA*SIG0ZZ(I)
             SIG0XY(I) = ALPHA*SIG0XY(I)
             SIG0ZX(I) = ALPHA*SIG0ZX(I)
             SIG0YZ(I) = ALPHA*SIG0YZ(I)
           ENDDO 
         ENDIF
C                 
C =====================================================
         DO I = 1,NEL
          ILOAD0 = UVAR(I,5)        
          IE_CST(I)= 0
          DELTA = EPST(I) - UVAR(I,4)
          E = UVAR(I,3)
          UVAR(I,9) = UVAR(I,4)
          IF(DELTA >= ZERO)THEN
           YLD(I) = YLDMAX(I)
           ILOAD(I) = 1     
          ELSEIF(DELTA < ZERO)THEN
           YLD(I) = YLDMIN(I)
           ILOAD(I) = -1
           IF(IDAMAGE /= 0 )THEN
             YLD(I) = YLDMAX(I)
           ENDIF
          ENDIF  
C         
          EPSS(I) = EPST(I) - YLD(I)/ E
          EPSS(I) = MAX(ZERO, EPSS(I))  
          DE = AA(I)*(EPSS(I) - UVAR(I,1))
C          E = E0(I) + AA(I)*EPSS(I)
c          IF(ILOAD(I) == 1)  THEN
c            IF(ILOAD0 == 1) E = MAX(E, UVAR(I,3))
c          ENDIF
          IF(ILOAD(I) == 1) THEN
             E = E + MAX(DE, ZERO)
             IF(ILOAD0 == -1) E= UVAR(I,3)
             UVAR(I,1) = MAX(UVAR(I,1), EPSS(I))
          ELSE
             E = E + MIN(DE ,ZERO)
             IF(ILOAD0 == 1) E= UVAR(I,3)
             UVAR(I,1) = MIN(EPSS(I),UVAR(I,1))
          ENDIF
CC          UVAR(I,1) = EPSS(I)
           E = MIN(E, EMAX)
           E = MAX(E, E0(I))
           UVAR(I,3) = E
C==================================================       
          AA1(I) = E*(UN-NU(I))/(UN + NU(I))/(UN - DEUX*NU(I))
          AA2(I) = AA1(I)*NU(I)/(UN - NU(I))  
          G(I) =UNDEMI*E/(UN + NU(I))
C ---- 
          SIGNXX(I)= AA1(I)*DEPSXX(I) +  AA2(I)*(DEPSYY(I) + DEPSZZ(I))
          SIGNYY(I)= AA1(I)*DEPSYY(I) +  AA2(I)*(DEPSXX(I) + DEPSZZ(I))
          SIGNZZ(I)= AA1(I)*DEPSZZ(I) +  AA2(I)*(DEPSXX(I) + DEPSYY(I))
          SIGNXY(I)= G(I) *DEPSXY(I)
          SIGNYZ(I)= G(I) *DEPSYZ(I)
          SIGNZX(I)= G(I) *DEPSZX(I)
          DSIG = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)      
     .       + DEUX*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2) 
          DSIG =SQRT(DSIG)                         
C   estimate stress   
          SIGNXX(I)=SIG0XX(I) +  AA1(I)*DEPSXX(I)
     .                       +  AA2(I)*(DEPSYY(I) + DEPSZZ(I))
          SIGNYY(I)=SIG0YY(I) +  AA1(I)*DEPSYY(I)
     .                       +  AA2(I)*(DEPSXX(I) + DEPSZZ(I))
          SIGNZZ(I)=SIG0ZZ(I) +  AA1(I)*DEPSZZ(I)
     .                       +  AA2(I)*(DEPSXX(I) + DEPSYY(I))
          SIGNXY(I)=SIG0XY(I) + G(I) *DEPSXY(I)
          SIGNYZ(I)=SIG0YZ(I) + G(I) *DEPSYZ(I)
          SIGNZX(I)=SIG0ZX(I) + G(I) *DEPSZX(I)

          SVM2 = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)      
     .       + DEUX*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2) 
          SVM =SQRT(SVM2)
C sound velocity         
          SOUNDSP(I) = SQRT(AA1(I)/RHO0(I))
          VISCMAX(I) = ZERO  
C            
         IF(IDAMAGE == 0 ) THEN
!!           IF(SVM >= YLDMAX(I) )THEN
!!             YLD(I) = YLDMAX(I)
!!             IF(DELTA < ZERO ) YLD(I) = YLDMIN(I)
!!           ELSEIF(SVM <= YLDMIN(I).AND. ILOAD(I) == -1) THEN
!!             YLD(I) = YLDMIN(I)
C           
!!           ELSE
!!            YLD(I) = SVM
!!            IE_CST(I) = 1
!!            IF(DELTA < ZERO .AND. DSIG > YLDMIN(I) .AND. DSIG > SVM)THEN
!!             YLD(I) = YLDMIN(I)
!!             IE_CST(I) = 0
!!            ENDIF
!!           ENDIF
            IF(DELTA >= ZERO ) THEN
                 YLD(I) = MIN(YLDMAX(I),SVM)
            ELSEIF(DELTA < ZERO) THEN
                YLD(I) = MAX(YLDMIN(I),SVM)
                YLD(I) = MIN(YLD(I),UVAR(I,6))
                IE_CST(I) = 1
                IF( DSIG > YLDMIN(I) .AND. DSIG > SVM)THEN
                  YLD(I) = YLDMIN(I)
                  IE_CST(I) = 0
                ENDIF
            ENDIF
          ELSE
             YLD(I) = YLDMAX(I)
              IF(DELTA > ZERO .AND. SVM < YLDMAX(I))YLD(I)=SVM
cc              IF(DELTA < ZERO .AND. SVM > YLDMAX(I)
cc     .                         .AND. DSIG < SVM )YLD(I) = SVM    
              UVAR(I,8) = UVAR(I,8) + 
     .                    UNDEMI*(YLD(I) + UVAR(I,6))*DELTA
              UVAR(I,8) = MAX(ZERO, UVAR(I,8))
              UVAR(I,2) = MAX(UVAR(I,2) , UVAR(I,8))  
          ENDIF
         ENDDO 
C 
C-------------------
C projection spherique
C-------------------
      
      IF(IDAMAGE == 0 ) THEN
       DO I=1,NEL        
        SIGNXX(I)= AA1(I)*EPSXX(I) + AA2(I)*(EPSYY(I)  + EPSZZ(I))
        SIGNYY(I)= AA1(I)*EPSYY(I) + AA2(I)*(EPSXX(I)  + EPSZZ(I))
        SIGNZZ(I)= AA1(I)*EPSZZ(I) + AA2(I)*(EPSXX(I)  + EPSYY(I))
        SIGNXY(I)= G(I) *EPSXY(I)
        SIGNYZ(I)= G(I) *EPSYZ(I)
        SIGNZX(I)= G(I) *EPSZX(I)
C
        SVM2 = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)      
     .       + DEUX*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2) 
        SVM =SQRT(SVM2)
        R  = YLD(I)/MAX(EM20,SVM)          
        SIGNXX(I)=SIGNXX(I)*R                                
        SIGNYY(I)=SIGNYY(I)*R                                
        SIGNZZ(I)=SIGNZZ(I)*R                                
        SIGNXY(I)=SIGNXY(I)*R                                
        SIGNYZ(I)=SIGNYZ(I)*R                                
        SIGNZX(I)=SIGNZX(I)*R     
c        E = E0(I) + AA(I)*EPS0(I)
c        EPSS(I) = EPST(I) - YLD(I)/E
c        EPSS(I) = MAX(EPSS(I), ZERO)        
c        UVAR(I,1) = EPSS(I)
c        E = E0(I) + AA(I)*EPSS(I)
c        UVAR(I,3) = E 
C                  
        IF(IE_CST(I) == 1) THEN
         ILOAD0 = INT(UVAR(I,5))
         IF(ILOAD0 /= ILOAD(I))THEN
           ILOAD(I)  = ILOAD0
           UVAR(I,1) = EPS0(I)
cc           UVAR(I,3) = E0(I) + AA(I)*EPS0(I)
         ENDIF
        ENDIF   
        UVAR(I,4) = EPST(I)
        UVAR(I,2) = SVM*R
        UVAR(I,5) = ILOAD(I)
        UVAR(I,6) = YLD(I)
        UVAR(I,7) = EPSP(I)
cc        UVAR(I,9) = UN
       ENDDO
      ELSEIF(IDAMAGE == 1) THEN
       DO I=1,NEL
C        
         R = UN
         SIGNXX(I)= AA1(I)*EPSXX(I) + AA2(I)*(EPSYY(I)  + EPSZZ(I))
         SIGNYY(I)= AA1(I)*EPSYY(I) + AA2(I)*(EPSXX(I)  + EPSZZ(I))
         SIGNZZ(I)= AA1(I)*EPSZZ(I) + AA2(I)*(EPSXX(I)  + EPSYY(I))
         SIGNXY(I)= G(I) *EPSXY(I)
         SIGNYZ(I)= G(I) *EPSYZ(I)
         SIGNZX(I)= G(I) *EPSZX(I)
C
         SVM2 = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)      
     .       + DEUX*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2) 
         SVM =SQRT(SVM2)
         R  = (YLD(I)/MAX(EM20,SVM))        
         SIGNXX(I)=SIGNXX(I)*R                                
         SIGNYY(I)=SIGNYY(I)*R                                
         SIGNZZ(I)=SIGNZZ(I)*R                                
         SIGNXY(I)=SIGNXY(I)*R                                
         SIGNYZ(I)=SIGNYZ(I)*R                                
         SIGNZX(I)=SIGNZX(I)*R   
C                 
         IF(ILOAD(I) == -1) THEN
            R = (YLDMIN(I)/MAX(EM20,YLDELAS(I)))
            P = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
            SIGNXX(I)=(SIGNXX(I) - P)*R + P 
            SIGNYY(I)=(SIGNYY(I) - P)*R + P                             
            SIGNZZ(I)=(SIGNZZ(I) - P)*R + P                             
            SIGNXY(I)=SIGNXY(I)*R                             
            SIGNYZ(I)=SIGNYZ(I)*R                             
            SIGNZX(I)=SIGNZX(I)*R
cc            UVAR(I,9) = R
          ENDIF        
C        
         SVM2 = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)        
     .     + DEUX*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2)   
         SVM =SQRT(SVM2)                                          
c         E = E0(I) + AA(I)*EPS0(I)                                
c         EPSS(I) = EPST(I) - YLD(I)/E
c         EPSS(I) = MAX(EPSS(I), ZERO)                  
c         E = E0(I) + AA(I)*EPSS(I)
c         UVAR(I,1) = EPSS(I) 
c         UVAR(I,3) = E
         UVAR(I,4) = EPST(I)
         UVAR(I,5) = ILOAD(I)
         UVAR(I,6) = YLD(I)
         UVAR(I,7) = EPSP(I)       
        ENDDO
       ELSEIF(IDAMAGE == 2 ) THEN
        DO I=1,NEL
C        
         SIGNXX(I)= AA1(I)*EPSXX(I) + AA2(I)*(EPSYY(I)  + EPSZZ(I))
         SIGNYY(I)= AA1(I)*EPSYY(I) + AA2(I)*(EPSXX(I)  + EPSZZ(I))
         SIGNZZ(I)= AA1(I)*EPSZZ(I) + AA2(I)*(EPSXX(I)  + EPSYY(I))
         SIGNXY(I)= G(I) *EPSXY(I)
         SIGNYZ(I)= G(I) *EPSYZ(I)
         SIGNZX(I)= G(I) *EPSZX(I)
C
         SVM2 = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)      
     .       + DEUX*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2) 
         SVM =SQRT(SVM2)
         R  = YLD(I)/MAX(EM20,SVM)         
         SIGNXX(I)=SIGNXX(I)*R                                
         SIGNYY(I)=SIGNYY(I)*R                                
         SIGNZZ(I)=SIGNZZ(I)*R                                
         SIGNXY(I)=SIGNXY(I)*R                                
         SIGNYZ(I)=SIGNYZ(I)*R                                
         SIGNZX(I)=SIGNZX(I)*R   
C         
         IF(ILOAD(I) == -1) THEN
            R = YLDMIN(I)/MAX(EM20,YLDELAS(I))
            SIGNXX(I)=SIGNXX(I)*R 
            SIGNYY(I)=SIGNYY(I)*R                             
            SIGNZZ(I)=SIGNZZ(I)*R                             
            SIGNXY(I)=SIGNXY(I)*R                             
            SIGNYZ(I)=SIGNYZ(I)*R                             
            SIGNZX(I)=SIGNZX(I)*R
CC            UVAR(I,9) = R
          ENDIF        
C        
         SVM2 = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)        
     .     + DEUX*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2)   
         SVM =SQRT(SVM2)                                          
c         E = E0(I) + AA(I)*EPS0(I)                                
c         EPSS(I) = EPST(I) - YLD(I)/E
c         EPSS(I) = MAX(EPSS(I), ZERO)                                    
c         UVAR(I,3) = E0(I) + AA(I)*EPSS(I)  
c         UVAR(I,1) = EPSS(I)  
         UVAR(I,4) = EPST(I)
         UVAR(I,5) = ILOAD(I)
         UVAR(I,6) = YLD(I)
         UVAR(I,7) = EPSP(I)
        ENDDO       

      ELSEIF(IDAMAGE == 3) THEN
       DO I=1,NEL
C        
         SIGNXX(I)= AA1(I)*EPSXX(I) + AA2(I)*(EPSYY(I)  + EPSZZ(I))
         SIGNYY(I)= AA1(I)*EPSYY(I) + AA2(I)*(EPSXX(I)  + EPSZZ(I))
         SIGNZZ(I)= AA1(I)*EPSZZ(I) + AA2(I)*(EPSXX(I)  + EPSYY(I))
         SIGNXY(I)= G(I) *EPSXY(I)
         SIGNYZ(I)= G(I) *EPSYZ(I)
         SIGNZX(I)= G(I) *EPSZX(I)
C
         SVM2 = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)      
     .       + DEUX*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2) 
         SVM =SQRT(SVM2)
         R  = (YLD(I)/MAX(EM20,SVM))        
         SIGNXX(I)=SIGNXX(I)*R                                
         SIGNYY(I)=SIGNYY(I)*R                                
         SIGNZZ(I)=SIGNZZ(I)*R                                
         SIGNXY(I)=SIGNXY(I)*R                                
         SIGNYZ(I)=SIGNYZ(I)*R                                
         SIGNZX(I)=SIGNZX(I)*R   
C         
         IF(ILOAD(I) == -1) THEN
            R = UN - (UVAR(I,8)/MAX(EM20,UVAR(I,2)))**EXPO
            R = UN - (UN - HYS)*R
            P = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
            SIGNXX(I)=(SIGNXX(I) - P)*R + P 
            SIGNYY(I)=(SIGNYY(I) - P)*R + P                             
            SIGNZZ(I)=(SIGNZZ(I) - P)*R + P                             
            SIGNXY(I)=SIGNXY(I)*R                             
            SIGNYZ(I)=SIGNYZ(I)*R                             
            SIGNZX(I)=SIGNZX(I)*R
CC            UVAR(I,9) = R
          ENDIF        
C                             
c         E = E0(I) + AA(I)*EPS0(I)                                
c         EPSS(I) = EPST(I) - YLD(I)/E
c         EPSS(I) = MAX(EPSS(I), ZERO)                  
c         E = E0(I) + AA(I)*EPSS(I)
c         UVAR(I,1) = EPSS(I)    
c         UVAR(I,3) = E                                            
         UVAR(I,4) = EPST(I)
         UVAR(I,5) = ILOAD(I)
         UVAR(I,6) = YLD(I)
         UVAR(I,7) = EPSP(I)
        ENDDO
      ELSEIF(IDAMAGE == 4) THEN
       DO I=1,NEL
C        
         SIGNXX(I)= AA1(I)*EPSXX(I) + AA2(I)*(EPSYY(I)  + EPSZZ(I))
         SIGNYY(I)= AA1(I)*EPSYY(I) + AA2(I)*(EPSXX(I)  + EPSZZ(I))
         SIGNZZ(I)= AA1(I)*EPSZZ(I) + AA2(I)*(EPSXX(I)  + EPSYY(I))
         SIGNXY(I)= G(I) *EPSXY(I)
         SIGNYZ(I)= G(I) *EPSYZ(I)
         SIGNZX(I)= G(I) *EPSZX(I)
C
         SVM2 = (SIGNXX(I)**2 + SIGNYY(I)**2 + SIGNZZ(I)**2)      
     .       + DEUX*(SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2) 
         SVM =SQRT(SVM2)
         R  = (YLD(I)/MAX(EM20,SVM))        
         SIGNXX(I)=SIGNXX(I)*R                                
         SIGNYY(I)=SIGNYY(I)*R                                
         SIGNZZ(I)=SIGNZZ(I)*R                                
         SIGNXY(I)=SIGNXY(I)*R                                
         SIGNYZ(I)=SIGNYZ(I)*R                                
         SIGNZX(I)=SIGNZX(I)*R    
C         
         IF(ILOAD(I) == -1) THEN
            R = UN - (UVAR(I,8)/MAX(EM20,UVAR(I,2)))**EXPO
            R = UN - (UN - HYS)*R
            SIGNXX(I)=SIGNXX(I)*R 
            SIGNYY(I)=SIGNYY(I)*R                             
            SIGNZZ(I)=SIGNZZ(I)*R                             
            SIGNXY(I)=SIGNXY(I)*R                             
            SIGNYZ(I)=SIGNYZ(I)*R                             
            SIGNZX(I)=SIGNZX(I)*R
cc            UVAR(I,9) = R
          ENDIF   
c         UVAR(I,3) = E 
         UVAR(I,4) = EPST(I)
         UVAR(I,5) = ILOAD(I)
         UVAR(I,6) = YLD(I)
         UVAR(I,7) = EPSP(I)
        ENDDO                           
      ENDIF
C
      IF(IFLAG > 0 ) THEN
         IPFUNC = IFUNC(1,NFUNC)
         DO I=1,NEL
             MU = 1 - RHO(I)/RHO0(I) 
             ALPHA = UN
             IF(MU > 0) THEN
                ALPHA = YFACC*FINTER(IPFUNC,MU,NPF,TF,DF)
                ALPHA = MAX(ZERO, ALPHA)
             ENDIF
             SIGNXX(I) = ALPHA*SIGNXX(I)
             SIGNYY(I) = ALPHA*SIGNYY(I)
             SIGNZZ(I) = ALPHA*SIGNZZ(I)
             SIGNXY(I) = ALPHA*SIGNXY(I)
             SIGNZX(I) = ALPHA*SIGNZX(I)
             SIGNYZ(I) = ALPHA*SIGNYZ(I)
             UVAR(I,10) = ALPHA
         ENDDO     
       ENDIF 
C------------------------------------ 
      RETURN
      END
