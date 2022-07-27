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
Chd|  SIGEPS90                      source/materials/mat/mat090/sigeps90.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        VALPVEC                       source/materials/mat/mat033/sigeps33.F
Chd|        VALPVECDP                     source/materials/mat/mat033/sigeps33.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS90(
     1     NEL    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,NPF    ,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0    ,RHO    ,
     3     VOLUME ,EINT   ,
     4     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIG0XX ,SIG0YY ,SIG0ZZ  ,SIG0XY  ,SIG0YZ  ,SIG0ZX ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     A     SOUNDSP,VISCMAX,UVAR    ,OFF     ,NGL     ,ISMSTR,
     B     ISRATE ,ASRATE ,OFFG    ,IHET    ,ET      ,EPSD  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
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
#include      "parit_c.inc"
#include      "scr17_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "impl1_c.inc"
C
      INTEGER, INTENT(IN) :: NEL, NUPARAM, NUVAR,ISMSTR,ISRATE,
     .   NGL(NEL),IHET
       
      my_real, INTENT(IN) ::
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
     .   OFFG(NEL),ASRATE
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real,INTENT(OUT)::
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
!!     .    SIGVXX(NEL),SIGVYY(NEL),SIGVZZ(NEL),
!!     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL),ET(NEL),EPSD(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real,INTENT(INOUT) ::
     .        UVAR(NEL,NUVAR), OFF(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NPF(*), NFUNC, IFUNC(NFUNC)
      my_real, INTENT(IN)   :: TF(*)
      my_real, EXTERNAL :: FINTER
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
      INTEGER  
     .     II,I,J,K,I1,J1,J2,IFLAG,ILOAD(NEL,3),ILOADE(NEL),
     .     IDAM,INDX_L(NEL),INDX_UNL(NEL),NE_L,NE_UNL
      my_real
     . E0,AA,G,NU,SHAPE,HYS,EMAX,
     . YFAC,YFACJ1,YFACJ2,RATEJ1,RATEJ2, EPSE,EP1,
     . EP2,EP3,EP4,EP5,EP6,ERT11,ERT12,ERT13,ERT21,
     . ERT22,ERT23,ERT31,ERT32,ERT33,SJ1,SJ2,FAC,T1,T2,T3,
     . DAM,EPE,EMIN,E_MIN(NEL),E1,E2,TMP,QUASI_EINT,DELTA
      my_real
     .  AV(6,NEL), EVV(3,NEL),EV(NEL,3),STRAIN(NEL,3),
     .  STRAINRATE(NEL,3),S(NEL,3),SQSTAT(NEL,3),DF(3),
     .  DIRPRV(3,3,NEL),EPSP(3),ECURENT(NEL),E(NEL),DEINT(NEL),
     .  RATEEPS, DEPS,E_MAX,E_NEW,E_OLD,EPSS,YLD(NEL),EPST(NEL),DE
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------    
       E0      =  UPARAM(1)
       G       =  UPARAM(4)
       NU      =  UPARAM(5)
       SHAPE   =  UPARAM(6)
       HYS     =  UPARAM(7)
       IFLAG   =  UPARAM(9)  
       IDAM    =  UPARAM(10)
       E_MAX   =  UPARAM(2*NFUNC + 11)
C       
       IF(TIME == ZERO )UVAR(1:NEL,8) = E0
C           
C-----------------------------------------------
C     
         DO I=1,NEL                       
             AV(1,I) = EPSXX(I)       
             AV(2,I) = EPSYY(I)       
             AV(3,I) = EPSZZ(I)       
             AV(4,I) = HALF*EPSXY(I)
             AV(5,I) = HALF*EPSYZ(I)
             AV(6,I) = HALF*EPSZX(I)
         ENDDO                     
CEigenvalues needed to be calcultated in double precision
C        for a simple precision executing*
      IF (IRESP.EQ.1) THEN
          CALL VALPVECDP(AV,EVV,DIRPRV,NEL)
      ELSE
          CALL VALPVEC(AV,EVV,DIRPRV,NEL)
      ENDIF
C-ISMSTR=0-NO SMALL STRAIN OPTION:STRAINS ARE LOGARITHMIC, STRESS IS CAUCHY
C-ISMSTR=1-SMALL STRAIN OPTION:STRAINS ARE ENGINEERING, STRESS IS CAUCHY
C-ISMSTR=2-SMALL STRAIN OPTION:STRAINS ARE ENGINEERING, STRESS IS BIOT
C-ISMSTR=3-NO SMALL STRAIN OPTION:STRESS IS BIOT
      IF(ISMSTR.EQ.0.OR.ISMSTR.EQ.2.OR.ISMSTR.EQ.4) THEN
        DO I=1,NEL
C ---- (STRAIN IS LOGARITHMIC)
            EV(I,1)=EXP(EVV(1,I))
            EV(I,2)=EXP(EVV(2,I))
            EV(I,3)=EXP(EVV(3,I))
        ENDDO 
      ELSEIF(ISMSTR==10.OR.ISMSTR==12) THEN
        DO I =1,NEL
          IF(OFFG(I)<=ONE) THEN
             EV(I,1)=SQRT(EVV(1,I) + ONE )
             EV(I,2)=SQRT(EVV(2,I) + ONE )
             EV(I,3)=SQRT(EVV(3,I) + ONE )
	  ELSE
             EV(I,1)=EVV(1,I)+ ONE
             EV(I,2)=EVV(2,I)+ ONE
             EV(I,3)=EVV(3,I)+ ONE
          END IF
        ENDDO 
      ELSE
C ----  STRAIN IS ENGINEERING)
        DO I=1,NEL
           EV(I,1)=EVV(1,I) + ONE
           EV(I,2)=EVV(2,I) + ONE
           EV(I,3)=EVV(3,I) + ONE 
        ENDDO 
      ENDIF
         
C engineering strain   and strain rate    
      DO I=1,NEL 
C engineering strain   e  = lambda-1 ,  according the input curve
C e=1-lambda (e > 0 compression and e < 0 traction)            
        STRAIN(I,1) = ONE - EV(I,1)         
        STRAIN(I,2) = ONE - EV(I,2)         
        STRAIN(I,3) = ONE - EV(I,3) 
        
        EPST(I) = SQRT(STRAIN(I,1)**2 + STRAIN(I,2)**2 + STRAIN(I,3)**2)       
C
        EP1 = EPSPXX(I)
        EP2 = EPSPYY(I)      
        EP3 = EPSPZZ(I) 
        EP4 = HALF*EPSPXY(I)        
        EP5 = HALF*EPSPYZ(I)
        EP6 = HALF*EPSPZX(I)
C phi_trans*L*phi_t    
        ERT11 =DIRPRV(1,1,I)*EP1 + DIRPRV(2,1,I)*EP4 + DIRPRV(3,1,I)*EP6
        ERT12 =DIRPRV(1,2,I)*EP1 + DIRPRV(2,2,I)*EP4 + DIRPRV(3,2,I)*EP6
        ERT13 =DIRPRV(1,3,I)*EP1 + DIRPRV(2,3,I)*EP4 + DIRPRV(3,3,I)*EP6
      
        ERT21 =DIRPRV(1,1,I)*EP4 + DIRPRV(2,1,I)*EP2 + DIRPRV(3,1,I)*EP5
        ERT22 =DIRPRV(1,2,I)*EP4 + DIRPRV(2,2,I)*EP2 + DIRPRV(3,2,I)*EP5
        ERT23 =DIRPRV(1,3,I)*EP4 + DIRPRV(2,3,I)*EP2 + DIRPRV(3,3,I)*EP5  
      
        ERT31 =DIRPRV(1,1,I)*EP6 + DIRPRV(2,1,I)*EP5 + DIRPRV(3,1,I)*EP3
        ERT32 =DIRPRV(1,2,I)*EP6 + DIRPRV(2,2,I)*EP5 + DIRPRV(3,2,I)*EP3
        ERT33 =DIRPRV(1,3,I)*EP6 + DIRPRV(2,3,I)*EP5 + DIRPRV(3,3,I)*EP3       
C
        EPSP(1) = DIRPRV(1,1,I)*ERT11 + DIRPRV(2,1,I)*ERT21 
     .                                + DIRPRV(3,1,I)*ERT31 
        EPSP(2) = DIRPRV(1,2,I)*ERT12 + DIRPRV(2,2,I)*ERT22 
     .                                + DIRPRV(3,2,I)*ERT32 
        EPSP(3) = DIRPRV(1,3,I)*ERT13 + DIRPRV(2,3,I)*ERT23 
     .                                + DIRPRV(3,3,I)*ERT33
C    abs(eps) not necessary  
        STRAINRATE(I,1) = EPSP(1)*(ONE - STRAIN(I,1)) ! eng
        STRAINRATE(I,2) = EPSP(2)*(ONE - STRAIN(I,2))
        STRAINRATE(I,3) = EPSP(3)*(ONE - STRAIN(I,3))
C computing energy increase
          YFAC = UPARAM(NFUNC + 11)
          QUASI_EINT= ZERO
          DO K=1,3
            EPSE = STRAIN(I,K)                                          
            SQSTAT(I,K) = YFAC*FINTER(IFUNC(1),EPSE,NPF,TF,DF(K))       
C compute curent energy
            QUASI_EINT= QUASI_EINT + HALF*STRAIN(I,K)*SQSTAT(I,K)
          ENDDO   
            DEINT(I)  = QUASI_EINT - UVAR(I,9)
            UVAR(I,9) = QUASI_EINT
C -----------                   
C check loading and unloading.
         ILOADE(I) = 1
         DO K=1,3
             EPE = EPSP(K)*STRAIN(I,K)
             ILOAD(I,K) = 1 
             IF(EPE > EM10 )ILOAD(I,K) = -1
         ENDDO 
C filtring strain
         RATEEPS = SQRT(STRAINRATE(I,1)**2 + STRAINRATE(I,2)**2 + STRAINRATE(I,3)**2)
        IF (ISRATE > 0) THEN
           RATEEPS =  ASRATE*RATEEPS + (ONE - ASRATE)*UVAR(I,3)
         ENDIF    
         EPSD(I) = RATEEPS
      ENDDO
C sous groupe  
         INDX_L(1:NEL) = 0
         INDX_UNL(1:NEL) = 0
         NE_L  = 0
         NE_UNL  = 0
C                
         DO I=1,NEL
             DEPS = EPST(I) - UVAR(I,6)
            IF(DEINT(I) >= ZERO .OR. DEPS >= ZERO) THEN
              NE_L = NE_L + 1 
              INDX_L(NE_L) = I
              UVAR(I,3) = EPSD(I)
            ELSE
              EPSD(I) = MIN(EPSD(I), UVAR(I,3))
              NE_UNL = NE_UNL + 1 
              INDX_UNL(NE_UNL) = I
              UVAR(I,3) = EPSD(I)
            ENDIF
           STRAINRATE(I,1)=  EPSD(I)
           STRAINRATE(I,2)=  EPSD(I)
           STRAINRATE(I,3)=  EPSD(I)
         ENDDO  
C case with unloading stress-strain curve
      IF(IFLAG == 1) THEN
        IF(NFUNC == 1) THEN
          DO I=1,NEL
             YFAC = UPARAM(NFUNC + 11)
             Emax = ZERO
             ET(I) = ZERO
             EMIN = EP20
             DO K=1,3
               EPSE = STRAIN(I,K)
               S(I,K) = YFAC*FINTER(IFUNC(1),EPSE,NPF,TF,DF(K))
               Emax = MAX(Emax, YFAC*DF(K))
               EMIN = MAX(EMIN, YFAC*DF(K))
             ENDDO 
             !! E(I) = MAX(E0,Emax)
               E(I) = EMAX
              ET(I) = EMIN/E0
              YLD(I) = SQRT(S(I,1)**2 + S(I,2)**2 + S(I,3)**2)
           ENDDO           
        ELSE ! multiple functions
          DO I=1,NEL
            Emax = ZERO
            EMIN = EP20
            DO K=1,3  ! by direction
              EPSE = STRAIN(I,K)
              IF(ILOAD(I,K) == -1) THEN
                YFAC = UPARAM(NFUNC + 11)
                S(I,K) = YFAC*FINTER(IFUNC(1),EPSE,NPF,TF,DF(K)) 
                Emax = MAX(Emax, YFAC*DF(K))
              ELSE
                IF(STRAINRATE(I,K) < UPARAM(11)) THEN
                    YFAC = UPARAM(NFUNC + 11)
                    S(I,K) = YFAC*FINTER(IFUNC(1),EPSE,NPF,TF,DF(K)) 
                    Emax = MAX(Emax, YFAC*DF(K)) 
                    EMIN = MIN(EMIN, YFAC*DF(K)) 
                ELSE
                    J1 = 1 
                    DO J= 2,NFUNC - 1       
                      IF(STRAINRATE(I,K) >= UPARAM(10 + J ))J1 = J  
                    ENDDO
                    J2 = J1 + 1
                    RATEJ1= UPARAM(10 + J1 )
                    RATEJ2= UPARAM(10 + J2 )
                    YFACJ1= UPARAM(10 + NFUNC + J1 )
                    YFACJ2= UPARAM(10 + NFUNC + J2 )
                    SJ1 =  YFACJ1*FINTER(IFUNC(J1),EPSE,NPF,TF,DF(K))
                    Emax = MAX(Emax, YFACJ1*DF(K))
                    EMIN = MAX(EMIN, YFACJ1*DF(K))
                    SJ2 =  YFACJ2*FINTER(IFUNC(J2),EPSE,NPF,TF,DF(K))
                    Emax = MAX(Emax, YFACJ2*DF(K))
                    EMIN = MIN(EMIN, YFACJ2*DF(K))
                    FAC    = (STRAINRATE(I,K) - RATEJ1)/(RATEJ2 - RATEJ1)
                    S(I,K) = SJ1 + FAC*(SJ2 - SJ1)
                 ENDIF ! strainrate    
               ENDIF !ILOAD
              ENDDO ! K
               E(I) = MAX(E0,Emax)
               E(I) = EMAX
               ET(I) = EMIN/E0  ! Not used
               YLD(I) = SQRT(S(I,1)**2 + S(I,2)**2 + S(I,3)**2)
            ENDDO ! NEL  
         ENDIF
       ENDIF 
C unloadind with damage based on the energy.
      IF(IFLAG == 2 ) THEN
             IF(NFUNC == 1) THEN
               YFAC = UPARAM(NFUNC + 11)
               DO I=1,NEL
                 ECURENT (I)= ZERO
                 Emax = ZERO
                 EMIN = EP20
                 ET(I) = ZERO
                 DO K=1,3
                   EPSE = STRAIN(I,K)
                   SQSTAT(I,K) = YFAC*FINTER(IFUNC(1),EPSE,NPF,TF,DF(K)) 
                   S(I,K) = SQSTAT(I,K)
                   Emax = MAX(Emax, YFAC*DF(K))
                   EMIN = MIN(EMIN, YFAC*DF(K))
C computecurent energy
                    ECURENT(I)= ECURENT(I) + HALF*STRAIN(I,K)*SQSTAT(I,K)
                 ENDDO
                   ET(I) = EMIN/E0 ! not used
                   E(I) = MAX(E0,Emax)
                   E_MIN(I) =EMIN
                   YLD(I) = SQRT(S(I,1)**2 + S(I,2)**2 + S(I,3)**2)
               ENDDO 
            ELSEIF(NFUNC > 1) THEN 
C
C  unlaoding (quasi-static dependency )
C   
                   YFAC = UPARAM(NFUNC + 11)
                   DO II=1,NE_UNL ! unloading
                      I= INDX_UNL(II)
                      ECURENT (I)= ZERO
                      Emax = ZERO
                      EMIN = EP20
                      DO K=1,3
                        EPSE = STRAIN(I,K)
                        SQSTAT(I,K) = YFAC*FINTER(IFUNC(1),EPSE,NPF,TF,DF(K)) 
                        S(I,K) = SQSTAT(I,K)
                        Emax = MAX(Emax, YFAC*DF(K))
                        EMIN = MIN(EMIN, YFAC*DF(K))
C compute cure     nt energy
                         ECURENT(I)= ECURENT(I) + HALF*STRAIN(I,K)*SQSTAT(I,K)
!!                         ECURENT(I)= ECURENT(I) + HALF*EV(I,K)*ABS(SQSTAT(I,K))
                      ENDDO 
                        E(I) = MAX(E0,Emax)
                        E(I) = EMAX
                        E_MIN(I) = EMIN
                        ET(I) = EMIN/E0
                        YLD(I) = SQRT(S(I,1)**2 + S(I,2)**2 + S(I,3)**2)
                    ENDDO 
                    !! loading 
                    YFAC = UPARAM(NFUNC + 11)
                    DO II=1,NE_L
                     I = INDX_L(II)                                                     
                     ECURENT (I)= ZERO                                            
                     Emax=ZERO                                                    
                     EMIN=EP20                                                    
                                                                                  
                     DO K=1,3                                                     
                       EPSE = STRAIN(I,K)                                         
                       SQSTAT(I,K) = YFAC*FINTER(IFUNC(1),EPSE,NPF,TF,DF(K))      
                       Emax = MAX(Emax,YFAC*DF(K))                                
                       EMIN = MIN(EMIN,YFAC*DF(K))                                
C                                                                                 
                      IF(STRAINRATE(I,K) < UPARAM(11)) THEN                       
                          S(I,K) = SQSTAT(I,K)                                    
                      ELSE                                                        
                          J1 = 1                                                  
                          DO J= 2,NFUNC - 1                                       
                             IF(STRAINRATE(I,K) >= UPARAM(10 + J ))J1 = J         
                          ENDDO                                                   
                          J2 = J1 + 1                                             
                       !!                                                         
                         RATEJ1= UPARAM(10 + J1 )                                 
                         RATEJ2= UPARAM(10 + J2 )                                 
                         YFACJ1= UPARAM(10 + NFUNC + J1 )                         
                         YFACJ2= UPARAM(10 + NFUNC + J2 )                         
                         SJ1 =  YFACJ1*FINTER(IFUNC(J1),EPSE,NPF,TF,DF(K))        
                         Emax = MAX(Emax, YFACJ1*DF(K))                           
                         EMIN = MIN(EMIN, YFACJ1*DF(K))                           
                         !! E1 = YFACJ1*DF(K)                                     
                         SJ2 =  YFACJ2*FINTER(IFUNC(J2),EPSE,NPF,TF,DF(K))        
                         Emax = MAX(Emax,YFACJ2*DF(K))                            
                         EMIN = MIN(EMIN,YFACJ2*DF(K))                            
                         !! E2 = YFACJ2*DF(K)                                     
                         FAC    = (STRAINRATE(I,K) - RATEJ1)/(RATEJ2 - RATEJ1)    
                         S(I,K) = SJ1 + FAC*(SJ2 - SJ1)                           
                         !! EMIN = MIN(EMIN,E1 + FAC*(E2 - E1))                   
                         ENDIF ! strainrate                                       
                     ENDDO  ! K                    
                      E(I) = MAX(E0,Emax)                                         
                      E(I) = Emax                                                 
                      E_MIN(I) = EMIN                                             
                      ET(I)    = EMIN/E0 ! not used                               
                      YLD(I) = SQRT(S(I,1)**2 + S(I,2)**2 + S(I,3)**2)            
                     ENDDO 
              ENDIF ! NFUNC 
              
              DO I=1,NEL
                 DELTA = EPST(I) - UVAR(I,6)
                 UVAR(I,4) = UVAR(I,4) + 
     .                       HALF*(YLD(I) + UVAR(I,1))*DELTA
                 UVAR(I,4) = MAX(ZERO, UVAR(I,4))
                 UVAR(I,2) = MAX(UVAR(I,2) , UVAR(I,4))
                 UVAR(I,1) = YLD(I) 
                 UVAR(I,6)  = EPST(I)
                 ECURENT(I) = UVAR(I,4)
              ENDDO
C 
C  flag is a hidden falg, only idam=0 is activated, Idam > o not tested.     
              IF(IDAM == 0) THEN 
                DO II=1,NE_UNL
                  I = INDX_UNL(II)
                  IF(UVAR(I,2) > ZERO) THEN
                     DAM = ONE - (ECURENT(I)/UVAR(I,2))**SHAPE
                     DAM = ONE - (ONE - HYS)*DAM
                     UVAR(I,7) = DAM
C  global      
                     DO K=1,3
                        S(I,K)= DAM*S(I,K)
                     ENDDO
                  ENDIF
                ENDDO ! NE_UNL
              ELSE 

C damage by direction to be  tested for               
                DO II=1,NE_UNL
                  I = INDX_UNL(II)
                  IF(UVAR(I,2) > ZERO) THEN
                     DAM = ONE - (ECURENT(I)/UVAR(I,2))**SHAPE
                     DAM = ONE - (ONE - HYS)*DAM 
                     DO K=1,3
                         IF(ILOAD(I,K) < 0)S(I,K) = DAM*S(I,K)
                     ENDDO
                  ENDIF
                ENDDO ! nel
               ENDIF ! IDAM   
          ENDIF ! iflag=2 
C                
C =====================================================
         DO I = 1,NEL
C S > 0 for compression - curve definition
C S < 0 for traction          
            T1 = -S(I,1)/EV(I,2)/EV(I,3) 
            T2 = -S(I,2)/EV(I,1)/EV(I,3) 
            T3 = -S(I,3)/EV(I,1)/EV(I,2) 
C 
C cauchy to glabale
C
        SIGNXX(I) = DIRPRV(1,1,I)*DIRPRV(1,1,I)*T1
     .            + DIRPRV(1,2,I)*DIRPRV(1,2,I)*T2
     .            + DIRPRV(1,3,I)*DIRPRV(1,3,I)*T3
     
        SIGNYY(I) = DIRPRV(2,2,I)*DIRPRV(2,2,I)*T2
     .            + DIRPRV(2,3,I)*DIRPRV(2,3,I)*T3
     .            + DIRPRV(2,1,I)*DIRPRV(2,1,I)*T1
     
        SIGNZZ(I) = DIRPRV(3,3,I)*DIRPRV(3,3,I)*T3        
     .            + DIRPRV(3,1,I)*DIRPRV(3,1,I)*T1
     .            + DIRPRV(3,2,I)*DIRPRV(3,2,I)*T2
     
        SIGNXY(I) = DIRPRV(1,1,I)*DIRPRV(2,1,I)*T1
     .            + DIRPRV(1,2,I)*DIRPRV(2,2,I)*T2     
     .            + DIRPRV(1,3,I)*DIRPRV(2,3,I)*T3
     
        SIGNYZ(I) = DIRPRV(2,2,I)*DIRPRV(3,2,I)*T2
     .            + DIRPRV(2,3,I)*DIRPRV(3,3,I)*T3
     .            + DIRPRV(2,1,I)*DIRPRV(3,1,I)*T1
     
        SIGNZX(I) = DIRPRV(3,3,I)*DIRPRV(1,3,I)*T3
     .            + DIRPRV(3,1,I)*DIRPRV(1,1,I)*T1
     .            + DIRPRV(3,2,I)*DIRPRV(1,2,I)*T2         
         
C==================================================       
      ! computing E
           E_OLD = UVAR(I,8)
           EPSS = EPST(I) - YLD(I)/ E_OLD
           EPSS = MAX(ZERO, EPSS)
           EPSS = MIN(ONE, EPSS)
           AA = E_MAX - E0  
           DE = AA*(EPSS - UVAR(I,10))
!!           E_NEW = E_OLD + AA*MAX(DE,ZERO)
           E_NEW = AA*EPSS + E0
           E_NEW= MIN(E_MAX, E_NEW)
           UVAR(I,10) = EPSS
           UVAR(I,8) = E_NEW
           AA = E_NEW*(ONE-NU)/(ONE + NU)/(ONE - TWO*NU) 
           SOUNDSP(I) = SQRT(AA/RHO0(I))
           VISCMAX(I) = ZERO 
C         
        ENDDO  
CE0
        IF (IMPL_S > 0 .OR. IHET > 1) THEN
          DO I=1,NEL
            ET(I) = YLD(I)/MAX(EM20,EPST(I))
!!            ET(I) = MIN(ONE , ET(I)/UVAR(I,8))
            ET(I) = MIN(ONE , ET(I)/E_MAX)
         ENDDO
        ENDIF
C------------------------------------ 
      RETURN
      END
C
