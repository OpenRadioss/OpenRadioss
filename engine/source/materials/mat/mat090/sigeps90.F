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
Chd|  SIGEPS90                      source/materials/mat/mat090/sigeps90.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        VALPVECDP_V                   source/materials/mat/mat033/sigeps33.F
Chd|        VALPVEC_V                     source/materials/mat/mat033/sigeps33.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS90(
     1     NEL    ,NUVAR   ,NFUNC   ,IFUNC   ,NPF    ,
     2     TF     ,TIME   ,UPARAM  ,RHO0    ,
     3     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     4     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX  ,
     5     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     6     SOUNDSP,VISCMAX,UVAR    ,ISMSTR,
     7     ISRATE ,ASRATE ,OFFG    ,IHET    ,ET      ,EPSD  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include "comlock.inc"
#include "mvsiz_p.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
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
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL     | F | R | INITIAL DENSITY
C RHO     | NEL     | F | R | DENSITY
C EINT    | NEL     | F | R | TOTAL INTERNAL ENERGY
C EPSPXX  | NEL     | F | R | STRAIN RATE XX
C EPSPYY  | NEL     | F | R | STRAIN RATE YY
C ...     |         |   |   |
C EPSXX   | NEL     | F | R | STRAIN XX
C EPSYY   | NEL     | F | R | STRAIN YY
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
#include      "scr05_c.inc"
#include      "impl1_c.inc"
C
      INTEGER, INTENT(IN) :: NEL,NUVAR,ISMSTR,ISRATE,IHET
       
      my_real, INTENT(IN) ::
     .   TIME,UPARAM(*),
     .   RHO0(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),EPSPZZ(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,EPSZZ(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   OFFG(NEL),ASRATE
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real,INTENT(OUT)::
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL),ET(NEL),EPSD(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real,INTENT(INOUT) :: UVAR(NEL,NUVAR)
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
     .     II,I,J,K,I1,J1,J2,IFLAG,ILOAD(MVSIZ,3),
     .     IDAM,INDX_L(MVSIZ),INDX_UNL(MVSIZ),NE_L,NE_UNL
      my_real
     . E0,AA,G,NU,SHAPE,HYS,
     . YFAC,YFACJ1,YFACJ2,RATEJ1,RATEJ2, EPSE,EP1,
     . EP2,EP3,EP4,EP5,EP6,ERT11,ERT12,ERT13,ERT21,
     . ERT22,ERT23,ERT31,ERT32,ERT33,SJ1,SJ2,FAC,T1,T2,T3,
     . DAM,EPE,E_MIN(MVSIZ),DELTA,ALPHA
      my_real :: DF(3),
     .  EPSP(3),ECURENT(MVSIZ),E(MVSIZ),DEINT(MVSIZ),
     .  RATEEPS, DEPS,E_MAX,E_NEW,E_OLD,EPSS,YLD(MVSIZ),EPST(MVSIZ),DE
      my_real, DIMENSION(MVSIZ) :: QUASI_EINT,EMAX,EMIN
      my_real, DIMENSION(MVSIZ,6) :: AV
      my_real, DIMENSION(MVSIZ,6) :: EVV,EV,STRAIN,STRAINRATE,S,SQSTAT
      my_real, DIMENSION(MVSIZ,3,3) :: DIRPRV
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
        ALPHA   =  UPARAM(2*NFUNC + 13)
C       
        IF(TIME == ZERO )UVAR(1:NEL,8) = E0
C           
C-----------------------------------------------
C     
        DO I=1,NEL                       
            AV(I,1) = EPSXX(I)       
            AV(I,2) = EPSYY(I)       
            AV(I,3) = EPSZZ(I)       
            AV(I,4) = HALF*EPSXY(I)
            AV(I,5) = HALF*EPSYZ(I)
            AV(I,6) = HALF*EPSZX(I)
        ENDDO                     
C Eigenvalues needed to be calculated in double precision
C        for a simple precision executing*
        IF (IRESP==1) THEN
            CALL VALPVECDP_V(AV,EVV,DIRPRV,NEL)
        ELSE
            CALL VALPVEC_V(AV,EVV,DIRPRV,NEL)
        ENDIF
C-ISMSTR=0-NO SMALL STRAIN OPTION:STRAINS ARE LOGARITHMIC, STRESS IS CAUCHY
C-ISMSTR=1-SMALL STRAIN OPTION:STRAINS ARE ENGINEERING, STRESS IS CAUCHY
C-ISMSTR=2-SMALL STRAIN OPTION:STRAINS ARE ENGINEERING, STRESS IS BIOT
C-ISMSTR=3-NO SMALL STRAIN OPTION:STRESS IS BIOT
        IF(ISMSTR==0.OR.ISMSTR==2.OR.ISMSTR==4) THEN
            DO I=1,NEL
C ---- (STRAIN IS LOGARITHMIC)
                EV(I,1)=EXP(EVV(I,1))
                EV(I,2)=EXP(EVV(I,2))
                EV(I,3)=EXP(EVV(I,3))
            ENDDO 
        ELSEIF(ISMSTR==10.OR.ISMSTR==12) THEN
            DO I =1,NEL
                IF(OFFG(I)<=ONE) THEN
                    EV(I,1)=SQRT(EVV(I,1) + ONE )
                    EV(I,2)=SQRT(EVV(I,2) + ONE )
                    EV(I,3)=SQRT(EVV(I,3) + ONE )
                ELSE
                    EV(I,1)=EVV(I,1)+ ONE
                    EV(I,2)=EVV(I,2)+ ONE
                    EV(I,3)=EVV(I,3)+ ONE
                END IF
            ENDDO 
        ELSE
C ----  STRAIN IS ENGINEERING)
            DO I=1,NEL
                EV(I,1)=EVV(I,1) + ONE
                EV(I,2)=EVV(I,2) + ONE
                EV(I,3)=EVV(I,3) + ONE 
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
            ERT11 =DIRPRV(I,1,1)*EP1 + DIRPRV(I,2,1)*EP4 + DIRPRV(I,3,1)*EP6
            ERT12 =DIRPRV(I,1,2)*EP1 + DIRPRV(I,2,2)*EP4 + DIRPRV(I,3,2)*EP6
            ERT13 =DIRPRV(I,1,3)*EP1 + DIRPRV(I,2,3)*EP4 + DIRPRV(I,3,3)*EP6
        
            ERT21 =DIRPRV(I,1,1)*EP4 + DIRPRV(I,2,1)*EP2 + DIRPRV(I,3,1)*EP5
            ERT22 =DIRPRV(I,1,2)*EP4 + DIRPRV(I,2,2)*EP2 + DIRPRV(I,3,2)*EP5
            ERT23 =DIRPRV(I,1,3)*EP4 + DIRPRV(I,2,3)*EP2 + DIRPRV(I,3,3)*EP5  
        
            ERT31 =DIRPRV(I,1,1)*EP6 + DIRPRV(I,2,1)*EP5 + DIRPRV(I,3,1)*EP3
            ERT32 =DIRPRV(I,1,2)*EP6 + DIRPRV(I,2,2)*EP5 + DIRPRV(I,3,2)*EP3
            ERT33 =DIRPRV(I,1,3)*EP6 + DIRPRV(I,2,3)*EP5 + DIRPRV(I,3,3)*EP3       
C
            EPSP(1) = DIRPRV(I,1,1)*ERT11 + DIRPRV(I,2,1)*ERT21 
     .                                    + DIRPRV(I,3,1)*ERT31 
            EPSP(2) = DIRPRV(I,1,2)*ERT12 + DIRPRV(I,2,2)*ERT22 
     .                                    + DIRPRV(I,3,2)*ERT32 
            EPSP(3) = DIRPRV(I,1,3)*ERT13 + DIRPRV(I,2,3)*ERT23 
     .                                    + DIRPRV(I,3,3)*ERT33
C    abs(eps) not necessary  
            STRAINRATE(I,1) = EPSP(1)*(ONE - STRAIN(I,1)) ! eng
            STRAINRATE(I,2) = EPSP(2)*(ONE - STRAIN(I,2))
            STRAINRATE(I,3) = EPSP(3)*(ONE - STRAIN(I,3))
        ENDDO
C computing energy increase
        YFAC = UPARAM(NFUNC + 11)
        QUASI_EINT(1:NEL)= ZERO
        DO K=1,3
            DO I=1,NEL
                EPSE = STRAIN(I,K)                                          
                SQSTAT(I,K) = YFAC*FINTER(IFUNC(1),EPSE,NPF,TF,DF(K))       
C compute current energy
                QUASI_EINT(I)= QUASI_EINT(I) + HALF*STRAIN(I,K)*SQSTAT(I,K)
            ENDDO
        ENDDO   
        DEINT(1:NEL)  = QUASI_EINT(1:NEL) - UVAR(1:NEL,9)
        UVAR(1:NEL,9) = QUASI_EINT(1:NEL)
C -----------                   
C check loading and unloading.
        DO K=1,3
            DO I=1,NEL
                EPE = EPSP(K)*STRAIN(I,K)
                ILOAD(I,K) = 1 
                IF(EPE > EM10 )ILOAD(I,K) = -1
            ENDDO
        ENDDO 
        DO I=1,NEL
C filtering strain
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
                YFAC = UPARAM(NFUNC + 11)
                DO I=1,NEL
                    EMAX(I) = ZERO
                    ET(I) = ZERO
                    EMIN(I) = EP20
                ENDDO
                DO K=1,3
                    DO I=1,NEL
                        EPSE = STRAIN(I,K)
                        S(I,K) = YFAC*FINTER(IFUNC(1),EPSE,NPF,TF,DF(K))
                        EMAX(I) = MAX(EMAX(I), YFAC*DF(K))
                        EMIN(I) = MAX(EMIN(I), YFAC*DF(K))
                    ENDDO
                ENDDO 
                E(1:NEL) = EMAX(1:NEL)
                ET(1:NEL) = EMIN(1:NEL)/E0
                YLD(1:NEL) = SQRT(S(1:NEL,1)**2 + S(1:NEL,2)**2 + S(1:NEL,3)**2)           
        
            ELSE ! multiple functions
                DO I=1,NEL
                    EMAX(I) = ZERO
                    EMIN(I) = EP20
                ENDDO
                DO I=1,NEL
                    DO K=1,3  ! by direction
                        EPSE = STRAIN(I,K)
                        IF(ILOAD(I,K) == -1) THEN
                            YFAC = UPARAM(NFUNC + 11)
                            S(I,K) = YFAC*FINTER(IFUNC(1),EPSE,NPF,TF,DF(K)) 
                            EMAX(I) = MAX(EMAX(I), YFAC*DF(K))
                        ELSE
                            IF(STRAINRATE(I,K) < UPARAM(11)) THEN
                                YFAC = UPARAM(NFUNC + 11)
                                S(I,K) = YFAC*FINTER(IFUNC(1),EPSE,NPF,TF,DF(K)) 
                                EMAX(I) = MAX(EMAX(I), YFAC*DF(K)) 
                                EMIN(I) = MIN(EMIN(I), YFAC*DF(K)) 
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
                                EMAX(I) = MAX(EMAX(I), YFACJ1*DF(K))
                                EMIN(I) = MAX(EMIN(I), YFACJ1*DF(K))
                                SJ2 =  YFACJ2*FINTER(IFUNC(J2),EPSE,NPF,TF,DF(K))
                                EMAX(I) = MAX(EMAX(I), YFACJ2*DF(K))
                                EMIN(I) = MIN(EMIN(I), YFACJ2*DF(K))
                                FAC    = (STRAINRATE(I,K) - RATEJ1)/(RATEJ2 - RATEJ1)
                                S(I,K) = SJ1 + FAC*(SJ2 - SJ1)
                            ENDIF ! strainrate    
                        ENDIF !ILOAD
                    ENDDO ! K
                ENDDO
                E(1:NEL) = MAX(E0,EMAX(1:NEL))
                E(1:NEL) = EMAX(1:NEL)
                ET(1:NEL) = EMIN(1:NEL)/E0  ! Not used
                YLD(1:NEL) = SQRT(S(1:NEL,1)**2 + S(1:NEL,2)**2 + S(1:NEL,3)**2)
            ENDIF
        ENDIF 
C unloading with damage based on the energy.
        IF(IFLAG == 2 ) THEN
            IF(NFUNC == 1) THEN
                YFAC = UPARAM(NFUNC + 11)
                DO I=1,NEL
                    ECURENT (I)= ZERO
                    EMAX(I) = ZERO
                    EMIN(I) = EP20
                    ET(I) = ZERO
                ENDDO
                DO K=1,3
                    DO I=1,NEL
                        EPSE = STRAIN(I,K)
                        SQSTAT(I,K) = YFAC*FINTER(IFUNC(1),EPSE,NPF,TF,DF(K)) 
                        S(I,K) = SQSTAT(I,K)
                        EMAX(I) = MAX(EMAX(I), YFAC*DF(K))
                        EMIN(I) = MIN(EMIN(I), YFAC*DF(K))
C compute current energy
                        ECURENT(I)= ECURENT(I) + HALF*STRAIN(I,K)*SQSTAT(I,K)
                    ENDDO
                ENDDO
                ET(1:NEL) = EMIN(1:NEL)/E0 ! not used
                E(1:NEL) = MAX(E0,EMAX(1:NEL))
                E_MIN(1:NEL) =EMIN(1:NEL)
                YLD(1:NEL) = SQRT(S(1:NEL,1)**2 + S(1:NEL,2)**2 + S(1:NEL,3)**2)
            ELSEIF(NFUNC > 1) THEN 
C
C  unloading (quasi-static dependency)
C   
                YFAC = UPARAM(NFUNC + 11)
                DO II=1,NE_UNL ! unloading
                    I= INDX_UNL(II)
                    ECURENT (I)= ZERO
                    EMAX(I) = ZERO
                    EMIN(I) = EP20
                    DO K=1,3
                        EPSE = STRAIN(I,K)
                        SQSTAT(I,K) = YFAC*FINTER(IFUNC(1),EPSE,NPF,TF,DF(K)) 
                        S(I,K) = SQSTAT(I,K)
                        EMAX(I) = MAX(EMAX(I), YFAC*DF(K))
                        EMIN(I) = MIN(EMIN(I), YFAC*DF(K))
C compute current energy
                        ECURENT(I)= ECURENT(I) + HALF*STRAIN(I,K)*SQSTAT(I,K)
                    ENDDO 
                    E(I) = MAX(E0,EMAX(I))
                    E(I) = EMAX(I)
                    E_MIN(I) = EMIN(I)
                    ET(I) = EMIN(I)/E0
                    YLD(I) = SQRT(S(I,1)**2 + S(I,2)**2 + S(I,3)**2)
                ENDDO 
                !! loading 
                YFAC = UPARAM(NFUNC + 11)
                DO II=1,NE_L
                    I = INDX_L(II)                                                     
                    ECURENT (I)= ZERO                                            
                    EMAX(I)=ZERO                                                    
                    EMIN(I)=EP20                                                    
                                                                                  
                    DO K=1,3                                                     
                        EPSE = STRAIN(I,K)                                         
                        SQSTAT(I,K) = YFAC*FINTER(IFUNC(1),EPSE,NPF,TF,DF(K))      
                        EMAX(I) = MAX(EMAX(I),YFAC*DF(K))                                
                        EMIN(I) = MIN(EMIN(I),YFAC*DF(K))                                
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
                            EMAX(I) = MAX(EMAX(I), YFACJ1*DF(K))                           
                            EMIN(I) = MIN(EMIN(I), YFACJ1*DF(K))                           
                            !! E1 = YFACJ1*DF(K)                                     
                            SJ2 =  YFACJ2*FINTER(IFUNC(J2),EPSE,NPF,TF,DF(K))        
                            EMAX(I) = MAX(EMAX(I),YFACJ2*DF(K))                            
                            EMIN(I) = MIN(EMIN(I),YFACJ2*DF(K))                            
                            !! E2 = YFACJ2*DF(K)                                     
                            FAC    = (STRAINRATE(I,K) - RATEJ1)/(RATEJ2 - RATEJ1)    
                            S(I,K) = SJ1 + FAC*(SJ2 - SJ1)                           
                            !! EMIN(I) = MIN(EMIN(I),E1 + FAC*(E2 - E1))                   
                         ENDIF ! strainrate                                       
                    ENDDO  ! K                              
                ENDDO 
                E(1:NEL) = MAX(E0,EMAX(1:NEL))                                         
                E(1:NEL) = EMAX(1:NEL)                                                 
                E_MIN(1:NEL) = EMIN(1:NEL)                                             
                ET(1:NEL)    = EMIN(1:NEL)/E0 ! not used                               
                YLD(1:NEL) = SQRT(S(1:NEL,1)**2 + S(1:NEL,2)**2 + S(1:NEL,3)**2) 
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
C  flag is a hidden flag, only idam=0 is activated, Idam > o not tested.
            IF(IDAM == 0) THEN
#include "vectorize.inc" 
                DO II=1,NE_UNL
                    I = INDX_UNL(II)
                    IF(UVAR(I,2) > ZERO) THEN
                        DAM = ONE - (ECURENT(I)/UVAR(I,2))**SHAPE
                        DAM = DAM**ALPHA
                        DAM = ONE - (ONE - HYS)*DAM
                        UVAR(I,7) = DAM
C  global      
                        DO K=1,3
                            S(I,K)= DAM*S(I,K)
                        ENDDO
                    ENDIF
                ENDDO ! NE_UNL
            ELSE 

C damage by direction to be tested for
#include "vectorize.inc"
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
            SIGNXX(I) = DIRPRV(I,1,1)*DIRPRV(I,1,1)*T1
     .                + DIRPRV(I,1,2)*DIRPRV(I,1,2)*T2
     .                + DIRPRV(I,1,3)*DIRPRV(I,1,3)*T3
     
            SIGNYY(I) = DIRPRV(I,2,2)*DIRPRV(I,2,2)*T2
     .                + DIRPRV(I,2,3)*DIRPRV(I,2,3)*T3
     .                + DIRPRV(I,2,1)*DIRPRV(I,2,1)*T1
     
            SIGNZZ(I) = DIRPRV(I,3,3)*DIRPRV(I,3,3)*T3        
     .                + DIRPRV(I,3,1)*DIRPRV(I,3,1)*T1
     .                + DIRPRV(I,3,2)*DIRPRV(I,3,2)*T2
            SIGNXY(I) = DIRPRV(I,1,1)*DIRPRV(I,2,1)*T1
     .                + DIRPRV(I,1,2)*DIRPRV(I,2,2)*T2     
     .                + DIRPRV(I,1,3)*DIRPRV(I,2,3)*T3
     
            SIGNYZ(I) = DIRPRV(I,2,2)*DIRPRV(I,3,2)*T2
     .                + DIRPRV(I,2,3)*DIRPRV(I,3,3)*T3
     .                + DIRPRV(I,2,1)*DIRPRV(I,3,1)*T1
     
            SIGNZX(I) = DIRPRV(I,3,3)*DIRPRV(I,1,3)*T3
     .                + DIRPRV(I,3,1)*DIRPRV(I,1,1)*T1
     .                + DIRPRV(I,3,2)*DIRPRV(I,1,2)*T2         
         
C==================================================       
      ! computing E
            E_OLD = UVAR(I,8)
            EPSS = EPST(I) - YLD(I)/ E_OLD
            EPSS = MAX(ZERO, EPSS)
            EPSS = MIN(ONE, EPSS)
            AA = E_MAX - E0  
            DE = AA*(EPSS - UVAR(I,10))
            E_NEW = AA*EPSS + E0
            E_NEW= MIN(E_MAX, E_NEW)
            UVAR(I,10) = EPSS
            UVAR(I,8) = E_NEW
            AA = E_NEW*(ONE-NU)/(ONE + NU)/(ONE - TWO*NU) 
            SOUNDSP(I) = SQRT(AA/RHO0(I))
            VISCMAX(I) = ZERO 
        
        ENDDO  
        IF (IMPL_S > 0 .OR. IHET > 1) THEN
          DO I=1,NEL
            ET(I) = YLD(I)/MAX(EM20,EPST(I))
            ET(I) = MIN(ONE , ET(I)/E_MAX)
            IF(ET(I) == ZERO) ET(I) = ONE
         ENDDO
        ENDIF
C------------------------------------ 
        RETURN
        END SUBROUTINE SIGEPS90
C
