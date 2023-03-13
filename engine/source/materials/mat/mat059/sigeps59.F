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
Chd|  SIGEPS59                      source/materials/mat/mat059/sigeps59.F
Chd|-- called by -----------
Chd|        SUSER43                       source/elements/solid/sconnect/suser43.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS59(
     1     NEL     ,TIME    ,TIMESTEP,UPARAM  ,OFF     ,
     2     EPSD    ,STIFM   ,NUPARAM ,
     3     IFUNC   ,MAXFUNC ,NPF     ,TF      ,AREA    ,
     4     EPSZZ   ,EPSYZ   ,EPSZX   ,DEPSZZ  ,DEPSYZ  ,DEPSZX  ,
     5     SIGOZZ  ,SIGOYZ  ,SIGOZX  ,SIGNZZ  ,SIGNYZ  ,SIGNZX  ,
     6     EPLASN  ,EPLAST  ,JSMS    ,DMELS   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C JSMS    |  1      | I | R | 0/1 (=1 IF /DT/AMS APPLIES TO THIS ELEMENT GROUP)
C---------+---------+---+---+--------------------------------------------
C MFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW not used
C KFUNC   | NFUNC   | I | R | FUNCTION INDEX not used
C NPF     |  *      | I | R | FUNCTION ARRAY   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
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
C DMELS   | NEL     | F | W | NON DIAGONAL TERM FOR AMS
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
#include      "sms_c.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s  
C----------------------------------------------------------
      INTEGER NEL,NUPARAM,NUVAR,MAXFUNC, JSMS
      INTEGER IFUNC(*),NPF(*)
      my_real
     .   TIME,TIMESTEP
      my_real
     .   UPARAM(NUPARAM),OFF(NEL),TF(*),EPLASN(NEL),EPLAST(NEL),AREA(NEL),
     .   EPSD(NEL),DEPSZZ(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSZZ(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIGOZZ(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   STIFM(NEL),SIGNZZ(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .   DMELS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I1,I2,I,J,IEL,IRATE,NRATE,IFILTR,NTRAC
      INTEGER IFUNN(MAXFUNC),IFUNT(MAXFUNC),JJ(NEL),ELTRAC(NEL),II(NEL)
      my_real
     .   E,ECOMP,G,G3,G12,G23,FCUT,DYDX1,DYDX2,Y1,Y2,R,DSIG,DEPLA,
     .   SVM,SIGT,DTB
      my_real
     .   DSIGX(NEL),DSIGY(NEL),DSIGZ(NEL),YFAC(MAXFUNC),RATE(MAXFUNC),
     .   YLD(2,NEL),HC(2,NEL),DEPST(NEL),STF(NEL),FAC(NEL)
C----------------------------------------------------------
C   E x t e r n a l  F u n c t i o n
C----------------------------------------------------------
       my_real
     .   FINTER
C=======================================================================
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
      E     = UPARAM(1) 
      G     = UPARAM(2)
      G3    = G*THREE
      NRATE = NINT(UPARAM(3))
      IFILTR= NINT(UPARAM(4))
      FCUT  = UPARAM(5)
      ECOMP = UPARAM(6)
c
      IF (ECOMP > 0) THEN
        ELTRAC(1:NEL) = 0
        NTRAC  = 0
        DO IEL=1,NEL
          IF (EPSZZ(IEL) > ZERO) THEN   ! element in traction
            STF(IEL)    = (E+G) * AREA(IEL)                                          
            DSIGZ(IEL)  = E*DEPSZZ(IEL)                                 
            NTRAC = NTRAC + 1
            ELTRAC(NTRAC) = IEL
          ELSE                          ! element in compression
            STF(IEL)    = (ECOMP+G) * AREA(IEL)                                          
            DSIGZ(IEL)  = ECOMP*DEPSZZ(IEL)
          ENDIF
        ENDDO
        DO IEL=1,NEL         
          DSIGY(IEL)  = G*DEPSYZ(IEL)                               
          DSIGX(IEL)  = G*DEPSZX(IEL)                               
          SIGNZZ(IEL) = SIGOZZ(IEL) + DSIGZ(IEL)                   
          SIGNYZ(IEL) = SIGOYZ(IEL) + DSIGY(IEL)                   
          SIGNZX(IEL) = SIGOZX(IEL) + DSIGX(IEL)                   
          STIFM(IEL)  = STIFM(IEL)  + STF(IEL)*OFF(IEL)                                               
        ENDDO      
      ELSE    ! symmetric traction/compression
        NTRAC = NEL
        DO IEL=1,NEL         
          ELTRAC(IEL) = IEL
          STF(IEL)    = (E+G) * AREA(IEL)                                            
          DSIGZ(IEL)  = E*DEPSZZ(IEL)                                   
          DSIGY(IEL)  = G*DEPSYZ(IEL)                               
          DSIGX(IEL)  = G*DEPSZX(IEL)                               
          SIGNZZ(IEL) = SIGOZZ(IEL) + DSIGZ(IEL)                   
          SIGNYZ(IEL) = SIGOYZ(IEL) + DSIGY(IEL)                   
          SIGNZX(IEL) = SIGOZX(IEL) + DSIGX(IEL)                   
          STIFM(IEL)  = STIFM(IEL)  + STF(IEL)*OFF(IEL)                                               
        ENDDO      
      ENDIF
c      
      IF (IDTMINS==2 .AND. JSMS/=0) THEN
        DTB = (DTMINS/DTFACS)**2
        DO IEL=1,NEL                                                 
c         omega = sqrt(2k/2*dmels), dt=2/omega, 2*dmels=dt**2 * 2k / 4
          DMELS(IEL) = MAX(DMELS(IEL),HALF*DTB*STF(IEL)*OFF(IEL))
        ENDDO                                                        
      END IF
c---    
      IF (NRATE > 0) THEN  ! plasticity
c---    
        DO IRATE=1,NRATE
          J = (IRATE-1)*2
          IFUNN(IRATE) = IFUNC(J+1)
          IFUNT(IRATE) = IFUNC(J+2)
          YFAC(IRATE)  = UPARAM(7+IRATE)
          RATE(IRATE)  = UPARAM(7+IRATE+NRATE)          
        ENDDO
c
c---    Calculate yield values
c
        IF (NRATE > 1) THEN    ! strain rate dependent                                         
c        
          DO IEL = 1,NEL                                                       
            II(IEL) = 1                                                        
            DO J=2,NRATE-1                                                
              IF (EPSD(IEL) > RATE(J)) II(IEL) = J
              EXIT
            ENDDO                                                         
          ENDDO                                                         
c
          DO IEL = 1,NEL                                                         
            I1 = II(IEL)                                                     
            I2 = I1 + 1                                                          
            FAC(IEL) = (EPSD(IEL) - RATE(I1)) / (RATE(I2) - RATE(I1))   
            Y1    = FINTER(IFUNT(I1),EPLAST(IEL),NPF,TF,DYDX1)*YFAC(I1)        
            Y2    = FINTER(IFUNT(I2),EPLAST(IEL),NPF,TF,DYDX2)*YFAC(I2)        
            Y1    = MAX(ZERO, Y1)  
            Y2    = MAX(ZERO, Y2)  
            DYDX1 = DYDX1*YFAC(I1)                                             
            DYDX2 = DYDX2*YFAC(I2)                                             
            YLD(2,IEL) = Y1 + FAC(IEL)*(Y2 - Y1)                                    
            HC(2,IEL)  = DYDX1 + FAC(IEL)*(DYDX2-DYDX1)                            
          ENDDO
          DO I=1,NTRAC                                                           
            IEL = ELTRAC(I)                                                          
            I1 = II(IEL)                                                     
            I2 = I1 + 1                                                          
            Y1  = FINTER(IFUNN(I1),EPLASN(IEL),NPF,TF,DYDX1)*YFAC(I1)        
            Y2  = FINTER(IFUNN(I2),EPLASN(IEL),NPF,TF,DYDX2)*YFAC(I2)        
            Y1 = MAX(ZERO, Y1)  
            Y2 = MAX(ZERO, Y2)  
            DYDX1 = DYDX1*YFAC(I1)                                             
            DYDX2 = DYDX2*YFAC(I2)                                             
            YLD(1,IEL) = Y1 + FAC(IEL)*(Y2 - Y1)                                    
            HC(1,IEL)  = DYDX1 + FAC(IEL)*(DYDX2-DYDX1)                            
          ENDDO
c
        ELSEIF (NRATE == 1) THEN    ! independent on strain rate                                
c
          DO IEL = 1,NEL                                                         
            I1 = 1                                                          
            YLD(2,IEL) = MAX(ZERO,
     .      FINTER(IFUNT(I1),EPLAST(IEL),NPF,TF,DYDX1)*YFAC(I1))
            HC(2,IEL) = DYDX1*YFAC(I1)
          ENDDO
          DO I=1,NTRAC                                                           
            IEL = ELTRAC(I)                                                          
            I1 = 1                                                          
            YLD(1,IEL) = MAX(ZERO,
     .      FINTER(IFUNN(I1),EPLASN(IEL),NPF,TF,DYDX1)*YFAC(I1))
            HC(1,IEL)  = DYDX1*YFAC(I1)
          ENDDO
        ENDIF
c-------
c       plasticity - normal  direction                                                        
c-------    
        DO I=1,NTRAC                                                           
          IEL = ELTRAC(I)                                                          
          SVM  = ABS(SIGNZZ(IEL))
          DSIG = SVM - YLD(1,IEL)
          IF (DSIG > ZERO) THEN
            DEPLA = DSIG / MAX((E + HC(1,IEL)),EM20)
            R  = MIN(ONE, ((YLD(1,IEL)+DEPLA*HC(1,IEL))/MAX(EM20,SVM)))                                   
            EPLASN(IEL) = EPLASN(IEL) + DEPLA*OFF(IEL)
            SIGNZZ(IEL) = SIGNZZ(IEL)*R                   
          ENDIF
        ENDDO                                                        
c-------
c       plasticity - tangent direction                                                        
c-------    
        DO IEL=1,NEL                                                     
          SVM  = SQRT(SIGNYZ(IEL)**2 + SIGNZX(IEL)**2)
          DSIG = SVM - YLD(2,IEL)
          IF (DSIG > ZERO) THEN
            DEPLA = DSIG / MAX((G + HC(2,IEL)),EM20)
            R  = MIN(ONE, ((YLD(2,IEL)+DEPLA*HC(2,IEL))/MAX(EM20,SVM)))
            EPLAST(IEL) = EPLAST(IEL) + DEPLA*OFF(IEL)
            SIGNYZ(IEL) = SIGNYZ(IEL)*R               
            SIGNZX(IEL) = SIGNZX(IEL)*R                   
          ENDIF                                                         
c
          SIGNZZ(IEL) = SIGNZZ(IEL)*OFF(IEL)                                                
          SIGNYZ(IEL) = SIGNYZ(IEL)*OFF(IEL)                                                
          SIGNZX(IEL) = SIGNZX(IEL)*OFF(IEL)
        ENDDO                                                          
c---    
      ENDIF
C-----------
      RETURN
      END
C
