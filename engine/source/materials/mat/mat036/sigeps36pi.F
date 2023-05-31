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
Chd|  SIGEPS36PI                    source/materials/mat/mat036/sigeps36pi.F
Chd|-- called by -----------
Chd|        MULAWP                        source/elements/beam/mulawp.F 
Chd|-- calls ---------------
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS36PI(
     1               NEL     ,KFUNC   ,IPT     ,NPF     ,TF      ,        
     2               NGL     ,TIME    ,TIMESTEP,UPARAM  ,IPM     ,       
     3               MAT     ,E       ,G       ,OFF     ,DEPSXX  ,      
     4               DEPSXY  ,DEPSXZ  ,SIGOXX  ,SIGOXY  ,SIGOXZ  ,      
     5               SIGNXX  ,SIGNXY  ,SIGNXZ  ,ETSE    ,PLA     ,
     6               EPSP    ,NVARTMP ,VARTMP)      
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL    |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C NFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW
C KFUNC   | NFUNC   | I | R | FUNCTION INDEX BIDON
C NPF     |  *      | I | R | FUNCTION ARRAY   
C IPT     |  1      | I | R | INTEGRATION POINT NUMBER   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C EPSP    | NEL    | F | R | STRAIN RATE 
C DEPSXX  | NEL    | F | R | STRAIN INCREMENT XX
C ...     |         |   |   |
C SIGOXX  | NEL    | F | R | OLD ELASTO PLASTIC STRESS XX 
C ...     |         |   |   |    
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL    | F | W | NEW ELASTO PLASTIC STRESS XX
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C PLA     | NEL    | F |R/W| PLASTIC STRAIN
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL    | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "comlock.inc"
C---------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUPARAM,IPT,NVARTMP
      INTEGER MAT(NEL),NGL(NEL),NPF(*),IPM(NPROPMI,*),KFUNC(*)
      my_real
     . TIME,TIMESTEP,UPARAM(*),PLA(NEL),EPSP(NEL),
     .   DEPSXX(NEL),DEPSXY(NEL),DEPSXZ(NEL),
     .   SIGOXX(NEL),SIGOXY(NEL),SIGOXZ(NEL),
     .   E(*),G(*),TF(*)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNXY(NEL),SIGNXZ(NEL),ETSE(NEL),
     .    OFF(NEL)
      INTEGER, INTENT(INOUT) :: VARTMP(NEL,NVARTMP)
C-----------------------------------------------
C   E X T E R N A L   F u n c t i o n s
C-----------------------------------------------
      my_real
     .  FINTER 
      EXTERNAL FINTER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,J1,J2,N,NINDX,IADBUF,NFUNC,IPLAS,IFAIL,MFUNCE,ISMOOTH
      INTEGER NRATE(NEL),IAD1(NEL),IPOS1(NEL),ILEN1(NEL),
     .        IAD2(NEL),IPOS2(NEL),ILEN2(NEL),JJ(NEL),
     .        IFUNC(NEL,100),OPTE(NEL)
      my_real :: G3(NEL),DYDX1(NEL),DYDX2(NEL),RATE(NEL,2),
     .        Y1(NEL),Y2(NEL),DR(NEL),YFAC(NEL,2),
     .        PP(NEL),QQ(NEL),FAIL(NEL),SVM(NEL),H(NEL),
     .        EPSMAX(NEL),EPSR1(NEL),EPSR2(NEL),
     .        SIGEXX(NEL),SIGEYY(NEL),SIGEXY(NEL),
     .        HK(NEL),NU(NEL),NU1(NEL),EPSF(NEL),YLD(NEL),
     .        AA(NEL),BB(NEL),CC(NEL),EINF(NEL),CE(NEL),
     .        ESCALE(NEL),DYDXE(NEL)
      my_real :: R,UMR,FAC,GS,SVM1,SHFACT,EPSP1,EPSP2,RFAC
C=======================================================================
      IPLAS  = 1                        
      SHFACT = FIVE_OVER_6
      DO I=1,NEL                            
        NFUNC  = IPM(10,MAT(I))   
        DO J=1,NFUNC                         
          IFUNC(I,J)=IPM(10+J,MAT(I))   
        ENDDO                                
        FAIL(I) = ONE                            
      ENDDO                                  
C
      DO I=1,NEL                                
        IADBUF = IPM(7,MAT(I))-1                
        E(I)   = UPARAM(IADBUF+2)                
        G(I)   = UPARAM(IADBUF+5)                
        G3(I)  = THREE*G(I)                      
        NU(I)  = UPARAM(IADBUF+6)
        NRATE(I) = NINT(UPARAM(IADBUF+1))        
        EPSMAX(I)= UPARAM(IADBUF+6+2*NRATE(I)+1) 
c---------------------  
        OPTE(I) = UPARAM(IADBUF+2*NRATE(I) + 23) 
        EINF(I) = UPARAM(IADBUF+2*NRATE(I) + 24) 
        CE(I)   = UPARAM(IADBUF+2*NRATE(I) + 25) 
        IFAIL   = NINT(UPARAM(2*NRATE(I) + 27))    ! flag for failure
        ISMOOTH = NINT(UPARAM(2*NRATE(I) + 29))    ! strain rate interpolation flag
c--------------------
        IF (OPTE(I) == 1)THEN                                                     
           IF(PLA(I) > ZERO)THEN             
              MFUNCE= UPARAM(IADBUF+2*NRATE(I)+ 22)                                          
              ESCALE(I) = FINTER(IFUNC(I,MFUNCE),PLA(I),NPF,TF,DYDXE(I)) 
              E(I) =  ESCALE(I)* E(I)                                 
              G(I) =  HALF*E(I)/(ONE+NU(I)) 
              G3(I) = THREE*G(I) 
           ENDIF                                                             
         ELSEIF (CE(I) /= ZERO) THEN                                          
           IF(PLA(I) > ZERO)THEN                                                       
              E(I) = E(I)-(E(I)-EINF(I))*(ONE-EXP(-CE(I)*PLA(I)))                     
              G(I) =  HALF*E(I)/(ONE+NU(I))  
              G3(I) = THREE*G(I)                                              
           ENDIF                                                                   
         ENDIF                   
      ENDDO                                      
C
C-------------------
c       DO I=1,NEL
C---     Module Cisaillement Transverse
c         SH  = FIVE_OVER_6*G(I)                        
c         YMA = TWELVE*E(I)/AL(I)**2              
c         SH1 = YMA*IYY(I)                       
c         SH2 = YMA*IZZ(I)                       
C        shf = ishear   ( no shear = 1, shear = 0 = default)
c         SH0 = (ONE - SHF(I))*SH              
c         GS1 = SH0*SH1/(SH+SH1) + SHF(I)*SH1 
c         GS2 = SH0*SH2/(SH+SH2) + SHF(I)*SH2 
C---     Contraintes Elastiques
C---      CONTRAINTES PLASTIQUEMENT ADMISSIBLES
      DO I=1,NEL                              
        GS = SHFACT*G(I)                         
        SIGNXX(I) = SIGOXX(I) + E(I)*DEPSXX(I) 
        SIGNXY(I) = SIGOXY(I) + GS*DEPSXY(I)   
        SIGNXZ(I) = SIGOXZ(I) + GS*DEPSXZ(I)   
        ETSE(I)   = ONE                         
      ENDDO                                    
C-------------------
C     Yield Stress
C-------------------
      JJ(1:NEL) = 1 
      DO I=1,NEL                                     
        IADBUF = IPM(7,MAT(I))-1
        DO J=2,NRATE(I)-1                             
          IF (EPSP(I) > UPARAM(IADBUF+6+J)) JJ(I) = J 
        ENDDO                                         
      ENDDO                                           
      DO I=1,NEL                                               
        IADBUF = IPM(7,MAT(I))-1
        RATE(I,1) = UPARAM(IADBUF+6+JJ(I))                        
        YFAC(I,1) = UPARAM(IADBUF+6+NRATE(I)+JJ(I))   
        IF (NRATE(I) > 1) THEN         
          RATE(I,2) = UPARAM(IADBUF+6+JJ(I)+1)                      
          YFAC(I,2) = UPARAM(IADBUF+6+NRATE(I)+JJ(I)+1)  
        ENDIF           
      ENDDO                                                     
C
      DO I=1,NEL                                               
        J1 = JJ(I)                                              
        J2 = J1+1                                               
        IPOS2(I) = 1                                          
        IAD2(I)  = 1                 
        ILEN2(I) = 1 
        IPOS1(I) = VARTMP(I,J1)                                         
        IAD1(I)  = NPF(IFUNC(I,J1)) / 2 + 1                    
        ILEN1(I) = NPF(IFUNC(I,J1)+1) / 2 - IAD1(I) - IPOS1(I) 
        IF (NRATE(I) > 1) THEN         
          IPOS2(I) = VARTMP(I,J2)                                           
          IAD2(I)  = NPF(IFUNC(I,J2)) / 2 + 1                    
          ILEN2(I) = NPF(IFUNC(I,J2)+1) / 2 - IAD2(I) - IPOS2(I) 
        ENDIF           
      ENDDO                                                     
C
      CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,PLA,DYDX1,Y1)        
      CALL VINTER(TF,IAD2,IPOS2,ILEN2,NEL,PLA,DYDX2,Y2)        
C
      DO I=1,NEL
        IF (NRATE(I) == 1) THEN
          J1 = JJ(I)                                              
          YLD(I)  = FAIL(I)*Y1(I) * YFAC(I,1)    
          YLD(I)  = MAX(YLD(I),EM20)                              
          H(I)    = FAIL(I)*DYDX1(I)*YFAC(I,1)         
          VARTMP(I,J1) = IPOS1(I)                                 
        ELSE                                           
          J1 = JJ(I)                                              
          J2 = J1+1                                               
          Y1(I) = Y1(I)*YFAC(I,1)                                 
          Y2(I) = Y2(I)*YFAC(I,2)
          IF (ISMOOTH == 2) THEN       ! log_n  based interpolation of strain rate
            EPSP1 = MAX(RATE(I,1), EM20)
            EPSP2 = RATE(I,2)
            FAC   = LOG(MAX(EPSP(I),EM20)/EPSP1) / LOG(EPSP2/EPSP1)   
          ELSE                         ! linear interpolation of strain rate
            EPSP1 = RATE(I,1)
            EPSP2 = RATE(I,2)
            FAC   = (EPSP(I) - EPSP1) / (EPSP2 - EPSP1)   
          END IF
          YLD(I)  = FAIL(I)*(Y1(I) + FAC*(Y2(I)-Y1(I)))        
          YLD(I)  = MAX(YLD(I),EM20)                              
          DYDX1(I)= DYDX1(I)*YFAC(I,1)                            
          DYDX2(I)= DYDX2(I)*YFAC(I,2)                            
          H(I)    = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))  
          VARTMP(I,J1) = IPOS1(I)                                 
          VARTMP(I,J2) = IPOS2(I)
        END IF                              
      ENDDO                                                     
C-------------------
C     PROJECTION
C-------------------
c      IF (IPLAS == 1) THEN                                              
C---    radial return                                                   
        DO I=1,NEL                                                     
          SVM1 = SIGNXX(I)**2 + THREE*(SIGNXY(I)**2 + SIGNXZ(I)**2)     
          SVM1 = SQRT(SVM1)                                             
          R  = MIN( ONE, YLD(I)/MAX(EM20,SVM1) )                         
          SIGNXX(I)=SIGNXX(I)*R                                         
          SIGNXY(I)=SIGNXY(I)*R                                         
          SIGNXZ(I)=SIGNXZ(I)*R                                         
          UMR = ONE - R                                                  
          PLA(I) = PLA(I) + OFF(I)*SVM1*UMR/(G3(I)+H(I))
          IF (R < ONE) ETSE(I)= H(I)/(H(I)+E(I))                         
        ENDDO                                                           
c      ELSE                                                              
C---    plasticite uniaxiale : 3 iterations Newton                      
ctmp        DO J=1,NEL                                                     
ctmp          AA(I) = SIGNXX(I)*SIGNXX(I)                               
ctmp          BB(I) = THREE*SIGNXY(I)*SIGNXY(I)                         
ctmp          CC(I) = THREE*SIGNXZ(I)*SIGNXZ(I)                         
ctmp          SVM(I)= AA(I) + BB(I) + CC(I)                             
ctmp        ENDDO                                                           
ctmpC       plastic flow                                                
ctmp        NINDX=0                                                     
ctmp        DO I=1,NEL                                                 
ctmp          IF(SVM(I)>YLD(I).AND.OFF(I)==1.) THEN                
ctmp            NINDX = NINDX+1                                         
ctmp            INDEX(NINDX) = I                                        
ctmp          ENDIF                                                     
ctmp        ENDDO                                                       
ctmpC       def plastiques en contrainte uniaxiale                      
ctmp        DO J=1,NINDX                                                
ctmp          I=INDEX(J)                                                
ctmp          DPLA_J(I) = (SVM(I)-YLD(I))/(G3(I)+H(I))                  
ctmp          ETSE(I)   = H(I)/(H(I)+E(I))                              
ctmp        ENDDO                                                       
ctmpC       3 iterations Newton                                         
ctmp        DO N=1,NITER                                                
ctmp          DO J=1,NINDX                                              
ctmp            I = INDEX(J)                                            
ctmp            DPLA_I(I) = DPLA_J(I)                                   
ctmp            YLD_I = YLD(I) + H(I)*DPLA_I(I)                         
ctmp            DR(I) = E(I)*DPLA_I(I)/YLD_I                            
ctmp            PP(I) = ONE/(ONE+DR(I))                                   
ctmp            QQ(I) = ONE/(ONE+NU1(I)*DR(I))                            
ctmp            P2    = PP(I)*PP(I)                                     
ctmp            Q2    = QQ(I)*QQ(I)                                     
ctmp            F     = AA(I)*P2 + (BB(I)+CC(I))*Q2 - YLD_I*YLD_I       
ctmp            DF    = AA(I)*P2*PP(I) + NU*(BB(I)+CC(I))*Q2*QQ(I)      
ctmp            DF    = DF *(H(I)*DR(I) - E(I)) / YLD_I - H(I)*YLD_I    
ctmp            DF    = DF * TWO                                       
ctmp            DF    = SIGN(MAX(ABS(DF),EM20),DF)                      
ctmp            IF(DPLA_I(I) > ZERO) THEN                               
ctmp              DPLA_J(I) = MAX(ZERO,DPLA_I(I)-F/DF)                  
ctmp            ELSE                                                    
ctmp              DPLA_J(I) = ZERO                                      
ctmp            ENDIF                                                   
ctmp          ENDDO                                                     
ctmp        ENDDO                                                       
ctmpC       Contraintes plastiquement admissibles                       
ctmp        DO J=1,NINDX                                                
ctmp          I=INDEX(J)                                                
ctmp          PLA(I) = PLA(I) + DPLA_I(I)                               
ctmp          SIGNXX(I) = SIGNXX(I)*PP(I)                               
ctmp          SIGNXY(I) = SIGNXY(I)*QQ(I)                               
ctmp          SIGNXZ(I) = SIGNXZ(I)*QQ(I)                               
ctmp        ENDDO                                                       
C---
c      ENDIF                                                             
C---
      IF (IFAIL > 0) THEN
        DO I=1,NEL                                   
          IF (PLA(I) > EPSMAX(I) .AND. OFF(I)==ONE) THEN
            OFF(I) = FOUR_OVER_5
#include "lockon.inc"
            WRITE(IOUT, 1000) NGL(I),PLA(I),EPSP(I)
#include "lockoff.inc"
          END IF
        ENDDO
      END IF                                         
c---------------------------------------------------------
 1000 FORMAT(5X,'FAILURE BEAM ELEMENT NUMBER ',I10,
     .          ' PLASTIC STRAIN =',1PE16.9,' STRAIN RATE =',1PE16.9)
c---------------------------------------------------------
      RETURN
      END
