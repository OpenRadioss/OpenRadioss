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
Chd|  VISC_PRONY                    source/materials/visc/visc_prony.F
Chd|-- called by -----------
Chd|        VISCMAIN                      source/materials/visc/viscmain.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE VISC_PRONY(
     .           NEL     ,NUVAR   ,NPRONY  ,IADBUF  ,UPARAM  ,UVAR    ,
     .           EPSPXX  ,EPSPYY  ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX  ,
     .           SV1     ,SV2     ,SV3     ,SV4     ,SV5     ,SV6     ,
     .           TIMESTEP,RHO     ,VISCMAX ,SOUNDSP )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO     | NEL     | F | R | INITIAL DENSITY
C EPSPXX  | NEL     | F | R | STRAIN RATE XX
C EPSPYY  | NEL     | F | R | STRAIN RATE YY
C ...     |         |   |   |
C ...     |         |   |   |
C VISC    | NEL*6   | F | W | VISCOUS  STRESS 
C ...     |         |   |   |
C VISCMAX | NEL     | F | W | MAXIMUN DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C---------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUVAR,NPRONY,IADBUF
      my_real
     .   TIME,TIMESTEP
      my_real
     .   UPARAM(*),RHO(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),EPSPZZ(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   SV1(NEL),SV2(NEL),SV3(NEL),SV4(NEL),SV5(NEL),SV6(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    VISCMAX(NEL),SOUNDSP(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real
     .    UVAR(NEL,NUVAR),VISC(6,NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,IFLAG,IMOD
      my_real 
     .  KV0,P,G,DAV,EPXX,EPYY,EPZZ
      my_real 
     .  AA(NPRONY),BB(NPRONY),GV(NPRONY),BETA(NPRONY),H0(6),H(6),
     .  AAK(NPRONY),BBK(NPRONY),BETAK(NPRONY),KV(NPRONY),HP0,
     .  TRACE,RBULK,HP
C=======================================================================
      NUVAR = NUVAR/NPRONY
      G  = ZERO
      RBULK = ZERO
      KV0 = UPARAM(IADBUF)
      IFLAG = INT(UPARAM(IADBUF + 4*NPRONY + 2))
      IMOD = INT(UPARAM(IADBUF + 4*NPRONY + 3))
      IF(IFLAG < 0) THEN
C old formulation      
        DO J=1,NPRONY                                  
          GV(J)   = UPARAM(IADBUF + 1 + J)            
          BETA(J) = UPARAM(IADBUF + 1 + NPRONY + J)
          KV(J)   = UPARAM(IADBUF + 1 + 2*NPRONY + J)            
          BETAK(J) = UPARAM(IADBUF + 1 + 3*NPRONY + J)
C             
          G = G + GV(J)     
          RBULK = RBULK + KV(J)
          AA(J) = EXP(-BETA(J)*TIMESTEP)
          BB(J) = TIMESTEP*GV(J)*EXP(-UNDEMI*BETA(J)*TIMESTEP)
C          
          AAK(J) = EXP(-BETAK(J)*TIMESTEP)
          BBK(J) = TIMESTEP*KV(J)*EXP(-UNDEMI*BETAK(J)*TIMESTEP)     
        ENDDO
      ELSE
        DO J=1,NPRONY                                  
          GV(J)   = UPARAM(IADBUF + 1 + J)            
          BETA(J) = UPARAM(IADBUF + 1 + NPRONY + J)
          KV(J)   = UPARAM(IADBUF + 1 + 2*NPRONY + J)            
          BETAK(J) = UPARAM(IADBUF + 1 + 3*NPRONY + J)
          G = G + GV(J)   
          RBULK = RBULK + KV(J)
          AA(J) = EXP(-BETA(J)*TIMESTEP)
          BB(J) = DEUX*TIMESTEP*GV(J)*EXP(-UNDEMI*BETA(J)*TIMESTEP) 
C          
          AAK(J) = EXP(-BETAK(J)*TIMESTEP)
          BBK(J) = TIMESTEP*KV(J)*EXP(-UNDEMI*BETAK(J)*TIMESTEP)   
        ENDDO 
      ENDIF 
C     
      IF(IMOD > 0) THEN        
         DO I=1,NEL                                                     
c       spheric part 
           TRACE = -(EPSPXX(I) + EPSPYY(I) + EPSPZZ(I))
           DAV = THIRD*TRACE          
           P   = ZERO 
                                             
c          deviatoric part                                           
           EPXX = EPSPXX(I) + DAV                                    
           EPYY = EPSPYY(I) + DAV                                    
           EPZZ = EPSPZZ(I) + DAV
C           
           SV1(I) = ZERO
           SV2(I) = ZERO
           SV3(I) = ZERO
           SV4(I) = ZERO
           SV5(I) = ZERO
           SV6(I) = ZERO
C            
           DO J= 1,NPRONY  
             II = NUVAR*(J-1)
C                                                                    
             H0(1) = UVAR(I,II + 1)                         
             H0(2) = UVAR(I,II + 2)                         
             H0(3) = UVAR(I,II + 3)                         
             H0(4) = UVAR(I,II + 4)                         
             H0(5) = UVAR(I,II + 5)                         
             H0(6) = UVAR(I,II + 6)                          
             HP0    = UVAR(I,II + 7)                         
C
             H(1) = AA(J)*H0(1) + BB(J)*EPXX                     
             H(2) = AA(J)*H0(2) + BB(J)*EPYY                     
             H(3) = AA(J)*H0(3) + BB(J)*EPZZ                     
             H(4) = AA(J)*H0(4) + UNDEMI*BB(J)*EPSPXY(I)         
             H(5) = AA(J)*H0(5) + UNDEMI*BB(J)*EPSPYZ(I)         
             H(6) = AA(J)*H0(6) + UNDEMI*BB(J)*EPSPZX(I)                    
             HP = AAK(J)*HP0 + BBK(J)*TRACE           
C
             UVAR(I,II + 1) = H(1)                        
             UVAR(I,II + 2) = H(2)                        
             UVAR(I,II + 3) = H(3)                        
             UVAR(I,II + 4) = H(4)                        
             UVAR(I,II + 5) = H(5)                        
             UVAR(I,II + 6) = H(6)                          
             UVAR(I,II + 7) = HP                      
c
             SV1(I) = SV1(I) + H(1)                                    
             SV2(I) = SV2(I) + H(2)                                    
             SV3(I) = SV3(I) + H(3)                                    
             SV4(I) = SV4(I) + H(4)                                    
             SV5(I) = SV5(I) + H(5)                                    
             SV6(I) = SV6(I) + H(6)
             P  = P  + HP                                       
           ENDDO                                                     
c
           SV1(I) = SV1(I) - P                                       
           SV2(I) = SV2(I) - P                                       
           SV3(I) = SV3(I) - P                                       
C                                                                       
           VISCMAX(I) = ZERO                                            
           SOUNDSP(I) = SQRT(SOUNDSP(I)**2 + (QTHIRD*G + RBULK)/RHO(I)) 
          ENDDO  !   I=1,NEL
       ELSE
         DO I=1,NEL                                                     
c       spheric part 
           TRACE = EPSPXX(I) + EPSPYY(I) + EPSPZZ(I) 
           DAV = THIRD*TRACE  
           P  = -KV0*TRACE                  
c          deviatoric part                                           
           EPXX = EPSPXX(I) - DAV                                    
           EPYY = EPSPYY(I) - DAV                                    
           EPZZ = EPSPZZ(I) - DAV  
C           
           SV1(I) = ZERO
           SV2(I) = ZERO
           SV3(I) = ZERO
           SV4(I) = ZERO
           SV5(I) = ZERO
           SV6(I) = ZERO
C            
           DO J= 1,NPRONY  
             II = NUVAR*(J-1)
C                                                                    
             H0(1) = UVAR(I,II + 1)                         
             H0(2) = UVAR(I,II + 2)                         
             H0(3) = UVAR(I,II + 3)                         
             H0(4) = UVAR(I,II + 4)                         
             H0(5) = UVAR(I,II + 5)                         
             H0(6) = UVAR(I,II + 6)                          
             HP0    = UVAR(I,II + 7)                         
C
             H(1) = AA(J)*H0(1) + BB(J)*EPXX                     
             H(2) = AA(J)*H0(2) + BB(J)*EPYY                     
             H(3) = AA(J)*H0(3) + BB(J)*EPZZ                     
             H(4) = AA(J)*H0(4) + UNDEMI*BB(J)*EPSPXY(I)         
             H(5) = AA(J)*H0(5) + UNDEMI*BB(J)*EPSPYZ(I)         
             H(6) = AA(J)*H0(6) + UNDEMI*BB(J)*EPSPZX(I)
C
             UVAR(I,II + 1) = H(1)                        
             UVAR(I,II + 2) = H(2)                        
             UVAR(I,II + 3) = H(3)                        
             UVAR(I,II + 4) = H(4)                        
             UVAR(I,II + 5) = H(5)                        
             UVAR(I,II + 6) = H(6)                   
c
             SV1(I) = SV1(I) + H(1)                                    
             SV2(I) = SV2(I) + H(2)                                    
             SV3(I) = SV3(I) + H(3)                                    
             SV4(I) = SV4(I) + H(4)                                    
             SV5(I) = SV5(I) + H(5)                                    
             SV6(I) = SV6(I) + H(6)                            
           ENDDO                                                     
c
           SV1(I) = SV1(I) - P                                       
           SV2(I) = SV2(I) - P                                       
           SV3(I) = SV3(I) - P                                       
C                                                                       
           VISCMAX(I) = ZERO                                            
           SOUNDSP(I) = SQRT(SOUNDSP(I)**2 + (QTHIRD*G + RBULK)/RHO(I)) 
          ENDDO  !   I=1,NEL 
       ENDIF                                                         
c------------
      RETURN
      END
