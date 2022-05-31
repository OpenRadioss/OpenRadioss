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
Chd|  PRONY25C                      source/materials/mat/mat025/prony25c.F
Chd|-- called by -----------
Chd|        SIGEPS25C                     source/materials/mat/mat025/sigeps25c.F
Chd|-- calls ---------------
Chd|        ROTO_SIG                      source/airbag/roto.F          
Chd|====================================================================
      SUBROUTINE PRONY25C(NEL    ,NPRONY  ,NPMAX  ,BETA   ,KV     ,
     2                    GV     ,TIMESTEP,RHO0   ,OFF    ,DIR    ,
     3                    EPSPXX ,EPSPYY  ,EPSPXY ,EPSPYZ ,EPSPZX ,
     4                    SIGVXX ,SIGVYY  ,SIGVXY ,SIGVYZ ,SIGVZX ,
     5                    SIGV   ,SOUNDSP ,VARI   ,IGTYP  )
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
C NEL    |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL    | F | R | INITIAL DENSITY
C EPSPXX  | NEL    | F | R | STRAIN RATE XX
C EPSPYY  | NEL    | F | R | STRAIN RATE YY
C ...     |         |   |   |
C ...     |         |   |   |
C SIGVXX  | NEL    | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL    | F | W | VISCOUS STRESS YY
C ...     |         |   |   |
C VISCMAX | NEL    | F | W | MAXIMUN DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL    | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL ,NPRONY,NPMAX,IGTYP
      my_real
     .   TIME,TIMESTEP(*),KV(*),GV(NPMAX),BETA(NPMAX),
     .   RHO0(*),EPSPXX(*),EPSPYY(*),
     .   EPSPXY(*),EPSPYZ(*),EPSPZX(*),SIGV(NEL,5),DIR(NEL,2)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGVXX(*),SIGVYY(*),
     .    SIGVXY(*),SIGVYZ(*),SIGVZX(*),
     .    SOUNDSP(*)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real
     .   VARI(NEL,*) , OFF(*)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
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
     . I,J,II,LL
      my_real 
     .  CC,G1,DAV,EPXX(MVSIZ),EPYY(MVSIZ),EPZZ(MVSIZ),SVXX,
     .  SVYY,SVZZ,P(MVSIZ),
     .  AA(MVSIZ,NPMAX),BB(MVSIZ,NPMAX),H0(6),EPSPZZ(MVSIZ),
     .  H(MVSIZ,6,NPMAX),S(MVSIZ,6),A1(MVSIZ),A2(MVSIZ),FAC,H01,H02,H03,H04,
     .  H05,H06
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
C------------------------------------------
C   estiamte stress
C------------------------------------------
C      
       G1 = ZERO
       DO J=1,NPRONY
             G1 = G1 + GV(J)
       ENDDO 
C  
       A1(1:NEL) = ZERO
       A2(1:NEL) = ZERO
       EPSPZZ(1:NEL) = ZERO          
C
       DO J=1,NPRONY
         DO I=  1,NEL
          AA(I,J)=  EXP(-BETA(J)*TIMESTEP(I)) 
          BB(I,J)=TIMESTEP(I)*GV(J)*EXP(-HALF*BETA(J)*TIMESTEP(I))
C
C  for computing epszz_dot (sigzz=0)
C
          H03 = VARI(I, (J - 1)*6 + 3)
          A1(I)  = A1(I) +  AA(I,J)*H03 
          A2(I)  = A2(I) +  BB(I,J)
         ENDDO
       ENDDO  
C
C  compute epszz_dot  sig33= 0
C            
       DO I=  1,NEL
         FAC = ONE/MAX(EM20,TWO_THIRD*A2(I) + KV(I))
         EPSPZZ(I) = -A1(I)+(THIRD*A2(I)-KV(I))*(EPSPXX(I)+EPSPYY(I))
         EPSPZZ(I)= FAC*EPSPZZ(I)
       ENDDO
C                
       DO I=  1,NEL
C spherique part
          DAV = THIRD*(EPSPXX(I) + EPSPYY(I) + EPSPZZ(I))
          P(I) = -THREE*KV(I)*DAV
c deviatorique part           
          EPXX(I) = EPSPXX(I) - DAV
          EPYY(I) = EPSPYY(I) - DAV
          EPZZ(I) = EPSPZZ(I) - DAV
       ENDDO          
C  
       DO J= 1,NPRONY
         DO I=1,NEL
C         
           H01 = VARI(I, (J - 1)*6 + 1)
           H02 = VARI(I, (J - 1)*6 + 2)
           H03 = VARI(I, (J - 1)*6 + 3)
           H04 = VARI(I, (J - 1)*6 + 4)
           H05 = VARI(I, (J - 1)*6 + 5)
           H06 = VARI(I, (J - 1)*6 + 6)
C
           
           H(I,1,J) =    AA(I,J)*H01 + BB(I,J)*EPXX(I)
           H(I,2,J) =    AA(I,J)*H02 + BB(I,J)*EPYY(I)          
           H(I,3,J) =    AA(I,J)*H03 + BB(I,J)*EPZZ(I)
           H(I,4,J) =    AA(I,J)*H04 + HALF*BB(I,J)*EPSPXY(I)          
           H(I,5,J) =    AA(I,J)*H05 + HALF*BB(I,J)*EPSPYZ(I)
           H(I,6,J) =    AA(I,J)*H06 + HALF*BB(I,J)*EPSPZX(I)
C

           VARI(I, (J - 1)*6 + 1) = H(I,1,J)
           VARI(I, (J - 1)*6 + 2) = H(I,2,J)
           VARI(I, (J - 1)*6 + 3) = H(I,3,J)
           VARI(I, (J - 1)*6 + 4) = H(I,4,J)
           VARI(I, (J - 1)*6 + 5) = H(I,5,J)
           VARI(I, (J - 1)*6 + 6) = H(I,6,J) 
         ENDDO
       ENDDO 
C     
C  comppute stress
C
           
       DO II = 1,6
           S(1:MVSIZ,II) = ZERO
       ENDDO        

       DO  J= 1,NPRONY
         DO I=1,NEL
            S(I,1) = S(I,1) + H(I,1,J)
            S(I,2) = S(I,2) + H(I,2,J)
cc            S(I,3) = S(I,3) + H(I,3,J)
            S(I,4) = S(I,4) + H(I,4,J)
            S(I,5) = S(I,5) + H(I,5,J)
            S(I,6) = S(I,6) + H(I,6,J)
         ENDDO
       ENDDO
       DO I=1,NEL
          SIGVXX(I) = S(I,1)  - P(I)
          SIGVYY(I) = S(I,2)  - P(I)
          SIGVXY(I) = S(I,4) 
          SIGVYZ(I) = S(I,5) 
          SIGVZX(I) = S(I,6)                      
       ENDDO
C       
        DO I=1,NEL
          SIGVXX(I) = SIGVXX(I)*OFF(I)
          SIGVYY(I) = SIGVYY(I)*OFF(I)
          SIGVXY(I) = SIGVXY(I)*OFF(I)
          SIGVYZ(I) = SIGVYZ(I)*OFF(I)
          SIGVZX(I) = SIGVZX(I)*OFF(I)
C          
          SIGV(I,1) = SIGVXX(I)
          SIGV(I,2) = SIGVYY(I)
          SIGV(I,3) = SIGVXY(I)
          SIGV(I,4) = SIGVYZ(I)
          SIGV(I,5) = SIGVZX(I)
C              
          SOUNDSP(I) = SQRT(SOUNDSP(I)**2 + G1/RHO0(I))
        ENDDO        

C transformation dans le rep re d'orthotropie
        LL = 1
        CALL ROTO_SIG(LL,NEL,SIGV, DIR,NEL)
C
        DO I=1,NEL
          SIGVXX(I) = SIGV(I,1)
          SIGVYY(I) = SIGV(I,2)
          SIGVXY(I) = SIGV(I,3)
          SIGVYZ(I) = SIGV(I,4)
          SIGVZX(I) = SIGV(I,5)
        ENDDO
C
       RETURN
       END
