Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  PRONY_MODELC                  source/materials/mat/mat066/prony_modelc.F
Chd|-- called by -----------
Chd|        SIGEPS66C                     source/materials/mat/mat066/sigeps66c.F
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRONY_MODELC(
     1     NEL  ,NUVAR  ,TIME   ,TIMESTEP,
     2     NPRONY   ,KV     ,GI , BETA     ,RHO0  ,
     3     EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     4     SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     5     SOUNDSP,UVAR    ,OFF )

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
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL    | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL, NUVAR,NPRONY
      my_real
     .   TIME,TIMESTEP(NEL),KV,GI(NPRONY),BETA(NPRONY),
     .   RHO0(NEL), EPSPXX(NEL),EPSPYY(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGVXX(NEL),SIGVYY(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real
     .    UVAR(NEL,NUVAR), OFF(NEL),THK(NEL),YLD(NEL)
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
     . I,J,II
      my_real 
     .  CC,G,DAV,EPXX,EPYY,EPZZ,SVXX,
     .  SVYY,SVZZ,P,
     .  AA(MVSIZ,NPRONY),BB(MVSIZ,NPRONY),H0(6),EPSPZZ(MVSIZ),
     .  H(6,NPRONY),S(6),A1(MVSIZ),A2(MVSIZ),FAC
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
C------------------------------------------
C   estiamte stress
C------------------------------------------
       G = ZERO
C       
       DO J=1,NPRONY
          G = G + GI(J)
       ENDDO   
       DO I=1,NEL
          A1(I) = ZERO
          A2(I) = ZERO
          EPSPZZ(I) = ZERO
C          
         DO J=1,NPRONY
          AA(I,J) =  EXP(-BETA(J)*TIMESTEP(I)) 
          BB(I,J) =  TIMESTEP(I)*GI(J)*EXP(-HALF*BETA(J)*TIMESTEP(I))
C
C  for computing epszz_dot (sigzz=0)
C
          H0(3) = UVAR(I, (J - 1)*6 + 3)
          A1(I)  = A1(I) +  AA(I,J)*H0(3) 
          A2(I)  = A2(I) +  BB(I,J)
         ENDDO
       ENDDO  
C
C  compute epszz_dot  sig33= 0
C            
       DO I=1,NEL
         FAC = ONE/MAX(EM20,TWO_THIRD*A2(I) + KV)
         EPSPZZ(I) = -A1(I) + (THIRD*A2(I)-KV)*(EPSPXX(I) + EPSPYY(I))
         EPSPZZ(I)= FAC*EPSPZZ(I)
       ENDDO
C                
       DO I=1,NEL 
C spherique part
           DAV = THIRD*(EPSPXX(I) + EPSPYY(I) + EPSPZZ(I))
           P = -THREE*KV*DAV
c deviatorique part           
          EPXX = EPSPXX(I) - DAV
          EPYY = EPSPYY(I) - DAV
          EPZZ = EPSPZZ(I) - DAV          
C  
          DO J= 1,NPRONY
C         
           H0(1) = UVAR(I, (J - 1)*6 + 1)
           H0(2) = UVAR(I, (J - 1)*6 + 2)
           H0(3) = UVAR(I, (J - 1)*6 + 3)
           H0(4) = UVAR(I, (J - 1)*6 + 4)
           H0(5) = UVAR(I, (J - 1)*6 + 5)
           H0(6) = UVAR(I, (J - 1)*6 + 6)
C
           
           H(1,J) =    AA(I,J)*H0(1) + BB(I,J)*EPXX
           H(2,J) =    AA(I,J)*H0(2) + BB(I,J)*EPYY          
           H(3,J) =    AA(I,J)*H0(3) + BB(I,J)*EPZZ
           H(4,J) =    AA(I,J)*H0(4) + HALF*BB(I,J)*EPSPXY(I)          
           H(5,J) =    AA(I,J)*H0(5) + HALF*BB(I,J)*EPSPYZ(I)
           H(6,J) =    AA(I,J)*H0(6) + HALF*BB(I,J)*EPSPZX(I)
C
           UVAR(I, (J - 1)*6 + 1) = H(1,J)
           UVAR(I, (J - 1)*6 + 2) = H(2,J)
           UVAR(I, (J - 1)*6 + 3) = H(3,J)
           UVAR(I, (J - 1)*6 + 4) = H(4,J)
           UVAR(I, (J - 1)*6 + 5) = H(5,J)
           UVAR(I, (J - 1)*6 + 6) = H(6,J)  
           
         ENDDO 
C     
C  comppute stress
C
           
          DO II = 1,6
           S(II) = ZERO
          ENDDO        

          DO  J= 1,NPRONY
            S(1) = S(1) + H(1,J)
            S(2) = S(2) + H(2,J)
cc            S(3) = S(3) + H(3,J)
            S(4) = S(4) + H(4,J)
            S(5) = S(5) + H(5,J)
            S(6) = S(6) + H(6,J)
          ENDDO

          SIGVXX(I) = S(1)  - P
          SIGVYY(I) = S(2)  - P
cc          SIGVZZ(I) = S(3)  - P
          SIGVXY(I) = S(4) 
          SIGVYZ(I) = S(5) 
          SIGVZX(I) = S(6) 
C                  
       ENDDO
C       
        DO I=1,NEL
          SIGVXX(I) = SIGVXX(I)*OFF(I)
          SIGVYY(I) = SIGVYY(I)*OFF(I)
          SIGVXY(I) = SIGVXY(I)*OFF(I)
          SIGVYZ(I) = SIGVYZ(I)*OFF(I)
          SIGVZX(I) = SIGVZX(I)*OFF(I)
C              
          SOUNDSP(I) = SQRT(SOUNDSP(I)**2 + G/RHO0(I))
        ENDDO
       RETURN
       END
