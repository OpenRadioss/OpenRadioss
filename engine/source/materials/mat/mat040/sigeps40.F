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
c jbm036
Chd|====================================================================
Chd|  SIGEPS40                      source/materials/mat/mat040/sigeps40.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIGEPS40 (
     1     NEL    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,NPF    ,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0    ,RHO    ,
     3     VOLUME ,EINT   ,
     4     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVZZ  ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX,UVAR    ,OFF     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "r4r8_p.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C NFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW
C IFUNC   | NFUNC   | I | R | FUNCTION INDEX 
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
C VISCMAX | NEL     | F | W | MAXIMUN DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL, NUPARAM, NUVAR
      my_real 
     .   TIME,TIMESTEP,UPARAM(NUPARAM),
     .   RHO(NEL),RHO0(NEL),VOLUME(NEL),EINT(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),EPSPZZ(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,EPSZZ(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOZZ(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL)
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
     .       UVAR(NEL,NUVAR), OFF(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real 
     .       FINTER ,TF(*)
      EXTERNAL FINTER
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
      INTEGER I,J,K,M
      my_real
     .  AK,G0,G(5),BETA(5),
     .  EV,EDXX,EDYY,EDZZ,EDXY,EDYZ,EDZX,      
     .  SXX,SYY,SZZ,SXY,SYZ,SZX, 
     .  DT,SIGV,SDV(30),
     .  EPXX,EPYY,EPZZ,EPXY,EPYZ,EPZX,GT,
c jbm037
     .  ASTAS,BSTAS,VMISK,
     .  SSIG1,SSIG2 
      my_real
     .  EPRNXX,EPRNYY,EPRNZZ,EPRNXY,EPRNYZ,EPRNZX,
     .  EVR,EDRNXX,EDRNYY,EDRNZZ,EDRNXY,EDRNYZ,EDRNZX,
     .  EDRVXX,EDRVYY,EDRVZZ,EDRVXY,EDRVYZ,EDRVZX
      my_real
     .  AXX,AYY,AZZ,AXY,AYZ,AZX,
     .  BXX,BYY,BZZ,BXY,BYZ,BZX,
     .  AAXX,AAYY,AAZZ,AAXY,AAYZ,AAZX,
     .  BBXX,BBYY,BBZZ,BBXY,BBYZ,BBZX,
     .  CCXX,CCYY,CCZZ,CCXY,CCYZ,CCZX,
     .  EVSDXX,EVSDYY,EVSDZZ,EVSDXY,EVSDYZ,EVSDZX
C=======================================================================
C
C     VISCO-ELASTIC LAW 
C
C=======================================================================
        AK = UPARAM(1)
        G0   =  TWO*UPARAM(2)
        G(1) =  TWO*UPARAM(3)
        G(2) =  TWO*UPARAM(4)
        G(3) =  TWO*UPARAM(5)
        G(4) =  TWO*UPARAM(6)
        G(5) =  TWO*UPARAM(7)	
	
        BETA(1) =  UPARAM(8) 
        BETA(2) =  UPARAM(9) 
        BETA(3) =  UPARAM(10) 
        BETA(4) =  UPARAM(11) 
        BETA(5) =  UPARAM(12)
        ASTAS   =  UPARAM(13)
        BSTAS   =  UPARAM(14)
        VMISK   =  UPARAM(15) 
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
      IF(TIME<=EM20)THEN
        DO 100 J=1,NUVAR
        DO 100 I=1,NEL
           UVAR(I,J)=ZERO
 100    CONTINUE
      ENDIF
C-----------------------------------------------
        GT = G0 +  G(1) + G(2) + G(3) + G(4) +  G(5)
C
        DT = TIMESTEP 
C
      DO I=1,NEL
C
       SDV(1) = UVAR(I,11)
       SDV(2) = UVAR(I,12)
       SDV(3) = UVAR(I,13)
       SDV(4) = UVAR(I,14)
       SDV(5) = UVAR(I,15)
       SDV(6) = UVAR(I,16)
       SDV(7) = UVAR(I,17)
       SDV(8) = UVAR(I,18)
       SDV(9) = UVAR(I,19)
       SDV(10) = UVAR(I,20)
       SDV(11) = UVAR(I,21)
       SDV(12) = UVAR(I,22)
       SDV(13) = UVAR(I,23)
       SDV(14) = UVAR(I,24)
       SDV(15) = UVAR(I,25)
       SDV(16) = UVAR(I,26)
       SDV(17) = UVAR(I,27)
       SDV(18) = UVAR(I,28)
       SDV(19) = UVAR(I,29)
       SDV(20) = UVAR(I,30)
       SDV(21) = UVAR(I,31)
       SDV(22) = UVAR(I,32)
       SDV(23) = UVAR(I,33)
       SDV(24) = UVAR(I,34)
       SDV(25) = UVAR(I,35)
       SDV(26) = UVAR(I,36)
       SDV(27) = UVAR(I,37)
       SDV(28) = UVAR(I,38)
       SDV(29) = UVAR(I,39)
       SDV(30) = UVAR(I,40)
C
       EPXX =  EPSXX(I) 
       EPYY =  EPSYY(I) 
       EPZZ =  EPSZZ(I) 
       EPXY =  EPSXY(I) 
       EPYZ =  EPSYZ(I) 
       EPZX =  EPSZX(I) 
C
       EV = (EPXX + EPYY + EPZZ) * THIRD
       EDXX = EPXX - EV 
       EDYY = EPYY - EV 
       EDZZ = EPZZ - EV 
       EDXY = EPXY * HALF  
       EDYZ = EPYZ * HALF   
       EDZX = EPZX * HALF        
C
       EPRNXX = EPSPXX(I)
       EPRNYY = EPSPYY(I)
       EPRNZZ = EPSPZZ(I)
       EPRNXY = EPSPXY(I)
       EPRNYZ = EPSPYZ(I)
       EPRNZX = EPSPZX(I)
C
       EVR = (EPRNXX + EPRNYY + EPRNZZ) * THIRD
       EDRNXX = EPRNXX - EVR
       EDRNYY = EPRNYY - EVR
       EDRNZZ = EPRNZZ - EVR
       EDRNXY = EPRNXY * HALF
       EDRNYZ = EPRNYZ * HALF
       EDRNZX = EPRNZX * HALF       
C
       EDRVXX = UVAR(I,5) 
       EDRVYY = UVAR(I,6) 
       EDRVZZ = UVAR(I,7) 
       EDRVXY = UVAR(I,8) 
       EDRVYZ = UVAR(I,9) 
       EDRVZX = UVAR(I,10) 
C
       AXX = EDRVXX
c jbm037
       BXX = TWO * (EDRNXX - EDRVXX) / MAX(DT,EM20)
       AYY = EDRVYY
       BYY = TWO * (EDRNYY - EDRVYY) / MAX(DT,EM20)
       AZZ = EDRVZZ
       BZZ = TWO * (EDRNZZ - EDRVZZ) / MAX(DT,EM20)
       AXY = EDRVXY
       BXY = TWO * (EDRNXY - EDRVXY) / MAX(DT,EM20)
       AYZ = EDRVYZ
       BYZ = TWO * (EDRNYZ - EDRVYZ) / MAX(DT,EM20)
       AZX = EDRVZX
       BZX = TWO * (EDRNZX - EDRVZX) / MAX(DT,EM20)
c jbm037 (fin)
C       WRITE (11,*)AXX,EDRNXX,EDRVXX,DT,BXX
C
        J = 1 
        K = (J -1)*6    
        AAXX = G(J)/BETA(J) * (AXX - BXX/BETA(J))
        BBXX = G(J)/BETA(J) * BXX
        CCXX = SDV(K+1) - AAXX
        EVSDXX = AAXX + BBXX*DT + CCXX*EXP(-BETA(J)*DT) 
        AAYY = G(J)/BETA(J) * (AYY - BYY/BETA(J))
        BBYY = G(J)/BETA(J) * BYY
        CCYY = SDV(K+2) - AAYY
        EVSDYY = AAYY + BBYY*DT + CCYY*EXP(-BETA(J)*DT) 
        AAZZ = G(J)/BETA(J) * (AZZ - BZZ/BETA(J))
        BBZZ = G(J)/BETA(J) * BZZ
        CCZZ = SDV(K+3) - AAZZ
        EVSDZZ = AAZZ + BBZZ*DT + CCZZ*EXP(-BETA(J)*DT) 
        AAXY = G(J)/BETA(J) * (AXY - BXY/BETA(J))
        BBXY = G(J)/BETA(J) * BXY
        CCXY = SDV(K+4) - AAXY
        EVSDXY = AAXY + BBXY*DT + CCXY*EXP(-BETA(J)*DT) 
        AAYZ = G(J)/BETA(J) * (AYZ - BYZ/BETA(J))
        BBYZ = G(J)/BETA(J) * BYZ
        CCYZ = SDV(K+5) - AAYZ
        EVSDYZ = AAYZ + BBYZ*DT + CCYZ*EXP(-BETA(J)*DT) 
        AAZX = G(J)/BETA(J) * (AZX - BZX/BETA(J))
        BBZX = G(J)/BETA(J) * BZX
        CCZX = SDV(K+6) - AAZX
        EVSDZX = AAZX + BBZX*DT + CCZX*EXP(-BETA(J)*DT) 
        UVAR(I,(K + 11)) = EVSDXX
        UVAR(I,(K + 12)) = EVSDYY
        UVAR(I,(K + 13)) = EVSDZZ
        UVAR(I,(K + 14)) = EVSDXY
        UVAR(I,(K + 15)) = EVSDYZ
        UVAR(I,(K + 16)) = EVSDZX
C
        J = 2 
        K = (J -1)*6    
        AAXX = G(J)/BETA(J) * (AXX - BXX/BETA(J))
        BBXX = G(J)/BETA(J) * BXX
        CCXX = SDV(K+1) - AAXX
        EVSDXX = AAXX + BBXX*DT + CCXX*EXP(-BETA(J)*DT) 
        AAYY = G(J)/BETA(J) * (AYY - BYY/BETA(J))
        BBYY = G(J)/BETA(J) * BYY
        CCYY = SDV(K+2) - AAYY
        EVSDYY = AAYY + BBYY*DT + CCYY*EXP(-BETA(J)*DT) 
        AAZZ = G(J)/BETA(J) * (AZZ - BZZ/BETA(J))
        BBZZ = G(J)/BETA(J) * BZZ
        CCZZ = SDV(K+3) - AAZZ
        EVSDZZ = AAZZ + BBZZ*DT + CCZZ*EXP(-BETA(J)*DT) 
        AAXY = G(J)/BETA(J) * (AXY - BXY/BETA(J))
        BBXY = G(J)/BETA(J) * BXY
        CCXY = SDV(K+4) - AAXY
        EVSDXY = AAXY + BBXY*DT + CCXY*EXP(-BETA(J)*DT) 
        AAYZ = G(J)/BETA(J) * (AYZ - BYZ/BETA(J))
        BBYZ = G(J)/BETA(J) * BYZ
        CCYZ = SDV(K+5) - AAYZ
        EVSDYZ = AAYZ + BBYZ*DT + CCYZ*EXP(-BETA(J)*DT) 
        AAZX = G(J)/BETA(J) * (AZX - BZX/BETA(J))
        BBZX = G(J)/BETA(J) * BZX
        CCZX = SDV(K+6) - AAZX
        EVSDZX = AAZX + BBZX*DT + CCZX*EXP(-BETA(J)*DT) 
        UVAR(I,(K + 11)) = EVSDXX
        UVAR(I,(K + 12)) = EVSDYY
        UVAR(I,(K + 13)) = EVSDZZ
        UVAR(I,(K + 14)) = EVSDXY
        UVAR(I,(K + 15)) = EVSDYZ
        UVAR(I,(K + 16)) = EVSDZX
C
        J = 3 
        K = (J -1)*6    
        AAXX = G(J)/BETA(J) * (AXX - BXX/BETA(J))
        BBXX = G(J)/BETA(J) * BXX
        CCXX = SDV(K+1) - AAXX
        EVSDXX = AAXX + BBXX*DT + CCXX*EXP(-BETA(J)*DT) 
        AAYY = G(J)/BETA(J) * (AYY - BYY/BETA(J))
        BBYY = G(J)/BETA(J) * BYY
        CCYY = SDV(K+2) - AAYY
        EVSDYY = AAYY + BBYY*DT + CCYY*EXP(-BETA(J)*DT) 
        AAZZ = G(J)/BETA(J) * (AZZ - BZZ/BETA(J))
        BBZZ = G(J)/BETA(J) * BZZ
        CCZZ = SDV(K+3) - AAZZ
        EVSDZZ = AAZZ + BBZZ*DT + CCZZ*EXP(-BETA(J)*DT) 
        AAXY = G(J)/BETA(J) * (AXY - BXY/BETA(J))
        BBXY = G(J)/BETA(J) * BXY
        CCXY = SDV(K+4) - AAXY
        EVSDXY = AAXY + BBXY*DT + CCXY*EXP(-BETA(J)*DT) 
        AAYZ = G(J)/BETA(J) * (AYZ - BYZ/BETA(J))
        BBYZ = G(J)/BETA(J) * BYZ
        CCYZ = SDV(K+5) - AAYZ
        EVSDYZ = AAYZ + BBYZ*DT + CCYZ*EXP(-BETA(J)*DT) 
        AAZX = G(J)/BETA(J) * (AZX - BZX/BETA(J))
        BBZX = G(J)/BETA(J) * BZX
        CCZX = SDV(K+6) - AAZX
        EVSDZX = AAZX + BBZX*DT + CCZX*EXP(-BETA(J)*DT) 
        UVAR(I,(K + 11)) = EVSDXX
        UVAR(I,(K + 12)) = EVSDYY
        UVAR(I,(K + 13)) = EVSDZZ
        UVAR(I,(K + 14)) = EVSDXY
        UVAR(I,(K + 15)) = EVSDYZ
        UVAR(I,(K + 16)) = EVSDZX
C
        J = 4 
        K = (J -1)*6    
        AAXX = G(J)/BETA(J) * (AXX - BXX/BETA(J))
        BBXX = G(J)/BETA(J) * BXX
        CCXX = SDV(K+1) - AAXX
        EVSDXX = AAXX + BBXX*DT + CCXX*EXP(-BETA(J)*DT) 
        AAYY = G(J)/BETA(J) * (AYY - BYY/BETA(J))
        BBYY = G(J)/BETA(J) * BYY
        CCYY = SDV(K+2) - AAYY
        EVSDYY = AAYY + BBYY*DT + CCYY*EXP(-BETA(J)*DT) 
        AAZZ = G(J)/BETA(J) * (AZZ - BZZ/BETA(J))
        BBZZ = G(J)/BETA(J) * BZZ
        CCZZ = SDV(K+3) - AAZZ
        EVSDZZ = AAZZ + BBZZ*DT + CCZZ*EXP(-BETA(J)*DT) 
        AAXY = G(J)/BETA(J) * (AXY - BXY/BETA(J))
        BBXY = G(J)/BETA(J) * BXY
        CCXY = SDV(K+4) - AAXY
        EVSDXY = AAXY + BBXY*DT + CCXY*EXP(-BETA(J)*DT) 
        AAYZ = G(J)/BETA(J) * (AYZ - BYZ/BETA(J))
        BBYZ = G(J)/BETA(J) * BYZ
        CCYZ = SDV(K+5) - AAYZ
        EVSDYZ = AAYZ + BBYZ*DT + CCYZ*EXP(-BETA(J)*DT) 
        AAZX = G(J)/BETA(J) * (AZX - BZX/BETA(J))
        BBZX = G(J)/BETA(J) * BZX
        CCZX = SDV(K+6) - AAZX
        EVSDZX = AAZX + BBZX*DT + CCZX*EXP(-BETA(J)*DT) 
        UVAR(I,(K + 11)) = EVSDXX
        UVAR(I,(K + 12)) = EVSDYY
        UVAR(I,(K + 13)) = EVSDZZ
        UVAR(I,(K + 14)) = EVSDXY
        UVAR(I,(K + 15)) = EVSDYZ
        UVAR(I,(K + 16)) = EVSDZX
C       
        J = 5 
        K = (J -1)*6    
        AAXX = G(J)/BETA(J) * (AXX - BXX/BETA(J))
        BBXX = G(J)/BETA(J) * BXX
        CCXX = SDV(K+1) - AAXX
        EVSDXX = AAXX + BBXX*DT + CCXX*EXP(-BETA(J)*DT) 
        AAYY = G(J)/BETA(J) * (AYY - BYY/BETA(J))
        BBYY = G(J)/BETA(J) * BYY
        CCYY = SDV(K+2) - AAYY
        EVSDYY = AAYY + BBYY*DT + CCYY*EXP(-BETA(J)*DT) 
        AAZZ = G(J)/BETA(J) * (AZZ - BZZ/BETA(J))
        BBZZ = G(J)/BETA(J) * BZZ
        CCZZ = SDV(K+3) - AAZZ
        EVSDZZ = AAZZ + BBZZ*DT + CCZZ*EXP(-BETA(J)*DT) 
        AAXY = G(J)/BETA(J) * (AXY - BXY/BETA(J))
        BBXY = G(J)/BETA(J) * BXY
        CCXY = SDV(K+4) - AAXY
        EVSDXY = AAXY + BBXY*DT + CCXY*EXP(-BETA(J)*DT) 
        AAYZ = G(J)/BETA(J) * (AYZ - BYZ/BETA(J))
        BBYZ = G(J)/BETA(J) * BYZ
        CCYZ = SDV(K+5) - AAYZ
        EVSDYZ = AAYZ + BBYZ*DT + CCYZ*EXP(-BETA(J)*DT) 
        AAZX = G(J)/BETA(J) * (AZX - BZX/BETA(J))
        BBZX = G(J)/BETA(J) * BZX
        CCZX = SDV(K+6) - AAZX
        EVSDZX = AAZX + BBZX*DT + CCZX*EXP(-BETA(J)*DT) 
        UVAR(I,(K + 11)) = EVSDXX
        UVAR(I,(K + 12)) = EVSDYY
        UVAR(I,(K + 13)) = EVSDZZ
        UVAR(I,(K + 14)) = EVSDXY
        UVAR(I,(K + 15)) = EVSDYZ
        UVAR(I,(K + 16)) = EVSDZX
C
       SIGV = (SIGOXX(I) + SIGOYY(I) + SIGOZZ(I))* THIRD
       SIGV = SIGV + (3. * AK) * 
     .        (DEPSXX(I) + DEPSYY(I) + DEPSZZ(I)) * THIRD 
C       SIGV = (3. * AK) * EV 
C
       SXX = G0 * EDXX
       SYY = G0 * EDYY
       SZZ = G0 * EDZZ
       SXY = G0 * EDXY
       SYZ = G0 * EDYZ
       SZX = G0 * EDZX
C
       SXX = SXX + UVAR(I,11)  
       SYY = SYY + UVAR(I,12)
       SZZ = SZZ + UVAR(I,13)
       SXY = SXY + UVAR(I,14)
       SYZ = SYZ + UVAR(I,15)
       SZX = SZX + UVAR(I,16)
C
       SXX = SXX + UVAR(I,17)  
       SYY = SYY + UVAR(I,18)
       SZZ = SZZ + UVAR(I,19)
       SXY = SXY + UVAR(I,20)
       SYZ = SYZ + UVAR(I,21)
       SZX = SZX + UVAR(I,22)
C
       SXX = SXX + UVAR(I,23)  
       SYY = SYY + UVAR(I,24)
       SZZ = SZZ + UVAR(I,25)
       SXY = SXY + UVAR(I,26)
       SYZ = SYZ + UVAR(I,27)
       SZX = SZX + UVAR(I,28)
C
       SXX = SXX + UVAR(I,29)  
       SYY = SYY + UVAR(I,30)
       SZZ = SZZ + UVAR(I,31)
       SXY = SXY + UVAR(I,32)
       SYZ = SYZ + UVAR(I,33)
       SZX = SZX + UVAR(I,34)
C
       SXX = SXX + UVAR(I,35)  
       SYY = SYY + UVAR(I,36)
       SZZ = SZZ + UVAR(I,37)
       SXY = SXY + UVAR(I,38)
       SYZ = SYZ + UVAR(I,39)
       SZX = SZX + UVAR(I,40)
C
       UVAR(I,5)  = TWO * EDRNXX - EDRVXX
       UVAR(I,6)  = TWO * EDRNYY - EDRVYY
       UVAR(I,7)  = TWO * EDRNZZ - EDRVZZ
       UVAR(I,8)  = TWO * EDRNXY - EDRVXY
       UVAR(I,9)  = TWO * EDRNYZ - EDRVYZ
       UVAR(I,10) = TWO * EDRNZX - EDRVZX       
C
       SIGNXX(I) = SXX + SIGV  
       SIGNYY(I) = SYY + SIGV
       SIGNZZ(I) = SZZ + SIGV 
       SIGNXY(I) = SXY 
       SIGNYZ(I) = SYZ 
       SIGNZX(I) = SZX 
C   
        SOUNDSP(I) = SQRT((AK/RHO(I)) + 
     .                ((TWO*TWO*GT)/(RHO(I)*THREE)))
        VISCMAX(I) = ZERO
C
C       WRITE (6,*) SIGNXX(I),SIGNYY(I),SIGNZZ(I),SIGNXY(I)
c       WRITE (6,*) SOUNDSP(I),AK,RHO0(I),GT
C CRITERES DE STASSI ET DE TYPE VON MISES
C
      SSIG1=SIGNXX(I)+SIGNYY(I)+SIGNZZ(I)
      SSIG2=HALF*(SXX**2+SYY**2+SZZ**2)+SXY**2+SYZ**2+SZX**2
      SSIG2=THREE*SSIG2
!      UVAR(I,1)=SSIG2**HALF/VMISK
      IF(SSIG2>ZERO) THEN
       UVAR(I,1)=EXP( HALF*LOG(SSIG2) )/VMISK
      ELSE
       UVAR(I,1) = ZERO
      ENDIF
!      UVAR(I,2)=(SSIG1+(SSIG1**2.+ASTAS*2.*SSIG2)**HALF)/BSTAS
      IF( (SSIG1**2.+ASTAS*2.*SSIG2)>ZERO ) THEN
       UVAR(I,2)=( SSIG1+EXP( HALF*LOG(SSIG1**2+ASTAS*2*SSIG2) ) )/BSTAS
      ELSE
       UVAR(I,2) = SSIG1/BSTAS
      ENDIF
      UVAR(I,3)=MAX(UVAR(I,3),UVAR(I,1))
      UVAR(I,4)=MAX(UVAR(I,4),UVAR(I,2))
C
      ENDDO
      RETURN
      END
