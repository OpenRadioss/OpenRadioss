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
Chd|  I2LOCEQ                       interf/i2loceq.F              
Chd|-- called by -----------
Chd|        I2_DTN_25                     starter/source/interfaces/inter3d1/i2_dtn.F
Chd|        I2_DTN_27_PEN                 starter/source/interfaces/inter3d1/i2_dtn_27.F
Chd|        I2FOR25                       engine/source/interfaces/interf/i2for25.F
Chd|        I2FOR25P                      engine/source/interfaces/interf/i2for25p.F
Chd|        I2FOR26                       engine/source/interfaces/interf/i2for26.F
Chd|        I2FOR26P                      engine/source/interfaces/interf/i2for26p.F
Chd|        I2FOR27P_PEN                  engine/source/interfaces/interf/i2for27p_pen.F
Chd|        I2FOR27_PEN                   engine/source/interfaces/interf/i2for27_pen.F
Chd|-- calls ---------------
Chd|        INV3                          starter/source/tools/univ/inv3.F
Chd|        INV3                          engine/source/elements/joint/rskew33.F
Chd|====================================================================
      SUBROUTINE I2LOCEQ(
     .           NIR    ,RS     ,RX     ,RY     ,RZ      ,
     .           FMX    ,FMY    ,FMZ    ,H      ,STIFM   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIR
C     REAL
      my_real
     .   RS(3),RX(4),RY(4),RZ(4),FMX(4),FMY(4),FMZ(4),H(4),STIFM
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J
C     REAL
      my_real
     .   FAC,MX,MY,MZ,D1,D2,DD,RAX,RBX,RAY,RBY,
     .   MXY,MYX,MYZ,MZY,MXZ,MZX,MXX,MYY,MZZ,RXX,RYY,FXX,FYY,FZZ,FA,FB
      my_real
     .   RSX(4),RSY(4),RSZ(4),DF(NIR),MAT(3,3),MATI(3,3),BX,BY,BZ
C=======================================================================
C     Moment equilibrium in LOCAL coordinates
C-------------------------------------------------
      RSX(1) = RX(1) - RS(1)                      
      RSY(1) = RY(1) - RS(2)                       
      RSZ(1) = RZ(1) - RS(3)                       
      RSX(2) = RX(2) - RS(1)                       
      RSY(2) = RY(2) - RS(2)                       
      RSZ(2) = RZ(2) - RS(3)                       
      RSX(3) = RX(3) - RS(1)                      
      RSY(3) = RY(3) - RS(2)                      
      RSZ(3) = RZ(3) - RS(3) 
      RSX(4) = RX(4) - RS(1)                      
      RSY(4) = RY(4) - RS(2)                      
      RSZ(4) = RZ(4) - RS(3) 
c
      MX = ZERO
      MY = ZERO
      MZ = ZERO
      DO J=1,NIR
        MX = MX + RSY(J)*FMZ(J) - RSZ(J)*FMY(J)
        MY = MY + RSZ(J)*FMX(J) - RSX(J)*FMZ(J)
        MZ = MZ + RSX(J)*FMY(J) - RSY(J)*FMX(J)
      ENDDO
C
      BX= RSX(1)*H(1)+RSX(2)*H(2)+RSX(3)*H(3)+RSX(4)*H(4)
      BY= RSY(1)*H(1)+RSY(2)*H(2)+RSY(3)*H(3)+RSY(4)*H(4)
      BZ= RSZ(1)*H(1)+RSZ(2)*H(2)+RSZ(3)*H(3)+RSZ(4)*H(4)
C-------------------------------------------------
      IF (NIR == 4) THEN
        RXX = RX(2)+RX(3)-RX(1)-RX(4)
        RYY = RY(3)+RY(4)-RY(1)-RY(2)
c        RXX = ABS(RXX)
c        RYY = ABS(RYY)
C---    Moment Z
c
        MXY = RSX(1)*FMY(1)+RSX(2)*FMY(2)+RSX(3)*FMY(3)+RSX(4)*FMY(4)
        MYX = RSY(1)*FMX(1)+RSY(2)*FMX(2)+RSY(3)*FMX(3)+RSY(4)*FMX(4)
        MZZ = MXY - MYX
        FYY = MXY/RXX         
        FXX = -MYX/RYY      
C
        STIFM=MAX(ABS(BX) / ABS(RXX),ABS(BY) / ABS(RYY))
C
        FMX(1) = FMX(1) - FXX    
        FMX(2) = FMX(2) - FXX    
        FMX(3) = FMX(3) + FXX    
        FMX(4) = FMX(4) + FXX    

        FMY(1) = FMY(1) + FYY    
        FMY(2) = FMY(2) - FYY    
        FMY(3) = FMY(3) - FYY    
        FMY(4) = FMY(4) + FYY    
c
c---    Moments Mx, MY
c
        MYZ = RSY(1)*FMZ(1)+RSY(2)*FMZ(2)+RSY(3)*FMZ(3)+RSY(4)*FMZ(4)
        MZY = RSZ(1)*FMY(1)+RSZ(2)*FMY(2)+RSZ(3)*FMY(3)+RSZ(4)*FMY(4)
        MXX = MYZ - MZY
        MZX = RSZ(1)*FMX(1)+RSZ(2)*FMX(2)+RSZ(3)*FMX(3)+RSZ(4)*FMX(4)
        MXZ = RSX(1)*FMZ(1)+RSX(2)*FMZ(2)+RSX(3)*FMZ(3)+RSX(4)*FMZ(4)
        MYY = MZX - MXZ
c
        RAX = RX(1) - RX(3)   
        RBX = RX(4) - RX(2)   
        RAY = RY(1) - RY(3)   
        RBY = RY(4) - RY(2)   
        D1 = -MXX*RBX - MYY*RBY            
        D2 =  MXX*RAX + MYY*RAY            
        DD =  RAY*RBX - RAX*RBY                   
        FA = D1 / DD                             
        FB = D2 / DD                             
C
        STIFM=MAX(STIFM,
     .        SQRT((BY*BY+BZ*BZ)*RBX*RBX+(BZ*BZ+BX*BX)*RBY*RBY)/ABS(DD),
     .        SQRT((BY*BY+BZ*BZ)*RAX*RAX+(BZ*BZ+BX*BX)*RAY*RAY)/ABS(DD))
C
        FMZ(1) = FMZ(1) + FA                     
        FMZ(2) = FMZ(2) - FB                     
        FMZ(3) = FMZ(3) - FA                     
        FMZ(4) = FMZ(4) + FB                     
c

      ELSEIF (NIR == 3) THEN
        RXX = RX(2)+RX(3)-TWO*RX(1)
        RYY = TWO*RY(3)-RY(1)-RY(2)
C---    Moment Z
C
        MXY = RSX(1)*FMY(1)+RSX(2)*FMY(2)+RSX(3)*FMY(3)
        MYX = RSY(1)*FMX(1)+RSY(2)*FMX(2)+RSY(3)*FMX(3)
        MZZ = MXY - MYX

        FYY = -MXY/RXX
        FXX = -MYX/RYY
C
        STIFM=MAX(ABS(BX) / ABS(RXX),ABS(BY) / ABS(RYY))
C
        FMX(1) = FMX(1) - FXX   
        FMX(2) = FMX(2) - FXX    
        FMX(3) = FMX(3) + TWO * FXX    

        FMY(1) = FMY(1) - TWO * FYY
        FMY(2) = FMY(2) + FYY    
        FMY(3) = FMY(3) + FYY    
c
        MX=MX+(TWO*RSZ(1)-RSZ(2)-RSZ(3))*FYY
        MY=MY+(TWO*RSZ(3)-RSZ(1)-RSZ(2))*FXX
c---    Moments Mx, MY
c
        MAT(1,1) = RSY(1)
        MAT(1,2) = RSY(2)
        MAT(1,3) = RSY(3)
        MAT(2,1) = RSX(1)
        MAT(2,2) = RSX(2)
        MAT(2,3) = RSX(3)
        MAT(3,1) = ONE
        MAT(3,2) = ONE
        MAT(3,3) = ONE
        CALL INV3(MAT,MATI)
        DF(1) = -MATI(1,1)*MX + MATI(1,2)*MY
        DF(2) = -MATI(2,1)*MX + MATI(2,2)*MY
        DF(3) = -MATI(3,1)*MX + MATI(3,2)*MY
        DO J=1,NIR
          FMZ(J) = FMZ(J) + DF(J)
        ENDDO
C
        BX= RSX(1)*H(1)+RSX(2)*H(2)+RSX(3)*H(3)
        BY= RSY(1)*H(1)+RSY(2)*H(2)+RSY(3)*H(3)
        BZ= RSZ(1)*H(1)+RSZ(2)*H(2)+RSZ(3)*H(3)
        STIFM= MAX(STIFM,SQRT(BX*BX+BY*BY+BZ*BZ)*SQRT(MAX(
     .    MATI(1,1)*MATI(1,1)+MATI(2,1)*MATI(2,1)+MATI(3,1)*MATI(3,1),
     .    MATI(1,2)*MATI(1,2)+MATI(2,2)*MATI(2,2)+MATI(3,2)*MATI(3,2),
     .    MATI(1,3)*MATI(1,3)+MATI(2,3)*MATI(2,3)+MATI(3,3)*MATI(3,3))))
c       MX = ZERO
c       MY = ZERO
c       MZ = ZERO
c       DO J=1,NIR
c         MX = MX + RSY(J)*FMZ(J) - RSZ(J)*FMY(J)
c         MY = MY + RSZ(J)*FMX(J) - RSX(J)*FMZ(J)
c         MZ = MZ + RSX(J)*FMY(J) - RSY(J)*FMX(J)
c       ENDDO
C       print *,mx,my,mz
C-----
      ENDIF
C-----------
      RETURN
      END 
Chd|====================================================================
Chd|  I2LOCEQ_27                    interf/i2loceq.F              
Chd|-- called by -----------
Chd|        I2_DTN_27_CIN                 starter/source/interfaces/inter3d1/i2_dtn_27.F
Chd|        I2FOR27P_CIN                  engine/source/interfaces/interf/i2for27p_cin.F
Chd|        I2FOR27_CIN                   engine/source/interfaces/interf/i2for27_cin.F
Chd|-- calls ---------------
Chd|        INV3                          starter/source/tools/univ/inv3.F
Chd|        INV3                          engine/source/elements/joint/rskew33.F
Chd|====================================================================
      SUBROUTINE I2LOCEQ_27(
     .           NIR    ,RS     ,RX     ,RY     ,RZ      ,
     .           FMX    ,FMY    ,FMZ    ,H      ,STIFM   ,
     .           MXS    ,MYS    ,MZS    ,STIFMR ,BETAX   ,
     .           BETAY)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIR
C     REAL
      my_real
     .   RS(3),RX(4),RY(4),RZ(4),FMX(4),FMY(4),FMZ(4),H(4),STIFM,MXS,MYS,MZS,STIFMR,
     .   BETAX,BETAY
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J
C     REAL
      my_real
     .   FAC,MX,MY,MZ,D1,D2,DD,RAX,RBX,RAY,RBY,
     .   MXY,MYX,MYZ,MZY,MXZ,MZX,MXX,MYY,MZZ,RXX,RYY,FXX,FYY,FZZ,FA,FB
      my_real
     .   RSX(4),RSY(4),RSZ(4),DF(NIR),MAT(3,3),MATI(3,3),BX,BY,BZ,ALPHA,
     .   LX1,LX2,LX3,LX4,LXMIN
C=======================================================================
C     Moment equilibrium in LOCAL coordinates
C-------------------------------------------------
      RSX(1) = RX(1) - RS(1)                      
      RSY(1) = RY(1) - RS(2)                       
      RSZ(1) = RZ(1) - RS(3)                       
      RSX(2) = RX(2) - RS(1)                       
      RSY(2) = RY(2) - RS(2)                       
      RSZ(2) = RZ(2) - RS(3)                       
      RSX(3) = RX(3) - RS(1)                      
      RSY(3) = RY(3) - RS(2)                      
      RSZ(3) = RZ(3) - RS(3) 
      RSX(4) = RX(4) - RS(1)                      
      RSY(4) = RY(4) - RS(2)                      
      RSZ(4) = RZ(4) - RS(3) 
c
      MX = ZERO
      MY = ZERO
      MZ = ZERO
      DO J=1,NIR
        MX = MX + RSY(J)*FMZ(J) - RSZ(J)*FMY(J)
        MY = MY + RSZ(J)*FMX(J) - RSX(J)*FMZ(J)
        MZ = MZ + RSX(J)*FMY(J) - RSY(J)*FMX(J)
      ENDDO
C
      BX= RSX(1)*H(1)+RSX(2)*H(2)+RSX(3)*H(3)+RSX(4)*H(4)
      BY= RSY(1)*H(1)+RSY(2)*H(2)+RSY(3)*H(3)+RSY(4)*H(4)
      BZ= RSZ(1)*H(1)+RSZ(2)*H(2)+RSZ(3)*H(3)+RSZ(4)*H(4)
C
C-------------------------------------------------
      IF (NIR == 4) THEN
        RXX = RX(2)+RX(3)-RX(1)-RX(4)
        RYY = RY(3)+RY(4)-RY(1)-RY(2)
c        RXX = ABS(RXX)
c        RYY = ABS(RYY)
C---    Moment Z
c
        ALPHA = ABS(RXX)/(ABS(RXX)+ABS(RYY))
C
C       RQ : ALPHA is a coeff used for a optimised repartition of moment MZS on FXX and FYY
C
        MXY = RSX(1)*FMY(1)+RSX(2)*FMY(2)+RSX(3)*FMY(3)+RSX(4)*FMY(4)-ALPHA*MZS
        MYX = RSY(1)*FMX(1)+RSY(2)*FMX(2)+RSY(3)*FMX(3)+RSY(4)*FMX(4)+(ONE-ALPHA)*MZS
        MZZ = MXY - MYX
C
C       RQ : BETAX = BETAY = 1 in standard situation - moment mz must be transferred either only by FXX or FXX if switch to 1D formulation
C       standard situation  -> FXX = -MYX/RYY and FYY = MXY/RXX
C       1D formulation in X -> FXX = 0 and FYY = MZ / RXX
C       1D formulation in Y -> FYY = 0 and FXX = MZ / RYY
C
        FYY = (BETAY*MXY-(ONE-BETAX)*MYX)/RXX         
        FXX = ((ONE-BETAY)*MXY-BETAX*MYX)/RYY      
C
        STIFM=MAX(SQRT((ONE-BETAX)*BY*BY+ BETAY*BX*BX)/ABS(RXX),SQRT(BETAX*BY*BY+ (ONE-BETAY)*BX*BX)/ABS(RYY))
        STIFMR=ONE/(ABS(RXX)+ABS(RYY))  
C
        FMX(1) = FMX(1) - FXX    
        FMX(2) = FMX(2) - FXX    
        FMX(3) = FMX(3) + FXX    
        FMX(4) = FMX(4) + FXX    

        FMY(1) = FMY(1) + FYY    
        FMY(2) = FMY(2) - FYY    
        FMY(3) = FMY(3) - FYY    
        FMY(4) = FMY(4) + FYY    
c
c---    Moments Mx, MY
C
C       RQ : BETAX = BETAY = 1 in standard situation - used to impose that mx=0 or my=0 if switch to 1D formulation
c
        MYZ = RSY(1)*FMZ(1)+RSY(2)*FMZ(2)+RSY(3)*FMZ(3)+RSY(4)*FMZ(4)
        MZY = RSZ(1)*FMY(1)+RSZ(2)*FMY(2)+RSZ(3)*FMY(3)+RSZ(4)*FMY(4)
        MXX = BETAX*(MYZ - MZY - MXS)
        MZX = RSZ(1)*FMX(1)+RSZ(2)*FMX(2)+RSZ(3)*FMX(3)+RSZ(4)*FMX(4)
        MXZ = RSX(1)*FMZ(1)+RSX(2)*FMZ(2)+RSX(3)*FMZ(3)+RSX(4)*FMZ(4)
        MYY = BETAY*(MZX - MXZ - MYS)
c
        RAX = RX(1) - RX(3)   
        RBX = RX(4) - RX(2)   
        RAY = RY(1) - RY(3)   
        RBY = RY(4) - RY(2)   
        D1 = -MXX*RBX - MYY*RBY            
        D2 =  MXX*RAX + MYY*RAY            
        DD =  RAY*RBX - RAX*RBY                   
        FA = D1 / DD                             
        FB = D2 / DD                             
C
        STIFM=MAX(STIFM,
     .        SQRT((BY*BY+BZ*BZ)*RBX*RBX*BETAX+(BZ*BZ+BX*BX)*RBY*RBY*BETAY)/ABS(DD),
     .        SQRT((BY*BY+BZ*BZ)*RAX*RAX*BETAX+(BZ*BZ+BX*BX)*RAY*RAY*BETAY)/ABS(DD))
C
        STIFMR=MAX(STIFMR,(ABS(RBX*BETAX)+ABS(RBY*BETAY))/ABS(DD),(ABS(RAX*BETAX)+ABS(RAY*BETAY))/ABS(DD)) 
C
        FMZ(1) = FMZ(1) + FA                     
        FMZ(2) = FMZ(2) - FB                     
        FMZ(3) = FMZ(3) - FA                     
        FMZ(4) = FMZ(4) + FB                     
c

      ELSEIF (NIR == 3) THEN
        RXX = RX(2)+RX(3)-TWO*RX(1)
        RYY = TWO*RY(3)-RY(1)-RY(2)
C
C       RQ : ALPHA is a coeff used for a optimised repartition of moment MZS on FXX and FYY
C
        ALPHA = ABS(RXX)/(ABS(RXX)+ABS(RYY))
C
C---    Moment Z
C
C
        MXY = RSX(1)*FMY(1)+RSX(2)*FMY(2)+RSX(3)*FMY(3)-ALPHA*MZS
        MYX = RSY(1)*FMX(1)+RSY(2)*FMX(2)+RSY(3)*FMX(3)+(ONE-ALPHA)*MZS
        MZZ = MXY - MYX
C
C       RQ : BETAX = BETAY = 1 in standard situation - moment mz must be transferred either only by FXX or FXX if switch to 1D formulation
C       standard situation  -> FXX = -MYX/RYY and FYY = MXY/RXX
C       1D formulation in X -> FXX = 0 and FYY = MZ / RXX
C       1D formulation in Y -> FYY = 0 and FXX = MZ / RYY
C
        FYY = -(BETAY*MXY-(ONE-BETAX)*MYX)/RXX         
        FXX = ((ONE-BETAY)*MXY-BETAX*MYX)/RYY      
C
        STIFM=MAX(SQRT((ONE-BETAX)*BY*BY+ BETAY*BX*BX)/ABS(RXX),SQRT(BETAX*BY*BY+ (ONE-BETAY)*BX*BX)/ABS(RYY))
        STIFMR=ONE/(ABS(RXX)+ABS(RYY))  
C
        FMX(1) = FMX(1) - FXX   
        FMX(2) = FMX(2) - FXX    
        FMX(3) = FMX(3) + TWO * FXX    

        FMY(1) = FMY(1) - TWO * FYY
        FMY(2) = FMY(2) + FYY    
        FMY(3) = FMY(3) + FYY    
c
c---    Moments Mx, MY
C
C       RQ : BETAX = BETAY = 1 in standard situation - used to impose that mx=0 or my=0 if switch to 1D formulation
c
        MX=BETAX*(MX+(TWO*RSZ(1)-RSZ(2)-RSZ(3))*FYY - MXS)
        MY=BETAY*(MY+(TWO*RSZ(3)-RSZ(1)-RSZ(2))*FXX - MYS)
c
        MAT(1,1) = RSY(1)
        MAT(1,2) = RSY(2)
        MAT(1,3) = RSY(3)
        MAT(2,1) = RSX(1)
        MAT(2,2) = RSX(2)
        MAT(2,3) = RSX(3)
        MAT(3,1) = ONE
        MAT(3,2) = ONE
        MAT(3,3) = ONE
        CALL INV3(MAT,MATI)
        DF(1) = -MATI(1,1)*MX + MATI(1,2)*MY
        DF(2) = -MATI(2,1)*MX + MATI(2,2)*MY
        DF(3) = -MATI(3,1)*MX + MATI(3,2)*MY
        DO J=1,NIR
          FMZ(J) = FMZ(J) + DF(J)
        ENDDO
C
        BX= RSX(1)*H(1)+RSX(2)*H(2)+RSX(3)*H(3)
        BY= RSY(1)*H(1)+RSY(2)*H(2)+RSY(3)*H(3)
        BZ= RSZ(1)*H(1)+RSZ(2)*H(2)+RSZ(3)*H(3)
        STIFM= MAX(STIFM,SQRT(BX*BX+BY*BY+BZ*BZ)*SQRT(MAX(
     .    BETAX*(MATI(1,1)*MATI(1,1)+MATI(2,1)*MATI(2,1)+MATI(3,1)*MATI(3,1)),
     .    BETAY*(MATI(1,2)*MATI(1,2)+MATI(2,2)*MATI(2,2)+MATI(3,2)*MATI(3,2)),
     .    MATI(1,3)*MATI(1,3)+MATI(2,3)*MATI(2,3)+MATI(3,3)*MATI(3,3))))
C
        STIFMR=MAX(STIFMR,ABS(MATI(1,1))+ABS(MATI(1,2)),ABS(MATI(2,1))+ABS(MATI(2,2)),ABS(MATI(3,1))+ABS(MATI(3,2)))
C
C-----
      ENDIF
C-----------
      RETURN
      END 
      
