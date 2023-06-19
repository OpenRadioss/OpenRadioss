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
Chd|  I2FORCES                      source/interfaces/interf/i2forces.F
Chd|-- called by -----------
Chd|        I2CURVF                       source/interfaces/interf/i2curvf.F
Chd|        I2CURVFP                      source/interfaces/interf/i2curvfp.F
Chd|        I2FOMO3                       source/interfaces/interf/i2for3.F
Chd|        I2FOMO3P                      source/interfaces/interf/i2for3p.F
Chd|        I2FOR27P_CIN                  source/interfaces/interf/i2for27p_cin.F
Chd|        I2FOR27_CIN                   source/interfaces/interf/i2for27_cin.F
Chd|        I2FOR28P_CIN                  source/interfaces/interf/i2for28p_cin.F
Chd|        I2FOR28_CIN                   source/interfaces/interf/i2for28_cin.F
Chd|        I2FOR3                        source/interfaces/interf/i2for3.F
Chd|        I2FOR3N                       source/interfaces/interf/i2for3.F
Chd|        I2FOR3O                       source/interfaces/interf/i2for3.F
Chd|        I2FOR3P                       source/interfaces/interf/i2for3p.F
Chd|        I2FOR3PN                      source/interfaces/interf/i2for3p.F
Chd|        I2FOR3PO                      source/interfaces/interf/i2for3p.F
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE I2FORCES_2D(X      ,V       ,A       ,MS      ,NSL     ,
     .                       IRECT  ,H       ,NIR     ,FSAV    ,FNCONT  ,
     .                       FNCONTP,FTCONTP ,WEIGHT  ,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD 
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER , INTENT(IN) :: NSL,IRECT(4),NIR,WEIGHT(NUMNOD)
      my_real , INTENT(IN) :: X(3,NUMNOD),V(3,NUMNOD),A(3,NUMNOD),MS(NUMNOD),
     .                        H(4)
      my_real , INTENT(INOUT) :: FSAV(6),FNCONT(3,NUMNOD),FNCONTP(3,NUMNOD),
     .                           FTCONTP(3,NUMNOD)
      TYPE(H3D_DATABASE),INTENT(IN) :: H3D_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr14_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N1,N2,N3,N4,J,JJ
C     REAL
      my_real
     .   XC0,YC0,ZC0,XC,YC,ZC,X0,X1,X2,Y0,Y1,Y2,
     .   Z0,Z1,Z2,XCDG,YCDG,ZCDG,MCDG,
     .   SUM,FX,FY,FZ,FNORM,VX,VY,VZ,DT12M,FN(3),FT(3)
C=======================================================================
      IF (TT == ZERO) THEN   
        DT12M = ONE/DT2
      ELSE
        DT12M = ONE/DT12
      ENDIF
C
      N1  = IRECT(1)
      N2  = IRECT(2)
C
      X0  = X(1,NSL)
      Y0  = X(2,NSL)
      Z0  = X(3,NSL)
      X1  = X(1,N1)
      Y1  = X(2,N1)
      Z1  = X(3,N1)
      X2  = X(1,N2)
      Y2  = X(2,N2)
      Z2  = X(3,N2)
C------------------------------------------------                  
C     direction of segment
      VX = X1 - X2
      VY = Y1 - Y2
      VZ = Z1 - Z2
C------------------------------------------------
      FX = A(1,NSL)
      FY = A(2,NSL)
      FZ = A(3,NSL)
      SUM = ONE / SQRT(VX*VX + VY*VY + VZ*VZ)
      VX = VX * SUM
      VY = VY * SUM
      VZ = VZ * SUM
C   
C     composantes N/T de la forces nodale
C
      FNORM = VX*FX + VY*FY + VZ*FZ
      FT(1) = VX*FNORM
      FT(2) = VY*FNORM
      FT(3) = VZ*FNORM
C
      FN(1) = FX - FT(1)
      FN(2) = FY - FT(2)
      FN(3) = FZ - FT(3)
C
C-------- print of forces in TH
C
      FSAV(1) = FSAV(1) + FN(1)*DT1*WEIGHT(NSL)
      FSAV(2) = FSAV(2) + FN(2)*DT1*WEIGHT(NSL)
      FSAV(3) = FSAV(3) + FN(3)*DT1*WEIGHT(NSL)
      FSAV(4) = FSAV(4) + FT(1)*DT1*WEIGHT(NSL)
      FSAV(5) = FSAV(5) + FT(2)*DT1*WEIGHT(NSL)
      FSAV(6) = FSAV(6) + FT(3)*DT1*WEIGHT(NSL)
C
C-------- print of forces in ANIM / H3D
C 
      IF(ANIM_V(13)+H3D_DATA%N_VECT_CONT2>0) THEN
        FNCONT(1,NSL) = (-FN(1)-FT(1)) * WEIGHT(NSL)
        FNCONT(2,NSL) = (-FN(2)-FT(2)) * WEIGHT(NSL)
        FNCONT(3,NSL) = (-FN(3)-FT(3)) * WEIGHT(NSL)
        DO JJ=1,NIR
          J=IRECT(JJ)
          FNCONT(1,J) = FNCONT(1,J) - FNCONT(1,NSL)*H(JJ)
          FNCONT(2,J) = FNCONT(2,J) - FNCONT(2,NSL)*H(JJ)
          FNCONT(3,J) = FNCONT(3,J) - FNCONT(3,NSL)*H(JJ)
        ENDDO
      ENDIF  
C
      IF(ANIM_V(27)+H3D_DATA%N_VECT_PCONT2>0) THEN ! Normal/Tangential forces output
        FNCONTP(1,NSL) = -FN(1) * WEIGHT(NSL)
        FNCONTP(2,NSL) = -FN(2) * WEIGHT(NSL)
        FNCONTP(3,NSL) = -FN(3) * WEIGHT(NSL)
        DO JJ=1,NIR
          J=IRECT(JJ)
          FNCONTP(1,J) = FNCONTP(1,J) - FNCONTP(1,NSL)*H(JJ)
          FNCONTP(2,J) = FNCONTP(2,J) - FNCONTP(2,NSL)*H(JJ)
          FNCONTP(3,J) = FNCONTP(3,J) - FNCONTP(3,NSL)*H(JJ)
        ENDDO

        FTCONTP(1,NSL) = -FT(1) * WEIGHT(NSL)
        FTCONTP(2,NSL) = -FT(2) * WEIGHT(NSL)
        FTCONTP(3,NSL) = -FT(3) * WEIGHT(NSL)
        DO JJ=1,NIR
          J=IRECT(JJ)
          FTCONTP(1,J) = FTCONTP(1,J) - FTCONTP(1,NSL)*H(JJ)
          FTCONTP(2,J) = FTCONTP(2,J) - FTCONTP(2,NSL)*H(JJ)
          FTCONTP(3,J) = FTCONTP(3,J) - FTCONTP(3,NSL)*H(JJ)
        ENDDO
      ENDIF
C---
      RETURN
      END
