Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    i2forces_2d   ../engine/source/interfaces/interf/i2forces_2D.F
      !||--- called by ------------------------------------------------------
      !||    i2for3n       ../engine/source/interfaces/interf/i2for3.F
      !||    i2for3pn      ../engine/source/interfaces/interf/i2for3p.F
      !||--- uses       -----------------------------------------------------
      !||    h3d_mod       ../engine/share/modules/h3d_mod.F
      !||====================================================================
      SUBROUTINE I2FORCES_2D(X      ,FS      ,FX      ,FY      ,FZ      ,
     .                       IRECT  ,NIR     ,FSAV    ,FNCONT  ,FNCONTP ,
     .                       FTCONTP,WEIGHT  ,H3D_DATA,NSL     ,H)
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
      my_real , INTENT(IN) :: X(3,NUMNOD),H(4)
      my_real , INTENT(INOUT) :: FSAV(6),FNCONT(3,NUMNOD),FNCONTP(3,NUMNOD),
     .                           FTCONTP(3,NUMNOD)
      my_real , INTENT(IN)    :: FS(3),FX(4),FY(4),FZ(4)
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
     .   SUM,FNORM,VX,VY,VZ,DT12M,FN(3),FT(3)
C=======================================================================
      IF (TT == ZERO) THEN   
        DT12M = ONE/DT2
      ELSE
        DT12M = ONE/DT12
      ENDIF
C
      IF (WEIGHT(NSL)==1) THEN
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
C       direction of segment
        VX = X1 - X2
        VY = Y1 - Y2
        VZ = Z1 - Z2
C------------------------------------------------
        SUM = ONE / SQRT(VX*VX + VY*VY + VZ*VZ)
        VX = VX * SUM
        VY = VY * SUM
        VZ = VZ * SUM
C   
C       composantes N/T de la forces nodale
C
        FNORM = VX*FS(1) + VY*FS(2) + VZ*FS(3)
        FT(1) = VX*FNORM
        FT(2) = VY*FNORM
        FT(3) = VZ*FNORM
C
        FN(1) = FS(1) - FT(1)
        FN(2) = FS(2) - FT(2)
        FN(3) = FS(3) - FT(3)
C
C-------- print of forces in TH
C
        FSAV(1) = FSAV(1) + FN(1)*DT1
        FSAV(2) = FSAV(2) + FN(2)*DT1
        FSAV(3) = FSAV(3) + FN(3)*DT1
        FSAV(4) = FSAV(4) + FT(1)*DT1
        FSAV(5) = FSAV(5) + FT(2)*DT1
        FSAV(6) = FSAV(6) + FT(3)*DT1
C
C-------- print of forces in ANIM / H3D
C 
        IF(ANIM_V(13)+H3D_DATA%N_VECT_CONT2>0) THEN
          FNCONT(1,NSL) = FNCONT(1,NSL) - FS(1)
          FNCONT(2,NSL) = FNCONT(2,NSL) - FS(2)
          FNCONT(3,NSL) = FNCONT(3,NSL) - FS(3)
          DO JJ=1,NIR
            J=IRECT(JJ)
            FNCONT(1,J) = FNCONT(1,J) + FX(JJ)
            FNCONT(2,J) = FNCONT(2,J) + FY(JJ)
            FNCONT(3,J) = FNCONT(3,J) + FZ(JJ)
          ENDDO
        ENDIF  
C
        IF(ANIM_V(27)+H3D_DATA%N_VECT_PCONT2>0) THEN ! Normal/Tangential forces output
          FNCONTP(1,NSL) = FNCONTP(1,NSL) - FS(1)
          FNCONTP(2,NSL) = FNCONTP(2,NSL) - FS(2)
          FNCONTP(3,NSL) = FNCONTP(3,NSL) - FS(3)
          DO JJ=1,NIR
            J=IRECT(JJ)
            FNCONTP(1,J) = FNCONTP(1,J) + FX(JJ)
            FNCONTP(2,J) = FNCONTP(2,J) + FY(JJ)
            FNCONTP(3,J) = FNCONTP(3,J) + FZ(JJ)
          ENDDO

          FTCONTP(1,NSL) = VX
          FTCONTP(2,NSL) = VY
          FTCONTP(3,NSL) = VZ
          DO JJ=1,NIR
            J=IRECT(JJ)
            FTCONTP(1,J) = FTCONTP(1,J) - FTCONTP(1,NSL)*H(JJ)
            FTCONTP(2,J) = FTCONTP(2,J) - FTCONTP(2,NSL)*H(JJ)
            FTCONTP(3,J) = FTCONTP(3,J) - FTCONTP(3,NSL)*H(JJ)
          ENDDO
        ENDIF
C
      ENDIF
C---
      RETURN
      END
