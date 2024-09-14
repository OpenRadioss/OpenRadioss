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
      !||====================================================================
      !||    i2forces       ../engine/source/interfaces/interf/i2forces.F
      !||--- called by ------------------------------------------------------
      !||    i2curvf        ../engine/source/interfaces/interf/i2curvf.F
      !||    i2curvfp       ../engine/source/interfaces/interf/i2curvfp.F
      !||    i2fomo3        ../engine/source/interfaces/interf/i2for3.F
      !||    i2fomo3p       ../engine/source/interfaces/interf/i2for3p.F
      !||    i2for25        ../engine/source/interfaces/interf/i2for25.F
      !||    i2for25p       ../engine/source/interfaces/interf/i2for25p.F
      !||    i2for27_cin    ../engine/source/interfaces/interf/i2for27_cin.F
      !||    i2for27_pen    ../engine/source/interfaces/interf/i2for27_pen.F
      !||    i2for27p_cin   ../engine/source/interfaces/interf/i2for27p_cin.F
      !||    i2for27p_pen   ../engine/source/interfaces/interf/i2for27p_pen.F
      !||    i2for28_cin    ../engine/source/interfaces/interf/i2for28_cin.F
      !||    i2for28_pen    ../engine/source/interfaces/interf/i2for28_pen.F
      !||    i2for28p_cin   ../engine/source/interfaces/interf/i2for28p_cin.F
      !||    i2for28p_pen   ../engine/source/interfaces/interf/i2for28p_pen.F
      !||    i2for3         ../engine/source/interfaces/interf/i2for3.F
      !||    i2for3n        ../engine/source/interfaces/interf/i2for3.F
      !||    i2for3o        ../engine/source/interfaces/interf/i2for3.F
      !||    i2for3p        ../engine/source/interfaces/interf/i2for3p.F
      !||    i2for3pn       ../engine/source/interfaces/interf/i2for3p.F
      !||    i2for3po       ../engine/source/interfaces/interf/i2for3p.F
      !||--- uses       -----------------------------------------------------
      !||    h3d_mod        ../engine/share/modules/h3d_mod.F
      !||====================================================================
      SUBROUTINE I2FORCES(X      ,FS      ,FX      ,FY      ,FZ      ,
     .                    IRECT  ,NIR     ,FSAV    ,FNCONT  ,FNCONTP ,
     .                    FTCONTP,WEIGHT  ,H3D_DATA,NSL     ,H)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD 
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL,IRECT(4),NIR,WEIGHT(*)
C     REAL
      my_real
     .   X(3,*),H(4),FSAV(*),FNCONT(3,*),FNCONTP(3,*),FTCONTP(3,*)
      my_real  , INTENT(IN)    :: FS(3),FX(4),FY(4),FZ(4)
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "scr14_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N1,N2,N3,N4,J,JJ
C     REAL
      my_real
     .   XC0,YC0,ZC0,XC,YC,ZC,X0,X1,X2,X3,X4,Y0,Y1,Y2,Y3,Y4,
     .   Z0,Z1,Z2,Z3,Z4,SX,SY,SZ,TX,TY,TZ,XCDG,YCDG,ZCDG,MCDG,
     .   SUM,FNORM,VNX,VNY,VNZ,VX,VY,VZ,DT12M,FN(3),FT(3)
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
        N3  = IRECT(3)
        N4  = IRECT(4)
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
        X3  = X(1,N3)
        Y3  = X(2,N3)
        Z3  = X(3,N3)
        X4  = X(1,N4)
        Y4  = X(2,N4)
        Z4  = X(3,N4)
C------------------------------------------------                  
C       directions normale/tangente facette main
        SX = X2 + X3 - X1 - X4
        SY = Y2 + Y3 - Y1 - Y4
        SZ = Z2 + Z3 - Z1 - Z4
        TX = X3 + X4 - X1 - X2
        TY = Y3 + Y4 - Y1 - Y2
        TZ = Z3 + Z4 - Z1 - Z2
C------------------------------------------------
        VNX = SY * TZ - SZ * TY
        VNY = SZ * TX - SX * TZ
        VNZ = SX * TY - SY * TX
        SUM = ONE / SQRT(VNX*VNX + VNY*VNY + VNZ*VNZ)
        VNX = VNX * SUM
        VNY = VNY * SUM
        VNZ = VNZ * SUM
C   
C       composantes N/T de la forces nodale
C
        FNORM = VNX*FS(1) + VNY*FS(2) + VNZ*FS(3)
        FN(1) = VNX*FNORM
        FN(2) = VNY*FNORM
        FN(3) = VNZ*FNORM
C
        FT(1) = FS(1) - FN(1)
        FT(2) = FS(2) - FN(2)
        FT(3) = FS(3) - FN(3)
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

          FTCONTP(1,NSL) = VNX
          FTCONTP(2,NSL) = VNY
          FTCONTP(3,NSL) = VNZ
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
