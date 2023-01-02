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
Chd|  SIGEPS22C                     source/materials/mat/mat022/sigeps22c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        M22CPLR                       source/materials/mat/mat022/m22cplr.F
Chd|====================================================================
      SUBROUTINE SIGEPS22C(
     1                   JFT      ,JLT    ,PM       ,THK      ,OFF     ,
     2                   SIGY     ,DT1C   ,IPLA     ,NEL      ,OFF_OLD ,
     3                   GS       ,DPLA   ,IOFF_DUCT,NPTT     ,IPT     ,
     4                   EPCHK    ,ALPE   ,THKLYL   ,IMAT     ,PLA     ,
     5                   DEPSXX   ,DEPSYY ,DEPSXY   ,DEPSYZ   ,DEPSZX  ,
     6                   SIGOXX   ,SIGOYY ,SIGOXY   ,SIGOYZ   ,SIGOZX  ,
     7                   SIGNXX   ,SIGNYY ,SIGNXY   ,SIGNYZ   ,SIGNZX  ,
     8                   INLOC    ,DPLANL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,IPLA,NEL,NPT,NPTT,IMAT,IPT
      INTEGER IOFF_DUCT(MVSIZ),INLOC
C     REAL
      my_real
     .   PM(NPROPM,*),THK(*),OFF(*),OFF_OLD(*),SIGY(*),DT1C(*),GS(*)
      my_real
     .   DPLA(*),PLA(NEL),
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),SIGOYZ(NEL),
     .   SIGOZX(NEL),SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),
     .   SIGNYZ(NEL),SIGNZX(NEL),DEPSXX(MVSIZ),DEPSYY(MVSIZ),
     .   DEPSXY(MVSIZ),DEPSYZ(MVSIZ),DEPSZX(MVSIZ),EPCHK(MVSIZ),
     .   ALPE(MVSIZ),THKLYL(NEL),DPLANL(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ICC(MVSIZ),I,ILAYER,ICC_1,J
C     REAL
      my_real
     .   A1(MVSIZ),YLD(MVSIZ),
     .   CA(MVSIZ),CB(MVSIZ),CN(MVSIZ),YMAX(MVSIZ),
     .   YOUNG(MVSIZ),CC(MVSIZ),EPDR(MVSIZ),NU(MVSIZ),
     .   EPSL(MVSIZ),HL(MVSIZ),YM(MVSIZ),YLDL(MVSIZ),EZZ(MVSIZ),
     .   YM_1, A1_1, CA_1, CB_1, CN_1, 
     .   EPMX, YMAX_1, CC_1, EPSL_1, 
     .   HL_1, YLDL_1,NU_1
C-----------------------------------------------
!
      NU_1   = PM(21,IMAT)
      YM_1   = PM(20,IMAT)
      A1_1   = PM(24,IMAT)
      CA_1   = PM(38,IMAT)
      CB_1   = PM(39,IMAT)
      CN_1   = PM(40,IMAT)
      EPMX   = PM(41,IMAT)
      YMAX_1 = PM(42,IMAT)
      CC_1   = PM(43,IMAT)
      EPSL_1 = PM(45,IMAT)
      HL_1   = PM(46,IMAT)
      YLDL_1 = PM(47,IMAT)
      ICC_1  = NINT(PM(49,IMAT))
!
      DO I=JFT,JLT
        YM(I)   = YM_1
        A1(I)   = A1_1
        CA(I)   = CA_1
        CB(I)   = CB_1
        CN(I)   = CN_1
        YMAX(I) = YMAX_1
        CC(I)   = CC_1
        EPDR(I) = MAX(EM20,PM(44,IMAT)*DT1C(I))
        EPSL(I) = EPSL_1
        HL(I)   = HL_1
        YLDL(I) = YLDL_1
        ICC(I)  = ICC_1
        NU(I) = NU_1
        EZZ(I) = ZERO
      ENDDO
C-----------------------------------------------------------
C     PLASTICITE AUX POINTS D'INTEGRATION
C-----------------------------------------------------------
C     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C-----------------------------------------------------------
        CALL M22CPLR(JFT    ,JLT    ,EZZ    ,OFF    ,PLA   ,
     2               IPLA   ,GS     ,YLD    ,A1     ,CA    ,
     3               CB     ,CN     ,YMAX   ,NU     ,DPLA  ,
     4               EPCHK  ,YOUNG  ,CC     ,EPDR   ,EPSL  ,
     5               HL     ,YM     ,YLDL   ,ALPE   ,ICC   ,
     6               DEPSXX ,DEPSYY ,DEPSXY ,DEPSYZ ,DEPSZX,
     7               SIGOXX ,SIGOYY ,SIGOXY ,SIGOYZ ,SIGOZX,
     8               SIGNXX ,SIGNYY ,SIGNXY ,SIGNYZ ,SIGNZX,
     9               NEL    )
!
        IF (IPT == 1+NPTT/2) THEN
          DO I=JFT,JLT
            SIGY(I)=YLD(I)
          ENDDO
        ENDIF
C----------------------------
C     TEST DE RUPTURE DUCTILE
C----------------------------
        DO I=JFT,JLT
          IF (OFF(I) == OFF_OLD(I) .and. OFF(I) > ZERO) THEN
            IF (OFF(I) == ONE .and. EPCHK(I) >= EPMX) THEN
              OFF(I)= FOUR_OVER_5
              IOFF_DUCT(I) = 1    ! start de la rupture
            ELSE IF (OFF(I) < ONE ) THEN
              OFF(I) = OFF(I)*FOUR_OVER_5
            ENDIF
          ENDIF
        ENDDO
!---------------------------
!     thickness update:
!---------------------------
      DO I=JFT,JLT
        IF (INLOC > 0) THEN
          EZZ(I) = -NU(I)*(SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/YOUNG(I)
          EZZ(I) = EZZ(I) - MAX(DPLANL(I),ZERO)*HALF*(SIGNXX(I)+SIGNYY(I))/YLD(I)
        ELSE
          EZZ(I) = -(DEPSXX(I)+DEPSYY(I))*NU(I)-(ONE - TWO*NU(I))*EZZ(I)
          EZZ(I) = EZZ(I)/(ONE-NU(I))
        ENDIF
        THK(I) = THK(I) + EZZ(I) * THKLYL(I)*OFF(I)
      ENDDO
c-----------
      RETURN
      END
