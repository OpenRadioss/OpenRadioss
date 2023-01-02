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
Chd|  SIGEPS15C                     source/materials/mat/mat015/sigeps15c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        M15CPLRC                      source/materials/mat/mat015/m15cplrc.F
Chd|        M15CRAK                       source/materials/mat/mat015/m15crak.F
Chd|====================================================================
      SUBROUTINE SIGEPS15C(
     1                 JFT    ,JLT    ,PM     ,DAMT   ,DIR     ,
     3                 IMAT   ,SHF    ,NGL    ,EPSPL  ,DMG_FLAG,
     7                 ILAYER ,NEL    ,ISRATE ,PLA    ,SIGDMG  ,
     A                 DEPSXX ,DEPSYY ,DEPSXY ,DEPSYZ ,DEPSZX  ,
     7                 SIGOXX ,SIGOYY ,SIGOXY ,SIGOYZ ,SIGOZX  ,
     8                 SIGNXX ,SIGNYY ,SIGNXY ,SIGNYZ ,SIGNZX  ,
     9                 SIGR   ,TSAIWU )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
Cc-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr06_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "com20_c.inc"
#include      "secur_c.inc"
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ILAYER,JFT,JLT,IMAT,NEL,DMG_FLAG,ISRATE
      INTEGER NGL(MVSIZ)
C     REAL
      my_real
     .   PM(NPROPM,*),DIR(NEL,2),SHF(*),EPSPL(*),SIGDMG(MVSIZ,5)
      my_real
     .   DEPSXX(MVSIZ),DEPSYY(MVSIZ),DEPSXY(MVSIZ),DEPSYZ(MVSIZ),
     .   DEPSZX(MVSIZ),PLA(NEL),SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),
     .   SIGOYZ(NEL),SIGOZX(NEL),SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),
     .   SIGNYZ(NEL),SIGNZX(NEL),SIGR(NEL,6),DAMT(NEL,2),TSAIWU(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   SIGE(MVSIZ,5)
C-----------------------------------------------
!---
C-----------------------------------------------------------
C     PLASTICITE AUX POINTS D'INTEGRATION
C-----------------------
C     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C-----------------------
      CALL M15CPLRC(JFT    ,JLT    ,PM     ,PLA    ,DIR   ,
     2              IMAT   ,DAMT   ,SHF    ,NGL    ,SIGE  ,
     3              DEPSXX ,DEPSYY ,DEPSXY ,DEPSYZ ,DEPSZX,
     4              SIGOXX ,SIGOYY ,SIGOXY ,SIGOYZ ,SIGOZX,
     5              SIGNXX ,SIGNYY ,SIGNXY ,SIGNYZ ,SIGNZX,
     6              EPSPL  ,ISRATE ,NEL    ,TSAIWU )
!
      DMG_FLAG = 2
!
      DO I=JFT,JLT
        IF (DAMT(I,1) < ONE) THEN
          SIGDMG(I,1)=SIGR(I,1)*DAMT(I,1)
          SIGDMG(I,2)=SIGR(I,2)*DAMT(I,1)
          SIGDMG(I,3)=SIGR(I,3)*DAMT(I,1)
          SIGDMG(I,4)=SIGR(I,4)*DAMT(I,1)
          SIGDMG(I,5)=SIGR(I,5)*DAMT(I,1)
        ELSEIF (DAMT(I,2) < ONE) THEN
          SIGDMG(I,1)=SIGE(I,1)
          SIGDMG(I,2)=SIGE(I,2)
          SIGDMG(I,3)=SIGE(I,3)
          SIGDMG(I,4)=SIGE(I,4)
          SIGDMG(I,5)=SIGE(I,5)
        ELSE
          SIGDMG(I,1)=SIGE(I,1)
          SIGDMG(I,2)=SIGE(I,2)
          SIGDMG(I,3)=SIGE(I,3)
          SIGDMG(I,4)=SIGE(I,4)
          SIGDMG(I,5)=SIGE(I,5)
        ENDIF
      ENDDO
!
      CALL M15CRAK(JFT   ,JLT   ,PM     ,DAMT ,DIR ,
     .             SIGR  ,IMAT  ,ILAYER ,SIGE ,NGL ,
     .             NEL   )
C---
      RETURN
      END
