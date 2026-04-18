Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!	------------------------------------
	subroutine law1_computation(
     1                  JFT       ,JLT      ,PM       ,EINT     ,
     2                  OFF       ,SIGY     ,DT1C     ,IPLA     ,NEL      ,
     3                  VOL0     ,GS       ,NGL       ,
     4                  EPSPDT   ,
     5                  DPLA      ,JTHE     ,HARDM    ,EPCHK    ,
     6                  IMAT      ,NPTTOT   ,PLA      ,
     7                  SIGOXX    ,SIGOYY   ,SIGOXY   ,SIGOYZ   ,SIGOZX   ,
     8                  SIGNXX    ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     9                  DEPSXX   ,DEPSYY   ,DEPSXY   ,DEPSYZ   ,
     A                  DEPSZX    ,
     B                  OFF_OLD   ,ISRATE   )
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
#include      "comlock.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER JFT,JLT,IPLA,NEL,JTHE,NPTTOT,IMAT,ISRATE
        INTEGER, DIMENSION(*) :: NGL
!---
C     REAL ou REAL*8
        my_real
     .   PM(PMSIZE,*),EINT(NEL,2),
     .   OFF(*),SIGY(*),DT1C(*),VOL0(*),GS(*),
     .   DPLA(*),
     .   DEPSXX(MVSIZ),DEPSYY(MVSIZ),DEPSXY(MVSIZ),DEPSYZ(MVSIZ),
     .   DEPSZX(MVSIZ),PLA(NEL),
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   OFF_OLD(MVSIZ)
!
        my_real
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),
     .   SIGNYZ(NEL),SIGNZX(NEL),EPCHK(MVSIZ)
!
        my_real
     .   HARDM(MVSIZ),EPSPDT(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER ICC,IRTY,I,J,NINDX,NPIF
        INTEGER INDX(MVSIZ)
C     REAL ou REAL*8
        my_real
     .   EPMX,CA,CB,CN,YMAX(MVSIZ),CP,CC,EPDR(MVSIZ),YLD(MVSIZ),
     .   T(MVSIZ),Z3,Z4,FISOKIN,EPIF,YOUNG,TI,TM,G,A11,A12,NU,A1,A2
        my_real, DIMENSION(MVSIZ) :: TSTAR,ETSE
C-----------------------------------------------
        EPIF = ZERO
        NPIF = 0
!---
        YOUNG  = PM(1,IMAT)
        NU     = PM(2,IMAT)
        G      = PM(3,IMAT)
        A11    = PM(4,IMAT)
        A12    = PM(5,IMAT)
        CA     = PM(6,IMAT)
        CB     = PM(7,IMAT)
        CN     = PM(8,IMAT)
        EPMX   = PM(9,IMAT)
        YMAX   = PM(10,IMAT)
        CC     = PM(11,IMAT)
        ICC    = NINT(PM(12,IMAT))
        IRTY   = NINT(PM(13,IMAT))
        Z3     = PM(14,IMAT)
        Z4     = PM(15,IMAT)
        CP     = PM(16,IMAT)
        TI     = PM(17,IMAT)
        FISOKIN= PM(18,IMAT)
!
      	NU = PM(21,IMAT)
      	G  = PM(22,IMAT)
      	A1 = PM(24,IMAT)
      	A2 = PM(25,IMAT)
!
      DO I=JFT,JLT
        SIGNXX(I)=SIGOXX(I)
        SIGNYY(I)=SIGOYY(I)
        SIGNXY(I)=SIGOXY(I)
        SIGNYZ(I)=SIGOYZ(I)
        SIGNZX(I)=SIGOZX(I)
      ENDDO

!---
!---------------------------
!     CONTRAINTES ELASTIQUES
!---------------------------
        DO I=JFT,JLT
          SIGNXX(I) = SIGNXX(I)+A1*DEPSXX(I)+A2*DEPSYY(I)
          SIGNYY(I) = SIGNYY(I)+A2*DEPSXX(I)+A1*DEPSYY(I)
          SIGNXY(I) = SIGNXY(I)+ G*DEPSXY(I)
          SIGNYZ(I) = SIGNYZ(I)+GS(I)*DEPSYZ(I)
          SIGNZX(I) = SIGNZX(I)+GS(I)*DEPSZX(I)
        ENDDO
!---
      DO I=JFT,JLT
        PLA(I) = -NU * (DEPSXX(I) + DEPSYY(I)) / (UN-NU)
        DPLA(I) = DPLA(I) + PLA(I) *OFF(I)
      ENDDO
!---
!-----------!	------------------------------------
	end subroutine law1_computation
!	------------------------------------
