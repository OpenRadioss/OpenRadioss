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
	SUBROUTINE SHELL_COMPUTATION(JFT ,JLT ,IPLA  ,NEL,
     .                               JTHE,NPTTOT,IMAT,ISRATE,
     .                               PM  ,EINT,OFF   ,SIGY,DT1C,
     .                               VOL0,GS  ,DPLA  ,PLA,FOR,
     .                               SIGO,SIGN,DEP   ,MLW,NGL)

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"

!	------------------------------------
        INTEGER, INTENT(IN) :: JFT,JLT,IPLA,NEL,JTHE
        INTEGER, INTENT(IN) :: NPTTOT,IMAT,ISRATE,MLW
        INTEGER, DIMENSION(*), INTENT(IN) :: NGL
!---
C     REAL ou REAL*8
        my_real
     .   PM(PMSIZE,*),EINT(NEL,2),
     .   OFF(*),SIGY(*),DT1C(*),VOL0(*),GS(*),
     .   DPLA(*),
     .   PLA(NEL)
!
        my_real, DIMENSION(NEL,5) :: SIGO,SIGN,DEP,FOR
!	------------------------------------
        INTEGER :: I
        my_real, DIMENSION(MVSIZ) :: EPCHK,DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX
        my_real, DIMENSION(MVSIZ) :: OFF_OLD,EPSPDT,HARDM
!	------------------------------------

        EPCHK(1:MVSIZ) = 1.d+00
        DO I=JFT,JLT
                DEPSXX(I)=DEP(I,1)
                DEPSYY(I)=DEP(I,2)
                DEPSXY(I)=DEP(I,3)
                DEPSYZ(I)=DEP(I,4)
                DEPSZX(I)=DEP(I,5)
                OFF_OLD(I) = OFF(I)
                EPSPDT(I) = I/MVSIZ
                HARDM(I) = EPSPDT(I)
        ENDDO
        IF(MLW==1) THEN
                CALL LAW1_COMPUTATION(
     1                          JFT       ,JLT      ,PM       ,EINT     ,
     2                          OFF       ,SIGY     ,DT1C     ,IPLA     ,NEL      ,
     3                          VOL0     ,GS        ,NGL      ,
     4                          EPSPDT    ,
     5                          DPLA      ,JTHE     ,HARDM    ,EPCHK    ,
     6                          IMAT      ,NPTTOT   ,PLA      ,
     7                          SIGO(1,1) ,SIGO(1,2),SIGO(1,3),SIGO(1,4),SIGO(1,5)   ,
     8                          SIGN(1,1) ,SIGN(1,2),SIGN(1,3),SIGN(1,4),SIGN(1,5)   ,
     9                          DEPSXX   ,DEPSYY   ,DEPSXY   ,DEPSYZ   ,
     A                          DEPSZX    ,
     B                          OFF_OLD   ,ISRATE   )

        ELSEIF(MLW==2) THEN
                CALL LAW2_COMPUTATION(
     1                          JFT       ,JLT      ,PM       ,EINT     ,
     2                          OFF       ,SIGY     ,DT1C     ,IPLA     ,NEL      ,
     3                          VOL0     ,GS        ,NGL      ,
     4                          EPSPDT    ,
     5                          DPLA      ,JTHE     ,HARDM    ,EPCHK    ,
     6                          IMAT      ,NPTTOT   ,PLA      ,
     7                          SIGO(1,1) ,SIGO(1,2),SIGO(1,3),SIGO(1,4),SIGO(1,5)   ,
     8                          SIGN(1,1) ,SIGN(1,2),SIGN(1,3),SIGN(1,4),SIGN(1,5)   ,
     9                          DEPSXX   ,DEPSYY   ,DEPSXY   ,DEPSYZ   ,
     A                          DEPSZX    ,
     B                          OFF_OLD   ,ISRATE   )

        ENDIF

        CALL UPDATE_FOR(JFT  ,JLT   ,NEL   ,SIGO   ,FOR , OFF)


	RETURN
!	------------------------------------
	END SUBROUTINE SHELL_COMPUTATION
!	------------------------------------
