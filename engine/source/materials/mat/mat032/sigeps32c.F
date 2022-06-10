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
Chd|  SIGEPS32C                     source/materials/mat/mat032/sigeps32c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- calls ---------------
Chd|        M32ELAS                       source/materials/mat/mat032/m32elas.F
Chd|        M32PLAS                       source/materials/mat/mat032/m32plas.F
Chd|====================================================================
      SUBROUTINE SIGEPS32C(
     1                     JFT    ,JLT    ,PM     ,THK    ,OFF    ,
     2                     DIR    ,IPT    ,IMAT   ,NEL    ,DT1C   ,
     3                     GS     ,EPSD   ,THKLYL ,IPLA   ,DPLA   ,
     4                     DEPSXX ,DEPSYY ,DEPSXY ,DEPSYZ ,DEPSZX ,
     5                     SIGOXX ,SIGOYY ,SIGOXY ,SIGOYZ ,SIGOZX ,
     6                     SIGNXX ,SIGNYY ,SIGNXY ,SIGNYZ ,SIGNZX ,
     7                     PLA    ,NGL    ,HARDM  ,INLOC  ,DPLANL )
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
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr06_c.inc"
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "com20_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,IPLA,NEL,IMAT,IPT,INLOC
      INTEGER NGL(*)
C     REAL
      my_real
     .   PM(NPROPM,*),THK(*),OFF(*),DIR(NEL,2),DT1C(*),
     .   GS(*),DPLA(*),EPSD(*),HARDM(*)
      my_real
     .   DEPSXX(MVSIZ),DEPSYY(MVSIZ),DEPSXY(MVSIZ),DEPSYZ(MVSIZ),
     .   DEPSZX(MVSIZ),SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),
     .   SIGOYZ(NEL),SIGOZX(NEL),SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),
     .   SIGNYZ(NEL),SIGNZX(NEL),PLA(NEL),THKLYL(NEL),DPLANL(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real
     .   NU,EZZ(MVSIZ),YOUNG,NORMXX,NORMYY,
     .   A01,A02,A03,YLD,YMAX,CM,CE,CN,CA,
     .   D11,D22,D12,SXX,SYY,SXY,P,Q,S11,S22,
     .   S12,A,B,C,SEQ,DSEQ_DA,DSEQ_DB,DSEQ_DC,
     .   DA_DS11,DA_DS22,DA_DS12,DB_DS11,DB_DS22,
     .   DB_DQ,DC_DQ,DQ_DSXX,DQ_DSYY,DS11_DSXX,
     .   DS11_DSYY,DS22_DSXX,DS22_DSYY,DSXX_DSIGX,
     .   DSXX_DSIGY,DSYY_DSIGX,DSYY_DSIGY,DSXY_DSIGX,
     .   DSXY_DSIGY,A12
C-----------------------------------------------
!
      YOUNG = PM(20,IMAT)
      NU    = PM(21,IMAT)
      A01   = PM(45,IMAT)
      A02   = PM(46,IMAT)
      A03   = PM(47,IMAT)
      A12   = PM(48,IMAT)
      CA    = PM(38,IMAT)
      CE    = PM(39,IMAT)
      CN    = PM(40,IMAT)
      YMAX  = PM(42,IMAT)
      CM    = PM(43,IMAT)
!
      EZZ(1:MVSIZ) = ZERO
C----------------------------
C     CONTRAINTES ELASTIQUES
C----------------------------
        CALL M32ELAS(JFT    ,JLT    ,PM     ,IMAT   ,GS     ,
     .               SIGOXX ,SIGOYY ,SIGOXY ,SIGOYZ ,SIGOZX ,
     .               SIGNXX ,SIGNYY ,SIGNXY ,SIGNYZ ,SIGNZX ,
     .               DEPSXX ,DEPSYY ,DEPSXY ,DEPSYZ ,DEPSZX ,
     .               NEL    )
C------------------------------------------
C     CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C------------------------------------------
        CALL M32PLAS(JFT    ,JLT    ,PM     ,OFF    ,PLA    ,
     .               IMAT   ,DIR    ,EZZ    ,IPLA   ,DT1C   ,
     .               DPLA   ,EPSD   ,NEL    ,NGL    ,HARDM  ,
     .               SIGNXX ,SIGNYY ,SIGNXY ,SIGNYZ ,SIGNZX ,
     .               DEPSXX ,DEPSYY ,DEPSXY ,DEPSYZ ,DEPSZX )
!---------------------------                                              
!     thickness update:

C--------------------------------
C     THICKNESS VARIATION
C--------------------------------
      ! Non-local
      IF (INLOC > 0) THEN 
        DO I = 1,NEL
          D11    = DIR(I,1)*DIR(I,1)
          D22    = DIR(I,2)*DIR(I,2)
          D12    = DIR(I,1)*DIR(I,2)
          SXX    = D11*SIGNXX(I) + D22*SIGNYY(I) + TWO*D12*SIGNXY(I)
          SYY    = D22*SIGNXX(I) + D11*SIGNYY(I) - TWO*D12*SIGNXY(I)
          SXY    = D12*(SIGNYY(I) - SIGNXX(I)) + ( D11 - D22 )*SIGNXY(I)
          P      = -(SXX+SYY) * THIRD
          Q      = (ONE - (NU/(ONE - NU)))*P
          S11    = SXX+Q
          S22    = SYY+Q
          S12    = SXY
          A      = A01*S11*S11 + A02*S22*S22 - A03*S11*S22 + A12*S12*S12
          B      = -Q*(A01*S11 + A02*S22 - HALF*A03*(S11+S22))
          C      = (A01+A02-A03)*Q*Q
          SEQ    = MAX(SQRT(A+B+B+C),EM20)
          DSEQ_DA = ONE/(TWO*SEQ)
          DSEQ_DB = ONE/(SEQ)
          DSEQ_DC = ONE/(TWO*SEQ)
          DA_DS11 = TWO*A01*S11 - A03*S22
          DA_DS22 = TWO*A02*S22 - A03*S11
          DA_DS12 = TWO*A12*S12
          DB_DS11 = (HALF*A03-A01)*Q
          DB_DS22 = (HALF*A03-A02)*Q
          DB_DQ   = -(A01*S11 + A02*S22 - HALF*A03*(S11+S22))
          DC_DQ   = TWO*(A01+A02-A03)*Q
          DQ_DSXX = -THIRD*(ONE - (NU/(ONE - NU)))
          DQ_DSYY = -THIRD*(ONE - (NU/(ONE - NU)))
          DS11_DSXX = ONE + DQ_DSXX
          DS11_DSYY = DQ_DSYY
          DS22_DSXX = DQ_DSXX
          DS22_DSYY = ONE + DQ_DSYY
          DSXX_DSIGX  = D11
          DSXX_DSIGY  = D22
          DSYY_DSIGX  = D22
          DSYY_DSIGY  = D11
          DSXY_DSIGX  = -D12
          DSXY_DSIGY  = D12
          NORMXX = DSEQ_DA*(
     .                 DA_DS11*(DS11_DSXX*DSXX_DSIGX + DS11_DSYY*DSYY_DSIGX) + 
     .                 DA_DS22*(DS22_DSXX*DSXX_DSIGX + DS22_DSYY*DSYY_DSIGX) +
     .                 DA_DS12*DSXY_DSIGX ) + 
     .             DSEQ_DB*(
     .                 DB_DS11*(DS11_DSXX*DSXX_DSIGX + DS11_DSYY*DSYY_DSIGX) + 
     .                 DB_DS22*(DS22_DSXX*DSXX_DSIGX + DS22_DSYY*DSYY_DSIGX) + 
     .                 DB_DQ*(DQ_DSXX*DSXX_DSIGX + DQ_DSYY*DSYY_DSIGX)) +
     .             DSEQ_DC*(
     .                 DC_DQ*(DQ_DSXX*DSXX_DSIGX + DQ_DSYY*DSYY_DSIGX))
          NORMYY = DSEQ_DA*(
     .                 DA_DS11*(DS11_DSXX*DSXX_DSIGY + DS11_DSYY*DSYY_DSIGY) + 
     .                 DA_DS22*(DS22_DSXX*DSXX_DSIGY + DS22_DSYY*DSYY_DSIGY) +
     .                 DA_DS12*DSXY_DSIGY ) + 
     .             DSEQ_DB*(
     .                 DB_DS11*(DS11_DSXX*DSXX_DSIGY + DS11_DSYY*DSYY_DSIGY) + 
     .                 DB_DS22*(DS22_DSXX*DSXX_DSIGY + DS22_DSYY*DSYY_DSIGY) + 
     .                 DB_DQ*(DQ_DSXX*DSXX_DSIGY + DQ_DSYY*DSYY_DSIGY)) +
     .             DSEQ_DC*(
     .                 DC_DQ*(DQ_DSXX*DSXX_DSIGY + DQ_DSYY*DSYY_DSIGY))
          EZZ(I) = MAX(DPLANL(I),ZERO)*(NORMXX + NORMYY)
          EZZ(I) = -NU*((SIGNXX(I)-SIGOXX(I)+SIGNYY(I)-SIGOYY(I))/YOUNG) - EZZ(I)
          THK(I) = THK(I) + EZZ(I)*THKLYL(I)*OFF(I)          
        ENDDO
      ! Local
      ELSE
        DO I=JFT,JLT
          EZZ(I) = -(DEPSXX(I)+DEPSYY(I))*(NU/(ONE-NU)) + EZZ(I)
          THK(I) = THK(I) + EZZ(I)*THKLYL(I)*OFF(I)
        ENDDO
      ENDIF
!-----------
      RETURN
!-----------
      END
