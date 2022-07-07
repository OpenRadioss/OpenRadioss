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
Chd|  CDLEN3                        source/elements/shell/coque/cdlen3.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CDLEN3(JFT,JLT ,PM  ,OFF,AREA, 
     2                  X2 ,X3  ,X4  ,Y2 ,Y3  ,  
     3                  Y4 ,ALDT,MAT,GEO ,PID ,
     4                  IHBE)
C  _clean     3                  Y4 ,ALDT,MAT,GEO ,PID)
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
C  _clean#include      "vect04_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr02_c.inc"
#include      "scr06_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(MVSIZ),JFT, JLT,PID(*),
     .                  IHBE
C     REAL
      my_real
     .   PM(NPROPM,*), OFF(*), GEO(NPROPG,*)
      my_real
     .   X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .     Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .     AREA(MVSIZ), ALDT(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, MX,IPID
C     REAL
      my_real
     .     AL1(MVSIZ), AL2(MVSIZ), AL3(MVSIZ),
     .     AL4(MVSIZ), AL5(MVSIZ), AL6(MVSIZ),
     .     ALMIN(MVSIZ),
     .     ALQUAD, DTDYN, H1, H2, DTHOUR
C-----------------------------------------------
      DO I=JFT,JLT
        AL1(I)= X2(I)       * X2(I)       + Y2(I)       * Y2(I)
        AL2(I)=(X3(I)-X2(I))*(X3(I)-X2(I))+(Y3(I)-Y2(I))*(Y3(I)-Y2(I))
        AL6(I)= X3(I)       * X3(I)       + Y3(I)       * Y3(I)
        AL3(I)=(X4(I)-X3(I))*(X4(I)-X3(I))+(Y4(I)-Y3(I))*(Y4(I)-Y3(I))
        AL4(I)= X4(I)       * X4(I)       + Y4(I)       * Y4(I)
        AL5(I)=(X4(I)-X2(I))*(X4(I)-X2(I))+(Y4(I)-Y2(I))*(Y4(I)-Y2(I))
      ENDDO
C
      DO I=JFT,JLT
        ALMIN(I)= MIN(AL1(I),AL2(I),AL4(I))
        ALQUAD  = MIN(AL3(I),AL5(I),AL6(I))
        IF(AL3(I).NE.ZERO) ALMIN(I)= MIN(ALMIN(I),ALQUAD)
        DTDYN= AREA(I)*AREA(I) / MAX(AL5(I),AL6(I),EM20)
        ALDT(I)= MAX( DTDYN , ALMIN(I) )
      ENDDO
C---------------------------------------------------------
C     DT COMPATIBLE AVEC FONCTIONS DE FORMES SUR HOURGLASS
C---------------------------------------------------------
        IF(IHBE.NE.0)THEN
            MX = MAT(JFT)
            DO  I=JFT,JLT
                IF(INVSTR.GE.35)THEN
                    IPID=PID(I)
                    H1 =GEO(13,IPID)
                    H2 =GEO(14,IPID)
                ELSE
                    H1 =PM( 5,MX)
                    H2 =PM( 6,MX)
                ENDIF
            DTHOUR = HALF * (ALMIN(I)+ALDT(I)) / MAX(H1,H2)
C Bug HP            ALDT(I)  = MIN(ALDT(I),DTHOUR)
            IF (DTHOUR.LT.ALDT(I)) ALDT(I)=DTHOUR
          ENDDO
        ELSE
          MX = MAT(JFT)
          DO  I=JFT,JLT
          IF(INVSTR.GE.35)THEN
           IPID=PID(I)
           H1 =GEO(13,IPID)
           H2 =GEO(14,IPID)
          ELSE
           H1 =PM( 5,MX)
           H2 =PM( 6,MX)
          ENDIF
            DTHOUR = HALF * (ALMIN(I)+ALDT(I)) / MAX(H1,H2)
            ALDT(I)  = MIN(ALDT(I),DTHOUR)
          ENDDO
        ENDIF
C---------------------------------------------------------
      DO I=JFT,JLT
        ALDT(I)= SQRT( ALDT(I) )
      ENDDO
C
      RETURN
      END
