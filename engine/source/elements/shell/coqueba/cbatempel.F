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
!||    cbatempel   ../engine/source/elements/shell/coqueba/cbatempel.F
!||--- called by ------------------------------------------------------
!||    cbaforc3    ../engine/source/elements/shell/coqueba/cbaforc3.F
!||====================================================================
      SUBROUTINE CBATEMPEL(JFT ,JLT   ,NG  ,IXC   ,TEMP ,
     3                  TEMPEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,NG,IXC(NIXC,*)
      my_real TEMP(*),TEMPEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,KSI ,ETA ,SF1,SF2 ,SF3 ,SF4
      MY_REAL 
     .   VPG(2,4),PG1,PG
       PARAMETER (PG=.577350269189626)
       PARAMETER (PG1=-.577350269189626)
C--------------------------
C     INITIALISATION
C-------------------------
      DATA VPG/PG1,PG1,PG,PG1,PG,PG,PG1,PG/
C-----------------------------------------------
C --- Temperature in Gauss point NG
       KSI = VPG(1,NG)
       ETA = VPG(2,NG)
 
       SF1 = (1-KSI)*(1-ETA)
       SF2 = (1+KSI)*(1-ETA)
       SF3 = (1+KSI)*(1+ETA)
       SF4 = (1-KSI)*(1+ETA)

        DO I=JFT,JLT          
          TEMPEL(I) = FOURTH * (SF1 *TEMP(IXC(2,I)) + SF2*TEMP(IXC(3,I)) + 
     .                          SF3 *TEMP(IXC(4,I)) + SF4*TEMP(IXC(5,I)) )
       ENDDO
C
      RETURN
      END
