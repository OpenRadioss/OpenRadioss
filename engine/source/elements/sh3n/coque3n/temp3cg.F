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
Chd|  TEMP3CG                       source/elements/sh3n/coque3n/temp3cg.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TEMP3CG(JFT ,JLT   , PM   ,MAT   ,IXTG ,   
     3                  TEMP ,TEMPEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,MAT(*),IXTG(NIXTG,*)
C     REAL
      my_real
     .   TEMP(*), PM(NPROPM,*),TEMPEL(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL  
C --- calcul de la tempurature au CG 
        DO I=JFT,JLT
          TEMPEL(I) = THIRD * ( TEMP(IXTG(2,I)) + TEMP(IXTG(3,I)) + 
     .                          TEMP(IXTG(4,I))  )
       ENDDO
C
      RETURN
      END
