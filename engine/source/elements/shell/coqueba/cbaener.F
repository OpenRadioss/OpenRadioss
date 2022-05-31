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
Chd|  CBAENER                       source/elements/shell/coqueba/cbaener.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBAENER(FORPG,EINT ,JFT  ,JLT  ,OFF  ,
     .                   VOL  ,EXY  ,NEL  )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
       INTEGER JFT ,JLT,NEL
       my_real 
     .     VOL(*),OFF(*), EXY(*) ,FORPG(NEL,5),EINT(NEL,2) 
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I 
        my_real  THOFF 
C=======================================================================
        DO I=JFT,JLT
          THOFF = HALF* OFF(I)*VOL(I)
          EINT(I,1) = EINT(I,1) - FORPG(I,3)*EXY(I)*THOFF
       ENDDO
c------------
       RETURN
       END
Chd|====================================================================
Chd|  CBAENERS                      source/elements/shell/coqueba/cbaener.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CBAENERS(JFT ,JLT  ,OFF ,AREA,THK0,
     .                      DEF ,FORPG,EINT,DT  ,NEL )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
       INTEGER JFT ,JLT,NEL
       my_real 
     .     AREA(*),THK0(*),OFF(*), DEF(MVSIZ,8) ,FORPG(NEL,5),EINT(NEL,2),DT 
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I 
        my_real  THOFF 
C
        DO I=JFT,JLT
          THOFF = OFF(I)* AREA(I)*THK0(I)*DT*HALF
          EINT(I,1) = EINT(I,1) + FORPG(I,3)*DEF(I,3)*THOFF
       ENDDO
       RETURN
       END
