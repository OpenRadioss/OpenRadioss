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
Chd|  PRINC_U1                      source/materials/mat/mat001/m1law.F
Chd|-- called by -----------
Chd|        M1ISMSTR11                    source/materials/mat/mat001/m1law.F
Chd|-- calls ---------------
Chd|        VALPVECOP_V                   source/materials/mat/mat033/sigeps33.F
Chd|====================================================================
              SUBROUTINE PRINC_U1(
     1                            NEL   , ES1 , ES2 ,ES3 ,ES4 ,
     2                            ES5   ,ES6  ,EV   , DIRPRV  )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C O M M O N 
C-----------------------------------------------
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL     
      my_real
     .   ES1(MVSIZ), ES2(MVSIZ), ES3(MVSIZ), 
     .   ES4(MVSIZ), ES5(MVSIZ), ES6(MVSIZ),
     .   EV(MVSIZ,3),DIRPRV(MVSIZ,3,3)
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER    I,J,K
      my_real
     .            AV(MVSIZ,6),EVV(MVSIZ,3)
C----------------------------------------------------------------
C--------- [B]=[F][F]^t -1 strain-----
      DO I=1,NEL
       AV(I,1) = ES1(I)
       AV(I,2) = ES2(I)
       AV(I,3) = ES3(I)
       AV(I,4) = ES4(I)
       AV(I,5) = ES5(I)
       AV(I,6) = ES6(I)
      ENDDO
        CALL VALPVECOP_V(AV,EVV,DIRPRV,NEL)

      DO I=1,NEL
        EV(I,1:3)=SQRT(MAX(ZERO,(EVV(I,1:3)+ ONE))) - ONE
      ENDDO
C----------------
C      
      RETURN
      END
