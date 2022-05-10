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
	SUBROUTINE TKEG3(JFT     ,JLT     ,
     1                   FX      ,AL      ,KE11    ,KE12    ,KE22   )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      MY_REAL 
     .    FX(*),AL(*),KE11(3,3,*),KE12(3,3,*),KE22(3,3,*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER EP,I,J
      MY_REAL 
     .    H(MVSIZ)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
       DO EP=JFT,JLT 
         H(EP)=FX(EP)/AL(EP)
         IF (H(EP)<ZERO) H(EP)=EM9
       ENDDO
       DO I=1,3
         DO EP=JFT,JLT
           KE11(I,I,EP) = KE11(I,I,EP)+H(EP)
           KE12(I,I,EP) = KE12(I,I,EP)-H(EP)
           KE22(I,I,EP) = KE22(I,I,EP)+H(EP)
         ENDDO! next EP
       ENDDO! next I
C-----------------------------------------------
      RETURN
      END
