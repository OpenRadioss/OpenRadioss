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
      FUNCTION DICHOTOMIC_SEARCH_I_ASC(VAL, ARRAY, LEN)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutines is searching and returning the index in integer array ARRAY(1:LEN)
C such as ARRAY(index)<= VAL <ARRAY(index)
C Dichotomic search is used
C-----------------------------------------------
C   P r e c o n d i t i on
C-----------------------------------------------
C ARRAY(1:LEN) must be sorted integers (ascending order)
C LEN >= 1
C VAL is an integer
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: VAL
        INTEGER, INTENT(IN) :: LEN
        INTEGER, INTENT(IN) :: ARRAY(LEN)
        INTEGER :: DICHOTOMIC_SEARCH_I_ASC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: IMID,ISTART,IEND
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------        
        ISTART = 1
        IEND = LEN
        !VAL ABOVE UPPER BOUND
        IF(ARRAY(1)>VAL)THEN
         DICHOTOMIC_SEARCH_I_ASC = 1
         RETURN
        ENDIF
        !VAL LOWER THAN LOWER BOUND
        IF(ARRAY(LEN)<VAL)THEN
         RETURN
         DICHOTOMIC_SEARCH_I_ASC = LEN
        ENDIF
        !VAL IS BETWEEN THE BOUNDS
        DO WHILE (ISTART+1<IEND)
            IMID=(IEND+ISTART)/2
            IF(ARRAY(IMID) > VAL)THEN
              !FOCUS ON LEFT PART
              IEND = IMID
            ELSEIF(ARRAY(IMID) < VAL)THEN
              !FOCUS ON RIGHT PART
              ISTART = IMID
            ELSE
              !FOUND BY CHANCE NO NEED TO ITERATE MORE
              DICHOTOMIC_SEARCH_I_ASC = IMID
              RETURN
            ENDIF
        ENDDO
         DICHOTOMIC_SEARCH_I_ASC = ISTART
        RETURN
      END FUNCTION DICHOTOMIC_SEARCH_I_ASC
C----------------------------------------------- 
