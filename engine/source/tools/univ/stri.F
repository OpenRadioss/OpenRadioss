Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  STRI                          source/tools/univ/stri.F      
Chd|-- called by -----------
Chd|        HIST1                         source/output/th/hist1.F      
Chd|        HIST13                        source/output/th/hist13.F     
Chd|        I21ASS3                       source/interfaces/int21/i21ass3.F
Chd|        I21FOR3                       source/interfaces/int21/i21for3.F
Chd|        I21MAINF                      source/interfaces/int21/i21mainf.F
Chd|        I23MAINF                      source/interfaces/int23/i23mainf.F
Chd|        LECNOISE                      source/general_controls/computation/lecnoise.F
Chd|        WRTDES                        source/output/th/wrtdes.F     
Chd|        WRTSQI                        source/output/tools/wrtsqi.F  
Chd|        STRI                          source/tools/univ/stri.F      
Chd|-- calls ---------------
Chd|        STRI                          source/tools/univ/stri.F      
Chd|====================================================================
      CHARACTER*8 FUNCTION STRI(N)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "chrasc_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NN, IS, MANT1, MANT2, MANT3, MANT4, MANT12, MANT21,
     .   MANT22, MANT31, MANT32, MANT41, MANT42
C-----------------------------------------------
      IF(N>=0)THEN
       NN=N
       IS=0
      ELSE
       NN=-N
       IS=1
      ENDIF
      MANT1=NN/1000000000
      NN   =NN-1000000000*MANT1
      MANT2=NN/1000000
      NN   =NN-1000000*MANT2
      MANT3=NN/1000
      MANT4=NN-1000*MANT3
C
      MANT12=MANT1+16*IS
      MANT21=MANT2/32
      MANT22=MANT2-32*MANT21
      MANT31=MANT3/32
      MANT32=MANT3-32*MANT31
      MANT41=MANT4/32
      MANT42=MANT4-32*MANT41
C
      STRI(1:1)='0'
      STRI(2:2)=CODAS(MANT12)
      STRI(3:3)=CODAS(MANT21)
      STRI(4:4)=CODAS(MANT22)
      STRI(5:5)=CODAS(MANT31)
      STRI(6:6)=CODAS(MANT32)
      STRI(7:7)=CODAS(MANT41)
      STRI(8:8)=CODAS(MANT42)
C
      RETURN
      END
