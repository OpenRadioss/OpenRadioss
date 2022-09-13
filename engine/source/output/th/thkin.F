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
Chd|  THKIN                         source/output/th/thkin.F      
Chd|-- called by -----------
Chd|        ABFHIST2                      source/output/abf/abfhist2.F  
Chd|        HIST2                         source/output/th/hist2.F      
Chd|-- calls ---------------
Chd|        WRTDES                        source/output/th/wrtdes.F     
Chd|====================================================================
      SUBROUTINE THKIN(J1,J2,ITHBUF,L1,L2,WA,FSAV,IFORM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
      INTEGER ITHBUF(*),IFORM
      INTEGER J1,J2,L1,L2,K
      my_real
     .        WA(*),FSAV(NTHVKI,*)
C
      INTEGER I,J,L,II
C
Cel gather deja effectue, seul p0 ecrit 
      IF (IMACH/=3.OR.ISPMD==0) THEN
        II = 0
        DO J=J1,J2
              I=ITHBUF(J)
              DO L=L1,L2
                K=ITHBUF(L)
                II=II+1
                WA(II)=FSAV(K,I)
              ENDDO
        ENDDO
        IF(II>0)CALL WRTDES(WA,WA,II,IFORM,1)
      ENDIF
C
      RETURN
      END
