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
Chd|  WRTDES0                       source/output/th/wrtdes0.F    
Chd|-- called by -----------
Chd|        ABFHIST2                      source/output/abf/abfhist2.F  
Chd|        HIST2                         source/output/th/hist2.F      
Chd|-- calls ---------------
Chd|        SPMD_GLOB_DSUM9               source/mpi/interfaces/spmd_th.F
Chd|        WRTDES                        source/output/th/wrtdes.F     
Chd|====================================================================
      SUBROUTINE WRTDES0(NG,WA,II,IFORM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
      INTEGER NG,II,IFORM
      my_real
     .    WA(*)
C-----------------------------------------------
c      IF (IMACH.NE.3) THEN
c        CALL WRTDES(WA,WA,II,ITFORM,1)
c      ELSE
       IF(NSPMD > 1) CALL SPMD_GLOB_DSUM9(WA,II)
       IF (ISPMD.EQ.0) CALL WRTDES(WA,WA,II,IFORM,1)
c      ENDIF
      RETURN
      END
