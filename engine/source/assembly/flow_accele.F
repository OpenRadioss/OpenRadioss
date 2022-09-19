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
Chd|  FLOW_ACCELE                   source/assembly/flow_accele.F 
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FLOW_ACCELE(NALE,MS     , A    ,V      ,NODFT ,NODLT  )
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT, NODLT,NALE(*)
C     REAL
      my_real
     .   A(3,*) ,V(3,*) ,MS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  J,K,N
      my_real
     .         RTMP
C
#include      "vectorize.inc"
      DO N=NODFT,NODLT
        IF(NALE(N) == 3) THEN
         IF(MS(N)>ZERO) THEN
           RTMP = ONE / MS(N)
           A(1,N) = A(1,N) * RTMP
           A(2,N) = A(2,N) * RTMP
           A(3,N) = A(3,N) * RTMP
         ELSE
           A(1,N) = ZERO
           A(2,N) = ZERO
           A(3,N) = ZERO
           IF(NMULT/=0) THEN
             V(1,N) = ZERO
             V(2,N) = ZERO
             V(3,N) = ZERO
           ENDIF
         ENDIF
        ELSE ! lagrangian or not Law77 node.
           A(1,N) = ZERO
           A(2,N) = ZERO
           A(3,N) = ZERO
        ENDIF
      ENDDO
C--------------------------------------------
      RETURN
      END
