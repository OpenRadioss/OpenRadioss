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
Chd|  FLOW_VITESSE                  source/assembly/flow_vitesse.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FLOW_VITESSE(NALE,A    ,V      ,FZERO,
     2                   NODFT,NODLT    ,W      ,VS,IFOAM)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT, NODLT, NDDIM, NALE(*),IFOAM(*)
C     REAL
      my_real
     .   FZERO(3,*) ,A(3,*) ,V(3,*),W(3,*), VS(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,K,N
C
#include "vectorize.inc"
      DO N=NODFT,NODLT
        IF(NALE(N) == 3 ) THEN
         IF( IFOAM(N) == 0 ) THEN
           V(1,N)=V(1,N)+DT12*A(1,N)
           V(2,N)=V(2,N)+DT12*A(2,N)
           V(3,N)=V(3,N)+DT12*A(3,N)
         ELSE
           V(1,N)=VS(1,N)
           V(2,N)=VS(2,N)
           V(3,N)=VS(3,N)
           IFOAM(N) = 0
         ENDIF
           W(1,N) = VS(1,N)
           W(2,N) = VS(2,N)
           W(3,N) = VS(3,N)
           A(1,N)=ZERO
           A(2,N)=ZERO
           A(3,N)=ZERO
        ELSE ! lagrangian or not Law77 node.
           V(1,N)=ZERO
           V(2,N)=ZERO
           V(3,N)=ZERO
C          
           W(1,N) = ZERO
           W(2,N) = ZERO
           W(3,N) = ZERO
C          
           A(1,N)=ZERO
           A(2,N)=ZERO
           A(3,N)=ZERO
        ENDIF
      ENDDO
C
      IF(ISIGI==2.OR.ISIGI==4) THEN
#include      "vectorize.inc"
        DO N=NODFT,NODLT
          IF(NALE(N) == 3) THEN
            A(1,N)=FZERO(1,N)
            A(2,N)=FZERO(2,N)
            A(3,N)=FZERO(3,N)

            W(1,N) = VS(1,N)
            W(2,N) = VS(2,N)
            W(3,N) = VS(3,N)
          ENDIF
        ENDDO
      ENDIF
C
      RETURN
      END
