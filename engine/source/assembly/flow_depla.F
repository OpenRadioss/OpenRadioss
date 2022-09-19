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
Chd|  FLOW_DEPLA                    source/assembly/flow_depla.F  
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FLOW_DEPLA(NALE,V    ,D    ,
     2                 NODFT,NODLT    )
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT, NODLT, NDDIM, NALE(*)
C     REAL
      my_real
     .  D(3,*) ,V(3,*)
cc      DOUBLE PRECISION
C     .   XDP(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,K,N,J
      my_real
     .   VDT
      DOUBLE PRECISION VDT2
C-------------------------------------------------------------
C         CALCUL DES  DEPLACEMENTS TRANSLATIONS PUIS ROTATIONS
C-------------------------------------------------------------

      IF (IRESP==1) THEN
#include      "vectorize.inc"
        DO N=NODFT,NODLT
          IF(NALE(N) == 3) THEN
             VDT2 = DT2*V(1,N)
             D(1,N)=D(1,N)+VDT2
cc             X(1,N)=X(1,N)+VDT2
cC             XDP(1,N)=XDP(1,N)+VDT2
C             X(1,N)=XDP(1,N)
             VDT2 = DT2*V(2,N)
             D(2,N)=D(2,N)+VDT2
cc             X(2,N)=X(2,N)+VDT2
c             XDP(2,N)=XDP(2,N)+VDT2
c             X(2,N)=XDP(2,N)
             VDT2 = DT2*V(3,N)
             D(3,N)=D(3,N)+VDT2
cc             X(3,N)=X(3,N)+VDT2
C             XDP(3,N)=XDP(3,N)+VDT2
C             X(3,N)=XDP(3,N)
          ENDIF
        ENDDO
      ELSE
#include      "vectorize.inc"
        DO N=NODFT,NODLT
          IF(NALE(N) == 3) THEN
              VDT = DT2*V(1,N)
              D(1,N)=D(1,N)+VDT
cc              X(1,N)=X(1,N)+VDT
              VDT = DT2*V(2,N)
              D(2,N)=D(2,N)+VDT
cc              X(2,N)=X(2,N)+VDT
              VDT = DT2*V(3,N)
              D(3,N)=D(3,N)+VDT
cc              X(3,N)=X(3,N)+VDT
          ENDIF
        ENDDO
      ENDIF
C
      RETURN
      END
