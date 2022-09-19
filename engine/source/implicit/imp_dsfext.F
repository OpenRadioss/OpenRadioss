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
Chd|  IMP_DSFEXT                    source/implicit/imp_dsfext.F  
Chd|-- called by -----------
Chd|        LIN_SOLVP2                    source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE IMP_DSFEXT(GRAPHE , NF, F, NDDL, IAD_ELEM,
     .                      FR_ELEM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "com04_c.inc"
#include "task_c.inc"
*
#include "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NF, NDDL, IAD_ELEM(2,*), FR_ELEM(*)
      my_real
     .        F(NF,*)
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ITAG(NUMNOD), J, NOD
C
      DO I=1,NUMNOD
         ITAG(I)=1
      ENDDO
C
      DO I=1,NSPMD
         DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            NOD=FR_ELEM(J)
            ITAG(NOD)=ITAG(NOD)+1
         ENDDO
      ENDDO
      DO I=1,NF
         DO J=1,NDDL
            NOD=GRAPHE(1)%LSDDL(1,J)
            F(I,J)=F(I,J)/ITAG(NOD)
         ENDDO
      ENDDO
C
      RETURN
      END

