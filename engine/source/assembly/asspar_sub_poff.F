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
Chd|  ASSPAR_SUB_POFF               source/assembly/asspar_sub_poff.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ASSPAR_SUB_POFF(A       ,NODFT   ,NODLT   ,
     .                           POSI    ,SIZEA   ,NTHREAD ) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C----------------------------------------------
      INTEGER NODFT,NODLT,POSI(*),NTHREAD
      INTEGER SIZEA
      my_real 
     .   A(SIZEA,NTHREAD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,NN,NPOS,NDDL,J
C
      ! Loop over threads
      DO K = 2,NTHREAD
        DO NN = NODFT, NODLT                              ! Loop over the nodes of the subset
          NPOS = POSI(NN)                                 !   Position of its first d.o.f in A
          NDDL = POSI(NN+1) - NPOS                        !   Number of additional degrees of freedom
          DO J = 1,NDDL                                   !   Loop over additional d.o.fs
            A(NPOS+J-1,1) = A(NPOS+J-1,1) + A(NPOS+J-1,K) !     Assembling acceleration on threads
            A(NPOS+J-1,K) = ZERO                          !     Setting to zero the acceleration on the secnd thread
          ENDDO
        ENDDO
      ENDDO
C
      RETURN
      END
