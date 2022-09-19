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
Chd|  ASSPAR_SUB                    source/assembly/asspar_sub.F  
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ASSPAR_SUB(A       ,FSKY    ,ADDCNE  ,NODFT_2 ,
     .                      NODLT_2 ,POSI    ,SIZESUB ,SIZEA   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C----------------------------------------------
      INTEGER ADDCNE(*),NODFT_2,NODLT_2,POSI(*)
      INTEGER SIZESUB,SIZEA
      my_real
     .   A(SIZEA),FSKY(ADDCNE(SIZESUB+1),*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,N,NCT,NC,NPOS,NDDL,J
C
      ! Loop over the current nodes
      DO N = NODFT_2, NODLT_2
        NCT  = ADDCNE(N)-1                          ! First element attached to the node in FSKY
        NC   = ADDCNE(N+1)-ADDCNE(N)                ! Number of elements attached to the node
        NPOS = POSI(N)                              ! Position of its first d.o.f in A
        NDDL = POSI(N+1) - NPOS                     ! Number of additional d.o.fs

        DO K = NCT+1, NCT+NC                        ! Loop over attached elements
          DO J = 1,NDDL                             !   Loop over additional d.o.fs
            A(NPOS+J-1) = A(NPOS+J-1) + FSKY(K,J)
            FSKY(K,J) = ZERO
          ENDDO
        ENDDO
      ENDDO
C
      RETURN
      END SUBROUTINE ASSPAR_SUB
