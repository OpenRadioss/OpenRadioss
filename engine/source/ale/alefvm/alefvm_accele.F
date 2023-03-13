Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  ALEFVM_ACCELE                 source/ale/alefvm/alefvm_accele.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ALEFVM_ACCELE(
     1                  A       ,AR    ,V    ,VR      ,IN   ,
     2                  NODFT   ,NODLT ,ITAB ,MSNF    ,MS   ,
     3                  NALE
     .                  )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C 'alefvm' is related to a collocated scheme (built from FVM and based on Godunov scheme)
C  which was temporarily introduced for experimental option /INTER/TYPE22 (FSI coupling with cut cell method)
C This cut cell method is not completed, abandoned, and is not an official option.
C There is no other use for this scheme which is automatically enabled when /INTER/TYPE22 is defined (INT22>0 => IALEFVM=1).
C
C This subroutine is treating an uncut cell.
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NODFT, NODLT, ITAB(*), NALE(*)
      my_real :: A(3,NUMNOD) , V(3,NUMNOD) , VR(3,NUMNOD),  AR(3,NUMNOD), MS(NUMNOD)  , MSNF(NUMNOD),  IN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: J,K,N
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutines is resetting accelerations on nodes
C since they are used in ecrit.F to update velocity.
C Velocity don't have to be updates in ecrit.F where
C they are still nodal momentum up to vitesse.F
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------


#include "vectorize.inc"
        !---TRANSLATIONS---!
        DO N=NODFT,NODLT
            IF(NALE(N)==0)CYCLE
            A(1,N)    = ZERO
            A(2,N)    = ZERO
            A(3,N)    = ZERO
        ENDDO
        IF (IRODDL/=0) THEN
#include "vectorize.inc"
          !---TROTATIONS---!
          DO N=NODFT,NODLT
              IF(NALE(N)==0)CYCLE          
              AR(1,N) = ZERO
              AR(2,N) = ZERO
              AR(3,N) = ZERO      
          ENDDO
        ENDIF

       
C-----------------------------------------------        
      RETURN
      END
