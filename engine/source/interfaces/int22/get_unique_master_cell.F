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
      FUNCTION GET_UNIQUE_MAIN_CELL (
     .                    NIN,   IB,   K )
C-----------------------------------------------
C   D e s c r i p t i o n
C----------------------------------------------- 
C Interface Type22 (/INTER/TYPE22) is an FSI coupling method based on cut cell method. 
C   This experimental cut cell method is not completed, abandoned, and is not an official option.
C
! With interface 22 Hexahedral cell is cut and
! main one is the biggest one which is greater
! then 50%. In case of multiple choice (2 are
! 50%-50% or several if criteria is reduced) we
! must ensure a unique choice which is repeatable
! with plateform and also different numbering.
!
! Here is chosen a sort by coordinates weighting
! X,Y,Z components. a score is computed
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE INITBUF_MOD
      USE I22BUFBRIC_MOD 
      USE I22TRI_MOD
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "i22edges.inc"  
#include      "inter22.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER   :: NIN,IB,K
      INTEGER   :: GET_UNIQUE_MAIN_CELL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER   ::  IsMain, N, IPOS, LEVEL, ICELL
      my_real   ::  CENTROID(3,K), SCORE(K), MIN, NextMIN
C-----------------------------------------------
C   P r e - C o n d i t i o n
C----------------------------------------------- 
      !IF(INT22==0)RETURN   !already checked
      !IF(K<2)  RETURN   !already checked
C-----------------------------------------------
C   S o u r c e    L i n e s
C----------------------------------------------- 
      CENTROID(:,:) = ZERO
      N             = 0
      IPOS          = 1
      LEVEL         = 3   !3:X, 2:Y, 1:Z
      SCORE(1:K)    = ZERO
      
      DO ICELL = 1, 9
        IsMain = BRICK_LIST(NIN,IB)%POLY(ICELL)%IsMain
        IF(IsMain==0)CYCLE
        N = N + 1
        CENTROID(1:3,N) = BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(1:3)
      ENDDO

      DO WHILE(LEVEL >= 1) 
        IPOS        = MINLOC(CENTROID(4-LEVEL,1:K),1)
        MIN         = CENTROID(4-LEVEL,IPOS)       !init with maximum value
        DO WHILE (IPOS < K)
          IPOS        = MINLOC(CENTROID(4-LEVEL,IPOS+1:K),1)
          NextMIN      = CENTROID(4-LEVEL,IPOS)
          IF(NextMIN == MIN) THEN
            SCORE(IPOS) = SCORE(IPOS) + 10**LEVEL
            IPOS        = IPOS + 1
          ENDIF
        ENDDO!next IPOS
        IPOS  = 1
        LEVEL = LEVEL -1
      ENDDO!next LEVEL

      IPOS                   = MAXLOC(SCORE(1:K),1)
      GET_UNIQUE_MAIN_CELL = IPOS
      
      RETURN
      END
