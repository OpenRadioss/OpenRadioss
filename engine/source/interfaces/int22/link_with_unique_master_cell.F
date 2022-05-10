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
      FUNCTION LINK_WITH_UNIQUE_MASTER_CELL (
     .                    SharedFACE, Centroid )
C-----------------------------------------------
C   D e s c r i p t i o n
C----------------------------------------------- 
! With interface 22 Hexahedral cell is cut and
! master one is the biggest one which is greater
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
      INTEGER   :: LINK_WITH_UNIQUE_MASTER_CELL
      my_real   :: SharedFACE(6), Centroid(3,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER   ::  IsMaster, N, IPOS, LEVEL, IIP
      my_real   ::  SCORE(6), MAX, NextMAX, MySum
      my_real   ::  XYZmax(3)
C-----------------------------------------------
C   P r e - C o n d i t i o n
C----------------------------------------------- 
      !IF(INT22==0)RETURN   !already checked
      !IF(K.LT.2)  RETURN   !already checked
C-----------------------------------------------
C   S o u r c e    L i n e s
C----------------------------------------------- 
      IPOS              = 1
      LEVEL             = 3   !3:X, 2:Y, 1:Z
      SCORE(1:6)        = ZERO
                     
      IPOS      = MAXLOC(SharedFACE(1:6),1)
      
      IF(SharedFACE(IPOS) == ZERO) THEN
        LINK_WITH_UNIQUE_MASTER_CELL = 0
        RETURN
      ENDIF
      
      MySum = SUM(SharedFACE(1:IPOS-1))
      IF (IPOS.NE.6)THEN
        MySUM = MySUM + SUM(SharedFACE(IPOS+1:6))
      ENDIF
      IF (MySUM == ZERO)THEN
        LINK_WITH_UNIQUE_MASTER_CELL = IPOS
        RETURN
      ENDIF

      XYZmax(1) = MAXVAL(Centroid(1,1:6))
      XYZmax(2) = MAXVAL(Centroid(2,1:6))
      XYZmax(3) = MAXVAL(Centroid(3,1:6))            
           
      !find max
      MAX                          = SharedFACE(IPOS)       !init with maximum value
      LINK_WITH_UNIQUE_MASTER_CELL = IPOS
      !score it
      SCORE(IPOS) = 1000*(XYZmax(1)-Centroid(1,IPOS))+ 100*(XYZmax(2)-Centroid(2,IPOS))+ 10*(XYZmax(3)-Centroid(3,IPOS))      
      
      !find other ones equal to MAX
      IF (IPOS == 6)RETURN
      IIP = IPOS
      DO WHILE(IIP < 6)
        IIP = MAXLOC(SharedFACE(IIP+1:6),1)
        NextMAX = SharedFACE(IIP)
        IF (NextMAX == MAX) THEN
          SCORE(IPOS) = 1000*(XYZmax(1)-Centroid(1,IIP))+100 * (XYZmax(2)-Centroid(2,IIP)) + 10*(XYZmax(3)-Centroid(3,IIP))
        ENDIF
      ENDDO

      IPOS                         = MAXLOC(SCORE(1:6),1)
      LINK_WITH_UNIQUE_MASTER_CELL = IPOS
      
      RETURN
      END
