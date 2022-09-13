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
Chd|  weighting_Cell_Nodes          source/interfaces/int22/weighting_cell_nodes.F
Chd|-- called by -----------
Chd|        SINIT22_FVM                   source/interfaces/int22/sinit22_fvm.F
Chd|-- calls ---------------
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE weighting_Cell_Nodes (
     .                    NIN,   IB,   ICELL , IRES, IDEMERGE)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Interface Type22 (/INTER/TYPE22) is an FSI coupling method based on cut cell method. 
C   This experimental cut cell method is not completed, abandonned, and is not an official option.
C
! Get Nodes which is the farthest from intersection points
! equivalent to calculating mean adjacent surface ratio.
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
c      USE INITBUF_MOD
      USE I22BUFBRIC_MOD 
c      USE I22TRI_MOD
c      USE ELBUFDEF_MOD
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
      INTEGER,INTENT(IN)      :: NIN,IB,ICELL
      INTEGER,INTENT(INOUT)   :: IRES, IDEMERGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER   ::  IsMain, N, IPOS, LEVEL, J, IFAC, I, NumNOD, ListNOD(8),K, NbCut
      my_real   ::  ALPHA(8),TAG(6),RATIO(6), NFAC(8)
C-----------------------------------------------
C   P r e - C o n d i t i o n
C----------------------------------------------- 
      !IF(INT22==0)RETURN   !already checked
C-----------------------------------------------
C   S o u r c e    L i n e s
C----------------------------------------------- 

      IRES         = 0
      NFAC(1:8)    = ZERO
      IDEMERGE     = 0
      
      NumNOD       = BRICK_LIST(NIN,IB)%POLY(ICELL)%NumNOD 
      NbCut        = BRICK_LIST(NIN,IB)%NbCut
      ListNOD(1:8) = BRICK_LIST(NIN,IB)%POLY(ICELL)%ListNodID(1:8)
      ALPHA(1:8)     =ZERO
      
      DO J=1,6
        RATIO(J)   = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Surf / BRICK_LIST(NIN,IB)%Face_Brick(J)
      ENDDO

      DO K=1,NumNOD
       I           = ListNOD(K)
        DO J=1,3
         IFAC      = iFacesFromNode(I,J)      
         !IF(RATIO(IFAC)==ZERO)CYCLE
         NFAC(I)   = NFAC(I) + ONE
         ALPHA(I)  = ALPHA(I) + RATIO(IFAC)
        ENDDO!next J
        IF(NFAC(I)>ZERO)ALPHA(I) = ALPHA(I) / NFAC(I)
        IF(ALPHA(I)>CritDVol22 .AND. BRICK_LIST(NIN,IB)%NODE(I)%NodWasMain==0)IDEMERGE=1
      ENDDO!next K
      
      IF(NumNOD>0) IRES = MAXLOC(ALPHA(1:8),1)
      
!      IF(NumNOD==0)THEN
!        print *, "**error : inter22. Connectivity issue."
!        stop
!      ENDIF
      
      RETURN
      END
