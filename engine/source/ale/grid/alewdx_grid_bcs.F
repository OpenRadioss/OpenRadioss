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
Chd|  ALEWDX_GRID_BCS               source/ale/grid/alewdx_grid_bcs.F
Chd|-- called by -----------
Chd|        ALEW5                         source/ale/grid/alew5.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ALEWDX_GRID_BCS(
     1     SKEW, ISKEW,  ICODT, VEC, NALE, ITASK, NODE_ID)
C-----------------------------------------------
C     D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is imposing BCS for grid points.
C done at each iteration since skew may evolve during a smoothing iteration
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(INOUT) :: ISKEW(*),ICODT(*),NALE(NUMNOD),ITASK,NODE_ID
      my_real,INTENT(INOUT) :: SKEW(LSKEW,*),VEC(3)
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  N, ISK, LCOD1,LCOD2, NINDX
      my_real AA
      LOGICAL TAG(3)
C-----------------------------------------------
C     S o u r c e   L i n e s
C-----------------------------------------------

C----------------------------------------
C     BOUNDARY CONDITIONS (MESH)
C----------------------------------------  
      !RETRIEVING CODE FOR BCS et ALE/BCS
      ! 000 <=> LCOD=0
      ! 001 <=> LCOD=1 (Z)
      ! 010 <=> LCOD=2 (Y)
      ! 011 <=> LCOD=3 (YZ)
      ! 100 <=> LCOD=4 (X)
      ! 101 <=> LCOD=5 (XZ)
      ! 110 <=> LCOD=6 (XY)
      ! 111 <=> LCOD=7 (XYZ)                                  
      TAG(1:3)=.FALSE.                                                                                     
      NINDX = 0                                                                                                                                                                             
      N = NODE_ID                                                                                           
      IF(NALE(N) /= 0)THEN                                                                             
         ISK=ISKEW(N)                                                                                  
         LCOD1=ICODT(N+NUMNOD)   !BCS                                                                       
         IF(LCOD1 /= 0) THEN                                                                            
            NINDX = N                                                                                                                                                         
         ENDIF                                                                                         
         LCOD2=ICODT(N+NUMNOD+NUMNOD) !ALE/BCS
         IF(LCOD2 /= 0) THEN                                                                            
            NINDX = N                                                                                                                                                         
         ENDIF                                                                            
      ENDIF 
      
      !TEST ENABLED BINARY (check for X,Y or Z in LCOD1 or LCOD2)
      !
      IF(BTEST(LCOD1,0).OR.BTEST(LCOD2,0))TAG(3)=.TRUE.    ! **1 => condition on Z dir
      IF(BTEST(LCOD1,1).OR.BTEST(LCOD2,1))TAG(2)=.TRUE.    ! *1* => condition on Y dir
      IF(BTEST(LCOD1,2).OR.BTEST(LCOD2,2))TAG(1)=.TRUE.    ! 1** => condition on X dir
      
      !UPDATE VECTOR DEPENDING ON BOUNDARY CONDITION AT RELATED NODE 
      !                                                                                                     
      IF (NINDX /= 0 )THEN                                                                            
          ISK  = ISKEW(N)                                                                                                                                                           
          IF(ISK == 1) THEN                                                                            
           !=====REPERE GLOBAL
           IF(TAG(1))VEC(1)=ZERO                                                                         
           IF(TAG(2))VEC(2)=ZERO
           IF(TAG(3))VEC(3)=ZERO                                                                                            
          ELSE                                                                                         
           !=====REPERE OBLIQUE                                                                                                                                                 
             IF(TAG(1))THEN                                                                                   
               AA  =SKEW(1,ISK)*VEC(1)+SKEW(2,ISK)*VEC(2)+SKEW(3,ISK)*VEC(3)                                    
               VEC(1)=VEC(1)-SKEW(1,ISK)*AA                                                               
               VEC(2)=VEC(2)-SKEW(2,ISK)*AA                                                               
               VEC(3)=VEC(3)-SKEW(3,ISK)*AA                                                               
             ENDIF 
             IF(TAG(2))THEN                                                                                    
               AA  =SKEW(4,ISK)*VEC(1)+SKEW(5,ISK)*VEC(2)+SKEW(6,ISK)*VEC(3)                                    
               VEC(1)=VEC(1)-SKEW(4,ISK)*AA                                                               
               VEC(2)=VEC(2)-SKEW(5,ISK)*AA                                                               
               VEC(3)=VEC(3)-SKEW(6,ISK)*AA                                                               
             ENDIF        
             IF(TAG(3))THEN                                                                                   
               AA  =SKEW(7,ISK)*VEC(1)+SKEW(8,ISK)*VEC(2)+SKEW(9,ISK)*VEC(3)                                    
               VEC(1)=VEC(1)-SKEW(7,ISK)*AA                                                               
               VEC(2)=VEC(2)-SKEW(8,ISK)*AA                                                               
               VEC(3)=VEC(3)-SKEW(9,ISK)*AA
             ENDIF                                                               
          ENDIF                                                                                                                                                                              
      ENDIF                                                                                            

      RETURN
      END
