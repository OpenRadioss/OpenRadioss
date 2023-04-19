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
Chd|  LINE_DECOMP                   source/model/group/line_decomp.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE LINE_DECOMP(IGRSLIN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FRONT_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       TYPE (SURF_)   , DIMENSION(NSLIN)   :: IGRSLIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NN,IAD,IAD1,IAD2,IAD11,ND1,ND2,P,P1,P2
      INTEGER I,J
      LOGICAL PSEARCH
C---------------------------------------------------------------
C Care : In lines -  Element ID is wrong after Element grouping
C I reuse last field to store the processor ID.
C When lines are taken care in Element Grouping 
C   1. Set processor only when no element is set in lines
C   2. Split lines accordingly
!
!        /\
!       /  \
!      / |  \ 
!     /  |   \
!    /   o    \
!   /__________\
!
!       /LINE are not used in the engine and the split is wrong in
!       case of useless line (ie. when a line is defined but not 
!       used by an interface or other stuffs) --> 
!       2 nodes (defining a segment) can be on 2 different processors
!       in this case, the segment is not written in the restart file
!       one could also define the nodes on the same processor but
!       it will increase the comm.
C---------------------------------------------------------------
      DO I = 1, NSLIN
        NN  = IGRSLIN(I)%NSEG
        DO J = 1, NN
          P=0
          PSEARCH= .TRUE.
          ND1 = IGRSLIN(I)%NODES(J,1)
          ND2 = IGRSLIN(I)%NODES(J,2)
C For better performance, I don't use nlocal, but use low level system
          IAD1 = IFRONT%IENTRY(ND1) 
          IAD2 = IFRONT%IENTRY(ND2) 

          IAD11 = IAD1
C
C Initialization
C
          P1 = IFRONT%P(1,IAD1)
          P2 = IFRONT%P(1,IAD2)
C
C Common case : node are on same domain
C
          IF(P1==P2) THEN
             P=P1
             PSEARCH= .FALSE.
          ENDIF
C
          DO WHILE (PSEARCH)

C P1 equal to p2 found
            IF(P1 == P2) THEN
               P=P1
               PSEARCH= .FALSE.
            ENDIF

C Search leaves - in case 2 nodes from segments are in different domain.
            IF(P1 == 0)THEN
               print*,'ERROR P1',ND1,ND2
               CALL MY_EXIT(2)
            ENDIF
            IF(P2 == 0)THEN
               print*,'ERROR P2',ND1,ND2
               CALL MY_EXIT(2)
            ENDIF
C Move forward

            !   ---------------------
            IF(P1 < P2)THEN
              IAD1 = IFRONT%P(2,IAD1)
            ENDIF

            IF(P1 > P2)THEN
              IAD2 = IFRONT%P(2,IAD2)
            ENDIF            
            !   ---------------------
            !   iad1 = 0 or iad2 = 0 --> the last processor id 
            !   is reached
            IF(IAD1==0.OR.IAD2==0) THEN
                ! skip this segment 
                P1=0
                P2=0
                P=0
                PSEARCH= .FALSE.
            ELSE
                P1 = IFRONT%P(1,IAD1)
                P2 = IFRONT%P(1,IAD2)
            ENDIF
            !   ---------------------
          ENDDO  ! DO WHILE (PSEARCH)
C For this split - I use 3rd field to set Processor (supposed empty)

          IGRSLIN(I)%PROC(J) = P

        ENDDO    ! DO J = 1, NN
      ENDDO      ! DO I = 1, NSLIN

      RETURN
      END

