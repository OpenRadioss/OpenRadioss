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
Chd|  PHASE_PROPAGATION                   source/initial_conditions/inivol/phase_propagation.F
Chd|-- called by -----------
Chd|        PHASE_DETECTION         source/initial_conditions/inivol/phase_detection.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE PHASE_PROPAGATION(IX,IY,IZ,NB_CELL_X,NB_CELL_Y,NB_CELL_Z,NB_BOX_LIMIT,CELL)
!$COMMENT
!       PHASE_PROPAGATION description
!       PHASE_PROPAGATION propagates the cell value to neighbour
!       
!       PHASE_PROPAGATION organization :
!       the propagation is done in the 3 directions
!                       can be done only if the neighbouring cells are empty 
!$ENDCOMMENT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------

C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: IX,IY,IZ ! current cell position
        INTEGER, INTENT(IN) :: NB_CELL_X,NB_CELL_Y,NB_CELL_Z ! number of cell in x/y/z direction
        INTEGER, INTENT(IN) :: NB_BOX_LIMIT ! upper limit of cell's number
        INTEGER, DIMENSION(NB_CELL_X,NB_CELL_Y,NB_CELL_Z), INTENT(INOUT) :: CELL ! phase of the voxcell
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        LOGICAL :: X_PROPAGATION,Y_PROPAGATION,Z_PROPAGATION
        INTEGER :: I,J,K
        INTEGER :: II,JJ,KK
        INTEGER :: NEXT_X,NEXT_Y,NEXT_Z
        INTEGER :: MY_PHASE
        INTEGER, DIMENSION(NB_BOX_LIMIT) :: X_LIST,Y_LIST,Z_LIST
C-----------------------------------------------

        MY_PHASE = CELL(IX,IY,IZ)
        NEXT_X = 0
        NEXT_Y = 0
        NEXT_Z = 0

        ! --------------------------
        ! upward x
        X_PROPAGATION = .TRUE.
        I = IX + 1
        IF(I>NB_CELL_X) X_PROPAGATION = .FALSE.
        DO WHILE( X_PROPAGATION )
            IF(CELL(I,IY,IZ)==0) THEN
                CELL(I,IY,IZ) = MY_PHASE
                NEXT_X = NEXT_X + 1
                X_LIST(NEXT_X) = I
                I = I + 1
            ELSE
                X_PROPAGATION = .FALSE.
            ENDIF
            IF(I>NB_CELL_X) X_PROPAGATION = .FALSE.
        ENDDO
        ! --------------------------

        ! --------------------------
        ! backward x        
        X_PROPAGATION = .TRUE.
        I = IX - 1
        IF(I<1) X_PROPAGATION = .FALSE.
        DO WHILE( X_PROPAGATION )
            IF(CELL(I,IY,IZ)==0) THEN
                CELL(I,IY,IZ) = MY_PHASE
                NEXT_X = NEXT_X + 1
                X_LIST(NEXT_X) = I
                I = I - 1
            ELSE
                X_PROPAGATION = .FALSE.
            ENDIF
            IF(I<1) X_PROPAGATION = .FALSE.
        ENDDO
        ! --------------------------

        ! --------------------------
        IF(NEXT_X>0) THEN

            ! --------------------------
            ! upward y and upward / backward z
            Y_PROPAGATION = .TRUE.
            J = IY + 1
            IF(J>NB_CELL_Y) Y_PROPAGATION = .FALSE.
            DO II=1,NEXT_X
                I = X_LIST(II)
                DO WHILE( Y_PROPAGATION )
                    IF(CELL(I,J,IZ)==0) THEN
                        CELL(I,J,IZ) = MY_PHASE
                        NEXT_Y = NEXT_Y + 1
                        Y_LIST(NEXT_Y) = J
                        J = J + 1
                    ELSE
                        Y_PROPAGATION = .FALSE.
                    ENDIF
                    IF(J>NB_CELL_Y) Y_PROPAGATION = .FALSE.
                ENDDO

                ! --------------------------
                ! upward z
                Z_PROPAGATION = .TRUE.
                K = IZ + 1
                IF(K>NB_CELL_Z) Z_PROPAGATION = .FALSE.
                DO JJ=1,NEXT_Y
                    J = Y_LIST(JJ)
                    DO WHILE( Z_PROPAGATION )
                        IF(CELL(I,J,K)==0) THEN
                            CELL(I,J,K) = MY_PHASE
                            K = K + 1
                        ELSE
                            Z_PROPAGATION = .FALSE.
                        ENDIF
                        IF(K>NB_CELL_Z) Z_PROPAGATION = .FALSE.                       
                    ENDDO
                ENDDO
                ! --------------------------

                ! --------------------------
                ! backward z
                Z_PROPAGATION = .TRUE.
                K = IZ - 1
                IF(K<1) Z_PROPAGATION = .FALSE.
                DO JJ=1,NEXT_Y
                    J = Y_LIST(JJ)
                    DO WHILE( Z_PROPAGATION )
                        IF(CELL(I,J,K)==0) THEN
                            CELL(I,J,K) = MY_PHASE
                            K = K - 1
                        ELSE
                            Z_PROPAGATION = .FALSE.
                        ENDIF
                        IF(K<1) Z_PROPAGATION = .FALSE.                       
                    ENDDO
                ENDDO
                ! --------------------------
            ENDDO
            ! --------------------------

            ! --------------------------
            ! backward y and upward / backward z
            NEXT_Y = 0
            Y_PROPAGATION = .TRUE.
            J = IY - 1
            IF(J<1) Y_PROPAGATION = .FALSE.
            DO II=1,NEXT_X
                I = X_LIST(II)
                DO WHILE( Y_PROPAGATION )
                    IF(CELL(I,J,IZ)==0) THEN
                        CELL(I,J,IZ) = MY_PHASE
                        NEXT_Y = NEXT_Y + 1
                        Y_LIST(NEXT_Y) = J
                        J = J - 1
                    ELSE
                        Y_PROPAGATION = .FALSE.
                    ENDIF
                    IF(J<1) Y_PROPAGATION = .FALSE.
                ENDDO

                ! --------------------------
                ! upward z
                Z_PROPAGATION = .TRUE.
                K = IZ + 1
                IF(K>NB_CELL_Z) Z_PROPAGATION = .FALSE.
                DO JJ=1,NEXT_Y
                    J = Y_LIST(JJ)
                    DO WHILE( Z_PROPAGATION )
                        IF(CELL(I,J,K)==0) THEN
                            CELL(I,J,K) = MY_PHASE
                            K = K + 1
                        ELSE
                            Z_PROPAGATION = .FALSE.
                        ENDIF
                        IF(K>NB_CELL_Z) Z_PROPAGATION = .FALSE.                       
                    ENDDO
                ENDDO
                ! --------------------------

                ! --------------------------
                ! backward z
                Z_PROPAGATION = .TRUE.
                K = IZ - 1
                IF(K<1) Z_PROPAGATION = .FALSE.
                DO JJ=1,NEXT_Y
                    J = Y_LIST(JJ)
                    DO WHILE( Z_PROPAGATION )
                        IF(CELL(I,J,K)==0) THEN
                            CELL(I,J,K) = MY_PHASE
                            K = K - 1
                        ELSE
                            Z_PROPAGATION = .FALSE.
                        ENDIF
                        IF(K<1) Z_PROPAGATION = .FALSE.                       
                    ENDDO
                ENDDO
                ! --------------------------
            ENDDO
        ENDIF
        ! --------------------------

        RETURN
        END SUBROUTINE PHASE_PROPAGATION
