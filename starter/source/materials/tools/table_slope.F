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
Chd|  TABLE_SLOPE                   source/materials/tools/table_slope.F
Chd|-- called by -----------
Chd|        LAW70_UPD                     source/materials/mat/mat070/law70_upd.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE4D_MOD                   ../common_source/modules/table4d_mod.F
Chd|====================================================================
      SUBROUTINE TABLE_SLOPE(TABLE,STIFFINI,STIFFMIN,STIFFMAX,XMAX)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE4D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TABLE_4D_) ,INTENT(IN) ::  TABLE
      my_real ,INTENT(OUT) :: STIFFINI,STIFFMIN,STIFFMAX  ! initial, min and max function slopes
      my_real ,INTENT(OUT) :: XMAX ! first abscissa corresponding to the max slope
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,NDIM,NPT,LEN2,LEN3,LEN4
      my_real X1,X2,Y1,Y2,DX,DY,DYDX
C=======================================================================
      ! COMPUTE INITIAL, MIN and MAX SLOPE OF A FUNCTION TABLE
C=======================================================================
      STIFFINI = ZERO
      STIFFMAX = ZERO
      STIFFMIN = EP20
      XMAX     = ZERO
      NDIM = TABLE%NDIM
      NPT  = SIZE(TABLE%X(1)%VALUES)
c
      IF (NDIM == 1) THEN
        X1  = TABLE%X(1)%VALUES(1)
        Y1  = TABLE%Y1D(1)
        IF (X1 >= ZERO) THEN
          DX = TABLE%X(1)%VALUES(2) - X1
          DY = TABLE%Y1D(2) - Y1
          STIFFINI = DY/DX
        ENDIF
        DO I = 2,NPT
          X2 = TABLE%X(1)%VALUES(I)
          Y2 = TABLE%Y1D(I)
          DX = X2 - X1
          DY = Y2 - Y1          
          DYDX = DY/DX
          IF (DYDX > STIFFMAX) THEN
            STIFFMAX = DYDX
            XMAX     = X1
          END IF
          STIFFMIN = MIN(STIFFMIN,DYDX)
          IF (X1 == ZERO .or. X2 == ZERO) THEN
            STIFFINI = MAX(STIFFINI, DYDX)
          ENDIF
          X1 = X2
          Y1 = Y2
        ENDDO
c        
      ELSE IF (NDIM == 2) THEN
        LEN2 = SIZE(TABLE%X(2)%VALUES)
        X1   = TABLE%X(1)%VALUES(1)
        Y1   = TABLE%Y2D(1,1)
        DO I = 2,NPT
          X2 = TABLE%X(1)%VALUES(I)
          DX = X2 - X1
          DO J = 1,LEN2
            Y2 = TABLE%Y2D(I,J)
            DY = Y2 - Y1          
            DYDX = DY/DX
            IF (DYDX > STIFFMAX) THEN
              STIFFMAX = DYDX
              XMAX     = X1
            END IF
            STIFFMIN = MIN(STIFFMIN,DYDX)
            IF (X1 == ZERO .or. X2 == ZERO) THEN
              STIFFINI = MAX(STIFFINI, DYDX)
            ENDIF
            Y1 = Y2
          ENDDO      
          X1 = X2
        ENDDO
c        
      ELSE IF (NDIM == 3) THEN
        LEN2 = SIZE(TABLE%X(2)%VALUES)
        LEN3 = SIZE(TABLE%X(3)%VALUES)
        X1   = TABLE%X(1)%VALUES(1)
        DO I = 2,NPT
          X2 = TABLE%X(1)%VALUES(I)
          DX = X2 - X1
          DO J = 1,LEN2
            DO K = 1,LEN3
              Y1 = TABLE%Y3D(I-1,J,K)
              Y2 = TABLE%Y3D(I  ,J,K)
              DY = Y2 - Y1          
              DYDX = DY/DX
              IF (DYDX > STIFFMAX) THEN
                STIFFMAX = DYDX
                XMAX     = X1
              END IF
              STIFFMIN = MIN(STIFFMIN,DYDX)
              IF (X1 == ZERO .or. X2 == ZERO) THEN
                STIFFINI = MAX(STIFFINI, DYDX)
              ENDIF
            ENDDO      
          ENDDO      
          X1 = X2
        ENDDO
c        
      ELSE IF (NDIM == 4) THEN
        LEN2 = SIZE(TABLE%X(2)%VALUES)
        LEN3 = SIZE(TABLE%X(3)%VALUES)
        LEN4 = SIZE(TABLE%X(4)%VALUES)
        X1   = TABLE%X(1)%VALUES(1)
        DO I = 2,NPT
          X2 = TABLE%X(1)%VALUES(I)
          DX = X2 - X1
          DO J = 1,LEN2
            DO K = 1,LEN3
              DO L = 1,LEN4
                Y1 = TABLE%Y4D(I-1,J,K,L)
                Y2 = TABLE%Y4D(I  ,J,K,L)
                DY = Y2 - Y1          
                DYDX = DY/DX
                IF (DYDX > STIFFMAX) THEN
                  STIFFMAX = DYDX
                  XMAX     = X1
                END IF
                STIFFMIN = MIN(STIFFMIN,DYDX)
                IF (X1 == ZERO .or. X2 == ZERO) THEN
                  STIFFINI = MAX(STIFFINI, DYDX)
                ENDIF
              ENDDO      
            ENDDO      
          ENDDO      
          X1 = X2
        ENDDO
c
      END IF
c-----------
      RETURN
      END
