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
Chd|               source/tools/curve/vw_smooth.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE VW_SMOOTH(NPT,NTARGET,X,Y)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
      INTEGER                     ,INTENT(INOUT)  :: NPT      ! number of input function points
      INTEGER                     ,INTENT(IN)     :: NTARGET  ! target length of abscissa vector
      my_real ,DIMENSION(NPT)     ,INTENT(INOUT)  :: X,Y
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,I0,I1,I2,IDX,IMIN,IPREV,INEXT,ITER,NITER,IZERO
      my_real :: AMIN,AA,S,T
      INTEGER ,DIMENSION(NPT) :: PREV,NEXT
      my_real ,DIMENSION(NPT) :: AREA
c-----------------------------------------------
c     smooth input curve using Visvalingam-Whyatt algorithm 
c     reduce number of points from NPT to NTARGET
c     first and last points are never eliminated, as well as (0,0)
c=======================================================================
      NITER = NPT - NTARGET
      IF (NITER < 1) RETURN
      
      PREV(1)   = 0
      NEXT(1)   = 2
      AREA(1)   = INFINITY / TEN
      PREV(NPT) = NPT - 1
      NEXT(NPT) = 10000000
      AREA(NPT) = INFINITY / TEN 
c
      DO I = 2,NPT-1
        I1 = I-1
        I2 = I+1
        PREV(I) = I1
        NEXT(I) = I2
        IF (X(I) == ZERO .and. Y(I) == ZERO) THEN
          AREA(I) = INFINITY / TEN
        ELSE
          AREA(I) = X(I1)*Y(I)  + X(I)*Y(I2) + X(I2)*Y(I1)
     .            - X(I1)*Y(I2) - X(I)*Y(I1) - X(I2)*Y(I)
          AREA(I)  = ABS(AREA(I))
        END IF
      END DO
c----------------------------------------------------      
c     eliminate points with min area until NPT = target number of points
c----------------------------------------------------      
      DO ITER = 1,NITER                           
        AMIN = INFINITY
        IDX  = 1
        DO I=1,NPT
          IF (AREA(I) < AMIN) THEN
            AMIN = AREA(I)
            IMIN = I
          ENDIF
        END DO      
c     
        IPREV = PREV(IMIN)
        INEXT = NEXT(IMIN)      
        NEXT(IPREV) = INEXT
        PREV(INEXT) = IPREV     
        AREA(IMIN)  = INFINITY
c
        ! recalculate PREV POINT AREA
        I0 = IPREV
        I1 = PREV(IPREV)
        I2 = INEXT
        IF (I1 > 0) THEN
          AA = X(I0)*Y(I0) + X(I0)*Y(I2) + X(I2)*Y(I1) 
     .       - X(I0)*Y(I2) - X(I0)*Y(I1) - X(I2)*Y(I0)
          AREA(I0) = ABS(AA)
        END IF
c
        ! recalculate NEXT POINT AREA
        I0 = INEXT                                                                 
        I1 = IPREV                                                                 
        I2 = NEXT(INEXT)                                               
        IF (I2 < NPT) THEN                                                   
          AA = X(I0)*Y(I0) + X(I0)*Y(I2) + X(I2)*Y(I1) 
     .       - X(I0)*Y(I2) - X(I0)*Y(I1) - X(I2)*Y(I0)
          AREA(I0) = ABS(AA)
        END IF
      END DO   ! ITER
c------------   
      IDX = 0
      DO I=1,NPT
        IF (AREA(I) < INFINITY) THEN
          IDX = IDX + 1
          X(IDX) = X(I)
          Y(IDX) = Y(I)
        END IF
      END DO
      NPT = 0
      IZERO = 0
      DO I = 1,IDX
        S = X(I)
        T = Y(I)
        IF (S <  ZERO .and. T >  ZERO .or.
     .      S >  ZERO .and. T <  ZERO .or.
     .      S == ZERO .and. T /= ZERO) THEN
          IF (IZERO == 1) CONTINUE
          S = ZERO
          T = ZERO
          IZERO = 1
        ELSE IF (S < ZERO .and. T < ZERO .and.
     .           X(I+1) > ZERO .and. Y(I+1) > ZERO) THEN ! .or. 
!     .           S > ZERO .and. T > ZERO .and.
!     .           X(I-1) < ZERO .and. Y(I-1) < ZERO .and. I > 1) THEN
          IF (IZERO == 1) CONTINUE
          S = ZERO
          T = ZERO
          IZERO = 1
        END IF
        NPT = NPT + 1
        X(NPT) = S
        Y(NPT) = T
      END DO
c-----------
      RETURN
      END
      
