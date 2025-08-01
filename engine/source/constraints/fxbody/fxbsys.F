Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    fxbsys     ../engine/source/constraints/fxbody/fxbsys.F
!||--- called by ------------------------------------------------------
!||    fxbodvp1   ../engine/source/constraints/fxbody/fxbodvp.F
!||--- calls      -----------------------------------------------------
!||    arret      ../engine/source/system/arret.F
!||====================================================================
      SUBROUTINE FXBSYS(MT, ST, N)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N
      my_real
     .   MT(N,*), ST(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,I1,M,K,INFO,IPIV(N)
#ifndef MYREAL8
          DOUBLE PRECISION :: MTDB(N,N)
          DOUBLE PRECISION :: STDB(N)
#endif
C
#ifdef WITHOUT_LINALG
      WRITE(6,*) "Error: Lapack needed for Flexible bodies"
      CALL FLUSH(6)
            CALL ARRET(5)
#else
#ifdef MYREAL8
      CALL DGETRF(N,N,MT,N,IPIV,INFO)
      CALL DGETRS('N',N,1,MT,N,IPIV,ST,N,INFO)
#else
      MTDB(1:N,1:N) = MT(1:N,1:N)
      CALL DGETRF(N,N,MTDB,N,IPIV,INFO)
      STDB(1:N) = ST(1:N)
      CALL DGETRS('N',N,1,MTDB,N,IPIV,STDB,N,INFO)
      ST(1:N) = STDB(1:N)
#endif
#endif
C      
      END
