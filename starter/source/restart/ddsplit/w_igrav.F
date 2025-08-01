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
C
!||====================================================================
!||    w_igrav     ../starter/source/restart/ddsplit/w_igrav.F
!||--- called by ------------------------------------------------------
!||    ddsplit     ../starter/source/restart/ddsplit/ddsplit.F
!||--- calls      -----------------------------------------------------
!||====================================================================
      SUBROUTINE W_IGRAV(
     .  IGRAV,LGRAV,NODLOCAL,PROC,LLGRAV_L,LEN_IA,NUMNOD_L)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PROC, LEN_IA, LLGRAV_L,NUMNOD_L,
     .        IGRAV(NIGRV,*), LGRAV(*), NODLOCAL(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
!      INTEGER  NLOCAL
!      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IAD, IADL, NL, N, NLL, NN, IGRTMP(NIGRV,NGRAV)
      INTEGER, DIMENSION(:), ALLOCATABLE :: LGRTMP ! LLGRAV_L
C
      ALLOCATE(LGRTMP(LLGRAV_L))
      IADL = 1
      DO I = 1, NGRAV
        NL = IGRAV(1,I)
        IAD= IGRAV(4,I)
        DO J = 1, NIGRV
          IGRTMP(J,I) = IGRAV(J,I)
        ENDDO
        NLL = 0
        DO J = 1, NL
          NN = LGRAV(IAD+J-1)
          N = ABS(NN)
          IF (NODLOCAL(N)/=0.AND.NODLOCAL(N)<=NUMNOD_L)THEN
            IF(NN>0) THEN
              LGRTMP(IADL+NLL) = NODLOCAL(N)
            ELSE
              LGRTMP(IADL+NLL) = -NODLOCAL(N)
            ENDIF
            NLL = NLL + 1
          ENDIF
        ENDDO
        IGRTMP(1,I) = NLL
        IGRTMP(4,I) = IADL
        IADL = IADL+NLL
      ENDDO
C
      CALL WRITE_I_C(IGRTMP,NIGRV*NGRAV)
      CALL WRITE_I_C(LGRTMP,IADL-1)
      LEN_IA = LEN_IA + NIGRV*NGRAV + IADL-1

      DEALLOCATE(LGRTMP)
C
      RETURN
      END
