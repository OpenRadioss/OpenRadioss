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
!||    w_fbft     ../starter/source/restart/ddsplit/w_fbft.F
!||--- called by ------------------------------------------------------
!||    ddsplit    ../starter/source/restart/ddsplit/ddsplit.F
!||--- calls      -----------------------------------------------------
!||    nlocal     ../starter/source/spmd/node/ddtools.F
!||====================================================================
      SUBROUTINE W_FBFT(VEL,NC,IBFT,NFXTEMP_L,LEN_AM,PROC,
     .           NIFT,LFACTHER)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NIFT
      INTEGER ,INTENT(IN) :: LFACTHER
      INTEGER :: NC, LEN_AM, PROC, NFXTEMP_L
      INTEGER :: IBFT(NIFT,*)
      my_real :: VEL(LFACTHER,*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,N,N_L 
      my_real :: VELTMP(LFACTHER,NFXTEMP_L)
C-----------------------------------------------
      N_L = 0
      DO I = 1, NC
        N = IABS(IBFT(1,I))
        IF(NLOCAL(N,PROC+1)==1)THEN
          N_L = N_L + 1
          DO J = 1, LFACTHER
            VELTMP(J,N_L) = VEL(J,I)
          ENDDO
        ENDIF
      ENDDO
C
      CALL WRITE_DB(VELTMP,LFACTHER*NFXTEMP_L)
      LEN_AM = LEN_AM + LFACTHER*NFXTEMP_L
C
      RETURN
      END
