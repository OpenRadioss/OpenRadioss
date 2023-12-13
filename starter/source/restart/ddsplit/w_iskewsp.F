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
C
Chd|====================================================================
Chd|  W_ISKEWSP                     source/restart/ddsplit/w_iskewsp.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|====================================================================
      SUBROUTINE W_ISKEWSP(ISKEW,NODGLOB,NUMNOD_L,NUMSPH_L,CEPSP,PROC,LEN_IA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISKEW(*), NODGLOB(*), CEPSP(*), PROC, LEN_IA
      INTEGER,INTENT(IN) :: NUMSPH_L, NUMNOD_L
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NL, NG, ISK
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITMP,LOCAL
C-----------------------------------------------
C
! --------------------------------
!     allocate 1d array
      ALLOCATE( ITMP(NUMNOD_L),LOCAL(NUMSPH) )
! --------------------------------
      NL = 0
      DO I = 1, NUMSPH
        IF(CEPSP(I)==PROC)THEN
          NL = NL + 1
          LOCAL(I) = NL
        ELSE
          LOCAL(I) = 0
        END IF
      END DO
C
      DO I = 1, NUMNOD_L
        ISK = ISKEW(NODGLOB(I))
        IF(ISK<=NUMSKW+1)THEN     ! no skew classique
          ITMP(I) = ISK
        ELSEIF(ISK <= NUMSKW+NUMSPH+1 .AND.NSPCOND > 0 .AND. NUMSPH > 0 )THEN                      ! no skew sph
          NG = ISK-NUMSKW-1
          NL = LOCAL(NG)
          IF(NL==0)print*,'error in sph decomp'
          ITMP(I)=NL+NUMSKW+1
        ELSE
          ITMP(I) = ISK-NUMSKW-NUMSPH+NUMSPH_L      
        END IF
      END DO
C
      CALL WRITE_I_C(ITMP,NUMNOD_L)
      LEN_IA = LEN_IA + NUMNOD_L
C
! --------------------------------
!     deallocate 1d array
      DEALLOCATE( ITMP,LOCAL )
! --------------------------------
      RETURN
      END
