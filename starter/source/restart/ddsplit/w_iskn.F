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
CCgw|============================================================
Chd|====================================================================
Chd|  W_ISKN                        source/restart/ddsplit/w_iskn.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE W_ISKN(ISKN,NUMSKW,NSPH  ,IFRAME,NUMFRAM,NODLOCAL,
     2                  P   ,NUMNOD,CEPSP  ,LEN_IA  )
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
#include      "sphcom.inc"
#include      "submod_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER P, NUMNOD, NUMSKW, NSPH, NUMFRAM, LEN_IA,
     .        ISKN(LISKN,*), NODLOCAL(*), CEPSP(*),
     .        IFRAME(LISKN,*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
      INTEGER I, IDEB, N1, N2, N3, N4, N5, N6, N_L,
     .        ISKN_L(LISKN,1+NUMSKW+NSUBMOD+NSPH+NUMFRAM+1)
C
      ISKN_L(1,1) = ISKN(1,1)
      ISKN_L(2,1) = ISKN(2,1)
      ISKN_L(3,1) = ISKN(3,1)
      ISKN_L(4,1) = ISKN(4,1)
      ISKN_L(5,1) = ISKN(5,1)
      ISKN_L(6,1) = ISKN(6,1)
C
      DO I = 1, NUMSKW
        N1 = ISKN(1,I+1)
        N2 = ISKN(2,I+1)
        N3 = ISKN(3,I+1)
        N4 = ISKN(4,I+1)
        N5 = ISKN(5,I+1)
        N6 = ISKN(6,I+1)
C
        IF(N1/=0)THEN
          IF(NLOCAL(N1,P)==1) N1 = NODLOCAL(N1)
C
        END IF
C
        IF(N2/=0)THEN
          IF(NLOCAL(N2,P)==1) N2 = NODLOCAL(N2)
C
        END IF
C
        IF(N3/=0) THEN
          IF(NLOCAL(N3,P)==1) N3 = NODLOCAL(N3)
C
        END IF
        ISKN_L(1,I+1) = N1
        ISKN_L(2,I+1) = N2
        ISKN_L(3,I+1) = N3
        ISKN_L(4,I+1) = N4
        ISKN_L(5,I+1) = N5
        ISKN_L(6,I+1) = N6
      ENDDO
C
      IDEB = 1+NUMSKW
C
      IF(NSPCOND>0)THEN
        N_L = 0
        DO I = 1, NUMSPH
          IF(CEPSP(I)+1==P)THEN
            N_L = N_L + 1
            ISKN_L(1,IDEB+N_L) = ISKN(1,I+IDEB)
            ISKN_L(2,IDEB+N_L) = ISKN(2,I+IDEB)
            ISKN_L(3,IDEB+N_L) = ISKN(3,I+IDEB)
            ISKN_L(4,IDEB+N_L) = ISKN(4,I+IDEB)
            ISKN_L(5,IDEB+N_L) = ISKN(5,I+IDEB)
            ISKN_L(6,IDEB+N_L) = ISKN(6,I+IDEB)
          END IF
        END DO
C NSPH = MIN(IUN,NSPCOND)*NUMSPH_L
        IDEB = IDEB + NSPH
      END IF
C submodel skew
      IF(NSUBMOD>0)THEN
        N_L = 0
        DO I = 1, NSUBMOD
          N_L = N_L + 1
          ISKN_L(1,IDEB+N_L) = ISKN(1,I+IDEB)
          ISKN_L(2,IDEB+N_L) = ISKN(2,I+IDEB)
          ISKN_L(3,IDEB+N_L) = ISKN(3,I+IDEB)
          ISKN_L(4,IDEB+N_L) = ISKN(4,I+IDEB)
          ISKN_L(5,IDEB+N_L) = ISKN(5,I+IDEB)
          ISKN_L(6,IDEB+N_L) = ISKN(6,I+IDEB)
        END DO
        IDEB = IDEB + NSUBMOD
      END IF
C
      DO I = 1, NUMFRAM+1
        N1 = IFRAME(1,I)
        N2 = IFRAME(2,I)
        N3 = IFRAME(3,I)
        IF(N1/=0)THEN
          IF(NLOCAL(N1,P)==1) N1 = NODLOCAL(N1)
        END IF
        IF(N2/=0)THEN
          IF(NLOCAL(N2,P)==1) N2 = NODLOCAL(N2)
        END IF
        IF(N3/=0) THEN
          IF(NLOCAL(N3,P)==1) N3 = NODLOCAL(N3)
        END IF
        ISKN_L(1,I+IDEB) = N1
        ISKN_L(2,I+IDEB) = N2
        ISKN_L(3,I+IDEB) = N3
        ISKN_L(4,I+IDEB) = IFRAME(4,I)
        ISKN_L(5,I+IDEB) = IFRAME(5,I)
        ISKN_L(6,I+IDEB) = IFRAME(6,I)
      ENDDO
C
      CALL WRITE_I_C(ISKN_L,LISKN*(NUMSKW+1+NSPH+NSUBMOD+NUMFRAM+1))

      LEN_IA = LEN_IA + LISKN*(NUMSKW+1+NSPH+NSUBMOD+NUMFRAM+1)
C
      RETURN
      END
