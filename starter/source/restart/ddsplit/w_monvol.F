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
Chd|  W_MONVOL                      source/restart/ddsplit/w_monvol.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|====================================================================
      SUBROUTINE W_MONVOL(MONVOL,LENVOLU,NODLOCAL,LEN_IA,MONVOL_L,ITAB,T_MONVOL,PROC)
      USE MONVOL_STRUCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MONVOL(*), NODLOCAL(*), LENVOLU, LEN_IA,
     .        MONVOL_L(LENVOLU),ITAB(*)
      TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(IN) :: T_MONVOL
      INTEGER, INTENT(in) :: PROC
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISHIFT, I, J, N, NJ, NJ1, NJ2, NJ3, IADJET, NJET
      INTEGER :: SIZE_L
      INTEGER, DIMENSION(NVOLU) :: NTRI,NTRI_LOCAL
      INTEGER, DIMENSION(:), ALLOCATABLE :: TRI_L
C
      ISHIFT = NIMV*NVOLU+LICBAG
c      DO I = 1, LENVOLU
c        MONVOL_L(I) = MONVOL(I)
c      END DO
      DO I = 1, NVOLU
         NTRI(I) = T_MONVOL(I)%NB_FILL_TRI
        NJET   = MONVOL(NIMV*(I-1)+8)
        IADJET = MONVOL(NIMV*(I-1)+9)
        DO NJ = 1, NJET
          NJ1 = MONVOL(ISHIFT+IADJET+(NJ-1)*NIBJET+5)
          NJ2 = MONVOL(ISHIFT+IADJET+(NJ-1)*NIBJET+6)
          NJ3 = MONVOL(ISHIFT+IADJET+(NJ-1)*NIBJET+7)
          IF(NJ1/=0)THEN
            NJ1 = NODLOCAL(NJ1)
            IF(NJ1/=0)MONVOL_L(ISHIFT+IADJET+(NJ-1)*NIBJET+5)=NJ1
          END IF
          IF(NJ2/=0)THEN
            NJ2 = NODLOCAL(NJ2)
            IF(NJ2/=0)MONVOL_L(ISHIFT+IADJET+(NJ-1)*NIBJET+6)=NJ2
          END IF
          IF(NJ3/=0)THEN
            NJ3 = NODLOCAL(NJ3)
            IF(NJ3/=0)MONVOL_L(ISHIFT+IADJET+(NJ-1)*NIBJET+7)=NJ3
          END IF
        END DO
      END DO
C
      CALL WRITE_I_C(MONVOL_L,LENVOLU)
      LEN_IA = LEN_IA + LENVOLU

      DO I = 1, NVOLU
        NTRI_LOCAL(I) = T_MONVOL(I)%NUMBER_TRI_PER_PROC(PROC)
      ENDDO
      CALL WRITE_I_C(NTRI_LOCAL, NVOLU)
      LEN_IA = LEN_IA + NVOLU

      DO I = 1, NVOLU
         IF (NTRI(I) > 0) THEN
            ALLOCATE(TRI_L(3 * NTRI(I)))
            SIZE_L = 0
            DO J = 1, NTRI(I)
               IF (NODLOCAL(T_MONVOL(I)%FILL_TRI(3 * (J - 1) + 1)) /= 0 .AND.
     .              NODLOCAL(T_MONVOL(I)%FILL_TRI(3 * (J - 1) + 2)) /= 0 .AND.
     .              NODLOCAL(T_MONVOL(I)%FILL_TRI(3 * (J - 1) + 3)) /= 0) THEN
                  SIZE_L = SIZE_L + 3
                  TRI_L(3 * (J - 1) + 1) = NODLOCAL(T_MONVOL(I)%FILL_TRI(3 * (J - 1) + 1))
                  TRI_L(3 * (J - 1) + 2) = NODLOCAL(T_MONVOL(I)%FILL_TRI(3 * (J - 1) + 2))
                  TRI_L(3 * (J - 1) + 3) = NODLOCAL(T_MONVOL(I)%FILL_TRI(3 * (J - 1) + 3))
               ENDIF
            ENDDO
            IF (SIZE_L > 0 ) THEN
               CALL WRITE_I_C(TRI_L(1:SIZE_L), SIZE_L)
               LEN_IA = LEN_IA + SIZE_L
            ENDIF
            DEALLOCATE(TRI_L)
         ENDIF
      ENDDO
C
      RETURN
      END
