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
Chd|  EBCS_EXTRAPOL                 source/boundary_conditions/ebcs/ebcs_extrapol.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        EBCS_EXTRAP2                  source/boundary_conditions/ebcs/ebcs_extrapol.F
Chd|        EBCS_MOD                      ../common_source/modules/ebcs_mod.F
Chd|====================================================================
      SUBROUTINE EBCS_EXTRAPOL(FV, NP, TF, EBCS_TAB)
      USE EBCS_MOD
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Function f for EBCS is defined for times from
C tmin to tmax
C When current time t is such that t < tmin value
C for F is taken as F(tmin).
C When current time t is such that t > tmax value
C for F is taken as F(tmax).
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) ::  NP(NFUNCT)
      my_real, INTENT(IN) :: FV(*), TF(*)
      TYPE(t_ebcs_tab), TARGET, INTENT(IN) :: EBCS_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, NPOINT
      INTEGER :: EBCS_EXTRAP(NFUNCT)
      CLASS(t_ebcs), POINTER :: EBCS

      DO I = 1, NFUNCT
         EBCS_EXTRAP(I) = 0
      ENDDO

      DO I = 1, NEBCS
         IF(.NOT.EBCS_TAB%need_to_compute(I)) CYCLE
         EBCS => EBCS_TAB%tab(i)%poly
         SELECT TYPE (EBCS)
           TYPE IS (t_ebcs_gradp0)
             IF (EBCS%irho > 0) EBCS_EXTRAP(EBCS%irho) = 1
             IF (EBCS%iener > 0) EBCS_EXTRAP(EBCS%iener) = 1
             IF (EBCS%ipres > 0) EBCS_EXTRAP(EBCS%ipres) = 1
           TYPE IS (t_ebcs_pres)
             IF (EBCS%irho > 0) EBCS_EXTRAP(EBCS%irho) = 1
             IF (EBCS%iener > 0) EBCS_EXTRAP(EBCS%iener) = 1
             IF (EBCS%ipres > 0) EBCS_EXTRAP(EBCS%ipres) = 1
           TYPE IS (t_ebcs_valvin)
             IF (EBCS%irho > 0) EBCS_EXTRAP(EBCS%irho) = 1
             IF (EBCS%iener > 0) EBCS_EXTRAP(EBCS%iener) = 1
             IF (EBCS%ipres > 0) EBCS_EXTRAP(EBCS%ipres) = 1
           TYPE IS (t_ebcs_valvout)
             IF (EBCS%irho > 0) EBCS_EXTRAP(EBCS%irho) = 1
             IF (EBCS%iener > 0) EBCS_EXTRAP(EBCS%iener) = 1
             IF (EBCS%ipres > 0) EBCS_EXTRAP(EBCS%ipres) = 1
           TYPE IS (t_ebcs_vel)
             IF (EBCS%irho > 0) EBCS_EXTRAP(EBCS%irho) = 1
             IF (EBCS%iener > 0) EBCS_EXTRAP(EBCS%iener) = 1
             IF (EBCS%ivx > 0) EBCS_EXTRAP(EBCS%ivx) = 1
             IF (EBCS%ivy > 0) EBCS_EXTRAP(EBCS%ivy) = 1
             IF (EBCS%ivz > 0) EBCS_EXTRAP(EBCS%ivz) = 1
           TYPE IS (t_ebcs_normv)
             IF (EBCS%irho > 0) EBCS_EXTRAP(EBCS%irho) = 1
             IF (EBCS%iener > 0) EBCS_EXTRAP(EBCS%iener) = 1
             IF (EBCS%ivimp > 0) EBCS_EXTRAP(EBCS%ivimp) = 1
           CLASS DEFAULT
        END SELECT
      ENDDO

      DO  N=1,NFUNCT
         IF (EBCS_EXTRAP(N) == 1) THEN
            NPOINT=(NP(N+1)-NP(N))/2
            CALL EBCS_EXTRAP2(TF(NP(N)),TT,NPOINT,FV(N))
         ENDIF
      ENDDO

      RETURN
      END SUBROUTINE EBCS_EXTRAPOL

Chd|====================================================================
Chd|  EBCS_EXTRAP2                  source/boundary_conditions/ebcs/ebcs_extrapol.F
Chd|-- called by -----------
Chd|        EBCS_EXTRAPOL                 source/boundary_conditions/ebcs/ebcs_extrapol.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE EBCS_EXTRAP2(TF,TT,NPOINT,F)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPOINT
C     REAL
      my_real
     .   TT, F
C     REAL
      my_real
     .   TF(2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, M
C     REAL
      my_real
     .   DT1, DT2, D1, D2, TG

      !!! If tt (=current time) is lower than tmin (=TF(1,1)) then take first value
      !!! If tt (=current time) is greater than tmax (=TF(1,NPOINT)= then take last value

      IF (TT <= TF(1, 1)) THEN
         F = TF(2,1)
      ENDIF
      IF (TT >= TF(1, NPOINT)) THEN
         F = TF(2, NPOINT)
      ENDIF

      RETURN
      END SUBROUTINE EBCS_EXTRAP2
