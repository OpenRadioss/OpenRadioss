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
Chd|  DTCDK_REG                     source/elements/sh3n/coquedk/dtcdk_reg.F
Chd|-- called by -----------
Chd|        CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
Chd|-- calls ---------------
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|====================================================================
      SUBROUTINE DTCDK_REG(NLOC_DMG,THK     ,NEL     ,OFF     ,
     .                     LE      ,IMAT    ,NDDL    ,DT2T    )
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE NLOCAL_REG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "scr02_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER                                    :: NEL,IMAT,NDDL
      my_real, INTENT(INOUT)                     ::
     .  DT2T
      my_real, DIMENSION(NEL), INTENT(IN)        :: 
     .  LE,OFF,THK
      TYPE(NLOCAL_STR_), TARGET                  :: NLOC_DMG 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real 
     .  L2,ZETA,DTNL_TH,DTNL,DAMPFAC,LE_MAX
      ! Coefficient for non-local stability to take into account damping
      my_real, PARAMETER :: CDAMP = 0.7D0
C=======================================================================
      ! Recovering non-local parameters
      L2     = NLOC_DMG%LEN(IMAT)**2 ! Non-local internal length ** 2
      ZETA   = NLOC_DMG%DENS(IMAT)   ! Non-local density
      LE_MAX = NLOC_DMG%LE_MAX(IMAT) ! Maximal length of convergence
c      
      !-----------------------------------------------------------------------
      ! Computing non-local timestep
      !-----------------------------------------------------------------------
      IF (NODADT == 0) THEN
        DO I = 1,NEL
          ! If the element is not broken, normal computation
          IF (OFF(I)/=ZERO) THEN
            ! Non-local critical time-step in the plane
            DTNL = (TWO*(MIN(LE(I),LE_MAX))*SQRT(THREE*ZETA))/
     .                  SQRT(TWELVE*L2 + (MIN(LE(I),LE_MAX))**2)
            ! Non-local critical time-step in the thickness
            IF (NDDL>1) THEN
              IF (NDDL > 2) THEN
                DTNL_TH = (TWO*(MIN(THK(I)/NDDL,LE_MAX))*SQRT(THREE*ZETA))/
     .                       SQRT(TWELVE*L2 + (MIN(THK(I)/NDDL,LE_MAX))**2)
              ELSE
                DTNL_TH = (TWO*(MIN(THK(I),LE_MAX))*SQRT(THREE*ZETA))/
     .                       SQRT(TWELVE*L2 + (MIN(THK(I),LE_MAX))**2)
              ENDIF
            ELSE
              DTNL_TH = EP20
            ENDIF
            ! Retaining the minimal value
            DT2T = MIN(DT2T,DTFAC1(1)*CDAMP*DTNL_TH,DTFAC1(1)*CDAMP*DTNL)
          ENDIF
        ENDDO
      ENDIF
c
      END
