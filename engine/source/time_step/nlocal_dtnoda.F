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
Chd|  NLOCAL_DTNODA                 source/time_step/nlocal_dtnoda.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|====================================================================
      SUBROUTINE NLOCAL_DTNODA(NODFT    ,NODLT    ,NLOC_DMG ,DTNOD_NLOCAL, DT2T )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE NLOCAL_REG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr02_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT, NODLT
      TYPE(NLOCAL_STR_), TARGET :: NLOC_DMG 
      my_real
     .    DT2T,DTNOD_NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,I,NDDL,NPOS
      my_real, DIMENSION(:), ALLOCATABLE ::
     .  DTN
      my_real, POINTER, DIMENSION(:) :: 
     .  MASS,STIFN
      my_real
     .  DTNOD,DT2P
      ! Safety coefficient for non-local stability vs mechanical stability
      ! (it has been slightly increased vs nloc_dmg_init.F)
      my_real, PARAMETER :: CSTA  = 40.0D0
      ! Coefficient for non-local stability to take into account damping
      my_real, PARAMETER :: CDAMP = 0.7D0
C  
      IF (NODADT > 0) THEN
C
        ! Recovering nodal stiffness and masses
        STIFN => NLOC_DMG%STIFNL(1:NLOC_DMG%L_NLOC,1)
        MASS  => NLOC_DMG%MASS(1:NLOC_DMG%L_NLOC)
C
        ! Allocating non-local nodal timestep table
        ALLOCATE(DTN(NLOC_DMG%L_NLOC))
        DTN(1:NLOC_DMG%L_NLOC) = ZERO   
        DTNOD = EP20
C
        ! Determining the minimum of non-local nodal timestep
        DO N = NODFT,NODLT
          ! Position of the first additional dof
          NPOS = NLOC_DMG%POSI(N)
          ! Number of additional degrees of freedom
          NDDL = NLOC_DMG%POSI(N+1) - NPOS
          ! Computation of the non-local nodal timestep
          DO I = NPOS,NPOS+NDDL-1
            DTN(I) = DTFAC1(11)*CDAMP*SQRT(TWO*MASS(I)/MAX(STIFN(I),EM20))
            DTNOD  = MIN(DTNOD,DTN(I))
          ENDDO    
        ENDDO
#include "lockon.inc"
        DTNOD_NLOCAL = MIN(DTNOD_NLOCAL,DTNOD)
#include "lockoff.inc"
C
        CALL MY_BARRIER()
        DTNOD = DTNOD_NLOCAL
        ! /DT/NODA/CSTX - Constant timestep with added mass
        IF ((IDTMIN(11)==3).OR.(IDTMIN(11)==4).OR.(IDTMIN(11)==8)) THEN 
          ! Added mass computation if necessary
          IF (DTNOD < DTMIN1(11)*(SQRT(CSTA))) THEN
            DO N = NODFT,NODLT
              ! Position of the first additional dof
              NPOS = NLOC_DMG%POSI(N)
              ! Number of additional degrees of freedom
              NDDL = NLOC_DMG%POSI(N+1) - NPOS
              ! Computation of the new non-local mass
              DO I = NPOS,NPOS+NDDL-1
                IF (DTN(I) < DTMIN1(11)) THEN
                  DT2P    = DTMIN1(11)/(DTFAC1(11)*CDAMP)
                  MASS(I) = CSTA*MAX(MASS(I),HALF * STIFN(I) * DT2P * DT2P * ONEP00001)
                ENDIF
              ENDDO
            ENDDO
          ENDIF
          DTNOD = DTMIN1(11)*(SQRT(CSTA))
        ENDIF 
C
        ! Classical nodal timestep check
        IF (DTNOD < DT2T) THEN
          DT2T = MIN(DT2T,DTNOD)
        ENDIF
C       
        ! Resetting non-local nodal stiffness
        DO N = NODFT,NODLT
          ! Position of the first additional dof
          NPOS = NLOC_DMG%POSI(N)
          ! Number of additional degrees of freedom
          NDDL = NLOC_DMG%POSI(N+1) - NPOS
          ! Resetting nodal stiffness
          STIFN(NPOS:NPOS+NDDL-1) = EM20
        ENDDO
C
        ! Deallocation
        IF (ALLOCATED(DTN)) DEALLOCATE(DTN)
C
      ENDIF
      END