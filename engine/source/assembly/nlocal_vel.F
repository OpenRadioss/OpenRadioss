Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    nlocal_vel       ../engine/source/assembly/nlocal_vel.F
!||--- called by ------------------------------------------------------
!||    resol            ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    nlocal_reg_mod   ../common_source/modules/nlocal_reg_mod.F
!||====================================================================
      SUBROUTINE NLOCAL_VEL(NLOC_DMG, NODFT   ,NODLT)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE NLOCAL_REG_MOD
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT, NODLT
      TYPE(NLOCAL_STR_), TARGET :: NLOC_DMG 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  N,NPOS,NDDL
      my_real, POINTER, DIMENSION(:) :: 
     .  VEL,VEL0,FNL
C-----------------------------------------------
c     calculation of velocity of the non-local variable
c=======================================================================c=======================================================================
      ! Recovering velocities and forces
      VEL  => NLOC_DMG%VNL(1:NLOC_DMG%L_NLOC)
      VEL0 => NLOC_DMG%VNL_OLD(1:NLOC_DMG%L_NLOC)
      FNL  => NLOC_DMG%FNL(1:NLOC_DMG%L_NLOC,1)
      ! Loop over current nodes
      DO N = NODFT,NODLT
        ! Position of the first additional dof
        NPOS = NLOC_DMG%POSI(N)
        ! Number of additional degrees of freedom
        NDDL = NLOC_DMG%POSI(N+1) - NPOS
        ! Save old values of velocities
        VEL0(NPOS:NPOS+NDDL-1) = VEL(NPOS:NPOS+NDDL-1)
        ! Updating velocities and forces
        VEL(NPOS:NPOS+NDDL-1)  = VEL(NPOS:NPOS+NDDL-1) + DT12*FNL(NPOS:NPOS+NDDL-1)
        ! Setting the forces to zero (ready for next cycle's accumulation)
        FNL(NPOS:NPOS+NDDL-1)  = ZERO
        ! Reset nodal stiffness for next cycle: STIFNL accumulates += each cycle so must
        ! be cleared here exactly as FNL is, so NLOCAL_DTNODA sees the current-cycle value only.
!       IF (NODADT > 0 .AND. ALLOCATED(NLOC_DMG%STIFNL)) THEN
!         NLOC_DMG%STIFNL(NPOS:NPOS+NDDL-1,1) = ZERO
!       END IF
      ENDDO

!      ! Debug: report the single node with the largest |VNL| after the update
!      BLOCK
!        INTEGER :: N_MAX, NPOS_MAX, NDDL_MAX, N_DBG
!        my_real :: V_MAX, V_CUR
!        N_MAX   = NODFT
!        NPOS_MAX= NLOC_DMG%POSI(NODFT)
!        NDDL_MAX= NLOC_DMG%POSI(NODFT+1) - NPOS_MAX
!        V_MAX   = MAXVAL(ABS(VEL(NPOS_MAX:NPOS_MAX+NDDL_MAX-1)))
!        DO N_DBG = NODFT+1, NODLT
!          NPOS = NLOC_DMG%POSI(N_DBG)
!          NDDL = NLOC_DMG%POSI(N_DBG+1) - NPOS
!          V_CUR = MAXVAL(ABS(VEL(NPOS:NPOS+NDDL-1)))
!          IF (V_CUR > V_MAX) THEN
!            V_MAX    = V_CUR
!            N_MAX    = N_DBG
!            NPOS_MAX = NPOS
!            NDDL_MAX = NDDL
!          ENDIF
!        ENDDO
!        IF (V_MAX > 1.0E+06) THEN
!          WRITE(6,'(A,I8,A,I8,A,1PE12.4)')
!     .      ' NLOCAL_VEL peak: nl_node=', N_MAX,
!     .      ' pos=', NPOS_MAX, ' |VNL|=', V_MAX
!          WRITE(6,'(A,5(1PE14.6,1X))')
!     .      '  VNL_new=', VEL(NPOS_MAX:NPOS_MAX+NDDL_MAX-1)
!          WRITE(6,'(A,5(1PE14.6,1X))')
!     .      '  VNL_old=', VEL0(NPOS_MAX:NPOS_MAX+NDDL_MAX-1)
!          IF (DT12 > ZERO) THEN
!            WRITE(6,'(A,5(1PE14.6,1X))')
!     .        '  FNL(implied)=',
!     .        (VEL(NPOS_MAX:NPOS_MAX+NDDL_MAX-1)
!     .         - VEL0(NPOS_MAX:NPOS_MAX+NDDL_MAX-1)) / DT12
!          ENDIF
!          WRITE(6,'(A,5(1PE14.6,1X))')
!     .      '  UNL=', NLOC_DMG%UNL(NPOS_MAX:NPOS_MAX+NDDL_MAX-1)
!          WRITE(6,'(A,5(1PE14.6,1X))')
!     .      '  MASS=', NLOC_DMG%MASS(NPOS_MAX:NPOS_MAX+NDDL_MAX-1)
!          WRITE(6,'(A,5(1PE14.6,1X))')
!     .      '  MASS0=', NLOC_DMG%MASS0(NPOS_MAX:NPOS_MAX+NDDL_MAX-1)
!        ENDIF
!      END BLOCK
c-----------
      RETURN
      END
      
