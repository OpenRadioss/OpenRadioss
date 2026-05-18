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
!||    read_failwave   ../engine/source/output/restart/read_failwave.F
!||--- called by ------------------------------------------------------
!||    rdresb          ../engine/source/output/restart/rdresb.F
!||--- calls      -----------------------------------------------------
!||    read_i_c        ../common_source/tools/input_output/write_routines.c
!||--- uses       -----------------------------------------------------
!||    failwave_mod    ../common_source/modules/failwave_mod.F
!||====================================================================
      SUBROUTINE READ_FAILWAVE(FAILWAVE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAILWAVE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      use my_alloc_mod
      use my_dealloc_mod, only : my_dealloc
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (FAILWAVE_STR_) :: FAILWAVE 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IAD,IWAVE,LEN,SIZE,NDDL,NNOD
      INTEGER, DIMENSION(4)  :: HEAD
      INTEGER, DIMENSION(:), ALLOCATABLE :: RBUF
C=======================================================================
c     read failwave data
c
      CALL READ_I_C(HEAD, 4)
      IWAVE = HEAD(1) 
      NDDL  = HEAD(2) 
      SIZE  = HEAD(3) 
      NNOD  = HEAD(4) 
      FAILWAVE%WAVE_MOD = IWAVE
      FAILWAVE%NDDL     = NDDL
      FAILWAVE%SIZE     = SIZE
      FAILWAVE%NNOD     = NNOD
c
      IF (IWAVE > 0) THEN
        LEN = NDDL*NNOD*SIZE
        call my_alloc(RBUF, LEN, "RBUF")
        call my_alloc(FAILWAVE%INDX, NNOD, "FAILWAVE%INDX")
        call my_alloc(FAILWAVE%IDXI, NUMNOD, "FAILWAVE%IDXI")
        call my_alloc(FAILWAVE%MAXLEV, NNOD, "FAILWAVE%MAXLEV")
        call my_alloc(FAILWAVE%FWAVE_NOD, NDDL, NNOD, SIZE, "FAILWAVE%FWAVE_NOD")
        call my_alloc(FAILWAVE%FWAVE_NOD_STACK, NDDL, NNOD, SIZE, "FAILWAVE%FWAVE_NOD_STACK")
        call my_alloc(FAILWAVE%MAXLEV_STACK, NNOD, "FAILWAVE%MAXLEV_STACK")
        FAILWAVE%FWAVE_NOD_STACK(:,:,:)  = 0
        FAILWAVE%MAXLEV_STACK(:) = 0
c
        CALL READ_I_C(FAILWAVE%INDX, NNOD)
c
        CALL READ_I_C(FAILWAVE%IDXI, NUMNOD)
c
        CALL READ_I_C(FAILWAVE%MAXLEV, NNOD)
c
        CALL READ_I_C(RBUF, LEN)
c
        IAD = 0
        DO K = 1,SIZE
          DO J = 1,NNOD
            DO I = 1,NDDL
              IAD = IAD+1
              FAILWAVE%FWAVE_NOD(I,J,K) = RBUF(IAD)
            END DO 
          END DO 
        END DO 
c
        call my_dealloc(RBUF)
      ELSE
        call my_alloc(FAILWAVE%FWAVE_NOD, 0, 0, 0, "FAILWAVE%FWAVE_NOD")
        call my_alloc(FAILWAVE%FWAVE_NOD_STACK, 0, 0, 0, "FAILWAVE%FWAVE_NOD_STACK")
        call my_alloc(FAILWAVE%INDX, 0, "FAILWAVE%INDX")
        call my_alloc(FAILWAVE%IDXI, 0, "FAILWAVE%IDXI")
        call my_alloc(FAILWAVE%MAXLEV, 0, "FAILWAVE%MAXLEV")
        call my_alloc(FAILWAVE%MAXLEV_STACK, 0, "FAILWAVE%MAXLEV_STACK")
      ENDIF
c-----------
      RETURN
      END
