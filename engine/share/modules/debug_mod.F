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
      !||    debug_mod                ../engine/share/modules/debug_mod.F
      !||--- called by ------------------------------------------------------
      !||    i24ass2                  ../engine/source/interfaces/int24/i24for3.F
      !||    i24cor3                  ../engine/source/interfaces/int24/i24cor3.F
      !||    i24ispt2_ini_opttri      ../engine/source/interfaces/int24/i24cor3.F
      !||    i24optcd                 ../engine/source/interfaces/intsort/i24optcd.F
      !||    i25cor3_e2s              ../engine/source/interfaces/int25/i25cor3_e2s.F
      !||    i25free_bound            ../engine/source/interfaces/int25/i25free_bound.F
      !||    i25normp                 ../engine/source/interfaces/int25/i25norm.F
      !||    i25optcd_e2s             ../engine/source/interfaces/intsort/i25optcd_e2s.F
      !||    i25trivox_edg            ../engine/source/interfaces/intsort/i25trivox_edg.F
      !||    prepare_debug            ../engine/share/modules/debug_mod.F
      !||    spmd_collect_multi_fvm   ../engine/source/mpi/output/spmd_collect_multi_fvm.F
      !||    spmd_collect_nlocal      ../engine/source/mpi/output/spmd_collect_nlocal.F
      !||    spmd_fiadd25e_poff       ../engine/source/mpi/interfaces/spmd_fiadd25e_poff.F
      !||    spmd_fiadd25e_pon        ../engine/source/mpi/interfaces/spmd_fiadd25e_pon.F
      !||    update_failwave          ../engine/source/materials/fail/failwave/update_failwave.F
      !||====================================================================
      MODULE DEBUG_MOD

        INTEGER, DIMENSION(:),ALLOCATABLE :: ITAB_DEBUG

        CONTAINS
      !||====================================================================
      !||    sp_checksum   ../engine/share/modules/debug_mod.F
      !||====================================================================
      PURE FUNCTION SP_CHECKSUM(A,SIZ1,SIZ2)  RESULT(CHECKSUM)
C Prints Adler 32 checksum of A(1:SIZ2,1:SIZ1) in *1.out file 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "chara_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: SIZ1,SIZ2
      REAL*4, INTENT(IN) :: A(SIZ2,SIZ1)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER, DIMENSION(:), ALLOCATABLE :: TMP
      INTEGER :: I,CHECKSUM,ROOT,S1,S2,TWO_POWER_16
      INTEGER :: PREC ! simple or double precision
      INTEGER :: SIZ
C-----------------------------------------------
      ROOT = 65521
      TWO_POWER_16 = 65536
      S1 = 1
      S2 = 0
      PREC = 2
C  If A is in simple precision
      IF(SIZEOF(A(1,1)) == 4) PREC = 1 
      SIZ = SIZ1*SIZ2 * PREC
      ALLOCATE(TMP(SIZ))
      TMP(1:SIZ) = 0
c     Convert A in integer
      TMP = TRANSFER(A(1:SIZ2,1:SIZ1),S1,SIZ)
      DO I = 1,SIZ
        S1 = MOD(S1 + TMP(I),ROOT)
        S2 = MOD(S1 + S2    ,ROOT)
      ENDDO
C     (s2 << 16) | s1
      CHECKSUM = IOR(S2 * TWO_POWER_16,S1)
      DEALLOCATE(TMP)
      END FUNCTION
      !||====================================================================
      !||    dp_checksum   ../engine/share/modules/debug_mod.F
      !||====================================================================
      PURE FUNCTION DP_CHECKSUM(A,SIZ1,SIZ2) RESULT(CHECKSUM)
C Prints Adler 32 checksum of A(1:SIZ2,1:SIZ1) in *1.out file 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "chara_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: SIZ1,SIZ2
      DOUBLE PRECISION, INTENT(IN) :: A(SIZ2,SIZ1)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER, DIMENSION(:), ALLOCATABLE :: TMP
      INTEGER :: I,CHECKSUM,ROOT,S1,S2,TWO_POWER_16
      INTEGER :: PREC ! simple or double precision
      INTEGER :: SIZ
C-----------------------------------------------
      ROOT = 65521
      TWO_POWER_16 = 65536
      S1 = 1
      S2 = 0
      PREC = 2
C  If A is in simple precision
      IF(SIZEOF(A(1,1)) == 4) PREC = 1 
      SIZ = SIZ1*SIZ2 * PREC
      ALLOCATE(TMP(SIZ))
      TMP(1:SIZ) = 0
c     Convert A in integer
      TMP = TRANSFER(A(1:SIZ2,1:SIZ1),S1,SIZ)
      DO I = 1,SIZ
        S1 = MOD(S1 + TMP(I),ROOT)
        S2 = MOD(S1 + S2    ,ROOT)
      ENDDO
C     (s2 << 16) | s1
      CHECKSUM = IOR(S2 * TWO_POWER_16,S1)
      DEALLOCATE(TMP)
      END FUNCTION
      !||====================================================================
      !||    int_checksum    ../engine/share/modules/debug_mod.F
      !||--- called by ------------------------------------------------------
      !||    i25cor3_e2s     ../engine/source/interfaces/int25/i25cor3_e2s.F
      !||    i25optcd_e2s    ../engine/source/interfaces/intsort/i25optcd_e2s.F
      !||    i25trivox_edg   ../engine/source/interfaces/intsort/i25trivox_edg.F
      !||====================================================================
      PURE FUNCTION INT_CHECKSUM(A,SIZ1,SIZ2) RESULT(CHECKSUM)
C Prints Adler 32 checksum of A(1:SIZ2,1:SIZ1) in *1.out file 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "chara_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: SIZ1,SIZ2
      INTEGER, INTENT(IN) :: A(SIZ2,SIZ1)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER, DIMENSION(:), ALLOCATABLE :: TMP
      INTEGER :: I,CHECKSUM,ROOT,S1,S2,TWO_POWER_16
      INTEGER :: PREC ! simple or double precision
      INTEGER :: SIZ
C-----------------------------------------------
      ROOT = 65521
      TWO_POWER_16 = 65536
      S1 = 1
      S2 = 0
      PREC = 2
C  If A is in simple precision
      IF(SIZEOF(A(1,1)) == 4) PREC = 1 
      SIZ = SIZ1*SIZ2 * PREC
      ALLOCATE(TMP(SIZ))
      TMP(1:SIZ) = 0
c     Convert A in integer
      TMP = TRANSFER(A(1:SIZ2,1:SIZ1),S1,SIZ)
      DO I = 1,SIZ
        S1 = MOD(S1 + TMP(I),ROOT)
        S2 = MOD(S1 + S2    ,ROOT)
      ENDDO
C     (s2 << 16) | s1
      CHECKSUM = IOR(S2 * TWO_POWER_16,S1)
      DEALLOCATE(TMP)
      END FUNCTION
      END MODULE DEBUG_MOD


      !||====================================================================
      !||    prepare_debug   ../engine/share/modules/debug_mod.F
      !||--- called by ------------------------------------------------------
      !||    resol           ../engine/source/engine/resol.F
      !||--- uses       -----------------------------------------------------
      !||    debug_mod       ../engine/share/modules/debug_mod.F
      !||====================================================================
      SUBROUTINE PREPARE_DEBUG(ITAB,NUMNOD)
        USE DEBUG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
            INTEGER, INTENT(IN) :: NUMNOD
            INTEGER, DIMENSION(:), INTENT(IN) :: ITAB(NUMNOD)


       ALLOCATE  (ITAB_DEBUG(NUMNOD))
       ITAB_DEBUG(1:NUMNOD)=ITAB(1:NUMNOD)

      END
      
