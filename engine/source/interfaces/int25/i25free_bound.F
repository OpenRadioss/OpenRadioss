Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  I25FREE_BOUND                 source/interfaces/int25/i25free_bound.F
Chd|-- called by -----------
Chd|        I25MAIN_FREE                  source/interfaces/intsort/i25main_free.F
Chd|        SPMD_I25FRONT_INIT            source/mpi/interfaces/spmd_i25front.F
Chd|-- calls ---------------
Chd|        DEBUG_MOD                     share/modules/debug_mod.F     
Chd|====================================================================
      SUBROUTINE I25FREE_BOUND( 
     . NRTM,
     . MVOISIN,
     . IRECT,
     . STIFM,
     . NRTM_FREE,
     . FREE_IRECT_ID)
C Get the list of free IRECT (i.e. at least one edge is at the boundary)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
#ifdef WITH_ASSERT
      USE DEBUG_MOD
#endif
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "assert.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "spmd_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NRTM
      INTEGER, INTENT(IN) :: MVOISIN(4,NRTM),IRECT(4,NRTM) 
      my_real, INTENT(IN) :: STIFM(NRTM)
      INTEGER, INTENT(INOUT) :: NRTM_FREE ! out: number of free irect
      INTEGER, INTENT(INOUT) :: FREE_IRECT_ID(NRTM)! [out] max size = NRTM, list of free irect
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IRM,IEDG
      LOGICAL :: FREE

      NRTM_FREE= 0
      DO IRM=1,NRTM
        IF(STIFM(IRM)==ZERO)CYCLE
        FREE = .FALSE.
        DO IEDG=1,4
          IF( MVOISIN(IEDG,IRM)==0 )THEN
            IF(.NOT.(IRECT(3,IRM)==IRECT(4,IRM).AND.IEDG==3))THEN
              FREE = .TRUE.
            ENDIF
           ENDIF
        ENDDO
        IF(FREE) THEN
           NRTM_FREE = NRTM_FREE + 1
           FREE_IRECT_ID(NRTM_FREE) = IRM
        ENDIF
      ENDDO
      END
