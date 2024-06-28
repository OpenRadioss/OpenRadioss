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
Chd|  W_DAMPVREL                    source/restart/ddsplit/w_dampvrel.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE W_DAMPVREL(DAMPR,IGRNOD,IDAMP_VREL_L,NDAMP_VREL_L,LEN_IA,
     .                      NGRNOD,NDAMP,NRDAMP,NSPMD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD     
C---------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NDAMP_VREL_L,NGRNOD,NDAMP,NRDAMP,NSPMD
      INTEGER, INTENT(IN) :: IDAMP_VREL_L(NDAMP)
      INTEGER, INTENT(INOUT) :: LEN_IA
      my_real, INTENT(IN) :: DAMPR(NRDAMP,NDAMP)
      TYPE (GROUP_),DIMENSION(NGRNOD),INTENT(IN) :: IGRNOD
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IGR,PROC,PMAIN,CPT
      INTEGER FR_DAMP_VREL(NSPMD+2,NDAMP_VREL_L),ID_DAMP_VREL(NDAMP_VREL_L)
C----------------------------------------------- 
C
      PMAIN = 1
      CPT = 0
      DO I=1,NDAMP
        IF (IDAMP_VREL_L(I) > 0) THEN
          CPT = CPT+1  
          ID_DAMP_VREL(CPT) = I    
        ENDIF      
      ENDDO        
C
      FR_DAMP_VREL(1:NSPMD+2,1:NDAMP_VREL_L) = 0
      DO I=1,NDAMP_VREL_L
        IGR   = NINT(DAMPR(2,ID_DAMP_VREL(I)))
C--     Tag of procs having nodes in damping                 
        DO PROC = 1,NSPMD
          DO J = 1,IGRNOD(IGR)%NENTITY
            IF (NLOCAL(IGRNOD(IGR)%ENTITY(J),PROC)==1) THEN
              FR_DAMP_VREL(PROC,I) = 1   
            ENDIF        
          ENDDO 
        ENDDO
C--     Find main proc for damping (smallest proc id)          
        DO PROC = 1,NSPMD
          IF (FR_DAMP_VREL(PROC,I) == 1) THEN
            PMAIN =  PROC
            exit
          ENDIF   
        ENDDO
        FR_DAMP_VREL(NSPMD+1,I) = 0 ! Not used for now
        FR_DAMP_VREL(NSPMD+2,I) = PMAIN
      ENDDO
C
      CALL WRITE_I_C(ID_DAMP_VREL,NDAMP_VREL_L)
      LEN_IA = LEN_IA + NDAMP_VREL_L
C      
      CALL WRITE_I_C(FR_DAMP_VREL,NDAMP_VREL_L*(NSPMD+2)) 
      LEN_IA = LEN_IA + NDAMP_VREL_L*(NSPMD+2)
C                
! --------------------------------------
      RETURN
      END
