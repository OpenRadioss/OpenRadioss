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
Chd|  W_DAMPING                       source/restart/ddsplit/w_seatbelts.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|====================================================================
      SUBROUTINE C_DAMPVREL(DAMPR,IGRNOD,PROC,IDAMP_VREL,NDAMP_VREL_L,
     .                      NGRNOD,NDAMP,NRDAMP)
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
      INTEGER, INTENT(IN) :: PROC,NGRNOD,NDAMP,NRDAMP
      INTEGER, INTENT(INOUT) :: NDAMP_VREL_L,IDAMP_VREL(NDAMP)
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
      INTEGER I,J,IGR,CPT
C----------------------------------------------- 
C
      DO I=1,NDAMP
        IF (NINT(DAMPR(21,I)) > 0) THEN
          IGR   = NINT(DAMPR(2,I))
          CPT = 0
          DO J = 1,IGRNOD(IGR)%NENTITY
            IF (NLOCAL(IGRNOD(IGR)%ENTITY(J),PROC)==1) THEN
               CPT = CPT + 1
            ENDIF        
          ENDDO
          IF (CPT > 0) THEN
            NDAMP_VREL_L = NDAMP_VREL_L +1  
            IDAMP_VREL(I) = NDAMP_VREL_L
          ENDIF  
        ENDIF  
      ENDDO  
C                
! --------------------------------------
      RETURN
      END