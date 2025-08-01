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
!||    write_units   ../common_source/comm/write_units.F
!||--- called by ------------------------------------------------------
!||    ddsplit       ../starter/source/restart/ddsplit/ddsplit.F
!||    wrrestp       ../engine/source/output/restart/wrrestp.F
!||--- calls      -----------------------------------------------------
!||    write_db      ../common_source/tools/input_output/write_db.F
!||    write_i_c     ../common_source/tools/input_output/write_routtines.c
!||--- uses       -----------------------------------------------------
!||    unitab_mod    ../common_source/modules/unitab_mod.F
!||====================================================================
      SUBROUTINE WRITE_UNITS(UNITAB)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C----------------------------------------------- 
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,UID
      my_real FAC_M,FAC_L,FAC_T
C----------------------------------------------- 
      CALL WRITE_I_C(UNITAB%NUNITS,1)
      CALL WRITE_I_C(UNITAB%NUNIT0,1)
      CALL WRITE_DB(UNITAB%FAC_M_WORK,1)
      CALL WRITE_DB(UNITAB%FAC_L_WORK,1)
      CALL WRITE_DB(UNITAB%FAC_T_WORK,1)
      CALL WRITE_DB(UNITAB%FAC_MASS,1)
      CALL WRITE_DB(UNITAB%FAC_LENGTH,1)
      CALL WRITE_DB(UNITAB%FAC_TIME,1)

      DO I=1,UNITAB%NUNITS
        UID = UNITAB%UNIT_ID(I)
        FAC_M = UNITAB%FAC_M(I)
        FAC_L = UNITAB%FAC_L(I)
        FAC_T = UNITAB%FAC_T(I)

        CALL WRITE_I_C(UID,1)
        CALL WRITE_DB(FAC_M,1)
        CALL WRITE_DB(FAC_L,1)
        CALL WRITE_DB(FAC_T,1)

      ENDDO
C----------------------------------------------- 
      END
!||====================================================================
!||    read_units    ../common_source/comm/write_units.F
!||--- called by ------------------------------------------------------
!||    rdresb        ../engine/source/output/restart/rdresb.F
!||--- calls      -----------------------------------------------------
!||    read_db       ../common_source/tools/input_output/read_db.F
!||    read_i_c      ../common_source/tools/input_output/write_routtines.c
!||--- uses       -----------------------------------------------------
!||    message_mod   ../engine/share/message_module/message_mod.F
!||    unitab_mod    ../common_source/modules/unitab_mod.F
!||====================================================================
      SUBROUTINE READ_UNITS(UNITAB)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C----------------------------------------------- 
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_) ::UNITAB 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,UID
      my_real FAC_M,FAC_L,FAC_T
      INTEGER :: NUNITS 
C----------------------------------------------- 
      CALL READ_I_C(NUNITS,1)
      UNITAB%NUNITS = NUNITS 
      CALL READ_I_C(UNITAB%NUNIT0,1)
      ALLOCATE(UNITAB%UNIT_ID(NUNITS))
      ALLOCATE(UNITAB%FAC_M(NUNITS))
      ALLOCATE(UNITAB%FAC_L(NUNITS))
      ALLOCATE(UNITAB%FAC_T(NUNITS))
      CALL READ_DB(UNITAB%FAC_M_WORK,1)
      CALL READ_DB(UNITAB%FAC_L_WORK,1)
      CALL READ_DB(UNITAB%FAC_T_WORK,1)
      CALL READ_DB(UNITAB%FAC_MASS,1)
      CALL READ_DB(UNITAB%FAC_LENGTH,1)
      CALL READ_DB(UNITAB%FAC_TIME,1)

      DO I=1,NUNITS

        CALL READ_I_C(UID,1)
        CALL READ_DB(FAC_M,1)
        CALL READ_DB(FAC_L,1)
        CALL READ_DB(FAC_T,1)

        UNITAB%UNIT_ID(I) = UID
        UNITAB%FAC_M(I) = FAC_M
        UNITAB%FAC_L(I) = FAC_L
        UNITAB%FAC_T(I) = FAC_T
      ENDDO
C----------------------------------------------- 
      END
