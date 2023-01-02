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
Chd|  WRITE_UNITS                   comm/write_units.F            
Chd|-- called by -----------
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|-- calls ---------------
Chd|        WRITE_DB                      starter/source/restart/ddsplit/wrrest.F
Chd|        WRITE_DB                      engine/source/output/tools/write_db.F
Chd|        WRITE_I_C                     starter/source/output/tools/write_routines.c
Chd|        WRITE_I_C                     engine/source/output/tools/sortie_c.c
Chd|====================================================================
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
#include      "sysunit.inc"
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
      DO I=1,NUNITS
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
Chd|====================================================================
Chd|  READ_UNITS                    comm/write_units.F            
Chd|-- called by -----------
Chd|        RADIOSS2                      engine/source/engine/radioss2.F
Chd|-- calls ---------------
Chd|        READ_DB                       starter/source/restart/ddsplit/wrrest.F
Chd|        READ_DB                       engine/source/output/tools/read_db.F
Chd|        READ_I_C                      starter/source/output/tools/write_routines.c
Chd|        READ_I_C                      engine/source/output/tools/sortie_c.c
Chd|        MESSAGE_MOD                   starter/share/message_module/message_mod.F
Chd|        MESSAGE_MOD                   engine/share/message_module/message_mod.F
Chd|====================================================================
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
#include      "sysunit.inc"
#include      "my_allocate.inc"
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
C----------------------------------------------- 
      MY_ALLOCATE(UNITAB%UNIT_ID,NUNITS)
      MY_ALLOCATE(UNITAB%FAC_M,NUNITS)
      MY_ALLOCATE(UNITAB%FAC_L,NUNITS)
      MY_ALLOCATE(UNITAB%FAC_T,NUNITS)

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
