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
Chd|  UPGRADE_REMNODE               source/interfaces/interf1/upgrade_remnode.F
Chd|-- called by -----------
Chd|        I7REMNODE                     source/interfaces/inter3d1/i7remnode.F
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|        ININTR                        source/interfaces/interf1/inintr.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUFMOD                     share/modules1/restart_mod.F  
Chd|====================================================================
      SUBROUTINE UPGRADE_REMNODE(IPARI,NREMNODE,INTBUF_TAB,NTY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFMOD
      USE INTBUFDEF_MOD
C-------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "tabsiz_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "intread_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NI,NREMNODE,IPARI(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,OLD_SIZE ,NTY

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      INTEGER, DIMENSION (:), ALLOCATABLE :: OLD_TAB
C======================================================================|
C Set Global parameters
C Set the new NREMNODE parameter for the given Interface
      IPARI(62) = NREMNODE

      IF(NTY/=11 .AND. NTY/=25) INTBUF_TAB%KREMNODE(1:INTBUF_TAB%S_KREMNODE)=0

      OLD_SIZE = INTBUF_TAB%S_REMNODE
      ALLOCATE (OLD_TAB(OLD_SIZE))
      !save values
      DO I=1,OLD_SIZE
        OLD_TAB(I)=INTBUF_TAB%REMNODE(I)
      ENDDO
      DEALLOCATE(INTBUF_TAB%REMNODE)

      !reallocate with new size and copy saved values
      INTBUF_TAB%S_REMNODE = NREMNODE
      ALLOCATE(INTBUF_TAB%REMNODE(INTBUF_TAB%S_REMNODE))
      INTBUF_TAB%REMNODE(1:INTBUF_TAB%S_REMNODE)=0
      DO I=1,OLD_SIZE
        INTBUF_TAB%REMNODE(I) = OLD_TAB(I)
      ENDDO
C
      DEALLOCATE(OLD_TAB)
C
      IF(NTY == 25 ) THEN

        INTBUF_TAB%KREMNOR(1:INTBUF_TAB%S_KREMNOR)=0

        OLD_SIZE = INTBUF_TAB%S_REMNOR
        ALLOCATE (OLD_TAB(OLD_SIZE))
        !save values
        DO I=1,OLD_SIZE
          OLD_TAB(I)=INTBUF_TAB%REMNOR(I)
        ENDDO
        DEALLOCATE(INTBUF_TAB%REMNOR)
        !reallocate with new size and copy saved values
        INTBUF_TAB%S_REMNOR = NREMNODE
        IPARI(81) = NREMNODE

        ALLOCATE(INTBUF_TAB%REMNOR(INTBUF_TAB%S_REMNOR))
        INTBUF_TAB%REMNOR(1:INTBUF_TAB%S_REMNOR)=0
        DO I=1,OLD_SIZE
          INTBUF_TAB%REMNOR(I) = OLD_TAB(I)
        ENDDO

        DEALLOCATE(OLD_TAB)
      ENDIF

      END
Chd|====================================================================
Chd|  UPGRADE_REMNODE2              source/interfaces/interf1/upgrade_remnode.F
Chd|-- called by -----------
Chd|        REMN_I2OP                     source/interfaces/inter3d1/i7remnode.F
Chd|        REMN_I2_EDGOP                 source/interfaces/inter3d1/i7remnode.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUFMOD                     share/modules1/restart_mod.F  
Chd|        RESTMOD                       share/modules1/restart_mod.F  
Chd|====================================================================
      SUBROUTINE UPGRADE_REMNODE2(NI,NREMNODE,INTBUF_TAB,NTY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RESTMOD
      USE INTBUFMOD
      USE INTBUFDEF_MOD
C-------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "tabsiz_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "intread_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NI,NREMNODE,NTY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,OLD_SIZE, NRTM, NSN

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      INTEGER, DIMENSION (:), ALLOCATABLE :: OLD_TAB
C======================================================================|
C Set Global parameters
C Set the new NREMNODE parameter for the given Interface
      IPARI(NPARI*(NI-1)+62) = NREMNODE
      IPARI(NPARI*(NI-1)+81) = NREMNODE ! NREMNOR for T25
      IPARI(NPARI*(NI-1)+63) = 2
      NRTM = IPARI(NPARI*(NI-1)+4)
      NSN  = IPARI(NPARI*(NI-1)+5)

      !save old size and values for KREMNODE array
      OLD_SIZE = INTBUF_TAB%S_KREMNODE
      ALLOCATE (OLD_TAB(OLD_SIZE))
      !save values
      DO I=1,OLD_SIZE
        OLD_TAB(I)=INTBUF_TAB%KREMNODE(I)
      ENDDO
      DEALLOCATE(INTBUF_TAB%KREMNODE)

      !reallocate with new size and copy saved values for KREMNODE array
      INTBUF_TAB%S_KREMNODE = 2*(NRTM + 1)
      ALLOCATE(INTBUF_TAB%KREMNODE(INTBUF_TAB%S_KREMNODE))
      INTBUF_TAB%KREMNODE(1:INTBUF_TAB%S_KREMNODE)=0
      DO I=1,OLD_SIZE
        INTBUF_TAB%KREMNODE(I) = OLD_TAB(I)
      ENDDO
      DEALLOCATE(OLD_TAB)

      !save old size and values for REMNODE array
      OLD_SIZE = INTBUF_TAB%S_REMNODE
      ALLOCATE (OLD_TAB(OLD_SIZE))
      !save values
      DO I=1,OLD_SIZE
        OLD_TAB(I)=INTBUF_TAB%REMNODE(I)
      ENDDO
      DEALLOCATE(INTBUF_TAB%REMNODE)

      !reallocate with new size and copy saved values for REMNODE array
      INTBUF_TAB%S_REMNODE = NREMNODE
      ALLOCATE(INTBUF_TAB%REMNODE(INTBUF_TAB%S_REMNODE))
      INTBUF_TAB%REMNODE(1:INTBUF_TAB%S_REMNODE)=0
      DO I=1,OLD_SIZE
        INTBUF_TAB%REMNODE(I) = OLD_TAB(I)
      ENDDO
      DEALLOCATE(OLD_TAB)
C
      IF(NTY == 25 ) THEN

        ! KREMNOR and REMNOR will be rebuilt after, from scratch !

        DEALLOCATE(INTBUF_TAB%KREMNOR)
        !reallocate with new size and initialize
        INTBUF_TAB%S_KREMNOR = NSN + 1
        ALLOCATE(INTBUF_TAB%KREMNOR(INTBUF_TAB%S_KREMNOR))
        INTBUF_TAB%KREMNOR(1:INTBUF_TAB%S_KREMNOR)=0

        DEALLOCATE(INTBUF_TAB%REMNOR)
        !reallocate with new size and initialize
        INTBUF_TAB%S_REMNOR = NREMNODE
        ALLOCATE(INTBUF_TAB%REMNOR(INTBUF_TAB%S_REMNOR))
        INTBUF_TAB%REMNOR(1:INTBUF_TAB%S_REMNOR)=0

      END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  UPGRADE_REMNODE_EDG           source/interfaces/interf1/upgrade_remnode.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|        ININTR                        source/interfaces/interf1/inintr.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUFMOD                     share/modules1/restart_mod.F  
Chd|====================================================================
      SUBROUTINE UPGRADE_REMNODE_EDG(IPARI,NREMNODE,INTBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFMOD
      USE INTBUFDEF_MOD
C-------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "tabsiz_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "intread_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NI,NREMNODE,IPARI(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,OLD_SIZE ,NTY

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      INTEGER, DIMENSION (:), ALLOCATABLE :: OLD_TAB
C======================================================================|
C Set Global parameters
C Set the new NREMNODE parameter for the given Interface
      IPARI(94) = NREMNODE

      OLD_SIZE = INTBUF_TAB%S_REMNODE_EDG
      ALLOCATE (OLD_TAB(OLD_SIZE))
      !save values
      DO I=1,OLD_SIZE
        OLD_TAB(I)=INTBUF_TAB%REMNODE_EDG(I)
      ENDDO
      DEALLOCATE(INTBUF_TAB%REMNODE_EDG)

      !reallocate with new size and copy saved values
      INTBUF_TAB%S_REMNODE_EDG = NREMNODE
      ALLOCATE(INTBUF_TAB%REMNODE_EDG(INTBUF_TAB%S_REMNODE_EDG))
      INTBUF_TAB%REMNODE_EDG(1:INTBUF_TAB%S_REMNODE_EDG)=0
      DO I=1,OLD_SIZE
        INTBUF_TAB%REMNODE_EDG(I) = OLD_TAB(I)
      ENDDO
C
      DEALLOCATE(OLD_TAB)
C

      END


Chd|====================================================================
Chd|  UPGRADE_REMNODE_EDG2          source/interfaces/interf1/upgrade_remnode.F
Chd|-- called by -----------
Chd|        REMN_I2OP_EDG25               source/interfaces/int25/i25remlin.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUFMOD                     share/modules1/restart_mod.F  
Chd|====================================================================
      SUBROUTINE UPGRADE_REMNODE_EDG2(IPARI,NREMNODE,INTBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFMOD
      USE INTBUFDEF_MOD
C-------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "tabsiz_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "intread_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER , INTENT(IN) :: NREMNODE
      INTEGER , INTENT(INOUT) :: IPARI(NPARI)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,OLD_SIZE ,NEDGE

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      INTEGER, DIMENSION (:), ALLOCATABLE :: OLD_TAB
C======================================================================|
C Set Global parameters
C Set the new NREMNODE parameter for the given Interface
      IPARI(94) = NREMNODE
      NEDGE = IPARI(68)

      DEALLOCATE(INTBUF_TAB%KREMNODE_EDG)

      !reallocate with new size and copy saved values for KREMNODE array
      INTBUF_TAB%S_KREMNODE_EDG = 2*(NEDGE + 1)
      ALLOCATE(INTBUF_TAB%KREMNODE_EDG(INTBUF_TAB%S_KREMNODE_EDG))
      INTBUF_TAB%KREMNODE_EDG(1:INTBUF_TAB%S_KREMNODE_EDG)=0

      OLD_SIZE = INTBUF_TAB%S_REMNODE_EDG
      ALLOCATE (OLD_TAB(OLD_SIZE))
      !save values
      DO I=1,OLD_SIZE
        OLD_TAB(I)=INTBUF_TAB%REMNODE_EDG(I)
      ENDDO
      DEALLOCATE(INTBUF_TAB%REMNODE_EDG)

      !reallocate with new size and copy saved values
      INTBUF_TAB%S_REMNODE_EDG = NREMNODE
      ALLOCATE(INTBUF_TAB%REMNODE_EDG(INTBUF_TAB%S_REMNODE_EDG))
      INTBUF_TAB%REMNODE_EDG(1:INTBUF_TAB%S_REMNODE_EDG)=0
      DO I=1,OLD_SIZE
        INTBUF_TAB%REMNODE_EDG(I) = OLD_TAB(I)
      ENDDO
C
      DEALLOCATE(OLD_TAB)
C

      END

Chd|====================================================================
Chd|  UPGRADE_REMNODE_E2S           source/interfaces/interf1/upgrade_remnode.F
Chd|-- called by -----------
Chd|        REMN_I2OP_EDG25               source/interfaces/int25/i25remlin.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUFMOD                     share/modules1/restart_mod.F  
Chd|====================================================================
      SUBROUTINE UPGRADE_REMNODE_E2S(IPARI,NREMNODE,INTBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFMOD
      USE INTBUFDEF_MOD
C-------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "tabsiz_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "intread_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER , INTENT(IN) :: NREMNODE
      INTEGER , INTENT(INOUT) :: IPARI(NPARI)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,OLD_SIZE ,NRTM

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      INTEGER, DIMENSION (:), ALLOCATABLE :: OLD_TAB
C======================================================================|
C Set Global parameters
C Set the new NREMNODE parameter for the given Interface
      IPARI(96) = NREMNODE
      NRTM = IPARI(4)

      DEALLOCATE(INTBUF_TAB%KREMNODE_E2S)

      !reallocate with new size and copy saved values for KREMNODE array
      INTBUF_TAB%S_KREMNODE_E2S = 2*(NRTM + 1)
      ALLOCATE(INTBUF_TAB%KREMNODE_E2S(INTBUF_TAB%S_KREMNODE_E2S))
      INTBUF_TAB%KREMNODE_E2S(1:INTBUF_TAB%S_KREMNODE_E2S)=0

      OLD_SIZE = INTBUF_TAB%S_REMNODE_E2S
      ALLOCATE (OLD_TAB(OLD_SIZE))
      !save values
      DO I=1,OLD_SIZE
        OLD_TAB(I)=INTBUF_TAB%REMNODE_E2S(I)
      ENDDO
      DEALLOCATE(INTBUF_TAB%REMNODE_E2S)

      !reallocate with new size and copy saved values
      INTBUF_TAB%S_REMNODE_E2S = NREMNODE
      ALLOCATE(INTBUF_TAB%REMNODE_E2S(INTBUF_TAB%S_REMNODE_E2S))
      INTBUF_TAB%REMNODE_E2S(1:INTBUF_TAB%S_REMNODE_E2S)=0
      DO I=1,OLD_SIZE
        INTBUF_TAB%REMNODE_E2S(I) = OLD_TAB(I)
      ENDDO
C
      DEALLOCATE(OLD_TAB)
C

      END
