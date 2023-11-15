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
Chd|  FLUSH_REMNODE_ARRAY           source/interfaces/inter3d1/flush_remnode_array.F
Chd|-- called by -----------
Chd|        ININTR                        source/interfaces/interf1/inintr.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
        SUBROUTINE FLUSH_REMNODE_ARRAY(NINTER,NPARI,IPARI,INTBUF_TAB)
!$COMMENT
!       FLUSH_REMNODE_ARRAY description :
!       flush arrays and structure related to RMENODE algo
!
!       FLUSH_REMNODE_ARRAY organization :
!           loop over the interfaces
!               * for interface type 24 --> flush structures and arrays
!$ENDCOMMENT

        USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: NINTER !< number of interface
        INTEGER, INTENT(in) :: NPARI !< 1rst dim of IPARI array
        INTEGER, DIMENSION(NPARI,NINTER), INTENT(in) :: IPARI !< interface data 
        TYPE(INTBUF_STRUCT_), DIMENSION(NINTER), INTENT(inout) :: INTBUF_TAB !< interface structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: N
        INTEGER :: INTERFACE_TYPE,LOCAL_SIZE
C-----------------------------------------------

        ! ---------------------------
        ! loop over the interface
        DO N=1,NINTER
          INTERFACE_TYPE = IPARI(7,N)
          IF(INTERFACE_TYPE==24) THEN
            LOCAL_SIZE = size(INTBUF_TAB(N)%KREMNODE)
            INTBUF_TAB(N)%KREMNODE(1:LOCAL_SIZE) = 0
            INTBUF_TAB(N)%S_REMNODE = 0
          ENDIF
        ENDDO
        ! ---------------------------

        RETURN
        END SUBROUTINE FLUSH_REMNODE_ARRAY
