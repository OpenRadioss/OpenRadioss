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
Chd|  IWCONTDD_TYPE24               source/spmd/domain_decomposition/iwcontdd_type24.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        I24FIC_GETN                   source/interfaces/inter3d1/i24surfi.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
        SUBROUTINE IWCONTDD_TYPE24(NUMNOD,NSN,NMN,NSV,MSR,IWCONT,NSNT,NMNT,INTBUF_TAB)
!$COMMENT
!       IWCONTDD_TYPE24 description :
!       update the weight of nodes belonging to an interface type 24 
!       to balance the sorting of the engine
!
!       IWCONTDD_TYPE24 organization :
!           for S node :
!            * check if the node is a real node or a "fictive node"
!            * for "fictive" node --> find hte id of the 2 real nodes
!            * add the weith to the S node
!           for M node :
!            * add the weith to the M node
!$ENDCOMMENT
        USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: NUMNOD !< number nodes
        INTEGER, INTENT(in) :: NSN !< number of S nodes
        INTEGER, INTENT(in) :: NMN !< number of M nodes
        INTEGER, DIMENSION(NSN), INTENT(in) :: NSV !< list of S nodes
        INTEGER, DIMENSION(NMN), INTENT(in) :: MSR !< list of M nodes
        INTEGER, DIMENSION(5,NUMNOD), INTENT(inout) :: IWCONT ! weight array for the interface
        INTEGER, INTENT(inout) :: NSNT !< total number of S node for all interfaces
        INTEGER, INTENT(inout) :: NMNT !< total number of M node for all interfaces
        TYPE(INTBUF_STRUCT_), INTENT(in) :: INTBUF_TAB !< interface structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I
        INTEGER :: S_NODE_ID,M_NODE_ID,S_NODE_ID_2
        INTEGER :: NODE_ID_1,NODE_ID_2 ! id of the 2 nodes of the edge for type 24 with edge to edge
        INTEGER :: SEGMENT_ID_TYPE24
C-----------------------------------------------

        ! ---------------------------
        ! loop over the S nodes
        DO I = 1,NSN
          S_NODE_ID = NSV(I)

          S_NODE_ID_2 = 0
          ! -------------
          ! interface 24 : secondary node can be "fictive" node (ID > numnod)
          !                a fictive node is the barycentre of an edge
          !                --> the weight is added to the first node of the edge
          IF (S_NODE_ID >NUMNOD) THEN
            S_NODE_ID = S_NODE_ID - NUMNOD
            CALL I24FIC_GETN(S_NODE_ID,INTBUF_TAB%IRTSE,INTBUF_TAB%IS2SE,SEGMENT_ID_TYPE24,NODE_ID_1,NODE_ID_2)
            S_NODE_ID = NODE_ID_1 ! add the weight to the first node of the edge
            S_NODE_ID_2 = NODE_ID_2 ! add the weight to the second node of the edge
          ENDIF
          ! -------------

          IWCONT(1,S_NODE_ID) = IWCONT(1,S_NODE_ID)+1
          NSNT = NSNT + 1
          IF(S_NODE_ID_2/=0) THEN
          IWCONT(1,S_NODE_ID_2) = IWCONT(1,S_NODE_ID_2)+1
            NSNT = NSNT + 1
          ENDIF
        ENDDO
        ! ---------------------------

        ! ---------------------------
        ! loop over the M nodes
        DO I = 1,NMN
          M_NODE_ID = MSR(I)
          IWCONT(2,M_NODE_ID) = IWCONT(2,M_NODE_ID)+1
          NMNT = NMNT + 1
        ENDDO
        ! ---------------------------

        RETURN
        END SUBROUTINE IWCONTDD_TYPE24
