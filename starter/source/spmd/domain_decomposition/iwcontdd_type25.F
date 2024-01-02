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
Chd|  IWCONTDD_TYPE25               source/spmd/domain_decomposition/iwcontdd_type25.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
        SUBROUTINE IWCONTDD_TYPE25(NLEDGE,NUMNOD,NSN,NMN,IEDGE,NRTM,NEDGE,NSV,MSR,IRECT,IWCONT,NSNT,NMNT,INTBUF_TAB)
!$COMMENT
!       IWCONTDD_TYPE25 description :
!       update the weight of nodes belonging to an interface type 25 
!       to balance the sorting of the engine
!
!       IWCONTDD_TYPE25 organization :
!           for S node :
!            * add the weith to the S node
!           for M node :
!            * add the weith to the M node
!           for edge to edge :
!            * add the weight to the 4 M nodes (e2s)
!            * add the weight to the 2 M nodes (e2e)
!            * add the weight to the 2 S nodes (e2e)
!$ENDCOMMENT
        USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: NLEDGE !< size of edge arrays
        INTEGER, INTENT(in) :: NUMNOD !< number nodes
        INTEGER, INTENT(in) :: NSN !< number of S nodes
        INTEGER, INTENT(in) :: NMN !< number of M nodes
        INTEGER, INTENT(in) :: IEDGE !< flag for edge / edge 
        INTEGER, INTENT(in) :: NRTM !< number of segment
        INTEGER, INTENT(in) :: NEDGE !< number of edge
        INTEGER, DIMENSION(NSN), INTENT(in) :: NSV !< list of S nodes
        INTEGER, DIMENSION(NMN), INTENT(in) :: MSR !< list of M nodes
        INTEGER, DIMENSION(4,NRTM), INTENT(in) :: IRECT !< id of the 4 nodes of the segment
        INTEGER, DIMENSION(5,NUMNOD), INTENT(inout) :: IWCONT ! weight array for the interface
        INTEGER, INTENT(inout) :: NSNT !< total number of S node for all interfaces
        INTEGER, INTENT(inout) :: NMNT !< total number of M node for all interfaces
        TYPE(INTBUF_STRUCT_), INTENT(in) :: INTBUF_TAB !< interface structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I
        INTEGER :: S_NODE_ID,M_NODE_ID,S_NODE_ID_2
        INTEGER :: NODE_ID_1,NODE_ID_2,NODE_ID_3,NODE_ID_4 ! node id
        INTEGER, DIMENSION(:), ALLOCATABLE :: TAG_NODE
C-----------------------------------------------

        ! ---------------------------
        ! loop over the S nodes
        DO I = 1,NSN
          S_NODE_ID = NSV(I)
          IWCONT(1,S_NODE_ID) = IWCONT(1,S_NODE_ID)+1
          NSNT = NSNT + 1
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


        ! ---------------------------
        ! special treatment for edge to edge
        IF(IEDGE/=0) THEN
          ALLOCATE(TAG_NODE(NUMNOD))
          TAG_NODE(1:NUMNOD) = 0
          ! ------------
          ! S node for e2e & e2s
          DO I=1,NEDGE
            NODE_ID_1 = INTBUF_TAB%LEDGE((I-1)*NLEDGE+5)
            NODE_ID_2 = INTBUF_TAB%LEDGE((I-1)*NLEDGE+6)
            IF(INTBUF_TAB%LEDGE((I-1)*NLEDGE+7)<0) CYCLE
            IF(TAG_NODE(NODE_ID_1)==0) THEN
              IWCONT(1,NODE_ID_1) = IWCONT(1,NODE_ID_1)+1
              TAG_NODE(NODE_ID_1) = 1
              NSNT = NSNT + 1
            ENDIF
            IF(TAG_NODE(NODE_ID_2)==0) THEN
              IWCONT(1,NODE_ID_2) = IWCONT(1,NODE_ID_2)+1
              TAG_NODE(NODE_ID_2) = 1
              NSNT = NSNT + 1
            ENDIF
          ENDDO
          ! ------------
          TAG_NODE(1:NUMNOD) = 0
          ! ------------
          ! M node for e2e
          DO I=1,NEDGE
            IF(IABS(INTBUF_TAB%LEDGE((I-1)*NLEDGE+7))==1) CYCLE ! Main solid edge
            NODE_ID_1 = INTBUF_TAB%LEDGE((I-1)*NLEDGE+5)
            NODE_ID_2 = INTBUF_TAB%LEDGE((I-1)*NLEDGE+6)
            IF(TAG_NODE(NODE_ID_1)==0) THEN
              IWCONT(2,NODE_ID_1) = IWCONT(2,NODE_ID_1)+1
              TAG_NODE(NODE_ID_1) = 1
              NMNT = NMNT + 1
            ENDIF
            IF(TAG_NODE(NODE_ID_2)==0) THEN
              IWCONT(2,NODE_ID_2) = IWCONT(2,NODE_ID_2)+1
              TAG_NODE(NODE_ID_2) = 1
              NMNT = NMNT + 1
            ENDIF
          ENDDO
          ! ------------
          TAG_NODE(1:NUMNOD) = 0
          ! ------------
          ! M node for e2s
          DO I=1,NRTM
            IF(INTBUF_TAB%MSEGTYP24(I)/=0) CYCLE ! not a solid edge
            NODE_ID_1 = IRECT(1,I)
            NODE_ID_2 = IRECT(2,I)
            NODE_ID_3 = IRECT(3,I)
            NODE_ID_4 = IRECT(4,I)

            IF(TAG_NODE(NODE_ID_1)==0) THEN
              IWCONT(2,NODE_ID_1) = IWCONT(2,NODE_ID_1)+1
              TAG_NODE(NODE_ID_1) = 1
              NMNT = NMNT + 1
            ENDIF
            IF(TAG_NODE(NODE_ID_2)==0) THEN
              IWCONT(2,NODE_ID_2) = IWCONT(2,NODE_ID_2)+1
              TAG_NODE(NODE_ID_2) = 1
              NMNT = NMNT + 1
            ENDIF
            IF(TAG_NODE(NODE_ID_3)==0) THEN
              IWCONT(2,NODE_ID_3) = IWCONT(2,NODE_ID_3)+1
              TAG_NODE(NODE_ID_3) = 1
              NMNT = NMNT + 1
            ENDIF
            IF(TAG_NODE(NODE_ID_4)==0) THEN
              IWCONT(2,NODE_ID_4) = IWCONT(2,NODE_ID_4)+1
              TAG_NODE(NODE_ID_4) = 1
              NMNT = NMNT + 1
            ENDIF
          ENDDO
          ! ------------    
          DEALLOCATE(TAG_NODE)
        ENDIF
        ! ---------------------------

        RETURN
        END SUBROUTINE IWCONTDD_TYPE25
