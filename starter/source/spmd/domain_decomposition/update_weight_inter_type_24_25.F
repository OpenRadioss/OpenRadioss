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
Chd|  I7NINTDD                      source/spmd/domain_decomposition/grid2mat.F
Chd|-- called by -----------
Chd|        I20INI3                       source/interfaces/inter3d1/i20ini3.F
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ZEROIN                        source/system/zeroin.F        
Chd|====================================================================
        SUBROUTINE UPDATE_WEIGHT_INTER_TYPE_24_25(NUMNOD,NELEMINT,INTERFACE_ID,NSN,NRTM,
     .                                       IFIEND,IRECT,NSV,I_STOK,CAND_E,
     .                                       CAND_N,DGAPLOAD,GAP_S,GAP_M,X,INTER_CAND,
     .                                       INTER_KIND,INTBUF_TAB)
!$COMMENT
!       UPDATE_WEIGHT_INTER_TYPE_24_25 description :
!       save the contact data for interface type 24 and 25 
!
!       UPDATE_WEIGHT_INTER_TYPE_24_25 organization :
!           for each contact, save :
!            * 4 main node IDs
!            * 1 secondary node ID
!            * 1 segment ID
!            * type of interface
!           For interface type 24 :
!            * for a fictive node, find the 2 nodes of the edge and add it to the save data
!$ENDCOMMENT
        USE INTER_CAND_MOD
        USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(inout) :: NELEMINT
        INTEGER, INTENT(in) :: INTERFACE_ID !< interface id 
        INTEGER, INTENT(in) :: NSN !< number of S node
        INTEGER, INTENT(in) :: NRTM !< number of segment
        INTEGER, INTENT(inout) :: IFIEND !< ???
        INTEGER, DIMENSION(4,NRTM) :: IRECT !< list of M nodes for the NRTM segments
        INTEGER, DIMENSION(NSN) :: NSV !< list of S nodes
        INTEGER, INTENT(in) :: I_STOK !< total number of pair of candidate
        INTEGER, INTENT(in) :: NUMNOD !< number of node
        INTEGER, DIMENSION(I_STOK), INTENT(in) :: CAND_E !< segment id of the candidate I
        INTEGER, DIMENSION(I_STOK), INTENT(in) :: CAND_N !< pointer to the S node id of the candidate I
        INTEGER, INTENT(in) :: INTER_KIND !< kind of interface : 24 or 25
        my_real, INTENT(IN) :: DGAPLOAD !< other kind of gap
        my_real, DIMENSION(NSN) :: GAP_S !< gap of S node
        my_real, DIMENSION(NRTM) :: GAP_M!< gap of segment
        my_real, DIMENSION(3,NUMNOD), INTENT(in) :: X
        TYPE(INTER_CAND_), INTENT(inout) :: INTER_CAND
        TYPE(INTBUF_STRUCT_), INTENT(in) :: INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: S_NODE_ID
        INTEGER :: SEGMENT_ID
        INTEGER :: SEGMENT_ID_TYPE24 ! id of the edge for type 24 with edge to edge
        INTEGER :: NODE_ID_1,NODE_ID_2 ! id of the 2 nodes of the edge for type 24 with edge to edge
        INTEGER :: II,I
        INTEGER :: IX1,IX2,IX3,IX4
        my_real
     .   XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,THRESHOLD,
     .   XI,X1,X2,X3,X4,YI,Y1,Y2,Y3,Y4,ZI,Z1,Z2,Z3,Z4
C-----------------------------------------------
        INTER_CAND%IXINT(1:INTER_CAND%S_IXINT_1,NELEMINT+1:NELEMINT+I_STOK) = 0
        INTER_CAND%ADDRESS(INTERFACE_ID) = NELEMINT ! save the adress of the first pair
        ! ---------------------------
        ! loop over the S candidates
        II = 0
        DO I = 1, I_STOK
          S_NODE_ID = NSV(CAND_N(I)) ! S node id
          SEGMENT_ID  = CAND_E(I) ! segment id
          ! -------------
          ! interface 24 : secondary node can be "fictive" node (ID > numnod)
          !                a fictive node is the barycentre of an edge
          !                --> the weight is added to the first node of the edge
          IF (S_NODE_ID >NUMNOD.AND.INTER_KIND==24) THEN
            S_NODE_ID = S_NODE_ID - NUMNOD
            CALL I24FIC_GETN(S_NODE_ID,INTBUF_TAB%IRTSE,INTBUF_TAB%IS2SE,SEGMENT_ID_TYPE24,NODE_ID_1,NODE_ID_2)
            S_NODE_ID = NODE_ID_1 ! add the weight to the first node of the edge
          ENDIF
          ! -------------
          II = II +1

          ! find the 4 M node id
          IX1=IRECT(1,SEGMENT_ID)
          IX2=IRECT(2,SEGMENT_ID)
          IX3=IRECT(3,SEGMENT_ID)
          IX4=IRECT(4,SEGMENT_ID)

          INTER_CAND%IXINT(1,NELEMINT+II) = IX1
          INTER_CAND%IXINT(2,NELEMINT+II) = IX2
          INTER_CAND%IXINT(3,NELEMINT+II) = IX3
          INTER_CAND%IXINT(4,NELEMINT+II) = IX4
          INTER_CAND%IXINT(5,NELEMINT+II) = S_NODE_ID
          INTER_CAND%IXINT(6,NELEMINT+II) = INTER_KIND
          INTER_CAND%IXINT(7,NELEMINT+II) = SEGMENT_ID
          INTER_CAND%IXINT(8,NELEMINT+II) = INTERFACE_ID

          ZI = X(3,S_NODE_ID)
          Z1=X(3,IX1)
          Z2=X(3,IX2)
          Z3=X(3,IX3)
          Z4=X(3,IX4)
          THRESHOLD=GAP_S(CAND_N(I))+GAP_M(CAND_E(I))+DGAPLOAD
          ZMIN = MIN(Z1,Z2,Z3,Z4)-THRESHOLD
          ZMAX = MAX(Z1,Z2,Z3,Z4)+THRESHOLD
          IF (ZMIN<=ZI.AND.ZMAX>=ZI) THEN
            YI = X(2,S_NODE_ID)
            Y1 = X(2,IX1)
            Y2 = X(2,IX2)
            Y3 = X(2,IX3)
            Y4 = X(2,IX4)
            YMIN = MIN(Y1,Y2,Y3,Y4)-THRESHOLD
            YMAX = MAX(Y1,Y2,Y3,Y4)+THRESHOLD
            IF (YMIN<=YI.AND.YMAX>=YI) THEN
              XI = X(1,S_NODE_ID)
              X1 = X(1,IX1)
              X2 = X(1,IX2)
              X3 = X(1,IX3)
              X4 = X(1,IX4)
              XMIN = MIN(X1,X2,X3,X4)-THRESHOLD
              XMAX = MAX(X1,X2,X3,X4)+THRESHOLD
              IF (XMIN<=XI.AND.XMAX>=XI) THEN
                INTER_CAND%IXINT(6,NELEMINT+II)=-INTER_KIND
              ENDIF
            ENDIF
          ENDIF          
        ENDDO

        IFIEND = IFIEND + II
        ! ---------------------------
        NELEMINT = NELEMINT + II
        INTER_CAND%ADDRESS(INTERFACE_ID+1) = NELEMINT ! save the adress of the last pair
        ! ---------------------------

        RETURN
        END SUBROUTINE UPDATE_WEIGHT_INTER_TYPE_24_25
