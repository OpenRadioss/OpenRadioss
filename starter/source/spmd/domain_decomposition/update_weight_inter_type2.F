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
Chd|  UPDATE_WEIGHT_INTER_TYPE2     source/spmd/domain_decomposition/update_weight_inter_type2.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        INTER_CAND_MOD                share/modules1/inter_cand_mod.F
Chd|====================================================================
        SUBROUTINE UPDATE_WEIGHT_INTER_TYPE2(NELEMINT,INTERFACE_ID,NSN,NRTM,IFIEND,
     1                                       N2D,IRECT,NSV,IRTL,INTER_CAND)
!$COMMENT
!       UPDATE_WEIGHT_INTER_TYPE2 description :
!       save the contact data for interface type 2 
!
!       UPDATE_WEIGHT_INTER_TYPE2 organization :
!           for each contact, save :
!            * 2 main node IDs
!            * 1 secondary node ID
!            * 1 segment ID
!            * type of interface
!$ENDCOMMENT

        USE INTER_CAND_MOD
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
        INTEGER, INTENT(inout) :: NELEMINT
        INTEGER, INTENT(in) :: INTERFACE_ID !< interface id 
        INTEGER, INTENT(in) :: NSN !< number of S node
        INTEGER, INTENT(in) :: NRTM !< number of segment
        INTEGER, INTENT(inout) :: IFIEND !< ???
        INTEGER, INTENT(in) :: N2D !< flag for 2D/3D, 0-->3D, 1-->2D
        INTEGER, DIMENSION(NSN), INTENT(in) :: IRTL
        INTEGER, DIMENSION(4,NRTM), INTENT(in) :: IRECT !< list of M nodes for the NRTM segments
        INTEGER, DIMENSION(NSN), INTENT(in) :: NSV !< list of S nodes
        TYPE(INTER_CAND_), INTENT(inout) :: INTER_CAND
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER N,NIR,NN
        INTEGER :: S_NODE_ID
        INTEGER :: SEGMENT_ID
C-----------------------------------------------
        ! flush %ixint array to 0
        INTER_CAND%IXINT(1:INTER_CAND%S_IXINT_1,NELEMINT+1:NELEMINT+NSN) = 0
        INTER_CAND%ADDRESS(INTERFACE_ID) = NELEMINT+1 ! save the adress of the first pair 
        NIR=2
        IF(N2D==0)NIR=4
        NN = 0            
        ! ---------------------------
        ! loop over the secondary node
        DO N = 1, NSN
          S_NODE_ID = NSV(N)  ! S node id
          SEGMENT_ID = IRTL(N) ! segment id
          IF(SEGMENT_ID/=0)THEN
            NN = NN + 1
            INTER_CAND%IXINT(1,NELEMINT+NN)=IRECT(1,SEGMENT_ID)
            INTER_CAND%IXINT(2,NELEMINT+NN)=IRECT(2,SEGMENT_ID)
            IF (NIR==2) THEN
              INTER_CAND%IXINT(3,NELEMINT+NN)=IRECT(1,SEGMENT_ID)
              INTER_CAND%IXINT(4,NELEMINT+NN)=IRECT(2,SEGMENT_ID)
            ELSE
              INTER_CAND%IXINT(3,NELEMINT+NN)=IRECT(3,SEGMENT_ID)
              INTER_CAND%IXINT(4,NELEMINT+NN)=IRECT(4,SEGMENT_ID)                
            END IF
            INTER_CAND%IXINT(5,NELEMINT+NN)=S_NODE_ID
            INTER_CAND%IXINT(6,NELEMINT+NN)=2
            INTER_CAND%IXINT(7,NELEMINT+NN)=SEGMENT_ID
            INTER_CAND%IXINT(8,NELEMINT+NN)=INTERFACE_ID
          END IF
        ENDDO
        ! ---------------------------
        NELEMINT = NELEMINT+NN
        IFIEND   = IFIEND + NN
        INTER_CAND%ADDRESS(INTERFACE_ID+1) = NELEMINT ! save the adress of the last pair
        ! ---------------------------
        RETURN
        END SUBROUTINE UPDATE_WEIGHT_INTER_TYPE2
      
