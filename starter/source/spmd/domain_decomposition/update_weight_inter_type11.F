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
        SUBROUTINE UPDATE_WEIGHT_INTER_TYPE11(NELEMINT,INTERFACE_ID,NSN,NRTM,IFIEND,
     .                                       IRECTM,IRECTS,I_STOK,CAND_E,CAND_N,
     .                                       INTER_CAND)
!$COMMENT
!       UPDATE_WEIGHT_INTER_TYPE11 description :
!       save the contact data for interface type 11 
!
!       UPDATE_WEIGHT_INTER_TYPE11 organization :
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(inout) :: NELEMINT
        INTEGER, INTENT(in) :: INTERFACE_ID !< interface id 
        INTEGER, INTENT(in) :: NSN !< number of S node
        INTEGER, INTENT(in) :: NRTM !< number of segment
        INTEGER, INTENT(inout) :: IFIEND !< ???
        INTEGER, DIMENSION(2,NRTM) :: IRECTM !< list of M nodes for the NRTM segments
        INTEGER, DIMENSION(2,NSN) :: IRECTS !< list of S nodes for the NSN segments
        INTEGER, INTENT(in) :: I_STOK !< total number of pair of candidate
        INTEGER, DIMENSION(I_STOK), INTENT(in) :: CAND_E !< pointer to M node of the candidate I
        INTEGER, DIMENSION(I_STOK), INTENT(in) :: CAND_N !< pointer to S node id of the candidate I
        TYPE(INTER_CAND_), INTENT(inout) :: INTER_CAND
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I
C-----------------------------------------------
        IFIEND = IFIEND + I_STOK
        INTER_CAND%IXINT(1:INTER_CAND%S_IXINT_1,NELEMINT+1:NELEMINT+I_STOK) = 0
        INTER_CAND%ADDRESS(INTERFACE_ID) = NELEMINT ! save the adress of the first pair 
        ! ---------------------------
        DO I = 1, I_STOK
          INTER_CAND%IXINT(1,NELEMINT+I)=IRECTS(1,CAND_N(I))
          INTER_CAND%IXINT(2,NELEMINT+I)=IRECTS(2,CAND_N(I))
          INTER_CAND%IXINT(3,NELEMINT+I)=IRECTM(1,CAND_E(I))
          INTER_CAND%IXINT(4,NELEMINT+I)=IRECTM(2,CAND_E(I))
          INTER_CAND%IXINT(5,NELEMINT+I)=IRECTS(1,CAND_N(I)) ! choose one S node
          INTER_CAND%IXINT(6,NELEMINT+I)=11
          INTER_CAND%IXINT(7,NELEMINT+I)=CAND_E(I)
          INTER_CAND%IXINT(8,NELEMINT+I)=INTERFACE_ID
        ENDDO
        ! ---------------------------
        NELEMINT=NELEMINT+I_STOK
        INTER_CAND%ADDRESS(INTERFACE_ID+1) = NELEMINT ! save the adress of the last pair
        ! ---------------------------

        RETURN
        END SUBROUTINE UPDATE_WEIGHT_INTER_TYPE11
