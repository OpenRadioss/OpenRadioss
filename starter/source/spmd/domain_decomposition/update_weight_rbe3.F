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
Chd|  UPDATE_WEIGHT_RBE3            source/spmd/domain_decomposition/update_weight_rbe3.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        INTER_CAND_MOD                share/modules1/inter_cand_mod.F
Chd|====================================================================
        SUBROUTINE UPDATE_WEIGHT_RBE3(NELEMINT,IFIEND,S_LRBE3,NRBE3L,NRBE3,
     .                                LRBE3,IRBE3,INTER_CAND)
        USE INTER_CAND_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(inout) :: NELEMINT !< number of new weight for rbe3
        INTEGER, INTENT(inout) :: IFIEND !< adress of the last weight
        INTEGER, INTENT(in) :: S_LRBE3 !< size of LRBE3 array
        INTEGER, INTENT(in) :: NRBE3L !< 1rst dim of size of IRBE3
        INTEGER, INTENT(in) :: NRBE3 !< 2nd dim of size of IRBE3, number of RBE3
        INTEGER, DIMENSION(S_LRBE3), INTENT(in) :: LRBE3 !< data for rbe3
        INTEGER, DIMENSION(NRBE3L,NRBE3), INTENT(in) :: IRBE3 !< data for rbe3
        TYPE(INTER_CAND_), INTENT(inout) :: INTER_CAND
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,J,NS,NM,NMT,M,NFT,JLT,IAD,II
C
C========================================================================|
        NMT = 0
        ! ---------------------------
        ! loop over the RBE3
        DO I=1,NRBE3
          IAD = IRBE3(1,I)
          NS = IRBE3(3,I)
          NM = IRBE3(5,I)
          DO NFT =0,NM-1 ,4
            JLT = MIN(4, NM - NFT )
            NELEMINT = NELEMINT + 1
            NMT = NMT + 1
            DO J = 1 ,JLT
              II = J+ NFT
              M= LRBE3(IAD+II)
              INTER_CAND%IXINT(J,NELEMINT)=M
            ENDDO
            IF (JLT==1) THEN
              INTER_CAND%IXINT(2,NELEMINT)=INTER_CAND%IXINT(1,NELEMINT)
              INTER_CAND%IXINT(3,NELEMINT)=INTER_CAND%IXINT(1,NELEMINT)
              INTER_CAND%IXINT(4,NELEMINT)=INTER_CAND%IXINT(1,NELEMINT)
            ELSEIF (JLT==2) THEN
              INTER_CAND%IXINT(3,NELEMINT)=INTER_CAND%IXINT(1,NELEMINT)
              INTER_CAND%IXINT(4,NELEMINT)=INTER_CAND%IXINT(2,NELEMINT)
            ELSEIF (JLT==3) THEN
              INTER_CAND%IXINT(4,NELEMINT)=INTER_CAND%IXINT(3,NELEMINT)
            ENDIF
            INTER_CAND%IXINT(5,NELEMINT)=NS
            INTER_CAND%IXINT(6,NELEMINT)=100
            INTER_CAND%IXINT(7:8,NELEMINT)=0
          ENDDO
        ENDDO
        ! ---------------------------
        IFIEND = IFIEND + NMT
        ! ---------------------------
        RETURN
        END SUBROUTINE UPDATE_WEIGHT_RBE3
