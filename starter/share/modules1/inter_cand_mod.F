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
Chd|  INTER_CAND_MOD                share/modules1/inter_cand_mod.F
Chd|-- called by -----------
Chd|        DOMETIS                       source/spmd/domain_decomposition/grid2mat.F
Chd|        I20INI3                       source/interfaces/inter3d1/i20ini3.F
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|        ININTR                        source/interfaces/interf1/inintr.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        UPDATE_WEIGHT_INTER_TYPE11    source/spmd/domain_decomposition/update_weight_inter_type11.F
Chd|        UPDATE_WEIGHT_INTER_TYPE2     source/spmd/domain_decomposition/update_weight_inter_type2.F
Chd|        UPDATE_WEIGHT_INTER_TYPE7     source/spmd/domain_decomposition/update_weight_inter_type7.F
Chd|        UPDATE_WEIGHT_INTER_TYPE_24_25source/spmd/domain_decomposition/update_weight_inter_type_24_25.F
Chd|        UPDATE_WEIGHT_RBE3            source/spmd/domain_decomposition/update_weight_rbe3.F
Chd|        UPGRADE_IXINT                 source/interfaces/interf1/upgrade_ixint.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE INTER_CAND_MOD

        TYPE INTER_CAND_
            INTEGER :: S_IXINT_1 = 8 !< 1rst dimension of IXINT array
            INTEGER :: S_IXINT_2 !< 2nd dimension of IXINT array
            INTEGER, DIMENSION(:,:), ALLOCATABLE :: IXINT !< "pseudo element" for interface (a segment + a secondary node)
            INTEGER, DIMENSION(:), ALLOCATABLE :: ADDRESS !< address array, dimension=ninter+1 (x) : adress of the first pair, (x+1)-(x) = number of pair for the interface x
        END TYPE INTER_CAND_

      END MODULE INTER_CAND_MOD
