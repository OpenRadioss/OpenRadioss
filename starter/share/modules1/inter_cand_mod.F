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
Chd|====================================================================
      MODULE INTER_CAND_MOD

        TYPE INTER_CAND_
            INTEGER :: S_IXINT_1 = 8 !< 1rst dimension of IXINT array
            INTEGER :: S_IXINT_2 !< 2nd dimension of IXINT array
            INTEGER, DIMENSION(:,:), ALLOCATABLE :: IXINT !< "pseudo element" for interface (a segment + a secondary node)
            INTEGER, DIMENSION(:), ALLOCATABLE :: ADDRESS !< address array, dimension=ninter+1 (x) : adress of the first pair, (x+1)-(x) = number of pair for the interface x
        END TYPE INTER_CAND_

      END MODULE INTER_CAND_MOD
