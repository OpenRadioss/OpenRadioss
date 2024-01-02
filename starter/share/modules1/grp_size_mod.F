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
Chd|  GRP_SIZE_MOD                  share/modules1/grp_size_mod.F 
Chd|-- called by -----------
Chd|        CONTRL                        source/starter/contrl.F       
Chd|        EXECARGCHECK                  source/starter/execargcheck.F 
Chd|-- calls ---------------
Chd|====================================================================
        MODULE GRP_SIZE_MOD
!$COMMENT
!       GRP_SIZE_MOD description
!       module for -grp_size command line option
!       -grp_size defines a ENGINE group size       
!
!       GRP_SIZE_MOD organization :
!       - GRP_SIZE_BOOL : logical, true if -grp_size is used
!       - GRP_SIZE : integer, group size 
!$ENDCOMMENT
            LOGICAL :: GRP_SIZE_BOOL
            INTEGER :: GRP_SIZE
        END MODULE GRP_SIZE_MOD
