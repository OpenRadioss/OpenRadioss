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
Chd|  LAW_USER                      source/user_interface/law_user.F
Chd|-- called by -----------
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|        HM_READ_MAT_99                source/materials/mat/matuser/hm_read_mat_user_99.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE LAW_USER
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C IMAT    |     1   | I | R | MATERIAL ID
C---------+---------+---+---+--------------------------------------------
C NAME    |   100   | C | R | MATERIAL NAME
C---------+---------+---+---+--------------------------------------------
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C
      TYPE ULAWBUF
       SEQUENCE
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C integer
        INTEGER   ID
C character
        CHARACTER NAME*500
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
C
C-----------------------------------------------
       END TYPE ULAWBUF
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      END MODULE LAW_USER
