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
Chd|  W_CRKXFEM                     source/restart/ddsplit/w_crkxfem.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE W_CRKXFEM(IELC_L,IELTG_L,INOD_L,NUMELC_L,
     .                     NUMELTG_L,NUMNOD_L,LEN_IA,NUMNODCRK_L,
     .                     NODLEVXF_L)
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
      INTEGER IELC_L(*),IELTG_L(*),INOD_L(*),LEN_IA,
     .        NUMELC_L,NUMELTG_L,NUMNOD_L,NUMNODCRK_L,NODLEVXF_L(*)
C=======================================================================
      CALL WRITE_I_C(INOD_L,NUMNOD_L)     ! nodes std
      LEN_IA = LEN_IA + NUMNOD_L
C      
      CALL WRITE_I_C(IELC_L,NUMELC_L)     ! shell 4N std
      LEN_IA = LEN_IA + NUMELC_L
C
      CALL WRITE_I_C(IELTG_L,NUMELTG_L)   ! shell 3N std 
      LEN_IA = LEN_IA + NUMELTG_L
C
      CALL WRITE_I_C(NODLEVXF_L,NUMNODCRK_L)  ! NLEV de chaque noeud local
      LEN_IA = LEN_IA + NUMNODCRK_L
c-----------         
      RETURN
      END
