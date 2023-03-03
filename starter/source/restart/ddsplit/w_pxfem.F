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
Chd|  W_PXFEM                       source/restart/ddsplit/w_pxfem.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE W_PXFEM(IEL_L   ,INOD_L ,INDEX,VEL1      ,VEL2 ,
     .             NUMELC_L,NUMNOD_L,NELPXFE_L,NNODPXFE_L,
     .             LEN_AM,LEN_IA,VEL3)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NELPXFE_L ,NNODPXFE_L,
     .     LEN_AM,LEN_IA,NUMELC_L ,NUMNOD_L  ,IEL_L(*),INOD_L(*),
     .     INDEX(*)
      my_real
     .        VEL1(NPLYXFE,*), VEL2(NPLYXFE,*),VEL3(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N_L, N,II,NEL_L,NEL1_L,JJ
      my_real
     .  VELTMP1(NNODPXFE_L,NPLYMAX), VELTMP2(NNODPXFE_L,NPLYMAX),
     .  VELTMP3(NNODPXFE_L)
C
      DO I = 1, NNODPXFE_L
          JJ = INDEX(I)
          VELTMP3(I) = VEL3(JJ)
          DO J = 1, NPLYMAX
            VELTMP1(I,J) = VEL1(JJ,J)
            VELTMP2(I,J) = VEL2(JJ,J)
          ENDDO
      ENDDO
C
      CALL WRITE_DB(VELTMP1,NPLYMAX*NNODPXFE_L)
      LEN_AM = LEN_AM + NPLYMAX*NNODPXFE_L
      CALL WRITE_DB(VELTMP2,NPLYMAX*NNODPXFE_L)
      LEN_AM = LEN_AM + NPLYMAX*NNODPXFE_L
C +1
      CALL WRITE_DB(VELTMP3,NNODPXFE_L)
      LEN_AM = LEN_AM + NNODPXFE_L
C
      CALL WRITE_I_C(INOD_L,NUMNOD_L)
      LEN_IA = LEN_IA + NUMNOD_L
C
      CALL WRITE_I_C(IEL_L,NUMELC_L)
      LEN_IA = LEN_IA + NUMELC_L
C
      RETURN
      END
