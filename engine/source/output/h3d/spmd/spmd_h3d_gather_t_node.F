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
Chd|  SPMD_H3D_GATHER_T_NODE        source/output/h3d/spmd/spmd_h3d_gather_t_node.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE SPMD_H3D_GATHER_T_NODE(WEIGHT,V,LEN,VP0,LENP0)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#ifdef MPI
#include      "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER LEN,LENP0,WEIGHT(*)
       my_real
     .   V(LEN),VP0(LENP0),V_TMP(LEN)

C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
       INTEGER IAD,J,LEN_TMP
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,
     .        MSGTYP,I,SIZ,LENP(NSPMD),DISP(NSPMD)



        J = 0
        DO I=1,LEN/6
          IF(WEIGHT(I) == 1) THEN
            V_TMP(J+1) = V(6*(I-1)+1)
            V_TMP(J+2) = V(6*(I-1)+2)
            V_TMP(J+3) = V(6*(I-1)+3)
            V_TMP(J+4) = V(6*(I-1)+4)
            V_TMP(J+5) = V(6*(I-1)+5)
            V_TMP(J+6) = V(6*(I-1)+6)
            J = J + 6
          ENDIF
        ENDDO
        LEN_TMP = J
         
        CALL MPI_GATHER(
     S    LEN_TMP       ,1     ,MPI_INTEGER,
     R    LENP          ,1     ,MPI_INTEGER,IT_SPMD(1),
     G    MPI_COMM_WORLD,IERROR)
C
        IAD=0
        IF(ISPMD == 0)THEN
          DO I=1,NSPMD
            DISP(I) = IAD
            IAD = IAD+LENP(I)
          END DO
        END IF
C
        CALL MPI_GATHERV(
     S    V_TMP         ,LEN_TMP ,REAL,
     R    VP0           ,LENP  ,DISP,REAL,IT_SPMD(1),
     G    MPI_COMM_WORLD,IERROR)
#endif
       RETURN
       END
