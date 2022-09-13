Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
C
Chd|====================================================================
Chd|  SPMD_FRWALL_NN                source/mpi/kinematic_conditions/spmd_frwall_nn.F
Chd|-- called by -----------
Chd|        SMS_BUILD_DIAG                source/ams/sms_build_diag.F   
Chd|        SMS_INI_KIN_1                 source/ams/sms_init.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_FRWALL_NN(FR_WALL,IWADD)
C 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FR_WALL(NSPMD+2), IWADD
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGOFF2,MSGTYP,INFO,P,PMAIN, LOC_PROC, J, LEN
      DATA MSGOFF/112/
      DATA MSGOFF2/113/
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
      INTEGER ITMP
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LEN=1
C
      PMAIN = FR_WALL(NSPMD+2)
      LOC_PROC = ISPMD+1
      IF(LOC_PROC==PMAIN) THEN
        DO P = 1, NSPMD
          IF(LOC_PROC/=P.AND.FR_WALL(P)/=0)THEN
            MSGTYP = MSGOFF
            CALL MPI_RECV(ITMP  ,LEN           ,MPI_INTEGER,IT_SPMD(P),
     .                    MSGTYP,MPI_COMM_WORLD,STATUS,IERROR    )
C
            IWADD = IWADD + ITMP
          ENDIF
        ENDDO
C
        DO P = 1, NSPMD
          IF(LOC_PROC/=P.AND.FR_WALL(P)/=0)THEN
            MSGTYP = MSGOFF2
            CALL MPI_SEND(IWADD ,LEN           ,MPI_INTEGER,IT_SPMD(P),
     .                    MSGTYP,MPI_COMM_WORLD,IERROR)
          ENDIF
        ENDDO
C
      ELSE
        MSGTYP = MSGOFF
        CALL MPI_SEND(IWADD ,LEN          ,MPI_INTEGER,IT_SPMD(PMAIN),
     .                MSGTYP,MPI_COMM_WORLD,IERROR)
        MSGTYP = MSGOFF2
        CALL MPI_RECV(IWADD ,LEN          ,MPI_INTEGER,IT_SPMD(PMAIN),
     .                MSGTYP,MPI_COMM_WORLD,STATUS,IERROR    )
      ENDIF
C
#endif
      RETURN
      END
