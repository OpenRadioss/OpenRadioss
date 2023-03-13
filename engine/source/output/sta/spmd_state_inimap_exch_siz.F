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
Chd|  SPMD_STATE_INIMAP_EXCH_SIZ    source/output/sta/spmd_state_inimap_exch_siz.F
Chd|-- called by -----------
Chd|        STAT_INIMAP1D_FILE_SPMD       source/output/sta/stat_inimap1d_file_spmd.F
Chd|        STAT_INIMAP1D_SPMD            source/output/sta/stat_inimap1d_spmd.F
Chd|        STAT_INIMAP2D_FILE_SPMD       source/output/sta/stat_inimap2d_file_spmd.F
Chd|        STAT_INIMAP2D_SPMD            source/output/sta/stat_inimap2d_spmd.F
Chd|-- calls ---------------
Chd|        STATE_INIMAP_MOD              share/modules/state_inimap_mod.F
Chd|====================================================================
       SUBROUTINE SPMD_STATE_INIMAP_EXCH_SIZ()
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE STATE_INIMAP_MOD
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
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGOFF
      INTEGER SIZ,MSGTYP,I,K,NG,NREC,MSGOFF2

      DATA MSGOFF/10002/
      DATA MSGOFF2/10002/
      INTEGER BUFF_I(4)
      my_real BUFF_R(5)      

       IF (ISPMD/=0) THEN
         BUFF_I(1) = STATE_INIMAP_BUF(1)%NUM_CENTROIDS
         BUFF_I(2) = STATE_INIMAP_BUF(1)%NUM_POINTS
         BUFF_I(3) = STATE_INIMAP_BUF(1)%MLW
         BUFF_I(4) = STATE_INIMAP_BUF(1)%NSUBMAT

         BUFF_R(1) = STATE_INIMAP_BUF(1)%LENGTH
         BUFF_R(2) = STATE_INIMAP_BUF(1)%SHIFT_Ny
         BUFF_R(3) = STATE_INIMAP_BUF(1)%SHIFT_Cy
         BUFF_R(4) = STATE_INIMAP_BUF(1)%SHIFT_Nz
         BUFF_R(5) = STATE_INIMAP_BUF(1)%SHIFT_Cz
         
         MSGTYP = MSGOFF2 
         CALL MPI_SEND(BUFF_I, 4,MPI_INTEGER,IT_SPMD(1),MSGTYP,MPI_COMM_WORLD,ierror)

         MSGTYP = MSGOFF 
         CALL MPI_SEND(BUFF_R, 5,       REAL,IT_SPMD(1),MSGTYP,MPI_COMM_WORLD,ierror)
          
       ELSE

          DO I=2,NSPMD

           !Reception du buffer entier des adresses DATA_I
            MSGTYP = MSGOFF2 
            CALL MPI_PROBE(IT_SPMD(I),MSGTYP,MPI_COMM_WORLD,STATUS,ierror)
            CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,SIZ,ierror)
            CALL MPI_RECV(BUFF_I,4,MPI_INTEGER,IT_SPMD(I),MSGTYP,MPI_COMM_WORLD,STATUS,ierror)
            
            ! Reception du buffer flottant double des adresses DATA_I
            MSGTYP = MSGOFF 
            CALL MPI_RECV(BUFF_R,5,REAL,IT_SPMD(I),MSGTYP,MPI_COMM_WORLD,STATUS,ierror)
            
            STATE_INIMAP_BUF(I)%NUM_CENTROIDS = BUFF_I(1)
            STATE_INIMAP_BUF(I)%NUM_POINTS = BUFF_I(2)
            STATE_INIMAP_BUF(I)%MLW = BUFF_I(3)
            STATE_INIMAP_BUF(I)%NSUBMAT = BUFF_I(4)

            STATE_INIMAP_BUF(I)%LENGTH = BUFF_R(1)
            STATE_INIMAP_BUF(I)%SHIFT_Ny = BUFF_R(2)
            STATE_INIMAP_BUF(I)%SHIFT_Cy = BUFF_R(3)
            STATE_INIMAP_BUF(I)%SHIFT_Nz = BUFF_R(4)
            STATE_INIMAP_BUF(I)%SHIFT_Cz = BUFF_R(5)
                       
          ENDDO

       ENDIF

#endif
       RETURN
       END SUBROUTINE SPMD_STATE_INIMAP_EXCH_SIZ
