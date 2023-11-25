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
C.../TASK/
      INTEGER INTSEG, PARASIZ, NTHMAX
      PARAMETER(INTSEG=128, PARASIZ=8192, NTHMAX=512)
      INTEGER NTHREAD
      COMMON /TASK/ NTHREAD
      INTEGER ILOCK
      COMMON /TASK/ ILOCK
      INTEGER NRBDONE
      COMMON /TASK/ NRBDONE
      INTEGER NGDONE
      COMMON /TASK/ NGDONE
      INTEGER NSGDONE
      COMMON /TASK/ NSGDONE
      INTEGER I7KGLO
      COMMON /TASK/ I7KGLO
      INTEGER PARALLEL_END
      COMMON /TASK/ PARALLEL_END
      INTEGER PARALLEL_SECTION
      COMMON /TASK/ PARALLEL_SECTION
      INTEGER ITASKP1_DEF
      COMMON /TASK/ ITASKP1_DEF
      INTEGER KMP_SET
      COMMON /TASK/ KMP_SET
      INTEGER KMPSTSIZ
      COMMON /TASK/ KMPSTSIZ
      INTEGER STACKSIZ
      COMMON /TASK/ STACKSIZ

      INTEGER REAL ! MPI_DOUBLE_PRECISION or MPI_REAL
      COMMON /SPMD/ REAL
      INTEGER ISPMD
      COMMON /SPMD/ ISPMD
      INTEGER IT_SPMD(PARASIZ)
      COMMON /SPMD/ IT_SPMD
      INTEGER IEXICODT
      COMMON /SPMD/ IEXICODT
      INTEGER IDDW
      COMMON /SPMD/ IDDW
      INTEGER IPREAD
      COMMON /SPMD/ IPREAD
      LOGICAL L_SPMD(PARASIZ)
      COMMON /SPMD/ L_SPMD
      INTEGER NSPMD_PER_NODE
      COMMON /SPMD/ NSPMD_PER_NODE
      INTEGER IDDWSTAT
      COMMON /SPMD/ IDDWSTAT
      INTEGER COMM_CONT !MPI communicator for contact search global criteria (syncmmx)
      COMMON /SPMD/ COMM_CONT

