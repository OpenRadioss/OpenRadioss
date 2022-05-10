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
Chd|====================================================================
Chd|  COMM_TRI7VOX_MOD              share/modules/comm_tri7vox_mod.F
Chd|-- called by -----------
Chd|        SPMD_SPLIT_COMM_INTER         source/mpi/interfaces/spmd_split_comm_inter.F
Chd|        SPMD_TRI7VOX_OPTIMIZED        source/mpi/interfaces/spmd_tri7vox_optimized.F
Chd|-- calls ---------------
Chd|====================================================================
        MODULE COMM_TRI7VOX_MOD
#include      "my_real.inc"    
!$COMMENT
!       COMM_TRI7VOX_MOD :
!              create the comm_tri7vox_type
!               - INIT : logical,used only for
!                        communicator creation
!               - PROC_NUMBER : integer, number of processor
!                               for a given interface
!               - PROC_MIN : integer, master processor for a 
!                            a given interface
!               - COLOR : integer, color of the processor for
!                         a given interface (used only for
!                         communicator creation)
!               - COMM : integer, communicator for a given
!                        interface
!               - RANK : integer, rank of the processor for
!                        a given interface
!               - PROC_LIST : integer, list of processors for 
!                             a given interface
!$ENDCOMMENT
            TYPE comm_tri7vox_type
                LOGICAL :: INIT
                INTEGER :: PROC_NUMBER
                INTEGER :: PROC_MIN
                INTEGER :: COLOR
                INTEGER :: COMM,RANK
                INTEGER, DIMENSION(:), ALLOCATABLE :: PROC_LIST

            END TYPE comm_tri7vox_type

            TYPE(comm_tri7vox_type), DIMENSION(:), ALLOCATABLE :: COMM_TRI7VOX
        END MODULE COMM_TRI7VOX_MOD
