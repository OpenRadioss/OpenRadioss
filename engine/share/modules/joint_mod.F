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
Chd|  JOINT_MOD                     share/modules/joint_mod.F     
Chd|-- called by -----------
Chd|        CJOINT                        source/constraints/general/cyl_joint/cjoint.F
Chd|        DEALLOCATE_JOINT              source/constraints/general/cyl_joint/deallocate_joint.F
Chd|        READ_JOINT                    source/output/restart/read_joint.F
Chd|        SPMD_SPLIT_COMM_JOINT         source/mpi/init/spmd_split_comm_joint.F
Chd|        TELESC                        source/constraints/general/cyl_joint/telesc.F
Chd|        WRITE_JOINT                   source/output/restart/write_joint.F
Chd|-- calls ---------------
Chd|====================================================================
        MODULE JOINT_MOD
#include "my_real.inc"
!$COMMENT
!       JOINT_MOD description
!       module for /CYL_JOINT option
!       
!       JOINT_MOD organization :
!       the main /CYL_JOINT structure is CYL_JOINT
!       the type(comm_mpi_joint) proc structure is used for mpi communication
!       **_GLOBAL & **_6 variables are save variables (omp)
!$ENDCOMMENT
            TYPE COMM_MPI_JOINT
                INTEGER :: COLOR        ! color of the processor : 1 is the proc participates to the comm
                INTEGER :: COMM,RANK    ! sub communicator for a given /CYL_JOINT + rank iin the sub communicator
                INTEGER, DIMENSION(:), ALLOCATABLE :: SIZE_SR   ! 
            END TYPE COMM_MPI_JOINT

            TYPE JOINT_TYPE
                INTEGER :: PROC_MASTER                                  !   proc master
                INTEGER :: NUMBER_MASTER_NODE                           !   number of master node
                INTEGER :: NUMBER_PROC,NUMBER_NODE,NUMBER_NODE_WEIGHT   !   number of proc / node / weight
                INTEGER, DIMENSION(:), ALLOCATABLE :: MASTER_NODE   !   list of master node
                INTEGER, DIMENSION(:), ALLOCATABLE :: LIST_PROC     !   list of processor
                INTEGER, DIMENSION(:), ALLOCATABLE :: NODE          !   list of node
                INTEGER, DIMENSION(:), ALLOCATABLE :: WEIGHT        !   weight array
                INTEGER, DIMENSION(:), ALLOCATABLE :: NODE_WEIGHT   !   number of node with weight = 1
                TYPE(comm_mpi_joint) :: COMM_MPI
            END TYPE JOINT_TYPE

            TYPE(joint_type), DIMENSION(:), ALLOCATABLE :: CYL_JOIN
            LOGICAL :: JOINT_SMS

            my_real, SAVE :: MASSE_GLOBAL,INER_GLOBAL 
            my_real, SAVE :: AX_GLOBAL,AY_GLOBAL,AZ_GLOBAL
            my_real, SAVE :: AXX_GLOBAL,AYY_GLOBAL,AZZ_GLOBAL
            my_real, SAVE :: VX_GLOBAL,VY_GLOBAL,VZ_GLOBAL
            my_real, SAVE :: VXX_GLOBAL,VYY_GLOBAL,VZZ_GLOBAL
            my_real, SAVE :: XCDG_GLOBAL,YCDG_GLOBAL,ZCDG_GLOBAL


            my_real, DIMENSION(:), ALLOCATABLE, SAVE :: MASS
            my_real, DIMENSION(:), ALLOCATABLE, SAVE :: X_MS,Y_MS,Z_MS
            my_real, DIMENSION(:), ALLOCATABLE, SAVE :: INER_VEC
            my_real, DIMENSION(:), ALLOCATABLE, SAVE :: AX_MS,AY_MS,AZ_MS
            my_real, DIMENSION(:), ALLOCATABLE, SAVE :: AXX_VEC,AYY_VEC,AZZ_VEC
            my_real, DIMENSION(:), ALLOCATABLE, SAVE :: VX_MS,VY_MS,VZ_MS
            my_real, DIMENSION(:), ALLOCATABLE, SAVE :: VXX_VEC,VYY_VEC,VZZ_VEC

            REAL(kind=8), DIMENSION(:,:), ALLOCATABLE, SAVE :: MASS_6
            REAL(kind=8), DIMENSION(:,:), ALLOCATABLE, SAVE :: X_MS_6,Y_MS_6,Z_MS_6
            REAL(kind=8), DIMENSION(:,:), ALLOCATABLE, SAVE :: INER_6
            REAL(kind=8), DIMENSION(:,:), ALLOCATABLE, SAVE :: AX_MS_6,AY_MS_6,AZ_MS_6
            REAL(kind=8), DIMENSION(:,:), ALLOCATABLE, SAVE :: AXX_6,AYY_6,AZZ_6
            REAL(kind=8), DIMENSION(:,:), ALLOCATABLE, SAVE :: VX_MS_6,VY_MS_6,VZ_MS_6
            REAL(kind=8), DIMENSION(:,:), ALLOCATABLE, SAVE :: VXX_6,VYY_6,VZZ_6
        END MODULE JOINT_MOD
