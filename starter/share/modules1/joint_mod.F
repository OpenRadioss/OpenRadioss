Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    joint_mod          ../starter/share/modules1/joint_mod.F
      !||--- called by ------------------------------------------------------
      !||    ddsplit            ../starter/source/restart/ddsplit/ddsplit.F
      !||    deallocate_joint   ../starter/source/constraints/general/cyl_joint/deallocate_joint.F
      !||    hm_read_cyljoint   ../starter/source/constraints/general/cyl_joint/hm_read_cyljoint.F
      !||    init_joint         ../starter/source/constraints/general/cyl_joint/init_joint.F
      !||    lectur             ../starter/source/starter/lectur.F
      !||    print_joint        ../starter/source/constraints/general/cyl_joint/split_joint.F
      !||    split_joint        ../starter/source/constraints/general/cyl_joint/split_joint.F
      !||    write_joint        ../starter/source/constraints/general/cyl_joint/write_joint.F
      !||====================================================================
        MODULE JOINT_MOD
!$COMMENT
!       JOINT_MOD description
!       module for /CYL_JOINT option
!       
!       JOINT_MOD organization :
!       the main /CYL_JOINT structure is CYL_JOINT
!       the type(joint_node) proc structure is used for the restart writing
!       the type(joint_main_node) structure is not really useful :)
!$ENDCOMMENT
            TYPE JOINT_NODE
                INTEGER, DIMENSION(:), ALLOCATABLE :: NODE          ! list of node
                INTEGER :: NB_NODE_WEIGHT                           ! number of node with weight =1
                INTEGER, DIMENSION(:), ALLOCATABLE :: WEIGHT        ! weight array (1 or 0)
            END TYPE JOINT_NODE
            TYPE JOINT_TYPE
                INTEGER :: PROC_MAIN
                INTEGER :: NUMBER_PROC
                INTEGER, DIMENSION(:), ALLOCATABLE :: MAIN_NODE   ! main node
                INTEGER, DIMENSION(:), ALLOCATABLE :: LIST_PROC     ! list of processor 
                INTEGER, DIMENSION(:), ALLOCATABLE :: NB_NODE       ! number of node on 
                TYPE(joint_node), DIMENSION(:), ALLOCATABLE :: PROC
                INTEGER :: NB_SECONDARY_NODE                        ! number of secondary node
                INTEGER, DIMENSION(:),ALLOCATABLE :: SECONDARY_NODE ! list of secondary node
            END TYPE JOINT_TYPE

            TYPE(joint_type), DIMENSION(:), ALLOCATABLE :: CYL_JOIN
            LOGICAL :: JOINT_SMS

            TYPE JOINT_MAIN_NODE
                INTEGER :: ID_JOINT
                INTEGER :: NB_PROC
                INTEGER, DIMENSION(:), ALLOCATABLE :: PROC_LIST
            END TYPE JOINT_MAIN_NODE
        END MODULE JOINT_MOD
