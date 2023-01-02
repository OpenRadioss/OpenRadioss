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
Chd|  READ_JOINT                    source/output/restart/read_joint.F
Chd|-- called by -----------
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|-- calls ---------------
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        JOINT_MOD                     share/modules/joint_mod.F     
Chd|        RESTMOD                       share/modules/restart_mod.F   
Chd|====================================================================
        SUBROUTINE READ_JOINT()
!$COMMENT
!       READ_JOINT description
!       read the joint structure 
!       
!       READ_JOINT organization :
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------     
        USE JOINT_MOD
        USE RESTMOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: N,I,NUMBER_MAIN_NODE,IS_SMS_AVAILABLE
        INTEGER, DIMENSION(2) :: MAIN_NODE
        INTEGER, DIMENSION(NJOINT) :: NUMBER_NODE
        INTEGER, DIMENSION(:), ALLOCATABLE :: NODE_WEIGHT

        ALLOCATE(CYL_JOIN(NJOINT))
        IF(NJOINT>0) THEN
            NUMBER_NODE(1:NJOINT) = 0
            !   number of node per proc (all proc --> size=nspmd)
            CALL READ_I_C(NUMBER_NODE,NJOINT)
            !   ------------------------------
            DO N=1,NJOINT     
                !   number of node and node with weight=1
                CYL_JOIN(N)%NUMBER_NODE = 0
                CYL_JOIN(N)%NUMBER_NODE_WEIGHT = 0
                !   number of node sent
                ALLOCATE( CYL_JOIN(N)%COMM_MPI%SIZE_SR(NSPMD) )
                CYL_JOIN(N)%COMM_MPI%SIZE_SR(1:NSPMD) = 0
                !   ------------------------------
                IF(NUMBER_NODE(N)>0) THEN
                    !   number of node for the current joint
                    CYL_JOIN(N)%NUMBER_NODE = NUMBER_NODE(N)
                    !   proc main for the current joint
                    CALL READ_I_C(CYL_JOIN(N)%PROC_MAIN,1)
                    !   number of proc for the current joint
                    CALL READ_I_C(CYL_JOIN(N)%NUMBER_PROC,1)
                    !   list of proc for the current joint
                    ALLOCATE( CYL_JOIN(N)%LIST_PROC(CYL_JOIN(N)%NUMBER_PROC) )
                    CALL READ_I_C(CYL_JOIN(N)%LIST_PROC,CYL_JOIN(N)%NUMBER_PROC)
                    !   list of node for the current joint
                    ALLOCATE( CYL_JOIN(N)%NODE(CYL_JOIN(N)%NUMBER_NODE) )
                    CALL READ_I_C(CYL_JOIN(N)%NODE,CYL_JOIN(N)%NUMBER_NODE)
                    !   weight array for the current joint
                    ALLOCATE( CYL_JOIN(N)%WEIGHT(CYL_JOIN(N)%NUMBER_NODE) )
                    CALL READ_I_C(CYL_JOIN(N)%WEIGHT,CYL_JOIN(N)%NUMBER_NODE)
                    !   -----------
                    !   weight array index for the current joint                 
                    ALLOCATE( NODE_WEIGHT( CYL_JOIN(N)%NUMBER_NODE ) )
                    DO  I=1,CYL_JOIN(N)%NUMBER_NODE
                        IF(CYL_JOIN(N)%WEIGHT(I)==1) THEN
                            CYL_JOIN(N)%NUMBER_NODE_WEIGHT = CYL_JOIN(N)%NUMBER_NODE_WEIGHT + 1
                            NODE_WEIGHT(CYL_JOIN(N)%NUMBER_NODE_WEIGHT) = CYL_JOIN(N)%NODE(I)
                        ENDIF
                    ENDDO
                    ALLOCATE( CYL_JOIN(N)%NODE_WEIGHT( CYL_JOIN(N)%NUMBER_NODE_WEIGHT ) )
                    DO I=1,CYL_JOIN(N)%NUMBER_NODE_WEIGHT
                        CYL_JOIN(N)%NODE_WEIGHT(I) = NODE_WEIGHT(I)
                    ENDDO                 
                    DEALLOCATE( NODE_WEIGHT )
                    !   -----------
                ENDIF
                !   ------------------------------
                !   number of sent node for mpi comm
                CALL READ_I_C(CYL_JOIN(N)%COMM_MPI%SIZE_SR,NSPMD)
                !   main node
                ALLOCATE( CYL_JOIN(N)%MAIN_NODE(2) )
                CALL READ_I_C(MAIN_NODE,2)
                CYL_JOIN(N)%MAIN_NODE(1:2) = MAIN_NODE(1:2)

                NUMBER_MAIN_NODE = 0
                DO I=1,2
                    IF(CYL_JOIN(N)%MAIN_NODE(I)>0) NUMBER_MAIN_NODE = NUMBER_MAIN_NODE + 1
                ENDDO
                !   number of main node 
                CYL_JOIN(N)%NUMBER_MAIN_NODE = NUMBER_MAIN_NODE
            ENDDO
            !   ------------------------------
            IS_SMS_AVAILABLE = 0
            CALL READ_I_C(IS_SMS_AVAILABLE,1)
            IF(IS_SMS_AVAILABLE==0) THEN
                JOINT_SMS = .FALSE.
            ELSE
                JOINT_SMS = .TRUE.
            ENDIF
            IF(JOINT_SMS) CALL READ_I_C(LJOINT,SLJOINT)

        ENDIF
C
        RETURN
        END SUBROUTINE READ_JOINT
