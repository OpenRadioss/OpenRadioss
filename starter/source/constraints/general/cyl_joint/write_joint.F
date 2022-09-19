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
Chd|  WRITE_JOINT                   source/constraints/general/cyl_joint/write_joint.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        JOINT_MOD                     share/modules1/joint_mod.F    
Chd|====================================================================
            SUBROUTINE WRITE_JOINT(LJOINT,CEP,CEL,PROC,
     +                   NODLOCAL,LJOINT_L,LEN_IA,NUMNOD_L)
!$COMMENT
!       WRITE_JOINT description
!       write the joint structure
!
!       WRITE_JOINT organization :
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------     
            USE JOINT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER  PROC, LJOINT_L, LEN_IA, NODLOCAL(*),
     .          LJOINT(*), CEP(*), CEL(*),NUMNOD_L
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER N,NN,K,J,P,I,NUMBER_MAIN_NODE,IS_SMS_AVAILABLE
        INTEGER, DIMENSION(:), ALLOCATABLE :: BUF_W
        INTEGER, DIMENSION(NSPMD) :: NB_NODE_WEIGHT
        INTEGER, DIMENSION(2) :: MAIN_NODE
        INTEGER, DIMENSION(:), ALLOCATABLE :: SECONDARY_NODE
C
        P = PROC + 1
        !   ----------------------
        !   count the number of data in order to allocate the buffer
        K = 0
        DO N=1,NJOINT
            IF(CYL_JOIN(N)%NB_NODE(P)>0) THEN
                K = K + 4 + CYL_JOIN(N)%NUMBER_PROC + 2*CYL_JOIN(N)%NB_NODE(P) + NSPMD
            ENDIF
        ENDDO
        K = K + NJOINT
        !   ----------------------
        !   buffer allocation
        ALLOCATE(BUF_W(K))

        !   write for all processor the number of node per proc
        DO N=1,NJOINT
            BUF_W(N) = CYL_JOIN(N)%NB_NODE(P)
        ENDDO
        CALL WRITE_I_C(BUF_W,NJOINT)
        LEN_IA = LEN_IA + NJOINT
        !   ----------------------
        !   loop over the CYL_JOINT
        DO N=1,NJOINT
            NB_NODE_WEIGHT(1:NSPMD) = 0
            !   ----------------------
            !   if 1 or several nodes are on the current processor, write all the stuff
            IF(CYL_JOIN(N)%NB_NODE(P)>0) THEN
                !   main processor
                CALL WRITE_I_C(CYL_JOIN(N)%PROC_MAIN,1)
                !   number of processor for the current joint
                CALL WRITE_I_C(CYL_JOIN(N)%NUMBER_PROC,1)
                !   list of processor for the current joint
                CALL WRITE_I_C(CYL_JOIN(N)%LIST_PROC,CYL_JOIN(N)%NUMBER_PROC)

                ALLOCATE( SECONDARY_NODE(CYL_JOIN(N)%NB_NODE(P)) )
                DO I=1,CYL_JOIN(N)%NB_NODE(P)
                    SECONDARY_NODE(I) = NODLOCAL(CYL_JOIN(N)%PROC(P)%NODE(I))
                ENDDO
                !   number of node for the curent processor and joint
                CALL WRITE_I_C(SECONDARY_NODE,CYL_JOIN(N)%NB_NODE(P))
                !   weight array (0 or 1) for the curent processor and joint
                CALL WRITE_I_C(CYL_JOIN(N)%PROC(P)%WEIGHT,CYL_JOIN(N)%NB_NODE(P))
                DEALLOCATE( SECONDARY_NODE )
            ENDIF
            !   ----------------------
            DO I=1,NSPMD
                NB_NODE_WEIGHT(I) = CYL_JOIN(N)%PROC(I)%NB_NODE_WEIGHT
            ENDDO
            !   number of node with weight = 1 (all processor) ; use for mpi comm
            CALL WRITE_I_C(NB_NODE_WEIGHT,NSPMD)
            MAIN_NODE(1:2) = NODLOCAL(CYL_JOIN(N)%MAIN_NODE(1:2))
            !   main nodes (all processor)
            CALL WRITE_I_C(MAIN_NODE,2)
        ENDDO

        IS_SMS_AVAILABLE = 0
        IF(JOINT_SMS) IS_SMS_AVAILABLE = 1
        CALL WRITE_I_C(IS_SMS_AVAILABLE,1)

        LEN_IA = LEN_IA + K + 1
        DEALLOCATE(BUF_W)
C
        RETURN
        END SUBROUTINE WRITE_jOINT
