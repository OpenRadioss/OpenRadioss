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
Chd|  SPLIT_JOINT                   source/constraints/general/cyl_joint/split_joint.F
Chd|-- called by -----------
Chd|        DOMDEC2                       source/spmd/domdec2.F         
Chd|-- calls ---------------
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        PLIST_IFRONT                  source/spmd/node/ddtools.F    
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        JOINT_MOD                     share/modules1/joint_mod.F    
Chd|====================================================================
        SUBROUTINE SPLIT_JOINT( )
!$COMMENT
!       SPLIT_JOINT description
!       split the node (secondary & main node) on the different processor
!       
!       SPLIT_JOINT organization :
!       loop over the joint :
!           - for each joint, loop over the node to find where is the node
!           - for each node, add the ID node to the CYL_JOIN(I)%PROC(P)%NODE
!           - count the max number of secondary node in order to find the main proc
!           - if 1 secondary node is on a given proc --> need to add the 2 main nodes to the proc
!           - if 1 main node is on a given proc --> need to add the other main to the proc
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE JOINT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,II,NS,LJ,K
        INTEGER :: J,N,NN,IJK
        INTEGER :: WEIGHT,M1,M2
        INTEGER :: P,PROC_NUMBER,NB_NODE
        INTEGER, DIMENSION(NSPMD) :: PROC_LIST

        INTEGER :: PROC,PROC_NUMBER_1,PROC_NUMBER_2
        INTEGER, DIMENSION(NSPMD) :: PROC_LIST_1,PROC_LIST_2
        INTEGER, EXTERNAL :: NLOCAL  

        LOGICAL, DIMENSION(:), ALLOCATABLE :: CHECK_MAIN
        TYPE(joint_main_node), DIMENSION(:), ALLOCATABLE :: MAIN_AND_SECONDARY
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------

C=======================================================================
        K = 0
        ALLOCATE( CHECK_MAIN(NUMNOD) )
        CHECK_MAIN(1:NUMNOD) = .false.
        DO I=1,NJOINT
            DO J=1,2
                N = CYL_JOIN(I)%MAIN_NODE(J)
                CHECK_MAIN(N) = .true.
            ENDDO
        ENDDO
        ALLOCATE( MAIN_AND_SECONDARY(NUMNOD) )
        
        MAIN_AND_SECONDARY(1:NUMNOD)%ID_JOINT = 0
        MAIN_AND_SECONDARY(1:NUMNOD)%NB_PROC = 0

        DO I=1,NJOINT                
            PROC_LIST(1:NSPMD) = -1
            PROC_NUMBER = 0
            ALLOCATE( CYL_JOIN(I)%PROC(NSPMD) )
            CYL_JOIN(I)%NB_NODE(1:NSPMD) = 0
            CYL_JOIN(I)%PROC(1:NSPMD)%NB_NODE_WEIGHT = 0
            NS = CYL_JOIN(I)%NB_SECONDARY_NODE
            DO LJ=1,NS
                !   ----------------------
                !   check where is the secondary node 
                    PROC_LIST(1:NSPMD) = -1
                    PROC_NUMBER = 0
                    NN = CYL_JOIN(I)%SECONDARY_NODE(LJ)
	                CALL PLIST_IFRONT(PROC_LIST,NN,PROC_NUMBER)
                    !returns in "PROC_LIST" array list of SPMD domains on which node I is sticked
                    !PROC_NUMBER is the number of SPMD domains on which node I is sticked
                    
!                    IF( CHECK_MAIN(NN) ) THEN
!                        MAIN_AND_SECONDARY(NN)%ID_JOINT = I
!                        MAIN_AND_SECONDARY(NN)%NB_PROC = PROC_NUMBER
!                        ALLOCATE( MAIN_AND_SECONDARY(NN)%PROC_LIST(PROC_NUMBER) )
!                        MAIN_AND_SECONDARY(NN)%PROC_LIST(1:PROC_NUMBER) = PROC_LIST(1:PROC_NUMBER)
!                    ENDIF

                    ! add the secondary node to the CYL_JOINT structure for proc P
                    ! for the restart writing
                    DO II=1,PROC_NUMBER
                        P = PROC_LIST(II)
                        CYL_JOIN(I)%NB_NODE(P) = CYL_JOIN(I)%NB_NODE(P) + 1
                        IF(.NOT.ALLOCATED(CYL_JOIN(I)%PROC(P)%NODE)) ALLOCATE( CYL_JOIN(I)%PROC(P)%NODE(NS) )
                        CYL_JOIN(I)%PROC(P)%NODE( CYL_JOIN(I)%NB_NODE(P) ) = CYL_JOIN(I)%SECONDARY_NODE(LJ)
                        WEIGHT = 0
                        IF(II==1) WEIGHT = 1
                        IF(.NOT.ALLOCATED(CYL_JOIN(I)%PROC(P)%WEIGHT)) ALLOCATE( CYL_JOIN(I)%PROC(P)%WEIGHT(NS) )
                        CYL_JOIN(I)%PROC(P)%WEIGHT( CYL_JOIN(I)%NB_NODE(P) ) = WEIGHT
                        IF(WEIGHT==1) CYL_JOIN(I)%PROC(P)%NB_NODE_WEIGHT = CYL_JOIN(I)%PROC(P)%NB_NODE_WEIGHT + 1
                    ENDDO
                !   ----------------------
            ENDDO
            K=K+NS+1
        ENDDO

        !   ----------------------
        ! loop over the CYL_JOINT in order to find the proc main
        ! and add the main nodes to the processor where at least 1 
        ! secondary node is present
        K = 0
        DO I=1,NJOINT                
            PROC_LIST(1:NSPMD) = -1
            PROC_NUMBER = 0
            NS = CYL_JOIN(I)%NB_SECONDARY_NODE
            NB_NODE = 0
            PROC_NUMBER = 0
            CYL_JOIN(I)%PROC_MAIN = -1
            PROC_LIST(1:NSPMD) = 0
            DO P=1,NSPMD
                IF(CYL_JOIN(I)%NB_NODE(P)>0) THEN
                    PROC_NUMBER = PROC_NUMBER + 1
                    PROC_LIST(PROC_NUMBER) = P
                    NB_NODE = CYL_JOIN(I)%NB_NODE(P)
                    !   add the 2 main nodes on each proc
                    DO IJK = 1,2
                        M1 = CYL_JOIN(I)%MAIN_NODE(IJK)
                        IF(NLOCAL(M1,P)==0) THEN
                            CALL IFRONTPLUS(M1,P)
                            CYL_JOIN(I)%NB_NODE(P) = CYL_JOIN(I)%NB_NODE(P) + 1
                            CYL_JOIN(I)%PROC(P)%WEIGHT( CYL_JOIN(I)%NB_NODE(P) ) = 0
                            CYL_JOIN(I)%PROC(P)%NODE( CYL_JOIN(I)%NB_NODE(P) ) = CYL_JOIN(I)%SECONDARY_NODE(IJK)
                        ENDIF
                    ENDDO
                    IF(CYL_JOIN(I)%NB_NODE(P)>=NB_NODE) CYL_JOIN(I)%PROC_MAIN = P
                ENDIF
!                M1 = CYL_JOIN(I)%MAIN_NODE(1)
!                M2 = CYL_JOIN(I)%MAIN_NODE(2)
!                IF(NLOCAL(M1,P)==1) CALL IFRONTPLUS(M2,P)
!                IF(NLOCAL(M2,P)==1) CALL IFRONTPLUS(M1,P)
            ENDDO
            CYL_JOIN(I)%NUMBER_PROC = PROC_NUMBER
            ALLOCATE( CYL_JOIN(I)%LIST_PROC( PROC_NUMBER ) )
            CYL_JOIN(I)%LIST_PROC(1:PROC_NUMBER) = PROC_LIST(1:PROC_NUMBER)
            K=K+NS+1
        ENDDO



        DO I=NJOINT,1,-1         
            PROC_LIST(1:NSPMD) = -1
            PROC_NUMBER = 0
            NS = CYL_JOIN(I)%NB_SECONDARY_NODE
            NB_NODE = 0
            PROC_NUMBER = 0
            CYL_JOIN(I)%PROC_MAIN = -1
            PROC_LIST(1:NSPMD) = 0
            DO P=1,NSPMD
                IF(CYL_JOIN(I)%NB_NODE(P)>0) THEN
                    PROC_NUMBER = PROC_NUMBER + 1
                    PROC_LIST(PROC_NUMBER) = P
                    NB_NODE = CYL_JOIN(I)%NB_NODE(P)
                    !   add the 2 main nodes on each proc
                    DO IJK = 1,2
                        M1 = CYL_JOIN(I)%MAIN_NODE(IJK)
                        IF(NLOCAL(M1,P)==0) THEN
                            CALL IFRONTPLUS(M1,P)
                            CYL_JOIN(I)%NB_NODE(P) = CYL_JOIN(I)%NB_NODE(P) + 1
                            CYL_JOIN(I)%PROC(P)%WEIGHT( CYL_JOIN(I)%NB_NODE(P) ) = 0
                            CYL_JOIN(I)%PROC(P)%NODE( CYL_JOIN(I)%NB_NODE(P) ) = CYL_JOIN(I)%SECONDARY_NODE(IJK)
                        ENDIF
                    ENDDO
                    IF(CYL_JOIN(I)%NB_NODE(P)>=NB_NODE) CYL_JOIN(I)%PROC_MAIN = P
                ENDIF
!                M1 = CYL_JOIN(I)%MAIN_NODE(1)
!                M2 = CYL_JOIN(I)%MAIN_NODE(2)
!                IF(NLOCAL(M1,P)==1) CALL IFRONTPLUS(M2,P)
!                IF(NLOCAL(M2,P)==1) CALL IFRONTPLUS(M1,P)
            ENDDO
            K=K+NS+1
        ENDDO


        !   ----------------------
        ! check : if a main node is on a processor, need to add the other main node
!        K = 0
!        DO I=1,NJOINT                
!            PROC_LIST(1:NSPMD) = -1
!            PROC_NUMBER = 0
!            NS = CYL_JOIN(I)%NB_SECONDARY_NODE
!            NB_NODE = 0
!            PROC_NUMBER = 0
!            PROC_LIST_1(1:NSPMD) = 0
!            CALL PLIST_IFRONT(PROC_LIST_1,CYL_JOIN(I)%MAIN_NODE(1),PROC_NUMBER_1)
!            PROC_NUMBER_2 = 0
!            PROC_LIST_2(1:NSPMD) = 0
!            CALL PLIST_IFRONT(PROC_LIST_2,CYL_JOIN(I)%MAIN_NODE(2),PROC_NUMBER_2)!
!
!            DO P=1,PROC_NUMBER_1
!                PROC = PROC_LIST_1(P)
!                CALL IFRONTPLUS(CYL_JOIN(I)%MAIN_NODE(2),PROC)
!            ENDDO
!            DO P=1,PROC_NUMBER_2
!                PROC = PROC_LIST_2(P)
!                CALL IFRONTPLUS(CYL_JOIN(I)%MAIN_NODE(1),PROC)
!            ENDDO
!            K=K+NS+1
!        ENDDO
        !   ----------------------

        DEALLOCATE( MAIN_AND_SECONDARY )

        RETURN
        END SUBROUTINE SPLIT_JOINT




Chd|====================================================================
Chd|  print_JOINT                   source/constraints/general/cyl_joint/split_joint.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        PLIST_IFRONT                  source/spmd/node/ddtools.F    
Chd|        JOINT_MOD                     share/modules1/joint_mod.F    
Chd|====================================================================
        SUBROUTINE print_JOINT(a)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE JOINT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        character(len=*) :: a
        INTEGER :: I,II,NS,LJ,K
        INTEGER :: WEIGHT,M1,M2
        INTEGER :: P,PROC_NUMBER,NB_NODE
        INTEGER, DIMENSION(NSPMD) :: PROC_LIST
      INTEGER, EXTERNAL :: NLOCAL  
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------

C=======================================================================
        K = 0
        DO I=1,NJOINT                
            PROC_LIST(1:NSPMD) = -1
            PROC_NUMBER = 0
            NS = CYL_JOIN(I)%NB_SECONDARY_NODE

            NB_NODE = 0
            PROC_NUMBER = 0
            CYL_JOIN(I)%PROC_MAIN = -1
            PROC_LIST(1:NSPMD) = 0
            DO P=1,NSPMD
                M1 = CYL_JOIN(I)%MAIN_NODE(1)
                M2 = CYL_JOIN(I)%MAIN_NODE(2)
!                IF(NLOCAL(M1,P)==1) CALL IFRONTPLUS(M2,P)
!                IF(NLOCAL(M2,P)==1) CALL IFRONTPLUS(M1,P)
!                print*,' split_joint :',p,NLOCAL(M1,P),NLOCAL(M2,P)
            ENDDO
!            CYL_JOIN(I)%NUMBER_PROC = PROC_NUMBER
!            ALLOCATE( CYL_JOIN(I)%LIST_PROC( PROC_NUMBER ) )
!            CYL_JOIN(I)%LIST_PROC(1:PROC_NUMBER) = PROC_LIST(1:PROC_NUMBER)
            K=K+NS+1

                    PROC_NUMBER = 0
        CALL PLIST_IFRONT(PROC_LIST,CYL_JOIN(I)%MAIN_NODE(1),PROC_NUMBER)
        print*,' ------- '
        print*,i,' M1:',a,PROC_LIST(1:PROC_NUMBER)
                    PROC_NUMBER = 0
        CALL PLIST_IFRONT(PROC_LIST,CYL_JOIN(I)%MAIN_NODE(2),PROC_NUMBER)
        print*,i,' M2:',a,PROC_LIST(1:PROC_NUMBER)
        print*,' ------- '
        print*,' '

        ENDDO


        RETURN
        END SUBROUTINE print_JOINT
