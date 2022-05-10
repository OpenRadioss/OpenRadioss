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
Chd|  INT18_LAW151_UPDATE           source/interfaces/int18/int18_law151_update.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
        SUBROUTINE INT18_LAW151_UPDATE(ITASK,MULTI_FVM,IGRBRIC,IPARI,IXS,
     1                               IGROUPS ,IPARG   ,ELBUF_TAB  ,FORCE_INT   ,
     2                               X       ,V       ,MS         ,KINET       ,
     3                               X_APPEND,V_APPEND,MASS_APPEND,KINET_APPEND)
!$COMMENT
!       INT18_LAW151_UPDATE description
!       mass/position/velocity update
!       
!       INT18_LAW151_UPDATE organization :
!       - // with openmp
!       - update of the element buffer is mandatory (for 2nd order scheme)
!       - force_int array needs to be flush to 0 for the next cycle
!$ENDCOMMENT  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------      
        USE MULTI_FVM_MOD
        USE GROUPDEF_MOD
        USE ELBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc" 
#include      "com01_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(in) :: ITASK
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), INTENT(in) :: ELBUF_TAB
      INTEGER, DIMENSION(NUMELS), INTENT(in) ::IGROUPS
      INTEGER, DIMENSION(NPARG,*), INTENT(in) ::IPARG

      INTEGER, DIMENSION(NPARI,*), INTENT(in) :: IPARI
      my_real, DIMENSION(3,*), INTENT(in) :: X,V
      my_real, DIMENSION(3,*), INTENT(inout) :: X_APPEND,V_APPEND
      my_real, DIMENSION(*), INTENT(in) :: MS
      INTEGER, DIMENSION(*), INTENT(in) :: KINET
      my_real, DIMENSION(*), INTENT(inout) :: MASS_APPEND
      INTEGER, DIMENSION(*), INTENT(inout) :: KINET_APPEND
      my_real, DIMENSION(3,*), INTENT(inout) :: FORCE_INT
      INTEGER, DIMENSION(NIXS, *), INTENT(in) :: IXS
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
      TYPE (GROUP_)  , DIMENSION(NGRBRIC), INTENT(in) :: IGRBRIC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,J
        INTEGER :: N,NN,II,JJ,MY_SIZE
        INTEGER :: NFT,GROUP_ID,ILOC,NEL
        INTEGER :: ISU1,NBRIC,NSN,NTY,INACTI,NODE_ID,IBRIC
        INTEGER :: NODF,NODL,NSNF,NSNL
        my_real :: MASS
        my_real, DIMENSION(3) :: LOCAL_FORCE_INT
        REAL(kind=8), DIMENSION(3) :: LOCAL_FORCE_INT_DP
        !   parith/on array
        INTEGER :: SIZE_VEL
        INTEGER, DIMENSION(NTHREAD) :: INDEX_THREADS
        my_real, DIMENSION(:), ALLOCATABLE, SAVE :: VEL
        REAL(kind=8), DIMENSION(:,:), ALLOCATABLE, SAVE :: VEL_DP
C-----------------------------------------------
        NODF = 1 + ITASK * NUMNOD / NTHREAD
        NODL = (1 + ITASK) * NUMNOD / NTHREAD 

        !   1:NUMNOD --> classical x/v/mass
        IF(IALE/=0) X_APPEND(1:3,NODF:NODL) = X(1:3,NODF:NODL)
        V_APPEND(1:3,NODF:NODL) = V(1:3,NODF:NODL)
        MASS_APPEND(NODF:NODL) = MS(NODF:NODL)
        KINET_APPEND(NODF:NODL) = KINET(NODF:NODL)
            
        CALL MY_BARRIER()
        ! -------------------------------------
        ! update of vel array : parith/on part
        IF(IPARIT/=0) THEN
            DO NN=1,MULTI_FVM%NUMBER_INT18
                N = MULTI_FVM%INT18_LIST(NN)
                ISU1 = IPARI(45,N)
                NBRIC = IGRBRIC(ISU1)%NENTITY 
                NSN = IPARI(5,N)    ! number of secondary nodes
                NSNF = 1 + ITASK * NSN / NTHREAD
                NSNL = (1 + ITASK) * NSN / NTHREAD 

                DO I = 1,NTHREAD
                    INDEX_THREADS(I) = 1 + 3*(I-1)*NSN/NTHREAD
                ENDDO
!$OMP SINGLE
                ALLOCATE( VEL(3*NSN) )
                ALLOCATE( VEL_DP(6,3*NSN) )
                DO II = 1,NSN
                    IBRIC = IGRBRIC(ISU1)%ENTITY(II)    ! id of the phantom element
                    GROUP_ID = IGROUPS(IBRIC)       ! id of the element group
                    NFT = IPARG(3,GROUP_ID)         ! first elem of the group
                    NEL=IPARG(2,GROUP_ID)           ! number of element of the group
                    ILOC =  IBRIC - NFT

                    VEL_DP(1:6,(II-1)+1) = MULTI_FVM%FORCE_INT_PON(1,1:6,IBRIC)
                    VEL_DP(1:6,(II-1)+2) = MULTI_FVM%FORCE_INT_PON(2,1:6,IBRIC)
                    VEL_DP(1:6,(II-1)+3) = MULTI_FVM%FORCE_INT_PON(3,1:6,IBRIC)

                    MULTI_FVM%FORCE_INT_PON(1,1:6,IBRIC) = 0.d+00
                    MULTI_FVM%FORCE_INT_PON(2,1:6,IBRIC) = 0.d+00
                    MULTI_FVM%FORCE_INT_PON(3,1:6,IBRIC) = 0.d+00

                    DO J=2,NTHREAD
                        VEL_DP(1:6,(II-1)+1) = VEL_DP(1:6,(II-1)+1) + MULTI_FVM%FORCE_INT_PON(1,1:6,IBRIC+(J-1)*NUMELS)
                        VEL_DP(1:6,(II-1)+2) = VEL_DP(1:6,(II-1)+2) + MULTI_FVM%FORCE_INT_PON(2,1:6,IBRIC+(J-1)*NUMELS)
                        VEL_DP(1:6,(II-1)+3) = VEL_DP(1:6,(II-1)+3) + MULTI_FVM%FORCE_INT_PON(3,1:6,IBRIC+(J-1)*NUMELS)
                        MULTI_FVM%FORCE_INT_PON(1:3,1:6,IBRIC+(J-1)*NUMELS) = 0.d+00
                    ENDDO

                    MASS = ELBUF_TAB(GROUP_ID)%GBUF%RHO(ILOC) * ELBUF_TAB(GROUP_ID)%GBUF%VOL(ILOC)

                    LOCAL_FORCE_INT_DP(1) = VEL_DP(1,(II-1)+1)
                    LOCAL_FORCE_INT_DP(2) = VEL_DP(1,(II-1)+2)
                    LOCAL_FORCE_INT_DP(3) = VEL_DP(1,(II-1)+3)
                    DO J=2,6
                        LOCAL_FORCE_INT_DP(1) = LOCAL_FORCE_INT_DP(1) + VEL_DP(J,(II-1)+1)
                        LOCAL_FORCE_INT_DP(2) = LOCAL_FORCE_INT_DP(2) + VEL_DP(J,(II-1)+2)
                        LOCAL_FORCE_INT_DP(3) = LOCAL_FORCE_INT_DP(3) + VEL_DP(J,(II-1)+3)
                    ENDDO
                    LOCAL_FORCE_INT_DP(1:3) = LOCAL_FORCE_INT_DP(1:3) / MASS
                    MULTI_FVM%VEL(1:3,IBRIC) = MULTI_FVM%VEL(1:3,IBRIC) + LOCAL_FORCE_INT_DP(1:3)              
                ENDDO

                DEALLOCATE( VEL )
                DEALLOCATE( VEL_DP )
!$OMP END SINGLE
            ENDDO  
        ! -------------------------------------
        ! update of vel array : parith/off part
        ELSE
            DO NN=1,MULTI_FVM%NUMBER_INT18
                N = MULTI_FVM%INT18_LIST(NN)
                ISU1 = IPARI(45,N)
                NBRIC = IGRBRIC(ISU1)%NENTITY 
                NSN = IPARI(5,N)    ! number of secondary nodes
                NSNF = 1 + ITASK * NSN / NTHREAD
                NSNL = (1 + ITASK) * NSN / NTHREAD 
                DO II = NSNF,NSNL
                    IBRIC = IGRBRIC(ISU1)%ENTITY(II)    ! id of the phantom element
                    GROUP_ID = IGROUPS(IBRIC)       ! id of the element group
                    NFT = IPARG(3,GROUP_ID)         ! first elem of the group
                    NEL=IPARG(2,GROUP_ID)           ! number of element of the group
                    ILOC =  IBRIC - NFT
                    !   mass
                    MASS = ELBUF_TAB(GROUP_ID)%GBUF%RHO(ILOC) * ELBUF_TAB(GROUP_ID)%GBUF%VOL(ILOC)
                    LOCAL_FORCE_INT(1:3) = ZERO
                    DO JJ=1,NTHREAD
                        LOCAL_FORCE_INT(1:3) = LOCAL_FORCE_INT(1:3) + FORCE_INT(1:3, IBRIC+(JJ-1)*NUMELS)
                    ENDDO

                    MULTI_FVM%VEL(1:3, IBRIC) = MULTI_FVM%VEL(1:3, IBRIC) + LOCAL_FORCE_INT(1:3) / MASS

                    ! initialization of FORCE_INT for the next cycle
                    DO JJ=1,NTHREAD
                        FORCE_INT(1:3, IBRIC+(JJ-1)*NUMELS) = ZERO
                    ENDDO
                ENDDO
            ENDDO
        ENDIF  
        ! -------------------------------------


        !   NUMNOD+1:NUMNOD+NUMELS --> x/v/mass of phantom nodes (located to the center of 
        !                              the ALE elements)
        !                              x_phantom = sum( 1/8 * x(i), i=1,8)

        DO NN=1,MULTI_FVM%NUMBER_INT18
            N = MULTI_FVM%INT18_LIST(NN)
            ISU1 = IPARI(45,N)
            NBRIC = IGRBRIC(ISU1)%NENTITY 
            NSN = IPARI(5,N)    ! number of secondary nodes
            NSNF = 1 + ITASK * NSN / NTHREAD
            NSNL = (1 + ITASK) * NSN / NTHREAD 
            DO II = NSNF,NSNL
                IBRIC = IGRBRIC(ISU1)%ENTITY(II)    ! id of the phantom element
                GROUP_ID = IGROUPS(IBRIC)       ! id of the element group
                NFT = IPARG(3,GROUP_ID)         ! first elem of the group
                NEL=IPARG(2,GROUP_ID)           ! number of element of the group
                ILOC =  IBRIC - NFT
                !   mass
                MASS = ELBUF_TAB(GROUP_ID)%GBUF%RHO(ILOC) * ELBUF_TAB(GROUP_ID)%GBUF%VOL(ILOC)
                MASS_APPEND(NUMNOD + IBRIC) = ZERO!MASS
                !   position
                IF(IALE/=0) THEN
                    X_APPEND(1, NUMNOD + IBRIC) = ZERO
                    X_APPEND(2, NUMNOD + IBRIC) = ZERO
                    X_APPEND(3, NUMNOD + IBRIC) = ZERO
                    DO JJ = 2, 9
                        NODE_ID = IXS(JJ, IBRIC) ! id of node of the phantom element
                        X_APPEND(1, NUMNOD + IBRIC) = X_APPEND(1, NUMNOD + IBRIC) + UNHUIT * X(1, NODE_ID)
                        X_APPEND(2, NUMNOD + IBRIC) = X_APPEND(2, NUMNOD + IBRIC) + UNHUIT * X(2, NODE_ID) 
                        X_APPEND(3, NUMNOD + IBRIC) = X_APPEND(3, NUMNOD + IBRIC) + UNHUIT * X(3, NODE_ID)
                    ENDDO
                ENDIF
                ! --------------------------
                !   velocity
                V_APPEND(1, NUMNOD + IBRIC) = MULTI_FVM%VEL(1, IBRIC)
                V_APPEND(2, NUMNOD + IBRIC) = MULTI_FVM%VEL(2, IBRIC)
                V_APPEND(3, NUMNOD + IBRIC) = MULTI_FVM%VEL(3, IBRIC)

                !   update the element buffer
                ELBUF_TAB(GROUP_ID)%GBUF%MOM(ILOC+0*NEL)= MULTI_FVM%VEL(1, IBRIC)
                ELBUF_TAB(GROUP_ID)%GBUF%MOM(ILOC+1*NEL)= MULTI_FVM%VEL(2, IBRIC)
                ELBUF_TAB(GROUP_ID)%GBUF%MOM(ILOC+2*NEL)= MULTI_FVM%VEL(3, IBRIC)
            ENDDO
        ENDDO   

        RETURN
        END SUBROUTINE INT18_LAW151_UPDATE
C===============================================================================
