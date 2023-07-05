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
Chd|  INT18_LAW151_INIT             source/interfaces/int18/int18_law151_init.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|====================================================================
        SUBROUTINE INT18_LAW151_INIT(MULTI_FVM,IGRBRIC,IPARI,IXS,
     1                               IGROUPS ,IPARG   ,ELBUF_TAB  ,FORCE_INT   ,
     2                               X       ,V       ,MS         ,KINET       ,
     3                               X_APPEND,V_APPEND,MASS_APPEND,KINET_APPEND,
     4                               FORCE_INT_PON)
        
!$COMMENT
!       INT18_LAW151_INIT description
!       initialization of array for /INT18 + LAW 151
!       
!       INT18_LAW151_INIT organization :
!       - x/V/MASS_append :
!            * (1:NUMNOD) : classical x/v/mass
!            * (NUMNOD+1:NUMNOD+NUMELS) : phantom node x/v/mass
!       - kinet_append :
!            * (1:NUMNOD) : classical kinet
!            * (NUMNOD+1:NUMNOD+NUMELS) : =0 (no kinet for phantom node)
!       - force_int array :
!            * for parith/off :
!                  dimension = 3,NUMELS*NTHREADS
!                  each itask thread works in (1+itask*numels:(1+itask)*numels)
!$ENDCOMMENT
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------   
        USE MULTI_FVM_MOD
        USE GROUPDEF_MOD
        USE ELBUFDEF_MOD 
        USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, DIMENSION(NPARI,*), INTENT(in) :: IPARI
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), INTENT(in) :: ELBUF_TAB
      INTEGER, DIMENSION(NUMELS), INTENT(in) ::IGROUPS
      INTEGER, DIMENSION(NPARG,*), INTENT(in) ::IPARG
      my_real, DIMENSION(3,*), INTENT(in) :: X,V
      my_real, DIMENSION(3,*), INTENT(inout) :: X_APPEND,V_APPEND
      my_real, DIMENSION(*), INTENT(in) :: MS
      INTEGER, DIMENSION(*), INTENT(in) :: KINET
      my_real, DIMENSION(*), INTENT(inout) :: MASS_APPEND
      INTEGER, DIMENSION(*), INTENT(inout) :: KINET_APPEND
      my_real, DIMENSION(3,*), INTENT(inout) :: FORCE_INT
      REAL(kind=8), DIMENSION(3,6,*), INTENT(inout) :: FORCE_INT_PON
      INTEGER, DIMENSION(NIXS, *), INTENT(in) :: IXS
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
      TYPE (GROUP_)  , DIMENSION(NGRBRIC), INTENT(in) :: IGRBRIC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: N,NN,II,JJ,MY_SIZE
        INTEGER :: NFT,GROUP_ID,ILOC
        INTEGER :: ISU1,NSN,NTY,INACTI,NODE_ID,IBRIC
        LOGICAL, DIMENSION(:), ALLOCATABLE :: TAG
C-----------------------------------------------
        !   X/V/MASS extended 
        !   1:NUMNOD --> classical x/v/mass
        !   NUMNOD+1:NUMNOD+NUMELS --> x/v/mass of phantom nodes (located to the center of 
        !                              the ALE elements)
        !   INT18_GLOBAL_LIST --> true if /INT18 + LAW151 for the NIN interface
        MULTI_FVM%INT18_GLOBAL_LIST(1:NINTER) = .FALSE.
        MY_ALLOCATE(TAG,NUMELS)
        TAG(1:NUMELS)=.FALSE.
        IF( MULTI_FVM%IS_INT18_LAW151 ) THEN
            DO N=1,MULTI_FVM%NUMBER_INT18
                NN = MULTI_FVM%INT18_LIST(N)
                MULTI_FVM%INT18_GLOBAL_LIST(NN) = .TRUE.
            ENDDO
!            ! 1:NUMNOD --> classical x/v/mass
            X_APPEND(1:3,1:NUMNOD) = X(1:3,1:NUMNOD)
            V_APPEND(1:3,1:NUMNOD) = V(1:3,1:NUMNOD)
            MASS_APPEND(1:NUMNOD) = MS(1:NUMNOD)
            KINET_APPEND(1:NUMNOD) = KINET(1:NUMNOD)
!            ! initialization of phantom nodes
            X_APPEND( 1:3,NUMNOD+1:NUMNOD+NUMELS ) = ZERO
            V_APPEND( 1:3,NUMNOD+1:NUMNOD+NUMELS ) = ZERO
            ! KINET : not used for phantom nodes, always equal to 0
            MASS_APPEND(NUMNOD+1:NUMNOD+NUMELS) = ZERO
            KINET_APPEND(NUMNOD+1:NUMNOD+NUMELS) = 0
            FORCE_INT(1:MULTI_FVM%SIZE_FORCE_INT_1,1:MULTI_FVM%SIZE_FORCE_INT_2) = ZERO

            MY_SIZE = MULTI_FVM%SIZE_FORCE_INT_PON
            FORCE_INT_PON(1:3,1:6,1:MY_SIZE) = ZERO

            DO NN=1,MULTI_FVM%NUMBER_INT18
                N = MULTI_FVM%INT18_LIST(NN)
                ISU1 = IPARI(45,N)
                NSN = IPARI(5,N)    ! number of secondary nodes 
                DO II = 1,NSN
                    IBRIC = IGRBRIC(ISU1)%ENTITY(II) ! id of the phantom element
                    IF(.NOT. TAG(IBRIC)) THEN ! otherwise already done with a previous interface
                      GROUP_ID = IGROUPS(IBRIC)       ! id of the element group
                      NFT = IPARG(3,GROUP_ID)
                      ILOC =  IBRIC - NFT
                      !   mass
                      MASS_APPEND(NUMNOD + IBRIC) = ELBUF_TAB(GROUP_ID)%GBUF%RHO(ILOC) * ELBUF_TAB(GROUP_ID)%GBUF%VOL(ILOC)
                      !   position
                      DO JJ = 2, 9
                        NODE_ID = IXS(JJ, IBRIC) ! id of node of the phantom element
                        X_APPEND(1, NUMNOD + IBRIC) = X_APPEND(1, NUMNOD + IBRIC) + ONE_OVER_8 * X(1, NODE_ID)
                        X_APPEND(2, NUMNOD + IBRIC) = X_APPEND(2, NUMNOD + IBRIC) + ONE_OVER_8 * X(2, NODE_ID) 
                        X_APPEND(3, NUMNOD + IBRIC) = X_APPEND(3, NUMNOD + IBRIC) + ONE_OVER_8 * X(3, NODE_ID)         
                      ENDDO
                      !   velocity
                      V_APPEND(1, NUMNOD + IBRIC) = MULTI_FVM%VEL(1, IBRIC)
                      V_APPEND(2, NUMNOD + IBRIC) = MULTI_FVM%VEL(2, IBRIC)
                      V_APPEND(3, NUMNOD + IBRIC) = MULTI_FVM%VEL(3, IBRIC)
                      TAG(IBRIC)=.TRUE.
                    ENDIF
                ENDDO
            ENDDO
        ENDIF
        IF(ALLOCATED(TAG))DEALLOCATE(TAG)

        RETURN
        END SUBROUTINE INT18_LAW151_INIT
C===============================================================================
