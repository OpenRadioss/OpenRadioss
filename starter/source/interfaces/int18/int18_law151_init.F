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
Chd|====================================================================
Chd|  INT18_LAW151_INIT             source/interfaces/int18/int18_law151_init.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|====================================================================
        SUBROUTINE INT18_LAW151_INIT(S_APPEND_ARRAY,NINTER,NPARI,
     1                               NUMNOD,NUMELS,NGRBRIC,
     2                               MULTI_FVM,IGRBRIC,IPARI,IXS,
     4                               X       ,V       ,MS         ,KINET       ,
     5                               X_APPEND,V_APPEND,MASS_APPEND,KINET_APPEND)
        
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
!$ENDCOMMENT
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------   
        USE MULTI_FVM_MOD
        USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: S_APPEND_ARRAY !< size of append arrays
        INTEGER, INTENT(in) :: NINTER !< number of interface
        INTEGER, INTENT(in) :: NPARI !< size of IPARI
        INTEGER, INTENT(in) :: NUMNOD !< number of node
        INTEGER, INTENT(in) :: NUMELS !< number of solid
        INTEGER, INTENT(in) :: NGRBRIC !< size of IGRBRIC structure
        INTEGER, DIMENSION(NPARI,NINTER), INTENT(in) :: IPARI
        my_real, DIMENSION(3,NUMNOD), INTENT(in) :: X,V
        my_real, DIMENSION(3,S_APPEND_ARRAY), INTENT(inout) :: X_APPEND,V_APPEND
        my_real, DIMENSION(NUMNOD), INTENT(in) :: MS
        INTEGER, DIMENSION(NUMNOD), INTENT(in) :: KINET
        my_real, DIMENSION(S_APPEND_ARRAY), INTENT(inout) :: MASS_APPEND
        INTEGER, DIMENSION(S_APPEND_ARRAY), INTENT(inout) :: KINET_APPEND
        INTEGER, DIMENSION(NIXS,NUMELS), INTENT(in) :: IXS
        TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
        TYPE (GROUP_)  , DIMENSION(NGRBRIC), INTENT(in) :: IGRBRIC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: N,NN,II,JJ
        INTEGER :: ISU1,NSN,NODE_ID,IBRIC
        LOGICAL, DIMENSION(:), ALLOCATABLE :: TAG
C-----------------------------------------------
        ! X/V/MASS extended 
        ! 1:NUMNOD --> classical x/v/mass
        ! NUMNOD+1:NUMNOD+NUMELS --> x/v/mass of phantom nodes (located to the center of 
        !                              the ALE elements)
        ! INT18_GLOBAL_LIST --> true if /INT18 + LAW151 for the NIN interface
        MULTI_FVM%INT18_GLOBAL_LIST(1:NINTER) = .FALSE.
        ALLOCATE(TAG(NUMELS))
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

            DO NN=1,MULTI_FVM%NUMBER_INT18
                N = MULTI_FVM%INT18_LIST(NN)
                ISU1 = IPARI(45,N)
                NSN = IPARI(5,N)    ! number of secondary nodes 
                DO II = 1,NSN
                    IBRIC = IGRBRIC(ISU1)%ENTITY(II) ! id of the phantom element
                    IF(.NOT. TAG(IBRIC)) THEN ! otherwise already done with a previous interface
                      !   mass
                      MASS_APPEND(NUMNOD + IBRIC) = ZERO
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
