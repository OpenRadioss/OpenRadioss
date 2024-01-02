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
Chd|  INT18_ALLOC                   source/interfaces/int18/int18_alloc.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ARRAY_MOD                     ../common_source/modules/array_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
        SUBROUTINE INT18_ALLOC(NUMBER_INTER18,INTER18_LIST,MULTI_FVM,IPARI,XCELL_REMOTE)
!$COMMENT
!       INT18_ALLOC description
!       allocation of array for interface 18 & interface 18 combined 
!       with law151
!       
!       INT18_ALLOC organization :
!       - check if /INT18 is used
!       - check if /INT18 + /LAW151 is used
!       - allocate the arrays
!$ENDCOMMENT
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------           
        USE MULTI_FVM_MOD
        USE GROUPDEF_MOD
        USE TRI7BOX
        USE ARRAY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc" 
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(inout) :: NUMBER_INTER18 !< number of interface 18
        INTEGER, DIMENSION(NINTER), INTENT(inout) :: INTER18_LIST !< list of interface 18
        INTEGER, DIMENSION(NPARI,NINTER), INTENT(in) :: IPARI
        TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
        TYPE(array_type), DIMENSION(NINTER), INTENT(inout) :: XCELL_REMOTE !< remote data structure for interface 18 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: N,NN,II,JJ,MY_SIZE,MY_SIZE_2,MY_SIZE_3
        INTEGER :: ISU1,NBRIC,NSN,NTY,INACTI,NODE_ID,IBRIC,NODFI
C-----------------------------------------------
        !   check if int18 + law151 is used in the model
        !   and create a list of int18
        MULTI_FVM%IS_INT18_LAW151 = .FALSE.
        MY_SIZE = 0
        MY_SIZE_2 = 0
        NUMBER_INTER18 = 0
        
        DO N=1,NINTER
            NTY   =IPARI(7,N)
            INACTI = IPARI(22,N)           
            XCELL_REMOTE(N)%SIZE_MY_REAL_ARRAY_1D = 0
            !   int18 = int7 + inacti=7 (7+7=18) 
            IF( (NTY==7).AND.(INACTI ==7)) THEN
              IF(MULTI_FVM%IS_USED) THEN
                MULTI_FVM%IS_INT18_LAW151 = .TRUE.
                MY_SIZE = NUMNOD + NUMELS
                MY_SIZE_2 = NUMELS
            ENDIF      
            NUMBER_INTER18 = NUMBER_INTER18 + 1
            INTER18_LIST(NUMBER_INTER18) = N  !   list of interface int18
          ENDIF
        ENDDO
        !   number & list of interface 18 for MULTI_FVM solve
        MULTI_FVM%NUMBER_INT18 = 0
        IF(MULTI_FVM%IS_INT18_LAW151) MULTI_FVM%NUMBER_INT18 = NUMBER_INTER18
        ALLOCATE( MULTI_FVM%INT18_LIST(MULTI_FVM%NUMBER_INT18) )
        !   allocation of X/V/MASS extended to NUMNOD+NUMELS
        !   1:NUMNOD --> classical x/v/mass
        !   NUMNOD+1:NUMNOD+NUMELS --> x/v/mass of phantom nodes (located to the center of 
        !                              the ALE elements)
        ALLOCATE( MULTI_FVM%X_APPEND(3*MY_SIZE) )
        ALLOCATE( MULTI_FVM%V_APPEND(3*MY_SIZE) )
        ALLOCATE( MULTI_FVM%MASS_APPEND(MY_SIZE) )
        ALLOCATE( MULTI_FVM%KINET_APPEND(MY_SIZE) )
        !   allocation of force array : size = NUMELS
        MY_SIZE_2 = MY_SIZE_2 * NTHREAD
        
        IF(IPARIT/=0) THEN
            MY_SIZE_3 = 0 
        ELSE
            MY_SIZE_3 = MY_SIZE_2
        ENDIF

        MULTI_FVM%SIZE_FORCE_INT_1 = 3
        MULTI_FVM%SIZE_FORCE_INT_2 = MY_SIZE_3
        ALLOCATE( MULTI_FVM%FORCE_INT(3,MY_SIZE_3) )
        !   allocation of INT18_GLOBAL_LIST : marker for the interface /INT18+LAW151
        ALLOCATE( MULTI_FVM%INT18_GLOBAL_LIST(NINTER) )

        ! --------------------------
        ! allocation of remote array for parith/on
        IF(MULTI_FVM%IS_USED) THEN
            ALLOCATE( MULTI_FVM%R_AFI(NINTER) )  
            DO II=1,MULTI_FVM%NUMBER_INT18
                N = INTER18_LIST(II) ! list of interface 18 + law151
                IF( ALLOCATED( MULTI_FVM%R_AFI(N)%R_FORCE_INT ) ) DEALLOCATE( MULTI_FVM%R_AFI(N)%R_FORCE_INT )
                NODFI = size(NSVFI(N)%P)
                MULTI_FVM%R_AFI(N)%NODFI = NODFI
                ALLOCATE( MULTI_FVM%R_AFI(N)%R_FORCE_INT(3,6,NODFI*NTHREAD) )
                MULTI_FVM%R_AFI(N)%R_FORCE_INT(1:3,1:6,1:NODFI*NTHREAD) = 0d+00 
            ENDDO
        ELSE
            ALLOCATE( MULTI_FVM%R_AFI(0) )
        ENDIF 
        ! allocation of local array for parith/on
        IF(IPARIT/=0) THEN
            ALLOCATE( MULTI_FVM%FORCE_INT_PON(3,6,MY_SIZE_2) )
            MULTI_FVM%SIZE_FORCE_INT_PON = MY_SIZE_2
        ELSE
            ALLOCATE( MULTI_FVM%FORCE_INT_PON(0,0,0) )
            MULTI_FVM%SIZE_FORCE_INT_PON = 0
        ENDIF
        ! --------------------------

        IF( MULTI_FVM%IS_INT18_LAW151 ) THEN
            MULTI_FVM%INT18_LIST(1:MULTI_FVM%NUMBER_INT18) = INTER18_LIST(1:MULTI_FVM%NUMBER_INT18)
        ENDIF

        RETURN
        END SUBROUTINE INT18_ALLOC
C===============================================================================
