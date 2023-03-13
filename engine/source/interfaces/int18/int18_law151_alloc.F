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
Chd|  INT18_LAW151_ALLOC            source/interfaces/int18/int18_law151_alloc.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
        SUBROUTINE INT18_LAW151_ALLOC(MULTI_FVM,IPARI)
!$COMMENT
!       INT18_LAW151_ALLOC description
!       allocation of array for interface 18 combined 
!       with law151
!       
!       INT18_LAW151_ALLOC organization :
!       - check if /INT18 + /LAW151 is used
!       - allocate the arrays
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
#include      "comlock.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc" 
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, DIMENSION(NPARI,*), INTENT(in) :: IPARI
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: N,NN,II,JJ,MY_SIZE,MY_SIZE_2,MY_SIZE_3
        INTEGER :: ISU1,NBRIC,NSN,NTY,INACTI,NODE_ID,IBRIC
        INTEGER, DIMENSION(NINTER) :: INT18_LIST
C-----------------------------------------------
        !   check if int18 + law151 is used in the model
        !   and create a list of int18 + law151
        MULTI_FVM%IS_INT18_LAW151 = .FALSE.
        MY_SIZE = 0
        MY_SIZE_2 = 0
        II = 0
        IF(MULTI_FVM%IS_USED) THEN
            DO N=1,NINTER
                NTY   =IPARI(7,N)
                INACTI = IPARI(22,N)           
                !   int18 = int7 + inacti=7 (7+7=18) 
                IF( (NTY==7).AND.(INACTI ==7)) THEN
                    MULTI_FVM%IS_INT18_LAW151 = .TRUE.
                    MY_SIZE = NUMNOD + NUMELS
                    MY_SIZE_2 = NUMELS
                    II = II + 1
                    INT18_LIST(II) = N  !   list of interface int18
                ENDIF
            ENDDO
        ENDIF       
        !   number & list of interface 18 
        MULTI_FVM%NUMBER_INT18 = II
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
            MULTI_FVM%INT18_LIST(1:MULTI_FVM%NUMBER_INT18) = INT18_LIST(1:MULTI_FVM%NUMBER_INT18)
        ENDIF

        RETURN
        END SUBROUTINE INT18_LAW151_ALLOC
C===============================================================================
