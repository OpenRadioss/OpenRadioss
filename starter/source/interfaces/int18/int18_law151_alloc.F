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
Chd|====================================================================
        SUBROUTINE INT18_LAW151_ALLOC(NPARI,NINTER,NUMNOD,NUMELS,MULTI_FVM,IPARI)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: NPARI !< first dim of ipari array
        INTEGER, INTENT(in) :: NINTER !< number of interface
        INTEGER, INTENT(in) :: NUMNOD !< number of node
        INTEGER, INTENT(in) :: NUMELS !< number of solid
        INTEGER, DIMENSION(NPARI,NINTER), INTENT(in) :: IPARI !< data of intarface
        TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM !< structure for multifluid solver
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: N,II,MY_SIZE
        INTEGER :: NTY,INACTI
        INTEGER, DIMENSION(NINTER) :: INT18_LIST
C-----------------------------------------------
        !   check if int18 + law151 is used in the model
        !   and create a list of int18 + law151
        MULTI_FVM%IS_INT18_LAW151 = .FALSE.
        MY_SIZE = 0
        II = 0
        ! -------------------------------
        IF(MULTI_FVM%IS_USED) THEN
            DO N=1,NINTER
                NTY   =IPARI(7,N)
                INACTI = IPARI(22,N)           
                !   int18 = int7 + inacti=7 (7+7=18) 
                IF( (NTY==7).AND.(INACTI ==7)) THEN
                    MULTI_FVM%IS_INT18_LAW151 = .TRUE.
                    MY_SIZE = NUMNOD + NUMELS
                    II = II + 1
                    INT18_LIST(II) = N  !   list of interface int18
                ENDIF
            ENDDO
        ENDIF     
        ! -------------------------------  
        MULTI_FVM%S_APPEND_ARRAY = MY_SIZE
        ! number & list of interface 18 
        MULTI_FVM%NUMBER_INT18 = II
        ALLOCATE( MULTI_FVM%INT18_LIST(MULTI_FVM%NUMBER_INT18) )
        ! allocation of X/V/MASS extended to NUMNOD+NUMELS
        ! 1:NUMNOD --> classical x/v/mass
        ! NUMNOD+1:NUMNOD+NUMELS --> x/v/mass of phantom nodes (located to the center of 
        !                            the ALE elements)
        ALLOCATE( MULTI_FVM%X_APPEND(3*MY_SIZE) )
        ALLOCATE( MULTI_FVM%V_APPEND(3*MY_SIZE) )
        ALLOCATE( MULTI_FVM%MASS_APPEND(MY_SIZE) )
        ALLOCATE( MULTI_FVM%KINET_APPEND(MY_SIZE) )
        
        ! allocation of INT18_GLOBAL_LIST : marker for the interface /INT18+LAW151
        ALLOCATE( MULTI_FVM%INT18_GLOBAL_LIST(NINTER) )

        IF( MULTI_FVM%IS_INT18_LAW151 ) THEN
            MULTI_FVM%INT18_LIST(1:MULTI_FVM%NUMBER_INT18) = INT18_LIST(1:MULTI_FVM%NUMBER_INT18)
        ENDIF

        RETURN
        END SUBROUTINE INT18_LAW151_ALLOC
C===============================================================================
