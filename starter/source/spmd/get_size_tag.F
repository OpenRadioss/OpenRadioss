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
Chd|  GET_SIZE_TAG                  source/spmd/get_size_tag.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE GET_SIZE_TAG(FLAG_FVM,FLAG_TAG,I24MAXNSNE,SIZE_TAG,IPARI)
!       ----------------------------------------------------------------
!       Description : GET_SIZE_TAG checks if TAG array is used and computes
!                     the size of TAG
!                     TAG is used for the following interfaces:
!                     7, 20, 22, 23, 24 and 25
!                     size of TAG: NUMNOD + I24MAXNSNE + SIZE_FVM
!                                               |            |
!                    max NSNE for TYP24     ____|            |
!       SIZE_FVM=NUMELS for INACTI=7 and FVM, otherwise 0 ___|
!       ----------------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------

C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        LOGICAL, INTENT(INOUT) :: FLAG_TAG
        LOGICAL, INTENT(IN) :: FLAG_FVM
        INTEGER, INTENT(IN) :: I24MAXNSNE
        INTEGER, INTENT(INOUT) :: SIZE_TAG
        INTEGER, DIMENSION(NPARI,*), INTENT(IN) ::  IPARI       
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: NI,ITYP,INACTI,SIZE_FVM
C-----------------------------------------------
C
        FLAG_TAG=.FALSE.
        SIZE_FVM=0
        SIZE_TAG=0
        DO NI =1, NINTER
                ITYP = IPARI(7,NI)
                INACTI = IPARI(22,NI)
                IF( ITYP==7.OR.ITYP==20.OR.ITYP==22.OR.ITYP==23.OR.
     .              ITYP==24.OR.ITYP==25 )    THEN
                        FLAG_TAG=.TRUE.                
                        SIZE_TAG = NUMNOD+I24MAXNSNE
                        IF (FLAG_FVM .AND. INACTI == 7) SIZE_FVM = NUMELS
                ENDIF
        ENDDO
        IF(FLAG_TAG) SIZE_TAG = NUMNOD+I24MAXNSNE+SIZE_FVM  


        RETURN
        END SUBROUTINE GET_SIZE_TAG

Chd|====================================================================
Chd|  GET_SIZE_INTER24              source/spmd/get_size_tag.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE GET_SIZE_INTER24(I24MAXNSNE,NINTER,NPARI,IPARI,FLAG_24_25)
!       ----------------------------------------------------------------
!       Description : GET_SIZE_INTER24 checks if NODLOCAL24 array is used and computes
!                     the size of NODLOCAL24
!                     NODLOCAL24 is used for the following interfaces:
!                     24 and 25
!                     size of NODLOCAL24: NUMNOD + I24MAXNSNE 
!                                                      |            
!                           max NSNE for TYP24     ____|            
!
!       ----------------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
!       NINTER : integer, number of interface
!       NPARI : integer, size of IPARI
!       I24MAXNSNE : integer, size of TAG_SCRATCH for interface 24
!       FLAG_24_25 : logical, flag for interface 24 or 25
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
        INTEGER, INTENT(IN) :: NINTER,NPARI
        INTEGER, INTENT(INOUT) :: I24MAXNSNE
        LOGICAL, INTENT(INOUT) :: FLAG_24_25
        INTEGER, DIMENSION(NPARI,*), INTENT(IN) :: IPARI
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: NI,ITYP,NSNE
C ----------------------------------------
C Care TYPE24 + E2E
C Edge to Edge Nodes fictive Nodes are numbered over NUMNOD 
C Tag Arrays must be increased accordingly
        I24MAXNSNE = 0
        FLAG_24_25 = .FALSE.
        DO NI= 1, NINTER
                ITYP = IPARI(7,NI)
                IF(ITYP==24)THEN
                        NSNE   = IPARI(55,NI)
                        I24MAXNSNE = MAX(I24MAXNSNE,NSNE)
                ENDIF
                IF( (ITYP==24).OR.(ITYP==25) ) FLAG_24_25 = .TRUE.
        ENDDO

        END SUBROUTINE GET_SIZE_INTER24

Chd|====================================================================
Chd|  GET_SIZE_NUMNOD_LOCAL         source/spmd/get_size_tag.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        PLIST_IFRONT                  source/spmd/node/ddtools.F    
Chd|====================================================================
        SUBROUTINE GET_SIZE_NUMNOD_LOCAL(NUMNOD,NUMNOD_L)
!       ----------------------------------------------------------------
!       Description : GET_SIZE_NUMNOD_LOCAL computes the local number of 
!                     element on each domain
!       ----------------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: NUMNOD
        INTEGER, DIMENSION(NSPMD), INTENT(INOUT) :: NUMNOD_L
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
!       NUMNOD : integer, 
!                total number of element
!       NSPMD  : integer,
!                number of domain
!       NUMNOD_L : integer, dimension=NSPMD
!                  number of element on each domain
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, J, ISPMD, NBR_PROC
        INTEGER, DIMENSION(NSPMD) :: ID_SPMD      
C ----------------------------------------
        NUMNOD_L(1:NSPMD) = 0   
        DO I=1,NUMNOD
                CALL PLIST_IFRONT(ID_SPMD,I,NBR_PROC)
#include "vectorize.inc"
                DO J=1,NBR_PROC
                        ISPMD = ID_SPMD(J)
                        NUMNOD_L(ISPMD) = NUMNOD_L(ISPMD) + 1
                ENDDO
        ENDDO
C
        RETURN
        END SUBROUTINE GET_SIZE_NUMNOD_LOCAL
C
