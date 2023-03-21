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
Chd|  SET_USER_WINDOW_NODES         source/user_interface/user_windows_tools.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        USERWIS_FRONT                 source/user_interface/user_windows_tools.F
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        RESTMOD                       share/modules1/restart_mod.F  
Chd|        USER_INTERFACE_MOD            share/modules1/user_interface_mod.F
Chd|        USER_WINDOWS_MOD              ../common_source/modules/user_windows_mod.F
Chd|====================================================================
      SUBROUTINE SET_USER_WINDOW_NODES(USERNODS,NUMBER_USERNODS)
!$COMMENT
   ! -----------------------------------------------
   !   ROUTINE DESCRIPTION :
   !   ===================
   !   Callback routine for User Windows
   !   Grabs the Nodes from the UserWindows for Storing 
   !   and Sets them to Domain 1
   ! ------------------------------------------------
   !   DUMMY ARGUMENTS DESCRIPTION:
   !   ===================
   !
   !     NAME          DESCRIPTION                         
   !
   !     USERNODS          Array with User nodes
   !     NUMBER_USERNODS   Array size
   !------------------------------------------------------------------
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE USER_WINDOWS_MOD
      USE USER_INTERFACE_MOD
      USE RESTMOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NUMBER_USERNODS
      INTEGER, INTENT(IN),DIMENSION(NUMBER_USERNODS) :: USERNODS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NOD,INTERNAL_NOD,USERWI_ID
      INTEGER,DIMENSION(:),ALLOCATABLE::WORK,USER_NDS,IDX,SORT
      CHARACTER(len=40) ::  MESS
      DATA MESS/'USER WINDOWS '/
C-----------------------------------------------
      INTEGER USR2SYS
      EXTERNAL USR2SYS
      
      USERWI_ID = USER_WINDOWS%USER_WINDOWS_ID
      USER_WINDOWS%N_USERNODS = NUMBER_USERNODS
      
      MY_ALLOCATE(USER_NDS,NUMBER_USERNODS)
      MY_ALLOCATE(USER_WINDOWS%USERNODS,NUMBER_USERNODS)
      
      !print*,'NUMBER_USERNODS=',NUMBER_USERNODS
      !print*,'USERNDS=',USERNODS(1:NUMBER_USERNODS)

      DO I=1,NUMBER_USERNODS
         NOD = USERNODS(I)
         INTERNAL_NOD = USR2SYS(NOD,ITABM1,MESS,USERWI_ID )
         USER_NDS(I) = INTERNAL_NOD
      ENDDO

      ! Sort Nodes
      MY_ALLOCATE(WORK,70000)
      MY_ALLOCATE(IDX,2*NUMBER_USERNODS)
      MY_ALLOCATE(SORT,NUMBER_USERNODS)
      DO I=1,NUMBER_USERNODS
         IDX(I)=I
         SORT(I)=USER_NDS(I)
      ENDDO
      CALL MY_ORDERS(0,WORK,SORT,IDX,NUMBER_USERNODS,1)
      DO I=1,NUMBER_USERNODS
         USER_WINDOWS%USERNODS(I) = SORT(IDX(I))
      ENDDO

      CALL USERWIS_FRONT(USER_WINDOWS%USERNODS,NUMBER_USERNODS)

      DEALLOCATE(WORK)
      DEALLOCATE(IDX)
      DEALLOCATE(SORT)
      DEALLOCATE(USER_NDS)

      END
Chd|====================================================================
Chd|  USERWIS_FRONT                 source/user_interface/user_windows_tools.F
Chd|-- called by -----------
Chd|        SET_USER_WINDOW_NODES         source/user_interface/user_windows_tools.F
Chd|-- calls ---------------
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|====================================================================
      SUBROUTINE USERWIS_FRONT(USERNODS,NUSERNODS)
!$COMMENT
   ! -----------------------------------------------
   !   ROUTINE DESCRIPTION :
   !   ===================
   !   Set User Windows Node to Proc 1
   ! ------------------------------------------------
   !   DUMMY ARGUMENTS DESCRIPTION:
   !   ===================
   !
   !     NAME          DESCRIPTION                         
   !
   !     USERNODS       Array with User nodes
   !     NUSERNODS      Array size
   !------------------------------------------------------------------
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------     
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) ::  NUSERNODS
      INTEGER, DIMENSION(1:NUSERNODS),INTENT(IN) ::  USERNODS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NOD
C======================================================================|
      DO I=1,NUSERNODS
        NOD = USERNODS(I)
        IF (NOD/=0)THEN
            CALL IFRONTPLUS(NOD,1)
        ENDIF
      ENDDO
      
      END
Chd|====================================================================
Chd|  SPMD_USERWI_REST              source/user_interface/user_windows_tools.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        PLIST_IFRONT                  source/spmd/node/ddtools.F    
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        USER_WINDOWS_MOD              ../common_source/modules/user_windows_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_USERWI_REST(USER_WINDOWS,NODLOCAL,NUMNOD,NUMNOD_L,
     *                             P,NSPMD)
!$COMMENT
   ! -----------------------------------------------
   !   ROUTINE DESCRIPTION :
   !   ===================
   !   Split & Write UserWindows on disk
   ! ------------------------------------------------
   !   DUMMY ARGUMENTS DESCRIPTION:
   !   ===================
   !
   !     NAME          DESCRIPTION                         
   !
   !     USER_WINDOWS  USER Windows type
   !     NODLOCAL      NODLOCAL ARRAY - Starter to Engine Node numbering
   !     NUMNOD        Total number of nodes
   !     P             Current SPMD Domain to write
   !     NSPMD         Total number of SPMD domains
   !------------------------------------------------------------------
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE USER_WINDOWS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(USER_WINDOWS_),INTENT(IN)        :: USER_WINDOWS
      INTEGER,DIMENSION(1:NUMNOD),INTENT(IN) :: NODLOCAL
      INTEGER ,INTENT(IN) :: P,NSPMD,NUMNOD,NUMNOD_L
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER,DIMENSION(:),ALLOCATABLE::FR_USERWI_L
      INTEGER,DIMENSION(:),ALLOCATABLE::IAD_USERW,IAD_USERW2,USERNODS_L
      INTEGER,DIMENSION(:),ALLOCATABLE::PLIST
      INTEGER I,II,J
      INTEGER SFR_USERWI_L,SPLIST,NP,K
      my_real, DIMENSION(:),ALLOCATABLE :: DUM_SAV
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------     
      INTEGER  NLOCAL
      EXTERNAL NLOCAL   
C======================================================================|
      CALL WRITE_I_C(USER_WINDOWS%HAS_USER_WINDOW,1)
      IF(USER_WINDOWS%HAS_USER_WINDOW /= 0 ) THEN
      
         IF (P == 1)THEN
         
           !  Send Index & Buffer
           ! ---------------------
           MY_ALLOCATE(IAD_USERW,NSPMD+1)
           MY_ALLOCATE(PLIST,NSPMD)
           IAD_USERW(1:NSPMD+1)=0

           DO II=1,USER_WINDOWS%N_USERNODS
               I = USER_WINDOWS%USERNODS(II)
               CALL PLIST_IFRONT(PLIST,I,SPLIST)
               DO J=1,SPLIST
                  NP = PLIST(J)
                  IF(NP /= 1)THEN
                     IAD_USERW(NP+1)=IAD_USERW(NP+1)+1
                  ENDIF
               ENDDO
           ENDDO
           
           IAD_USERW(1)=1
           DO NP=2,NSPMD+1
              IAD_USERW(NP)=IAD_USERW(NP) + IAD_USERW(NP-1)
           ENDDO
           SFR_USERWI_L = IAD_USERW(NSPMD+1)-1
           MY_ALLOCATE(FR_USERWI_L,SFR_USERWI_L)
           MY_ALLOCATE(IAD_USERW2,NSPMD+1)
           IAD_USERW2(1:NSPMD+1) = IAD_USERW(1:NSPMD+1)

           DO II=1,USER_WINDOWS%N_USERNODS
               I = USER_WINDOWS%USERNODS(II)
               CALL PLIST_IFRONT(PLIST,I,SPLIST)
               DO J=1,SPLIST
                  NP = PLIST(J)
                  K = IAD_USERW2(NP)
                  FR_USERWI_L(K) = NODLOCAL(K)
                  IAD_USERW2(NP) = IAD_USERW2(NP) + 1
               ENDDO
           ENDDO
           
           !  UserNode LIST Transform
           ! -----------------------
           MY_ALLOCATE(USERNODS_L,USER_WINDOWS%N_USERNODS)
           DO I=1,USER_WINDOWS%N_USERNODS
               USERNODS_L(I) = NODLOCAL(USER_WINDOWS%USERNODS(I))
           ENDDO

           !  WRITE IN RESTART FILE
           ! -----------------------
           ! UserWindows Structure
           CALL WRITE_I_C(USER_WINDOWS%NUVAR,1)
           CALL WRITE_I_C(USER_WINDOWS%NUVARI,1)
           CALL WRITE_I_C(USER_WINDOWS%S_IUSER,1)
           CALL WRITE_I_C(USER_WINDOWS%S_USER,1)
           CALL WRITE_I_C(USER_WINDOWS%N_USERNODS,1)
           CALL WRITE_I_C(USER_WINDOWS%IUSER,USER_WINDOWS%S_IUSER)
           CALL WRITE_DB(USER_WINDOWS%USREINT,1)
           CALL WRITE_DB(USER_WINDOWS%USER,USER_WINDOWS%S_USER)
           CALL WRITE_I_C(USERNODS_L,USER_WINDOWS%N_USERNODS)
           !SPMD Structures
           CALL WRITE_I_C(SFR_USERWI_L,1)
           CALL WRITE_I_C(IAD_USERW,NSPMD+1)
           CALL WRITE_I_C(FR_USERWI_L,SFR_USERWI_L)

           IF(ALLOCATED(USERNODS_L))   DEALLOCATE(USERNODS_L)
           IF(ALLOCATED(IAD_USERW))    DEALLOCATE(IAD_USERW)
           IF(ALLOCATED(FR_USERWI_L))  DEALLOCATE(FR_USERWI_L)
           
           ! WRITE A_SAV & AR_SAV only on ispmd==0
           ALLOCATE(DUM_SAV(3*NUMNOD_L))
           DUM_SAV(1:3*NUMNOD_L)=ZERO
           CALL WRITE_DB(DUM_SAV,3*NUMNOD_L)
           CALL WRITE_DB(DUM_SAV,3*NUMNOD_L)
           DEALLOCATE(DUM_SAV)

         ELSE                               ! Receive buffer from P=1
           MY_ALLOCATE(FR_USERWI_L,USER_WINDOWS%N_USERNODS)
           FR_USERWI_L(1:USER_WINDOWS%N_USERNODS)=0
           SFR_USERWI_L = 0
           DO II=1,USER_WINDOWS%N_USERNODS
            
              I = USER_WINDOWS%USERNODS(II)
              IF(NLOCAL(I,P)==1)THEN
                 SFR_USERWI_L = SFR_USERWI_L +1
                 FR_USERWI_L(SFR_USERWI_L) = NODLOCAL(I)
              ENDIF

           ENDDO

           !  WRITE IN RESTART FILE
           ! -----------------------

           CALL WRITE_I_C(SFR_USERWI_L,1)
           CALL WRITE_I_C(FR_USERWI_L,SFR_USERWI_L)
           IF(ALLOCATED(FR_USERWI_L))  DEALLOCATE(FR_USERWI_L)
           
         ENDIF
      ENDIF


      END

