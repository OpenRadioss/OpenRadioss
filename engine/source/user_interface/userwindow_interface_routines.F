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
Chd|  USERWI_READ                   source/user_interface/userwindow_interface_routines.F
Chd|-- called by -----------
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        USER_WINDOWS_MOD              ../common_source/modules/user_windows_mod.F
Chd|====================================================================
      SUBROUTINE USERWI_READ(USER_WINDOWS,ISPMD,NSPMD,NUMNOD)
!$COMMENT
   ! -----------------------------------------------
   !   ROUTINE DESCRIPTION :
   !   ===================
   !   Read Userlib Restart
   ! ------------------------------------------------
   !   DUMMY ARGUMENTS DESCRIPTION:
   !   ===================
   !
   !     NAME          DESCRIPTION                         
   !
   !     USER_WINDOWS  USER Windows type
   !     ISPMD         Current SPMD domains
   !     NSPMD         #SPMD domains
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
      TYPE(USER_WINDOWS_),INTENT(INOUT)        :: USER_WINDOWS
      INTEGER ,INTENT(IN) :: ISPMD
      INTEGER ,INTENT(IN) :: NSPMD
      INTEGER ,INTENT(IN) :: NUMNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      CALL READ_I_C(USER_WINDOWS%HAS_USER_WINDOW,1)

      IF(USER_WINDOWS%HAS_USER_WINDOW /= 0 ) THEN

         IF (ISPMD == 0)THEN
             CALL READ_I_C(USER_WINDOWS%NUVAR,1)
             CALL READ_I_C(USER_WINDOWS%NUVARI,1)
             CALL READ_I_C(USER_WINDOWS%S_IUSER,1)
             CALL READ_I_C(USER_WINDOWS%S_USER,1)
             CALL READ_I_C(USER_WINDOWS%N_USERNODS,1)

             MY_ALLOCATE(USER_WINDOWS%IUSER,USER_WINDOWS%S_IUSER)
             MY_ALLOCATE(USER_WINDOWS%USER,USER_WINDOWS%S_USER)
             MY_ALLOCATE(USER_WINDOWS%USERNODS,USER_WINDOWS%N_USERNODS)

             CALL READ_I_C(USER_WINDOWS%IUSER,USER_WINDOWS%S_IUSER)
             CALL READ_DB(USER_WINDOWS%USREINT,1)
             CALL READ_DB(USER_WINDOWS%USER,USER_WINDOWS%S_USER)
             CALL READ_I_C(USER_WINDOWS%USERNODS,USER_WINDOWS%N_USERNODS)

             ! SPMD Structures
             CALL READ_I_C(USER_WINDOWS%S_FR_USERW,1)
             MY_ALLOCATE(USER_WINDOWS%IAD_USERW,NSPMD+1)
             MY_ALLOCATE(USER_WINDOWS%FR_USERW,USER_WINDOWS%S_FR_USERW)

             CALL READ_I_C(USER_WINDOWS%IAD_USERW,NSPMD+1)
             CALL READ_I_C(USER_WINDOWS%FR_USERW,USER_WINDOWS%S_FR_USERW)

             ! SAV & AR_SAV on ispmd==0
              MY_ALLOCATE2D(USER_WINDOWS%A_SAV,3,NUMNOD)
              MY_ALLOCATE2D(USER_WINDOWS%AR_SAV,3,NUMNOD)
              CALL READ_DB(USER_WINDOWS%A_SAV,3*NUMNOD)
              CALL READ_DB(USER_WINDOWS%AR_SAV,3*NUMNOD)

             ! ALLOCATE WA to 3x NUMNOD
             USER_WINDOWS%S_WA = 3*NUMNOD
             MY_ALLOCATE(USER_WINDOWS%WA,3*NUMNOD)
         ELSE
             CALL READ_I_C(USER_WINDOWS%S_FR_USERW,1)

             MY_ALLOCATE(USER_WINDOWS%FR_USERW,USER_WINDOWS%S_FR_USERW)

             CALL READ_I_C(USER_WINDOWS%FR_USERW,USER_WINDOWS%S_FR_USERW)

         ENDIF
      ENDIF 
      END
Chd|====================================================================
Chd|  USERWI_WRITE                  source/user_interface/userwindow_interface_routines.F
Chd|-- called by -----------
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        USER_WINDOWS_MOD              ../common_source/modules/user_windows_mod.F
Chd|====================================================================
      SUBROUTINE USERWI_WRITE(USER_WINDOWS,ISPMD,NSPMD,NUMNOD)
!$COMMENT
   ! -----------------------------------------------
   !   ROUTINE DESCRIPTION :
   !   ===================
   !   WRITE Userlib Restart
   ! ------------------------------------------------
   !   DUMMY ARGUMENTS DESCRIPTION:
   !   ===================
   !
   !     NAME          DESCRIPTION                         
   !
   !     USER_WINDOWS  USER Windows type
   !     ISPMD         Current SPMD domains
   !     NSPMD         #SPMD domains
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
      TYPE(USER_WINDOWS_),INTENT(INOUT)        :: USER_WINDOWS
      INTEGER ,INTENT(IN) :: ISPMD
      INTEGER ,INTENT(IN) :: NSPMD
      INTEGER ,INTENT(IN) :: NUMNOD
      
      CALL WRITE_I_C(USER_WINDOWS%HAS_USER_WINDOW,1)
      IF(USER_WINDOWS%HAS_USER_WINDOW /= 0 ) THEN

         IF (ISPMD == 0)THEN

             CALL WRITE_I_C(USER_WINDOWS%NUVAR,1)
             CALL WRITE_I_C(USER_WINDOWS%NUVARI,1)
             CALL WRITE_I_C(USER_WINDOWS%S_IUSER,1)
             CALL WRITE_I_C(USER_WINDOWS%S_USER,1)
             CALL WRITE_I_C(USER_WINDOWS%N_USERNODS,1)

             CALL WRITE_I_C(USER_WINDOWS%IUSER,USER_WINDOWS%S_IUSER)
             CALL WRITE_DB(USER_WINDOWS%USREINT,1)
             CALL WRITE_DB(USER_WINDOWS%USER,USER_WINDOWS%S_USER)
             CALL WRITE_I_C(USER_WINDOWS%USERNODS,USER_WINDOWS%N_USERNODS)

             ! SPMD Structures
             CALL WRITE_I_C(USER_WINDOWS%S_FR_USERW,1)
             CALL WRITE_I_C(USER_WINDOWS%IAD_USERW,NSPMD+1)
             CALL WRITE_I_C(USER_WINDOWS%FR_USERW,USER_WINDOWS%S_FR_USERW)

             ! A_SAV & AR_SAV
             CALL WRITE_DB(USER_WINDOWS%A_SAV,3*NUMNOD)
             CALL WRITE_DB(USER_WINDOWS%AR_SAV,3*NUMNOD)

         ELSE

             CALL WRITE_I_C(USER_WINDOWS%S_FR_USERW,1)
             CALL WRITE_I_C(USER_WINDOWS%FR_USERW,USER_WINDOWS%S_FR_USERW)

         ENDIF
      ENDIF 
      
      END
Chd|====================================================================
Chd|  GET_USER_WINDOW_NODES         source/user_interface/userwindow_interface_routines.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        RESTMOD                       share/modules/restart_mod.F   
Chd|        USER_INTERFACE_MOD            share/modules/user_interface_mod.F
Chd|====================================================================
      SUBROUTINE GET_USER_WINDOW_NODES(INTERNAL_ID,USER_ID)
!$COMMENT
   ! -----------------------------------------------
   !   ROUTINE DESCRIPTION :
   !   ===================
   !   CALLBACK Routine Give to user Windows the list of nodes
   !   As engine nodes & user nodes
   ! ------------------------------------------------
   !   DUMMY ARGUMENTS DESCRIPTION:
   !   ===================
   !
   !     NAME          DESCRIPTION                         
   !
   !     INTERNAL_ID : internal list
   !     USER_ID     : user id list
   !------------------------------------------------------------------
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE USER_INTERFACE_MOD
      USE RESTMOD
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,intent(INOUT),DIMENSION(USER_WINDOWS%N_USERNODS) :: INTERNAL_ID
      INTEGER,intent(INOUT),DIMENSION(USER_WINDOWS%N_USERNODS) :: USER_ID
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ND
C-----------------------------------------------
      IF(USER_WINDOWS%HAS_USER_WINDOW /= 0)THEN
        DO I=1,USER_WINDOWS%N_USERNODS
          ND = USER_WINDOWS%USERNODS(I)
          INTERNAL_ID(I)=ND
          USER_ID(I)=ITAB(ND)
        ENDDO      
      ENDIF
      END


Chd|====================================================================
Chd|  USERWINDOW_GET_A              source/user_interface/userwindow_interface_routines.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        USER_INTERFACE_MOD            share/modules/user_interface_mod.F
Chd|====================================================================
      SUBROUTINE USERWINDOW_GET_A(A_BUF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE USER_INTERFACE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      DOUBLE PRECISION A_BUF(3,NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ND
C-----------------------------------------------
      IF(USER_WINDOWS%HAS_USER_WINDOW /= 0)THEN
        DO I=1,USER_WINDOWS%N_USERNODS
            ND = USER_WINDOWS%USERNODS(I)
            A_BUF(1,I)=USER_WINDOWS%A_SAV(1,ND)
            A_BUF(2,I)=USER_WINDOWS%A_SAV(2,ND)
            A_BUF(3,I)=USER_WINDOWS%A_SAV(3,ND)
        ENDDO      
      ENDIF
      END
Chd|====================================================================
Chd|  USERWINDOW_GET_AR             source/user_interface/userwindow_interface_routines.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        USER_INTERFACE_MOD            share/modules/user_interface_mod.F
Chd|====================================================================
      SUBROUTINE USERWINDOW_GET_AR(AR_BUF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE USER_INTERFACE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      DOUBLE PRECISION AR_BUF(3,NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ND
C-----------------------------------------------
      IF(USER_WINDOWS%HAS_USER_WINDOW /= 0)THEN
        DO I=1,USER_WINDOWS%N_USERNODS
            ND = USER_WINDOWS%USERNODS(I)
            AR_BUF(1,I)=USER_WINDOWS%AR_SAV(1,ND)
            AR_BUF(2,I)=USER_WINDOWS%AR_SAV(2,ND)
            AR_BUF(3,I)=USER_WINDOWS%AR_SAV(3,ND)
        ENDDO      
      ENDIF
      END
