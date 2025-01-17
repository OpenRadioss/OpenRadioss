!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      !||====================================================================
      !||    eosfun_usr2sys_mod   ../starter/source/materials/tools/eosfun_usr2sys.F90
      !||--- called by ------------------------------------------------------
      !||    updmat               ../starter/source/materials/updmat.F
      !||====================================================================
      module EOSFUN_USR2SYS_MOD
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief convert user function identifier into internal function identifiers
!! \details
      !||====================================================================
      !||    eosfun_usr2sys         ../starter/source/materials/tools/eosfun_usr2sys.F90
      !||--- called by ------------------------------------------------------
      !||    updmat                 ../starter/source/materials/updmat.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||    table_mod              ../starter/share/modules1/table_mod.F
      !||====================================================================
      SUBROUTINE EOSFUN_USR2SYS(TITR, EOS_ID, NFUNC  ,IFUNC  ,FUNC_ID, NFUNCT  )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit None
! ----------------------------------------------------------------------------------------------------------------------
      IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      INTEGER,INTENT(IN) :: NFUNCT !< number of /FUNCT
      INTEGER,INTENT(IN) :: EOS_ID !< EoS id
      INTEGER,INTENT(IN) :: NFUNC !< number of function to convert (uder id -> internal id)
      INTEGER, DIMENSION(NFUNC),INTENT(INOUT) :: IFUNC !< array of function identifiers to convert
      INTEGER,INTENT(IN), DIMENSION(NFUNCT) :: FUNC_ID !< data structure for /FUNCT
      CHARACTER(LEN=NCHARTITLE),INTENT(IN) :: TITR !< EoS title
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Variables
! ----------------------------------------------------------------------------------------------------------------------
      INTEGER I,J,ID,OK
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      DO I=1,NFUNC                           
        ID = IFUNC(I)
        OK = 0
        IF (ID > 0) THEN
          DO J=1,NFUNCT     ! total number of functions                   
           IF (ID == FUNC_ID(J)) THEN           
              IFUNC(I) = J
              OK = 1
              EXIT                            
            ENDIF                              
          ENDDO                                
          IF (OK == 0) THEN
            !eos error with function identifer
            CALL ANCMSG(MSGID=135,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_1,I1=EOS_ID,C1=TITR,I2=ID)
          ENDIF                                 
        ENDIF                                 
      ENDDO  ! I=1,NFUNC 

      RETURN
      END SUBROUTINE EOSFUN_USR2SYS
! ----------------------------------------------------------------------------------------------------------------------
      
      END MODULE EOSFUN_USR2SYS_MOD
