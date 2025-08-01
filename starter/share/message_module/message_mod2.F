Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    message_mod2           ../starter/share/message_module/message_mod2.F
!||--- called by ------------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    message_mod            ../starter/share/message_module/message_mod.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      MODULE MESSAGE_MOD2
      USE NAMES_AND_TITLES_MOD , ONLY : ncharline
      TYPE TMESSAGE
C message size
        INTEGER SMESSAGE
        CHARACTER(len=ncharline),DIMENSION(:), ALLOCATABLE :: MESSAGE
C dimension : size
      END TYPE TMESSAGE
c
C dimensions : size,title(=1)/desc(=2)
      TYPE(TMESSAGE), DIMENSION(:,:), ALLOCATABLE :: MESSAGES
      INTEGER SMESSAGES
      INTEGER SMSGTYPE
      INTEGER SMESSAGESMAX
      PARAMETER (SMESSAGESMAX=210000)
C This SMESSAGESMAX could be increased without problem
C it is just a security because message structured object
C is sized according maxid message read (to avoid to hanble an index)
C so in case of error during read this bound will head to a clean error
C better than a durty one

      INTEGER ANINFO,ANINFO_BLIND,ANINFO_MSG,ANINFO_BLIND_1,ANINFO_BLIND_2
      INTEGER MSGINFO,MSGWARNING,MSGERROR,ANSTOP,MSG_CUMU,MSG_PRINT
      INTEGER IMSG, IERR, IWARN, KWARN

C Dyna Deck - catch if input deck comes from Dyna 
C     -> Skip the research if Native Radioss Deck
      INTEGER DYNA_MESSAGE
C
      PARAMETER (MSGINFO=0)
      PARAMETER (MSGWARNING=1)
      PARAMETER (MSGERROR=2)
      PARAMETER (ANSTOP=1)
      PARAMETER (ANINFO=2)
      PARAMETER (ANINFO_BLIND  =3)
      PARAMETER (ANINFO_BLIND_1=3)
      PARAMETER (ANINFO_BLIND_2=4)
      PARAMETER (ANINFO_MSG=4)
      PARAMETER (SMSGTYPE=4)
      PARAMETER (MSG_CUMU=0)
      PARAMETER (MSG_PRINT=1)

      CHARACTER(len=ncharline), DIMENSION(:), ALLOCATABLE :: MESSAGESFILE
      CHARACTER(len=ncharline) :: ERR_CATEGORY
      INTEGER SMESSAGESFILE

      INTEGER NCHAROUT
      PARAMETER (NCHAROUT=200)
      END MODULE MESSAGE_MOD2
