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
!||    message_mod2           ../engine/share/message_module/message_mod2.F
!||--- called by ------------------------------------------------------
!||    ancmsg                 ../engine/source/output/message/message.F
!||    message_mod            ../engine/share/message_module/message_mod.F
!||--- uses       -----------------------------------------------------
!||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
!||====================================================================
      MODULE MESSAGE_MOD2
C
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100,NCHARLINE
      TYPE TMESSAGE
C message size
        INTEGER SMESSAGE
        CHARACTER(LEN=NCHARLINE100),DIMENSION(:), ALLOCATABLE :: MESSAGE
C dimension : size
      END TYPE TMESSAGE
c
C dimensions : size,title(=1)/desc(=2)
      TYPE(TMESSAGE), DIMENSION(:,:), ALLOCATABLE :: MESSAGES
      INTEGER SMESSAGES

      INTEGER SMESSAGESMAX
      PARAMETER (SMESSAGESMAX=200000)
C This SMESSAGESMAX could be increased without problem
C it is just a security because message structured object
C is sized according maxid message read (to avoid to hanble an index)
C so in case of error during read this bound will head to a clean error
C better than a durty one

      INTEGER ANINFO,ANINFO_BLIND,ANINFO_MSG,ANINFO_BLIND_1,ANINFO_BLIND_2
      INTEGER MSGINFO,MSGWARNING,MSGERROR,ANSTOP,MSG_CUMU,MSG_PRINT

      PARAMETER (MSGINFO   =0)
      PARAMETER (MSGWARNING=1)
      PARAMETER (MSGERROR  =2)
      PARAMETER (ANSTOP=1)
      PARAMETER (ANINFO=2)
      PARAMETER (ANINFO_BLIND  =3)
      PARAMETER (ANINFO_BLIND_1=3)
      PARAMETER (ANINFO_BLIND_2=4)
      PARAMETER (ANINFO_MSG    =4)

      CHARACTER(LEN=NCHARLINE), DIMENSION(:), ALLOCATABLE :: MESSAGESFILE
      INTEGER SMESSAGESFILE   

      INTEGER NCHAROUT
      PARAMETER (NCHAROUT=200)
      END MODULE MESSAGE_MOD2
