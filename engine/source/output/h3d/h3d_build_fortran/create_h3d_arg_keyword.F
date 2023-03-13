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
Chd|  CREATE_H3D_ARG_KEYWORD        source/output/h3d/h3d_build_fortran/create_h3d_arg_keyword.F
Chd|-- called by -----------
Chd|        LECH3D                        source/output/h3d/h3d_build_fortran/lech3d.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE CREATE_H3D_ARG_KEYWORD(KEY2, KEY3 , KEY4  , KEY5 , KEY6 ,
     .                                  KEY7, KEY8 , IS_ALL,IS_LOWER, IS_UPPER ,
     .                                   ARG , S_ARG, IS_ARG, VALUE, 
     .                                   IS_ARG_ALL,IS_ARG_LOWER,IS_ARG_UPPER, IS_DEF,
     .                                   IS_ARG_DEF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IS_ALL, IS_UPPER, IS_LOWER, S_ARG, VALUE, IS_ARG, IS_ARG_ALL, IS_ARG_LOWER, IS_ARG_UPPER,
     .        IS_DEF, IS_ARG_DEF
      CHARACTER(LEN=ncharkey) ::  KEY2, KEY3, KEY4,KEY5,KEY6,KEY7,KEY8
      CHARACTER(LEN=S_ARG) ::  ARG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IS_ARG_KEY(5),I
C-----------------------------------------------
         IS_ARG_KEY(1:5) = 0
         VALUE = 0
         IS_ARG = 0
         IS_ARG_ALL = 0
         IS_ARG_LOWER = 0
         IS_ARG_UPPER = 0
         IS_ARG_DEF = 0

         IF ( KEY4(1:S_ARG) == ARG(1:S_ARG)) IS_ARG_KEY(1) = 1
         IF ( KEY5(1:S_ARG) == ARG(1:S_ARG)) IS_ARG_KEY(2) = 1
         IF ( KEY6(1:S_ARG) == ARG(1:S_ARG)) IS_ARG_KEY(3) = 1
         IF ( KEY7(1:S_ARG) == ARG(1:S_ARG)) IS_ARG_KEY(4) = 1
         IF ( KEY8(1:S_ARG) == ARG(1:S_ARG)) IS_ARG_KEY(5) = 1

         IF ( IS_ARG_KEY(1) + IS_ARG_KEY(2) + IS_ARG_KEY(3) + IS_ARG_KEY(4)+ IS_ARG_KEY(5) > 1)THEN
           CALL ANCMSG(MSGID=276,c1=ARG(1:S_ARG+1),c2='/H3D/'//KEY2//'/'//KEY3//'/'//KEY4//'/'//KEY5//'/'//KEY6,ANMODE=ANINFO)
         ELSEIF ( IS_ARG_KEY(1) + IS_ARG_KEY(2) + IS_ARG_KEY(3) + IS_ARG_KEY(4) + IS_ARG_KEY(5) == 1)THEN
           IS_ARG = 1
           IF ( IS_ALL == 1 .AND.
     .          ((KEY4(S_ARG+1:S_ARG+4) == '=ALL' .AND. IS_ARG_KEY(1) == 1) .OR. 
     .          (KEY5(S_ARG+1:S_ARG+4) == '=ALL' .AND. IS_ARG_KEY(2) == 1) .OR. 
     .          (KEY6(S_ARG+1:S_ARG+4) == '=ALL' .AND. IS_ARG_KEY(3) == 1)  .OR. 
     .          (KEY7(S_ARG+1:S_ARG+4) == '=ALL' .AND. IS_ARG_KEY(4) == 1)  .OR. 
     .          (KEY8(S_ARG+1:S_ARG+4) == '=ALL' .AND. IS_ARG_KEY(5) == 1)) )THEN
             IS_ARG_ALL = 1
           ELSEIF ( IS_LOWER == 1 .AND.
     .          ((KEY4(S_ARG+1:S_ARG+6) == '=LOWER' .AND. IS_ARG_KEY(1) == 1) .OR. 
     .          (KEY5(S_ARG+1:S_ARG+6) == '=LOWER' .AND. IS_ARG_KEY(2) == 1) .OR. 
     .          (KEY6(S_ARG+1:S_ARG+6) == '=LOWER' .AND. IS_ARG_KEY(3) == 1)  .OR. 
     .          (KEY7(S_ARG+1:S_ARG+6) == '=LOWER' .AND. IS_ARG_KEY(4) == 1)  .OR. 
     .          (KEY8(S_ARG+1:S_ARG+4) == '=ALL' .AND. IS_ARG_KEY(5) == 1)) )THEN
             IS_ARG_LOWER = 1
           ELSEIF ( IS_UPPER == 1 .AND.
     .          ((KEY4(S_ARG+1:S_ARG+6) == '=UPPER' .AND. IS_ARG_KEY(1) == 1) .OR. 
     .          (KEY5(S_ARG+1:S_ARG+6) == '=UPPER' .AND. IS_ARG_KEY(2) == 1) .OR. 
     .          (KEY6(S_ARG+1:S_ARG+6) == '=UPPER' .AND. IS_ARG_KEY(3) == 1)  .OR. 
     .          (KEY7(S_ARG+1:S_ARG+6) == '=UPPER' .AND. IS_ARG_KEY(4) == 1)  .OR. 
     .          (KEY8(S_ARG+1:S_ARG+4) == '=ALL' .AND. IS_ARG_KEY(5) == 1)) )THEN
             IS_ARG_UPPER = 1
           ELSEIF ( IS_DEF == 1 .AND.
     .          ((KEY4(S_ARG+1:S_ARG+4) == '=DEF' .AND. IS_ARG_KEY(1) == 1) .OR. 
     .          (KEY5(S_ARG+1:S_ARG+4) == '=DEF' .AND. IS_ARG_KEY(2) == 1) .OR. 
     .          (KEY6(S_ARG+1:S_ARG+4) == '=DEF' .AND. IS_ARG_KEY(3) == 1)  .OR. 
     .          (KEY7(S_ARG+1:S_ARG+4) == '=DEF' .AND. IS_ARG_KEY(4) == 1)  .OR. 
     .          (KEY8(S_ARG+1:S_ARG+4) == '=ALL' .AND. IS_ARG_KEY(5) == 1)) )THEN
             IS_ARG_DEF = 1
           ELSE
             IF (IS_ARG_KEY(1) == 1) READ (KEY4(S_ARG+2:S_ARG+11),'(I10)',ERR=100) VALUE
             IF (IS_ARG_KEY(2) == 1) READ (KEY5(S_ARG+2:S_ARG+11),'(I10)',ERR=100) VALUE
             IF (IS_ARG_KEY(3) == 1) READ (KEY6(S_ARG+2:S_ARG+11),'(I10)',ERR=100) VALUE
             IF (IS_ARG_KEY(4) == 1) READ (KEY7(S_ARG+2:S_ARG+11),'(I10)',ERR=100) VALUE
             IF (IS_ARG_KEY(5) == 1) READ (KEY8(S_ARG+2:S_ARG+11),'(I10)',ERR=100) VALUE
           ENDIF
         ENDIF

      RETURN
100   CALL ANCMSG(MSGID=277,c1=ARG(1:S_ARG)//KEY2//'/'//KEY3//'/'//KEY4//'/'//KEY5//'/'//KEY6,ANMODE=ANINFO)
      END
