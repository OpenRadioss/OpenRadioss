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
!||    error_alloc   ../engine/source/system/error_alloc.F
!||--- calls      -----------------------------------------------------
!||    ancmsg        ../engine/source/output/message/message.F
!||    arret         ../engine/source/system/arret.F
!||--- uses       -----------------------------------------------------
!||    message_mod   ../engine/share/message_module/message_mod.F
!||====================================================================
        SUBROUTINE ERROR_ALLOC(ARRAY,MY_IERR0)

      USE MESSAGE_MOD
       
        IMPLICIT NONE
        INTEGER :: MY_IERR0
        CHARACTER(*) :: ARRAY 
        IF(MY_IERR0/=0) THEN
                CALL ANCMSG(MSGID=19,ANMODE=ANINFO,C1=ARRAY)
                CALL ARRET(2)
        ENDIF
        
        END SUBROUTINE ERROR_ALLOC
