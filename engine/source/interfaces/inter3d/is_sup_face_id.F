Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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

      FUNCTION IS_SUP_FACE_ID(A,B)
C-----------------------------------------------
C   I n f o r m a t i o n s            
C-----------------------------------------------
C Comparison of the User ID of two faces.
C This UID is defined by four User ID of the nodes
C-----------------------------------------------
C   Dummy   Arguments
C-----------------------------------------------
        INTEGER :: A(4),B(4)
        INTEGER :: IS_SUP_FACE_ID
        IS_SUP_FACE_ID = 0       
        IF( A(1) > B(1) .OR.
     .  A(1) == B(1) .AND. ( A(2) > B(2) .OR.
     .  A(2) == B(2) .AND. ( A(3) > B(3) .OR.
     .  A(3) == B(3) .AND. A(4) > B(4)))) THEN
          IS_SUP_FACE_ID = 1
        ENDIF
        RETURN
      END FUNCTION
