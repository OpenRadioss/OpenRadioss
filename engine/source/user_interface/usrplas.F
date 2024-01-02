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
Chd|====================================================================
Chd|  USRPLAS_DUM                   source/user_interface/usrplas.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE USRPLAS_DUM(IERR)
C---------+---------+---+---+--------------------------------------------
#include      "implicit_f.inc"
       INTEGER IERR
       IERR=0
      END
Chd|====================================================================
Chd|  SET_U_SHLPLAS                 source/user_interface/usrplas.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SET_U_SHLPLAS(USRNEL,SIGY,ETSE)
C---------+---------+---+---+--------------------------------------------
C This routine must be called by SIGEPS29C, SIGEPS30C, SIGEPS31C 
C if such a law is used with QPEH, and the law includes placticity.
C---------+---------+---+---+--------------------------------------------
C
C VAR     | SIZE    |TYP|   | DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I |   | NUMBER OF ELEMENTS
C SIGY    |  NEL    | F |   | YELD VALUE FOR EACH ELEMENT, 
C                                   (FOR THE CURRENT INTEGRATION POINT)
C ETSE    |  NEL    | F |   | VALUE FOR EACH ELEMENT, 
C                                   AND THE CURRENT INTEGRATION POINT OF
C                             ETSE = 1         if elastic increment
C                                  = H/(H+E)   if plastic increment
C          where H : plastic tangent modulus, H=d(Sigma)/d(plastic strain)
C                E : Young modulus
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "usrplas_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER USRNEL
      my_real
     .        SIGY(USRNEL),ETSE(USRNEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
      DO I=1,USRNEL
        U_TAGPLAS(I)=1
        U_YELD(I)=SIGY(I)
        U_ETSE(I)=ETSE(I)
      END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  SET_U_SOLPLAS                 source/user_interface/usrplas.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SET_U_SOLPLAS(USRNEL,SIGY,PLA)
C---------+---------+---+---+--------------------------------------------
C This routine must be called by SIGEPS29, SIGEPS30, SIGEPS31
C if such a law is used with HEPH, and the law includes placticity.
C---------+---------+---+---+--------------------------------------------
C
C VAR     | SIZE    |TYP|   | DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I |   | NUMBER OF ELEMENTS
C SIGY    |  NEL    | F |   | YELD VALUE FOR EACH ELEMENT, 
C                                   (FOR THE CURRENT INTEGRATION POINT)
C PLA     |  NEL    | F |   | VALUE FOR EACH ELEMENT, 
C                                   AND THE CURRENT INTEGRATION POINT OF
C                             plastic strain
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "usrplas_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER USRNEL
      my_real
     .        SIGY(USRNEL),PLA(USRNEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
      DO I=1,USRNEL
        U_TAGPLAS(I)=1
        U_YELD(I)=SIGY(I)
        U_DEFP(I)=PLA(I)
      END DO
C
      RETURN
      END
