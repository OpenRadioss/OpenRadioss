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
Chd|  FAIL_TAB2SYS                source/materials/tools/matfun_usr2sys.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FAIL_TAB2SYS(FAIL ,TITLE  ,MAT_ID ,NTABL  ,TABLE )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
      USE FAIL_PARAM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: MAT_ID
      INTEGER ,INTENT(IN) :: NTABL   ! total number of system function tables   
      CHARACTER(LEN=nchartitle) ,INTENT(IN) :: TITLE
      TYPE(TTABLE), DIMENSION(NTABL) ,INTENT(INOUT) ::  TABLE
      TYPE (FAIL_PARAM_) ,INTENT (INOUT) :: FAIL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,OK
C-----------------------------------------------
c     Replace table IDs in failure models by system numbers
C=======================================================================
      DO I=1,FAIL%NTABLE
        OK = 0
        IF (FAIL%TABLE(I) > 0) THEN
          DO J=1,NTABL     ! total number of function tables                   
           IF (FAIL%TABLE(I) == TABLE(J)%NOTABLE) THEN           
               FAIL%TABLE(I) = J
               OK = 1
               EXIT                             
            ENDIF                              
          ENDDO                                
          IF (OK == 0) THEN
            CALL ANCMSG(MSGID = 779, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,  
     .                  I1 = MAT_ID,              
     .                  C1 = TITLE ,              
     .                  I2 = FAIL%TABLE(I))
          ENDIF                                 
        ENDIF                                 
      ENDDO
c-----------
      RETURN
      END
