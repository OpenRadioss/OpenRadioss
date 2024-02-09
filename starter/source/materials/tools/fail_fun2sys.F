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
Chd|  FAIL_FUN2SYS                  source/materials/tools/fail_fun2sys.F
Chd|-- called by -----------
Chd|        UPDFAIL                       source/materials/updfail.F    
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FAIL_PARAM_MOD                ../common_source/modules/mat_elem/fail_param_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FAIL_FUN2SYS(FAIL ,TITLE  ,MAT_ID ,NFUNCT  ,FUNC_ID  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAIL_PARAM_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: MAT_ID
      INTEGER ,INTENT(IN) :: NFUNCT   ! total number of system functions   
      INTEGER  ,DIMENSION(NFUNCT) ,INTENT(IN) :: FUNC_ID
      CHARACTER(LEN=nchartitle) ,INTENT(IN) :: TITLE
      TYPE (FAIL_PARAM_) ,INTENT (INOUT) :: FAIL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,OK
C-----------------------------------------------
c     Replace function IDs in failure models by system numbers
C=======================================================================
      DO I=1,FAIL%NFUNC
        OK = 0
        IF (FAIL%IFUNC(I) > 0) THEN
          DO J=1,NFUNCT     ! total number of functions                   
           IF (FAIL%IFUNC(I) == FUNC_ID(J)) THEN           
              FAIL%IFUNC(I) = J
              OK = 1
              EXIT                            
            ENDIF                              
          ENDDO                                
          IF (OK == 0) THEN
            CALL ANCMSG(MSGID = 126, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,  
     .                  I1 = MAT_ID,              
     .                  C1 = TITLE  ,              
     .                  I2 = FAIL%IFUNC(I))              
          ENDIF                                 
        ENDIF                                 
      ENDDO
c-----------
      RETURN
      END
