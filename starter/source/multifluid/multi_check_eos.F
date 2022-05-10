Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  MULTI_CHECK_EOS               source/multifluid/multi_check_eos.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE MULTI_CHECK_EOS(MULTI_FVM, NUMMAT, NPROPMI, NPROPM, IPM, PM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      INTEGER, INTENT(IN) :: NUMMAT, NPROPM, NPROPMI, IPM(NPROPMI, *)
      my_real, INTENT(IN) :: PM(NPROPM, *)
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, II, J, MATID, IEOS

      DO I = 1, NUMMAT
         IF (IPM(2, I) .EQ. 151) THEN
            DO II = 1, IPM(20, I)
               MATID = IPM(20 + II, I)
               DO J = 1, NUMMAT
                  IF (IPM(1, J) .EQ. MATID) EXIT
               ENDDO
               IF (IPM(4,J).EQ.0 .AND. PM(32, J).LE.ZERO) THEN
                  CALL ANCMSG(MSGID = 1645, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .                        I1=IPM(1, I),
     .                        I2=II,
     .                        I3=IPM(20, I)  )
               ENDIF
            ENDDO
         ENDIF
      ENDDO
      
      END SUBROUTINE MULTI_CHECK_EOS
C-----------------------------------------------
