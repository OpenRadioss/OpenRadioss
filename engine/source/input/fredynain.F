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
Chd|  FREDYNAIN                     source/input/fredynain.F      
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FREDEC_8KEY_I                 source/input/fredec_8key_i.F  
Chd|        WRIUSC2                       source/input/wriusc2.F        
Chd|        NVAR                          source/input/nvar.F           
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STATE_MOD                     ../common_source/modules/state_mod.F
Chd|====================================================================
      SUBROUTINE FREDYNAIN(IKAD,KEY0,KDYNAIN,DYNAIN_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE STATE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IKAD(0:*),KDYNAIN
      CHARACTER KEY0(*)*5
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr16_c.inc"
#include      "nchar_c.inc"
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NBC, K, IKEY, N1, KK, IV2(10), J
      CHARACTER KEY2*ncharkey, KEY3*ncharkey, KEY4*ncharkey, KEY5*ncharkey, 
     .          KEY6*ncharkey,KEY7*ncharkey, KEY8*ncharkey, CARTE*ncharline, 
     .          LINE*line120
      TYPE (DYNAIN_DATABASE), INTENT(INOUT) :: DYNAIN_DATA
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------

      IKEY=KDYNAIN

      DYNAIN_DATA%TDYNAIN0 = ZERO
      DYNAIN_DATA%DTDYNAIN0 = ZERO

      DO I = 1,DYNAIN_DATA%MX_DYNAIN
        DYNAIN_DATA%DYNAIN_C(I) = 0
      ENDDO
     
      DYNAIN_DATA%NDYNAINPRT=0
      DYNAIN_DATA%NDYNAINALL=0
      DYNAIN_DATA%ZIPDYNAIN=0
      
c---
      IF (IKAD(IKEY) /= IKAD(IKEY+1)) THEN
        K  = 0
 1175   READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)')LINE
        CALL FREDEC_8KEY_I(LINE,KEY2,KEY3,KEY4,KEY5,KEY6,KEY7,KEY8,NBC)
        K=K+1
        IF (KEY2(1:5) == 'DT   ') THEN
          READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990)CARTE
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
          IF(NVAR(CARTE) ==2 ) THEN
             READ(IUSC2,*)DYNAIN_DATA%TDYNAIN0,DYNAIN_DATA%DTDYNAIN0
          ELSE
             READ(IUSC2,*)DYNAIN_DATA%TDYNAIN0,DYNAIN_DATA%DTDYNAIN0,DYNAIN_DATA%ZIPDYNAIN
          ENDIF
          K=K+1
          IF (KEY3(1:3) == 'ALL') THEN
            DYNAIN_DATA%NDYNAINPRT = 0
            DYNAIN_DATA%NDYNAINALL = 1
            IF (NBC /= 1) THEN
              CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .        C1=KEY0(IKEY),C2=LINE(1:35))
              CALL ARRET(0)
            ENDIF
          ELSE
            DO I=2,NBC
              READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990)CARTE
              CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
              K=K+1
              READ(IUSC2,*,ERR=9990,END=9990)(IV2(J),J=1,NVAR(CARTE))
              DO J=1,NVAR(CARTE)
          WRITE(IIN,'(I10)')IV2(J)
          DYNAIN_DATA%NDYNAINPRT=DYNAIN_DATA%NDYNAINPRT+1
              ENDDO
            ENDDO ! DO I=2,NBC
            IF(DYNAIN_DATA%NDYNAINPRT == 0)THEN
              CALL ANCMSG(MSGID=291,ANMODE=ANINFO)            
              CALL ARRET(0)
            ENDIF
          ENDIF
        ELSE
          IF (KEY2(1:5) == 'SHELL') THEN
            IF (KEY3(1:5) == 'STRES') THEN
              IF (KEY4(1:5) == 'FULL ') THEN
                DYNAIN_DATA%DYNAIN_C(4) = 1
              ELSE
                GOTO 9990
              ENDIF
            ELSEIF (KEY3(1:5) == 'STRAI') THEN
              IF (KEY4(1:5) == 'FULL ') THEN
                DYNAIN_DATA%DYNAIN_C(5) = 1
              ELSE
                GOTO 9990
              ENDIF
            ELSE
              GOTO 9990
            ENDIF ! IF (KEY3)
       
          ENDIF!KEY2 
        ENDIF !IF (KEY2)
c
        IF (IKAD(IKEY)+K /= IKAD(IKEY+1)) GOTO 1175
c
      ENDIF ! IF (IKAD(IKEY) /= IKAD(IKEY+1))
C-----------------------------------------------
      DYNAIN_DATA%NC_DYNAIN = 0
      DO I = 1,DYNAIN_DATA%MX_DYNAIN
        DYNAIN_DATA%NC_DYNAIN = DYNAIN_DATA%NC_DYNAIN + DYNAIN_DATA%DYNAIN_C(I)
      ENDDO
C
      RETURN
C-----------------------------------------------
 9990 CONTINUE
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .            C1=KEY0(IKEY),C2=LINE(1:35))
      CALL ARRET(0)
      END
