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
Chd|  I2SMS25                       source/interfaces/interf/i2sms25.F
Chd|-- called by -----------
Chd|        I2FOR25                       source/interfaces/interf/i2for25.F
Chd|        I2FOR25P                      source/interfaces/interf/i2for25p.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I2SMS25(JLT   ,IX1   ,IX2  ,IX3  ,IX4   ,
     2                  NSVG  ,H      ,STIF ,NOINT ,
     3                  DMINT2,NODNX_SMS ,VIS   ,STIFM,DTI)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER JLT, NOINT,
     .          IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),NSVG(MVSIZ),
     .          NODNX_SMS(*)
        my_real
     .      H(4,MVSIZ), STIF(MVSIZ), VIS(MVSIZ), STIFM(MVSIZ),
     .      DMINT2(4,*), DTI
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, IG, NISKYL1, NISKYL, NN
        my_real
     .          MAS, DTS, MASM, FAC
C
        DO I=1,JLT
          DMINT2(1,I)=ZERO
          DMINT2(2,I)=ZERO
          DMINT2(3,I)=ZERO
          DMINT2(4,I)=ZERO
C
          IF(NSVG(I) < 0) CYCLE
          IF((IDTMINS_INT==0.AND.NODNX_SMS(NSVG(I))+
     .       NODNX_SMS(IX1(I))+
     .       NODNX_SMS(IX2(I))+
     .       NODNX_SMS(IX3(I))+
     .       NODNX_SMS(IX4(I))==0) .OR.
     .       STIF(I)==ZERO) THEN
            CYCLE
          END IF
C
          IF(IDTMINS_INT==0.AND.NODNX_SMS(NSVG(I))+
     .       NODNX_SMS(IX1(I))+
     .       NODNX_SMS(IX2(I))+
     .       NODNX_SMS(IX3(I))+
     .       NODNX_SMS(IX4(I))/=0)THEN
            DTS = DTMINS/DTFACS
            DTI=MIN(DTI,DTMINS)
          ELSE
            DTS = DTMINS_INT/DTFACS_INT
            DTI=MIN(DTI,DTMINS_INT)
          END IF

          MAS = HALF * DTS * ( DTS * STIF(I) + VIS(I) )
C
          MASM=ABS(STIFM(I))*MAS
C
          DMINT2(1,I)=ABS(H(1,I))*MAS+MASM
          DMINT2(2,I)=ABS(H(2,I))*MAS+MASM
          DMINT2(3,I)=ABS(H(3,I))*MAS+MASM
          IF(IX4(I)/=IX3(I)) DMINT2(4,I)=ABS(H(4,I))*MAS+MASM
        ENDDO
C
        RETURN
      END
C
