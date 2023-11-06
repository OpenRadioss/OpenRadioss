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
Chd|  FREOUTP                       source/input/freoutp.F
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F
Chd|        GET_KEYS_ID                   source/input/freoutp.F
Chd|        WRIUSC2                       source/input/wriusc2.F
Chd|        NVAR                          source/input/nvar.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F
Chd|====================================================================
      SUBROUTINE FREOUTP(IKAD,KEY0,KOUTP,SENSORS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE MESSAGE_MOD
        USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IKAD(0:*),
     .          KOUTP
        CHARACTER KEY0(*)*5
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "nchar_c.inc"
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
        INTEGER NVAR
        TYPE (SENSORS_), INTENT(INOUT) :: SENSORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, NBC, K, IKEY, N1, N
        CHARACTER KEY2*5, KEY3*5, KEY4*5
        CHARACTER  CART*ncharline,LINE*ncharline
C
        IKEY=KOUTP
        TOUTP0 = ZERO
        DTOUTP0 = ZERO
        DO I = 1,MX_OUTP
          OUTP_V(I) = 0
          OUTP_SS(I) = 0
          OUTP_ST(I) = 0
          OUTP_CS(I) = 0
          OUTP_CT(I) = 0
          OUTP_TS(I) = 0
          OUTP_PS(I) = 0
          OUTP_PT(I) = 0
          OUTP_RS(I) = 0
          OUTP_RT(I) = 0
          OUTP_SPS(I)=0
          OUTP_SPT(I)=0
        ENDDO
c
        IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
          K=0
 1175     READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)') CART
          READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)') LINE

          CALL GET_KEYS_ID(CART,KEY2,KEY3,KEY4,NBC)
          K=K+1
          IF(KEY2=='DT   ')THEN
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,*)TOUTP0,DTOUTP0
          ELSEIF(KEY2(1:4)=='VECT')THEN
            IF    (KEY3(1:3)=='VEL')THEN
              OUTP_V(1) = 1
            ELSEIF(KEY3(1:3)=='DIS')THEN
              OUTP_V(2) = 1
            ELSEIF(KEY3(1:3)=='ACC')THEN
              OUTP_V(3) = 1
            ELSEIF(KEY3(1:4)=='CONT')THEN
              OUTP_V(4) = 1
            ELSEIF(KEY3(1:4)=='FINT')THEN
              OUTP_V(5) = 1
            ELSEIF(KEY3(1:4)=='FEXT')THEN
              OUTP_V(6) = 1
            ELSEIF(KEY3(1:4)=='FOPT')THEN
              OUTP_V(7) = 1
              OUTP_V(8) = 1
            ELSEIF(KEY3(1:4)=='VROT')THEN
              OUTP_V(9) = 1
            ELSEIF(KEY3(1:5)=='PCONT')THEN
              OUTP_V(12) = 1
            ELSE
              GOTO 9990
            ENDIF
          ELSEIF(KEY2(1:4)=='NODA')THEN
            IF    (KEY3(1:2)=='DT')THEN
              OUTP_N(1) = 1
            ELSEIF(KEY3(1:4)=='DMAS')THEN
              OUTP_N(2) = 1
            ELSEIF(KEY3(1:5)=='DINER')THEN
              OUTP_N(3) = 1
            ELSE
              GOTO 9990
            ENDIF
          ELSEIF(KEY2(1:4)=='BRIC'.OR.KEY2(1:4)=='SOLI'.OR.
     .           KEY2(1:4)=='QUAD')THEN
            IF    (KEY3(1:3)=='OFF')THEN
              OUTP_SS(1) = 1
            ELSEIF(KEY3(1:4)=='EPSP')THEN
              OUTP_SS(2) = 1
            ELSEIF(KEY3(1:4)=='ENER')THEN
              OUTP_SS(3) = 1
            ELSEIF(KEY3(1:4)=='DENS')THEN
              OUTP_SS(4) = 1
            ELSEIF(KEY3(1:4)=='TEMP')THEN
              OUTP_SS(5) = 1
            ELSEIF(KEY3(1:1)=='P')THEN
              OUTP_SS(6) = 1
            ELSEIF(KEY3(1:4)=='VONM')THEN
              OUTP_SS(7) = 1
            ELSEIF(KEY3(1:5)=='STRES')THEN
C             IF(KEY4(1:1)=='')THEN
C              OUTP_ST(1) = 1
              IF(KEY4(1:4)=='FULL')THEN
                OUTP_ST(2) = 1
              ELSE
                OUTP_ST(1) = 1
              ENDIF
            ELSEIF(KEY3(1:5)=='USER1')THEN
              OUTP_SS(20) = 1
            ELSEIF(KEY3(1:5)=='USER2')THEN
              OUTP_SS(21) = 1
            ELSEIF(KEY3(1:5)=='USER3')THEN
              OUTP_SS(22) = 1
            ELSEIF(KEY3(1:5)=='USER4')THEN
              OUTP_SS(23) = 1
            ELSEIF(KEY3(1:5)=='USER5')THEN
              OUTP_SS(24) = 1
            ELSEIF(KEY3(1:4)=='HOUR')THEN
              OUTP_SS(25) = 1
            ELSEIF(KEY3(1:5)=='USERS')THEN
              IF(KEY4(1:4)=='FULL')THEN
                OUTP_SS(26) = 1
              ELSE
              ENDIF
            ELSEIF(KEY3(1:5)=='STRAI')THEN
              IF(KEY4(1:4)=='FULL')THEN
                OUTP_ST(3) = 1
              ELSE
                OUTP_ST(3) = 1
              ENDIF
C-----------------------------------------------
C   element equivalent stress criteria
C-----------------------------------------------
            ELSEIF(KEY3(1:5)=='SIGEQ')THEN
              OUTP_SS(27) = 1
            ELSE
              GOTO 9990
            ENDIF
          ELSEIF(KEY2(1:4)=='SHEL')THEN
            IF    (KEY3(1:3)=='OFF')THEN
              OUTP_CS(1) = 1
            ELSEIF(KEY3(1:4)=='EPSP')THEN
              OUTP_CS(2) = 1
            ELSEIF(KEY3(1:5)=='USER1')THEN
              OUTP_CS(20) = 1
            ELSEIF(KEY3(1:5)=='USER2')THEN
              OUTP_CS(21) = 1
            ELSEIF(KEY3(1:5)=='USER3')THEN
              OUTP_CS(22) = 1
            ELSEIF(KEY3(1:5)=='USER4')THEN
              OUTP_CS(23) = 1
            ELSEIF(KEY3(1:5)=='USER5')THEN
              OUTP_CS(24) = 1
            ELSEIF(KEY3(1:5)=='USR01' .OR.KEY3(1:4)=='USR1')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(27) = 1
            ELSEIF(KEY3(1:5)=='USR02')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(28) = 1
            ELSEIF(KEY3(1:5)=='USR03')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(29) = 1
            ELSEIF(KEY3(1:5)=='USR04')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(30) = 1
            ELSEIF(KEY3(1:5)=='USR05')THEN
              IF  (KEY4(1:4)=='FULL') OUTP_CS(31) = 1
            ELSEIF(KEY3(1:5)=='USR06')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(32) = 1
            ELSEIF(KEY3(1:5)=='USR07')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(33) = 1
            ELSEIF(KEY3(1:5)=='USR08')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(34) = 1
            ELSEIF(KEY3(1:5)=='USR09')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(35) = 1
            ELSEIF(KEY3(1:5)=='USR10')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(36) = 1
            ELSEIF(KEY3(1:5)=='USR11')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(37) = 1
            ELSEIF(KEY3(1:5)=='USR12')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(38) = 1
            ELSEIF(KEY3(1:5)=='USR13')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(39) = 1
            ELSEIF(KEY3(1:5)=='USR14')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(40) = 1
            ELSEIF(KEY3(1:5)=='USR15')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(41) = 1
            ELSEIF(KEY3(1:5)=='USR16')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(42) = 1
            ELSEIF(KEY3(1:5)=='USR17')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(43) = 1
            ELSEIF(KEY3(1:5)=='USR18')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(44) = 1
            ELSEIF(KEY3(1:5)=='USR19')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(45) = 1
            ELSEIF(KEY3(1:5)=='USR20')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(46) = 1
            ELSEIF(KEY3(1:5)=='USR21')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(47) = 1
            ELSEIF(KEY3(1:5)=='USR22')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(48) = 1
            ELSEIF(KEY3(1:5)=='USR23')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(49) = 1
            ELSEIF(KEY3(1:5)=='USR24')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(50) = 1
            ELSEIF(KEY3(1:5)=='USR25')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(51) = 1
            ELSEIF(KEY3(1:5)=='USR26')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(52) = 1
            ELSEIF(KEY3(1:5)=='USR27')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(53) = 1
            ELSEIF(KEY3(1:5)=='USR28')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(54) = 1
            ELSEIF(KEY3(1:5)=='USR29')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(55) = 1
            ELSEIF(KEY3(1:5)=='USR30')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(56) = 1
            ELSEIF(KEY3(1:5)=='USR31')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(57) = 1
            ELSEIF(KEY3(1:5)=='USR32')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(58) = 1
            ELSEIF(KEY3(1:5)=='USR33')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(59) = 1
            ELSEIF(KEY3(1:5)=='USR34')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(60) = 1
            ELSEIF(KEY3(1:5)=='USR35')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(61) = 1
            ELSEIF(KEY3(1:5)=='USR36')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(62) = 1
            ELSEIF(KEY3(1:5)=='USR37')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(63) = 1
            ELSEIF(KEY3(1:5)=='USR38')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(64) = 1
            ELSEIF(KEY3(1:5)=='USR39')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(65) = 1
            ELSEIF(KEY3(1:5)=='USR40')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(66) = 1
            ELSEIF(KEY3(1:5)=='USR41')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(67) = 1
            ELSEIF(KEY3(1:5)=='USR42')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(68) = 1
            ELSEIF(KEY3(1:5)=='USR43')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(69) = 1
            ELSEIF(KEY3(1:5)=='USR44')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(70) = 1
            ELSEIF(KEY3(1:5)=='USR45')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(71) = 1
            ELSEIF(KEY3(1:5)=='USR46')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(72) = 1
            ELSEIF(KEY3(1:5)=='USR47')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(73) = 1
            ELSEIF(KEY3(1:5)=='USR48')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(74) = 1
            ELSEIF(KEY3(1:5)=='USR49')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(75) = 1
            ELSEIF(KEY3(1:5)=='USR50')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(76) = 1
            ELSEIF(KEY3(1:5)=='USR51')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(77) = 1
            ELSEIF(KEY3(1:5)=='USR52')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(78) = 1
            ELSEIF(KEY3(1:5)=='USR53')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(79) = 1
            ELSEIF(KEY3(1:5)=='USR54')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(80) = 1
            ELSEIF(KEY3(1:5)=='USR55')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(81) = 1
            ELSEIF(KEY3(1:5)=='USR56')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(82) = 1
            ELSEIF(KEY3(1:5)=='USR57')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(83) = 1
            ELSEIF(KEY3(1:5)=='USR58')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(84) = 1
            ELSEIF(KEY3(1:5)=='USR59')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(85) = 1
            ELSEIF(KEY3(1:5)=='USR60')THEN
              IF    (KEY4(1:4)=='FULL') OUTP_CS(86) = 1
            ELSEIF(KEY3(1:4)=='HOUR')THEN
              OUTP_CS(25) = 1
            ELSEIF(KEY3(1:4)=='ENER')THEN
              OUTP_CS(3) = 1
            ELSEIF(KEY3(1:4)=='THIC')THEN
              OUTP_CS(4) = 1
            ELSEIF(KEY3(1:4)=='VONM')THEN
              OUTP_CS(7) = 1
            ELSEIF(KEY3(1:5)=='STRES')THEN
              IF    (KEY4(1:4)=='MEMB')THEN
                OUTP_CT(1) = 1
              ELSEIF(KEY4(1:4)=='BEND')THEN
                OUTP_CT(2) = 1
              ELSEIF(KEY4(1:5)=='UPPER')THEN
                OUTP_CT(3) = 1
              ELSEIF(KEY4(1:5)=='LOWER')THEN
                OUTP_CT(4) = 1
              ELSEIF(KEY4(1:4)=='FULL')THEN
                OUTP_CT(95) = 1
              ELSE
                READ(KEY4,'(I5)')N1
                IF(N1>=0.AND.N1<=30)THEN
                  OUTP_CT(10+N1) = 1
                ELSE
                  GOTO 9990
                ENDIF
              ENDIF
            ELSEIF(KEY3(1:5)=='USERS')THEN
              IF(KEY4(1:4)=='FULL')THEN
                OUTP_CS(26) = 1
              ELSE
                GOTO 9990
              ENDIF
            ELSEIF(KEY3(1:5)=='STRAI')THEN
              IF    (KEY4(1:4)=='MEMB')THEN
                OUTP_CT(5) = 1
              ELSEIF(KEY4(1:4)=='BEND')THEN
                OUTP_CT(6) = 1
              ELSEIF(KEY4(1:5)=='UPPER')THEN
                OUTP_CT(7) = 1
              ELSEIF(KEY4(1:5)=='LOWER')THEN
                OUTP_CT(8) = 1
              ELSEIF(KEY4(1:4)=='FULL')THEN
                OUTP_CT(96) = 1
              ELSE
                READ(KEY4,'(I5)')N1
                IF(N1>0.AND.N1<=30)THEN
                  OUTP_CT(50+N1) = 1
                ELSE
                  GOTO 9990
                ENDIF
              ENDIF
            ELSEIF(KEY3(1:5)=='EPSDO')THEN
              IEPSDOT = 1
              IF    (KEY4(1:4)=='MEMB')THEN
                OUTP_CT(91) = 1
              ELSEIF(KEY4(1:4)=='BEND')THEN
                OUTP_CT(92) = 1
              ELSEIF(KEY4(1:5)=='UPPER')THEN
                OUTP_CT(93) = 1
              ELSEIF(KEY4(1:5)=='LOWER')THEN
                OUTP_CT(94) = 1
              ELSE
                READ(KEY4,'(I5)')N1
                IF(N1>0.AND.N1<=30)THEN
                  OUTP_CT(100+N1) = 1
                ELSE
                  GOTO 9990
                ENDIF
              ENDIF
C-----------------------------------------------
C   element equivalent stress criteria
C-----------------------------------------------
            ELSEIF(KEY3(1:5)=='SIGEQ')THEN
              OUTP_CS(87) = 1
            ELSE
              GOTO 9990
            ENDIF
          ELSEIF(KEY2(1:4)=='TRUS')THEN
            IF    (KEY3(1:3)=='OFF')THEN
              OUTP_TS(1) = 1
            ELSEIF(KEY3(1:4)=='EPSP')THEN
              OUTP_TS(2) = 1
            ELSE
              GOTO 9990
            ENDIF
          ELSEIF(KEY2(1:4)=='BEAM')THEN
            IF    (KEY3(1:3)=='OFF')THEN
              OUTP_PS(1) = 1
            ELSEIF(KEY3(1:4)=='EPSP')THEN
              OUTP_PS(2) = 1
            ELSE
              GOTO 9990
            ENDIF
          ELSEIF(KEY2(1:4)=='SPRI')THEN
            IF    (KEY3(1:3)=='OFF')THEN
              OUTP_RS(1) = 1
            ELSEIF(KEY3(1:4)=='FULL')THEN
              OUTP_RS(2) = 1
            ELSE
              GOTO 9990
            ENDIF
          ELSEIF(KEY2(1:3)=='SPH')THEN
            IF    (KEY3(1:3)=='OFF')THEN
              OUTP_SPS(1) = 1
            ELSEIF(KEY3(1:4)=='EPSP')THEN
              OUTP_SPS(2) = 1
            ELSEIF(KEY3(1:4)=='ENER')THEN
              OUTP_SPS(3) = 1
            ELSEIF(KEY3(1:4)=='DENS')THEN
              OUTP_SPS(4) = 1
            ELSEIF(KEY3(1:4)=='TEMP')THEN
              OUTP_SPS(5) = 1
            ELSEIF(KEY3(1:1)=='P')THEN
              OUTP_SPS(6) = 1
            ELSEIF(KEY3(1:4)=='VONM')THEN
              OUTP_SPS(7) = 1
            ELSEIF(KEY3(1:5)=='STRES')THEN
              IF(KEY4(1:4)=='FULL')THEN
                OUTP_SPT(2) = 1
              ELSE
                OUTP_SPT(1) = 1
              ENDIF
            ELSEIF(KEY3(1:5)=='USER1')THEN
              OUTP_SPS(20) = 1
            ELSEIF(KEY3(1:5)=='USER2')THEN
              OUTP_SPS(21) = 1
            ELSEIF(KEY3(1:5)=='USER3')THEN
              OUTP_SPS(22) = 1
            ELSEIF(KEY3(1:5)=='USER4')THEN
              OUTP_SPS(23) = 1
            ELSEIF(KEY3(1:5)=='USER5')THEN
              OUTP_SPS(24) = 1
            ELSEIF(KEY3(1:4)=='SLEN')THEN
              OUTP_SPS(25) = 1
C-----------------------------------------------
C   element equivalent stress criteria
C-----------------------------------------------
            ELSEIF(KEY3(1:5)=='SIGEQ')THEN
              OUTP_SPS(26) = 1
            ELSE
              GOTO 9990
            ENDIF
          ELSEIF(KEY2(1:4)=='ELEM')THEN
            IF    (KEY3(1:3)=='OFF')THEN
              OUTP_SS(1) = 1
              OUTP_CS(1) = 1
              OUTP_TS(1) = 1
              OUTP_PS(1) = 1
              OUTP_RS(1) = 1
              OUTP_SPS(1) = 1
            ELSEIF(KEY3(1:4)=='EPSP')THEN
              OUTP_SS(2) = 1
              OUTP_CS(2) = 1
              OUTP_TS(2) = 1
              OUTP_PS(2) = 1
              OUTP_SPS(2) = 1
            ELSEIF(KEY3(1:4)=='ENER')THEN
              OUTP_SS(3) = 1
              OUTP_CS(3) = 1
              OUTP_SPS(3) = 1
            ELSEIF(KEY3(1:4)=='VONM')THEN
              OUTP_SS(7) = 1
              OUTP_CS(7) = 1
              OUTP_SPS(7) = 1
            ELSEIF(KEY3(1:5)=='USER1')THEN
              OUTP_SS(20) = 1
              OUTP_CS(20) = 1
              OUTP_SPS(20) = 1
            ELSEIF(KEY3(1:5)=='USER2')THEN
              OUTP_SS(21) = 1
              OUTP_CS(21) = 1
              OUTP_SPS(21) = 1
            ELSEIF(KEY3(1:5)=='USER3')THEN
              OUTP_SS(22) = 1
              OUTP_CS(22) = 1
              OUTP_SPS(22) = 1
            ELSEIF(KEY3(1:5)=='USER4')THEN
              OUTP_SS(23) = 1
              OUTP_CS(23) = 1
              OUTP_SPS(23) = 1
            ELSEIF(KEY3(1:5)=='USER5')THEN
              OUTP_SS(24) = 1
              OUTP_CS(24) = 1
              OUTP_SPS(24) = 1
            ELSEIF(KEY3(1:4)=='HOUR')THEN
              OUTP_SS(25) = 1
              OUTP_CS(25) = 1
            ELSEIF(KEY3(1:4)=='SLEN')THEN
              OUTP_SPS(25) = 1
C-----------------------------------------------
C   element equivalent stress criteria
C-----------------------------------------------
            ELSEIF(KEY3(1:5)=='SIGEQ')THEN
              OUTP_SS(27) = 1
              OUTP_CS(87) = 1
              OUTP_SPS(26) = 1
            ELSE
              GOTO 9990
            ENDIF
          ELSEIF (KEY2=='LSENS')THEN
            DO I=1,NBC
              READ(IUSC1,REC=IKAD(IKEY)+K+I-1,FMT='(A)',ERR=9990)CART
              CALL WRIUSC2(IKAD(IKEY)+K+I-1,1,KEY0(IKEY))
              READ(IUSC2,*,ERR=9990,END=9990)
     .            (SENSORS%OUTP_TMP(N),N=1,NVAR(CART))
              SENSORS%NOUTP = SENSORS%NOUTP + NVAR(CART)
            ENDDO
          ELSE
            GOTO 9990
          ENDIF
          K=K+NBC
          IF(IKAD(IKEY)+K/=IKAD(IKEY+1))GO TO 1175
        ENDIF
        NV_OUTP = 0
        NSS_OUTP = 0
        NST_OUTP = 0
        NCS_OUTP = 0
        NCT_OUTP = 0
        NTS_OUTP = 0
        NPS_OUTP = 0
        NPT_OUTP = 0
        NRS_OUTP = 0
        NRT_OUTP = 0
        NSPS_OUTP = 0
        NSPT_OUTP = 0
        DO I = 1,MX_OUTP
          NV_OUTP = NV_OUTP + OUTP_V(I)
          NSS_OUTP = NSS_OUTP + OUTP_SS(I)
          NST_OUTP = NST_OUTP + OUTP_ST(I)
          NCS_OUTP = NCS_OUTP + OUTP_CS(I)
          NCT_OUTP = NCT_OUTP + OUTP_CT(I)
          NTS_OUTP = NTS_OUTP + OUTP_TS(I)
          NPS_OUTP = NPS_OUTP + OUTP_PS(I)
          NPT_OUTP = NPT_OUTP + OUTP_PT(I)
          NRS_OUTP = NRS_OUTP + OUTP_RS(I)
          NRT_OUTP = NRT_OUTP + OUTP_RT(I)
          NSPS_OUTP = NSPS_OUTP + OUTP_SPS(I)
          NSPT_OUTP = NSPT_OUTP + OUTP_SPT(I)
        ENDDO
        IF(OUTP_V(12)==1)NV_OUTP=NV_OUTP+1
C
        RETURN
C
 9990   CONTINUE
        CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .              C1=KEY0(IKEY),C2=LINE(1:35))
        CALL ARRET(0)
      END

C             FREOUTP                         /freef/freoutp.F
Chd|====================================================================
Chd|  GET_KEYS_ID                   source/input/freoutp.F
Chd|-- called by -----------
Chd|        FREOUTP                       source/input/freoutp.F
Chd|        FREUPWM                       source/input/freupwm.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GET_KEYS_ID(CART, KEY2, KEY3,KEY4, ID)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   V a r i a b l e s
C-----------------------------------------------
        CHARACTER CART*(*), KEY2*(*),KEY3*(*),KEY4*(*)
        CHARACTER MOT1*80
        INTEGER   I1,I2,I3,I4,ID
C======================================================================|
C     /Key/Id/
C-----------------------------------------------
        I1=8
        DO WHILE(CART(I1:I1)/='/')
          I1=I1+1
        ENDDO
        KEY2=CART(8:I1-1)
        I1=I1+1
        I2=I1
        DO WHILE(CART(I2:I2)/='/')
          I2=I2+1
        ENDDO
        KEY3 = CART(I1:I2-1)
        I2=I2+1
        I3=I2
        DO WHILE(CART(I3:I3)/='/')
          I3=I3+1
        ENDDO
        KEY4 = CART(I2:I3-1)
        I3=I3+1
        I4=I3
        MOT1 = CART(I4+25:I4+34)
        READ(MOT1,'(I10)')ID
C---
        RETURN
      END
