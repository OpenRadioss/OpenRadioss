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
Chd|  FRESTAT                       source/input/frestat.F        
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FREDEC_8KEY_I                 source/input/fredec_8key_i.F  
Chd|        WRIUSC2                       source/input/wriusc2.F        
Chd|        NVAR                          source/input/nvar.F           
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STATE_INIMAP_MOD              share/modules/state_inimap_mod.F
Chd|====================================================================
      SUBROUTINE FRESTAT(IKAD,KEY0,KSTATF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE STATE_INIMAP_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IKAD(0:*),
     .        KSTATF
      CHARACTER KEY0(*)*5
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "chara_c.inc"
#include      "nchar_c.inc"
#include      "nchar_c.inc"
#include      "state_c.inc"
#include      "com01_c.inc" 
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NBC, K, IKEY, N1, KK, IV2(10), K_STAT, J
      CHARACTER KEY2*ncharkey, KEY3*ncharkey, KEY4*ncharkey, KEY5*ncharkey, 
     .          KEY6*ncharkey,KEY7*ncharkey, KEY8*ncharkey, CARTE*ncharline, 
     .          LINE*line120
      LOGICAL BOOL
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------
      IS_STAT = .FALSE.
      IS_STAT_DT = .FALSE.
      IS_STAT_SHELL = .FALSE.
      IS_STAT_BRICK = .FALSE.
      IS_STAT_SPRING = .FALSE.
      IS_STAT_BEAM = .FALSE.
      IS_STAT_TRUSS = .FALSE.
      IS_STAT_INIMAP1D = .FALSE.
      IS_STAT_INIMAP2D = .FALSE.
      IS_STAT_STRF = .FALSE.
      IS_STAT_NODE = .FALSE.
      IS_STAT_LSENS = .FALSE.
      IS_STAT_NO_DE = .FALSE.
      IS_STAT_INIMAP_FILE = .FALSE. 
      IS_STAT_INIMAP_VP = .FALSE.
      IS_STAT_INIMAP_VE = .FALSE.
      
      IKEY=KSTATF

      TSTAT0 = ZERO
      DTSTAT0 = ZERO
C-----MX_STAT=20
      DO I = 1,MX_STAT
        STAT_N(I) = 0
        STAT_C(I) = 0
        STAT_S(I) = 0
        STAT_R(I) = 0
        STAT_P(I) = 0
        STAT_T(I) = 0
      ENDDO
      DO I=1,MX_STAT3
        STAT_INIMAP(I) = 0
      ENDDO      
      NSTATPRT=0
      NSTATALL=0
      
      IS_STAT_INIMAP_SINGLE = .TRUE.
      IS_STAT_INIMAP_MSG_ALREADY_DISPLAYED = .FALSE.
c---
      IF (IKAD(IKEY) /= IKAD(IKEY+1)) THEN
        K  = 0
 1175   READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)')LINE
        CALL FREDEC_8KEY_I(LINE,KEY2,KEY3,KEY4,KEY5,KEY6,KEY7,KEY8,NBC)
        K=K+1
        IS_STAT = .TRUE.
        IF (KEY2(1:5) == 'DT   ') THEN
          IS_STAT_DT = .TRUE.
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
          READ(IUSC2,*)TSTAT0,DTSTAT0
          K=K+1
          IF (KEY3(1:3) == 'ALL') THEN
            NSTATPRT = 0
            NSTATALL = 1
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
           	NSTATPRT=NSTATPRT+1
              ENDDO
            ENDDO ! DO I=2,NBC
            IF(NSTATPRT == 0)THEN
              CALL ANCMSG(MSGID=289,ANMODE=ANINFO)            
              CALL ARRET(0)
            ENDIF
          ENDIF
        ELSEIF (KEY2(1:5) == 'STR_F') THEN
          IS_STAT_STRF = .TRUE.
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
          READ(IUSC2,*,ERR=9990,END=9990)IZIPSTRS
          IZIPSTRS = IZIPSTRS + 1
        ELSE
          IF (KEY2(1:4) == 'NODE') THEN
            IS_STAT_NODE = .TRUE.
            IF (KEY3(1:4) == 'TEMP') THEN
              STAT_N(1) = 1
            ELSEIF (KEY3(1:3) == 'BCS') THEN
              STAT_N(2) = 1
            ELSEIF (KEY3(1:3) == 'VEL') THEN
              STAT_N(3) = 1
            ELSE
              GOTO 9990
            ENDIF
          ELSEIF (KEY2(1:5) == 'SHELL') THEN
            IS_STAT_SHELL = .TRUE.
            IF (KEY3(1:5) == 'OFF  ') THEN
              STAT_C(1) = 1
            ELSEIF (KEY3(1:5) == 'THICK') THEN
              STAT_C(2) = 1
            ELSEIF (KEY3(1:5) == 'EPSP ') THEN
              IF (KEY4(1:5) == 'FULL ') THEN
                STAT_C(3) = 1	    
              ELSE
                GOTO 9990
              ENDIF
            ELSEIF (KEY3(1:5) == 'STRES') THEN
              IF (KEY4(1:5) == 'FULL ') THEN
                STAT_C(4) = 1
              ELSEIF (KEY4(1:5) == 'GLOBF') THEN
                STAT_C(10) = 1
              ELSE
                GOTO 9990
              ENDIF
            ELSEIF (KEY3(1:5) == 'STRAI') THEN
              IF (KEY4(1:5) == 'FULL ') THEN
                STAT_C(5) = 1
              ELSEIF (KEY4(1:5) == 'GLOBF') THEN
                STAT_C(11) = 1
              ELSE
                GOTO 9990
              ENDIF
            ELSEIF (KEY3(1:3) == 'AUX') THEN
              IF (KEY4(1:5) == 'FULL ') THEN
                STAT_C(6) = 1
              ELSE
                GOTO 9990
              ENDIF
	    ELSEIF (KEY3(1:5) == 'ORTHL') THEN
              STAT_C(7) = 1
	    ELSEIF (KEY3(1:4) == 'FAIL') THEN
              STAT_C(8) = 1
            ELSE
              GOTO 9990
            ENDIF ! IF (KEY3)
          ELSEIF (KEY2(1:5) == 'BRICK') THEN
            IS_STAT_BRICK = .TRUE.
            IF (KEY3(1:5) == 'STRES') THEN
              IF (KEY4(1:5) == 'FULL ') THEN
                STAT_S(4) = 1
              ELSEIF (KEY4(1:5) == 'GLOBF') THEN
                STAT_S(8) = 1
              ELSE
                GOTO 9990
              ENDIF
            ELSEIF (KEY3(1:5) == 'STRAI') THEN
              IF (KEY4(1:5) == 'FULL ') THEN
                STAT_S(5) = 1
              ELSEIF (KEY4(1:5) == 'GLOBF') THEN
                STAT_S(9) = 1
              ELSE
                GOTO 9990
              ENDIF
            ELSEIF (KEY3(1:3) == 'AUX') THEN
              IF (KEY4(1:5) == 'FULL ') THEN
                STAT_S(6) = 1
              ELSE
                GOTO 9990
              ENDIF
            ELSEIF (KEY3(1:5) == 'ORTHO') THEN            
              IF (KEY4(1:5) == 'GLOBF ') THEN
                STAT_S(10) = 1 
              ELSE
                STAT_S(7) = 1
              ENDIF
            ELSEIF (KEY3(1:4) == 'FAIL') THEN
              STAT_S(11) = 1 
            ELSEIF (KEY3(1:4) == 'EREF') THEN
              STAT_S(13) = 1 
            ELSE
              GOTO 9990
            ENDIF
          ELSEIF (KEY2(1:5) == 'LSENS') THEN
            IS_STAT_LSENS = .TRUE.
            DO I=1,NBC
              READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990)CARTE
              CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
              K=K+1
              READ(IUSC2,*,ERR=9990,END=9990)
     .            (LSTATSENS(J),J=1,NVAR(CARTE))
              NSTATSENS=NSTATSENS+NVAR(CARTE)
            ENDDO 
          ELSEIF (KEY2(1:5) == 'NO_DE') THEN
            IS_STAT_NO_DE = .TRUE.
            STAT_C(9) = 1
            STAT_S(12) = 1
            STAT_R(2) = 1
            STAT_P(2) = 1
            STAT_T(2) = 1
          ELSEIF (KEY2(1:5) == 'SPRIN') THEN
            IS_STAT_SPRING = .TRUE.
            IF (KEY3(1:4) == 'FULL') THEN
              STAT_R(1) = 1
            ELSE
              GOTO 9990
            ENDIF
          ELSEIF (KEY2(1:4) == 'BEAM') THEN
            IS_STAT_BEAM = .TRUE.          
            IF (KEY3(1:4) == 'FULL') THEN
              STAT_P(1) = 1
            ELSEIF (KEY3(1:3) == 'AUX') THEN
              STAT_P(3) = 1
            ELSE
              GOTO 9990
            ENDIF
          ELSEIF (KEY2(1:5) == 'TRUSS') THEN
            IS_STAT_TRUSS = .TRUE.          
            IF (KEY3(1:4) == 'FULL') THEN
              STAT_T(1) = 1
            ELSE
              GOTO 9990
            ENDIF
          ELSEIF (KEY2(1:8) == 'INIMAP1D') THEN
              IS_STAT_INIMAP1D=.TRUE.                      
              STATE_INIMAP_CALL_NUMBER = 0
              STAT_INIMAP(1) = 1
              IF(KEY3(1:5) == 'FILE ')THEN
                STAT_INIMAP(1) = 11
                IS_STAT_INIMAP_FILE = .TRUE.
              ELSEIF(KEY3(1:2) == 'VP')THEN
                IS_STAT_INIMAP_VP = .TRUE.
              ELSEIF(KEY3(1:2) == 'VE')THEN              
                IS_STAT_INIMAP_VE = .TRUE.   
              ELSE
                CALL ANCMSG(MSGID=73,ANMODE=ANINFO,C1=KEY0(IKEY),C2=LINE(1:35))
                CALL ARRET(0)                                                               
              ENDIF
          ELSEIF (KEY2(1:8) == 'INIMAP2D') THEN
              IS_STAT_INIMAP2D=.TRUE.  
              STATE_INIMAP_CALL_NUMBER = 0 
              STAT_INIMAP(1) = 2                      
              IF(KEY3(1:5) == 'FILE ')THEN
                STAT_INIMAP(1) = 12
                IS_STAT_INIMAP_FILE = .TRUE.
              ELSEIF(KEY3(1:2) == 'VP')THEN
                IS_STAT_INIMAP_VP = .TRUE.
              ELSEIF(KEY3(1:2) == 'VE')THEN           
                IS_STAT_INIMAP_VE = .TRUE.                                
              ELSE
                CALL ANCMSG(MSGID=73,ANMODE=ANINFO,C1=KEY0(IKEY),C2=LINE(1:35))
                CALL ARRET(0)                                                               
              ENDIF         
          ENDIF!KEY2 
        ENDIF !IF (KEY2)
c
        IF (IKAD(IKEY)+K /= IKAD(IKEY+1)) GOTO 1175

        BOOL=.FALSE.
        IF(IS_STAT_SHELL)BOOL=.TRUE.
        IF(IS_STAT_BRICK)BOOL=.TRUE.
        IF(IS_STAT_SPRING)BOOL=.TRUE.
        IF(IS_STAT_BEAM)BOOL=.TRUE.
        IF(IS_STAT_TRUSS)BOOL=.TRUE.
        IF(IS_STAT_STRF)BOOL=.TRUE.
        IF(IS_STAT_NODE)BOOL=.TRUE.
        IF(IS_STAT_LSENS)BOOL=.TRUE.
        IF(IS_STAT_NO_DE)BOOL=.TRUE.
        
        !do not export STATE FILE IF /STATE/INIMAP IS USED AS A SINGLE /STATE OPTION
        IF(BOOL .AND. (IS_STAT_INIMAP2D .OR. IS_STAT_INIMAP1D))THEN
          ! /STATE/INIMAP used with another /STATE/ option (SHELL,BRICK,..)
          ! state file must be written
          IS_STAT_INIMAP_SINGLE=.FALSE.
        ELSEIF(IS_STAT_DT .AND.  .NOT.IS_STAT_INIMAP2D .AND. .NOT.IS_STAT_INIMAP1D)THEN
          ! /STATE/INIMAP not used but STATE/DT requires also to output .sta file
          IS_STAT_INIMAP_SINGLE=.FALSE.
        ENDIF                                                                                            !
c
      ENDIF ! IF (IKAD(IKEY) /= IKAD(IKEY+1))
C-----------------------------------------------
      NC_STAT = 0
      DO I = 1,MX_STAT
        NC_STAT = NC_STAT + STAT_C(I)
      ENDDO
C
      RETURN
C-----------------------------------------------
 9990 CONTINUE
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .            C1=KEY0(IKEY),C2=LINE(1:35))
      CALL ARRET(0)
      END
