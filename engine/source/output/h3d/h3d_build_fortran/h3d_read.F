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
Chd|  H3D_READ                      source/output/h3d/h3d_build_fortran/h3d_read.F
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        CREATE_H3D_INPUT              source/output/h3d/h3d_build_fortran/create_h3d_input.F
Chd|        FREDEC_8KEY_I                 source/input/fredec_8key_i.F  
Chd|        WRIUSC2                       source/input/wriusc2.F        
Chd|        NVAR                          source/input/nvar.F           
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE H3D_READ(IKAD,KEY0,KH3D,NSLASH,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE STACK_MOD
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IKAD(0:*),
     .        KH3D,IFID,MFID,NSLASH(*)
      CHARACTER KEY0(*)*5
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "scr25_c.inc"
#include      "scrcut_c.inc"
#include      "chara_c.inc"
#include      "sms_c.inc"
#include      "nchara_c.inc"
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NBC, L, KCUR, N1, N2, N3, ITYP,IADG, J,NTITLE,IUS,
     .        NH3D,IREC,NBSENS
      CHARACTER  TITLE*72, KEY2*ncharkey, KEY3*ncharkey, KEY4*ncharkey, KEY5*ncharkey, 
     .           KEY6*ncharkey,KEY7*ncharkey, KEY8*ncharkey,
     .           CARTE*ncharline,TITLE2*80,LINE*120
      my_real
     .   X0,Y0,Z0,VNX,VNY,VNZ,V0
     
C=========================================================================
      KCUR           = KH3D
      NH3D = NSLASH(KCUR)
c
      H3D_DATA%TH3D0         = ZERO
      H3D_DATA%DTH3D0        = ZERO
      H3D_DATA%PERCENTAGE_ERROR = ZERO
      H3D_DATA%COMP_LEVEL = 7
      H3D_DATA%N_SENS_H3D = 0
c
      IREC = IKAD(KCUR)
      DO L=1,NH3D
        READ(IUSC1,REC=IREC,FMT='(A)')LINE
        CALL FREDEC_8KEY_I(LINE,KEY2,KEY3,KEY4,KEY5,KEY6,KEY7,KEY8,NBC)

        IREC=IREC+1
        IF(KEY2=='DT   ')THEN
          H3D_DATA%TH3D = ZERO
          CALL WRIUSC2(IREC,1,KEY0(KCUR))
!!          READ(IUSC2,*,ERR=9990)H3D_DATA%TH3D0,H3D_DATA%DTH3D0
         READ(IUSC2,*,ERR=310,END=320)H3D_DATA%TH3D0,H3D_DATA%DTH3D0
         GOTO 320
 310     READ(IUSC2,*,ERR=320,END=320)H3D_DATA%TH3D0
 320     CONTINUE
!
         IF (H3D_DATA%DTH3D0 <= ZERO) THEN
           CALL ANCMSG(MSGID=293,ANMODE=ANINFO,c1='H3D',c2='H3D')
           CALL ARRET(0)
         ENDIF
!
        ELSEIF(KEY2=='SENSO')THEN
          CALL WRIUSC2(IREC,1,KEY0(KCUR))
          READ(IUSC2,*,ERR=9990)IANISENS,DTANISENS
        ELSEIF(KEY2=='LSENSOR')THEN
          DO I=1,NBC
            READ(IUSC1,REC=IREC+I-1,FMT='(A)',ERR=9990)CARTE
            H3D_DATA%N_SENS_H3D = H3D_DATA%N_SENS_H3D + NVAR(CARTE)
          ENDDO
          ALLOCATE(H3D_DATA%LSENS_H3D(H3D_DATA%N_SENS_H3D))
          NBSENS = 0
          DO I=1,NBC
            READ(IUSC1,REC=IREC+I-1,FMT='(A)',ERR=9990)CARTE
            CALL WRIUSC2(IREC+I-1,1,KEY0(KCUR))
            READ(IUSC2,*,ERR=9990,END=9990)
     .          (H3D_DATA%LSENS_H3D(J+NBSENS),J=1,NVAR(CARTE))
            NBSENS = NBSENS + NVAR(CARTE)
          ENDDO
        ELSEIF(KEY2=='COMPRESS')THEN
          CALL WRIUSC2(IREC,1,KEY0(KCUR))
          READ(IUSC2,*,ERR=9990)H3D_DATA%PERCENTAGE_ERROR

        ELSEIF(KEY2=='COMP_LEVEL')THEN
          CALL WRIUSC2(IREC,1,KEY0(KCUR))
          READ(IUSC2,*,ERR=9990)H3D_DATA%COMP_LEVEL
        ELSEIF(KEY2=='TITLE')THEN
          H3D_DATA%N_TITLE = NBC
          ALLOCATE(H3D_DATA%ITITLE(NBC))
          ALLOCATE(H3D_DATA%TITLE(NBC))
          DO I=1,NBC
            READ(IUSC1,REC=IREC+I-1,FMT='(A)',ERR=9990)CARTE
            CALL WRIUSC2(IREC+I-1,1,KEY0(KCUR))
            READ(IUSC2,*,ERR=9990)H3D_DATA%ITITLE(I),H3D_DATA%TITLE(I)
          ENDDO
c
c        ELSEIF(KEY2=='GENE')THEN
c          CALL CREATE_H3D_GENE(IKAD,KCUR,K,NBC,KEY0,KEY2,KEY3)
c
        ELSEIF(KEY2=='RBODY')THEN
          IF(KEY3 == 'SINGLE_PART') THEN
            H3D_DATA%RBODY_SINGLE = 1
          ELSE
            CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .                  C1=KEY0(KCUR),C2=LINE(1:35))
            CALL ARRET(0)
          ENDIF
        ELSEIF(KEY2=='RBE2')THEN
          IF(KEY3 == 'SINGLE_PART') THEN
            H3D_DATA%RBE2_SINGLE = 1
          ELSE
            CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .                  C1=KEY0(KCUR),C2=LINE(1:35))
            CALL ARRET(0)
          ENDIF
        ELSEIF(KEY2=='RBE3')THEN
          IF(KEY3 == 'SINGLE_PART') THEN
            H3D_DATA%RBE3_SINGLE = 1
          ELSE
            CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .                  C1=KEY0(KCUR),C2=LINE(1:35))
            CALL ARRET(0)
          ENDIF
C----- TMIN automatically created /w TMAX  (/H3D/?/TENS/STRESS/TMAX ;/H3D/NODA/GPS/TMAX        
        ELSEIF(KEY5=='TMAX'.AND.(KEY4=='STRESS'.OR.KEY4=='STRAIN').AND.KEY2/='BEAM')THEN
          CALL CREATE_H3D_INPUT(H3D_DATA,IKAD,KCUR,IREC,NBC,KEY0,KEY2,KEY3,KEY4,KEY5,KEY6,KEY7,KEY8)
          KEY5 = 'TMIN'
          CALL CREATE_H3D_INPUT(H3D_DATA,IKAD,KCUR,IREC,NBC,KEY0,KEY2,KEY3,KEY4,KEY5,KEY6,KEY7,KEY8)
        ELSEIF(KEY4=='TMAX'.AND.(KEY3=='GPS'.OR.KEY3=='GPSTRAIN'))THEN
          CALL CREATE_H3D_INPUT(H3D_DATA,IKAD,KCUR,IREC,NBC,KEY0,KEY2,KEY3,KEY4,KEY5,KEY6,KEY7,KEY8)
          KEY4 = 'TMIN'
          CALL CREATE_H3D_INPUT(H3D_DATA,IKAD,KCUR,IREC,NBC,KEY0,KEY2,KEY3,KEY4,KEY5,KEY6,KEY7,KEY8)
        ELSE
          CALL CREATE_H3D_INPUT(H3D_DATA,IKAD,KCUR,IREC,NBC,KEY0,KEY2,KEY3,KEY4,KEY5,KEY6,KEY7,KEY8)
        ENDIF
        IREC=IREC+NBC
      ENDDO
C

      RETURN
C
 9990 CONTINUE
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .            C1=KEY0(KCUR),C2=LINE(1:35))
      CALL ARRET(0)
      END
