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
Chd|  FREANIM                       source/output/anim/reader/freanim.F
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ANIM_DCOD_KEY_0               source/output/anim/reader/anim_dcod_key_0.F
Chd|        ANIM_RESET_INDEX_ALL          source/output/anim/reader/anim_reset_index_all.F
Chd|        ANIM_SET2ZERO_STRUCT          source/output/anim/reader/anim_set2zero_struct.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FREDEC_8KEY_I                 source/input/fredec_8key_i.F  
Chd|        WRIUSC2                       source/input/wriusc2.F        
Chd|        NVAR                          source/input/nvar.F           
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE FREANIM(IKAD,KEY0,KANIM,IFID,MFID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE STACK_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr14_c.inc"
#include      "scr25_c.inc"
#include      "scrcut_c.inc"
#include      "nchara_c.inc"
#include      "nchar_c.inc"
#include      "chara_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IKAD(0:*),
     .        KANIM,IFID,MFID
      CHARACTER KEY0(*)*5
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IDX, NBC, K, IKEY, N1, N2, N3, ITYP,IADG, J,NTITLE,IUS, IXITKEY
      CHARACTER  TITLE*72, KEY2*ncharkey, KEY3*ncharkey, KEY4*ncharkey, KEY5*ncharkey,
     .           KEY6*ncharkey,KEY7*ncharkey, KEY8*ncharkey, CARTE*ncharline,TITLE2*80,
     .           LINE*line120
      my_real X0,Y0,Z0,VNX,VNY,VNZ,V0
C-----------------------------------------------
      IKEY              = KANIM
      ANIMCONT = 0
C-----------------------------------------------
C     Set all indexes and some other variables for ANIM to zero
C-----------------------------------------------
      CALL ANIM_SET2ZERO_STRUCT()
C-----------------------------------------------
C     Loop over /ANIM cards
C-----------------------------------------------
      IXITKEY=0
      IF(IKAD(IKEY).NE.IKAD(IKEY+1))THEN
       K=0
 1160  READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)')LINE
       CALL FREDEC_8KEY_I(LINE,KEY2,KEY3,KEY4,KEY5,KEY6,KEY7,KEY8,NBC)

       K=K+1

       IF(NBC/=0)THEN
C----------------------------
C        THE FOLLOWING OPTIONS USE AN ADDITIONAL CARD
C----------------------------
         IF(KEY2(1:5).EQ.'DT   ')THEN
           CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
!!           READ(IUSC2,*,ERR=9990)TANIM0,DTANIM0
           READ(IUSC2,*,ERR=310,END=320)TANIM0,DTANIM0
           GOTO 320
 310       READ(IUSC2,*,ERR=320,END=320)TANIM0
 320       CONTINUE
!
           IF (DTANIM0 <= ZERO) THEN
             CALL ANCMSG(MSGID=293,ANMODE=ANINFO)            
             CALL ARRET(0)
           ENDIF
!
         ELSEIF(KEY2(1:5).EQ.'SENSO')THEN
           CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
           READ(IUSC2,*,ERR=9990)IANISENS,DTANISENS
         ELSEIF(KEY2(1:5).EQ.'LSENS')THEN
          DO I=1,NBC
             READ(IUSC1,REC=IKAD(IKEY)+K+I-1,FMT='(A)',ERR=9990)CARTE
             CALL WRIUSC2(IKAD(IKEY)+K+I-1,1,KEY0(IKEY))
             READ(IUSC2,*,ERR=9990,END=9990)(LANISENS(J+NLSENS),J=1,NVAR(CARTE))
             DO J=1,NVAR(CARTE)
               NLSENS=NLSENS+1
             ENDDO
          ENDDO
         ELSEIF(KEY2(1:5).EQ.'TITLE')THEN
          DO I=1,NBC
             READ(IUSC1,REC=IKAD(IKEY)+K+I-1,FMT='(A)',ERR=9990)CARTE
             CALL WRIUSC2(IKAD(IKEY)+K+I-1,1,KEY0(IKEY))
             READ(IUSC2,*,ERR=9990,END=9990) NTITLE,TITLE2
             NLTITLE=NLTITLE+1
             NTITLETAB(NLTITLE)=NTITLE
             TITLETAB(NLTITLE)=TITLE2
          ENDDO
         ELSEIF(KEY2(1:3).EQ.'CUT')THEN
          NCUTS=NCUTS+1
          READ(KEY3(1:4),'(I4)')ITYP
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
          READ(IUSC2,'(A)',ERR=9990,END=9990)TITLE
          WRITE(IIN,'(I8,A)')ITYP,TITLE
          K=K+1
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
          IF(ITYP.LT.3)THEN
            READ(IUSC2,*,ERR=9990,END=9990)X0,Y0,Z0,VNX,VNY,VNZ,V0
            WRITE(IIN,'(7E16.9)')X0,Y0,Z0,VNX,VNY,VNZ,V0
          ELSE
            READ(IUSC2,*,ERR=9990,END=9990)N1,N2,N3
            WRITE(IIN,'(3I8)')N1,N2,N3
          ENDIF
          K=K-1
         END IF
       ELSE ! IF(NBC/=0)THEN
C----------------------------
C        THE FOLLOWING OPTIONS DONT USE ANY ADDITIONAL CARD
C----------------------------
         CALL ANIM_DCOD_KEY_0(KEY2,KEY3,KEY4,KEY5,KEY6,IERR,IXITKEY)
         IF(IXITKEY/=0) GOTO 9990
       ENDIF
       K=K+NBC
       IF(IKAD(IKEY)+K.NE.IKAD(IKEY+1))GO TO 1160
      ENDIF
C-----------------------------------------------
C     This routine is resetting indexes in cas of /STRESS/ALL, /STRAIN/ALL, etc
C     The correct indexes wrt integration points or layers of all actual elements 
C     will be rebuilt after reading the restart file.
C-----------------------------------------------
      CALL ANIM_RESET_INDEX_ALL()
C-----------------------------------------------
      NV_ANI = 0
      NT_ANI = 0
      NE_ANI = 0
      NN_ANI = 0
      NCT_ANI = 0
      NCE_ANI = 0
      NST_ANI = 0
      NSE_ANI = 0
      NFT_ANI = 0
      NFE_ANI = 0
      DO I = 1,MX_ANI
        NV_ANI = NV_ANI + ANIM_V(I)
        NT_ANI = NT_ANI + ANIM_T(I)
        NE_ANI = NE_ANI + ANIM_E(I)
        NN_ANI = NN_ANI + ANIM_N(I)
        NST_ANI = NST_ANI + ANIM_ST(I)
        NSE_ANI = NSE_ANI + ANIM_SE(I)
        NCT_ANI = NCT_ANI + ANIM_CT(I)
        NCE_ANI = NCE_ANI + ANIM_CE(I)
        NFT_ANI = NFT_ANI + ANIM_FT(I)
        NFE_ANI = NFE_ANI + ANIM_FE(I)
      ENDDO
      IF(ANIM_V(12).EQ.1)NV_ANI=NV_ANI+1
      IF(ANIM_V(4)==1.AND.ANIMCONT == 0) NV_ANI=NV_ANI-1
      IF(ANIM_V(27).EQ.1)NV_ANI=NV_ANI+1
C-----------------------------------------------
C     Anim Cut non portees en SPMD
      IF (NCUTS.GT.0.AND.NSPMD.GT.1) THEN
       CALL ANCMSG(MSGID=191,ANMODE=ANINFO)
       IERR=IERR+1
      ENDIF
      IF(ANIM_VERS.LT.40) THEN
        CALL ANCMSG(MSGID=192,ANMODE=ANINFO)
        IERR=IERR+1
      ENDIF
      IF (ANIM_VERS.GE.50) THEN
        CALL ANCMSG(MSGID=193,ANMODE=ANINFO)
        IERR=IERR+1
      ENDIF
      RETURN
C-----------------------------------------------
 9990 CONTINUE
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .            C1=KEY0(IKEY),C2=LINE(1:35))
      CALL ARRET(0)
      END
