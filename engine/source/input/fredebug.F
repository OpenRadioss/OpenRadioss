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
Chd|  FREDEBUG                      source/input/fredebug.F       
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        WRIUSC2                       source/input/wriusc2.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FREDEBUG(IKAD,KEY0,KDEBUG) 
C----------------------------------------------- 
C   M o d u l e s 
C----------------------------------------------- 
      USE MESSAGE_MOD 
C----------------------------------------------- 
C   I m p l i c i t   T y p e s 
C----------------------------------------------- 
#include      "implicit_f.inc" 
C----------------------------------------------- 
C   D u m m y   A r g u m e n t s 
C----------------------------------------------- 
      INTEGER   :: IKAD(0:*),KDEBUG 
      CHARACTER :: KEY0(*)*5 
C----------------------------------------------- 
C   C o m m o n   B l o c k s 
C----------------------------------------------- 
#include      "units_c.inc" 
#include      "warn_c.inc"    
#include      "macro.inc"
C----------------------------------------------- 
C   E x t e r n a l   F u n c t i o n s 
C----------------------------------------------- 
C 
C----------------------------------------------- 
C   L o c a l   V a r i a b l e s 
C----------------------------------------------- 
      INTEGER I, NN, IKEY, K 
      CHARACTER KEY2*5 
      IF (ITRACE/=-1) THEN 
        ITRACE=0 
      END IF 
      DEBSTART    = 0 
      RSTFREQ     = 0 
      IF(ITRACE/=-1)ITRACE=1 
      IKEY=KDEBUG 
C-------  DEBUG(12) is free, can be used--- 
      DEBUG(1:MX_DEBUG) = 0 
      K=0 
      DO WHILE(IKAD(IKEY)+K/=IKAD(IKEY+1)) 
        READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,1X,I5)',ERR=9990)KEY2,NN 
        K=K+1 
        IF(KEY2(1:5)=='INTER')THEN 
         DEBUG(MACRO_DEBUG_INTER) = NN 
        ELSEIF(KEY2(1:5)=='RESTA')THEN 
         DEBUG(MACRO_DEBUG_RESTA) = NN 
        ELSEIF(KEY2(1:4)=='CAND')THEN 
         DEBUG(MACRO_DEBUG_CAND) = NN 
        ELSEIF(KEY2(1:4)=='CORE')THEN 
           DEBUG(MACRO_DEBUG_CORE) = NN 
           IF(ITRACE/=-1)ITRACE = NN 
           IF(NN/=0.AND.ITRACE/=-1)ITRACE=1 
        ELSEIF(KEY2(1:3)=='ACC')THEN 
          DEBUG(MACRO_DEBUG_ACC) = 1 
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY)) 
          K=K+1 
          READ(IUSC2,*,ERR=9990,END=9990) DEBSTART,RSTFREQ 
          IF (RSTFREQ==0) RSTFREQ=1 
        ELSEIF(KEY2(1:3)=='MEM')THEN 
         ! memory map (aralloc) 
          DEBUG(MACRO_DEBUG_MEM) = 1 
        ELSEIF(KEY2(1:3)=='AMS')THEN 
          DEBUG(MACRO_DEBUG_AMS) = 1 
        ELSEIF(KEY2(1:4)=='TEMP')THEN 
          DEBUG(MACRO_DEBUG_TEMP) = 1 
          CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY)) 
          READ(IUSC2,*,ERR=9990,END=9990) TDEBSTART,TRSTFREQ 
          IF (TRSTFREQ==0) TRSTFREQ=1 
        ELSEIF(KEY2(1:3)=='PON')THEN 
          DEBUG(MACRO_DEBUG_PON) = 1 
        ELSEIF(KEY2(1:5)=='CONV0')THEN 
          DEBUG(MACRO_DEBUG_CONV0)= NN 
        ELSEIF(KEY2(1:5)=='ADYRE')THEN 
          DEBUG(MACRO_DEBUG_ADYRE)= 1 
        ELSEIF(KEY2(1:5)=='CHKSM')THEN 
          DEBUG(MACRO_DEBUG_CHKSM)= NN 
        ELSEIF(KEY2(1:3)=='NAN') THEN
          DEBUG(MACRO_DEBUG_NAN) = 1
        ENDIF 

      ENDDO 
 
      RETURN 
 
 9990 CONTINUE 
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,C1=KEY0(IKEY)) 
      CALL ARRET(0) 
      END 
