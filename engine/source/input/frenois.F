Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    frenois                ../engine/source/input/frenois.F
      !||--- called by ------------------------------------------------------
      !||    freform                ../engine/source/input/freform.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../engine/source/output/message/message.F
      !||    arret                  ../engine/source/system/arret.F
      !||    nvar                   ../engine/source/input/nvar.F
      !||    read10                 ../engine/source/input/read10.F
      !||    wriusc2                ../engine/source/input/wriusc2.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod            ../engine/share/message_module/message_mod.F
      !||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
      !||====================================================================
      SUBROUTINE FRENOIS(IKAD,KEY0,KNOISE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scrnoi_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IKAD(0:*),KNOISE
      CHARACTER KEY0(*)*5, KEY2*5, KEY3*5, KEY4*5, KEY5*5
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NBC, K, KK, IKEY
      CHARACTER(LEN=NCHARLINE100)::CARTE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IKEY=KNOISE
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       K=0
 1160  READ(IUSC1,REC=IKAD(IKEY)+K, FMT='(7X,A,1X,A,1X,A,1X,A,19X,I10)',ERR=9990)KEY2,KEY3,KEY4,KEY5,NBC
       K=K+1
       IF(KEY2(1:3)=='VEL')THEN
        NOISEV=1
       ELSEIF(KEY2(1:1)=='P')THEN
        NOISEP=1
       ELSEIF(KEY2(1:3)=='ACC')THEN
        NOISEA=1
       ELSEIF(KEY2(1:4)=='CONT')THEN
        RNOI=1
       ELSE
        IF(NBC>0)THEN
c         K=K+1
         NNOISE=0
         RNOI=0
         CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
         READ(IUSC2,*)TNOISE,DTNOISE
         K=K+1
         KK=K
         DO 1300 I=1,NBC-1
          READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990)CARTE
          K=K+1
          NNOISE=NNOISE+NVAR(CARTE)
 1300    CONTINUE
         CALL READ10(IKAD(IKEY)+KK,NBC-1,KEY0(IKEY))
         IF(KEY2(1:4)=='INIT')RNOI=2
        ELSE
         RNOI=1
        ENDIF
       ENDIF
       IF(IKAD(IKEY)+K/=IKAD(IKEY+1))GO TO 1160
      ENDIF
C
      RETURN
C
 9990 CONTINUE
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .            C1=KEY0(IKEY))
      CALL ARRET(0)
      END
