Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||    spmd_inisph   ../engine/source/mpi/sph/spmd_sptool.F
      !||--- called by ------------------------------------------------------
      !||    rdresb        ../engine/source/output/restart/rdresb.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg        ../engine/source/output/message/message.F
      !||    arret         ../engine/source/system/arret.F
      !||    read_db       ../common_source/tools/input_output/read_db.F
      !||    read_i_c      ../common_source/tools/input_output/write_routtines.c
      !||--- uses       -----------------------------------------------------
      !||    message_mod   ../engine/share/message_module/message_mod.F
      !||    sphbox        ../engine/share/modules/sphbox.F
      !||====================================================================
      SUBROUTINE SPMD_INISPH(IFLG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "sphcom.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFLG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER P, IERROR, IERROR1
C-----------------------------------------------
      IF(IFLG==1)THEN
       NSPHR = 0
       NSPHS = 0
       IF(NUMSPHG>0.AND.IMACH==3.AND.NSPMD>1)THEN
        ALLOCATE(PSPHR(NSPMD),STAT=IERROR)
        ALLOCATE(PSPHS(NSPMD),STAT=IERROR1)
        IERROR = IERROR + IERROR1
        IF(IERROR/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        END IF
        CALL READ_I_C(PSPHR,NSPMD)
        CALL READ_I_C(PSPHS,NSPMD)
        NSPHR = 0
        NSPHS = 0
        DO P = 1, NSPMD
          NSPHR = NSPHR + PSPHR(P)
          NSPHS = NSPHS + PSPHS(P)
        END DO
        IERROR = 0
        IERROR1 = 0
        IF(NSPHR/=0)ALLOCATE(ISPHR(NSPHR),STAT=IERROR)
        IF(NSPHS/=0)THEN
          ALLOCATE(ISPHS(NSPHS),STAT=IERROR1)
          IERROR = IERROR + IERROR1
          ALLOCATE(LSPHS(NSPHS),STAT=IERROR1)
          IERROR = IERROR + IERROR1
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          END IF
          CALL READ_I_C(LSPHS,NSPHS)
        END IF
        IF(NSPCOND>0)THEN
          ALLOCATE(ISPSYMR(NSPCOND,NSPHR),STAT=IERROR)
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          END IF
          CALL READ_I_C(ISPSYMR,NSPCOND*NSPHR)
        END IF
       END IF
C
      ELSEIF(IFLG==2.AND.IMACH==3.AND.NSPMD>1)THEN
        IF(NSPHR/=0)THEN
          ALLOCATE(XSPHR(SIZSPC,NSPHR),STAT=IERROR)
          ALLOCATE(WACOMPR(SIZSPW,NSPHR),STAT=IERROR1)
          IERROR = IERROR + IERROR1
          IF(IERROR/=0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          END IF
          CALL READ_DB(XSPHR,SIZSPC*NSPHR)
        END IF
      END IF
C
      RETURN
      END 
C
      !||====================================================================
      !||    spmd_savesph   ../engine/source/mpi/sph/spmd_sptool.F
      !||--- called by ------------------------------------------------------
      !||    wrrestp        ../engine/source/output/restart/wrrestp.F
      !||--- calls      -----------------------------------------------------
      !||    write_db       ../common_source/tools/input_output/write_db.F
      !||    write_i_c      ../common_source/tools/input_output/write_routtines.c
      !||--- uses       -----------------------------------------------------
      !||    sphbox         ../engine/share/modules/sphbox.F
      !||====================================================================
      SUBROUTINE SPMD_SAVESPH(IFLG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFLG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      IF(NUMSPH>0.AND.IMACH==3.AND.NSPMD>1)THEN
        IF(IFLG==1)THEN
          CALL WRITE_I_C(PSPHR,NSPMD)
          CALL WRITE_I_C(PSPHS,NSPMD)
          IF(NSPHS/=0)
     .      CALL WRITE_I_C(LSPHS,NSPHS)
          IF(NSPCOND/=0.AND.NSPHR/=0)   
     .      CALL WRITE_I_C(ISPSYMR,NSPCOND*NSPHR)
        ELSEIF(IFLG==2)THEN
          IF(NSPHR/=0)
     .      CALL WRITE_DB(XSPHR,NSPHR*SIZSPC)
        END IF
      END IF
C
      RETURN
      END 
C
