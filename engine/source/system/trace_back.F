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
#define NSUB_MAX 21
#define NIVEAU_MAX 10
#define NLIGNES_MAX 20
!||====================================================================
!||    trace_in      ../engine/source/system/trace_back.F
!||--- called by ------------------------------------------------------
!||    radioss2      ../engine/source/engine/radioss2.F
!||    resol         ../engine/source/engine/resol.F
!||    resol_head    ../engine/source/engine/resol_head.F
!||--- calls      -----------------------------------------------------
!||    trace_pile    ../engine/source/system/trace_back.F
!||    trace_print   ../engine/source/system/trace_back.F
!||====================================================================
      SUBROUTINE TRACE_IN(NSUB,ITAB,ATAB)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "comlock.inc"
#include      "task_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSUB,ITAB(*)
      my_real
     .   ATAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER*132 LINE
      CHARACTER*132 LINES( NSUB_MAX )
      INTEGER TRACE_PILE,LL( NSUB_MAX ) ,TYP( NSUB_MAX )
      EXTERNAL TRACE_PILE
      INTEGER NIVEAU_PILE
      SAVE LINES,LL,TYP
      DATA LINES /'RADIOSS ENGINE','SOLUTION PHASE: Processor=',
     .'MAIN LOOP: Cycle=','CONTACT INTERFACES','SORTIES L00 ANIM TH',
     .'INITIALIZATION','SPH INITIALIZATION','CONTACT INTERFACES SORT',
     .'GLOBAL USER WINDOW',
     .'CONCENTRADED LOAD','MONITORED VOLUME','ALE',
     .'SPH INTERNAL FORCES','INTERNAL FORCES','FORCE ASSEMBLING',
     .'READ INPUT FILE','READ RESTART FILE','TH INITIALIZATION',
     .'NO LICENSE',
     .'IMPLICIT','EIGENSOLVER'/
c                                  123456789012345678901234567890
      DATA LL /14,26,17,18,19,14,18,23,18,17,16, 3,19,15,16,15,17,17,10,-1,-1/
      DATA TYP/ 1, 3, 2, 2, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1,-1,-1/
          
C-----------------------------------------------
      IF(ITRACE/=1)RETURN
      NIVEAU_PILE = TRACE_PILE(1)
      IF(NIVEAU_PILE>=NIVEAU_MAX-1)THEN
         CALL TRACE_PRINT(LINE,-2,1)
         RETURN
      ENDIF
C-----------------------------------------------
      IF(NSUB<=0.OR.NSUB> NSUB_MAX )THEN
          LINE=' ?????? '  
      ELSE
        GOTO(1,2,3)TYP(NSUB)
 1      CONTINUE
          LINE=LINES(NSUB)
          GOTO 999
 2      CONTINUE
          IF (PARALLEL_SECTION == 1) THEN
#include "lockon.inc"
           WRITE(LINE,'(A,I5,A1,I5)')LINES(NSUB)(1:LL(NSUB)),ITAB(1)
#include "lockoff.inc"
          ELSE
           WRITE(LINE,'(A,I5,A1,I5)')LINES(NSUB)(1:LL(NSUB)),ITAB(1)
          ENDIF
          GOTO 999
 3      CONTINUE
          IF (PARALLEL_SECTION == 1) THEN
#include "lockon.inc"
           WRITE(LINE,'(A,I5,A1,I5)')LINES(NSUB)(1:LL(NSUB)),
     .                              ITAB(1)+1,'/',ITAB(2)
#include "lockoff.inc"
          ELSE
           WRITE(LINE,'(A,I5,A1,I5)')LINES(NSUB)(1:LL(NSUB)),
     .                              ITAB(1)+1,'/',ITAB(2)
          ENDIF
          GOTO 999
C
 999    CONTINUE
      ENDIF
C-----------------------------------------------
      CALL TRACE_PRINT(LINE,NSUB,0)
C-----------------------------------------------
      RETURN
C
      END
!||====================================================================
!||    trace_out     ../engine/source/system/trace_back.F
!||--- called by ------------------------------------------------------
!||    radioss2      ../engine/source/engine/radioss2.F
!||    resol         ../engine/source/engine/resol.F
!||    resol_head    ../engine/source/engine/resol_head.F
!||--- calls      -----------------------------------------------------
!||    trace_pile    ../engine/source/system/trace_back.F
!||    trace_print   ../engine/source/system/trace_back.F
!||====================================================================
      SUBROUTINE TRACE_OUT(NSUB)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSUB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER TRACE_PILE
      EXTERNAL TRACE_PILE
      INTEGER NIVEAU_PILE
      CHARACTER*132 BLANC
C-----------------------------------------------
      IF(ITRACE/=1)RETURN
      BLANC=' '
      CALL TRACE_PRINT(BLANC,NSUB,-1)
      NIVEAU_PILE = TRACE_PILE(-1)
C-----------------------------------------------
      RETURN
      END
!||====================================================================
!||    trace_pile    ../engine/source/system/trace_back.F
!||--- called by ------------------------------------------------------
!||    trace_in      ../engine/source/system/trace_back.F
!||    trace_out     ../engine/source/system/trace_back.F
!||    trace_print   ../engine/source/system/trace_back.F
!||====================================================================
      INTEGER FUNCTION TRACE_PILE(INOUT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "scr01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER INOUT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIVEAU_PILE(PARASIZ),IT
      DATA NIVEAU_PILE/PARASIZ*0/
      SAVE NIVEAU_PILE
      IF(ITASKP1==0)THEN
        IT = 1
      ELSE
        IT = ITASKP1
      ENDIF
C-----------------------------------------------
      NIVEAU_PILE(IT) = NIVEAU_PILE(IT) + INOUT
      TRACE_PILE = NIVEAU_PILE(IT)
C-----------------------------------------------
      RETURN
      END
!||====================================================================
!||    trace_cf      ../engine/source/system/trace_back.F
!||--- calls      -----------------------------------------------------
!||    trace_print   ../engine/source/system/trace_back.F
!||====================================================================
      SUBROUTINE TRACE_CF(SIGNAL,IW)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SIGNAL,IW
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER*132 LINE
C-----------------------------------------------
      CALL TRACE_PRINT(LINE,SIGNAL,IW)
      RETURN
      END
!||====================================================================
!||    trace_print   ../engine/source/system/trace_back.F
!||--- called by ------------------------------------------------------
!||    trace_cf      ../engine/source/system/trace_back.F
!||    trace_in      ../engine/source/system/trace_back.F
!||    trace_out     ../engine/source/system/trace_back.F
!||--- calls      -----------------------------------------------------
!||    ancmsg        ../engine/source/output/message/message.F
!||    arret         ../engine/source/system/arret.F
!||    trace_pile    ../engine/source/system/trace_back.F
!||--- uses       -----------------------------------------------------
!||    message_mod   ../engine/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE TRACE_PRINT(LINE,NS_SIGNAL,IW)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "task_c.inc"
#include      "scr01_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*132 LINE
      INTEGER NS_SIGNAL,IW
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER TRACE_PILE
      EXTERNAL TRACE_PILE
      INTEGER NIVEAU_PILE,NLIGNES_PILE(0:NIVEAU_MAX,PARASIZ)
      INTEGER CHECK_PILE(NIVEAU_MAX,PARASIZ)
      CHARACTER*80 BLANC
      CHARACTER*132 TEXT(NLIGNES_MAX,PARASIZ)
      INTEGER I,IP,IL,NLIGNES,IT
      LOGICAL :: FIRST_CALL = .TRUE.
      SAVE NLIGNES_PILE,TEXT,CHECK_PILE, FIRST_CALL
      INTEGER NS_SIGNAL1,IW1,LAST_IN
C-----------------------------------------------
      IF(FIRST_CALL) THEN
        FIRST_CALL = .FALSE.
        NLIGNES_PILE = 0
        CHECK_PILE = 0
      ENDIF
C-----------------------------------------------
      BLANC=' '
      NIVEAU_PILE = TRACE_PILE(0)
      LAST_IN = NIVEAU_PILE
      NS_SIGNAL1=NS_SIGNAL
      IW1=IW
      IF(ITASKP1_DEF==0)THEN
        IT = 1
      ELSE
        IT = ITASKP1
      ENDIF
C
      IF(IW1==0) THEN
C------------------------------------------------------------------------
C       Ajout d'une ligne a TEXT (1 ou plusieurs X par descente de pile)
C------------------------------------------------------------------------
        NLIGNES_PILE(NIVEAU_PILE,IT) 
     .         = NLIGNES_PILE(NIVEAU_PILE,IT) + 1
        NLIGNES_PILE(NIVEAU_PILE+1,IT)
     .         = NLIGNES_PILE(NIVEAU_PILE,IT)
        NLIGNES = NLIGNES_PILE(NIVEAU_PILE,IT)
        TEXT(NLIGNES,IT)(1:NIVEAU_PILE)    =BLANC(1:NIVEAU_PILE)
        TEXT(NLIGNES,IT)(NIVEAU_PILE+1:132)=LINE(1:132-NIVEAU_PILE)
        CHECK_PILE(NIVEAU_PILE,IT) = NS_SIGNAL
      ELSEIF(IW1==-1) THEN
C------------------------------------------
C       remonte de pile 
C------------------------------------------
        IF(NS_SIGNAL1==CHECK_PILE(NIVEAU_PILE,IT))THEN
          NLIGNES_PILE(NIVEAU_PILE,IT) 
     .           = NLIGNES_PILE(NIVEAU_PILE-1,IT)
        ELSE
          NS_SIGNAL1 = 4
          IW1=1
        ENDIF
      ENDIF
C
      IF(IW1>0) THEN
C------------------------------------------
C       Ecriture du Trace Back et Arret
C------------------------------------------
        IF(NIVEAU_PILE<=0)STOP
        NLIGNES = NLIGNES_PILE(NIVEAU_PILE,IT)
        NIVEAU_PILE = TRACE_PILE(-9999)
C
        IF(NS_SIGNAL1==-1)THEN
          CALL ANCMSG(MSGID=40,ANMODE=ANINFO)
        ELSEIF(NS_SIGNAL1==-2)THEN
          CALL ANCMSG(MSGID=41,ANMODE=ANINFO)
        ELSEIF(NS_SIGNAL1==1)THEN
          CALL ANCMSG(MSGID=42,ANMODE=ANINFO)
        ELSEIF(NS_SIGNAL1==2)THEN
          CALL ANCMSG(MSGID=43,ANMODE=ANINFO)
        ELSEIF(NS_SIGNAL1==3)THEN
          CALL ANCMSG(MSGID=44,ANMODE=ANINFO)
        ELSEIF(NS_SIGNAL1==4)THEN
          CALL ANCMSG(MSGID=45,ANMODE=ANINFO,
     .                I1=NS_SIGNAL,
     .                I2=LAST_IN)
        ENDIF
C
        WRITE(IOUT,'(A,A)')'      ',
     .'+=============================================================+'

        DO I=1,NLIGNES
          WRITE(IOUT,'(A,A)')'      | + ',TEXT(I,IT)
        ENDDO
        WRITE(IOUT,'(A,A)')'      ',
     .'+=============================================================+'
        IF (NS_SIGNAL1/=4) THEN
          CALL ARRET(6)
        END IF
      ENDIF
C
      RETURN
      END
