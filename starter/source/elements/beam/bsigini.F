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
!||    bsigini                ../starter/source/elements/beam/bsigini.F
!||--- called by ------------------------------------------------------
!||    pinit3                 ../starter/source/elements/beam/pinit3.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    fretitl2               ../starter/source/starter/freform.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE BSIGINI(ELBUF_STR,
     .                   IGTYP    ,NEL    ,NSIGBEAM  ,SIGBEAM   ,PTBEAM,
     .                   IXP      ,IGEO   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IGTYP,NEL,NSIGBEAM
      INTEGER :: PTBEAM(*),IXP(NIXP,*),IGEO(NPROPGI,*)
      my_real :: SIGBEAM(NSIGBEAM,*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,II,JJ,IPT,IR,IS,PT,KK(3),ILAY,NPTI,PID,IPID
      CHARACTER(LEN=NCHARTITLE) :: TITR1
      TYPE(G_BUFEL_),POINTER :: GBUF
      TYPE(L_BUFEL_),POINTER :: LBUF
C----------------------------------------------------------------------- 
      GBUF => ELBUF_STR%GBUF
!---
!     INITIAL STRESS
!---
      DO I=1,3
        KK(I) = NEL*(I-1)
      ENDDO
!
      DO I=LFT,LLT
        II = I+NFT
        JJ = PTBEAM(II)
        IF (JJ > 0) THEN
!---
          NPTI = NINT(SIGBEAM(2,JJ))
!---
          GBUF%EINT(KK(1)+I) = SIGBEAM(4,JJ)
          GBUF%EINT(KK(2)+I) = SIGBEAM(5,JJ)
!
          GBUF%FOR(KK(1)+I) = SIGBEAM(6,JJ)
          GBUF%FOR(KK(2)+I) = SIGBEAM(7,JJ)
          GBUF%FOR(KK(3)+I) = SIGBEAM(8,JJ)
!
          GBUF%MOM(KK(1)+I) = SIGBEAM(9,JJ)
          GBUF%MOM(KK(2)+I) = SIGBEAM(10,JJ)
          GBUF%MOM(KK(3)+I) = SIGBEAM(11,JJ)
!---
          IF (IGTYP == 3) THEN
            IF(GBUF%G_PLA > 0) GBUF%PLA(I) = SIGBEAM(12,JJ)
! check NPT /= NPTI
            IF (NPT /= NPTI  .AND . NPTI /= 0) THEN
              IPID=IXP(5,I)
              PID=IGEO(1,IXP(5,I))
              CALL FRETITL2(TITR1,IGEO(NPROPGI-LTITR+1,IPID),LTITR)
              CALL ANCMSG(MSGID=1233,ANMODE=ANINFO,MSGTYPE=MSGERROR,I1=PID,I2=IXP(NIXP,I),C1=TITR1)
            ENDIF
!
          ELSEIF (IGTYP == 18) THEN
            PT = 11
! check NPT /= NPTI
            IF (NPT /= NPTI  .AND. NPTI /= 0) THEN
              IPID=IXP(5,I)
              PID=IGEO(1,IXP(5,I))            
              CALL FRETITL2(TITR1,IGEO(NPROPGI-LTITR+1,IPID),LTITR)
              CALL ANCMSG(MSGID=1233,ANMODE=ANINFO,MSGTYPE=MSGERROR,I1=PID,I2=IXP(NIXP,I),C1=TITR1)
            ENDIF
!
            DO IPT=1,NPT
              ILAY=1
              IR = 1
              IS = 1
              LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(IR,IS,IPT)
              LBUF%SIG(KK(1)+I) = SIGBEAM(PT+1,JJ)
              LBUF%SIG(KK(2)+I) = SIGBEAM(PT+2,JJ)
              LBUF%SIG(KK(3)+I) = SIGBEAM(PT+3,JJ)
              IF(ELBUF_STR%BUFLY(ILAY)%L_PLA > 0) LBUF%PLA(I) = SIGBEAM(PT+4,JJ)
              PT = PT + 4
            ENDDO ! DO IPT=1,NPT
          ENDIF ! IF (IGTYP == 5)
!---
        ENDIF ! IF (JJ > 0)
      ENDDO ! DO I=LFT,LLT
!---
      RETURN
      END
